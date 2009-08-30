;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-GSSM.
;;;
;;; CLJ-GSSM is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-GSSM is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-GSSM.  If not, see <http://www.gnu.org/licenses/>.

(ns gssm.line-of-sight-model
  (:refer-clojure)
  (:use [gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]
	[gssm.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[gssm.params    :only (*decay-rate* *trans-threshold*)]))

(defn find-viewpath
  "Returns the sequence of all points [i j] intersected by the line
   from provider to beneficiary.  Since this is calculated over a
   regular integer-indexed grid, diagonal lines will be approximated
   by lines bending at right angles along the p-to-b line.  This
   calculation imagines the indeces of each point to be located at the
   center of a square of side length 1.  Note that the first point in
   each path will be the provider id, and the last will be the
   beneficiary id.  If provider=beneficiary, the path will contain
   only this one point."
  [provider beneficiary]
  (let [[pi pj] (:id provider)
	[bi bj] (:id beneficiary)
	m (if (not= pj bj) (/ (- bi pi) (- bj pj)))
	b (if m (- pi (* m pj)))
	f (fn [x] (+ (* m x) b))]
    (cond (nil? m) (map (fn [i] [i pj])
			(if (< pi bi)
			  (range pi (inc bi))
			  (range pi (dec bi) -1)))

	  (== m 0) (map (fn [j] [pi j])
			(if (< pj bj)
			  (range pj (inc bj))
			  (range pj (dec bj) -1)))

	  :otherwise (let [get-i-range
			   (cond (and (< pi bi) (< pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j pj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j bj) 0.0 0.5))))]
					   (range left-i (inc right-i))))
					       
				 (and (< pi bi) (> pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j bj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j pj) 0.0 0.5))))]
					   (range right-i (inc left-i))))
					       
				 (and (> pi bi) (< pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j pj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j bj) 0.0 0.5))))]
					   (range left-i  (dec right-i) -1)))
					       
				 (and (> pi bi) (> pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j bj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j pj) 0.0 0.5))))]
					   (range right-i (dec left-i)  -1))))
			   j-range (if (< pj bj)
				     (range pj (inc bj))
				     (range pj (dec bj) -1))]
		       (for [j j-range i (get-i-range j)] [i j])))))

(defn get-valid-elevation
  [location]
  (let [elev ((:flow-features location) "Altitude")]
    (if (> elev 55535.0) 0.0 elev)))

(defn elevation-interference?
  [provider beneficiary path steps]
  (let [source-elev (get-valid-elevation provider)
	rise        (- (get-valid-elevation beneficiary) source-elev)
	m           (/ rise steps)
	f           (fn [x] (+ (* m x) source-elev))]
    (loop [step 1
	   untraversed-locs (rest path)]
      (when (< step steps)
	(let [current-loc (first untraversed-locs)]
	  (if (>= (get-valid-elevation current-loc) (f step))
	    true
	    (recur (inc step)
		   (rest untraversed-locs))))))))
;;    (some (fn [[loc step]] (>= (get-valid-elevation loc) (f step)))
;;	  (map vector (rest path) (range steps)))

(defn update-sinks!
  [source-val path steps]
  (loop [step 0
	 traversed-locs []
	 untraversed-locs path]
    (when (< step steps)
      (let [current-loc  (first untraversed-locs)
	    current-path (conj traversed-locs current-loc)]
	(when (sink-loc? current-loc)
	  (swap! (:carrier-cache current-loc) conj
		 (struct service-carrier
			 (/ (* source-val (Math/pow *decay-rate* step))
			    (* step step))
			 current-path)))
	(recur (inc step)
	       current-path
	       (rest untraversed-locs))))))

(defn distribute-raycast!
  [provider beneficiary location-map]
  (let [source-val (force (:source provider))
	path       (map location-map (find-viewpath provider beneficiary))
	steps      (dec (count path))
	asset-propagated (if (pos? steps)
			   (/ (* source-val (Math/pow *decay-rate* steps))
			      (* steps steps))
			   source-val)]
    (when (pos? steps)
      (if (and (> asset-propagated *trans-threshold*)
	       (not (elevation-interference? provider beneficiary path steps)))
	(update-sinks! source-val path steps)))
    (swap! (:carrier-cache beneficiary) conj
	   (struct service-carrier asset-propagated (vec path)))))

(defmethod distribute-flow! "LineOfSight"
  [_ location-map _ _]
  (println "Global LineOfSight Model begins...")
  (let [locations     (vals location-map)
        providers     (filter source-loc? locations)
        beneficiaries (filter use-loc? locations)]
    (println "Num Providers:" (count providers))
    (println "Num Beneficiaries:" (count beneficiaries))
    (distribute-load-over-processors
     (fn [_ [p b]] (distribute-raycast! p b location-map))
     (for [p providers b beneficiaries] [p b]))))
