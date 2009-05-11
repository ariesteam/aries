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
  (:use [misc.utils     :only (euclidean-distance)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]))

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

(defn find-viewpaths
  [providers beneficiaries]
  (for [p providers b beneficiaries]
    (find-viewpath p b)))

(defn get-valid-elevation
  [location]
  (let [elev ((:flow-features location) "Altitude")]
    (if (> elev 55535.0) 0.0 elev)))

(defn distribute-raycast!
  [provider beneficiary decay-rate location-map]
  (let [source-val (force (:source provider))]
    (if (= provider beneficiary)
      (swap! (:carrier-cache beneficiary) conj
	     (struct service-carrier source-val [provider]))
      (let [source-elev (get-valid-elevation provider)
	    rise  (- (get-valid-elevation beneficiary) source-elev)
	    run   (euclidean-distance (:id provider) (:id beneficiary))
	    m     (/ rise run)
	    f     (fn [x] (+ (* m x) source-elev))
	    path  (find-viewpath provider beneficiary)
	    steps (dec (count path))
	    dx    (/ run steps)]
	(loop [step 0
	       traversed-locs []
	       untraversed-ids path]
	  (when (seq untraversed-ids)
	    (let [current-loc (location-map (first untraversed-ids))]
	      (if (or (> (force (:sink current-loc)) 0.14)
		      (== step steps))
		(swap! (:carrier-cache current-loc) conj
		       (struct service-carrier
			       (* source-val (Math/pow decay-rate (* step dx)))
			       (conj traversed-locs current-loc))))
	      (let [current-elev (get-valid-elevation current-loc)
		    view-elev    (f (* step dx))]
		(when (<= current-elev view-elev)
		  (recur (inc step)
			 (conj traversed-locs current-loc)
			 (rest untraversed-ids)))))))))))

(defn distance-within-range?
  [provider beneficiary decay-rate trans-threshold]
  (let [source-val       (force (:source provider))
	path-length      (euclidean-distance (:id provider) (:id beneficiary))
	asset-propagated (* source-val (Math/pow decay-rate path-length))]
    (> asset-propagated trans-threshold)))

(defmethod distribute-flow! "LineOfSight"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (println "Global LineOfSight Model begins...")
  (let [locations     (vals location-map)
        providers     (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries (filter #(> (force (:use %)) 0.0) locations)]
    (println "Num Providers:" (count providers))
    (println "Num Beneficiaries:" (count beneficiaries))
    (distribute-load-over-processors
     (fn [_ [p b]] (if (distance-within-range? p b decay-rate trans-threshold)
			(distribute-raycast! p b decay-rate location-map)))
     (for [p (take 270 providers) b beneficiaries] [p b]))))
