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
  (:use [misc.stats     :only (rv-scalar-div rv-add rv-gt rv-scalar-gt scalar-rv-gt rv-scalar-mult rv-sub rv-from-scalar rv-lt)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]
	[gssm.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[gssm.params    :only (*decay-rate* *trans-threshold*)]))

(def #^{:private true} elev-concept (tl/conc 'geophysics:Altitude))

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

;; FIXME what is this NODATA value evilness doing in my code?!
(defn get-valid-elevation
  [location gt-fn nodata-threshold zero-val]
  (let [elev ((:flow-features location) elev-concept)]
    (if (gt-fn elev nodata-threshold) zero-val elev)))

(defn elevation-interference?
  [provider beneficiary path steps mult-fn elev-div-fn sub-fn add-fn gt-fn nodata-threshold zero-val elev-ge-fn]
  (let [source-elev (get-valid-elevation provider gt-fn nodata-threshold zero-val)
	rise        (sub-fn (get-valid-elevation beneficiary gt-fn nodata-threshold zero-val) source-elev)
	m           (elev-div-fn rise steps)
	f           (fn [x] (add-fn (mult-fn m x) source-elev))]
    (loop [step 1
	   untraversed-locs (rest path)]
      (when (< step steps)
	(let [current-loc (first untraversed-locs)]
	  (if (elev-ge-fn (get-valid-elevation current-loc gt-fn nodata-threshold zero-val) (f step))
	    true
	    (recur (inc step)
		   (rest untraversed-locs))))))))
;;    (some (fn [[loc step]] (>= (get-valid-elevation loc gt-fn nodata-threshold zero-val) (f step)))
;;	  (map vector (rest path) (range steps)))

(defn update-sinks!
  [source-val path steps source-div-fn]
  (loop [step 0
	 traversed-locs []
	 untraversed-locs path]
    (when (< step steps)
      (let [current-loc  (first untraversed-locs)
	    current-path (conj traversed-locs current-loc)]
	(when (sink-loc? current-loc)
	  (swap! (:carrier-cache current-loc) conj
		 (struct service-carrier
			 (source-div-fn source-val (* step step))
			 current-path)))
	(recur (inc step)
	       current-path
	       (rest untraversed-locs))))))

;; FIXME re-examine the use of *decay-rate* in this function
(defn distribute-raycast!
  [provider beneficiary location-map mult-fn source-div-fn elev-div-fn sub-fn add-fn gt-trans-fn gt-fn nodata-threshold zero-val elev-ge-fn]
  (let [source-val       (:source provider)
	path             (map location-map (find-viewpath provider beneficiary))
	steps            (dec (count path))
	asset-propagated (if (zero? steps) source-val (source-div-fn source-val (* steps steps)))]
    (when (pos? steps)
      (if (and (gt-trans-fn asset-propagated *trans-threshold*)
	       (not (elevation-interference? provider beneficiary path steps mult-fn elev-div-fn sub-fn add-fn gt-fn nodata-threshold zero-val elev-ge-fn)))
	(update-sinks! source-val path steps source-div-fn)))
    (swap! (:carrier-cache beneficiary) conj
	   (struct service-carrier asset-propagated (vec path)))))

;; FIXME the use of rv-from-scalar assumes that the first key in
;; first-elev is the zero value of the distribution.
(defmethod distribute-flow! "LineOfSight"
  [_ location-map _ _]
  (println "Global LineOfSight Model begins...")
  (let [locations                         (vals location-map)
        providers                         (filter source-loc? locations)
        beneficiaries                     (filter use-loc? locations)
	first-loc                         (first locations)
	first-source                      (:source first-loc)
	first-elev                        ((:flow-features first-loc) elev-concept)
	[source-div-fn add-fn]            (if (map? first-source) [rv-scalar-div rv-add] [/ +])
	gt-trans-fn                       (if (map? first-source)
					    (if (map? *trans-threshold*) rv-gt rv-scalar-gt)
					    (if (map? *trans-threshold*) scalar-rv-gt >))
	[mult-fn sub-fn elev-div-fn gt-fn nodata-threshold zero-val elev-ge-fn] (if (map? first-elev)
										  [rv-scalar-mult rv-sub rv-scalar-div rv-gt
										   (rv-from-scalar first-elev 55535.0)
										   (rv-from-scalar first-elev (first (keys first-elev)))
										   (complement rv-lt)]
										  [* - / > 55535.0 0.0])]
    (println "Num Providers:" (count providers))
    (println "Num Beneficiaries:" (count beneficiaries))
    (distribute-load-over-processors
     (fn [_ [p b]] (distribute-raycast! p b location-map
					mult-fn source-div-fn elev-div-fn sub-fn add-fn
					gt-trans-fn gt-fn nodata-threshold zero-val elev-ge-fn))
     (for [p providers b beneficiaries] [p b]))))
