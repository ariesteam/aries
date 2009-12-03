;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.line-of-sight-model
  (:refer-clojure)
  (:use [misc.stats     :only (rv-zero-above-scalar rv-add rv-subtract
			       rv-scalar-divide rv-scale rv-lt rv-mean)]
	[span.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]
	[span.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[span.params    :only (*trans-threshold*)]))
(refer 'tl :only '(conc))

(def #^{:private true} elev-concept (conc 'geophysics:Altitude))

(defn- find-viewpath
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
(defn- get-valid-elevation
  [location]
  (rv-zero-above-scalar (get-in location [:flow-features elev-concept]) 55535.0))
(def get-valid-elevation (memoize get-valid-elevation))

;; FIXME convert step to distance metric based on map resolution
(defn- decay [weight step] (rv-scalar-divide weight (* step step)))

(defn- distribute-raycast!
  [provider beneficiary location-map]
  (if (= provider beneficiary)
    (swap! (:carrier-cache provider) conj
	   (struct service-carrier (:source provider) (vec provider)))
    (let [path        (map location-map (find-viewpath provider beneficiary))
	  steps       (dec (count path))
	  source-val  (:source provider)]
      (when (or (== steps 1)
		(> (rv-mean (rv-scalar-divide source-val (Math/pow (dec steps) 2))) *trans-threshold*))
	(let [source-elev (get-valid-elevation provider)
	      rise        (rv-subtract (get-valid-elevation beneficiary) source-elev)
	      m           (rv-scalar-divide rise steps)]
	  (loop [step        1
		 current     (second path)
		 explored    (vec (take 2 path))
		 frontier    (drop 2 path)
		 elev-bounds (rest (iterate #(rv-add m %) source-elev))
		 weight      source-val
		 delayed-ops (if (sink-loc? provider)
			       [#(swap! (:carrier-cache provider) conj
					(struct service-carrier weight (vec provider)))]
			       [])]
	    (let [decayed-weight (decay weight step)]
	      (if (== step steps)
		(do
		  (doseq [op delayed-ops] (op))
		  (swap! (:carrier-cache current) conj
			 (struct service-carrier decayed-weight explored)))
		(when (> (rv-mean decayed-weight) *trans-threshold*)
		  (recur (inc step)
			 (first frontier)
			 (conj explored (first frontier))
			 (rest frontier)
			 (rest elev-bounds)
			 (rv-scale weight (rv-lt (get-valid-elevation current) (first elev-bounds)))
			 (if (sink-loc? current)
			   (conj delayed-ops
				 #(swap! (:carrier-cache current) conj
					 (struct service-carrier decayed-weight explored)))
			   delayed-ops)))))))))))

(defmethod distribute-flow! "LineOfSight"
  [_ location-map _ _]
  (let [locations (vals location-map)]
    (distribute-load-over-processors
     (fn [_ [p b]] (distribute-raycast! p b location-map))
     (for [p (filter source-loc? locations) b (filter use-loc? locations)] [p b]))))
