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

(ns span.proximity-model
  (:refer-clojure)
  (:use [misc.stats :only (rv-from-scalar scalar-rv-subtract rv-multiply rv-scalar-multiply scalar-rv-multiply rv-gt rv-scalar-gt scalar-rv-gt)]
	[span.model-api :only (distribute-flow! service-carrier distribute-load-over-processors)]
	[span.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[span.params    :only (*decay-rate* *trans-threshold*)]))

(defn expand-box
  "Returns a new list of points which completely bounds the
   rectangular region defined by points and remains within the bounds
   [0-rows],[0-cols]."
  [points rows cols]
  (when (seq points)
    (let [row-coords (map first  points)
	  col-coords (map second points)
	  min-i (apply min row-coords)
	  min-j (apply min col-coords)
	  max-i (apply max row-coords)
	  max-j (apply max col-coords)
	  bottom (dec min-i)
	  top    (inc max-i)
	  left   (dec min-j)
	  right  (inc max-j)]
      (concat
       (when (>= left   0)    (for [i (range min-i top) j [left]]     [i j]))
       (when (<  right  cols) (for [i (range min-i top) j [right]]    [i j]))
       (when (>= bottom 0)    (for [i [bottom] j (range min-j right)] [i j]))
       (when (<  top    rows) (for [i [top]    j (range min-j right)] [i j]))
       (when (and (>= left 0)     (<  top rows)) (list [top left]))
       (when (and (>= left 0)     (>= bottom 0)) (list [bottom left]))
       (when (and (<  right cols) (<  top rows)) (list [top right]))
       (when (and (<  right cols) (>= bottom 0)) (list [bottom right]))))))

(defn manhattan-distance
  [[i1 j1] [i2 j2]]
  (+ (Math/abs (- i1 i2)) (Math/abs (- j1 j2))))

(defn nearest-neighbors
  "Selects all elements from the frontier which are within 2 manhattan
   steps of location."
  [location frontier]
  (let [loc-id (:id location)]
    (filter (fn [[_ route]]
	      (<= (manhattan-distance loc-id (:id (peek route))) 2))
	    frontier)))

(defn select-optimal-path
  [location frontier sink-mult-fn decay-mult-fn zero-val sub-fn gt-fn]
  (let [frontier-options (nearest-neighbors location frontier)]
    (when (seq frontier-options)
      (let [[outflow route] (reduce (fn [[w1 r1 :as prev] [w2 r2]]
				      (let [outflow (sink-mult-fn w2 (sub-fn 1.0 (:sink (peek r2))))]
					(if (gt-fn outflow w1)
					  [outflow r2]
					  prev)))
				    [zero-val []]
				    frontier-options)]
	[(decay-mult-fn outflow *decay-rate*) (conj route location)]))))

(defn distribute-gaussian!
  [location-map rows cols source-location source-weight gt-trans-fn sink-mult-fn decay-mult-fn zero-val sub-fn gt-fn]
  (loop [frontier (list [source-weight [source-location]])]
    (when (seq frontier)
      (doseq [[weight route] frontier]
	(let [loc (peek route)]
	  (when (or (sink-loc? loc) (use-loc? loc))
	    (swap! (:carrier-cache loc) conj (struct service-carrier weight route)))))
      (recur
       (let [frontier-ids  (map (comp :id peek second) frontier)
	     bounding-ids  (expand-box frontier-ids rows cols)
	     bounding-locs (map location-map bounding-ids)]
	 (filter #(and % (gt-trans-fn (first %) *trans-threshold*))
		 (map #(select-optimal-path % frontier sink-mult-fn decay-mult-fn zero-val sub-fn gt-fn) bounding-locs)))))))

;; FIXME rv-from-scalar assumes the first key in :source is the zero
;; value.
(defmethod distribute-flow! "Proximity"
  [_ location-map rows cols]
  (println "Local Proximity Model begins...")
  (let [first-loc     (val (first location-map))
	zero-val      (if (map? (:source first-loc)) (rv-from-scalar (:source first-loc) (first (keys (:source first-loc)))))
	sub-fn        (if (map? (:sink first-loc)) scalar-rv-subtract -)
	sink-mult-fn  (if (map? (:source first-loc))
			(if (map? (:sink first-loc)) rv-multiply rv-scalar-multiply)
			(if (map? (:sink first-loc)) scalar-rv-multiply *))
	decay-mult-fn (if (map? (:source first-loc))
			(if (map? *decay-rate*) rv-multiply rv-scalar-multiply)
			(if (map? *decay-rate*) scalar-rv-multiply *))
	gt-trans-fn   (if (map? (:source first-loc))
			(if (map? *trans-threshold*) rv-gt rv-scalar-gt)
			(if (map? *trans-threshold*) scalar-rv-gt >))
	gt-fn         (if (map? (:source first-loc)) rv-gt >)]
    (distribute-load-over-processors
     (fn [_ source-location]
       (distribute-gaussian! location-map
			     rows
			     cols
			     source-location
			     (:source source-location)
			     gt-trans-fn
			     sink-mult-fn
			     decay-mult-fn
			     zero-val
			     sub-fn
			     gt-fn))
     (filter source-loc? (vals location-map)))))
