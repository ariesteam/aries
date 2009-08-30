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

(ns gssm.proximity-model
  (:refer-clojure)
  (:use [gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]
	[gssm.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[gssm.params    :only (*decay-rate* *trans-threshold*)]))

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
  [location frontier]
  (let [frontier-options (nearest-neighbors location frontier)]
    (when (seq frontier-options)
      (let [[outflow route] (reduce (fn [[w1 r1 :as prev] [w2 r2]]
				      (let [outflow (* w2 (- 1.0 (force (:sink (peek r2)))))]
					(if (> outflow w1)
					  [outflow r2]
					  prev)))
				    [0.0 nil]
				    frontier-options)]
	[(* outflow *decay-rate*) (conj route location)]))))

(defn distribute-gaussian!
  [location-map rows cols source-location source-weight]
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
	 (filter #(and % (> (first %) *trans-threshold*))
		 (map #(select-optimal-path % frontier) bounding-locs)))))))

(defmethod distribute-flow! "Proximity"
  [_ location-map rows cols]
  (println "Local Proximity Model begins...")
  (distribute-load-over-processors
   (fn [_ source-location]
     (distribute-gaussian! location-map
			   rows
			   cols
			   source-location
			   (force (:source source-location))))
   (filter source-loc? (vals location-map))))

(defmethod distribute-flow! "Proximity_Sequential"
  [_ location-map rows cols]
  (println "Local Proximity Model begins...")
  (let [sources (vec (filter source-loc? (vals location-map)))
	num-sources (count sources)]
    (dotimes [i num-sources]
	(let [source-location (sources i)]
	  (println "Projection" (inc i) "/" num-sources)
	  (distribute-gaussian! location-map
				rows
				cols
				source-location
				(force (:source source-location)))))))
