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
  (:use [misc.utils     :only (seq2map)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]))

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
   steps of loc-id."
  [loc-id frontier]
  (filter (fn [[l c]]
	    (<= (manhattan-distance loc-id (:id l)) 2))
	  frontier))

(defn construct-optimal-carrier
  [location frontier decay-rate]
  (let [potential-parents (nearest-neighbors (:id location) frontier)]
    (if (seq potential-parents)
      (let [[c outflow] (reduce (fn [[c1 outflow1] [c2 outflow2]]
				  (if (> outflow1 outflow2)
				    [c1 outflow1]
				    [c2 outflow2]))
				(map (fn [[l c]]
				       [c (* (:weight c) (- 1.0 (force (:sink l))))])
				     potential-parents))]
	(struct service-carrier
		(* outflow decay-rate)
		(conj (:route c) location))))))

(defn distribute-gaussian!
  [{:keys [decay-rate trans-threshold]}
   location-map rows cols source-location root-carrier]
  (loop [frontier (list [source-location root-carrier])]
    (when (seq frontier)
      (doseq [[loc carrier] frontier]
	  (if (> (+ (force (:sink loc))
		    (force (:use loc)))
		 0.0)
	    (swap! (:carrier-cache loc) conj carrier)))
      (recur
       (let [new-frontier-locs (map location-map
				    (expand-box (map (comp :id first) frontier)
						rows cols))]
	 (filter #(and (val %) (> (:weight (val %)) trans-threshold))
		 (seq2map new-frontier-locs
			  #(vector % (construct-optimal-carrier
				      % frontier decay-rate)))))))))

(defmethod distribute-flow! "Proximity"
  [_ {:keys [trans-threshold] :as flow-params} location-map rows cols]
  (println "Local Proximity Model begins...")
  (distribute-load-over-processors
   (fn [_ source-loc]
     (distribute-gaussian! flow-params
			   location-map
			   rows
			   cols
			   source-loc
			   (struct service-carrier (force (:source source-loc)) [source-loc])))
   (filter #(> (force (:source %)) 0.0) (vals location-map))))

(defmethod distribute-flow! "Proximity_Sequential"
  [_ {:keys [trans-threshold] :as flow-params} location-map rows cols]
  (println "Local Proximity Model begins...")
  (let [sources (vec (filter #(> (force (:source %)) 0.0) (vals location-map)))
	num-sources (count sources)]
    (dotimes [i num-sources]
	(let [source-loc (sources i)]
	  (println "Projection" i "/" num-sources)
	  (distribute-gaussian! flow-params
				location-map
				rows
				cols
				source-loc
				(struct service-carrier (force (:source source-loc)) [source-loc]))))))
