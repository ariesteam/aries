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

(ns span.water-model
  (:refer-clojure)
  (:use [misc.utils     :only (memoize-by-first-arg depth-first-tree-search)]
	[misc.stats     :only (rv-min rv-eq rv-lt rv-scale rv-scalar-divide rv-zero-below-scalar mean)]
	[span.model-api :only (distribute-flow! service-carrier distribute-load-over-processors)]
	[span.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[span.params    :only (*trans-threshold*)]))
(refer 'tl :only '(conc))

(def #^{:private true} elev-concept (conc 'geophysics:Altitude))

(defn- most-downhill-neighbors
  [location location-map]
  (let [neighbors      (map location-map (:neighbors location))
	neighbor-elevs (map #((:flow-features %) elev-concept) neighbors)
	local-elev     ((:flow-features location) elev-concept)
	min-elev       (reduce rv-min local-elev neighbor-elevs)]
    (remove nil?
	    (map (fn [n elev] (if (rv-eq elev min-elev) n))
		 neighbors neighbor-elevs))))
(def most-downhill-neighbors (memoize-by-first-arg most-downhill-neighbors))

(defn- distribute-downhill!-old
  "Depth-first search with successors = downhill neighbors.
   Stop when no successors.  No decay-rate, but branching-factor is
   possible, so check for weight below trans-threshold."
  [location-map source-location]
  (depth-first-tree-search
   (list [(:source source-location) [source-location]])
   (fn [[weight route]]
     (when-let [downhill-neighbors (most-downhill-neighbors (peek route) location-map)]
       (let [downhill-weight (rv-zero-below-scalar (rv-scalar-divide weight (count downhill-neighbors)) *trans-threshold*)]
	 (if (pos? (mean downhill-weight))
	   (map #(vector downhill-weight (conj route %)) downhill-neighbors)))))
   (fn [[weight route]]
     (let [loc (peek route)]
       (when (or (sink-loc? loc) (use-loc? loc))
	 (swap! (:carrier-cache loc) conj (struct service-carrier weight route)))
       false))))

(defn- transition-probabilities
  [location neighbors]
  (let [local-elev        (get-in location [:flow-features elev-concept])
	neighbor-elevs    (vec (map #(get-in % [:flow-features elev-concept]) neighbors))
	neighbors-lower?  (vec (map #(rv-lt % local-elev) neighbor-elevs))
	local-lowest?     (reduce (fn [a b] (* (- 1 a) (- 1 b))) neighbors-lower?)
        neighbors-lowest? (loop [i   (dec (count neighbor-elevs))
				 j   0
				 i<j neighbors-lower?]
			    (if (== i 0)
			      i<j
			      (if (== j i)
				(recur (dec i) 0 i<j)
				(recur i (inc j) 
				       (let [lt-prob (rv-lt (neighbor-elevs i) (neighbor-elevs j))]
					 (-> i<j
					     (assoc i (* (i<j i) lt-prob))
					     (assoc j (* (i<j j) (- 1 lt-prob)))))))))]
    (cons local-lowest? neighbors-lowest?)))
(def transition-probabilities (memoize-by-first-arg transition-probabilities))

(defn- distribute-downhill!
  "Depth-first search with successors = downhill neighbors.
   Stop when no successors.  No decay-rate, but branching-factor is
   possible, so check for weight below trans-threshold."
  [location-map source-location]
  (depth-first-tree-search
   (list [(:source source-location) [source-location]])
   (fn [[weight route]]
     (let [current-loc (peek route)
	   neighbors   (map location-map (:neighbors current-loc))
	   trans-probs (transition-probabilities current-loc neighbors)]
       (when (< (first trans-probs) 0.9) ;; FIXME add this to flow-params
	 (filter (fn [[w _]] (> (mean w) *trans-threshold*))
		 (map (fn [l p] [(rv-scale weight p) (conj route l)]) neighbors (rest trans-probs))))))
   (fn [[weight route]]
     (let [current-loc (peek route)]
       (when (or (sink-loc? current-loc) (use-loc? current-loc))
	 (swap! (:carrier-cache current-loc) conj (struct service-carrier weight route)))
       false))))

(defmethod distribute-flow! "Water"
  [_ location-map _ _]
  (distribute-load-over-processors
   (fn [_ source-location] (distribute-downhill! location-map source-location))
   (filter source-loc? (vals location-map))))
