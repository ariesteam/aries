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

(ns gssm.water-model
  (:refer-clojure)
  (:use [misc.utils     :only (memoize-by-first-arg depth-first-tree-search)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]
	[gssm.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[gssm.params    :only (*trans-threshold*)]))

(defn most-downhill-neighbors
  [location location-map]
  (let [neighbors      (map location-map (:neighbors location))
	neighbor-elevs (map #((:flow-features %) "Elevation") neighbors)
	local-elev     ((:flow-features location) "Elevation")
	min-elev       (apply min local-elev neighbor-elevs)]
    (filter identity
	    (map (fn [n elev] (if (== elev min-elev) n))
		 neighbors neighbor-elevs))))
(def most-downhill-neighbors (memoize-by-first-arg most-downhill-neighbors))

(defn distribute-downhill!
  "Depth-first search with successors = downhill neighbors.
   Stop when no successors.  No decay-rate, but branching-factor is
   possible, so check for weight below trans-threshold."
  [location-map source-location source-weight]
  (depth-first-tree-search
   (list [source-weight [source-location]])
   (fn [[weight route]]
     (let [downhill-neighbors (most-downhill-neighbors (peek route) location-map)
	   downhill-weight    (if (seq downhill-neighbors)
			        (/ weight (count downhill-neighbors))
				0.0)]
       (when (> downhill-weight *trans-threshold*)
	 (map #(vector downhill-weight (conj route %))
	      downhill-neighbors))))
   (fn [[weight route]]
     (let [loc (peek route)]
       (when (or (sink-loc? loc) (use-loc? loc))
	 (swap! (:carrier-cache loc) conj (struct service-carrier weight route)))
       false))))

(defmethod distribute-flow! "Water"
  [_ location-map _ _]
  (distribute-load-over-processors
   (fn [_ source-location]
     (distribute-downhill! location-map
			   source-location
			   (force (:source source-location))))
   (filter source-loc? (vals location-map))))
