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
  (:use [misc.utils     :only (seq2map depth-first-tree-search)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]))

(defn memoize-by-first-arg
  [function]
  (let [cache (atom {})]
    (fn [& args]
      (or (@cache (first args))
          (let [result (apply function args)]
	    (swap! cache assoc (first args) result)
            result)))))

(def most-downhill-neighbors
  (memoize-by-first-arg (fn [location location-map]
    (let [neighbors      (map location-map (:neighbors location))
	  neighbor-elevs (map #((:flow-features %) "Elevation") neighbors)
	  local-elev     ((:flow-features location) "Elevation")
	  min-elev       (apply min local-elev neighbor-elevs)]
      (filter identity
	      (map (fn [n elev] (if (== elev min-elev) n))
		   neighbors neighbor-elevs))))))

(defn distribute-downhill!
  "Depth-first search with successors = downhill neighbors.
   Stop when no successors.  No decay-rate, but branching-factor is
   possible, so check for weight below trans-threshold."
  [trans-threshold location-map source-location root-carrier]
  (depth-first-tree-search
   (list [source-location root-carrier])
   (fn [[loc carrier]]
     (let [downhill-neighbors (most-downhill-neighbors loc location-map)
	   downhill-weight    (if (seq downhill-neighbors)
				(/ (:weight carrier)
				   (count downhill-neighbors))
				0.0)]
       (when (> downhill-weight trans-threshold)
	 (map #(vector % (struct service-carrier 
				 downhill-weight
				 (conj (:route carrier) %)))
	      downhill-neighbors))))
   (fn [[loc carrier]]
     (if (> (+ (force (:sink loc))
	       (force (:use loc)))
	    0.0)
       (swap! (:carrier-cache loc) conj carrier))
     false)))

(defmethod distribute-flow! "Water"
  [_ {trans-threshold :trans-threshold} location-map _ _]
  (distribute-load-over-processors
   (fn [_ source-location]
     (distribute-downhill! trans-threshold
			   location-map
			   source-location
			   (struct service-carrier
				   (force (:source source-location))
				   [source-location])))
   (filter #(> (force (:source %)) trans-threshold) (vals location-map))))
