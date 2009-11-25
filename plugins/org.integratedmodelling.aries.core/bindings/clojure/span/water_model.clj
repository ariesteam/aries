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
	[misc.stats     :only (rv-scalar-div rv-from-scalar rv-gt rv-scalar-gt scalar-rv-gt rv-min rv-eq)]
	[span.model-api :only (distribute-flow! service-carrier distribute-load-over-processors)]
	[span.analyzer  :only (source-loc? sink-loc? use-loc?)]
	[span.params    :only (*trans-threshold*)]))
(refer 'tl :only '(conc))

(def #^{:private true} elev-concept (conc 'geophysics:Altitude))

(defn- most-downhill-neighbors
  [location location-map min-fn eq-fn]
  (let [neighbors      (map location-map (:neighbors location))
	neighbor-elevs (map #((:flow-features %) elev-concept) neighbors)
	local-elev     ((:flow-features location) elev-concept)
	min-elev       (reduce min-fn local-elev neighbor-elevs)]
    (remove nil?
	    (map (fn [n elev] (if (eq-fn elev min-elev) n))
		 neighbors neighbor-elevs))))
(def most-downhill-neighbors (memoize-by-first-arg most-downhill-neighbors))

(defn distribute-downhill!
  "Depth-first search with successors = downhill neighbors.
   Stop when no successors.  No decay-rate, but branching-factor is
   possible, so check for weight below trans-threshold."
  [location-map source-location div-fn zero-val gt-fn min-fn eq-fn]
  (depth-first-tree-search
   (list [(:source source-location) [source-location]])
   (fn [[weight route]]
     (let [downhill-neighbors (most-downhill-neighbors (peek route) location-map min-fn eq-fn)
	   downhill-weight    (if (seq downhill-neighbors)
				(div-fn weight (count downhill-neighbors))
				zero-val)]
       (when (gt-fn downhill-weight *trans-threshold*)
	 (map #(vector downhill-weight (conj route %))
	      downhill-neighbors))))
   (fn [[weight route]]
     (let [loc (peek route)]
       (when (or (sink-loc? loc) (use-loc? loc))
	 (swap! (:carrier-cache loc) conj (struct service-carrier weight route)))
       false))))

;; FIXME the use of rv-from-scalar assumes that the first key in s is
;; the zero value of the distribution.
(defmethod distribute-flow! "Water"
  [_ location-map _ _]
  (let [first-loc         (val (first location-map))
	s                 (:source first-loc)
	[div-fn zero-val] (if (map? s) [rv-scalar-div (rv-from-scalar s (first (keys s)))] [/ 0.0])
	gt-fn             (if (map? s)
			    (if (map? *trans-threshold*) rv-gt rv-scalar-gt)
			    (if (map? *trans-threshold*) scalar-rv-gt >))
	[min-fn eq-fn]    (if (map? ((:flow-features first-loc) elev-concept)) [rv-min rv-eq] [min ==])]
    (distribute-load-over-processors
     (fn [_ source-location]
       (distribute-downhill! location-map source-location div-fn zero-val gt-fn min-fn eq-fn))
     (filter source-loc? (vals location-map)))))
