;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-span.
;;;
;;; clj-span is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-span is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-span.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines the carbon model.
;;;
;;; * Routes run from Source to Use (no Sinks in this model)
;;;
;;; * Contain positive utility values only
;;;
;;; * Divides all CO2 sequestration among users by relative
;;;   consumption (i.e. Emissions)
;;;
;;; * If there is more production (total source) than consumption
;;;   (total sink), then the excess is not assigned to any user.  As a
;;;   result, inaccessible-source (theoretical - possible) will show
;;;   the excess CO2 sequestration/storage values.

(ns clj-span.carbon-model
  (:use [clj-misc.utils      :only (p &)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.randvars   :only (_0_ _+_ _*_ _d_ rv-min)]
        [clj-misc.matrix-ops :only (print-matrix filter-matrix-for-coords make-matrix map-matrix get-rows get-cols)]))

(defmethod distribute-flow "CO2Removed"
  [_ source-layer _ use-layer _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative :use
   values."
  (println "Running Carbon flow model.")
  (println "Source Layer info:")
  (println "Rows:" (get-rows source-layer))
  (println "Cols:" (get-cols source-layer))
  ;;(print-matrix source-layer)
  (println "Use Layer info:")
  (println "Rows:" (get-rows use-layer))
  (println "Cols:" (get-cols use-layer))
  ;;(print-matrix use-layer)
  (let [cache-layer       (make-matrix (get-rows source-layer) (get-cols source-layer) (constantly (atom ())))
        source-points     (filter-matrix-for-coords (p not= _0_) source-layer)
        use-points        (filter-matrix-for-coords (p not= _0_) use-layer)]
    (println "Source points:" (count source-points))
    (println "Use points:   " (count use-points))
    (if (and (seq source-points) (seq use-points))
      (let [source-values     (map (p get-in source-layer) source-points)
            use-values        (map (p get-in use-layer)    use-points)
            total-production  (do (println "Summing Source Values") (time (reduce _+_ source-values)))
            total-consumption (do (println "Summing Use Values") (time (reduce _+_ use-values)))
            percent-produced  (do (println "Calculating Source Percents") (time (map #(_d_ % total-production) source-values)))
            source-use-ratio  (do (println "Computing Source/Use Ratio") (time (_d_ total-production total-consumption)))
            get-carrier-list  (memoize (fn [use-capacity]
                                         (let [amount-usable (rv-min use-capacity (_*_ use-capacity source-use-ratio))]
                                           (doall (map (fn [source-id source-percent]
                                                         (let [weight (_*_ source-percent amount-usable)]
                                                           (struct-map service-carrier
                                                             :source-id       source-id
                                                             :route           nil
                                                             :possible-weight weight
                                                             :actual-weight   weight
                                                             :sink-effects    nil)))
                                                       source-points
                                                       percent-produced)))))]
        (time
         (do
           (println "Updating" (count use-points) "carrier caches...")
           (doseq [_ (pmap
                      (fn [use-id use-capacity]
                        (swap! (get-in cache-layer use-id) (constantly (get-carrier-list use-capacity))))
                      use-points
                      use-values)]
             (print "*") (flush))
           (println "\nAll done.")))))
    (println "Simulation complete. Returning the cache-layer.")
    (map-matrix (& seq deref) cache-layer)))
