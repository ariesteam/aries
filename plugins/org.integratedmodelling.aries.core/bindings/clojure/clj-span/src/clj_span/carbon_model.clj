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
;;; * Contain positive utility values only
;;; * Divides all CO2 sequestration among users by relative
;;;   consumption (i.e. Emissions)

(ns clj-span.carbon-model
  (:use [clj-misc.utils      :only (p &)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.randvars   :only (_0_ _+_ _*_ _d_ rv-min)]
        [clj-misc.matrix-ops :only (filter-matrix-for-coords make-matrix map-matrix get-rows get-cols)]))

(defmethod distribute-flow "Carbon"
  [_ source-layer _ use-layer _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative :use
   values."
  (println "Running Carbon flow model.")
  (let [cache-layer       (make-matrix (get-rows source-layer) (get-cols source-layer) (constantly (atom ())))
        source-points     (filter-matrix-for-coords (p not= _0_) source-layer)
        use-points        (filter-matrix-for-coords (p not= _0_) use-layer)]
    (println "Source points:" (count source-points))
    (println "Use points:   " (count use-points))
    (if (and (seq source-points) (seq use-points))
      (let [source-values     (map (p get-in source-layer) source-points)
            use-values        (map (p get-in use-layer)    use-points)
            total-production  (reduce _+_ source-values)
            total-consumption (reduce _+_ use-values)
            percent-produced  (map #(_d_ % total-production) source-values)
            source-use-ratio  (_d_ total-production total-consumption)]
        (dorun (pmap
                (fn [uid use-cap]
                  (let [amount-usable (rv-min use-cap (_*_ use-cap source-use-ratio))]
                    (reset! (get-in cache-layer uid)
                            (map (fn [sid source-percent]
                                   (let [weight (_*_ source-percent amount-usable)]
                                     (struct-map service-carrier
                                       :source-id       sid
                                       :route           nil
                                       :possible-weight weight
                                       :actual-weight   weight
                                       :sink-effects    nil)))
                                 source-points
                                 percent-produced))))
                use-points
                use-values))))
    (map-matrix (& seq deref) cache-layer)))
