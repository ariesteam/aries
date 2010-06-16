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
;;; This namespace defines the line-of-sight model.
;;;
;;; * Routes run from Source to Use
;;; * Contain positive and negative utility values (total decayed
;;;   source and sink values)
;;; * Projects a line of sight from each user to source point
;;; * Weights utility by amount of visible height of source above view
;;;   line
;;; * Sinks are counted independently of source sight lines

(ns clj-span.line-of-sight-model
  (:use [clj-span.params     :only (*trans-threshold*)]
        [clj-misc.utils      :only (euclidean-distance p &)]
        [clj-misc.matrix-ops :only (make-matrix
                                    map-matrix
                                    get-rows
                                    get-cols
                                    filter-matrix-for-coords
                                    bitpack-route
                                    find-line-between)]
        [clj-misc.randvars   :only (_0_ _+_ _-_ _*_ _d_ _* _d -_ rv-mean rv-max rv-pos)]
        [clj-span.model-api  :only (distribute-flow decay undecay service-carrier)]))

;; FIXME convert step to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod decay "LineOfSight"
  [_ weight distance] (if (> distance 1) (_d weight (* distance distance)) weight))

;; FIXME convert distance to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod undecay "LineOfSight"
  [_ weight distance] (if (> distance 1) (_* weight (* distance distance)) weight))

;; sink decay = slow decay to 1/2 mile, fast decay to 1 mile, gone after 1 mile

(defn- compute-view-impact
  [flow-model utility slope distance elev use-elev]
  (let [projected-elev   (rv-pos (_+_ use-elev (_* slope distance)))
        visible-fraction (rv-pos (-_ 1 (_d_ projected-elev elev)))]
    (decay flow-model (_*_ utility visible-fraction) distance)))

(defn- raycast!
  "Finds a line of sight path between source and use points, checks
   for obstructions, and determines (using elevation info) how much of
   the source element can be seen from the use point.  A distance
   decay function is applied to the results to compute the visual
   utility originating from the source point."
  [flow-model cache-layer source-layer sink-layer elev-layer [source-point use-point]]
  (if-let [carrier
           (when (not= source-point use-point) ;; no in-situ use
             (let [sight-line     (rest (find-line-between use-point source-point))
                   use-elev       (get-in elev-layer use-point)
                   elevs          (map (p get-in elev-layer) sight-line)
                   rises          (map #(_-_ % use-elev) elevs)
                   runs           (map (p euclidean-distance use-point) sight-line)
                   slopes         (butlast (map _d rises runs))
                   sight-slopes   (cons _0_
                                        (reduce #(conj %1 (rv-max (peek %1) %2))
                                                [(first slopes)]
                                                (rest slopes)))
                   source-utility (compute-view-impact flow-model
                                                       (get-in source-layer source-point)
                                                       (last sight-slopes)
                                                       (last runs)
                                                       (last elevs)
                                                       use-elev)]
               (when (> (rv-mean source-utility) *trans-threshold*)
                 (let [sink-effects (into {}
                                          (map #(let [sink-value (get-in sink-layer %1)]
                                                  (if (not= sink-value _0_)
                                                    (vector %1 (compute-view-impact flow-model sink-value %2 %3 %4 use-elev))))
                                               sight-line
                                               sight-slopes
                                               runs
                                               elevs))]
                   (struct-map service-carrier
                     :source-id       source-point
                     :route           (bitpack-route (reverse (cons use-point sight-line)))
                     :possible-weight source-utility
                     :actual-weight   (rv-pos (reduce _-_ source-utility (vals sink-effects)))
                     :sink-effects    sink-effects)))))]
    (swap! (get-in cache-layer use-point) conj carrier)))

;; Detects all sources and sinks visible from the use-point and stores
;; their utility contributions in the cache-layer."
(defmethod distribute-flow "LineOfSight"
  [flow-model source-layer sink-layer use-layer {elev-layer "Altitude"}]
  (println "Running LineOfSight flow model.")
  (let [cache-layer   (make-matrix (get-rows source-layer) (get-cols source-layer) (constantly (atom ())))
        source-points (filter-matrix-for-coords (p not= _0_) source-layer)
        use-points    (filter-matrix-for-coords (p not= _0_) use-layer)]
    (println "Source points:" (count source-points))
    (println "Use points:   " (count use-points))
    (dorun (pmap (p raycast! flow-model cache-layer source-layer sink-layer elev-layer)
                 (for [source-point source-points use-point use-points] [source-point use-point])))
    (map-matrix (& seq deref) cache-layer)))
