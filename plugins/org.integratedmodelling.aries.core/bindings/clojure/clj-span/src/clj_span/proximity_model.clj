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
;;; This namespace defines the proximity model.
;;;
;;; * Routes run from Source to Use and Sink to Use
;;; * Contain positive and negative utility values (total source +
;;;   sink values)
;;; * Searches outward from Source points until decay and sink effects
;;;   block the frontier's progress

(ns clj-span.proximity-model
  (:use [clj-misc.utils      :only (seq2map p &)]
        [clj-span.params     :only (*trans-threshold*)]
        [clj-span.model-api  :only (distribute-flow decay undecay service-carrier)]
        [clj-misc.randvars   :only (_0_ _-_ _* _d _>_ rv-pos rv-mean)]
        [clj-misc.matrix-ops :only (get-neighbors
                                    make-matrix
                                    map-matrix
                                    get-rows
                                    get-cols
                                    filter-matrix-for-coords
                                    bitpack-route
                                    find-bounding-box)]))

;; FIXME convert step to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod decay "Proximity"
  [_ weight step] (if (> step 1) (_d weight (* step step)) weight))

;; FIXME convert step to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod undecay "Proximity"
  [_ weight step] (if (> step 1) (_* weight (* step step)) weight))

(defn- make-frontier-element!
  "If the location is a sink, its id is saved on the sinks-encountered
   list and its sink-value is subtracted from the current utility
   along this path.  If the location is a use point, negative utility
   carriers are stored on it from each of the previously encountered
   sinks as well as a positive utility carrier from the original
   source point. This function returns a pair of [location-id
   [outgoing-utility route-including-location-id sinks-encountered]]."
  [location-id flow-model cache-layer source-layer sink-layer
   use-layer [incoming-utility exclusive-route sinks-encountered]]
  (let [sink-value        (get-in sink-layer location-id)
        use-value         (get-in use-layer  location-id)
        inclusive-route   (conj exclusive-route location-id)
        outgoing-utility  (if (not= _0_ sink-value) ; sink-location?
                            (rv-pos (_-_ incoming-utility sink-value))
                            incoming-utility)
        sinks-encountered (if (not= _0_ sink-value) ; sink location?
                            (conj sinks-encountered location-id)
                            sinks-encountered)]
    (when (not= _0_ use-value)  ; use location?
      (let [carrier-cache      (get-in cache-layer location-id)
            source-id          (first inclusive-route)
            steps              (dec (count inclusive-route))
            possible-weight    (decay flow-model (get-in source-layer source-id) steps)
            actual-weight      (decay flow-model outgoing-utility steps)
            last-actual-weight (:actual-weight (@carrier-cache source-id))]
        (if (or (nil? last-actual-weight) (_>_ actual-weight last-actual-weight))
          (swap! carrier-cache assoc source-id
                 (struct-map service-carrier
                   :source-id       source-id
                   :route           (bitpack-route inclusive-route)
                   :possible-weight possible-weight
                   :actual-weight   actual-weight
                   :sink-effects    (seq2map sinks-encountered #(vector % (get-in sink-layer %))))))))
    [location-id [outgoing-utility inclusive-route sinks-encountered]]))

(defn- distribute-gaussian!
  [flow-model cache-layer source-layer sink-layer use-layer rows cols source-id source-weight]
  (loop [frontier (into {} (list (make-frontier-element! source-id
                                                         flow-model
                                                         cache-layer
                                                         source-layer
                                                         sink-layer
                                                         use-layer
                                                         [source-weight [] []])))]
    (when (seq frontier)
      (recur (into {}
                   (remove #(or (nil? %)
                                (let [[_ [u r _]] %]
                                  (< (rv-mean (decay flow-model u (dec (count r))))
                                     *trans-threshold*)))
                           (for [boundary-id (find-bounding-box (keys frontier) rows cols)]
                             (when-let [frontier-options (seq (remove nil? (map frontier (get-neighbors boundary-id rows cols))))]
                               (make-frontier-element! boundary-id flow-model cache-layer source-layer sink-layer use-layer
                                                       (apply max-key (fn [[u r s]] (rv-mean u)) frontier-options))))))))))

(defmethod distribute-flow "Proximity"
  [flow-model source-layer sink-layer use-layer _]
  (println "Running Proximity flow model.")
  (let [rows          (get-rows source-layer)
        cols          (get-cols source-layer)
        cache-layer   (make-matrix rows cols (constantly (atom {})))
        source-points (filter-matrix-for-coords (p not= _0_) source-layer)]
    (println "Source points:" (count source-points))
    (dorun (pmap
            (p distribute-gaussian! flow-model cache-layer source-layer sink-layer use-layer rows cols)
            source-points
            (map (p get-in source-layer) source-points)))
    (map-matrix (& vals deref) cache-layer)))
