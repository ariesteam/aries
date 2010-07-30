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
;;; * Routes run from Source to Use
;;;
;;; * Contain positive utility values (total source - sink effects)
;;;
;;; * Searches outward from Source points until decay and sink effects
;;;   block the frontier's progress

(ns clj-span.proximity-model
  (:use [clj-misc.utils      :only (p & my->>)]
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

(defstruct frontier-option :utility :route :sink-effects)

(defn- make-frontier-option
  "If the location is a sink, its id and sink-value are saved on the
   sink-effects map and its sink-value is subtracted from the current
   utility along this path.  Whether a sink or not, the current
   location's id is appended to the route."
  [location-id sink-layer {:keys [utility route sink-effects] :as last-frontier-option}]
  (let [sink-value (get-in sink-layer location-id)]
    (if (not= _0_ sink-value)
      (struct-map frontier-option
        :utility      (rv-pos (_-_ utility sink-value))
        :route        (conj route location-id)
        :sink-effects (conj sink-effects [location-id sink-value]))
      (update-in last-frontier-option [:route] conj location-id))))

(defn- too-little-utility?
  "Returns true if the rv-mean of this frontier-option's
   distance-decayed utility is less than *trans-threshold*."
  [flow-model {:keys [utility route]}]
  (< (rv-mean (decay flow-model utility (dec (count route))))
     *trans-threshold*))

(defn- expand-frontier
  "Returns a new frontier which surrounds the one passed in and
   contains only those frontier-options which have a decayed utility
   greater than *trans-threshold*. A frontier is here defined to be a
   map of location ids [i j] to frontier-option structs."
  [flow-model sink-layer rows cols frontier]
  (my->> (for [boundary-id (find-bounding-box (keys frontier) rows cols)]
           (when-let [frontier-options (my->> boundary-id
                                              (get-neighbors rows cols)
                                              (map frontier)
                                              (remove nil?)
                                              seq)]
             (my->> frontier-options
                    (apply max-key (& rv-mean :utility))
                    (make-frontier-option boundary-id sink-layer)
                    (array-map boundary-id))))
         (remove #(or (nil? %) (too-little-utility? flow-model (val %))))
         (apply merge)))

(defn- store-carrier!
  "If the location is a use point, a service-carrier is stored in its
   carrier-cache containing the highest utility route that
   expand-frontier found from the source-point to this one."
  [flow-model cache-layer source-layer use-layer
   [location-id {:keys [utility route sink-effects]}]]
  (when (not= _0_ (get-in use-layer location-id))
    (let [carrier-cache      (get-in cache-layer location-id)
          source-id          (first route)
          steps              (dec (count route))
          possible-weight    (decay flow-model (get-in source-layer source-id) steps)
          actual-weight      (decay flow-model utility steps)
          last-actual-weight (:actual-weight (@carrier-cache source-id))]
      (if (or (nil? last-actual-weight) (_>_ actual-weight last-actual-weight))
        (swap! carrier-cache assoc source-id
               (struct-map service-carrier
                 :source-id       source-id
                 :route           (bitpack-route route)
                 :possible-weight possible-weight
                 :actual-weight   actual-weight
                 :sink-effects    sink-effects))))))

(defn- distribute-gaussian!
  "Creates a frontier struct for the source location and then expands
   the frontier in all directions until no further utility can be
   distributed (due to distance decay or sinks).  Stores a
   service-carrier in every use location it encounters."
  [flow-model cache-layer source-layer sink-layer use-layer rows cols source-id]
  (doseq [frontier (take-while seq (iterate (p expand-frontier flow-model sink-layer rows cols)
                                            {source-id (make-frontier-option source-id
                                                                             sink-layer
                                                                             (struct-map frontier-option
                                                                               :utility      (get-in source-layer source-id)
                                                                               :route        []
                                                                               :sink-effects {}))}))]
    (dorun (map (p store-carrier! flow-model cache-layer source-layer use-layer) frontier))))

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
            source-points))
    (map-matrix (& vals deref) cache-layer)))
