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

(ns clj-span.models.proximity
  (:use [clj-misc.utils      :only (def- p my->> mapmap euclidean-distance with-progress-bar-cool with-message remove-nil-val-entries)]
        [clj-span.params     :only (*trans-threshold*)]
        [clj-span.core       :only (distribute-flow! service-carrier)]
        [clj-misc.randvars   :only (_0_ _+_ _* _>_ rv-fn rv-above?)]
        [clj-misc.matrix-ops :only (get-neighbors get-line-fn find-bounding-box)]))

;; in meters
(def- half-mile    805.0)
(def- mile        1610.0)

(def- fast-source-decay (get-line-fn {:slope (/ -0.75 half-mile) :intercept 1.0}))
(def- slow-source-decay (get-line-fn {:slope (/ -0.25 half-mile) :intercept 0.5}))

;; source decay = fast decay to 1/2 mile, slow decay to 1 mile, gone after 1 mile
(defn- source-decay
  [distance]
  (cond (> distance mile)
        0.0

        (< distance half-mile)
        (fast-source-decay distance)

        :otherwise
        (slow-source-decay distance)))

(defn- distance-decay
  [to-meters A B]
  (let [distance (euclidean-distance (to-meters A) (to-meters B))]
    (source-decay distance)))

(defn- store-carrier!
  "If the location is a use point, a service-carrier is stored in its
   carrier-cache containing the highest utility route that
   expand-frontier found from the source-point to this one."
  [use-layer cache-layer possible-flow-layer actual-flow-layer
   [boundary-id {:keys [source-id route possible-weight actual-weight sink-effects decay-value]}]]
  (if (not= _0_ (get-in use-layer boundary-id))
    (let [decayed-pweight (_* possible-weight decay-value)
          decayed-aweight (_* actual-weight   decay-value)
          decayed-carrier (struct-map service-carrier
                            :source-id       source-id
                            :route           nil
                            :possible-weight decayed-pweight
                            :actual-weight   decayed-aweight
                            :sink-effects    (mapmap identity #(_* % decay-value) sink-effects))]
      (dosync
       (doseq [id route]
         (alter (get-in possible-flow-layer id) _+_ decayed-pweight)
         (if (not= _0_ decayed-aweight)
           (alter (get-in actual-flow-layer id) _+_ decayed-aweight)))
       (alter (get-in cache-layer boundary-id) conj decayed-carrier)))))

(defn- progress-carrier
  "If the location is a sink, its id and sink-value are saved on the
   sink-effects map and its sink-value is subtracted from the current
   utility along this path.  Whether a sink or not, the current
   location's id is appended to the route."
  [boundary-id sink-layer to-meters {:keys [source-id route possible-weight actual-weight sink-effects] :as prev-carrier}]
  (let [decay-value (distance-decay to-meters source-id boundary-id)]
    (if (rv-above? (_* possible-weight decay-value) *trans-threshold*)
      (let [sink-value (get-in sink-layer boundary-id)]
        (if (or (= _0_ sink-value)
                (= _0_ actual-weight))
          (assoc prev-carrier
            :decay-value decay-value
            :route       (conj route boundary-id))
          (assoc prev-carrier
            :decay-value   decay-value
            :route         (conj route boundary-id)
            :actual-weight (rv-fn (fn [a s] (max 0.0 (- a s))) actual-weight sink-value)
            :sink-effects  (conj sink-effects [boundary-id (rv-fn (fn [a s] (min a s)) actual-weight sink-value)])))))))

(defn- expand-frontier
  "Returns a new frontier which surrounds the one passed in and
   contains only those frontier-options which have a decayed
   possible-weight greater than *trans-threshold*. A frontier is here
   defined to be a map of location ids [i j] to service-carrier
   structs."
  [sink-layer to-meters rows cols frontier]
  (into {}
        (for [boundary-id (find-bounding-box rows cols (keys frontier))]
          [boundary-id
           (if-let [frontier-options (my->> boundary-id
                                            (get-neighbors rows cols)
                                            (map frontier)
                                            (remove nil?)
                                            seq)]
             (if-let [best-path-carrier (my->> frontier-options
                                               (reduce (fn [c1 c2] (if (_>_ (:actual-weight c1) (:actual-weight c2)) c1 c2)))
                                               (progress-carrier boundary-id sink-layer to-meters))]
               best-path-carrier))])))

(defn- distribute-gaussian!
  "Creates a service-carrier struct for the source location and then
   expands the frontier in all directions until no further utility can
   be distributed (due to distance decay).  Stores a service-carrier
   in every use location it encounters."
  [source-layer sink-layer use-layer cache-layer possible-flow-layer
   actual-flow-layer to-meters rows cols source-id]
  (let [source-value (get-in source-layer source-id)]
    (doseq [frontier (take-while seq
                                 (map remove-nil-val-entries
                                      (iterate (p expand-frontier sink-layer to-meters rows cols)
                                               {source-id (progress-carrier source-id
                                                                            sink-layer
                                                                            to-meters
                                                                            (struct-map service-carrier
                                                                              :source-id       source-id
                                                                              :route           []
                                                                              :possible-weight source-value
                                                                              :actual-weight   source-value
                                                                              :sink-effects    {}))})))]
      (doseq [frontier-element frontier]
        (store-carrier! use-layer
                        cache-layer
                        possible-flow-layer
                        actual-flow-layer
                        frontier-element)))))

(defmethod distribute-flow! "Proximity"
  [_ cell-width cell-height rows cols cache-layer possible-flow-layer
   actual-flow-layer source-layer sink-layer use-layer source-points _ _ _]
  (let [to-meters (fn [[i j]] [(* i cell-height) (* j cell-width)])]
    (with-message (str "Projecting " (count source-points) " search bubbles...\n") "\nAll done."
      (with-progress-bar-cool
        :drop
        (count source-points)
        (pmap (p distribute-gaussian!
                 source-layer
                 sink-layer
                 use-layer
                 cache-layer
                 possible-flow-layer
                 actual-flow-layer
                 to-meters
                 rows
                 cols)
              source-points)))))
