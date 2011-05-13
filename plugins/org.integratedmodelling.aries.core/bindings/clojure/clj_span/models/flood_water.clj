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
;;; This namespace defines the flood-water model.
;;;

(ns clj-span.models.flood-water
  (:use [clj-span.core       :only (distribute-flow! service-carrier)]
        [clj-misc.utils      :only (seq2map mapmap iterate-while-seq with-message
                                    memoize-by-first-arg angular-distance p def-
                                    with-progress-bar-cool euclidean-distance)]
        [clj-misc.matrix-ops :only (get-neighbors on-bounds? add-ids subtract-ids find-nearest
                                    find-line-between rotate-2d-vec find-point-at-dist-in-m)]
        [clj-misc.randvars   :only (_0_ _+_ *_ _d rv-fn rv-min)]))

(defn- lowest-neighbors
  [id in-stream? elevation-layer rows cols]
  (if-not (on-bounds? rows cols id)
    (let [neighbors      (if (in-stream? id)
                           ;; Step downstream
                           (filter in-stream? (get-neighbors rows cols id))
                           ;; Step downhill
                           (get-neighbors rows cols id))
          local-elev     (get-in elevation-layer id)
          neighbor-elevs (map (p get-in elevation-layer) neighbors)
          min-elev       (reduce rv-min local-elev neighbor-elevs)]
      (filter #(= min-elev (get-in elevation-layer %)) neighbors))))
(def- lowest-neighbors (memoize-by-first-arg lowest-neighbors))

(defn- nearest-to-bearing
  [bearing id neighbors]
  (if (seq neighbors)
    (if bearing
      (let [bearing-changes (seq2map neighbors
                                     #(let [bearing-to-neighbor (subtract-ids % id)]
                                        [(angular-distance bearing bearing-to-neighbor)
                                         %]))]
        (bearing-changes (apply min (keys bearing-changes))))
      (first neighbors))))

;; FIXME: Somehow this still doesn't terminate correctly for some carriers.
(defn- find-next-step
  [id in-stream? elevation-layer rows cols bearing]
  (let [prev-id (if bearing (subtract-ids id bearing))]
    (nearest-to-bearing bearing
                        id
                        (remove (p = prev-id)
                                (lowest-neighbors id
                                                  in-stream?
                                                  elevation-layer
                                                  rows
                                                  cols)))))
(def- find-next-step (memoize find-next-step))

(defn- handle-use-effects!
  "Place a copy of the flood-carrier in each affected user's
   carrier-cache.  Users only benefit from upstream sinks (so we're
   not using new-actual-weight and new-sink-effects here)."
  [current-id use-stream-intakes use-AFs cache-layer mm2-per-cell
   {:keys [possible-weight actual-weight sink-effects] :as flood-water-carrier}]
  (if-let [use-id (use-stream-intakes current-id)]
    (let [use-AF (use-AFs use-id)
          scale-factor (/ use-AF mm2-per-cell)]
      (dosync (alter (get-in cache-layer use-id) conj
                     (assoc flood-water-carrier
                       :route           nil
                       :possible-weight (*_ scale-factor possible-weight)
                       :actual-weight   (*_ scale-factor actual-weight)
                       :sink-effects    (mapmap identity #(*_ scale-factor %) sink-effects)))))))

;; FIXME: Must merge sink-AFs with sink-caps or our math is wrong.
(defn- handle-sink-effects!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id stream-bound? sink-stream-intakes sink-AFs actual-weight sink-caps]
  (if (= _0_ actual-weight)
    ;; Skip all computations, since there's no water left in this
    ;; carrier anyway.
    [actual-weight {}]
    (if stream-bound?
      ;; We're in the stream. Spread the collected source weights
      ;; latitudinally among all sinks in the floodplain. Activation
      ;; factors must be applied to the sinks before they are used.
      (if-let [affected-sink (sink-stream-intakes current-id)]
        (let [sink-cap-ref (sink-caps affected-sink)
              sink-AF      (sink-AFs  affected-sink)]
          (dosync
           (let [sink-cap (*_ sink-AF (deref sink-cap-ref))]
             (if (= _0_ sink-cap)
               [actual-weight {}]
               (do
                 (alter sink-cap-ref (p rv-fn (fn [a s] (- s (min a (* sink-AF s)))) actual-weight))
                 [(rv-fn (fn [a s] (max (- a s) 0.0)) actual-weight sink-cap)
                  {affected-sink (rv-fn (fn [a s] (min a s)) actual-weight sink-cap)}])))))
        [actual-weight {}])
      ;; Not in the stream. Only one source weight and one sink. Activation factors don't matter.
      (if-let [sink-cap-ref (sink-caps current-id)]
        (dosync
         (let [sink-cap (deref sink-cap-ref)]
           (if (= _0_ sink-cap)
             [actual-weight {}]
             (do
               (alter sink-cap-ref (p rv-fn (fn [a s] (max (- s a) 0.0)) actual-weight))
               [(rv-fn (fn [a s] (max (- a s) 0.0)) actual-weight sink-cap)
                {current-id (rv-fn (fn [a s] (min a s)) actual-weight sink-cap)}]))))
        [actual-weight {}]))))

(def- *max-levee-distance* 100.0) ;; in meters

(defn- nearby-levees
  [origin bearing levee? cell-width cell-height]
  (let [left-dir     (rotate-2d-vec (/ Math/PI 2.0) bearing)
        right-dir    (map - left-dir)
        left-bounds  (find-point-at-dist-in-m origin left-dir  *max-levee-distance* cell-width cell-height)
        right-bounds (find-point-at-dist-in-m origin right-dir *max-levee-distance* cell-width cell-height)]
    (seq (filter levee? (find-line-between left-bounds right-bounds)))))
(def- nearby-levees (memoize-by-first-arg nearby-levees))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
;; FIXME: Add possible-weight and actual-weight to the entire
;; latitudinal floodplain stripe around a stream-bound carrier's
;; location.
(defn- to-the-ocean!
  "Computes the state of the flood-water-carrier after it takes
   another step downhill.  If it encounters a sink location, it drops
   some water according to the remaining sink capacity at this
   location.  If it encounters a use location, a service-carrier is
   stored in the user's carrier-cache."
  [cache-layer possible-flow-layer actual-flow-layer mm2-per-cell
   sink-caps levee? in-stream? sink-stream-intakes use-stream-intakes
   sink-AFs use-AFs elevation-layer cell-width cell-height rows cols
   {:keys [route possible-weight actual-weight sink-effects stream-bound?] :as flood-water-carrier}]
  (let [current-id (peek route)
        prev-id    (peek (pop route))
        bearing    (if prev-id (subtract-ids current-id prev-id))]
    (dosync
     (alter (get-in possible-flow-layer current-id) _+_ (_d possible-weight mm2-per-cell))
     (alter (get-in actual-flow-layer   current-id) _+_ (_d actual-weight   mm2-per-cell)))
    (if (and stream-bound? bearing (nearby-levees current-id bearing levee? cell-width cell-height))
      ;; Levees channel the water, so floodplain sinks and users will
      ;; not be affected.
      (if-let [next-id (find-next-step current-id in-stream? elevation-layer rows cols bearing)]
        (assoc flood-water-carrier
          :route         (conj route next-id)
          :stream-bound? (in-stream? next-id)))
      ;; Either we're over-land or there are no levees nearby, so we
      ;; may proceed with the local checks.
      (let [[new-actual-weight new-sink-effects] (handle-sink-effects! current-id
                                                                       stream-bound?
                                                                       sink-stream-intakes
                                                                       sink-AFs
                                                                       actual-weight
                                                                       sink-caps)
            post-sink-carrier (assoc flood-water-carrier
                                :actual-weight new-actual-weight
                                :sink-effects  (merge-with _+_ sink-effects new-sink-effects))]
        (if stream-bound?
          (handle-use-effects! current-id
                               use-stream-intakes
                               use-AFs
                               cache-layer
                               mm2-per-cell
                               post-sink-carrier))
        (if-let [next-id (find-next-step current-id in-stream? elevation-layer rows cols bearing)]
          (assoc post-sink-carrier
            :route           (conj route next-id)
            :stream-bound?   (in-stream? next-id)))))))

(defn- stop-unless-reducing
  [n coll]
  (take-while (fn [[p c]] (> p c)) (partition 2 1 (map count (take-nth n coll)))))

(defn- propagate-runoff!
  "Constructs a sequence of surface-water-carrier objects (one per
   source point) and then iteratively propagates them downhill until
   they reach a stream location, get stuck in a low elevation point,
   or fall off the map bounds.  Once they reach a stream location, the
   carriers will attempt to continue downhill while staying in a
   stream course.  Sinks affect carriers overland.  Users affect
   carriers in stream channels.  All the carriers are moved together
   in timesteps (more or less)."
  [cache-layer possible-flow-layer actual-flow-layer source-layer source-points
   mm2-per-cell sink-caps levee? in-stream? sink-stream-intakes use-stream-intakes
   sink-AFs use-AFs elevation-layer cell-width cell-height rows cols]
  (with-message "Moving the flood water carriers downhill and downstream...\n" "All done."
    (dorun
     (stop-unless-reducing
      100
      (iterate-while-seq
       (fn [flood-water-carriers]
         (let [on-land-carriers   (count (remove :stream-bound? flood-water-carriers))
               in-stream-carriers (- (count flood-water-carriers) on-land-carriers)]
           (printf "Carriers: %10d | On Land: %10d | In Stream: %10d%n"
                   (+ on-land-carriers in-stream-carriers)
                   on-land-carriers
                   in-stream-carriers)
           (flush)
           (pmap (p to-the-ocean!
                    cache-layer
                    possible-flow-layer
                    actual-flow-layer
                    mm2-per-cell
                    sink-caps
                    levee?
                    in-stream?
                    sink-stream-intakes
                    use-stream-intakes
                    sink-AFs
                    use-AFs
                    elevation-layer
                    cell-width
                    cell-height
                    rows
                    cols)
                 flood-water-carriers)))
       (map
        #(let [source-weight (*_ mm2-per-cell (get-in source-layer %))]
           (struct-map service-carrier
             :source-id       %
             :route           [%]
             :possible-weight source-weight
             :actual-weight   source-weight
             :sink-effects    {}
             :stream-bound?   (in-stream? %)))
        (remove on-bounds? source-points)))))))

(defn- make-buckets
  [mm2-per-cell layer active-points]
  (seq2map active-points (fn [id] [id (ref (*_ mm2-per-cell (get-in layer id)))])))

;; FIXME: Should we be considering the elevation of our data-point?
(defn- flood-activation-factors
  "Returns a map of each data-id (e.g. a sink or use location) to a
   number between 0.0 and 1.0, representing its relative position
   between the stream edge (1.0) and the floodplain boundary (0.0)."
  [in-floodplain? in-stream-map]
  (with-message "Computing flood activation factors...\n" "\nAll done."
    (into {}
          (with-progress-bar-cool
            :keep
            (count in-stream-map)
            (for [[in-stream-id data-id] in-stream-map]
              (if (= in-stream-id data-id)
                ;; location is already in-stream, activation is 100%
                [data-id 1.0]
                ;; location is out-of-stream, activation is scaled
                ;; by the relative distance between this location,
                ;; the in-stream proxy location, and the nearest
                ;; floodplain boundary
                (let [loc-delta       (subtract-ids data-id in-stream-id)
                      inside-id       (last (take-while in-floodplain? (rest (iterate (p add-ids loc-delta) data-id))))
                      outside-id      (add-ids inside-id loc-delta)
                      boundary-id     (first (remove in-floodplain? (find-line-between inside-id outside-id)))
                      run-to-boundary (euclidean-distance in-stream-id boundary-id)
                      run-to-data     (euclidean-distance in-stream-id data-id)]
                  [data-id (- 1.0 (/ run-to-data run-to-boundary))])))))))

(defn- find-nearest-stream-point!
  [in-stream? in-stream-map rows cols id]
  (dosync
   (let [available-id? (complement @in-stream-map)
         stream-point  (find-nearest #(and (in-stream? %) (available-id? %)) rows cols id)]
     (if stream-point (alter in-stream-map conj [stream-point id])))))

(defn- find-nearest-stream-points
  [in-stream? rows cols ids]
  (with-message
    "Finding nearest stream points..."
    #(str "done. [Shifted " (count %) " ids]")
    (let [in-stream-ids (filter in-stream? ids)
          in-stream-map (ref (zipmap in-stream-ids in-stream-ids))]
      (dorun
       (pmap (p find-nearest-stream-point! in-stream? in-stream-map rows cols)
             (remove in-stream? ids)))
      @in-stream-map)))

(defmethod distribute-flow! "FloodWaterMovement"
  [_ cell-width cell-height rows cols cache-layer possible-flow-layer actual-flow-layer
   source-layer sink-layer _ source-points sink-points use-points
   {stream-layer "River", elevation-layer "Altitude", levees-layer "Levees",
    floodplain-layer100 "Floodplains100", floodplain-layer500 "Floodplains500"}]
  (println "Operating in" (if floodplain-layer500 "500" "100") "year floodplain.")
  (let [floodplain-layer    (or floodplain-layer500 floodplain-layer100)
        levee?              (memoize #(not= _0_ (get-in levees-layer     %)))
        in-stream?          (memoize #(not= _0_ (get-in stream-layer     %)))
        in-floodplain?      (memoize #(not= _0_ (get-in floodplain-layer %)))
        floodplain-sinks    (filter in-floodplain? sink-points)
        floodplain-users    (filter in-floodplain? use-points)
        sink-stream-intakes (find-nearest-stream-points in-stream? rows cols floodplain-sinks)
        use-stream-intakes  (find-nearest-stream-points in-stream? rows cols floodplain-users)
        sink-AFs            (flood-activation-factors in-floodplain? sink-stream-intakes)
        use-AFs             (flood-activation-factors in-floodplain? use-stream-intakes)
        mm2-per-cell        (* cell-width cell-height (Math/pow 10.0 6.0))
        sink-caps           (make-buckets mm2-per-cell sink-layer sink-points)]
    (propagate-runoff! cache-layer
                       possible-flow-layer
                       actual-flow-layer
                       source-layer
                       source-points
                       mm2-per-cell
                       sink-caps
                       levee?
                       in-stream?
                       sink-stream-intakes
                       use-stream-intakes
                       sink-AFs
                       use-AFs
                       elevation-layer
                       cell-width
                       cell-height
                       rows
                       cols)))
