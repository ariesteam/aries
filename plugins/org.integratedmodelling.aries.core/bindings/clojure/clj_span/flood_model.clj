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
;;; This namespace defines the flood model.
;;;
;;; FIXME:
;;;  1) optimize movement downstream after source goes to 0
;;;  2) find a smarter way to compute the out of stream case in handle-sink-effects!

(ns clj-span.flood-model
  (:use [clj-span.model-api      :only (distribute-flow service-carrier)]
        [clj-misc.utils          :only (seq2map seq2redundant-map euclidean-distance p & my->>)]
        [clj-misc.matrix-ops     :only (get-rows get-cols make-matrix filter-matrix-for-coords
                                        in-bounds? find-line-between map-matrix bitpack-route)]
        [clj-misc.randvars       :only (_0_ _+_ _-_ _* *_ _d rv-lt? rv-convolutions rv-resample)]
        [clj-span.sediment-model :only (hydrosheds-delta-codes)]))

(defn- move-points-into-stream-channel
  "Returns a map of in-stream-ids to lists of the out-of-stream-ids
   that were shifted into this position.  If the location's downstream
   path leads it beyond the map bounds or into the ocean or an inland
   sink prior to reaching a stream, then it will be left out of the
   returned results."
  [step-downstream in-stream? data-points]
  (dissoc (seq2redundant-map data-points
                             (fn [id] [(my->> id
                                              (iterate step-downstream)
                                              (take-while (& not nil?))
                                              (filter in-stream?)
                                              first
                                              vec)
                                       id])
                             conj)
          []))

(defn- flood-activation-factors-complicated
  "Returns a map of each data-id (e.g. a sink or use location) to a
   number between 0.0 and 1.0, representing its relative position
   between the stream edge (1.0) and the floodplain boundary (0.0).
   Any data-ids whose elevations are above the projected elevation
   from the stream to the floodplain boundary are left out of this map
   and should be considered to have an activation-factor of 0.0."
  [in-floodplain? elevation-layer in-stream-map]
  (into {}
        (remove nil?
                (for [in-stream-id (keys in-stream-map) data-id (in-stream-map in-stream-id)]
                  (if (= in-stream-id data-id)
                    ;; location is already in-stream, activation is 100%
                    [data-id 1.0]
                    ;; location is out-of-stream, activation is scaled by the
                    ;; relative elevation difference between this location,
                    ;; the in-stream proxy location, and the nearest
                    ;; floodplain boundary
                    (let [loc-delta       (map - data-id in-stream-id)
                          outside-id      (first (filter (& not in-floodplain?)
                                                         (rest (iterate (p map + loc-delta) data-id))))
                          inside-id       (map - outside-id loc-delta)
                          boundary-id     (first (filter (& not in-floodplain?)
                                                         (find-line-between inside-id outside-id)))
                          rise            (_-_ (get-in elevation-layer boundary-id)
                                               (get-in elevation-layer in-stream-id))
                          run-to-boundary (euclidean-distance in-stream-id boundary-id)
                          run-to-data     (euclidean-distance in-stream-id data-id)
                          slope           (_d rise run-to-boundary)
                          elev-limit      (_+_ (_* slope run-to-data) (get-in elevation-layer in-stream-id))
                          elev-at-loc     (get-in elevation-layer data-id)]
                      (if (rv-lt? elev-at-loc elev-limit)
                        [data-id (- 1.0 (/ run-to-data run-to-boundary))])))))))

(defn- flood-activation-factors
  "Returns a map of each data-id (e.g. a sink or use location) to a
   number between 0.0 and 1.0, representing its relative position
   between the stream edge (1.0) and the floodplain boundary (0.0)."
  [in-floodplain? elevation-layer in-stream-map]
  (into {}
        (for [in-stream-id (keys in-stream-map) data-id (in-stream-map in-stream-id)]
          (if (= in-stream-id data-id)
            ;; location is already in-stream, activation is 100%
            [data-id 1.0]
            ;; location is out-of-stream, activation is scaled
            ;; by the relative distance between this location,
            ;; the in-stream proxy location, and the nearest
            ;; floodplain boundary
            (let [loc-delta       (map - data-id in-stream-id)
                  outside-id      (first (filter (& not in-floodplain?)
                                                 (rest (iterate (p map + loc-delta) data-id))))
                  inside-id       (map - outside-id loc-delta)
                  boundary-id     (first (filter (& not in-floodplain?)
                                                 (find-line-between inside-id outside-id)))
                  run-to-boundary (euclidean-distance in-stream-id boundary-id)
                  run-to-data     (euclidean-distance in-stream-id data-id)]
              [data-id (- 1.0 (/ run-to-data run-to-boundary))])))))

(defn- handle-sink-effects!
  "Computes the amount sunk by each sink encountered either along an
   out-of-stream flow path or within a floodplain if the carrier is
   moving down a stream. Reduces the sink-caps for each sink which
   captures some of the service medium. The sunk amount is based on
   the relative capacity of each sink with respect to the others
   encountered at the same time. Returns the actual-weight minus the
   amount sunk and a map of the sink-ids to the amount each sinks."
  [stream-bound? sink-map unsaturated-sink? sink-caps sink-AFs current-id actual-weight]
   (if stream-bound?
     (dosync
      (if-let [affected-sinks (seq (filter unsaturated-sink? (sink-map current-id)))]
        (let [convolutions         (rv-convolutions actual-weight
                                                    (apply rv-convolutions
                                                           (map (& deref sink-caps) affected-sinks)))
              result-rv-type       (meta convolutions)
              sink-effects-results (for [[[x unscaled-k-states] pxk] convolutions]
                                     (let [k-states (map * unscaled-k-states (map sink-AFs affected-sinks))
                                           k        (apply + k-states)]
                                       (if (>= x k)
                                         ;; all the sinks become saturated
                                         [x k-states (map - unscaled-k-states k-states) pxk]
                                         ;; distribute the source weight among the sinks by their relative sink value
                                         (let [k-dist (map #(* x (/ % k)) k-states)]
                                           [x k-dist (map - unscaled-k-states k-dist) pxk]))))
              new-actual-weight    (rv-resample
                                    (with-meta
                                      (apply merge-with +
                                             (map (fn [[x k-sunk k-left pxk]] {(- x (apply + k-sunk)) pxk})
                                                  sink-effects-results))
                                      result-rv-type))
              new-sink-effects     (zipmap affected-sinks
                                           (apply map
                                                  #(rv-resample (with-meta (apply merge-with + %&) result-rv-type))
                                                  (for [[_ k-sunk _ pxk] sink-effects-results]
                                                    (map (fn [k] {k pxk}) k-sunk))))
              new-sink-caps        (zipmap affected-sinks
                                           (apply map
                                                  #(rv-resample (with-meta (apply merge-with + %&) result-rv-type))
                                                  (for [[_ _ k-left pxk] sink-effects-results]
                                                    (map (fn [k] {k pxk}) k-left))))]
          (doseq [sink-id affected-sinks]
            (alter (sink-caps sink-id) (constantly (new-sink-caps sink-id))))
          [new-actual-weight new-sink-effects])))
     (dosync
      (if-let [sink-cap-ref (sink-caps current-id)]
        (let [sink-cap              (deref sink-cap-ref)
              sink-AF               (sink-AFs current-id)
              scaled-convolutions   (rv-convolutions actual-weight (_* sink-cap sink-AF))
              unscaled-convolutions (rv-convolutions actual-weight sink-cap)
              result-rv-type        (meta unscaled-convolutions)
              new-actual-weight (apply merge-with + (map (fn [[[x k] pxk]] {(max (- x k) 0.0) pxk}) scaled-convolutions))
              new-sink-effects  (apply merge-with + (map (fn [[[x k] pxk]] {(min x k)         pxk}) scaled-convolutions))
              new-sink-cap      (apply merge-with + (map (fn [[[x k] pxk] [[_ k*] _]] {(- k* (min x k)) pxk})
                                                         scaled-convolutions
                                                         unscaled-convolutions))]
          (alter sink-cap-ref (constantly (rv-resample (with-meta new-sink-cap result-rv-type))))
          (map #(rv-resample (with-meta % result-rv-type)) [new-actual-weight new-sink-effects]))))))

(defn- step-downstream!
  "Computes the state of the flood-carrier after it takes another step
   downstream.  If it encounters a sink location, it drops some water
   according to the remaining sink capacity at this location.  If it
   encounters a use location, a service-carrier is stored in the
   user's carrier-cache."
  [cache-layer step-downstream in-stream? unsaturated-sink? sink-map use-map sink-AFs use-AFs sink-caps
   {:keys [route possible-weight actual-weight sink-effects stream-bound?] :as flood-carrier}]
  (let [current-id (peek route)]
    (if stream-bound?
      ;; Place a copy of the flood-carrier in each affected user's
      ;; carrier-cache.  Users only benefit from upstream sinks (so
      ;; we're not using new-actual-weight and new-sink-effects here).
      (if-let [affected-users (seq (use-map current-id))]
        (let [bitpacked-carrier (assoc flood-carrier :route (bitpack-route route))]
          (doseq [use-id affected-users]
            (let [use-AF (use-AFs use-id)]
              (swap! (get-in cache-layer use-id) conj
                     (assoc bitpacked-carrier
                       :possible-weight (*_ use-AF possible-weight)
                       :actual-weight   (*_ use-AF actual-weight))))))))
    ;; Compute the local sink-effects and the remaining
    ;; actual-weight.  new-actual-weight and new-sink-effects will
    ;; be nil if there are no sinks associated with this location.
    (let [[new-actual-weight new-sink-effects] (handle-sink-effects! stream-bound?
                                                                     sink-map
                                                                     unsaturated-sink?
                                                                     sink-caps
                                                                     sink-AFs
                                                                     current-id
                                                                     actual-weight)]
      ;; Continue until we run off the map, hit the ocean, or fall
      ;; down an endorheic sink.
      (if-let [new-id (step-downstream current-id)]
        (assoc flood-carrier
          :route         (conj route new-id)
          :actual-weight (or new-actual-weight actual-weight)
          :sink-effects  (merge sink-effects new-sink-effects)
          :stream-bound? (in-stream? new-id))))))

(defn- distribute-downstream!
  "Constructs a sequence of flood-carrier objects (one per source
   point) and then iteratively computes the next-step downstream
   flood-carriers from the previous until they no longer have any
   water, fall off the map bounds, or hit an inland sink.  All the
   carriers are moved together in timesteps (more or less)."
  [cache-layer step-downstream in-stream? unsaturated-sink? source-layer
   source-points sink-map use-map sink-AFs use-AFs sink-caps]
  (println "Moving the flood-carriers downstream...")
  (doseq [_ (take-while seq (iterate
                             (fn [flood-carriers]
                               (remove nil?
                                       (pmap (p step-downstream!
                                                cache-layer
                                                step-downstream
                                                in-stream?
                                                unsaturated-sink?
                                                sink-map
                                                use-map
                                                sink-AFs
                                                use-AFs
                                                sink-caps)
                                             flood-carriers)))
                             (map
                              #(let [source-weight (get-in source-layer %)]
                                 (struct-map service-carrier
                                   :source-id       %
                                   :route           [%]
                                   :possible-weight source-weight
                                   :actual-weight   source-weight
                                   :sink-effects    {}
                                   :stream-bound?   (in-stream? %)))
                              source-points)))]
    (print "*") (flush))
  (println "\nAll done."))

(defmethod distribute-flow "FloodWaterMovement"
  [_ source-layer sink-layer use-layer
   {hydrosheds-layer "Hydrosheds", stream-layer "River", elevation-layer "Altitude"
    floodplain-layer100 "Floodplains100", floodplain-layer500 "Floodplains500"}]
  (println "Running Flood flow model for" (if floodplain-layer500 "500" "100") "year floodplain.")
  (let [floodplain-layer (or floodplain-layer500 floodplain-layer100)
        rows             (get-rows source-layer)
        cols             (get-cols source-layer)
        cache-layer      (make-matrix rows cols (constantly (atom ())))
        [source-points sink-points use-points] (pmap (p filter-matrix-for-coords (p not= _0_))
                                                     [source-layer sink-layer use-layer])]
    (println "Source points:" (count source-points))
    (println "Sink points:  " (count sink-points))
    (println "Use points:   " (count use-points))
    (let [flow-delta         (fn [id] (hydrosheds-delta-codes (get-in hydrosheds-layer id)))
          step-downstream    (fn [id] (if-let [dir (flow-delta id)] ; if nil, we've hit 0.0 (ocean) or -1.0 (inland sink)
                                        (let [next-id (map + id dir)]
                                          (if (in-bounds? rows cols next-id)
                                            next-id))))
          in-stream?         (fn [id] (not= _0_ (get-in stream-layer id)))
          in-floodplain?     (fn [id] (not= _0_ (get-in floodplain-layer id)))
          [sink-map use-map] (do (println "Shifting sink and use points into the nearest stream channel...")
                                 (time (pmap (p move-points-into-stream-channel step-downstream in-stream?)
                                             [sink-points use-points])))
          [sink-AFs use-AFs] (do (println "Computing sink and use flood-activation-factors...")
                                 (time (pmap (p flood-activation-factors in-floodplain? elevation-layer)
                                             [sink-map use-map])))
          sink-caps          (seq2map sink-points (fn [id] [id (ref (get-in sink-layer id))]))
          unsaturated-sink?  (fn [id] (not= _0_ (deref (sink-caps id))))]
      (time (distribute-downstream! cache-layer step-downstream in-stream? unsaturated-sink? source-layer
                                    source-points sink-map use-map sink-AFs use-AFs sink-caps))
      (println "Simulation complete. Returning the cache-layer.")
      (map-matrix (& seq deref) cache-layer))))
