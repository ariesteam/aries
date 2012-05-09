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
  (:use [clj-misc.utils      :only (seq2map mapmap iterate-while-seq with-message
                                    memoize-by-first-arg angular-distance p my->>
                                    with-progress-bar-cool euclidean-distance)]
        [clj-misc.matrix-ops :only (get-neighbors on-bounds? add-ids subtract-ids find-nearest
                                    find-line-between filter-matrix-for-coords)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier with-typed-math-syms))

(def #^{:dynamic true} _0_)
(def #^{:dynamic true} _+_)
(def #^{:dynamic true} *_)
(def #^{:dynamic true} _d)
(def #^{:dynamic true} rv-fn)
(def #^{:dynamic true} _min_)

(defn assign-floodwater-to-floodplain-users!
  [{:keys [use-stream-intakes use-AFs cache-layer]}]
  (with-message "Assigning floodwater detected at stream intakes to floodplain users..." "done."
    (doseq [[stream-id use-ids] use-stream-intakes]
      (let [stream-cache     (get-in cache-layer stream-id)
            service-carriers (map #(dissoc % :stream-bound?) (deref stream-cache))]
        (dosync
         (ref-set stream-cache ())
         (doseq [{:keys [possible-weight actual-weight sink-effects] :as carrier} service-carriers]
           (doseq [id use-ids]
             (let [cache (get-in cache-layer id)
                   AF    (use-AFs id)]
               (commute cache conj
                        (assoc carrier
                          :possible-weight (*_ AF possible-weight)
                          :actual-weight   (*_ AF actual-weight)
                          :sink-effects    (mapmap identity #(*_ AF %) sink-effects)))))))))))

(defn nearest-to-bearing
  [bearing id neighbors]
  (if (seq neighbors)
    (if bearing
      (let [bearing-changes (seq2map neighbors
                                     #(let [bearing-to-neighbor (subtract-ids % id)]
                                        [(angular-distance bearing bearing-to-neighbor)
                                         %]))]
        (bearing-changes (apply min (keys bearing-changes))))
      (first neighbors))))
(def nearest-to-bearing (memoize nearest-to-bearing))

(defn lowest-neighbors
  [id in-stream? flow-layers rows cols]
  (if-not (on-bounds? rows cols id)
    (let [elev-layer     (flow-layers "Altitude")
          neighbors      (if (in-stream? id)
                           (filter in-stream? (get-neighbors rows cols id)) ; Step downstream
                           (get-neighbors rows cols id)) ; Step downhill
          local-elev     (get-in elev-layer id)
          neighbor-elevs (map (p get-in elev-layer) neighbors)
          min-elev       (reduce _min_ local-elev neighbor-elevs)]
      (filter #(= min-elev (get-in elev-layer %)) neighbors))))
(def lowest-neighbors (memoize-by-first-arg lowest-neighbors))

;; FIXME: Somehow this still doesn't terminate correctly for some carriers.
(defn find-next-step
  [current-id in-stream? flow-layers rows cols route]
  (let [prev-id (peek (pop route))
        bearing (if prev-id (subtract-ids current-id prev-id))]
    (my->> (lowest-neighbors current-id
                             in-stream?
                             flow-layers
                             rows
                             cols)
           (remove (p = prev-id))
           (nearest-to-bearing bearing current-id))))

(defn take-next-step
  [current-id
   {:keys [in-stream? flow-layers rows cols]}
   {:keys [route] :as floodwater-carrier}]
  (if-let [next-id (find-next-step current-id in-stream? flow-layers rows cols route)]
    (assoc floodwater-carrier
      :route         (conj route next-id)
      :stream-bound? (in-stream? next-id))))

(defn calculate-sink!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [actual-weight sink-cap-ref]
  (dosync
   (let [sink-cap (deref sink-cap-ref)]
     (if (= sink-cap _0_)
       [actual-weight _0_]
       (do
         (alter sink-cap-ref #(rv-fn '(fn [a s] (max (- s a) 0.0)) actual-weight %))
         [(rv-fn '(fn [a s] (max (- a s) 0.0)) actual-weight sink-cap)
          (rv-fn '(fn [a s] (min a s)) actual-weight sink-cap)])))))

(defn handle-sink-effects!
  [current-id
   {:keys [floodplain-sink-caps overland-sink-caps]}
   {:keys [actual-weight sink-effects stream-bound?] :as floodwater-carrier}]
  (if (not= actual-weight _0_)
    (if-let [sink-cap-ref ((if stream-bound? floodplain-sink-caps overland-sink-caps) current-id)]
      (let [[new-actual-weight actual-sink] (calculate-sink! actual-weight sink-cap-ref)]
        (assoc floodwater-carrier
          :actual-weight new-actual-weight
          :sink-effects  (merge-with _+_ sink-effects {current-id actual-sink})))
      floodwater-carrier)
    floodwater-carrier))

(defn store-carrier!
  [current-id
   {:keys [cache-layer possible-flow-layer actual-flow-layer mm2-per-cell]}
   {:keys [route possible-weight actual-weight sink-effects] :as floodwater-carrier}]
  (let [possible-density (_d possible-weight mm2-per-cell)
        actual-density   (_d actual-weight   mm2-per-cell)]
    (dosync
     (doseq [id route]
       (commute (get-in possible-flow-layer id) _+_ possible-density))
     (if (not= actual-weight _0_)
       (doseq [id route]
         (commute (get-in actual-flow-layer id) _+_ actual-density)))
     (commute (get-in cache-layer current-id) conj
              (assoc floodwater-carrier
                :route           nil
                :possible-weight possible-density
                :actual-weight   actual-density
                :sink-effects    (mapmap identity #(_d % mm2-per-cell) sink-effects))))))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
(defn to-the-ocean!
  "Computes the state of the floodwater-carrier after it takes another
   step downhill. If it encounters a sink location, it drops some
   floodwater according to the remaining sink capacity at this
   location. If it encounters a use location, a service-carrier is
   stored in the user's carrier-cache."
  [{:keys [use-stream-intakes] :as params}
   {:keys [route] :as floodwater-carrier}]
  (try
    (let [current-id (peek route)]
      (if (use-stream-intakes current-id)
        (store-carrier! current-id params floodwater-carrier))
      (my->> floodwater-carrier
             (handle-sink-effects! current-id params)
             (take-next-step current-id params)))
    (catch Exception _ (println "Bad agent go BOOM!"))))

(defn report-carrier-counts
  [floodwater-carriers]
  (let [on-land-carriers   (count (remove :stream-bound? floodwater-carriers))
        in-stream-carriers (- (count floodwater-carriers) on-land-carriers)]
    (printf "Carriers: %10d | On Land: %10d | In Stream: %10d%n"
            (+ on-land-carriers in-stream-carriers)
            on-land-carriers
            in-stream-carriers)
    (flush)))

(defn move-carriers-one-step-downstream!
  [params floodwater-carriers]
  (report-carrier-counts floodwater-carriers)
  (pmap (p to-the-ocean! params) floodwater-carriers))

(defn create-initial-service-carriers
  [{:keys [source-layer source-points mm2-per-cell in-stream?]}]
  (map
   #(let [source-weight (*_ mm2-per-cell (get-in source-layer %))]
      (struct-map service-carrier
        :source-id       %
        :route           [%]
        :possible-weight source-weight
        :actual-weight   source-weight
        :sink-effects    {}
        :stream-bound?   (in-stream? %)))
   source-points))

(defn stop-unless-reducing
  [n coll]
  (dorun (take-while (fn [[p c]] (> p c)) (partition 2 1 (map count (take-nth n coll))))))

(defn propagate-floodwater!
  "Constructs a sequence of floodwater-carrier objects (one per source
   point) and then iteratively propagates them downhill until they
   reach a stream location, get stuck in a low elevation point, or
   fall off the map bounds. Once they reach a stream location, the
   carriers will attempt to continue downhill while staying in a
   stream course. All the carriers are moved together in
   timesteps (more or less)."
  [params]
  (with-message "Moving the floodwater carriers downhill and downstream...\n" "Done moving floodwater carriers."
    (stop-unless-reducing
     100
     (iterate-while-seq
      (p move-carriers-one-step-downstream! params)
      (create-initial-service-carriers params))))
  (select-keys params [:use-stream-intakes :use-AFs :cache-layer]))

(defn make-buckets
  "Stores maps from {ids -> mm3-ref} for overland-sink-caps and floodplain-sink-caps in params."
  [{:keys [sink-layer sink-points sink-AFs sink-stream-intakes in-floodplain? mm2-per-cell] :as params}]
  (let [in-stream-points     (keys sink-stream-intakes)
        total-sink-by-intake (for [id in-stream-points]
                               (let [floodplain-sink-ids (sink-stream-intakes id)]
                                 (reduce _+_ (map #(*_ (sink-AFs %) (get-in sink-layer %)) floodplain-sink-ids))))]
    (assoc params
      :overland-sink-caps   (seq2map (remove in-floodplain? sink-points) (fn [id] [id (ref (*_ mm2-per-cell (get-in sink-layer id)))]))
      :floodplain-sink-caps (into {} (map (fn [stream-id total-sink] [stream-id (ref (*_ mm2-per-cell total-sink))])
                                          in-stream-points
                                          total-sink-by-intake)))))

(defn get-boundary-id
  [stream-id sink-id in-floodplain?]
  (let [loc-delta  (subtract-ids sink-id stream-id)
        inside-id  (my->> (iterate #(add-ids loc-delta %) sink-id)
                          (take-while in-floodplain?)
                          last)
        outside-id (add-ids inside-id loc-delta)]
    (first (remove in-floodplain? (find-line-between inside-id outside-id)))))

;; FIXME: Should we be considering the elevation of our data-point?
(defn compute-floodplain-activation-factors
  [in-floodplain? stream-intakes stream-intakes-label]
  (with-message (str "Computing floodplain activation factors for " stream-intakes-label "...\n") "\nAll done."
    (into {}
          (with-progress-bar-cool
            :keep
            (count (apply concat (vals stream-intakes)))
            (for [[stream-id sink-ids] stream-intakes sink-id sink-ids]
              (if (= sink-id stream-id)
                ;; location is already in-stream, activation is 100%
                [sink-id 1.0]
                ;; location is out-of-stream, activation is scaled
                ;; by the relative distance between this location,
                ;; the in-stream proxy location, and the nearest
                ;; floodplain boundary
                [sink-id (- 1.0 (/ (euclidean-distance stream-id sink-id)
                                   (euclidean-distance stream-id (get-boundary-id stream-id sink-id in-floodplain?))))]))))))

(defn compute-sink-and-use-floodplain-activation-factors
  "Stores maps under (params :sink-AFs) and (params :use-AFs) of
   {floodplain-sink-ids -> AF} and {floodplain-use-ids -> AF}
   respectively, where AF is a number between 0.0 and 1.0,
   representing the sink or use id's relative position between the
   stream edge (1.0) and the floodplain boundary (0.0)."
  [{:keys [in-floodplain? sink-stream-intakes use-stream-intakes] :as params}]
  (assoc params
    :sink-AFs (compute-floodplain-activation-factors in-floodplain? sink-stream-intakes "sinks")
    :use-AFs  (compute-floodplain-activation-factors in-floodplain? use-stream-intakes  "users")))

(defn find-nearest-stream-points
  [in-stream? levee? rows cols floodplain-points floodplain-points-label]
  (with-message
    (str "Finding nearest stream points to " floodplain-points-label " in floodplains...\n")
    #(str "\nDone. Found " (count %) " intake points.")
    (let [in-stream-extractors (filter in-stream? floodplain-points)
          claimed-intakes      (zipmap in-stream-extractors (map vector in-stream-extractors))]
      (println (str "Detected " (count in-stream-extractors) " in-stream " floodplain-points-label "."))
      (println (str "Continuing with out-of-stream " floodplain-points-label "..."))
      (apply merge-with concat
             claimed-intakes
             (with-progress-bar-cool
               :keep
               (- (count floodplain-points) (count in-stream-extractors))
               (pmap #(if-let [stream-id (find-nearest in-stream? rows cols %)]
                        (if (not-any? levee? (find-line-between stream-id %))
                          {stream-id [%]}))
                     (remove in-stream? floodplain-points)))))))

(defn link-streams-to-sinks-and-users
  "Stores maps of {stream-ids -> nearest-floodplain-sink-ids} and
   {stream-ids -> nearest-floodplain-use-ids}
   under (params :sink-stream-intakes)
   and (params :use-stream-intakes) respectively."
  [{:keys [levee? in-stream? in-floodplain? rows cols sink-points use-points] :as params}]
  (assoc params
    :sink-stream-intakes (find-nearest-stream-points in-stream? levee? rows cols (filter in-floodplain? sink-points) "sinks")
    :use-stream-intakes  (find-nearest-stream-points in-stream? levee? rows cols (filter in-floodplain? use-points)  "users")))

(defn create-feature-tests
  "Stores sets in params map of all levee, river, and floodplain ids under :levee?, :in-stream?, and :in-floodplain?"
  [{:keys [flow-layers] :as params}]
  (assoc params
    :levee?         (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "Levees")))
    :in-stream?     (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "River")))
    :in-floodplain? (set (filter-matrix-for-coords #(not= _0_ %) (or (flow-layers "Floodplains100Code")
                                                                     (flow-layers "Floodplains500Code"))))))

(defn compute-mm2-per-cell
  "Stores cell-width * cell-height * 10^6 under (params :mm2-per-cell)."
  [{:keys [cell-width cell-height] :as params}]
  (assoc params
    :mm2-per-cell (* cell-width cell-height (Math/pow 10.0 6.0))))

(defmethod distribute-flow! "FloodWaterMovement"
  [{:keys [source-layer sink-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points sink-points use-points
           value-type cell-width cell-height rows cols]
    :as params}]
  (println "Operating in" (if (flow-layers "Floodplains500Code") "500" "100") "year floodplain.")
  (with-typed-math-syms value-type [_0_ _+_ *_ _d rv-fn _min_]
    (-> params
        compute-mm2-per-cell
        create-feature-tests
        link-streams-to-sinks-and-users
        compute-sink-and-use-floodplain-activation-factors
        make-buckets
        propagate-floodwater!
        assign-floodwater-to-floodplain-users!)))
