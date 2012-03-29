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
;;; This namespace defines the surface-water model.
;;;

(ns clj-span.models.surface-water
  (:use [clj-misc.utils      :only (seq2map mapmap iterate-while-seq with-message
                                    memoize-by-first-arg angular-distance p
                                    with-progress-bar-cool my->>)]
        [clj-misc.matrix-ops :only (get-neighbors on-bounds? subtract-ids find-nearest filter-matrix-for-coords)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier with-typed-math-syms))

(def #^{:dynamic true} _0_)
(def #^{:dynamic true} _+_)
(def #^{:dynamic true} *_)
(def #^{:dynamic true} _d)
(def #^{:dynamic true} rv-fn)
(def #^{:dynamic true} _min_)
(def #^{:dynamic true} _>)
(def #^{:dynamic true} _*_)
(def #^{:dynamic true} _d_)

(defn assign-water-to-users!
  [{:keys [stream-intakes cache-layer use-layer]}]
  (with-message "Assigning water captured at stream intakes to users..." "done."
    (doseq [[stream-id use-ids] stream-intakes]
      (let [stream-cache     (get-in cache-layer stream-id)
            service-carriers (map #(dissoc % :stream-bound?) (deref stream-cache))
            use-caches       (map #(get-in cache-layer %) use-ids)
            use-values       (map #(get-in use-layer %) use-ids)
            total-use        (reduce _+_ use-values)
            use-percents     (map #(_d_ % total-use) use-values)]
        (dosync
         (ref-set stream-cache ())
         (doseq [{:keys [possible-weight actual-weight sink-effects] :as carrier} service-carriers]
           (dorun
            (map (fn [cache percent]
                   (commute cache conj
                            (assoc carrier
                              :possible-weight (_*_ possible-weight percent)
                              :actual-weight   (_*_ actual-weight   percent)
                              :sink-effects    (mapmap identity #(_*_ % percent) sink-effects))))
                 use-caches
                 use-percents))))))))

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
   {:keys [trans-threshold-volume in-stream? flow-layers rows cols]}
   {:keys [possible-weight route] :as surface-water-carrier}]
  (if (_> possible-weight trans-threshold-volume)
    (if-let [next-id (find-next-step current-id in-stream? flow-layers rows cols route)]
      (assoc surface-water-carrier
        :route         (conj route next-id)
        :stream-bound? (in-stream? next-id)))))

(defn calculate-use!
  [current-id use-caps weight]
  (let [use-cap-ref (use-caps current-id)
        use-cap     (deref use-cap-ref)]
    (if (or (= use-cap _0_)
            (= weight  _0_))
      [weight _0_]
      (do (alter use-cap-ref #(rv-fn '(fn [w u] (max (- u w) 0.0)) weight %))
          [(rv-fn '(fn [w u] (max (- w u) 0.0)) weight use-cap)
           (rv-fn '(fn [w u] (min w u))         weight use-cap)]))))

(defn local-use!
  [current-id
   {:keys [cache-layer possible-flow-layer actual-flow-layer possible-use-caps actual-use-caps mm2-per-cell]}
   {:keys [route possible-weight actual-weight sink-effects] :as surface-water-carrier}]
  (dosync
   (let [[new-possible-weight possible-use] (calculate-use! current-id possible-use-caps possible-weight)
         [new-actual-weight   actual-use]   (calculate-use! current-id actual-use-caps   actual-weight)]
     (if-not (and (= possible-use _0_)
                  (= actual-use   _0_))
       (let [possible-density (_d possible-use mm2-per-cell)
             actual-density   (_d actual-use   mm2-per-cell)]
         (if (not= possible-density _0_)
           (doseq [id route]
             (commute (get-in possible-flow-layer id) _+_ possible-density)))
         (if (not= actual-density _0_)
           (doseq [id route]
             (commute (get-in actual-flow-layer id) _+_ actual-density)))
         (commute (get-in cache-layer current-id) conj (assoc surface-water-carrier
                                                         :route           nil
                                                         :possible-weight possible-density
                                                         :actual-weight   actual-density
                                                         :sink-effects    (mapmap identity #(_d % mm2-per-cell) sink-effects)))))
     [new-possible-weight new-actual-weight])))

(defn handle-use-effects!
  [current-id {:keys [possible-use-caps] :as params} surface-water-carrier]
  (if (possible-use-caps current-id)
    (let [[new-possible-weight new-actual-weight] (local-use! current-id
                                                              params
                                                              surface-water-carrier)]
      (assoc surface-water-carrier
        :possible-weight new-possible-weight
        :actual-weight   new-actual-weight))
    surface-water-carrier))

(defn local-sink!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id actual-weight sink-cap-ref]
  (dosync
   (let [sink-cap (deref sink-cap-ref)]
     (if (= sink-cap _0_)
       [actual-weight {}]
       (do
         (alter sink-cap-ref #(rv-fn '(fn [a s] (max (- s a) 0.0)) actual-weight %))
         [(rv-fn '(fn [a s] (max (- a s) 0.0)) actual-weight sink-cap)
          {current-id (rv-fn '(fn [a s] (min a s)) actual-weight sink-cap)}])))))

(defn handle-sink-effects!
  [current-id {:keys [sink-caps]} {:keys [actual-weight sink-effects] :as surface-water-carrier}]
  (if (not= actual-weight _0_)
    (if-let [sink-cap-ref (sink-caps current-id)]
      (let [[new-actual-weight new-sink-effects] (local-sink! current-id
                                                              actual-weight
                                                              sink-cap-ref)]
        (assoc surface-water-carrier
          :actual-weight new-actual-weight
          :sink-effects  (merge-with _+_ sink-effects new-sink-effects)))
      surface-water-carrier)
    surface-water-carrier))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
(defn to-the-ocean!
  "Computes the state of the surface-water-carrier after it takes
   another step downhill. If it encounters a sink location, it drops
   some water according to the remaining sink capacity at this
   location."
  [params {:keys [route stream-bound?] :as surface-water-carrier}]
  (try
    (let [current-id        (peek route)
          local-effects-fn! (if stream-bound? handle-use-effects! handle-sink-effects!)]
      (my->> surface-water-carrier
             (local-effects-fn! current-id params)
             (take-next-step current-id params)))
    (catch Exception _ (println "Bad agent go BOOM!"))))

(defn report-carrier-counts
  [surface-water-carriers]
  (let [on-land-carriers   (count (remove :stream-bound? surface-water-carriers))
        in-stream-carriers (- (count surface-water-carriers) on-land-carriers)]
    (printf "Carriers: %10d | On Land: %10d | In Stream: %10d%n"
            (+ on-land-carriers in-stream-carriers)
            on-land-carriers
            in-stream-carriers)
    (flush)))

(defn move-carriers-one-step-downstream!
  [params surface-water-carriers]
  (report-carrier-counts surface-water-carriers)
  (map (p to-the-ocean! params) surface-water-carriers))
  ;; (pmap (p to-the-ocean! params) surface-water-carriers))

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

(defn propagate-runoff!
  "Constructs a sequence of surface-water-carrier objects (one per
   source point) and then iteratively propagates them downhill until
   they reach a stream location, get stuck in a low elevation point,
   or fall off the map bounds. Once they reach a stream location, the
   carriers will attempt to continue downhill while staying in a
   stream course. Sinks affect carriers overland. Users affect
   carriers in stream channels. All the carriers are moved together in
   timesteps (more or less)."
  [params]
  (with-message "Moving the surface water carriers downhill and downstream...\n" "Done moving surface water carriers."
    (stop-unless-reducing
     100
     (iterate-while-seq
      (p move-carriers-one-step-downstream! params)
      (create-initial-service-carriers params))))
  (select-keys params [:stream-intakes :cache-layer :use-layer]))

(defn make-buckets
  "Stores maps from {ids -> mm3-ref} for sink-caps, possible-use-caps, and actual-use-caps in params."
  [{:keys [sink-layer sink-points use-layer stream-intakes mm2-per-cell] :as params}]
  (let [in-stream-points    (keys stream-intakes)
        total-use-by-intake (for [id in-stream-points]
                              (let [use-ids (stream-intakes id)]
                                (reduce _+_ (map #(get-in use-layer %) use-ids))))]
    (assoc params
      :sink-caps         (seq2map sink-points (fn [id] [id (ref (*_ mm2-per-cell (get-in sink-layer id)))]))
      :possible-use-caps (into {} (map (fn [stream-id total-use] [stream-id (ref (*_ mm2-per-cell total-use))])
                                       in-stream-points
                                       total-use-by-intake))
      :actual-use-caps   (into {} (map (fn [stream-id total-use] [stream-id (ref (*_ mm2-per-cell total-use))])
                                       in-stream-points
                                       total-use-by-intake)))))

(defn find-nearest-stream-points
  [in-stream? rows cols use-points]
  (with-message
    "Finding nearest stream points to all users...\n"
    #(str "\nDone. Found " (count %) " intake points.")
    (let [in-stream-users (filter in-stream? use-points)
          claimed-intakes (zipmap in-stream-users (map vector in-stream-users))]
      (println "Detected" (count in-stream-users) "in-stream users.\nContinuing with out-of-stream users...")
      (apply merge-with concat
             claimed-intakes
             (with-progress-bar-cool
               :keep
               (- (count use-points) (count in-stream-users))
               (pmap #(if-let [stream-id (find-nearest in-stream? rows cols %)] {stream-id [%]})
                     (remove in-stream? use-points)))))))

(defn link-streams-to-users
  "Stores a map of {stream-ids -> nearest-use-ids} under (params :stream-intakes)."
  [{:keys [in-stream? rows cols use-points] :as params}]
  (assoc params
    :stream-intakes (find-nearest-stream-points in-stream? rows cols use-points)))

(defn create-in-stream-test
  "Stores a set of all in-stream ids under (params :in-stream?)."
  [{:keys [flow-layers] :as params}]
  (assoc params
    :in-stream? (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "River")))))

(defn compute-trans-threshold-volume
  "Stores trans-threshold * mm2-per-cell under (params :trans-threshold-volume)."
  [{:keys [trans-threshold mm2-per-cell] :as params}]
  (assoc params
    :trans-threshold-volume (* trans-threshold mm2-per-cell)))

(defn compute-mm2-per-cell
  "Stores cell-width * cell-height * 10^6 under (params :mm2-per-cell)."
  [{:keys [cell-width cell-height] :as params}]
  (assoc params
    :mm2-per-cell (* cell-width cell-height (Math/pow 10.0 6.0))))

(defmethod distribute-flow! "SurfaceWaterMovement"
  [{:keys [source-layer sink-layer use-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points sink-points use-points
           cell-width cell-height rows cols
           value-type trans-threshold]
    :as params}]
  (with-typed-math-syms value-type [_0_ _+_ *_ _d rv-fn _min_ _> _*_ _d_]
    (-> params
        compute-mm2-per-cell
        compute-trans-threshold-volume
        create-in-stream-test
        link-streams-to-users
        make-buckets
        propagate-runoff!
        assign-water-to-users!)))
