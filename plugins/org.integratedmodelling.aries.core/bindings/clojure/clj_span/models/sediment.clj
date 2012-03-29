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
;;; This namespace defines the sediment model.
;;;
;;; FIXME: Model updates due to conversation with Ken on 10/15/10:
;;;
;;; Three classes of sinks: floodplains, dams/reservoirs, water intakes
;;; Two class of beneficiaries: farmers in floodplains, avoided turbidity beneficiaries (dams and water intakes)
;;;
;;; In order to distinguish their behavior in this model, I need layers
;;; for the presence/absence of floodplains, dams/reservoirs, water
;;; intakes, and farmers passed as flow dependencies in the span
;;; statement.
;;;

(ns clj-span.models.sediment
  (:use [clj-misc.utils      :only (seq2map mapmap iterate-while-seq with-message
                                    memoize-by-first-arg angular-distance p
                                    with-progress-bar-cool euclidean-distance my->>)]
        [clj-misc.matrix-ops :only (get-neighbors on-bounds? add-ids subtract-ids find-nearest
                                    find-line-between filter-matrix-for-coords)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier with-typed-math-syms))

(def #^{:dynamic true} _0_)
(def #^{:dynamic true} _+_)
(def #^{:dynamic true} *_)
(def #^{:dynamic true} _d)
(def #^{:dynamic true} rv-fn)
(def #^{:dynamic true} _min_)
(def #^{:dynamic true} _*_)
(def #^{:dynamic true} _d_)

(defn assign-sediment-to-floodplain-users!
  [{:keys [stream-intakes sink-layer sink-AFs cache-layer actual-flow-layer use-id?]}]
  (with-message "Assigning sediment captured at stream intakes to floodplain sinks..." "done."
    (doseq [[stream-id sink-ids] stream-intakes]
      (let [stream-cache     (get-in cache-layer stream-id)
            service-carriers (map #(dissoc % :stream-bound?) (deref stream-cache))
            sink-values      (map #(*_ (sink-AFs %) (get-in sink-layer %)) sink-ids)
            total-sink       (reduce _+_ sink-values)
            sink-percents    (map #(_d_ % total-sink) sink-values)]
        (dosync
         (ref-set stream-cache ())
         (doseq [{:keys [route actual-weight sink-effects] :as carrier} service-carriers]
           (dorun
            (map (fn [id percent]
                   (if (use-id? id) ; this floodplain sink is co-located with a user
                     (let [actual-weight-portion (_*_ actual-weight percent)]
                       (doseq [id route]
                         (commute (get-in actual-flow-layer id) _+_ actual-weight-portion))
                       (commute (get-in cache-layer id) conj
                                (assoc carrier
                                  :route         nil
                                  :actual-weight actual-weight-portion
                                  :sink-effects  (mapmap identity #(_*_ % percent) sink-effects))))))
                 sink-ids
                 sink-percents))))))))

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
   {:keys [route] :as sediment-carrier}]
  (if-let [next-id (find-next-step current-id in-stream? flow-layers rows cols route)]
    (assoc sediment-carrier
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

(defn store-carrier!
  [current-id actual-sink {:keys [cache-layer ha-per-cell]} {:keys [sink-effects] :as sediment-carrier}]
  (dosync
   (commute (get-in cache-layer current-id) conj
            (assoc sediment-carrier
              :actual-weight (_d actual-sink ha-per-cell)
              :sink-effects  (mapmap identity #(_d % ha-per-cell) sink-effects)))))

(defn handle-sink-effects!
  [current-id sink-caps params {:keys [actual-weight sink-effects stream-bound?] :as sediment-carrier}]
  (if (not= actual-weight _0_)
    (if-let [sink-cap-ref (sink-caps current-id)]
      (let [[new-actual-weight actual-sink] (calculate-sink! actual-weight sink-cap-ref)]
        (if (and stream-bound? (not= actual-sink _0_))
          (store-carrier! current-id actual-sink params sediment-carrier))
        (assoc sediment-carrier
          :actual-weight new-actual-weight
          :sink-effects  (merge-with _+_ sink-effects {current-id actual-sink})))
      sediment-carrier)
    sediment-carrier))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
(defn to-the-ocean!
  "Computes the state of the sediment-carrier after it takes another
   step downhill. If it encounters a sink location, it drops some
   sediment according to the remaining sink capacity at this location.
   If it encounters a use location, a service-carrier is stored in the
   user's carrier-cache."
  [{:keys [floodplain-sink-caps highland-sink-caps] :as params} {:keys [route stream-bound?] :as sediment-carrier}]
  (try
    (let [current-id (peek route)
          sink-caps  (if stream-bound? floodplain-sink-caps highland-sink-caps)]
      (my->> sediment-carrier
             (handle-sink-effects! current-id sink-caps params)
             (take-next-step current-id params)))
    (catch Exception _ (println "Bad agent go BOOM!"))))

(defn report-carrier-counts
  [sediment-carriers]
  (let [on-land-carriers   (count (remove :stream-bound? sediment-carriers))
        in-stream-carriers (- (count sediment-carriers) on-land-carriers)]
    (printf "Carriers: %10d | On Land: %10d | In Stream: %10d%n"
            (+ on-land-carriers in-stream-carriers)
            on-land-carriers
            in-stream-carriers)
    (flush)))

(defn move-carriers-one-step-downstream!
  [params sediment-carriers]
  (report-carrier-counts sediment-carriers)
  (pmap (p to-the-ocean! params) sediment-carriers))

(defn create-initial-service-carriers
  [{:keys [source-layer source-points ha-per-cell in-stream?]}]
  (map
   #(let [source-weight (*_ ha-per-cell (get-in source-layer %))]
      (struct-map service-carrier
        :source-id       %
        :route           [%]
        :possible-weight _0_
        :actual-weight   source-weight
        :sink-effects    {}
        :stream-bound?   (in-stream? %)))
   source-points))

(defn stop-unless-reducing
  [n coll]
  (dorun (take-while (fn [[p c]] (> p c)) (partition 2 1 (map count (take-nth n coll))))))

(defn propagate-sediment!
  "Constructs a sequence of sediment-carrier objects (one per
   in-stream source id) and then iteratively propagates them downhill
   until they run out of sediment, reach a stream location, get stuck
   in a low elevation point, or fall off the map bounds. Once they
   reach a stream location, the carriers will attempt to continue
   downhill while staying in a stream course. All the carriers are
   moved together in timesteps (more or less)."
  [params]
  (with-message "Moving the sediment carriers downhill and downstream...\n" "Done moving sediment carriers."
    (stop-unless-reducing
     100
     (iterate-while-seq
      (p move-carriers-one-step-downstream! params)
      (create-initial-service-carriers params))))
  (select-keys params [:stream-intakes :sink-layer :sink-AFs :cache-layer :actual-flow-layer :use-id?]))

(defn make-buckets
  "Stores maps from {ids -> tons-ref} for highland-sink-caps and floodplain-sink-caps in params."
  [{:keys [sink-layer sink-points sink-AFs stream-intakes in-floodplain? ha-per-cell] :as params}]
  (let [in-stream-points     (keys stream-intakes)
        total-sink-by-intake (for [id in-stream-points]
                               (let [floodplain-sink-ids (stream-intakes id)]
                                 (reduce _+_ (map #(*_ (sink-AFs %) (get-in sink-layer %)) floodplain-sink-ids))))]
    (assoc params
      :highland-sink-caps   (seq2map (remove in-floodplain? sink-points) (fn [id] [id (ref (*_ ha-per-cell (get-in sink-layer id)))]))
      :floodplain-sink-caps (into {} (map (fn [stream-id total-sink] [stream-id (ref (*_ ha-per-cell total-sink))])
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
(defn compute-flood-activation-factors
  "Stores a map under (params :sink-AFs) of {floodplain-sink-ids -> AF}, where AF
   is a number between 0.0 and 1.0, representing the sink-id's
   relative position between the stream edge (1.0) and the floodplain
   boundary (0.0)."
  [{:keys [in-floodplain? stream-intakes] :as params}]
  (assoc params
    :sink-AFs (with-message "Computing flood activation factors...\n" "\nAll done."
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
                                               (euclidean-distance stream-id (get-boundary-id stream-id sink-id in-floodplain?))))])))))))

(defn find-nearest-stream-points
  [in-stream? levee? rows cols sink-points]
  (with-message
    "Finding nearest stream points to sinks in floodplains...\n"
    #(str "\nDone. Found " (count %) " intake points.")
    (let [in-stream-sinks (filter in-stream? sink-points)
          claimed-intakes (zipmap in-stream-sinks (map vector in-stream-sinks))]
      (println "Detected" (count in-stream-sinks) "in-stream sinks.\nContinuing with out-of-stream sinks...")
      (apply merge-with concat
             claimed-intakes
             (with-progress-bar-cool
               :keep
               (- (count sink-points) (count in-stream-sinks))
               (pmap #(if-let [stream-id (find-nearest in-stream? rows cols %)]
                        (if (not-any? levee? (find-line-between stream-id %))
                          {stream-id [%]}))
                     (remove in-stream? sink-points)))))))

(defn link-streams-to-sinks
  "Stores a map of {stream-ids -> nearest-floodplain-sink-ids} under (params :stream-intakes)."
  [{:keys [levee? in-stream? in-floodplain? rows cols sink-points] :as params}]
  (assoc params
    :stream-intakes (find-nearest-stream-points in-stream? levee? rows cols (filter in-floodplain? sink-points))))

(defn create-feature-tests
  "Stores sets in params map of all use, levee, river, and floodplain ids under :use-id?, :levee?, :in-stream?, and :in-floodplain?"
  [{:keys [use-points flow-layers] :as params}]
  (assoc params
    :use-id?        (set use-points)
    :levee?         (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "Levee")))
    :in-stream?     (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "River")))
    :in-floodplain? (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "FloodplainsCode")))))

(defn compute-ha-per-cell
  "Stores cell-width * cell-height * 10^-4 under (params :ha-per-cell)."
  [{:keys [cell-width cell-height] :as params}]
  (assoc params
    :ha-per-cell (* cell-width cell-height (Math/pow 10.0 -4.0))))

;; FIXME: 100-yr vs. 500-yr floodplains?
(defmethod distribute-flow! "SedimentTransport"
  [{:keys [source-layer sink-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points sink-points use-points
           value-type cell-width cell-height rows cols]
    :as params}]
  (with-typed-math-syms value-type [_0_ _+_ *_ _d rv-fn _min_ _*_ _d_]
    (-> params
        compute-ha-per-cell
        create-feature-tests
        link-streams-to-sinks
        compute-flood-activation-factors
        make-buckets
        propagate-sediment!
        assign-sediment-to-floodplain-users!)))
