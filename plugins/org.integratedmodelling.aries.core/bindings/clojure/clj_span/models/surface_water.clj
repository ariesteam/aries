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
  (:use [clj-span.core          :only (distribute-flow! service-carrier)]
        [clj-span.params        :only (*trans-threshold*)]
        [clj-misc.utils         :only (seq2map mapmap iterate-while-seq with-message
                                       memoize-by-first-arg angular-distance p & def-
                                       with-progress-bar-cool)]
        [clj-misc.matrix-ops    :only (get-neighbors on-bounds? subtract-ids find-nearest)]
        [clj-misc.varprop       :only (_0_ _+_ *_ _d rv-fn _min_ _>)]))

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
          min-elev       (reduce _min_ local-elev neighbor-elevs)]
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
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id possible-weight actual-weight stream-intakes possible-use-caps
   actual-use-caps cache-layer mm2-per-cell surface-water-carrier]
  (if-let [use-id (stream-intakes current-id)]
    (dosync
     (let [possible-use-cap-ref (possible-use-caps use-id)
           actual-use-cap-ref   (actual-use-caps   use-id)
           possible-use-cap     (deref possible-use-cap-ref)
           actual-use-cap       (deref actual-use-cap-ref)
           [new-possible-weight possible-use]
           (if (= _0_ possible-use-cap)
             [possible-weight _0_]
             (do
               (alter possible-use-cap-ref #(rv-fn (fn [p u] (max (- u p) 0.0)) possible-weight %))
               [(rv-fn (fn [p u] (max (- p u) 0.0)) possible-weight possible-use-cap)
                (rv-fn (fn [p u] (min p u))         possible-weight possible-use-cap)]))
           [new-actual-weight actual-use]
           (if (or (= _0_ actual-use-cap)
                   (= _0_ actual-weight))
             [actual-weight _0_]
             (do
               (alter actual-use-cap-ref #(rv-fn (fn [a u] (max (- u a) 0.0)) actual-weight %))
               [(rv-fn (fn [a u] (max (- a u) 0.0)) actual-weight actual-use-cap)
                (rv-fn (fn [a u] (min a u))         actual-weight actual-use-cap)]))]
       (if (or (not= _0_ possible-use)
               (not= _0_ actual-use))
         (alter (get-in cache-layer use-id) conj (assoc surface-water-carrier
                                                   :possible-weight (_d possible-use mm2-per-cell)
                                                   :actual-weight   (_d actual-use   mm2-per-cell)
                                                   :sink-effects    (mapmap identity #(_d % mm2-per-cell)
                                                                            (:sink-effects surface-water-carrier)))))
       [new-possible-weight new-actual-weight]))
    [possible-weight actual-weight]))

(defn- handle-sink-effects!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id actual-weight sink-caps]
  (if-let [sink-cap-ref (sink-caps current-id)]
    (dosync
     (let [sink-cap (deref sink-cap-ref)]
       (if (or (= _0_ actual-weight)
               (= _0_ sink-cap))
         [actual-weight {}]
         (do
           (alter sink-cap-ref #(rv-fn (fn [a s] (max (- s a) 0.0)) actual-weight %))
           [(rv-fn (fn [a s] (max (- a s) 0.0)) actual-weight sink-cap)
            {current-id (rv-fn (fn [a s] (min a s)) actual-weight sink-cap)}]))))
    [actual-weight {}]))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
(defn- to-the-ocean!
  "Computes the state of the surface-water-carrier after it takes
   another step downhill.  If it encounters a sink location, it drops
   some water according to the remaining sink capacity at this
   location."
  [cache-layer possible-flow-layer actual-flow-layer sink-caps possible-use-caps actual-use-caps
   in-stream? stream-intakes mm2-per-cell trans-threshold-volume elevation-layer rows cols
   {:keys [route possible-weight actual-weight sink-effects stream-bound?] :as surface-water-carrier}]
  (let [current-id (peek route)
        prev-id    (peek (pop route))
        bearing    (if prev-id (subtract-ids current-id prev-id))]
    (dosync
     (alter (get-in possible-flow-layer current-id) _+_ (_d possible-weight mm2-per-cell))
     (alter (get-in actual-flow-layer   current-id) _+_ (_d actual-weight   mm2-per-cell)))
    (if stream-bound?
      (let [[new-possible-weight new-actual-weight] (handle-use-effects! current-id
                                                                         possible-weight
                                                                         actual-weight
                                                                         stream-intakes
                                                                         possible-use-caps
                                                                         actual-use-caps
                                                                         cache-layer
                                                                         mm2-per-cell
                                                                         surface-water-carrier)]
        (if (_> new-possible-weight trans-threshold-volume)
          (if-let [next-id (find-next-step current-id in-stream? elevation-layer rows cols bearing)]
            (assoc surface-water-carrier
              :route           (conj route next-id)
              :possible-weight new-possible-weight
              :actual-weight   new-actual-weight))))
      (let [[new-actual-weight new-sink-effects] (handle-sink-effects! current-id
                                                                       actual-weight
                                                                       sink-caps)]
        (if-let [next-id (find-next-step current-id in-stream? elevation-layer rows cols bearing)]
          (assoc surface-water-carrier
            :route           (conj route next-id)
            :actual-weight   new-actual-weight
            :sink-effects    (merge-with _+_ sink-effects new-sink-effects)
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
  [cache-layer possible-flow-layer actual-flow-layer source-layer
   source-points mm2-per-cell sink-caps possible-use-caps actual-use-caps
   in-stream? stream-intakes elevation-layer rows cols]
  (with-message "Moving the surface water carriers downhill and downstream...\n" "All done."
    (dorun
     (stop-unless-reducing
      100
      (iterate-while-seq
       (fn [surface-water-carriers]
         (let [on-land-carriers   (count (remove :stream-bound? surface-water-carriers))
               in-stream-carriers (- (count surface-water-carriers) on-land-carriers)]
           (printf "Carriers: %10d | On Land: %10d | In Stream: %10d%n"
                   (+ on-land-carriers in-stream-carriers)
                   on-land-carriers
                   in-stream-carriers)
           (flush)
           (pmap (p to-the-ocean!
                    cache-layer
                    possible-flow-layer
                    actual-flow-layer
                    sink-caps
                    possible-use-caps
                    actual-use-caps
                    in-stream?
                    stream-intakes
                    mm2-per-cell
                    (* mm2-per-cell *trans-threshold*)
                    elevation-layer
                    rows
                    cols)
                 surface-water-carriers)))
       (map
        #(let [source-weight (*_ mm2-per-cell (get-in source-layer %))]
           (struct-map service-carrier
             :source-id       %
             :route           [%]
             :possible-weight source-weight
             :actual-weight   source-weight
             :sink-effects    {}
             :stream-bound?   (in-stream? %)))
        source-points))))))

(defn find-nearest-stream-point!
  [in-stream? claimed-intakes rows cols id]
  (dosync
   (let [available-intake? (complement @claimed-intakes)
         stream-point      (find-nearest #(and (in-stream? %) (available-intake? %)) rows cols id)]
     (if stream-point (alter claimed-intakes conj [stream-point id])))))

(defn find-nearest-stream-points
  [in-stream? rows cols use-points]
  (with-message
    "Finding nearest stream points to all users..."
    #(str "done. [Claimed intakes: " (count %) "]")
    (let [in-stream-users (filter in-stream? use-points)
          claimed-intakes (ref (zipmap in-stream-users in-stream-users))]
      (with-progress-bar-cool
        :drop
        (- (count use-points) (count in-stream-users))
        (pmap (p find-nearest-stream-point! in-stream? claimed-intakes rows cols)
              (remove in-stream? use-points)))
      @claimed-intakes)))

(defn- make-buckets
  [mm2-per-cell layer active-points]
  (seq2map active-points (fn [id] [id (ref (*_ mm2-per-cell (get-in layer id)))])))

(defmethod distribute-flow! "SurfaceWaterMovement"
  [_ cell-width cell-height rows cols cache-layer possible-flow-layer
   actual-flow-layer source-layer sink-layer use-layer source-points
   sink-points use-points {stream-layer "River", elevation-layer "Altitude"}]
  (let [mm2-per-cell      (* cell-width cell-height (Math/pow 10.0 6.0))
        sink-caps         (make-buckets mm2-per-cell sink-layer sink-points)
        possible-use-caps (make-buckets mm2-per-cell use-layer  use-points)
        actual-use-caps   (mapmap identity (& ref deref) possible-use-caps)
        in-stream?        (memoize #(not= _0_ (get-in stream-layer %)))
        stream-intakes    (find-nearest-stream-points in-stream? rows cols use-points)]
    (propagate-runoff! cache-layer
                       possible-flow-layer
                       actual-flow-layer
                       source-layer
                       source-points
                       mm2-per-cell
                       sink-caps
                       possible-use-caps
                       actual-use-caps
                       in-stream?
                       stream-intakes
                       elevation-layer
                       rows
                       cols)))
