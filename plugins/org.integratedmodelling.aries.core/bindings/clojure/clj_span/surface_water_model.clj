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

(ns clj-span.surface-water-model
  (:use [clj-span.model-api     :only (distribute-flow service-carrier)]
        [clj-span.gui           :only (draw-ref-layer)]
        [clj-span.params        :only (*trans-threshold*)]
        [clj-misc.utils         :only (seq2map mapmap with-progress-bar iterate-while-seq
                                       memoize-by-first-arg angular-distance p &)]
        [clj-misc.matrix-ops    :only (get-rows get-cols make-matrix map-matrix find-bounding-box
                                       filter-matrix-for-coords get-neighbors on-bounds? matrix2seq
                                       subtract-ids)]
        [clj-misc.randvars      :only (_0_ _+_ *_ _d rv-fn rv-min rv-above?)]))

;; FIXME: Somehow this still doesn't terminate correctly for some carriers.
(defn step-downstream
  [id in-stream? elevation-layer rows cols prev-bearing]
  (if-not (on-bounds? rows cols id)
    (let [in-stream-neighbors (filter in-stream? (get-neighbors rows cols id))
          neighbor-elevs      (map (p get-in elevation-layer) in-stream-neighbors)
          local-elev          (get-in elevation-layer id)
          min-elev            (reduce rv-min local-elev neighbor-elevs)
          prev-id             (if prev-bearing (subtract-ids id prev-bearing))
          lowest-neighbors    (filter #(and (not= prev-id %)
                                            (= min-elev (get-in elevation-layer %)))
                                      in-stream-neighbors)]
      (if (seq lowest-neighbors)
        (if prev-bearing
          (let [bearing-changes (seq2map lowest-neighbors
                                         #(let [bearing-to-neighbor (subtract-ids % id)]
                                            [(angular-distance prev-bearing bearing-to-neighbor)
                                             %]))]
            (bearing-changes (apply min (keys bearing-changes))))
          (first lowest-neighbors))))))
(def step-downstream (memoize-by-first-arg step-downstream))

;; FIXME: Somehow this still doesn't terminate correctly for some carriers.
(defn step-downhill
  "Returns the steepest downhill neighboring cell from id within the
   bounds [[0 rows] [0 cols]].  If id is lower than all of its
   neighbors, returns nil.  If more than one neighbor shares the
   steepest downhill slope, returns the first one found.  If the
   current id is located on the bounds [[0 rows][0 cols]], returns
   nil."
  [id elevation-layer rows cols prev-bearing]
  (if-not (on-bounds? rows cols id)
    (let [neighbors        (get-neighbors rows cols id)
          neighbor-elevs   (map (p get-in elevation-layer) neighbors)
          local-elev       (get-in elevation-layer id)
          min-elev         (reduce rv-min local-elev neighbor-elevs)
          prev-id          (if prev-bearing (subtract-ids id prev-bearing))
          lowest-neighbors (filter #(and (not= prev-id %)
                                         (= min-elev (get-in elevation-layer %)))
                                   neighbors)]
      (if (seq lowest-neighbors)
        (if prev-bearing
          (let [bearing-changes (seq2map lowest-neighbors
                                         #(let [bearing-to-neighbor (subtract-ids % id)]
                                            [(angular-distance prev-bearing bearing-to-neighbor)
                                             %]))]
            (bearing-changes (apply min (keys bearing-changes))))
          (first lowest-neighbors))))))
(def step-downhill (memoize-by-first-arg step-downhill))

(defn handle-use-effects!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id possible-weight actual-weight stream-intakes possible-use-caps
   actual-use-caps cache-layer mm2-per-cell surface-water-carrier]
  (if-let [use-id (stream-intakes current-id)]
    (let [possible-use-cap-ref (possible-use-caps use-id)
          actual-use-cap-ref   (actual-use-caps   use-id)]
      (dosync
       (let [possible-use-cap    (deref possible-use-cap-ref)
             actual-use-cap      (deref actual-use-cap-ref)
             new-possible-weight (rv-fn (fn [p u] (max (- p u) 0)) possible-weight possible-use-cap)
             new-actual-weight   (rv-fn (fn [a u] (max (- a u) 0)) actual-weight   actual-use-cap)
             possible-use        (rv-fn (fn [p u] (min p u))       possible-weight possible-use-cap)
             actual-use          (rv-fn (fn [a u] (min a u))       actual-weight   actual-use-cap)
             possible-use-left   (rv-fn (fn [p u] (max (- u p) 0)) possible-weight possible-use-cap)
             actual-use-left     (rv-fn (fn [a u] (max (- u a) 0)) actual-weight   actual-use-cap)]
         (if (not= _0_ possible-use-cap)
           (alter possible-use-cap-ref (constantly possible-use-left)))
         (if (and (not= _0_ actual-use-cap)
                  (not= _0_ actual-weight))
           (alter actual-use-cap-ref   (constantly actual-use-left)))
         (if (or (not= _0_ possible-use)
                 (not= _0_ actual-use))
           (alter (get-in cache-layer use-id) conj (assoc surface-water-carrier
                                                     :possible-weight (_d possible-use mm2-per-cell)
                                                     :actual-weight   (_d actual-use   mm2-per-cell)
                                                     :sink-effects    (mapmap identity #(_d % mm2-per-cell)
                                                                              (:sink-effects surface-water-carrier)))))
         [new-possible-weight new-actual-weight])))
    [possible-weight actual-weight]))

(defn handle-sink-effects!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id actual-weight sink-caps]
  (if-let [sink-cap-ref (sink-caps current-id)]
    (dosync
     (let [sink-cap (deref sink-cap-ref)]
       (if (and (not= _0_ actual-weight)
                (not= _0_ sink-cap))
         (do
           (alter sink-cap-ref (p rv-fn (fn [a s] (max (- s a) 0)) actual-weight))
           [(rv-fn (fn [a s] (max (- a s) 0)) actual-weight sink-cap)
            {current-id (rv-fn (fn [a s] (min a s)) actual-weight sink-cap)}])
         [actual-weight {}])))
    [actual-weight {}]))

;; FIXME: Make sure carriers can hop from stream to stream as necessary.
(defn to-the-ocean!
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
        (if (rv-above? new-possible-weight trans-threshold-volume)
          (if-let [next-id (step-downstream current-id in-stream? elevation-layer rows cols bearing)]
            (assoc surface-water-carrier
              :route           (conj route next-id)
              :possible-weight new-possible-weight
              :actual-weight   new-actual-weight))))
      (let [[new-actual-weight new-sink-effects] (handle-sink-effects! current-id
                                                                       actual-weight
                                                                       sink-caps)]
        (if-let [next-id (step-downhill current-id elevation-layer rows cols bearing)]
          (assoc surface-water-carrier
            :route           (conj route next-id)
            :actual-weight   new-actual-weight
            :sink-effects    (merge-with _+_ sink-effects new-sink-effects)
            :stream-bound?   (in-stream? next-id)))))))

(def *animation-sleep-ms* 1000)

;; FIXME: This is really slow. Speed it up.
(defn run-animation [panel]
  (send-off *agent* run-animation)
  (Thread/sleep *animation-sleep-ms*)
  (doto panel (.repaint)))

(defn end-animation [panel] panel)

(defn propagate-runoff!
  "Constructs a sequence of surface-water-carrier objects (one per
   source point) and then iteratively propagates them downhill until
   they reach a stream location, get stuck in a low elevation point,
   or fall off the map bounds.  Once they reach a stream location, the
   carriers will attempt to continue downhill while staying in a
   stream course.  Sinks affect carriers overland.  Users affect
   carriers in stream channels.  All the carriers are moved together
   in timesteps (more or less)."
  [cache-layer possible-flow-layer actual-flow-layer animation?
   source-layer mm2-per-cell sink-caps possible-use-caps
   actual-use-caps in-stream? stream-intakes elevation-layer rows
   cols]
  (println "Moving the surface water carriers downhill and downstream...")
  (let [possible-flow-animator (if animation? (agent (draw-ref-layer "Possible Flow" possible-flow-layer :flow 1)))
        actual-flow-animator   (if animation? (agent (draw-ref-layer "Actual Flow"   actual-flow-layer   :flow 1)))]
    ;;    (when animation?
    ;;      (send-off possible-flow-animator run-animation)
    ;;      (send-off actual-flow-animator   run-animation))
    (with-progress-bar
      (iterate-while-seq
       (fn [surface-water-carriers]
         (println "Carriers:" (count surface-water-carriers))
         ;;(.repaint @possible-flow-animator)
         ;;(.repaint @actual-flow-animator)
         (time
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
        (filter-matrix-for-coords (p not= _0_) source-layer))))))
;;    (when animation?
;;      (send-off possible-flow-animator end-animation)
;;      (send-off actual-flow-animator   end-animation))))

(defn find-nearest-stream-point-lazily
  [in-stream? rows cols id]
  (if (in-stream? id)
    id
    (first
     (drop-while nil?
                 (map #(first (filter in-stream? %))
                      (iterate (p find-bounding-box rows cols)
                               (find-bounding-box rows cols (list id))))))))

(defn find-nearest-stream-point
  [in-stream? claimed-intakes rows cols id]
  (dosync
   (if (in-stream? id)
     id
     (loop [bounding-box (find-bounding-box rows cols (list id))]
       (if (seq bounding-box)
         (if-let [stream-point (first (filter #(and (in-stream? %)
                                                    (not (contains? @claimed-intakes %)))
                                              bounding-box))]
           (do
             (alter claimed-intakes conj stream-point)
             stream-point)
           (recur (find-bounding-box rows cols bounding-box))))))))

(defn find-nearest-stream-points
  [in-stream? rows cols use-layer]
  (println "Finding nearest stream points to all users...")
  (let [use-points      (filter-matrix-for-coords (p not= _0_) use-layer)
        claimed-intakes (ref (set (map in-stream? use-points)))
        stream-intakes  (with-progress-bar
                          (pmap (p find-nearest-stream-point in-stream? claimed-intakes rows cols)
                                use-points))]
    (dissoc (zipmap stream-intakes use-points) nil)))

(defn make-buckets
  [mm2-per-cell layer]
  (let [active-points (filter-matrix-for-coords (p not= _0_) layer)]
    (seq2map active-points (fn [id] [id (ref (*_ mm2-per-cell (get-in layer id)))]))))

(defmethod distribute-flow "SurfaceWaterMovement"
  [_ animation? cell-width cell-height source-layer sink-layer use-layer
   {stream-layer "River", elevation-layer "Altitude"}]
  (println "Running Surface Water flow model.")
  (let [rows                (get-rows source-layer)
        cols                (get-cols source-layer)
        cache-layer         (make-matrix rows cols (fn [_] (ref ())))
        possible-flow-layer (make-matrix rows cols (fn [_] (ref _0_)))
        actual-flow-layer   (make-matrix rows cols (fn [_] (ref _0_)))]
    ;;        [source-points sink-points use-points stream-points] (pmap (p filter-matrix-for-coords (p not= _0_))
    ;;                                                                   [source-layer sink-layer use-layer stream-layer])]
    ;;    (println "Source points:" (count source-points))
    ;;    (println "Sink points:  " (count sink-points))
    ;;    (println "Use points:   " (count use-points))
    ;;    (println "Stream points:" (count stream-points))
    (let [mm2-per-cell       (* cell-width cell-height 1000000)
          sink-caps          (make-buckets mm2-per-cell sink-layer)
          possible-use-caps  (make-buckets mm2-per-cell use-layer)
          actual-use-caps    (mapmap identity (& ref deref) possible-use-caps)
          in-stream?         #(not= _0_ (get-in stream-layer %))
          stream-intakes     (find-nearest-stream-points in-stream? rows cols use-layer)]
      (propagate-runoff! cache-layer
                         possible-flow-layer
                         actual-flow-layer
                         animation?
                         source-layer
                         mm2-per-cell
                         sink-caps
                         possible-use-caps
                         actual-use-caps
                         in-stream?
                         stream-intakes
                         elevation-layer
                         rows
                         cols)
      (println "Users affected:" (count (filter (& seq deref) (matrix2seq cache-layer))))
      (println "Simulation complete. Returning the cache-layer.")
      [(map-matrix (& seq deref) cache-layer)
       (map-matrix deref possible-flow-layer)
       (map-matrix deref actual-flow-layer)])))
