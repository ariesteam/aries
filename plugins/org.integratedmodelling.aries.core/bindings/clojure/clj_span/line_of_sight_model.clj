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
;;; This namespace defines the line-of-sight model.
;;;
;;; * Routes run from Source to Use
;;;
;;; * Contain positive utility values (total decayed source values)
;;;
;;; * Projects a line of sight from each user to source point
;;;
;;; * Weights utility by amount of visible height of source above view
;;;   line
;;;
;;; * Sink effects are computed as the decayed negative utility (sink
;;;   value) received from any sink location along a view path to a
;;;   source point.

(ns clj-span.line-of-sight-model
  (:use [clj-span.params     :only (*trans-threshold*)]
        [clj-misc.utils      :only (euclidean-distance p & def- between? with-progress-bar-cool)]
        [clj-misc.matrix-ops :only (make-matrix
                                    map-matrix
                                    matrix2seq
                                    get-rows
                                    get-cols
                                    filter-matrix-for-coords
                                    find-line-between
                                    get-line-fn)]
        [clj-misc.randvars   :only (_0_ _+_ _-_ _*_ _d_ _* *_ _d -_ _>_ rv-max rv-pos rv-above?)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-span.gui        :only (draw-ref-layer)]))

;; in meters
(def- half-mile    805.0)
(def- mile        1610.0)
(def- _4-miles    6440.0)
(def- _5-miles    8050.0)
(def- _15-miles  24150.0)
(def- _20-miles  32200.0)
(def- _40-miles  64400.0)

(def- source-ramp-up        (get-line-fn {:slope (/  1.0  mile)      :intercept 0.0}))
(def- slow-source-decay     (get-line-fn {:slope (/ -0.25 _4-miles)  :intercept 1.0625}))
(def- fast-source-decay     (get-line-fn {:slope (/ -0.5  _15-miles) :intercept 0.9166667}))
(def- moderate-source-decay (get-line-fn {:slope (/ -0.25 _20-miles) :intercept 0.5}))

(def- slow-sink-decay (get-line-fn {:slope (/ -0.25 half-mile) :intercept 1.0}))
(def- fast-sink-decay (get-line-fn {:slope (/ -0.75 half-mile) :intercept 1.5}))

;; source decay = ramp up in 1 mile, slow decay to 5 miles, fast decay to 20 miles, moderate decay to 40 miles, then gone
(defn- source-decay
  [distance]
  (cond (> distance _40-miles)
        0.0

        (< distance mile)
        (source-ramp-up distance)

        (between? mile _5-miles distance)
        (slow-source-decay distance)

        (between? _5-miles _20-miles distance)
        (fast-source-decay distance)

        :otherwise
        (moderate-source-decay distance)))

;; sink decay = slow decay to 1/2 mile, fast decay to 1 mile, gone after 1 mile
(defn- sink-decay
  [distance]
  (cond (> distance mile)
        0.0

        (< distance half-mile)
        (slow-sink-decay distance)

        :otherwise
        (fast-sink-decay distance)))

(defn- compute-view-impact
  [scenic-value scenic-elev use-elev slope distance]
  (let [projected-elev (rv-pos (_+_ use-elev (_* slope distance)))]
    (if (_>_ slope _0_)
      ;; We are looking up, so only count the visible part of the feature.
      (let [visible-fraction (rv-pos (-_ 1.0 (_d_ projected-elev scenic-elev)))]
        (_*_ scenic-value visible-fraction))
      ;; We are looking straight ahead or down, so count the whole
      ;; feature if we can see any part of it.
      (if (_>_ projected-elev scenic-elev)
        _0_
        scenic-value))))

(defn- raycast!
  "Finds a line of sight path between source and use points, checks
   for obstructions, and determines (using elevation info) how much of
   the source element can be seen from the use point.  A distance
   decay function is applied to the results to compute the visual
   utility originating from the source point."
  [source-layer sink-layer elev-layer cache-layer possible-flow-layer
   actual-flow-layer to-meters [source-point use-point]]
  (when (not= source-point use-point) ;; no in-situ use
    (let [use-loc-in-m    (to-meters use-point)
          source-loc-in-m (to-meters source-point)
          view-distance   (euclidean-distance use-loc-in-m source-loc-in-m)
          distance-decay  (source-decay view-distance)]
      (if (pos? distance-decay) ;; are we in range?
        (let [sight-line      (rest (find-line-between use-point source-point))
              use-elev        (get-in elev-layer use-point)
              elevs           (map (p get-in elev-layer) sight-line)
              rises           (map #(_-_ % use-elev) elevs)
              runs            (map (p euclidean-distance use-loc-in-m)
                                   (map to-meters sight-line))
              slopes          (map _d rises runs)
              sight-slopes    (cons _0_
                                    (butlast
                                     ;;(reductions rv-max slopes)
                                     (reduce #(conj %1 (rv-max (peek %1) %2))
                                             [(first slopes)]
                                             (rest slopes))))
              possible-weight (*_ distance-decay
                                  (compute-view-impact (get-in source-layer source-point)
                                                       (last elevs)
                                                       use-elev
                                                       (last sight-slopes)
                                                       (last runs)))]
          (when (rv-above? possible-weight *trans-threshold*)
            (let [sink-effects  (into {}
                                      (map #(let [sink-value (get-in sink-layer %1)]
                                              (if (not= sink-value _0_)
                                                (let [view-impact (compute-view-impact sink-value %2 use-elev %3 %4)]
                                                  (if (not= view-impact _0_)
                                                    (vector %1 (*_ %5 view-impact))))))
                                           sight-line
                                           elevs
                                           sight-slopes
                                           runs
                                           (take-while pos? (map sink-decay runs))))
                  actual-weight (rv-pos (reduce _-_ possible-weight (vals sink-effects)))
                  carrier       (struct-map service-carrier
                                  :source-id       source-point
                                  ;;:route           (bitpack-route (reverse (cons use-point sight-line))) ;; Temporary efficiency hack
                                  :possible-weight possible-weight
                                  :actual-weight   actual-weight
                                  :sink-effects    sink-effects)]
              (dosync
               (doseq [id (cons use-point sight-line)]
                 (alter (get-in possible-flow-layer id) _+_ possible-weight)
                 (if (not= _0_ actual-weight)
                   (alter (get-in actual-flow-layer id) _+_ actual-weight)))
               (alter (get-in cache-layer use-point) conj carrier)))))))))

(def *animation-sleep-ms* 100)

;; FIXME: This is really slow. Speed it up.
(defn run-animation [panel]
  (send-off *agent* run-animation)
  (Thread/sleep *animation-sleep-ms*)
  (doto panel (.repaint)))

(defn end-animation [panel] panel)

(defmethod distribute-flow "LineOfSight"
  [_ animation? cell-width cell-height source-layer sink-layer use-layer {elev-layer "Altitude"}]
  (println "\nRunning LineOfSight flow model.")
  (let [rows                   (get-rows source-layer)
        cols                   (get-cols source-layer)
        cache-layer            (make-matrix rows cols (fn [_] (ref ())))
        possible-flow-layer    (make-matrix rows cols (fn [_] (ref _0_)))
        actual-flow-layer      (make-matrix rows cols (fn [_] (ref _0_)))
        source-points          (filter-matrix-for-coords (p not= _0_) source-layer)
        use-points             (filter-matrix-for-coords (p not= _0_) use-layer)
        num-view-lines         (* (count source-points) (count use-points))
        to-meters              (fn [[i j]] [(* i cell-height) (* j cell-width)])
        animation-pixel-size   (Math/round (/ 600.0 (max rows cols)))
        possible-flow-animator (if animation? (agent (draw-ref-layer "Possible Flow"
                                                                     possible-flow-layer
                                                                     :pflow
                                                                     animation-pixel-size)))
        actual-flow-animator   (if animation? (agent (draw-ref-layer "Actual Flow"
                                                                     actual-flow-layer
                                                                     :aflow
                                                                     animation-pixel-size)))]
    (println "Source points:" (count source-points))
    (println "Use points:   " (count use-points))
    (when animation?
      (send-off possible-flow-animator run-animation)
      (send-off actual-flow-animator   run-animation))
    (println "Scanning" num-view-lines "view lines...")
    (with-progress-bar-cool
      :drop
      num-view-lines
      (pmap (p raycast!
               source-layer
               sink-layer
               elev-layer
               cache-layer
               possible-flow-layer
               actual-flow-layer
               to-meters)
            (for [source-point source-points use-point use-points]
              [source-point use-point])))
    (println "\nAll done.")
    (when animation?
      (send-off possible-flow-animator end-animation)
      (send-off actual-flow-animator   end-animation))
    (println "Users affected:" (count (filter (& seq deref) (matrix2seq cache-layer))))
    (println "Simulation complete. Returning the cache-layer.")
    [(map-matrix (& seq deref) cache-layer)
     (map-matrix deref possible-flow-layer)
     (map-matrix deref actual-flow-layer)]))
