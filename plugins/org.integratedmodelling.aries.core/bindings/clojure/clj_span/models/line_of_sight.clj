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

(ns clj-span.models.line-of-sight
  (:use [clj-misc.utils      :only (euclidean-distance p def- between? with-progress-bar-cool with-message my-partition-all)]
        [clj-misc.matrix-ops :only (find-line-between get-line-fn)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier))

(def #^{:dynamic true} _0_)
(def #^{:dynamic true} _+_)
(def #^{:dynamic true} _-_)
(def #^{:dynamic true} _*_)
(def #^{:dynamic true} _d_)
(def #^{:dynamic true} _*)
(def #^{:dynamic true} *_)
(def #^{:dynamic true} _d)
(def #^{:dynamic true} -_)
(def #^{:dynamic true} _>_)
(def #^{:dynamic true} _max_)
(def #^{:dynamic true} rv-fn)
(def #^{:dynamic true} _>)

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
  (let [projected-elev (rv-fn '(fn [e r] (max 0.0 (+ e r))) use-elev (_* slope distance))]
    (if (_>_ slope _0_)
      ;; We are looking up, so only count the visible part of the feature.
      (let [visible-fraction (-_ 1.0 (rv-fn '(fn [p s] (if (< p s) (/ p s) 1.0))
                                            projected-elev
                                            scenic-elev))]
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
   actual-flow-layer to-meters trans-threshold source-point use-point]
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
                                     ;;(reductions _max_ slopes)
                                     (reduce #(conj %1 (_max_ (peek %1) %2))
                                             [(first slopes)]
                                             (rest slopes))))
              possible-weight (*_ distance-decay
                                  (compute-view-impact (get-in source-layer source-point)
                                                       (last elevs)
                                                       use-elev
                                                       (last sight-slopes)
                                                       (last runs)))]
          (when (_> possible-weight trans-threshold)
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
                  actual-weight (rv-fn '(fn [p s] (max 0.0 (- p s))) possible-weight (reduce _+_ _0_ (vals sink-effects)))
                  carrier       (struct-map service-carrier
                                  :source-id       source-point
                                  ;;:route           (bitpack-route (reverse (cons use-point sight-line))) ;; Temporary efficiency hack
                                  :possible-weight possible-weight
                                  :actual-weight   actual-weight
                                  :sink-effects    sink-effects)]
              (dosync
               (doseq [id (cons use-point sight-line)]
                 (commute (get-in possible-flow-layer id) _+_ possible-weight)
                 (if (not= _0_ actual-weight)
                   (commute (get-in actual-flow-layer id) _+_ actual-weight)))
               (commute (get-in cache-layer use-point) conj carrier)))))))))

(defmethod distribute-flow! "LineOfSight"
  [{:keys [source-layer sink-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points use-points cell-width cell-height
           value-type trans-threshold]}]
  (let [{elev-layer "Altitude"} flow-layers
        prob-ns (cond
                 (= value-type :numbers)  'clj-misc.numbers
                 (= value-type :varprop)  'clj-misc.varprop
                 (= value-type :randvars) 'clj-misc.randvars)]
    (binding [_0_   (var-get (ns-resolve prob-ns '_0_))
              _+_   (var-get (ns-resolve prob-ns '_+_))
              _-_   (var-get (ns-resolve prob-ns '_-_))
              _*_   (var-get (ns-resolve prob-ns '_*_))
              _d_   (var-get (ns-resolve prob-ns '_d_))
              _*    (var-get (ns-resolve prob-ns '_*))
              *_    (var-get (ns-resolve prob-ns '*_))
              _d    (var-get (ns-resolve prob-ns '_d))
              -_    (var-get (ns-resolve prob-ns '-_))
              _>_   (var-get (ns-resolve prob-ns '_>_))
              _max_ (var-get (ns-resolve prob-ns '_max_))
              rv-fn (var-get (ns-resolve prob-ns 'rv-fn))
              _>    (var-get (ns-resolve prob-ns '_>))]
      (let [num-view-lines (* (count source-points) (count use-points))
            to-meters      (fn [[i j]] [(* i cell-height) (* j cell-width)])
            partition-size 1]
        (with-message (str "Scanning " num-view-lines " view lines in chunks of size " partition-size "...\n") "\nAll done."
          (with-progress-bar-cool
            :drop
            (int (Math/ceil (/ num-view-lines partition-size)))
            (pmap (fn [view-lines]
                    (doseq [[source-point use-point] view-lines]
                      (raycast!
                       source-layer
                       sink-layer
                       elev-layer
                       cache-layer
                       possible-flow-layer
                       actual-flow-layer
                       to-meters
                       trans-threshold
                       source-point
                       use-point)))
                  (my-partition-all partition-size (for [use-point use-points source-point source-points] [source-point use-point])))))))))
