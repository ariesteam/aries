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

(ns clj-span.sediment-model
  (:use [clj-span.params        :only (*trans-threshold*)]
        [clj-span.model-api     :only (distribute-flow service-carrier)]
        [clj-misc.utils         :only (mapmap seq2map seq2redundant-map euclidean-distance square-distance def- p &)]
        [clj-misc.matrix-ops    :only (get-rows get-cols make-matrix filter-matrix-for-coords in-bounds? find-line-between map-matrix)]
        [clj-misc.randvars      :only (_0_ _+_ _-_ _*_ _d_ -_ _* _d rv-min rv-mean rv-cdf-lookup)]))

(def hydrosheds-delta-codes {{  1.0 1.0} [ 0  1]    ; e
                             {  2.0 1.0} [-1  1]    ; se
                             {  4.0 1.0} [-1  0]    ; s
                             {  8.0 1.0} [-1 -1]    ; sw
                             { 16.0 1.0} [ 0 -1]    ; w
                             { 32.0 1.0} [ 1 -1]    ; nw
                             { 64.0 1.0} [ 1  0]    ; n
                             {128.0 1.0} [ 1  1]}); ne

;; try out (vecfoo (floor (/ (atan2 x y) 45))) or approximate by checking quadrant of vector-direction
(defn aggregate-flow-dirs
  [hydrocodes]
  (if-let [exit-code (some #{{-1.0 1.0} {0.0 1.0}} hydrocodes)]
    exit-code
    (let [vector-direction (reduce (p map +) (map hydrosheds-delta-codes hydrocodes))]
      (if (= vector-direction [0 0])
        {-1.0 1.0} ; since we don't know where to go, we'll just terminate this path by saying we hit an inland sink
        (let [vector-magnitude  (euclidean-distance [0 0] vector-direction)
              unit-vector       (map #(/ % vector-magnitude) vector-direction)
              distances-to-dirs (seq2map hydrosheds-delta-codes (fn [[code v]] [(square-distance unit-vector v) code]))]
          (distances-to-dirs (apply min (keys distances-to-dirs))))))))

(defn- move-points-into-stream-channel
  "Returns a map of in-stream-ids to lists of the out-of-stream-ids
   that were shifted into this position."
  [hydrosheds-layer stream-layer data-points]
  (seq2redundant-map data-points
                     #(loop [id %]
                        (if (not= _0_ (get-in stream-layer id))
                          ;; in-stream
                          [(vec id) %]
                          ;; not in-stream
                          (let [flow-delta (hydrosheds-delta-codes (get-in hydrosheds-layer id))]
                            (recur (map + id flow-delta)))))
                     conj))

(defn- scale-by-stream-proximity
  [floodplain-layer elevation-layer in-stream-map data-layer]
  (into {}
        (remove nil?
                (for [in-stream-id (keys in-stream-map) data-id (in-stream-map in-stream-id)]
                  (if (= in-stream-id data-id)
                    ;; location is already in-stream, activation is 100%
                    [data-id (atom (get-in data-layer data-id))]
                    ;; location is out-of-stream, activation is scaled by the
                    ;; relative elevation difference between this location,
                    ;; the in-stream proxy location, and the nearest
                    ;; floodplain boundary
                    (let [loc-delta       (map - data-id in-stream-id)
                          outside-id      (first (drop-while #(not= _0_ (get-in floodplain-layer %))
                                                             (rest (iterate (p map + loc-delta) data-id))))
                          inside-id       (map - outside-id loc-delta)
                          boundary-id     (first (drop-while #(not= _0_ (get-in floodplain-layer %))
                                                             (find-line-between inside-id outside-id)))
                          rise            (_-_ (get-in elevation-layer boundary-id)
                                               (get-in elevation-layer in-stream-id))
                          run-to-boundary (euclidean-distance in-stream-id boundary-id)
                          run-to-data     (euclidean-distance in-stream-id data-id)
                          slope           (_d rise run-to-boundary)
                          elev-limit      (_+_ (_* slope run-to-data) (get-in elevation-layer in-stream-id))]
                      (if (< (rv-mean (get-in elevation-layer data-id)) (rv-mean elev-limit))
                        (let [activation-factor (- 1 (/ run-to-data run-to-boundary))]
                          [data-id (atom (_* (get-in data-layer data-id) activation-factor))]))))))))

(defn- step-downstream!
  "Computes the state of the sediment-carrier after it takes another
   step downstream.  If it encounters a sink location, it drops some
   sediment according to the remaining sink capacity at this location.
   If there are also users present, a service-carrier is stored in the
   user's carrier-cache."
  [cache-layer hydrosheds-layer sink-map use-map scaled-sinks rows cols
   [current-id source-ids source-fractions incoming-utilities sink-effects]]
  (let [flow-delta (hydrosheds-delta-codes (get-in hydrosheds-layer current-id))]
    (when flow-delta ; if nil, we've hit 0 (ocean) or -1 (inland sink)
      (let [new-id (map + current-id flow-delta)]
        (when (in-bounds? rows cols new-id) ; otherwise, we've gone off the map
          (let [affected-sinks     (filter (& (p not= _0_) deref scaled-sinks) (sink-map new-id))
                total-incoming     (reduce _+_ incoming-utilities)
                sink-effects       (if (seq affected-sinks) ; sinks encountered
                                     (merge sink-effects
                                            (let [total-sink-cap (reduce _+_ (map (& deref scaled-sinks) affected-sinks))
                                                  sink-fraction  (_d_ total-incoming total-sink-cap)]
                                              (seq2map affected-sinks
                                                       #(let [sink-cap @(scaled-sinks %)]
                                                          [% (rv-min sink-cap (_*_ sink-cap sink-fraction))]))))
                                     sink-effects)
                outgoing-utilities (if (seq affected-sinks) ; sinks encountered
                                     (do
                                       (doseq [sink-id affected-sinks]
                                         (swap! (scaled-sinks sink-id) _-_ (sink-effects sink-id)))
                                       (let [total-sunk   (reduce _+_ (map sink-effects affected-sinks))
                                             out-fraction (-_ 1 (_d_ total-sunk total-incoming))]
                                         (map (p _*_ out-fraction) incoming-utilities)))
                                     incoming-utilities)]
            (doseq [cache (map (p get-in cache-layer) (if (seq affected-sinks) (use-map new-id)))]
              (swap! cache concat
                     (map (fn [sid sfrac utility]
                            (struct-map service-carrier
                              :source-id       sid
                              :route           nil
                              :possible-weight _0_
                              :actual-weight   utility
                              :sink-effects    (mapmap identity (p _*_ sfrac) sink-effects)))
                          source-ids
                          source-fractions
                          incoming-utilities)))
            (when (< (rv-cdf-lookup (reduce _+_ _0_ outgoing-utilities) *trans-threshold*) 0.5)
              [new-id source-ids source-fractions outgoing-utilities sink-effects])))))))

(defstruct sediment-carrier :in-stream-id :source-ids :source-fractions :source-values :sink-effects)

(defn- distribute-downstream!
  "Constructs a sequence of sediment-carrier objects (one per
   in-stream source id) and then iteratively computes the next-step
   downstream sediment-carriers from the previous until they no longer
   have any sediment, fall off the map bounds, or hit an inland sink.
   All the carriers are moved together in timesteps (more or less)."
  [cache-layer hydrosheds-layer source-map sink-map use-map scaled-sources scaled-sinks rows cols]
  (dorun (take-while seq (iterate
                          (fn [sediment-carriers]
                            (remove nil?
                                    (pmap (p step-downstream!
                                             cache-layer
                                             hydrosheds-layer
                                             sink-map
                                             use-map
                                             scaled-sinks
                                             rows cols)
                                          sediment-carriers)))
                          (pmap
                           (fn [in-stream-id]
                             (let [source-ids       (source-map in-stream-id)
                                   source-values    (map (& deref scaled-sources) source-ids)
                                   total-source     (reduce _+_ source-values)
                                   source-fractions (map #(_d_ % total-source) source-values)]
;;                               (struct-map sediment-carrier
;;                                   :in-stream-id     in-stream-id
;;                                   :source-ids       source-ids
;;                                   :source-fractions source-fractions
;;                                   :source-values    source-values
;;                                   :sink-effects     {})
                               [in-stream-id source-ids source-fractions source-values {}]))
                           (keys source-map))))))

;; Model updates due to conversation with Ken on 10/15/10:
;;
;; Three classes of sinks: floodplains, dams/reservoirs, water intakes
;; Two class of beneficiaries: farmers in floodplains, avoided turbidity beneficiaries (dams and water intakes)
;;
;; In order to distinguish their behavior in this model, I need layers
;; for the presence/absence of floodplains, dams/reservoirs, water
;; intakes, and farmers passed as flow dependencies in the span
;; statement.
;;

(defmethod distribute-flow "Sediment"
  [_ source-layer sink-layer use-layer
   {hydrosheds-layer "Hydrosheds", stream-layer "RiverStream",
    floodplain-layer "FloodPlainPresence", elevation-layer "Altitude"}]
  (println "Running Sediment flow model.")
  (let [rows        (get-rows source-layer)
        cols        (get-cols source-layer)
        cache-layer (make-matrix rows cols (constantly (atom ())))
        [source-map sink-map use-map] (pmap (& (p move-points-into-stream-channel hydrosheds-layer stream-layer)
                                               (p filter-matrix-for-coords (p not= _0_)))
                                            [source-layer sink-layer use-layer])
        [scaled-sources scaled-sinks] (pmap (p scale-by-stream-proximity floodplain-layer elevation-layer)
                                            [source-map   sink-map]
                                            [source-layer sink-layer])]
    (println "Source points:" (count (concat (vals source-map))))
    (println "Sink points:  " (count (concat (vals sink-map))))
    (println "Use points:   " (count (concat (vals use-map))))
    (distribute-downstream! cache-layer hydrosheds-layer source-map sink-map use-map scaled-sources scaled-sinks rows cols)
    (map-matrix (& seq deref) cache-layer)))
