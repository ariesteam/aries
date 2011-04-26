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
;;; This namespace defines the run-span, generate-results-map, and
;;; preprocess-data-layers functions.  run-span is the main entry
;;; point into the SPAN system and may be called with a number of
;;; different options specifying the form of its results.

(ns clj-span.core
  (:use [clj-misc.utils          :only (seq2map constraints-1.0 p &)]
        [clj-span.params         :only (set-global-params!)]
        [clj-span.interface      :only (provide-results)]
        [clj-span.gui            :only (draw-ref-layer run-animation end-animation)]
        [clj-misc.randvars       :only (_0_ rv-below? rv-intensive-sampler)]
        [clj-misc.matrix-ops     :only (map-matrix
                                        make-matrix
                                        resample-matrix
                                        matrix2seq
                                        get-rows
                                        get-cols
                                        grids-align?
                                        is-matrix?
                                        filter-matrix-for-coords)]
        [clj-span.analyzer       :only (theoretical-source
                                        inaccessible-source
                                        possible-source
                                        blocked-source
                                        actual-source
                                        theoretical-sink
                                        inaccessible-sink
                                        actual-sink
                                        theoretical-use
                                        inaccessible-use
                                        possible-use
                                        blocked-use
                                        actual-use
                                        blocked-flow)]))

(defn zero-layer-below-threshold
  "Takes a two dimensional array of RVs and replaces all values which
   have a >50% likelihood of being below the threshold with _0_."
  [threshold layer]
  (println (str "Zeroing layer below " threshold "..."))
  (let [result (map-matrix #(if (rv-below? % threshold) _0_ %) layer)]
    (printf "  Distinct Layer Values: [Pre] %d [Post] %d\n"
            (count (distinct (matrix2seq layer)))
            (count (distinct (matrix2seq result))))
    result))

(defn preprocess-data-layers
  "Preprocess data layers (downsampling and zeroing below their thresholds)."
  [source-layer source-threshold sink-layer sink-threshold use-layer use-threshold flow-layers downscaling-factor]
  (println "Preprocessing the input data layers.")
  (let [rows             (get-rows source-layer)
        cols             (get-cols source-layer)
        scaled-rows      (int (/ rows downscaling-factor))
        scaled-cols      (int (/ cols downscaling-factor))
        row-scale-factor (float (/ scaled-rows rows))
        col-scale-factor (float (/ scaled-cols cols))
        preprocess-layer (fn [l t] (if l
                                     (let [scaled-layer (resample-matrix scaled-rows scaled-cols rv-intensive-sampler l)]
                                       (if t
                                         (zero-layer-below-threshold t scaled-layer)
                                         scaled-layer))
                                     (make-matrix scaled-rows scaled-cols (constantly _0_))))
        [scaled-source-layer scaled-sink-layer scaled-use-layer] (map preprocess-layer
                                                                      [source-layer     sink-layer     use-layer]
                                                                      [source-threshold sink-threshold use-threshold])
        scaled-flow-layers (seq2map flow-layers (fn [[name layer]]
                                                  [name (resample-matrix
                                                         scaled-rows
                                                         scaled-cols
                                                         rv-intensive-sampler
                                                         layer)]))]
    [scaled-source-layer scaled-sink-layer scaled-use-layer scaled-flow-layers row-scale-factor col-scale-factor]))

(defstruct service-carrier
  :source-id      ; starting id of this flow path
  :route          ; byte array of directions from source-id to use-id or nil
  :possible-weight; amount of source-weight which reaches (and is used by) this use location disregarding sink-effects
  :actual-weight  ; amount of source-weight which reaches (and is used by) this use location including sink-effects
  :sink-effects   ; map of sink-ids to sink-effects on this flow path (decayed as necessary)
  :use-effects)   ; map of use-ids to rival use-effects on this flow path (decayed as necessary)

(defmulti distribute-flow!
  ;;"Creates a network of interconnected locations, and starts a
  ;; service-carrier propagating in every location whose source value is
  ;; greater than 0.  These carriers propagate child carriers through
  ;; the network which collect information about the routes traveled and
  ;; the service weight transmitted along these routes.  Over the course
  ;; of the simulation, this function will update the passed in
  ;; cache-layer, possible-flow-layer, and actual-flow-layer.  Its
  ;; return result is ignored."
  (fn [flow-model cell-width cell-height rows cols
       cache-layer possible-flow-layer actual-flow-layer
       source-layer sink-layer use-layer source-points
       sink-points use-points flow-layers] flow-model))

(defmethod distribute-flow! :default
  [flow-model _ _ _ _ _ _ _ _ _ _ _ _ _ _]
  (throw (Exception. (str "distribute-flow! is undefined for flow type: " flow-model))))

(defn run-simulation
  [flow-model animation? cell-width cell-height source-layer sink-layer use-layer flow-layers]
  (println "\nRunning" flow-model "flow model.")
  (let [rows                (get-rows source-layer)
        cols                (get-cols source-layer)
        cache-layer         (make-matrix rows cols (fn [_] (ref ())))
        possible-flow-layer (make-matrix rows cols (fn [_] (ref _0_)))
        actual-flow-layer   (make-matrix rows cols (fn [_] (ref _0_)))
        source-points       (filter-matrix-for-coords (p not= _0_) source-layer)
        sink-points         (filter-matrix-for-coords (p not= _0_) sink-layer)
        use-points          (filter-matrix-for-coords (p not= _0_) use-layer)]
    (println "Source points:" (count source-points))
    (println "Sink points:  " (count sink-points))
    (println "Use points:   " (count use-points))
    (if (and (seq source-points) (seq use-points))
      (let [animation-pixel-size   (Math/round (/ 600.0 (max rows cols)))
            possible-flow-animator (if animation? (agent (draw-ref-layer "Possible Flow"
                                                                         possible-flow-layer
                                                                         :pflow
                                                                         animation-pixel-size)))
            actual-flow-animator   (if animation? (agent (draw-ref-layer "Actual Flow"
                                                                         actual-flow-layer
                                                                         :aflow
                                                                         animation-pixel-size)))]
        (when animation?
          (send-off possible-flow-animator run-animation)
          (send-off actual-flow-animator   run-animation))
        (distribute-flow! flow-model
                          cell-width
                          cell-height
                          rows
                          cols
                          cache-layer
                          possible-flow-layer
                          actual-flow-layer
                          source-layer
                          sink-layer
                          use-layer
                          source-points
                          sink-points
                          use-points
                          flow-layers)
        (when animation?
          (send-off possible-flow-animator end-animation)
          (send-off actual-flow-animator   end-animation))
        (println "Simulation complete.\nUsers affected:" (count (filter (& seq deref) (matrix2seq cache-layer)))))
      ;; Either source or use points are lacking, so no service is provided.
      ;; Note that in this case, the final results will show up like so:
      ;;
      ;; Theoretical Source = Inaccessible Source
      ;; Theoretical Sink = Inaccessible Sink
      ;; Theoretical Use = Inaccessible Use
      ;;
      ;; All of the remaining maps will be _0_ everywhere:
      ;;
      ;; Possible/Actual/Blocked Source
      ;; Actual Sink
      ;; Possible/Actual/Blocked Use
      ;; Possible/Actual/Blocked Flow
      (println "Either source or use is zero everywhere. Therefore, there can be no service flow."))
    [(map-matrix (& seq deref) cache-layer)
     (map-matrix deref possible-flow-layer)
     (map-matrix deref actual-flow-layer)]))

(defn generate-results-map
  "Run flow model and return the results as a map of layer names to closures."
  [flow-model animation? orig-rows orig-cols cell-width cell-height
   scaled-source-layer scaled-sink-layer scaled-use-layer scaled-flow-layers
   row-scale-factor col-scale-factor]
  (let [[cache-layer possible-flow-layer actual-flow-layer] (run-simulation flow-model
                                                                            animation?
                                                                            (* cell-width  col-scale-factor)
                                                                            (* cell-height row-scale-factor)
                                                                            scaled-source-layer
                                                                            scaled-sink-layer
                                                                            scaled-use-layer
                                                                            scaled-flow-layers)]
    (apply array-map
           (mapcat (fn [[name f]] [name (& (p resample-matrix orig-rows orig-cols rv-intensive-sampler) f)])
                   (array-map
                    "Source - Theoretical"  #(theoretical-source  scaled-source-layer scaled-use-layer)
                    "Source - Inaccessible" #(inaccessible-source scaled-source-layer scaled-use-layer cache-layer)
                    "Source - Possible"     #(possible-source     cache-layer)
                    "Source - Blocked"      #(blocked-source      cache-layer)
                    "Source - Actual"       #(actual-source       cache-layer)
                    "Sink   - Theoretical"  #(theoretical-sink    scaled-source-layer scaled-sink-layer scaled-use-layer)
                    "Sink   - Inaccessible" #(inaccessible-sink   scaled-source-layer scaled-sink-layer scaled-use-layer cache-layer)
                    "Sink   - Actual"       #(actual-sink         cache-layer)
                    "Use    - Theoretical"  #(theoretical-use     scaled-source-layer scaled-use-layer)
                    "Use    - Inaccessible" #(inaccessible-use    scaled-source-layer scaled-use-layer cache-layer)
                    "Use    - Possible"     #(possible-use        cache-layer)
                    "Use    - Blocked"      #(blocked-use         cache-layer)
                    "Use    - Actual"       #(actual-use          cache-layer)
                    "Flow   - Possible"     (constantly possible-flow-layer)
                    "Flow   - Blocked"      #(blocked-flow possible-flow-layer actual-flow-layer)
                    "Flow   - Actual"       (constantly actual-flow-layer))))))

(def double>0?         #(and (float?   %) (pos? %)))
(def nil-or-double>=0? #(or  (nil?     %) (and (float? %) (>= % 0))))
(def integer>=1?       #(and (integer? %) (>= % 1)))
(def number>=1?        #(and (number?  %) (>= % 1)))
(def nil-or-matrix?    #(or  (nil?     %) (is-matrix? %)))

(defn run-span
  [{:keys [source-layer source-threshold sink-layer    sink-threshold
           use-layer    use-threshold    flow-layers   trans-threshold
           cell-width   cell-height      rv-max-states downscaling-factor
           source-type  sink-type        use-type      benefit-type
           flow-model   result-type      animation?]
    :or {rv-max-states      10
         downscaling-factor 1}}]
  ;; Validate the inputs
  (constraints-1.0
   {:pre [(every? is-matrix?        [source-layer use-layer])
          (every? nil-or-matrix?    (cons sink-layer (vals flow-layers)))
          (apply  grids-align?      (remove nil? (list* source-layer sink-layer use-layer (vals flow-layers))))
          (every? nil-or-double>=0? [source-threshold sink-threshold use-threshold])
          (every? double>0?         [trans-threshold cell-width cell-height])
          (integer>=1? rv-max-states)
          (number>=1?  downscaling-factor)
          (every? #{:finite :infinite} [source-type use-type])
          (contains? #{:finite :infinite nil} sink-type)
          (contains? #{:rival :non-rival} benefit-type)
          (contains? #{"LineOfSight"
                       "Proximity"
                       "CO2Removed"
                       "FloodWaterMovement"
                       "SurfaceWaterMovement"
                       "Sediment"
                       "CoastalStormMovement"
                       "SubsistenceFishAccessibility"}
                     flow-model)
          (contains? #{:cli-menu :closure-map} result-type)
          (contains? #{true false nil} animation?)]})
  ;; Initialize global parameters
  (set-global-params! {:rv-max-states      rv-max-states
                       :trans-threshold    trans-threshold
                       :source-type        source-type
                       :sink-type          sink-type
                       :use-type           use-type
                       :benefit-type       benefit-type})
  ;; Run flow model and return the results
  (provide-results result-type
                   source-layer
                   sink-layer
                   use-layer
                   flow-layers
                   (apply generate-results-map
                          flow-model
                          animation?
                          (get-rows source-layer)
                          (get-cols source-layer)
                          cell-width
                          cell-height
                          (preprocess-data-layers source-layer source-threshold
                                                  sink-layer   sink-threshold
                                                  use-layer    use-threshold
                                                  flow-layers  downscaling-factor))))

;; Now we can import our SPAN model implementations.
(require '(clj-span carbon-model
                    proximity-model
                    line-of-sight-model
                    surface-water-model
                    subsistence-fisheries-model
                    coastal-storm-protection-model
                    flood-water-model
                    sediment-model))
