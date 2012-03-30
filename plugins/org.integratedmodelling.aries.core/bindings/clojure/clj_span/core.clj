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
;;; This namespace defines the run-span function, which is the main
;;; entry point into the SPAN system and may be called with a number
;;; of different options specifying the form of its results.

(ns clj-span.core
  (:use [clj-misc.utils      :only (p & with-message my->>)]
        [clj-misc.matrix-ops :only (map-matrix
                                    make-matrix
                                    resample-matrix
                                    matrix2seq
                                    get-rows
                                    get-cols
                                    grids-align?
                                    is-matrix?
                                    filter-matrix-for-coords)]
        [clj-span.interface  :only [provide-results]]
        [clj-span.gui        :only [draw-ref-layer run-animation end-animation]]
        [clj-span.analyzer   :only [theoretical-source
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
                                    possible-flow
                                    blocked-flow
                                    actual-flow]])
  (:require (clj-misc [numbers :as nb] [varprop :as vp] [randvars :as rv])))

(defmacro with-typed-math-syms [value-type symbols & body]
  (let [prob-ns (gensym)]
    `(let [~prob-ns (cond
                     (= ~value-type :numbers)  'clj-misc.numbers
                     (= ~value-type :varprop)  'clj-misc.varprop
                     (= ~value-type :randvars) 'clj-misc.randvars)]
       ~@(for [sym symbols] `(alter-var-root (var ~sym) (constantly (var-get (ns-resolve ~prob-ns '~sym)))))
       ~@body)))

(defstruct service-carrier
  :source-id      ; starting id of this flow path
  :route          ; byte array of directions from source-id to use-id or nil
  :possible-weight; amount of source-weight which reaches (and is used by) this use location disregarding sink-effects
  :actual-weight  ; amount of source-weight which reaches (and is used by) this use location including sink-effects
  :sink-effects   ; map of sink-ids to sink-effects on this flow path (decayed as necessary)
  :use-effects)   ; map of use-ids to rival use-effects on this flow path (decayed as necessary)

(defmulti distribute-flow!
  ;; "Creates a network of interconnected locations, and starts a
  ;;  service-carrier propagating in every location whose source value is
  ;;  greater than 0.  These carriers propagate child carriers through
  ;;  the network which collect information about the routes traveled and
  ;;  the service weight transmitted along these routes.  Over the course
  ;;  of the simulation, this function will update the passed in
  ;;  cache-layer, possible-flow-layer, and actual-flow-layer.  Its
  ;;  return result is ignored."
  :flow-model)

(defmethod distribute-flow! :default
  [{:keys [flow-model]}]
  (throw (Exception. (str "distribute-flow! is undefined for flow-model " flow-model))))

;; Pull in model namespaces that use service-carrier and distribute-flow!
(require '(clj-span.models carbon
                           proximity
                           line-of-sight
                           surface-water
                           subsistence-fisheries
                           coastal-storm-protection
                           flood-water
                           sediment))

(defn generate-results-map
  "Return the simulation results as a map of layer names to closures."
  [{:keys [value-type orig-rows orig-cols]
    :as params}]
  (let [rv-intensive-sampler (cond
                              (= value-type :numbers)  nb/rv-intensive-sampler
                              (= value-type :varprop)  vp/rv-intensive-sampler
                              (= value-type :randvars) rv/rv-intensive-sampler)]
    (with-message
      "Generating result maps...\n"
      "Finished generating result maps."
      (apply array-map
             (mapcat (fn [[label f]]
                       (with-message (str "Producing " label "...") "done"
                         (let [layer (f params)]
                           [label #(resample-matrix orig-rows orig-cols rv-intensive-sampler layer)])))
                     (array-map
                      "Source - Theoretical"  theoretical-source
                      "Source - Inaccessible" inaccessible-source
                      "Source - Possible"     possible-source
                      "Source - Blocked"      blocked-source
                      "Source - Actual"       actual-source
                      "Sink   - Theoretical"  theoretical-sink
                      "Sink   - Inaccessible" inaccessible-sink
                      "Sink   - Actual"       actual-sink
                      "Use    - Theoretical"  theoretical-use
                      "Use    - Inaccessible" inaccessible-use
                      "Use    - Possible"     possible-use
                      "Use    - Blocked"      blocked-use
                      "Use    - Actual"       actual-use
                      "Flow   - Possible"     possible-flow
                      "Flow   - Blocked"      blocked-flow
                      "Flow   - Actual"       actual-flow))))))

(defn deref-result-layers
  [{:keys [cache-layer possible-flow-layer actual-flow-layer]
    :as params}]
  (assoc params
    :cache-layer         (map-matrix (& seq deref) cache-layer)
    :possible-flow-layer (map-matrix deref possible-flow-layer)
    :actual-flow-layer   (map-matrix deref actual-flow-layer)))

(defmacro with-animation
  [value-type possible-flow-layer actual-flow-layer & body]
  (let [f (gensym)]
    `(let [[rows# cols#]           (map (fn [~f] (~f ~possible-flow-layer)) [get-rows get-cols])
           animation-pixel-size#   (quot 600 (max rows# cols#))
           possible-flow-animator# (agent (draw-ref-layer "Possible Flow"
                                                          ~possible-flow-layer
                                                          animation-pixel-size#
                                                          ~value-type))
           actual-flow-animator#   (agent (draw-ref-layer "Actual Flow"
                                                          ~actual-flow-layer
                                                          animation-pixel-size#
                                                          ~value-type))]
       (send-off possible-flow-animator# run-animation)
       (send-off actual-flow-animator#   run-animation)
       (let [result# ~@body]
         (send-off possible-flow-animator# end-animation)
         (send-off actual-flow-animator#   end-animation)
         result#))))

(defn count-affected-users
  [{:keys [cache-layer]}]
  (count (filter (& seq deref) (matrix2seq cache-layer))))

(defn run-simulation
  [{:keys [flow-model source-points use-points animation?
           value-type possible-flow-layer actual-flow-layer]
    :as params}]
  (with-message
    (str "\nRunning " flow-model " flow model...\n")
    #(str "Simulation complete.\nUsers affected: " (count-affected-users %))
    (if (and (seq source-points)
             (seq use-points))
      (if animation?
        (with-animation value-type possible-flow-layer actual-flow-layer (distribute-flow! params))
        (distribute-flow! params))
      (println "Either source or use is zero everywhere. Therefore, there can be no service flow."))
    params))

(defn create-simulation-inputs
  [{:keys [source-layer sink-layer use-layer rows cols value-type]
    :as params}]
  (with-message
    "\nCreating simulation inputs...\n"
    #(str "Source points: " (count (:source-points %)) "\n"
          "Sink points:   " (count (:sink-points   %)) "\n"
          "Use points:    " (count (:use-points    %)))
    (let [_0_ (cond
               (= value-type :numbers)  nb/_0_
               (= value-type :varprop)  vp/_0_
               (= value-type :randvars) rv/_0_)]
      (assoc params
        :source-points       (filter-matrix-for-coords (p not= _0_) source-layer)
        :sink-points         (filter-matrix-for-coords (p not= _0_) sink-layer)
        :use-points          (filter-matrix-for-coords (p not= _0_) use-layer)
        :cache-layer         (make-matrix rows cols (fn [_] (ref ())))
        :possible-flow-layer (make-matrix rows cols (fn [_] (ref _0_)))
        :actual-flow-layer   (make-matrix rows cols (fn [_] (ref _0_)))))))

(defn zero-layer-below-threshold
  "Takes a two dimensional array of RVs and replaces all values which
   have a >50% likelihood of being below the threshold with _0_."
  [value-type threshold layer]
  (with-message
    (str "Zeroing layer below " threshold "...\n")
    #(format "  Distinct Layer Values: [Pre] %d [Post] %d"
             (count (distinct (matrix2seq layer)))
             (count (distinct (matrix2seq %))))
    (let [[_< _0_] (cond
                    (= value-type :numbers)  [nb/_< nb/_0_]
                    (= value-type :varprop)  [vp/_< vp/_0_]
                    (= value-type :randvars) [rv/_< rv/_0_])]
      (map-matrix #(if (_< % threshold) _0_ %) layer))))

(defn resample-and-zero
  [value-type scaled-rows scaled-cols layer threshold]
  (let [[rv-intensive-sampler _0_] (cond
                                    (= value-type :numbers)  [nb/rv-intensive-sampler nb/_0_]
                                    (= value-type :varprop)  [vp/rv-intensive-sampler vp/_0_]
                                    (= value-type :randvars) [rv/rv-intensive-sampler rv/_0_])]
    (cond (nil? layer)     (make-matrix scaled-rows scaled-cols (constantly _0_))
          (nil? threshold) (resample-matrix scaled-rows scaled-cols rv-intensive-sampler layer)
          :otherwise       (zero-layer-below-threshold value-type
                                                       threshold
                                                       (resample-matrix scaled-rows scaled-cols rv-intensive-sampler layer)))))

(defn preprocess-data-layers
  "Preprocess data layers (downsampling and zeroing below their thresholds)."
  [{:keys [source-layer sink-layer use-layer flow-layers
           source-threshold sink-threshold use-threshold
           cell-width cell-height downscaling-factor value-type]
    :as params}]
  (println "Preprocessing the input data layers.")
  (let [[rows cols] (map #(% source-layer) [get-rows get-cols])
        scaled-rows (quot rows downscaling-factor)
        scaled-cols (quot cols downscaling-factor)
        r-and-z     (p resample-and-zero value-type scaled-rows scaled-cols)]
    (assoc params
      :orig-rows    rows
      :orig-cols    cols
      :rows         scaled-rows
      :cols         scaled-cols
      :cell-width   (* cell-width  (/ scaled-cols cols))
      :cell-height  (* cell-height (/ scaled-rows rows))
      :source-layer (r-and-z source-layer source-threshold)
      :sink-layer   (r-and-z sink-layer   sink-threshold)
      :use-layer    (r-and-z use-layer    use-threshold)
      :flow-layers  (into {} (for [[name layer] flow-layers] [name (r-and-z layer nil)])))))

(def double>0?         #(and (float?   %) (pos? %)))
(def nil-or-double>=0? #(or  (nil?     %) (and (float? %) (>= % 0))))
(def integer>=1?       #(and (integer? %) (>= % 1)))
(def number>=1?        #(and (number?  %) (>= % 1)))
(def nil-or-matrix?    #(or  (nil?     %) (is-matrix? %)))

(defn verify-params-or-throw
  [{:keys [source-layer sink-layer use-layer flow-layers
           source-threshold sink-threshold use-threshold trans-threshold
           cell-width cell-height rv-max-states downscaling-factor
           source-type sink-type use-type benefit-type
           value-type flow-model animation? result-type]
    :as params}]
  {:pre [(every? is-matrix? [source-layer use-layer])
         (every? nil-or-matrix? (cons sink-layer (vals flow-layers)))
         (apply grids-align? (remove nil? (list* source-layer sink-layer use-layer (vals flow-layers))))
         (every? nil-or-double>=0? [source-threshold sink-threshold use-threshold])
         (every? double>0? [trans-threshold cell-width cell-height])
         (integer>=1? rv-max-states)
         (number>=1? downscaling-factor)
         (every? #{:finite :infinite} [source-type use-type])
         (contains? #{:finite :infinite nil} sink-type)
         (contains? #{:rival :non-rival} benefit-type)
         (contains? #{:randvars :varprop :numbers} value-type)
         (contains? #{"LineOfSight"
                      "Proximity"
                      "CO2Removed"
                      "FloodWaterMovement"
                      "SurfaceWaterMovement"
                      "SedimentTransport"
                      "CoastalStormMovement"
                      "SubsistenceFishAccessibility"}
                    flow-model)
         (contains? #{:cli-menu :closure-map} result-type)
         (contains? #{true false nil} animation?)]}
  params)

(defn set-global-vars!
  [{:keys [value-type rv-max-states]}]
  (if (and (= value-type :randvars)
           (integer>=1? rv-max-states))
    (rv/reset-rv-max-states! rv-max-states)))

(defn run-span
  "Run a flow model and return the results."
  [{:keys [result-type value-type source-layer
           sink-layer use-layer flow-layers]
    :as params}]
  (set-global-vars! params)
  (my->> params
         verify-params-or-throw
         preprocess-data-layers
         create-simulation-inputs
         run-simulation
         deref-result-layers
         generate-results-map
         (provide-results result-type value-type source-layer sink-layer use-layer flow-layers)))
