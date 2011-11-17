(ns clj-span.repl-utils
  (:use (clj-span core commandline aries-span-bridge analyzer gui)
        (clj-misc utils matrix-ops varprop stats)
        clojure.contrib.pprint)
  (:require [clj-misc.randvars :as rv]))

(defn load-layers
  [filename]
  (let [[s k u f cell-w cell-h] (read-span-layers filename)]
    (def source-layer s)
    (def sink-layer   k)
    (def use-layer    u)
    (def flow-layers  f)
    (def cell-width   cell-w)
    (def cell-height  cell-h)
    (def rows (get-rows s))
    (def cols (get-cols s))))

(defn extract-results
  [result-map]
  (def tsrc  (let [rmap ((:theoretical-source  result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def isrc  (let [rmap ((:inaccessible-source result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def psrc  (let [rmap ((:possible-source     result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def bsrc  (let [rmap ((:blocked-source      result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def asrc  (let [rmap ((:actual-source       result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def tsnk  (let [rmap ((:theoretical-sink    result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def isnk  (let [rmap ((:inaccessible-sink   result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def asnk  (let [rmap ((:actual-sink         result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def tuse  (let [rmap ((:theoretical-use     result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def iuse  (let [rmap ((:inaccessible-use    result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def puse  (let [rmap ((:possible-use        result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def buse  (let [rmap ((:blocked-use         result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def ause  (let [rmap ((:actual-use          result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def pflow (let [rmap ((:possible-flow       result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def bflow (let [rmap ((:blocked-flow        result-map))] (make-matrix rows cols #(get rmap % _0_))))
  (def aflow (let [rmap ((:actual-flow         result-map))] (make-matrix rows cols #(get rmap % _0_)))))

(defn test-run-sediment
  []
  (run-span {:flow-model         "SedimentTransport"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   1000.0
             :sink-threshold     500.0
             :use-threshold      0.0
             :trans-threshold    100.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :infinite
             :benefit-type       :rival ;; or :non-rival for turbidity
             :downscaling-factor 3
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-flood
  []
  (run-span {:flow-model         "FloodWaterMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   50.0
             :sink-threshold     3000.0
             :use-threshold      0.0
             :trans-threshold    5.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :infinite
             :benefit-type       :non-rival
             :downscaling-factor 3
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-storm
  []
  (run-span {:flow-model         "CoastalStormMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
             :result-type        :closure-map}))

(defn test-run-fishing
  []
  (run-span {:flow-model         "SubsistenceFishAccessibility"
             :source-layer       source-layer
             :sink-layer         nil
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     nil
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          nil
             :use-type           :finite
             :benefit-type       :rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-water
  []
  (run-span {:flow-model         "SurfaceWaterMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   nil ;; 1500.0
             :sink-threshold     nil ;; 300.0
             :use-threshold      nil ;; 500.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          :finite
             :use-type           :finite
             :benefit-type       :rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-carbon
  []
  (run-span {:flow-model         "CO2Removed"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          :finite
             :use-type           :finite
             :benefit-type       :rival
             :downscaling-factor 20
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-view
  []
  (run-span {:flow-model         "LineOfSight"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   25.0
             :sink-threshold     25.0
             :use-threshold      0.2
             :trans-threshold    1.0
             :source-type        :infinite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         :varprop
             :downscaling-factor 4
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-proximity
  []
  (run-span {:flow-model         "Proximity"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   40.0
             :sink-threshold     0.0
             :use-threshold      0.2
             :trans-threshold    1.0
             :source-type        :infinite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         :varprop
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))
