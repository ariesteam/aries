(ns clj-span.repl-utils
  (:use (clj-span core commandline aries-span-bridge analyzer gui)
        (clj-misc utils matrix-ops randvars stats)
        clojure.contrib.pprint))

(defn load-layers
  [filename]
  (let [[s k u f cell-w cell-h] (read-span-layers filename)]
    (def source-layer s)
    (def sink-layer   k)
    (def use-layer    u)
    (def flow-layers  f)
    (def cell-width   cell-w)
    (def cell-height  cell-h)))

(defn test-run-flood
  []
  (run-span {:flow-model         "FloodWaterMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   600.0   ;; excludes all but >50% ModerateFloodSource
             :sink-threshold     30000.0 ;; excludes all but >50% VeryHighFloodSink
             :use-threshold      0.1
             :trans-threshold    10.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :infinite
             :benefit-type       :non-rival
             :downscaling-factor 1
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
             :source-threshold   nil
             :sink-threshold     nil
             :use-threshold      nil
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          nil
             :use-type           :finite
             :benefit-type       :rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
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
             :trans-threshold    10.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :finite
             :benefit-type       :rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
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
             :source-threshold   50.0
             :sink-threshold     50.0
             :use-threshold      0.2
             :trans-threshold    1.0
             :source-type        :infinite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
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
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
             :result-type        :closure-map}))
