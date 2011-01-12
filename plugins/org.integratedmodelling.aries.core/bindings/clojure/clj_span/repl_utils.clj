(ns clj-span.repl-utils
  (:use (clj-span core commandline aries-span-bridge analyzer gui)
        (clj-misc utils matrix-ops randvars)
        clojure.contrib.pprint))

(defn load-layers
  [filename]
  (let [[s k u f] (read-span-layers filename)]
    (def source-layer s)
    (def sink-layer k)
    (def use-layer u)
    (def flow-layers f)))

(defn test-run-flood
  []
  (run-span {:source-layer source-layer
             :source-threshold 600.0 ;; excludes all but >50% ModerateFloodSource
             :sink-layer sink-layer
             :sink-threshold 30000.0 ;; excludes all but >50% VeryHighFloodSink
             :use-layer use-layer
             :use-threshold 0.1
             :flow-layers flow-layers
             :trans-threshold 10.0
             :cell-width    1
             :cell-height   1
             :downscaling-factor 1   ;; MUST NOT trigger resampling! Fucking hydrosheds extent is prime!
             :rv-max-states 10
             :source-type :finite
             :sink-type :finite
             :use-type :infinite
             :benefit-type :non-rival
             :flow-model "FloodWaterMovement"
             :result-type :closure-map}))

(defn test-run-storm
  []
  (run-span {:source-layer source-layer
             :source-threshold nil
             :sink-layer sink-layer
             :sink-threshold nil
             :use-layer use-layer
             :use-threshold nil
             :flow-layers flow-layers
             :trans-threshold 0.1
             :cell-width    2610.403605972515
             :cell-height   733.4169286979684
             :downscaling-factor 1
             :rv-max-states 10
             :source-type :finite
             :sink-type :infinite
             :use-type :infinite
             :benefit-type :non-rival
             :flow-model "CoastalStormMovement"
             :result-type :closure-map
             :animation? true}))

(defn test-run-fishing
  []
  (run-span {:source-layer source-layer
             :source-threshold nil
             :sink-layer nil
             :sink-threshold nil
             :use-layer use-layer
             :use-threshold nil
             :flow-layers flow-layers
             :trans-threshold 0.1
             :cell-width    2610.403605972515
             :cell-height   733.4169286979684
             :downscaling-factor 1
             :rv-max-states 10
             :source-type :finite
             :sink-type nil
             :use-type :finite
             :benefit-type :rival
             :flow-model "SubsistenceFishAccessibility"
             :result-type :closure-map
             :animation? false}))
