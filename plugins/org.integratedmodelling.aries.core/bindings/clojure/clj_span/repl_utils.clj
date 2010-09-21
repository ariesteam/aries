(ns clj-span.repl-utils
  (:use [clj-span.core :only (run-span)]
        [clj-span.aries-span-bridge :only (read-span-layers)]))

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
             :rv-max-states 10
             :downscaling-factor 1   ;; MUST NOT trigger resampling! Fucking hydrosheds extent is prime!
             :source-type :finite
             :sink-type :finite
             :use-type :infinite
             :benefit-type :non-rival
             :flow-model "FloodWaterMovement"
             :result-type :closure-map}))
