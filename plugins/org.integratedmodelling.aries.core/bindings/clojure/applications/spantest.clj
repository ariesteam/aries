;; File to test that clj-span loads and runs.

(println "Hello from spantest.clj!")

(let [observation (modelling/run-at-location "core.models.aesthetics/data" 'chehalis 64)
      source-conc (tl/conc 'aestheticService:NaturalBeauty)
      use-conc    (tl/conc 'aestheticService:HomeownersEnjoyment)
      sink-conc   (tl/conc 'aestheticService:ViewSink)
      flow-conc   (tl/conc 'geophysics:Altitude)
      flow-params  {:source-threshold   0.05
                    :sink-threshold     0.20
                    :use-threshold      0.05
                    :trans-threshold    1.0
                    :rv-max-states      10
                    :downscaling-factor 1
                    :source-type        :infinite
                    :sink-type          :infinite
                    :use-type           :infinite
                    :benefit-type       :non-rival
                    :result-type        :closure-map}]
  (list observation source-conc use-conc sink-conc flow-conc flow-params))
;;  (aries-span-bridge/span-driver observation source-conc use-conc sink-conc flow-conc flow-params))
