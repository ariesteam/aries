;; File to test that clj-span loads and runs.

(println "Hello from spantest.clj!")

(def span-spec-1 {:model-name        "core.models.aesthetics/data"
                  :location           'chehalis
                  :resolution         64
                  :source-concept    'aestheticService:TheoreticalNaturalBeauty
                  :sink-concept      'aestheticService:TotalVisualBlight
                  :use-concept       'aestheticService:HomeownerViewUse
                  :flow-concept      'geophysics:Altitude
                  :source-threshold   0.05
                  :sink-threshold     0.20
                  :use-threshold      0.05
                  :trans-threshold    1.0
                  :rv-max-states      10
                  :downscaling-factor 1
                  :source-type        :infinite
                  :sink-type          :infinite
                  :use-type           :infinite
                  :benefit-type       :non-rival
                  :result-type        :closure-map})

(defn run-span-example
  [{:keys [model-name location resolution source-concept sink-concept use-concept flow-concept]
    :as span-spec}]
  (let [model-spec     [model-name location resolution]
        source-concept (tl/conc source-concept)
        sink-concept   (tl/conc sink-concept)
        use-concept    (tl/conc use-concept)
        flow-concept   (tl/conc flow-concept)
        flow-params    (select-keys :source-threshold :sink-threshold :use-threshold :trans-threshold
                                    :rv-max-states :downscaling-factor :source-type :sink-type :use-type
                                    :benefit-type :result-type :save-file)]
    (aries-span-bridge/span-driver model-spec source-concept sink-concept use-concept flow-concept flow-params)))

;;(def outfile (System/getProperty "user.home"))

(run-span-example span-spec-1)
