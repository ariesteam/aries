;; File to test that clj-span loads and runs.
(ns applications.spantest)

(refer 'clj-span.aries-span-bridge :only '(span-driver))
(refer 'tl :only '(conc))

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
        source-concept (conc source-concept)
        sink-concept   (conc sink-concept)
        use-concept    (conc use-concept)
        flow-concept   (conc flow-concept)
        flow-params    (select-keys span-spec
                                    [:source-threshold :sink-threshold :use-threshold :trans-threshold
                                     :rv-max-states :downscaling-factor :source-type :sink-type :use-type
                                     :benefit-type :result-type :save-file])]
    (println model-spec)
    (println source-concept)
    (println sink-concept)
    (println use-concept)
    (println flow-concept)
    (println flow-params)
    (println "Running the SPAN model...")
    (span-driver model-spec source-concept sink-concept use-concept flow-concept flow-params)))

(run-span-example span-spec-1)

;;(def outfile (System/getProperty "user.home"))
