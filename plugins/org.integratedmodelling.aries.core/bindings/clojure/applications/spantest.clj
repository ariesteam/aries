;; File to test that clj-span loads and runs.
(ns applications.spantest)

(refer 'clj-span.aries-span-bridge :only '(span-driver))
(refer 'tl :only '(conc))

(def homedir (System/getProperty "user.home"))

;; Scenic Beauty
(def span-spec-1 {:model-name         "core.models.aesthetics/data"
                  :location           'chehalis
                  :resolution         256
                  :source-concept     'aestheticService:TheoreticalNaturalBeauty
                  :sink-concept       'aestheticService:TotalVisualBlight
                  :use-concept        'aestheticService:HomeownerViewUse
                  :flow-concept       'geophysics:Altitude
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
                  :result-type        :closure-map
                  :save-file          (str homedir "/viewshed_data.clj")})

;; Proximity to Open Space (FIXME)
(def span-spec-2 {:model-name         "core.models.aesthetics/data"
                  :location           'chehalis
                  :resolution         256
                  :source-concept     'aestheticService:TheoreticalNaturalBeauty
                  :sink-concept       'aestheticService:TotalVisualBlight
                  :use-concept        'aestheticService:HomeownerViewUse
                  :flow-concept       'geophysics:Altitude
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
                  :result-type        :closure-map
                  :save-file          (str homedir "/viewshed_data.clj")})

;; Carbon (FIXME)
(def span-spec-3 {:model-name         "core.models.aesthetics/data"
                  :location           'chehalis
                  :resolution         256
                  :source-concept     'aestheticService:TheoreticalNaturalBeauty
                  :sink-concept       'aestheticService:TotalVisualBlight
                  :use-concept        'aestheticService:HomeownerViewUse
                  :flow-concept       'geophysics:Altitude
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
                  :result-type        :closure-map
                  :save-file          (str homedir "/viewshed_data.clj")})

;; Sediment (FIXME)
(def span-spec-4 {:model-name         "core.models.aesthetics/data"
                  :location           'chehalis
                  :resolution         256
                  :source-concept     'aestheticService:TheoreticalNaturalBeauty
                  :sink-concept       'aestheticService:TotalVisualBlight
                  :use-concept        'aestheticService:HomeownerViewUse
                  :flow-concept       'geophysics:Altitude
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
                  :result-type        :closure-map
                  :save-file          (str homedir "/viewshed_data.clj")})

(defn run-span-example
  [{:keys [model-name location resolution source-concept sink-concept use-concept flow-concept]
    :as span-spec}]
  (span-driver [model-name location resolution]
               (conc source-concept)
               (conc sink-concept)
               (conc use-concept)
               (conc flow-concept)
               (select-keys span-spec
                            [:source-threshold :sink-threshold :use-threshold :trans-threshold
                             :rv-max-states :downscaling-factor :source-type :sink-type :use-type
                             :benefit-type :result-type :save-file])))

(run-span-example span-spec-1)
;;(run-span-example span-spec-2)
;;(run-span-example span-spec-3)
;;(run-span-example span-spec-4)
