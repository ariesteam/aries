
;; ---------------------------------------------------------------------------------------------------
;; test runs; should eventually contain functions to test run all ARIES models at the command line
;; ---------------------------------------------------------------------------------------------------

;; run the view model
(defn run-view-model
  "Run the view model at the given resolution"
  [resolution]
  (let [flow-params {:decay-rate       0.95
		     :trans-threshold  1.0
		     :source-threshold 0.05
		     :sink-threshold   0.20
		     :use-threshold    0.05
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival}]
    (aries.demo/run-gssm-demo
     'aestheticService:SensoryEnjoyment
     'aestheticService:AestheticViewshedUse
     'aestheticService:ViewSink
     'aestheticService:LineOfSight flow-params resolution)))

(defn run-proximity-model
  "Run the proximity model at the given resolution" 
  [resolution]
  (let [flow-params {:decay-rate       0.6
		     :trans-threshold  1.0
		     :source-threshold 0.0
		     :sink-threshold   0.0
		     :use-threshold    0.0
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival}]
    (aries.demo/run-gssm-demo
     'aestheticService:ProximityToBeauty
     'aestheticService:AestheticProximityUse
     'aestheticService:ProximitySink
     'aestheticService:Proximity flow-params resolution)))

;;(run-view-model 256)
(run-proximity-model 256)

;(aries.demo/make-dataset 'aestheticService:ProximityToBeauty "proximity_data" 512)
;(aries.demo/run-gssm-demo 'aestheticService:ProximityToBeauty 'aestheticService:AestheticProximityUse 64 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:AllPeopleEverywhere 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:GreenhouseGasEmitters 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:ParticularlyVulnerableGroups 256 0.2)
