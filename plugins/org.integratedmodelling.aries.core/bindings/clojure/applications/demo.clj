
;; ---------------------------------------------------------------------------------------------------
;; test runs; should eventually contain functions to test run all ARIES models at the command line
;; ---------------------------------------------------------------------------------------------------

;; run the view model
(defn run-view-model
	"Run the view model at the given resolution" 
	[resolution]
	(let [view-params 
					{:decay-rate      0.98
				 	 :trans-threshold 0.01
				 	 :sink-type       :relative
				 	 :use-type        :relative
				 	 :benefit-type    :non-rival}]
		(aries.demo/run-gssm-demo 
			'aestheticService:SensoryEnjoyment
			'aestheticService:AestheticViewshedUse
			'aestheticService:ViewSink
			'aestheticService:LineOfSight view-params resolution)))
		
(run-view-model 64)

;(aries.demo/make-dataset 'aestheticService:ProximityToBeauty "proximity_data" 512)
;(aries.demo/run-gssm-demo 'aestheticService:ProximityToBeauty 'aestheticService:AestheticProximityUse 64 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:AllPeopleEverywhere 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:GreenhouseGasEmitters 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:ParticularlyVulnerableGroups 256 0.2)
