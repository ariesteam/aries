
;; ---------------------------------------------------------------------------------------------------
;; test runs; should eventually contain functions to test run all ARIES models at the command line
;; ---------------------------------------------------------------------------------------------------

;; run the view model
(defn save-view-model
	"Run the view model at the given resolution" 
	[resolution output-file]
	(let [view-params 
					{:decay-rate      0.98
				 	 :trans-threshold 0.01
				 	 :sink-type       :relative
				 	 :use-type        :relative
				 	 :benefit-type    :non-rival}]
				(aries.demo/save-gssm-demo-data 
					'aestheticService:SensoryEnjoyment
					'aestheticService:AestheticViewshedUse
					'aestheticService:ViewSink
					'aestheticService:LineOfSight view-params resolution output-file)))
		
(save-view-model 200 "C:/A/results/viewflow.nc")