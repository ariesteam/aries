;; ---------------------------------------------------------------------------------------------------
;; test runs; should eventually contain functions to test run all ARIES models at the command line
;; ---------------------------------------------------------------------------------------------------

;; run the view model
(defn run-view-model
  "Run the view model at the given resolution"
  [location resolution]
  (let [flow-params {:trans-threshold  1.0
		     :source-threshold 0.05
		     :sink-threshold   0.20
		     :use-threshold    0.05
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival
		     :rv-max-states    10}]
    (aries.demo/run-span-interactive
     "aries/view/data" 
     'aestheticService:NaturalBeauty
     'aestheticService:HomeownersEnjoyment
     'aestheticService:ViewSink
     'geophysics:Altitude flow-params location resolution)))

(defn diocan
	[model-id location max-resolution]
	(let [data-obs  (modelling/run model-id location max-resolution)]
		(println (geospace/grid-rows data-obs) "X" (geospace/grid-columns data-obs) "\n"
						 (corescience/find-state data-obs 'aestheticService:NaturalBeauty) "\n" 
						 (corescience/find-state data-obs 'aestheticService:HomeownersEnjoyment) "\n" 
						 (corescience/find-state data-obs 'aestheticService:ViewSink) "\n" 
						 (corescience/find-state data-obs 'geophysics:Altitude))))

(diocan "aries/view/data" 'chehalis 128)

;(defn run-proximity-model
;  "Run the proximity model at the given resolution" 
;  [location resolution]
;  (let [flow-params {:trans-threshold  1.0
;		     :source-threshold 0.0
;		     :sink-threshold   0.0
;		     :use-threshold    0.0
;		     :sink-type        :relative
;		     :use-type         :relative
;		     :benefit-type     :non-rival
;		     :rv-max-states    10}]
;    (aries.demo/run-span-interactive
;	   ""
;     'aestheticService:ProximityToBeauty
;     'aestheticService:AestheticProximityUse
;     'aestheticService:ProximitySink
;     'aestheticService:Proximity flow-params location resolution)))

;(run-view-model 'chehalis 128)
;;(run-proximity-model 256)

;(aries.demo/make-dataset 'aestheticService:ProximityToBeauty "proximity_data" 512)
;(aries.demo/run-gssm-demo 'aestheticService:ProximityToBeauty 'aestheticService:AestheticProximityUse 64 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:AllPeopleEverywhere 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:GreenhouseGasEmitters 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:ParticularlyVulnerableGroups 256 0.2)
