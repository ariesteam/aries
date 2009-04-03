
;; ---------------------------------------------------------------------------------------------------
;; test: this should run the interactive GSSM interface on the whole ARIES dataset for climate stability
;; ---------------------------------------------------------------------------------------------------

;(aries.demo/run-gssm-demo 'aestheticService:SensoryEnjoyment 'aestheticService:AestheticViewshedUse 256 0.2)
;(aries.demo/run-gssm-demo 'aestheticService:ProximityToBeauty 'aestheticService:AestheticProximityUse 64 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:AllPeopleEverywhere 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:GreenhouseGasEmitters 256 0.2)
(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:ParticularlyVulnerableGroups 256 0.2)
(tl/alert "done")