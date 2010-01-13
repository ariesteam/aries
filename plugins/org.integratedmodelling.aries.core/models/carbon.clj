(ns aries/models/carbon
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-ph 'carbonService:Soilph
		 (classification (ranking 'carbonService:Soilph)
        1      'carbonService:HighPh
        2      'carbonService:LowPh
        {3 4}  'carbonService:ModeratePh))
    
(defmodel slope 'carbonService:Slope
		(classification (ranking 'geophysics:DegreeSlope "°")
			 [:< 1.15] 	  'carbonService:Level
			 [1.15 4.57] 	'carbonService:GentlyUndulating
			 [4.57 16.70] 'carbonService:RollingToHilly
			 [16.70 :>] 	'carbonService:SteeplyDissectedToMountainous))    
    
(defmodel successional-stage 'carbonService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6}      'carbonService:OldGrowth
	 		4           'carbonService:LateSuccession
	 		3           'carbonService:MidSuccession
	 		2           'carbonService:EarlySuccession
	 		1           'carbonService:PoleSuccession
	 		:otherwise  'carbonService:NoSuccession))    

(defmodel vegetation-cover 'carbonService:VegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 :>] 'carbonService:VeryHighVegetationCover
		[60 80] 'carbonService:HighVegetationCover
		[40 60] 'carbonService:ModerateVegetationCover
		[20 40] 'carbonService:LowVegetationCover
		[0 20]  'carbonService:VeryLowVegetationCover))
		
(defmodel carbon-source 'carbonService:CarbonSourceValue
	  (bayesian 'carbonService:CarbonSourceValue)
	  	:import   "aries.core::CarbonSourceValue.xsdl"
	  	:keep     ('carbonService:VegetationAndSoilCarbonStorage)
	 	 	:context  (soil-ph slope successional-stage vegetation-cover))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------------------
;; flow model data needs
;; ----------------------------------------------------------------------------------------------
 	 								
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------


		 			