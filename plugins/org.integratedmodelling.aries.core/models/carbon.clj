(ns aries/carbon
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

    
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
	 		    
;; INTERMEDIATE VARIABLE?  Genie refuses to set evidence for this one.
;; TODO check with Ken
(defmodel vegetation-cover 'carbonService:VegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 :>] 'carbonService:VeryHighVegetationCover
		[60 80] 'carbonService:HighVegetationCover
		[40 60] 'carbonService:ModerateVegetationCover
		[20 40] 'carbonService:LowVegetationCover
		[0 20]  'carbonService:VeryLowVegetationCover))

(defmodel soil-ph 'carbonService:Soilph
		 (classification (ranking 'habitat:SoilPh)
        1       'carbonService:HighPh
        2       'carbonService:LowPh
        #{3 4}  'carbonService:ModeratePh))

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))

; use NLCD layers to infer anoxic vs. oxic
(defmodel oxygen 'carbonService:SoilOxygenConditions 
 (classification (ranking 'nlcd:NLCDNumeric)
		  #{90 95}   'carbonService:Anoxic
		  :otherwise 'carbonService:Oxic))

; use NLCD layers to infer vegetation type
(defmodel vegetation-type 'carbonService:VegetationType 
  (classification (ranking 'nlcd:NLCDNumeric)
		  71   'carbonService:GrasslandType
		  [41 43 :inclusive] 'carbonService:ForestType))
		  
;; missing: HardwoodSoftwood, FireFrequency, CommercialForestyPractices (later), evapotraspiration
;; then add sumhiwinlo for the 3 scenarios

;; Bayesian source model		
(defmodel source 'carbonService:CarbonSourceValue
	  (bayesian 'carbonService:CarbonSourceValue
	  		(classification 'carbonService:VegetationAndSoilCarbonStorage
	  				[12 :>]   'carbonService:VeryHighStorage
	  				[9 12]    'carbonService:HighStorage
	  				[6 9]     'carbonService:ModerateStorage
	  				[3 6]     'carbonService:LowStorage
	  				[0.01 3]  'carbonService:VeryLowStorage
	  				[:< 0.01] 'carbonService:NoStorage))
	  	:import   "aries.core::CarbonSourceValue.xdsl"
	  	:keep     ('carbonService:VegetationAndSoilCarbonStorage)
	 	 	:context  (soil-ph slope successional-stage  summer-high-winter-low
	 	 	           vegetation-type))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

;; missing - simple, just greenhouse_gas_emissions + the BN (useful?)
;; ----------------------------------------------------------------------------------------------
;; flow model data needs
;; ----------------------------------------------------------------------------------------------
 	 								
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------


		 			