(ns aries/carbon
	(:refer-clojure)
  (:refer modelling :only (defmodel defscenario measurement classification categorization ranking identification bayesian)))

;; output and training
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
	(classification 'carbonService:VegetationAndSoilCarbonStorage
	  				[12 :>]   'carbonService:VeryHighStorage
	  				[9 12]    'carbonService:HighStorage
	  				[6 9]     'carbonService:ModerateStorage
	  				[3 6]     'carbonService:LowStorage
	  				[0.01 3]  'carbonService:VeryLowStorage
	  				[:< 0.01] 'carbonService:NoStorage))

;; output and training
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
	  				[12 :>]   'carbonService:VeryHighVegetationStorage
	  				[9 12]    'carbonService:HighVegetationStorage
	  				[6 9]     'carbonService:ModerateVegetationStorage
	  				[3 6]     'carbonService:LowVegetationStorage
	  				[0.01 3]  'carbonService:VeryLowVegetationStorage
	  				[:< 0.01] 'carbonService:NoVegetationStorage)) 				

;; output and training	  				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
	  				[12 :>]   'carbonService:VeryHighSoilStorage
	  				[9 12]    'carbonService:HighSoilStorage
	  				[6 9]     'carbonService:ModerateSoilStorage
	  				[3 6]     'carbonService:LowSoilStorage
	  				[0.01 3]  'carbonService:VeryLowSoilStorage
	  				[:< 0.01] 'carbonService:NoSoilStorage))	  			
	  				
;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel slope 'carbonService:Slope
		(classification (ranking 'geophysics:DegreeSlope "�")
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
		  
(defmodel hardwood-softwood-ratio 'carbonService:HardwoodSoftwoodRatio
		 (classification (ranking 'habitat:HardwoodSoftwoodRatio)
        [80 100] 'carbonService:VeryLowHardness
        [60 80]  'carbonService:LowHardness
        [40 60]  'carbonService:ModerateHardness
        [20 40]  'carbonService:HighHardness
        [0 20]   'carbonService:VeryHighHardness))

(defmodel evapotranspiration 'carbonService:ActualEvapotranspiration
		 (classification (measurement 'habitat:Evapotranspiration "mm/year")
				[:< 12] 'carbonService:VeryLowEvapotranspiration
				[12 32] 'carbonService:LowEvapotranspiration
				[32 58] 'carbonService:ModerateEvapotranspiration
				[58 92] 'carbonService:HighEvapotranspiration
				[92 :>] 'carbonService:VeryHighEvapotranspiration))
				
(defmodel fire-frequency 'carbonService:FireFrequency
		 (classification (ranking 'habitat:FireFrequency)	
		 			[:< 0.25]  'carbonService:LowFireFrequency
		 			[0.25 0.9] 'carbonService:ModerateFireFrequency
		 			[0.9 :>]   'carbonService:HighFireFrequency))
			
;; Bayesian source model		
(defmodel source 'carbonService:CarbonSourceValue
	  (bayesian 'carbonService:CarbonSourceValue)
	  	:import   "aries.core::CarbonSourceValue.xdsl"
	  	:keep     ('carbonService:VegetationCarbonStorage
	  						 'carbonService:StoredCarbonRelease
	   						 'carbonService:SoilCarbonStorage)
	    :observed (veg-soil-storage soil-storage veg-storage)
	 	 	:context  (soil-ph slope successional-stage  summer-high-winter-low fire-frequency
	 	 	           vegetation-type hardwood-softwood-ratio evapotranspiration))

;; missing: CommercialForestyPractices (later - based on clearcuts which is not available in chehalis)

;; ----------------------------------------------------------------------------------------------
;; modified source dependencies to account for different scenarios
;; ----------------------------------------------------------------------------------------------

;; old growth has been incentivized, so what was late succession is now old growth
(defmodel successional-stage-incentivized 'carbonService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6 4}    'carbonService:OldGrowth
	 		3           'carbonService:MidSuccession
	 		2           'carbonService:EarlySuccession
	 		1           'carbonService:PoleSuccession
	 		:otherwise  'carbonService:NoSuccession))
	 		
(defmodel summer-high-winter-low-hadley-a2 'carbonService:SummerHighWinterLow
		 (classification (ranking 'carbonService:SummerHighWinterLowHadleyA2)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))
        
(defmodel summer-high-winter-low-hadley-b2 'carbonService:SummerHighWinterLow
		 (classification (ranking 'carbonService:SummerHighWinterLowHadleyB2)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))

;; Bayesian source model		
(defmodel source-hadley-a2 'carbonService:CarbonSourceValue
	  (bayesian 'carbonService:CarbonSourceValue)
	  	:import   "aries.core::CarbonSourceValue.xdsl"
	  	:keep     ('carbonService:VegetationCarbonStorage
	  						 'carbonService:StoredCarbonRelease
	  						 'carbonService:SoilCarbonStorage)
			:observed (veg-soil-storage soil-storage veg-storage)
	 	 	:context  (soil-ph slope successional-stage-incentivized  
	 	 						 summer-high-winter-low-hadley-a2 fire-frequency
	 	 	           vegetation-type hardwood-softwood-ratio evapotranspiration))	
	  				
;; Bayesian source model		
(defmodel source-hadley-b2 'carbonService:CarbonSourceValue
	  (bayesian 'carbonService:CarbonSourceValue)
	  	:import   "aries.core::CarbonSourceValue.xdsl"
	  	:keep     ('carbonService:VegetationCarbonStorage
	  						 'carbonService:StoredCarbonRelease
	  						 'carbonService:SoilCarbonStorage)
	 	 	:context  (soil-ph slope successional-stage-incentivized  
	 	 						 summer-high-winter-low-hadley-b2 fire-frequency
	 	 	           vegetation-type hardwood-softwood-ratio evapotranspiration)
	 	 	:observed (veg-soil-storage soil-storage veg-storage))	 	 	           
	 	 	           	 		
;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel greenhouse-gas-emitter 'carbonService:GreenhouseGasEmitters
			;; TODO make this a measurement
		 (classification (ranking 'carbonService:GreenhouseGasEmissions)
		 	 [250000 :>]     'carbonService:VeryHighEmitter
		 	 [100000 250000] 'carbonService:HighEmitter
		 	 [25000 100000]  'carbonService:ModerateEmitter
		 	 [1000 25000]    'carbonService:LowEmitter
		 	 [100 1000]      'carbonService:VeryLowEmitter
		 	 [:< 100]        'carbonService:NoEmitter))
		 	 
(defmodel use-emitters 'carbonService:CarbonUse
	  (bayesian 'carbonService:CarbonUse
	  		(classification 'carbonService:CarbonEmitterUse
	  				0          'carbonService:EmitterUseAbsent
	  			  :otherwise 'carbonService:EmitterUsePresent))
	  	:import  "aries.core::CarbonUse.xdsl"
	  	:keep    ('carbonService:CarbonEmitterUse)
	  	:context (greenhouse-gas-emitter))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------

;; data for emission trading
(defmodel data-emitters 'carbonService:EmissionTrading 
	(identification 'carbonService:EmissionTrading)
		:context (source use-emitters))

;; Hadley A2 scenario
(defmodel data-emitters-hadley-a2 'carbonService:EmissionHadleyA2Scenario 
	(identification 'carbonService:EmissionTrading)
		:context (source-hadley-a2 use-emitters))

;; Hadley B2 scenario
(defmodel data-emitters-hadley-b2 'carbonService:EmissionHadleyB2Scenario 
	(identification 'carbonService:EmissionTrading)
		:context (source-hadley-b2 use-emitters))
		
;; ----------------------------------------------------------------------------------------------
;; scenarios
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2 'carbonService:CarbonSourceValue
	  "This scenario represents the effects of the Hadley A2 IPCC climate scenario." 
		(classification (ranking 'carbonService:SummerHighWinterLowHadleyA2)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL)) 

(defscenario ipcc-hadley-b2 'carbonService:CarbonSourceValue
	  "This scenario represents the effects of the Hadley B2 IPCC climate scenario." 
		(classification (ranking 'carbonService:SummerHighWinterLowHadleyB2)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL)) 
		 			