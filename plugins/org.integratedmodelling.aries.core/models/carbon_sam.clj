(ns aries.carbonSam
	(:refer-clojure)
  (:refer modelling :only (defmodel defscenario measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
	(classification 'carbonService:VegetationAndSoilCarbonStorage
						:units "t/ha*year" 
	  				[300 :>]    'carbonService:VeryHighStorage
	  				[220 300]   'carbonService:HighStorage
	  				[140 220]   'carbonService:ModerateStorage
	  				[70 140]    'carbonService:LowStorage
	  				[0 70]      'carbonService:VeryLowStorage
	  				0           'carbonService:NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units "t/ha*yr" 
	  				[100 :>]   'carbonService:VeryHighVegetationStorage
	  				[80 100]   'carbonService:HighVegetationStorage
	  				[60 80]    'carbonService:ModerateVegetationStorage
	  				[30 60]    'carbonService:LowVegetationStorage
	  				[0 30]     'carbonService:VeryLowVegetationStorage
	  				0          'carbonService:NoVegetationStorage)) 				

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha*year" 
	  				[200 :>]   'carbonService:VeryHighSoilStorage
	  				[140 200]  'carbonService:HighSoilStorage
	  				[80 140]   'carbonService:ModerateSoilStorage
	  				[40 80]    'carbonService:LowSoilStorage
	  				[0 40]     'carbonService:VeryLowSoilStorage
	  				0          'carbonService:NoSoilStorage))	  			
	  				
;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [44 :>]       'carbonService:VeryHighSOL
        [42 44]       'carbonService:HighSOL
        [40 42]       'carbonService:ModerateSOL
        [38 40]       'carbonService:LowSOL
        [:< 38]       'carbonService:VeryLowSOL))

(defmodel mean-annual-precip 'carbonService:MeanAnnualPrecipitation
			(classification (ranking 'carbonService:MeanAnnualPrecipitation)
					[70 :>]				'carbonService:VeryHighMeanAnnualPrecipitation
					[60 70]				'carbonService:HighMeanAnnualPrecipitation
					[50 60]				'carbonService:ModerateMeanAnnualPrecipitation
					[40 50]				'carbonService:LowMeanAnnualPrecipitation
					[:< 40]				'carbonService:VeryLowMeanAnnualPrecipitation))
						
(defmodel soil-CN-ratio 'carbonService:SoilCNRatio
			(classification (ranking 'carbonService:SoilCNRatio)
					[25 :>]				'carbonService:VeryHighCNRatio
					[17.5 25]			'carbonService:HighCNRatio
					[10 17.5]			'carbonService:LowCNRatio
					[:< 10]				'carbonService:VeryLowCNRatio))
					
(defmodel veg-type 'carbonService:VegetationType
			(classification (ranking 'carbonService:VegetationType)
					#{1 5 25}			'carbonService:RowCrops
					#{36 37 62}		'carbonService:GrasslandHerbaceous
					63					  'carbonService:Forest
					87						'carbonService:Wetland
					#{61 82}			'carbonService:NoVegetation))
					
(defmodel biomass-removal-rate 'carbonService:BiomassRemovalRate
			(classification (ranking 'carbonService:BiomassRemovalRate)
					[90 :>]				'carbonService:VeryHighRemovalRate
					[66 90]				'carbonService:HighRemovalRate
					[10 66]				'carbonService:LowRemovalRate
					[:< 10]				'carbonService:VeryLowRemovalRate)) 

;; Bayesian source model
;; keep = observations computed by the Bayesian network that we keep.  context = leaf nodes as derived from models
(defmodel source 'carbonService:CarbonSourceValue   
	  (bayesian 'carbonService:CarbonSourceValue 
	  	:import   "aries.core::CarbonSourceValue.xdsl"
	  	:keep     ('carbonService:NetCarbonUptake
                 'carbonService:VegetationAndSoilCarbonSequestration
                 'carbonService:VegetationAndSoilCarbonStorage
                 'carbonService:VegetationCarbonStorage
	  						 'carbonService:StoredCarbonRelease
	   						 'carbonService:SoilCarbonStorage)
	    :observed (soil-storage veg-storage)
	 	 	:context  (veg-soil-storage veg-storage soil-storage summer-high-winter-low mean-annual-precip
	 	 								soil-CN-ratio veg-type biomass-removal-rate)))  
	 	 	           	 		
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
	  			  :otherwise 'carbonService:EmitterUsePresent) 
	  	:import  "aries.core::CarbonUse.xdsl"
	  	:keep    ('carbonService:CarbonEmitterUse)
	  	:context (greenhouse-gas-emitter)))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------

;; flow model for emitters
(defmodel emitter-flow 'carbonService:ClimateStability
  (span 'carbonService:CO2Removed 
  	    'carbonService:VegetationAndSoilCarbonStorage
  	    'carbonService:CarbonEmitterUse
      	nil
      	nil
  	    nil
  	:source-threshold 1,
   	:sink-threshold   0.5,
   	:use-threshold    0.5,
   	:trans-threshold  1.0,
   	:sink-type        :relative,
   	:use-type         :relative,
   	:benefit-type     :rival,
   	:rv-max-states    10 
    :context (source use-emitters)))		
		
;; ----------------------------------------------------------------------------------------------
;; scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized 'carbonService:IPCCHadleyA2Incentivized
  "This scenario represents the effects of the Hadley A1 IPCC climate scenario. A12 
  represents a future world of very rapid economic growth, global population that peaks 
  in mid-century and declines thereafter, and rapid introduction of new and more efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6 4}    'carbonService:OldGrowth
	 		3           'carbonService:MidSuccession
	 		2           'carbonService:EarlySuccession
	 		1           'carbonService:PoleSuccession
	 		:otherwise  'carbonService:NoSuccession)) 

(defscenario ipcc-hadley-b2-incentivized 'carbonService:IPCCHadleyB2Incentivized
  "This scenario represents the effects of the Hadley B1 IPCC climate scenario. The B1 
  world is a convergent world with the same global population as in the A1 storyline but 
  with rapid changes in economic structures toward a service and information economy, with 
  reductions in material intensity, and the introduction of clean and resource-efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking 'ecology:SuccessionalStage)
      #{5 6 4}    'carbonService:OldGrowth
	 		3           'carbonService:MidSuccession
	 		2           'carbonService:EarlySuccession
	 		1           'carbonService:PoleSuccession
	 		:otherwise  'carbonService:NoSuccession))  
       
(defscenario ipcc-hadley-a2 'carbonService:IPCCHadleyA2
	  "This scenario represents the effects of the Hadley A2 IPCC climate scenario. A2
     represents a very heterogeneous world with continuously increasing global population 
     and regionally oriented economic growth that is more fragmented and slower than in
     other storylines." 
) 

(defscenario ipcc-hadley-b2 'carbonService:IPCCHadleyB2
	  "This scenario represents the effects of the Hadley B2 IPCC climate scenario. B2 
     is a world in which the emphasis is on local solutions to economic, social, and 
     environmental sustainability, with continuously increasing population (lower than A2)
     and intermediate economic development. " 
) 
		 			

		 			