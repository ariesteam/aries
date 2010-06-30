(ns core.models.carbon-jen
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; output and training TODO make it classify the appropriate measurement - buggy for now
;; Jen leaves out the catgory Very Low from her discretization
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units 		"t/ha" 
	  				[80 :>]    'carbonService:VeryHighVegetationStorage
	  				[70 80]    'carbonService:HighVegetationStorage
	  				[50 70]    'carbonService:ModerateVegetationStorage
	  				[0 50]     'carbonService:LowVegetationStorage
	  				0          'carbonService:NoVegetationStorage)) 				

;; output and training TODO make it classify the appropriate measurement - buggy for now				
;; Jen leaves out the catgory Very Low from her discretization
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha" 
	  				[210 :>]   'carbonService:VeryHighSoilStorage
	  				[140 210]  'carbonService:HighSoilStorage
	  				[70 140]   'carbonService:ModerateSoilStorage
	  				[0 70]     'carbonService:LowSoilStorage
	  				0          'carbonService:NoSoilStorage))	  			
	  				
;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [40 :>]       'carbonService:VeryHighSOL
        [34 40]       'carbonService:HighSOL
        [29 34]       'carbonService:ModerateSOL
        [24 29]       'carbonService:LowSOL
        [:< 24]       'carbonService:VeryLowSOL))

(defmodel fire-frequency 'carbonService:FireFrequency
		 (classification (ranking 'habitat:FireFrequency)	
		 			[0.9 :>]    'carbonService:HighFireFrequency
		 			[0.25 0.9]  'carbonService:ModerateFireFrequency 
		 			[0.05 0.25] 'carbonService:LowFireFrequency
		 			[:< 0.05]   'carbonService:VeryLowFireFrequency))
		 			
(defmodel stand-condition 'carbonService:StandCondition
			(classification (ranking 'carbonService:StandCondition)
					#{4 5 6}			'carbonService:HighStandCondition
					#{7 8 9}			'carbonService:ModerateStandCondition
					#{1 2 3}			'carbonService:LowStandCondition
					:otherwise		'carbonService:NoStandCondition))
					
(defmodel stand-size-density 'carbonService:StandSizeDensity
			(classification (ranking 'carbonService:StandSizeDensity)
					#{5 6 8 9}		'carbonService:HighStandSizeDensity
					#{3 4 7}	  	'carbonService:ModerateStandSizeDensity
					#{1 2}				'carbonService:LowStandSizeDensity
					0							'carbonService:NoStandSizeDensity))
					
(defmodel soil-CN-ratio 'carbonService:SoilCNRatio
			(classification (ranking 'carbonService:SoilCNRatio)
					[35 :>]					'carbonService:VeryHighCNRatio
					[20 35]					'carbonService:HighCNRatio
					[10 20]					'carbonService:LowCNRatio
					[:< 10]					'carbonService:VeryLowCNRatio)) 
			
;; Bayesian source model
;; keep = observations computed by the Bayesian network that we keep.  context = leaf nodes as derived from models
(defmodel source 'carbonService:CarbonSourceValue   
	  (bayesian 'carbonService:CarbonSourceValue 
	  	:import   "aries.core::CarbonSourceValueLyeBrook.xdsl"
	  	:keep     ('carbonService:NetCarbonUptake
                 'carbonService:VegetationAndSoilCarbonSequestration
                 'carbonService:VegetationAndSoilCarbonStorage
                 'carbonService:VegetationCarbonStorage
	  						 'carbonService:SoilCarbonStorage)
	    :observed (soil-storage veg-storage)
	 	 	:context  (veg-storage soil-storage summer-high-winter-low fire-frequency 
	 	 							stand-condition stand-size-density soil-CN-ratio)))  
	 	 	           	 		
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
		 			

		 			