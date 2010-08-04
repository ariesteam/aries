(ns core.models.carbon
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; these are the undiscretization statements, necessary for training purposes.
;; output and training TODO make it classify the appropriate measurement - buggy for now
;;KB: Should this actually be sequestration?  And should discretization on the two 
;; below defmodels be updated?
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
	(classification 'carbonService:VegetationAndSoilCarbonStorage
						:units "t/ha" 
	  				[1000 3200]   'carbonService:VeryHighStorage
            [600 1000]    'carbonService:HighStorage
            [300 600]     'carbonService:ModerateStorage
            [100 300]     'carbonService:LowStorage
            [0.01 100]    'carbonService:VeryLowStorage
            [0 0.01]      'carbonService:NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units "t/ha" 
	  				[900 2301]     'carbonService:VeryHighVegetationStorage
            [500 900]      'carbonService:HighVegetationStorage
            [250 500]      'carbonService:ModerateVegetationStorage
            [75 250]       'carbonService:LowVegetationStorage
            [0.01 75]      'carbonService:VeryLowVegetationStorage
            [0 0.01]       'carbonService:NoVegetationStorage)) 			

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha" 
	  				[680 820]      'carbonService:VeryHighSoilStorage
            [440 680]      'carbonService:HighSoilStorage
            [200 440]      'carbonService:ModerateSoilStorage
            [50 200]       'carbonService:LowSoilStorage
            [0.01 50]      'carbonService:VeryLowSoilStorage
            [0 0.01]       'carbonService:NoSoilStorage))
	  				
;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel slope 'carbonService:Slope
		(classification (measurement 'geophysics:DegreeSlope "\u00b0")
			 [:< 1.15] 	  'carbonService:Level
			 [1.15 4.57] 	'carbonService:GentlyUndulating
			 [4.57 16.70] 'carbonService:RollingToHilly
			 [16.70 :>] 	'carbonService:SteeplyDissectedToMountainous))
    
(defmodel successional-stage 'carbonService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6}                         'carbonService:OldGrowth
	 		4                              'carbonService:LateSuccession
	 		3                              'carbonService:MidSuccession
	 		2                              'carbonService:PoleSuccession
	 		1                              'carbonService:EarlySuccession
	 		#{22 23 24 25 26 27 28 40 41}  'carbonService:NoSuccession))
	 		  
(defmodel percent-vegetation-cover 'carbonService:PercentVegetationCover
	(classification (ranking 'habitat:PercentVegetationCover :units "%")
		[80 :>] 'carbonService:VeryHighVegetationCover
		[60 80] 'carbonService:HighVegetationCover
		[40 60] 'carbonService:ModerateVegetationCover
		[20 40] 'carbonService:LowVegetationCover
		[1 20]  'carbonService:VeryLowVegetationCover))

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
 (classification (numeric-coding 'nlcd:NLCDNumeric)
		  #{90 95}   'carbonService:AnoxicSoils
		  :otherwise 'carbonService:OxicSoils))

(defmodel soil-cn-ratio 'carbonService:SoilCNRatio
  (classification (ranking 'habitat:SoilCNRatio)
       [35 :>]   'carbonService:VeryHighCNRatio
       [20 35]   'carbonService:HighCNRatio
       [10 20]   'carbonService:LowCNRatio
       [:< 10]   'carbonService:VeryLowCNRatio)) 

(defmodel hardwood-softwood-ratio 'carbonService:HardwoodSoftwoodRatio
		 (classification (ranking 'habitat:HardwoodSoftwoodRatio)
        [8 10] 'carbonService:VeryLowHardness
        [6 8]  'carbonService:LowHardness
        [4 6]  'carbonService:ModerateHardness
        [2 4]  'carbonService:HighHardness
        [1 2]  'carbonService:VeryHighHardness))
				
(defmodel fire-frequency 'carbonService:FireFrequency
		 (classification (ranking 'habitat:FireFrequency)	
		 			[:< 0.25]  'carbonService:LowFireFrequency
		 			[0.25 0.9] 'carbonService:ModerateFireFrequency
		 			[0.9 :>]   'carbonService:HighFireFrequency))

;;defmodel for carbon sequestration (NPP) BUT remember values over 30 are null!
;;FERD: Do we do defmodels for intermediate variables??

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
	    :observed (veg-soil-storage soil-storage veg-storage)
	 	 	:context  (soil-ph slope successional-stage  summer-high-winter-low fire-frequency 
	 	 	            soil-cn-ratio oxygen percent-vegetation-cover hardwood-softwood-ratio)))  
	 	 	           	 		
;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel greenhouse-gas-emitter 'carbonService:GreenhouseGasEmitters
		 (classification (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year")
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
		 			

		 			