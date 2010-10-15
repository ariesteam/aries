(ns core.models.carbon
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;;This model is for western Washington State

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

(defmodel veg-soil-sequestration 'carbonService:VegetationAndSoilCarbonSequestration
  (classification 'carbonService:VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]     'carbonService:VeryHighSequestration
                  [9 12]      'carbonService:HighSequestration
                  [6 9]       'carbonService:ModerateSequestration
                  [3 6]       'carbonService:LowSequestration
                  [0.01 3]    'carbonService:VeryLowSequestration
                  [0 0.01]    'carbonService:NoSequestration))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release 'carbonService:StoredCarbonRelease
  (classification 'carbonService:StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 3200]   'carbonService:VeryHighRelease ;;may need to lower this number so the calculations work out.
                  [9 12]      'carbonService:HighRelease
                  [6 9]       'carbonService:ModerateRelease
                  [3 6]       'carbonService:LowRelease
                  [0.01 3]    'carbonService:VeryLowRelease
                  [0 0.01]    'carbonService:NoRelease))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.
    
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

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))

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

;; Bayesian source model
(defmodel source 'carbonService:CarbonSourceValue   
  (bayesian 'carbonService:CarbonSourceValue 
            :import   "aries.core::CarbonSequestration.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
            :context  (hardwood-softwood-ratio soil-cn-ratio summer-high-winter-low percent-vegetation-cover successional-stage)))

;; ----------------------------------------------------------------------------------------------
;; sink models
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel slope 'carbonService:Slope
    (classification (measurement 'geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    'carbonService:Level
       [1.15 4.57]  'carbonService:GentlyUndulating
       [4.57 16.70] 'carbonService:RollingToHilly
       [16.70 :>]   'carbonService:SteeplyDissectedToMountainous))

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph 'carbonService:Soilph
  (classification (ranking 'habitat:SoilPhShallow)
                  [7.3 :>]       'carbonService:HighPh
                  [5.5 7.3]      'carbonService:ModeratePh
                  [:< 5.5]       'carbonService:LowPh))

; use NLCD layers to infer anoxic vs. oxic
(defmodel oxygen 'carbonService:SoilOxygenConditions 
 (classification (numeric-coding 'nlcd:NLCDNumeric)
      #{90 95}   'carbonService:AnoxicSoils
      :otherwise 'carbonService:OxicSoils))

(defmodel fire-frequency 'carbonService:FireFrequency
     (classification (measurement 'habitat:FireFrequency "/km^2") 
          [:< 0.25]  'carbonService:LowFireFrequency
          [0.25 0.9] 'carbonService:ModerateFireFrequency
          [0.9 :>]   'carbonService:HighFireFrequency))

(defmodel sink 'carbonService:CarbonSinkValue   
  (bayesian 'carbonService:CarbonSinkValue 
            :import   "aries.core::StoredCarbonRelease.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio 
                       successional-stage soil-cn-ratio summer-high-winter-low fire-frequency)))
	 		
;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel use-simple 'carbonService:GreenhouseGasEmissions
  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------

(defmodel identification-carbon 'carbonService:ClimateStability
  (identification 'carbonService:ClimateStability
                  :context (source :as source
                            sink :as sink
                            use-simple :as use)))

;; flow model for emitters (why doesn't 'carbonService:ClimateStability = 'carbonService:CO2Removed ?)
(defmodel carbon-flow 'carbonService:ClimateStability
  (span 'carbonService:CO2Removed
        'carbonService:CarbonSourceValue 
        'carbonService:GreenhouseGasEmissions
        'carbonService:CarbonSinkValue 
        nil
        nil
        :source-threshold   10.0
        :sink-threshold     10.0
        :use-threshold       1.0
        :trans-threshold    nil
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 8
        :keep ('carbonService:CarbonSequestration 'carbonService:StoredCarbonRelease 
               'carbonService:GreenhouseGasEmissions 'carbonService:PotentialCarbonMitigationProvision
               'carbonService:PotentialCarbonMitigationUse 'carbonService:UsedCarbonMitigation
               'carbonService:UsedCarbonSink 'carbonService:SatisfiedCarbonMitigationDemand
               'carbonService:CarbonMitigationSurplus 'carbonService:CarbonMitigationDeficit
               'carbonService:DepletedCarbonMitigation 'carbonService:DepletedCarbonMitigationDemand)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source use-simple sink)))
		
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
		 			

		 			