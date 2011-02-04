(ns core.models.carbon
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            probabilistic-measurement probabilistic-classification
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

;;This model is for western Washington State

;; these are the undiscretization statements, necessary for training purposes.
(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
	(probabilistic-measurement VegetationAndSoilCarbonStorage "t/ha*year" 
	  		[1000 3200]   VeryHighStorage
            [600 1000]    HighStorage
            [300 600]     ModerateStorage
            [100 300]     LowStorage
            [0.01 100]    VeryLowStorage
            [0 0.01]      NoStorage))

(defmodel veg-storage VegetationCarbonStorage
	(probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
	  		[900 2301]     VeryHighVegetationStorage
            [500 900]      HighVegetationStorage
            [250 500]      ModerateVegetationStorage
            [75 250]       LowVegetationStorage
            [0.01 75]      VeryLowVegetationStorage
            [0 0.01]       NoVegetationStorage)) 			

(defmodel soil-storage SoilCarbonStorage
		(probabilistic-measurement SoilCarbonStorage "t/ha*year" 
	  		[680 820]      VeryHighSoilStorage
            [440 680]      HighSoilStorage
            [200 440]      ModerateSoilStorage
            [50 200]       LowSoilStorage
            [0.01 50]      VeryLowSoilStorage
            [0 0.01]       NoSoilStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.
    
(defmodel successional-stage SuccessionalStage
	 (classification (ranking ecology:SuccessionalStage)
	 		#{5 6}                         OldGrowth
	 		4                              LateSuccession
	 		3                              MidSuccession
	 		2                              PoleSuccession
	 		1                              EarlySuccession
	 		#{22 23 24 25 26 27 28 40 41}  NoSuccession))
	 		  
(defmodel percent-vegetation-cover PercentVegetationCover
	(classification (ranking habitat:PercentVegetationCover :units "%")
		[80 100] VeryHighVegetationCover
		[60 80]  HighVegetationCover
		[40 60]  ModerateVegetationCover
		[20 40]  LowVegetationCover
		[1 20]   VeryLowVegetationCover))

(defmodel summer-high-winter-low SummerHighWinterLow
		 (classification (ranking habitat:SummerHighWinterLow)
        [:< 24]       VeryLowSOL
        [24 30]       LowSOL
        [30 35]       ModerateSOL
        [35 40]       HighSOL
        [40 :>]       VeryHighSOL))

(defmodel soil-cn-ratio SoilCNRatio
  (classification (ranking habitat:SoilCNRatio)
       [35 :>]   VeryHighCNRatio
       [20 35]   HighCNRatio
       [10 20]   LowCNRatio
       [:< 10]   VeryLowCNRatio)) 

(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
		 (classification (ranking habitat:HardwoodSoftwoodRatio)
        [8 10] VeryLowHardness
        [6 8]  LowHardness
        [4 6]  ModerateHardness
        [2 4]  HighHardness
        [1 2]  VeryHighHardness))

(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [12 30]     VeryHighSequestration
                  [9 12]      HighSequestration
                  [6 9]       ModerateSequestration
                  [3 6]       LowSequestration
                  [0.01 3]    VeryLowSequestration
                  [0 0.01]    NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestration.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (SuccessionalStage)
            :result    veg-soil-sequestration
            :context  (hardwood-softwood-ratio soil-cn-ratio summer-high-winter-low 
                       percent-vegetation-cover successional-stage)))

;; ----------------------------------------------------------------------------------------------
;; Sink models
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel slope Slope
    (classification (measurement geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    Level
       [1.15 4.57]  GentlyUndulating
       [4.57 16.70] RollingToHilly
       [16.70 :>]   SteeplyDissectedToMountainous))

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhShallow)
                  [7.3 :>]                 HighPh
                  [5.5 7.3]                ModeratePh
                  [:exclusive 0 5.5]       LowPh))

; use NLCD layers to infer anoxic vs. oxic
(defmodel oxygen SoilOxygenConditions 
 (classification (numeric-coding nlcd:NLCDNumeric)
      #{90 95}   AnoxicSoils
      :otherwise OxicSoils))

(defmodel fire-frequency FireFrequency
     (classification (measurement habitat:FireFrequency "/km^2") 
          [:< 0.25]  LowFireFrequency
          [0.25 0.9] ModerateFireFrequency
          [0.9 :>]   HighFireFrequency))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (classification StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 1000]   VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]      HighRelease
                  [6 9]       ModerateRelease
                  [3 6]       LowRelease
                  [0.01 3]    VeryLowRelease
                  [0 0.01]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonRelease.xdsl"
            :keep     (StoredCarbonRelease)
            :required (SuccessionalStage)
            :observed (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio 
                       successional-stage soil-cn-ratio summer-high-winter-low fire-frequency)))
	 		
;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))
 	 					
;; ----------------------------------------------------------------------------------------------
;; Top-level service models
;; ----------------------------------------------------------------------------------------------

(defmodel identification-carbon ClimateStability
  (identification ClimateStability
                  :context (source :as source
                            sink :as sink
                            use-simple :as use)))

(defmodel carbon-flow ClimateStability
  (span CO2Removed
        VegetationAndSoilCarbonSequestration 
        GreenhouseGasEmissions
        StoredCarbonRelease 
        nil
        nil
        :source-threshold   1.0
        :sink-threshold     1.0
        :use-threshold      10.0
        :trans-threshold    nil
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :keep (CarbonSequestration StoredCarbonRelease 
               GreenhouseGasEmissions PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse UsedCarbonMitigation
               UsedCarbonSink SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus CarbonMitigationDeficit
               DepletedCarbonMitigation DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))
		
;; ----------------------------------------------------------------------------------------------
;; scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized IPCCHadleyA2Incentivized
  "This scenario represents the effects of the Hadley A1 IPCC climate scenario. A12 
  represents a future world of very rapid economic growth, global population that peaks 
  in mid-century and declines thereafter, and rapid introduction of new and more efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (model SuccessionalStage
    (classification (ranking ecology:SuccessionalStage)
	 		#{5 6 4}    OldGrowth
	 		3           MidSuccession
	 		2           EarlySuccession
	 		1           PoleSuccession
	 		:otherwise  NoSuccession))) 

(defscenario ipcc-hadley-b2-incentivized IPCCHadleyB2Incentivized
  "This scenario represents the effects of the Hadley B1 IPCC climate scenario. The B1 
  world is a convergent world with the same global population as in the A1 storyline but 
  with rapid changes in economic structures toward a service and information economy, with 
  reductions in material intensity, and the introduction of clean and resource-efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (model SuccessionalStage
    (classification (ranking ecology:SuccessionalStage)
      #{5 6 4}    OldGrowth
	 		3           MidSuccession
	 		2           EarlySuccession
	 		1           PoleSuccession
	 		:otherwise  NoSuccession)))  
       
(defscenario ipcc-hadley-a2 IPCCHadleyA2
	  "This scenario represents the effects of the Hadley A2 IPCC climate scenario. A2
     represents a very heterogeneous world with continuously increasing global population 
     and regionally oriented economic growth that is more fragmented and slower than in
     other storylines." 
) 

(defscenario ipcc-hadley-b2 IPCCHadleyB2
	  "This scenario represents the effects of the Hadley B2 IPCC climate scenario. B2 
     is a world in which the emphasis is on local solutions to economic, social, and 
     environmental sustainability, with continuously increasing population (lower than A2)
     and intermediate economic development. " 
) 
		 			

		 			