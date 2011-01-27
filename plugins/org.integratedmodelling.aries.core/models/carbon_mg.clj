(ns core.models.carbon-mg
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)


;; these are the undiscretization statements, necessary for training purposes.
;; output and training TODO make it classify the appropriate measurement - buggy for now
;;KB: Should this actually be sequestration?  And should discretization on the two 
;; below defmodels be updated?
(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
	(classification VegetationAndSoilCarbonStorage
						:units "t/ha" 
	  				[1000 3200]   VeryHighStorage
            [600 1000]    HighStorage
            [300 600]     ModerateStorage
            [100 300]     LowStorage
            [0.01 100]    VeryLowStorage
            [0 0.01]      NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
;;These values may be a bit high - compare to the mg data for veg C storage.
(defmodel veg-storage VegetationCarbonStorage
	(classification VegetationCarbonStorage
						:units "t/ha" 
	  				[900 2301]     VeryHighVegetationStorage
            [500 900]      HighVegetationStorage
            [250 500]      ModerateVegetationStorage
            [75 250]       LowVegetationStorage
            [0.01 75]      VeryLowVegetationStorage
            [0 0.01]       NoVegetationStorage)) 			

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage SoilCarbonStorage
		(classification SoilCarbonStorage
						:units    "t/ha" 
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
	 		  
(defmodel percent-vegetation-cover PercentVegetationCover
	(classification (ranking habitat:PercentVegetationCover :units "%")
		[80 :>] VeryHighVegetationCover
		[60 80] HighVegetationCover
		[40 60] ModerateVegetationCover
		[20 40] LowVegetationCover
		[0 20]  VeryLowVegetationCover))

(defmodel summer-high-winter-low SummerHighWinterLow
		 (classification (ranking habitat:SummerHighWinterLow)
        [0 24]        VeryLowSOL
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

(defmodel degradation-status DegradationStatus
  (classification (numeric-coding mglulc:MGLULCNumeric)
         #{3 7 23}         Degraded
         :otherwise       NotDegraded)) 

(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (classification VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]     VeryHighSequestration
                  [9 12]      HighSequestration
                  [6 9]       ModerateSequestration
                  [3 6]       LowSequestration
                  [0.01 3]    VeryLowSequestration
                  [0 0.01]    NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationMg.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (SummerHighWinterLow)
            :observed (veg-soil-sequestration)
            :context  (percent-vegetation-cover summer-high-winter-low soil-cn-ratio degradation-status)))

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

;; Values of "1" correspond to high pH (>7.3), "2" to low pH (<5.5), "3-4" to intermediate pH (5.5-7.3)
;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph Soilph
     (classification (ranking habitat:SoilPhShallow)
        #{3 4}  HighPh
        2       ModeratePh
        1       LowPh))

;; Mg wetlands layer is just a wetlands layer - no reclass of a LULC layer.
(defmodel oxygen SoilOxygenConditions
 (classification (binary-coding habitat:Wetland)
      1          AnoxicSoils
      :otherwise OxicSoils))

;;No data on fire frequency for Madagascar - use Bayesian priors until we can get a layer.

(defmodel population-density PopulationDensity
  (classification (count policytarget:PopulationDensity "/km^2")
    [12 :>]    HighPopulationDensity
    [7 12]     ModeratePopulationDensity
    [:< 7]     LowPopulationDensity))

(defmodel deforestation-risk DeforestationRiskClass
  (classification (ranking DeforestationRisk)
       "High"                HighDeforestationRisk
       "Moderate"            ModerateDeforestationRisk
       "Low"                 LowDeforestationRisk
       :otherwise            NoDeforestationRisk)) 

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (classification StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 1200]   VeryHighRelease ;;Need to check this ceiling; values of 1000 can be found in the Pacific NW rainforests, need to see how this compares to Madagascar's rainforests.
                  [9 12]      HighRelease
                  [6 9]       ModerateRelease
                  [3 6]       LowRelease
                  [0.01 3]    VeryLowRelease
                  [0 0.01]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseMg.xdsl"
            :keep     (StoredCarbonRelease)
            :required (SummerHighWinterLow)
            :observed (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover summer-high-winter-low soil-cn-ratio
                         degradation-status population-density deforestation-risk)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

;:GHG emissions map for Madagascar: use global population density layer multiplied by per capita emissions
;; for that country from EIA.  2006 data used as this corresponds to current population density layer: 
;; 0.14 tonnes CO2/capita for Madagascar in 2006, which is equivalent to 0.04 tonnes C/capita
(defmodel use-simple GreenhouseGasEmitters
  (measurement GreenhouseGasEmissions "t/ha*year"
               :context ((count policytarget:PopulationDensity "/km^2" :as population-density-count))
               :state   #(* (:population-density-count %) 0.04)))
 	 					
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
;; Scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized IPCCHadleyA2Incentivized
  "This scenario represents the effects of the Hadley A1 IPCC climate scenario. A12 
  represents a future world of very rapid economic growth, global population that peaks 
  in mid-century and declines thereafter, and rapid introduction of new and more efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking ecology:SuccessionalStage)
	 		#{5 6 4}    OldGrowth
	 		3           MidSuccession
	 		2           EarlySuccession
	 		1           PoleSuccession
	 		:otherwise  NoSuccession)) 

(defscenario ipcc-hadley-b2-incentivized IPCCHadleyB2Incentivized
  "This scenario represents the effects of the Hadley B1 IPCC climate scenario. The B1 
  world is a convergent world with the same global population as in the A1 storyline but 
  with rapid changes in economic structures toward a service and information economy, with 
  reductions in material intensity, and the introduction of clean and resource-efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking ecology:SuccessionalStage)
      #{5 6 4}    OldGrowth
	 		3           MidSuccession
	 		2           EarlySuccession
	 		1           PoleSuccession
	 		:otherwise  NoSuccession))  
       
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
		 			

		 			