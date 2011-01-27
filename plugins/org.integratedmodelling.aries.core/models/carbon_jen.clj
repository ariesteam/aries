(ns core.models.carbon-lyebrook
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
  (classification VegetationAndSoilCarbonStorage
                  :units      "t/ha" 
                  [500 3200]    VeryHighStorage
                  [300 500]     HighStorage
                  [150 300]     ModerateStorage
                  [75 150]      LowStorage
                  [0.01 75]     VeryLowStorage
                  [0 0.01]      NoStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel summer-high-winter-low SummerHighWinterLow
		 (classification (ranking habitat:SummerHighWinterLow)
        [40 :>]       VeryHighSOL
        [34 40]       HighSOL
        [29 34]       ModerateSOL
        [24 29]       LowSOL
        [:< 24]       VeryLowSOL))

(defmodel stand-condition StandCondition
			(classification (ranking habitat:StandCondition) 
					#{4 5 6}			HighStandCondition
					#{7 8 9}			ModerateStandCondition
					#{1 2 3}			LowStandCondition
					:otherwise		NoStandCondition))

(defmodel stand-size-density StandSizeDensity
			(classification (ranking habitat:StandSizeDensity) 
					#{5 6 8 9}		HighStandSizeDensity
					#{3 4 7}	  	ModerateStandSizeDensity
					#{1 2}				LowStandSizeDensity
					0							NoStandSizeDensity))
					
(defmodel soil-CN-ratio SoilCNRatio
			(classification (ranking habitat:SoilCNRatio)
					[35 :>]					VeryHighCNRatio
					[20 35]					HighCNRatio
					[10 20]					LowCNRatio
					[:< 10]					VeryLowCNRatio)) 

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
            :import   "aries.core::CarbonSequestrationJen.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
            :context  (soil-CN-ratio stand-size-density stand-condition summer-high-winter-low)))
 	    
;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel fire-frequency FireFrequency
     (classification (ranking habitat:FireFrequency) 
          [0.9 :>]    HighFireFrequency
          [0.25 0.9]  ModerateFireFrequency 
          [0.05 0.25] LowFireFrequency
          [:< 0.05]   NoFireFrequency))

(defmodel veg-storage VegetationCarbonStorage
  (classification (measurement habitat:VegetationCarbonStorage "t/ha")
            [80 :>]    VeryHighVegetationStorage
            [70 80]    HighVegetationStorage
            [50 70]    ModerateVegetationStorage
            [0 50]     LowVegetationStorage
            0          NoVegetationStorage))         

(defmodel soil-storage SoilCarbonStorage
    (classification (measurement habitat:SoilCarbonStorage "t/ha") 
            [210 :>]   VeryHighSoilStorage
            [140 210]  HighSoilStorage
            [70 140]   ModerateSoilStorage
            [0 70]     LowSoilStorage
            0          NoSoilStorage))

;;Use Bayesian priors for insect & blowdown frequencies

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (classification StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 300]    VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]      HighRelease
                  [6 9]       ModerateRelease
                  [3 6]       LowRelease
                  [0.01 3]    VeryLowRelease
                  [0 0.01]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseJen.xdsl"
            :keep     (StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (fire-frequency veg-storage soil-storage)))

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
		 			

		 			