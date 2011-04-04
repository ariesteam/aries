(ns core.models.carbon-puget
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            probabilistic-measurement probabilistic-classification
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService
  (PercentVegetationCover :editable "true")
  (FireFrequency :editable "true")
  (GreenhouseGasEmissions :editable "true")
  (thinklab-core:BooleanRanking
        (LandOrSea
            (OnLand) (NotOnLand))))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))  

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
    (classification  (measurement geophysics:Altitude "m")
       [:exclusive 0 :>] OnLand))

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
    [80 100 :inclusive] VeryHighVegetationCover
    [60 80]             HighVegetationCover
    [40 60]             ModerateVegetationCover
    [20 40]             LowVegetationCover
    [1 20]              VeryLowVegetationCover))

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
        [8 10 :inclusive] VeryLowHardness
        [6 8]             LowHardness
        [4 6]             ModerateHardness
        [2 4]             HighHardness
        [1 2]             VeryHighHardness))

;;Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [9 14]     VeryHighSequestration
                  [6.5 9]    HighSequestration
                  [4 6.5]    ModerateSequestration
                  [2 4]      LowSequestration
                  [0.01 2]   VeryLowSequestration
                  [0 0.01]   NoSequestration))

;; Bayesian sink model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestration.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (LandOrSea)
            :result    veg-soil-sequestration
            :context  (hardwood-softwood-ratio soil-cn-ratio summer-high-winter-low 
                       percent-vegetation-cover successional-stage land-selector)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

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

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
            [500 900]      VeryHighVegetationStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
            [200 500]      HighVegetationStorage
            [75 200]       ModerateVegetationStorage
            [25 75]        LowVegetationStorage
            [0.01 25]      VeryLowVegetationStorage
            [0 0.01]       NoVegetationStorage)) 

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
            :import   "aries.core::StoredCarbonRelease.xdsl"
            :context  (percent-vegetation-cover hardwood-softwood-ratio 
                       successional-stage summer-high-winter-low land-selector)
            :required (LandOrSea)
            :result    veg-storage
            :keep     (VegetationCarbonStorage)))

(defmodel soil-storage SoilCarbonStorage
    (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
            [60 115]       VeryHighSoilStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
            [30 60]        HighSoilStorage
            [15 30]        ModerateSoilStorage
            [5 15]         LowSoilStorage
            [0.01 5]       VeryLowSoilStorage
            [0 0.01]       NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
            :import   "aries.core::StoredCarbonRelease.xdsl"
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio 
                       successional-stage soil-cn-ratio land-selector)
            :required (LandOrSea)
            :result    soil-storage
            :keep     (SoilCarbonStorage)))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
               :context (vegetation-carbon-storage :as vegetation-c-storage soil-carbon-storage :as soil-c-storage) 
               :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                          (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
            [550 1015]    VeryHighStorage
            [250 550]     HighStorage
            [100 250]     ModerateStorage
            [30 100]      LowStorage
            [0.01 30]     VeryLowStorage
            [0 0.01]      NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [200 500]     VeryHighRelease ;;Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
                  [100 200]     HighRelease
                  [50 100]      ModerateRelease
                  [20 50]       LowRelease
                  [0.01 20]     VeryLowRelease
                  [0 0.01]      NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonRelease.xdsl"
            :keep     (StoredCarbonRelease)
            :required (LandOrSea)
            :result    stored-carbon-release
            :context  (veg-soil-storage fire-frequency land-selector)))
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
        CarbonSourceValue
        GreenhouseGasEmitters
        CarbonSinkValue
        nil
        nil
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 20
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_san_pedro_data.clj")
        :keep (StoredCarbonRelease
               CarbonSequestration
               GreenhouseGasEmissions
               PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse
               DetrimentalCarbonSource
               UsedCarbonSink
               SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus
               CarbonMitigationDeficit
               DepletedCarbonMitigation
               DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))
		
;; ----------------------------------------------------------------------------------------------
;; scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized 
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

(defscenario ipcc-hadley-b2-incentivized 
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
       
(defscenario ipcc-hadley-a2 
	  "This scenario represents the effects of the Hadley A2 IPCC climate scenario. A2
     represents a very heterogeneous world with continuously increasing global population 
     and regionally oriented economic growth that is more fragmented and slower than in
     other storylines." 
) 

(defscenario ipcc-hadley-b2 
	  "This scenario represents the effects of the Hadley B2 IPCC climate scenario. B2 
     is a world in which the emphasis is on local solutions to economic, social, and 
     environmental sustainability, with continuously increasing population (lower than A2)
     and intermediate economic development. " 
) 
		 			

		 			