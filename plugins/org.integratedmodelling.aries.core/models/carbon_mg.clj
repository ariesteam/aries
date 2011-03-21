(ns core.models.carbon-mg
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification 
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

;; output and training 
(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
	(probabilistic-measurement VegetationAndSoilCarbonStorage "t/ha" 
	  				[1000 3200]   VeryHighStorage
            [600 1000]    HighStorage
            [300 600]     ModerateStorage
            [100 300]     LowStorage
            [0.01 100]    VeryLowStorage
            [0 0.01]      NoStorage))

;;These values may be a bit high - compare to the mg data for veg C storage.
(defmodel veg-storage VegetationCarbonStorage
	(probabilistic-measurement VegetationCarbonStorage "t/ha" 
	  				[900 2301]     VeryHighVegetationStorage
            [500 900]      HighVegetationStorage
            [250 500]      ModerateVegetationStorage
            [75 250]       LowVegetationStorage
            [0.01 75]      VeryLowVegetationStorage
            [0 0.01]       NoVegetationStorage)) 			
		
(defmodel soil-storage SoilCarbonStorage
		(probabilistic-measurement SoilCarbonStorage "t/ha" 
	  				[680 820]      VeryHighSoilStorage
            [440 680]      HighSoilStorage
            [200 440]      ModerateSoilStorage
            [50 200]       LowSoilStorage
            [0.01 50]      VeryLowSoilStorage
            [0 0.01]       NoSoilStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

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
         :otherwise        NotDegraded))

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
            :import   "aries.core::CarbonSequestrationMg.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (SummerHighWinterLow)
            :result   veg-soil-sequestration
            :context  (percent-vegetation-cover summer-high-winter-low soil-cn-ratio degradation-status)))

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
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
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
            :result   stored-carbon-release
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
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_mg_data.clj")
        :keep (StoredCarbonRelease CarbonSequestration 
               GreenhouseGasEmissions PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse DetrimentalCarbonSource
               UsedCarbonSink SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus CarbonMitigationDeficit
               DepletedCarbonMitigation DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))
		

		 			