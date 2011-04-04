(ns core.models.carbon-mg
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification 
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

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

;;This discretization is based off comparison with Reusch & Gibbs' global vegetation carbon storage layer, with maximum values from \
;; Madagascar being slightly over half of that in Puget Sound.  Need to run this by folks familiar with carbon data/forestry in Madagascar.
;; Note that using spatial data to determine these magnitudes (versus published literature estimates like Smith et al. 2006 for the United
;; States) gives different relative magnitudes and that for San Pedro, Puget Sound, California, and Vermont we used Smith et al. 2006 for
;; discretization.
(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
            [300 500]      VeryHighVegetationStorage  
            [100 300]      HighVegetationStorage
            [25 100]       ModerateVegetationStorage
            [10 25]        LowVegetationStorage
            [0.01 10]      VeryLowVegetationStorage
            [0 0.01]       NoVegetationStorage))      

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
            :import   "aries.core::StoredCarbonReleaseMg.xdsl"
            :context  (percent-vegetation-cover summer-high-winter-low degradation-status population-density)
            :required (SummerHighWinterLow)
            :result    veg-storage
            :keep     (VegetationCarbonStorage)))

;;This discretization is based off comparison with Reusch & Gibbs' global vegetation carbon storage layer, with maximum values from 
;; Madagascar being 133% of that in Puget Sound.  Need to run this by folks familiar with carbon data in Madagascar.
;; Note that using spatial data to determine these magnitudes (versus published literature estimates like Smith et al. 2006 for the United
;; States) gives different relative magnitudes and that for San Pedro, Puget Sound, California, and Vermont we used Smith et al. 2006 for
;; discretization.
(defmodel soil-storage SoilCarbonStorage
    (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
            [75 150]       VeryHighSoilStorage
            [40 75]        HighSoilStorage
            [20 40]        ModerateSoilStorage
            [10 20]        LowSoilStorage
            [0.01 10]      VeryLowSoilStorage
            [0 0.01]       NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
            :import   "aries.core::StoredCarbonReleaseMg.xdsl"
            :context  (soil-cn-ratio degradation-status soil-ph slope oxygen percent-vegetation-cover)
            :required (SummerHighWinterLow)
            :result    soil-storage
            :keep     (SoilCarbonStorage)))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
               :context (vegetation-carbon-storage :as vegetation-c-storage soil-carbon-storage :as soil-c-storage) 
               :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                          (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
            [300 650]     VeryHighStorage
            [100 300]     HighStorage
            [50 100]      ModerateStorage
            [10 50]       LowStorage
            [0.01 10]     VeryLowStorage
            [0 0.01]      NoStorage))

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
            :context  (veg-soil-storage degradation-status)))

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
		

		 			