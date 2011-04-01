(ns core.models.carbon-vt ;;Model is for Vermont agriculture (Sam Gorton's model)
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

;; No data here (at least apparently, see what Sam has to say) - use priors unless there's a layer.  Assume the below discretization
;; is in percentages?
;;(defmodel biomass-removal-rate BiomassRemovalRate
;;      (classification (ranking habitat:BiomassRemovalRate)  
;;          [90 :>]       VeryHighRemovalRate
;;          [66 90]       HighRemovalRate
;;          [10 66]       LowRemovalRate
;;          [:< 10]       VeryLowRemovalRate)) 

;;No data here for "biomass residue input," "soil tillage" "biomass removal rate"- use priors

(defmodel summer-high-winter-low SummerHighWinterLow
     (classification (ranking habitat:SummerHighWinterLow)
        [44 :>]       VeryHighSOL
        [42 44]       HighSOL
        [40 42]       ModerateSOL
        [38 40]       LowSOL
        [:< 38]       VeryLowSOL))

(defmodel mean-annual-precip MeanAnnualPrecipitation
      (classification (measurement habitat:AnnualPrecipitation "mm")
          [2500 :>]         VeryHighMeanAnnualPrecipitation
          [1500 2500]       HighMeanAnnualPrecipitation
          [1000 1500]       ModerateMeanAnnualPrecipitation
          [700 1000]        LowMeanAnnualPrecipitation
          [:< 700]          VeryLowMeanAnnualPrecipitation))
            
(defmodel soil-CN-ratio SoilCNRatio
      (classification (ranking habitat:SoilCNRatio)
          [25 :>]       VeryHighCNRatio
          [17.5 25]     HighCNRatio
          [10 17.5]     LowCNRatio
          [:< 10]       VeryLowCNRatio))

(defmodel veg-type VegetationType
      (classification (ranking VegType)
          #{1 5 25}     RowCrops
          #{36 37 62}   GrasslandHerbaceous
          63            Forest
          87            Wetland
          #{61 82}      NoVegetation
          111           OpenWater))

;;Not used in the model but masks out carbon over open water
(defmodel slope Slope
    (classification (measurement geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    Level
       [1.15 4.57]  GentlyUndulating
       [4.57 16.70] RollingToHilly
       [16.70 :>]   SteeplyDissectedToMountainous))


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
            :import   "aries.core::CarbonSequestrationVt.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (Slope)
            :result   veg-soil-sequestration
            :context  (summer-high-winter-low mean-annual-precip soil-CN-ratio veg-type slope)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
            [100 300]         VeryHighVegetationStorage ;; High value bound from Smith et al. (2006); check with local experts.
            [50 100]          HighVegetationStorage
            [25 50]           ModerateVegetationStorage
            [10 25]           LowVegetationStorage
            [0.01 10]         VeryLowVegetationStorage
            [0 0.01]          NoVegetationStorage))   

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
            :import   "aries.core::StoredCarbonReleaseVt.xdsl"
            :context  (veg-type summer-high-winter-low mean-annual-precip)
            :result    veg-storage
            :keep     (VegetationCarbonStorage)))

(defmodel soil-storage SoilCarbonStorage
    (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
            [60 100]          VeryHighSoilStorage  ;; High value bound from Smith et al. (2006); check with local experts.
            [35 60]           HighSoilStorage
            [10 35]           ModerateSoilStorage
            [5 10]            LowSoilStorage
            [0.01 5]          VeryLowSoilStorage
            [0 0.01]          NoSoilStorage))   

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
            :import   "aries.core::StoredCarbonReleaseVt.xdsl"
            :context  (veg-type soil-CN-ratio)
            :result    soil-storage
            :keep     (SoilCarbonStorage)))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
               :context (vegetation-carbon-storage :as vegetation-c-storage soil-carbon-storage :as soil-c-storage) 
               :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                          (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage 
            [150 400]          VeryHighStorage
            [75 150]           HighStorage
            [40 75]            ModerateStorage
            [20 40]            LowStorage
            [0.02 20]          VeryLowStorage
            [0 0.02]           NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [75 200]    VeryHighRelease ;;Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
                  [30 75]     HighRelease
                  [15 30]     ModerateRelease
                  [5 15]      LowRelease
                  [0.01 5]    VeryLowRelease
                  [0 0.01]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseVt.xdsl"
            :keep     (StoredCarbonRelease)            
            :required (Slope)
            :result   stored-carbon-release
            :context  (veg-soil-storage)))  ;; add biomass-removal-rate if there's supporting data
  	 		
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
		
		 			

		 			