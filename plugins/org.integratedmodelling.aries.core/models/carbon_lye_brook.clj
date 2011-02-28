(ns core.models.carbon-lyebrook ;;Model is for Vermont forests (Jennifer Wright's model)
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            probabilistic-measurement probabilistic-classification
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
  (probabilistic-measurement VegetationAndSoilCarbonStorage "t/ha" 
                  [500 3200]    VeryHighStorage
                  [300 500]     HighStorage
                  [150 300]     ModerateStorage
                  [75 150]      LowStorage
                  [0.01 75]     VeryLowStorage
                  [0 0.01]      NoStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

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

(defmodel summer-high-winter-low SummerHighWinterLow
     (classification (ranking habitat:SummerHighWinterLow)
        [40 :>]       VeryHighSOL
        [34 40]       HighSOL
        [29 34]       ModerateSOL
        [24 29]       LowSOL
        [:< 24]       VeryLowSOL))

(defmodel stand-condition StandCondition
      (classification (ranking habitat:StandCondition) 
          #{4 5 6}      HighStandCondition
          #{7 8 9}      ModerateStandCondition
          #{1 2 3}      LowStandCondition
          :otherwise    NoStandCondition))

(defmodel stand-size-density StandSizeDensity
      (classification (ranking habitat:StandSizeDensity) 
          #{5 6 8 9}    HighStandSizeDensity
          #{3 4 7}      ModerateStandSizeDensity
          #{1 2}        LowStandSizeDensity
          0             NoStandSizeDensity))
          
(defmodel soil-CN-ratio SoilCNRatio
      (classification (ranking habitat:SoilCNRatio)
          [35 :>]         VeryHighCNRatio
          [20 35]         HighCNRatio
          [10 20]         LowCNRatio
          [:< 10]         VeryLowCNRatio)) 

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
            :import   "aries.core::CarbonSequestrationLyeBrook.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (Slope)
            :result   veg-soil-sequestration
            :context  (soil-CN-ratio stand-size-density stand-condition summer-high-winter-low)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

;;Use Bayesian priors for insect & blowdown frequencies

(defmodel fire-frequency FireFrequency
     (classification (ranking habitat:FireFrequency) 
          [0.9 :>]    HighFireFrequency
          [0.25 0.9]  ModerateFireFrequency 
          [0.05 0.25] LowFireFrequency
          [:< 0.05]   NoFireFrequency))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [12 300]    VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]      HighRelease
                  [6 9]       ModerateRelease
                  [3 6]       LowRelease
                  [0.01 3]    VeryLowRelease
                  [0 0.01]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseLyeBrook.xdsl"
            :keep     (StoredCarbonRelease)
            :required (Slope)
            :result   stored-carbon-release
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
        :keep (StoredCarbonRelease CarbonSequestration 
               GreenhouseGasEmissions PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse DetrimentalCarbonSource
               UsedCarbonSink SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus CarbonMitigationDeficit
               DepletedCarbonMitigation DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))


		 			