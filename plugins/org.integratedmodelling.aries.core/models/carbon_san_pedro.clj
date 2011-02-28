(ns core.models.carbon-san-pedro
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
                  [500 3200]    VeryHighStorage
                  [300 500]     HighStorage
                  [150 300]     ModerateStorage
                  [75 150]      LowStorage
                  [0.01 75]     VeryLowStorage
                  [0 0.01]      NoStorage))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha" 
                  [325 2301]      VeryHighVegetationStorage
                  [190 325]       HighVegetationStorage
                  [105 190]       ModerateVegetationStorage
                  [40 105]        LowVegetationStorage
                  [0.01 40]       VeryLowVegetationStorage
                  [0 0.01]        NoVegetationStorage)) 			
      
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

;;NB: ARIES defines sources of carbon emissions as areas at risk of deforestation or fire, which can release carbon
;; into the atmosphere.  Sinks are areas that are sequester carbon in vegetation and soils.  The difference between 
;; carbon sinks and sources is the amount remaining to mitigate direct anthropogenic emissions (aside from land conversion
;; and fire).

(defmodel percent-vegetation-cover PercentVegetationCover
  (classification (ranking habitat:PercentVegetationCover :units "%")
                  [80 100] VeryHighVegetationCover
                  [60 80]  HighVegetationCover
                  [40 60]  ModerateVegetationCover
                  [20 40]  LowVegetationCover
                  [0 20]   VeryLowVegetationCover))

;;ARE WE OK HAVING SOME CLASSES IN THE DATA WITH NO CORRESPONDING DISCRETE STATES (i.e., ag, developed, barren)?
;; Not considered but in the area: 5 9 15 16 17 18 19 20 21 65 93 110 111 112 114 117 (could set these to hard, indicating slower sequestration)
;;Ditto for Mexico: most LULC categories are not used, and not all discrete states are represented.
(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
     (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
        #{14 34 35 83 92}                   LowHardness
        #{33 45 51 91}                      ModerateHardness
        #{52 55 56 57 59 60 84 96 105 118}  HighHardness)
     (classification (categorization mexico:CONABIOLULCCategory)
        #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"} LowHardness
        #{"Chaparral"}                                              ModerateHardness
        #{"Bosque de encino" "Mezquital-huizachal"}                 HighHardness))

;;Have removed this from the model and replaced it with annual precip.  This is probably a better variable to include
;; in carbon models in wetter regions, while annual precip is far more important in water-limited regions.
(defmodel summer-high-winter-low SummerHighWinterLow
     (classification (ranking habitat:SummerHighWinterLow)
        [:< 24]       VeryLowSOL
        [24 30]       LowSOL
        [30 35]       ModerateSOL
        [35 40]       HighSOL
        [40 :>]       VeryHighSOL))

;;Brown et al. (2010) use 0-130, 130-230, 230-460, >460 mm as their discretization for rangeland carbon modeling.
;; For the San Pedro, the entire valley floor would be in the 230-460 range and the surrounding mountains as >460.
;; For now, keep the below discretization, though strongly consider using it.
(defmodel annual-precipitation MeanAnnualPrecipitation
     (classification (measurement habitat:AnnualPrecipitation "mm")
        [500 :>]        HighMeanAnnualPrecipitation
        [400 500]       ModerateMeanAnnualPrecipitation
        [:< 400]        LowMeanAnnualPrecipitation))

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
;;This should work OK with both global & SSURGO data, but check to make sure.
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhDeep)
                  [7.3 :>]       HighPh
                  [5.5 7.3]      ModeratePh
                  [:< 5.5]       LowPh))

(defmodel slope Slope
    (classification (measurement geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    Level
       [1.15 4.57]  GentlyUndulating
       [4.57 16.70] RollingToHilly
       [16.70 :>]   SteeplyDissectedToMountainous))

;;Use NLCD or GLC layers to infer anoxic vs. oxic: no Mexican LULC data (i.e., CONABIO) 
;; denote wetlands at least for Sonora.
(defmodel oxygen SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
                  #{90 95}   AnoxicSoils
                  :otherwise OxicSoils)
  (classification (numeric-coding glc:GLCNumeric)
                  15         AnoxicSoils
                  :otherwise OxicSoils))

;;Per Schussman et al. (2006), the middle of each of these ranges is around every 5 yrs for high frequency, 50 yrs for moderate, 200 yrs for low.
(defmodel fire-frequency FireFrequency
  (classification (numeric-coding habitat:FireReturnInterval) 
                  1        HighFireFrequency
                  #{2 3}   ModerateFireFrequency ;;includes "variable" fire frequency
                  4        LowFireFrequency
                  #{5 6}   NoFireFrequency))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [12 200]   VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]      HighRelease
                  [6 9]       ModerateRelease
                  [3 6]       LowRelease
                  [0.01 3]    VeryLowRelease
                  [0 0.01]    NoRelease))

;;Consider reworking the soil carbon storage part of the model based on Martens et al. 2005 - soil texture, precip, 
;; temperature as most important correlates of high soil carbon storage.

(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :keep     (StoredCarbonRelease)
            :result   (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio 
                        annual-precipitation fire-frequency)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon emissions as areas at risk of deforestation or fire, which can release carbon
;; into the atmosphere.  Sinks are areas that are sequester carbon in vegetation and soils.  The difference between 
;; carbon sinks and sources is the amount remaining to mitigate direct anthropogenic emissions (aside from land conversion
;; and fire).

(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [12 30]     VeryHighSequestration
                  [9 12]      HighSequestration
                  [6 9]       ModerateSequestration
                  [1 6]       LowSequestration
                  [0.01 1]    VeryLowSequestration  ;;Common annual values for desert scrub & desert grassland (Svejvcar et al. 2008)
                  [0 0.01]    NoSequestration))

;; Bayesian source model
(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::CarbonSequestrationSanPedro.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :result   (veg-soil-sequestration)
            :context  (hardwood-softwood-ratio percent-vegetation-cover annual-precipitation)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

;:GHG emissions map for the U.S.  For the rest of the world, use global population density layer multiplied
;; by per capita emissions for that country from EIA.  2006 data used as this corresponds to current population
;; density layer: 4.05 tonnes CO2/capita for Mexico in 2006, which is equivalent to 1.105 tonnes C/capita

(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))

;;(defmodel use-simple GreenhouseGasEmitters
;;  [(categorization geofeatures:Country :as country)]
;;  (measurement GreenhouseGasEmissions "t/ha*year"
;;               :when #(= (:country %) "United States"))
;;  (measurement GreenhouseGasEmissions "t/ha*year"
;;               :context ((count policytarget:PopulationDensity "/km^2" :as population-density-count))
;;               :state   #(* (:population-density-count %) 1.105)))

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
        StoredCarbonRelease
        GreenhouseGasEmissions
        VegetationAndSoilCarbonSequestration
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
        ;;:downscaling-factor 2
        :downscaling-factor 1
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :save-file          "/home/gjohnson/code/java/imt/identifications/carbon_san_pedro_data.clj"
        :keep (StoredCarbonRelease CarbonSequestration 
               GreenhouseGasEmissions PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse DetrimentalCarbonSource
               UsedCarbonSink SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus CarbonMitigationDeficit
               DepletedCarbonMitigation DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario mesquite-management sanPedro:MesquiteManagement: change to VeryLowVegetationCover in source & sink models
;;  within mesquite management polygons)
      
;;(defscenario urban-growth sanPedro:UrbanGrowth: change developed areas to VeryLowVegetationCover in source & sink
      ;; models; change fire-frequency to NoFireFrequency; bump up use by 10.4% in constrained and 56.8% in open development scenarios)
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development sanPedro:BSRDevelopment: change developed areas to VeryLowVegetationCover in source & sink
      ;; models; change fire-frequency to NoFireFrequency; bump up use by 3.6%)
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5

