(ns core.models.carbon-san-pedro
  (:refer-clojure :rename {count length})
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel model measurement classification
                           categorization ranking numeric-coding binary-coding
                           probabilistic-measurement probabilistic-classification
                           identification bayesian namespace-ontology count])
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
                  [80 100 :inclusive] VeryHighVegetationCover
                  [60 80]             HighVegetationCover
                  [40 60]             ModerateVegetationCover
                  [20 40]             LowVegetationCover
                  [0 20]              VeryLowVegetationCover))

;;ARE WE OK HAVING SOME CLASSES IN THE DATA WITH NO CORRESPONDING DISCRETE STATES (i.e., ag, developed, barren)?
;; Not considered but in the area: 5 9 15 16 17 18 19 20 21 65 93 110 111 112 114 117 (could set these to hard, indicating slower sequestration)
;;Ditto for Mexico: most LULC categories are not used, and not all discrete states are represented.
(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
     (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
        #{14 34 35 83 92}                   LowHardness
        #{33 45 51 91}                      ModerateHardness
        #{52 55 56 57 59 60 84 96 105 118}  HighHardness))
;;     (classification (categorization mexico:CONABIOLULCCategory)
;;        #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"} LowHardness
;;        #{"Chaparral"}                                              ModerateHardness
;;        #{"Bosque de encino" "Mezquital-huizachal"}                 HighHardness))

;;Brown et al. (2010) use 0-130, 130-230, 230-460, >460 mm as their discretization for rangeland carbon modeling.
;; For the San Pedro, the entire valley floor would be in the 230-460 range and the surrounding mountains as >460.
;; For now, keep the below discretization, though strongly consider using it.
(defmodel annual-precipitation MeanAnnualPrecipitation
     (classification (measurement habitat:AnnualPrecipitation "mm")
        [500 :>]        HighMeanAnnualPrecipitation
        [400 500]       ModerateMeanAnnualPrecipitation
        [:< 400]        LowMeanAnnualPrecipitation))

(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [3 4.6]       VeryHighSequestration
                  [2 3]         HighSequestration
                  [1.5 2]       ModerateSequestration
                  [1 1.5]       LowSequestration
                  [0.01 1]      VeryLowSequestration ;;Common annual values for desert scrub & desert grassland (Svejvcar et al. 2008); values can also be negative in dry years, should ideally account for that too.
                  [0 0.01]      NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationSanPedro.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :result   veg-soil-sequestration
            :context  (hardwood-softwood-ratio percent-vegetation-cover annual-precipitation)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

;;Have removed this from the model and replaced it with annual precip.  This is probably a better variable to include
;; in carbon models in wetter regions, while annual precip is far more important in water-limited regions.
(defmodel summer-high-winter-low SummerHighWinterLow
     (classification (ranking habitat:SummerHighWinterLow)
        [:< 24]       VeryLowSOL
        [24 30]       LowSOL
        [30 35]       ModerateSOL
        [35 40]       HighSOL
        [40 :>]       VeryHighSOL))

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
;;This should work OK with both global & SSURGO data, but check to make sure.
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhDeep)
                  [7.3 :>]                 HighPh
                  [5.5 7.3]                ModeratePh
                  [:exclusive 0 5.5]       LowPh))

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

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
                  [75 100]        VeryHighVegetationStorage
                  [20 75]         HighVegetationStorage
                  [5 20]          ModerateVegetationStorage
                  [2 5]           LowVegetationStorage
                  [0.01 2]        VeryLowVegetationStorage
                  [0 0.01]        NoVegetationStorage)) 

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :context  (annual-precipitation percent-vegetation-cover hardwood-softwood-ratio)
            :result    veg-storage
            :keep     (VegetationCarbonStorage)))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
                  [40 80]        VeryHighSoilStorage
                  [20 40]        HighSoilStorage
                  [5 20]         ModerateSoilStorage
                  [2 5]          LowSoilStorage
                  [0.01 2]       VeryLowSoilStorage
                  [0 0.01]       NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio)
            :result    soil-storage
            :keep     (SoilCarbonStorage)))

;;Consider reworking the soil carbon storage part of the model based on Martens et al. 2005 - soil texture, precip, 
;; temperature as most important correlates of high soil carbon storage.

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
               :context (vegetation-carbon-storage soil-carbon-storage)
               :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                          (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
                  [50 180]    VeryHighStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [15 50]     HighStorage
                  [6 15]      ModerateStorage
                  [3 6]       LowStorage
                  [0.02 3]    VeryLowStorage
                  [0 0.02]    NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                             [12 90]     VeryHighRelease ;;Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
                             [9 12]      HighRelease
                             [6 9]       ModerateRelease
                             [3 6]       LowRelease
                             [0.02 3]    VeryLowRelease
                             [0 0.02]    NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :keep     (StoredCarbonRelease)
            :result   stored-carbon-release
            :context  (veg-soil-storage fire-frequency)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

;:GHG emissions map for the U.S.  For the rest of the world, use global population density layer multiplied
;; by per capita emissions for that country from EIA.  2006 data used as this corresponds to current population
;; density layer: 4.05 tonnes CO2/capita for Mexico in 2006, which is equivalent to 1.105 tonnes C/capita

;;(defmodel use-simple GreenhouseGasEmissions
;;  (measurement GreenhouseGasEmissions "t/ha*year"))

(defmodel use-simple GreenhouseGasEmissions
  [(categorization geofeatures:Country) :as country]
  (measurement GreenhouseGasEmissions "t/ha*year")
               :when #(= (:country %) "United States")
  (measurement GreenhouseGasEmissions "t/ha*year"
               :context ((count policytarget:PopulationDensity "/km^2" :as population-density-count))
               :state   #(* (:population-density-count %) 1.105)))

(defmodel use-simple-pop GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"
               :context ((count policytarget:PopulationDensity "/km^2" :as population-density-count))
               :state   #(* (:population-density-count %) 1.105)))

(defmodel use-simple-us GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))
;; ----------------------------------------------------------------------------------------------
;; Top-level service models
;; ----------------------------------------------------------------------------------------------

(defmodel identification-carbon ClimateStability
  (identification ClimateStability
                  :context (source sink use-simple)))

(defmodel carbon-flow ClimateStability
  (span CO2Removed
        CarbonSourceValue
        GreenhouseGasEmissions
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
;; Scenarios

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defmodel constrained-development-scenario sanPedro:ConstrainedDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained) 
      #{10 11 12 13 19 22 25}                      sanPedro:DevelopedConstrained
      #{1 2 4 5 6 7 8 9 14 16 23 26 27 28}         sanPedro:NotDevelopedConstrained))

(defmodel open-development-scenario sanPedro:OpenDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen) 
      #{10 11 12 13 19 22 25}                      sanPedro:DevelopedOpen
      #{1 2 4 5 6 7 8 9 14 16 23 26 27 28 29}      sanPedro:NotDevelopedOpen))

(defmodel percent-vegetation-cover-new PercentVegetationCover
    (classification PercentVegetationCover
        :context (open-development-scenario percent-vegetation-cover)
        :state #(if (is? (:open-development %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:VeryLowVegetationCover)
                  (:percent-vegetation-cover %))))

(defmodel fire-frequency-new FireFrequency
    (classification FireFrequency
        :context (open-development-scenario fire-frequency)
        :state #(if (is? (:open-development %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:NoFireFrequency)    
                  (:fire-frequency %))))

;; Bayesian source model
(defmodel source-new CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationSanPedro.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :result   veg-soil-sequestration
            :context  (hardwood-softwood-ratio percent-vegetation-cover-new annual-precipitation)))

(defmodel sink-new CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :keep     (StoredCarbonRelease)
            :result   stored-carbon-release
            :context  (veg-soil-storage fire-frequency-new)))

(defscenario open-development-carbon
  "Changes values in developed areas to very low vegetation cover, no fire frequency, increased greenhouse gas emissions."
  (model PercentVegetationCover
    (classification PercentVegetationCover
        :context (open-development-scenario percent-vegetation-cover)
        :state #(if (is? (:open-development %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:VeryLowVegetationCover)
                  (:percent-vegetation-cover %))))
  (model FireFrequency
    (classification FireFrequency
        :context (open-development-scenario fire-frequency)
        :state #(if (is? (:open-development %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:NoFireFrequency)    
                  (:fire-frequency %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
        :context (open-development-scenario use-simple)
        :state #(if (is? (:open-development %) (conc 'sanPedro:DevelopedOpen))
                  (* 1.568 (:greenhouse-gas-emissions %)) ;;Reflects 56.8% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))

(defscenario constrained-development-carbon
  "Changes values in developed areas to very low vegetation cover, no fire frequency, increased greenhouse gas emissions."
  (model PercentVegetationCover
    (classification PercentVegetationCover
        :context (constrained-development-scenario percent-vegetation-cover)
        :state #(if (is? (:constrained-development %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'carbonService:VeryLowVegetationCover)
                  (:percent-vegetation-cover %))))
  (model FireFrequency
    (classification FireFrequency
        :context (constrained-development-scenario fire-frequency)
        :state #(if (is? (:constrained-development %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'carbonService:NoFireFrequency)
                  (:fire-frequency %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
        :context (constrained-development-scenario use-simple)
        :state #(if (is? (:constrained-development %) (conc 'sanPedro:DevelopedConstrained))
                  (* 1.104 (:greenhouse-gas-emissions %)) ;;Reflects 10.4% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))
