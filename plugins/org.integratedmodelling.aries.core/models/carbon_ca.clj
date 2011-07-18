(ns core.models.carbon-ca  ;;Model is for Southern California (Mark Casias' model for Orange County)
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService
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

(defmodel percent-vegetation-cover PercentVegetationCover
  (classification (ranking habitat:PercentVegetationCover :units "%")
                  [80 100 :inclusive]    VeryHighVegetationCover
                  [60 80]                HighVegetationCover
                  [40 60]                ModerateVegetationCover
                  [20 40]                LowVegetationCover
                  [:exclusive 0 20]      VeryLowVegetationCover
                  [0]                    NoVegetationCover))

;;This does not account for barren, water, agriculture, or urban cover (though these are accounted for in NLCD)
(defmodel vegetation-type southernCalifornia:VegetationTypeSoCalCarbon
  (classification (numeric-coding southernCalifornia:VegTypeSoCal)
                  1          southernCalifornia:HardwoodForestVegetation
                  #{4 7}     southernCalifornia:MixedConiferVegetation
                  0          southernCalifornia:ShrubVegetation
                  3          southernCalifornia:HerbaceousVegetation))

;;"Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use southernCalifornia:LandCover
  (classification (numeric-coding nlcd:NLCDNumeric)
                  11                  southernCalifornia:OpenWater
                  #{90 95}            southernCalifornia:Wetland
                  #{41 42 43 51 52}   southernCalifornia:ScrubAndForest
                  #{71 81 82}         southernCalifornia:GrasslandAndCultivated
                  21                  southernCalifornia:OpenSpace
                  22                  southernCalifornia:LowDensityDeveloped
                  #{23 24}            southernCalifornia:HighAndMedDensityDeveloped))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
    (classification  (measurement geophysics:Altitude "m")
       [:exclusive 0 :>] OnLand))

;;Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [6 13]              VeryHighSequestration
                  [4 6]               HighSequestration
                  [3 4]               ModerateSequestration
                  [1.5 3]             LowSequestration
                  [:exclusive 0 1.5]  VeryLowSequestration
                  [0]                 NoSequestration))

;;See above statement for AET: Add back in if you use it for wider extents of Southern California
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationCa.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :required (LandOrSea)
            :result   veg-soil-sequestration
            :context  (percent-vegetation-cover vegetation-type land-use land-selector)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines sources of carbon areas that are sequester carbon in vegetation and soils.  
;;.  Sinks are emissions from areas at risk of deforestation or fire, which can release carbon
;;   into the atmosphere.  The difference between carbon sinks and sources is the amount remaining 
;;   to mitigate direct anthropogenic emissions (aside from land conversion and fire).

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhDeep)
                  [7.3 :>]             HighPh
                  [5.5 7.3]            ModeratePh
                  [:exclusive 0 5.5]   LowPh))

;; use NLCD layers to infer anoxic vs. oxic
(defmodel soil-oxygen-conditions SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
                  #{90 95}   AnoxicSoils
                  :otherwise OxicSoils))
        
(defmodel fire-threat FireThreatClass
  (classification (ranking habitat:FireThreat) 
                  4  VeryHighFireThreat
                  3  HighFireThreat
                  2  ModerateFireThreat
                  1  LowFireThreat))

;;Problems with coarse-grain pixels; removed this from the bayesian statement and set the prior
;; to its actual value from the data (LowActualEvapotranspiration) - a good temporary solution for
;; WCH but change if you ran it again for Southern California.
(defmodel actual-evapotranspiration ActualEvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
                  [92 :>]   VeryHighActualEvapotranspiration
                  [58 92]   HighActualEvapotranspiration
                  [32 58]   ModerateActualEvapotranspiration
                  [12 32]   LowActualEvapotranspiration
                  [:< 12]   VeryLowActualEvapotranspiration))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
                  [150 315]       VeryHighVegetationStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [80 150]        HighVegetationStorage
                  [30 80]         ModerateVegetationStorage
                  [10 30]         LowVegetationStorage
                  [0.01 10]       VeryLowVegetationStorage
                  [0 0.01]        NoVegetationStorage))   

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
            :import   "aries.core::StoredCarbonReleaseCa.xdsl"
            :context  (vegetation-type land-use percent-vegetation-cover land-selector)
            :result    veg-storage
            :keep     (VegetationCarbonStorage)))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
                  [25 50]         VeryHighSoilStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [15 25]         HighSoilStorage
                  [5 15]          ModerateSoilStorage
                  [2 5]           LowSoilStorage
                  [0.01 2]        VeryLowSoilStorage
                  [0 0.01]        NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
            :import   "aries.core::StoredCarbonReleaseCa.xdsl"
            :context  (soil-ph percent-vegetation-cover soil-oxygen-conditions land-selector)
            :result    soil-storage
            :keep     (SoilCarbonStorage)))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
               :context (vegetation-carbon-storage :as vegetation-c-storage soil-carbon-storage :as soil-c-storage) 
               :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                          (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
                  [160 365]     VeryHighStorage ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [100 160]     HighStorage
                  [40 100]      ModerateStorage
                  [15 40]       LowStorage
                  [0.01 15]     VeryLowStorage
                  [0 0.01]      NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [100 180]            VeryHighRelease ;;Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
                  [50 100]             HighRelease
                  [25 50]              ModerateRelease
                  [10 25]              LowRelease
                  [0.01 10]            VeryLowRelease
                  [0 0.01]             NoRelease))

;;Source and sink values are still calculated over the ocean, though they're set to almost-zero, so it's a temporary fix.
;; For some reason slope has value here in the ocean and using elevation as a mask made the whole model disappear.
(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseCa.xdsl"
            :keep     (StoredCarbonRelease)
            :required (LandOrSea)
            :result   stored-carbon-release
            :context  (veg-soil-storage fire-threat land-selector)))

;; ----------------------------------------------------------------------------------------------
;; Carbon model accuracy check
;; ----------------------------------------------------------------------------------------------

;;Decomposition Factor (DF) has been noted to be a good predictor of NPP for chaparral ecosystems 
;;(Li, et al. 2006), which is highly correlated with carbon sequestration rates (sources). 
;;Since water is a primary limiting factor for the study site's ecoregion this sub-modeled deterministic 
;;node serves to assess model accuracy (Expressed as:  DF = P/PET)
;;Values from this check should be compared to soil and vegetation carbon sequestration, or possibly 
;; used as a higher resolution proxy.

(defmodel decomposition-factor habitat:DecompositionFactor
  (ranking habitat:DecompositionFactor
           :context ((measurement habitat:PotentialEvapotranspiration "mm" :as potential-evapotranspiration)
                     (measurement habitat:AnnualPrecipitation  "mm" :as mean-annual-precipitation))
           :state    #(/ (:mean-annual-precipitation %) (:potential-evapotranspiration %))))

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
