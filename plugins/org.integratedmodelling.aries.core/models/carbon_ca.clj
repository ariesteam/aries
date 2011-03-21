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

;; output and training
(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
  (probabilistic-measurement VegetationAndSoilCarbonStorage "t/ha" 
                  [500 3200]    VeryHighStorage
                  [300 500]     HighStorage
                  [150 300]     ModerateStorage
                  [75 150]      LowStorage
                  [0.01 75]     VeryLowStorage
                  [0 0.01]      NoStorage))

;;Note: 500 t/ha is the max sum of vegetation and soil carbon storage in Mark's study area.  
;; Could change the upper bound discretization on that one for veg C storage, stored C release, net C uptake...
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
                  [680 820]          VeryHighSoilStorage
                  [440 680]          HighSoilStorage
                  [200 440]          ModerateSoilStorage
                  [50 200]           LowSoilStorage
                  [:exclusive 0 50]  VeryLowSoilStorage
                  [0]                NoSoilStorage))


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
                  1          southernCalifornia:HardwoodForestVegetationType
                  #{4 7}     southernCalifornia:MixedConiferVegetationType
                  0          southernCalifornia:ShrubVegetationType
                  3          southernCalifornia:HerbaceousVegetationType))

;;"Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use southernCalifornia:LandCover
  (classification (numeric-coding nlcd:NLCDNumeric)
                  11                  southernCalifornia:OpenWaterLandCover
                  #{90 95}            southernCalifornia:WetlandLandCover
                  #{41 42 43 51 52}   southernCalifornia:ScrubAndForestLandCover
                  #{71 81 82}         southernCalifornia:GrasslandAndCultivatedLandCover
                  21                  southernCalifornia:OpenSpaceLandCover
                  22                  southernCalifornia:LowDevelopedLandCover
                  #{23 24}            southernCalifornia:HighAndMedDevelopedLandCover))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
    (classification  (measurement geophysics:Altitude "m")
       [:exclusive 0 :>] OnLand))

;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
                  [12 30]             VeryHighSequestration
                  [9 12]              HighSequestration
                  [6 9]               ModerateSequestration
                  [1.5 6]             LowSequestration
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

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
                  [12 300]           VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]             HighRelease
                  [6 9]              ModerateRelease
                  [3 6]              LowRelease
                  [0.01 3]           VeryLowRelease
                  [0 0.01]           NoRelease))

;;Source and sink values are still calculated over the ocean, though they're set to almost-zero, so it's a temporary fix.
;; For some reason slope has value here in the ocean and using elevation as a mask made the whole model disappear.
(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseCa.xdsl"
            :keep     (StoredCarbonRelease)
            :required (LandOrSea)
            :result   stored-carbon-release
            :context  (soil-ph percent-vegetation-cover soil-oxygen-conditions 
                       fire-threat vegetation-type land-use land-selector)))

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
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_ca_data.clj")
        :keep (StoredCarbonRelease CarbonSequestration 
               GreenhouseGasEmissions PotentialCarbonMitigationProvision
               PotentialCarbonMitigationUse DetrimentalCarbonSource
               UsedCarbonSink SatisfiedCarbonMitigationDemand
               CarbonMitigationSurplus CarbonMitigationDeficit
               DepletedCarbonMitigation DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))
