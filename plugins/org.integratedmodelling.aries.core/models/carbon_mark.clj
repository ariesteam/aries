(ns core.models.carbon-ca
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification 
                            categorization ranking numeric-coding binary-coding 
                            identification bayesian namespace-ontology count))
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

;; output and training
;; TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
  (classification VegetationAndSoilCarbonStorage
                  :units      "t/ha" 
                  [500 3200]    VeryHighStorage
                  [300 500]     HighStorage
                  [150 300]     ModerateStorage
                  [75 150]      LowStorage
                  [0.01 75]     VeryLowStorage
                  [0 0.01]      NoStorage))

;;Note: 500 t/ha is the max sum of vegetation and soil carbon storage in Mark's study area.  
;; Could change the upper bound discretization on that one for veg C storage, stored C release, net C uptake...
;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage VegetationCarbonStorage
  (classification VegetationCarbonStorage
                  :units      "t/ha" 
                  [325 2301]      VeryHighVegetationStorage
                  [190 325]       HighVegetationStorage
                  [105 190]       ModerateVegetationStorage
                  [40 105]        LowVegetationStorage
                  [0.01 40]       VeryLowVegetationStorage
                  [0 0.01]        NoVegetationStorage)) 				

;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage SoilCarbonStorage
  (classification SoilCarbonStorage
                  :units      "t/ha" 
                  [680 820]          VeryHighSoilStorage
                  [440 680]          HighSoilStorage
                  [200 440]          ModerateSoilStorage
                  [50 200]           LowSoilStorage
                  [:exclusive 0 50]  VeryLowSoilStorage
                  [0]                NoSoilStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel percent-vegetation-cover PercentVegetationCover
  (classification (ranking habitat:PercentVegetationCover :units "%")
                  [80 100]           VeryHighVegetationCover
                  [60 80]            HighVegetationCover
                  [40 60]            ModerateVegetationCover
                  [20 40]            LowVegetationCover
                  [:exclusive 0 20]  VeryLowVegetationCover
                  [0]                NoVegetationCover))

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

;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (classification VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]             VeryHighSequestration
                  [9 12]              HighSequestration
                  [6 9]               ModerateSequestration
                  [1.5 6]             LowSequestration
                  [:exclusive 0 1.5]  VeryLowSequestration
                  [0]                 NoSequestration))

;;See above statement for AET: Add back in if you use it for wider extents of Southern California
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationMark.xdsl"
            :keep     (VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
	 	 	      :context  (percent-vegetation-cover vegetation-type land-use)))

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
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

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

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (classification StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 300]           VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]             HighRelease
                  [6 9]              ModerateRelease
                  [3 6]              LowRelease
                  [:exclusive 0 3]   VeryLowRelease
                  0                  NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseMark.xdsl"
            :keep     (StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (soil-ph percent-vegetation-cover soil-oxygen-conditions fire-threat vegetation-type land-use)))

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
  "This scenario represents the effects of the Hadley A2 IPCC climate
   scenario. A2 represents a very heterogeneous world with
   continuously increasing global population and regionally oriented
   economic growth that is more fragmented and slower than in other
   storylines."
  )

(defscenario ipcc-hadley-b2 IPCCHadleyB2
  "This scenario represents the effects of the Hadley B2 IPCC climate
   scenario. B2 is a world in which the emphasis is on local solutions
   to economic, social, and environmental sustainability, with
   continuously increasing population (lower than A2) and intermediate
   economic development."
  )
