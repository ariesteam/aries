(ns core.models.carbon-mark
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; output and training
;; TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
  (classification 'carbonService:VegetationAndSoilCarbonStorage
                  :units      "t/ha" 
                  [500 3200]    'carbonService:VeryHighStorage
                  [300 500]     'carbonService:HighStorage
                  [150 300]     'carbonService:ModerateStorage
                  [75 150]      'carbonService:LowStorage
                  [0.01 75]     'carbonService:VeryLowStorage
                  [0 0.01]      'carbonService:NoStorage))

;;Note: 500 t/ha is the max sum of vegetation and soil carbon storage in Mark's study area.  
;; Could change the upper bound discretization on that one for veg C storage, stored C release, net C uptake...
;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
  (classification 'carbonService:VegetationCarbonStorage
                  :units      "t/ha" 
                  [325 2301]      'carbonService:VeryHighVegetationStorage
                  [190 325]       'carbonService:HighVegetationStorage
                  [105 190]       'carbonService:ModerateVegetationStorage
                  [40 105]        'carbonService:LowVegetationStorage
                  [0.01 40]       'carbonService:VeryLowVegetationStorage
                  [0 0.01]        'carbonService:NoVegetationStorage)) 				

;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
  (classification 'carbonService:SoilCarbonStorage
                  :units      "t/ha" 
                  [680 820]          'carbonService:VeryHighSoilStorage
                  [440 680]          'carbonService:HighSoilStorage
                  [200 440]          'carbonService:ModerateSoilStorage
                  [50 200]           'carbonService:LowSoilStorage
                  [:exclusive 0 50]  'carbonService:VeryLowSoilStorage
                  [0]                'carbonService:NoSoilStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel percent-vegetation-cover 'carbonService:PercentVegetationCover
  (classification (ranking 'habitat:PercentVegetationCover :units "%")
                  [80 100]           'carbonService:VeryHighVegetationCover
                  [60 80]            'carbonService:HighVegetationCover
                  [40 60]            'carbonService:ModerateVegetationCover
                  [20 40]            'carbonService:LowVegetationCover
                  [:exclusive 0 20]  'carbonService:VeryLowVegetationCover
                  [0]                'carbonService:NoVegetationCover))

;;Problems with coarse-grain pixels; removed this from the bayesian statement and set the prior
;; to its actual value from the data (LowActualEvapotranspiration) - a good temporary solution for
;; WCH but change if you ran it again for Southern California.
(defmodel actual-evapotranspiration 'carbonService:ActualEvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [92 :>]   'carbonService:VeryHighActualEvapotranspiration
                  [58 92]   'carbonService:HighActualEvapotranspiration
                  [32 58]   'carbonService:ModerateActualEvapotranspiration
                  [12 32]   'carbonService:LowActualEvapotranspiration
                  [:< 12]   'carbonService:VeryLowActualEvapotranspiration))

;;This does not account for barren, water, agriculture, or urban cover (though these are accounted for in NLCD)
(defmodel vegetation-type 'southernCalifornia:VegetationTypeSoCalCarbon
  (classification (numeric-coding 'southernCalifornia:VegTypeSoCal)
                  1          'southernCalifornia:HardwoodForestVegetationType
                  #{4 7}     'southernCalifornia:MixedConiferVegetationType
                  0          'southernCalifornia:ShrubVegetationType
                  3          'southernCalifornia:HerbaceousVegetationType))

;;"Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use 'southernCalifornia:LandCover
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  11                  'southernCalifornia:OpenWaterLandCover
                  #{90 95}            'southernCalifornia:WetlandLandCover
                  #{41 42 43 51 52}   'southernCalifornia:ScrubAndForestLandCover
                  #{71 81 82}         'southernCalifornia:GrasslandAndCultivatedLandCover
                  21                  'southernCalifornia:OpenSpaceLandCover
                  22                  'southernCalifornia:LowDevelopedLandCover
                  #{23 24}            'southernCalifornia:HighAndMedDevelopedLandCover))

;;Need a regular defmodel statement for the below data - for training purposes (not yet, but soon)
(defmodel veg-soil-sequestration 'carbonService:VegetationAndSoilCarbonSequestration
  (classification 'carbonService:VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]             'carbonService:VeryHighSequestration
                  [9 12]              'carbonService:HighSequestration
                  [6 9]               'carbonService:ModerateSequestration
                  [1.5 6]             'carbonService:LowSequestration
                  [:exclusive 0 1.5]  'carbonService:VeryLowSequestration
                  [0]                 'carbonService:NoSequestration))

;;See above statement for AET: Add back in if you use it for wider extents of Southern California
(defmodel source 'carbonService:CarbonSourceValue   
  (bayesian 'carbonService:CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationMark.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
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

(defmodel decomposition-factor 'habitat:DecompositionFactor
  (ranking 'habitat:DecompositionFactor
           :context ((measurement 'habitat:PotentialEvapotranspiration "mm" :as potential-evapotranspiration)
                     (measurement 'habitat:AnnualPrecipitation  "mm" :as mean-annual-precipitation))
           :state    #(/ (:mean-annual-precipitation %) (:potential-evapotranspiration %))))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph 'carbonService:Soilph
  (classification (ranking 'habitat:SoilPhDeep)
                  [7.3 :>]             'carbonService:HighPh
                  [5.5 7.3]            'carbonService:ModeratePh
                  [:exclusive 0 5.5]   'carbonService:LowPh))

;; use NLCD layers to infer anoxic vs. oxic
(defmodel soil-oxygen-conditions 'carbonService:SoilOxygenConditions 
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{90 95}   'carbonService:AnoxicSoils
                  :otherwise 'carbonService:OxicSoils))
        
(defmodel fire-threat 'carbonService:FireThreatClass
  (classification (ranking 'habitat:FireThreat) 
                  4  'carbonService:VeryHighFireThreat
                  3  'carbonService:HighFireThreat
                  2  'carbonService:ModerateFireThreat
                  1  'carbonService:LowFireThreat))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release 'carbonService:StoredCarbonRelease
  (classification 'carbonService:StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 300]           'carbonService:VeryHighRelease ;;Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
                  [9 12]             'carbonService:HighRelease
                  [6 9]              'carbonService:ModerateRelease
                  [3 6]              'carbonService:LowRelease
                  [:exclusive 0 3]   'carbonService:VeryLowRelease
                  [0]                'carbonService:NoRelease))

(defmodel sink 'carbonService:CarbonSinkValue   
  (bayesian 'carbonService:CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseMark.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (soil-ph percent-vegetation-cover soil-oxygen-conditions fire-threat vegetation-type land-use)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------
 	 					
(defmodel use-simple 'carbonService:GreenhouseGasEmissions
  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"))

;; ----------------------------------------------------------------------------------------------
;; Top-level service models
;; ----------------------------------------------------------------------------------------------

(defmodel identification-carbon 'carbonService:ClimateStability
  (identification 'carbonService:ClimateStability
                  :context (source :as source
                            sink :as sink
                            use-simple :as use)))

(defmodel carbon-flow 'carbonService:ClimateStability
  (span 'carbonService:CO2Removed
        'carbonService:VegetationAndSoilCarbonSequestration 
        'carbonService:GreenhouseGasEmissions
        'carbonService:StoredCarbonRelease 
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
        :keep ('carbonService:CarbonSequestration 'carbonService:StoredCarbonRelease 
               'carbonService:GreenhouseGasEmissions 'carbonService:PotentialCarbonMitigationProvision
               'carbonService:PotentialCarbonMitigationUse 'carbonService:UsedCarbonMitigation
               'carbonService:UsedCarbonSink 'carbonService:SatisfiedCarbonMitigationDemand
               'carbonService:CarbonMitigationSurplus 'carbonService:CarbonMitigationDeficit
               'carbonService:DepletedCarbonMitigation 'carbonService:DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))


;; ----------------------------------------------------------------------------------------------
;; Scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized 'carbonService:IPCCHadleyA2Incentivized
  "This scenario represents the effects of the Hadley A1 IPCC climate scenario. A12 
  represents a future world of very rapid economic growth, global population that peaks 
  in mid-century and declines thereafter, and rapid introduction of new and more efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking 'ecology:SuccessionalStage)
                  #{5 6 4}    'carbonService:OldGrowth
                  3           'carbonService:MidSuccession
                  2           'carbonService:EarlySuccession
                  1           'carbonService:PoleSuccession
                  :otherwise  'carbonService:NoSuccession)) 

(defscenario ipcc-hadley-b2-incentivized 'carbonService:IPCCHadleyB2Incentivized
  "This scenario represents the effects of the Hadley B1 IPCC climate scenario. The B1 
  world is a convergent world with the same global population as in the A1 storyline but 
  with rapid changes in economic structures toward a service and information economy, with 
  reductions in material intensity, and the introduction of clean and resource-efficient 
  technologies." 
  ;; old growth has been incentivized, so what was late succession is now old growth
  (classification (ranking 'ecology:SuccessionalStage)
                  #{5 6 4}    'carbonService:OldGrowth
                  3           'carbonService:MidSuccession
                  2           'carbonService:EarlySuccession
                  1           'carbonService:PoleSuccession
                  :otherwise  'carbonService:NoSuccession))  
       
(defscenario ipcc-hadley-a2 'carbonService:IPCCHadleyA2
  "This scenario represents the effects of the Hadley A2 IPCC climate
   scenario. A2 represents a very heterogeneous world with
   continuously increasing global population and regionally oriented
   economic growth that is more fragmented and slower than in other
   storylines."
  )

(defscenario ipcc-hadley-b2 'carbonService:IPCCHadleyB2
  "This scenario represents the effects of the Hadley B2 IPCC climate
   scenario. B2 is a world in which the emphasis is on local solutions
   to economic, social, and environmental sustainability, with
   continuously increasing population (lower than A2) and intermediate
   economic development."
  )
