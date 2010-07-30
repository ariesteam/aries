(ns core.models.carbon-mark
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; output and training
;; TODO make it classify the appropriate measurement - buggy for now
;; KB: Should this actually be sequestration?  And should
;; discretization on the two below defmodels be updated?
;; YES!  CREATE A NEW DEFMODEL FOR VEG & SOIL C SEQUESTRATION
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
  (classification 'carbonService:VegetationAndSoilCarbonStorage
                  :units      "t/ha" 
                  [12 :>]     'carbonService:VeryHighStorage
                  [9 12]      'carbonService:HighStorage
                  [6 9]       'carbonService:ModerateStorage
                  [3 6]       'carbonService:LowStorage
                  [0.01 3]    'carbonService:VeryLowStorage
                  [:< 0.01]   'carbonService:NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
  (classification 'carbonService:VegetationCarbonStorage
                  :units      "t/ha" 
                  [12 :>]     'carbonService:VeryHighVegetationStorage
                  [9 12]      'carbonService:HighVegetationStorage
                  [6 9]       'carbonService:ModerateVegetationStorage
                  [3 6]       'carbonService:LowVegetationStorage
                  [0.01 3]    'carbonService:VeryLowVegetationStorage
                  [:< 0.01]   'carbonService:NoVegetationStorage)) 				

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
  (classification 'carbonService:SoilCarbonStorage
                  :units      "t/ha" 
                  [12 :>]     'carbonService:VeryHighSoilStorage
                  [9 12]      'carbonService:HighSoilStorage
                  [6 9]       'carbonService:ModerateSoilStorage
                  [3 6]       'carbonService:LowSoilStorage
                  [0.01 3]    'carbonService:VeryLowSoilStorage
                  [:< 0.01]   'carbonService:NoSoilStorage))

(defmodel veg-soil-sequestration 'carbonService:VegetationAndSoilCarbonSequestration
  (classification 'carbonService:VegetationAndSoilCarbonSequestration
                  :units      "t/ha"
                  [12 :>]     'carbonService:VeryHighSequestration
                  [9 12]      'carbonService:HighSequestration
                  [6 9]       'carbonService:ModerateSequestration
                  [3 6]       'carbonService:LowSequestration
                  [0.01 3]    'carbonService:VeryLowSequestration
                  [:< 0.01]   'carbonService:NoSequestration))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release 'carbonService:StoredCarbonRelease
  (classification 'carbonService:StoredCarbonRelease
                  :units      "t/ha"
                  [12 :>]     'carbonService:VeryHighRelease
                  [9 12]      'carbonService:HighRelease
                  [6 9]       'carbonService:ModerateRelease
                  [3 6]       'carbonService:LowRelease
                  [0.01 3]    'carbonService:VeryLowRelease
                  [:< 0.01]   'carbonService:NoRelease))

(defmodel net-carbon-uptake 'carbonService:NetCarbonUptake
  (classification 'carbonService:NetCarbonUptake
                  :units      "t/ha"
                  [12 :>]     'carbonService:VeryHighCarbonUptake
                  [9 12]      'carbonService:HighCarbonUptake
                  [6 9]       'carbonService:ModerateCarbonUptake
                  [3 6]       'carbonService:LowCarbonUptake
                  [0.01 3]    'carbonService:VeryLowCarbonUptake
                  [:< 0.01]   'carbonService:NoCarbonUptake))
	  				
;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel percent-vegetation-cover 'carbonService:PercentVegetationCover
  (classification (ranking 'habitat:PercentVegetationCover :units "%")
                  [80 :>] 'carbonService:VeryHighVegetationCover
                  [60 80] 'carbonService:HighVegetationCover
                  [40 60] 'carbonService:ModerateVegetationCover
                  [20 40] 'carbonService:LowVegetationCover
                  [0 20]  'carbonService:VeryLowVegetationCover))

(defmodel soil-ph 'carbonService:Soilph
  (classification (ranking 'habitat:SoilPh)
                  [7.3 :>]       'carbonService:HighPh
                  [5.5 7.3]      'carbonService:ModeratePh
                  [:< 5.5]       'carbonService:LowPh))

; use NLCD layers to infer anoxic vs. oxic
(defmodel oxygen 'carbonService:SoilOxygenConditions 
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{90 95}   'carbonService:AnoxicSoils
                  :otherwise 'carbonService:OxicSoils))
				
(defmodel fire-threat 'carbonService:FireThreatClass
  (classification (ranking 'habitat:FireThreat)	
                  4  'carbonService:VeryHighFireThreat
                  3  'carbonService:HighFireThreat
                  2  'carbonService:ModerateFireThreat
                  1  'carbonService:LowFireThreat))

(defmodel actual-evapotranspiration 'carbonService:ActualEvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [92 :>]   'carbonService:VeryHighActualEvapotranspiration
                  [58 92]   'carbonService:HighActualEvapotranspiration
                  [32 58]   'carbonService:ModerateActualEvapotranspiration
                  [12 32]   'carbonService:LowActualEvapotranspiration
                  [:< 12]   'carbonService:VeryLowActualEvapotranspiration))

;;This does not account for barren, water, agriculture, or urban cover (though these are accounted for in NLCD)
(defmodel vegetation-type 'southernCalifornia:VegetationTypeSoCal
  (classification (categorization 'southernCalifornia:VegetationTypeSoCal)
                  "HDW"          'southernCalifornia:HardwoodForestVegetationType
                  #{"CON" "MIX"} 'southernCalifornia:MixedConiferVegetationType
                  "SHB"          'southernCalifornia:ShrubVegetationType
                  "HEB"          'southernCalifornia:HerbaceousVegetationType))

;;"Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use 'southernCalifornia:LandCover
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{11 90 95}         'southernCalifornia:WetlandOpenWaterLandCover
                  #{41 42 43 51 52}   'southernCalifornia:ScrubAndForestLandCover
                  #{71 81 82}         'southernCalifornia:GrasslandAndCultivatedLandCover
                  21                  'southernCalifornia:OpenSpaceLandCover
                  22                  'southernCalifornia:LowDevelopedLandCover
                  #{23 24}            'southernCalifornia:HighAndMedDevelopedLandCover))

;; Bayesian source model
;; keep = observations computed by the Bayesian network that we keep.  context = leaf nodes as derived from models
(defmodel source 'carbonService:CarbonSourceValue   
  (bayesian 'carbonService:CarbonSourceValue 
            :import   "aries.core::CarbonSourceValueMark.xdsl"
            :keep     ('carbonService:NetCarbonUptake
                       'carbonService:VegetationAndSoilCarbonSequestration
                       'carbonService:VegetationAndSoilCarbonStorage
                       'carbonService:VegetationCarbonStorage
                       'carbonService:StoredCarbonRelease
                       'carbonService:SoilCarbonStorage)
            :observed (veg-soil-storage soil-storage veg-storage veg-soil-sequestration stored-carbon-release net-carbon-uptake)
	 	 	:context  (soil-ph percent-vegetation-cover oxygen fire-threat actual-evapotranspiration vegetation-type land-use)))
;; don't forget about the land-use model

;; ----------------------------------------------------------------------------------------------
;; carbon model accuracy check
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
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel greenhouse-gas-emitter 'carbonService:GreenhouseGasEmitters
  (classification (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year")
                  [250000 :>]     'carbonService:VeryHighEmitter
                  [100000 250000] 'carbonService:HighEmitter
                  [25000 100000]  'carbonService:ModerateEmitter
                  [1000 25000]    'carbonService:LowEmitter
                  [100 1000]      'carbonService:VeryLowEmitter
                  [:< 100]        'carbonService:NoEmitter))
		 	 
(defmodel use-emitters 'carbonService:CarbonUse
  (bayesian 'carbonService:CarbonUse
	  		(classification 'carbonService:CarbonEmitterUse
                            0          'carbonService:EmitterUseAbsent
                            :otherwise 'carbonService:EmitterUsePresent) 
            :import  "aries.core::CarbonUse.xdsl"
            :keep    ('carbonService:CarbonEmitterUse)
            :context (greenhouse-gas-emitter)))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------

;; flow model for emitters
(defmodel emitter-flow 'carbonService:ClimateStability
  (span 'carbonService:CO2Removed 
  	    'carbonService:VegetationAndSoilCarbonStorage
  	    'carbonService:CarbonEmitterUse
      	nil
      	nil
  	    nil
        :source-threshold 1,
        :sink-threshold   0.5,
        :use-threshold    0.5,
        :trans-threshold  1.0,
        :sink-type        :relative,
        :use-type         :relative,
        :benefit-type     :rival,
        :rv-max-states    10 
        :context (source use-emitters)))		
		
;; ----------------------------------------------------------------------------------------------
;; scenarios (evolving)
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
