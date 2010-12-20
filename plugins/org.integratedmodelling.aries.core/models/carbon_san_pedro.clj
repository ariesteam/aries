(ns core.models.carbon-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement classification categorization ranking numeric-coding
                           binary-coding identification bayesian count])
  (:refer aries :only [span]))

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

;; output and training TODO make it classify the appropriate measurement - buggy for now        
(defmodel soil-storage 'carbonService:SoilCarbonStorage
  (classification 'carbonService:SoilCarbonStorage
                  :units      "t/ha" 
                  [680 820]      'carbonService:VeryHighSoilStorage
                  [440 680]      'carbonService:HighSoilStorage
                  [200 440]      'carbonService:ModerateSoilStorage
                  [50 200]       'carbonService:LowSoilStorage
                  [0.01 50]      'carbonService:VeryLowSoilStorage
                  [0 0.01]       'carbonService:NoSoilStorage))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel percent-vegetation-cover 'carbonService:PercentVegetationCover
  (classification (ranking 'habitat:PercentVegetationCover :units "%")
                  [80 :>] 'carbonService:VeryHighVegetationCover
                  [60 80] 'carbonService:HighVegetationCover
                  [40 60] 'carbonService:ModerateVegetationCover
                  [20 40] 'carbonService:LowVegetationCover
                  [0 20]  'carbonService:VeryLowVegetationCover))

;;ARE WE OK HAVING SOME CLASSES IN THE DATA WITH NO CORRESPONDING DISCRETE STATES (i.e., ag, developed, barren)?
;; Not considered but in the area: 5 9 15 16 17 18 19 20 21 65 93 110 111 112 114 117 (could set these to hard, indicating slower sequestration)
;;Ditto for Mexico: most LULC categories are not used, and not all discrete states are represented.
(defmodel hardwood-softwood-ratio 'carbonService:HardwoodSoftwoodRatio
     (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
        #{14 34 35 83 92}                   'carbonService:LowHardness
        #{33 45 51 91}                      'carbonService:ModerateHardness
        #{52 55 56 57 59 60 84 96 105 118}  'carbonService:HighHardness))
;;     (classification (categorization 'mexico:CONABIOLULCCategory)
;;        #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"} 'carbonService:LowHardness
;;        #{"Chaparral"}                                              'carbonService:ModerateHardness
;;        #{"Bosque de encino" "Mezquital-huizachal"}                 'carbonService:HighHardness))

;;Have removed this from the model and replaced it with annual precip.  This is probably a better variable to include
;; in carbon models in wetter regions, while annual precip is far more important in water-limited regions.
(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
     (classification (ranking 'habitat:SummerHighWinterLow)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))

;;Brown et al. (2010) use 0-130, 130-230, 230-460, >460 mm as their discretization for rangeland carbon modeling.
;; For the San Pedro, the entire valley floor would be in the 230-460 range and the surrounding mountains as >460.
;; For now, keep the below discretization, though strongly consider using it.
(defmodel annual-precipitation 'carbonService:MeanAnnualPrecipitation
     (classification (measurement 'habitat:AnnualPrecipitation "mm")
        [500 :>]        'carbonService:HighMeanAnnualPrecipitation
        [400 500]       'carbonService:ModerateMeanAnnualPrecipitation
        [:< 400]        'carbonService:LowMeanAnnualPrecipitation))

(defmodel veg-soil-sequestration 'carbonService:VegetationAndSoilCarbonSequestration
  (classification 'carbonService:VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]     'carbonService:VeryHighSequestration
                  [9 12]      'carbonService:HighSequestration
                  [6 9]       'carbonService:ModerateSequestration
                  [3 6]       'carbonService:LowSequestration
                  [0.01 3]    'carbonService:VeryLowSequestration
                  [0 0.01]    'carbonService:NoSequestration))

;; Bayesian source model
(defmodel source 'carbonService:CarbonSourceValue   
  (bayesian 'carbonService:CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationSanPedro.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
	 	 	      :context  (hardwood-softwood-ratio percent-vegetation-cover annual-precipitation)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
;;This should work OK with both global & SSURGO data, but check to make sure.
(defmodel soil-ph 'carbonService:Soilph
  (classification (ranking 'habitat:SoilPhDeep)
                  [7.3 :>]       'carbonService:HighPh
                  [5.5 7.3]      'carbonService:ModeratePh
                  [:< 5.5]       'carbonService:LowPh))

(defmodel slope 'carbonService:Slope
    (classification (measurement 'geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    'carbonService:Level
       [1.15 4.57]  'carbonService:GentlyUndulating
       [4.57 16.70] 'carbonService:RollingToHilly
       [16.70 :>]   'carbonService:SteeplyDissectedToMountainous))

;;Use NLCD or GLC layers to infer anoxic vs. oxic: no Mexican LULC data (i.e., CONABIO) 
;; denote wetlands at least for Sonora.
(defmodel oxygen 'carbonService:SoilOxygenConditions 
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{90 95}   'carbonService:AnoxicSoils
                  :otherwise 'carbonService:OxicSoils)
  (classification (numeric-coding 'glc:GLCNumeric)
                  15         'carbonService:AnoxicSoils
                  :otherwise 'carbonService:OxicSoils))

(defmodel fire-frequency 'carbonService:FireFrequency
  (classification (numeric-coding 'habitat:FireReturnInterval) 
                  1        'carbonService:HighFireFrequency
                  #{2 3}   'carbonService:ModerateFireFrequency ;;includes "variable" fire frequency
                  4        'carbonService:LowFireFrequency
                  #{5 6}   'carbonService:NoFireFrequency))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release 'carbonService:StoredCarbonRelease
  (classification 'carbonService:StoredCarbonRelease
                  :units      "t/ha*year"
                  [12 3200]   'carbonService:VeryHighRelease ;;may need to lower this number so the calculations work out.
                  [9 12]      'carbonService:HighRelease
                  [6 9]       'carbonService:ModerateRelease
                  [3 6]       'carbonService:LowRelease
                  [0.01 3]    'carbonService:VeryLowRelease
                  [0 0.01]    'carbonService:NoRelease))

;;Consider reworking the soil carbon storage part of the model based on Martens et al. 2005 - soil texture, precip, 
;; temperature as most important correlates of high soil carbon storage.

(defmodel sink 'carbonService:CarbonSinkValue   
  (bayesian 'carbonService:CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseSanPedro.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover hardwood-softwood-ratio 
                        annual-precipitation fire-frequency)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

;:GHG emissions map for the U.S.  For the rest of the world, use global population density layer multiplied
;; by per capita emissions for that country from EIA.  2006 data used as this corresponds to current population
;; density layer: 4.05 tonnes CO2/capita for Mexico in 2006, which is equivalent to 1.105 tonnes C/capita

(defmodel use-simple 'carbonService:GreenhouseGasEmissions
  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"))

;;(defmodel use-simple 'carbonService:GreenhouseGasEmitters
;;  [(categorization 'geofeatures:Country :as country)]
;;  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"
;;               :when #(= (:country %) "United States"))
;;  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"
;;               :context ((count 'policytarget:PopulationDensity "/km^2" :as population-density-count))
;;               :state   #(* (:population-density-count %) 1.105)))

;; ----------------------------------------------------------------------------------------------
;; Top-level service models
;; ----------------------------------------------------------------------------------------------

(defmodel identification-carbon 'carbonService:ClimateStability
  (identification 'carbonService:ClimateStability
                  :context (source :as source
                            sink :as sink
                            use-simple :as use)))

;; flow model for emitters (why doesn't 'carbonService:ClimateStability = 'carbonService:CO2Removed ?)
(defmodel carbon-flow 'carbonService:ClimateStability
  (span 'carbonService:CO2Removed
        'carbonService:CarbonSourceValue 
        'carbonService:GreenhouseGasEmissions
        'carbonService:CarbonSinkValue 
        nil
        nil
        :source-threshold   10.0
        :sink-threshold     10.0
        :use-threshold       1.0
        :trans-threshold    nil
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        :save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :keep ('carbonService:CarbonSequestration 'carbonService:StoredCarbonRelease 
               'carbonService:GreenhouseGasEmissions 'carbonService:PotentialCarbonMitigationProvision
               'carbonService:PotentialCarbonMitigationUse 'carbonService:UsedCarbonMitigation
               'carbonService:UsedCarbonSink 'carbonService:SatisfiedCarbonMitigationDemand
               'carbonService:CarbonMitigationSurplus 'carbonService:CarbonMitigationDeficit
               'carbonService:DepletedCarbonMitigation 'carbonService:DepletedCarbonMitigationDemand)
        :context (source use-simple sink)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario mesquite-management 'sanPedro:MesquiteManagement: change to VeryLowVegetationCover in source & sink models
;;  within mesquite management polygons)
      
;;(defscenario urban-growth 'sanPedro:UrbanGrowth: change developed areas to VeryLowVegetationCover in source & sink
      ;; models; change fire-frequency to NoFireFrequency; bump up use by 10.4% in constrained and 56.8% in open development scenarios)
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development 'sanPedro:BSRDevelopment: change developed areas to VeryLowVegetationCover in source & sink
      ;; models; change fire-frequency to NoFireFrequency; bump up use by 3.6%)
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5

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
