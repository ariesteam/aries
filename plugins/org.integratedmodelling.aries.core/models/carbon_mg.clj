(ns core.models.carbon-mg
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; these are the undiscretization statements, necessary for training purposes.
;; output and training TODO make it classify the appropriate measurement - buggy for now
;;KB: Should this actually be sequestration?  And should discretization on the two 
;; below defmodels be updated?
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
	(classification 'carbonService:VegetationAndSoilCarbonStorage
						:units "t/ha" 
	  				[1000 3200]   'carbonService:VeryHighStorage
            [600 1000]    'carbonService:HighStorage
            [300 600]     'carbonService:ModerateStorage
            [100 300]     'carbonService:LowStorage
            [0.01 100]    'carbonService:VeryLowStorage
            [0 0.01]      'carbonService:NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
;;These values may be a bit high - compare to the mg data for veg C storage.
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units "t/ha" 
	  				[900 2301]     'carbonService:VeryHighVegetationStorage
            [500 900]      'carbonService:HighVegetationStorage
            [250 500]      'carbonService:ModerateVegetationStorage
            [75 250]       'carbonService:LowVegetationStorage
            [0.01 75]      'carbonService:VeryLowVegetationStorage
            [0 0.01]       'carbonService:NoVegetationStorage)) 			

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha" 
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

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [:< 24]       'carbonService:VeryLowSOL
        [24 30]       'carbonService:LowSOL
        [30 35]       'carbonService:ModerateSOL
        [35 40]       'carbonService:HighSOL
        [40 :>]       'carbonService:VeryHighSOL))

(defmodel soil-cn-ratio 'carbonService:SoilCNRatio
  (classification (ranking 'habitat:SoilCNRatio)
       [35 :>]   'carbonService:VeryHighCNRatio
       [20 35]   'carbonService:HighCNRatio
       [10 20]   'carbonService:LowCNRatio
       [:< 10]   'carbonService:VeryLowCNRatio)) 

(defmodel degradation-status 'carbonService:DegradationStatus
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
         #{3 7 23}         'carbonService:Degraded
         :otherwise       'carbonService:NotDegraded)) 

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
            :import   "aries.core::CarbonSequestrationMg.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
            :context  (percent-vegetation-cover summer-high-winter-low soil-cn-ratio degradation-status)))

;; ----------------------------------------------------------------------------------------------
;; Sink models
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel slope 'carbonService:Slope
    (classification (measurement 'geophysics:DegreeSlope "\u00b0")
       [:< 1.15]    'carbonService:Level
       [1.15 4.57]  'carbonService:GentlyUndulating
       [4.57 16.70] 'carbonService:RollingToHilly
       [16.70 :>]   'carbonService:SteeplyDissectedToMountainous))

;; Values of "1" correspond to high pH (>7.3), "2" to low pH (<5.5), "3-4" to intermediate pH (5.5-7.3)
;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph 'carbonService:Soilph
     (classification (ranking 'habitat:SoilPhShallow)
        #{3 4}  'carbonService:HighPh
        2       'carbonService:ModeratePh
        1       'carbonService:LowPh))

;; Mg wetlands layer is just a wetlands layer - no reclass of a LULC layer.
(defmodel oxygen 'carbonService:SoilOxygenConditions
 (classification (binary-coding 'habitat:Wetland)
      1          'carbonService:AnoxicSoils
      :otherwise 'carbonService:OxicSoils))

;;No data on fire frequency for Madagascar - use Bayesian priors until we can get a layer.

(defmodel population-density 'carbonService:PopulationDensity
  (classification (count 'policytarget:PopulationDensity "/km^2")
    [12 :>]    'carbonService:HighPopulationDensity
    [7 12]     'carbonService:ModeratePopulationDensity
    [:< 7]     'carbonService:LowPopulationDensity))

(defmodel deforestation-risk 'carbonService:DeforestationRiskClass
  (classification (ranking 'carbonService:DeforestationRisk)
       "High"                'carbonService:HighDeforestationRisk
       "Moderate"            'carbonService:ModerateDeforestationRisk
       "Low"                 'carbonService:LowDeforestationRisk
       :otherwise            'carbonService:NoDeforestationRisk)) 

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

(defmodel sink 'carbonService:CarbonSinkValue   
  (bayesian 'carbonService:CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseMg.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (soil-ph slope oxygen percent-vegetation-cover summer-high-winter-low soil-cn-ratio
                         degradation-status population-density deforestation-risk)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

;:GHG emissions map for Madagascar: use global population density layer multiplied by per capita emissions
;; for that country from EIA.  2006 data used as this corresponds to current population density layer: 
;; 0.14 tonnes CO2/capita for Madagascar in 2006, which is equivalent to 0.04 tonnes C/capita
(defmodel use-simple 'carbonService:GreenhouseGasEmitters
  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"
               :context ((count 'policytarget:PopulationDensity "/km^2" :as population-density-count))
               :state   #(* (:population-density-count %) 0.04)))
 	 					
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
        :downscaling-factor 8
        :keep ('carbonService:CarbonSequestration 'carbonService:StoredCarbonRelease 
               'carbonService:GreenhouseGasEmissions 'carbonService:PotentialCarbonMitigationProvision
               'carbonService:PotentialCarbonMitigationUse 'carbonService:UsedCarbonMitigation
               'carbonService:UsedCarbonSink 'carbonService:SatisfiedCarbonMitigationDemand
               'carbonService:CarbonMitigationSurplus 'carbonService:CarbonMitigationDeficit
               'carbonService:DepletedCarbonMitigation 'carbonService:DepletedCarbonMitigationDemand)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
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
	  "This scenario represents the effects of the Hadley A2 IPCC climate scenario. A2
     represents a very heterogeneous world with continuously increasing global population 
     and regionally oriented economic growth that is more fragmented and slower than in
     other storylines." 
) 

(defscenario ipcc-hadley-b2 'carbonService:IPCCHadleyB2
	  "This scenario represents the effects of the Hadley B2 IPCC climate scenario. B2 
     is a world in which the emphasis is on local solutions to economic, social, and 
     environmental sustainability, with continuously increasing population (lower than A2)
     and intermediate economic development. " 
) 
		 			

		 			