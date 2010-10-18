(ns core.models.carbon-sam
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
	(classification 'carbonService:VegetationAndSoilCarbonStorage
						:units "t/ha" 
	  				[300 3200]  'carbonService:VeryHighStorage
	  				[220 300]   'carbonService:HighStorage
	  				[140 220]   'carbonService:ModerateStorage
	  				[70 140]    'carbonService:LowStorage
	  				[0 70]      'carbonService:VeryLowStorage
	  				0           'carbonService:NoStorage))

;; output and training TODO make it classify the appropriate measurement - buggy for now
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units "t/ha" 
	  				[100 2301] 'carbonService:VeryHighVegetationStorage
	  				[80 100]   'carbonService:HighVegetationStorage
	  				[60 80]    'carbonService:ModerateVegetationStorage
	  				[30 60]    'carbonService:LowVegetationStorage
	  				[0 30]     'carbonService:VeryLowVegetationStorage
	  				0          'carbonService:NoVegetationStorage)) 				

;; output and training TODO make it classify the appropriate measurement - buggy for now				
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha" 
	  				[200 820]  'carbonService:VeryHighSoilStorage
	  				[140 200]  'carbonService:HighSoilStorage
	  				[80 140]   'carbonService:ModerateSoilStorage
	  				[40 80]    'carbonService:LowSoilStorage
	  				[0 40]     'carbonService:VeryLowSoilStorage
	  				0          'carbonService:NoSoilStorage))	  			

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [44 :>]       'carbonService:VeryHighSOL
        [42 44]       'carbonService:HighSOL
        [40 42]       'carbonService:ModerateSOL
        [38 40]       'carbonService:LowSOL
        [:< 38]       'carbonService:VeryLowSOL))

(defmodel mean-annual-precip 'carbonService:MeanAnnualPrecipitation
			(classification (measurement 'habitat:AnnualPrecipitation "mm")
					[70 :>]				'carbonService:VeryHighMeanAnnualPrecipitation
					[60 70]				'carbonService:HighMeanAnnualPrecipitation
					[50 60]				'carbonService:ModerateMeanAnnualPrecipitation
					[40 50]				'carbonService:LowMeanAnnualPrecipitation
					[:< 40]				'carbonService:VeryLowMeanAnnualPrecipitation))
						
(defmodel soil-CN-ratio 'carbonService:SoilCNRatio
			(classification (ranking 'habitat:SoilCNRatio)
					[25 :>]				'carbonService:VeryHighCNRatio
					[17.5 25]			'carbonService:HighCNRatio
					[10 17.5]			'carbonService:LowCNRatio
					[:< 10]				'carbonService:VeryLowCNRatio))

(defmodel veg-type 'carbonService:VegetationType
			(classification (ranking 'carbonService:VegType)
					#{1 5 25}			'carbonService:RowCrops
					#{36 37 62}		'carbonService:GrasslandHerbaceous
					63					  'carbonService:Forest
					87						'carbonService:Wetland
					#{61 82}			'carbonService:NoVegetation))

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
            :import   "aries.core::CarbonSequestrationSam.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
            :context  (summer-high-winter-low mean-annual-precip soil-CN-ratio veg-type)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

;; No data here (at least apparently, see what Sam has to say) - use priors unless there's a layer.  Assume the below discretization
;; is in percentages?
;;(defmodel biomass-removal-rate 'carbonService:BiomassRemovalRate
;;      (classification (ranking 'habitat:BiomassRemovalRate)  
;;          [90 :>]       'carbonService:VeryHighRemovalRate
;;          [66 90]       'carbonService:HighRemovalRate
;;          [10 66]       'carbonService:LowRemovalRate
;;          [:< 10]       'carbonService:VeryLowRemovalRate)) 

;;No data here for "biomass residue input," "soil tillage" "biomass removal rate"- use priors

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
            :import   "aries.core::StoredCarbonReleaseSam.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (summer-high-winter-low mean-annual-precip soil-CN-ratio veg-type)))  ;; add biomass-removal-rate if there's supporting data
   	 		
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
		 			

		 			