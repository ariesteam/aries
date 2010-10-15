(ns core.models.carbon-jen
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; output and training TODO make it classify the appropriate measurement - buggy for now
;; Jen leaves out the catgory Very Low from her discretization
(defmodel veg-storage 'carbonService:VegetationCarbonStorage
	(classification 'carbonService:VegetationCarbonStorage
						:units 		"t/ha" 
	  				[80 2301]  'carbonService:VeryHighVegetationStorage
	  				[70 80]    'carbonService:HighVegetationStorage
	  				[50 70]    'carbonService:ModerateVegetationStorage
	  				[0 50]     'carbonService:LowVegetationStorage
	  				0          'carbonService:NoVegetationStorage)) 				

;; output and training TODO make it classify the appropriate measurement - buggy for now				
;; Jen leaves out the catgory Very Low from her discretization
(defmodel soil-storage 'carbonService:SoilCarbonStorage
		(classification 'carbonService:SoilCarbonStorage
						:units    "t/ha" 
	  				[210 820]  'carbonService:VeryHighSoilStorage
	  				[140 210]  'carbonService:HighSoilStorage
	  				[70 140]   'carbonService:ModerateSoilStorage
	  				[0 70]     'carbonService:LowSoilStorage
	  				0          'carbonService:NoSoilStorage))

(defmodel veg-soil-storage 'carbonService:VegetationAndSoilCarbonStorage
  (classification 'carbonService:VegetationAndSoilCarbonStorage
                  :units      "t/ha" 
                  [500 3200]    'carbonService:VeryHighStorage
                  [300 500]     'carbonService:HighStorage
                  [150 300]     'carbonService:ModerateStorage
                  [75 150]      'carbonService:LowStorage
                  [0.01 75]     'carbonService:VeryLowStorage
                  [0 0.01]      'carbonService:NoStorage))

(defmodel veg-soil-sequestration 'carbonService:VegetationAndSoilCarbonSequestration
  (classification 'carbonService:VegetationAndSoilCarbonSequestration
                  :units      "t/ha*year"
                  [12 30]     'carbonService:VeryHighSequestration
                  [9 12]      'carbonService:HighSequestration
                  [6 9]       'carbonService:ModerateSequestration
                  [3 6]       'carbonService:LowSequestration
                  [0.01 3]    'carbonService:VeryLowSequestration
                  [0 0.01]    'carbonService:NoSequestration))

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

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel summer-high-winter-low 'carbonService:SummerHighWinterLow
		 (classification (ranking 'habitat:SummerHighWinterLow)
        [40 :>]       'carbonService:VeryHighSOL
        [34 40]       'carbonService:HighSOL
        [29 34]       'carbonService:ModerateSOL
        [24 29]       'carbonService:LowSOL
        [:< 24]       'carbonService:VeryLowSOL))

(defmodel stand-condition 'carbonService:StandCondition
			(classification (ranking 'habitat:StandCondition) 
					#{4 5 6}			'carbonService:HighStandCondition
					#{7 8 9}			'carbonService:ModerateStandCondition
					#{1 2 3}			'carbonService:LowStandCondition
					:otherwise		'carbonService:NoStandCondition))

(defmodel stand-size-density 'carbonService:StandSizeDensity
			(classification (ranking 'habitat:StandSizeDensity) 
					#{5 6 8 9}		'carbonService:HighStandSizeDensity
					#{3 4 7}	  	'carbonService:ModerateStandSizeDensity
					#{1 2}				'carbonService:LowStandSizeDensity
					0							'carbonService:NoStandSizeDensity))
					
(defmodel soil-CN-ratio 'carbonService:SoilCNRatio
			(classification (ranking 'habitat:SoilCNRatio)
					[35 :>]					'carbonService:VeryHighCNRatio
					[20 35]					'carbonService:HighCNRatio
					[10 20]					'carbonService:LowCNRatio
					[:< 10]					'carbonService:VeryLowCNRatio)) 
			
;; Bayesian source model
(defmodel source 'carbonService:CarbonSourceValue   
  (bayesian 'carbonService:CarbonSourceValue 
            :import   "aries.core::CarbonSequestrationJen.xdsl"
            :keep     ('carbonService:VegetationAndSoilCarbonSequestration)
            :observed (veg-soil-sequestration)
            :context  (soil-CN-ratio stand-size-density stand-condition summer-high-winter-low)))
 	    
;; ----------------------------------------------------------------------------------------------
;; sink models
;; ----------------------------------------------------------------------------------------------

;;NB: ARIES defines the "source" of carbon sequestration as areas that are sequestering carbon (traditionally referred
;; to as "sinks" in the field of carbon research).  "Sinks" in ARIES refer to landscape features that deplete the
;; quantity of a carrier (in this case, sequestered CO2) from being available for human use.  These sinks include
;; areas at risk of deforestation or fire.

(defmodel fire-frequency 'carbonService:FireFrequency
     (classification (ranking 'habitat:FireFrequency) 
          [0.9 :>]    'carbonService:HighFireFrequency
          [0.25 0.9]  'carbonService:ModerateFireFrequency 
          [0.05 0.25] 'carbonService:LowFireFrequency
          [:< 0.05]   'carbonService:NoFireFrequency))

;;Use Bayesian priors for insect & blowdown frequencies

;;There may be some funkiness here from using veg-storage and soil-storage as leaf nodes when they've been used in the 
;; past as undiscretizers.  Check with Gary on this when it's time to run the model.
(defmodel sink 'carbonService:CarbonSinkValue   
  (bayesian 'carbonService:CarbonSinkValue 
            :import   "aries.core::StoredCarbonReleaseJen.xdsl"
            :keep     ('carbonService:StoredCarbonRelease)
            :observed (stored-carbon-release)
            :context  (fire-frequency veg-storage soil-storage)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel use-simple 'carbonService:GreenhouseGasEmissions
  (measurement 'carbonService:GreenhouseGasEmissions "t/ha*year"))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
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
		 			

		 			