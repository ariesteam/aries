(ns core.models.sediment
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; This is the model for Puget Sound.

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'soilretentionEcology:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking 'habitat:HydrologicSoilsGroup)
			1       'soilretentionEcology:SoilGroupA
			2       'soilretentionEcology:SoilGroupB
			3       'soilretentionEcology:SoilGroupC
			4       'soilretentionEcology:SoilGroupD))

(defmodel slope 'soilretentionEcology:SlopeClass
		(classification (measurement 'geophysics:DegreeSlope "\u00b0")
			 [0 1.15] 	  'soilretentionEcology:Level
			 [1.15 4.57] 	'soilretentionEcology:GentlyUndulating
			 [4.57 16.70] 'soilretentionEcology:RollingToHilly
			 [16.70 :>] 	'soilretentionEcology:SteeplyDissectedToMountainous))

(defmodel slope-stability 'soilretentionEcology:SlopeStabilityClass
    (classification (numeric-coding 'habitat:SlopeStability)	 		
      1           'soilretentionEcology:HighSlopeStability
	 		2           'soilretentionEcology:ModerateSlopeStability
	 		3           'soilretentionEcology:LowSlopeStability))

(defmodel soil-texture 'soilretentionEcology:SoilTextureClass
    (classification (categorization 'habitat:SoilTexture)
      "Coarse"    'soilretentionEcology:CoarseSoilTexture
      "Medium"    'soilretentionEcology:MediumSoilTexture
      "Fine"      'soilretentionEcology:FineSoilTexture))

;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility 'soilretentionEcology:SoilErodibilityClass
     (classification (numeric-coding 'habitat:SoilErodibility)
       [:< 0.1]    'soilretentionEcology:VeryLowSoilErodibility
       [0.1 0.225]   'soilretentionEcology:LowSoilErodibility
       [0.225 0.3]   'soilretentionEcology:ModerateSoilErodibility
       [0.3 0.375]   'soilretentionEcology:HighSoilErodibility
       [0.375 :>]     'soilretentionEcology:VeryHighSoilErodibility))

(defmodel precipitation-annual 'soilretentionEcology:AnnualPrecipitationClass
	"FIXME this is annual precipitation."
	(classification (measurement 'habitat:AnnualPrecipitation "mm")
    [:< 600] 	    'soilretentionEcology:VeryLowAnnualPrecipitation
		[600 1200] 	  'soilretentionEcology:LowAnnualPrecipitation
		[1200 1800]   'soilretentionEcology:ModerateAnnualPrecipitation
		[1800 2200] 	'soilretentionEcology:HighAnnualPrecipitation
		[2200 :>] 	  'soilretentionEcology:VeryHighAnnualPrecipitation))

(defmodel runoff 'soilretentionEcology:AnnualRunoffClass
	(classification (measurement 'habitat:AnnualRunoff "mm")
		[0 200] 	    'soilretentionEcology:VeryLowAnnualRunoff
		[200 600] 	  'soilretentionEcology:LowAnnualRunoff
		[600 1200]  	'soilretentionEcology:ModerateAnnualRunoff
		[1200 2400] 	'soilretentionEcology:HighAnnualRunoff
		[2400 :>] 	  'soilretentionEcology:VeryHighAnnualRunoff))

;;CAN'T do a global vegetation type defmodel if classes are different: split this up & use the local
;; vegetation type defmodel into the BN
;;Vegetation type
(defmodel vegetation-type 'soilretentionEcology:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		#{41 42 43 71 90 95} 'soilretentionEcology:ForestGrasslandWetland
		#{52 81}             'soilretentionEcology:ShrublandPasture
		#{21 22 23 24 31 82} 'soilretentionEcology:CropsBarrenDeveloped))

(defmodel percent-vegetation-cover 'soilretentionEcology:PercentVegetationCoverClass
	(classification (numeric-coding 'habitat:PercentVegetationCover)
		[80 100] 'soilretentionEcology:VeryHighVegetationCover
		[60 80]  'soilretentionEcology:HighVegetationCover
		[40 60]  'soilretentionEcology:ModerateVegetationCover
		[20 40]  'soilretentionEcology:LowVegetationCover
		[0 20]   'soilretentionEcology:VeryLowVegetationCover))

(defmodel successional-stage 'soilretentionEcology:SuccessionalStageClass
	 (classification (numeric-coding 'ecology:SuccessionalStage)  
	 		#{5 6}      'soilretentionEcology:OldGrowth
	 		4           'soilretentionEcology:LateSuccession
	 		3           'soilretentionEcology:MidSuccession
      2           'soilretentionEcology:PoleSuccession
	 		1           'soilretentionEcology:EarlySuccession
	 		:otherwise  'soilretentionEcology:NoSuccession))

;;Sediment source value
(defmodel sediment-source-value-annual 'soilretentionEcology:SedimentSourceValueAnnualClass
 (classification (measurement 'soilretentionEcology:SedimentSourceValueAnnual "kg/ha")
  		0                          'soilretentionEcology:NoAnnualSedimentSource
  		[:exclusive 0 30000]       'soilretentionEcology:LowAnnualSedimentSource 
  		[30000 100000]             'soilretentionEcology:ModerateAnnualSedimentSource
  		[100000 :>]                'soilretentionEcology:HighAnnualSedimentSource))

;; source bayesian model for Puget Sound   	 
(defmodel source-puget 'soilretentionEcology:SedimentSourceValueAnnualClass
  (bayesian 'soilretentionEcology:SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueAdHoc.xdsl"
    :keep     ('soilretentionEcology:SedimentSourceValueAnnual ) 
    :observed (sediment-source-value-annual) 
    :context  (soil-group slope soil-texture slope-stability precipitation-annual vegetation-type percent-vegetation-cover 
              successional-stage)))

;; Add deterministic model for USLE: Have data for it for the western U.S. and world in 1980.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs 'soilretentionEcology:ReservoirsClass 
  (classification (binary-coding 'geofeatures:Reservoir)
      0          'soilretentionEcology:ReservoirAbsent
      :otherwise 'soilretentionEcology:ReservoirPresent))

(defmodel stream-gradient 'soilretentionEcology:StreamGradientClass 
  (classification (measurement 'habitat:StreamGradient "\u00b0")
    [:<   1.15]  'soilretentionEcology:LowStreamGradient
    [1.15 2.86]  'soilretentionEcology:ModerateStreamGradient
    [2.86 :>]    'soilretentionEcology:HighStreamGradient))

(defmodel floodplain-vegetation-cover 'soilretentionEcology:FloodplainVegetationCoverClass 
  (classification (ranking 'habitat:PercentFloodplainVegetationCover)
    [0 20]   'soilretentionEcology:VeryLowFloodplainVegetationCover
    [20 40]  'soilretentionEcology:LowFloodplainVegetationCover
    [40 60]  'soilretentionEcology:ModerateVegetationCover
    [60 80]  'soilretentionEcology:HighFloodplainVegetationCover
    [80 100] 'soilretentionEcology:VeryHighFloodplainVegetationCover))

(defmodel floodplain-width 'soilretentionEcology:FloodplainWidthClass 
  (classification (measurement 'habitat:FloodplainWidth "m")
    [0 350]     'soilretentionEcology:VeryNarrowFloodplain
    [350 800]   'soilretentionEcology:NarrowFloodplain
    [800 1300]  'soilretentionEcology:WideFloodplain
    [1300 :>]   'soilretentionEcology:VeryWideFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual 'soilretentionEcology:AnnualSedimentSinkClass 
  (classification (measurement 'soilretentionEcology:AnnualSedimentSink "kg/ha")
       [20000 30000]          'soilretentionEcology:HighAnnualSedimentSink
       [10000 20000]          'soilretentionEcology:ModerateAnnualSedimentSink
       [:exclusive 0 10000]   'soilretentionEcology:LowAnnualSedimentSink
       0                      'soilretentionEcology:NoAnnualSedimentSink)) 

(defmodel sediment-sink-us 'soilretentionEcology:AnnualSedimentSinkClass
  (bayesian 'soilretentionEcology:AnnualSedimentSink    
    :import  "aries.core::SedimentSink.xdsl"
    :keep    ('soilretentionEcology:AnnualSedimentSink)
    :observed (sediment-sink-annual) 
    :context (reservoirs stream-gradient floodplain-vegetation-cover floodplain-width)))


;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'soilretentionEcology:Floodplains
	(classification (binary-coding 'geofeatures:Floodplain)
			0 'soilretentionEcology:NotInFloodplain
			1 'soilretentionEcology:InFloodplain))

(defmodel farmland 'soilretentionEcology:Farmland
	"Just a reclass of the regionally appropriate LULC layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		82	       'soilretentionEcology:Farmland
		:otherwise 'soilretentionEcology:Farmland))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level 'soilretentionEcology:HydroelectricUseLevel
  (measurement 'soilretentionEcology:HydroelectricUseLevel "m^3"))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-puget 'soilretentionEcology:DepositionProneFarmers 
  (ranking 'soilretentionEcology:DepositionProneFarmers
       :context ((ranking 'lulc:NLCDNumeric :as farmlandpresent)
                 (ranking 'geofeatures:Floodplain :as floodplains))
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 82.0))
                    1
                    0))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-erosion-use-puget 'soilretentionEcology:ErosionProneFarmers
  (ranking 'soilretentionEcology:ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 82.0)
                  (cond (= (:sediment-source-value-annual %) 'soilretentionEcology:ModerateAnnualSedimentSource)
                        1
                        (= (:sediment-source-value-annual %) 'soilretentionEcology:HighAnnualSedimentSource)
                        2
                        :otherwise
                        0)
                  0)
       :context ((ranking 'lulc:NLCDNumeric :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;(defmodel fishermen-use-puget 'soilretentionEcology:FishermenUse 
	  ;;(bayesian 'soilretentionEcology:FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUse.xdsl"
	 ;; 	:keep     ('soilretentionEcology:FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Everything below needs to be updated correctly for sediment.
 	 								
;;(defmodel altitude 'geophysics:Altitude
  ;;(measurement 'geophysics:Altitude "m"))	 								

(defmodel levees 'soilretentionEcology:LeveesClass 
  (classification (binary-coding 'infrastructure:Levee)
      0          'soilretentionEcology:LeveeAbsent
      :otherwise 'soilretentionEcology:LeveePresent))

;;Add defmodels for stream network & HydroSHEDS?

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
 ;;(defmodel data 'aestheticService:AestheticEnjoyment 
	;;(identification 'aestheticService:AestheticEnjoyment)
		;;  :context (
		;;	source :as source
		;;	homeowners :as use
		;;	sink :as sink
		;;	altitude :as altitude))

;;Sediment flow model for recipients of beneficial sedimentation; 
;; all other parameters except for flow concepts need to be updated	
;;(defmodel sediment-flow 'carbonService:ClimateStability
;;  (span 'carbonService:CO2Removed
;;        'carbonService:CarbonSourceValue 
;;        'carbonService:GreenhouseGasEmissions
;;        'carbonService:CarbonSinkValue 
;;        nil
;;        nil
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold       1.0
;;        :trans-threshold    nil
;;        :source-type        :finite
;;        :sink-type          :finite
;;        :use-type           :finite
;;        :benefit-type       :rival
;;        :rv-max-states      10
;;        :downscaling-factor 8
;;        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
;;               'soilretentionEcology:PotentialSedimentDepositionBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
;;               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleSedimentDepositionBeneficiaries
;;               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
;;               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualSedimentDepositionBeneficiaries
;;               'soilretentionEcology:UnutilizedSedimentSource 'soilretentionEcology:InaccessibleSedimentDepositionBeneficiaries
;;               'soilretentionEcology:AbsorbedSedimentFlow 'soilretentionEcology:NegatedSedimentSource
;;               'soilretentionEcology:LostValuableSediment)
;;        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
;;        :context (source use-simple sink)))

;;Sediment flow model for recipients of avoided detrimental sedimentation; 
;; all other parameters except for flow concepts need to be updated 
;;(defmodel sediment-flow 'carbonService:ClimateStability
;;  (span 'carbonService:CO2Removed
;;        'carbonService:CarbonSourceValue 
;;        'carbonService:GreenhouseGasEmissions
;;        'carbonService:CarbonSinkValue 
;;        nil
;;        nil
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold       1.0
;;        :trans-threshold    nil
;;        :source-type        :finite
;;        :sink-type          :finite
;;        :use-type           :finite
;;        :benefit-type       :rival
;;        :rv-max-states      10
;;        :downscaling-factor 8
;;        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
;;               'soilretentionEcology:PotentialReducedSedimentDepositionBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
;;               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleReducedSedimentDepositionBeneficiaries
;;               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
;;               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualReducedSedimentDepositionBeneficiaries
;;               'soilretentionEcology:UnutilizedDeposition 'soilretentionEcology:AbsorbedSedimentFlow
;;               'soilretentionEcology:NegatedSedimentSource 'soilretentionEcology:BlockedHarmfulSediment)
;;        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
;;        :context (source use-simple sink)))

;;Sediment flow model for recipients of reduced turbidity; 
;; all other parameters except for flow concepts need to be updated 
;;(defmodel sediment-flow 'carbonService:ClimateStability
;;  (span 'carbonService:CO2Removed
;;        'carbonService:CarbonSourceValue 
;;        'carbonService:GreenhouseGasEmissions
;;        'carbonService:CarbonSinkValue 
;;        nil
;;        nil
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold       1.0
;;        :trans-threshold    nil
;;        :source-type        :finite
;;        :sink-type          :finite
;;        :use-type           :finite
;;        :benefit-type       :rival
;;        :rv-max-states      10
;;        :downscaling-factor 8
;;        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
;;               'soilretentionEcology:PotentialReducedTurbidityBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
;;               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleReducedTurbidityBeneficiaries
;;               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
;;               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualReducedTurbidityBeneficiaries
;;               'soilretentionEcology:UnutilizedDeposition 'soilretentionEcology:AbsorbedSedimentFlow 
;;               'soilretentionEcology:NegatedSedimentSource 'soilretentionEcology:ReducedTurbidity)
;;        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
;;        :context (source use-simple sink)))
