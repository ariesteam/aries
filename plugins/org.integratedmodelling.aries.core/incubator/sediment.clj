(ns core.models.sediment
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; This is the model for Puget Sound.

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'soilRetentionService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking 'habitat:HydrologicSoilsGroup)
			1       'soilRetentionService:SoilGroupA
			2       'soilRetentionService:SoilGroupB
			3       'soilRetentionService:SoilGroupC
			4       'soilRetentionService:SoilGroupD))

(defmodel slope 'soilRetentionService:SlopeClass
		(classification (measurement 'geophysics:DegreeSlope "\u00b0")
			 [0 1.15] 	  'soilRetentionService:Level
			 [1.15 4.57] 	'soilRetentionService:GentlyUndulating
			 [4.57 16.70] 'soilRetentionService:RollingToHilly
			 [16.70 :>] 	'soilRetentionService:SteeplyDissectedToMountainous))

(defmodel slope-stability 'soilRetentionService:SlopeStabilityClass
    (classification (numeric-coding 'habitat:SlopeStability)	 		
      1           'soilRetentionService:HighSlopeStability
	 		2           'soilRetentionService:ModerateSlopeStability
	 		3           'soilRetentionService:LowSlopeStability))

;;This discretization is for SSURGO/STATSGO, paying attention to texture over inclusion of various sized rock fragments.
(defmodel soil-texture 'soilRetentionService:SoilTextureClass
    (classification (numeric-coding 'habitat:SoilTexture)
      #{2 3 8 9 12 13 15 17 18 19 20 21 22 25 26 27 29 31 32 34 35 36 37 39 40 43 47 48 50 55 59 62 64 65 66 67 68 69 73 74 75 76 78 79 81 82 84 85 86 87 88 89 91 92 96 98 99 105} 'soilRetentionService:CoarseSoilTexture
      #{1 4 5 6 10 11 14 24 28 30 33 38 42 49 57 60 61 63 70 71 72 77 80 83 90 93 94 95 97 102 103 104} 'soilRetentionService:MediumSoilTexture
      #{7 16 23 41 44 45 46 51 52 53 54 56 58 100 101} 'soilRetentionService:FineSoilTexture))


;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility 'soilRetentionService:SoilErodibilityClass
     (classification (numeric-coding 'habitat:SoilErodibility)
       [:< 0.1]    'soilRetentionService:VeryLowSoilErodibility
       [0.1 0.225]   'soilRetentionService:LowSoilErodibility
       [0.225 0.3]   'soilRetentionService:ModerateSoilErodibility
       [0.3 0.375]   'soilRetentionService:HighSoilErodibility
       [0.375 :>]     'soilRetentionService:VeryHighSoilErodibility))

(defmodel precipitation-annual 'soilRetentionService:AnnualPrecipitationClass
	"FIXME this is annual precipitation."
	(classification (measurement 'habitat:AnnualPrecipitation "mm")
    [:< 600] 	    'soilRetentionService:VeryLowAnnualPrecipitation
		[600 1200] 	  'soilRetentionService:LowAnnualPrecipitation
		[1200 1800]   'soilRetentionService:ModerateAnnualPrecipitation
		[1800 2200] 	'soilRetentionService:HighAnnualPrecipitation
		[2200 :>] 	  'soilRetentionService:VeryHighAnnualPrecipitation))

(defmodel runoff 'soilRetentionService:AnnualRunoffClass
	(classification (measurement 'habitat:AnnualRunoff "mm")
		[0 200] 	    'soilRetentionService:VeryLowAnnualRunoff
		[200 600] 	  'soilRetentionService:LowAnnualRunoff
		[600 1200]  	'soilRetentionService:ModerateAnnualRunoff
		[1200 2400] 	'soilRetentionService:HighAnnualRunoff
		[2400 :>] 	  'soilRetentionService:VeryHighAnnualRunoff))

;;CAN'T do a global vegetation type defmodel if classes are different: split this up & use the local
;; vegetation type defmodel into the BN
;;Vegetation type
(defmodel vegetation-type 'soilRetentionService:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		#{41 42 43 71 90 95} 'soilRetentionService:ForestGrasslandWetland
		#{52 81}             'soilRetentionService:ShrublandPasture
		#{21 22 23 24 31 82} 'soilRetentionService:CropsBarrenDeveloped))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover 'soilRetentionService:PercentVegetationCoverClass
  (classification (numeric-coding 'habitat:PercentVegetationCover)
    [70 100]  'soilRetentionService:HighVegetationCover
    [30 70]  'soilRetentionService:ModerateVegetationCover
    [0 30]  'soilRetentionService:LowVegetationCover))

(defmodel successional-stage 'soilRetentionService:SuccessionalStageClass
	 (classification (numeric-coding 'ecology:SuccessionalStage)  
	 		#{5 6}      'soilRetentionService:OldGrowth
	 		4           'soilRetentionService:LateSuccession
	 		3           'soilRetentionService:MidSuccession
      2           'soilRetentionService:PoleSuccession
	 		1           'soilRetentionService:EarlySuccession
	 		:otherwise  'soilRetentionService:NoSuccession))

;;Sediment source value
(defmodel sediment-source-value-annual 'soilRetentionService:SedimentSourceValueAnnualClass
 (classification (measurement 'soilRetentionService:SedimentSourceValueAnnualClass "kg/ha")
  		0                          'soilRetentionService:NoAnnualSedimentSource
  		[:exclusive 0 30000]       'soilRetentionService:LowAnnualSedimentSource 
  		[30000 100000]             'soilRetentionService:ModerateAnnualSedimentSource
  		[100000 :>]                'soilRetentionService:HighAnnualSedimentSource))

;; source bayesian model for Puget Sound   	 
(defmodel source-puget 'soilRetentionService:SedimentSourceValueAnnual
  (bayesian 'soilRetentionService:SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueAdHoc.xdsl"
    :keep     ('soilRetentionService:SedimentSourceValueAnnualClass) 
    :observed (sediment-source-value-annual) 
    :context  (soil-group slope soil-texture precipitation-annual vegetation-type percent-vegetation-cover 
              successional-stage slope-stability)))

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs 'soilRetentionService:ReservoirsClass 
  (classification (binary-coding 'geofeatures:Reservoir)
      0          'soilRetentionService:ReservoirAbsent
      :otherwise 'soilRetentionService:ReservoirPresent))

(defmodel stream-gradient 'soilRetentionService:StreamGradientClass 
  (classification (measurement 'habitat:StreamGradient "\u00b0")
    [:<   1.15]  'soilRetentionService:LowStreamGradient
    [1.15 2.86]  'soilRetentionService:ModerateStreamGradient
    [2.86 :>]    'soilRetentionService:HighStreamGradient))

(defmodel floodplain-vegetation-cover 'soilRetentionService:FloodplainVegetationCoverClass 
  (classification (ranking 'habitat:PercentFloodplainVegetationCover)
    [0 20]   'soilRetentionService:VeryLowFloodplainVegetationCover
    [20 40]  'soilRetentionService:LowFloodplainVegetationCover
    [40 60]  'soilRetentionService:ModerateVegetationCover
    [60 80]  'soilRetentionService:HighFloodplainVegetationCover
    [80 100] 'soilRetentionService:VeryHighFloodplainVegetationCover))

(defmodel floodplain-width 'soilRetentionService:FloodplainWidthClass 
  (classification (measurement 'habitat:FloodplainWidth "m")
    [0 350]     'soilRetentionService:VeryNarrowFloodplain
    [350 800]   'soilRetentionService:NarrowFloodplain
    [800 1300]  'soilRetentionService:WideFloodplain
    [1300 :>]   'soilRetentionService:VeryWideFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual 'soilRetentionService:AnnualSedimentSinkClass 
  (classification (measurement 'soilRetentionService:AnnualSedimentSink "kg/ha")
       [20000 30000]          'soilRetentionService:HighAnnualSedimentSink
       [10000 20000]          'soilRetentionService:ModerateAnnualSedimentSink
       [:exclusive 0 10000]   'soilRetentionService:LowAnnualSedimentSink
       0                      'soilRetentionService:NoAnnualSedimentSink)) 

(defmodel sediment-sink-us 'soilRetentionService:AnnualSedimentSink
  (bayesian 'soilRetentionService:AnnualSedimentSink    
    :import  "aries.core::SedimentSink.xdsl"
    :keep    ('soilRetentionService:AnnualSedimentSinkClass)
    :observed (sediment-sink-annual) 
    :context (reservoirs stream-gradient floodplain-vegetation-cover floodplain-width)))


;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'soilRetentionService:Floodplains
	(classification (categorization 'geofeatures:Floodplain)
			#{"A" "X500"} 'soilRetentionService:InFloodplain
			:otherwise    'soilRetentionService:NotInFloodplain))

(defmodel farmland 'soilRetentionService:Farmland
	"Just a reclass of the regionally appropriate LULC layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		82	       'soilRetentionService:FarmlandPresent
		:otherwise 'soilRetentionService:FarmlandAbsent))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level 'soilRetentionService:HydroelectricUseLevel
  (measurement 'soilRetentionService:HydroelectricUseLevel "m^3"))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-puget 'soilRetentionService:DepositionProneFarmers 
  (ranking 'soilRetentionService:DepositionProneFarmers
       :context ((ranking 'nlcd:NLCDNumeric :as farmlandpresent)
                 (ranking 'geofeatures:Floodplain :as floodplains))
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 82.0))
                    1
                    0))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-erosion-use-puget 'soilRetentionService:ErosionProneFarmers
  (ranking 'soilRetentionService:ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 82.0)
                  (cond (= (:sediment-source-value-annual %) 'soilRetentionService:ModerateAnnualSedimentSource)
                        1
                        (= (:sediment-source-value-annual %) 'soilRetentionService:HighAnnualSedimentSource)
                        2
                        :otherwise
                        0)
                  0)
       :context ((ranking 'nlcd:NLCDNumeric :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;(defmodel fishermen-use-puget 'soilRetentionService:FishermenUse 
	  ;;(bayesian 'soilRetentionService:FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUse.xdsl"
	 ;; 	:keep     ('soilRetentionService:FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								

(defmodel levees 'soilRetentionService:LeveesClass 
  (classification (binary-coding 'infrastructure:Levee)
      0          'soilRetentionService:LeveeAbsent
      :otherwise 'soilRetentionService:LeveePresent))

(defmodel streams 'geofeatures:River
  (binary-coding 'geofeatures:River))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

(defmodel reservoir-soil-deposition-data 'soilRetentionService:ReservoirSoilDeposition
   (identification 'soilRetentionService:ReservoirSoilDeposition 
     :context (
       source-puget
       sediment-sink-us
       hydroelectric-use-level
       levees)))

(defmodel farmland-soil-deposition-data 'soilRetentionService:FarmlandSoilDeposition
   (identification 'soilRetentionService:FarmlandSoilDeposition 
     :context (
       source-puget
       sediment-sink-us
       farmers-deposition-use-puget
       levees)))

;;Sediment flow model for recipients of beneficial sedimentation
(defmodel sediment-beneficial 'soilRetentionService:BeneficialSedimentTransport
  (span 'soilRetentionService:SedimentTransport
        'soilRetentionService:SedimentSourceValueAnnualClass 
        'soilRetentionService:DepositionProneFarmers
        'soilRetentionService:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilRetentionService:Floodplains 'soilRetentionService:LeveesClass 'geofeatures:River) 
        :source-threshold   1000.0
        :sink-threshold     500.0
        :use-threshold      10.0
        :trans-threshold    100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilRetentionService:MaximumSedimentSource 'soilRetentionService:MaximumPotentialDeposition 
               'soilRetentionService:PotentialSedimentDepositionBeneficiaries 'soilRetentionService:PossibleSedimentFlow
               'soilRetentionService:PossibleSedimentSource 'soilRetentionService:PossibleSedimentDepositionBeneficiaries
               'soilRetentionService:ActualSedimentFlow 'soilRetentionService:ActualSedimentSource
               'soilRetentionService:UtilizedDeposition 'soilRetentionService:ActualSedimentDepositionBeneficiaries
               'soilRetentionService:UnutilizedSedimentSource 'soilRetentionService:InaccessibleSedimentDepositionBeneficiaries
               'soilRetentionService:AbsorbedSedimentFlow 'soilRetentionService:NegatedSedimentSource
               'soilRetentionService:LostValuableSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-puget farmers-deposition-use-puget sediment-sink-us altitude levees streams floodplains)))

;;Sediment flow model for recipients of avoided detrimental sedimentation
(defmodel sediment-detrimental 'soilRetentionService:DetrimentalSedimentTransport
  (span 'soilRetentionService:SedimentTransport
        'soilRetentionService:SedimentSourceValueAnnualClass 
        'soilRetentionService:DepositionProneFarmers ;;change the beneficiary group as needed
        'soilRetentionService:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilRetentionService:Floodplains 'soilRetentionService:LeveesClass 'geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold     500.0
        :use-threshold      10.0
        :trans-threshold    100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilRetentionService:MaximumSedimentSource 'soilRetentionService:MaximumPotentialDeposition 
               'soilRetentionService:PotentialReducedSedimentDepositionBeneficiaries 'soilRetentionService:PossibleSedimentFlow
               'soilRetentionService:PossibleSedimentSource 'soilRetentionService:PossibleReducedSedimentDepositionBeneficiaries
               'soilRetentionService:ActualSedimentFlow 'soilRetentionService:ActualSedimentSource
               'soilRetentionService:UtilizedDeposition 'soilRetentionService:ActualReducedSedimentDepositionBeneficiaries
               'soilRetentionService:UnutilizedDeposition 'soilRetentionService:AbsorbedSedimentFlow
               'soilRetentionService:NegatedSedimentSource 'soilRetentionService:BlockedHarmfulSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-puget farmers-deposition-use-puget sediment-sink-us altitude levees streams floodplains))) ;;change the beneficiary group as needed

;;Sediment flow model for recipients of reduced turbidity
(defmodel sediment-turbidity 'soilRetentionService:DetrimentalTurbidity
  (span 'soilRetentionService:SedimentTransport
        'soilRetentionService:SedimentSourceValueAnnualClass 
        'carbonService:GreenhouseGasEmissions  ;;change the beneficiary group as needed
        'soilRetentionService:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilRetentionService:Floodplains 'soilRetentionService:LeveesClass 'geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold     500.0
        :use-threshold       10.0
        :trans-threshold    nil
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilRetentionService:MaximumSedimentSource 'soilRetentionService:MaximumPotentialDeposition 
               'soilRetentionService:PotentialReducedTurbidityBeneficiaries 'soilRetentionService:PossibleSedimentFlow
               'soilRetentionService:PossibleSedimentSource 'soilRetentionService:PossibleReducedTurbidityBeneficiaries
               'soilRetentionService:ActualSedimentFlow 'soilRetentionService:ActualSedimentSource
               'soilRetentionService:UtilizedDeposition 'soilRetentionService:ActualReducedTurbidityBeneficiaries
               'soilRetentionService:UnutilizedDeposition 'soilRetentionService:AbsorbedSedimentFlow 
               'soilRetentionService:NegatedSedimentSource 'soilRetentionService:ReducedTurbidity)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-puget farmers-deposition-use-puget sediment-sink-us altitude levees streams floodplains))) ;;change the beneficiary group as needed
