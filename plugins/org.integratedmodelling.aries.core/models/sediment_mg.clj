(ns core.models.sediment-mg
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

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

;;Annual precipitation for Mg & DR
(defmodel precipitation-annual 'soilretentionEcology:AnnualPrecipitationClass
	(classification (measurement 'habitat:AnnualPrecipitation "mm")
    [:< 600] 	    'soilretentionEcology:VeryLowAnnualPrecipitation
		[600 1200] 	  'soilretentionEcology:LowAnnualPrecipitation
		[1200 1800]   'soilretentionEcology:ModerateAnnualPrecipitation
		[1800 2200] 	'soilretentionEcology:HighAnnualPrecipitation
		[2200 :>] 	  'soilretentionEcology:VeryHighAnnualPrecipitation))

;;Tropical storm probability, use only in DR & Mg
(defmodel storm-probability 'soilretentionEcology:TropicalStormProbabilityClass
 (classification (numeric-coding 'habitat:TropicalStormProbability)
        0     'soilretentionEcology:NoTropicalStormProbability
      [1 5]   'soilretentionEcology:ModerateTropicalStormProbability
      [6 10]  'soilretentionEcology:HighTropicalStormProbability)) 

;;Annual runoff, whereas snowmelt, precipitation, and temperature are monnthly, so this is problematic.
;;Could divide yearly runoff by 12 but obviously it's not evenly distributed throughout the year.
;;Or could strongly consider just running it on an annual time step, as that's what the data support.
(defmodel runoff 'soilretentionEcology:AnnualRunoffClass
	(classification (measurement 'habitat:AnnualRunoff "mm")
		[0 200] 	    'soilretentionEcology:VeryLowAnnualRunoff
		[200 600] 	  'soilretentionEcology:LowAnnualRunoff
		[600 1200]  	'soilretentionEcology:ModerateAnnualRunoff
		[1200 2400] 	'soilretentionEcology:HighAnnualRunoff
		[2400 :>] 	  'soilretentionEcology:VeryHighAnnualRunoff))

;;Vegetation type
(defmodel vegetation-type 'soilretentionEcology:VegetationType
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
    #{1 2 4 5 6 10 14}                         'soilretentionEcology:ForestWetland
    #{3 7 23}                                  'soilretentionEcology:DegradedForest
		#{8 9 20 21 22 24 25 26 28 29 30 31 32 33} 'soilretentionEcology:Savanna
    #{11 12 13 16 17}                          'soilretentionEcology:CroplandDeveloped))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover 'soilretentionEcology:PercentVegetationCoverClass
  (classification (numeric-coding 'habitat:PercentVegetationCover)
    [70 100]  'soilretentionEcology:HighVegetationCover
    [30 70]  'soilretentionEcology:ModerateVegetationCover
    [0 30]  'soilretentionEcology:LowVegetationCover))

;;Sediment source value
(defmodel sediment-source-value-annual 'soilretentionEcology:SedimentSourceValueAnnualClass
	(classification (measurement 'soilretentionEcology:SedimentSourceValueAnnualClass "t/ha")
      0                     'soilretentionEcology:NoAnnualSedimentSource
  		[:exclusive 0 15]     'soilretentionEcology:LowAnnualSedimentSource 
  		[15 40]               'soilretentionEcology:ModerateAnnualSedimentSource
  		[40 :>]               'soilretentionEcology:HighAnnualSedimentSource))

;; source bayesian model for Madagascar   	 
(defmodel source-mg 'soilretentionEcology:SedimentSourceValueAnnual
  (bayesian 'soilretentionEcology:SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueMgAdHoc.xdsl"
    :keep     ('soilretentionEcology:SedimentSourceValueAnnualClass)
    :observed (sediment-source-value-annual) 
    :context  (soil-group slope soil-texture soil-erodibility precipitation-annual  
              storm-probability runoff vegetation-type percent-vegetation-cover))) 

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs 'soilretentionEcology:ReservoirsClass 
  (classification (binary-coding 'geofeatures:Reservoir)
      1          'soilretentionEcology:ReservoirPresent
      :otherwise 'soilretentionEcology:ReservoirAbsent))

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

;;Having problems generating this layer from Dartmouth Flood Observatory data
;;(defmodel floodplain-width 'soilretentionEcology:FloodplainWidthClass 
;;  (classification (measurement 'habitat:FloodplainWidth "m")
;;    [0 350]     'soilretentionEcology:VeryNarrowFloodplain
;;    [350 800]   'soilretentionEcology:NarrowFloodplain
;;    [800 1300]  'soilretentionEcology:WideFloodplain
;;    [1300 :>]   'soilretentionEcology:VeryWideFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual 'soilretentionEcology:AnnualSedimentSinkClass 
  (classification (measurement 'soilretentionEcology:AnnualSedimentSinkClass "t/ha")
       [10 15]              'soilretentionEcology:HighAnnualSedimentSink
       [5 10]               'soilretentionEcology:ModerateAnnualSedimentSink
       [:exclusive 0 5]     'soilretentionEcology:LowAnnualSedimentSink
       0                    'soilretentionEcology:NoAnnualSedimentSink)) 

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-mg 'soilretentionEcology:AnnualSedimentSink
  (bayesian 'soilretentionEcology:AnnualSedimentSink 
    :import  "aries.core::SedimentSinkMg.xdsl"
    :keep    ('soilretentionEcology:AnnualSedimentSinkClass)
    :observed (sediment-sink-annual) 
    :context (reservoirs stream-gradient floodplain-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'soilretentionEcology:Floodplains
	(classification (binary-coding 'geofeatures:Floodplain)
			0 'soilretentionEcology:NotInFloodplain
			1 'soilretentionEcology:InFloodplain))

(defmodel farmland 'soilretentionEcology:Farmland
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
		#{11 12 13} 'soilretentionEcology:FarmlandPresent 
		:otherwise 'soilretentionEcology:FarmlandAbsent))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level 'soilretentionEcology:HydroelectricUseLevel
  (measurement 'soilretentionEcology:HydroelectricUseLevel "m^3"))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-mg 'soilretentionEcology:DepositionProneFarmers 
  (ranking 'soilretentionEcology:DepositionProneFarmers
       :state #(if (and (= (:floodplains %) 1.0)
                        (contains? #{11.0 12.0 13.0} (:farmlandpresent %)))
                    1
                    0)
       :context (
          (ranking 'mglulc:MGLULCNumeric :as farmlandpresent)
          (ranking 'geofeatures:Floodplain :as floodplains)))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-erosion-use-mg 'soilretentionEcology:ErosionProneFarmers
  (ranking 'soilretentionEcology:ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 11 12 13)
                  (cond (= (:sediment-source-value-annual %) 'soilretentionEcology:ModerateAnnualSedimentSource)
                        1
                        (= (:sediment-source-value-annual %) 'soilretentionEcology:HighAnnualSedimentSource)
                        2
                        :otherwise
                        0)
                  0)
       :context ((ranking 'mglulc:MGLULCNumeric :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;defmodel fishermen-use-mg 'soilretentionEcology:FishermenUse 
	;;  (bayesian 'soilretentionEcology:FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUseMg.xdsl"
	 ;; 	:keep     ('soilretentionEcology:FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands mangroves reefs seagrass population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								

(defmodel streams 'geofeatures:River
  (binary-coding 'geofeatures:River))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

(defmodel farmland-soil-deposition-data 'soilretentionEcology:FarmlandSoilDeposition
   (identification 'soilretentionEcology:FarmlandSoilDeposition 
     :context (
       source-mg
       sediment-sink-mg
       farmers-deposition-use-mg)))

(defmodel reservoir-soil-deposition-data 'soilretentionEcology:ReservoirSoilDeposition
   (identification 'soilretentionEcology:ReservoirSoilDeposition 
     :context (
       source-mg
       sediment-sink-mg
       hydroelectric-use-level)))
			
;;Sediment flow model for recipients of beneficial sedimentation
(defmodel sediment-beneficial 'soilretentionEcology:BeneficialSedimentTransport
  (span 'soilretentionEcology:SedimentTransport
        'soilretentionEcology:SedimentSourceValueAnnualClass 
        'soilretentionEcology:DepositionProneFarmers
        'soilretentionEcology:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilretentionEcology:Floodplains 'geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
               'soilretentionEcology:PotentialSedimentDepositionBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleSedimentDepositionBeneficiaries
               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualSedimentDepositionBeneficiaries
               'soilretentionEcology:UnutilizedSedimentSource 'soilretentionEcology:InaccessibleSedimentDepositionBeneficiaries
               'soilretentionEcology:AbsorbedSedimentFlow 'soilretentionEcology:NegatedSedimentSource
               'soilretentionEcology:LostValuableSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams)))

;;Sediment flow model for recipients of avoided detrimental sedimentation
(defmodel sediment-detrimental 'soilretentionEcology:DetrimentalSedimentTransport
  (span 'soilretentionEcology:SedimentTransport
        'soilretentionEcology:SedimentSourceValueAnnualClass 
        'soilretentionEcology:DepositionProneFarmers ;;change the beneficiary group as needed
        'soilretentionEcology:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilretentionEcology:Floodplains 'geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
               'soilretentionEcology:PotentialReducedSedimentDepositionBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleReducedSedimentDepositionBeneficiaries
               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualReducedSedimentDepositionBeneficiaries
               'soilretentionEcology:UnutilizedDeposition 'soilretentionEcology:AbsorbedSedimentFlow
               'soilretentionEcology:NegatedSedimentSource 'soilretentionEcology:BlockedHarmfulSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams))) ;;change the beneficiary group as needed

;;Sediment flow model for recipients of reduced turbidity; 
(defmodel sediment-turbidity 'soilretentionEcology:DetrimentalTurbidity
  (span 'soilretentionEcology:SedimentTransport
        'soilretentionEcology:SedimentSourceValueAnnualClass 
        'soilretentionEcology:DepositionProneFarmers ;;change the beneficiary group as needed
        'soilretentionEcology:AnnualSedimentSinkClass 
        nil
        ('geophysics:Altitude 'soilretentionEcology:Floodplains 'geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep ('soilretentionEcology:MaximumSedimentSource 'soilretentionEcology:MaximumPotentialDeposition 
               'soilretentionEcology:PotentialReducedTurbidityBeneficiaries 'soilretentionEcology:PossibleSedimentFlow
               'soilretentionEcology:PossibleSedimentSource 'soilretentionEcology:PossibleReducedTurbidityBeneficiaries
               'soilretentionEcology:ActualSedimentFlow 'soilretentionEcology:ActualSedimentSource
               'soilretentionEcology:UtilizedDeposition 'soilretentionEcology:ActualReducedTurbidityBeneficiaries
               'soilretentionEcology:UnutilizedDeposition 'soilretentionEcology:AbsorbedSedimentFlow 
               'soilretentionEcology:NegatedSedimentSource 'soilretentionEcology:ReducedTurbidity)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams))) ;;change the beneficiary group as needed