(ns core.models.sediment-dr
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
     (classification (ranking 'habitat:SoilErodibility)
       [:< 0.1]    'soilretentionEcology:VeryLowSoilErodibility
       [0.1 0.225] 'soilretentionEcology:LowSoilErodibility
       [0.225 0.3] 'soilretentionEcology:ModerateSoilErodibility
       [0.3 0.375] 'soilretentionEcology:HighSoilErodibility
       [0.375 :>]  'soilretentionEcology:VeryHighSoilErodibility))

;;Annual precipitation for Mg & DR
(defmodel precipitation-annual 'soilretentionEcology:AnnualPrecipitationClass
	"FIXME this is annual precipitation."
	(classification (measurement 'habitat:AnnualPrecipitation "mm")
    [:< 600] 	    'soilretentionEcology:VeryLowAnnualPrecipitation
		[600 1200] 	  'soilretentionEcology:LowAnnualPrecipitation
		[1200 1800]   'soilretentionEcology:ModerateAnnualPrecipitation
		[1800 2200] 	'soilretentionEcology:HighAnnualPrecipitation
		[2200 :>] 	  'soilretentionEcology:VeryHighAnnualPrecipitation))

;;Tropical storm probability, use only in DR & Mg
(defmodel storm-probability 'soilretentionEcology:TropicalStormProbabilityClass
 (classification (ranking 'habitat:TropicalStormProbability)
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
;; FV the models in here are DR-specific so I see no point in using general specs. It does
;; create problems because the BNs are not prepared to get the GLC classes, which do come
;; up when the DR data have holes. Commenting out the non-DR contingencies - we should put them
;; back when models are general, but then only after making the BNs aware of all possible values.
(defmodel vegetation-type 'soilretentionEcology:VegetationType
	"Just a reclass of the NLCD land use layer"
;	(classification (numeric-coding 'nlcd:NLCDNumeric)
;		#{41 42 43 71 90 95} 'soilretentionEcology:ForestGrasslandWetland
;		#{52 81}             'soilretentionEcology:ShrublandPasture
;		#{21 22 23 24 31 82} 'soilretentionEcology:CropsBarrenDeveloped)
;  (classification (numeric-coding 'mglulc:MGLULCNumeric)
;    #{1 2 4 5 6 10 14}                         'soilretentionEcology:ForestWetland
;    #{3 7 23}                                  'soilretentionEcology:DegradedForest
;		#{8 9 20 21 22 24 25 26 28 29 30 31 32 33} 'soilretentionEcology:Savanna
;    #{11 12 13 16 17}                          'soilretentionEcology:CroplandDeveloped)
  (classification (numeric-coding 'domlulc:DOMLULCNumeric)
    #{1 2 4 6 8 9 11 18 35} 'soilretentionEcology:ForestAndShrubland
    #{22 24 62 63}          'soilretentionEcology:WaterWetlandsMangroves
	 	#{41 45 53}             'soilretentionEcology:ShadeCoffeeCocoa
    #{23 36 38 40 59}       'soilretentionEcology:IntensiveCroplandAndPasture
    #{42}                   'soilretentionEcology:UrbanAndRoads)
;  (classification (numeric-coding 'glc:GLCNumeric)
;		#{1 2 3 4 5 6 7 8 9 15} 'soilretentionEcology:ForestGrasslandWetland
;		#{10 11 12 13 14 17 18} 'soilretentionEcology:ShrublandPasture
;    #{16 19 22}             'soilretentionEcology:CropsBarrenDeveloped)
)

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover 'soilretentionEcology:PercentVegetationCoverClass
	(classification (numeric-coding 'habitat:PercentVegetationCover)
		[70 100]  'soilretentionEcology:HighVegetationCover
		[30 70]  'soilretentionEcology:ModerateVegetationCover
		[0 30]  'soilretentionEcology:LowVegetationCover))

;;Sediment source value
(defmodel sediment-source-value-annual 'soilretentionEcology:SedimentSourceValueAnnualClass
  ;; FV - sorry, my bad - there's a bug so the right way doesn't work as a prototype obs. Will be fixed asap.
  ;; please leave as is for now or the BN won't compile.
	(classification 'soilretentionEcology:SedimentSourceValueAnnual
;	(classification (measurement 'soilretentionEcology:SedimentSourceValueAnnual "t/ha")
      0                     'soilretentionEcology:NoAnnualSedimentSource
  		[:exclusive 0 15]     'soilretentionEcology:LowAnnualSedimentSource 
  		[15 40]               'soilretentionEcology:ModerateAnnualSedimentSource
  		[40 :>]               'soilretentionEcology:HighAnnualSedimentSource))
  		
;; source bayesian model for Dominican Republic
;; FV there is much evidence setting for intermediate nodes here - those should be used for
;; training, when the PI eventually implements it. Commented those below.
(defmodel source-dr 'soilretentionEcology:SedimentSourceValueAnnualClass
  (bayesian 'soilretentionEcology:SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueDRAdHoc.xdsl"
    :keep     ('soilretentionEcology:SedimentSourceValueAnnualClass)
    :observed (sediment-source-value-annual) 
    :context  (soil-group slope soil-texture 
              (comment  soil-erodibility)
              precipitation-annual  
              storm-probability 
              (comment runoff) 
              vegetation-type percent-vegetation-cover))) 

;; Add deterministic model for USLE: Have data for it for the western U.S. and world in 1980.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs 'soilretentionEcology:ReservoirsClass 
  (classification (binary-coding 'geofeatures:Reservoir)
      1          'soilretentionEcology:ReservoirPresent
      :otherwise 'soilretentionEcology:ReservoirAbsent))

(defmodel stream-gradient 'soilretentionEcology:StreamGradientClass 
  (classification (measurement 'habitat:StreamGradient "\u00B0")
    [:<   1.15]  'soilretentionEcology:LowStreamGradient
    [1.15 2.86]  'soilretentionEcology:ModerateStreamGradient
    [2.86 :>]    'soilretentionEcology:HighStreamGradient))

(defmodel floodplain-vegetation-cover 'soilretentionEcology:FloodplainVegetationCoverClass 
  (classification (numeric-coding 'habitat:PercentFloodplainVegetationCover)
    [0 20]   'soilretentionEcology:VeryLowFloodplainVegetationCover
    [20 40]  'soilretentionEcology:LowFloodplainVegetationCover
    [40 60]  'soilretentionEcology:ModerateVegetationCover
    [60 80]  'soilretentionEcology:HighFloodplainVegetationCover
    [80 100] 'soilretentionEcology:VeryHighFloodplainVegetationCover))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual 'soilretentionEcology:AnnualSedimentSinkClass 
  ;; FV temporarily subst with dumb classification - see comment for sediment-source-value-annual
  (classification 'soilretentionEcology:AnnualSedimentSinkClass 
;;  (classification (measurement 'soilretentionEcology:AnnualSedimentSink "t/ha")
       [10 15]              'soilretentionEcology:HighAnnualSedimentSink
       [5 10]               'soilretentionEcology:ModerateAnnualSedimentSink
       [:exclusive 0 5]     'soilretentionEcology:LowAnnualSedimentSink
       0                    'soilretentionEcology:NoAnnualSedimentSink)) 

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-dr 'soilretentionEcology:AnnualSedimentSink
  (bayesian 'soilretentionEcology:AnnualSedimentSink 
    :import  "aries.core::SedimentSinkDR.xdsl"
    :keep    ('soilretentionEcology:AnnualSedimentSinkClass)
    :observed (sediment-sink-annual) 
    :context (reservoirs stream-gradient floodplain-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

;;FROM MG MODEL: UPDATED GLOBAL FLOODPLAINS LAYER - but what's below seems to work...
(defmodel floodplains 'soilretentionEcology:Floodplains
  (classification (binary-coding 'geofeatures:Floodplain)
      0 'soilretentionEcology:NotInFloodplain
      1 'soilretentionEcology:InFloodplain))

;;(defmodel floodplains 'soilretentionEcology:FloodplainsClass
;;	(classification (binary-coding 'geofeatures:Floodplain)
;;			0          'soilretentionEcology:InFloodplain
;;			:otherwise 'soilretentionEcology:NotInFloodplain))

;;(defmodel farmland 'soilretentionEcology:Farmland
;;	"Just a reclass of the regionally appropriate LULC layer"
;;  (classification (binary-coding 'soilretentionEcology:FarmlandCode)
;;    1          'soilretentionEcology:FarmlandPresent
;;    0          'soilretentionEcology:FarmlandAbsent))
;;Above statement (soilretentionEcology:Farmland) is for coffee farmers in the DR; to use farmland 
;; from DR LULC data, comment out the above and turn on the statement below (domlulc:DOMLULCNumeric)
(defmodel farmland 'soilretentionEcology:Farmland
  (classification (numeric-coding 'domlulc:DOMLULCNumeric)
	  	#{23 36 38 40 41 45 53 59}	'soilretentionEcology:FarmlandPresent
		  :otherwise                  'soilretentionEcology:FarmlandAbsent))

;;Reservoirs use for DR: presence/absence only.
(defmodel hydroelectric-use-presence 'soilretentionEcology:HydroelectricUsePresenceClass
	(classification (binary-coding 'geofeatures:Reservoir)
			0			'soilretentionEcology:HydroelectricUseAbsent
			1			'soilretentionEcology:HydroelectricUsePresent))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-dr 'soilretentionEcology:DepositionProneFarmers 
  (binary-coding 'soilretentionEcology:DepositionProneFarmers
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 1.0))
                    1
                    0)
       :context (
          (binary-coding 'soilretentionEcology:FarmlandCode  :as farmlandpresent)
          (binary-coding 'geofeatures:Floodplain :as floodplains)))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-erosion-use-dr 'soilretentionEcology:ErosionProneFarmers
  (ranking 'soilretentionEcology:ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 1.0)
                  (cond (= (:sediment-source-value-annual %) 'soilretentionEcology:ModerateAnnualSedimentSource)
                        1
                        (= (:sediment-source-value-annual %) 'soilretentionEcology:HighAnnualSedimentSource)
                        2
                        :otherwise
                        0)
                  0)
       :context ((binary-coding 'soilretentionEcology:FarmlandCode :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;(defmodel fishermen-use-puget 'soilretentionEcology:FishermenUse 
	  ;;(bayesian 'soilretentionEcology:FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUse.xdsl"
	 ;; 	:keep     ('soilretentionEcology:FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density)))

;;defmodel fishermen-use-mg 'soilretentionEcology:FishermenUse 
	;;  (bayesian 'soilretentionEcology:FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUseMg.xdsl"
	 ;; 	:keep     ('soilretentionEcology:FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands mangroves reefs seagrass population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Everything below needs to be updated correctly for sediment.
 	 								
;;(defmodel altitude 'geophysics:Altitude
  ;;(measurement 'geophysics:Altitude "m"))

;;Add defmodels for stream network & HydroSHEDS?			
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel farmland-soil-deposition-data 'soilretentionEcology:FarmlandSoilDeposition
   (identification 'soilretentionEcology:FarmlandSoilDeposition 
     :context (
       source-dr
       sediment-sink-dr
       farmers-deposition-use-dr)))

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
