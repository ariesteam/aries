(ns aries.sedimentDR
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'soilretentionEcology:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking 'soilretentionEcology:HydrologicSoilsGroup)
			1       'soilretentionEcology:SoilGroupA
			2       'soilretentionEcology:SoilGroupB
			3       'soilretentionEcology:SoilGroupC
			4       'soilretentionEcology:SoilGroupD))

(defmodel slope 'soilretentionEcology:Slope
		(classification (ranking 'geophysics:DegreeSlope)
			 [:< 1.15] 	  'soilretentionEcology:Level
			 [1.15 4.57] 	'soilretentionEcology:GentlyUndulating
			 [4.57 16.70] 'soilretentionEcology:RollingToHilly
			 [16.70 :>] 	'soilretentionEcology:SteeplyDissectedToMountainous))

(defmodel soil-texture 'soilretentionEcology:SoilTexture
    (classification (categorization 'soilretentionEcology:SoilTexture)
      "Coarse"    'soilretentionEcology:CoarseSoilTexture
      "Medium"    'soilretentionEcology:MediumSoilTexture
      "Fine"      'soilretentionEcology:FineSoilTexture)) 

;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility 'soilretentionEcology:SoilErodibility
     (classification (ranking 'soilretentionEcology:SoilErodibility)
       [:< 0.1]    'soilretentionEcology:VeryLowSoilErodibility
       [0.1 0.225]   'soilretentionEcology:LowSoilErodibility
       [0.225 0.3]   'soilretentionEcology:ModerateSoilErodibility
       [0.3 0.375]   'soilretentionEcology:HighSoilErodibility
       [0.375 :>]     'soilretentionEcology:VeryHighSoilErodibility))

;;Annual precipitation for Mg & DR
(defmodel precipitation-annual 'soilretentionEcology:AnnualPrecipitation
	"FIXME this is annual precipitation."
	(classification (measurement 'soilretentionEcology:AnnualPrecipitation "mm")
    [:< 600] 	    'soilretentionEcology:VeryLowAnnualPrecipitation
		[600 1200] 	  'soilretentionEcology:LowAnnualPrecipitation
		[1200 1800]   'soilretentionEcology:ModerateAnnualPrecipitation
		[1800 2200] 	'soilretentionEcology:HighAnnualPrecipitation
		[2200 :>] 	  'soilretentionEcology:VeryHighAnnualPrecipitation))

;;Tropical storm probability, use only in DR & Mg
(defmodel storm-probability 'soilretentionEcology:TropicalStormProbability
 (classification (ranking 'habitat:TropicalStormProbability)
        0     'soilretentionEcology:NoTropicalStormProbability
      [1 5]   'soilretentionEcology:ModerateTropicalStormProbability
      [6 10]  'soilretentionEcology:HighTropicalStormProbability)) 

;;Annual runoff, whereas snowmelt, precipitation, and temperature are monnthly, so this is problematic.
;;Could divide yearly runoff by 12 but obviously it's not evenly distributed throughout the year.
;;Or could strongly consider just running it on an annual time step, as that's what the data support.
(defmodel runoff 'soilretentionEcology:AnnualRunoff
	(classification (measurement 'soilretentionEcology:AnnualRunoff "mm")
		[0 200] 	    'soilretentionEcology:VeryLowAnnualRunoff
		[200 600] 	  'soilretentionEcology:LowAnnualRunoff
		[600 1200]  	'soilretentionEcology:ModerateAnnualRunoff
		[1200 2400] 	'soilretentionEcology:HighAnnualRunoff
		[2400 :>] 	  'soilretentionEcology:VeryHighAnnualRunoff))

;;Vegetation type
(defmodel vegetation-type 'soilretentionEcology:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		#{41 42 43 71 90 95} 'soilretentionEcology:ForestGrasslandWetland
		#{52 81}             'soilretentionEcology:ShrublandPasture
		#{21 22 23 24 31 82} 'soilretentionEcology:CropsBarrenDeveloped)
  (classification (ranking 'mglulc:MGLULCNumeric)
    #{1 2 4 5 6 10 14}                         'soilretentionEcology:ForestWetland
    #{3 7 23}                                  'soilretentionEcology:DegradedForest
		#{8 9 20 21 22 24 25 26 28 29 30 31 32 33} 'soilretentionEcology:Savanna
    #{11 12 13 16 17}                          'soilretentionEcology:CroplandDeveloped)
  (classification (ranking 'domlulc:DOMLULCNumeric)
    #{1 2 4 6 8 9 11 18 35} 'soilretentionEcology:ForestAndShrubland
    #{22 24 62 63}          'soilretentionEcology:WaterWetlandsMangroves
	 	#{41 45 53}             'soilretentionEcology:ShadeCoffeeCocoa
    #{23 36 38 40 59}       'soilretentionEcology:IntensiveCroplandAndPasture
    #{42}                   'soilretentionEcology:UrbanAndRoads)
  (classification (ranking 'glc:GLCNumeric)
		#{1 2 3 4 5 6 7 8 9 15} 'soilretentionEcology:ForestGrasslandWetland
		#{10 11 12 13 14 17 18} 'soilretentionEcology:ShrublandPasture
    #{16 19 22}             'soilretentionEcology:CropsBarrenDeveloped))

(defmodel percent-vegetation-cover 'soilretentionEcology:PercentVegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 100] 'soilretentionEcology:VeryHighVegetationCover
		[60 80]  'soilretentionEcology:HighVegetationCover
		[40 60]  'soilretentionEcology:ModerateVegetationCover
		[20 40]  'soilretentionEcology:LowVegetationCover
		[0 20]   'soilretentionEcology:VeryLowVegetationCover))

;;Sediment source value
(defmodel sediment-source-value-annual 'soilretentionEcology:SedimentSourceValueAnnual
	(classification (measurement 'soilretentionEcology:SedimentSourceValueAnnual "t/ha")
      0                     'soilretentionEcology:NoAnnualSedimentSource
  		[:exclusive 0 15]     'soilretentionEcology:LowAnnualSedimentSource 
  		[15 40]               'soilretentionEcology:ModerateAnnualSedimentSource
  		[40 :>]               'soilretentionEcology:HighAnnualSedimentSource))

;; source bayesian model for Dominican Republic
(defmodel source-dr 'soilretentionEcology:SedimentSourceValueAnnual
  (bayesian 'soilretentionEcology:SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueDRAdHoc.xdsl"
    :keep     ('soilretentionEcology:SedimentSourceValueAnnual)
    :observed (sediment-source-value-annual) 
    :context  (soil-group slope soil-texture soil-erodibility precipitation-annual  
              storm-probability runoff vegetation-type percent-vegetation-cover))) 

;; Add deterministic model for USLE: Have data for it for the western U.S. and world in 1980.

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'soilretentionEcology:Floodplains
	(classification (ranking 'soilretentionEcology:Floodplains)
			0 'soilretentionEcology:NotInFloodplain
			1 'soilretentionEcology:InFloodplain))

(defmodel farmland 'soilretentionEcology:Farmland
	"Just a reclass of the regionally appropriate LULC layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	       'soilretentionEcology:Farmland
		:otherwise 'soilretentionEcology:Farmland)
  (classification (ranking 'corine:CORINENumeric)
		[12 22 :inclusive]	 'soilretentionEcology:Farmland
		:otherwise           'soilretentionEcology:Farmland)
  (classification (ranking 'mglulc:MGLULCNumeric)
		#{11 12 13} 'soilretentionEcology:Farmland
		:otherwise 'soilretentionEcology:Farmland)
  (classification (ranking 'soilretentionEcology:Farmland)
    1          'soilretentionEcology:FarmlandPresent
    0          'soilretentionEcology:FarmlandAbsent) 
;;Above statement (soilretentionEcology:Farmland) is for coffee farmers in the DR; to use farmland 
;; from DR LULC data, comment out the above and turn on the statement below (domlulc:DOMLULCNumeric)
;;(classification (ranking 'domlulc:DOMLULCNumeric)
		;;#{23 36 38 40 41 45 53 59}	'soilretentionEcology:Farmland
		;;:otherwise                'soilretentionEcology:Farmland)
	(classification (ranking 'glc:GLCNumeric)
		#{16 17 18} 'soilretentionEcology:Farmland
		:otherwise 'soilretentionEcology:Farmland))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.
(defmodel hydroelectric-use-level 'soilretentionEcology:HydroelectricUseLevel
  (measurement 'soilretentionEcology:HydroelectricUseLevel "m^3"))

;;Reservoirs use for DR: presence/absence only.
(defmodel hydroelectric-use-presence 'soilretentionEcology:HydroelectricUsePresence
	(ranking 'soilretentionEcology:HydroelectricUsePresence))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-dr 'soilretentionEcology:DepositionProneFarmers 
  (ranking 'soilretentionEcology:DepositionProneFarmers
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 1.0))
                    1
                    0)
       :context (
          (ranking 'soilretentionEcology:Farmland :as farmlandpresent)
          (ranking 'soilretentionEcology:Floodplains :as floodplains)))) 

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
       :context ((ranking 'soilretentionEcology:Farmland :as farmlandpresent))))

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
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs 'soilretentionEcology:Reservoirs 
  (classification (ranking 'soilretentionEcology:Reservoirs)
		  0          'soilretentionEcology:ReservoirAbsent
		  :otherwise 'soilretentionEcology:ReservoirPresent))

(defmodel stream-gradient 'soilretentionEcology:StreamGradient 
  (classification (ranking 'soilretentionEcology:StreamGradient)
    [:<   1.15]  'soilretentionEcology:LowStreamGradient
    [1.15 2.86]  'soilretentionEcology:ModerateStreamGradient
    [2.86 :>]    'soilretentionEcology:HighStreamGradient))

(defmodel floodplain-vegetation-cover 'soilretentionEcology:FloodplainVegetationCover 
  (classification (ranking 'soilretentionEcology:FloodplainVegetationCover)
    [0 20]   'soilretentionEcology:VeryLowFloodplainVegetationCover
    [20 40]  'soilretentionEcology:LowFloodplainVegetationCover
    [40 60]  'soilretentionEcology:ModerateVegetationCover
    [60 80]  'soilretentionEcology:HighFloodplainVegetationCover
    [80 100] 'soilretentionEcology:VeryHighFloodplainVegetationCover))

(defmodel floodplain-width 'soilretentionEcology:FloodplainWidth 
  (classification (measurement 'soilretentionEcology:FloodplainWidth "m")
    [0 350]     'soilretentionEcology:VeryNarrowFloodplain
    [350 800]   'soilretentionEcology:NarrowFloodplain
    [800 1300]  'soilretentionEcology:WideFloodplain
    [1300 :>]   'soilretentionEcology:VeryWideFloodplain))

(defmodel levees 'soilretentionEcology:Levees 
  (classification (ranking 'soilretentionEcology:Levees)
		  0          'soilretentionEcology:LeveeAbsent
		  :otherwise 'soilretentionEcology:LeveePresent))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual 'soilretentionEcology:AnnualSedimentSink 
  (classification (measurement 'soilretentionEcology:AnnualSedimentSink "t/ha")
       [10 15]              'soilretentionEcology:HighAnnualSedimentSink
       [5 10]               'soilretentionEcology:ModerateAnnualSedimentSink
       [:exclusive 0 5]     'soilretentionEcology:LowAnnualSedimentSink
       0                    'soilretentionEcology:NoAnnualSedimentSink)) 

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-dr 'soilretentionEcology:AnnualSedimentSink
  (bayesian 'soilretentionEcology:AnnualSedimentSink 
    :import  "aries.core::SedimentSinkDR.xdsl"
    :keep    ('soilretentionEcology:AnnualSedimentSink)
    :observed (sediment-sink-annual) 
    :context (reservoirs stream-gradient floodplain-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Everything below needs to be updated correctly for sediment.
 	 								
;;(defmodel altitude 'geophysics:Altitude
  ;;(measurement 'geophysics:Altitude "m"))	 								
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
 ;;(defmodel data 'aestheticService:AestheticEnjoyment 
	;;(identification 'aestheticService:AestheticEnjoyment)
		;;  :context (
		;;	source :as source
		;;	homeowners :as use
		;;	sink :as sink
		;;	altitude :as altitude))
			
;; the real enchilada
;;(defmodel view 'aestheticService:AestheticView
 ;; (span 'aestheticService:LineOfSight 
  	;;    'aestheticService:TheoreticalNaturalBeauty
  	;;    'aestheticService:HomeownerViewUse
    ;;  	'aestheticService:TotalVisualBlight
    ;;  	'aestheticService:View
  	;;    'geophysics:Altitude
  ;; 	:sink-type        :relative
  ;; 	:use-type         :relative
  ;; 	:benefit-type     :non-rival
  ;; 	:downscaling-factor 3
  ;; 	:rv-max-states    10 
  ;;  :context
    ;;     (source homeowners sink altitude
    ;;     (ranking 'eserv:SourceThreshold :value 50)
    ;;     (ranking 'eserv:SinkThreshold :value 0.3)
    ;;     (ranking 'eserv:UseThreshold :value 0.1)
    ;;     (ranking 'eserv:TransitionThreshold :value 1.0))
;;))