(ns aries.sediment
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'floodService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (categorization 'floodService:HydrologicSoilsGroup)
			"A"        'floodService:SoilGroupA
			"B"        'floodService:SoilGroupB
			"C"        'floodService:SoilGroupC
			"D"        'floodService:SoilGroupD))

(defmodel slope 'floodService:Slope
		(classification (ranking 'geophysics:DegreeSlope)
			 :units       "degrees" 
			 [:< 1.15] 	  'floodService:Level
			 [1.15 4.57] 	'floodService:GentlyUndulating
			 [4.57 16.70] 'floodService:RollingToHilly
			 [16.70 :>] 	'floodService:SteeplyDissectedToMountainous))

(defmodel slope-stability 'soilretentionEcology:SlopeStability
    (classification (ranking 'soilretentionEcology:SlopeStability)	 		
      1           'soilretentionEcology:HighSlopeStability
	 		2           'soilretentionEcology:ModerateSlopeStability
	 		3           'soilretentionEcology:LowSlopeStability)) 

(defmodel soil-texture 'soilretentionEcology:SoilTexture
    (classification (categorization 'soilretentionEcology:SoilTexture)
      "Coarse"    'soilretentionEcology:CoarseSoilTexture
      "Medium"    'soilretentionEcology:MediumSoilTexture
      "Fine"      'soilretentionEcology:FineSoilTexture)) 

;;First discretization is for Puget Sound, second is for DR & Mg (based on global data)
(defmodel soil-erodibility 'soilretentionEcology:SoilErodibility
     (classification (ranking 'soilretentionEcology:SoilErodibility)
       [:< 0.15]    'soilretentionEcology:VeryLowSoilErodibility
       [0.15 0.2]   'soilretentionEcology:LowSoilErodibility
       [0.2 0.25]   'soilretentionEcology:ModerateSoilErodibility
       [0.25 0.3]   'soilretentionEcology:HighSoilErodibility
       [0.3 :>]     'soilretentionEcology:VeryHighSoilErodibility))

;;(defmodel soil-erodibility 'soilretentionEcology:SoilErodibility
;;     (classification (ranking 'soilretentionEcology:SoilErodibility)
;;       [:< 0.02]       'soilretentionEcology:VeryLowSoilErodibility
;;       [0.02 0.275]    'soilretentionEcology:LowSoilErodibility
;;       [0.275 0.325]   'soilretentionEcology:ModerateSoilErodibility
;;       [0.325 0.375]   'soilretentionEcology:HighSoilErodibility
;;      [0.375 :>]      'soilretentionEcology:VeryHighSoilErodibility))

(defmodel precipitation 'floodService:Precipitation
	"FIXME this is total monthly precipitation I believe."
	(classification (measurement 'habitat:Precipitation "in")
		[:< 3] 	  'floodService:VeryLowPrecipitation
		[3 6] 	  'floodService:LowPrecipitation
		[6 12] 	  'floodService:ModeratePrecipitation
		[12 24] 	'floodService:HighPrecipitation
		[24 :>] 	'floodService:VeryHighPrecipitation))

;; surface temperature - again, should be monthly and matched by temporal extents.
(defmodel monthly-temperature 'floodService:MonthlyTemperature
		(classification (ranking 'geophysics:GroundSurfaceTemperature "C")
			 [4 :>] 	'floodService:HighTemperature
			 [-4 4] 	'floodService:ModerateTemperature
			 [:< -4] 	'floodService:LowTemperature))

(defmodel monthly-snowmelt 'soilretentionEcology:MonthlySnowmelt
	(classification (measurement 'soilretentionEcology:MonthlySnowmelt "mm/month")
		0 	                  'soilretentionEcology:NoSnowmelt
		[:exclusive 0 50] 	  'soilretentionEcology:LowSnowmelt
		[50 100] 	            'soilretentionEcology:ModerateSnowmelt
		[100 :>]            	'soilretentionEcology:HighSnowmelt))

;;This is annual runoff, whereas snowmelt, precipitation, and temperature are monnthly, so this is problematic.
;;Could divide yearly runoff by 12 but obviously it's not evenly distributed throughout the year.
(defmodel runoff 'soilretentionEcology:Runoff
	(classification (measurement 'soilretentionEcology:Runoff "mm/yr")
		[0 200] 	    'soilretentionEcology:VeryLowRunoff
		[200 600] 	  'soilretentionEcology:LowRunoff
		[600 1200]  	'soilretentionEcology:ModerateRunoff
		[1200 2400] 	'soilretentionEcology:HighRunoff
		[2400 :>] 	  'soilretentionEcology:VeryHighRunoff))

;;DOUBLE CHECK flood model to make sure braces are used correctly and not parenthesis
(defmodel vegetation-type 'soilretentionEcology:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		#{21 22 23 24 31 82} 'soilretentionEcology:CropsBarrenDeveloped
		#{41 42 43 71 90 95} 'soilretentionEcology:ForestGrasslandWetland
		#{52 81}             'soilretentionEcology:ShrublandPasture))
;;NEED TO GET GLOBAL LULC TO UNDERLIE THIS

;;Some null values greater than 100 - comment these out.
(defmodel percent-vegetation-cover 'floodService:PercentVegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 :>] 'floodService:VeryHighVegetationCover
		[60 80] 'floodService:HighVegetationCover
		[40 60] 'floodService:ModerateVegetationCover
		[20 40] 'floodService:LowVegetationCover
		[0 20]  'floodService:VeryLowVegetationCover))

(defmodel successional-stage 'floodService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)  
	 		#{5 6}      'floodService:OldGrowth
	 		4           'floodService:LateSuccession
	 		3           'floodService:MidSuccession
	 		2           'floodService:EarlySuccession
	 		1           'floodService:PoleSuccession
	 		:otherwise  'floodService:NoSuccession))

;;We have data for the top node, which is great, but it's from 1980 at the global level, so not
;;temporally consistent.  I'm also unclear on how to use conditional statements to model the process
;;with different discretizations and model components in Puget Sound, Madagascar, and the DR.
;;Discretization below is for Puget Sound; discretization will look different for DR & Mg
(defmodel sediment-source-value 'soilretentionEcology:SedimentSourceValue
	(classification 'soilretentionEcology:SedimentSourceValue
  		0                          'soilretentionEcology:NoSedimentSource
  		[:exclusive 0 30000]       'soilretentionEcology:LowSedimentSource 
  		[30000 100000]             'soilretentionEcology:ModerateSedimentSource
  		[100000 :>]                'soilretentionEcology:HighSedimentSource))

;; source bayesian model for Puget Sound   
;; I'm unclear about what to replace "AestheticEnjoymentProvision" and "theoretical-beauty" with since 
;; they're different from the name of the top node. 		 
(defmodel source 'aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticEnjoymentProvision 
    :import   "aries.core::SedimentSourceValueAdHoc.xdsl"
    :keep     ('soilretentionEcology:SedimentSourceValue
    :context  (soil-group slope soil-texture slope-stability precipitation monthly-snowmelt 
              monthly-temperature runoff vegetation-type percent-vegetation-cover successional-stage)
    :observed (theoretical-beauty)))

;;BELOW is another piece of the model to use in Mg & DR:

(defmodel storm-probability 'coastalProtection:TropicalStormProbability
	(ranking 'habitat:TropicalStormProbability
      0     'habitat:NoTropicalStormProbability
      [1 5]   'habitat:ModerateTropicalStormProbability
      [6 10]  'habitat:HighTropicalStormProbability))

;;Discretization of top node for DR & Mg (could revisit values - see what Lee & Nathlay come up with)
(defmodel sediment-source-value 'soilretentionEcology:SedimentSourceValue
	(classification 'soilretentionEcology:SedimentSourceValue
      0                     'soilretentionEcology:NoSedimentSource
  		[:exclusive 0 15]     'soilretentionEcology:LowSedimentSource 
  		[15 40]               'soilretentionEcology:ModerateSedimentSource
  		[40 :>]               'soilretentionEcology:HighSedimentSource))

;; source bayesian model for DR & Mg
;; I'm unclear about what to replace "AestheticEnjoymentProvision" and "theoretical-beauty" with since 
;; they're different from the name of the top node. 
;; Need to add successional stage if Lee & Nathaly are going to use it for the DR: it won't be used for Mg		 
(defmodel source 'aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticEnjoymentProvision 
    :import   "aries.core::SedimentSourceValueMgAdHoc"
    :keep     ('soilretentionEcology:SedimentSourceValue
    :context  (soil-group slope soil-texture precipitation storm-probability 
              runoff vegetation-type percent-vegetation-cover)
    :observed (theoretical-beauty)))

;; Add deterministic model for USLE: Have data for it for the western U.S. and world in 1980.

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;THIS IS FOR PUGET SOUND: DO WE WANT A SEPARATE CLOJURE FILE FOR MG?  SEPARATE MODELS?  SAME MODELS?
;;GARY THOUGHTS?

;;STILL NEED DEFMODELS FOR all components of fisheries BNs.  In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.

(defmodel floodplains 'floodService:Floodplain
	(classification (ranking 'floodService:Floodplains)
			0 'floodService:NotInFloodplain
			1 'floodService:InFloodplain))
	
(defmodel farmland 'floodService:Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	       'floodService:FarmlandPresent
		:otherwise 'floodService:FarmlandAbsent
;    :agent     "aries/flood/farm"
    :editable  true))

;;Use normal dam storage (ac-ft) as a proxy for hyroelectric generation capacity 
;;(in reality dam heigh & flow are important factors but we don't have flow data.
;; NEED TO do wfs2opal for reservoirs, use "normal_sto" as the attribute of interest.
(defmodel hydroelectric-use 'soilretentionEcology:HydroelectricUse
	(classification (ranking 'soilretentionEcology:HydroelectricUse)
    0                        'soilretentionEcology:NoHydroelectricUse
		[:exclusive 0 500000]    'soilretentionEcology:ModerateHydroelectricUse
		[500000 :>]              'soilretentionEcology:HighHydroelectricUse))

;;Have rasterized the dam point layer for Mg but it's not yet in Geoserver.  Gary: problems with using
;; a raster point file for reservoirs?

;;Reservoirs layer for DR: no values, so it's presence/absence only.  Need to set below so it's like below
;;without creating errors.  Get rid of []'s
(defmodel hydroelectric-use 'soilretentionEcology:HydroelectricUse
	(classification (ranking 'soilretentionEcology:HydroelectricUse)
    [0]           'soilretentionEcology:NoHydroelectricUse
		[1]           'soilretentionEcology:ModerateHydroelectricUse
		[NULL (?)]    'soilretentionEcology:HighHydroelectricUse))

;; Farmer users in floodplains, i.e., where sedimentation is desirable or undesirable, depending on conditions
(defmodel farmers-use 'soilretentionEcology:DepositionProneFarmers 
	  (bayesian 'soilretentionEcology:DepositionProneFarmers 
	  	:import   "aries.core::SedimentDepositionProneFarmersUse.xdsl"
	  	:keep     ('soilretentionEcology:DepositionProneFarmers)
	 	 	:context  (farmland floodplains)))

;;Get Gary's input on what to do with "erosion prone farmers" since erosion source value goes into the BN.

;; Fishermen use model: lots of work to do on this one.  Define the model
;; with all possible nodes included and let it choose which to use based
;; on the BN???  Or do 2 of these statements, 1 each for Mg & Puget?
;; What about intermediate variables & deterministic nodes?
(defmodel fishermen-use 'soilretentionEcology:FishermenUse 
	  (bayesian 'soilretentionEcology:FishermenUse  
	  	:import   "aries.core::SedimentFishermenUse.xdsl"
	  	:keep     ('soilretentionEcology:FishermenUse)
	 	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density fishing-sites habitat-at-risk-of-sedimentation)))

defmodel fishermen-use 'soilretentionEcology:FishermenUse 
	  (bayesian 'soilretentionEcology:FishermenUse  
	  	:import   "aries.core::SedimentFishermenUseMg.xdsl"
	  	:keep     ('soilretentionEcology:FishermenUse)
	 	 	:context  (lakes rivers coastline coastal-wetlands mangroves reefs seagrass population-density fishing-sites habitat-at-risk-of-sedimentation)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;; Make sure dam point files are converted to reservoir shapefiles.  Make reservoir storage a new xml entity, with presence/absence, rename "reservoir storage" in the flood model, BNs, ontologies, xml...
(defmodel reservoirs 'soilretentionEcology:Reservoirs 
  (classification (ranking 'soilretentionEcology:Reservoirs)
		  0          'soilretentionEcology:ReservoirAbsent
		  :otherwise 'soilretentionEcology:ReservoirPresent))

(defmodel stream-gradient 'soilretentionEcology:StreamGradient 
  (classification (measurement 'soilretentionEcology:StreamGradient "°")
    [:<   1.15]  'soilretentionEcology:LowStreamGradient
    [1.15 2.86] 'soilretentionEcology:ModerateStreamGradient
    [2.86 :>]   'soilretentionEcology:HighStreamGradient))

(defmodel floodplain-vegetation-cover 'soilretentionEcology:FloodplainVegetationCover 
  (classification (measurement 'soilretentionEcology:FloodplainVegetationCover "%")
    [0 20]  'soilretentionEcology:VeryLowFloodplainVegetationCover
    [20 40]  'soilretentionEcology:LowFloodplainVegetationCover
    [40 60] 'soilretentionEcology:ModerateVegetationCover
    [60 80]  'soilretentionEcology:HighFloodplainVegetationCover
    [80 100]   'soilretentionEcology:VeryHighStreamGradient))

;; Need to add floodplain width to geoserver & xml.  These units are in decimal degrees - correspond to
;; breakpoints of roughly 375, 820, and 1320 m.
(defmodel floodplain-width 'soilretentionEcology:FloodplainWidth 
  (classification (measurement 'soilretentionEcology:FloodplainWidth "INSERT UNITS HERE")
    [0 0.0034]  'soilretentionEcology:VeryNarrowFloodplain
    [0.0034 0.0074]  'soilretentionEcology:NarrowFloodplain
    [0.0074 0.0119] 'soilretentionEcology:WideFloodplain
    [0.0119 :>]   'soilretentionEcology:VeryWideFloodplain))

(defmodel levees 'soilretentionEcology:Levees 
  (classification (ranking 'soilretentionEcology:Levees)
		  0          'soilretentionEcology:LeveeAbsent
		  :otherwise 'soilretentionEcology:LeveePresent))

(defmodel sink 'soilretentionEcology:SedimentSink
  (bayesian 'soilretentionEcology:SedimentSink 
    :import  "aries.core::SedimentSink.xdsl"
    :keep    ('soilretentionEcology:SedimentSink)
    :context (reservoirs stream-gradient floodplain-vegetation-cover floodplain-width levees)))

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'aestheticService:AestheticEnjoyment 
	(identification 'aestheticService:AestheticEnjoyment)
		:context (
			source :as source
			homeowners :as use
			sink :as sink
			altitude :as altitude))
			
;; the real enchilada
(defmodel view 'aestheticService:AestheticView
  (span 'aestheticService:LineOfSight 
  	    'aestheticService:TheoreticalNaturalBeauty
  	    'aestheticService:HomeownerViewUse
      	'aestheticService:TotalVisualBlight
      	'aestheticService:View
  	    'geophysics:Altitude
   	:sink-type        :relative
   	:use-type         :relative
   	:benefit-type     :non-rival
   	:downscaling-factor 3
   	:rv-max-states    10 
    :context
         (source homeowners sink altitude
          (ranking 'eserv:SourceThreshold :value 50)
          (ranking 'eserv:SinkThreshold :value 0.3)
          (ranking 'eserv:UseThreshold :value 0.1)
          (ranking 'eserv:TransitionThreshold :value 1.0))
))
