(ns aries.sediment
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel lake 'aestheticService:Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (ranking 'geofeatures:Lake)
		  0          'aestheticService:LakeAbsent
		  :otherwise 'aestheticService:LakePresent))

(defmodel ocean 'aestheticService:Ocean
  "Just being there."
  (classification (ranking 'geofeatures:Ocean)
		  0          'aestheticService:OceanAbsent
		  :otherwise 'aestheticService:OceanPresent))

(defmodel mountain 'aestheticService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement 'geophysics:Altitude "m")
		  [1000 2750]  'aestheticService:SmallMountain ; 
		  [2750 8850]  'aestheticService:LargeMountain ; no higher than mount Everest!
		  :otherwise   'aestheticService:NoMountain ; will catch artifacts too
		  ))

(defmodel theoretical-beauty 'aestheticService:TheoreticalNaturalBeauty
	(classification 'aestheticService:TheoreticalNaturalBeauty
  		[0 25]   'aestheticService:NoNaturalBeauty 
  		[25 50]  'aestheticService:LowNaturalBeauty 
  		[50 75]  'aestheticService:ModerateNaturalBeauty 
  		[75 100] 'aestheticService:HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticEnjoymentProvision 
    :import   "aries.core::ViewSource.xdsl"
    :keep     ('aestheticService:TheoreticalNaturalBeauty)
    :context  (mountain lake ocean)
    :observed (theoretical-beauty)))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;STILL NEED DEFMODELS FOR hydro reservoirs + all components of fisheries BNs
;; In the long run, could take 2 paths: 1) ditch fisheries BNs & use source/use models for actual fisheries; 2) use BNs as generalized fisheries impact model.

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
    [0]             'soilretentionEcology:NoHydroelectricUse
		[:< 500000]    'soilretentionEcology:ModerateHydroelectricUse
		[500000 :>]    'soilretentionEcology:HighHydroelectricUse))

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

;; STRONGLY consider removing bridges - discuss with Brian - they don't actually narrow the floodplain...
(defmodel bridges 'soilretentionEcology:Bridges 
  (classification (ranking 'soilretentionEcology:Bridges)
		  0          'soilretentionEcology:BridgeAbsent
		  :otherwise 'soilretentionEcology:BridgePresent))

(defmodel sink 'soilretentionEcology:SedimentSink
  (bayesian 'soilretentionEcology:SedimentSink 
    :import  "aries.core::SedimentSink.xdsl"
    :keep    ('soilretentionEcology:SedimentSink)
    :context (reservoirs stream-gradient floodplain-vegetation-cover floodplain-width levees bridges)))

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
