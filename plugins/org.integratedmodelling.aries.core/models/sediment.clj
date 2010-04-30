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

(defmodel housing 'aestheticService:PresenceOfHousing
  "Classifies land use from property data."
  ;; specific to Puget region, will not be used if data unavailable
  (classification (categorization 'puget:ParcelUseCategoryKing)
		  #{"R" "K"}  'aestheticService:HousingPresent
		  :otherwise  'aestheticService:HousingAbsent)
  (classification (categorization 'puget:ParcelUseCategoryGraysHarbor)
		"RESIDENTIAL" 'aestheticService:HousingPresent
		:otherwise    'aestheticService:HousingAbsent))
	
(defmodel property-value 'aestheticService:HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  'economics:AppraisedPropertyValue)
		  [:< 100000]      'aestheticService:VeryLowHousingValue
		  [100000 200000]  'aestheticService:LowHousingValue
		  [200000 400000]  'aestheticService:ModerateHousingValue
		  [400000 1000000] 'aestheticService:HighHousingValue
		  [1000000 :>]     'aestheticService:VeryHighHousingValue))

;; bayesian model
(defmodel homeowners 'aestheticService:ViewUse
  "Property owners who can afford to pay for the view"
  (bayesian 'aestheticService:ViewUse 
    :import  "aries.core::ViewUse.xdsl"
    :keep    ('aestheticService:HomeownerViewUse)
    :context (property-value housing)))

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

;; Need to add floodplain width to geoserver & xml.  Need to figure out what the units are, and fix them in the discretization.
(defmodel floodplain-width 'soilretentionEcology:FloodplainWidth 
  (classification (measurement 'soilretentionEcology:FloodplainWidth "INSERT UNITS HERE")
    [0 20]  'soilretentionEcology:VeryNarrowFloodplain
    [20 40]  'soilretentionEcology:NarrowFloodplain
    [40 60] 'soilretentionEcology:WideFloodplain
    [80 100]   'soilretentionEcology:VeryWideFloodplain))

(defmodel levees 'soilretentionEcology:Levees 
  (classification (ranking 'soilretentionEcology:Levees)
		  0          'soilretentionEcology:LeveeAbsent
		  :otherwise 'soilretentionEcology:LeveePresent))

(defmodel bridges 'soilretentionEcology:Bridges 
  (classification (ranking 'soilretentionEcology:Bridges)
		  0          'soilretentionEcology:BridgeAbsent
		  :otherwise 'soilretentionEcology:BridgePresent))

(defmodel sink 'soilretentionEcology:SedimentSink
  "Whatever is absorptive enough to absorb our water"
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
