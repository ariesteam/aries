(ns aries/view
  (:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel lake 'recreationService:Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (ranking 'geofeatures:Lake)
		  0          'aestheticService:LakeAbsent
		  :otherwise 'aestheticService:LakePresent))

(defmodel river-stream 'recreationService:RiverStream
  "Just being there."
  (classification (ranking 'geofeatures:RiverStream)
		  0          'recreationService:RiverStreamAbsent
		  :otherwise 'recreationService:RiverStreamPresent))

(defmodel mountain 'recreationService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement 'geophysics:Altitude "m")
		  [457 914]  'recreationService:SmallMountain ; 
		  [914 1676]  'recreationService:LargeMountain ; no higher than the Adirondacks!
		  :otherwise   'recreationService:NoMountain ; will catch artifacts too
		  ))
		  
(defmodel open-space 'recreationService:OpenSpace
  "Classifies an area as open space"
  (classification (ranking 'nlcd:NLCDNumeric)
      #{81 82} 'recreationService:AgriculturalLand
      #{41 42 43} 'recreationService:ForestedLand
      #{31 90 95 52} 'recreationService:OtherOpenLand
      :otherwise 'recreationService:NotOpenLand
      )) 

(defmodel theoretical-beauty 'recreationService:TheoreticalNaturalBeauty
	(classification 'recreationService:TheoreticalNaturalBeauty
  		[0 25]   'recreationService:NoNaturalBeauty 
  		[25 50]  'recreationService:LowNaturalBeauty 
  		[50 75]  'recreationService:ModerateNaturalBeauty 
  		[75 100] 'recreationService:HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source 'recreationService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'recreationService:AestheticEnjoymentProvision 
    :import   "aries.core::RecreationViewSource.xdsl"
    :keep     ('recreationService:TheoreticalNaturalBeauty)
    :context  (lake river-stream mountain open-space)
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

;; TODO errors
(defmodel clearcut 'aestheticService:Clearcuts 
  (classification (ranking 'geofeatures:Clearcut)
		  0          'aestheticService:ClearcutsAbsent
		  :otherwise 'aestheticService:ClearcutsPresent))

; use NLCD layers to extract transportation infrastructure
(defmodel commercial-transportation 'aestheticService:CommercialIndustrialTransportation 
  (classification (ranking 'nlcd:NLCDNumeric)
		  23         'aestheticService:TransportationInfrastructurePresent
		  :otherwise 'aestheticService:TransportationInfrastructureAbsent))

; presence/absence of highways
(defmodel highway 'aestheticService:Highways 
  (classification (ranking 'infrastructure:Highway)
		  0          'aestheticService:HighwaysAbsent
		  :otherwise 'aestheticService:HighwaysPresent))

(defmodel sink 'aestheticService:ViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian 'aestheticService:ViewSink 
    :import  "aries.core::ViewSink.xdsl"
    :keep    ('aestheticService:TotalVisualBlight)
    :context (commercial-transportation highway)))

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
