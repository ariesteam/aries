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
		  0          'recreationService:LakeAbsent
		  :otherwise 'recreationService:LakePresent))

(defmodel river-stream 'recreationService:RiverStream
  "Presence of a river or stream."
  (classification (ranking 'geofeatures:RiverStream)
		  0          'recreationService:RiverStreamAbsent
		  :otherwise 'recreationService:RiverStreamPresent))

(defmodel mountain 'recreationService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement 'geophysics:Altitude "m")
		  [457 914]  'recreationService:SmallMountain ; 
		  [914 1676] 'recreationService:LargeMountain ; no higher than the Adirondacks!
		  :otherwise 'recreationService:NoMountain)) ; will catch artifacts too		  
		  
(defmodel open-space 'recreationService:OpenSpace
  "Classifies an area as open space according to NLCD 2001 data"
  (classification (ranking 'nlcd:NLCDNumeric)
      #{81 82}       'recreationService:AgriculturalLand
      #{41 42 43}    'recreationService:ForestedLand
      #{31 90 95 52} 'recreationService:OtherOpenLand
      :otherwise     'recreationService:NotOpenLand))

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
;; ViewPosition, TravelTime, PublicAccess, HikingDistance, HikingSlope

(defmodel view-position 'recreationService:ViewPosition
  "Location of a view point, a function of elevation."
  (classification (measurement 'geophysics:Altitude "m")
		  [0 457]   'recreationService:LowViewPosition
		  [457 914] 'recreationService:MediumViewPosition
		  [914 :>]  'recreationService:HighViewPosition))
		  
(defmodel travel-time 'recreationService:TravelTime
	"Travel time to hiking resources"
	(classification (ranking 'recreationService:TravelTime)
			1  'recreationService:ShortTravelTime
			2  'recreationService:ModerateTravelTime
			3  'recreationService:LongTravelTime))
			
(defmodel public-access 'recreationService:PublicAccess
	"describes access constraints to a particular parcel"
	(classification (ranking 'recreationService:PublicAccess)
		  0   'recreationService:PublicLand
		  1		'recreationService:PrivateLandWithAcess
		  2		'recreationService:NoPublicAccess)) 
	
(defmodel hiking-distance 'recreationService:HikingDistance
	"Refers to trail distance between the starting point and the view point"
	(classification (ranking recreationService:HikingDistance)
			1   'recreationService:ShortHikingDistance
			2   'recreationService:MediumHikingDistance
			3   'recreationService:LongHikingDistance))
	
(defmodel hiking-slope 'recreationService:HikingSlope
	"describes the steepness of the hiking trail"
	(classification (ranking 'geophysics:DegreeSlope :units "degrees")
			[:< 10] 'recreationService:LowSlope
			[10 45] 'recreationService:ModerateSlope
			[45 :>]	'recreationService:SteepSlope))
			
(defmodel viewer-enjoyment 'recreationService:ViewerEnjoyment
	(classification 'recreationService:ViewerEnjoyment
  		[0 33]  'recreationService:LowViewerEnjoyment 
  		[33 67]  'recreationService:ModerateViewerEnjoyment 
  		[67 100] 'recreationService:HighViewerEnjoyment))

;; bayesian model
(defmodel user 'recreationService:ViewerEnjoyment
  "Views afforded to recreational users"
  (bayesian 'recreationService:ViewerEnjoyment
    :import   "aries.core::RecreationViewUse.xdsl"
    :keep     ('recreationService:ViewerEnjoyment)
    :context  (view-position travel-time public-access hiking-distance hiking-slope)
    :observed (viewer-enjoyment)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------
;;development, clearcuts, roads, energy infrastructure
(defmodel development 'recreationService:Development
	"Development as defined by the NLCD 2001"
	(classification (ranking 'nlcd:NLCDNumeric)
			22 	'recreationService:LowIntensityDevelopment
			23  'recreationService:MediumIntensityDevelopment
			24  'recreationService:HighIntensityDevelopment)) 

(defmodel clearcuts 'recreationService:Clearcuts
	"Presence of clearcuts" 
  (classification (ranking 'geofeatures:Clearcut)
		  0          'recreationService:ClearcutsAbsent
		  :otherwise 'recreationService:ClearcutsPresent))

(defmodel roads 'recreationService:Roads
  (classification (ranking 'recreationService:Roads)
		  0          'recreationService:RoadsAbsent
		  :otherwise 'recreationService:RoadsPresent))

(defmodel energy-infrastructure 'recreationService:EnergyInfrastructure
	"Presence of energy infrastructure"
	(classification (ranking 'recreationService:EnergyInfrastructure)
			0						'recreationService:EnergyInfrastructureAbsent
			:otherwise	'recreationService:EnergyInfrastructurePresent)) 

(defmodel visual-blight 'recreationService:VisualBlight
	(classification 'recreationService:VisualBlight
  		[0 10]   'recreationService:NoVisualBlight
  		[10 50]  'recreationService:LowVisualBlight
  		[50 90]  'recreationService:ModerateVisualBlight
  		[67 100] 'recreationService:HighVisualBlight))
  		
(defmodel sink 'recreationService:RecreationViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian 'recreationService:ViewSink 
    :import  "aries.core::RecreationViewSink.xdsl"
    :keep    ('recreationService:VisualBlight)
    :context (development roads energy-infrastructure)
    :observed (visual-blight)))

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
