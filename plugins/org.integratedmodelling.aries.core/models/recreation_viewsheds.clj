(ns core.models.recreation-viewsheds
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;Need a new defmodel statement combining lake & river into "waterview"
(defmodel lake 'aestheticService:Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (binary-coding 'geofeatures:Lake)
		  0          'aestheticService:LakeAbsent
		  :otherwise 'aestheticService:LakePresent))

(defmodel river-stream 'recreationService:RiverStream
  "Presence of a river or stream."
  (classification (binary-coding 'geofeatures:River)
		  0          'recreationService:RiverStreamAbsent
		  :otherwise 'recreationService:RiverStreamPresent))

(defmodel mountain 'aestheticService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement 'geophysics:Altitude "m")
		  [457 914]  'aestheticService:SmallMountain ; 
		  [914 8850] 'aestheticService:LargeMountain ;; no higher than Mt. Everest, catches artifacts
		  :otherwise 'aestheticService:NoMountain)) ; will catch artifacts too		  
		  
(defmodel open-space 'recreationService:OpenSpaceClass
  "Classifies an area as open space according to NLCD 2001 data"
  (classification (numeric-coding 'nlcd:NLCDNumeric)
      #{81 82}       'recreationService:AgriculturalLand
      #{41 42 43}    'recreationService:ForestedLand
      #{31 90 95 52} 'recreationService:OtherOpenLand
      :otherwise     'recreationService:NotOpenLand))

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
    :import   "aries.core::RecreationViewSource.xdsl"
    :keep     ('aestheticService:TheoreticalNaturalBeauty)
    :context  (lake river-stream mountain open-space)
    :observed (theoretical-beauty)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------
;;development, clearcuts, roads, energy infrastructure
(defmodel development 'recreationService:Development
  "Development as defined by the NLCD 2001"
  (classification (numeric-coding 'nlcd:NLCDNumeric)
      22          'recreationService:LowIntensityDevelopment
      23          'recreationService:MediumIntensityDevelopment
      24          'recreationService:HighIntensityDevelopment
      :otherwise  'recreationService:NotDeveloped)) 

(defmodel transportation-energy-infrastructure-code 'recreationService:TransportationEnergyInfrastructureCode
   (binary-coding 'recreationService:TransportationEnergyInfrastructureCode
        :context ((binary-coding 'infrastructure:Road                  :as road)
                  (binary-coding 'infrastructure:EnergyInfrastructure  :as energy-infrastructure)) 
        :state   #(if (or (= (:road %) 1)
                          (= (:energy-infrastructure %) 1))
                      1
                      0))) 
(defmodel transportation-energy-infrastructure 'recreationService:TransportationEnergyInfrastructure
   (classification transportation-energy-infrastructure-code
             1  'recreationService:TransportationEnergyInfrastructurePresent 
             0  'recreationService:TransportationEnergyInfrastructureAbsent))                        

(defmodel visual-blight 'aestheticService:VisualBlight
  (classification 'aestheticService:VisualBlight
      [0 10]   'aestheticService:NoBlight
      [10 50]  'aestheticService:LowBlight
      [50 90]  'aestheticService:ModerateBlight
      [90 100] 'aestheticService:HighBlight))
      
(defmodel sink 'aestheticService:ViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian 'aestheticService:ViewSink 
    :import  "aries.core::RecreationViewSink.xdsl"
    :keep    ('aestheticService:VisualBlight)
    :context (development transportation-energy-infrastructure)
    :observed (visual-blight)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------
;; ViewPosition, TravelTime, PublicAccess, HikingDistance, HikingSlope

(defmodel view-position 'recreationService:ViewPositionClass
  "Location of a view point, a function of elevation."
  (classification (measurement 'recreationService:ViewPosition "m")
		  [0 457]   'recreationService:LowViewPosition
		  [457 914] 'recreationService:MediumViewPosition
		  [914 :>]  'recreationService:HighViewPosition))
		  
(defmodel travel-time 'recreationService:TravelTimeClass
	"Travel time to hiking resources"
	(classification (ranking 'recreationService:TravelTime)
			1  'recreationService:ShortTravelTime
			2  'recreationService:ModerateTravelTime
			3  'recreationService:LongTravelTime))
			
(defmodel public-access 'recreationService:PublicAccessClass
	"describes access constraints to a particular parcel"
	(classification (ranking 'recreationService:PublicAccess)
		  0   'recreationService:PublicLand
		  1		'recreationService:PrivateLandWithAccess
		  2		'recreationService:NoPublicAccess)) 
	
(defmodel hiking-distance 'recreationService:HikingDistanceClass
	"Refers to trail distance between the starting point and the view point"
	(classification (ranking 'recreationService:HikingDistance)
			1   'recreationService:ShortHikingDistance
			2   'recreationService:ModerateHikingDistance
			3   'recreationService:LongHikingDistance))
	
(defmodel hiking-slope 'recreationService:HikingSlopeClass
	"describes the steepness of the hiking trail"
	(classification (measurement 'recreationService:HikingSlope "\u00b0")
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
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'aestheticService:AestheticEnjoyment 
	(identification 'aestheticService:AestheticEnjoyment
		:context (
			source :as source
			user :as use
			sink :as sink
			altitude :as altitude)))
			
;; the real enchilada
;;(defmodel view 'aestheticService:AestheticView
  ;;(span 'aestheticService:LineOfSight 
  	    ;;'aestheticService:TheoreticalNaturalBeauty
  	    ;;'aestheticService:HomeownerViewUse
      	;;'aestheticService:TotalVisualBlight
      	;;'aestheticService:View
  	    ;;'geophysics:Altitude
   	;;:sink-type        :relative
   	;;:use-type         :relative
   	;;:benefit-type     :non-rival
   	;;:downscaling-factor 3
   	;;:rv-max-states    10 
    ;;:context
      ;;   (source homeowners sink altitude)
        ;;  (ranking 'eserv:SourceThreshold :value 50)
          ;;(ranking 'eserv:SinkThreshold :value 0.3)
          ;;(ranking 'eserv:UseThreshold :value 0.1)
          ;;(ranking 'eserv:TransitionThreshold :value 1.0))
;;))
