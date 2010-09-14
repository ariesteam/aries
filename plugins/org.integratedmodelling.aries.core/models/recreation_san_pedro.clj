(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

;;(defmodel bird-richness 'habitat:AvianRichness

;;(defmodel harvestable-species...

;;(defmodel public-lands...

(defmodel riparian 'recreationService:Riparian
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  #{77 78 79 81 85 94 97 98 109 110 118}  'aestheticService:RiparianPresent                  
                  :otherwise                              'aestheticService:RiparianAbsent))


;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------
		  
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
		  1		'recreationService:PrivateLandWithAccess
		  2		'recreationService:NoPublicAccess)) 
	
(defmodel hiking-distance 'recreationService:HikingDistance
	"Refers to trail distance between the starting point and the view point"
	(classification (ranking 'recreationService:HikingDistance)
			1   'recreationService:ShortHikingDistance
			2   'recreationService:ModerateHikingDistance
			3   'recreationService:LongHikingDistance))
			
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
    :context  (travel-time public-access hiking-distance)
    :observed (viewer-enjoyment)))

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------
 	 								
;;(defmodel travel-capacity 'recreationService:RoadTravelCapacity

;;(defmodel roads 'infrastructure:Road

;;(defmodel trails 'infrastructure:Path	
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
;;(defmodel data 'aestheticService:AestheticEnjoyment 
;;	(identification 'aestheticService:AestheticEnjoyment
;;		:context (
;;			source :as source
;;			user :as use
;;			sink :as sink
;;			altitude :as altitude)))
			
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
