(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

;;Presumably put this in a BN, though if you don't need to account for the goofy # system 
;; (i.e., "1" is the highest, but "10" 2nd highest)
(defmodel bird-richness 'recreationService:BirdSpeciesRichness
   (classification (ranking 'habitat:AvianRichness)
       [8 10]       'recreationService:VeryHighBirdSpeciesRichness
       [6 7]        'recreationService:HighBirdSpeciesRichness
       [4 5]        'recreationService:ModerateBirdSpeciesRichness
       [0 3]        'recreationService:LowBirdSpeciesRichness))

;;(defmodel harvestable-species...

;;No public access includes private land, Native American reservations, military land.
;; Accessible public land includes state trust land, BLM, Forest Service, NPS, FWS, etc.
(defmodel public-lands 'recreationService:PublicAccess
    (classification (numeric-coding 'habitat:LandOwnership)
     #{2 3 4 8 12 13 14 15 16 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 61 62 63 64 65 66 67 
       68 69 70 71 73 75 76 82 83 86 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
       117 118 119 120 121 122 123 124 125 126 127}   'recreationService:PublicLand
     #{1 5 6 11 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 72 74 77 
       78 79 84 87 88 89 90}                          'recreationService:NoPublicAccess))

;;(defmodel elevation (high elevation areas may be more attractive for recreation, especially as an escape from 
;;  summer heat)
;;(defmodel altitude 'geophysics:Altitude
;;  (measurement 'geophysics:Altitude "m")) 
;;(defmodel mountain 'aestheticService:Mountain
;;  (classification (measurement 'geophysics:Altitude "m")
;;                  [1400 1800]  'aestheticService:SmallMountain  
;;                  [1800 8850]  'aestheticService:LargeMountain ;; no higher than Mt. Everest, catches artifacts
;;                  :otherwise   'aestheticService:NoMountain))  ;; catches low artifacts

;;defmodel riparian-condition (include this or not?)

(defmodel riparian 'recreationService:Riparian
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  #{77 78 79 81 85 94 97 98 109 110 118}  'recreationService:RiparianPresent                  
                  :otherwise                              'recreationService:RiparianAbsent))


;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;Uncertain whether to keep this or if it goes in the flow model
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

;;Uncertain whether to keep this or if it goes in the flow model
(defmodel hiking-distance 'recreationService:HikingDistance
	"Refers to trail distance between the starting point and the view point"
	(classification (ranking 'recreationService:HikingDistance)
			1   'recreationService:ShortHikingDistance
			2   'recreationService:ModerateHikingDistance
			3   'recreationService:LongHikingDistance))

;;This actually belongs in the source model, right: How much enjoyment can a place potentially provide?
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
;; This is a categorization; values are "interstates", "highways", "arterials", "streets", 
;; "primitive roads", "trails & alleys", "access ramps".
;; This is a massive layer - it may be really tough for ARIES to rasterize every time, while also time
;;   consuming for me to do so...

;;(defmodel roads 'recreationService:Road                   ;;add to recreationService ontology
;;  (classification (binary-coding 'infrastructure:Road)
;;                  1          'recreationService:RoadPresent
;;                  :otherwise 'recreationService:RoadAbsent))

;;(defmodel trails 'recreationService:Trails 
;;  (classification (binary-coding 'infrastructure:Path)
;;                  1          'recreationService:TrailPresent
;;                  :otherwise 'recreationService:TrailAbsent))

;;How to account for congestion and visitor response to it?  See Hunt et al. 2005, Hunt 2008 on modeling
;; visitor choice to recreational sites.
 
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
