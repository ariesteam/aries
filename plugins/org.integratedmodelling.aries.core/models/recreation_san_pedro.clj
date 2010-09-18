(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

;;Presumably put this in a BN
(defmodel bird-richness 'recreationService:BirdSpeciesRichness
   (classification (ranking 'habitat:AvianRichness)
       [8 10]       'recreationService:VeryHighBirdSpeciesRichness
       [6 8]        'recreationService:HighBirdSpeciesRichness
       [4 6]        'recreationService:ModerateBirdSpeciesRichness
       [0 4]        'recreationService:LowBirdSpeciesRichness))

;;(defmodel harvestable-species...

;;TRANSITION ZONES?  CONVERT RASTER LULC TO VECTOR & BUFFER.  WHAT TRANSITION ZONES MATTER TO WHAT SPECIES
;; IS IMPORTANT.  ASK BILL K & KEN BOYKIN ABOUT IMPORTANT HABITAT TYPES/TRANSITION ZONES FOR OUR 8 SPP. OF 
;; INTEREST.

;;ELEVATION & COOLNESS MAY MATTER LESS FOR HUNTING IF THE SEASON IS IN LATE FALL/WINTER.  CHECK THE REST OF THE
;; HUNTING SEASONS INFO.

;;No public access includes private land, Native American reservations, military land.
;; Accessible public land includes state trust land, BLM, Forest Service, NPS, FWS, etc.
;; Ft. Huachucha currently is accessible to the public and birdwatching and hunting occur on-base
(defmodel public-lands 'recreationService:PublicAccess
    (classification (numeric-coding 'habitat:LandOwnership)
     #{2 3 4 8 12 13 14 15 16 36 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 61 62 63 64 65 66 67 
       68 69 70 71 73 75 76 82 83 86 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
       117 118 119 120 121 122 123 124 125 126 127}   'recreationService:PublicLand
     #{1 5 6 11 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 72 74 77 
       78 79 84 87 88 89 90}                          'recreationService:NoPublicAccess))

;;(defmodel elevation (high elevation areas may be more attractive for recreation, especially as an escape from 
;;  summer heat).  People do recreate in teh mountains but this might already be accounted for by presense & biodiversity?
;;(defmodel altitude 'geophysics:Altitude
;;  (measurement 'geophysics:Altitude "m")) 
;;(defmodel mountain 'aestheticService:Mountain
;;  (classification (measurement 'geophysics:Altitude "m")
;;                  [1400 1800]  'aestheticService:SmallMountain  
;;                  [1800 8850]  'aestheticService:LargeMountain ;; no higher than Mt. Everest, catches artifacts
;;                  :otherwise   'aestheticService:NoMountain))  ;; catches low artifacts

;;Riparian zones as important to birding and hunting because of their importance to valued animal species
;; and for human preferences to recreate in riparian areas in the desert.
(defmodel riparian-wetland-quality 'sanPedro:RiparianAndWetlandCode
  (numeric-coding 'sanPedro:RiparianAndWetlandCode
                  :context ((numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC :as lulc)
                            (ranking 'sanPedro:RiparianConditionClass :as condition))
                  :state   #(cond (and (== (:condition %) 1)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
                                  1 ;;'sanPedro:LowQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 2)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
                                  2 ;;'sanPedro:ModerateQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 3)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110} (:lulc %)))
                                  3 ;;'sanPedro:HighQualityRiparianOrWetlandPresent

                                  :otherwise 0 ;;'sanPedro:RiparianOrWetlandAbsent
                                  )))

;;VIEW QUALITY AS IMPORTANT, AT LEAST FOR SOME RECREATIONAL TYPES.
;; IF YOU HAD SOURCE MODELS FOR HUNTING, BIRDING, VIEW QUALITY YOU COULD COMBINE THE 3 BASED ON PREFERENCES
;; FOR MULTI-ACTIVITY/MULTI-GOAL RECREATION (SUMMED, WEIGHTED UTILITY FUNCTION)

;;Below are undiscretization statements to be used in a BN.
;;(defmodel birding-quality 'recreationService:BirdingQuality
;;  (classification 'recreationService:BirdingQuality
;;      [0 33]  'recreationService:LowBirdingQuality 
;;      [33 67]  'recreationService:ModerateBirdingQuality 
;;      [67 100] 'recreationService:HighBirdingQuality))

;;(defmodel hunting-quality 'recreationService:HuntingQuality
;;  (classification 'recreationService:HuntingQuality
;;      [0 33]  'recreationService:LowHuntingQuality 
;;      [33 67]  'recreationService:ModerateHuntingQuality 
;;      [67 100] 'recreationService:HighHuntingQuality))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

(defmodel population-density 'policytarget:PopulationDensity
  (count 'policytarget:PopulationDensity "/km^2"))

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
;; visitor choice between alternative recreational sites.
 
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
