(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;;  Birding source model
;; ----------------------------------------------------------------------------------------------

(defmodel bird-richness 'recreationService:BirdSpeciesRichness
   (classification (ranking 'habitat:AvianRichness)
       [8 10]       'recreationService:VeryHighBirdSpeciesRichness
       [6 8]        'recreationService:HighBirdSpeciesRichness
       [4 6]        'recreationService:ModerateBirdSpeciesRichness
       [0 4]        'recreationService:LowBirdSpeciesRichness))

;;Riparian zones as important to birding and hunting because of their importance to valued animal species
;; and for human preferences to recreate in riparian areas in the desert.
(defmodel riparian-wetland-code 'sanPedro:RiparianAndWetlandCode
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

                                  :otherwise 0 ;;'sanPedro:NoRiparianOrWetlandPresentHig
                                  )))
(defmodel riparian-wetland 'sanPedro:RiparianAndWetland
  (classification riparian-wetland-code
                  3 'sanPedro:HighQualityRiparianOrWetlandPresent
                  2 'sanPedro:ModerateQualityRiparianOrWetlandPresent
                  1 'sanPedro:LowQualityRiparianOrWetlandPresent
                  0 'sanPedro:RiparianOrWetlandAbsent))

;;No public access includes private land, Native American reservations, military land.
;; Accessible public land includes state trust land, BLM, Forest Service, NPS, FWS, etc.
;; Ft. Huachucha currently is accessible to the public and birdwatching and hunting occur on-base
(defmodel public-lands 'recreationService:PublicAccessClass
    (classification (numeric-coding 'habitat:LandOwnership)
     #{2 3 4 8 12 13 14 15 16 36 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 61 62 63 64 65 66 67 
       68 69 70 71 73 75 76 82 83 86 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
       117 118 119 120 121 122 123 124 125 126 127}   'recreationService:PublicLand
     #{1 5 6 11 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 72 74 77 
       78 79 84 87 88 89 90}                          'recreationService:NoPublicAccess))

;;Undiscretization statements to be use in BNs
(defmodel birding-quality 'recreationService:SiteBirdingQuality
  (classification 'recreationService:SiteBirdingQuality
      [0 10]   'recreationService:VeryLowBirdingQuality 
      [10 33]  'recreationService:LowBirdingQuality 
      [33 67]  'recreationService:ModerateBirdingQuality 
      [67 100] 'recreationService:HighBirdingQuality))

;; Bayesian source models
(defmodel source-birding 'recreationService:BirdingSourceValue   
  (bayesian 'recreationService:BirdingSourceValue                
            :import   "aries.core::RecreationBirdingSourceSanPedro.xdsl"
            :keep     ('recreationService:SiteBirdingQuality)
            :observed (birding-quality)
            :context  (bird-richness riparian-wetland public-lands)))

;; ----------------------------------------------------------------------------------------------
;;  Hunting source model
;; ----------------------------------------------------------------------------------------------

(defmodel dove-habitat-code 'sanPedro:DoveHabitatCode
   (numeric-coding 'sanPedro:DoveHabitatCode
        :context ((numeric-coding 'sanPedro:MourningDoveHabitat     :as mourning-dove)
                  (numeric-coding 'sanPedro:WhiteWingedDoveHabitat  :as white-winged-dove)) 
       :state    #(let [num-dove (Math/ceil (/ (+ (:mourning-dove  %)
                                                   (:white-winged-dove %)) 
                                              29.0))]
                        num-dove)))
(defmodel dove-habitat 'sanPedro:DoveHabitat
    (classification dove-habitat-code
             2 'sanPedro:MultipleDoveSpeciesPresent
             1 'sanPedro:SingleDoveSpeciesPresent
             0 'sanPedro:DoveSpeciesAbsent))

(defmodel deer-habitat-code 'sanPedro:DeerHabitatCode
   (numeric-coding 'sanPedro:DeerHabitatCode
        :context ((numeric-coding 'sanPedro:MuleDeerHabitat       :as mule-deer)
                  (numeric-coding 'sanPedro:WhiteTailDeerHabitat  :as white-tail-deer)) 
        :state    #(let [num-deer (Math/ceil (/ (+ (:mule-deer  %)
                                                   (:white-tail-deer %)) 
                                              29.0))]
                        num-deer)))
(defmodel deer-habitat 'sanPedro:DeerHabitat
    (classification deer-habitat-code
             2 'sanPedro:MultipleDeerSpeciesPresent
             1 'sanPedro:SingleDeerSpeciesPresent
             0 'sanPedro:DeerSpeciesAbsent))

(defmodel quail-habitat-code 'sanPedro:QuailHabitatCode
   (numeric-coding 'sanPedro:QuailHabitatCode
        :context ((numeric-coding 'sanPedro:ScaledQuailHabitat  :as scaled-quail)
                  (numeric-coding 'sanPedro:MearnsQuailHabitat  :as mearns-quail) 
                  (numeric-coding 'sanPedro:GambelsQuailHabitat :as gambels-quail)) 
        :state    #(let [num-quail (Math/floor (/ (+ (:scaled-quail %)
                                                     (:mearns-quail %)
                                                     (:gambels-quail %)) 
                                                29.0))]
                      (min num-quail 2)))) 
(defmodel quail-habitat 'sanPedro:QuailHabitat
    (classification quail-habitat-code
             2 'sanPedro:MultipleQuailSpeciesPresent
             1 'sanPedro:SingleQuailSpeciesPresent
             0 'sanPedro:QuailSpeciesAbsent))

(defmodel javelina-habitat 'sanPedro:JavelinaHabitat
    (classification (numeric-coding 'sanPedro:JavelinaHabitat)
     29            'sanPedro:JavelinaHabitatPresent
     :otherwise    'sanPedro:JavelinaHabitatAbsent)) 

;;TRANSITION ZONES?  CONVERT RASTER LULC TO VECTOR & BUFFER.  WHAT TRANSITION ZONES MATTER TO WHAT SPECIES
;; IS IMPORTANT.  ASK BILL K & KEN BOYKIN ABOUT IMPORTANT HABITAT TYPES/TRANSITION ZONES FOR OUR 8 SPP. OF 
;; INTEREST.

;;ELEVATION & COOLNESS MAY MATTER LESS FOR HUNTING IF THE SEASON IS IN LATE FALL/WINTER.  CHECK THE REST OF THE
;; HUNTING SEASONS INFO.

;;(defmodel elevation (high elevation areas may be more attractive for recreation, especially as an escape from 
;;  summer heat).  People do recreate in teh mountains but this might already be accounted for by presense & biodiversity?
;;(defmodel mountain 'aestheticService:Mountain
;;  (classification (measurement 'geophysics:Altitude "m")
;;                  [1400 1800]  'aestheticService:SmallMountain  
;;                  [1800 8850]  'aestheticService:LargeMountain
;;                  :otherwise   'aestheticService:NoMountain))

;;VIEW QUALITY AS IMPORTANT, AT LEAST FOR SOME RECREATIONAL TYPES.
;; IF YOU HAD SOURCE MODELS FOR HUNTING, BIRDING, VIEW QUALITY YOU COULD COMBINE THE 3 BASED ON PREFERENCES
;; FOR MULTI-ACTIVITY/MULTI-GOAL RECREATION (SUMMED, WEIGHTED UTILITY FUNCTION)

;;Undiscretization statements to be use in BNs
(defmodel deer-hunting-quality 'recreationService:SiteDeerHuntingQuality
  (classification 'recreationService:SiteDeerHuntingQuality
      [0 5]    'recreationService:VeryLowDeerHuntingQuality 
      [5 33]   'recreationService:LowDeerHuntingQuality 
      [33 67]  'recreationService:ModerateDeerHuntingQuality 
      [67 100] 'recreationService:HighDeerHuntingQuality))

(defmodel javelina-hunting-quality 'recreationService:SiteJavelinaHuntingQuality
  (classification 'recreationService:SiteJavelinaHuntingQuality
      [0 5]    'recreationService:VeryLowJavelinaHuntingQuality 
      [5 33]   'recreationService:LowJavelinaHuntingQuality 
      [33 67]  'recreationService:ModerateJavelinaHuntingQuality 
      [67 100] 'recreationService:HighJavelinaHuntingQuality))

(defmodel dove-hunting-quality 'recreationService:SiteDoveHuntingQuality
  (classification 'recreationService:SiteDoveHuntingQuality
      [0 5]    'recreationService:VeryLowDoveHuntingQuality 
      [5 33]   'recreationService:LowDoveHuntingQuality 
      [33 67]  'recreationService:ModerateDoveHuntingQuality 
      [67 100] 'recreationService:HighDoveHuntingQuality))

(defmodel quail-hunting-quality 'recreationService:SiteQuailHuntingQuality
  (classification 'recreationService:SiteQuailHuntingQuality
      [0 5]    'recreationService:VeryLowQuailHuntingQuality 
      [5 33]   'recreationService:LowQuailHuntingQuality 
      [33 67]  'recreationService:ModerateQuailHuntingQuality 
      [67 100] 'recreationService:HighQuailHuntingQuality))

;; Bayesian source models
(defmodel source-deer-hunting 'recreationService:DeerHuntingSourceValue
  (bayesian 'recreationService:DeerHuntingSourceValue                
            :import   "aries.core::RecreationHuntingDeerSourceSanPedro.xdsl"
            :keep     ('recreationService:SiteDeerHuntingQuality)
            :observed (deer-hunting-quality)
            :context  (riparian-wetland public-lands deer-habitat)))

(defmodel source-javelina-hunting 'recreationService:JavelinaHuntingSourceValue  
  (bayesian 'recreationService:JavelinaHuntingSourceValue                
            :import   "aries.core::RecreationHuntingJavelinaSourceSanPedro.xdsl"
            :keep     ('recreationService:SiteJavelinaHuntingQuality)
            :observed (javelina-hunting-quality)
            :context  (riparian-wetland public-lands javelina-habitat)))

(defmodel source-dove-hunting 'recreationService:DoveHuntingSourceValue
  (bayesian 'recreationService:DoveHuntingSourceValue
            :import   "aries.core::RecreationHuntingDoveSourceSanPedro.xdsl"
            :keep     ('recreationService:SiteDoveHuntingQuality)
            :observed (dove-hunting-quality)
            :context  (riparian-wetland public-lands dove-habitat)))

(defmodel source-quail-hunting 'recreationService:QuailHuntingSourceValue
  (bayesian 'recreationService:QuailHuntingSourceValue                
            :import   "aries.core::RecreationHuntingQuailSourceSanPedro.xdsl"
            :keep     ('recreationService:SiteQuailHuntingQuality)
            :observed (quail-hunting-quality)
            :context  (riparian-wetland public-lands quail-habitat)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel population-density 'policytarget:PopulationDensity
  (count 'policytarget:PopulationDensity "/km^2"))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
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
;; Top-level service models 
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
;;(defmodel recreation-flow 'carbonService:ClimateStability
;;  (span 'carbonService:CO2Removed
;;        'recreationService:BirdingSourceValue   ;;and corresponding hunting/wildlife viewing models 
;;        'carbonService:GreenhouseGasEmissions
;;        nil
;;        nil
;;        nil
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold       1.0
;;        :trans-threshold    nil
;;        :source-type        :finite
;;        :sink-type          :finite
;;        :use-type           :finite
;;        :benefit-type       :rival
;;        :rv-max-states      10
;;        :downscaling-factor 8
;;        :keep ('recreationService:RecreationalAttractiveness 'recreationService:PotentialRecreationalUsers 
;;               'recreationService:RecreationalUserFlow       'recreationService:RecreationalUse
;;               'recreationService:ActualRecreationalUsers    'recreationService:TransportationRestrictedRecreationalUse
;;               'recreationService:TransportationRestrictedRecreationalUsers)
;;        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
;;        :context (source-birding use-simple sink)))

;; ----------------------------------------------------------------------------------------------
;; scenarios (evolving)
;; observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario cap-water-augmentation 'sanPedro:CAPWaterAugmentation
      ;;sanPedro:CAPWaterAugmentationHalfMeterRise
      ;;sanPedro:CAPWaterAugmentationAllPerennial
      
;;(defscenario urban-growth 'sanPedro:UrbanGrowth
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development 'sanPedro:BSRDevelopment
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5
