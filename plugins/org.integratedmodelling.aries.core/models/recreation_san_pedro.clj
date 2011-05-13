(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel model measurement classification categorization
                            namespace-ontology ranking numeric-coding binary-coding 
                            probabilistic-measurement probabilistic-classification probabilistic-ranking
                            identification bayesian no-data? count))
  (:refer aries :only (span)))

(namespace-ontology recreationService)

;; ----------------------------------------------------------------------------------------------
;;  Birding source model
;; ----------------------------------------------------------------------------------------------

(defmodel bird-richness BirdSpeciesRichness
  (classification (ranking habitat:AvianRichness)
                  [8 10 :inclusive]       VeryHighBirdSpeciesRichness
                  [6 8]                   HighBirdSpeciesRichness
                  [4 6]                   ModerateBirdSpeciesRichness
                  [0 4]                   LowBirdSpeciesRichness))

(defmodel bird-rarity RareCharismaticBirdHabitat
  (classification (ranking habitat:RareBirdHabitat)
                 #{4 5}          HighRareCharismaticBirdHabitat
                 #{1 2 3}        ModerateRareCharismaticBirdHabitat
                  0              LowRareCharismaticBirdHabitat)) 

(defmodel bird-rarity2 RareCharismaticBirdHabitat
  (classification (ranking habitat:RareBirdHabitat)
                 [4 5 :inclusive]   HighRareCharismaticBirdHabitat
                 [1 4]              ModerateRareCharismaticBirdHabitat
                  0                 LowRareCharismaticBirdHabitat)) 

(defmodel bird-rarity3 RareCharismaticBirdHabitat
  (classification RareCharismaticBirdHabitat
                  :context ((ranking habitat:RareBirdHabitat :as rbh))
                  :state #(cond (contains? #{4 5} (:rbh %))   (tl/conc 'recreationService:HighRareCharismaticBirdHabitat)
                                (contains? #{1 2 3} (:rbh %)) (tl/conc 'recreationService:ModerateRareCharismaticBirdHabitat)
                                :otherwise                    (tl/conc 'recreationService:LowRareCharismaticBirdHabitat))))

;;Riparian zones as important to birding and hunting because of their importance to valued animal species
;; and for human preferences to recreate in riparian areas in the desert.
;;Hydrography-simple currently covers only the Upper San Pedro, so need to get full
;; layer from Bill Kepner.  Check syntax on the statements with Gary.
(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel springs waterSupplyService:Springs
  (binary-coding waterSupplyService:Springs))

(defmodel water-presence sanPedro:WaterPresence
  (binary-coding sanPedro:WaterPresence
                 :context (streams :as streams springs :as springs) 
                 :state #(or (:streams %)
                             (:springs %))))

;; THIS IS THE CODE FROM THE AESTHETIC PROXIMITY MODEL AND SHOULD BE USED HERE AS WELL, PENDING REVIEW W GARY
;;This model assumes that all riparian areas that are not mapped within the SPRNCA are low quality.  This is a poor assumption -
;; moderate quality might also be appropriate and it would be better to run these as a simple BN for presence and quality like
;; the housing presence and value BNs, incoprorating priors for quality when we lack data.
(defmodel riparian-wetland sanPedro:RiparianSpringWetlandQuality
  (classification sanPedro:RiparianSpringWetlandQuality
                  :context (water-presence :as water-presence
                                           (ranking sanPedro:RiparianConditionClass :as condition))
                  :state   #(if (nil? (:water-presence %))
                              (tl/conc 'sanPedro:RiparianSpringWetlandAbsent)
                              (cond (= (:condition %) 3) (tl/conc 'sanPedro:HighQualityRiparianSpringWetland)
                                    (= (:condition %) 2) (tl/conc 'sanPedro:ModerateQualityRiparianSpringWetland)
                                    (= (:condition %) 1) (tl/conc 'sanPedro:LowQualityRiparianSpringWetland)
                                    :otherwise           (tl/conc 'sanPedro:LowQualityRiparianSpringWetland)))))

;;No public access includes private land, Native American reservations, military land.
;; Accessible public land includes state trust land, BLM, Forest Service, NPS, FWS, etc.
;; Ft. Huachucha currently is accessible to the public and birdwatching and hunting occur on-base
(defmodel public-lands PublicAccessClass
  (classification (numeric-coding habitat:LandOwnership)
                  #{2 3 4 8 12 13 14 15 16 36 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 61 62 63 64 65 66 67 
                    68 69 70 71 73 75 76 82 83 86 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
                    117 118 119 120 121 122 123 124 125 126 127}   PublicLand
                  #{1 5 6 11 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 72 74 77 
                    78 79 84 87 88 89 90}                          NoPublicAccess))

;;Undiscretization statements to be use in BNs
(defmodel birding-quality SiteBirdingQuality
  (probabilistic-ranking SiteBirdingQuality
                         [0 10]   VeryLowBirdingQuality 
                         [10 33]  LowBirdingQuality 
                         [33 67]  ModerateBirdingQuality 
                         [67 100] HighBirdingQuality))

;; Bayesian source models
(defmodel source-birding BirdingSourceValue   
  (bayesian BirdingSourceValue                
            :import   "aries.core::RecreationBirdingSourceSanPedro.xdsl"
            :keep     (SiteBirdingQuality)
            :result   birding-quality
            :context  (bird-richness bird-rarity riparian-wetland public-lands)))

;; ----------------------------------------------------------------------------------------------
;;  Wildlife viewing source model
;; ----------------------------------------------------------------------------------------------

(defmodel wildlife-species-richness WildlifeSpeciesRichness
  (ranking WildlifeSpeciesRichness
           :context ((ranking habitat:AvianRichness      :as bird-species-richness)
                     (ranking habitat:MammalRichness     :as mammal-species-richness)
                     (ranking habitat:ReptileRichness    :as reptile-species-richness)
                     (ranking habitat:AmphibianRichness  :as amphibian-species-richness))
           :state   #(Math/round (* (+ (or (:bird-species-richness %) 0)  
                                       (or (:mammal-species-richness %) 0) 
                                       (or (:reptile-species-richness %) 0) 
                                       (or (:amphibian-species-richness %) 0)) 
                                    0.25))))
;;#(let [b (:bird-species-richness %)                     alternative code for the above state statement
;;                      m (:mammal-species-richness %) 
;;                      r (:reptile-species-richness %)
;;                      a (:amphibian-species-richness %)] 
;;                  (Math/round (* 0.25 (reduce + 0 (remove nil? [b m r a])))))))
(defmodel wildlife-species-richness-class WildlifeSpeciesRichnessClass
  (classification wildlife-species-richness
                  [8 10] VeryHighWildlifeSpeciesRichness
                  [6 8]  HighWildlifeSpeciesRichness
                  [4 6]  ModerateWildlifeSpeciesRichness
                  [0 4]  LowWildlifeSpeciesRichness)) 

(defmodel wildlife-quality SiteWildlifeQuality
  (probabilistic-ranking SiteWildlifeQuality
                         [0 10]   VeryLowWildlifeQuality 
                         [10 33]  LowWildlifeQuality 
                         [33 67]  ModerateWildlifeQuality 
                         [67 100] HighWildlifeQuality))

(defmodel source-wildlife WildlifeViewingSourceValue
  (bayesian WildlifeViewingSourceValue
            :import   "aries.core::RecreationWildlifeSourceSanPedro.xdsl"
            :keep     (SiteWildlifeQuality)
            :result   wildlife-quality
            :context  (riparian-wetland public-lands wildlife-species-richness-class)))

;; ----------------------------------------------------------------------------------------------
;;  Hunting source model
;; ----------------------------------------------------------------------------------------------

(defmodel dove-habitat sanPedro:DoveHabitat
  (classification sanPedro:DoveHabitat
                  :context ((numeric-coding sanPedro:MourningDoveHabitat    :as mourning-dove)
                            (numeric-coding sanPedro:WhiteWingedDoveHabitat :as white-winged-dove))
                  :state   #(let [num-dove (+ (if (no-data? (:mourning-dove %)) 0 1)
                                              (if (no-data? (:white-winged-dove %)) 0 1))]
                              (cond (= num-dove 2) (tl/conc 'sanPedro:MultipleDoveSpeciesPresent)
                                    (= num-dove 1) (tl/conc 'sanPedro:SingleDoveSpeciesPresent)
                                    :otherwise     (tl/conc 'sanPedro:DoveSpeciesAbsent)))))

(defmodel deer-habitat sanPedro:DeerHabitat
  (classification sanPedro:DeerHabitat
                  :context ((numeric-coding sanPedro:MuleDeerHabitat      :as mule-deer)
                            (numeric-coding sanPedro:WhiteTailDeerHabitat :as white-tail-deer))
                  :state   #(let [num-deer (+ (if (no-data? (:mule-deer %)) 0 1)
                                              (if (no-data? (:white-tail-deer %)) 0 1))]
                              (cond (= num-deer 2) (tl/conc 'sanPedro:MultipleDeerSpeciesPresent)
                                    (= num-deer 1) (tl/conc 'sanPedro:SingleDeerSpeciesPresent)
                                    :otherwise     (tl/conc 'sanPedro:DeerSpeciesAbsent)))))

(defmodel quail-habitat sanPedro:QuailHabitat
  (classification sanPedro:QuailHabitat
                  :context ((numeric-coding sanPedro:ScaledQuailHabitat  :as scaled-quail)
                            (numeric-coding sanPedro:MearnsQuailHabitat  :as mearns-quail)
                            (numeric-coding sanPedro:GambelsQuailHabitat :as gambels-quail))
                  :state   #(let [num-quail (+ (if (no-data? (:scaled-quail %)) 0 1)
                                               (if (no-data? (:mearns-quail %)) 0 1)
                                               (if (no-data? (:gambels-quail %)) 0 1))]
                              (cond (> num-quail 1) (tl/conc 'sanPedro:MultipleQuailSpeciesPresent)
                                    (= num-quail 1) (tl/conc 'sanPedro:SingleQuailSpeciesPresent)
                                    :otherwise      (tl/conc 'sanPedro:QuailSpeciesAbsent)))))

(defmodel javelina-habitat sanPedro:JavelinaHabitat
  (classification (numeric-coding sanPedro:JavelinaHabitat)
                  29            sanPedro:JavelinaHabitatPresent
                  :otherwise    sanPedro:JavelinaHabitatAbsent))

;;TRANSITION ZONES?  CONVERT RASTER LULC TO VECTOR & BUFFER.  WHAT TRANSITION ZONES MATTER TO WHAT SPECIES
;; IS IMPORTANT.  ASK BILL K & KEN BOYKIN ABOUT IMPORTANT HABITAT TYPES/TRANSITION ZONES FOR OUR 8 SPP. OF 
;; INTEREST.

;;ELEVATION & COOLNESS MAY MATTER LESS FOR HUNTING IF THE SEASON IS IN LATE FALL/WINTER.  CHECK THE REST OF THE
;; HUNTING SEASONS INFO.

;;(defmodel elevation (high elevation areas may be more attractive for recreation, especially as an escape from 
;;  summer heat).  People do recreate in the mountains but this might already be accounted for by presense & biodiversity?
;;(defmodel mountain aestheticService:Mountain
;;  (classification (measurement geophysics:Altitude "m")
;;                  [1400 1800]  aestheticService:SmallMountain  
;;                  [1800 8850]  aestheticService:LargeMountain
;;                  :otherwise   aestheticService:NoMountain))

;;VIEW QUALITY AS IMPORTANT, AT LEAST FOR SOME RECREATIONAL TYPES.
;; IF YOU HAD SOURCE MODELS FOR HUNTING, BIRDING, VIEW QUALITY YOU COULD COMBINE THE 3 BASED ON PREFERENCES
;; FOR MULTI-ACTIVITY/MULTI-GOAL RECREATION (SUMMED, WEIGHTED UTILITY FUNCTION)

;;Undiscretization statements to be use in BNs
(defmodel deer-hunting-quality SiteDeerHuntingQuality
  (probabilistic-ranking SiteDeerHuntingQuality
                         [0 5]    VeryLowDeerHuntingQuality 
                         [5 33]   LowDeerHuntingQuality 
                         [33 67]  ModerateDeerHuntingQuality 
                         [67 100] HighDeerHuntingQuality))

(defmodel javelina-hunting-quality SiteJavelinaHuntingQuality
  (probabilistic-ranking SiteJavelinaHuntingQuality
                         [0 5]    VeryLowJavelinaHuntingQuality 
                         [5 33]   LowJavelinaHuntingQuality 
                         [33 67]  ModerateJavelinaHuntingQuality 
                         [67 100] HighJavelinaHuntingQuality))

(defmodel dove-hunting-quality SiteDoveHuntingQuality
  (probabilistic-ranking SiteDoveHuntingQuality
                         [0 5]    VeryLowDoveHuntingQuality 
                         [5 33]   LowDoveHuntingQuality 
                         [33 67]  ModerateDoveHuntingQuality 
                         [67 100] HighDoveHuntingQuality))

(defmodel quail-hunting-quality SiteQuailHuntingQuality
  (probabilistic-ranking SiteQuailHuntingQuality
                         [0 5]    VeryLowQuailHuntingQuality 
                         [5 33]   LowQuailHuntingQuality 
                         [33 67]  ModerateQuailHuntingQuality 
                         [67 100] HighQuailHuntingQuality))

;; Bayesian source models
(defmodel source-deer-hunting DeerHuntingSourceValue
  (bayesian DeerHuntingSourceValue                
            :import   "aries.core::RecreationHuntingDeerSourceSanPedro.xdsl"
            :keep     (SiteDeerHuntingQuality)
            :result   deer-hunting-quality
            :context  (riparian-wetland public-lands deer-habitat)))

(defmodel source-javelina-hunting JavelinaHuntingSourceValue  
  (bayesian JavelinaHuntingSourceValue                
            :import   "aries.core::RecreationHuntingJavelinaSourceSanPedro.xdsl"
            :keep     (SiteJavelinaHuntingQuality)
            :result   javelina-hunting-quality
            :context  (riparian-wetland public-lands javelina-habitat)))

(defmodel source-dove-hunting DoveHuntingSourceValue
  (bayesian DoveHuntingSourceValue
            :import   "aries.core::RecreationHuntingDoveSourceSanPedro.xdsl"
            :keep     (SiteDoveHuntingQuality)
            :result   dove-hunting-quality
            :context  (riparian-wetland public-lands dove-habitat)))

(defmodel source-quail-hunting QuailHuntingSourceValue
  (bayesian QuailHuntingSourceValue                
            :import   "aries.core::RecreationHuntingQuailSourceSanPedro.xdsl"
            :keep     (SiteQuailHuntingQuality)
            :result   quail-hunting-quality
            :context  (riparian-wetland public-lands quail-habitat)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel population-density policytarget:PopulationDensity
  (count policytarget:PopulationDensity "/km^2"))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;(defmodel travel-capacity RoadTravelCapacity
;; This is a categorization; values are "interstates", "highways", "arterials", "streets", 
;; "primitive roads", "trails & alleys", "access ramps".
;; This is a massive layer - it may be really tough for ARIES to rasterize every time, while also time
;;   consuming for me to do so...

(defmodel roads Roads                   
  (classification (binary-coding infrastructure:Road)
                  1          RoadsPresent
                  :otherwise RoadsAbsent))

;;SPRNCA trails: need to expand bounding box so it doesn't throw errors.
;;(defmodel trails Trails 
;;  (classification (binary-coding infrastructure:Path)
;;                  1          TrailPresent
;;                  :otherwise TrailAbsent))

;;How to account for congestion and visitor response to it?  See Hunt et al. 2005, Hunt 2008 on modeling
;; visitor choice between alternative recreational sites.
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;;Identifications for recreation: flow models are not yet ready but concepts will be exported to NetCDF.
(defmodel recreation-data OutdoorRecreation
  (identification OutdoorRecreation
                  :context (source-birding
                            source-wildlife
                            source-deer-hunting
                            source-quail-hunting
                            source-dove-hunting
                            source-javelina-hunting
                            population-density
                            roads))) ;;add trails data once its bounding box has been adjusted

;;SPAN statements written but recreational flow models are not yet developed.
;;(defmodel recreation-flow-birding BirdingUse
;;  (span BirdingAccessAndUse
;;        BirdingSourceValue
;;        BirdingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-birding population-density roads))) ;;replace with final use concept

;;(defmodel recreation-flow-wildlife WildlifeViewingUse
;;  (span WildlifeViewingAccessAndUse
;;        WildlifeViewingSourceValue
;;        WildlifeViewingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;       :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;              ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-wildlife population-density roads))) ;;replace with final use concept

;;(defmodel recreation-flow-javelina-hunting JavelinaHuntingUse
;;  (span JavelinaHuntingAccessAndUse
;;        JavelinaHuntingSourceValue
;;        JavelinaHuntingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-javelina-hunting population-density roads))) ;;replace with final use concept

;;(defmodel recreation-flow-dove-hunting DoveHuntingUse
;;  (span DoveHuntingAccessAndUse
;;        DoveHuntingSourceValue
;;        DoveHuntingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-dove-hunting population-density roads))) ;;replace with final use concept

;;(defmodel recreation-flow-quail-hunting QuailHuntingUse
;;  (span QuailHuntingAccessAndUse
;;        QuailHuntingSourceValue
;;        QuailHuntingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-quail-hunting population-density roads))) ;;replace with final use concept

;;(defmodel recreation-flow-deer-hunting DeerHuntingUse
;; (span DeerHuntingAccessAndUse
;;        DeerHuntingSourceValue
;;        DeerHuntingDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-deer-hunting population-density roads))) ;;replace with final use concept

;; ----------------------------------------------------------------------------------------------
;; Scenarios 

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defscenario cap-water-augmentation-half-meter-rise
"Water augmentation leading to a 0.5 meter rise across the San Pedro Riparian National Conservation Area" 
  (model sanPedro:RiparianConditionClass
         (ranking sanPedro:RiparianConditionClassHalfMeterRise)))

(defscenario cap-water-augmentation-all-perennial
"Water augmentation leading to perennial flow conditions across the San Pedro Riparian National Conservation Area" 
  (model sanPedro:RiparianConditionClass
         (ranking sanPedro:RiparianConditionClassAllWet)))

;;(defscenario urban-growth sanPedro:UrbanGrowth (add new users, up 10.4% in constrained, 56.8% in open)
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      ;;sanPedro:Steinitz30ClassUrbanGrowthLULCOpen
      ;;sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained