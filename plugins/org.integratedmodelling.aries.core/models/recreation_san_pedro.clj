(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization
                            namespace-ontology ranking numeric-coding binary-coding 
                            probabilistic-measurement probabilistic-classification probabilistic-ranking
                            identification bayesian count))
  (:refer aries :only (span)))

(namespace-ontology recreationService)

;; ----------------------------------------------------------------------------------------------
;;  Birding source model
;; ----------------------------------------------------------------------------------------------

(defmodel bird-richness BirdSpeciesRichness
   (classification (ranking habitat:AvianRichness)
       [8 10]       VeryHighBirdSpeciesRichness
       [6 8]        HighBirdSpeciesRichness
       [4 6]        ModerateBirdSpeciesRichness
       [0 4]        LowBirdSpeciesRichness))

(defmodel bird-rarity RareCharismaticBirdHabitat
    (classification (ranking habitat:RareBirdHabitat)
      #{4 5}   HighRareCharismaticBirdHabitat
      #{1 2 3} ModerateRareCharismaticBirdHabitat
      0        LowRareCharismaticBirdHabitat)) 

;;Riparian zones as important to birding and hunting because of their importance to valued animal species
;; and for human preferences to recreate in riparian areas in the desert.
;;Hydrography-simple currently covers only the Upper San Pedro, so need to get full
;; layer from Bill Kepner.  Check syntax on the statements with Gary.
(defmodel streams geofeatures:River  ;;A simplified hydrography layer: NHD is too much here
  (binary-coding geofeatures:River))
(defmodel springs waterSupplyService:Springs
  (binary-coding waterSupplyService:Springs))
(defmodel water-presence sanPedro:WaterPresence
  (binary-coding sanPedro:WaterPresence
    :context (streams :as streams springs :as springs) 
    :state #(+ (:streams %)
               (:springs %))))
(defmodel riparian-wetland-code sanPedro:RiparianAndWetlandCode
  (numeric-coding sanPedro:RiparianAndWetlandCode
                  :context ((binary-coding sanPedro:WaterPresence :as water-presence)
                            (ranking sanPedro:RiparianConditionClass :as condition))
                  :state   #(cond (and (== (:condition %) 1) ;;NEED TO SET AS "OTHERWISE" also.
                                       (contains? #{1} (:water-presence %)))
                                  1 ;;sanPedro:LowQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 2)
                                       (contains? #{1} (:water-presence %)))
                                  2 ;;sanPedro:ModerateQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 3)
                                       (contains? #{1} (:water-presence %)))
                                  3 ;;sanPedro:HighQualityRiparianOrWetlandPresent

                                  :otherwise 0 ;;sanPedro:NoRiparianOrWetlandPresentHig
                                  )))
(defmodel riparian-wetland sanPedro:RiparianSpringWetlandQuality
  (classification riparian-wetland-code
                  3 sanPedro:HighQualityRiparianSpringWetland
                  2 sanPedro:ModerateQualityRiparianSpringWetland
                  1 sanPedro:LowQualityRiparianSpringWetland
                  0 sanPedro:RiparianSpringWetlandAbsent))

;; THIS IS THE CODE FROM THE AESTHETIC PROXIMITY MODEL AND SHOULD BE USED HERE AS WELL, PENDING REVIEW W GARY
;;This model assumes that all riparian areas that are not mapped within the SPRNCA are low quality.  This is a poor assumption -
;; moderate quality might also be appropriate and it would be better to run these as a simple BN for presence and quality like
;; the housing presence and value BNs, incoprorating priors for quality when we lack data.
;;(defmodel riparian-wetland-code sanPedro:RiparianAndWetlandCode
;;  (numeric-coding sanPedro:RiparianAndWetlandCode
;;                  :context ((numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC :as lulc)
;;                            (ranking sanPedro:RiparianConditionClass :as condition))
;;                  :state   #(if (contains? #{77.0 78.0 79.0 80.0 81.0 83.0 84.0 85.0 98.0 109.0 110.0 118.0} (:lulc %))
;;                                (let [condition (:condition %)]
;;                                  (if (or (nil? condition) (Double/isNaN condition))
;;                                    1
;;                                    condition))
;;                                0)))

;;(defmodel riparian-wetland-code sanPedro:RiparianAndWetlandCode
;;  (numeric-coding sanPedro:RiparianAndWetlandCode
;;                  :context ((numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC :as lulc)
;;                            (ranking sanPedro:RiparianConditionClass :as condition))
;;                  :state   #(cond (and (== (:condition %) 1)
;;                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
;;                                  1 ;;sanPedro:LowQualityRiparianOrWetlandPresent

;;                                  (and (== (:condition %) 2)
;;                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
;;                                  2 ;;sanPedro:ModerateQualityRiparianOrWetlandPresent

;;                                  (and (== (:condition %) 3)
;;                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110} (:lulc %)))
;;                                 3 ;;sanPedro:HighQualityRiparianOrWetlandPresent

;;                                  :otherwise 0 ;;sanPedro:NoRiparianOrWetlandPresentHig
;;                                  )))
;;(defmodel riparian-wetland sanPedro:RiparianAndWetland
;;  (classification riparian-wetland-code
;;                  3 sanPedro:HighQualityRiparianOrWetlandPresent
;;                  2 sanPedro:ModerateQualityRiparianOrWetlandPresent
;;                  1 sanPedro:LowQualityRiparianOrWetlandPresent
;;                  0 sanPedro:RiparianOrWetlandAbsent))

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

;;Check syntax with Gary: stuck commeting this out for now as its throwing errors.
;;(defmodel wildlife-richness :WildlifeSpeciesRichness
;;    (classification (ranking :WildlifeSpeciesRichness)
;;      :context ((ranking habitat:AvianRichness      :as bird-species-richness)
;;                (ranking habitat:MammalRichness     :as mammal-species-richness)
;;                (ranking habitat:ReptileRichness    :as reptile-species-richness)
;;                (ranking habitat:AmphibianRichness  :as amphibian-species-richness))
;;      :state   #(* (+ (bird-species-richness mammal-species-richness reptile-species-richness amphibian-species-richness)) 0.25))) 
;;(defmodel wildlife-species-richness WildlifeSpeciesRichnessClass
;;    (classification (ranking WildlifeSpeciesRichness)
;;      [8 10] VeryHighWildlifeSpeciesRichness
;;      [6 8]  HighWildlifeSpeciesRichness
;;      [4 6]  ModerateWildlifeSpeciesRichness
;;      [0 4]  LowWildlifeSpeciesRichness)) 

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
            :context  (riparian-wetland public-lands))) ;;add wildlife-richness when statement's OK'd

;; ----------------------------------------------------------------------------------------------
;;  Hunting source model
;; ----------------------------------------------------------------------------------------------

(defmodel dove-habitat-code sanPedro:DoveHabitatCode
   (numeric-coding sanPedro:DoveHabitatCode
        :context ((numeric-coding sanPedro:MourningDoveHabitat     :as mourning-dove)
                  (numeric-coding sanPedro:WhiteWingedDoveHabitat  :as white-winged-dove)) 
       :state    #(let [num-dove (Math/ceil (/ (+ (:mourning-dove  %)
                                                   (:white-winged-dove %)) 
                                              29.0))]
                        num-dove)))
(defmodel dove-habitat sanPedro:DoveHabitat
    (classification dove-habitat-code
             2 sanPedro:MultipleDoveSpeciesPresent
             1 sanPedro:SingleDoveSpeciesPresent
             0 sanPedro:DoveSpeciesAbsent))

(defmodel deer-habitat-code sanPedro:DeerHabitatCode
   (numeric-coding sanPedro:DeerHabitatCode
        :context ((numeric-coding sanPedro:MuleDeerHabitat       :as mule-deer)
                  (numeric-coding sanPedro:WhiteTailDeerHabitat  :as white-tail-deer)) 
        :state    #(let [num-deer (Math/ceil (/ (+ (:mule-deer  %)
                                                   (:white-tail-deer %)) 
                                              29.0))]
                        num-deer)))
(defmodel deer-habitat sanPedro:DeerHabitat
    (classification deer-habitat-code
             2 sanPedro:MultipleDeerSpeciesPresent
             1 sanPedro:SingleDeerSpeciesPresent
             0 sanPedro:DeerSpeciesAbsent))

(defmodel quail-habitat-code sanPedro:QuailHabitatCode
   (numeric-coding sanPedro:QuailHabitatCode
        :context ((numeric-coding sanPedro:ScaledQuailHabitat  :as scaled-quail)
                  (numeric-coding sanPedro:MearnsQuailHabitat  :as mearns-quail) 
                  (numeric-coding sanPedro:GambelsQuailHabitat :as gambels-quail)) 
        :state    #(let [num-quail (Math/floor (/ (+ (:scaled-quail %)
                                                     (:mearns-quail %)
                                                     (:gambels-quail %)) 
                                                29.0))]
                      (min num-quail 2)))) 
(defmodel quail-habitat sanPedro:QuailHabitat
    (classification quail-habitat-code
             2 sanPedro:MultipleQuailSpeciesPresent
             1 sanPedro:SingleQuailSpeciesPresent
             0 sanPedro:QuailSpeciesAbsent))

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
;;  summer heat).  People do recreate in teh mountains but this might already be accounted for by presense & biodiversity?
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
            source-deer-hunting       
            source-quail-hunting 
            source-dove-hunting 
            source-javelina-hunting 
            population-density
            roads)))                ;;add trails data once its bounding box has been adjusted

;; the real enchilada
;;(defmodel recreation-flow carbonService:ClimateStability
;;  (span carbonService:CO2Removed
;;        BirdingSourceValue   ;;and corresponding hunting/wildlife viewing models 
;;        carbonService:GreenhouseGasEmissions
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
;;        :keep (RecreationalAttractiveness PotentialRecreationalUsers 
;;               RecreationalUserFlow       RecreationalUse
;;               ActualRecreationalUsers    TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
;;        :context (source-birding use-simple sink)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios 

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario cap-water-augmentation sanPedro:CAPWaterAugmentation (change RiparianWetlandQuality)
      ;;sanPedro:CAPWaterAugmentationHalfMeterRise
      ;;sanPedro:CAPWaterAugmentationAllPerennial
      
;;(defscenario urban-growth sanPedro:UrbanGrowth (add new users, up 10.4% in constrained, 56.8% in open)
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development sanPedro:BSRDevelopment (add new users, up 3.6% in BSR scenarios)
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5
