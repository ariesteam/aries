;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; Recreation model for San Pedro
;;;
;;; Valid Contexts: core.contexts.beta/san_pedro_us*
;;;
;;;-------------------------------------------------------------------

(ns core.models.recreation-san-pedro
  (:refer-clojure :rename {count length})
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel model measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking identification
                           bayesian no-data? count])
  (:refer aries :only [span]))

(namespace-ontology recreationService)

;;;-------------------------------------------------------------------
;;; Birding source models
;;;-------------------------------------------------------------------

(defmodel bird-richness BirdSpeciesRichness
  (classification (ranking habitat:AvianRichness)
    [8 10 :inclusive] VeryHighBirdSpeciesRichness
    [6  8]            HighBirdSpeciesRichness
    [4  6]            ModerateBirdSpeciesRichness
    [0  4]            LowBirdSpeciesRichness))

(defmodel bird-rarity RareCharismaticBirdHabitat
  (classification (ranking habitat:RareBirdHabitat)
    #{4 5}   HighRareCharismaticBirdHabitat
    #{1 2 3} ModerateRareCharismaticBirdHabitat
    0        LowRareCharismaticBirdHabitat))

;; Riparian zones as important to birding and hunting because of their
;; importance to valued animal species and for human preferences to
;; recreate in riparian areas in the desert.
(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel springs waterSupplyService:Springs
  (binary-coding waterSupplyService:Springs))

(defmodel water-presence sanPedro:WaterPresence
  (binary-coding sanPedro:WaterPresence
    :context [streams springs]
    :state   #(or (:river %)
                  (:springs %))))

;; This model assumes that all riparian areas that are not mapped
;; within the SPRNCA are low quality.  This is an iffy assumption -
;; moderate quality might also be appropriate and it might be better
;; to run these as a simple BN for presence and quality like the
;; housing presence and value BNs, incoprorating priors for quality
;; when we lack data.
(defmodel condition-class sanPedro:RiparianConditionClass
  (ranking sanPedro:RiparianConditionClass))

(defmodel riparian-wetland sanPedro:RiparianSpringWetlandQuality
  (classification sanPedro:RiparianSpringWetlandQuality
    :context [water-presence condition-class]
    :state   #(if (nil? (:water-presence %))
                (tl/conc 'sanPedro:RiparianSpringWetlandAbsent)
                (cond (= (:riparian-condition-class %) 3) (tl/conc 'sanPedro:HighQualityRiparianSpringWetland)
                      (= (:riparian-condition-class %) 2) (tl/conc 'sanPedro:ModerateQualityRiparianSpringWetland)
                      (= (:riparian-condition-class %) 1) (tl/conc 'sanPedro:LowQualityRiparianSpringWetland)
                      :otherwise                          (tl/conc 'sanPedro:LowQualityRiparianSpringWetland)))))

;; No public access includes private land, Native American
;; reservations, military land.  Accessible public land includes state
;; trust land, BLM, Forest Service, NPS, FWS, etc.  Ft. Huachucha
;; currently is accessible to the public and birdwatching and hunting
;; occur on-base
(defmodel public-lands sanPedro:PublicAccessClass
  (classification (numeric-coding habitat:LandOwnership)
    #{2 3 4 8 12 13 14 15 16 36 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 61 62 63 64 65 66 67 
      68 69 70 71 73 75 76 82 83 86 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
      117 118 119 120 121 122 123 124 125 126 127} sanPedro:PublicLand
      #{1 5 6 11 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 72 74 77 
        78 79 84 87 88 89 90}                      sanPedro:NoPublicAccess))

;; This statement (not yet implemented) accounts for public recognition
;; of a site, e.g., SPRNCA & Ramsy Canyon's high values for birding,
;; or Mt. Mansfield & Camels Hump's high values for hiking in the VT
;; model.  Need to develop a data layer that has these features, link
;; nodes in the BN and finish the CPT, and include the correct
;; concepts in the defmodel statement below, then test results.
;;(defmodel site-recognition SiteRecognitionClass
;;  (classification (ranking SiteRecognition)
;;     1 HighSiteRecognition
;;     2 ModerateSiteRecognition
;;     3 LowSiteRecognition))

;;Undiscretization statements to be use in BNs
(defmodel birding-quality SiteBirdingQuality
  (probabilistic-ranking SiteBirdingQuality
    [67 100] HighBirdingQuality
    [33  67] ModerateBirdingQuality
    [10  33] LowBirdingQuality 
    [ 0  10] VeryLowBirdingQuality))

;; Bayesian source models
(defmodel source-birding BirdingSourceValue   
  (bayesian BirdingSourceValue                
    :import  "aries.core::RecreationSourceSanPedroBirding.xdsl"
    :context [bird-richness bird-rarity riparian-wetland public-lands]
    :keep    [SiteBirdingQuality]
    :result  birding-quality))

;;;-------------------------------------------------------------------
;;; Wildlife viewing source models
;;;-------------------------------------------------------------------

(defmodel wildlife-species-richness WildlifeSpeciesRichness
  (ranking WildlifeSpeciesRichness
    :context [(ranking habitat:AvianRichness)   (ranking habitat:MammalRichness)
              (ranking habitat:ReptileRichness) (ranking habitat:AmphibianRichness)]
    :state   #(Math/round (* (+ (or (:avian-richness %) 0)  
                                (or (:mammal-richness %) 0) 
                                (or (:reptile-richness %) 0) 
                                (or (:amphibian-richness %) 0)) 
                             0.25))))
;;#(let [b (:bird-species-richness %)                     alternative code for the above state statement
;;                      m (:mammal-species-richness %) 
;;                      r (:reptile-species-richness %)
;;                      a (:amphibian-species-richness %)] 
;;                  (Math/round (* 0.25 (reduce + 0 (remove nil? [b m r a])))))))
(defmodel wildlife-species-richness-class WildlifeSpeciesRichnessClass
  (classification wildlife-species-richness
    [8 10] VeryHighWildlifeSpeciesRichness
    [6  8] HighWildlifeSpeciesRichness
    [4  6] ModerateWildlifeSpeciesRichness
    [0  4] LowWildlifeSpeciesRichness))

(defmodel wildlife-quality SiteWildlifeQuality
  (probabilistic-ranking SiteWildlifeQuality
    [67 100] HighWildlifeQuality
    [33  67] ModerateWildlifeQuality
    [10  33] LowWildlifeQuality
    [ 0  10] VeryLowWildlifeQuality))

(defmodel source-wildlife WildlifeViewingSourceValue
  (bayesian WildlifeViewingSourceValue
    :import  "aries.core::RecreationSourceSanPedroWildlife.xdsl"
    :context [riparian-wetland public-lands wildlife-species-richness-class]
    :keep    [SiteWildlifeQuality]
    :result  wildlife-quality))

;;;-------------------------------------------------------------------
;;; Hunting source models
;;;-------------------------------------------------------------------

(defmodel dove-habitat sanPedro:DoveHabitat
  (classification sanPedro:DoveHabitat
    :context [(numeric-coding sanPedro:MourningDoveHabitat)
              (numeric-coding sanPedro:WhiteWingedDoveHabitat)]
    :state   #(let [num-dove (+ (if (no-data? (:mourning-dove-habitat %)) 0 1)
                                (if (no-data? (:white-winged-dove-habitat %)) 0 1))]
                (cond (= num-dove 2) (tl/conc 'sanPedro:MultipleDoveSpeciesPresent)
                      (= num-dove 1) (tl/conc 'sanPedro:SingleDoveSpeciesPresent)
                      :otherwise     (tl/conc 'sanPedro:DoveSpeciesAbsent)))))

(defmodel deer-habitat sanPedro:DeerHabitat
  (classification sanPedro:DeerHabitat
    :context [(numeric-coding sanPedro:MuleDeerHabitat)
              (numeric-coding sanPedro:WhiteTailDeerHabitat)]
    :state   #(let [num-deer (+ (if (no-data? (:mule-deer-habitat %)) 0 1)
                                (if (no-data? (:white-tail-deer-habitat %)) 0 1))]
                (cond (= num-deer 2) (tl/conc 'sanPedro:MultipleDeerSpeciesPresent)
                      (= num-deer 1) (tl/conc 'sanPedro:SingleDeerSpeciesPresent)
                      :otherwise     (tl/conc 'sanPedro:DeerSpeciesAbsent)))))

(defmodel quail-habitat sanPedro:QuailHabitat
  (classification sanPedro:QuailHabitat
    :context [(numeric-coding sanPedro:ScaledQuailHabitat)
              (numeric-coding sanPedro:MearnsQuailHabitat)
              (numeric-coding sanPedro:GambelsQuailHabitat)]
    :state   #(let [num-quail (+ (if (no-data? (:scaled-quail-habitat %)) 0 1)
                                 (if (no-data? (:mearns-quail-habitat %)) 0 1)
                                 (if (no-data? (:gambels-quail-habitat %)) 0 1))]
                (cond (> num-quail 1) (tl/conc 'sanPedro:MultipleQuailSpeciesPresent)
                      (= num-quail 1) (tl/conc 'sanPedro:SingleQuailSpeciesPresent)
                      :otherwise      (tl/conc 'sanPedro:QuailSpeciesAbsent)))))

(defmodel javelina-habitat sanPedro:JavelinaHabitat
  (classification (numeric-coding sanPedro:JavelinaHabitat)
    29            sanPedro:JavelinaHabitatPresent
    nil           sanPedro:JavelinaHabitatAbsent
    :otherwise    sanPedro:JavelinaHabitatAbsent))

;; TRANSITION ZONES?  CONVERT RASTER LULC TO VECTOR & BUFFER.  WHAT
;; TRANSITION ZONES MATTER TO WHAT SPECIES IS IMPORTANT. COULD ASK BILL K &
;; KEN BOYKIN ABOUT IMPORTANT HABITAT TYPES/TRANSITION ZONES FOR OUR 8
;; SPP. OF INTEREST.

;; ELEVATION & COOLNESS MAY MATTER LESS FOR HUNTING IF THE SEASON IS IN
;; LATE FALL/WINTER.  CHECK THE REST OF THE HUNTING SEASONS INFO.

;;(defmodel elevation (high elevation areas may be more attractive for
;;  recreation, especially as an escape from summer heat).  People do
;;  recreate in the mountains but this might already be accounted for
;;  by presense & biodiversity?
;;(defmodel mountain aestheticService:Mountain
;;  (classification (measurement geophysics:Altitude "m")
;;    [1800 8850] aestheticService:LargeMountain
;;    [1400 1800] aestheticService:SmallMountain                
;;    :otherwise  aestheticService:NoMountain))

;; VIEW QUALITY AS IMPORTANT, AT LEAST FOR SOME RECREATIONAL TYPES.  IF
;; YOU HAD SOURCE MODELS FOR HUNTING, BIRDING, VIEW QUALITY YOU COULD
;; COMBINE THE 3 BASED ON PREFERENCES FOR MULTI-ACTIVITY/MULTI-GOAL
;; RECREATION (SUMMED, WEIGHTED UTILITY FUNCTION)

;; Undiscretization statements to be use in BNs
(defmodel deer-hunting-quality SiteDeerHuntingQuality
  (probabilistic-ranking SiteDeerHuntingQuality
    [67 100] HighDeerHuntingQuality
    [33  67] ModerateDeerHuntingQuality
    [ 5  33] LowDeerHuntingQuality 
    [ 0   5] VeryLowDeerHuntingQuality))

(defmodel javelina-hunting-quality SiteJavelinaHuntingQuality
  (probabilistic-ranking SiteJavelinaHuntingQuality
    [67 100] HighJavelinaHuntingQuality
    [33  67] ModerateJavelinaHuntingQuality
    [ 5  33] LowJavelinaHuntingQuality
    [ 0   5] VeryLowJavelinaHuntingQuality))

(defmodel dove-hunting-quality SiteDoveHuntingQuality
  (probabilistic-ranking SiteDoveHuntingQuality
    [67 100] HighDoveHuntingQuality
    [33  67] ModerateDoveHuntingQuality
    [ 5  33] LowDoveHuntingQuality
    [ 0   5] VeryLowDoveHuntingQuality))

(defmodel quail-hunting-quality SiteQuailHuntingQuality
  (probabilistic-ranking SiteQuailHuntingQuality
    [67 100] HighQuailHuntingQuality
    [33  67] ModerateQuailHuntingQuality
    [ 5  33] LowQuailHuntingQuality
    [ 0   5] VeryLowQuailHuntingQuality))

;; Bayesian source models
(defmodel source-deer-hunting DeerHuntingSourceValue
  (bayesian DeerHuntingSourceValue                
    :import   "aries.core::RecreationSourceSanPedroDeerHunting.xdsl"
    :context  [riparian-wetland public-lands deer-habitat]
    :keep     [SiteDeerHuntingQuality]
    :result   deer-hunting-quality))

(defmodel source-javelina-hunting JavelinaHuntingSourceValue  
  (bayesian JavelinaHuntingSourceValue                
    :import   "aries.core::RecreationSourceSanPedroJavelinaHunting.xdsl"
    :context  [riparian-wetland public-lands javelina-habitat]
    :keep     [SiteJavelinaHuntingQuality]
    :result   javelina-hunting-quality))

(defmodel source-dove-hunting DoveHuntingSourceValue
  (bayesian DoveHuntingSourceValue
    :import   "aries.core::RecreationSourceSanPedroDoveHunting.xdsl"
    :context  [riparian-wetland public-lands dove-habitat]
    :keep     [SiteDoveHuntingQuality]
    :result   dove-hunting-quality))

(defmodel source-quail-hunting QuailHuntingSourceValue
  (bayesian QuailHuntingSourceValue                
    :import   "aries.core::RecreationSourceSanPedroQuailHunting.xdsl"
    :context  [riparian-wetland public-lands quail-habitat]
    :keep     [SiteQuailHuntingQuality]
    :result   quail-hunting-quality))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel population-density policytarget:PopulationDensity
  (count policytarget:PopulationDensity "/km^2"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

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
;;    1          TrailPresent
;;    :otherwise TrailAbsent))

;; How to account for congestion and visitor response to it?  See Hunt
;; et al. 2005, Hunt 2008 on modeling visitor choice between
;; alternative recreational sites, RUMs, etc.

;;Identifications for recreation: flow models are not yet ready but concepts will be exported to NetCDF.
(defmodel recreation-data OutdoorRecreation
  (identification OutdoorRecreation
    :context [source-birding source-wildlife source-deer-hunting
              source-quail-hunting source-dove-hunting
              source-javelina-hunting population-density
              roads])) ; Add trails data once its bounding box has been adjusted

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;SPAN statements written but recreational flow models are not yet developed.
;;(defmodel recreation-flow-birding BirdingUse
;;  (span BirdingAccessAndUse
;;        BirdingSourceValue
;;        BirdingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-birding population-density roads]
;;        :keep    [RecreationalAttractiveness                  PotentialRecreationalUsers
;;                  RecreationalUserFlow                        RecreationalUse
;;                  ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;                  TransportationRestrictedRecreationalUsers])) ; Replace with final use concept

;;(defmodel recreation-flow-wildlife WildlifeViewingUse
;;  (span WildlifeViewingAccessAndUse
;;        WildlifeViewingSourceValue
;;        WildlifeViewingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-wildlife population-density roads] ; Replace with final use concept
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers]))


;;(defmodel recreation-flow-javelina-hunting JavelinaHuntingUse
;;  (span JavelinaHuntingAccessAndUse
;;        JavelinaHuntingSourceValue
;;        JavelinaHuntingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-javelina-hunting population-density roads]
;;        :keep [RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers])) ; Replace with final use concept

;;(defmodel recreation-flow-dove-hunting DoveHuntingUse
;;  (span DoveHuntingAccessAndUse
;;        DoveHuntingSourceValue
;;        DoveHuntingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-dove-hunting population-density roads]
;;        :keep [RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers])) ; Replace with final use concept

;;(defmodel recreation-flow-quail-hunting QuailHuntingUse
;;  (span QuailHuntingAccessAndUse
;;        QuailHuntingSourceValue
;;        QuailHuntingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-quail-hunting population-density roads]
;;        :keep [RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers])) ; Replace with final use concept

;;(defmodel recreation-flow-deer-hunting DeerHuntingUse
;; (span DeerHuntingAccessAndUse
;;        DeerHuntingSourceValue
;;        DeerHuntingDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
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
;;        :context [source-deer-hunting population-density roads]
;;        :keep [RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers])) ; Replace with final use concept

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

(defscenario cap-water-augmentation-half-meter-rise
  "Water augmentation leading to a 0.5 meter rise across the San Pedro Riparian National Conservation Area" 
  (model sanPedro:RiparianConditionClass
    (ranking sanPedro:RiparianConditionClassHalfMeterRise)))

(defscenario cap-water-augmentation-all-perennial
  "Water augmentation leading to perennial flow conditions across the San Pedro Riparian National Conservation Area" 
  (model sanPedro:RiparianConditionClass
    (ranking sanPedro:RiparianConditionClassAllWet)))

(defmodel constrained-development-scenario sanPedro:ConstrainedDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained) 
    #{10 11 12 13 19 22 25}                sanPedro:DevelopedConstrained
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28} sanPedro:NotDevelopedConstrained))

(defmodel open-development-scenario sanPedro:OpenDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen) 
    #{10 11 12 13 19 22 25}                   sanPedro:DevelopedOpen
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28 29} sanPedro:NotDevelopedOpen))

(defscenario constrained-development-recreation
  "Changes values in developed areas to very private land and locally appropriate low values for habitat and species richness."
  (model BirdSpeciesRichness
    (classification BirdSpeciesRichness
      :context [bird-richness :as br constrained-development-scenario :as cd]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'recreationService:LowBirdSpeciesRichness)
                  (:br %))))
  (model RareCharismaticBirdHabitat
    (classification RareCharismaticBirdHabitat      
      :context [bird-rarity :as br constrained-development-scenario :as cd]
      :state #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                (conc 'recreationService:LowRareCharismaticBirdHabitat)    
                (:br %))))
  (model sanPedro:PublicAccessClass
    (classification sanPedro:PublicAccessClass
      :context [public-lands :as pl constrained-development-scenario :as cd]
      :state #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                (conc 'sanPedro:NoPublicAccess)    
                (:pl %))))
  (model WildlifeSpeciesRichnessClass
    (classification WildlifeSpeciesRichnessClass
      :context [constrained-development-scenario :as cd wildlife-species-richness-class :as wl]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'recreationService:LowWildlifeSpeciesRichness)    
                  (:wl %))))
  (model sanPedro:DoveHabitat
    (classification sanPedro:DoveHabitat
      :context [constrained-development-scenario :as cd dove-habitat :as dh]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:SingleDoveSpeciesPresent)
                  (:dh %))))
  (model sanPedro:DeerHabitat
    (classification sanPedro:DeerHabitat
      :context [constrained-development-scenario :as cd deer-habitat :as dh]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:DeerSpeciesAbsent)    
                  (:dh %))))
  (model sanPedro:QuailHabitat
    (classification sanPedro:QuailHabitat
      :context [constrained-development-scenario :as cd quail-habitat :as qh]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:QuailSpeciesAbsent)    
                  (:qh %))))
  (model sanPedro:JavelinaHabitat
    (classification sanPedro:JavelinaHabitat
      :context [constrained-development-scenario :as cd javelina-habitat :as jh]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:JavelinaHabitatAbsent)    
                  (:jh %)))))

(defscenario open-development-recreation
  "Changes values in developed areas to very private land and locally appropriate low values for habitat and species richness."
  (model BirdSpeciesRichness
    (classification BirdSpeciesRichness
      :context [open-development-scenario :as od bird-richness :as br]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'recreationService:LowBirdSpeciesRichness)
                  (:br %))))
  (model RareCharismaticBirdHabitat
    (classification RareCharismaticBirdHabitat
      :context [open-development-scenario :as od bird-rarity :as br]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'recreationService:LowRareCharismaticBirdHabitat)    
                  (:br %))))
  (model PublicAccessClass
    (classification PublicAccessClass
      :context [open-development-scenario :as od public-lands :as pl]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'recreationService:NoPublicAccess)    
                  (:pl %))))
  (model WildlifeSpeciesRichnessClass
    (classification WildlifeSpeciesRichnessClass
      :context [open-development-scenario :as od wildlife-species-richness-class :as wl]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'recreationService:LowWildlifeSpeciesRichness)    
                  (:wl %))))
  (model sanPedro:DoveHabitat
    (classification sanPedro:DoveHabitat
      :context [open-development-scenario :as od dove-habitat :as dh]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:SingleDoveSpeciesPresent)
                  (:dh %))))
  (model sanPedro:DeerHabitat
    (classification sanPedro:DeerHabitat
      :context [open-development-scenario :as od deer-habitat :as dh]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:DeerSpeciesAbsent)    
                  (:dh %))))
  (model sanPedro:QuailHabitat
    (classification sanPedro:QuailHabitat
      :context [open-development-scenario :as od quail-habitat :as qh]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:QuailSpeciesAbsent)    
                  (:qh %))))
  (model sanPedro:JavelinaHabitat
    (classification sanPedro:JavelinaHabitat
      :context [open-development-scenario :as od javelina-habitat :as jh]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:JavelinaHabitatAbsent)    
                  (:jh %)))))

;;(defscenario urban-growth sanPedro:UrbanGrowth (add new users, up 10.4% in constrained, 56.8% in open)
;;sanPedro:UrbanGrowth2020Open
;;sanPedro:UrbanGrowth2020Constrained
;;sanPedro:Steinitz30ClassUrbanGrowthLULCOpen
;;sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained