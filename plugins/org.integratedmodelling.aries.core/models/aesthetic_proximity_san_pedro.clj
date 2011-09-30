;;; Copyright 2011 The ARIES Consortium
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
;;; Proximity model for San Pedro
;;;
;;; Valid Contexts: core.contexts.beta/san_pedro_us*
;;;
;;;-------------------------------------------------------------------

(ns core.models.aesthetic-proximity-san-pedro
  (:refer-clojure :rename {count length})
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking model
                           probabilistic-measurement
                           probabilistic-ranking numeric-coding
                           binary-coding identification bayesian
                           count no-data?])
  (:refer aries :only [span]))

(namespace-ontology aestheticService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; Data on land cover types are for the U.S. only, using SWReGAP data.
;; Since we have no parcel/housing location data for Mexico, plan to
;; solely run model in the U.S. on U.S. LULC types.  Bare rock/dune,
;; etc. are not included here.
(defmodel forest sanPedro:ForestAndWoodland
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) 
    #{22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 41 45 46 63 64 91 92 95 101 102 103} sanPedro:ForestOrWoodlandPresent               
    :otherwise                                                                                sanPedro:ForestOrWoodlandAbsent))

(defmodel farmland Farmland
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    114        FarmlandPresent                     
    :otherwise FarmlandAbsent))

(defmodel grassland Grassland
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{65 68 73 74 75 76 90 93 106} GrasslandPresent                     
    :otherwise                     GrasslandAbsent))

(defmodel desert-scrub DesertScrub ; includes chapparal and other shrubland
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{40 44 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 66 67 82 94 96 97 105 108} DesertScrubPresent
    :otherwise                                                                         DesertScrubAbsent))

;; This model assumes that all riparian areas that are not mapped
;; within the SPRNCA are low quality.  This is a poor assumption -
;; moderate quality might also be appropriate and it would be better
;; to run these as a simple BN for presence and quality like the
;; housing presence and value BNs, incoprorating priors for quality
;; when we lack data.
(defmodel riparian-wetland-code sanPedro:RiparianAndWetlandCode
  (numeric-coding sanPedro:RiparianAndWetlandCode
    :context [(numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
              (ranking sanPedro:RiparianConditionClass)]
    :state   #(if (contains? #{77.0 78.0 79.0 80.0 81.0 83.0 84.0 85.0 98.0 109.0 110.0 118.0} (:southwest-regional-gap-analysis-l-u-l-c %))
                (let [condition (:riparian-condition-class %)]
                  (if (no-data? condition)
                    1
                    condition))
                0)))

(defmodel riparian-wetland sanPedro:RiparianAndWetland
  (classification riparian-wetland-code
    3 sanPedro:HighQualityRiparianOrWetlandPresent
    2 sanPedro:ModerateQualityRiparianOrWetlandPresent
    1 sanPedro:LowQualityRiparianOrWetlandPresent
    0 sanPedro:RiparianOrWetlandAbsent))

(defmodel park Park
  (classification (numeric-coding habitat:LandOwnership)
    #{8 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
      116 117 118 119 120 121 122 123 124 125 126 127} ParkPresent
      :otherwise                                       ParkAbsent))

(defmodel fire-threat FireThreat
  (classification (numeric-coding habitat:FireReturnInterval) 
    #{1 2 3} HighFireThreat ; Includes high, moderate, variable fire frequency
    #{4 5 6} LowFireThreat))

;; This uses the WDPA data - need to double check that the numbers
;; correspond to protected/not protected (see what's getting output).
;; Might be worthwhile to replace with local data.  Should also change
;; unprotected land to state & private land only: other BLM for
;; instance isn't going to be developed.
(defmodel formal-protection FormalProtection
  (classification (binary-coding conservation:ProtectedStatus)
    1          Protected
    :otherwise NotProtected))

;; Computes area of open space polygons as a GIS operation and stores
;; this value in each pixel
(defmodel area OpenSpaceAreaClass
  (classification (measurement OpenSpaceArea "ha")
    [40 :>]  VeryLargeArea
    [10 40]  LargeArea
    [ 2 10]  SmallArea
    [:<  2]  VerySmallArea))

(defmodel theoretical-open-space TheoreticalProximitySource
  (probabilistic-ranking TheoreticalProximitySource
    [50 100] HighProximityPotential
    [25  50] ModerateProximityPotential
    [ 5  25] LowProximityPotential
    [ 0   5] NoProximityPotential))

(defmodel source AestheticProximityProvision
  (bayesian AestheticProximityProvision
    :import  "aries.core::ProximitySourceSanPedro.xdsl"
    :context [forest farmland grassland desert-scrub park fire-threat formal-protection riparian-wetland area]
    :keep    [TheoreticalProximitySource]
    :result  theoretical-open-space))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; 50 units of proximity value are depleted by the sink if highways
;; are present. Otherwise zero sink.
(defmodel sink ProximitySink
  (ranking ProximitySink
    :context [(binary-coding infrastructure:Highway)]
    :state   #(if (== (:highway %) 1) 50 0)))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel housing PresenceOfHousing
  (classification (ranking economics:AppraisedPropertyValue)
    [1 :>]     HousingPresent
    :otherwise HousingAbsent)
  (classification (numeric-coding nlcd:NLCDNumeric) ; Using NLCD where parcel data are unavailable.
    [22 23 24] HousingPresent  ; Assumes (incorrectly) that all developed land is housing.
    :otherwise HousingAbsent))

;; Value is in $/ac, which is not a legitimate unit in thinklab, so
;; kept as a ranking for now.
(defmodel property-value HousingValue
  (classification (ranking economics:AppraisedPropertyValue)
    [200000     :>] VeryHighHousingValue
    [ 50000 200000] HighHousingValue
    [ 25000  50000] ModerateHousingValue
    [ 10000  25000] LowHousingValue
    [     0  10000] VeryLowHousingValue))

;; Urban proximity proxied by year 2000 population density for Arizona
(defmodel urban-proximity UrbanProximity
  (classification (count policytarget:PopulationDensity "/km^2")
    [309  :>] Urban
    [ 77 309] Suburban
    [:<   77] Rural))

(defmodel proximity-use-undiscretizer HomeownerProximityUse
  (probabilistic-ranking HomeownerProximityUse
    [0.05 1] HomeownerProximityUsePresent
    [0 0.05] HomeownerProximityUseAbsent))

(defmodel homeowners ProximityUse
  "Property owners who can afford to pay for proximity to open space."
  (bayesian ProximityUse 
    :import  "aries.core::ProximityUseSanPedro.xdsl"
    :context [property-value urban-proximity housing]
    :keep    [HomeownerProximityUse]
    :result  proximity-use-undiscretizer))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data Proximity
  (identification Proximity
    :context [source sink homeowners]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Gary: For the flow models, distance decay on proxmity should
;; usually be steep (i.e., very steep after 0.5 mi, almost nothing
;; left after 1.0 mi.  However, this decay should be a little less
;; steep for rivers in western regions (i.e., San Pedro) - perhaps a
;; similar decay function stretched with an inflection point at 1.0 mi
;; and decaying to very little at 2.0 mi.

(defmodel proximity AestheticProximity
  (span Proximity
        AestheticProximityProvision
        ProximityUse
        ProximitySink
        nil
        nil
        :source-threshold   20.0  ; Excludes LowProximityPotential
        :sink-threshold     0.0   ; Deterministic as 0.0 or 50.0 based on presence of highways
        :use-threshold      0.1   ; Excludes HomeownerProximityUseAbsent
        :trans-threshold    1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_proximity_san_pedro_data.clj")
        :context            [source homeowners sink]
        :keep               [PotentialProximateOpenSpace
                             PotentialProximitySink
                             HomeownersWithOpenSpaceDemand
                             PossibleProximateOpenSpace
                             AccessibleOpenSpace
                             OpenSpaceProximateHomeowners
                             AccessibleProximity
                             EnjoyedOpenSpace
                             BlockingProximitySink
                             HomeownersWithProximateOpenSpace
                             UnaccessedOpenSpace
                             InaccessibleProximitySink
                             HomeownersWithoutProximateOpenSpace
                             BlockedProximity
                             BlockedOpenSpace
                             HomeownersWithBlockedProximity]))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

(defscenario cap-water-augmentation-half-meter-rise
  "Water augmentation leading to a 0.5 meter rise across the San Pedro
   Riparian National Conservation Area"
  (model sanPedro:RiparianConditionClass
    (ranking sanPedro:RiparianConditionClassHalfMeterRise)))

(defscenario cap-water-augmentation-all-perennial
  "Water augmentation leading to perennial flow conditions across the
   San Pedro Riparian National Conservation Area"
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

(defscenario open-development-proximity
  "Changes values in developed areas to no valuable open space type,
   fire threat to low, high housing value present."
  (model sanPedro:ForestAndWoodland
    (classification sanPedro:ForestAndWoodland
      :context [open-development-scenario :as od forest :as fw]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:ForestOrWoodlandAbsent)
                  (:fw %))))
  (model Farmland
    (classification Farmland
      :context [open-development-scenario :as od farmland :as f]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:FarmlandAbsent) 
                  (:f %))))
  (model Grassland
    (classification Grassland
      :context [open-development-scenario :as od grassland :as g]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:GrasslandAbsent)
                  (:g %))))
  (model DesertScrub
    (classification DesertScrub
      :context [open-development-scenario :as od desert-scrub :as ds]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:DesertScrubAbsent)
                  (:ds %))))
  (model Park
    (classification Park
      :context [open-development-scenario :as od park :as p]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:ParkAbsent) 
                  (:p %))))
  (model sanPedro:RiparianAndWetland
    (classification sanPedro:RiparianAndWetland
      :context [open-development-scenario :as od riparian-wetland :as rw]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:RiparianOrWetlandAbsent)
                  (:rw %))))
  (model FireThreat
    (classification FireThreat
      :context [open-development-scenario :as od fire-threat :as ft]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:LowFireThreat) 
                  (:ft %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
      :context [open-development-scenario :as od housing :as h]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:HousingPresent)           
                  (:h %))))
  (model HousingValue
    (classification HousingValue
      :context [open-development-scenario :as od property-value :as pv]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'aestheticService:HighHousingValue)           
                  (:pv %)))))

(defscenario constrained-development-proximity
  "Changes values in developed areas to no valuable open space type,
   fire threat to low, high housing value present."
  (model sanPedro:ForestAndWoodland
    (classification sanPedro:ForestAndWoodland
      :context [constrained-development-scenario :as cd forest :as fw]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:ForestOrWoodlandAbsent)
                  (:fw %))))
  (model Farmland
    (classification Farmland
      :context [constrained-development-scenario :as cd farmland :as f]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:FarmlandAbsent)
                  (:f %))))
  (model Grassland
    (classification Grassland
      :context [constrained-development-scenario :as cd grassland :as g]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:GrasslandAbsent)  
                  (:g %))))
  (model DesertScrub
    (classification DesertScrub
      :context [constrained-development-scenario :as cd desert-scrub :as ds]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:DesertScrubAbsent) 
                  (:ds %))))
  (model Park
    (classification Park
      :context [constrained-development-scenario :as cd park :as p]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:ParkAbsent) 
                  (:p %))))
  (model sanPedro:RiparianAndWetland
    (classification sanPedro:RiparianAndWetland
      :context [constrained-development-scenario :as cd riparian-wetland :as rw]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:RiparianOrWetlandAbsent)
                  (:rw %))))
  (model FireThreat
    (classification FireThreat
      :context [constrained-development-scenario :as cd fire-threat :as ft]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:LowFireThreat)     
                  (:ft %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
      :context [constrained-development-scenario :as cd housing :as h]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:HousingPresent)          
                  (:h %))))
  (model HousingValue
    (classification HousingValue
      :context [constrained-development-scenario :as cd property-value :as pv]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'aestheticService:HighHousingValue)            
                  (:pv %)))))
