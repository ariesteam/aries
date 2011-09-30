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
;;; Proximity model for Western Washington
;;;
;;; Valid Contexts: core.contexts.beta/{chehalis,wria9,viewshed,western_wa}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.aesthetic-proximity-puget
  (:refer-clojure :rename {count length})
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking model
                           probabilistic-measurement
                           probabilistic-ranking numeric-coding
                           binary-coding identification bayesian
                           count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService
  (thinklab-core:BooleanRanking
   (LandOrSea
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel lake-front LakeFront
  (classification (binary-coding LakeFrontPresence)
    1          LakeFrontPresent
    :otherwise LakeFrontAbsent))

(defmodel river-front RiverFront
  (classification (binary-coding RiverFrontPresence)
    1          RiverFrontPresent
    :otherwise RiverFrontAbsent))

(defmodel beach Beach
  (classification (binary-coding BeachPresence)
    1          BeachPresent
    :otherwise BeachAbsent))

(defmodel forest Forest
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{41 42 43} ForestPresent                  
    :otherwise  ForestAbsent))

(defmodel woody-wetland WoodyWetland
  (classification (numeric-coding nlcd:NLCDNumeric)
    90         WoodyWetlandPresent                  
    :otherwise WoodyWetlandAbsent))

(defmodel emergent-wetland EmergentWetland
  (classification (numeric-coding nlcd:NLCDNumeric)
    95         EmergentWetlandPresent                  
    :otherwise EmergentWetlandAbsent))

(defmodel farmland Farmland
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{81 82}   FarmlandPresent                  
    :otherwise FarmlandAbsent))

(defmodel park Park
  (classification (binary-coding ParkPresence)
    1          ParkPresent
    :otherwise ParkAbsent))

(defmodel crime-potential CrimePotential
  (classification (categorization geofeatures:City)
    #{"Seattle" "Tacoma"} HighCrimePotential
    :otherwise            LowCrimePotential))

(defmodel water-quality WaterQuality
  (classification (ranking WaterQualityAssessment)
    1          MeetsStandards
    #{2 4 24}  OfConcern
    5          RequiringTMDL
    :otherwise NoSurfaceWater))

;; This is set as a Puget-specific concept since it's a binary coding
;; rather than a ranking like the global conservation status dataset.
(defmodel formal-protection FormalProtection
  (classification (binary-coding puget:ProtectedStatus)
    1          Protected
    :otherwise NotProtected))

;; Computed area of open space polygons as a GIS operation and stored
;; this value in each pixel
(defmodel area OpenSpaceAreaClass
  (classification (measurement OpenSpaceArea "ha")
    [40           :>] VeryLargeArea
    [10           40] LargeArea
    [ 2           10] SmallArea
    [:exclusive 0  2] VerySmallArea))

(defmodel theoretical-open-space TheoreticalProximitySource
  (probabilistic-ranking TheoreticalProximitySource
    [50 100] HighProximityPotential
    [25  50] ModerateProximityPotential
    [ 5  25] LowProximityPotential
    [ 0   5] NoProximityPotential))

(defmodel source AestheticProximityProvision
  (bayesian AestheticProximityProvision
    :import  "aries.core::ProximitySourcePuget.xdsl"
    :context [lake-front river-front beach forest woody-wetland
              emergent-wetland farmland park crime-potential
              water-quality formal-protection area]
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

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

(defmodel housing PresenceOfHousing
  (classification (ranking PresenceOfHousingRank)
    [1 :>]     HousingPresent
    :otherwise HousingAbsent)
  (classification (numeric-coding nlcd:NLCDNumeric) ; Using NLCD where parcel data are unavailable.
    [22 23 24] HousingPresent  ; Assumes (incorrectly) that all developed land is housing.
    :otherwise HousingAbsent))

;; TODO we need this to become an actual valuation with currency and
;; date, so we can turn any values into these dollars
(defmodel property-value HousingValue
  (classification (ranking economics:AppraisedPropertyValue)
    [:exclusive 0  50000] VeryLowHousingValue
    [ 50000       150000] LowHousingValue
    [150000       300000] ModerateHousingValue
    [300000       500000] HighHousingValue
    [500000           :>] VeryHighHousingValue))

;; Urban proximity proxied by year 2007 population density for
;; Washington
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
    :import   "aries.core::ProximityUsePuget.xdsl"
    :context  [property-value urban-proximity housing land-selector]
    :required [LandOrSea]
    :keep     [HomeownerProximityUse]
    :result   proximity-use-undiscretizer))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data Proximity
  (identification Proximity
    :context [source sink homeowners]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel proximity AestheticProximity
  (span Proximity
        AestheticProximityProvision
        ProximityUse
        ProximitySink
        nil
        nil
        :source-threshold   40.0  ; Excludes LowProximityPotential
        :sink-threshold     0.0   ; Deterministic as 0.0 or 50.0 based on presence of highways
        :use-threshold      0.2   ; Excludes HomeownerProximityUseAbsent
        :trans-threshold    1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_proximity_puget_data.clj")
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

(defmodel constrained-development-scenario puget:ConstrainedDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCConstrained2060) 
    4                                   puget:HighDensityDevelopedConstrained
    6                                   puget:ModerateDensityDevelopedConstrained
    5                                   puget:LowDensityDevelopedConstrained
    7                                   puget:UrbanOpenSpaceConstrained
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedConstrained))

(defmodel open-development-scenario puget:OpenDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCOpen2060) 
    4                                   puget:HighDensityDevelopedOpen
    6                                   puget:ModerateDensityDevelopedOpen
    5                                   puget:LowDensityDevelopedOpen
    7                                   puget:UrbanOpenSpaceOpen
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedOpen))

(defscenario open-development-proximity
  "Changes values in developed areas to no valuable open space type,
   moderate housing value present."
  (model Forest
    (classification Forest
      :context [open-development-scenario :as od forest :as for]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:ForestAbsent)
                  (:for %))))
  (model Farmland
    (classification Farmland
      :context [open-development-scenario :as od farmland :as farm]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:FarmlandAbsent) 
                  (:farm %))))
  (model WoodyWetland
    (classification WoodyWetland
      :context [open-development-scenario :as od woody-wetland :as ww]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:WoodyWetlandAbsent)
                  (:ww %))))
  (model EmergentWetland
    (classification EmergentWetland
      :context [open-development-scenario :as od emergent-wetland :as ew]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:EmergentWetlandAbsent)
                  (:ew %))))
  (model Park
    (classification Park
      :context [open-development-scenario :as od park :as p]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'aestheticService:ParkAbsent)
                    
                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'aestheticService:ParkPresent)

                      :otherwise (:p %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
      :context [open-development-scenario :as od housing :as h]
      :state   #(if (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                  (conc 'aestheticService:HousingPresent)           
                  (:h %))))
  (model HousingValue
    (classification HousingValue
      :context [open-development-scenario :as od property-value :as pv]
      :state   #(if (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                  (conc 'aestheticService:ModerateHousingValue)           
                  (:pv %)))))

(defscenario constrained-development-proximity
  "Changes values in developed areas to no valuable open space type,
   moderate housing value present."
  (model Forest
    (classification Forest
      :context [constrained-development-scenario :as cd forest :as for]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:ForestAbsent)
                  (:for %))))
  (model Farmland
    (classification Farmland
      :context [constrained-development-scenario :as cd farmland :as farm]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:FarmlandAbsent)
                  (:farm %))))
  (model WoodyWetland
    (classification WoodyWetland
      :context [constrained-development-scenario :as cd woody-wetland :as ww]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:WoodyWetlandAbsent)
                  (:ww %))))
  (model EmergentWetland
    (classification EmergentWetland
      :context [constrained-development-scenario :as cd emergent-wetland :as ew]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:EmergentWetlandAbsent)
                  (:ew %))))
  (model Park
    (classification Park
      :context [constrained-development-scenario :as cd park :as p]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'aestheticService:ParkAbsent)
                    
                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'aestheticService:ParkPresent)

                      :otherwise (:p %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
      :context [constrained-development-scenario :as cd housing :as h]
      :state   #(if (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                  (conc 'aestheticService:HousingPresent)           
                  (:h %))))
  (model HousingValue
    (classification HousingValue
      :context [constrained-development-scenario :as cd property-value :as pv]
      :state   #(if (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                  (conc 'aestheticService:ModerateHousingValue)           
                  (:pv %)))))
