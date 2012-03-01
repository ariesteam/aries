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
;;; Viewshed model for Western Washington
;;;
;;; Valid Contexts: core.contexts.puget/{chehalis,wria9,viewshed,western-wa}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.aesthetic-view-puget
  (:refer-clojure :rename {count length})
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario namespace-ontology model
                           defmodel measurement classification
                           categorization ranking numeric-coding
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking binary-coding
                           identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService
  (thinklab-core:BooleanRanking
   (LandOrSea
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel lake Lake
  (classification (binary-coding geofeatures:Lake)
    1          LakePresent
    :otherwise LakeAbsent))

(defmodel ocean Ocean
  (classification (binary-coding geofeatures:Ocean)
    1          OceanPresent
    :otherwise OceanAbsent))

(defmodel mountain Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement geophysics:Altitude "m")
    [2500 8850] LargeMountain ; No higher than Mt. Everest, catches artifacts
    [1000 2500] SmallMountain
    :otherwise  NoMountain))  ; Catches low artifacts

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
    [50 100] HighNaturalBeauty
    [25  50] ModerateNaturalBeauty
    [ 5  25] LowNaturalBeauty
    [ 0   5] NoNaturalBeauty))

;; source bayesian model                 
(defmodel source AestheticViewProvision
  (bayesian AestheticViewProvision 
    :import  "aries.core::ViewSourcePuget.xdsl"
    :context [mountain lake ocean]
    :keep    [TheoreticalNaturalBeauty]
    :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel clearcut Clearcuts 
  (classification (categorization geofeatures:Clearcut)
    #{"EVEN-AGE" "EVEN R/W" "EVEN/SALVAGE"} ClearcutsPresent
    :otherwise                              ClearcutsAbsent))

;; NLCD 1992 for Commercial/Industrial/Transportation land use
(defmodel commercial-transportation CommercialIndustrialTransportation
  (classification (numeric-coding nlcd:NLCD1992Typology)
    23         TransportationInfrastructurePresent
    :otherwise TransportationInfrastructureAbsent))

(defmodel highway Highways
  (classification (binary-coding infrastructure:Highway)
    1          HighwaysPresent
    :otherwise HighwaysAbsent))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
    [50 100] HighBlight
    [25  50] ModerateBlight
    [ 5  25] LowBlight
    [ 0   5] NoBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality of scenic views"
  (bayesian ViewSink 
    :import  "aries.core::ViewSinkPuget.xdsl"
    :context [commercial-transportation clearcut highway]
    :keep    [VisualBlight]
    :result  view-sink-undiscretizer))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))  

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

(defmodel property-value HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  economics:AppraisedPropertyValue)
    [:exclusive 0 50000] VeryLowHousingValue
    [ 50000      150000] LowHousingValue
    [150000      300000] ModerateHousingValue
    [300000      500000] HighHousingValue
    [500000          :>] VeryHighHousingValue))

;; Training data for King County: actual housing with views.  
;; Do not use until BNs can be properly trained.
(defmodel view-use-king HomeownerViewUse
  (classification (binary-coding HomeownerViewUse)
    [0   5] HomeownerViewUsePresent ; Change to "IF ZERO OR GREATER" view use present when using training data, otherwise not.
    [5 100] HomeownerViewUseAbsent)) 

;;undiscretizer for view use
(defmodel view-use-undiscretizer HomeownerViewUse
  (probabilistic-ranking HomeownerViewUse
    [0.05 1] HomeownerViewUsePresent
    [0 0.05] HomeownerViewUseAbsent))

;; bayesian model
(defmodel homeowners ViewUse
  "Property owners who benefit from high-quality views"
  (bayesian ViewUse 
    :import   "aries.core::ViewUsePuget.xdsl"
    :context  [property-value housing land-selector]
    :required [LandOrSea]
    :keep     [HomeownerViewUse]
    :result   view-use-undiscretizer))

;;Scenic highways as another beneficiary class - i.e., their drivers benefit from views along highways.
(defmodel scenic-highways ScenicDrivePresence
  (classification (binary-coding ScenicDrives)
    1          ScenicDrivesPresent
    :otherwise ScenicDrivesAbsent))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data LineOfSight
  (identification LineOfSight
    :context [source homeowners sink altitude])) ; Add scenic-highways when there's time to test

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel view AestheticViewsheds
  (span LineOfSight 
        AestheticViewProvision
        ViewUse
        ViewSink
        nil
        (geophysics:Altitude)
        :source-threshold    5.0  
        :sink-threshold     11.0
        :use-threshold       0.1
        :trans-threshold     0.1
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 4
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_view_puget_data.clj")
        :context [source homeowners sink altitude]
        :keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSource
                  InaccessibleSink
                  InaccessibleUse
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))

;; Develop another flow model to account for scenic drives.

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

(defscenario open-development-viewshed
  "Changes values in low-density developed areas to moderate housing value present. Moderate to high density assumed to be primarily nonresidential land use, in the absence of more information."
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

(defscenario constrained-development-viewshed  "Changes values in low-density developed areas to moderate housing value present. Moderate to high density assumed to be primarily nonresidential land use, in the absence of more information."
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