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
;;; Viewshed model for Colorado
;;;
;;; Valid Contexts: core.contexts.beta/co*
;;;
;;;-------------------------------------------------------------------

(ns core.models.aesthetic-view-colorado
  (:refer-clojure :rename {count length})
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking model
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking numeric-coding
                           binary-coding identification bayesian
                           count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel lake Lake
  (classification (binary-coding geofeatures:Lake)
    1          LakePresent
    :otherwise LakeAbsent))

(defmodel peaks SharpPeaks
  (classification (binary-coding colorado:SharpPeakPresence)
    1          PeaksPresent
    :otherwise PeaksAbsent))

;; Consider combining the four below based on the simplified BN if
;; Gary's flow model description.
(defmodel conifer colorado:ConiferForests
  (classification (binary-coding colorado:ConiferForestPresence)
    1          colorado:ConifersPresent
    :otherwise colorado:ConifersAbsent))

(defmodel aspen colorado:AspenForests
  (classification (binary-coding colorado:AspenForestPresence)
    1          colorado:AspensPresent
    :otherwise colorado:AspensAbsent))

(defmodel high-grass-shrubs colorado:HighGrassOrShrubs
  (classification (binary-coding colorado:HighGrassShrubPresence)
    1          colorado:HighGrassShrubsPresent
    :otherwise colorado:HighGrassShrubsAbsent))

(defmodel low-grass-shrubs colorado:LowGrassOrShrubs
  (classification (binary-coding colorado:LowGrassShrubPresence)
    1          colorado:LowGrassShrubsPresent
    :otherwise colorado:LowGrassShrubsAbsent))

;; Interesting that they decided to exclude elevation. Consider
;; re-adding if the choice isn't well justified.
;; (defmodel mountain Mountain
;;   (classification (measurement geophysics:Altitude "m")
;;      [1800 8850]  LargeMountain ; No higher than Mt. Everest, catches artifacts
;;      [1400 1800]  SmallMountain
;;      :otherwise   NoMountain)) ; Catches low artifacts

;; Replace above pending review of Gary's plain-English flow model description
;; (defmodel scenic-vegetation colorado:ScenicVegetationType
;; (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
;;  #{24 26 28 29 30 32 34 35 36 78 79} colorado:ConiferForest
;;  #{22 38}                            colorado:AspenForest
;;  #{69 70 71 77 86}                   colorado:HighGrassShrubs
;;  #{41 42 48 62 67 82 85 114}         colorado:LowGrassShrubs
;;  :otherwise                          colorado:Other))

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
    [50 100] HighNaturalBeauty
    [25  50] ModerateNaturalBeauty
    [ 1  25] LowNaturalBeauty
    [ 0   1] NoNaturalBeauty))

;; source bayesian model                 
(defmodel source AestheticViewProvision
  (bayesian AestheticViewProvision 
    :import  "aries.core::ViewSourceColorado.xdsl"
    :context [lake peaks conifer aspen high-grass-shrubs low-grass-shrubs]
    :keep    [TheoreticalNaturalBeauty]
    :result  theoretical-beauty))

;; Source bayesian model, simplified. Consider using when you can look
;; at Gary's plain-English description of the flow models.              
;;(defmodel source AestheticViewProvision
;;  (bayesian AestheticViewProvision 
    ;;    :import  "aries.core::ViewSourceColoradoSimplified.xdsl"
    ;;    :context [lake peaks scenic-vegetation]
    ;;    :keep    [TheoreticalNaturalBeauty]
    ;;    :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel general-disturbance colorado:GenericDisturbance
  (classification (binary-coding colorado:GeneralDisturbance)
    1          colorado:DisturbancePresent
    :otherwise colorado:DisturbanceAbsent))

(defmodel clearcut Clearcuts 
  (classification (binary-coding geofeatures:Clearcut)
    1          ClearcutsPresent
    :otherwise ClearcutsAbsent))

(defmodel development-infrastructure colorado:DevelopmentInfrastructure 
  (classification (binary-coding colorado:Development)
    1          colorado:InfrastructurePresent
    :otherwise colorado:InfrastructureAbsent))

(defmodel gray-kill colorado:GrayBeetleKill
  (classification (binary-coding colorado:GrayBeetleStage)
    1          colorado:GrayKillPresent
    :otherwise colorado:GrayKillAbsent))

(defmodel green-gray-kill colorado:GreenGrayBeetleKill
  (classification (binary-coding colorado:GreenGrayBeetleStage)
    1          colorado:GreenGrayKillPresent
    :otherwise colorado:GreenGrayKillAbsent))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
    [50 100] HighBlight
    [25  50] ModerateBlight
    [ 5  25] LowBlight
    [ 0   5] NoBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality of scenic views"
  (bayesian ViewSink 
    :import  "aries.core::ViewSinkColorado.xdsl"
    :context [general-disturbance clearcut development-infrastructure gray-kill green-gray-kill]
    :keep    [VisualBlight]
    :result  view-sink-undiscretizer))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel housing PresenceOfHousing
  (classification (binary-coding aestheticService:PresenceOfHousing) ; This may give a problem being a binary coding when it's not for other case studies - hopefully this won't be an issue.
    1          HousingPresent
    :otherwise HousingAbsent))

(defmodel property-value HousingValue
  (classification (ranking aestheticService:HousingValue)
    1 VeryLowHousingValue ; This is a poor classification, would be
                          ; better to have the actual $ valus and
                          ; classify according to those.
    2 LowHousingValue
    3 ModerateHousingValue
    4 HighHousingValue
    5 VeryHighHousingValue))

;; Undiscretizer for view use
;; This could be a range (high-mod-low)
(defmodel view-use-undiscretizer HomeownerViewUse
  (probabilistic-ranking HomeownerViewUse
    [0.05 1] HomeownerViewUsePresent
    [0 0.05] HomeownerViewUseAbsent))

;; bayesian model
(defmodel homeowners ViewUse
  "Property owners who benefit from high-quality views"
  (bayesian ViewUse 
    :import  "aries.core::ViewUseColorado.xdsl"
    :context [housing property-value]
    :keep    [HomeownerViewUse]
    :result  view-use-undiscretizer))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))                                    

(defmodel data-homeowners LineOfSight
  (identification LineOfSight
    :context [source homeowners sink altitude]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel view AestheticView
  (span LineOfSight 
        AestheticViewProvision
        ViewUse
        ViewSink
        nil
        (geophysics:Altitude)
        :source-threshold    4.0
        :sink-threshold      5.0
        :use-threshold       0.05
        :trans-threshold     1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_view_san_pedro_data.clj")
        :context [source homeowners sink altitude]
        :keep    [PotentialViews
                  PotentialVisualBlight
                  HomeownersWithViewDemand
                  PossibleViews
                  VisibleNaturalBeauty
                  HomeownersWithPossibleViews
                  ActualViews
                  EnjoyedViews
                  RelevantVisualBlight
                  HomeownersWithViews
                  UnseenViews
                  InaccessibleVisualBlight
                  HomeownersWithoutViews
                  BlockedViews
                  DegradedNaturalBeauty
                  HomeownersWithDegradedViews]))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------