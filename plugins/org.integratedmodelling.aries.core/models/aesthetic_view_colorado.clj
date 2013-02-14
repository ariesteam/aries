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
;;; Valid Contexts: core.contexts.colorado/co*
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
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    110        LakePresent
    :otherwise LakeAbsent))

(defmodel peaks MountainPeaks
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{1 2 3}            SharpPeaks
    #{5 7 8 9 10 12 15} OtherPeaks
    :otherwise          PeaksAbsent))

(defmodel scenic-vegetation colorado:ScenicVegetationType
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{24 26 28 29 30 32 33 34 35 36 78 79 92 103} colorado:ConiferForest
    #{22 38}                                      colorado:AspenForest
    #{39 44 62 69 70 71 77 86}                    colorado:HighElevationGrassShrubs
    #{19 40 41 42 43 46 48 50 53 56 58 62 63 64 67 68 71 72 73 74 75 76 79 82 85 95 104 108 109 114 119 120 121 122} colorado:LowElevationGrassShrubs
    :otherwise                                    colorado:Other))

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
    [50 100] HighNaturalBeauty
    [25  50] ModerateNaturalBeauty
    [ 1  25] LowNaturalBeauty
    [ 0   1] NoNaturalBeauty))

;; Source bayesian model, simplified. Consider using when you can look
;; at Gary's plain-English description of the flow models.              
(defmodel source ViewSource
  (bayesian ViewSource
        :import  "aries.core::ViewSourceColorado.xdsl"
        :context [lake peaks scenic-vegetation]
        :keep    [TheoreticalNaturalBeauty]
        :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel clearcut Clearcuts
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{123 124} ClearcutsPresent
    :otherwise ClearcutsAbsent))

(defmodel mine Mines
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)         
    117        MinesPresent
    :otherwise MinesAbsent))

(defmodel burn BurnedArea
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)         
    116        BurnedAreaPresent
    :otherwise BurnedAreaAbsent))

(defmodel transmission-line TransmissionLines 
  (classification (binary-coding infrastructure:TransmissionLine)
    1          TransmissionLinesPresent
    :otherwise TransmissionLinesAbsent))

(defmodel highway Highways
  (classification (binary-coding infrastructure:Highway)
    1          HighwaysPresent
    :otherwise HighwaysAbsent))

(defmodel developed-land DevelopedLand
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)           
    112        HighDensityDevelopment
    111        LowDensityDevelopment
    :otherwise NoDevelopment))

(defmodel beetle-kill colorado:GreenGrayBeetleKill
  (classification (count colorado:MountainPineBeetleDamageTreesPerHectare "/ha")
    [19 1236]  colorado:GrayKillPresent
    [ 1   19]  colorado:GreenGrayKillPresent
    :otherwise colorado:BeetleKillAbsent))

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
    :context [clearcut mine burn developed-land transmission-line highway beetle-kill]
    :keep    [VisualBlight]
    :result  view-sink-undiscretizer))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; Commenting out housing data until we get the parcel data for
;Colorado completed.
;(defmodel housing PresenceOfHousing
; (classification (binary-coding aestheticService:PresenceOfHousing) ; This may give a problem being a binary coding when it's not for other case studies - hopefully this won't be an issue.
;  1          HousingPresent
;  :otherwise HousingAbsent))

(defmodel housing PresenceOfHousing
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{22 23 24} HousingPresent
    :otherwise  HousingAbsent))

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

(defmodel view AestheticViewsheds
  (span LineOfSight 
        ViewSource
        ViewUse
        ViewSink
        nil
        (geophysics:Altitude)
        :source-threshold    10.0
        :sink-threshold      5.0
        :use-threshold       0.05
        :trans-threshold     1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 9
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_view_san_pedro_data.clj")
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------