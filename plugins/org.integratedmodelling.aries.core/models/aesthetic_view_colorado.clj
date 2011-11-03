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

(defmodel mountain Mountain
  (classification (measurement geophysics:Altitude "m")
    [1800 8850]  LargeMountain ; No higher than Mt. Everest, catches artifacts
    [1400 1800]  SmallMountain
    :otherwise   NoMountain)) ; Catches low artifacts

(defmodel scenic-vegetation sanPedro:ScenicVegetationType
  ;;  [(categorization geofeatures:Country)]
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) 
    ;;                  :when #(= (:country %) "United States")
    #{1 2 3 4 5 6 7 8 9 15 39 69 70 71 86 89}               sanPedro:AlpineAndCliff
    #{22 23 33 37 38 91}                                    sanPedro:Forests
    #{34 35 36 41 42 44 46 63 64 92 95 100 101 102 103 109} sanPedro:Woodland ; Includes pinon & juniper savannas
    #{77 78 79 80 81 83 84 85 98 109 110 118}               sanPedro:RiparianAndWater
    :otherwise                                              sanPedro:Other)
  (classification (categorization mexico:CONABIOLULCCategory)
    #{"Bosque de coniferas distintas a Pinus" "Bosque de encino" "Bosque de pino"} sanPedro:Forests
    #{"Vegetacion de galeria"}                                                     sanPedro:Woodland
    #{"Cuerpos de agua"}                                                           sanPedro:RiparianAndWater
    :otherwise                                                                     sanPedro:Other))

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
    [50 100] HighNaturalBeauty
    [25  50] ModerateNaturalBeauty
    [10  25] LowNaturalBeauty
    [ 0   1] NoNaturalBeauty))

;; source bayesian model                 
(defmodel source AestheticViewProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian AestheticViewProvision 
    :import  "aries.core::ViewSourceSanPedro.xdsl"
    :context [mountain scenic-vegetation]
    :keep    [TheoreticalNaturalBeauty]
    :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel mine Mines                         
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)         
    #{19 117}  MinesPresent
    :otherwise MinesAbsent))

(defmodel transmission-line TransmissionLines 
  (classification (binary-coding infrastructure:TransmissionLine)
    1          TransmissionLinesPresent
    :otherwise TransmissionLinesAbsent))

(defmodel highway Highways 
  (classification (binary-coding infrastructure:Highway)
    1          HighwaysPresent
    :otherwise HighwaysAbsent))

;; Insert correct concepts for Mexico
(defmodel developed-land DevelopedLand
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)           
    112        HighDensityDevelopment
    111        LowDensityDevelopment
    :otherwise NoDevelopment))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
    [50 100] HighBlight
    [25  50] ModerateBlight
    [ 5  25] LowBlight
    [ 0   5] NoBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality and enjoyment of scenic views"
  (bayesian ViewSink 
    :import  "aries.core::ViewSinkSanPedro.xdsl"
    :context [mine highway transmission-line developed-land]
    :keep    [VisualBlight]
    :result  view-sink-undiscretizer))

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

(defmodel property-value HousingValue ; Value is in $/ac, which is not a legitimate unit in thinklab, so kept as a ranking for now.
  (classification (ranking economics:AppraisedPropertyValue)
    [     0  10000] VeryLowHousingValue
    [ 10000  25000] LowHousingValue
    [ 25000  50000] ModerateHousingValue
    [ 50000 200000] HighHousingValue
    [200000     :>] VeryHighHousingValue))

;; Scenic highways as another beneficiary class - i.e., their drivers benefit from views along highways.
(defmodel scenic-highways ScenicDrivePresence
  (classification (binary-coding ScenicDrives)
    1          ScenicDrivesPresent
    :otherwise ScenicDrivesAbsent))

;; Undiscretizer for view use
;; This could be a range (high-mod-low)
(defmodel view-use-undiscretizer HomeownerViewUse
  (probabilistic-ranking HomeownerViewUse
    [0.05 1] HomeownerViewUsePresent
    [0 0.05] HomeownerViewUseAbsent))

;; bayesian model
(defmodel homeowners ViewUse
  "Property owners who can afford to pay for the view"
  (bayesian ViewUse 
    :import  "aries.core::ViewUseSanPedro.xdsl"
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
        :source-threshold   25.0 ; Excludes LowNaturalBeauty
        :sink-threshold     25.0 ; Excludes LowBlight
        :use-threshold       0.2 ; Excludes HomeownerViewUseAbsent
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