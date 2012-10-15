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
;;; Aesthetic view model for Molise
;;;
;;; Valid Contexts: core.contexts.molise/molise
;;;
;;;-------------------------------------------------------------------

(ns core.models.aesthetic-view-molise
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
   (RiverLakeCode)
   (RiverLake
    (RiverLakePresent)
    (RiverLakeAbsent))
   (Ocean
    (OceanPresent)
    (OceanAbsent)))
  (lulc:LandClassificationNumericMapping
   (CorineNumeric))
  (ScenicViews
   (ScenicVegetationClass
    (OliveGroves)
    (Meadows)
    (Hedgerows)
    (DenseForestCover)
    (SparseForestCover)
    (NotScenicVegetation))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(declare ocean
         river-lake-code
         river-lake
         scenic-vegetation
         theoretical-beauty)

;;molise:rivers_molise
;;molise:lakes_molise
(defmodel river-lake-code RiverLakeCode
  (binary-coding RiverLakeCode
    :context [(binary-coding geofeatures:Lake)
              (binary-coding geofeatures:River)]
    :state   #(if (or (= (:lake %) 1)
                      (= (:river %) 1))
                1
                0)))

(defmodel river-lake RiverLake
  (classification river-lake-code
    1          RiverLakePresent
    :otherwise RiverLakeAbsent))

;;molise:CorineLandCover
(defmodel ocean Ocean
  "Identifies the presence of the ocean (sea)."
  (classification (numeric-coding CorineNumeric)
    523           OceanPresent
    :otherwise    OceanAbsent))

;;molise:CorineLandCover
(defmodel scenic-vegetation ScenicVegetationClass
  "Uses Corine 2006 land cover data to identify scenic vegetation."
  (classification (numeric-coding CorineNumeric)
    223            OliveGroves
    243            Meadows
    #{3232 324}    Hedgerows
    #{3116 3121 1} DenseForestCover
    2              SparseForestCover
    :otherwise     NotScenicVegetation))

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
    [75 100]       HighNaturalBeauty
    [50  75]       ModerateNaturalBeauty
    [25  50]       LowNaturalBeauty
    [ 0  25]       NoNaturalBeauty))

;; source bayesian model
(defmodel source ViewSource
  (bayesian ViewSource
    :import  "aries.core::AestheticSourceMoliseView.xdsl"
    :context [river-lake ocean scenic-vegetation]
    :keep    [TheoreticalNaturalBeauty]
    :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare developed-land
         transport-infrastructure)

;;molise:CorineLandCover
(defmodel developed-land DevelopedLand
  "Uses Corine 2006 land cover data to identify development density."
  (classification (numeric-coding CorineNumeric)
    #{111 131}     HighDensityDevelopment
    112            ModerateDensityDevelopment
    #{242 243}     LowDensityDevelopment
    :otherwise     NoDevelopment))

;;molise:TransportInfrastructure
(defmodel transport-infrastructure CommercialIndustrialTransportation
  "Identifies the presence of transportation infrastructure."
  (classification (binary-coding infrastructure:Road)
    1            TransportationInfrastructurePresent
    :otherwise   TransportationInfrastructureAbsent))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
    [50 100] HighBlight
    [25  50] ModerateBlight
    [ 5  25] LowBlight
    [ 0   5] NoBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality of scenic views"
  (bayesian ViewSink
    :import  "aries.core::AestheticSinkMoliseView.xdsl"
    :context [developed-land transport-infrastructure]
    :keep    [VisualBlight]
    :result  view-sink-undiscretizer))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;molise:Trails
(defmodel hiking-use recreationService:HikingUse
  "Trail network at the farm."
 (binary-coding recreationService:HikingUse))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;; molise:DEM_molise_20m
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data-homeowners LineOfSight
  (identification LineOfSight
    :context [source sink hiking-use altitude]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel view AestheticViewsheds
  (span LineOfSight
        ViewSource
        recreationService:HikingUse
        ViewSink
        nil
        (geophysics:Altitude)
        :source-threshold    55.0
        :sink-threshold      10.0
        :use-threshold       0.25
        :trans-threshold     1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 2
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/aesthetic_ontario_data.clj")
        ;; need to add additional context(s) for use
        :context [source sink hiking-use altitude]
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
