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
;;; Recreation model for Ontario
;;;
;;; Valid Contexts: core.contexts.ontario/algonquin-wgs84
;;;                 core.contexts.ontario/algonquin-bbox-wgs84
;;;-------------------------------------------------------------------

(ns core.models.recreation-molise
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

(namespace-ontology recreationService)

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
  (classification (numeric-coding molise-lulc:CorineNumeric)
    523           aestheticService:OceanPresent
    :otherwise    aestheticService:OceanAbsent))

;;molise:CorineLandCover
(defmodel scenic-vegetation ScenicVegetationClass
  "Uses Corine 2006 land cover data to identify scenic vegetation."
  (classification (numeric-coding molise-lulc:CorineNumeric)
    223            OliveGroves
    243            Meadows
    #{3232 324}    Hedgerows
    #{3116 3121 1} DenseForestCover
    2              SparseForestCover
    :otherwise     NotScenicVegetation))

(defmodel theoretical-beauty aestheticService:TheoreticalNaturalBeauty
  (probabilistic-ranking aestheticService:TheoreticalNaturalBeauty
    [75 100]       aestheticService:HighNaturalBeauty
    [50  75]       aestheticService:ModerateNaturalBeauty
    [25  50]       aestheticService:LowNaturalBeauty
    [ 0  25]       aestheticService:NoNaturalBeauty))

;; source bayesian model                 
(defmodel source aestheticService:ViewSource
  (bayesian aestheticService:ViewSource
    :import  "aries.core::AestheticSourceMoliseView.xdsl"
    :context [river-lake ocean scenic-vegetation]
    :keep    [aestheticService:TheoreticalNaturalBeauty]
    :result  theoretical-beauty))
;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare developed-land
         transport-infrastructure)

;;molise:CorineLandCover
(defmodel developed-land DevelopedLand
  "Uses Corine 2006 land cover data to identify development density."
  (classification (numeric-coding molise-lulc:CorineNumeric)
    #{111 131}     HighIntensityDevelopment
    112            ModerateIntensityDevelopment
    #{242 243}     LowIntensityDevelopment
    :otherwise     NoDevelopment))

;;molise:TransportInfrastructure
(defmodel transport-infrastructure TransportationInfrastructure
  "Identifies the presence of transportation infrastructure."
  (classification (binary-coding TransportationInfrastructure)
    1            TransportationInfrastructurePresent
    :otherwise   TransportationInfrastructureAbsent))

(defmodel view-sink-undiscretizer aestheticService:VisualBlight
  (probabilistic-ranking aestheticService:VisualBlight
    [50 100] aestheticService:HighBlight
    [25  50] aestheticService:ModerateBlight
    [ 5  25] aestheticService:LowBlight
    [ 0   5] aestheticService:NoBlight))

(defmodel sink aestheticService:ViewSink
  "Landscape features that reduce the quality of scenic views"
  (bayesian aestheticService:ViewSink 
    :import  "aries.core::AestheticSinkMoliseView.xdsl"
    :context [developed-land transport-infrastructure]
    :keep    [aestheticService:VisualBlight]
    :result  view-sink-undiscretizer))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;molise:Trails
(defmodel hiking-use HikingUse
  "Trail network at the farm."
 (binary-coding HikingUse))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;; molise:DEM_molise_20m
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data-homeowners aestheticService:LineOfSight
  (identification aestheticService:LineOfSight
    :context [source sink hiking-use altitude]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel view aestheticService:AestheticViewsheds
  (span aestheticService:LineOfSight 
        aestheticService:ViewSource
        HikingUse
        aestheticService:ViewSink
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
        :downscaling-factor 4
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/recreation_ontario_data.clj")
        ;; need to add additional context(s) for use
        :context [source sink hiking-use altitude]
        :keep    [aestheticService:TheoreticalSource
                  aestheticService:TheoreticalSink
                  aestheticService:TheoreticalUse
                  aestheticService:PossibleFlow
                  aestheticService:PossibleSource
                  aestheticService:PossibleUse
                  aestheticService:ActualFlow
                  aestheticService:ActualSource
                  aestheticService:ActualSink
                  aestheticService:ActualUse
                  aestheticService:InaccessibleSource
                  aestheticService:InaccessibleSink
                  aestheticService:InaccessibleUse
                  aestheticService:BlockedFlow
                  aestheticService:BlockedSource
                  aestheticService:BlockedUse]))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------
