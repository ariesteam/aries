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

(ns core.models.recreation-ontario
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

(declare mountain
         river-stream
         lake
         open-space
         theoretical-beauty)

;;ontario:dem10m_alg
(defmodel mountain aestheticService:Mountain
  "Classifies elevation data into three levels of service provision."
  (classification (measurement geophysics:Altitude "m")
    [540 8850]   aestheticService:LargeMountain
    [500  540]   aestheticService:SmallMountain
    :otherwise    aestheticService:NoMountain))

;;ontario:hydrography_alg
(defmodel river-stream RiverStream
  "Identifies the presence of a river or stream"
  (classification (binary-coding geofeatures:River)
    0            RiverStreamAbsent
    :otherwise   RiverStreamPresent))

;;ontario:lakes_alg
(defmodel lake aestheticService:Lake
  "Identifies the presence of a lake"
  (classification (binary-coding geofeatures:Lake)
    0            aestheticService:LakeAbsent
    :otherwise   aestheticService:LakePresent))

;;ontario:lulc2000_alg
(defmodel open-space OpenSpaceClass
  "Uses MNR LULC data to identify open space of varying classes."
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{25 27}       AgriculturalLand
    #{11 12 13}    ForestedLand
    #{18 19 21 23} OtherOpenLand
    :otherwise     NotOpenLand))

;;This statement (not yet implemented) would be used to
;;identify significant sites in Algonquin Provincial Park
;;based on user surveys and input from MNR staff
;;(defmodel site-recognition SiteRecognitionClass
;;  (classification (ranking SiteRecognition)
;;     1 HighSiteRecognition
;;     2 ModerateSiteRecognition
;;     3 LowSiteRecognition))

(defmodel theoretical-beauty aestheticService:TheoreticalNaturalBeauty
  (probabilistic-ranking aestheticService:TheoreticalNaturalBeauty
    [75 100] aestheticService:HighNaturalBeauty
    [50  75]  aestheticService:ModerateNaturalBeauty
    [25  50]  aestheticService:LowNaturalBeauty
    [ 0  25]   aestheticService:NoNaturalBeauty))

;; source bayesian model                 
(defmodel source aestheticService:ViewSource
  (bayesian aestheticService:ViewSource
    :import  "aries.core::RecreationSourceOntarioView.xdsl"
    :context [lake river-stream mountain open-space]
    :keep    [aestheticService:TheoreticalNaturalBeauty]
    :result  theoretical-beauty))
;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare clearcuts
         transportation-energy-infrastructure-code
         transportation-energy-infrastructure
         park-infrastructure)

;;ontario:lulc2000_alg
(defmodel clearcuts aestheticService:Clearcuts
   "Uses MNR LULC data to identify open space of varying classes."
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{7 8}       aestheticService:ClearcutsPresent
    :otherwise   aestheticService:ClearcutsAbsent))  

;;ontario:roads_alg
;;ontario:utility_lines_alg
;;ontario:railway_alg
(defmodel transportation-energy-infrastructure-code TransportationEnergyInfrastructureCode
  (binary-coding TransportationEnergyInfrastructureCode
    :context [(binary-coding infrastructure:Road)
              (binary-coding infrastructure:TransmissionLine)
              (binary-coding infrastructure:Railway)]
    :state   #(if (or (= (:road %) 1)
                      (= (:transmission-line %) 1)
                      (= (:railway %) 1))
                1
                0)))

(defmodel transportation-energy-infrastructure TransportationEnergyInfrastructure
  (classification transportation-energy-infrastructure-code
    1          TransportationEnergyInfrastructurePresent
    :otherwise TransportationEnergyInfrastructureAbsent))

;;ontario:park_infrastructure_alg
;; still a problem here that needs to be resolved.
(defmodel park-infrastructure infrastructure:ParkInfrastructureCode
  "Use data supplied by MNR to identify locations within Algonquin Provincial Park where Park-related infrastructure is located."
  (binary-coding infrastructure:ParkInfrastructure))
;;  (classification (binary-coding infrastructure:ParkInfrastructure)
;;    1            infrastructure:ParkInfrastructurePresent
;;    :otherwise   infrastructure:ParkInfrastructureAbsent))

(defmodel view-sink-undiscretizer aestheticService:VisualBlight
  (probabilistic-ranking aestheticService:VisualBlight
    [50 100] aestheticService:HighBlight
    [25  50] aestheticService:ModerateBlight
    [ 5  25] aestheticService:LowBlight
    [ 0   5] aestheticService:NoBlight))

(defmodel sink aestheticService:ViewSink
  "Landscape features that reduce the quality of scenic views"
  (bayesian aestheticService:ViewSink 
    :import  "aries.core::RecreationSinkOntarioView.xdsl"
    :context [clearcuts transportation-energy-infrastructure transportation-energy-infrastructure-code park-infrastructure]
    :keep    [aestheticService:VisualBlight]
    :result  view-sink-undiscretizer))



;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;ontario:canoe_use_alg
(defmodel canoe-use CanoeUse
  "Use data from MNR park surveys where backcountry user indicated a canoe trip."
  (binary-coding CanoeUse))

;;ontario:hiking_use_alg
(defmodel hiking-use HikingUse
  "Use data from MNR park surveys where backcountry user indicated a hiking trip."
 (binary-coding HikingUse))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

;; ontario:dem10m_alg
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel data-homeowners aestheticService:LineOfSight
  (identification aestheticService:LineOfSight
    ;;:context [source canoe-use sink altitude]
    :context [source hiking-use sink altitude]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel view aestheticService:AestheticViewsheds
  (span aestheticService:LineOfSight 
        aestheticService:ViewSource
        ;;CanoeUse
        HikingUse
        ;;aestheticService:ViewUse
        aestheticService:ViewSink
        nil
        (geophysics:Altitude)
        :source-threshold    55.0  ;; Brian: NB, you've probably
        ;; eliminated all sources by using such a high source
        ;; threshold. Reduced threshold from 75 to 25
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
