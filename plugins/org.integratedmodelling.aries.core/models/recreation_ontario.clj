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
;;;
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
    1 TransportationEnergyInfrastructurePresent
    0 TransportationEnergyInfrastructureAbsent))

;;ontario:park_infrastructure_alg
(defmodel park-infrastructure infrastructure:ParkInfrastructureCode
  "Use data supplied by MNR to identify locations within Algonquin Provincial Park where Park-related infrastructure is located."
  (classification (binary-coding infrastructure:ParkInfrastructure)
    1            infrastructure:ParkInfrastructurePresent
    :otherwise   infrastructure:ParkInfrastructureAbsent))



;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------
