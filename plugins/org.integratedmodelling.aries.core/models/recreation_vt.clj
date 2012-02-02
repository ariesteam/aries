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
;;; Recreation model for Vermont
;;;
;;; Valid Contexts: core.contexts.vermont/{vt,raven-ridge}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.recreation-vt
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking identification
                           bayesian count])
  (:refer aries :only [span]))

(namespace-ontology recreationService
  (representation:GenericObservable
   (Lake
    (LakeAbsent)
    (LakePresent))
   (Mountain
    (NoMountain)
    (SmallMountain)
    (LargeMountain))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;;Need a new defmodel statement combining lake & river into "waterview," with rivers as low quality and
;; lakes as high.
(defmodel lake aestheticService:Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (binary-coding geofeatures:Lake)
    0          aestheticService:LakeAbsent
    :otherwise aestheticService:LakePresent))

(defmodel river-stream RiverStream
  "Presence of a river or stream."
  (classification (binary-coding geofeatures:River)
    0          RiverStreamAbsent
    :otherwise RiverStreamPresent))

(defmodel mountain aestheticService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement geophysics:Altitude "m")
    [914 8850] aestheticService:LargeMountain ; No higher than Mt. Everest, catches artifacts
    [457  914] aestheticService:SmallMountain ; 
    :otherwise aestheticService:NoMountain))  ; Will catch artifacts too         

(defmodel open-space OpenSpaceClass
  "Classifies an area as open space according to NLCD 2001 data"
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{81 82}       AgriculturalLand
    #{41 42 43}    ForestedLand
    #{31 90 95 52} OtherOpenLand
    :otherwise     NotOpenLand))

;;This statement (not yet implemented) accounts for public recognition of a site, e.g., SPRNCA & Ramsy Canyon's high
;; values for birding, or Mt. Mansfield & Camels Hump's high values for hiking in the VT model.  Need to develop a
;; data layer that has these features, link nodes in the BN and finish the CPT, and include the correct concepts in
;; the defmodel statement below, then test results.
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
(defmodel source aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian aestheticService:AestheticEnjoymentProvision 
    :import  "aries.core::RecreationSourceVtView.xdsl"
    :context [lake river-stream mountain open-space]
    :keep    [aestheticService:TheoreticalNaturalBeauty]
    :result  theoretical-beauty))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;;Includes development, clearcuts, roads, energy infrastructure. Should
;;we remove low-density development from this list?  Can we really
;;call views of small towns from mountain summits visually
;;displeasing?
(defmodel development Development
  "Development as defined by the NLCD 2001"
  (classification (numeric-coding nlcd:NLCDNumeric)
    24         HighIntensityDevelopment
    23         MediumIntensityDevelopment
    22         LowIntensityDevelopment
    :otherwise NotDeveloped))

(defmodel transportation-energy-infrastructure-code TransportationEnergyInfrastructureCode
  (binary-coding TransportationEnergyInfrastructureCode
    :context [(binary-coding infrastructure:Road)
              (binary-coding infrastructure:TransmissionLine)]
    :state   #(if (or (= (:road %) 1)
                      (= (:transmission-line%) 1))
                1
                0))) 
(defmodel transportation-energy-infrastructure TransportationEnergyInfrastructure
  (classification transportation-energy-infrastructure-code
    1 TransportationEnergyInfrastructurePresent
    0 TransportationEnergyInfrastructureAbsent))                        

(defmodel visual-blight aestheticService:VisualBlight
  (probabilistic-ranking aestheticService:VisualBlight
    [90 100] aestheticService:HighBlight
    [50  90] aestheticService:ModerateBlight
    [10  50] aestheticService:LowBlight
    [ 0  10] aestheticService:NoBlight))

(defmodel sink aestheticService:ViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian aestheticService:ViewSink 
    :import  "aries.core::RecreationSinkVtView.xdsl"
    :context [development transportation-energy-infrastructure]
    :keep    [aestheticService:VisualBlight]
    :result  visual-blight))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; ViewPosition, TravelTime, PublicAccess, HikingDistance, HikingSlope

(defmodel view-position ViewPositionClass
  "Location of a view point, a function of elevation."
  (classification (measurement ViewPosition "m")
    [914  :>] HighViewPosition
    [457 914] MediumViewPosition
    [0   457] LowViewPosition))

(defmodel travel-time TravelTimeClass
  "Travel time to hiking resources"
  (classification (ranking TravelTime)
    1 ShortTravelTime
    2 ModerateTravelTime
    3 LongTravelTime))

(defmodel public-access PublicAccessClass
  "describes access constraints to a particular parcel"
  (classification (ranking PublicAccess)
    0 PublicLand
    1 PrivateLandWithAccess
    2 NoPublicAccess)) 

(defmodel hiking-distance HikingDistanceClass
  "Refers to trail distance between the starting point and the view point"
  (classification (ranking HikingDistance)
    1 ShortHikingDistance
    2 ModerateHikingDistance
    3 LongHikingDistance))

(defmodel hiking-slope HikingSlopeClass
  "describes the steepness of the hiking trail"
  (classification (measurement HikingSlope "\u00b0")
    [:< 10] LowSlope
    [10 45] ModerateSlope
    [45 :>] SteepSlope))

(defmodel viewer-enjoyment ViewerEnjoyment
  (probabilistic-ranking ViewerEnjoyment
    [67 100] HighViewerEnjoyment
    [33  67] ModerateViewerEnjoyment
    [ 0  33] LowViewerEnjoyment))

;; bayesian model
(defmodel user ViewerEnjoyment
  "Views afforded to recreational users"
  (bayesian ViewerEnjoyment
    :import  "aries.core::RecreationViewUse.xdsl"
    :context [view-position travel-time public-access hiking-distance hiking-slope]
    :keep    [ViewerEnjoyment]
    :result  viewer-enjoyment))

(defmodel population-density policytarget:PopulationDensity
  (count policytarget:PopulationDensity "/km^2"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))        

(defmodel data OutdoorRecreation 
  (identification aestheticService:AestheticEnjoyment
    :context [source sink altitude population-density]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; The "first stage" flow model calculates view quality from view points, and passes results to the recreation
;;   flow model, which moves people toward those points.    
;;(defmodel view aestheticService:AestheticView
;;  (span aestheticService:LineOfSight 
;;        aestheticService:AestheticViewProvision
;;        aestheticService:ViewUse
;;        aestheticService:ViewSink
;;        nil
;;        (geophysics:Altitude)
;;        :source-threshold   25.0  ; Excludes LowNaturalBeauty
;;        :sink-threshold     25.0  ; Excludes LowBlight
;;        :use-threshold       0.2  ; Excludes HomeownerViewUseAbsent
;;        :trans-threshold     1.0
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 1
;;        :rv-max-states      10
;;        :animation?         true
;;:save-file          (str (System/getProperty "user.home") "/aesthetic_view_san_pedro_data.clj")
;;        :context [source viewpoints sink altitude] ; Need to create the viewpoints layer, showing mountain views.
;;        :keep    [aestheticService:ActualFlow
;;                  aestheticService:ActualSource
;;                  aestheticService:ActualSink
;;                  aestheticService:ActualUse]))

;; The "second stage" flow model moves people to mountain summits with views.
;;(defmodel recreation-flow-mountain-view MountainViewUse
;;  (span MountainViewAccessAndUse
;;        MountainViewSourceValue
;;        MountainViewDemand        ; Need to create this model
;;        nil
;;        nil
;;        nil                  ; May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :context [source-mountain-view population-density roads] ;Replace with final use concept
;;        :keep    [TheoreticalSource
;;                  TheoreticalUse
;;                  ActualFlow
;;                  ActualSource
;;                  ActualUse
;;                  InaccessibleSource
;;                  InaccessibleUse]))
