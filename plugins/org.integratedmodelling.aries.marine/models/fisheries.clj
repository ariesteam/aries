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
;;; UNEP marine project
;;; Models for subsistence fisheries in Madagascar
;;;
;;; Valid Contexts: core.contexts.beta/mg*
;;;
;;;-------------------------------------------------------------------

(ns marine.models.fisheries
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization ranking
                           namespace-ontology numeric-coding
                           binary-coding identification
                           probabilistic-measurement bayesian count])
  (:refer aries :only [span]))

(namespace-ontology fisheries)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; Statements below estimate harvest by pixel of three species of
;; marine pelagic fishes valued for human consumption. Assumptions
;; about the harvested quantity of each species are documented in the
;; ARIES modeling guide. These values can be adjusted as needed with
;; improved data or expert knowledge.
(defmodel slender-emperor-harvest LethrinusBorbonicusHarvest
  "Estimates the subsistence harvest of Lethrinus borbonicus in
   kg/km^2*year."
  (measurement LethrinusBorbonicusHarvest "kg/km^2*year"
    :context [(ranking LethrinusBorbonicusAbundanceMg)]
    :state   #(if-let [abundance (:lethrinus-borbonicus-abundance-mg %)]
                (* abundance 8712431))))

(defmodel sky-emperor-harvest LethrinusMahsenaHarvest
  "Estimates the subsistence harvest of Lethrinus mahsena in
   kg/km^2*year."
  (measurement LethrinusMahsenaHarvest "kg/km^2*year"
    :context [(ranking LethrinusMahsenaAbundanceMg)]
    :state   #(if-let [abundance (:lethrinus-mahsena-abundance-mg %)]
                (* abundance 8712431))))

(defmodel mangrove-red-snapper-harvest LutjanusArgentimaculatusHarvest
  "Estimates the subsistence harvest of Lutjanus argentimaculatus in
   kg/km^2*year."
  (measurement LutjanusArgentimaculatusHarvest "kg/km^2*year"
    :context [(ranking LutjanusArgentimaculatusAbundanceMg)]
    :state   #(if-let [abundance (:lutjanus-argentimaculatus-abundance-mg %)]
                (* abundance 8712431))))

(defmodel total-pelagic-subsistence-harvest TotalSubsistenceHarvest
  "Estimates the total subsistence harvest of marine pelagic fishes in
   kg/km^2*year."
  (measurement TotalSubsistenceHarvest "kg/km^2*year"
    :context [slender-emperor-harvest sky-emperor-harvest mangrove-red-snapper-harvest]
    :state   #(reduce +
                      (remove nil?
                              [(:lethrinus-borbonicus-harvest      %)
                               (:lethrinus-mahsena-harvest         %)
                               (:lutjanus-argentimaculatus-harvest %)]))))

;; KB, 8/11/10: Statements below are to link habitat change to fish
;; change. This is not part of the 1st generation flow models, and
;; could be added to subsequent marine modeling work.
;;
;; Most polygons do not report any data on bleaching, these are placed
;; under "moderate bleaching".  There's a misspelling for "HIgh" in
;; the data that's accounted for in the discretization below.
(defmodel coral-quality CoralQuality
  "Reclasses coral bleaching into 4 discrete categories."
  (classification (categorization coastalProtection:CoralBleaching)
    #{"None" "Low"}  MinimallyBleachedCoralPresent
    #{"Moderate" ""} ModeratelyBleachedCoralPresent
    #{"HIgh" "High"} HighlyBleachedCoralPresent
    :otherwise       NoCoralPresent))

(defmodel reef-area CoralReefAreaClass
  "Reclasses coral reef area into 4 discrete categories."
  (classification (measurement CoralReefArea "km^2")
    [250 :>]          HighReefArea
    [25 250]          ModerateReefArea
    [:exclusive 0 25] LowReefArea
    :otherwise        NoReefArea))

(defmodel estuary-area EstuaryAreaClass
  "Reclasses estuary area into 3 discrete categories."
  (classification (categorization EstuaryArea)
    "Large"    LargeEstuary
    "Small"    SmallEstuary
    :otherwise NoEstuary))

(defmodel nitrogen NitrogenRunoff
  "Reclasses nitrogen runoff into 4 discrete categories."
  (classification (measurement policytarget:NitrogenFromFertilizerAndManure "kg/ha*year")
    [90 :>] HighNitrogenRunoff
    [40 90] ModerateNitrogenRunoff
    [15 40] LowNitrogenRunoff
    [:< 15] NoNitrogenRunoff))

(defmodel fish-habitat-quality FishHabitat
  "Computes the discrete marginal distribution of FishHabitat from a
   Bayesian network, conditioned on the variables in the context
   list."
  (bayesian FishHabitat
    :import  "aries.marine::unused/FisheriesA_hololepidotus.xdsl"
    :context [coral-quality reef-area estuary-area nitrogen]
    :keep    [ReefQuality EstuaryQuality]))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; FV FIXME The distance to coast thing urgently needs to reclassify a
;; proper distance layer or algorithm with actual numbers and not
;; specific of fisheries. The discretization should be bounded so that
;; non-coastal proximate areas return nulls, so it can be used to clip
;; the bayesian network instead of having to write a
;; "subsistence-selector" as below.
(defmodel coastal-proximity CoastalProximity
  "Reclasses distance to coast into 3 discrete categories."
  (classification (measurement DistanceToCoast "km")
    1       HighCoastalProximity ; Consider revisiting these numbers
                                 ; based on Halpern et al. (2008):
                                 ; might go 5-10-25
    5       ModerateCoastalProximity
    [25 :>] LowCoastalProximity))

(defmodel poverty Poverty
  "Reclasses povery percentage into 3 discrete categories."
  (classification (ranking policytarget:PovertyPercentage)
    [50 :>] HighPoverty
    [25 50] ModeratePoverty
    [:< 25] LowPoverty))

(defmodel population-density-class PopulationDensityClass
  "Reclasses population density into 5 discrete categories."
  (classification (count policytarget:PopulationDensity "/km^2")
    [2000 :>]   VeryHighPopulationDensity
    [1000 2000] HighPopulationDensity
    [200 1000]  ModeratePopulationDensity
    [50 200]    LowPopulationDensity
    [:< 50]     VeryLowPopulationDensity))

;; Assume high subsistence use = per capita demand of 6.8 kg fish/yr,
;; moderate use = 4.6 kg fish/yr low use = 2.3 kg fish/yr. This
;; calculates total demand.
;;
;; FIXME: Evil hack warning! Ferd doesn't have any way in his
;; modelling API for CategoricalDistributionDatasource to extract the
;; undiscretized values of a deterministic distribution.  Therefore,
;; we're going to be silly and just make them into ranges centered
;; around the values we want. My code (Gary) will just take their
;; midpoints anyway, so it's no big deal.
(defmodel subsistence-fishing-undiscretized SubsistenceUse
  "Maps SubsistenceUse subconcepts to numeric value ranges in
   kg/km^2*year."
  (probabilistic-measurement SubsistenceUse "kg/km^2*year" ; per person, multiply by population density.
    [ 5.6 8.0] HighSubsistenceUse      ; 6.8
    [ 3.6 5.6] ModerateSubsistenceUse  ; 4.6
    [ 1.0 3.6] LowSubsistenceUse       ; 2.3
    [-1.0 1.0] NoSubsistenceUse))      ; 0.0

;;(defmodel subsistence-fishing-undiscretized SubsistenceUse
;;  (classification SubsistenceUse
;;    :units "kg/km^2*year"
;;    :context [(count policytarget:PopulationDensity "/km^2")]
;;    ;; This classification syntax is documented as working but isn't implemented!
;;    (* (:population-density self) 6.8) HighSubsistenceUse
;;    (* (:population-density self) 4.6) ModerateSubsistenceUse
;;    (* (:population-density self) 2.3) LowSubsistenceUse
;;    0                                  NoSubsistenceUse))

;; FIXME this should be removed when the distance to coast model is done properly
(defmodel subsistence-selector ProximityBuffer
  "Indicates whether or not the CoastalProximity concept is defined in a location."
  (classification ProximityBuffer
    :context [coastal-proximity]
    :state   #(if-let [proximity-class (:coastal-proximity %)]
                (if (or (= proximity-class (tl/conc 'fisheries:HighCoastalProximity))
                        (= proximity-class (tl/conc 'fisheries:ModerateCoastalProximity))
                        (= proximity-class (tl/conc 'fisheries:LowCoastalProximity)))
                  (tl/conc 'fisheries:ProximityBufferPresent)))))

(defmodel subsistence-fishing SubsistenceFishing
  "Computes the discrete marginal distribution of SubsistenceFishing
   from a Bayesian network, conditioned on the variables in the
   context list."
  (bayesian SubsistenceFishing
    :import   "aries.marine::FisheriesSubsistenceUseMg.xdsl"
    :context  [poverty population-density-class coastal-proximity subsistence-selector]
    :required [ProximityBuffer] ; FIXME substitute with distance to coast when it's correctly written and based on decent data
    :keep     [SubsistenceUse]
    :result   subsistence-fishing-undiscretized))

;;;-------------------------------------------------------------------
;;; Flow data models
;;;-------------------------------------------------------------------

(defmodel paths infrastructure:Path
  "Indicates presence of paths."
  (binary-coding infrastructure:Path))

(defmodel population-density PopulationDensity
  "Returns the population density in people/km^2."
  (count policytarget:PopulationDensity "/km^2"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel fisheries-subsistence-data SubsistenceFisheries
  "Runs all the toplevel source, use, and flow data models and
   collects their results."
  (identification SubsistenceFishProvision
    :context [total-pelagic-subsistence-harvest
              subsistence-fishing
              paths
              population-density]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel fisheries-ass-saver SubsistenceFisheries
  "Ferd's legendary fisheries SPANK model."
  (modelling/spank SubsistenceFishProvision
                   :context [total-pelagic-subsistence-harvest subsistence-fishing paths population-density]))

(defmodel fisheries-subsistence-flow SubsistenceFisheries
  "Computes SPAN flow results for subsistence fisheries."
  (span SubsistenceFishAccessibility
        TotalSubsistenceHarvest
        SubsistenceFishing
        nil
        nil
        (infrastructure:Path PopulationDensity)
        :source-threshold   0.0
        :sink-threshold     nil
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          nil
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/subsistence-fisheries-data.clj")
        :context            [total-pelagic-subsistence-harvest subsistence-fishing paths population-density]
        :keep               [TheoreticalSource
                             TheoreticalUse
                             ActualFlow
                             ActualSource
                             ActualUse
                             InaccessibleSource
                             InaccessibleUse]))