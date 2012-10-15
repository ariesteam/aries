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
;;; Sediment regulation model for Molise
;;;
;;; Valid Contexts: core.contexts.molise/molise
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-molise
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

(namespace-ontology soilRetentionService
  (lulc:LandClassificationNumericMapping
   (CorineNumeric))
  (owl:Thing (AllSedimentData))
  (thinklab-core:Categorical
   (SedimentVegetationType
    (ForestGrasslandWetland)
    (ShrublandPasture)
    (MineralExtraction)
    (CropsBarrenDeveloped)))
  (thinklab-core:OrdinalRanking
   (PercentTreeCanopyCoverClass
    (VeryHighCanopyCover)
    (HighCanopyCover)
    (ModerateCanopyCover)
    (LowCanopyCover)
    (VeryLowCanopyCover)))
  (thinklab-core:BooleanRanking
   (DepositionProneLand)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(declare sediment-vegetation-type
         percent-tree-canopy-cover-class
         annual-runoff-class
         soil-erodibility-class
         annual-sediment-source-class
         annual-sediment-source)

;; molise:CorineLandCover
(defmodel sediment-vegetation-type SedimentVegetationType
  (classification (numeric-coding CorineNumeric)
    #{1 2 311 312 313 321 323 221 222 223} ForestGrasslandWetland
    #{231 324}                             ShrublandPasture
    131                                    MineralExtraction
    #{111 112 241 242 243 244}             CropsBarrenDeveloped))

;; molise:CanopyCover
(defmodel percent-tree-canopy-cover-class PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [0.80 1.00 :inclusive] VeryHighCanopyCover
    [0.60 0.80]            HighCanopyCover

    [0.30 0.60]            ModerateCanopyCover
    [0.05 0.30]            LowCanopyCover
    [0.00 0.05]            VeryLowCanopyCover))

;; may use this in the revised model, but there is no data to support
;;this for the current version
;;(defmodel annual-evapotranspiration AnnualEvapotranspiration
;;  (measurement habitat:Evapotranspiration "mm"))

;;(defmodel annual-runoff AnnualRunoff
;;  (measurement AnnualRunoff "mm"
;;    :context [annual-precipitation
;;              annual-snowmelt
;;              annual-evapotranspiration]
;;    :state   #(let [p (or (:annual-precipitation %)      0.0)
;;                    s (or (:annual-snowmelt %)           0.0)
;;                    e (or (:annual-evapotranspiration %) 0.0)]
;;                (- (+ p s) e))))

;; molise:AnnualRunoff
(defmodel annual-runoff-class AnnualRunoffClass
  (classification (measurement AnnualRunoff "mm")
    [800  :>] VeryHighAnnualRunoff
    [600 800] HighAnnualRunoff
    [400 600] ModerateAnnualRunoff
    [200 400] LowAnnualRunoff
    [ 0  200] VeryLowAnnualRunoff))

;; molise:PotentialErosion
(defmodel soil-erodibility-class SoilErodibilityClass
  (classification (ranking SoilErodibility)
    1      VeryLowSoilErodibility
    #{2 5} LowSoilErodibility
    4      ModerateSoilErodibility
    3      HighSoilErodibility
    6      VeryHighSoilErodibility))

(defmodel annual-sediment-source-class AnnualSedimentSourceClass
  (probabilistic-measurement AnnualSedimentSourceClass "t/ha"
    [100.00 300.00] HighAnnualSedimentSource
    [ 30.00 100.00] ModerateAnnualSedimentSource
    [  0.01  30.00] LowAnnualSedimentSource
    [  0.00   0.01] NoAnnualSedimentSource))

(defmodel annual-sediment-source AnnualSedimentSource
  (bayesian AnnualSedimentSource
    :import   "aries.core::SedimentSourceMolise.xdsl"
    :context  [sediment-vegetation-type
               percent-tree-canopy-cover-class
               annual-runoff-class
               soil-erodibility-class]
    :required [SedimentVegetationType]
    :keep     [AnnualSedimentSourceClass]
    :result   annual-sediment-source-class))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare stream-gradient-class
         floodplains
         floodplain-tree-canopy-cover
         floodplain-tree-canopy-cover-class
         floodplain-sediment-sink-class
         annual-sediment-sink)

;; molise:StreamGradient
(defmodel stream-gradient-class StreamGradientClass
  (classification (measurement habitat:StreamGradient "\u00b0")
    [2.86   :>] HighStreamGradient
    [1.15 2.86] ModerateStreamGradient
    [:<   1.15] LowStreamGradient))

;; molise:Floodplain
(defmodel floodplains Floodplains
  (classification (binary-coding FloodplainsCode)
    1 InFloodplain
    0 NotInFloodplain))

(defmodel floodplain-tree-canopy-cover FloodplainTreeCanopyCover
  (ranking FloodplainTreeCanopyCover
    :context [(ranking habitat:PercentTreeCanopyCover)
              (binary-coding FloodplainsCode)]
    :state   #(if (and (:floodplains-code %)
                       (:percent-tree-canopy-cover %))
                (int (* 100 (:percent-tree-canopy-cover %)))
                0)))

(defmodel floodplain-tree-canopy-cover-class FloodplainTreeCanopyCoverClass
  (classification floodplain-tree-canopy-cover
    [80 100 :inclusive] VeryHighFloodplainCanopyCover
    [60  80]            HighFloodplainCanopyCover
    [40  60]            ModerateFloodplainCanopyCover
    [20  40]            LowFloodplainCanopyCover
    [ 0  20]            VeryLowFloodplainCanopyCover))

(defmodel floodplain-sediment-sink-class FloodplainSedimentSinkClass
  (probabilistic-measurement FloodplainSedimentSinkClass "t/ha"
    [100.00 300.00] HighFloodplainSedimentSink
    [ 30.00 100.00] ModerateFloodplainSedimentSink
    [  0.01  30.00] LowFloodplainSedimentSink
    [  0.00   0.01] NoFloodplainSedimentSink))

(defmodel annual-sediment-sink FloodplainSedimentSink
  (bayesian FloodplainSedimentSink
    :import   "aries.core::SedimentSinkMolise.xdsl"
    :context  [stream-gradient-class floodplains floodplain-tree-canopy-cover-class]
    :keep     [FloodplainSedimentSinkClass]
    :result   floodplain-sediment-sink-class))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(declare deposition-prone-land)

;; molise:Floodplain
(defmodel deposition-prone-land DepositionProneLand
  (binary-coding DepositionProneLand))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

(declare altitude
         river
         floodplains-code)

;;  molise:DEM_molise_20m
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;; ;molise:rivers_molise
(defmodel river geofeatures:River
  (binary-coding geofeatures:River))

;; molise:Floodplain
(defmodel floodplains-code FloodplainsCode
  (binary-coding FloodplainsCode))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel all-sediment-data AllSedimentData
  (identification AllSedimentData
    :context [annual-sediment-source
              annual-sediment-sink
              deposition-prone-land
              altitude
              river
              floodplains-code]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel beneficial-sediment-transport BeneficialSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        DepositionProneFarmers
        FloodplainSedimentSink
        nil
        (geophysics:Altitude geofeatures:River FloodplainsCode)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    10.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 2
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/beneficial_sediment_transport_ontario_data.clj")
        :context [annual-sediment-source annual-sediment-sink deposition-prone-land altitude river floodplains-code]
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
