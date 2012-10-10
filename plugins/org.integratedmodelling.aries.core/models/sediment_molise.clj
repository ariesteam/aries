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
;;; Sediment regulation model for Ontario
;;;
;;; Valid Contexts: core.contexts.ontario/lakeofthewoods-wgs84
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
  (owl:Thing (AllSedimentData)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(declare annual-sediment-source
         annual-sediment-source-class
         sediment-vegetation-type
         successional-stage
         percent-tree-canopy-cover-class
         annual-runoff-class
         annual-runoff
         annual-precipitation
         annual-snowmelt
         annual-evapotranspiration
         slope-class
         soil-drainage-class)

;; ontario:lulc2000_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel sediment-vegetation-type ontario:SedimentVegetationType
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{11 12 13 15 16 17 18 19 20 21 22 23} ontario:ForestGrasslandWetland
    #{24 25}                               ontario:ShrublandPasture
    #{7 8 9 10}                            ontario:ImpairedForest
    #{3 4 5 6 27}                          ontario:CropsBarrenDeveloped))

;; ontario:successional_stage_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    6 OldGrowth
    5 LateSuccession
    4 MidSuccession
    3 PoleSuccession
    2 EarlySuccession
    1 NoSuccession))

;; ontario:canopy_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel percent-tree-canopy-cover-class ontario:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [0.80 1.00 :inclusive] ontario:VeryHighCanopyCover
    [0.60 0.80]            ontario:HighCanopyCover
    [0.30 0.60]            ontario:ModerateCanopyCover
    [0.05 0.30]            ontario:LowCanopyCover
    [0.00 0.05]            ontario:VeryLowCanopyCover))

;; ontario:precipitation_low
(defmodel annual-precipitation AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; ontario:snow_low
(defmodel annual-snowmelt AnnualSnowmelt
  (measurement habitat:AnnualSnowmelt "mm"))

;; ontario:evapotranspiration_low
(defmodel annual-evapotranspiration AnnualEvapotranspiration
  (measurement habitat:Evapotranspiration "mm"))

(defmodel annual-runoff AnnualRunoff
  (measurement AnnualRunoff "mm"
    :context [annual-precipitation
              annual-snowmelt
              annual-evapotranspiration]
    :state   #(let [p (or (:annual-precipitation %)      0.0)
                    s (or (:annual-snowmelt %)           0.0)
                    e (or (:annual-evapotranspiration %) 0.0)]
                (- (+ p s) e))))

;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel annual-runoff-class ontario:AnnualRunoffClass
  (classification annual-runoff
    [2400   :>] ontario:VeryHighMeanAnnualRunoff
    [1800 2400] ontario:HighMeanAnnualRunoff
    [1200 1800] ontario:ModerateMeanAnnualRunoff
    [ 600 1200] ontario:LowMeanAnnualRunoff
    [   0  600] ontario:VeryLowMeanAnnualRunoff))

;; ontario:slope20m_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel slope-class SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15]            Level
    [ 1.15  4.57]            GentlyUndulating
    [ 4.57 16.70]            RollingToHilly
    [16.70 90.00 :inclusive] SteeplyDissectedToMountainous))

;; ontario:soil_drainage_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel soil-drainage-class ontario:SoilDrainageClass
  (classification (ranking ontario:SoilDrainageCode)
    2 ontario:PoorlyDrainedSoils))

(defmodel annual-sediment-source-class AnnualSedimentSourceClass
  (probabilistic-measurement AnnualSedimentSourceClass "t/ha"
    [100.00 300.00] HighAnnualSedimentSource
    [ 30.00 100.00] ModerateAnnualSedimentSource
    [  0.01  30.00] LowAnnualSedimentSource
    [  0.00   0.01] NoAnnualSedimentSource))

(defmodel annual-sediment-source AnnualSedimentSource
  (bayesian AnnualSedimentSource
    :import   "aries.core::SedimentSourceOntario.xdsl"
    :context  [sediment-vegetation-type
               successional-stage
               percent-tree-canopy-cover-class
               annual-runoff-class
               slope-class
               soil-drainage-class]
    :required [ontario:SedimentVegetationType]
    :keep     [AnnualSedimentSourceClass]
    :result   annual-sediment-source-class))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare stream-gradient-class
         ;; dam-storage
         floodplains
         floodplain-tree-canopy-cover
         floodplain-tree-canopy-cover-class
         annual-sediment-sink-class
         annual-sediment-sink)

;; ontario:stream_gradient_low
(defmodel stream-gradient-class StreamGradientClass
  (classification (measurement habitat:StreamGradient "\u00b0")
    [2.86   :>] HighStreamGradient
    [1.15 2.86] ModerateStreamGradient
    [:<   1.15] LowStreamGradient))

;; ontario:dams_low
;; (defmodel dam-storage floodService:DamStorage
;;   (measurement floodService:DamStorage "mm"))

;; ontario:floodplains_low
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
    :import   "aries.core::SedimentSinkOntario.xdsl"
    :context  [stream-gradient-class floodplains floodplain-tree-canopy-cover-class]
    :keep     [FloodplainSedimentSinkClass]
    :result   floodplain-sediment-sink-class))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(declare deposition-prone-farmers)

;; ontario:floodplain_farmland_low
(defmodel deposition-prone-farmers DepositionProneFarmers
  (binary-coding DepositionProneFarmers))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

(declare altitude
         river
         floodplains-code)

;; ontario:dem20m_low
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;; ontario:hydrography_low
(defmodel river geofeatures:River
  (binary-coding geofeatures:River))

;; ontario:floodplains_low
(defmodel floodplains-code FloodplainsCode
  (binary-coding FloodplainsCode))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel all-sediment-data AllSedimentData
  (identification AllSedimentData
    :context [annual-sediment-source
              annual-sediment-sink
              deposition-prone-farmers
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
        (geophysics:Altitude geofeatures:River FloodplainsCode) ; we don't have information on levees
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    10.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/beneficial_sediment_transport_ontario_data.clj")
        :context [annual-sediment-source annual-sediment-sink deposition-prone-farmers altitude river floodplains-code]
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
