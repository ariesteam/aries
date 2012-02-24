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

(ns core.models.sediment-ontario
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

(namespace-ontology soilRetentionService)

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

;; lulc2000_low
(defmodel sediment-vegetation-type ontario:SedimentVegetationType
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{4 5} ontario:ForestGrasslandWetland
    #{6}   ontario:ShrublandPasture
    #{2 3} ontario:CropsBarrenDeveloped))

;; successional_stage_low
(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    6 OldGrowth
    5 LateSuccession
    4 MidSuccession
    3 PoleSuccession
    2 EarlySuccession
    1 NoSuccession))

;; canopy_low
(defmodel percent-tree-canopy-cover-class ontario:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    5 ontario:VeryHighCanopyCover
    4 ontario:HighCanopyCover
    3 ontario:ModerateCanopyCover
    2 ontario:LowCanopyCover
    1 ontario:VeryLowCanopyCover))

;; precipitation_low
(defmodel annual-precipitation AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; snow_low
(defmodel annual-snowmelt AnnualSnowmelt
  (measurement habitat:AnnualSnowmelt "mm"))

;; evapotranspiration_low
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

(defmodel annual-runoff-class ontario:AnnualRunoffClass
  (classification annual-runoff
    [2400   :>] ontario:VeryHighMeanAnnualRunoff
    [1800 2400] ontario:HighMeanAnnualRunoff
    [1200 1800] ontario:ModerateMeanAnnualRunoff
    [ 600 1200] ontario:LowMeanAnnualRunoff
    [   0  600] ontario:VeryLowMeanAnnualRunoff))

;; slope20m_low
(defmodel slope-class SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

;; soil_drainage_low
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
    :import   "aries.core::SedimentSourceOntarioAdHoc.xdsl"
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

;; CapturedSediment = f(Slope,Dams,StreamGradient)

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; SedimentUsers = f(PopulationDensity,Dams)

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;; SedimentRouting = f(DEM,Hydrography)

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------
