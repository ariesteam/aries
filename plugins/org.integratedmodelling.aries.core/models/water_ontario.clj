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
;;; Water supply model for Ontario
;;;
;;; Valid Contexts: core.contexts.ontario/*
;;;
;;;-------------------------------------------------------------------

(ns core.models.water-ontario
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

(namespace-ontology waterSupplyService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------
;; Annual precipitation interpolated from the National Climate Data
;; and Information Archive.
;; http://climate.weatheroffice.gc.ca/climateData/canada_e.html
(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; Annual snowfall interpolated from the National Climate Data
;; and Information Archvie.
;; http://climate.weatheroffice.gc.ca/climateData/canada_e.html
;; Snowfall is converted to its rainfall equivalent so that it can
;; be summed with the AnnualPrecipitation value to estimate
;; TotalPrecipitation.

(defmodel snowfall-annual AnnualSnowfall
  (measurement habitat:AnnualSnowmelt "cm"))

;; Total Precipitation as the sum of AnnualPrecipitation and
;; AnnualSnowfall.
(defmodel runoff AnnualRunoff
  (measurement habitat:AnnualRunoff "mm"
    :context [precipitation-annual snowfall-annual]
    :state #(+ (:precipitation-annual %)
               (or (:snowfall-annual  %) 0.0))))
;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------
;; Still need to deal with land cover types: 4, 6, 24 28, 29
;; need to check if these land cover classes are present
;; in the Lake of the Woods region
;;(defmodel land-cover-type ontario:WaterSupplyVegetationType
;;  "Reclass of MNR Land cover data"
;;  (classification (numeric-coding ontario:MNRLC)
;;  #{1 2}                                                    ontario:Water
;;  #{25 27}                                                  ontario:Agriculture
;;  #{3 5}                                                    ontario:UrbanInfrastructureRock
;;  #{15 16 17 18 19 20 21 22 23}                             ontario:BogFenMarshSwamp
;;  #{9 10 11 12 13}                                          ontario:Forest
;;  #{7 8}                                                    ontario:ImpairedForest))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive]   VeryHighCanopyCover
    [60 80]               HighCanopyCover
    [30 60]               ModerateCanopyCover
    [ 5 30]               LowCanopyCover
    [ 0  5]               VeryLowCanopyCover))

;;(defmodel soil-drainage HydrologicSoilsGroup
;;  "this data is problematic because there is only a single polygon with data"
;;(classification (numeric-coding waterSupplyService:HydrologicSoilsGroup)
;;  1                  SoilGroupA
;;  2                  SoilGroupB
;;  3                  SoilGroupC
;;  4                  SoilGroupD))

(defmodel percent-impervious-cover PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousSurface)
    [80 100 :inclusive] VeryHighImperviousCover
    [50 80]             HighImperviousCover
    [20 50]             ModeratelyHighImperviousCover
    [10 20]             ModeratelyLowImperviousCover
    [ 5 10]             LowImperviousCover
    [ 0  5]             VeryLowImperviousCover))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00B0")
    [    0  1.15]         Level
    [ 1.15  4.57]         GentlyUndulating
    [ 4.57 16.70]         RollingToHilly
    [16.70 90 :inclusive] SteeplyDissectedToMountainous))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; RESIDENTIAL
(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm" ;;This is an annual value
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(* 0.8 0.082855 (:population-density %))))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams-simple geofeatures:River
  (binary-coding geofeatures:River))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------