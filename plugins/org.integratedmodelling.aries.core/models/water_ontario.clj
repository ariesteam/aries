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
;;; Valid Contexts: core.contexts.ontario/lakeofthewoods-wgs84
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

(declare annual-precipitation
         annual-snowmelt
         annual-runoff)

;; Annual precipitation interpolated from the National Climate Data
;; and Information Archive.
;; http://climate.weatheroffice.gc.ca/climateData/canada_e.html
;; precipitation_low
(defmodel annual-precipitation AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; Annual snowmelt interpolated from the National Climate Data
;; and Information Archvie.
;; http://climate.weatheroffice.gc.ca/climateData/canada_e.html
;; Snowmelt is converted to its rainfall equivalent so that it can
;; be summed with the AnnualPrecipitation value to estimate
;; TotalPrecipitation.
;; snow_low
(defmodel annual-snowmelt AnnualSnowmelt
  (measurement habitat:AnnualSnowmelt "mm"))

;; Total Precipitation as the sum of AnnualPrecipitation and
;; AnnualSnowmelt.
(defmodel annual-runoff AnnualRunoff
  (measurement AnnualRunoff "mm"
    :context [annual-precipitation annual-snowmelt]
    :state   #(let [p (:annual-precipitation %)
                    s (:annual-snowmelt      %)]
                (+ (or s 0)
                   (or p 0)))))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare water-supply-vegetation-type
         percent-tree-canopy-cover-class
         soil-drainage-class
         slope-class
         percent-impervious-cover-class
         evapotranspiration-class
         evapotranspiration
         soil-infiltration-class
         soil-infiltration
         surface-water-sink)

;; Still need to deal with land cover types: 4, 6, 24 28, 29
;; need to check if these land cover classes are present
;; in the Lake of the Woods region
;; lulc2000_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel water-supply-vegetation-type ontario:WaterSupplyVegetationType
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{1 2}                        ontario:NotVegetated
    #{25 27}                      ontario:Agriculture
    #{3 5}                        ontario:UrbanInfrastructureRock
    #{15 16 17 18 19 20 21 22 23} ontario:BogFenMarshSwamp
    #{9 10 11 12 13}              ontario:Forest
    #{7 8}                        ontario:ImpairedForest))

;; canopy_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel percent-tree-canopy-cover-class ontario:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [0.80 1.00 :inclusive] ontario:VeryHighCanopyCover
    [0.60 0.80]            ontario:HighCanopyCover
    [0.30 0.60]            ontario:ModerateCanopyCover
    [0.05 0.30]            ontario:LowCanopyCover
    [0.00 0.05]            ontario:VeryLowCanopyCover))

;; This data is problematic because there is only a single polygon
;; with an actual value.
;; soil_drainage_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel soil-drainage-class ontario:SoilDrainageClass
  (classification (ranking ontario:SoilDrainageCode)
    2  ontario:PoorlyDrainedSoils))

;; slope20m_low
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel slope-class SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00B0")
    [    0  1.15]            Level
    [ 1.15  4.57]            GentlyUndulating
    [ 4.57 16.70]            RollingToHilly
    [16.70 90.00 :inclusive] SteeplyDissectedToMountainous))

;; FIXME: ontario-lulc:PercentImperviousSurface runs from 0 to 2.088. WTF?
;; FIXME: This reclassing loses a lot of cells to nodata. Why?
(defmodel percent-impervious-cover-class PercentImperviousCoverClass
  (classification (ranking ontario-lulc:PercentImperviousSurface)
    [0.80 1.00 :inclusive] VeryHighImperviousCover
    [0.50 0.80]            HighImperviousCover
    [0.20 0.50]            ModeratelyHighImperviousCover
    [0.10 0.20]            ModeratelyLowImperviousCover
    [0.05 0.10]            LowImperviousCover
    [0.00 0.05]            VeryLowImperviousCover))

(defmodel evapotranspiration-class EvapotranspirationClass
  (probabilistic-measurement EvapotranspirationClass "mm"
    [180 260] VeryHighEvapotranspiration
    [100 180] HighEvapotranspiration
    [ 50 100] ModerateEvapotranspiration
    [  0  50] LowEvapotranspiration
    [  0   0] VeryLowEvapotranspiration))

(defmodel evapotranspiration Evapotranspiration
  (bayesian Evapotranspiration
    :import  "aries.core::SurfaceWaterSinkOntario.xdsl"
    :context [water-supply-vegetation-type percent-tree-canopy-cover-class]
    :keep    [EvapotranspirationClass]
    :result  evapotranspiration-class))

(defmodel soil-infiltration-class SoilInfiltrationClass
  (probabilistic-measurement SoilInfiltrationClass "mm"
    [180 260] VeryHighInfiltration
    [100 180] HighInfiltration
    [ 50 100] ModerateInfiltration
    [  0  50] LowInfiltration
    [  0   0] VeryLowInfiltration))

(defmodel soil-infiltration SoilInfiltration
  (bayesian SoilInfiltration
    :import  "aries.core::SurfaceWaterSinkOntario.xdsl"
    :context [soil-drainage-class slope-class percent-impervious-cover-class]
    :keep    [SoilInfiltrationClass]
    :result  soil-infiltration-class))

(defmodel surface-water-sink SurfaceWaterSink
  (measurement SurfaceWaterSink "mm"
    :context [soil-infiltration evapotranspiration]
    :state   #(let [s (:soil-infiltration  %)
                    e (:evapotranspiration %)]
                (+
                 (if (nil? s) 0.0 (.getMean s))
                 (if (nil? e) 0.0 (.getMean e))))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(declare residential-surface-water-use)

;; RESIDENTIAL
;; Canadians use an average of 1600 cubic meters of water per person
;; per year. See
;; http://www.environmentalindicators.com/htdocs/indicators/6wate.htm
;; population_density_low
(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm" ;;This is an annual value
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(* 1.6 (or (:population-density %) 0))))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

(declare altitude
         river)

;; dem20m_low
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;; hydrography_low
(defmodel river geofeatures:River
  (binary-coding geofeatures:River))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel all-water-data ontario:AllWaterData
  (identification ontario:AllWaterData
    :context [annual-runoff
              surface-water-sink
              residential-surface-water-use
              altitude
              river]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------
