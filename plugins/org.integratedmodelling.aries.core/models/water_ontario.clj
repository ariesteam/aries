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
;;; Valid Contexts: core.contexts.ontario/lakeofthewoods_wgs84
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
  (measurement habitat:AnnualSnowmelt "mm"))

;; Total Precipitation as the sum of AnnualPrecipitation and
;; AnnualSnowfall.
;; I'm able to use the measure command to test this, but the model
;; command returns a Null pointer exception

(defmodel runoff-annual AnnualRunoff
  (measurement habitat:AnnualRunoff "mm"
    :context [precipitation-annual snowfall-annual]
    :state   #(let [p (:annual-precipitation %)
                    s (:annual-snowfall      %)]
                (+ (or s 0)
                   (or p 0)))))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------
;; Still need to deal with land cover types: 4, 6, 24 28, 29
;; need to check if these land cover classes are present
;; in the Lake of the Woods region
(defmodel land-cover-type ontario:WaterSupplyVegetationType
  "Reclass of MNR Land cover data"
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
  #{1 2}                              ontario:NotVegetated
  #{25 27}                            ontario:Agriculture
  #{3 5}                              ontario:UrbanInfrastructureRock
  #{15 16 17 18 19 20 21 22 23}       ontario:BogFenMarshSwamp
  #{9 10 11 12 13}                    ontario:Forest
  #{7 8}                              ontario:ImpairedForest))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive]   VeryHighCanopyCover
    [60 80]               HighCanopyCover
    [30 60]               ModerateCanopyCover
    [ 5 30]               LowCanopyCover
    [ 0  5]               VeryLowCanopyCover))

(defmodel soil-drainage ontario:SoilDrainageClass
  "this data is problematic because there is only a single polygon with an actual value."
  (classification (ranking ontario:SoilDrainageCode)
    2  ontario:PoorlyDrainedSoils))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00B0")
    [    0  1.15]         Level
    [ 1.15  4.57]         GentlyUndulating
    [ 4.57 16.70]         RollingToHilly
    [16.70 90 :inclusive] SteeplyDissectedToMountainous))

(defmodel percent-impervious-cover PercentImperviousCoverClass
  (classification (ranking ontario-lulc:PercentImperviousSurface)
    [80 100 :inclusive] VeryHighImperviousCover
    [50 80]             HighImperviousCover
    [20 50]             ModeratelyHighImperviousCover
    [10 20]             ModeratelyLowImperviousCover
    [ 5 10]             LowImperviousCover
    [ 0  5]             VeryLowImperviousCover))

;;(defmodel evapotranspiration EvapotranspirationClass
;;  (probabilistic-measurement EvapotranspirationClass "mm" 
;;    [180 260] VeryHighEvapotranspiration
;;    [100 180] HighEvapotranspiration
;;    [ 50 100] ModerateEvapotranspiration
;;    [  0  50] LowEvapotranspiration
;;    [  0   0] VeryLowEvapotranspiration))

;;(defmodel et-sink Evapotranspiration
;;  (bayesian Evapotranspiration
;;    :import  "aries.core::SurfaceWaterSinkOntario.xdsl"
;;    :context [land-cover-type percent-canopy-cover]
;;    :keep    [EvapotranspirationClass]
;;    :result  evapotranspiration))

;;(defmodel soil-infiltration SoilInfiltrationClass
;;  (probabilistic-measurement SoilInfiltrationClass "mm" 
;;    [180 260] VeryHighInfiltration
;;    [100 180] HighInfiltration
;;    [ 50 100] ModerateInfiltration
;;    [  0  50] LowInfiltration
;;    [  0   0] VeryLowInfiltration))

;;(defmodel soil-sink SoilInfiltration
;;  (bayesian SoilInfiltration
;;    :import  "aries.core::SurfaceWaterSinkOntario.xdsl"
;;    :context [soil-drainage slope percent-impervious-cover]
;;    :keep    [SoilInfiltrationClass]
;;    :result  soil-infiltration))

;;(defmodel surface-water-sink SurfaceWaterSink
;;  (measurement SurfaceWaterSink "mm"
;;    :context [soil-sink et-sink] 
;;    :state   #(let [si (:soil-infiltration-class  %)
;;                    et (:evapotranspiration-class %)]
;;                (+ 
;;                (if (nil? si) 0.0 (.getMean si))
;;                (if (nil? et) 0.0 (.getMean et))))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; RESIDENTIAL
;; Canadians use an average of 1600 cubic meters of water per person
;; per year. Need to factor this into the model statement below.
;; See www.environmentalindicators.com/htdocs/indicators/6wate.htm
;; 
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