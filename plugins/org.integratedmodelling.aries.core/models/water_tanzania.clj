;;; Copyright 2011 The ARIES Consortium
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
;;; Water supply model for the Great Ruaha River Basin, Tanzania
;;;
;;; Valid Contexts: core.contexts.beta/grr_tanzania
;;;
;;;-------------------------------------------------------------------

(ns core.models.water-tanzania
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding identification
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking bayesian count])
  (:refer aries :only [span]))

(namespace-ontology waterSupplyService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; Runoff would be preferable to precipitation, but it does not exist
;; for Tanzania. Use precip for now, with the goal of incorporating a
;; better runoff model (plus sink models that actually capture
;; infiltration & ET).
(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; Ad hoc sink model adapted from the ad hoc flood sink model.
;; Includes infiltration & evapotranspiration processes.  Deterministic
;; models could likely be used.

;; I subsetted the SRTM slope data to include only the little Ruaha region
;; I then ran tabulate areas to calculate the area of each slope class
;; within my region of interest. Finally, I compute the probabilities and
;; updated SurfaceWaterSinkTZ.SlopeClass values
(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00B0")
    [    0  1.15]         Level
    [ 1.15  4.57]         GentlyUndulating
    [ 4.57 16.70]         RollingToHilly
    [16.70 90 :inclusive] SteeplyDissectedToMountainous))

;; estimated priors using the global HSG dataset
(defmodel soil-group HydrologicSoilsGroup
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 SoilGroupA
    2 SoilGroupB
    3 SoilGroupC
    4 SoilGroupD))

;; I subsetted the global impervious surface data to include only the
;; little Ruaha region. I then ran tabulate areas to calculate the area 
;; of each impervious class within my region of interest. Finally, I 
;; compute the probabilities and updated SurfaceWaterSinkTZ.%Impervious
(defmodel imperviousness PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousness)
    [80 100 :inclusive] VeryHighImperviousCover
    [50 80]             HighImperviousCover
    [20 50]             ModeratelyHighImperviousCover
    [10 20]             ModeratelyLowImperviousCover
    [ 5 10]             LowImperviousCover
    [ 0  5]             VeryLowImperviousCover))

;; Based on the AFRICOVER land cover data set. There are five classes of
;; land cover based on this aggregated data set. See the notes from
;; AFRICOVER in the file spatial-agg-procedure.pdf for more details on
;; the aggregation procedure. The future iteration of the model should
;; revisit this aggregation to potentially generate our own more specific
;; land cover classification scheme.
(defmodel land-cover-typology lulc:LandCoverTypology
  (classification (categorization tanzania-lulc:TanzaniaLULCCategory)
    "AG"   tanzania-lulc:Agriculture
    "NVT"  tanzania-lulc:TerrestrialVegetation
    "NVW"  tanzania-lulc:AquaticVegetation
    "UR"   tanzania-lulc:Urban
    "WAT"  tanzania-lulc:Water))

;; there are values of 254 and 255 in the source data set and we're not sure what that means
;; so we're treating them as No Data along with the other No Data values
(defmodel percent-vegetation-cover PercentVegetationCoverClass
  (classification (ranking habitat:PercentVegetationCover)
    [80 100 :inclusive] VeryHighVegetationCover
    [60  80]            HighVegetationCover
    [40  60]            ModerateVegetationCover
    [20  40]            LowVegetationCover
    [ 0  20]            VeryLowVegetationCover))

;; there is a dam in the area but don't currently have the information
;; to conclude that it is a factor for water provision in the
;; watershed. I leave this here for now, until I can determine if we
;; do / don't need this to be included.
;;(defmodel dam-presence Dams
;;  (classification (binary-coding NonRivalWaterUseCode)
;;      1         DamPresent
;;     :otherwise DamAbsent))

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration, dams, etc.
(defmodel evapotranspiration EvapotranspirationClass
  (probabilistic-measurement EvapotranspirationClass "mm" 
    [180 340] VeryHighEvapotranspiration
    [100 180] HighEvapotranspiration
    [ 50 100] ModerateEvapotranspiration
    [  0  50] LowEvapotranspiration
    [  0   0] VeryLowEvapotranspiration))

(defmodel et-sink Evapotranspiration
  (bayesian Evapotranspiration
    :import  "aries.core::SurfaceWaterSinkTZ.xdsl"
    :context [land-cover-typology percent-vegetation-cover]
    :keep    [EvapotranspirationClass]
    :result  evapotranspiration))

(defmodel soil-infiltration SoilInfiltrationClass
  (probabilistic-measurement SoilInfiltrationClass "mm" 
    [180 260] VeryHighInfiltration
    [100 180] HighInfiltration
    [ 50 100] ModerateInfiltration
    [  0  50] LowInfiltration
    [  0   0] VeryLowInfiltration))

(defmodel soil-sink SoilInfiltration
  (bayesian SoilInfiltration
    :import  "aries.core::SurfaceWaterSinkTZ.xdsl"
    :context [soil-group slope imperviousness]
    :keep    [SoilInfiltrationClass]
    :result  soil-infiltration))

(defmodel surface-water-sink SurfaceWaterSink
  (measurement SurfaceWaterSink "mm"
    :context [soil-sink et-sink] 
    :state   #(let [si (:soil-infiltration  %)
                    et (:evapotranspiration %)]
                (+ 
                 (if (nil? si) 0.0 (.getMean si))
                 (if (nil? et) 0.0 (.getMean et))))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; The models for the Great Ruaha River basin are for SURFACE WATER ONLY

;; RESIDENTIAL
(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm" ;;This is an annual value
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(* 0.8 0.082855 (:population-density %))))

;; AGRICULTURE
;; Agricultural surface water use. Step 1: Estimate total livestock water needs.
;; Livestock use below is from different sources for pigs and other livestock: its unlikely
;; that pigs should use more water per capita than cattle.
(defmodel livestock-total-water-use LivestockWaterUse
  (measurement LivestockWaterUse "mm"  ;;This is an annual value
    :context [(count CattlePopulation "/km^2")
              (count SheepPopulation  "/km^2")
              (count PigsPopulation   "/km^2")
              (count GoatsPopulation  "/km^2")]
    :state   #(+ (* (or (:sheep-population  %) 0.0) 0.002745) ;;this is in m^3/1000 animals, the conversion ultimately giving mm
                 (* (or (:goats-population  %) 0.0) 0.002745)
                 (* (or (:cattle-population %) 0.0) 0.011032)
                 (* (or (:pigs-population   %) 0.0) 0.013310))))

(defmodel livestock-total-water-use-class LivestockTotalWaterUseClass
  (classification livestock-total-water-use
    [1.15 :>]  HighLivestockTotalWaterUse
    [0.5 1.15] ModerateLivestockTotalWaterUse
    [:<  0.5]  LowLivestockTotalWaterUse))

;; Agricultural surface water use. Step 2: Consider proximity to surface water.
(defmodel proximity-to-surface-water-class ProximityToSurfaceWaterClass
  (classification (measurement ProximityToSurfaceWater "m")
    [:<  250] HighSurfaceWaterProximity
    [250 500] ModerateSurfaceWaterProximity
    [500  :>] LowSurfaceWaterProximity))

;; Agricultural surface water use. Step 3: Estimate crop irrigation water needs.
(defmodel irrigation-water-use IrrigationWaterUse
  (measurement IrrigationWaterUse "mm"  ;;This is an annual value
     :context [(categorization tanzania-lulc:TanzaniaLULCCategory)]
     :state   #(if (= (:tanzania-l-u-l-c-category %) "AG")
                  2000
                  0)))

;; Classification of irrigationWaterUse into 6 classes.
(defmodel irrigation-water-use-class IrrigationWaterUseClass
  (classification irrigation-water-use
    [2400   :>] VeryHighIrrigationUse
    [2150 2400] HighIrrigationUse
    [1850 2150] ModerateIrrigationUse
    [1600 1850] LowIrrigationUse
    [:exclusive 0   1600] VeryLowIrrigationUse
    0           NoIrrigationUse))

;; Undiscretization of agricultural surface water use.
(defmodel use-undiscretizer AgriculturalSurfaceWaterUseClass
  (probabilistic-measurement AgriculturalSurfaceWaterUseClass "mm" 
    [2000 3000] HighAgriculturalSurfaceWaterUse
    [1000 2000] ModerateAgriculturalSurfaceWaterUse
    [   0 1000] LowAgriculturalSurfaceWaterUse)) 

(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUse
 (bayesian AgriculturalSurfaceWaterUse
   :import   "aries.core::SurfaceWaterUseTZAgriculture.xdsl"
   :context  [proximity-to-surface-water-class livestock-total-water-use-class irrigation-water-use-class]
   :keep     [AgriculturalSurfaceWaterUseClass]
   :result   use-undiscretizer)) 

;; Total surface water use. Step 5: Add the two rival user groups
(defmodel total-surface-water-use TotalSurfaceWaterUse
  (measurement TotalSurfaceWaterUse "mm"  ;;This is an annual value
    :context [agricultural-surface-water-use residential-surface-water-use]
    :state   #(+ (if (nil? (:agricultural-surface-water-use %)) 0 (.getMean (:agricultural-surface-water-use %)))
                 (or (:residential-surface-water-use  %) 0))))

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel data WaterSupply 
  (identification WaterSupply 
    :context [precipitation-annual
              surface-water-sink
              total-surface-water-use
              altitude
              streams]))              

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; flow model for surface water
(defmodel surface-flow SurfaceWaterMovement
  (span SurfaceWaterMovement
        AnnualPrecipitation
        TotalSurfaceWaterUse
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   nil
        :sink-threshold     nil
        :use-threshold      nil
        :trans-threshold    10.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/water_tanzania_data.clj")
        :context             [precipitation-annual
                              total-surface-water-use
                              surface-water-sink
                              altitude
                              streams]
        :keep                [SurfaceWaterSupply
                              MaximumSurfaceWaterSink
                              SurfaceWaterDemand
                              PossibleSurfaceWaterFlow
                              PossibleSurfaceWaterSupply
                              PossibleSurfaceWaterUse
                              ActualSurfaceWaterFlow
                              UsedSurfaceWaterSupply
                              ActualSurfaceWaterSink
                              SatisfiedSurfaceWaterDemand
                              UnusableSurfaceWaterSupply
                              UnusableSurfaceWaterSink
                              InaccessibleSurfaceWaterDemand
                              SunkSurfaceWaterFlow
                              SunkSurfaceWaterSupply
                              BlockedSurfaceWaterDemand]))
