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
;;; Carbon model for Vermont forests
;;;
;;; Valid Contexts: core.contexts.beta/{vt,lye_brook}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-lye-brook
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel model measurement
                           classification categorization ranking
                           numeric-coding binary-coding
                           probabilistic-measurement
                           probabilistic-classification identification
                           bayesian namespace-ontology count])
  (:refer aries :only [span]))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService)

(defmodel veg-soil-storage VegetationAndSoilCarbonStorage
  (probabilistic-measurement VegetationAndSoilCarbonStorage "t/ha" 
    [150 400] VeryHighStorage
    [75 150]  HighStorage
    [40 75]   ModerateStorage
    [20 40]   LowStorage
    [0.02 20] VeryLowStorage
    [0 0.02]  NoStorage))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

(defmodel veg-storage VegetationCarbonStorage
  (classification (measurement habitat:VegetationCarbonStorage "t/ha")
    [80 :>] VeryHighVegetationStorage
    [70 80] HighVegetationStorage
    [50 70] ModerateVegetationStorage
    [ 5 50] LowVegetationStorage
    [ 0  5] VeryLowVegetationStorage
    0       NoVegetationStorage))      

(defmodel soil-storage SoilCarbonStorage
  (classification (measurement habitat:SoilCarbonStorage "t/ha") 
    [210  :>] VeryHighSoilStorage
    [140 210] HighSoilStorage
    [70  140] ModerateSoilStorage
    [10   70] LowSoilStorage
    [ 0   10] VeryLowSoilStorage
    0         NoSoilStorage))

(defmodel summer-high-winter-low SummerHighWinterLow
  (classification (ranking habitat:SummerHighWinterLow)
    [40 :>] VeryHighSOL
    [34 40] HighSOL
    [29 34] ModerateSOL
    [24 29] LowSOL
    [:< 24] VeryLowSOL))

(defmodel stand-condition StandCondition
  (classification (ranking habitat:StandCondition) 
    #{4 5 6}   HighStandCondition
    #{7 8 9}   ModerateStandCondition
    #{1 2 3}   LowStandCondition
    :otherwise NoStandCondition))

(defmodel stand-size-density StandSizeDensity
  (classification (ranking habitat:StandSizeDensity) 
    #{5 6 8 9} HighStandSizeDensity
    #{3 4 7}   ModerateStandSizeDensity
    #{1 2}     LowStandSizeDensity
    0          NoStandSizeDensity))

(defmodel soil-CN-ratio SoilCNRatio
  (classification (ranking habitat:SoilCNRatio)
    [35 :>] VeryHighCNRatio
    [20 35] HighCNRatio
    [10 20] LowCNRatio
    [:< 10] VeryLowCNRatio))

;; Not used in the model but masks out carbon over open water
(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:< 1.15]    Level
    [1.15 4.57]  GentlyUndulating
    [4.57 16.70] RollingToHilly
    [16.70 :>]   SteeplyDissectedToMountainous))

;; Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [5 9]      VeryHighSequestration
    [4 5]      HighSequestration
    [3 4]      ModerateSequestration
    [1.5 3]    LowSequestration
    [0.01 1.5] VeryLowSequestration
    [0 0.01]   NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
    :import   "aries.core::CarbonSourceLyeBrook.xdsl"
    :context  [soil-CN-ratio stand-size-density stand-condition summer-high-winter-low]
    :required [SlopeClass]
    :keep     [VegetationAndSoilCarbonSequestration]
    :result   veg-soil-sequestration))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

;; Use Bayesian priors for insect & blowdown frequencies

(defmodel fire-frequency FireFrequency
  (classification (ranking habitat:FireFrequency) 
    [0.9 :>]    HighFireFrequency
    [0.25 0.9]  ModerateFireFrequency 
    [0.05 0.25] LowFireFrequency
    [:< 0.05]   NoFireFrequency))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [12 300] VeryHighRelease ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [9 12]   HighRelease
    [6 9]    ModerateRelease
    [3 6]    LowRelease
    [0.01 3] VeryLowRelease
    [0 0.01] NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
    :import   "aries.core::CarbonSinkLyeBrook.xdsl"
    :context  [fire-frequency veg-storage soil-storage]
    :required [SlopeClass]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel identification-carbon CarbonSequestrationAndStorage
  (identification CarbonSequestrationAndStorage
    :context [source sink use-simple]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel carbon-flow CarbonSequestrationAndStorage
  (span CO2Removed
        CarbonSourceValue
        GreenhouseGasEmissions
        CarbonSinkValue
        nil
        nil
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 20
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_san_pedro_data.clj")
        :context [source use-simple sink]
        :keep    [TheoreticalSink
                  TheoreticalSource
                  TheoreticalUse
                  PossibleSource
                  PossibleUse
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSource
                  InaccessibleUse
                  BlockedSink
                  BlockedUse]))


