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
;;; Carbon model for Madagascar
;;;
;;; Valid Contexts: core.contexts.beta/mg*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-mg
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

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 :>] VeryHighCanopyCover
    [60 80] HighCanopyCover
    [40 60] ModerateCanopyCover
    [20 40] LowCanopyCover
    [ 0 20] VeryLowCanopyCover))

(defmodel summer-high-winter-low SummerHighWinterLow
  (classification (ranking habitat:SummerHighWinterLow)
    [40 :>] VeryHighSOL
    [35 40] HighSOL
    [30 35] ModerateSOL
    [24 30] LowSOL
    [:< 24] VeryLowSOL))

(defmodel soil-cn-ratio SoilCNRatio
  (classification (ranking habitat:SoilCNRatio)
    [35 :>] VeryHighCNRatio
    [20 35] HighCNRatio
    [10 20] LowCNRatio
    [:< 10] VeryLowCNRatio))

(defmodel degradation-status ForestDegradationStatus
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{3 7 23}  Degraded
    :otherwise NotDegraded))

;; Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [13 23]  VeryHighSequestration
    [9 13]   HighSequestration
    [6 9]    ModerateSequestration
    [4 6]    LowSequestration
    [0.01 4] VeryLowSequestration
    [0 0.01] NoSequestration))

(defmodel vss VegetationAndSoilCarbonSequestration
  (measurement VegetationAndSoilCarbonSequestration "t/ha*year"))

;; Bayesian source model
(defmodel source CarbonSourceValue
  (bayesian CarbonSourceValue
    :import   "aries.core::CarbonSourceMg.xdsl"
    :context  [percent-canopy-cover summer-high-winter-low soil-cn-ratio degradation-status]
    :required [SummerHighWinterLow]
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

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:<     1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

;; Values of "1" correspond to high pH (>7.3), "2" to low pH (<5.5), "3-4" to intermediate pH (5.5-7.3)
;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhShallow)
    #{3 4} HighPh
    2      ModeratePh
    1      LowPh))

;; Mg wetlands layer is just a wetlands layer - no reclass of a LULC layer.
(defmodel oxygen SoilOxygenConditions
  (classification (binary-coding habitat:Wetland)
    1          AnoxicSoils
    :otherwise OxicSoils))

;;No data on fire frequency for Madagascar - use Bayesian priors until we can get a layer.

(defmodel population-density PopulationDensity
  (classification (count policytarget:PopulationDensity "/km^2")
    [12 :>] HighPopulationDensity
    [7  12] ModeratePopulationDensity
    [:<  7] LowPopulationDensity))

(defmodel deforestation-risk DeforestationRiskClass
  (classification (ranking DeforestationRisk)
    "High"     HighDeforestationRisk
    "Moderate" ModerateDeforestationRisk
    "Low"      LowDeforestationRisk
    :otherwise NoDeforestationRisk))

;; This discretization is based off comparison with Reusch & Gibbs'
;; global vegetation carbon storage layer, with maximum values from
;; Madagascar being slightly over half of that in Puget Sound.  Need
;; to run this by folks familiar with carbon data/forestry in
;; Madagascar.  Note that using spatial data to determine these
;; magnitudes (versus published literature estimates like Smith et
;; al. 2006 for the United States) gives different relative magnitudes
;; and that for San Pedro, Puget Sound, California, and Vermont we
;; used Smith et al. 2006 for discretization.
(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha"
    [300 500] VeryHighVegetationStorage
    [100 300] HighVegetationStorage
    [25 100]  ModerateVegetationStorage
    [10 25]   LowVegetationStorage
    [0.01 10] VeryLowVegetationStorage
    [0 0.01]  NoVegetationStorage))

(defmodel vegetation-carbon-storage VegetationCStorage
  (bayesian VegetationCStorage
    :import   "aries.core::CarbonSinkMg.xdsl"
    :context  [percent-canopy-cover summer-high-winter-low degradation-status population-density]
    :required [SummerHighWinterLow]
    :keep     [VegetationCarbonStorage]
    :result    veg-storage))

;; This discretization is based off comparison with Reusch & Gibbs'
;; global vegetation carbon storage layer, with maximum values from
;; Madagascar being 133% of that in Puget Sound.  Need to run this by
;; folks familiar with carbon data in Madagascar.  Note that using
;; spatial data to determine these magnitudes (versus published
;; literature estimates like Smith et al. 2006 for the United States)
;; gives different relative magnitudes and that for San Pedro, Puget
;; Sound, California, and Vermont we used Smith et al. 2006 for
;; discretization.
(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha"
    [75 150]  VeryHighSoilStorage
    [40 75]   HighSoilStorage
    [20 40]   ModerateSoilStorage
    [10 20]   LowSoilStorage
    [0.01 10] VeryLowSoilStorage
    [0 0.01]  NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage
    :import   "aries.core::CarbonSinkMg.xdsl"
    :context  [soil-cn-ratio degradation-status soil-ph slope oxygen percent-canopy-cover]
    :required [SummerHighWinterLow]
    :keep     [SoilCarbonStorage]
    :result    soil-storage))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage] 
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [300 650] VeryHighStorage
    [100 300] HighStorage
    [50 100]  ModerateStorage
    [10 50]   LowStorage
    [0.01 10] VeryLowStorage
    [0 0.01]  NoStorage))

;; no numbers included in the discretization worksheet so the same numbers as the other concepts are used
(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [12 1200] VeryHighRelease ; Need to check this ceiling; values of 1000 can be found in the Pacific NW rainforests, need to see how this compares to Madagascar's rainforests.
    [9 12]    HighRelease
    [6 9]     ModerateRelease
    [3 6]     LowRelease
    [0.01 3]  VeryLowRelease
    [0 0.01]  NoRelease))

(defmodel sink CarbonSinkValue
  (bayesian CarbonSinkValue 
    :import   "aries.core::CarbonSinkMg.xdsl"
    :context  [veg-soil-storage degradation-status]
    :required [SummerHighWinterLow]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;: GHG emissions map for Madagascar: use global population density
;; layer multiplied by per capita emissions for that country from EIA.
;; 2006 data used as this corresponds to current population density
;; layer: 0.14 tonnes CO2/capita for Madagascar in 2006, which is
;; equivalent to 0.04 tonnes C/capita
(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(if (nil? (:population-density %)) nil (* (:population-density %) 0.04))))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel identification-carbon ClimateStability
  (identification ClimateStability
    :context [source sink use-simple]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel carbon-flow ClimateStability
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