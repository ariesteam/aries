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
;;; Carbon model for Southern California
;;;
;;; Valid Contexts: core.contexts.beta/{ca_mark_watershed,ca_mark}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-ca
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel model measurement
                           classification categorization ranking
                           numeric-coding binary-coding
                           probabilistic-measurement
                           probabilistic-classification identification
                           bayesian namespace-ontology count])
  (:refer aries :only [span]))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService
  (thinklab-core:BooleanRanking
   (LandOrSea
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

;; Problems with coarse-grain pixels; removed this from the bayesian
;; statement and set the prior to its actual value from the data
;; (LowActualEvapotranspiration) - a good temporary solution for WCH
;; but change if you ran it again for Southern California.
(defmodel actual-evapotranspiration ActualEvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
    [92 :>] VeryHighActualEvapotranspiration
    [58 92] HighActualEvapotranspiration
    [32 58] ModerateActualEvapotranspiration
    [12 32] LowActualEvapotranspiration
    [:< 12] VeryLowActualEvapotranspiration))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 100] VeryHighCanopyCover
    [60  80] HighCanopyCover
    [40  60] ModerateCanopyCover
    [20  40] LowCanopyCover
    [ 0  20] VeryLowCanopyCover))

;; This does not account for barren, water, agriculture, or urban cover
;; (though these are accounted for in NLCD)
(defmodel vegetation-type southernCalifornia:CarbonVegetationType
  (classification (numeric-coding southernCalifornia:VegTypeSoCal)
    1      southernCalifornia:HardwoodForest
    #{4 7} southernCalifornia:MixedConifer
    0      southernCalifornia:Shrub
    3      southernCalifornia:Herbaceous))

;; "Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use southernCalifornia:LandCover
  (classification (numeric-coding nlcd:NLCDNumeric)
    11                southernCalifornia:OpenWater
    #{90 95}          southernCalifornia:Wetland
    #{41 42 43 51 52} southernCalifornia:ScrubAndForest
    #{71 81 82}       southernCalifornia:GrasslandAndCultivated
    21                southernCalifornia:OpenSpace
    22                southernCalifornia:LowDensityDeveloped
    #{23 24}          southernCalifornia:HighAndMedDensityDeveloped))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification  (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;; Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [6   13]    VeryHighSequestration
    [4    6]    HighSequestration
    [3    4]    ModerateSequestration
    [1.5  3]    LowSequestration
    [0.01 1.5]  VeryLowSequestration
    [0    0.01] NoSequestration))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel source-sj CarbonSourceValue
  (bayesian CarbonSourceValue
    :import   "aries.core::CarbonSourceCa.xdsl"
    :context  [percent-canopy-cover land-use land-selector]
    :required [LandOrSea]
    :keep     [VegetationAndSoilCarbonSequestration]
    :result   veg-soil-sequestration))

;; See above statement for AET: Add back in if you use it for wider
;; extents of Southern California
(defmodel source CarbonSourceValue
  (bayesian CarbonSourceValue
    :import   "aries.core::CarbonSourceCa.xdsl"
    :context  [percent-canopy-cover vegetation-type land-use land-selector]
    :required [LandOrSea]
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

;; Using deep soil pH for grasslands and deserts, shallow for all
;; other ecosystem types
(defmodel soil-ph Soilph
  (classification (ranking habitat:SoilPhDeep)
    [7.3 :>]           HighPh
    [5.5 7.3]          ModeratePh
    [:exclusive 0 5.5] LowPh))

;; use NLCD layers to infer anoxic vs. oxic
(defmodel soil-oxygen-conditions SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}   AnoxicSoils
    :otherwise OxicSoils))

(defmodel fire-threat FireThreatClass
  (classification (ranking habitat:FireThreat) 
    4        VeryHighFireThreat
    3        HighFireThreat
    2        ModerateFireThreat
    #{1 255} LowFireThreat))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha" 
    [150 315] VeryHighVegetationStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [80 150]  HighVegetationStorage
    [30 80]   ModerateVegetationStorage
    [10 30]   LowVegetationStorage
    [0.01 10] VeryLowVegetationStorage
    [0 0.01]  NoVegetationStorage))   

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel vegetation-carbon-storage-sj VegetationCStorage 
  (bayesian VegetationCStorage
    :import  "aries.core::CarbonSinkCa.xdsl"
    :context [land-use percent-canopy-cover land-selector]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
    :import  "aries.core::CarbonSinkCa.xdsl"
    :context [vegetation-type land-use percent-canopy-cover land-selector]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha" 
    [25 50]  VeryHighSoilStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [15 25]  HighSoilStorage
    [5 15]   ModerateSoilStorage
    [2 5]    LowSoilStorage
    [0.01 2] VeryLowSoilStorage
    [0 0.01] NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage
    :import  "aries.core::CarbonSinkCa.xdsl"
    :context [soil-ph percent-canopy-cover soil-oxygen-conditions land-selector]
    :keep    [SoilCarbonStorage]
    :result  soil-storage))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel vegetation-soil-storage-sj VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage-sj soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel veg-soil-storage-sj VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage-sj
    [160 365] VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [100 160] HighStorage
    [40 100]  ModerateStorage
    [15 40]   LowStorage
    [0.01 15] VeryLowStorage
    [0 0.01]  NoStorage))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [160 365] VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [100 160] HighStorage
    [40 100]  ModerateStorage
    [15 40]   LowStorage
    [0.01 15] VeryLowStorage
    [0 0.01]  NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [100 180] VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [50 100]  HighRelease
    [25 50]   ModerateRelease
    [10 25]   LowRelease
    [0.01 10] VeryLowRelease
    [0 0.01]  NoRelease))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel sink-sj CarbonSinkValue   
  (bayesian CarbonSinkValue 
    :import   "aries.core::CarbonSinkCa.xdsl"
    :context  [veg-soil-storage-sj fire-threat land-selector]
    :required [LandOrSea]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;; Source and sink values are still calculated over the ocean, though
;; they're set to almost-zero, so it's a temporary fix.  For some
;; reason slope has value here in the ocean and using elevation as a
;; mask made the whole model disappear.
(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
    :import   "aries.core::CarbonSinkCa.xdsl"
    :context  [veg-soil-storage fire-threat land-selector]
    :required [LandOrSea]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Carbon model accuracy check
;;;-------------------------------------------------------------------

;; Decomposition Factor (DF) has been noted to be a good predictor of
;; NPP for chaparral ecosystems (Li, et al. 2006), which is highly
;; correlated with carbon sequestration rates (sources).  Since water
;; is a primary limiting factor for the study site's ecoregion this
;; sub-modeled deterministic node serves to assess model accuracy
;; (Expressed as: DF = P/PET) Values from this check should be compared
;; to soil and vegetation carbon sequestration, or possibly used as a
;; higher resolution proxy.

(defmodel decomposition-factor habitat:DecompositionFactor
  (ranking habitat:DecompositionFactor
    :context [(measurement habitat:PotentialEvapotranspiration "mm")
              (measurement habitat:AnnualPrecipitation  "mm")]
    :state   #(/ (:annual-precipitation %) (:potential-evapotranspiration %))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel identification-carbon ClimateStability
  (identification ClimateStability
    :context [source sink use-simple]))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel identification-carbon-sj ClimateStability
  (identification ClimateStability
    :context [source-sj sink-sj use-simple]))

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
        :keep    [StoredCarbonRelease
                  CarbonSequestration
                  GreenhouseGasEmissions
                  PotentialCarbonMitigationProvision
                  PotentialCarbonMitigationUse
                  DetrimentalCarbonSource
                  UsedCarbonSink
                  SatisfiedCarbonMitigationDemand
                  CarbonMitigationSurplus
                  CarbonMitigationDeficit
                  DepletedCarbonMitigation
                  DepletedCarbonMitigationDemand]))