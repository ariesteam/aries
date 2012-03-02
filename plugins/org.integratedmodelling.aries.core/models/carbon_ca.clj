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
;;; Valid Contexts: core.contexts.california/{san-gabriel,west-coyote-hills}*
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
    [92 :>] VeryHighEvapotranspiration
    [58 92] HighEvapotranspiration
    [32 58] ModerateEvapotranspiration
    [12 32] LowEvapotranspiration
    [:< 12] VeryLowEvapotranspiration))

(defmodel percent-canopy-cover california:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 100] california:VeryHighCanopyCover
    [60  80] california:HighCanopyCover
    [40  60] california:ModerateCanopyCover
    [20  40] california:LowCanopyCover
    [ 0  20] california:VeryLowCanopyCover))

;; This does not account for barren, water, agriculture, or urban cover
;; (though these are accounted for in NLCD)
(defmodel vegetation-type california:CarbonVegetationType
  (classification (numeric-coding california:VegTypeSoCal)
    1      california:HardwoodForest
    #{4 7} california:MixedConifer
    0      california:Shrub
    3      california:Herbaceous))

;; "Reclass of the NLCD land use for the purposes of carbon modeling"
(defmodel land-use california:LandCover
  (classification (numeric-coding nlcd:NLCDNumeric)
    11                california:OpenWater
    #{90 95}          california:Wetland
    #{41 42 43 51 52} california:ScrubAndForest
    #{71 81 82}       california:GrasslandAndCultivated
    21                california:OpenSpace
    22                california:LowDensityDeveloped
    #{23 24 31}       california:HighAndMedDensityDeveloped))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification  (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;; Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration california:VegetationAndSoilCarbonSequestration
  (probabilistic-measurement california:VegetationAndSoilCarbonSequestration "t/ha*year"
    [6   13]    california:VeryHighSequestration
    [4    6]    california:HighSequestration
    [3    4]    california:ModerateSequestration
    [1.5  3]    california:LowSequestration
    [0.01 1.5]  california:VeryLowSequestration
    [0    0.01] california:NoSequestration))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel source-sj california:CarbonSourceValue
  (bayesian california:CarbonSourceValue
    :import   "aries.core::CarbonSourceCa.xdsl"
    :context  [percent-canopy-cover land-use land-selector]
    :required [LandOrSea]
    :keep     [VegetationAndSoilCarbonSequestration]
    :result   veg-soil-sequestration))

;; See above statement for AET: Add back in if you use it for wider
;; extents of Southern California
(defmodel source california:CarbonSourceValue
  (bayesian california:CarbonSourceValue
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
(defmodel soil-ph california:SoilPh
  (classification (ranking habitat:SoilPhDeep)
    [7.3 :>]           california:HighPh
    [5.5 7.3]          california:ModeratePh
    [:exclusive 0 5.5] california:LowPh))

;; use NLCD layers to infer anoxic vs. oxic
(defmodel soil-oxygen-conditions california:SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}   california:AnoxicSoils
    :otherwise california:OxicSoils))

(defmodel fire-threat california:FireThreatClass
  (classification (ranking habitat:FireThreat) 
    4        california:VeryHighFireThreat
    3        california:HighFireThreat
    2        california:ModerateFireThreat
    #{1 255} california:LowFireThreat))

(defmodel veg-storage california:VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha" 
    [150 315] california:VeryHighVegetationStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [80 150]  california:HighVegetationStorage
    [30 80]   california:ModerateVegetationStorage
    [10 30]   california:LowVegetationStorage
    [0.01 10] california:VeryLowVegetationStorage
    [0 0.01]  california:NoVegetationStorage))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel vegetation-carbon-storage-sj california:VegetationCStorage 
  (bayesian california:VegetationCStorage
    :import  "aries.core::CarbonSinkCa.xdsl"
    :context [land-use percent-canopy-cover land-selector]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel vegetation-carbon-storage california:VegetationCStorage 
  (bayesian california:VegetationCStorage 
    :import  "aries.core::CarbonSinkCa.xdsl"
    :context [vegetation-type land-use percent-canopy-cover land-selector]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage california:SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha" 
    [110  150] california:VeryHighSoilStorage ;; Old discretizaton was 0-2,
                                        ; 2-5, 5-15, 15-25, 25-50,
                                        ; per Smith et al. 2006.
                                        ; Discretization below fits
                                        ; global soil C layer; revisit
                                        ; once SSURGO soil C data are in
    [ 95 110]  california:HighSoilStorage
    [ 80  95]  california:ModerateSoilStorage
    [ 50  80]  california:LowSoilStorage
    [0.01 50]  california:VeryLowSoilStorage
    [0  0.01]  california:NoSoilStorage))

(defmodel soil-carbon-storage california:SoilCStorage 
  (bayesian california:SoilCStorage
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
(defmodel veg-soil-storage-sj california:VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage-sj
    [160 365] california:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [100 160] california:HighStorage
    [40 100]  california:ModerateStorage
    [15 40]   california:LowStorage
    [0.01 15] california:VeryLowStorage
    [0 0.01]  california:NoStorage))

(defmodel veg-soil-storage california:VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [160 365] california:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [100 160] california:HighStorage
    [40 100]  california:ModerateStorage
    [15 40]   california:LowStorage
    [0.01 15] california:VeryLowStorage
    [0 0.01]  california:NoStorage))

(defmodel stored-carbon-release california:StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [100 180] california:VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [50 100]  california:HighRelease
    [25 50]   california:ModerateRelease
    [10 25]   california:LowRelease
    [0.01 10] california:VeryLowRelease
    [0 0.01]  california:NoRelease))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel sink-sj california:CarbonSinkValue   
  (bayesian california:CarbonSinkValue 
    :import   "aries.core::CarbonSinkCa.xdsl"
    :context  [veg-soil-storage-sj fire-threat land-selector]
    :required [LandOrSea]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;; Source and sink values are still calculated over the ocean, though
;; they're set to almost-zero, so it's a temporary fix.  For some
;; reason slope has value here in the ocean and using elevation as a
;; mask made the whole model disappear.
(defmodel sink california:CarbonSinkValue   
  (bayesian california:CarbonSinkValue 
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

(defmodel identification-carbon CarbonSequestrationAndStorage
  (identification CarbonSequestrationAndStorage
    :context [source sink use-simple]))

;; This is a hack to run the model for San Joaquin.  Hopefully can remove it soon.
(defmodel identification-carbon-sj CarbonSequestrationAndStorage
  (identification CarbonSequestrationAndStorage
    :context [source-sj sink-sj use-simple]))

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
