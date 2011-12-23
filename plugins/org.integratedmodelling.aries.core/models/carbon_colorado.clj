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
;;; Carbon model for Colorado
;;;
;;; Valid Contexts: core.contexts.beta/co*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-colorado
  (:refer-clojure :rename {count length})
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel model measurement
                           classification categorization ranking
                           numeric-coding binary-coding
                           probabilistic-measurement no-data?
                           probabilistic-classification identification
                           bayesian namespace-ontology count])
  (:refer aries :only (span)))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService
	(CarbonVegetationType (CarbonVegetationTypeOpen) (CarbonVegetationTypeConstrained)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

(defmodel percent-tree-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [75 100 :inclusive] VeryHighCanopyCover
    [50  75]            HighCanopyCover
    [30  50]            ModerateCanopyCover
    [10  30]            LowCanopyCover
    [ 0  10]            VeryLowCanopyCover))

(defmodel vegetation-type colorado:CarbonVegetationType
  "Reclass of SWReGAP LULC"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) 
    #{24 26 28 29 30 32 34 35 36 92 103}                                             colorado:ConiferousForest
    #{22 33 38 41}                                                                   colorado:DeciduousForest
    #{68 70 71 72 73 74 75 76 106 119 120 121 122}                                   colorado:Grassland
    #{40 41 42 43 44 46 48 50 53 56 58 62 63 64 67 69 82 95 104 108 109}             colorado:Shrubland
    #{77 78 79 81 85 86 99 118}                                                      colorado:Wetland
    114                                                                              colorado:Cropland
    #{1 2 4 5 7 8 9 10 11 13 14 15 17 19 21 110 111 112 113 115 116 117 123 124 125} colorado:Unvegetated))

;; Annual precipitation used as the main climatic variable in the
;; model, as opposed to the difference between mean summer high and
;; winter low temperatures. This is probably a better variable to
;; include in carbon models in wetter regions, while annual precip
;; is far more important in water-limited regions.

(defmodel annual-precipitation MeanAnnualPrecipitation
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [600  :>] HighMeanAnnualPrecipitation
    [400 600] ModerateMeanAnnualPrecipitation
    [:<  400] LowMeanAnnualPrecipitation))

(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [10   20]    VeryHighSequestration
    [ 6   10]    HighSequestration
    [ 4    6]    ModerateSequestration
    [ 2.5  4]    LowSequestration
    [ 0.01 2.5]  VeryLowSequestration
    [ 0    0.01] NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
    :import  "aries.core::CarbonSourceColorado.xdsl"
    :context [vegetation-type percent-tree-canopy-cover annual-precipitation]
    :keep    [VegetationAndSoilCarbonSequestration]
    :result  veg-soil-sequestration))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

(defmodel soil-type colorado:SoilType
  (classification (categorization colorado:CarbonSoilType)
    "Alfisols"    colorado:Alfisols
    "Aridisols"   colorado:Aridisols
    "Entisols"    colorado:Entisols
    "Inceptisols" colorado:Inceptisols
    "Mollisols"   colorado:Mollisols
    "Water"       colorado:Water))

(defmodel beetle-kill colorado:MountainPineBeetleDamageClass ; Values
                                        ; in trees killed/ac: not a
                                        ; thinklab-recognized unit, so
                                        ; keep as a ranking.
  (classification (ranking colorado:MountainPineBeetleDamageTreesPerAcre)
    [17.6 500] colorado:SevereDamage
    [ 2 17.6]  colorado:ModerateDamage
    [  1  2]   colorado:LowDamage
    :otherwise colorado:NoDamage))

(defmodel fire-threat FireThreatClass ; This is actually fire return
                                      ; interval data, but uses
                                      ; student discretization.
  (classification (ranking habitat:FireThreat) 
    [ 2   9] VeryHighFireThreat
    [ 9  12] HighFireThreat
    [12  18] ModerateFireThreat
    [18 133] LowFireThreat))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha" 
    [89  127]    VeryHighVegetationStorage
    [69   89]    HighVegetationStorage
    [37   69]    ModerateVegetationStorage
    [ 9.3 37]    LowVegetationStorage
    [ 0.01 9.3]  VeryLowVegetationStorage
    [ 0    0.01] NoVegetationStorage))

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
    :import  "aries.core::CarbonSinkColorado.xdsl"
    :context [percent-tree-canopy-cover vegetation-type beetle-kill]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

;; Hack; remove once a proper context model is working
(defmodel vegetation-carbon-storage-no-beetle colorado:VegetationCStorageNoBeetle
  (bayesian VegetationCStorage 
    :import  "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context [percent-tree-canopy-cover vegetation-type]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha" 
    [69   108]    VeryHighSoilStorage
    [46.6  69]    HighSoilStorage
    [27    46.6]  ModerateSoilStorage
    [10    27]    LowSoilStorage
    [ 0.01 10]    VeryLowSoilStorage
    [ 0     0.01] NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
    :import  "aries.core::CarbonSinkColorado.xdsl"
    :context [soil-type annual-precipitation]
    :keep    [SoilCarbonStorage]
    :result  soil-storage))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

;; Hack, remove when contexts are working properly.
(defmodel vegetation-soil-storage-no-beetle colorado:VegetationAndSoilCarbonStorageNoBeetle
  (measurement colorado:VegetationAndSoilCarbonStorageNoBeetle "t/ha"
    :context [vegetation-carbon-storage-no-beetle soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage-no-beetle %)) 0.0 (.getMean (:vegetation-c-storage-no-beetle %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [159   235]    VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [116   159]    HighStorage
    [ 64   116]    ModerateStorage
    [ 10    64]    LowStorage
    [  0.02 10]    VeryLowStorage
    [  0     0.02] NoStorage))

;; One more hack for good measure
(defmodel veg-soil-storage-no-beetle colorado:VegetationAndSoilCarbonStorageClassNoBeetle
  (classification vegetation-soil-storage-no-beetle
    [159   235]    colorado:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [116   159]    colorado:HighStorage
    [ 64   116]    colorado:ModerateStorage
    [ 10    64]    colorado:LowStorage
    [  0.02 10]    colorado:VeryLowStorage
    [  0     0.02] colorado:NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [59   118]    VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [29.5  59]    HighRelease
    [14.8  29.5]  ModerateRelease
    [ 7.4  14.8]  LowRelease
    [ 0.02  7.4]  VeryLowRelease
    [ 0     0.02] NoRelease))

(defmodel sink CarbonSinkValue
  (bayesian CarbonSinkValue
    :import  "aries.core::CarbonSinkColorado.xdsl"
    :context [veg-soil-storage fire-threat]
    :keep    [StoredCarbonRelease]
    :result  stored-carbon-release))

;; Hack, remove when contexts are working properly.
(defmodel sink-no-beetle colorado:CarbonSinkValueNoBeetle
  (bayesian colorado:CarbonSinkValueNoBeetle
    :import  "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context [veg-soil-storage-no-beetle fire-threat]
    :keep    [StoredCarbonRelease]
    :result  stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;: GHG emissions map for the U.S.
(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel identification-carbon ClimateStability
  (identification ClimateStability
    :context [source sink use-simple]))

;; Hack, remove when contexts are working properly.
(defmodel identification-carbon-no-beetle colorado:ClimateStabilityNoBeetle
  (identification ClimateStability
    :context [source sink-no-beetle use-simple]))

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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------