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
    [80 100 :inclusive] VeryHighCanopyCover
    [60  80]            HighCanopyCover
    [40  60]            ModerateCanopyCover
    [20  40]            LowCanopyCover
    [ 0  20]            VeryLowCanopyCover))

(defmodel vegetation-type colorado:CarbonVegetationType
  "Reclass of SWReGAP LULC"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) ; Discretize correctly!
    #{22 23 24 25 26 27 28 29 30 31 32 34 35 36 37 38 45 92}                           colorado:ConiferousForest
    #{33 41 91}                                                                        colorado:DeciduousForest
    #{52 109}                                                                          colorado:Grassland
    #{62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 90 93}                              colorado:Shrubland
    #{19 39 40 42 43 44 46 47 48 49 50 51 53 54 55 56 57 58 59 60 61 94 95 96 105 108} colorado:Wetland
    #{77 78 79 80 81 83 84 85 98 118}                                                  colorado:Cropland
    #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 110 111 112 114}              colorado:Unvegetated))

;; Brown et al. (2010) use 0-130, 130-230, 230-460, >460 mm as their
;; discretization for rangeland carbon modeling.  For the San Pedro,
;; the entire valley floor would be in the 230-460 range and the
;; surrounding mountains as >460.  For now, keep the below
;; discretization, though strongly consider using it.

;; Annual precipitation used as the main climatic variable in the
;; model, as opposed to the difference between mean summer high and
;; winter low temperatures. This is probably a better variable to
;; include in carbon models in wetter regions, while annual precip
;; is far more important in water-limited regions.

(defmodel annual-precipitation MeanAnnualPrecipitation
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [500  :>] HighMeanAnnualPrecipitation
    [400 500] ModerateMeanAnnualPrecipitation
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
    :import  "aries.core::CarbonSourceSanPedro.xdsl"
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
  (classification (ranking colorado:CarbonSoilType)
    1 colorado:Alfisols
    2 colorado:Aridisols
    3 colorado:Entisols
    4 colorado:Inceptisols
    5 colorado:Mollisols
    6 colorado:Water))

(defmodel beetle-kill colorado:BeetleKill
  (classification (ranking colorado:BeetleKillLevel)
    [150 200] colorado:VeryHighBeetleKill ; Values are in trees
                                        ; killed/ha - definitely not a
                                        ; thinklab-recognized unit, so
                                        ; keep as is.
    [100 150] colorado:HighBeetleKill
    [ 50 100] colorado:ModerateBeetleKill
    [  1  50] colorado:LowBeetleKill
           0  colorado:NoBeetleKill))

(defmodel fire-threat FireThreatClass
  (classification (ranking habitat:FireThreat) 
    1 VeryHighFireThreat
    2 HighFireThreat
    3 ModerateFireThreat
    4 LowFireThreat))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year" 
    [89  127]    VeryHighVegetationStorage
    [69   89]    HighVegetationStorage
    [37   69]    ModerateVegetationStorage
    [ 9.3 37]    LowVegetationStorage
    [ 0.01 9.3]  VeryLowVegetationStorage
    [ 0    0.01] NoVegetationStorage))

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
    :import  "aries.core::CarbonSinkColorado.xdsl"
    :context [annual-precipitation percent-tree-canopy-cover vegetation-type]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha*year" 
    [69   108]    VeryHighSoilStorage
    [46.6  69]    HighSoilStorage
    [27    46.6]  ModerateSoilStorage
    [10    27]    LowSoilStorage
    [ 0.01 10]    VeryLowSoilStorage
    [ 0     0.01] NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
    :import  "aries.core::CarbonSinkColorado.xdsl"
    :context [soil-ph slope oxygen percent-tree-canopy-cover vegetation-type]
    :keep    [SoilCarbonStorage]
    :result  soil-storage))

;;Consider reworking the soil carbon storage part of the model based
;; on Martens et al. 2005 - soil texture, precip, temperature as most
;; important correlates of high soil carbon storage.

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [159 235] VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [116   159]    HighStorage
    [ 66   116]    ModerateStorage
    [ 10    64]    LowStorage
    [  0.02 10]    VeryLowStorage
    [  0     0.02] NoStorage))

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
    :import  "aries.core::CarbonSinkSanPedro.xdsl"
    :context [veg-soil-storage fire-frequency]
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------