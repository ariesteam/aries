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
;;; Valid Contexts: core.contexts.colorado/co*
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
  (CarbonVegetationType (CarbonVegetationTypeOpen) (CarbonVegetationTypeConstrained))
  (thinklab-core:BooleanRanking
   (LandOrWater
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

;; Used to mask out open water, perennial snow & ice, barren land
(defmodel land-selector LandOrWater
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    ;;    #{12 21 22 23 24 31 41 42 43 52 71 81 82 90 95} OnLand
    ;;    Use this once NLCD is working again - much cleaner and simpler.
    #{13 19 20 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 111 112 114 115 116 118 119 120 121 122 123 124 125} OnLand))

(defmodel percent-tree-canopy-cover colorado:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [75 100 :inclusive] colorado:VeryHighCanopyCover
    [50  75]            colorado:HighCanopyCover
    [30  50]            colorado:ModerateCanopyCover
    [10  30]            colorado:LowCanopyCover
    [ 0  10]            colorado:VeryLowCanopyCover))

(defmodel vegetation-type colorado:CarbonVegetationType
  "Reclass of SWReGAP LULC"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) 
    #{24 26 28 29 30 32 33 34 35 36 78 92 103}
    colorado:ConiferousForest
    #{22 23 38}
    colorado:DeciduousForest
    #{62 63 64 67 68 69 70 71 72 73 74 75 76 86 95 106}
    colorado:Grassland
    #{39 40 41 42 43 44 46 48 50 53 56 58 79 104 108 109 118 119 120 121 122}
    colorado:Shrubland
    #{19 77 81 82 85 86 99}
    colorado:Wetland
    114
    colorado:Cropland
    #{1 2 4 5 7 8 9 10 11 12 13 14 15 17 21 111 112 113 115 116 117 123 124 125}
    colorado:Unvegetated))

;; Annual precipitation used as the main climatic variable in the
;; model, as opposed to the difference between mean summer high and
;; winter low temperatures. This is probably a better variable to
;; include in carbon models in wetter regions, while annual precip
;; is far more important in water-limited regions.

(defmodel annual-precipitation colorado:AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [600  :>] colorado:HighMeanAnnualPrecipitation
    [400 600] colorado:ModerateMeanAnnualPrecipitation
    [:<  400] colorado:LowMeanAnnualPrecipitation))
;;These are from sediment model- need to decide which to go with
   ;; [1150   :>] colorado:VeryHighMeanAnnualPrecipitation
   ;; [ 700 1150] colorado:HighMeanAnnualPrecipitation
   ;; [ 300  700] colorado:ModerateMeanAnnualPrecipitation
   ;; [ 200  300] colorado:LowMeanAnnualPrecipitation
   ;; [:<    200] colorado:VeryLowMeanAnnualPrecipitation))


(defmodel veg-soil-sequestration colorado:VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [10   20]    VeryHighSequestration
    [ 6   10]    HighSequestration
    [ 4    6]    ModerateSequestration
    [ 2.5  4]    LowSequestration
    [ 0.01 2.5]  VeryLowSequestration
    [ 0    0.01] NoSequestration))

;; Bayesian source model
(defmodel source colorado:CarbonSourceValue   
  (bayesian colorado:CarbonSourceValue 
    :import   "aries.core::CarbonSourceColorado.xdsl"
    :context  [vegetation-type percent-tree-canopy-cover annual-precipitation land-selector]
    :required [LandOrWater]
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

(defmodel soil-type colorado:SoilType
  (classification (categorization colorado:CarbonSoilType)
    "Alfisols"                 colorado:Alfisols
    #{"Aridisols" "Vertisols"} colorado:AridisolsVertisols
    #{"Entisols" "Ultisols"}   colorado:EntisolsUltisols
    "Inceptisols"              colorado:Inceptisols
    "Mollisols"                colorado:Mollisols
    :otherwise                 colorado:RockAndWater))

(defmodel beetle-kill colorado:MountainPineBeetleDamageClass
  (classification (count colorado:MountainPineBeetleDamageTreesPerHectare "/ha")
    [43 1236]  colorado:SevereDamage
    [ 5   43]  colorado:ModerateDamage
    [ 1    5]  colorado:LowDamage
    :otherwise colorado:NoDamage))

(defmodel fire-threat colorado:FireThreatClass ; This is actually fire return
                                      ; interval data, but uses
                                      ; student discretization.
  (classification (numeric-coding habitat:FireThreat) 
    [ 2   9] colorado:VeryHighFireThreat
    [ 9  12] colorado:HighFireThreat
    [12  18] colorado:ModerateFireThreat
    [18 133] colorado:LowFireThreat))

(defmodel veg-storage colorado:VegetationCarbonStorage
  (probabilistic-measurement colorado:VegetationCarbonStorage "t/ha" 
    [89  127]    colorado:VeryHighVegetationStorage
    [69   89]    colorado:HighVegetationStorage
    [37   69]    colorado:ModerateVegetationStorage
    [ 9.3 37]    colorado:LowVegetationStorage
    [ 0.01 9.3]  colorado:VeryLowVegetationStorage
    [ 0    0.01] colorado:NoVegetationStorage))

(defmodel vegetation-carbon-storage colorado:VegetationCStorage 
  (bayesian colorado:VegetationCStorage 
    :import   "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context  [percent-tree-canopy-cover vegetation-type land-selector]
    :required [LandOrWater]
    :keep     [colorado:VegetationCarbonStorage]
    :result   veg-storage))

;; Hack; remove once a proper context model is working
(defmodel vegetation-carbon-storage-no-beetle colorado:VegetationCStorageNoBeetle
  (bayesian colorado:VegetationCStorage 
    :import   "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context  [percent-tree-canopy-cover vegetation-type land-selector]
    :required [LandOrWater]
    :keep     [colorado:VegetationCarbonStorage]
    :result   veg-storage))

(defmodel soil-storage colorado:SoilCarbonStorage
  (probabilistic-measurement colorado:SoilCarbonStorage "t/ha" 
    [69   108]    colorado:VeryHighSoilStorage
    [46.6  69]    colorado:HighSoilStorage
    [27    46.6]  colorado:ModerateSoilStorage
    [10    27]    colorado:LowSoilStorage
    [ 0.01 10]    colorado:VeryLowSoilStorage
    [ 0     0.01] colorado:NoSoilStorage))

(defmodel soil-carbon-storage colorado:SoilCStorage 
  (bayesian colorado:SoilCStorage 
    :import   "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context  [soil-type annual-precipitation land-selector]
    :required [LandOrWater]
    :keep     [colorado:SoilCarbonStorage]
    :result   soil-storage))

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

(defmodel veg-soil-storage colorado:VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [159   235]    colorado:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [116   159]    colorado:HighStorage
    [ 64   116]    colorado:ModerateStorage
    [ 10    64]    colorado:LowStorage
    [  0.02 10]    colorado:VeryLowStorage
    [  0     0.02] colorado:NoStorage))

;; One more hack for good measure
(defmodel veg-soil-storage-no-beetle colorado:VegetationAndSoilCarbonStorageClassNoBeetle
  (classification vegetation-soil-storage-no-beetle
    [159   235]    colorado:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [116   159]    colorado:HighStorage
    [ 64   116]    colorado:ModerateStorage
    [ 10    64]    colorado:LowStorage
    [  0.02 10]    colorado:VeryLowStorage
    [  0     0.02] colorado:NoStorage))

(defmodel stored-carbon-release colorado:StoredCarbonRelease
  (probabilistic-measurement colorado:StoredCarbonRelease "t/ha*year"
    [59   118]    colorado:VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [29.5  59]    colorado:HighRelease
    [14.8  29.5]  colorado:ModerateRelease
    [ 7.4  14.8]  colorado:LowRelease
    [ 0.02  7.4]  colorado:VeryLowRelease
    [ 0     0.02] colorado:NoRelease))

(defmodel sink colorado:CarbonSinkValue
  (bayesian colorado:CarbonSinkValue
    :import   "aries.core::CarbonSinkColorado.xdsl"
    :context  [veg-soil-storage fire-threat land-selector]
    :required [LandOrWater]
    :keep     [colorado:StoredCarbonRelease]
    :result   stored-carbon-release))

;; Hack, remove when contexts are working properly.
(defmodel sink-no-beetle colorado:CarbonSinkValueNoBeetle
  (bayesian colorado:CarbonSinkValueNoBeetle
    :import   "aries.core::CarbonSinkColoradoNoBeetle.xdsl"
    :context  [veg-soil-storage-no-beetle fire-threat land-selector]
    :required [LandOrWater]
    :keep     [colorado:StoredCarbonRelease]
    :result   stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;: GHG emissions map for the U.S.
(defmodel use-simple GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel identification-carbon CarbonSequestrationAndStorage
  (identification CarbonSequestrationAndStorage
    :context [source sink use-simple]))

;; Hack, remove when contexts are working properly.
(defmodel identification-carbon-no-beetle colorado:ClimateStabilityNoBeetle
  (identification CarbonSequestrationAndStorage
    :context [source sink-no-beetle use-simple]))

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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------