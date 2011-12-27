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
;;; Carbon model for San Pedro
;;;
;;; Valid Contexts: core.contexts.beta/san_pedro_us*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-san-pedro
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

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 100 :inclusive] VeryHighCanopyCover
    [60  80]            HighCanopyCover
    [40  60]            ModerateCanopyCover
    [20  40]            LowCanopyCover
    [ 0  20]            VeryLowCanopyCover))

;; Add the Mexican layers in if/when cross-boundary data integration is enabled.
(defmodel vegetation-type sanPedro:CarbonVegetationType
  "Reclass of SWReGAP & CONABIO LULC layers"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{22 23 24 25 26 27 28 29 30 31 32 34 35 36 37 38 45 92}                           sanPedro:Forest
    #{33 41 91}                                                                        sanPedro:OakWoodland
    #{52 109}                                                                          sanPedro:MesquiteWoodland
    #{62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 90 93}                              sanPedro:Grassland
    #{19 39 40 42 43 44 46 47 48 49 50 51 53 54 55 56 57 58 59 60 61 94 95 96 105 108} sanPedro:DesertScrub
    #{77 78 79 80 81 83 84 85 98 118}                                                  sanPedro:Riparian
    #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 110 111 112 114}              sanPedro:UrbanBarrenWaterAgriculture))
;;  (classification (categorization mexico:CONABIOLULCCategory)
;;    #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"}                  sanPedro:Forest
;;    #{"Bosque de encino" "Vegetacion de galeria"}                                sanPedro:OakWoodland
;;    #{"Mezquital-huizachal"}                                                     sanPedro:MesquiteWoodland
;;    #{"Pastizal natural"}                                                        sanPedro:Grassland
;;    #{"Chaparral" "Matorral desertico microfilo" "Mattoral sarcocrasicaule" "Vegetacion halofila y gipsofila" "Vegetacion de suelos arenosos"} sanPedro:DesertScrub
;;    #{"Manejo agricola, pecuario y forestal (plantaciones)"}                     sanPedro:Riparian
;;    #{"Cuerpos de agua" "Ciudades importantes" "Areas sin vegetacion aparente"}  sanPedro:UrbanBarrenWaterAgriculture))

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
    [3 4.6]  VeryHighSequestration
    [2 3]    HighSequestration
    [1.5 2]  ModerateSequestration
    [1 1.5]  LowSequestration
    [0.01 1] VeryLowSequestration ; Common annual values for desert scrub & desert grassland (Svejvcar et al. 2008); values can also be negative in dry years, should ideally account for that too.
    [0 0.01] NoSequestration))

;; Bayesian source model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
    :import  "aries.core::CarbonSourceSanPedro.xdsl"
    :context [vegetation-type percent-canopy-cover annual-precipitation]
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

;; Using deep soil pH for grasslands and deserts, shallow for all
;; other ecosystem types This should work OK with both global & SSURGO
;; data, but check to make sure.
(defmodel soil-ph SoilPh
  (classification (ranking habitat:SoilPhDeep)
    [7.3 :>]           HighPh
    [5.5 7.3]          ModeratePh
    [:exclusive 0 5.5] LowPh))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:<     1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

;; Use NLCD or GLC layers to infer anoxic vs. oxic: no Mexican LULC
;; data (i.e., CONABIO) denote wetlands at least for Sonora.
(defmodel oxygen SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}   AnoxicSoils
    :otherwise OxicSoils)
  (classification (numeric-coding glc:GLCNumeric)
    15         AnoxicSoils
    :otherwise OxicSoils))

;; Per Schussman et al. (2006), the middle of each of these ranges is
;; around every 5 yrs for high frequency, 50 yrs for moderate, 200 yrs
;; for low.
(defmodel fire-frequency FireFrequency
  (classification (numeric-coding habitat:FireReturnInterval) 
    1      HighFireFrequency
    #{2 3} ModerateFireFrequency ; includes "variable" fire frequency
    4      LowFireFrequency
    #{5 6} NoFireFrequency))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha" 
    [75 100] VeryHighVegetationStorage
    [20 75]  HighVegetationStorage
    [5 20]   ModerateVegetationStorage
    [2 5]    LowVegetationStorage
    [0.01 2] VeryLowVegetationStorage
    [0 0.01] NoVegetationStorage))

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
    :import  "aries.core::CarbonSinkSanPedro.xdsl"
    :context [annual-precipitation percent-canopy-cover vegetation-type]
    :keep    [VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha" 
    [40 80]        VeryHighSoilStorage
    [20 40]        HighSoilStorage
    [5 20]         ModerateSoilStorage
    [2 5]          LowSoilStorage
    [0.01 2]       VeryLowSoilStorage
    [0 0.01]       NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
    :import  "aries.core::CarbonSinkSanPedro.xdsl"
    :context [soil-ph slope oxygen percent-canopy-cover vegetation-type]
    :keep    [SoilCarbonStorage]
    :result  soil-storage))

;;Consider reworking the soil carbon storage part of the model based
;; on Martens et al. 2005 - soil texture, precip, temperature as most
;; important correlates of high soil carbon storage.

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [50 180] VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [15 50]  HighStorage
    [6 15]   ModerateStorage
    [3 6]    LowStorage
    [0.02 3] VeryLowStorage
    [0 0.02] NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [12 90]  VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [9 12]   HighRelease
    [6 9]    ModerateRelease
    [3 6]    LowRelease
    [0.02 3] VeryLowRelease
    [0 0.02] NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
    :import  "aries.core::CarbonSinkSanPedro.xdsl"
    :context [veg-soil-storage fire-frequency]
    :keep    [StoredCarbonRelease]
    :result  stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;: GHG emissions map for the U.S.  For the rest of the world, use
;; global population density layer multiplied by per capita emissions
;; for that country from EIA.  2006 data used as this corresponds to
;; current population density layer: 4.05 tonnes CO2/capita for Mexico
;; in 2006, which is equivalent to 1.105 tonnes C/capita

(defmodel use-simple GreenhouseGasEmissions
  [(categorization geofeatures:Country)]
  (measurement GreenhouseGasEmissions "t/ha*year")
  :when #(= (:country %) "United States")
  (measurement GreenhouseGasEmissions "t/ha*year"
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(* (:population-density %) 1.105)))

(defmodel use-simple-pop GreenhouseGasEmissions
  (measurement GreenhouseGasEmissions "t/ha*year"
    :context [(count policytarget:PopulationDensity "/km^2")]
    :state   #(* (:population-density %) 1.105)))

(defmodel use-simple-us GreenhouseGasEmissions
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

(defmodel constrained-development-scenario sanPedro:ConstrainedDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained) 
    #{10 11 12 13 19 22 25}                sanPedro:DevelopedConstrained
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28} sanPedro:NotDevelopedConstrained))

(defmodel open-development-scenario sanPedro:OpenDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen) 
    #{10 11 12 13 19 22 25}                   sanPedro:DevelopedOpen
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28 29} sanPedro:NotDevelopedOpen))

(defmodel vegetation-type-constrained CarbonVegetationTypeConstrained
  "Reclass of Steinitz LULC layers where they have coverage"
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained)
    1                                 sanPedro:Forest
    2                                 sanPedro:OakWoodland
    #{6 26}                           sanPedro:MesquiteWoodland
    #{4 5}                            sanPedro:Grassland
    #{7 23}                           sanPedro:DesertScrub
    #{27 28 29 30}                    sanPedro:Riparian
    #{8 9 10 11 12 13 14 16 19 22 25} sanPedro:UrbanBarrenWaterAgriculture))

(defmodel vegetation-type-open CarbonVegetationTypeOpen
  "Reclass of Steinitz LULC layers where they have coverage"
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen)
    1                                 sanPedro:Forest
    2                                 sanPedro:OakWoodland
    #{6 26}                           sanPedro:MesquiteWoodland
    #{4 5}                            sanPedro:Grassland
    #{7 23}                           sanPedro:DesertScrub
    #{27 28 29 30}                    sanPedro:Riparian
    #{8 9 10 11 12 13 14 16 19 22 25} sanPedro:UrbanBarrenWaterAgriculture))

(defscenario open-development-carbon
  "Changes values in developed areas to very low vegetation cover, no
fire frequency, increased greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:VeryLowCanopyCover)
                  (:pcc %))))
  (model FireFrequency
    (classification FireFrequency
      :context [open-development-scenario :as od fire-frequency :as ff]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'carbonService:NoFireFrequency)    
                  (:ff %))))
  (model sanPedro:CarbonVegetationType
    (classification sanPedro:CarbonVegetationType
      :context [vegetation-type-open :as vto vegetation-type :as vt]
      :state   #(if (no-data? (:vto %))
                  (:vt %)
                  (:vto %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
      :context [open-development-scenario :as od (measurement GreenhouseGasEmissions "t/ha*year")]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (* 1.568 (:greenhouse-gas-emissions %)) ; Reflects 56.8% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))

(defscenario constrained-development-carbon
  "Changes values in developed areas to very low vegetation cover, no
fire frequency, increased greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'carbonService:VeryLowCanopyCover)
                  (:pcc %))))
  (model FireFrequency
    (classification FireFrequency
      :context [constrained-development-scenario :as cd fire-frequency :as ff]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'carbonService:NoFireFrequency)
                  (:ff %))))
  (model sanPedro:CarbonVegetationType
    (classification sanPedro:CarbonVegetationType
      :context [vegetation-type-constrained :as vtc vegetation-type :as vt]
      :state   #(if (no-data? (:vtc %))
                  (:vt %)
                  (:vtc %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
      :context [constrained-development-scenario :as cd use-simple]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (* 1.104 (:greenhouse-gas-emissions %)) ; Reflects 10.4% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))