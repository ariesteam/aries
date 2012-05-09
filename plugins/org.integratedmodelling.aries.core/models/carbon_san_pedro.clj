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
;;; Valid Contexts: core.contexts.san-pedro/san-pedro-us*
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
	(CarbonVegetationType (CarbonVegetationTypeOpen) (CarbonVegetationTypeConstrained))
 (FireThreat (VeryHighFireThreat) (HighFireThreat)(ModerateFireThreat) (LowFireThreat)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; ARIES defines sources of carbon areas that are sequester carbon
;; in vegetation and soils. Sinks are emissions from areas at risk
;; of deforestation or fire, which can release carbon into the
;; atmosphere.  The difference between carbon sinks and sources is the
;; amount remaining to mitigate direct anthropogenic emissions (aside
;; from land conversion and fire).

(defmodel percent-canopy-cover sanPedro:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 100 :inclusive] sanPedro:VeryHighCanopyCover
    [60  80]            sanPedro:HighCanopyCover
    [40  60]            sanPedro:ModerateCanopyCover
    [20  40]            sanPedro:LowCanopyCover
    [ 0  20]            sanPedro:VeryLowCanopyCover))

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

(defmodel annual-precipitation sanPedro:AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [500  :>] sanPedro:HighMeanAnnualPrecipitation
    [400 500] sanPedro:ModerateMeanAnnualPrecipitation
    [:<  400] sanPedro:LowMeanAnnualPrecipitation))

(defmodel veg-soil-sequestration sanPedro:VegetationAndSoilCarbonSequestration
  (probabilistic-measurement sanPedro:VegetationAndSoilCarbonSequestration "t/ha*year"
    [3 4.6]  sanPedro:VeryHighSequestration
    [2 3]    sanPedro:HighSequestration
    [1.5 2]  sanPedro:ModerateSequestration
    [1 1.5]  sanPedro:LowSequestration
    [0.01 1] sanPedro:VeryLowSequestration ; Common annual values for desert scrub & desert grassland (Svejvcar et al. 2008); values can also be negative in dry years, should ideally account for that too.
    [0 0.01] sanPedro:NoSequestration))

;; Bayesian source model
(defmodel source sanPedro:CarbonSourceValue
  (bayesian sanPedro:CarbonSourceValue
    :import  "aries.core::CarbonSourceSanPedro.xdsl"
    :context [vegetation-type percent-canopy-cover annual-precipitation]
    :keep    [sanPedro:VegetationAndSoilCarbonSequestration]
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
(defmodel soil-ph sanPedro:SoilPh
  (classification (ranking habitat:SoilPhDeep)
    [7.3 :>]           sanPedro:HighPh
    [5.5 7.3]          sanPedro:ModeratePh
    [:exclusive 0 5.5] sanPedro:LowPh))

(defmodel slope sanPedro:SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:<     1.15] sanPedro:Level
    [ 1.15  4.57] sanPedro:GentlyUndulating
    [ 4.57 16.70] sanPedro:RollingToHilly
    [16.70    :>] sanPedro:SteeplyDissectedToMountainous))

;; Use NLCD or GLC layers to infer anoxic vs. oxic: no Mexican LULC
;; data (i.e., CONABIO) denote wetlands at least for Sonora.
(defmodel oxygen sanPedro:SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}   sanPedro:AnoxicSoils
    :otherwise sanPedro:OxicSoils)
  (classification (numeric-coding glc:GLCNumeric)
    15         sanPedro:AnoxicSoils
    :otherwise sanPedro:OxicSoils))

;; Per Schussman et al. (2006), the middle of each of these ranges is
;; around every 5 yrs for high frequency, 50 yrs for moderate, 200 yrs
;; for low.
(defmodel fire-threat sanPedro:FireThreatClass
  (classification (numeric-coding habitat:FireReturnInterval) 
    1      sanPedro:VeryHighFireThreat
    #{2 3} sanPedro:HighFireThreat ; includes "variable" fire frequency
    4      sanPedro:ModerateFireThreat
    #{5 6} sanPedro:LowFireThreat))

(defmodel veg-storage sanPedro:VegetationCarbonStorage
  (probabilistic-measurement sanPedro:VegetationCarbonStorage "t/ha" 
    [75 100] sanPedro:VeryHighVegetationStorage
    [20  75] sanPedro:HighVegetationStorage
    [10  20] sanPedro:ModerateVegetationStorage
    [2   10] sanPedro:LowVegetationStorage
    [0.01 2] sanPedro:VeryLowVegetationStorage
    [0 0.01] sanPedro:NoVegetationStorage))

(defmodel vegetation-carbon-storage sanPedro:VegetationCStorage 
  (bayesian sanPedro:VegetationCStorage 
    :import  "aries.core::trained/CarbonSinkSanPedro.xdsl"
    :context [annual-precipitation percent-canopy-cover vegetation-type]
    :keep    [sanPedro:VegetationCarbonStorage]
    :result  veg-storage))

(defmodel soil-storage sanPedro:SoilCarbonStorage
  (probabilistic-measurement sanPedro:SoilCarbonStorage "t/ha" 
    [100  150]    sanPedro:VeryHighSoilStorage
    [ 70  100]    sanPedro:HighSoilStorage
    [ 50   70]    sanPedro:ModerateSoilStorage
    [  2   50]    sanPedro:LowSoilStorage
    [  0.01 2]    sanPedro:VeryLowSoilStorage
    [  0    0.01] sanPedro:NoSoilStorage))

(defmodel soil-carbon-storage sanPedro:SoilCStorage 
  (bayesian sanPedro:SoilCStorage 
    :import  "aries.core::trained/CarbonSinkSanPedro.xdsl"
    :context [soil-ph slope oxygen percent-canopy-cover vegetation-type]
    :keep    [sanPedro:SoilCarbonStorage]
    :result  soil-storage))

;;Consider reworking the soil carbon storage part of the model based
;; on Martens et al. 2005 - soil texture, precip, temperature as most
;; important correlates of high soil carbon storage.

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state   #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
                 (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage sanPedro:VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [50 180] sanPedro:VeryHighStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [15 50]  sanPedro:HighStorage
    [6 15]   sanPedro:ModerateStorage
    [3 6]    sanPedro:LowStorage
    [0.02 3] sanPedro:VeryLowStorage
    [0 0.02] sanPedro:NoStorage))

(defmodel stored-carbon-release sanPedro:StoredCarbonRelease
  (probabilistic-measurement sanPedro:StoredCarbonRelease "t/ha*year"
    [12 90]  sanPedro:VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [9 12]   sanPedro:HighRelease
    [6 9]    sanPedro:ModerateRelease
    [3 6]    sanPedro:LowRelease
    [0.02 3] sanPedro:VeryLowRelease
    [0 0.02] sanPedro:NoRelease))

(defmodel sink sanPedro:CarbonSinkValue   
  (bayesian sanPedro:CarbonSinkValue 
    :import  "aries.core::trained/CarbonSinkSanPedro.xdsl"
    :context [veg-soil-storage fire-threat]
    :keep    [sanPedro:StoredCarbonRelease]
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
  (model sanPedro:PercentTreeCanopyCoverClass
    (classification sanPedro:PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:VeryLowCanopyCover)
                  (:pcc %))))
  (model sanPedro:FireThreatClass
    (classification sanPedro:FireThreatClass
      :context [open-development-scenario :as od fire-threat :as ff]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:LowFireThreat)    
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
                  (* 1.568 (or (:greenhouse-gas-emissions %) 0)) ; Reflects 56.8% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))

(defscenario constrained-development-carbon
  "Changes values in developed areas to very low vegetation cover, no
fire frequency, increased greenhouse gas emissions."
  (model sanPedro:PercentTreeCanopyCoverClass
    (classification sanPedro:PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:VeryLowCanopyCover)
                  (:pcc %))))
  (model sanPedro:FireThreatClass
    (classification sanPedro:FireThreatClass
      :context [constrained-development-scenario :as cd fire-threat :as ff]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:LowFireThreat)
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
                  (* 1.104 (or (:greenhouse-gas-emissions %) 0)) ; Reflects 10.4% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))