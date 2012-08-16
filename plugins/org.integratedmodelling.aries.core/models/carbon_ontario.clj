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
;;; Carbon model for Ontario
;;;
;;; Valid Contexts: core.contexts.ontario/algonquin-wgs84
;;;                 core.contexts.ontatio/algonquin-bbox-wgs84
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-ontario
  (:refer-clojure :rename {count length})
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario namespace-ontology model
                           defmodel measurement classification
                           categorization ranking numeric-coding
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking binary-coding
                           identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology carbonService
  (thinklab-core:BooleanRanking
   (VegetatedLand
    (OnVegetatedLand)
    (NotOnVegetatedLand)))
  (owl:Thing (AllCarbonData)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(declare carbon-vegetation-type
         percent-tree-canopy-cover-class
         successional-stage
         summer-high-winter-low
         vegetated-land
         vegetation-and-soil-carbon-sequestration
         carbon-source-value)

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

;; ontario:lulc2000_alg
;; ontario:lakes_alg
(defmodel vegetated-land VegetatedLand
  (classification VegetatedLand
    :context [(numeric-coding ontario-lulc:MNRLULCNumeric)
              (binary-coding geofeatures:Lake)]
    :state   #(if (or (== (:lake %) 1)
                      (contains? #{1 2 3 4 5 6 15 16 17 20 22 24 28 29} (:m-n-r-l-u-l-c-numeric %)))
                (tl/conc 'carbonService:NotOnVegetatedLand)
                (tl/conc 'carbonService:OnVegetatedLand))))

;; ontario:successional_stage_alg
(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    6 OldGrowth
    5 LateSuccession
    4 MidSuccession
    3 PoleSuccession
    2 EarlySuccession
    1 NoSuccession))

;; ontario:canopy_alg
(defmodel percent-tree-canopy-cover-class PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [0.8 1.0 :inclusive] VeryHighCanopyCover
    [0.6 0.8]            HighCanopyCover
    [0.4 0.6]            ModerateCanopyCover
    [0.2 0.4]            LowCanopyCover
    [0.0 0.2]            VeryLowCanopyCover))

;; global:sum_hi_wint_lo_global
(defmodel summer-high-winter-low SummerHighWinterLow
  (classification (ranking ontario:SummerHighWinterLow)
    [44 45 :inclusive] VeryHighSOL
    [43 44]            HighSOL
    [42 43]            ModerateSOL
    [41 42]            LowSOL
    [40 41]            VeryLowSOL))

(defmodel soil-cn-ratio SoilCNRatio
  (classification (ranking habitat:SoilCNRatio)
    [25 :>] VeryHighCNRatio
    [15 25] HighCNRatio
    [ 8 15] LowCNRatio
    [:<  8] VeryLowCNRatio))

;;(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
;;(classification (ranking habitat:HardwoodSoftwoodRatio)
;;  [0   20]            VeryHighHardness
;;  [20  40]            HighHardness
;;  [40  60]            ModerateHardness
;;  [60  80]            LowHardness
;;  [80 100 :inclusive] VeryLowHardness))

(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
  (classification (ranking habitat:HardwoodSoftwoodRatio)
    [80 100 :inclusive]     VeryHighHardness
    [60  80]                HighHardness
    [40  60]                ModerateHardness
    [20  40]                LowHardness
    [0   20]                VeryLowHardness))

;; ontario:lulc2000_alg
(defmodel carbon-vegetation-type ontario:CarbonVegetationType
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    13             ontario:ConiferousForest
    11             ontario:DeciduousForest
    12             ontario:MixedForest
    #{7 8 9 10}    ontario:ImpairedForest
    #{18 19 21 23} ontario:SwampFenBog
    #{25 27}       ontario:CroplandPasture))

;; (defmodel veg-storage VegetationCarbonStorage
;;   (probabilistic-measurement VegetationCarbonStorage "t/ha"
;;     [500 900] VeryHighVegetationStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
;;     [200 500] HighVegetationStorage
;;     [75 200]  ModerateVegetationStorage
;;     [25 75]   LowVegetationStorage
;;     [0.01 25] VeryLowVegetationStorage
;;     [0 0.01]  NoVegetationStorage))

;; Ceiling based off highest local values from MODIS NPP data
;; global:npp_modis -> measure -d carbonService:VegetationAndSoilCarbonSequestration t/ha*year core.contexts.ontario/algonquin-wgs84
;;(defmodel vegetation-and-soil-carbon-sequestration VegetationAndSoilCarbonSequestration
;;(probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
;;  [26.00 33.00] VeryHighSequestration
;;  [19.50 26.00] HighSequestration
;;  [13.00 19.50] ModerateSequestration
;;  [ 6.50 13.00] LowSequestration
;;  [ 0.01  6.50] VeryLowSequestration
;;  [ 0.00  0.01] NoSequestration))

;; Ceiling based off data and references provided by Kim Taylor (24
;; May 2012). Barr et al 2002 & FLUXNET data report from 2011 indicate
;; that "All of the values in Ontario are under 3.0 t C/ha/yr they are
;; about 1.7 t C/ha/yr for the province"
(defmodel vegetation-and-soil-carbon-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
  [1.20  1.70] VeryHighSequestration
  [0.85  1.20] HighSequestration
  [0.50  0.85] ModerateSequestration
  [0.15  0.50] LowSequestration
  [0.01  0.15] VeryLowSequestration
  [0.00  0.01] NoSequestration))


(defmodel carbon-source-value CarbonSourceValue
  (bayesian CarbonSourceValue
    :import   "aries.core::CarbonSourceOntario.xdsl"
    :context  [altitude vegetated-land successional-stage percent-tree-canopy-cover-class summer-high-winter-low
               soil-cn-ratio hardwood-softwood-ratio carbon-vegetation-type]
    :required [VegetatedLand]
    :keep     [VegetationAndSoilCarbonSequestration]
    :result   vegetation-and-soil-carbon-sequestration))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(declare soil-oxygen-conditions
         soil-ph
         slope-class
         soil-carbon-storage
         soil-c-storage
         vegetation-carbon-storage
         vegetation-c-storage
         deforestation-risk-class
         fire-threat-class
         vegetation-and-carbon-soil-storage
         vegetation-and-carbon-soil-storage-class
         stored-carbon-release
         carbon-sink-value)

;; ontario:lulc2000_alg
(defmodel soil-oxygen-conditions SoilOxygenConditions
  (classification (numeric-coding ontario-lulc:MNRLULCNumeric)
    #{18 19 21 23}             AnoxicSoils
    #{7 8 9 10 11 12 13 25 27} OxicSoils))

;; FIXME: Not sure how to discretize this, since the global layer (below) shows 1 everywhere.
;; global:soil_pH_0_30_global
(defmodel soil-ph SoilPh
  (classification (ranking ontario:SoilPhShallow)
    [10 14 :inclusive] HighPh
    [ 5 10]            ModeratePh
    [ 0  5]            LowPh))

;; ontario:slope10m_alg
(defmodel slope-class SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15]            Level
    [ 1.15  4.57]            GentlyUndulating
    [ 4.57 16.70]            RollingToHilly
    [16.70 90.00 :inclusive] SteeplyDissectedToMountainous))

;; FIXME: Not sure how to discretize this, since the global layer (below) shows 160 t/ha*year everywhere.
;; global:soil_carbon_storage -> measure -d carbonService:SoilCarbonStorage t/ha core.contexts.ontario/algonquin-wgs84
(defmodel soil-carbon-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha*year"
    [128.00 160.00 :inclusive] VeryHighSoilStorage
    [ 96.00 128.00]            HighSoilStorage
    [ 64.00  96.00]            ModerateSoilStorage
    [ 32.00  64.00]            LowSoilStorage
    [  0.01  32.00]            VeryLowSoilStorage
    [  0.00   0.01]            NoSoilStorage))

(defmodel soil-c-storage SoilCStorage
  (bayesian SoilCStorage
    :import   "aries.core::CarbonSinkOntario.xdsl"
    :context  [soil-oxygen-conditions soil-ph slope-class carbon-vegetation-type
               percent-tree-canopy-cover-class successional-stage vegetated-land]
    :required [VegetatedLand]
    :keep     [SoilCarbonStorage]
    :result   soil-carbon-storage))

;; global:biomass_carbon_global -> measure -d carbonService:VegetationCarbonStorage t/ha core.contexts.ontario/algonquin-wgs84
(defmodel vegetation-carbon-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha*year"
    [64.00 80.00 :inclusive] VeryHighVegetationStorage
    [48.00 64.00]            HighVegetationStorage
    [32.00 48.00]            ModerateVegetationStorage
    [16.00 32.00]            LowVegetationStorage
    [ 0.01 16.00]            VeryLowVegetationStorage
    [ 0.00  0.01]            NoVegetationStorage))

(defmodel vegetation-c-storage VegetationCStorage
  (bayesian VegetationCStorage
    :import   "aries.core::CarbonSinkOntario.xdsl"
    :context  [carbon-vegetation-type percent-tree-canopy-cover-class
               successional-stage summer-high-winter-low vegetated-land]
    :required [VegetatedLand]
    :keep     [VegetationCarbonStorage]
    :result   vegetation-carbon-storage))

;; ontario:deforestation_risk_alg
(defmodel deforestation-risk-class DeforestationRiskClass
  (classification (ranking carbonService:DeforestationRisk)
    4 HighDeforestationRisk
    3 ModerateDeforestationRisk
    2 LowDeforestationRisk
    1 NoDeforestationRisk))

;; ontario:fire_frequency_alg
(defmodel fire-threat-class ontario:FireThreatClass
  (classification (ranking habitat:FireThreat)
    3 ontario:HighFireThreat
    2 ontario:ModerateFireThreat
    1 ontario:LowFireThreat))

(defmodel vegetation-and-soil-carbon-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha*year"
    :context [vegetation-c-storage soil-c-storage]
    :state   #(+ (if-let [v (:vegetation-c-storage %)] (.getMean v) 0.0)
                 (if-let [s (:soil-c-storage       %)] (.getMean s) 0.0))))

(defmodel vegetation-and-soil-carbon-storage-class VegetationAndSoilCarbonStorageClass
  (classification vegetation-and-soil-carbon-storage
    [192.00 240.00] VeryHighStorage
    [144.00 192.00] HighStorage
    [ 96.00 144.00] ModerateStorage
    [ 48.00  96.00] LowStorage
    [  0.01  48.00] VeryLowStorage
    [  0.00   0.01] NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [192.00 240.00] VeryHighRelease
    [144.00 192.00] HighRelease
    [ 96.00 144.00] ModerateRelease
    [ 48.00  96.00] LowRelease
    [  0.01  48.00] VeryLowRelease
    [  0.00   0.01] NoRelease))

(defmodel carbon-sink-value CarbonSinkValue
  (bayesian CarbonSinkValue
    :import   "aries.core::CarbonSinkOntario.xdsl"
    :context  [deforestation-risk-class fire-threat-class vegetation-and-soil-carbon-storage-class vegetated-land]
    :required [VegetatedLand]
    :keep     [StoredCarbonRelease]
    :result   stored-carbon-release))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;
;; calculating the difference between source and use and estimating
;; the number of people the sequestration capacity of the park can handle
;;

;;;-------------------------------------------------------------------
;;; Routing models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel all-carbon-data AllCarbonData
  (identification AllCarbonData
    :context [carbon-source-value
              carbon-sink-value]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------
