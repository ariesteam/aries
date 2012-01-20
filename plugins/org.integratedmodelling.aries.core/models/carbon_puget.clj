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
;;; Carbon model for Western Washington
;;;
;;; Valid Contexts: core.contexts.beta/{chehalis,wria9,viewshed,western_wa}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.carbon-puget
  (:refer-clojure :rename {count length}) 
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario defmodel model measurement
                           classification categorization ranking
                           numeric-coding binary-coding
                           probabilistic-measurement
                           probabilistic-classification identification
                           bayesian namespace-ontology count])
  (:refer aries :only [span]))

;; defines the ontology associated with this namespace, which may or may not exist.
(namespace-ontology carbonService
  (PercentTreeCanopyCoverClass :editable "true")
  (FireThreat :editable "true")
  (GreenhouseGasEmissions :editable "true")
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

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))  

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification  (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    #{5 6}                           OldGrowth
    4                                LateSuccession
    3                                MidSuccession
    2                                PoleSuccession
    1                                EarlySuccession
    #{21 22 23 24 25 26 27 28 40 41} NoSuccession))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    [80 100 :inclusive] VeryHighCanopyCover
    [60  80]            HighCanopyCover
    [40  60]            ModerateCanopyCover
    [20  40]            LowCanopyCover
    [ 0  20]            VeryLowCanopyCover))

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

(defmodel hardwood-softwood-ratio HardwoodSoftwoodRatio
  (classification (ranking habitat:HardwoodSoftwoodRatio)
    [1  2]            VeryHighHardness
    [2  4]            HighHardness
    [4  6]            ModerateHardness
    [6  8]            LowHardness
    [8 10 :inclusive] VeryLowHardness))

;; Ceiling based off highest local values from MODIS NPP data.
(defmodel veg-soil-sequestration VegetationAndSoilCarbonSequestration
  (probabilistic-measurement VegetationAndSoilCarbonSequestration "t/ha*year"
    [9 14]   VeryHighSequestration
    [6.5 9]  HighSequestration
    [4 6.5]  ModerateSequestration
    [2 4]    LowSequestration
    [0.01 2] VeryLowSequestration
    [0 0.01] NoSequestration))

;; Bayesian sink model
(defmodel source CarbonSourceValue   
  (bayesian CarbonSourceValue 
    :import   "aries.core::CarbonSourcePuget.xdsl"
    :context  [hardwood-softwood-ratio soil-cn-ratio summer-high-winter-low 
               percent-canopy-cover successional-stage land-selector]
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

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:<     1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

;; Using deep soil pH for grasslands and deserts, shallow for all other ecosystem types
(defmodel soil-ph SoilPh
  (classification (ranking habitat:SoilPhShallow)
    [7.3 :>]           HighPh
    [5.5 7.3]          ModeratePh
    [:exclusive 0 5.5] LowPh))

; use NLCD layers to infer anoxic vs. oxic
(defmodel oxygen SoilOxygenConditions 
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}   AnoxicSoils
    :otherwise OxicSoils))

(defmodel fire-threat FireThreatClass
  (classification (measurement habitat:FireFrequency "/km^2")
    [0.9 :>]    VeryHighFireThreat
    [0.25 0.9]  HighFireThreat
    [0.01 0.25] ModerateFireThreat
    [0    0.01] LowFireThreat))

(defmodel veg-storage VegetationCarbonStorage
  (probabilistic-measurement VegetationCarbonStorage "t/ha"
    [500 900] VeryHighVegetationStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [200 500] HighVegetationStorage
    [75 200]  ModerateVegetationStorage
    [25 75]   LowVegetationStorage
    [0.01 25] VeryLowVegetationStorage
    [0 0.01]  NoVegetationStorage))

(defmodel vegetation-carbon-storage VegetationCStorage 
  (bayesian VegetationCStorage 
    :import   "aries.core::CarbonSinkPuget.xdsl"
    :context  [percent-canopy-cover hardwood-softwood-ratio 
               successional-stage summer-high-winter-low land-selector]
    :required [LandOrSea]
    :keep     [VegetationCarbonStorage]
    :result   veg-storage))

(defmodel soil-storage SoilCarbonStorage
  (probabilistic-measurement SoilCarbonStorage "t/ha" 
    [60 115] VeryHighSoilStorage ; Ceiling is a very high carbon storage value for the region's forests from Smith et al. (2006).
    [30 60]  HighSoilStorage
    [15 30]  ModerateSoilStorage
    [5 15]   LowSoilStorage
    [0.01 5] VeryLowSoilStorage
    [0 0.01] NoSoilStorage))

(defmodel soil-carbon-storage SoilCStorage 
  (bayesian SoilCStorage 
    :import   "aries.core::CarbonSinkPuget.xdsl"
    :context  [soil-ph slope oxygen percent-canopy-cover hardwood-softwood-ratio 
               successional-stage soil-cn-ratio land-selector]
    :required [LandOrSea]
    :keep     [SoilCarbonStorage]
    :result   soil-storage))

(defmodel vegetation-soil-storage VegetationAndSoilCarbonStorage
  (measurement VegetationAndSoilCarbonStorage "t/ha"
    :context [vegetation-carbon-storage soil-carbon-storage]
    :state #(+ (if (nil? (:vegetation-c-storage %)) 0.0 (.getMean (:vegetation-c-storage %)))
               (if (nil? (:soil-c-storage %))       0.0 (.getMean (:soil-c-storage %))))))

(defmodel veg-soil-storage VegetationAndSoilCarbonStorageClass
  (classification vegetation-soil-storage
    [550 1015] VeryHighStorage
    [250 550]  HighStorage
    [100 250]  ModerateStorage
    [30 100]   LowStorage
    [0.01 30]  VeryLowStorage
    [0 0.01]   NoStorage))

(defmodel stored-carbon-release StoredCarbonRelease
  (probabilistic-measurement StoredCarbonRelease "t/ha*year"
    [200 500] VeryHighRelease ; Ceiling for stored carbon release is set as half of the total carbon in the system - check this assumption.
    [100 200] HighRelease
    [50 100]  ModerateRelease
    [20 50]   LowRelease
    [0.01 20] VeryLowRelease
    [0 0.01]  NoRelease))

(defmodel sink CarbonSinkValue   
  (bayesian CarbonSinkValue 
    :import   "aries.core::CarbonSinkPuget.xdsl"
    :context  [veg-soil-storage fire-threat land-selector]
    :required [LandOrSea]
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

(defscenario ipcc-hadley-a2-incentivized 
  "This scenario represents the effects of the Hadley A1 IPCC climate
  scenario. A12 represents a future world of very rapid economic
  growth, global population that peaks in mid-century and declines
  thereafter, and rapid introduction of new and more efficient
  technologies."
  ;; old growth has been incentivized, so what was late succession is
  ;; now old growth
  (model SuccessionalStage
    (classification (ranking ecology:SuccessionalStage)
      #{5 6 4}   OldGrowth
      3          MidSuccession
      2          EarlySuccession
      1          PoleSuccession
      :otherwise NoSuccession)))

(defscenario ipcc-hadley-b2-incentivized 
  "This scenario represents the effects of the Hadley B1 IPCC climate
  scenario. The B1 world is a convergent world with the same global
  population as in the A1 storyline but with rapid changes in economic
  structures toward a service and information economy, with reductions
  in material intensity, and the introduction of clean and
  resource-efficient technologies."
  ;; old growth has been incentivized, so what was late succession is
  ;; now old growth
  (model SuccessionalStage
    (classification (ranking ecology:SuccessionalStage)
      #{5 6 4}   OldGrowth
      3          MidSuccession
      2          EarlySuccession
      1          PoleSuccession
      :otherwise NoSuccession)))

(defscenario ipcc-hadley-a2 
  "This scenario represents the effects of the Hadley A2 IPCC climate
     scenario. A2 represents a very heterogeneous world with
     continuously increasing global population and regionally oriented
     economic growth that is more fragmented and slower than in other
     storylines."  )

(defscenario ipcc-hadley-b2 
  "This scenario represents the effects of the Hadley B2 IPCC climate
     scenario. B2 is a world in which the emphasis is on local
     solutions to economic, social, and environmental sustainability,
     with continuously increasing population (lower than A2) and
     intermediate economic development."  )

(defmodel constrained-development-scenario puget:ConstrainedDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCConstrained2060) 
    4                                   puget:HighDensityDevelopedConstrained
    6                                   puget:ModerateDensityDevelopedConstrained
    5                                   puget:LowDensityDevelopedConstrained
    7                                   puget:UrbanOpenSpaceConstrained
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedConstrained))

(defmodel open-development-scenario puget:OpenDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCOpen2060) 
    4                                   puget:HighDensityDevelopedOpen
    6                                   puget:ModerateDensityDevelopedOpen
    5                                   puget:LowDensityDevelopedOpen
    7                                   puget:UrbanOpenSpaceOpen
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedOpen))

(defscenario open-development-carbon
  "Changes values in developed areas to no succession, low canopy
cover, moderate hardwood-softwood ratio,low fire frequency, increased
greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen)))
                      (conc 'carbonService:VeryLowCanopyCover)

                      (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'carbonService:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model HardwoodSoftwoodRatio
    (classification HardwoodSoftwoodRatio
      :context [open-development-scenario :as od hardwood-softwood-ratio :as hsr]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'carbonService:ModerateHardness)
                  (:hsr %))))
  (model SuccessionalStage                              
    (classification SuccessionalStage
      :context [open-development-scenario :as od successional-stage :as ss]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'carbonService:NoSuccession)
                  (:ss %))))
  (model FireThreat
    (classification FireThreat
      :context [open-development-scenario :as od fire-threat :as ff]
    :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                      (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                      (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                (conc 'carbonService:LowFireThreat)
                (:ff %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
      :context [open-development-scenario :as od use-simple]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                (* 1.871 (or (:greenhouse-gas-emissions %) 0)) ; Reflects 87.1% population growth, assuming (crudely) same per capita emissions levels
                (:greenhouse-gas-emissions %)))))

(defscenario constrained-development-carbon
  "Changes values in developed areas to no succession, low canopy
cover, moderate hardwood-softwood ratio,low fire frequency, increased
greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained)))
                      (conc 'carbonService:VeryLowCanopyCover)
                    
                      (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'carbonService:ModerateCanopyCover)
                    
                      :otherwise (:pcc %))))
  (model HardwoodSoftwoodRatio
    (classification HardwoodSoftwoodRatio
      :context [constrained-development-scenario :as cd hardwood-softwood-ratio :as hsr]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'carbonService:ModerateHardness)
                  (:hsr %))))
  (model SuccessionalStage
    (classification SuccessionalStage
      :context [constrained-development-scenario :as cd successional-stage :as ss]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'carbonService:NoSuccession)
                  (:ss %))))
  (model FireThreat
    (classification FireThreat
      :context [constrained-development-scenario :as cd fire-threat :as ff]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'carbonService:LowFireThreat)
                  (:ff %))))
  (model GreenhouseGasEmissions
    (measurement GreenhouseGasEmissions "t/ha*year"
      :context [constrained-development-scenario :as cd use-simple]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                  (* 1.871 (or (:greenhouse-gas-emissions %) 0)) ; Reflects 87.1% population growth, assuming (crudely) same per capita emissions levels
                  (:greenhouse-gas-emissions %)))))