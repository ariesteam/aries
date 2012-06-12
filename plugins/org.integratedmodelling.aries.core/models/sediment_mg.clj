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
;;; Sediment regulation model for Madagascar
;;;
;;; Valid Contexts: core.contexts.mg/mg*
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-mg
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking identification
                           bayesian count])
  (:refer aries :only [span]))

(namespace-ontology soilRetentionService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel soil-group HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 SoilGroupA
    2 SoilGroupB
    3 SoilGroupC
    4 SoilGroupD))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

(defmodel soil-texture SoilTextureClass
  (classification (categorization habitat:SoilTexture)
    "Coarse" CoarseSoilTexture
    "Medium" MediumSoilTexture
    "Fine"   FineSoilTexture))

;; Soil erodibility factor (k) from USLE/RUSLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
  (classification (numeric-coding habitat:SoilErodibility)
    [0.0375     :>] VeryHighSoilErodibility
    [0.0325 0.0375] HighSoilErodibility
    [0.0275 0.0325] ModerateSoilErodibility
    [0.02   0.0275] LowSoilErodibility
    [:<     0.02]   VeryLowSoilErodibility))

;; Annual precipitation for Mg & DR
(defmodel precipitation-annual AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [2200   :>] VeryHighMeanAnnualPrecipitation
    [1800 2200] HighMeanAnnualPrecipitation
    [1200 1800] ModerateMeanAnnualPrecipitation
    [ 600 1200] LowMeanAnnualPrecipitation
    [:<    600] VeryLowMeanAnnualPrecipitation))

;; Tropical storm probability, use only in DR & Mg
(defmodel storm-probability TropicalStormProbabilityClass
  (classification (numeric-coding habitat:TropicalStormProbability)
    [6 :>]     HighTropicalStormProbability   
    [1 6]      ModerateTropicalStormProbability     
    :otherwise NoTropicalStormProbability)) 

;; Annual runoff, whereas snowmelt, precipitation, and temperature are
;; monnthly, so this is problematic.  Could divide yearly runoff by 12
;; but obviously it's not evenly distributed throughout the year.  Or
;; could strongly consider just running it on an annual time step, as
;; that's what the data support.
(defmodel runoff AnnualRunoffClass
  (classification (measurement habitat:AnnualRunoff "mm")
    [2400   :>] VeryHighAnnualRunoff
    [1200 2400] HighAnnualRunoff
    [ 600 1200] ModerateAnnualRunoff
    [ 200  600] LowAnnualRunoff
    [   0  200] VeryLowAnnualRunoff))

;; Vegetation type
(defmodel vegetation-type mg:SedimentVegetationType
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{1 2 4 5 6 10 14}                         mg:ForestWetland
    #{3 7 23}                                  mg:DegradedForest
    #{8 9 20 21 22 24 25 26 28 29 30 31 32 33} mg:Savanna
    #{11 12 13 16 17}                          mg:CroplandDeveloped))

;; Discretization based on Quinton et al. (1997)
;; 254 & 255 in the global layer are equal to zero, hence the strange
;; discretization here.  Also 80 represents 80-100% tree canopy cover,
;; and there are no values from 1-9.
(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    #{70 71 72 73 74 75 76 77 78 79 80} HighCanopyCover
    #{30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69} ModerateCanopyCover
    #{0 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 254 255} LowCanopyCover))

(defmodel sediment-source-value-annual AnnualSedimentSourceClass
  (probabilistic-measurement AnnualSedimentSourceClass "t/ha"
    [40 100]  HighAnnualSedimentSource
    [15  40]  ModerateAnnualSedimentSource
    [0.01 15] LowAnnualSedimentSource 
    [0 0.01]  NoAnnualSedimentSource))

;; Source bayesian model for Madagascar      
(defmodel source-mg AnnualSedimentSource
  (bayesian  AnnualSedimentSource
    :import   "aries.core::SedimentSourceMgAdHoc.xdsl"
    :context  [soil-group slope soil-texture soil-erodibility precipitation-annual  
               storm-probability runoff vegetation-type percent-canopy-cover]
    :required [SlopeClass]
    :keep     [AnnualSedimentSourceClass]
    :result   sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel reservoir-sediment-sink ReservoirSedimentSink
  (measurement ReservoirSedimentSink "t/ha"
    :context [(binary-coding geofeatures:Reservoir)]
    :state   #(if (== (:reservoir %) 1) 5000 0)))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00b0")
    [2.86 :>]             HighStreamGradient
    [1.15 2.86]           ModerateStreamGradient
    [:exclusive 0   1.15] LowStreamGradient))

(defmodel floodplain-canopy-cover FloodplainTreeCanopyCoverClass 
  (classification (ranking habitat:PercentFloodplainTreeCanopyCover)
    [80 100 :inclusive] VeryHighFloodplainCanopyCover
    [60  80]            HighFloodplainCanopyCover
    [40  60]            ModerateFloodplainCanopyCover
    [20  40]            LowFloodplainCanopyCover
    [:exclusive 0 20]   VeryLowFloodplainCanopyCover))

(defmodel floodplains-code FloodplainsCode
  (binary-coding geofeatures:Floodplain))

;; Having problems generating this layer from Dartmouth Flood Observatory data
;;(defmodel floodplain-width FloodplainWidthClass 
;;  (classification (measurement habitat:FloodplainWidth "m")
;;    [1300   :>] VeryWideFloodplain
;;    [ 800 1300] WideFloodplain
;;    [ 350  800] NarrowFloodplain
;;    [   0  350] VeryNarrowFloodplain))

;; These are arbitrary numbers discretized based on the "low" soil
;; erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about
;; sedimentation.
(defmodel sediment-sink-floodplain FloodplainSedimentSinkClass 
  (probabilistic-measurement FloodplainSedimentSinkClass "t/ha"
    [10 15]  HighFloodplainSedimentSink
    [ 5 10]  ModerateFloodplainSedimentSink
    [0.01 5] LowFloodplainSedimentSink
    [0 0.01] NoFloodplainSedimentSink)) 

;; If we successfully get FPWidth data for Mg & DR, add these to the
;; "context" part of the model.
(defmodel floodplain-sediment-sink FloodplainSedimentSink
  (bayesian FloodplainSedimentSink 
    :import   "aries.core::SedimentSinkMg.xdsl"
    :context  [stream-gradient floodplain-canopy-cover]
    :required [StreamGradientClass]
    :keep     [FloodplainSedimentSinkClass]
    :result   sediment-sink-floodplain))

(defmodel sink-total TotalSedimentSink
  (measurement TotalSedimentSink "t/ha"
    :context [floodplain-sediment-sink reservoir-sediment-sink]
    :state   #(+ 
               (if (nil? (:floodplain-sediment-sink %)) 0.0 (.getMean (:floodplain-sediment-sink %)))
               (or       (:reservoir-sediment-sink %) 0.0))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel farmland Farmland
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{11 12 13} FarmlandPresent 
    :otherwise  FarmlandAbsent))

;; Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the
;; world) as a proxy for hyroelectric generation capacity (use) - in
;; reality dam height & flow are important factors but we don't have
;; flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level HydroelectricUseLevel
  (measurement HydroelectricUseLevel "m^3"))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-deposition-use-mg DepositionProneFarmers 
  (ranking DepositionProneFarmers
    :context [(ranking mglulc:MGLULCNumeric) (ranking geofeatures:Floodplain)]
    :state   #(if (and (= (:floodplain %) 1.0)
                       (contains? #{11.0 12.0 13.0} (:m-g-l-u-l-c-numeric %)))
                1
                0))) 

;; Models farmland in regions with erodible soils via basic spatial overlap.
;; FV FIXME I don't see any AnnualSedimentSource in the context?
;; Gary, is the context now correct to use the annual sediment source value properly?
(defmodel farmers-erosion-use-mg ErosionProneFarmers
  (ranking ErosionProneFarmers
    :context [(ranking mglulc:MGLULCNumeric) source-mg]
    :state   #(if (= (:m-g-l-u-l-c-numeric %) 11 12 13)
                (cond (= (:source-mg %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                      1
                      (= (:source-mg %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                      2
                      :otherwise
                      0)
                0)))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;defmodel fishermen-use-mg FishermenUse 
;;  (bayesian FishermenUse  
;;     :import   "aries.core::SedimentUseMgFishermen.xdsl"
;;     :keep     (FishermenUse)
;;     :context  (lakes rivers coastline coastal-wetlands mangroves reefs seagrass population-density)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))                                    

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))        

(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
  (identification FarmlandSoilDeposition 
    :context [source-mg sink-total farmers-deposition-use-mg
              altitude streams]))

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
  (identification ReservoirSoilDeposition 
    :context [source-mg sink-total hydroelectric-use-level
              streams altitude]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Sediment flow model for recipients of beneficial sedimentation.
;; We're assuming for now that sediment is detrimental to rice farmers
;; in Madagascar, so DON'T run this flow model for now, run the next
;; two below (farmers and hydro).  It should be able to be easily
;; implemented in the future, however.
(defmodel sediment-beneficial BeneficialSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        DepositionProneFarmers
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode geofeatures:River)
        :source-threshold   2.0 ; Note that threshold values are different in the Puget sediment SPAN models than in DR or Mg. This is because units are different, so keep these values (or similar ones)
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_mg_data_beneficial.clj")
        :context [source-mg farmers-deposition-use-mg sink-total altitude floodplains-code streams]
        :keep [TheoreticalSource
               TheoreticalSink
               TheoreticalUse
               PossibleFlow
               PossibleSource
               PossibleUse
               ActualFlow
               ActualSource
               ActualSink
               ActualUse
               InaccessibleSource
               InaccessibleUse
               BlockedFlow
               BlockedSource
               BlockedUse]))

;; Sediment flow model for recipients of avoided detrimental
;; sedimentation: farmers.  This is one of two beneficiary models
;; currently designed to be run.
(defmodel sediment-detrimental-farmers DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        DepositionProneFarmers
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode geofeatures:River)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 1
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_mg_data_detrimental_farmers.clj")
        :context [source-mg farmers-deposition-use-mg sink-total altitude floodplains-code streams]
        :keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSink
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))

;; Sediment flow model for recipients of avoided detrimental
;; sedimentation: hydro reservoirs.  This is one of two beneficiary
;; models currently designed to be run.
(defmodel sediment-detrimental-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        HydroelectricUseLevel
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_mg_data_detrimental_reservoirs.clj")
        :context [source-mg hydroelectric-use-level sink-total altitude floodplains-code streams]
        :keep [TheoreticalSource
               TheoreticalSink
               TheoreticalUse
               PossibleFlow
               PossibleSource
               PossibleUse
               ActualFlow
               ActualSource
               ActualSink
               ActualUse
               InaccessibleSink
               BlockedFlow
               BlockedSource
               BlockedUse]))

;; Sediment flow model for recipients of reduced turbidity.  This SPAN
;; statement is not currently set up to run as we lack data on
;; beneficiary groups for reduced turbidity.  It should be able to be
;; easily implemented in the future, however.
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        AnnualSedimentSource
        WaterIntakeUse  ; Change the beneficiary group as needed.  This one is for drinking water intakes (though we currently lack information on their location)
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_mg_data_turbidity.clj")
        :context [source-mg farmers-deposition-use-mg sink-total altitude floodplains-code streams] ; Change the beneficiary group as needed
        :keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSink
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))