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
;;; Sediment regulation model for Madagascar
;;;
;;; Valid Contexts: core.contexts.beta/mg*
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-mg
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization
                            namespace-ontology ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification probabilistic-ranking
                            identification bayesian count))
  (:refer aries :only (span)))

(namespace-ontology soilRetentionService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel soil-group HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1       SoilGroupA
    2       SoilGroupB
    3       SoilGroupC
    4       SoilGroupD))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [0 1.15]     Level
    [1.15 4.57]    GentlyUndulating
    [4.57 16.70] RollingToHilly
    [16.70 :>]     SteeplyDissectedToMountainous))

(defmodel soil-texture SoilTextureClass
  (classification (categorization habitat:SoilTexture)
    "Coarse"    CoarseSoilTexture
    "Medium"    MediumSoilTexture
    "Fine"      FineSoilTexture))

;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
  (classification (numeric-coding habitat:SoilErodibility)
    [:< 0.02]         VeryLowSoilErodibility
    [0.02 0.0275]     LowSoilErodibility
    [0.0275 0.0325]   ModerateSoilErodibility
    [0.0325 0.0375]   HighSoilErodibility
    [0.0375 :>]       VeryHighSoilErodibility))

;;Annual precipitation for Mg & DR
(defmodel precipitation-annual AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [:< 600]        VeryLowAnnualPrecipitation
    [600 1200]    LowAnnualPrecipitation
    [1200 1800]   ModerateAnnualPrecipitation
    [1800 2200]     HighAnnualPrecipitation
    [2200 :>]     VeryHighAnnualPrecipitation))

;;Tropical storm probability, use only in DR & Mg
(defmodel storm-probability TropicalStormProbabilityClass
  (classification (numeric-coding habitat:TropicalStormProbability)
    [6 :>]           HighTropicalStormProbability   
    [1 6]            ModerateTropicalStormProbability     
    :otherwise       NoTropicalStormProbability)) 

;;Annual runoff, whereas snowmelt, precipitation, and temperature are monnthly, so this is problematic.
;;Could divide yearly runoff by 12 but obviously it's not evenly distributed throughout the year.
;;Or could strongly consider just running it on an annual time step, as that's what the data support.
(defmodel runoff AnnualRunoffClass
  (classification (measurement habitat:AnnualRunoff "mm")
    [0 200]         VeryLowAnnualRunoff
    [200 600]     LowAnnualRunoff
    [600 1200]      ModerateAnnualRunoff
    [1200 2400]     HighAnnualRunoff
    [2400 :>]     VeryHighAnnualRunoff))

;;Vegetation type
(defmodel vegetation-type VegetationTypeSedimentMg
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{1 2 4 5 6 10 14}                         ForestWetland
    #{3 7 23}                                  DegradedForest
    #{8 9 20 21 22 24 25 26 28 29 30 31 32 33} Savanna
    #{11 12 13 16 17}                          CroplandDeveloped))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover PercentVegetationCoverClass
  (classification (ranking habitat:PercentVegetationCover)
    [70 100 :inclusive]           HighVegetationCover
    [30 70]                       ModerateVegetationCover
    [:exclusive 0 30]             LowVegetationCover))

(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
  (probabilistic-measurement SedimentSourceValueAnnualClass "t/ha"
    [0 0.01]              NoAnnualSedimentSource
    [0.01 15]             LowAnnualSedimentSource 
    [15 40]               ModerateAnnualSedimentSource
    [40 100]              HighAnnualSedimentSource))

;; source bayesian model for Madagascar      
(defmodel source-mg SedimentSourceValueAnnual
  (bayesian SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceMgAdHoc.xdsl"
    :keep     (SedimentSourceValueAnnualClass)
    :required (SlopeClass)
    :result   sediment-source-value-annual
    :context  (soil-group slope soil-texture soil-erodibility precipitation-annual  
                          storm-probability runoff vegetation-type percent-vegetation-cover))) 

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel reservoirs ReservoirsClass 
  (classification (binary-coding geofeatures:Reservoir)
    1          ReservoirPresent
    :otherwise ReservoirAbsent))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00b0")
    [:exclusive 0   1.15]  LowStreamGradient
    [1.15 2.86]            ModerateStreamGradient
    [2.86 :>]              HighStreamGradient))

(defmodel floodplain-vegetation-cover FloodplainVegetationCoverClass 
  (classification (ranking habitat:PercentFloodplainVegetationCover)
    [:exclusive 0 20]              VeryLowFloodplainVegetationCover
    [20 40]                        LowFloodplainVegetationCover
    [40 60]                        ModerateFloodplainVegetationCover
    [60 80]                        HighFloodplainVegetationCover
    [80 100 :inclusive]            VeryHighFloodplainVegetationCover))

(defmodel floodplains Floodplains
  (classification (binary-coding geofeatures:Floodplain)
    1          InFloodplain
    :otherwise NotInFloodplain))

;;Having problems generating this layer from Dartmouth Flood Observatory data
;;(defmodel floodplain-width FloodplainWidthClass 
;;  (classification (measurement habitat:FloodplainWidth "m")
;;    [0 350]     VeryNarrowFloodplain
;;    [350 800]   NarrowFloodplain
;;    [800 1300]  WideFloodplain
;;    [1300 :>]   VeryWideFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual AnnualSedimentSinkClass 
  (probabilistic-measurement AnnualSedimentSinkClass "t/ha"
    [10 15]              HighAnnualSedimentSink
    [5 10]               ModerateAnnualSedimentSink
    [0.01 5]             LowAnnualSedimentSink
    [0 0.01]             NoAnnualSedimentSink)) 

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-mg AnnualSedimentSink
  (bayesian AnnualSedimentSink 
    :import  "aries.core::SedimentSinkMg.xdsl"
    :keep    (AnnualSedimentSinkClass)
    :required (StreamGradientClass)
    :result   sediment-sink-annual 
    :context  (reservoirs stream-gradient floodplain-vegetation-cover)))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel farmland Farmland
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{11 12 13} FarmlandPresent 
    :otherwise FarmlandAbsent))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level HydroelectricUseLevel
  (measurement HydroelectricUseLevel "m^3"))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-deposition-use-mg DepositionProneFarmers 
  (ranking DepositionProneFarmers
    :state #(if (and (= (:floodplains %) 1.0)
                     (contains? #{11.0 12.0 13.0} (:farmlandpresent %)))
              1
              0)
    :context (
              (ranking mglulc:MGLULCNumeric :as farmlandpresent)
              (ranking geofeatures:Floodplain :as floodplains)))) 

;; Models farmland in regions with erodible soils via basic spatial overlap.
;; FV FIXME I don't see any SedimentSourceValueAnnual in the context?
(defmodel farmers-erosion-use-mg ErosionProneFarmers
  (ranking ErosionProneFarmers
    :state #(if (= (:farmlandpresent %) 11 12 13)
              (cond (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                    1
                    (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                    2
                    :otherwise
                    0)
              0)
    :context ((ranking mglulc:MGLULCNumeric :as farmlandpresent))))

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
    :context (
              source-mg
              sediment-sink-mg
              farmers-deposition-use-mg
              altitude
              streams)))

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
  (identification ReservoirSoilDeposition 
    :context (
              source-mg
              sediment-sink-mg
              hydroelectric-use-level
              streams
              altitude)))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;Sediment flow model for recipients of beneficial sedimentation.  We're assuming for now that sediment is detrimental
;; to rice farmers in Madagascar, so DON'T run this flow model for now, run the next two below (farmers and hydro).
;; It should be able to be easily implemented in the future, however.
(defmodel sediment-beneficial BeneficialSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnualClass 
        DepositionProneFarmers
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
        :source-threshold   2.0 ;;Note that threshold values are different in the Puget sediment SPAN models than in DR or Mg. This is because units are different, so keep these values (or similar ones)
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
        :keep (MaximumSedimentSource                       MaximumPotentialDeposition 
                                                           PotentialSedimentDepositionBeneficiaries    PossibleSedimentFlow
                                                           PossibleSedimentSource                      PossibleSedimentDepositionBeneficiaries
                                                           ActualSedimentFlow                          ActualSedimentSource
                                                           UtilizedDeposition                          ActualSedimentDepositionBeneficiaries
                                                           UnutilizedSedimentSource                    InaccessibleSedimentDepositionBeneficiaries
                                                           AbsorbedSedimentFlow                        NegatedSedimentSource
                                                           LostValuableSediment)
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams)))

;;Sediment flow model for recipients of avoided detrimental sedimentation: farmers.  This is one of two beneficiary models
;; currently designed to be run.
(defmodel sediment-detrimental-farmers DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnualClass 
        DepositionProneFarmers
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
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
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_mg_data_detrimental_farmers.clj")
        :keep (MaximumSedimentSource                                MaximumPotentialDeposition 
                                                                    PotentialReducedSedimentDepositionBeneficiaries      PossibleSedimentFlow
                                                                    PossibleSedimentSource                               PossibleReducedSedimentDepositionBeneficiaries
                                                                    ActualSedimentFlow                                   ActualSedimentSource
                                                                    UtilizedDeposition                                   ActualReducedSedimentDepositionBeneficiaries
                                                                    UnutilizedDeposition                                 AbsorbedSedimentFlow
                                                                    NegatedSedimentSource                                BlockedHarmfulSediment)
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams)))

;;Sediment flow model for recipients of avoided detrimental sedimentation: hydro reservoirs.  This is one of two beneficiary 
;; models currently designed to be run.
(defmodel sediment-detrimental-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnualClass 
        HydroelectricUseLevel
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
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
        :keep (MaximumSedimentSource                                MaximumPotentialDeposition 
                                                                    PotentialReducedSedimentDepositionBeneficiaries      PossibleSedimentFlow
                                                                    PossibleSedimentSource                               PossibleReducedSedimentDepositionBeneficiaries
                                                                    ActualSedimentFlow                                   ActualSedimentSource
                                                                    UtilizedDeposition                                   ActualReducedSedimentDepositionBeneficiaries
                                                                    UnutilizedDeposition                                 AbsorbedSedimentFlow
                                                                    NegatedSedimentSource                                BlockedHarmfulSediment)
        :context (source-mg hydroelectric-use-level sediment-sink-mg altitude floodplains streams)))

;;Sediment flow model for recipients of reduced turbidity.  This SPAN statement is not currently set up to run as we
;; lack data on beneficiary groups for reduced turbidity.  It should be able to be easily implemented in the future, however.
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        SedimentSourceValueAnnualClass 
        WaterIntakeUse  ;;change the beneficiary group as needed.  This one is for drinking water intakes (though we currently lack information on their location)
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
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
        :keep (MaximumSedimentSource                        MaximumPotentialDeposition 
                                                            PotentialReducedTurbidityBeneficiaries       PossibleSedimentFlow
                                                            PossibleSedimentSource                       PossibleReducedTurbidityBeneficiaries
                                                            ActualSedimentFlow                           ActualSedimentSource
                                                            UtilizedDeposition                           ActualReducedTurbidityBeneficiaries
                                                            UnutilizedDeposition                         AbsorbedSedimentFlow 
                                                            NegatedSedimentSource                        ReducedTurbidity)
        :context (source-mg farmers-deposition-use-mg sediment-sink-mg altitude floodplains streams))) ;;change the beneficiary group as needed