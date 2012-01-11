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
;;; Sediment regulation model for Western Washington
;;;
;;; Valid Contexts: core.contexts.beta/{chehalis,wria9,viewshed,western_wa}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-puget
  (:refer-clojure :rename {count length}) 
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding model
                           probabilistic-measurement
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

(defmodel slope-stability SlopeStabilityClass
  (classification (numeric-coding habitat:SlopeStability)         
    1 HighSlopeStability
    2 ModerateSlopeStability
    3 LowSlopeStability))

;; This discretization is for SSURGO/STATSGO, paying attention to
;; texture over inclusion of various sized rock fragments.
(defmodel soil-texture SoilTextureClass
  (classification (numeric-coding habitat:SoilTexture)
    #{2 3 8 9 12 13 15 17 18 19 20 21 22 25 26 27 29 31 32 34 35 36 37 39 40 43 47 48 50 51 55 59 62 64 65 66 67 68 69 73 74 75 76 78 79 81 82 84 85 86 87 88 89 91 92 96 98 99 105 107 108 109 110 111 112 114 115 117 118 121 123 125 127 128 129 130 132 133 134 137 139 141 142 143 144 147 150 152 153 154 155 157 159 160 161 162 164 165 167 172 173 175 176 180 184 185 187 190 191 192 195} CoarseSoilTexture
    #{1 4 5 6 10 11 14 24 28 30 33 38 42 49 57 60 61 63 70 71 72 77 80 83 90 93 94 95 97 102 103 104 116 124 126 140 151 163 166 168 169 179 181 189} MediumSoilTexture
    #{7 16 23 41 44 45 46 52 53 54 56 58 100 101 106 113 119 120 122 131 135 136 138 145 146 148 149 156 170 171 174 177 182 183 186 188 193 194} FineSoilTexture))

;;Soil erodibility factor (k) from USLE/RUSLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
  (classification (numeric-coding habitat:SoilErodibility)
    [0.375 :>]    VeryHighSoilErodibility
    [0.3   0.375] HighSoilErodibility
    [0.225 0.3]   ModerateSoilErodibility
    [0.1   0.225] LowSoilErodibility
    [:<    0.1]   VeryLowSoilErodibility))

(defmodel precipitation-annual AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [2200   :>] VeryHighAnnualPrecipitation
    [1800 2200] HighAnnualPrecipitation
    [1200 1800] ModerateAnnualPrecipitation
    [ 600 1200] LowAnnualPrecipitation
    [:<    600] VeryLowAnnualPrecipitation))

(defmodel runoff AnnualRunoffClass
  (classification (measurement habitat:AnnualRunoff "mm")
    [2400   :>] VeryHighAnnualRunoff
    [1200 2400] HighAnnualRunoff
    [ 600 1200] ModerateAnnualRunoff
    [ 200  600] LowAnnualRunoff
    [   0  200] VeryLowAnnualRunoff))

;;CANT do a global vegetation type defmodel if classes are different:
;; split this up & use the local vegetation type defmodel into the BN
;;Vegetation type
(defmodel vegetation-type puget:SedimentVegetationType
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{41 42 43 71 90 95} puget:ForestGrasslandWetland
    #{52 81}             puget:ShrublandPasture
    #{21 22 23 24 31 82} puget:CropsBarrenDeveloped))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [70 100 :inclusive] HighCanopyCover
    [30  70]            ModerateCanopyCover
    [ 0  30]            LowCanopyCover))

(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)  
    #{5 6}                           OldGrowth
    4                                LateSuccession
    3                                MidSuccession
    2                                PoleSuccession
    1                                EarlySuccession
    #{21 22 23 24 25 26 27 28 40 41} NoSuccession))

;;Sediment source value - we have evidence for this but can't yet
;; train so keep this commented out for now and use the
;; undiscretization statement below (?)
;;(defmodel sediment-source-value-annual AnnualSedimentSourceClass
;; (classification (measurement AnnualSedimentSourceClass "kg/ha")
;;    [100000 :>]          HighAnnualSedimentSource
;;    [30000 100000]       ModerateAnnualSedimentSource
;;    [:exclusive 0 30000] LowAnnualSedimentSource 
;;    0                    NoAnnualSedimentSource))

(defmodel sediment-source-value-annual AnnualSedimentSourceClass
  (probabilistic-measurement AnnualSedimentSourceClass "kg/ha"
    [100000 300000] HighAnnualSedimentSource
    [ 30000 100000] ModerateAnnualSedimentSource
    [   0.01 30000] LowAnnualSedimentSource 
    [     0   0.01] NoAnnualSedimentSource))

;; source bayesian model for Puget Sound     
(defmodel source-puget AnnualSedimentSource
  (bayesian AnnualSedimentSource 
    :import   "aries.core::SedimentSourcePugetAdHoc.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage slope-stability]
    :required [SlopeClass]
    :keep     [AnnualSedimentSourceClass]
    :result   sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western
;; U.S. and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel reservoirs-class ReservoirsClass 
  (classification (binary-coding geofeatures:Reservoir)
    1          ReservoirPresent
    :otherwise ReservoirAbsent))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00b0")
    [2.86   :>] HighStreamGradient
    [1.15 2.86] ModerateStreamGradient
    [:<   1.15] LowStreamGradient))

(defmodel floodplain-canopy-cover FloodplainTreeCanopyCoverClass 
  (classification (ranking habitat:PercentFloodplainTreeCanopyCover)
    [80 100 :inclusive] VeryHighFloodplainCanopyCover
    [60  80]            HighFloodplainCanopyCover
    [40  60]            ModerateFloodplainCanopyCover
    [20  40]            LowFloodplainCanopyCover
    [ 0  20]            VeryLowFloodplainCanopyCover))

(defmodel floodplain-width FloodplainWidthClass 
  (classification (measurement habitat:FloodplainWidth "m")
    [1300   :>] VeryWideFloodplain
    [ 800 1300] WideFloodplain
    [ 350  800] NarrowFloodplain
    [   0  350] VeryNarrowFloodplain))

;; These are arbitrary numbers discretized based on the "low" soil
;; erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about
;; sedimentation.
(defmodel sediment-sink-annual AnnualSedimentSinkClass 
  (probabilistic-measurement AnnualSedimentSinkClass "kg/ha"
    [20000 30000] HighAnnualSedimentSink
    [10000 20000] ModerateAnnualSedimentSink
    [0.01  10000] LowAnnualSedimentSink
    [    0  0.01] NoAnnualSedimentSink)) 

(defmodel sediment-sink-us AnnualSedimentSink
  (bayesian AnnualSedimentSink    
    :import  "aries.core::SedimentSinkPuget.xdsl"
    :context  [reservoirs-class stream-gradient floodplain-canopy-cover floodplain-width]
    :required [FloodplainWidthClass]
    :keep     [AnnualSedimentSinkClass]
    :result   sediment-sink-annual))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel reservoirs geofeatures:Reservoir
  (binary-coding geofeatures:Reservoir))

(defmodel floodplains Floodplains
  (classification (categorization geofeatures:Floodplain)
    #{"A" "X500"} InFloodplain
    :otherwise    NotInFloodplain))

(defmodel floodplains-code FloodplainsCode
  (binary-coding FloodplainsCode
    :context [(categorization geofeatures:Floodplain)]
    :state   #(if (contains? #{"A" "X500"} (:floodplain %)) 1 0)))

(defmodel farmland Farmland
  "Just a reclass of the regionally appropriate LULC layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    82         FarmlandPresent
    :otherwise FarmlandAbsent))

;; Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the
;; world) as a proxy for hyroelectric generation capacity (use) - in
;; reality dam height & flow are important factors but we don't have
;; flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level HydroelectricUseLevel
  (measurement HydroelectricUseLevel "m^3" :as hydro-use-level))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-deposition-use-puget DepositionProneFarmers 
  (binary-coding DepositionProneFarmers
    :context [floodplains farmland]
    :state   #(if (and (= (tl/conc 'soilRetentionService:InFloodplain)    (:floodplains %))
                       (= (tl/conc 'soilRetentionService:FarmlandPresent) (:farmland    %)))
                1
                0)))

;; Models farmland in regions with erodible soils, via basic spatial overlap.
;; FV FIXME I don't see any AnnualSedimentSource in the context?
;; Gary, is the context now correct to use the annual sediment source value properly?
(defmodel farmers-erosion-use-puget ErosionProneFarmers
  (ranking ErosionProneFarmers
    :context [(ranking nlcd:NLCDNumeric :as farmlandpresent) source-puget]
    :state   #(if (= (:farmlandpresent %) 82.0)
                (cond (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                      1
                      (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                      2
                      :otherwise
                      0)
                0)))

;; Still need defmodels for all components of fisheries BNs.  What
;; about deterministic nodes?  Need an undiscretization defmodel before
;; this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.

;;(defmodel fishermen-use-puget FishermenUse 
;;(bayesian FishermenUse  
;;     :import  "aries.core::SedimentUsePugetFishermen.xdsl"
;;     :context [lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density]
;;     :keep    [FishermenUse]))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))                                    

(defmodel levees infrastructure:Levee
  (binary-coding infrastructure:Levee))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))        

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
  (identification ReservoirSoilDeposition 
    :context [source-puget sediment-sink-us hydroelectric-use-level levees]))

(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
  (identification FarmlandSoilDeposition 
    :context [source-puget sediment-sink-us farmers-deposition-use-puget levees]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Sediment flow model for farmers in floodplains
(defmodel sediment-farmers BeneficialSedimentTransport ; or DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        DepositionProneFarmers
        AnnualSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River) 
        :source-threshold   1000.0 ; Note that threshold values are different in the Puget sediment SPAN models than in DR or Mg. This is because units are different, so keep these values (or similar ones)
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 3
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_farmers_puget_data.clj")
        :context [source-puget farmers-deposition-use-puget sediment-sink-us altitude levees streams floodplains-code]
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
                  InaccessibleSource
                  InaccessibleUse
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
        geofeatures:Reservoir
        AnnualSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 3
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_puget_data.clj")
        :context [source-puget reservoirs sediment-sink-us altitude levees streams floodplains-code]
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

;; FIXME: There is no WaterIntakeUse observation defined above.
;; Sediment flow model for assessing turbidity
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        AnnualSedimentSource
        WaterIntakeUse  ; Change the beneficiary group as needed.  This one is for drinking water intakes (though we currently lack information on their location)
        AnnualSedimentSink 
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival ; This will cause the model to store sediment values on users who are not co-located with sinks
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
      ;;:save-file          (str (System/getProperty "user.home") "/sediment_turbidity_data.clj")
        :context [source-puget sediment-sink-us altitude levees streams floodplains-code] ;change the beneficiary group as needed
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

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

(defscenario open-development-sediment
  "Changes values in developed areas to no succession, low canopy
cover, moderate hardwood-softwood ratio, low fire frequency, increased
greenhouse gas emissions."
  (model PercentTreeCanopyCover
    (classification PercentTreeCanopyCover
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'carbonService:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model SuccessionalStage
    (classification SuccessionalStage
      :context [open-development-scenario :as od successional-stage :as ss]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'soilRetentionService:NoSuccession)
                  (:ss %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
      :context [open-development-scenario :as od soil-group :as sg]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'soilRetentionService:SoilGroupD)
                  (:sg %))))
  (model puget:SedimentVegetationType
    (classification puget:SedimentVegetationType
      :context [open-development-scenario :as od vegetation-type :as vt]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'puget:CropsBarrenDeveloped)
                      
                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'puget:ForestGrasslandWetland)

                      :otherwise (:vt %))))
  (model Farmland
    (classification Farmland
      :context [open-development-scenario :as od farmland :as f]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'soilRetentionService:FarmlandAbsent) 
                  (:f %)))))

(defscenario constrained-development-sediment
  "Changes values in developed areas to no succession, low canopy cover, moderate hardwood-softwood ratio,low fire frequency, increased greenhouse gas emissions."
  (model PercentTreeCanopyCover
    (classification PercentTreeCanopyCover
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'carbonService:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model SuccessionalStage
    (classification SuccessionalStage
      :context [constrained-development-scenario :as cd successional-stage :as ss]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'soilRetentionService:NoSuccession)
                  (:ss %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
      :context [constrained-development-scenario :as cd soil-group :as sg]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'soilRetentionService:SoilGroupD)
                  (:sg %))))
  (model puget:SedimentVegetationType
    (classification puget:SedimentVegetationType
      :context [constrained-development-scenario :as cd vegetation-type :as vt]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'puget:CropsBarrenDeveloped)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'puget:ForestGrasslandWetland)
                    
                      :otherwise (:vt %))))
  (model Farmland
    (classification Farmland
      :context [constrained-development-scenario :as cd farmland :as f]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'soilRetentionService:FarmlandAbsent) 
                  (:f %)))))