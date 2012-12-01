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
;;; Valid Contexts: core.contexts.puget/{chehalis,wria9,viewshed,western-wa}*
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

(namespace-ontology soilRetentionService
  (thinklab-core:BooleanRanking
   (LandOrSea
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel soil-group puget:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 puget:SoilGroupA
    2 puget:SoilGroupB
    3 puget:SoilGroupC
    4 puget:SoilGroupD))

(defmodel slope puget:SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] puget:Level
    [ 1.15  4.57] puget:GentlyUndulating
    [ 4.57 16.70] puget:RollingToHilly
    [16.70    :>] puget:SteeplyDissectedToMountainous))

(defmodel slope-stability puget:SlopeStabilityClass
  (classification (numeric-coding habitat:SlopeStability)         
    1 puget:HighSlopeStability
    2 puget:ModerateSlopeStability
    3 puget:LowSlopeStability))

;; This discretization is for SSURGO/STATSGO, paying attention to
;; texture over inclusion of various sized rock fragments.
(defmodel soil-texture puget:SoilTextureClass
  (classification (numeric-coding habitat:SoilTexture)
    #{2 3 8 9 12 13 15 17 18 19 20 21 22 25 26 27 29 31 32 34 35 36 37 39 40 43 47 48 50 51 55 59 62 64 65 66 67 68 69 73 74 75 76 78 79 81 82 84 85 86 87 88 89 91 92 96 98 99 105 107 108 109 110 111 112 114 115 117 118 121 123 125 127 128 129 130 132 133 134 137 139 141 142 143 144 147 150 152 153 154 155 157 159 160 161 162 164 165 167 172 173 175 176 180 184 185 187 190 191 192 195} puget:CoarseSoilTexture
    #{1 4 5 6 10 11 14 24 28 30 33 38 42 49 57 60 61 63 70 71 72 77 80 83 90 93 94 95 97 102 103 104 116 124 126 140 151 163 166 168 169 179 181 189} puget:MediumSoilTexture
    #{7 16 23 41 44 45 46 52 53 54 56 58 100 101 106 113 119 120 122 131 135 136 138 145 146 148 149 156 170 171 174 177 182 183 186 188 193 194} puget:FineSoilTexture))

;;Soil erodibility factor (k) from USLE/RUSLE (unitless).
(defmodel soil-erodibility puget:SoilErodibilityClass
  (classification (numeric-coding habitat:SoilErodibility)
    [0.375 :>]    puget:VeryHighSoilErodibility
    [0.3   0.375] puget:HighSoilErodibility
    [0.225 0.3]   puget:ModerateSoilErodibility
    [0.1   0.225] puget:LowSoilErodibility
    [:<    0.1]   puget:VeryLowSoilErodibility))

(defmodel precipitation-annual puget:AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [2200   :>] puget:VeryHighMeanAnnualPrecipitation
    [1800 2200] puget:HighMeanAnnualPrecipitation
    [1200 1800] puget:ModerateMeanAnnualPrecipitation
    [ 600 1200] puget:LowMeanAnnualPrecipitation
    [:<    600] puget:VeryLowMeanAnnualPrecipitation))

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
(defmodel percent-canopy-cover puget:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [70 100 :inclusive] puget:HighCanopyCover
    [30  70]            puget:ModerateCanopyCover
    [ 0  30]            puget:LowCanopyCover))

(defmodel successional-stage puget:SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)  
    #{5 6}                           puget:OldGrowth
    4                                puget:LateSuccession
    3                                puget:MidSuccession
    2                                puget:PoleSuccession
    1                                puget:EarlySuccession
    #{20 21 22 23 24 25 26 27 28 40 41 101 102 103 104 105 106 107 108 109 120 121} puget:NoSuccession))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification  (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;;Sediment source value - we have evidence for this but can't yet
;; train so keep this commented out for now and use the
;; undiscretization statement below (?)
;;(defmodel sediment-source-value-annual AnnualSedimentSourceClass
;; (classification (measurement AnnualSedimentSourceClass "kg/ha")
;;    [100000 :>]          HighAnnualSedimentSource
;;    [30000 100000]       ModerateAnnualSedimentSource
;;    [:exclusive 0 30000] LowAnnualSedimentSource 
;;    0                    NoAnnualSedimentSource))

(defmodel sediment-source-value-annual puget:AnnualSedimentSourceClass
  (probabilistic-measurement puget:AnnualSedimentSourceClass "t/ha"
    [100   300]    puget:HighAnnualSedimentSource
    [ 30   100]    puget:ModerateAnnualSedimentSource
    [  0.01 30]    puget:LowAnnualSedimentSource 
    [  0     0.01] puget:NoAnnualSedimentSource))

;; source bayesian model for Puget Sound     
(defmodel source-puget puget:AnnualSedimentSource
  (bayesian puget:AnnualSedimentSource 
    :import   "aries.core::SedimentSourcePugetAdHoc.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage slope-stability land-selector]
    :required [LandOrSea]
    :keep     [puget:AnnualSedimentSourceClass]
    :result   sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western
;; U.S. and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel reservoir-sediment-sink ReservoirSedimentSink
  (measurement ReservoirSedimentSink "t/ha"
    :context [(binary-coding geofeatures:Reservoir)]
    :state   #(if (== (:reservoir %) 1) 5000 0)))

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
(defmodel sediment-sink-floodplain FloodplainSedimentSinkClass 
  (probabilistic-measurement FloodplainSedimentSinkClass "t/ha"
    [20    30]    HighFloodplainSedimentSink
    [10    20]    ModerateFloodplainSedimentSink
    [ 0.01 10]    LowFloodplainSedimentSink
    [ 0     0.01] NoFloodplainSedimentSink)) 

(defmodel floodplain-sediment-sink FloodplainSedimentSink
  (bayesian FloodplainSedimentSink    
    :import  "aries.core::SedimentSinkPuget.xdsl"
    :context  [stream-gradient floodplain-canopy-cover floodplain-width]
    :required [FloodplainWidthClass]
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
    :context [source-puget sink-total hydroelectric-use-level levees streams]))

(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
  (identification FarmlandSoilDeposition 
    :context [source-puget sink-total farmers-deposition-use-puget levees]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Sediment flow model for farmers in floodplains
(defmodel sediment-farmers BeneficialSedimentTransport ; or DetrimentalSedimentTransport
  (span SedimentTransport
        puget:AnnualSedimentSource
        DepositionProneFarmers
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River) 
        :source-threshold      2.0
        :sink-threshold        1.0
        :use-threshold         0.0
        :trans-threshold       1.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 4
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_farmers_puget_data.clj")
        :context [source-puget farmers-deposition-use-puget sink-total altitude levees streams floodplains-code]
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
        puget:AnnualSedimentSource
        geofeatures:Reservoir
        TotalSedimentSink
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River)
        :source-threshold      2.0
        :sink-threshold        1.0
        :use-threshold         0.0
        :trans-threshold       1.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_puget_data.clj")
        :context [source-puget reservoirs sink-total altitude levees streams floodplains-code]
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
        puget:AnnualSedimentSource
        WaterIntakeUse  ; Change the beneficiary group as needed.  This one is for drinking water intakes (though we currently lack information on their location)
        TotalSedimentSink 
        nil
        (geophysics:Altitude FloodplainsCode infrastructure:Levee geofeatures:River)
        :source-threshold      2.0
        :sink-threshold        1.0
        :use-threshold         0.0
        :trans-threshold       1.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival ; This will cause the model to store sediment values on users who are not co-located with sinks
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
      ;;:save-file          (str (System/getProperty "user.home") "/sediment_turbidity_data.clj")
        :context [source-puget sink-total altitude levees streams floodplains-code] ;change the beneficiary group as needed
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
 "Changes values in developed areas to low canopy cover, no forest successional stage, soil group D (lowest permeability), developed vegetation type, and farmland absent."
  "Changes values in developed areas to no succession, low canopy
cover, moderate hardwood-softwood ratio, low fire frequency, increased
greenhouse gas emissions."
  (model puget:PercentTreeCanopyCoverClass
    (classification puget:PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'puget:LowCanopyCover)

                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'puget:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model puget:SuccessionalStage
    (classification puget:SuccessionalStage
      :context [open-development-scenario :as od successional-stage :as ss]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'puget:NoSuccession)
                  (:ss %))))
  (model puget:HydrologicSoilsGroup
    (classification puget:HydrologicSoilsGroup
      :context [open-development-scenario :as od soil-group :as sg]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'puget:SoilGroupD)
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
  (model puget:PercentTreeCanopyCoverClass
    (classification puget:PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'puget:LowCanopyCover)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'puget:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model puget:SuccessionalStage
    (classification puget:SuccessionalStage
      :context [constrained-development-scenario :as cd successional-stage :as ss]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'puget:NoSuccession)
                  (:ss %))))
  (model puget:HydrologicSoilsGroup
    (classification puget:HydrologicSoilsGroup
      :context [constrained-development-scenario :as cd soil-group :as sg]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'puget:SoilGroupD)
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