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
;;; Sediment regulation model for Colorado
;;;
;;; Valid Contexts: core.contexts.beta/co*
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-colorado
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
    [1150   :>] VeryHighAnnualPrecipitation
    [ 700 1150] HighAnnualPrecipitation
    [ 300  700] ModerateAnnualPrecipitation
    [ 200  300] LowAnnualPrecipitation
    [:<    200] VeryLowAnnualPrecipitation))

(defmodel vegetation-type colorado:SedimentVegetationType
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{30 34} colorado:PonderosaPine
    29       colorado:LodgepolePine
    #{26 28} colorado:SpruceFir
    22       colorado:AspenWoodland))

(defmodel mountain-pine-beetle colorado:MountainPineBeetleDamageClass
  (classification (ranking colorado:MountainPineBeetleDamageSeverity)
     2         colorado:SevereDamage
     1         colorado:ModerateDamage
    -1         colorado:LowDamage
    :otherwise colorado:NoDamage))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [70 100 :inclusive] HighCanopyCover
    [30  70]            ModerateCanopyCover
    [ 0  30]            LowCanopyCover))

(defmodel successional-stage SuccessionalStageClass
  (classification (ranking ecology:SuccessionalStage)
    1          OldGrowth
    2          LateSuccession
    3          MidSuccession
    4          PoleSuccession
    5          EarlySuccession
    :otherwise NoSuccession))

;;Sediment source value - we have evidence for this but can't yet
;; train so keep this commented out for now and use the
;; undiscretization statement below (?)
;;(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
;; (classification (measurement SedimentSourceValueAnnualClass "kg/ha")
;;    [100000 :>]          HighAnnualSedimentSource
;;    [30000 100000]       ModerateAnnualSedimentSource
;;    [:exclusive 0 30000] LowAnnualSedimentSource 
;;    0                    NoAnnualSedimentSource))

(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
  (probabilistic-measurement SedimentSourceValueAnnualClass "kg/ha"
    [9000   17000]    HighAnnualSedimentSource
    [4900    9000]    ModerateAnnualSedimentSource
    [   0.01 4900]    LowAnnualSedimentSource 
    [   0       0.01] NoAnnualSedimentSource))

;; The two Bayesian statements calculate source value with and without
;; fire, for comparison.
(defmodel source-fire SedimentSourceValueAnnualFire
  (bayesian SedimentSourceValueAnnualFire
    :import   "aries.core::SedimentSourceColoradoAdHoc.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage mountain-pine-beetle]
    :required [SlopeClass]
    :keep     [SedimentSourceValueAnnualClass]
    :result   sediment-source-value-annual))

(defmodel source-no-fire SedimentSourceValueAnnualNoFire ; Delete this
                                        ; and the other statements
                                        ; once contexts are working correctly.
  (bayesian SedimentSourceValueAnnualNoFire
    :import   "aries.core::SedimentSourceColoradoAdHocNoFire.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage]
    :required [SlopeClass]
    :keep     [SedimentSourceValueAnnualClass]
    :result   sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western
;; U.S., CO/NM, and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; There are essentially no floodplain sinks in the steep mountain
;; regions in the Rockies.  Hence all sediment that's eroded moves
;; downslope and eventually ends up in reservoirs.

(defmodel sink ReservoirSedimentSink
  (measurement ReservoirSedimentSink "kg/ha"
    :context [(binary-coding geofeatures:Reservoir)]
    :state   #(if (== (:reservoir %) 1) 5000000 0)))
 
;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel reservoirs geofeatures:Reservoir
  (binary-coding geofeatures:Reservoir))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel floodplains-code FloodplainsCode
  (binary-coding geofeatures:Reservoir))

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))        

(defmodel reservoir-deposition-data-fire ReservoirSoilDepositionFire
  (identification ReservoirSoilDepositionFire
    :context [source-fire sink reservoirs altitude streams]))

(defmodel reservoir-deposition-data-no-fire ReservoirSoilDepositionNoFire
  (identification ReservoirSoilDepositionNoFire
    :context [source-no-fire sink reservoirs altitude streams]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; For the time being there's no floodplain sedimentation for farmers supported in the Colorado models
;; (though they could be included if the models were extended to agricultural areas in the eastern, southern,
;; or western parts of the state).

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs-fire DetrimentalSedimentTransportFire
  (span SedimentTransport
        SedimentSourceValueAnnualFire
        geofeatures:Reservoir
        ReservoirSedimentSink
        nil
        (geophysics:Altitude geofeatures:River FloodplainsCode)
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
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_colorado_data.clj")
        :context [source-fire reservoirs sink altitude streams floodplains-code]
        :keep    [MaximumSedimentSource
                  MaximumPotentialDeposition
                  PotentialReducedSedimentDepositionBeneficiaries
                  PossibleSedimentFlow
                  PossibleSedimentSource
                  PossibleReducedSedimentDepositionBeneficiaries
                  ActualSedimentFlow
                  ActualSedimentSource
                  UtilizedDeposition
                  ActualReducedSedimentDepositionBeneficiaries
                  UnutilizedDeposition
                  AbsorbedSedimentFlow
                  NegatedSedimentSource
                  BlockedHarmfulSediment]))

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs-no-fire DetrimentalSedimentTransportNoFire
  (span SedimentTransport
        SedimentSourceValueAnnualNoFire
        geofeatures:Reservoir
        ReservoirSedimentSink
        nil
        (geophysics:Altitude geofeatures:River FloodplainsCode)
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
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_colorado_data.clj")
        :context [source-no-fire reservoirs sink altitude streams floodplains-code]
        :keep    [MaximumSedimentSource
                  MaximumPotentialDeposition
                  PotentialReducedSedimentDepositionBeneficiaries
                  PossibleSedimentFlow
                  PossibleSedimentSource
                  PossibleReducedSedimentDepositionBeneficiaries
                  ActualSedimentFlow
                  ActualSedimentSource
                  UtilizedDeposition
                  ActualReducedSedimentDepositionBeneficiaries
                  UnutilizedDeposition
                  AbsorbedSedimentFlow
                  NegatedSedimentSource
                  BlockedHarmfulSediment]))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------