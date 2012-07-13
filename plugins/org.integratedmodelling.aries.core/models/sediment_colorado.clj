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
;;; Valid Contexts: core.contexts.colorado/co*
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

(namespace-ontology soilRetentionService
  (thinklab-core:BooleanRanking
   (LandOrWater
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; Used to mask out open water
(defmodel land-selector LandOrWater
  (classification  (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    ;;    #{12 21 22 23 24 31 41 42 43 52 71 81 82 90 95} OnLand
    ;;    Use this once NLCD is working again - much cleaner and simpler.
    #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125} OnLand))

(defmodel soil-group colorado:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 colorado:SoilGroupA
    2 colorado:SoilGroupB
    3 colorado:SoilGroupC
    4 colorado:SoilGroupD))

(defmodel slope colorado:SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] colorado:Level
    [ 1.15  4.57] colorado:GentlyUndulating
    [ 4.57 16.70] colorado:RollingToHilly
    [16.70    :>] colorado:SteeplyDissectedToMountainous))

;; This discretization is for SSURGO/STATSGO, paying attention to
;; texture over inclusion of various sized rock fragments.
(defmodel soil-texture colorado:SoilTextureClass
  (classification (numeric-coding habitat:SoilTexture)
    #{2 3 8 9 12 13 15 17 18 19 20 21 22 25 26 27 29 31 32 34 35 36 37 39 40 43 47 48 50 51 55 59 62 64 65 66 67 68 69 73 74 75 76 78 79 81 82 84 85 86 87 88 89 91 92 96 98 99 105 107 108 109 110 111 112 114 115 117 118 121 123 125 127 128 129 130 132 133 134 137 139 141 142 143 144 147 150 152 153 154 155 157 159 160 161 162 164 165 167 172 173 175 176 180 184 185 187 190 191 192 195} colorado:CoarseSoilTexture
    #{1 4 5 6 10 11 14 24 28 30 33 38 42 49 57 60 61 63 70 71 72 77 80 83 90 93 94 95 97 102 103 104 116 124 126 140 151 163 166 168 169 179 181 189} colorado:MediumSoilTexture
    #{7 16 23 41 44 45 46 52 53 54 56 58 100 101 106 113 119 120 122 131 135 136 138 145 146 148 149 156 170 171 174 177 182 183 186 188 193 194} colorado:FineSoilTexture))

;;Soil erodibility factor (k) from USLE/RUSLE (unitless).
(defmodel soil-erodibility colorado:SoilErodibilityClass
  (classification (numeric-coding habitat:SoilErodibility)
    [0.375 :>]    colorado:VeryHighSoilErodibility
    [0.3   0.375] colorado:HighSoilErodibility
    [0.225 0.3]   colorado:ModerateSoilErodibility
    [0.1   0.225] colorado:LowSoilErodibility
    [:<    0.1]   colorado:VeryLowSoilErodibility))

(defmodel precipitation-annual colorado:AnnualPrecipitationClass
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [1150   :>] colorado:VeryHighMeanAnnualPrecipitation
    [ 700 1150] colorado:HighMeanAnnualPrecipitation
    [ 300  700] colorado:ModerateMeanAnnualPrecipitation
    [ 200  300] colorado:LowMeanAnnualPrecipitation
    [:<    200] colorado:VeryLowMeanAnnualPrecipitation))

(defmodel vegetation-type colorado:SedimentVegetationType
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{19 22 23 24 26 28 29 30 32 33 34 35 36 38 39 40 41 42 43 44 46 48 50 53 56 58 62 63 64 67 68 69 70 71 72 73 74 75 76 77 78 79 81 82 85 86 92 95 99 103 104 106 108 109 }
    colorado:ForestShrublandGrasslandWetland
    #{118 119 120 121 122}
    colorado:InvasiveAnnualsAndPerennials
    #{1 2 4 5 7 8 9 10 11 12 13 14 15 17 21 111 112 113 114 115 116 117 123 124 125}
    colorado:CropsBarrenDeveloped))

;;  These are further distinctions in case we decide to group classes
;;together differently in the future
;;#{22 23 24 26 28 29 30 32 33 34 35 36 38 78 79 92 99 103}colorado:Forests
;;#{19 39 40 41 42 43 44 46 48 50 53 56 58 77 81 82 104 108 109}colorado:Shrubland
;;#{62 63 64 67 68 69 70 71 72 73 74 75 76 85 86 95 106}colorado:Grassland
;;#{ 121 122}colorado:InvasiveAnnuals
;;#{118 119 120}colorado:InvasivePerennials
;;#{1 2 4 5 7 8 9 10 11 12 13 14 15 17 21 111 112 113 115 116 117 123
;;#124 125}colorado:CropsBarrenDeveloped
(defmodel mountain-pine-beetle colorado:MountainPineBeetleDamageClass
  (classification (ranking colorado:MountainPineBeetleDamageSeverity)
     2         colorado:SevereDamage
     1         colorado:ModerateDamage
    -1         colorado:LowDamage
    :otherwise colorado:NoDamage))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-canopy-cover colorado:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [70 100 :inclusive] colorado:HighCanopyCover
    [30  70]            colorado:ModerateCanopyCover
    [ 0  30]            colorado:LowCanopyCover))

(defmodel successional-stage colorado:SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    5          colorado:OldGrowth
    4          colorado:LateSuccession
    3          colorado:MidSuccession
    2          colorado:PoleSuccession
    1          colorado:EarlySuccession
    :otherwise colorado:NoSuccession))

;;Sediment source value - we have evidence for this but can't yet
;; train so keep this commented out for now and use the
;; undiscretization statement below (?)
;;(defmodel sediment-source-value-annual AnnualSedimentSourceClass
;; (classification (measurement AnnualSedimentSourceClass "kg/ha")
;;    [100000 :>]          HighAnnualSedimentSource
;;    [30000 100000]       ModerateAnnualSedimentSource
;;    [:exclusive 0 30000] LowAnnualSedimentSource 
;;    0                    NoAnnualSedimentSource))

(defmodel sediment-source-value-annual colorado:AnnualSedimentSourceClass
  (probabilistic-measurement colorado:AnnualSedimentSourceClass "t/ha"
    [9   17]    colorado:HighAnnualSedimentSource
    [4.9  9]    colorado:ModerateAnnualSedimentSource
    [0.01 4.9]  colorado:LowAnnualSedimentSource 
    [0    0.01] colorado:NoAnnualSedimentSource))

;; The two Bayesian statements calculate source value with and without
;; fire, for comparison.
(defmodel source-fire colorado:AnnualSedimentSourceFire
  (bayesian colorado:AnnualSedimentSourceFire
    :import   "aries.core::SedimentSourceColoradoAdHoc.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage mountain-pine-beetle land-selector]
    :required [LandOrWater]
    :keep     [colorado:AnnualSedimentSourceClass]
    :result   sediment-source-value-annual))

(defmodel source-no-fire colorado:AnnualSedimentSourceNoFire ; Delete this
                                        ; and the other statements
                                        ; once contexts are working correctly.
  (bayesian colorado:AnnualSedimentSourceNoFire
    :import   "aries.core::SedimentSourceColoradoAdHocNoFire.xdsl"
    :context  [soil-group slope soil-texture precipitation-annual
               vegetation-type percent-canopy-cover
               successional-stage land-selector]
    :required [LandOrWater]
    :keep     [colorado:AnnualSedimentSourceClass]
    :result   sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western
;; U.S., CO/NM, and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; There are essentially no floodplain sinks in the steep mountain
;; regions in the Rockies.  Hence all sediment that's eroded moves
;; downslope and eventually ends up in reservoirs.

(defmodel reservoir-sediment-sink ReservoirSedimentSink
  (measurement ReservoirSedimentSink "t/ha"
    :context [(binary-coding geofeatures:Reservoir)]
    :state   #(if (== (:reservoir %) 1) 5000 0)))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel stream-gradient StreamGradient
  (measurement StreamGradient "\u00b0"
    :context [(binary-coding geofeatures:River)
              (measurement   geophysics:DegreeSlope "\u00b0")]
    :state   #(if (and (:river %) (pos? (:river %)))
                (:degree-slope %)
                0.0)))

(defmodel stream-gradient-class StreamGradientClass 
  (classification stream-gradient
    [2.86   :>] HighStreamGradient
    [1.15 2.86] ModerateStreamGradient
    [:<   1.15] LowStreamGradient))

(defmodel floodplain-tree-canopy-cover FloodplainTreeCanopyCover
  (ranking FloodplainTreeCanopyCover
    :context [(ranking     habitat:PercentTreeCanopyCover)
              (measurement habitat:FloodplainWidth "m")]
    :state   #(if (and (:floodplain-width %) (pos? (:floodplain-width %))
                       (:percent-tree-canopy-cover %))
                (:percent-tree-canopy-cover %)
                0)))

(defmodel floodplain-tree-canopy-cover-class FloodplainTreeCanopyCoverClass
  (classification floodplain-tree-canopy-cover
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
    :import  "aries.core::SedimentSinkColorado.xdsl"
    :context  [floodplain-tree-canopy-cover-class floodplain-width stream-gradient-class] 
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

(defmodel reservoirs geofeatures:Reservoir
  (binary-coding geofeatures:Reservoir))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel floodplains-code FloodplainsCode
  (binary-coding FloodplainsCode
    :context [(measurement habitat:FloodplainWidth "m")]
    :state #(if (and (:floodplain-width %) (pos? (:floodplain-width %))) 1 0)))

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel levees Levees
  (binary-coding Levees
    :context [(categorization infrastructure:Levee)]
    :state #(if (= (:levee %) "LEVEE") 0 1)))

(defmodel reservoir-deposition-data-fire ReservoirSoilDepositionFire
  (identification ReservoirSoilDepositionFire
    :context [source-fire sink-total reservoirs altitude streams levees]))

(defmodel reservoir-deposition-data-no-fire ReservoirSoilDepositionNoFire
  (identification ReservoirSoilDepositionNoFire
    :context [source-no-fire sink-total reservoirs altitude streams levees]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; For the time being there's no floodplain sedimentation for farmers supported in the Colorado models
;; (though they could be included if the models were extended to agricultural areas in the eastern, southern,
;; or western parts of the state).

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs-fire DetrimentalSedimentTransportFire
  (span SedimentTransport
        colorado:AnnualSedimentSourceFire
        geofeatures:Reservoir
        TotalSedimentSink
        nil
        (geophysics:Altitude geofeatures:River FloodplainsCode Levees)
        :source-threshold      0.0
        :sink-threshold        0.0
        :use-threshold         0.0
        :trans-threshold       0.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 9
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_colorado_data.clj")
        :context [source-fire reservoirs sink-total altitude streams floodplains-code levees]
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

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs-no-fire DetrimentalSedimentTransportNoFire
  (span SedimentTransport
        colorado:AnnualSedimentSourceNoFire
        geofeatures:Reservoir
        TotalSedimentSink
        nil
        (geophysics:Altitude geofeatures:River FloodplainsCode Levees)
        :source-threshold      0.0
        :sink-threshold        0.0
        :use-threshold         0.0
        :trans-threshold       0.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 4
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_colorado_data.clj")
        :context [source-no-fire reservoirs sink-total altitude streams floodplains-code levees]
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