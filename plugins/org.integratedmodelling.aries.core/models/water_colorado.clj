;;; Copyright 2013 The ARIES Consortium (http://www.ariesonline.org)
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
;;; Water supply model for Colorado
;;;
;;; Valid Contexts: core.contexts.colorado/co*
;;;
;;;-------------------------------------------------------------------

(ns core.models.water-colorado
  (:refer-clojure :rename {count length}) 
  (:refer tl        :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization ranking
                           numeric-coding binary-coding
                           namespace-ontology model identification
                           bayesian no-data? count
                           probabilistic-measurement])
  (:refer aries :only [span]))

(namespace-ontology waterSupplyService
   (representation:GenericObservable
   (TempSurfaceWaterData SurfaceDiversionCapacityCode)))

;;;-------------------------------------------------------------------
;;; Surface water source models
;;;-------------------------------------------------------------------

;; Surface water source = precipitaiton + snowmelt + springs + baseflow
;; + incoming interbasin water transfers

;; While Colorado does have some "glaciers" that keep snow cover year-round,
;; their spatial extent is very small when compared to more northern and/or
;; high altitude regions with extensive glaciers.  Snowmelt could become
;; important in a seasonal water supply model but for the time being is being
;; left out of the annual time step source model.

;; Springs not included as a surface water source as their contribution to
;; the water budget of large watersheds is exceedingly small.

;; Baseflows are not currently modeled as doing so requires MODFLOW outputs to
;; identify contributions to gaining reaches.

;; Incomring interbasin water transfers are a major water input to Denver
;; and other Front Range communities.

(defmodel incoming-water-transfer IncomingWaterTransfer
  (measurement IncomingWaterTransfer "mm"))

(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; Runoff as the sum of annual precipitation and incoming water transfers.
(defmodel runoff AnnualRunoffSummed
  (measurement AnnualRunoffSummed "mm"
    :context [precipitation-annual incoming-water-transfer]
    :state #(+ (:annual-precipitation    %)
               (or (:incoming-water-transfer %) 0.0))))

;;;-------------------------------------------------------------------
;;; Groundwater source models
;;;-------------------------------------------------------------------

;; Consider using percolation data here instead if more appropriate?
;; This is great but not using it in the models right now as groundwater
;; flows aren't yet supported.
(defmodel recharge habitat:AnnualRecharge
  (measurement habitat:AnnualRecharge "mm"))

;;;-------------------------------------------------------------------
;;; Surface water sink models
;;;-------------------------------------------------------------------

;; Ad hoc sink model adapted from the ad hoc flood sink model.
;; Includes infiltration & evapotranspiration processes.  Deterministic
;; models could be used to replace this as appropriate.

(defmodel vegetation-type colorado:WaterSupplyVegetationType
  "Reclass of SWReGAP LULC layer"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{22 24 26 28 29 30 32 33 34 35 36 38 63 64 78 79 81 92 95 103 118}                       colorado:Forest
    #{19 39 40 41 42 43 44 46 48 50 53 56 58 62 67 77 82 104 108 109} colorado:ScrubBrush
    #{68 69 70 71 72 73 74 75 76 85 86 99 106 119 120 121 122}                                   colorado:ShortgrassPrairie
    110                                                                                                colorado:Water
    #{1 2 4 5 7 8 9 10 11 12 13 14 15 17 21 111 112 113 115 116 117 123 124 125}                                                                             colorado:UrbanBarren
    114                                                                                                colorado:Agriculture))

(defmodel percent-canopy-cover colorado:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive] colorado:VeryHighCanopyCover
    [60 80]             colorado:HighCanopyCover
    [30 60]             colorado:ModerateCanopyCover
    [ 5 30]             colorado:LowCanopyCover
    [ 0  5]             colorado:VeryLowCanopyCover))

(defmodel annual-temperature colorado:AnnualMaximumTemperature
  (classification (measurement geophysics:AnnualMaximumGroundSurfaceTemperature "\u00b0C")
    [15 :>]   colorado:HighAnnualMaximumTemperature
    [10 15]   colorado:ModerateAnnualMaximumTemperature
    [:< 10]   colorado:LowAnnualMaximumTemperature)) 

(defmodel slope colorado:SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] colorado:Level
    [ 1.15  4.57] colorado:GentlyUndulating
    [ 4.57 16.70] colorado:RollingToHilly
    [16.70    :>] colorado:SteeplyDissectedToMountainous))

(defmodel soil-group colorado:HydrologicSoilsGroup
  (classification (ranking habitat:HydrologicSoilsGroup)
    1      colorado:SoilGroupA ;1, then 2, 3, 4 when using reclassed STATSGO/local SSURGO.
    #{2 5} colorado:SoilGroupB
    #{3 6} colorado:SoilGroupC
    #{4 7} colorado:SoilGroupD))

(defmodel imperviousness colorado:PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousSurface)
    [80 100 :inclusive] colorado:VeryHighImperviousCover
    [50  80]            colorado:HighImperviousCover
    [20  50]            colorado:ModeratelyHighImperviousCover
    [10  20]            colorado:ModeratelyLowImperviousCover
    [ 5  10]            colorado:LowImperviousCover
    [ 0   5]            colorado:VeryLowImperviousCover))

;; Global dataset values are in the range of 18-39 mm for Colorado.
; (defmodel evapotranspiration colorado:EvapotranspirationClass
;   (probabilistic-measurement colorado:EvapotranspirationClass "mm"
;     [30 40] colorado:HighEvapotranspiration
;     [24 30] colorado:ModerateEvapotranspiration
;     [18 24] colorado:LowEvapotranspiration))

;; Brown et al. 2008 values are in the range of 137-768 mm for Colorado.
(defmodel evapotranspiration colorado:EvapotranspirationClass
  (probabilistic-measurement colorado:EvapotranspirationClass "mm"
    [400 768] colorado:HighEvapotranspiration
    [310 400] colorado:ModerateEvapotranspiration
    [137 310] colorado:LowEvapotranspiration))

(defmodel infiltration colorado:SoilInfiltrationClass
  (probabilistic-measurement colorado:SoilInfiltrationClass "mm"
    [250 500] colorado:HighInfiltration
    [ 50 250] colorado:ModerateInfiltration
    [  0  50] colorado:LowInfiltration))

(defmodel et-sink colorado:Evapotranspiration
  (bayesian colorado:Evapotranspiration
    :import   "aries.core::SurfaceWaterSinkColorado.xdsl"
    :context  [annual-temperature vegetation-type percent-canopy-cover]
    :keep     [colorado:EvapotranspirationClass]
    :result   evapotranspiration))

(defmodel infiltration-sink colorado:SoilInfiltration
  (bayesian colorado:SoilInfiltration
    :import   "aries.core::SurfaceWaterSinkColorado.xdsl"
    :context  [slope imperviousness soil-group]
    :keep     [colorado:SoilInfiltrationClass]
    :result   infiltration))

(defmodel surface-water-sink SurfaceWaterSink
  (measurement SurfaceWaterSink "mm"
    :context [infiltration-sink et-sink]
    :state   #(+ 
               (if (nil? (:soil-infiltration  %)) 0.0 (.getMean (:soil-infiltration  %)))
               (if (nil? (:evapotranspiration %)) 0.0 (.getMean (:evapotranspiration %))))))

;;;-------------------------------------------------------------------
;;; Groundwater sink models
;;;-------------------------------------------------------------------

;; (defmodel baseflow -> this is complex and requires MODFLOW outputs to identify contributions to gaining reaches.  
;; Give it a closer look when we've gotten a better handle on whether MODFLOW integration is possible.

;; (defmodel groundwater-sink GroundwaterSink
;;  (measurement GroundwaterSink "mm"
;;    :context [spring-discharge baseflow] 
;;    :state #(+ (:baseflow %)
;;               (:spring-discharge     %))))

;;;-------------------------------------------------------------------
;;; Surface water use models
;;;-------------------------------------------------------------------

;; Add any outgoing interbasin trasnfers here - their locations and quantities.
;; This quantity of water would just disappear from the watershed of
;; interest and appear in the watershed as a source.

(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm"))

(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUse
  (measurement AgriculturalSurfaceWaterUse "mm")) 

;; NB: this assumes each well uses 5 million gallons, a nonconservative estimate (range is 2-5 million).
(defmodel oil-and-gas-surface-water-use OilAndGasSurfaceWaterUse
  (measurement OilAndGasSurfaceWaterUse "mm"))

;; Total surface water use. Add the rival user groups
(defmodel total-surface-water-use TotalSurfaceWaterUse
  (measurement TotalSurfaceWaterUse "mm"  ;;This is an annual value
    :context [agricultural-surface-water-use residential-surface-water-use oil-and-gas-surface-water-use]
    :state   #(let [a (:agricultural-surface-water-use %)
                    r (:residential-surface-water-use  %)
                    o (:oil-and-gas-surface-water-use  %)]
                (+ (or a 0)
                   (or r 0)
                   (or o 0)))))

;; Water compacts. Actual is in mm, for the South Platte, though it needs to be moved. Testing, below, to see what the percentage of average flows are for the Arkansas and Republican. After getting that, need to create another layer with those actual values.
;;(defmodel water-compact-mm InterstateCompactActual
;;  (measurement InterstateCompactActual "mm"))

(defmodel water-compact-percent InterstateCompactActual
  (measurement InterstateCompactActual "mm"
    :context [(ranking InterstateCompactPercent)]
    :state   #(if (nil? (:interstate-compact-percent %)) nil (* (:interstate-compact-percent %) 10000000000))))

;; Total surface water use, without hydrofracking.
(defmodel total-surface-water-use-no-fracking colorado:TotalSurfaceWaterUseNoFracking
  (measurement TotalSurfaceWaterUse "mm"  ;;This is an annual value
    :context [agricultural-surface-water-use residential-surface-water-use water-compact-percent]
    :state   #(let [a (:agricultural-surface-water-use %)
                    r (:residential-surface-water-use  %)
                    c (:interstate-compact-actual      %)]
                (+ (or a 0)
                   (or r 0)
                   (or c 0)))))

;;;-------------------------------------------------------------------
;;; Groundwater use models
;;;-------------------------------------------------------------------

;; Add data on well use if you get it and are able to include groundwater.
(defmodel annual-well-capacity AnnualWellCapacity
  (measurement AnnualWellCapacity "mm"))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams-simple geofeatures:River
  (binary-coding geofeatures:River))

;; (defmodel groundwater-elevation GroundwaterElevation

(defmodel data-all WaterSupply
  (identification WaterSupply
    :context [runoff surface-water-sink
              total-surface-water-use altitude streams-simple]))

(defmodel data-all-no-fracking colorado:WaterSupplyNoFracking
  (identification WaterSupply
    :context [runoff surface-water-sink
              total-surface-water-use-no-fracking altitude streams-simple]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel surface-flow FreshwaterSupply
  (span SurfaceWaterMovement
        AnnualRunoffSummed
        TotalSurfaceWaterUse
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/water_colorado_data.clj")
        :context            [runoff surface-water-sink
                             total-surface-water-use altitude
                             streams-simple]
        :keep               [TheoreticalSource
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
                             InaccessibleSink
                             InaccessibleUse
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel surface-flow-no-fracking colorado:SurfaceWaterMovementNoFracking
  (span SurfaceWaterMovement
        AnnualRunoffSummed
        colorado:TotalSurfaceWaterUseNoFracking
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   0.0
        :sink-threshold     0.0 ; May want to set a source & use threshold.
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 2  ; Ran OK at 4, crashed at 1. Try intermediate values.
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/water_colorado_data_no_fracking.clj")
        :context            [runoff surface-water-sink
                             total-surface-water-use-no-fracking altitude
                             streams-simple]
        :keep               [TheoreticalSource
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
                             InaccessibleSink
                             InaccessibleUse
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

;; Flow model for groundwater
;; (defmodel groundwater-flow GroundwaterMovement
;;  (span GroundwaterMovement
;;    habitat:AnnualRecharge   
;;    Wells
;;    SurfaceWaterSinkClass ; Replace with proper GW sink class when it's ready
;;    nil
;;    geophysics:Altitude     ; Uncertain of dependencies here
;;      :source-threshold   1.0  ; Tough to define without further consideration of GW flow model
;;      :sink-threshold     1.0  ; Tough to define without further consideration of GW flow model
;;      :use-threshold      10.0 ; Tough to define without further consideration of GW flow model
;;      :trans-threshold    nil  ; Tough to define without further consideration of GW flow model
;;      :source-type      :finite
;;      :sink-type        :finite
;;      :use-type         :finite
;;      :benefit-type     :rival
;;      :downscaling-factor 3
;;      :rv-max-states      10 
;;      :save-file          (str (System/getProperty "user.home") "/water_san_pedro_data_groundwater.clj")
;;      :context [recharge groundwater-sink well-presence]
;;      :keep [TheoreticalSource
;;             TheoreticalSink
;;             TheoreticalUse
;;             PossibleFlow
;;             PossibleSource
;;             PossibleUse
;;             ActualFlow
;;             ActualSource
;;             ActualSink
;;             ActualUse
;;             InaccessibleSource
;;             InaccessibleSink
;;             InaccessibleUse
;;             BlockedFlow
;;             BlockedSource
;;             BlockedUse])))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------