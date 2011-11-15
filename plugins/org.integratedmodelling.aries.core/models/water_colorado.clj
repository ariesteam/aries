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
;;; Water supply model for Colorado
;;;
;;; Valid Contexts: core.contexts.beta/co*
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

(defmodel vegetation-type colorado:EvapotranspirationVegetationType
  "Reclass of SWReGAP LULC layer"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{22 24 26 28 29 30 32 33 34 35 36 38 78 79 81 92 95 99 103 116 118 123 124}                       colorado:Forest
    #{1 2 4 5 7 8 9 10 11 13 14 15 17 19 21 40 41 42 43 44 46 48 50 53 56 58 62 77 82 104 108 109 113} colorado:ScrubBrush
    #{63 64 67 68 69 70 71 72 73 74 75 76 85 86 106 119 120 121 122}                                   colorado:ShortgrassPrairie
    110                                                                                                colorado:Water
    #{111 112 115 117 125}                                                                             colorado:UrbanBarren
    114                                                                                                colorado:Agriculture))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive] VeryHighCanopyCover
    [60 80]             HighCanopyCover
    [30 60]             ModerateCanopyCover
    [ 5 30]             LowCanopyCover
    [ 0  5]             VeryLowCanopyCover))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

;; Global dataset values are in the range of 18-39 mm for Colorado.
(defmodel evapotranspiration sanPedro:EvapotranspirationClass
  (probabilistic-measurement sanPedro:EvapotranspirationClass "mm"
    [30 40] sanPedro:HighEvapotranspiration
    [24 30] sanPedro:ModerateEvapotranspiration
    [18 24] sanPedro:LowEvapotranspiration))

(defmodel infiltration sanPedro:SoilInfiltrationClass
  (probabilistic-measurement sanPedro:SoilInfiltrationClass "mm"
    [250 500] sanPedro:HighInfiltration
    [ 50 250] sanPedro:ModerateInfiltration
    [  0  50] sanPedro:LowInfiltration))

(defmodel et-sink Evapotranspiration
  (bayesian Evapotranspiration
    :import   "aries.core::SurfaceWaterSinkColorado.xdsl"
    :context  [slope vegetation-type percent-canopy-cover]
    :keep     [sanPedro:EvapotranspirationClass]
    :result   evapotranspiration))

(defmodel infiltration-sink SoilInfiltration
  (bayesian SoilInfiltration
    :import   "aries.core::SurfaceWaterSinkColorado.xdsl"
    :context  [slope vegetation-type percent-canopy-cover]
    :keep     [sanPedro:SoilInfiltrationClass]
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

(defmodel flow-direction geophysics:FlowDirection
  (ranking geophysics:FlowDirection))

(defmodel streams-simple geofeatures:River ; This could be rasterized to speed the process, though probably not critical.
  (binary-coding geofeatures:River))

;; (defmodel groundwater-elevation GroundwaterElevation

(defmodel data-all WaterSupply
  (identification WaterSupply
    :context [precipitation-annual surface-water-sink
              total-surface-water-use altitude streams-simple]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel surface-flow SurfaceWaterMovement
  (span SurfaceWaterMovement
        AnnualPrecipitation
        TotalSurfaceWaterUse
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   0
        :sink-threshold     0
        :use-threshold      0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/water_san_pedro_data_dry_year.clj")
        :context            [precipitation-annual surface-water-sink
                             total-surface-water-use altitude
                             streams-simple]
        :keep               [SurfaceWaterSupply
                             MaximumSurfaceWaterSink
                             SurfaceWaterDemand
                             PossibleSurfaceWaterFlow
                             PossibleSurfaceWaterSupply
                             PossibleSurfaceWaterUse
                             ActualSurfaceWaterFlow
                              UsedSurfaceWaterSupply
                             ActualSurfaceWaterSink
                             SatisfiedSurfaceWaterDemand
                             UnusableSurfaceWaterSupply
                             UnusableSurfaceWaterSink
                             InaccessibleSurfaceWaterDemand
                             SunkSurfaceWaterFlow
                             SunkSurfaceWaterSupply
                             BlockedSurfaceWaterDemand]))

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
;;      :keep [GroundwaterRecharge        MaximumGroundwaterSink      GroundwaterDemand
;;             PossibleGroundwaterFlow    PossibleGroundwaterRecharge PossibleGroundwaterUse
;;             ActualGroundwaterFlow      UsedGroundwaterRechage      ActualGroundwaterSink          SatisfiedGroundwaterDemand
;;             UnusableGroundwaterRechage UnusableGroundwaterSink     InaccessibleGroundwaterDemand
;;             SunkGroundwaterFlow        SunkSurfaceWaterSupply      BlockedGroundwaterDemand])))

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------