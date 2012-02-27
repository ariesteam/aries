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
;;; Water supply model for San Pedro
;;;
;;; Valid Contexts: core.contexts.san-pedro/san-pedro-us*
;;;
;;;-------------------------------------------------------------------

(ns core.models.water-san-pedro
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
  (WaterSupplyVegetationType (WaterSupplyVegetationTypeOpen) (WaterSupplyVegetationTypeConstrained))
  (representation:GenericObservable
   (TempSurfaceWaterData SurfaceDiversionCapacityCode)))

;;;-------------------------------------------------------------------
;;; Surface water source models
;;;-------------------------------------------------------------------

;; Surface water source = precipitaiton + snowmelt + springs + baseflow
;; + incoming interbasin water transfers

;; Runoff data might be preferable to precipitation - check scale.  

;; Snowmelt data (currently only at global scale) says the only
;; snowmelt in AZ is from the White Mtns & Colorado Plateau - none for
;; the SE AZ.  Keep snowmelt out of the model unless a local dataset
;; says otherwise.

;; No incoming interbasin water transfers to the San Pedro at this
;; point (nb: incoming interbasin water transfers could be groundwater
;; sources if incoming water is directly used to recharge
;; groundwater).

(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

(defmodel precipitation-dry-year AnnualPrecipitationDryYear
  (measurement habitat:AnnualPrecipitation2002 "mm"))

(defmodel precipitation-wet-year AnnualPrecipitationWetYear
  (measurement habitat:AnnualPrecipitation2007 "mm"))

;; Springs can be a source of surface water or a sink for groundwater.
;; At least for arid regions, springs are likely not a net source - 
(defmodel spring-discharge SpringDischarge
  (measurement SpringDischarge "mm"
    :context [(binary-coding Springs)]
    :state   #(cond (== (:springs %) 0) 0
                    (== (:springs %) 1) 100)))

;; (defmodel baseflow -> this is complex and requires MODFLOW outputs
;; to identify contributions to gaining reaches.  Give it a closer
;; look when we've gotten a better handle on whether MODFLOW
;; integration is possible.

;; Incorporate actual runoff data in the future once we've done a
;; better job with the hydro modeling.  Runoff as a sum of precip,
;; snowmelt, spring discharge, baseflow, incoming interbasin water
;; transfers.
(defmodel runoff AnnualRunoffSummed
  (measurement AnnualRunoffSummed "mm"
    :context [precipitation-annual spring-discharge]
    :state #(+ (:precipitation-annual %)
               (:spring-discharge     %))))

;;;-------------------------------------------------------------------
;;; Groundwater source models
;;;-------------------------------------------------------------------

;; Consider using percolation data here instead if more appropriate?
(defmodel recharge habitat:AnnualRecharge
  (measurement habitat:AnnualRecharge "mm"))

;; (defmodel artificial-recharge ArtificialRecharge (artificial recharge can be added to natural recharge
;;  as a source of groundwater)

;; No incoming interbasin water transfers to the San Pedro at this
;; point (nb: incoming interbasin water transfers could be groundwater
;; sources if incoming water is directly used to recharge
;; groundwater).

;;;-------------------------------------------------------------------
;;; Surface water sink models
;;;-------------------------------------------------------------------

;; Ad hoc sink model adapted from the ad hoc flood sink model.
;; Includes infiltration & evapotranspiration processes.  Deterministic
;; models could be used to replace this as appropriate.

(defmodel mountain-front sanPedro:MountainFront 
  (classification (binary-coding geofeatures:MountainFront)
    1           sanPedro:MountainFrontPresent
    :otherwise  sanPedro:MountainFrontAbsent))

(defmodel stream-channel sanPedro:StreamChannel
  (classification (binary-coding geofeatures:EphemeralStream) 
    1           sanPedro:StreamChannelPresent
    :otherwise  sanPedro:StreamChannelAbsent))

;; Global layer looks funny (when using for Mexico as well) -
;; discretization should be something like >38, 34-38, <34.  Clearly
;; these don't refer to identical concepts.
(defmodel annual-temperature sanPedro:AnnualMaximumTemperature
  (classification (measurement geophysics:AnnualMaximumGroundSurfaceTemperature "\u00b0C")
    [28 :>]   sanPedro:VeryHighAnnualMaximumTemperature
    [22 28]   sanPedro:HighAnnualMaximumTemperature
    [:< 22]   sanPedro:ModerateAnnualMaximumTemperature)) 

(defmodel vegetation-type sanPedro:WaterSupplyVegetationType
  "Reclass of SWReGAP & CONABIO LULC layers"
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{22 23 24 25 26 27 28 29 30 31 32 34 35 36 37 38 45 92}                           sanPedro:Forest
    #{33 41 91}                                                                        sanPedro:OakWoodland
    #{52 109}                                                                          sanPedro:MesquiteWoodland
    #{62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 90 93}                              sanPedro:Grassland
    #{19 39 40 42 43 44 46 47 48 49 50 51 53 54 55 56 57 58 59 60 61 94 95 96 105 108} sanPedro:DesertScrub
    #{77 78 79 80 81 83 84 85 98 110 118}                                              sanPedro:Riparian
    114                                                                                sanPedro:Agriculture
    #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 111 112}                      sanPedro:UrbanBarrenWater)
  (classification (categorization mexico:CONABIOLULCCategory)
    #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"}                  sanPedro:Forest
    #{"Bosque de encino" "Vegetacion de galeria"}                                sanPedro:OakWoodland
    #{"Mezquital-huizachal"}                                                     sanPedro:MesquiteWoodland
    #{"Pastizal natural"}                                                        sanPedro:Grassland
    #{"Chaparral" "Matorral desertico microfilo" "Mattoral sarcocrasicaule" "Vegetacion halofila y gipsofila" "Vegetacion de suelos arenosos"} sanPedro:DesertScrub
    #{"Manejo agricola, pecuario y forestal (plantaciones)"}                     sanPedro:Riparian
    #{"Cuerpos de agua" "Ciudades importantes" "Areas sin vegetacion aparente"}  sanPedro:UrbanBarrenWater))

(defmodel percent-canopy-cover sanPedro:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive] sanPedro:VeryHighCanopyCover
    [60 80]             sanPedro:HighCanopyCover
    [40 60]             sanPedro:ModerateCanopyCover
    [20 40]             sanPedro:LowCanopyCover
    [0 20]              sanPedro:VeryLowCanopyCover))

;; Global dataset values are in the range of 25-30 mm for the San
;; Pedro but (uncalibrated) SWAT model results say 99-482.  Need to
;; resolve which is correct.  Later on this should be used for
;; training, but not yet.
(defmodel evapotranspiration sanPedro:EvapotranspirationClass
  (probabilistic-measurement sanPedro:EvapotranspirationClass "mm"
    [60 120] sanPedro:HighEvapotranspiration
    [30  60] sanPedro:ModerateEvapotranspiration
    [ 0  30] sanPedro:LowEvapotranspiration))

(defmodel infiltration sanPedro:SoilInfiltrationClass
  (probabilistic-measurement waterSupplyService:SoilInfiltrationClass  "mm"
    [10 30] sanPedro:HighInfiltration
    [ 5 10] sanPedro:ModerateInfiltration
    [ 0  5] sanPedro:LowInfiltration))

(defmodel et-sink Evapotranspiration
  (bayesian Evapotranspiration
    :import   "aries.core::SurfaceWaterSinkSanPedro.xdsl"
    :context  [annual-temperature vegetation-type percent-canopy-cover]
    :keep     [sanPedro:EvapotranspirationClass]
    :result   evapotranspiration))

(defmodel infiltration-sink sanPedro:SoilInfiltration
  (bayesian sanPedro:SoilInfiltration
    :import   "aries.core::SurfaceWaterSinkSanPedro.xdsl"
    :context  [stream-channel mountain-front]
    :keep     [sanPedro:SoilInfiltrationClass]
    :result   infiltration))

(defmodel surface-water-sink SurfaceWaterSink
  (measurement SurfaceWaterSink "mm"
    :context [infiltration-sink et-sink]
    :state   #(+ 
               (if (nil? (:soil-infiltration  %)) 0.0 (.getMean (:soil-infiltration  %)))
               (if (nil? (:evapotranspiration %)) 0.0 (.getMean (:evapotranspiration %))))))

;; Add artificial recharge as a sink of surface water.  Can sum with
;; the natural surface-sink to get total surface water sink.

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

;; Add any interbasin trasnfers here - their locations and quantities.
;; This quantity of water would just disappear from the watershed of
;; interest and appear in the watershed as a source.

;; The Pomerene Diversion is 7.5 miles downstream of the St. David
;;  diversion, actually located between the Highway 80 bridge over the
;;  San Pedro and Benson (far south of Pomerene).  The canal irrigates
;;  1,050 ac, mostly pasture (67%) and small grains (11%).  From
;;  1968-1972 its discharge was 1,400 ac-ft/yr.  St. David Ditch also
;;  irrigates 1,050 ac of pasture (79%) and alfalfa (7%).  From
;;  1968-1972 its discharge was 4,600 ac-ft/yr (Lacher 1994).  This is
;;  equivalent to 1,335 mm water/yr for agricultural acreage watered
;;  by the St. David diversion and 406 mm water/yr for ag acreage
;;  watered by the Pomerene Diversion.  To get to points, I placed
;;  points for the two diversions at their approximate locations.  I
;;  took the acre-feet/yr extracted, assumed each point to be 1 ha in
;;  size when converted to raster (to ensure that it intersected the
;;  hydrography network), and converted ac-ft/yr from acres to ha and
;;  from ft to mm.  This gives values of 511,521 mm for the St. David
;;  Diversion and 155,680 mm for the Pomerene Diversion.

(defmodel surface-diversions SurfaceDiversionCapacity
  (measurement SurfaceDiversionCapacity "mm"))

(defmodel sdwrapper SurfaceDiversionCapacityCode
  (binary-coding SurfaceDiversionCapacityCode
    :context [surface-diversions]
    :state   #(cond (no-data? (:surface-diversion-capacity %)) 0
                    (zero?    (:surface-diversion-capacity %)) 0
                    :otherwise                                 1)))

;;;-------------------------------------------------------------------
;;; Groundwater use models
;;;-------------------------------------------------------------------

;; USPP Tech Committee has some recent docs on rural well water use
;; (see email from Susan Bronson)
(defmodel annual-well-capacity AnnualWellCapacity
  (measurement AnnualWellCapacity "mm"))

(defmodel well-presence Wells 
  (binary-coding Wells
    :context [annual-well-capacity]
    :state   #(if (nil? (:annual-well-capacity %)) 0 1))) 

(defmodel well-presence2 Wells
  [(categorization geofeatures:Country)]
  (binary-coding Wells
    :context [annual-well-capacity]
    :state   #(if (nil? (:annual-well-capacity %)) 0 1) 
    :when    #(= (:country %) "United States"))
  (binary-coding Wells))

;; Have data on well locations/depths/capacities/ownership.  To use
;; well depth in the model you'd need to pair it with a groundwater
;; surface contour map (which we don't have but may be able to get).
;; Use capacity as a proxy for use, for wells lacking capacity data
;; use their locations plus probability distribution of known well
;; capacities.  Separate out wells by type of ownership to designate
;; different beneficiary groups and ways of valuing water.

;; For AZ, removed "monitoring" and "other" (e.g., exploration,
;; geotechnical) wells leaving only exempt & non-exempt.  Exempt wells
;; have a maximum capacity of less than or equal to 35 gallons per
;; minute.  Non-exempt wells are within Active Management Areas,
;; drilled pursuant to a groundwater right or authorized groundwater
;; withdrawal permit
;; (http://gisweb.azwater.gov/WellRegistry/SearchFAQ.aspx#WellType)

;;Currently setting wells in Sonora to 35 gallons per minute (conversion below is to start with the "1" from 
;; presence absence, multiply by 35, then convert from GPM to mm/yr.  Talk with Gary on how to turn this into a 
;; probability distribution.
;;(defmodel well-extraction2 AnnualWellCapacity
;;  [(categorization geofeatures:Country)]
;;  (measurement AnnualWellCapacity "mm"
;;               :when #(= (:country %) "United States"))
;;  (measurement AnnualWellCapacity "mm"
;;               :context [(binary-coding infrastructure:Well)]
;;               :state   #(* (:well %) 17407215)))

;;(defmodel well-extraction AnnualWellCapacity
;; (measurement AnnualWellCapacity "mm")) 

;;Use "OWNER_NAME" attribute
;;(defmodel well-ownership WellOwnership
;;Agricultural ;;Name includes "ranch," "farms"
;;Domestic
;;Military ;;US ARMY FT HUACHUCA,,
;;Mining  ;;ASARCO INC, ASARCO INCORPORATED-RAY COMPLEX, ASARCO INC,, BHP COPPER INC,, BHP MINERALS, BHP BILLITON, PHELPS DODGE CORP,, PHELPS DODGE CORPORATION
;;Other

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams-simple geofeatures:River ; This could be rasterized to speed the process, though probably not critical.
  (binary-coding geofeatures:River))

;; (defmodel groundwater-elevation GroundwaterElevation

(defmodel data-wet-year WaterSupplyWetYear
  (identification WaterSupplyWetYear
    :context [precipitation-wet-year surface-water-sink
              surface-diversions altitude streams-simple]))

(defmodel data-dry-year WaterSupplyDryYear
  (identification WaterSupplyDryYear
    :context [precipitation-dry-year surface-water-sink
              surface-diversions altitude streams-simple]))

(defmodel data-full-test WaterSupplyDryYear
  (identification WaterSupplyDryYear
    :context [precipitation-wet-year precipitation-dry-year
              surface-water-sink surface-diversions altitude
              streams-simple]))

;; Other elements for export to NetCDF, that will eventually go into groundwater models.
(defmodel other-data GroundwaterSupply 
  (identification GroundwaterSupply 
    :context [precipitation-annual recharge well-presence]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Flow model for surface water in a dry year
(defmodel surface-flow-dry FreshwaterSupply
  (span SurfaceWaterMovement
        AnnualPrecipitationDryYear
        SurfaceDiversionCapacity
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   5.0  ; La Antigua used 1500, also 500 & 100
        :sink-threshold     5.0  ; La Antigua used 25
        :use-threshold      1.0  ; La Antigua used 5
        :trans-threshold    0.1  ; La Antigua used 5
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/water_san_pedro_data_dry_year.clj")
        :context            [precipitation-dry-year surface-water-sink
                             surface-diversions altitude
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

;; Flow model for surface water in a wet year
(defmodel surface-flow-wet FreshwaterSupply
  (span SurfaceWaterMovement
        AnnualPrecipitationWetYear
        SurfaceDiversionCapacity
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   5.0
        :sink-threshold     5.0
        :use-threshold      1.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/water_san_pedro_data_wet_year.clj")
        :context            [precipitation-wet-year surface-water-sink
                             surface-diversions altitude
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

(defmodel constrained-development-scenario sanPedro:ConstrainedDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained) 
    #{10 11 12 13 19 22 25}                sanPedro:DevelopedConstrained
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28} sanPedro:NotDevelopedConstrained))

(defmodel open-development-scenario sanPedro:OpenDevelopment
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen) 
    #{10 11 12 13 19 22 25}                   sanPedro:DevelopedOpen
    #{0 1 2 4 5 6 7 8 9 14 16 23 26 27 28 29} sanPedro:NotDevelopedOpen))

(defmodel vegetation-type-constrained WaterSupplyVegetationTypeConstrained
  "Reclass of Steinitz LULC layers where they have coverage"
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained)
    1                             sanPedro:Forest
    2                             sanPedro:OakWoodland
    #{6 26}                       sanPedro:MesquiteWoodland
    #{4 5}                        sanPedro:Grassland
    #{7 23}                       sanPedro:DesertScrub
    #{27 28 29 30}                sanPedro:Riparian
    #{8 9}                        sanPedro:Agriculture
    #{10 11 12 13 14 16 19 22 25} sanPedro:UrbanBarrenWater))

(defmodel vegetation-type-open WaterSupplyVegetationTypeOpen
  "Reclass of Steinitz LULC layers where they have coverage"
  (classification (numeric-coding sanPedro:Steinitz30ClassUrbanGrowthLULCOpen)
    1                             sanPedro:Forest
    2                             sanPedro:OakWoodland
    #{6 26}                       sanPedro:MesquiteWoodland
    #{4 5}                        sanPedro:Grassland
    #{7 23}                       sanPedro:DesertScrub
    #{27 28 29 30}                sanPedro:Riparian
    #{8 9}                        sanPedro:Agriculture
    #{10 11 12 13 14 16 19 22 25} sanPedro:UrbanBarrenWater))

(defscenario open-development-water
  "Changes values in developed areas to very low vegetation cover, no fire frequency, increased greenhouse gas emissions."
  (model sanPedro:PercentTreeCanopyCoverClass
    (classification sanPedro:PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:VeryLowCanopyCover)
                  (:pcc %))))
  (model sanPedro:WaterSupplyVegetationType
    (classification sanPedro:WaterSupplyVegetationType
      :context [vegetation-type-open :as vto vegetation-type :as vt]
      :state   #(if (no-data? (:vto %))
                  (:vt %)
                  (:vto %))))
  (model sanPedro:MountainFront
    (classification sanPedro:MountainFront
      :context [open-development-scenario :as od mountain-front :as mf]
      :state   #(if (is? (:od %) (conc 'sanPedro:DevelopedOpen))
                  (conc 'sanPedro:MountainFrontAbsent)
                  (:mf %)))))

(defscenario constrained-development-water
  "Changes values in developed areas to very low vegetation cover, no fire frequency, increased greenhouse gas emissions."
  (model sanPedro:PercentTreeCanopyCoverClass
    (classification sanPedro:PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:VeryLowCanopyCover)
                  (:pcc %))))
  (model sanPedro:WaterSupplyVegetationType
    (classification sanPedro:WaterSupplyVegetationType
      :context [vegetation-type-constrained :as vtc  vegetation-type :as vt]
      :state   #(if (no-data? (:vtc %))
                  (:vt %)
                  (:vtc %))))
  (model sanPedro:MountainFront
    (classification sanPedro:MountainFront
      :context [constrained-development-scenario :as cd mountain-front :as mf]
      :state   #(if (is? (:cd %) (conc 'sanPedro:DevelopedConstrained))
                  (conc 'sanPedro:MountainFrontAbsent)
                  (:mf %)))))