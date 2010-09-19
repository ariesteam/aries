(ns core.models.water-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; surface water source model
;; ----------------------------------------------------------------------------------------------

;;Runoff data might be preferable to precipitation - check scale.  Snowmelt data (currently only at global scale) 
;; says the only snowmelt in AZ is from the White Mtns & Colorado Plateau - none for the SE AZ.  
;; Keep snowmelt out of the model unless a local dataset says otherwise.
(defmodel precipitation-annual 'waterSupplyService:AnnualPrecipitation
  (measurement 'habitat:AnnualPrecipitation "mm"))

;;Springs can be a source of surface water or a sink for groundwater.
;; Springs data are having a lot of problems with Geoserver.
(defmodel spring-discharge 'waterSupplyService:SpringDischarge
  (measurement 'waterSupplyService:SpringDischarge "mm"
    :context ((binary-coding 'waterSupplyService:Springs :as spring-presence))
    :state #(cond (== (:spring-presence %) 0) 0
                  (== (:spring-presence %) 1) 100)))

;;(defmodel baseflow (as water yield?) - would do this as a GIS operation, assigning baseflow to 
;; particular segments of the stream network - either all at the base or distributed somehow across
;; the network - both are naive assumptions, in different ways.  Talk to Darius about this.

;;Incorporate actual runoff data in the future once we've done a better job with the hydro modeling.
;; Runoff as a sum of precip, snowmelt, spring discharge.
(defmodel runoff 'soilretentionEcology:AnnualRunoff
  (measurement 'soilretentionEcology:AnnualRunoff "mm/year"
    :context (precipitation-annual :as precipitation-annual spring-discharge :as spring-discharge) 
    :state #(+ (:precipitation-annual %)
               (:spring-discharge     %))))

;; ----------------------------------------------------------------------------------------------
;; groundwater source model
;; ----------------------------------------------------------------------------------------------

;;Consider using percolation data here instead if more appropriate?
(defmodel recharge 'habitat:AnnualRecharge
  (measurement 'habitat:AnnualRecharge "mm"))

;; ----------------------------------------------------------------------------------------------
;; surface water sink model
;; ----------------------------------------------------------------------------------------------

;;Ad hoc sink model adapted from the ad hoc flood sink model.  Includes infiltration & evapotranspiration
;;processes.  Deterministic models could be used to replace this as appropriate.

(defmodel slope 'waterSupplyService:SlopeClass
    (classification (measurement 'geophysics:DegreeSlope "\u00B0")
       [0 1.15]               'waterSupplyService:Level
       [1.15 4.57]            'waterSupplyService:GentlyUndulating
       [4.57 16.70]           'waterSupplyService:RollingToHilly
       [16.70 90 :inclusive]  'waterSupplyService:SteeplyDissectedToMountainous))

(defmodel soil-group 'waterSupplyService:HydrologicSoilsGroup
  (classification (ranking 'habitat:HydrologicSoilsGroup)
      1       'waterSupplyService:SoilGroupA
      2       'waterSupplyService:SoilGroupB
      3       'waterSupplyService:SoilGroupC
      4       'waterSupplyService:SoilGroupD))

(defmodel imperviousness 'waterSupplyService:PercentImperviousCoverClass
   (classification (ranking 'habitat:PercentImperviousness)
       [80 100 :inclusive]   'waterSupplyService:VeryHighImperviousCover
       [50 80]               'waterSupplyService:HighImperviousCover
       [20 50]               'waterSupplyService:ModeratelyHighImperviousCover
       [10 20]               'waterSupplyService:ModeratelyLowImperviousCover
       [5 10]                'waterSupplyService:LowImperviousCover
       [1 5]                 'waterSupplyService:VeryLowImperviousCover))

(defmodel vegetation-type 'waterSupplyService:VegetationType
  "Reclass of SWReGAP & CONABIO LULC layers"
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
    #{22 23 24 25 26 27 28 29 30 31 32 34 35 36 37 38 45 92}                           'sanPedro:Forest
    #{33 41 91}                                                                        'sanPedro:OakWoodland
    #{52 109}                                                                          'sanPedro:MesquiteWoodland
    #{62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 90 93}                              'sanPedro:Grassland
    #{19 39 40 42 43 44 46 47 48 49 50 51 53 54 55 56 57 58 59 60 61 94 95 96 105 108} 'sanPedro:DesertScrub
    #{77 78 79 80 81 83 84 85 98 109 110 118}                                          'sanPedro:Riparian
    114                                                                                'sanPedro:Agriculture
    #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 110 111 112}                  'sanPedro:UrbanBarrenWater)
   (classification (categorization 'mexico:CONABIOLULCCategory)
     #{"Bosque de coniferas distintas a Pinus" "Bosque de pino"}                  'sanPedro:Forest
     #{"Bosque de encino" "Vegetacion de galeria"}                                'sanPedro:OakWoodland
     #{"Mezquital-huizachal"}                                                     'sanPedro:MesquiteWoodland
     #{"Pastizal natural"}                                                        'sanPedro:Grassland
     #{"Chaparral" "Matorral desertico microfilo" "Mattoral sarcocrasicaule" "Vegetacion halofila y gipsofila" "Vegetacion de suelos arenosos"} 'sanPedro:DesertScrub
     #{"Manejo agricola, pecuario y forestal (plantaciones)"}                     'sanPedro:Riparian
     #{"Cuerpos de agua" "Ciudades importantes" "Areas sin vegetacion aparente"}  'sanPedro:UrbanBarrenWater))

(defmodel percent-vegetation-cover 'waterSupplyService:PercentVegetationCoverClass
  (classification (ranking 'habitat:PercentVegetationCover)
    [80 100] 'waterSupplyService:VeryHighVegetationCover
    [60 80]  'waterSupplyService:HighVegetationCover
    [40 60]  'waterSupplyService:ModerateVegetationCover
    [20 40]  'waterSupplyService:LowVegetationCover
    [1 20]   'waterSupplyService:VeryLowVegetationCover))

;;Global dataset values are in the range of 25-30 mm for the San Pedro but SWAT model results say 99-482.
;; Need to resolve which is correct.
(defmodel evapotranspiration 'floodService:EvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [90 :>]    'floodService:VeryHighEvapotranspiration
                  [60 90]    'floodService:HighEvapotranspiration
                  [30 60]    'floodService:ModerateEvapotranspiration
                  [12 30]    'floodService:LowEvapotranspiration
                  [0 12]     'floodService:VeryLowEvapotranspiration)) 

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration.

;; TIE TO RUNOFF?
(defmodel sink-undiscretizer 'waterSupplyService:SurfaceWaterSinkClass
  (classification 'waterSupplyService:SurfaceWaterSinkClass 
    [180 :>]           'waterSupplyService:VeryHighSurfaceWaterSink
    [100 180]          'waterSupplyService:HighSurfaceWaterSink
    [50 100]           'waterSupplyService:ModerateSurfaceWaterSink
    [:exclusive 0 50]  'waterSupplyService:LowSurfaceWaterSink
    0                  'waterSupplyService:NoSurfaceWaterSink))

(defmodel sink 'waterSupplyService:SurfaceWaterSinkClass
    (bayesian 'waterSupplyService:SurfaceWaterSinkClass
      :import   "aries.core::SurfaceWaterSupplySinkSanPedro.xdsl"
      :context  (soil-group vegetation-type slope imperviousness percent-vegetation-cover)
      :keep     ('waterSupplyService:SurfaceWaterSinkClass)
      :observed (sink-undiscretizer)))

;; ----------------------------------------------------------------------------------------------
;; groundwater sink model
;; ----------------------------------------------------------------------------------------------

;;(defmodel baseflow (as water yield?) - would do this as a GIS operation, assigning baseflow to 
;; particular segments of the stream network - either all at the base or distributed somehow across
;; the network - both are naive assumptions, in different ways.  Talk to Darius about this.

;;(defmodel groundwater-sink 'soilretentionEcology:GroundwaterSink
;;  (measurement 'soilretentionEcology:GroundwaterSink "mm/year"
;;    :context (spring-discharge :as spring-discharge baseflow :as baseflow) 
;;    :state #(+ (:baseflow %)
;;               (:spring-discharge     %))))

;; ----------------------------------------------------------------------------------------------
;; surface water use model
;; ----------------------------------------------------------------------------------------------

;;The Pomerene Diversion is 7.5 miles downstream of the St. David diversion, actually located between 
;;  the Highway 80 bridge over the San Pedro and Benson (far south of Pomerene).  The canal irrigates 1,050
;;  ac, mostly pasture (67%) and small grains (11%).  From 1968-1972 its discharge was 1,400 ac-ft/yr.  St. David
;;  Ditch also irrigates 1,050 ac of pasture (79%) and alfalfa (7%).  From 1968-1972 its discharge was 
;;  4,600 ac-ft/yr (Lacher 1994).  This is equivalent to 1,335 mm water/yr for agricultural acreage watered 
;;  by the St. David diversion and 406 mm water/yr for ag acreage watered by the Pomerene Diversion.
;;To get to points, I placed points for the two diversions at their approximate locations.  I took the acre-feet/yr
;; extracted, assumed each point to be 1 ha in size when converted to raster (to ensure that it intersected the 
;; hydrography network), and converted ac-ft/yr from acres to ha and from ft to mm.  This gives values of 511,521 mm
;; for the St. David Diversion and 155,680 mm for the Pomerene Diversion.

(defmodel surface-diversions 'waterSupplyService:SurfaceDiversionCapacity
 (measurement 'waterSupplyService:SurfaceDiversionCapacity "mm"))

;; ----------------------------------------------------------------------------------------------
;; groundwater use model
;; ----------------------------------------------------------------------------------------------

(defmodel well-presence 'waterSupplyService:Wells 
 (binary-coding 'waterSupplyService:Wells
    :context ((measurement 'waterSupplyService:AnnualWellCapacity "mm" :as well-capacity))
    :state   #(if (nil? (:well-capacity %)) 0 1))) 

(defmodel well-presence2 'waterSupplyService:Wells
  [(categorization 'geofeatures:Country :as country)]
  (binary-coding 'waterSupplyService:Wells
    :context ((measurement 'waterSupplyService:AnnualWellCapacity "mm" :as well-capacity))
    :state   #(if (nil? (:well-capacity %)) 0 1) 
    :when    #(= (:country %) "United States"))
  (binary-coding 'waterSupplyService:Wells))

;;Have data on well locations/depths/capacities/ownership.  To use well depth in the model you'd need to
;; pair it with a groundwater surface contour map (which we don't have but may be able to get).  Use
;; capacity as a proxy for use, for wells lacking capacity data use their locations plus probability
;; distribution of known well capacities.  Separate out wells by type of ownership to designate 
;; different beneficiary groups and ways of valuing water.

;;For AZ, removed "monitoring" and "other" (e.g., exploration, geotechnical) wells leaving only exempt & non-exempt.
;; Exempt wells have a maximum capacity of less than or equal to 35 gallons per minute.  Non-exempt wells are 
;; within Active Management Areas, drilled pursuant to a groundwater right or authorized groundwater withdrawal
;; permit (http://gisweb.azwater.gov/WellRegistry/SearchFAQ.aspx#WellType)

;;Currently setting wells in Sonora to 35 gallons per minute (conversion below is to start with the "1" from 
;; presence absence, multiply by 35, then convert from GPM to mm/yr.  Talk with Gary on how to turn this into a 
;; probability distribution.
;;(defmodel well-extraction2 'waterSupplyService:AnnualWellCapacity
;;  [(categorization 'geofeatures:Country :as country)]
;;  (measurement 'waterSupplyService:AnnualWellCapacity "mm"
;;               :when #(= (:country %) "United States"))
;;  (measurement 'waterSupplyService:AnnualWellCapacity "mm"
;;               :context ((binary-coding 'infrastructure:Well :as well-presence))
;;               :state   #(* (:well-presence %) 17407215)))

;;(defmodel well-extraction 'waterSupplyService:AnnualWellCapacity
;; (measurement 'waterSupplyService:AnnualWellCapacity "mm")) 

;;Use "OWNER_NAME" attribute
;;(defmodel well-ownership 'waterSupplyService:WellOwnership
      ;;'waterSupplyService:Agricultural ;;Name includes "ranch," "farms"
      ;;'waterSupplyService:Domestic
      ;;'waterSupplyService:Military ;;US ARMY FT HUACHUCA,,
      ;;'waterSupplyService:Mining  ;;ASARCO INC, ASARCO INCORPORATED-RAY COMPLEX, ASARCO INC,, BHP COPPER INC,, BHP MINERALS, BHP BILLITON, PHELPS DODGE CORP,, PHELPS DODGE CORPORATION
      ;;'waterSupplyService:Other

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Update below for water.
 	 								
;;(defmodel altitude 'geophysics:Altitude
  ;;(measurement 'geophysics:Altitude "m"))	 								
 
;;(defmodel groundwater-elevation 'waterSupplyService:GroundwaterElevation

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
;;(defmodel data 'waterSupplyService:WaterSupply 
;;  (identification 'waterSupplyService:WaterSupply 
;;  :context (irrigation-water-use
;;            agricultural-surface-water-use
;;            industrial-users
;;            non-rival-water-users
;;            sink
;;            precipitation-annual)))
  
;; the real enchilada
;;(defmodel view 'aestheticService:AestheticView
 ;; (span 'aestheticService:LineOfSight 
  	;;    'aestheticService:TheoreticalNaturalBeauty
  	;;    'aestheticService:HomeownerViewUse
    ;;  	'aestheticService:TotalVisualBlight
    ;;  	'aestheticService:View
  	;;    'geophysics:Altitude
  ;; 	:sink-type        :relative
  ;; 	:use-type         :relative
  ;; 	:benefit-type     :non-rival
  ;; 	:downscaling-factor 3
  ;; 	:rv-max-states    10 
  ;;  :context
    ;;     (source homeowners sink altitude
    ;;     (ranking 'eserv:SourceThreshold :value 50)
    ;;     (ranking 'eserv:SinkThreshold :value 0.3)
    ;;     (ranking 'eserv:UseThreshold :value 0.1)
    ;;     (ranking 'eserv:TransitionThreshold :value 1.0))
;;))

