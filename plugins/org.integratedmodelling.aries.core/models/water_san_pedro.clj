(ns core.models.water-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

;;Runoff data might be preferable to precipitation - check scale.  Snowmelt data (currently only at global scale) 
;; says the only snowmelt in AZ is from the White Mtns & Colorado Plateau - none for the Sky Islands.  
;; Keep snowmelt out of the model unless a local dataset says otherwise.
(defmodel precipitation-annual 'waterSupplyService:AnnualPrecipitation
  (measurement 'habitat:AnnualPrecipitation "mm"))

;;USE APPROPRIATE SWAT/KINEROS INPUTS:
;; SWAT water yield, transmission loss, surface runoff, percolation precip, ET
;; KINEROS runoff, infiltration

(defmodel recharge 'habitat:AnnualRecharge
  (measurement 'habitat:AnnualRecharge "mm"))

;;Incorporate runoff data in the future once we've done a better job with the hydro modeling.
;;(defmodel runoff 'soilretentionEcology:AnnualRunoff
	;;(classification (measurement 'soilretentionEcology:Runoff "mm/year"))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;;Ad hoc sink model adapted from the ad hoc flood sink model.  Includes infiltration & evapotranspiration
;;processes.  Deterministic models could likely be used.

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

;;REWORK FOR APPROPRIATE SAN PEDRO LAYER
(defmodel vegetation-type 'waterSupplyService:VegetationType
  "Just a reclass of the Veracruz land use layer"
  (classification (categorization 'veracruz-lulc:VeracruzLULCCategory)
    "Bosque mesofilo de montana"                                                 'waterSupplyService:CloudForest
    #{"Pastizal cultivado" "Pastizal inducido" "Zona Urbana" "riego" "temporal"} 'waterSupplyService:DevelopedCultivated
    #{"Selva alta subperennifolia" "Bosque cultivado" "Bosque de encino" "Bosque de encino-pino" "Bosque de oyamel" "Bosque de pino" "Bosque de pino-encino" "Bosque de tascate"} 'waterSupplyService:DryForest
    #{"Matorral desertico rosetofilo" "Pradera de alta montana" "Vegetacion de dunas costeras" "Vegetacion halofila" "Popal"} 'waterSupplyService:GrasslandShrubland
    #{"Selva baja caducifolia" "Selva mediana subcaducifolia"}                   'waterSupplyService:Rainforest))

(defmodel percent-vegetation-cover 'waterSupplyService:PercentVegetationCoverClass
  (classification (ranking 'habitat:PercentVegetationCover)
    [80 100] 'waterSupplyService:VeryHighVegetationCover
    [60 80]  'waterSupplyService:HighVegetationCover
    [40 60]  'waterSupplyService:ModerateVegetationCover
    [20 40]  'waterSupplyService:LowVegetationCover
    [1 20]   'waterSupplyService:VeryLowVegetationCover))

;;Make discretization reflect values from SE AZ
(defmodel evapotranspiration 'floodService:EvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [90 :>]    'floodService:VeryHighEvapotranspiration
                  [60 90]    'floodService:HighEvapotranspiration
                  [30 60]    'floodService:ModerateEvapotranspiration
                  [12 30]    'floodService:LowEvapotranspiration
                  [0 12]     'floodService:VeryLowEvapotranspiration)) 

;;Springs can be a source of surface water or a sink for groundwater
(defmodel spring-discharge 'waterSupplyService:SpringDischarge
  (measurement 'waterSupplyService:SpringDischarge "mm"
    :context ((binary-coding 'waterSupplyService:Springs :as spring-presence))
    :state #(cond (== (:spring-presence %) 0) 0
                  (== (:spring-presence %) 1) 100)))

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration.
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
;; use model
;; ----------------------------------------------------------------------------------------------

;;Well locations/depths/capacities.  Any way to separate domestic from other uses?
;;  mining/agriculture/Ft. Huachuca?
;;Create a layer showing locations of Pomerene & St. David diversions, plus ag serviced
;;  by them: Pomerene Diversion is 7.5 miles downstream of the St. David diversion, actually located between 
;;  the Highway 80 bridge over the San Pedro and Benson (far south of Pomerene).  The canal irrigates 1,050
;;  ac, mostly pasture (67%) and small grains (11%).  From 1968-1972 its discharge was 1,400 ac-ft/yr.  St. David
;;  Ditch also irrigates 1,050 ac of pasture (79%) and alfalfa (7%).  
;;  From 1968-1972 its discharge was 4,600 ac-ft/yr (Lacher 1994).

;;(defmodel well-extraction 'waterSupplyService:AnnualWellCapacity










;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Everything below needs to be updated correctly for water.
 	 								
;;(defmodel altitude 'geophysics:Altitude
  ;;(measurement 'geophysics:Altitude "m"))	 								
 
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

