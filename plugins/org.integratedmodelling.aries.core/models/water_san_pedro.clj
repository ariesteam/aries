(ns core.models.water-san-pedro
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

;;Runoff would be preferable to precipitation, but it's at a very coarse spatial resolution
;; (i.e., <3 full pixels for the La Antigua watershed.  Use precip for now, with the goal of 
;; incorporating a better runoff model (plus sink models that actually capture infiltration & ET).
(defmodel precipitation-annual 'waterSupplyService:AnnualPrecipitation
  (measurement 'habitat:AnnualPrecipitation "mm"))

;;Incorporate runoff data in the future once we've done a better job with the hydro modeling.
;;(defmodel runoff 'soilretentionEcology:AnnualRunoff
	;;(classification (measurement 'soilretentionEcology:Runoff "mm/year")
		;;[0 200] 	    'soilretentionEcology:VeryLowAnnualRunoff
		;;[200 600] 	  'soilretentionEcology:LowAnnualRunoff
		;;[600 1200]  	'soilretentionEcology:ModerateAnnualRunoff
		;;[1200 2400] 	'soilretentionEcology:HighAnnualRunoff
		;;[2400 :>] 	  'soilretentionEcology:VeryHighAnnualRunoff))

;;check snowmelt data for the San Pedro: see if it's an at all significant component (e.g., which 
;; mountain ranges does it fall on: Huachucas/Rincons, or others too?

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;Well locations/depths/capacities.  Any way to separate domestic from other uses?
;;  mining/agriculture/Ft. Huachuca?
;;Create a layer showing locations of Pomerene & St. David diversions, plus ag servicesd
;;  by them?

;;Industrial surface water use
;;This is all we have to model industrial use right now: presence/absence of an industrial
;;user and whether they use ground or surface water. 
;;Current extraction is unknown.  Values below are 100% guesses - need information from local plants.
;;2 & 3 are zeros because they represent groundwater use, which we're not yet modeling.
(defmodel industrial-users 'waterSupplyService:IndustrialWaterUse
  (measurement 'waterSupplyService:IndustrialWaterUse "mm" ;;This is an annual value.
    :context ((numeric-coding 'waterSupplyService:IndustrialWaterUseCode :as industrial-water-use))
    :state #(cond (== (:industrial-water-use %) 0) 50   ;;Paper factory, using surface water
                  (== (:industrial-water-use %) 1) 100  ;;Bottled water plant, using surface water
                  (== (:industrial-water-use %) 2)  0   ;;Nestle plant, using groundwater
                  (== (:industrial-water-use %) 3)  0   ;;Coca-Cola plant, using groundwater
                  :otherwise                        0)))

;;Nonrival surface water use: Hydropower plus rafting
;;This is all we have to model rafting and hydropower use right now: presence/absence of a user
;;user. It's a little strange to lump hydro and rafting together, but we'll
;;do it for now.
(defmodel non-rival-water-users 'waterSupplyService:NonRivalWaterUse
  (binary-coding 'waterSupplyService:NonRivalWaterUse
    :context ((binary-coding 'waterSupplyService:NonRivalWaterUseCode :as non-rival-water-users))
    :state #(cond (== (:non-rival-water-users %) 0) 1  ;;Rafting use
                  (== (:non-rival-water-users %) 1) 1  ;;Hydropower use
                  :otherwise                        0)))

;;Residential surface water use
;;The ranking model should really be a count with spatial ctx (data are persons/30 arc-second pixel)
;;The first example is for a probability distribution function (discrete values of the probability states)
;;The second example, used, is for a cumulative distribution function (ranges)
;;Neither of these are enabled yet, so we're just using a deterministic function for now.
;;Residential surface water use: currently only looking at surface water use (80% of the total, per Rowan's BN. 
;;Top node discretization for water use is from Alberta: worth asking about better local sources.
;;www1.agric.gov.ab.ca/$department/deptdocs.nsf/all/agdex1349
(defmodel residential-surface-water-use 'waterSupplyService:ResidentialSurfaceWaterUse
  (measurement 'waterSupplyService:ResidentialSurfaceWaterUse "mm" ;;This is an annual value
    :context ((count 'policytarget:PopulationDensity "/km^2" :as population-density))
    :state   #(* 0.8 0.082855 (:population-density %))))
;;  :state   #(rv-scalar-multiply {10 25/100, 20 50/100, 30 25/100} (* 0.8 (:population-density %))) 
;;  :state   #(rv-scalar-multiply {70.81 0, 78.84 25/100, 86.87 75/100, 94.9 1} (* 0.8 (:population-density %))))) 

;;Agricultural surface water use. Step 1: Estimate total livestock water needs.
;;Livestock use below is from different sources for pigs and other livestock: it's unlikely
;; that pigs should use more water per capita than cattle (Rowan to check on this)
(defmodel livestock-total-water-use 'waterSupplyService:LivestockWaterUse
  (measurement 'waterSupplyService:LivestockWaterUse "mm"  ;;This is an annual value
    :context ((count 'waterSupplyService:CattlePopulation "/km^2" :as cattle-population)
              (count 'waterSupplyService:SheepPopulation  "/km^2" :as sheep-population)
              (count 'waterSupplyService:PigsPopulation   "/km^2" :as pigs-population)
              (count 'waterSupplyService:GoatsPopulation  "/km^2" :as goats-population))
    :state    #(+ (* (:sheep-population  %) 0.002745) ;;this is in m^3/1000 animals, the conversion ultimately giving mm
                  (* (:goats-population  %) 0.002745)
                  (* (:cattle-population %) 0.011032)
                  (* (:pigs-population   %) 0.013310))))

(defmodel livestock-total-water-use-discretized 'waterSupplyService:LivestockTotalWaterUseClass
  (classification livestock-total-water-use
    [1.15 :>]  'waterSupplyService:HighLivestockTotalWaterUse
    [0.5 1.15] 'waterSupplyService:ModerateLivestockTotalWaterUse
    [:<  0.5]  'waterSupplyService:LowLivestockTotalWaterUse))

;;Agricultural surface water use. Step 2: Consider proximity to surface water.
(defmodel surface-water-proximity 'waterSupplyService:ProximityToSurfaceWaterClass
  (classification (measurement 'waterSupplyService:ProximityToSurfaceWater "m")
    [500 :>]     'waterSupplyService:LowSurfaceWaterProximity
    [250 500]    'waterSupplyService:ModerateSurfaceWaterProximity
    [:< 250]     'waterSupplyService:HighSurfaceWaterProximity))

;;Agricultural surface water use. Step 3: Estimate crop irrigation water needs.
(defmodel irrigation-water-use 'waterSupplyService:IrrigationWaterUseClass
  (measurement 'waterSupplyService:IrrigationWaterUse "mm"  ;;This is an annual value
     :context ((categorization 'veracruz-lulc:VeracruzLULCCategory))
     ;; FV temporarily changed the ID to veracruzlulccategory - attribution with :as seems broken for categories.
     ;; without the :as, the ID defaults to the lowercased concept name, so the code below works.
     :state   #(if (= (:veracruzlulccategory %) "riego")
                  2000
                  0)))

;;Classification of irrigationWaterUse into 6 classes.
(defmodel irrigation-water-use-discretized 'waterSupplyService:IrrigationWaterUseClass
  (classification irrigation-water-use
    [2400 :>]   'waterSupplyService:VeryHighIrrigationUse
    [2150 2400] 'waterSupplyService:HighIrrigationUse
    [1850 2150] 'waterSupplyService:ModerateIrrigationUse
    [1600 1850] 'waterSupplyService:LowIrrigationUse
    [:< 1600]   'waterSupplyService:VeryLowIrrigationUse
    0           'waterSupplyService:NoIrrigationUse))

;;Undiscretization of agricultural surface water use
(defmodel use-undiscretizer 'waterSupplyService:AgriculturalSurfaceWaterUseClass
  (classification 'waterSupplyService:AgriculturalSurfaceWaterUseClass 
    [2000 :>]    'waterSupplyService:HighAgriculturalSurfaceWaterUse
    [1000 2000]  'waterSupplyService:ModerateAgriculturalSurfaceWaterUse
    [:< 1000]    'waterSupplyService:LowAgriculturalSurfaceWaterUse)) 

(defmodel agricultural-surface-water-use 'waterSupplyService:AgriculturalSurfaceWaterUseClass
  (bayesian 'waterSupplyService:AgriculturalSurfaceWaterUseClass  
    :import   "aries.core::SurfaceWaterUseAgriculture.xdsl"
    :context  (surface-water-proximity livestock-total-water-use-discretized irrigation-water-use-discretized)
    :keep     ('waterSupplyService:AgriculturalSurfaceWaterUseClass)
    :observed (use-undiscretizer))) 

;;Below would be the logical and simple way to do things.  However these features are not yet enabled.

;;Agricultural surface water use. Step 5: Add crop irrigation and livestock surface water use.
 ;;(defmodel agricultural-surface-water-use 'waterSupplyService:AgriculturalSurfaceWaterUse
  ;;(measurement 'waterSupplyService:AgriculturalSurfaceWaterUse "mm"  ;;This is an annual value
    ;;  :context (irrigation-water-use livestock-surface-water-use)
    ;;  :state    #(+ (:irrigation-water-use %) (:livestock-surface-water-use %))))


;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;;Ad hoc sink model adapted from the ad hoc flood sink model.  Includes infiltration & evapotranspiration
;;processes.  Deterministic models could likely be used.

(defmodel slope 'waterSupplyService:SlopeClass
		(classification (measurement 'geophysics:DegreeSlope "\u00B0")
			 [0 1.15] 	            'waterSupplyService:Level
			 [1.15 4.57] 	          'waterSupplyService:GentlyUndulating
			 [4.57 16.70]           'waterSupplyService:RollingToHilly
			 [16.70 90 :inclusive] 	'waterSupplyService:SteeplyDissectedToMountainous))

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
	"Just a reclass of the Veracruz land use layer"
	(classification (categorization 'veracruz-lulc:VeracruzLULCCategory)
		"Bosque mesofilo de montana"	                                               'waterSupplyService:CloudForest
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

(defmodel dam-presence 'waterSupplyService:Dams
	(classification (binary-coding 'waterSupplyService:NonRivalWaterUseCode)
			1		      'waterSupplyService:DamPresent
     :otherwise 'waterSupplyService:DamAbsent))

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration, dams, etc.
(defmodel sink-undiscretizer 'waterSupplyService:SurfaceWaterSinkClass
  (classification 'waterSupplyService:SurfaceWaterSinkClass 
    [180 :>]           'waterSupplyService:VeryHighSurfaceWaterSink
    [100 180]          'waterSupplyService:HighSurfaceWaterSink
    [50 100]           'waterSupplyService:ModerateSurfaceWaterSink
    [:exclusive 0 50]  'waterSupplyService:LowSurfaceWaterSink
    0                  'waterSupplyService:NoSurfaceWaterSink)) 

(defmodel sink 'waterSupplyService:SurfaceWaterSinkClass
	  (bayesian 'waterSupplyService:SurfaceWaterSinkClass
	  	:import   "aries.core::SurfaceWaterSupplySink.xdsl"
	 	 	:context  (soil-group vegetation-type slope imperviousness dam-presence percent-vegetation-cover)
      :keep     ('waterSupplyService:SurfaceWaterSinkClass)
      :observed (sink-undiscretizer)))

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
(defmodel data 'waterSupplyService:WaterSupply 
  (identification 'waterSupplyService:WaterSupply 
  :context (irrigation-water-use
            agricultural-surface-water-use
            industrial-users
            non-rival-water-users
            sink
            precipitation-annual)))
  
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

