(ns aries.waterLaAntigua
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

;;Runoff would be preferable to precipitation, but it's at a very coarse spatial resolution
;; (i.e., <3 full pixels for the La Antigua watershed.  Use precip for now, with the goal of 
;; incorporating a better runoff model (plus sink models that actually capture infiltration & ET).
(defmodel precipitation-annual 'waterSupplyService:AnnualPrecipitation
  (measurement 'waterSupplyService:AnnualPrecipitation "mm"))

;;Incorporate runoff data in the future once we've done a better job with the hydro modeling.
;;(defmodel runoff 'soilretentionEcology:AnnualRunoff
	;;(classification (measurement 'soilretentionEcology:Runoff "mm/year")
		;;[0 200] 	    'soilretentionEcology:VeryLowAnnualRunoff
		;;[200 600] 	  'soilretentionEcology:LowAnnualRunoff
		;;[600 1200]  	'soilretentionEcology:ModerateAnnualRunoff
		;;[1200 2400] 	'soilretentionEcology:HighAnnualRunoff
		;;[2400 :>] 	  'soilretentionEcology:VeryHighAnnualRunoff))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;This is all we have to model industrial use right now: presence/absence of an industrial
;;user and whether they use ground or surface water. 
;;Current extraction is unknown.  Values below are 100% guesses - need information from local plants.
;;2 & 3 are zeros because they represent groundwater use, which we're not yet modeling.
(defmodel industrial-users 'waterSupplyService:IndustrialWaterUse
  (measurement 'waterSupplyService:IndustrialWaterUse "m^3" ;;This is an annual value.
    :state #(cond (== (:industrial-water-use %) 0) 10000  ;;Paper factory, using surface water
                  (== (:industrial-water-use %) 1) 20000  ;;Bottled water plant, using surface water
                  (== (:industrial-water-use %) 2)     0  ;;Nestle plant, using groundwater
                  (== (:industrial-water-use %) 3)     0  ;;Coca-Cola plant, using groundwater
                  :otherwise                           0)
    :context ((ranking 'waterSupplyService:IndustrialWaterUseClass :as industrial-water-use))))

;;This is all we have to model rafting and hydropower use right now: presence/absence of a user
;;user. It's a little strange to lump hydro and rafting together, but we'll
;;do it for now (ROWAN: is this correct?)
(defmodel non-rival-water-users 'waterSupplyService:NonRivalWaterUse
  (ranking 'waterSupplyService:NonRivalWaterUse
    :state #(cond (== (:non-rival-water-users %) 0) 1  ;;Rafting use
                  (== (:non-rival-water-users %) 1) 1  ;;Hydropower use
                  :otherwise                        0)
    :context ((ranking 'waterSupplyService:NonRivalWaterUseClass :as non-rival-water-users))))

;;The ranking model should really be a count with spatial ctx (data are persons/30 arc-second pixel)
;;The first example is for a probability distribution function (discrete values of the probability states)
;;The second example, used, is for a cumulative distribution function (ranges)
(defmodel residential-surface-water-use 'waterSupplyService:ResidentialSurfaceWaterUse
  (measurement 'waterSupplyService:ResidentialSurfaceWaterUse "m^3" ;;This is an annual value
    :context ((ranking 'waterSupplyService:PopulationDensity :as population-density))
;;  :state   #(rv-scalar-multiply {10 25/100, 20 50/100, 30 25/100} (* 0.8 (:population-density %))) 
    :state   #(rv-scalar-multiply {70.81 0, 78.84 25/100, 86.87 75/100, 94.9 1} (* 0.8 (:population-density %))))) 

;;Residential surface water use: currently only looking at surface water use.  

;;Top node discretization for water use is from Alberta: worth asking about better local sources.
;;www1.agric.gov.ab.ca/$department/deptdocs.nsf/all/agdex1349

(defmodel cattle-population 'waterSupplyService:CattlePopulation
	(classification (ranking 'waterSupplyService:CattlePopulation)
		[100 :>]    'waterSupplyService:HighCattlePopulation
		[35 100]    'waterSupplyService:ModerateCattlePopulation
		[:< 35]     'waterSupplyService:LowCattlePopulation))

(defmodel sheep-population 'waterSupplyService:SheepPopulation
	(classification (ranking 'waterSupplyService:SheepPopulation)
		[25 :>]    'waterSupplyService:HighSheepPopulation
		[8 25]    'waterSupplyService:ModerateSheepPopulation
		[:< 8]     'waterSupplyService:LowSheepPopulation))

(defmodel pigs-population 'waterSupplyService:PigsPopulation
	(classification (ranking 'waterSupplyService:PigsPopulation)
		[40 :>]    'waterSupplyService:HighPigsPopulation
		[15 40]    'waterSupplyService:ModeratePigsPopulation
		[:< 15]     'waterSupplyService:LowPigsPopulation))

(defmodel goats-population 'waterSupplyService:GoatsPopulation
	(classification (ranking 'waterSupplyService:GoatsPopulation)
		[20 :>]    'waterSupplyService:HighGoatsPopulation
		[5 20]    'waterSupplyService:ModerateGoatsPopulation
		[:< 5]     'waterSupplyService:LowGoatsPopulation))

(defmodel goats-population 'waterSupplyService:GoatsPopulation
	(classification (ranking 'waterSupplyService:GoatsPopulation)
		[20 :>]    'waterSupplyService:HighGoatsPopulation
		[5 20]    'waterSupplyService:ModerateGoatsPopulation
		[:< 5]     'waterSupplyService:LowGoatsPopulation))

(defmodel surface-water-proximity 'waterSupplyService:ProximityToSurfaceWater
	(classification (measurement 'waterSupplyService:ProximityToSurfaceWater "m")
		[500 :>]     'waterSupplyService:SurfaceWaterNotProximate
		[250 500]    'waterSupplyService:SurfaceWaterModeratelyProximate
		[:< 250]     'waterSupplyService:SurfaceWaterProximate))


;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;;Ad hoc sink model adapted from the ad hoc flood sink model.  Includes infiltration & evapotranspiration
;;processes.  Deterministic models could likely be used.

(defmodel slope 'waterSupplyService:Slope
		(classification (ranking 'geophysics:DegreeSlope)
			 [:< 1.15] 	  'waterSupplyService:Level
			 [1.15 4.57] 	'waterSupplyService:GentlyUndulating
			 [4.57 16.70] 'waterSupplyService:RollingToHilly
			 [16.70 :>] 	'waterSupplyService:SteeplyDissectedToMountainous))

(defmodel soil-group 'waterSupplyService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking 'waterSupplyService:HydrologicSoilsGroup)
			1       'waterSupplyService:SoilGroupA
			2       'waterSupplyService:SoilGroupB
			3       'waterSupplyService:SoilGroupC
			4       'waterSupplyService:SoilGroupD))

(defmodel imperviousness 'waterSupplyService:ImperviousSurfaceCover
	 (classification (ranking 'habitat:PercentImperviousness)
	 	   [80 100 :inclusive]   'waterSupplyService:VeryHighImpervious
	 	   [50 80]               'waterSupplyService:HighImpervious
	 	   [20 50]               'waterSupplyService:ModeratelyHighImpervious
	 	   [10 20]               'waterSupplyService:ModeratelyLowImpervious
	 	   [5 10]                'waterSupplyService:LowImpervious
	 	   [0 5]                 'waterSupplyService:VeryLowImpervious))

;;NOT NLCD - replace as appropriate.
(defmodel vegetation-type 'waterSupplyService:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		#{90 95}	         'floodService:WetlandVegetation
		#{41 42 43 52 71}  'floodService:ForestGrasslandShrublandVegetation
		#{21 22 23 24 82}	 'floodService:DevelopedCultivatedVegetation))
		
(defmodel percent-vegetation-cover 'waterSupplyService:PercentVegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 100] 'waterSupplyService:VeryHighVegetationCover
		[60 80]  'waterSupplyService:HighVegetationCover
		[40 60]  'waterSupplyService:ModerateVegetationCover
		[20 40]  'waterSupplyService:LowVegetationCover
		[0 20]   'waterSupplyService:VeryLowVegetationCover))
	 	   
;;This is actually in m^3 and should be a ranking but I'm getting error messages- 
;;"measurements can only be of physical properties: floodService:DamStorage" - so left as ranking for now
(defmodel dam-storage 'floodService:DamStorage
	(classification (ranking 'floodService:DamStorage)
			[6000000 :>]		  'floodService:VeryLargeDamStorage
			[3750000 6000000]	'floodService:LargeDamStorage
			[1750000 3750000]	'floodService:ModerateDamStorage
			[500000 1750000]	'floodService:SmallDamStorage
			[:< 400]		      'floodService:VerySmallDamStorage))
		
;; Flood sink probability
(defmodel sink 'waterSupplyService:WaterSupplySink
	  (bayesian 'waterSupplyService:WaterSupplySink 
	  	:import   "aries.core::WaterSupplySink.xdsl"
	  	:keep     (
	  			'floodService:FloodSink 
	  			'floodService:GreenInfrastructureStorage
	  			'floodService:GrayInfrastructureStorage)
	 	 	:context  (
	 	 			soil-group vegetation-type slope imperviousness dam-storage 
	 	 			(comment mean-days-precipitation vegetation-height)
	 	 			percent-vegetation-cover)))


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
 ;;(defmodel data 'aestheticService:AestheticEnjoyment 
	;;(identification 'aestheticService:AestheticEnjoyment)
		;;  :context (
		;;	source :as source
		;;	homeowners :as use
		;;	sink :as sink
		;;	altitude :as altitude))
			
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

