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
;;user and whether they use ground or surface water. (ROWAN: is this correct?)
(defmodel industrial-users 'waterSupplyService:IndustrialWaterUse
  (classification (ranking 'waterSupplyService:IndustrialWaterUse)
     #{0 1} 'waterSupplyService:IndustrialSurfaceWaterUse
     #{2 3} 'waterSupplyService:IndustrialGroundwaterUse))

;;This is all we have to model rafting and hydropower use right now: presence/absence of a user
;;user. It's a little strange to lump hydro and rafting together, but we'll
;;do it for now (ROWAN: is this correct?)
(defmodel non-rival-water-users 'waterSupplyService:NonRivalWaterUse
  (classification (ranking 'waterSupplyService:NonRivalWaterUse)
     0 'waterSupplyService:RaftingUse
     1 'waterSupplyService:HydropowerUse))

;; the ranking model should really be a count with spatial ctx (data are persons/30 arc-second pixel)
(defmodel population-density 'waterSupplyService:PopulationDensity
	(classification (ranking 'policytarget:PopulationDensity)
		[10000 :>]    'waterSupplyService:VeryHighPopulationDensity
		[4000 10000]  'waterSupplyService:HighPopulationDensity
		[1500 4000]   'waterSupplyService:ModeratePopulationDensity
		[400 1500]    'waterSupplyService:LowPopulationDensity
		[:< 400]      'waterSupplyService:VeryLowPopulationDensity))

;;Check with Rowan on top node discretization for residential water use: how confident are you 
;; that you can apply these values (from Alberta) to Veracruz?


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

;;At this point we're naively saying there are no sinks for suface water.  This model should
;;be defined and implemented in the future to incorporate infiltration, evapotranspiration, etc.

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

