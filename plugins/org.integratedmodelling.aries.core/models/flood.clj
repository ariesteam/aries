(ns aries/flood
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; ----------------------------------------------------------------------------------------------
;; common models
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'floodService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (categorization 'floodService:HydrologicSoilsGroup)
			"A"        'floodService:SoilGroupA
			"B"        'floodService:SoilGroupB
			"C"        'floodService:SoilGroupC
			"D"        'floodService:SoilGroupD))

(defmodel precipitation 'floodService:Precipitation
	"FIXME this is total monthly precipitation I believe."
	(classification (measurement 'habitat:Precipitation "in")
		[:< 3] 	  'floodService:VeryLowPrecipitation
		[3 6] 	  'floodService:LowPrecipitation
		[6 12] 	  'floodService:ModeratePrecipitation
		[12 24] 	'floodService:HighPrecipitation
		[24 :>] 	'floodService:VeryHighPrecipitation))
		
;; ----------------------------------------------------------------------------------------------
;; source models
;; ----------------------------------------------------------------------------------------------

(defmodel land-use 'floodService:LandUseLandCover
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	               'floodService:Agriculture
		#{11 90 95 12}	   'floodService:WetlandsOpenWater
		21	               'floodService:DevelopedOpenSpace
		#{41 42 43}        'floodService:Forest
		#{71 81 52}	       'floodService:GrassPasture
		#{22	31}          'floodService:DevelopedLowIntensity
		23	               'floodService:DevelopedMediumIntensity
		24	               'floodService:DevelopedHighIntensity))
		
;; surface temperature - again, should be monthly and matched by temporal extents.
(defmodel monthly-temperature 'floodService:MonthlyTemperature
		(classification (ranking 'geophysics:GroundSurfaceTemperature "C")
			 [4 :>] 	'floodService:HighTemperature
			 [-4 4] 	'floodService:ModerateTemperature
			 [:< -4] 	'floodService:LowTemperature))
			 
;; snow presence - only the puget-specific statement for now
(defmodel snow-presence 'floodService:SnowPresence
		(classification (categorization 'puget:SnowPrecipitationCategory)
			#{"LL" "HL"} 'floodService:LowlandAndHighland
			#{"RD" "SD"} 'floodService:RainDominatedAndSnowDominated
			"RS"         'floodService:PeakRainOnSnow))

;; Flood source probability (runoff levels), SCS curve number method
;; See: http://www.ecn.purdue.edu/runoff/documentation/scs.htm
;; TODO it's deterministic, remove the BN and program this one in code
(defmodel source-cn 'floodService:FloodSourceCurveNumberMethod
	  (bayesian 'floodService:FloodSourceCurveNumberMethod)
	  	:import   "aries.core::FloodSourceValueCurveNumber.xdsl"
	  	:keep     ('floodService:Runoff)
	 	 	:context  (land-use soil-group precipitation))

;; Flood source probability, ad hoc method
(defmodel source 'floodService:FloodSource
	  (bayesian 'floodService:FloodSource
			;; model for the main observable
			;; TODO this should classify a measurement of runoff, not just the concept
	  	(classification 'floodService:FloodSourceValue
	  		[:< 200]    'floodService:VeryLowFloodSource
	  		[200 600]   'floodService:LowFloodSource
	  		[600 1200]  'floodService:ModerateFloodSource
	  		[1200 2400] 'floodService:HighFloodSource
	  		[2400 :>]   'floodService:VeryHighFloodSource))
	  	:import   "aries.core::FloodSourceValueAdHoc.xdsl"
	 	 	:context  (precipitation monthly-temperature snow-presence))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel slope 'floodService:Slope
		(classification (ranking 'geophysics:DegreeSlope "°")
			 [:< 1.15] 	  'floodService:Level
			 [1.15 4.57] 	'floodService:GentlyUndulating
			 [4.57 16.70] 'floodService:RollingToHilly
			 [16.70 :>] 	'floodService:SteeplyDissectedToMountainous))
			 
(defmodel levees 'floodService:Levees
	"Presence of a levee in given context"
	(classification (ranking 'infrastructure:Levee)
			0 'floodService:LeveesNotPresent
			1 'floodService:LeveesPresent))

(defmodel bridges 'floodService:Bridges
	"Presence of a bridge in given context"
	(classification (ranking 'infrastructure:Bridge)
			0 'floodService:BridgesNotPresent
			1 'floodService:BridgesPresent))

(defmodel vegetation-type 'floodService:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		#{90 95}	         'floodService:WetlandVegetation
		#{41 42 43 52 71}  'floodService:ForestGrasslandShrublandVegetation
		#{21 22 23 24 82}	 'floodService:DevelopedCultivatedVegetation))

(defmodel vegetation-height 'floodService:VegetationHeight
	(classification (measurement 'habitat:VegetationHeight "ft")
;   CHECK -- BN only has high, moderate, low, very low 
;		[120 :>] 'floodService:VeryHighVegetationHeight
		[80 :>] 'floodService:HighVegetationHeight
		[50 80]  'floodService:ModerateVegetationHeight
		[20 50]  'floodService:LowVegetationHeight
		[:< 20]  'floodService:VeryLowVegetationHeight))
		
(defmodel vegetation-cover 'floodService:VegetationCover
	(classification (ranking 'habitat:PercentCanopyCover)
		[80 :>] 'floodService:VeryHighVegetationCover
		[60 80] 'floodService:HighVegetationCover
		[40 60] 'floodService:ModerateVegetationCover
		[20 40] 'floodService:LowVegetationCover
		[0 20]  'floodService:VeryLowVegetationCover))
		
(defmodel successional-stage 'floodService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6}      'floodService:OldGrowth
	 		4           'floodService:LateSuccession
	 		3           'floodService:MidSuccession
	 		2           'floodService:EarlySuccession
	 		1           'floodService:PoleSuccession
	 		:otherwise  'floodService:NoSuccession))
	 		
(defmodel imperviousness 'floodService:ImperviousSurfaceCover
	 (classification (ranking 'habitat:PercentImperviousness)
	 	   [80 100 :inclusive]   'floodService:VeryHighlyImpervious
	 	   [50 80]   'floodService:HighlyImpervious
	 	   [20 50]   'floodService:ModeratelyHighlyImpervious
	 	   [10 20]   'floodService:ModeratelyLowImpervious
	 	   [5 10]     'floodService:LowImpervious
	 	   [0 5]    'floodService:VeryLowImpervious))
	 	   
(defmodel dam-storage 'floodService:DamStorage
	(classification (ranking 'floodService:DamStorage)
			[5000 :>]		'floodService:VeryLargeDamStorage
			[3126 5000]	'floodService:LargeDamStorage
			[1600 3126]	'floodService:ModerateDamStorage
			[400 1600]	'floodService:SmallDamStorage
			[:< 400]		'floodService:VerySmallDamStorage))
			
(defmodel mean-days-precipitation 'floodService:MeanDaysOfPrecipitation
	(classification (ranking 'puget:DaysOfPrecipitationGridcode)
		#{8 9}    'floodService:VeryHighDaysPrecipitation
		#{6 7}    'floodService:HighDaysPrecipitation
		#{4 5}    'floodService:LowDaysPrecipitation
		#{1 2 3}  'floodService:VeryLowDaysPrecipitation))
		
(defmodel detention-basin-storage 'floodService:DetentionBasinStorage
	(classification (ranking 'infrastructure:DetentionBasin)
		0            'floodService:DetentionBasinStorageNotPresent
		:otherwise   'floodService:DetentionBasinStoragePresent))
		
;; Flood sink probability
(defmodel sink 'floodService:FloodSink
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodSink)
	  	:import   "aries.core::FloodSink.xdsl"
	  	:keep     (
	  			'floodService:FloodSink 
	  			'floodService:GreenInfrastructureStorage
	  			'floodService:GrayInfrastructureStorage)
	 	 	:context  (
	 	 			soil-group vegetation-type slope monthly-temperature 
	 	 			successional-stage imperviousness dam-storage mean-days-precipitation
	 	 			detention-basin-storage (comment vegetation-height) vegetation-cover))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'floodService:Floodplains
	"Presence of a floodplain in given context"
	(classification (ranking 'floodService:Floodplains)
			0 'floodService:NotInFloodplain
			1 'floodService:InFloodplain))
			
(defmodel structures 'floodService:Structures
	"Assume that any privately owned land in floodplain has vulnerable structures. TODO make more specific when we know more"
	(classification (ranking 'lulc:PrivatelyOwnedLand)
			0 'floodService:StructuresNotPresent
			1 'floodService:StructuresPresent))
			
(defmodel housing 'floodService:PresenceOfHousing
	"Classifies land use from property data."
	; specific to Puget region, will not be used if data unavailable
	(classification (categorization 'puget:ParcelUseCategoryGraysHarbor)
		"RESIDENTIAL" 'floodService:HousingPresent
		:otherwise    'floodService:HousingNotPresent)
	;; TODO add generalized fall-back definitions using NCLD and/or other global lu/lc data
	)
	
(defmodel public-asset 'floodService:PublicAsset
	"Public assets are defined as presence of highways, railways or both."
	(classification 'floodService:PublicAsset)
		:state   (if (> (+ highway railway) 0) 
								(tl/conc 'floodService:PublicAssetPresent) 
								(tl/conc 'floodService:PublicAssetNotPresent))
		:context (
			(ranking 'infrastructure:Highway) :as highway
			(ranking 'infrastructure:Railway) :as railway))

;; 
(defmodel farmland 'floodService:Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	       'floodService:FarmlandPresent
		:otherwise 'floodService:FarmlandNotPresent))

;; Resident users in floodplains
(defmodel residents-use 'floodService:FloodResidentsUse
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodResidentsUse)
	  	:import   "aries.core::FloodResidentsUse.xdsl"
	  	:keep     ('floodService:ResidentsInFloodHazardZones)
	 	 	:context  (housing floodplains))

;; Farmer users in floodplains
(defmodel farmers-use 'floodService:FloodFarmersUse
		"Interface to Flood farmers use bayesian network"
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "aries.core::FloodFarmersUse.xdsl"
	  	:keep     ('floodService:FarmersInFloodHazardZones)
	 	 	:context  (farmland floodplains))

;; Public assets in floodplains
(defmodel public-use 'floodService:FloodPublicAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPublicAssetsUse)
	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PublicAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (public-asset floodplains))
	 	 	
;; Private assets in floodplains
(defmodel private-use 'floodService:FloodPrivateAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPrivateAssetsUse)
	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PrivateAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (structures floodplains))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'floodService:FloodPreventionService 
	(identification 'floodService:FloodPreventionService)
		:context (
			source :as source
			sink :as sink
			residents-use :as residents))
	 	 	