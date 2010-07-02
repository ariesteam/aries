(ns core.models.flood
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defagent defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; common models
;; ----------------------------------------------------------------------------------------------

(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	
	
(defmodel flow-direction 'geophysics:FlowDirection
	(ranking 'geophysics:FlowDirection)) 

(defmodel soil-group-puget 'floodService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking 'habitat:HydrologicSoilsGroup)
			1        'floodService:SoilGroupA
			2        'floodService:SoilGroupB
			3        'floodService:SoilGroupC
			4        'floodService:SoilGroupD))

(defmodel precipitation 'floodService:Precipitation
	(classification (measurement 'habitat:JanuaryPrecipitation "mm")
		[:< 75] 	  'floodService:VeryLowPrecipitation
		[75 150] 	  'floodService:LowPrecipitation
		[150 300] 	'floodService:ModeratePrecipitation
		[300 600] 	'floodService:HighPrecipitation
		[600 :>] 	  'floodService:VeryHighPrecipitation))
		

;; ----------------------------------------------------------------------------------------------
;; ad-hoc source models
;; ----------------------------------------------------------------------------------------------

(defmodel land-use 'floodService:LandUseLandCover
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
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
		(classification (measurement 'geophysics:JanuaryMeanGroundSurfaceTemperature "\u00b0C")
			 [4 :>] 	'floodService:HighTemperature
			 [-4 4] 	'floodService:ModerateTemperature
			 [:< -4] 	'floodService:LowTemperature))
			 
;; snow presence - only the puget-specific statement for now
(defmodel snow-presence 'floodService:SnowPresence
		(classification (categorization 'puget:SnowPrecipitationCategory)
			#{"LL" "HL"} 'floodService:LowlandAndHighland
			#{"RD" "SD"} 'floodService:RainDominatedAndSnowDominated
			"RS"         'floodService:PeakRainOnSnow))
	 	 	
(defmodel flood-source-value 'floodService:FloodSourceValue
	(classification 'floodService:FloodSourceValue
	  		[:< 200]    'floodService:VeryLowFloodSource
	  		[200 600]   'floodService:LowFloodSource
	  		[600 1200]  'floodService:ModerateFloodSource
	  		[1200 2400] 'floodService:HighFloodSource
	  		[2400 :>]   'floodService:VeryHighFloodSource))

;; Flood source probability, ad hoc method
(defmodel source 'floodService:FloodSource
	  (bayesian 'floodService:FloodSource 
	  	:import   "aries.core::FloodSourceValueAdHoc.xdsl"
	 	 	:context  (precipitation monthly-temperature snow-presence)
	 	 	:observed (flood-source-value)))

;; ----------------------------------------------------------------------------------------------
;; CN source model
;; ----------------------------------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
(defmodel source-cn 'floodService:FloodSource
	  (measurement 'habitat:AnnualRunoff "mm" 
	 	 	:context  (land-use :as landuse 
                 soil-group-puget :as soilgroup
                 (ranking 'habitat:PercentImperviousness) :as imperv
                 precipitation :as precipitation)
      :state #(let [
                    ctable 
                       {(tl/conc 'floodService:Agriculture) [64 75 82 85],
                        (tl/conc 'floodService:Forest) [64 75 82 85],
                        (tl/conc 'floodService:GrassPasture) [64 75 82 85],
                        (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85],
                        (tl/conc 'floodService:Agriculture) [64 75 82 85]}
                       ]
                )
))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel slope 'floodService:Slope
		(classification (measurement 'geophysics:DegreeSlope "\u00b0")
			 [:< 1.15] 	  'floodService:Level
			 [1.15 4.57] 	'floodService:GentlyUndulating
			 [4.57 16.70] 'floodService:RollingToHilly
			 [16.70 :>] 	'floodService:SteeplyDissectedToMountainous))
			 
(defmodel levees 'floodService:Levees
	"Presence of a levee in given context"
	(classification (binary-coding 'infrastructure:Levee)
			0 'floodService:LeveesNotPresent
			1 'floodService:LeveesPresent
;    :agent "aries/flood/levee"
))

;; Bridges removed - effects on flooding assumed to be minimal.

;;(defmodel bridges 'floodService:Bridges
;;	"Presence of a bridge in given context"
;;	(classification (binary-coding 'infrastructure:Bridge)
;;			0 'floodService:BridgesNotPresent
;;			1 'floodService:BridgesPresent
;;;   :agent "aries/flood/bridge"
;;))

(defmodel vegetation-type 'floodService:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		#{90 95}	         'floodService:WetlandVegetation
		#{41 42 43 52 71}  'floodService:ForestGrasslandShrublandVegetation
		#{21 22 23 24 82}	 'floodService:DevelopedCultivatedVegetation))

(defmodel vegetation-height 'floodService:VegetationHeight
	(classification (measurement 'habitat:VegetationHeight "ft")
		[120 :>] 'floodService:VeryHighVegetationHeight
		[80 120] 'floodService:HighVegetationHeight
		[50 80]  'floodService:ModerateVegetationHeight
		[20 50]  'floodService:LowVegetationHeight
		[:< 20]  'floodService:VeryLowVegetationHeight))
		
(defmodel percent-vegetation-cover 'floodService:PercentVegetationCover
	(classification (ranking 'habitat:PercentVegetationCover)
		[80 100] 'floodService:VeryHighVegetationCover
		[60 80] 'floodService:HighVegetationCover
		[40 60] 'floodService:ModerateVegetationCover
		[20 40] 'floodService:LowVegetationCover
		[0 20]  'floodService:VeryLowVegetationCover))

(defmodel floodplain-width 'floodService:FloodplainWidth
(classification (measurement 'habitat:FloodplainWidth "m")
    [400 :>]    'floodService:HighFloodplainWidth
    [200 400]   'floodService:ModerateFloodplainWidth
    [:< 200]    'floodService:LowFloodplainWidth
    :otherwise  'floodService:NoFloodplain))

(defmodel successional-stage 'floodService:SuccessionalStage
	 (classification (ranking 'ecology:SuccessionalStage)
	 		#{5 6}                          'floodService:OldGrowth
	 		4                               'floodService:LateSuccession
	 		3                               'floodService:MidSuccession
	 		2                               'floodService:PoleSuccession
	 		1                               'floodService:EarlySuccession
	 		#{22 23 24 25 26 27 28 40 41}   'floodService:NoSuccession))
	 		
(defmodel imperviousness 'floodService:PercentImperviousCover
	 (classification (ranking 'habitat:PercentImperviousness)
	 	   [80 100 :inclusive]   'floodService:VeryHighImperviousCover
	 	   [50 80]               'floodService:HighImperviousCover
	 	   [20 50]               'floodService:ModeratelyHighImperviousCover
	 	   [10 20]               'floodService:ModeratelyLowImperviousCover
	 	   [5 10]                'floodService:LowImperviousCover
	 	   [0 5]                 'floodService:VeryLowImperviousCover))
	 	   
;;This is actually in m^3 and should be a measurement but I'm getting error messages- 
;;"measurements can only be of physical properties: floodService:DamStorage" - so left as ranking for now
(defmodel dam-storage 'floodService:DamStorage
	(classification (ranking 'floodService:DamStorage)
			[6000000 :>]		  'floodService:VeryLargeDamStorage
			[3750000 6000000]	'floodService:LargeDamStorage
			[1750000 3750000]	'floodService:ModerateDamStorage
			[500000 1750000]	'floodService:SmallDamStorage
			[:< 400]		      'floodService:VerySmallDamStorage))
			
(defmodel mean-days-precipitation 'floodService:MeanDaysPrecipitationPerMonth
	(classification (ranking 'habitat:JanuaryDaysOfPrecipitation)
		#{8 9}    'floodService:VeryHighDaysPrecipitation
		#{6 7}    'floodService:HighDaysPrecipitation
		#{4 5}    'floodService:LowDaysPrecipitation
		#{1 2 3}  'floodService:VeryLowDaysPrecipitation))
		
(defmodel detention-basin-storage 'floodService:DetentionBasinStorage
	(classification (binary-coding 'infrastructure:DetentionBasin)
		0            'floodService:DetentionBasinStorageNotPresent
		:otherwise   'floodService:DetentionBasinStoragePresent))
		
;; Flood sink probability
;; TODO missing data
(defmodel sink 'floodService:FloodSink
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodSink 
	  	:import   "aries.core::FloodSink.xdsl"
	  	:keep     (
	  			'floodService:FloodSink 
	  			'floodService:GreenInfrastructureStorage
	  			'floodService:GrayInfrastructureStorage)
	 	 	:context  (
	 	 			soil-group-puget vegetation-type slope monthly-temperature levees 
	 	 			successional-stage imperviousness dam-storage floodplain-width
          (comment detention-basin-storage)
	 	 			(comment mean-days-precipitation vegetation-height)
	 	 			percent-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'floodService:Floodplains
	"Presence of a floodplain in given context"
	(classification (binary-coding 'geofeatures:Floodplain)
			0 'floodService:NotInFloodplain
			1 'floodService:InFloodplain))
			
(defmodel structures 'floodService:Structures
	"Assume that any privately owned land in floodplain has vulnerable structures. TODO make more specific when we know more"
	(classification (ranking 'lulc:PrivatelyOwnedLand)
			0 'floodService:StructuresNotPresent
			1 'floodService:StructuresPresent))
			
(defmodel housing 'floodService:PresenceOfHousing
	"Classifies land use from property data."
	; following sources are specific to Puget region, will not be used if data unavailable
	(classification (categorization 'puget:ParcelUseCategoryGraysHarbor)
		"RESIDENTIAL" 'floodService:HousingPresent
		:otherwise    'floodService:HousingNotPresent)
  (classification (categorization 'puget:ParcelUseCategoryKing)
		  #{"R" "K"}  'floodService:HousingPresent
		  :otherwise  'floodService:HousingNotPresent)
  ;; fall-back: if no data in the ones above, use NLCD high-intensity development category
	;; TODO check if that's ok
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		24	       'floodService:HousingPresent
		:otherwise 'floodService:HousingNotPresent)
	)
	
(defmodel public-asset 'floodService:PublicAsset
	"Public assets are defined as presence of highways, railways or both."
	(classification 'floodService:PublicAsset 
		:state   #(if (> (+ (:highway %) (:railway %)) 0) 
								(tl/conc 'floodService:PublicAssetPresent) 
								(tl/conc 'floodService:PublicAssetNotPresent))
		:context (
			(ranking 'infrastructure:Highway) :as highway
			(ranking 'infrastructure:Railway) :as railway)))

;; 
(defmodel farmland 'floodService:Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		82	       'floodService:FarmlandPresent
		:otherwise 'floodService:FarmlandAbsent
;    :agent     "aries/flood/farm"
    :editable  true))

;; Resident users in floodplains
(defmodel residents-use 'floodService:FloodResidentsUse
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodResidentsUse 
	  	:import   "aries.core::FloodResidentsUse.xdsl"
	  	:keep     ('floodService:ResidentsInFloodHazardZones)
	 	 	:context  (housing floodplains)))

;; Farmer users in floodplains
(defmodel farmers-use 'floodService:FloodFarmersUse
		"Interface to Flood farmers use bayesian network"
	  (bayesian 'floodService:FloodFarmersUse 
	  	:import   "aries.core::FloodFarmersUse.xdsl"
	  	:keep     ('floodService:FarmersInFloodHazardZones)
	 	 	:context  (farmland floodplains)))

;; Public assets in floodplains
(defmodel public-use 'floodService:FloodPublicAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPublicAssetsUse 
	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PublicAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (public-asset floodplains)))
	 	 	
;; Private assets in floodplains
(defmodel private-use 'floodService:FloodPrivateAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPrivateAssetsUse 
	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PrivateAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (structures floodplains)))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data-farmers 'floodService:AvoidedDamageToFarms 
	(identification 'floodService:AvoidedDamageToFarms 
		:context (
			source :as source
			sink :as sink
			farmers-use :as use)))

(defmodel data-public 'floodService:AvoidedDamageToPublicAssets 
	(identification 'floodService:AvoidedDamageToPublicAssets 
		:context (
			source :as source
			sink :as sink
			public-use :as use)))

(defmodel data-private 'floodService:AvoidedDamageToPrivateAssets 
	(identification 'floodService:AvoidedDamageToPrivateAssets 
		:context (
			source :as source
			sink :as sink
			private-use :as use)))

(defmodel data-residents 'floodService:AvoidedDamageToResidents 
	(identification 'floodService:AvoidedDamageToResidents 
		:context (
			source :as source
			sink :as sink
			residents-use :as use)))

;; flow model	for farmers		
(defmodel flood-regulation-farmers 'floodService:AvoidedDamageToFarms
  (span 'floodService:FarmlandFlooding 
  	    'floodService:FloodSourceValue
  	    'floodService:FarmersInFloodHazardZones
      	'floodService:FloodSink
      	'floodService:WaterMovement
  	    'geophysics:Altitude
  	:source-threshold 10,
   	:sink-threshold   0.3,
   	:use-threshold    0.3,
   	:trans-threshold  1.0,
   	:sink-type        :relative,
   	:use-type         :relative,
   	:benefit-type     :non-rival,
   	:downscaling-factor 3,
   	:rv-max-states    10 
    :context (source farmers-use sink altitude)))

