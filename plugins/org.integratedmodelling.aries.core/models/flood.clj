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

(defmodel streams 'geofeatures:River
  (binary-coding 'geofeatures:River))

(defmodel soil-group-puget 'floodService:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking 'habitat:HydrologicSoilsGroup)
      1        'floodService:SoilGroupA
      2        'floodService:SoilGroupB
      3        'floodService:SoilGroupC
      4        'floodService:SoilGroupD))

;; ----------------------------------------------------------------------------------------------
;; ad-hoc source models
;; ----------------------------------------------------------------------------------------------

(defmodel precipitation-monthly 'floodService:Precipitation
  (measurement 'habitat:JanuaryPrecipitation "mm"))

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
		
;; Surface temperature - again, should be monthly and matched by temporal extents.  For mean temperature
;;  could use a slightly lower discretization, i.e., < -4, -4 to 4, >4
(defmodel monthly-temperature 'floodService:MonthlyMaximumTemperature
		(classification (measurement 'geophysics:JanuaryMaximumGroundSurfaceTemperature "\u00b0C")
			 [6 :>] 	'floodService:HighMonthlyMaximumTemperature
			 [0 6] 	  'floodService:ModerateMonthlyMaximumTemperature
			 [:< 0] 	'floodService:LowMonthlyMaximumTemperature))

(defmodel annual-temperature 'floodService:AnnualMaximumTemperature
    (classification (measurement 'geophysics:AnnualMaximumGroundSurfaceTemperature "\u00b0C")
       [6 :>]   'floodService:HighAnnualMaximumTemperature
       [0 6]    'floodService:ModerateAnnualMaximumTemperature
       [:< 0]   'floodService:LowAnnualMaximumTemperature)) 
			 
;; Snow presence - only the puget-specific statement for now.  This is not currently part of any
;; model but could be incorporated in the future.
(defmodel snow-presence 'floodService:SnowPresence
		(classification (categorization 'puget:SnowPrecipitationCategory)
			#{"LL" "HL"} 'floodService:LowlandAndHighland
			#{"RD" "SD"} 'floodService:RainDominatedAndSnowDominated
			"RS"         'floodService:PeakRainOnSnow))

;;These bins are producing uniform snowmelt results everywhere - consider altering if these data are 
;; actually used in another model statement.
(defmodel snowmelt-annual 'floodService:AnnualSnowmelt
    (classification (measurement 'habitat:AnnualSnowmelt "mm")
        [700 :>]             'floodService:HighAnnualSnowmelt
        [250 700]            'floodService:ModerateAnnualSnowmelt
        [:exclusive 0 250]   'floodService:LowAnnualSnowmelt
        [0]                  'floodService:NoAnnualSnowmelt))

(defmodel snowmelt-monthly 'floodService:MonthlySnowmelt
    (measurement 'habitat:JanuarySnowmelt "mm"))

;;Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel runoff-training 'floodService:FloodSourceValue
  (classification (measurement 'habitat:AnnualRunoff "mm")
                  [:< 200]    'floodService:VeryLowFloodSource
                  [200 600]   'floodService:LowFloodSource
                  [600 1200]  'floodService:ModerateFloodSource
                  [1200 2400] 'floodService:HighFloodSource
                  [2400 :>]   'floodService:VeryHighFloodSource))

;;Monthly source data is just the sum of precipitation and snowmelt.
(defmodel source-monthly 'floodService:FloodSourceMonthly
   (measurement 'floodService:FloodSourceMonthly "mm"
        :context (precipitation-monthly snowmelt-monthly)
        :state   #(+ (:precipitation-monthly %) (:snowmelt-monthly %)))) 

;;Annual source data is simply precipitation-annual (assume all snow melts in each year, which
;; is true everywhere but for glaciers.  Assume that glaciers are neither gaining nor losing
;; mass, which is not true but a simplifying assumption for now.
(defmodel source-annual 'floodService:Precipitation
    (measurement 'habitat:AnnualPrecipitation "mm"))

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
                 precipitation-monthly :as precipitation)
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

(defmodel evapotranspiration 'floodService:EvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [90 :>]    'floodService:VeryHighEvapotranspiration
                  [60 90]    'floodService:HighEvapotranspiration
                  [30 60]    'floodService:ModerateEvapotranspiration
                  [12 30]    'floodService:LowEvapotranspiration
                  [0 12]     'floodService:VeryLowEvapotranspiration))

(defmodel dam-storage 'floodService:DamStorageClass
  (classification (measurement 'floodService:DamStorage "mm")
                  [30000 :>]      'floodService:VeryLargeDamStorage
                  [9000 30000]    'floodService:LargeDamStorage
                  [3000 9000]     'floodService:ModerateDamStorage
                  [900 3000]      'floodService:SmallDamStorage
                  [0 900]         'floodService:VerySmallDamStorage
                   :otherwise     'floodService:NoDamStorage))
			
(defmodel mean-days-precipitation-monthly 'floodService:MeanDaysPrecipitationPerMonth
	(classification (ranking 'habitat:JanuaryDaysOfPrecipitation)
		#{8 9}    'floodService:VeryHighDaysPrecipitationPerMonth
		#{6 7}    'floodService:HighDaysPrecipitationPerMonth
		#{4 5}    'floodService:LowDaysPrecipitationPerMonth
		#{1 2 3}  'floodService:VeryLowDaysPrecipitationPerMonth))

(defmodel mean-days-precipitation-annual 'floodService:MeanDaysPrecipitationPerYear
  (classification (ranking 'habitat:AnnualDaysOfPrecipitation)
    #{8 9}    'floodService:VeryHighDaysPrecipitationPerYear
    #{6 7}    'floodService:HighDaysPrecipitationPerYear
    #{4 5}    'floodService:LowDaysPrecipitationPerYear
    #{1 2 3}  'floodService:VeryLowDaysPrecipitationPerYear))

;;Assumes that detention basins average 3 m, i.e., 3000 mm, in depth, i.e., storage capacity when
;;  empty.  Can alter this as appropriate.
(defmodel detention-basin-storage 'floodService:DetentionBasinStorage
	(measurement 'floodService:DetentionBasinStorage "mm" 
    :context ((binary-coding 'infrastructure:DetentionBasin :as detention-basin-storage))
    :state #(cond (== (:detention-basin-storage %) 0) 0
                  (== (:detention-basin-storage %) 1) 3000)))

;;Undiscretizer for FloodSink
(defmodel flood-sink 'floodService:FloodSink
  (classification 'floodService:FloodSink
                  :units      "mm" 
                  [30000 90000]     'floodService:VeryHighFloodSink
                  [10000 30000]     'floodService:HighFloodSink
                  [3000 10000]      'floodService:ModerateFloodSink
                  [900 3000]        'floodService:LowFloodSink
                  [0 900]           'floodService:VeryLowFloodSink))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage 'floodService:GreenInfrastructureStorage
  (classification 'floodService:GreenInfrastructureStorage
                  :units      "mm" 
                  [115 320]    'floodService:VeryHighGreenStorage
                  [72 115]     'floodService:HighGreenStorage
                  [40 72]      'floodService:ModerateGreenStorage
                  [15 40]      'floodService:LowGreenStorage
                  [0 15]       'floodService:VeryLowGreenStorage))

;;Undiscretizer for GrayInfrastructureStorage
(defmodel gray-infrastructure-storage 'floodService:GrayInfrastructureStorage
  (classification 'floodService:GrayInfrastructureStorage
                  :units      "mm" 
                  [30000 90000]     'floodService:VeryHighGrayStorage
                  [10000 30000]     'floodService:HighGrayStorage
                  [3000 10000]      'floodService:ModerateGrayStorage
                  [900 3000]        'floodService:LowGrayStorage
                  [0 900]           'floodService:VeryLowGrayStorage))

;; Flood sink probability, monthly
(defmodel sink-monthly 'floodService:MonthlyFloodSink
	  (bayesian 'floodService:MonthlyFloodSink
	  	:import   "aries.core::FloodSinkMonthly.xdsl"
	  	:keep     (
	  			'floodService:FloodSink 
	  			'floodService:GreenInfrastructureStorage
	  			'floodService:GrayInfrastructureStorage)
	 	 	:context  (
	 	 			soil-group-puget vegetation-type slope monthly-temperature  
	 	 			successional-stage imperviousness dam-storage 
          (comment detention-basin-storage) ;;Why is this commented out?
	 	 			(comment mean-days-precipitation-monthly vegetation-height)
	 	 			percent-vegetation-cover)))

;; Flood sink probability, annual
(defmodel sink-annual 'floodService:AnnualFloodSink
    (bayesian 'floodService:AnnualFloodSink
      :import   "aries.core::FloodSinkAnnual.xdsl"
      :keep   (
              'floodService:FloodSink 
              'floodService:GreenInfrastructureStorage 
              'floodService:GrayInfrastructureStorage) 
      :context (soil-group-puget vegetation-type slope annual-temperature  
          successional-stage imperviousness dam-storage 
          (comment detention-basin-storage) ;;Why is this commented out?
          (comment mean-days-precipitation-annual vegetation-height)
          percent-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

;;(defmodel floodplains 'floodService:Floodplains
;;	"Presence of a floodplain in given context"
;;	(classification (binary-coding 'geofeatures:Floodplain)
;;			0 'floodService:NotInFloodplain
;;			1 'floodService:InFloodplain))

(defmodel floodplains-100 'floodService:Floodplains100
  (classification (categorization 'geofeatures:Floodplain)
                  "A"            'floodService:In100YrFloodplain
                  :otherwise     'floodService:NotIn100YrFloodplain))

(defmodel floodplains-500 'floodService:Floodplains500
  (classification (categorization 'geofeatures:Floodplain)
                  #{"A" "X500"}      'floodService:In500YrFloodplain
                  :otherwise         'floodService:NotIn500YrFloodplain))

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
		:otherwise 'floodService:HousingNotPresent))
	
(defmodel public-asset 'floodService:PublicAsset
	"Public assets are defined as presence of highways, railways or both."
	(classification 'floodService:PublicAsset 
		:state   #(if (> (+ (:highway %) (:railway %)) 0) 
								(tl/conc 'floodService:PublicAssetPresent) 
								(tl/conc 'floodService:PublicAssetNotPresent))
		:context (
			(ranking 'infrastructure:Highway) :as highway
			(ranking 'infrastructure:Railway) :as railway)))

(defmodel farmland 'floodService:Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		82	       'floodService:FarmlandPresent
		:otherwise 'floodService:FarmlandAbsent
;    :agent     "aries/flood/farm"
    :editable  true))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-use-100 'floodService:FloodFarmersUse100
  (binary-coding 'floodService:FloodFarmersUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains-100 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-100)))

(defmodel farmers-use-500 'floodService:FloodFarmersUse500
  (binary-coding 'floodService:FloodFarmersUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains-500 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-500)))

;; Models public infrastructure in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel public-use-100 'floodService:FloodPublicAssetsUse100
  (binary-coding 'floodService:FloodPublicAssetsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains-100 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
                    1
                    0)
       :context  (public-asset floodplains-100)))

(defmodel public-use-500 'floodService:FloodPublicAssetsUse500
  (binary-coding 'floodService:FloodPublicAssetsUse500
       :state #(do (println "Floodplains-500:" (:floodplains-500 %))
                   (println "Public Asset:"    (:public-asset %)) 
                   (if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains-500 %))
                            (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
                        1
                        0))
       :context  (public-asset floodplains-500)))

;; Models housing in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel residents-use-100 'floodService:FloodResidentsUse100
  (binary-coding 'floodService:FloodResidentsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains-100 %))
                        (= (tl/conc 'floodService:HousingPresent)      (:housing %)))
                    1
                    0)
       :context (housing floodplains-100)))

(defmodel residents-use-500 'floodService:FloodResidentsUse500
  (binary-coding 'floodService:FloodResidentsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains-500 %))
                        (= (tl/conc 'floodService:HousingPresent)      (:housing %)))
                    1
                    0)
       :context (housing floodplains-500)))

;; Models other private structures in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel private-use-100 'floodService:FloodPrivateAssetsUse100
  (binary-coding 'floodService:FloodPrivateAssetsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains-100 %))
                        (= (tl/conc 'floodService:StructuresPresent)      (:structures %)))
                    1
                    0)
       :context (structures floodplains-100)))

(defmodel private-use-500 'floodService:FloodPrivateAssetsUse500
  (binary-coding 'floodService:FloodPrivateAssetsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)      (:floodplains-500 %))
                        (= (tl/conc 'floodService:StructuresPresent)      (:structures %)))
                    1
                    0)
       :context (structures floodplains-500)))

;;Old Bayesian models - delete once you know others work
;; Resident users in floodplains
;;(defmodel residents-use 'floodService:FloodResidentsUse
;;		"Interface to Flood resident use bayesian network"
;;	  (bayesian 'floodService:FloodResidentsUse 
;;	  	:import   "aries.core::FloodResidentsUse.xdsl"
;;	  	:keep     ('floodService:ResidentsInFloodHazardZones)
;;	 	 	:context  (housing floodplains-100)))

;; Farmer users in floodplains
;;(defmodel farmers-use 'floodService:FloodFarmersUse
;;		"Interface to Flood farmers use bayesian network"
;;	  (bayesian 'floodService:FloodFarmersUse 
;;	  	:import   "aries.core::FloodFarmersUse.xdsl"
;;	  	:keep     ('floodService:FarmersInFloodHazardZones)
;;	 	 	:context  (farmland floodplains-100)))

;; Public assets in floodplains
;;(defmodel public-use 'floodService:FloodPublicAssetsUse
;;  	"Interface to Flood public asset use bayesian network"
;;	  (bayesian 'floodService:FloodPublicAssetsUse 
;;	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
;;	  	:keep     ('floodService:PublicAssetOwnersAndUsersInFloodHazardZones)
;;	 	 	:context  (public-asset floodplains-100)))
	 	 	
;; Private assets in floodplains
;;(defmodel private-use 'floodService:FloodPrivateAssetsUse100
;; 	"Interface to Flood public asset use bayesian network"
;;	  (bayesian 'floodService:FloodPrivateAssetsUse100
;;	  	:import   "aries.core::FloodPublicAssetsUse.xdsl"
;;	  	:keep     ('floodService:PrivateAssetOwnersAndUsersInFloodHazardZones)
;;	 	 	:context  (structures floodplains-100)))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage.  These are currently set to "source-annual" but should also be
;; tested for "source-monthly"
(defmodel data-farmers-100 'floodService:AvoidedDamageToFarms100 
	(identification 'floodService:AvoidedDamageToFarms100 
		:context (
			source-annual :as source
			sink-annual :as sink
			farmers-use-100 :as use)))

(defmodel data-farmers-500 'floodService:AvoidedDamageToFarms500 
  (identification 'floodService:AvoidedDamageToFarms500 
    :context (
      source-annual :as source
      sink-annual :as sink
      farmers-use-500 :as use)))

(defmodel data-public-100 'floodService:AvoidedDamageToPublicAssets100 
	(identification 'floodService:AvoidedDamageToPublicAssets100 
		:context (
			source-annual :as source
			sink-annual :as sink
			public-use-100 :as use)))

(defmodel data-public-500 'floodService:AvoidedDamageToPublicAssets500 
  (identification 'floodService:AvoidedDamageToPublicAssets500 
    :context (
      source-annual :as source
      sink-annual :as sink
      public-use-500 :as use)))

(defmodel data-private-100 'floodService:AvoidedDamageToPrivateAssets100 
	(identification 'floodService:AvoidedDamageToPrivateAssets100 
		:context (
			source-annual :as source
			sink-annual :as sink
			private-use-100 :as use)))

(defmodel data-private-500 'floodService:AvoidedDamageToPrivateAssets500 
  (identification 'floodService:AvoidedDamageToPrivateAssets500 
    :context (
      source-annual :as source
      sink-annual :as sink
      private-use-500 :as use)))

(defmodel data-residents-100 'floodService:AvoidedDamageToResidents100 
	(identification 'floodService:AvoidedDamageToResidents100 
		:context (
			source-annual :as source
			sink-annual :as sink
			residents-use-100 :as use)))

(defmodel data-residents-500 'floodService:AvoidedDamageToResidents500 
  (identification 'floodService:AvoidedDamageToResidents500 
    :context (
      source-annual :as source
      sink-annual :as sink
      residents-use-500 :as use)))

(defmodel flood-flow-data100 'floodService:TempFloodData100$
  (identification 'floodService:TempFloodData100
    :context (altitude streams floodplains-100))) 

(defmodel flood-flow-data500 'floodService:TempFloodData500$
  (identification 'floodService:TempFloodData500
    :context (altitude streams floodplains-500))) 

;; Flow model	for farmers	in the 100-yr floodplain at annual time step. 
;;  Build the others when this is shown to work for 500-yr floodplain, other beneficiary groups, monthly models.
(defmodel flood-regulation-farmers100 'floodService:AvoidedDamageToFarms100
  (span 'floodService:FloodWaterMovement
  	    'floodService:FloodSourceValue
  	    'floodService:FloodFarmersUse100
      	'floodService:FloodSink
      	nil
  	    'floodService:TempFloodData100
  	:source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
    :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
    :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
    :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
   	:source-type        :finite
    :sink-type          :finite
   	:use-type           :infinite
   	:benefit-type       :non-rival
   	:downscaling-factor 8
   	:rv-max-states      10 
    ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
    :context (source-annual farmers-use-100 sink-annual flood-flow-data100)))

;;Levees and floodplain width: used in the flow model
(defmodel levees 'floodService:Levees
  "Presence of a levee in given context"
  (classification (binary-coding 'infrastructure:Levee)
      0 'floodService:LeveesNotPresent
      1 'floodService:LeveesPresent
;    :agent "aries/flood/levee"
))

(defmodel floodplain-width 'floodService:FloodplainWidth
(classification (measurement 'habitat:FloodplainWidth "m")
    [400 :>]    'floodService:HighFloodplainWidth
    [200 400]   'floodService:ModerateFloodplainWidth
    [:< 200]    'floodService:LowFloodplainWidth
    :otherwise  'floodService:NoFloodplainWidth))
