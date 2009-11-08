(ns aries/flood
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

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

;(defmodel land-use 'floodService:Precipitation
;	"Reclass rainfall for now. Should be all precipitation. NOTE: this is annual by default? 
;	 Simply precipitation should be abstract or we should use temporal extents too"
;	(classification (measurement 'habitat:Rainfall "mm")
;		[:< 3] 	  'floodService:VeryLowPrecipitation
;		[3 6] 	  'floodService:LowPrecipitation
;		[6 12] 	  'floodService:ModeratePrecipitation
;		[12 24] 	'floodService:HighPrecipitation
;		[24 :>] 	'floodService:VeryHighPrecipitation))

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

(defmodel soil-group 'floodService:HydrologicSoilsGroup
	"Relevant soil group"
	(classification (categorization 'floodService:HydrologicSoilsGroup)
			"A"        'floodService:SoilGroupA
			"B"        'floodService:SoilGroupB
			"C"        'floodService:SoilGroupC
			"D"        'floodService:SoilGroupD))

(defmodel vegetation-type 'floodService:VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		#{90 95}	         'floodService:WetlandVegetation
		#{41 42 43 52 71}  'floodService:ForestGrasslandShrublandVegetation
		#{21 22 23 24 82}	 'floodService:DevelopedCultivatedVegetation))

;; Flood sink probability
(defmodel sink 'floodService:FloodSink
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodSink)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodSink.xdsl"
	  	:keep     (
	  			'floodService:FloodSink 
	  			'floodService:GreenInfrastructureStorage
	  			'floodService:GrayInfrastructureStorage)
	 	 	:context  (soil-group vegetation-type slope))

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
	"Classifies land use from property data. TODO must reconceptualize the attribute"
	(classification (categorization 'floodService:PresenceOfHousing)
		"RESIDENTIAL" 'floodService:HousingPresent
		:otherwise    'floodService:HousingNotPresent))
		
(defmodel public-asset 'floodService:PublicAsset
	"Public assets are defined as presence of highways, railways or both."
	(classification 'floodService:PublicAsset)
		:state   (if (> (+ highway railway) 0) 
								(tl/conc 'floodService:PublicAssetPresent) 
								(tl/conc 'floodService:PublicAssetNotPresent))
		:context (
			(ranking 'infrastructure:Highway) :as highway
			(ranking 'infrastructure:Railway) :as railway))
						
(defmodel farmland 'floodService:Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	       'floodService:FarmlandPresent
		:otherwise 'floodService:FarmlandNotPresent))

;; Resident users in floodplains
(defmodel residents-use 'floodService:FloodResidentsUse
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodResidentsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodResidentsUse.xdsl"
	  	:keep     ('floodService:ResidentsInFloodHazardZones)
	 	 	:context  (housing floodplains))

;; Farmer users in floodplains
(defmodel farmers-use 'floodService:FloodFarmersUse
		"Interface to Flood farmers use bayesian network"
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodFarmersUse.xdsl"
	  	:keep     ('floodService:FarmersInFloodHazardZones)
	 	 	:context  (farmland floodplains))

;; Public assets in floodplains
(defmodel public-use 'floodService:FloodPublicAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPublicAssetsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PublicAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (public-asset floodplains))
	 	 	
;; Private assets in floodplains
(defmodel private-use 'floodService:FloodPrivateAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPrivateAssetsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PrivateAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (structures floodplains))
	 	 	