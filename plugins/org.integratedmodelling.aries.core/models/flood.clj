(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; ----------------------------------------------------------------------------------------------
;; source models
;; ----------------------------------------------------------------------------------------------

(defmodel flood-related-land-use 'floodService:LandUseLandCover
	"Just a reclass of the NLCD land use layer"
	(classification (ranking 'nlcd:NLCDNumeric)
		82	               'floodService:Agriculture
		#{11 90 95 12}	   'floodService:WetlandsOpenWater
		21	               'floodService:DevelopedOpenSpace
		#{41 42 43}        'floodService:Forest
		#{71 81 52}	       'floodService:GrassPasture
		#{22	31}          'floodService:DevelopedLowIntensity
		23	               'floodService:DevelopedMediumIntensity
		24	               'floodService:DevelopedHighIntensity
		:otherwise         'floodService:NonFloodControllingHabitat))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'floodService:Floodplains
	"Presence of a floodplain in given context"
	(classification (ranking 'floodService:Floodplains)
			0 'floodService:NotInFloodplain
			1 'floodService:InFloodplain))
			
(defmodel presence-of-housing 'floodService:PresenceOfHousing
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
(defmodel flood-residents-use 'floodService:FloodResidentsUse
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodResidentsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodResidentsUse.xdsl"
	  	:keep     ('floodService:ResidentsInFloodHazardZones)
	 	 	:context  (presence-of-housing floodplains))

;; Farmer users in floodplains
(defmodel flood-farmers-use 'floodService:FloodFarmersUse
		"Interface to Flood farmers use bayesian network"
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodFarmersUse.xdsl"
	  	:keep     ('floodService:FarmersInFloodHazardZones)
	 	 	:context  (farmland floodplains))

;; Public assets in floodplains
(defmodel flood-public-use 'floodService:FloodPublicAssetsUse
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'floodService:FloodPublicAssetsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodPublicAssetsUse.xdsl"
	  	:keep     ('floodService:PublicAssetOwnersAndUsersInFloodHazardZones)
	 	 	:context  (public-asset floodplains))
	 	 	