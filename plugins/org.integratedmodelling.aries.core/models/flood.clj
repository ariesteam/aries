(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

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

(defmodel flood-residents-use 'floodService:FloodResidentsUse
		"Interface to Flood resident use bayesian network"
	  (bayesian 'floodService:FloodResidentsUse)
	  	:import   "../aries/plugins/org.integratedmodelling.aries.core/demo/bn/FloodResidentsUse.xdsl"
	  	:keep     ('floodService:ResidentsInFloodHazardZones)
	 	 	:context  (presence-of-housing floodplains))