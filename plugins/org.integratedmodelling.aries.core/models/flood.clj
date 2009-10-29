(ns aries.models
	(:refer-clojure)
  (:use 
  	[modelling :only (defmodel measurement classification ranking
  			 							noisymax gssm flow bayesian)]
  	[aries     :only  (gssm)]))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel monthly-precipitation 'floodService:MonthlyPrecipitation
		 (classification (measurement 'habitat:Precipitation "in")
        [:< 3]  'floodService:VeryLowPrecipitation
        [3 6]   'floodService:LowPrecipitation
        [6 12]  'floodService:ModeratePrecipitation
        [12 24] 'floodService:HighPrecipitation
        [24 :>] 'floodService:VeryHighPrecipitation))
        
(defmodel monthly-temperature 'floodService:MonthlyTemperature
		 (classification (measurement 'habitat:AtmosphericTemperature "celsius")
        [:< -4] 'floodService:LowTemperature
        [-4 4]  'floodService:ModerateTemperature
        [4 :>]  'floodService:HighTemperature))
        
(defmodel snow-presence 'floodService:SnowPresence
		 (classification (ranking 'habitat:SnowPresence)
        [:exclusive 0 :>] 'floodService:LowlandAndHighland
        []                'floodService:RainDominatedAndSnowDominated
        :otherwise        'floodService:PeakRainOnSnow))        
        
(defmodel flood-source-adhoc 'floodService:FloodSourceValue
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
		
	  (bayesian 'floodService:FloodSourceValue)
	  	:import   "bn/FloodSourceValueAdHoc.xsdl"
	  	:keep     ('floodService:FloodSourceValue 'floodService:MonthlySnowmelt)
	 	 	:context  (monthly-precipitation monthly-temperature (comment snow-presence)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel floodplains 'floodService:Floodplains
	"Presence of a floodplain in given context"
	(classification (ranking 'floodService:Floodplains)
			0 'floodService:NotInFloodplain
			1 'floodService:InFloodplain))
			
(defmodel presence-of-housing 'floodService:PresenceOfHousing
	(classification (ranking 'floodService:PresenceOfHousing)
		"RESIDENTIAL" 'floodService:HousingNotPresent
		:otherwise    'floodService:HousingPresent))

(defmodel flood-use-farmers 'floodService:FloodFarmersUse
		""
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "bn/FloodFarmersUse.xsdl"
	  	:keep     ('floodService:FloodFarmersUse)
	 	 	:context  (floodplains))
	 	 	
(defmodel flood-use-private 'floodService:FloodPrivateAssetsUse
		""
	  (bayesian 'floodService:FloodPrivateAssetsUse)
	  	:import   "bn/FloodPrivateAssetsUse.xsdl"
	  	:keep     ('floodService:FloodPrivateAssetsUse)
	 	 	:context  (floodplains))	 	 	

(defmodel flood-use-public 'floodService:FloodPublicAssetsUse
		""
	  (bayesian 'floodService:FloodPublicAssetsUse)
	  	:import   "bn/FloodFarmersUse.xsdl"
	  	:keep     ('floodService:PublicAssetsOwnersAndUsersInFloodHazardZones)
	 	 	:context  (floodplains public-asset-locations))
	 	 	
(defmodel flood-use-residents 'floodService:FloodResidentUse
		""
	  (bayesian 'floodService:FloodResidentUse)
	  	:import   "bn/FloodResidentUse.xsdl"
	  	:keep     ('floodService:ResidentsInFloodHabitat)
	 	 	:context  (floodplains presence-of-housing))

;; ----------------------------------------------------------------------------------------------
;; TODO sink model
;; ----------------------------------------------------------------------------------------------

(defmodel flood-sink 'floodService:FloodSink
		""
	  (bayesian 'floodService:FloodSink)
	  	:import   "bn/FloodSink.xsdl"
	  	:keep     ('floodService:FloodSink)
	 	 	:context  ())
	 	 	
;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------
			
		 			