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
		 (classification (measurement 'ecology:Rainfall "mm")
        [:< ] 'floodService:LowMonthlyPrecipitation
        []    'floodService:MediumMonthlyPrecipitation
        [ :>] 'floodService:HighMonthlyPrecipitation))
        
(defmodel monthly-precipitation 'floodService:MonthlyTemperature
		 (classification (measurement 'ecology:MonthlyTemperature "mm")
        [:< ] 'floodService:LowMonthlyPrecipitation
        []    'floodService:MediumMonthlyPrecipitation
        [ :>] 'floodService:HighMonthlyPrecipitation))
        
(defmodel monthly-precipitation 'floodService:SnowPresence
		 (classification (measurement 'ecology:Snowfall "mm")
        [:exclusive 0 :>] 'floodService:LowMonthlyPrecipitation
        []                'floodService:MediumMonthlyPrecipitation
        :otherwise        'floodService:HighMonthlyPrecipitation))        
        
(defmodel flood-source-adhoc 'floodService:FloodSourceValue
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
		
	  (bayesian 'floodService:FloodSourceValue)
	  	:import   "bn/FloodSourceValueAdHoc.xsdl"
	  	:keep     ('floodService:FloodSourceValue)
	 	 	:context  (monthly-precipitation monthly-temperature snow-presence))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

(defmodel flood-use-farmers 'floodService:FloodFarmersUse
		""
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "bn/FloodSink.xsdl"
	  	:keep     ('floodService:FloodFarmersUse)
	 	 	:context  ())
	 	 	
(defmodel flood-use-private 'floodService:FloodPrivateAssetsUse
		""
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "bn/FloodSink.xsdl"
	  	:keep     ('floodService:FloodFarmersUse)
	 	 	:context  ())	 	 	

(defmodel flood-use-public 'floodService:FloodPublicAssetsUse
		""
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "bn/FloodSink.xsdl"
	  	:keep     ('floodService:FloodFarmersUse)
	 	 	:context  ())
	 	 	
(defmodel flood-use-residents 'floodService:FloodResidentUse
		""
	  (bayesian 'floodService:FloodFarmersUse)
	  	:import   "bn/FloodSink.xsdl"
	  	:keep     ('floodService:FloodFarmersUse)
	 	 	:context  ())

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
 	 								
(defmodel flood-flow
	
		"Only the data that feedthe raycast flow model in the gssm package. The actual computation 
		 is integrated with gssm, and the observable class from this will select the raycasting 
		 submodel in it."
		
		(identification 'aestheticService:LineOfSight)
			:context 
				((measurement 'geophysics:Altitude "m")))
;; --- this is what is should be - LIDAR data
;;				 (measurement 'geophysics:GroundElevation "m")))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------
			
(defmodel aesthetic-views 'aestheticService:ViewService
	
		"Hypothetical for now. The GSSM connecting view provision to use of views, using
		 raycasting to model the flows, influenced by athmospheric pollution."
		 
		(gssm 'aestheticService:ViewService)
					:source     view-source
					:use        real-estate-use
		 			:transport  raycast-view-flow
		 			:sink       aesthetic-visual-blight)
		 			