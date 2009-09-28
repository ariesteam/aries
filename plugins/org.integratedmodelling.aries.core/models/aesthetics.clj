(ns aries.models
	(:refer-clojure)
  (:use 
  	[modelling :only (defmodel measurement classification ranking
  			 							noisymax gssm flow bayesian)]
  	[aries     :only  (gssm)]))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel viewable-waterbodies 'aestheticService:WaterBody
		 (classification (ranking 'nlcd:NLCDNumeric)
	 			23   'aestheticService:Lake
	 			32   'aestheticService:Ocean
	 			:otherwise 'aestheticService:NoWater))

(defmodel viewable-mountains 'aestheticService:Mountain
   "Classifies an elevation model into three levels of provision of beautiful mountains"
   (classification  (measurement 'geophysics:Altitude "m")
   		[:< 2000]    'aestheticService:NoMountain
   		[2000 2750]  'aestheticService:SmallMountain 
   		[2750 :>]    'aestheticService:LargeMountain))
   		   		    		 
(defmodel view-source 'aestheticService:SensoryEnjoymentProvision
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
	  (bayesian 'aestheticService:SensoryEnjoymentProvision)
	  	:import "bn/aestheticService_SensoryEnjoyment.xsdl"
	  	:keep ('aestheticService:SensoryEnjoyment)
	 	 	:context
  	 	 (viewable-mountains
  	 	  viewable-waterbodies))

;; ----------------------------------------------------------------------------------------------
;; TODO use model
;; ----------------------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;(defmodel view-clearcuts 'aestheticService:Clearcuts 
;	(classification ))

; use NLCD layers to extract transportation infrastructure
(defmodel view-commercial-transportation 'aestheticService:CommercialIndustrialTransportation 
	(classification (ranking 'nlcd:NLCDNumeric)
			23         'aestheticService:TransportationInfrastructurePresent
			:otherwise 'aestheticService:TransportationInfrastructureAbsent))

; classify a highway (vector) file - if ranks 0 or below, no highway, otherwise highway
(defmodel view-highways 'aestheticService:Highways 
	(classification (ranking 'habitat:Highway) 
			[:< 0 :inclusive]  'aestheticService:HighwaysAbsent
			:otherwise         'aestheticService:HighwaysPresent))

(defmodel view-sink 'aestheticService:ViewSink

	(bayesian 'aestheticService:ViewSink)
	  	:import "bn/aestheticService_ViewSink.xsdl"
	  	:keep ('aestheticService:ViewSink)
	 	 	:context
  	 	 ((comment view-clearcuts) view-commercial-transportation view-highways))

;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel raycast-view-flow
	
		"Only the data that feed the raycast flow model in the gssm package. The actual computation 
		 is integrated with gssm, and the observable class from this will select the raycasting 
		 submodel in it."
		
		(identification 'aestheticService:LineOfSight)
			:context 
				((measurement 'geophysics:Altitude "m")))
;; --- this is what is should be - LIDAR data
;;				 (measurement 'geophysics:GroundElevation "m")))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service model
;; ----------------------------------------------------------------------------------------------
			
(defmodel aesthetic-views 'aestheticService:ViewService
	
		"Hypothetical for now. The GSSM connecting view provision to use of views, using
		 raycasting to model the flows, influenced by athmospheric pollution."
		 
		(gssm 'aestheticService:AestheticView)
					:source     view-source
					:use        real-estate-use
		 			:transport  raycast-view-flow
		 			:sink       aesthetic-visual-blight)
		 			