(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification ranking
  			 									 noisymax gssm flow bayesian)))

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
		evidence, and produce a new observation of stochastic values for all its nodes."
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
;; TODO sink model
;; ----------------------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel raycast-view-flow
	
		"Hypothetical for now. Will map to the raycast flow model in the gssm package. The model/flow 
		 proxy produces only the harmonized data; the actual computation is integrated with gssm, and 
		 the observable class from this will select the raycasting submodel in it."
		
		(flow 'aestheticService:LineOfSight)
			:context 
				((measurement 'geophysics:Altitude "m")
				 (measurement 'geophysics:GroundElevation "m")))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service model
;; ----------------------------------------------------------------------------------------------
			
(defmodel aesthetic-views 'aestheticService:ViewService
	
		"Hypothetical for now. The GSSM connecting view provision to use of views, using
		 raycasting to model the flows, influenced by athmospheric pollution."
		 
		(gssm 'aestheticService:ViewService 
					:source     view-source
					:use        real-estate-use
		 			:transport  raycast-view-flow
		 			:sink       aesthetic-visual-blight))
		 			