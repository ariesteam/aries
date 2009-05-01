(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification ranking
  			 									 noisymax gssm flow)))

(defmodel valuable-waterbodies 'aestheticService:WaterBody
		 (classification (ranking 'nlcd:NLCDNumeric)
	 			23   'aestheticService:Lake
	 			32   'aestheticService:Ocean
	 			:otherwise 'aestheticService:NoWater))

(defmodel valuable-mountain 'aestheticService:Mountain
   "Classifies an elevation model into three levels of provision of beautiful mountains"
   (classification  (measurement 'geophysics:Altitude "m")
   		[:< 2000]    'aestheticService:NoMountain
   		[2000 2750]  'aestheticService:SmallMountain 
   		[2750 :>]    'aestheticService:LargeMountain))
   		    		 
(defmodel aesthetic-enjoyment-provision 'aestheticService:SensoryEnjoyment

 	 "Unconditional bayesian model of sensory enjoyment provision. Will assign probabilities to 
 	  4 levels of provision of aesthetic enjoyment based on altitude and NCLD numeric class
 	  data."
 	  
 	 (classification 'aestheticService:SensoryEnjoyment
 	 	 0 'aestheticService:NoSensoryEnjoyment 
 	 	 1 'aestheticService:LowSensoryEnjoyment 
 	 	 2 'aestheticService:ModerateSensoryEnjoyment 
 	 	 3 'aestheticService:HighSensoryEnjoyment)
 	 	 	
 	 	 :context
  	 	 (valuable-mountain valuable-waterbodies)
  	 	 
 	 	 :probability	 	 	
 			 (noisymax [0.4 0.3 0.2 0.1 0.2 0.3 0.3 0.2 0.0 0.0 0.0 1.0 0.7 0.1
 	 								0.1 0.1 0.5 0.2 0.2 0.1 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0]))
 	 								
(defmodel raycast-view-flow
	
		"Hypothetical for now. Will map to the raycast flow model in the gssm package. The model/flow 
		 proxy produces only the harmonized data; the actual computation is integrated with gssm, and 
		 the observable class from this will select the raycasting submodel in it."
		
		(flow 'aestheticService:LineOfSight)
			:context 
				((measurement 'geophysics:Altitude "m")
				 (measurement 'geophysics:GroundElevation "m")))
 	 								
(defmodel aesthetic-views 'aestheticService:ViewService
	
		"Hypothetical for now. The GSSM connecting view provision to use of views, using
		 raycasting to model the flows, influenced by athmospheric pollution."
		 
		(gssm 'aestheticService:ViewService 
					:source     aesthetic-enjoyment-provision
					:use        real-estate-use
		 			:transport  raycast-view-flow
		 			:sink       aesthetic-visual-blight))
		 			