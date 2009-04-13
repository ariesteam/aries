(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification ranking
  			 									 random-classification noisymax-classification)))

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
 	  
 	 (noisymax-classification 'aestheticService:SensoryEnjoyment
 	 
 	 	 ; 4 states -- should be all the disjoint subclasses, but we don't know the ordering unless we specify
 	 	 ('aestheticService:NoSensoryEnjoyment 'aestheticService:LowSensoryEnjoyment 
 	 	 	'aestheticService:ModerateSensoryEnjoyment 'aestheticService:HighSensoryEnjoyment)	
 	 	 	
	   ; cpt for the noisymax - all others default to uniform
 		 [0.4 0.3 0.2 0.1 0.2 0.3 0.3 0.2 0.0 0.0 0.0 1.0 0.7 0.1
 	 		0.1 0.1 0.5 0.2 0.2 0.1 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0])
 	 		
 	 	 :context
  	 	 ((random-classification valuable-mountain)
  	 	  (random-classification valuable-waterbodies)))
