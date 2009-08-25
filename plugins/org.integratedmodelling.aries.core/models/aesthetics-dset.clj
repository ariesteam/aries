(ns aries.models
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification ranking identification)))

(defmodel valuable-waterbodies 'aestheticService:WaterBody

   "Classifies a land use model into lakes, oceans and land"
	(classification (ranking 'nlcd:NLCDNumeric)
	 		23   'aestheticService:Lake
			32   'aestheticService:Ocean
			:otherwise 'aestheticService:NoWater))

(defmodel valuable-mountain 'aestheticService:Mountain

   "Classifies an elevation model into three levels of provision of beautiful mountains"
   (classification  (measurement 'geophysics:Altitude "m")
   		[:< 2000]    'aestheticService:NoMountain
   		[2000 2750]  'aestheticService:SmallMountain 
   		[2750 :>]    'aestheticService:LargeMountain
   		))
   		    		 
(defmodel view-data 'aestheticService:SensoryEnjoyment

 	 "Just the dataset for further processing. This is a test model that merely collects, transforms and
 	  harmonizes data."
 	 (identification 'aestheticService:SensoryEnjoyment)
 	 	 :context
  	 	 (valuable-mountain    :as mountain 
 ; 	 	  valuable-waterbodies :as water))
))