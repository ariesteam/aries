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
	"Public assets are defined as presence of highways, railways or both."
	(classification 'aestheticService:WaterBody)
		:state   (if (> (+ lake ocean) 0) 
								(tl/conc 'aestheticService:WaterbodyPresent) 
								(tl/conc 'aestheticService:WaterbodyNotPresent))
		:context (
			(ranking 'geofeatures:Lake)  :as lake
			(ranking 'geofeatures:Ocean) :as ocean))

(defmodel valuable-mountain 'aestheticService:Mountain
   "Classifies an elevation model into three levels of provision of beautiful mountains"
   (classification (measurement 'geophysics:Altitude "m")
   		[2000 2750]  'aestheticService:SmallMountain  ; 
   		[2750 8850]  'aestheticService:LargeMountain  ; no higher than Mount everest!
   		:otherwise   'aestheticService:NoMountain     ; will catch artifacts too
))
   		   		    		 
(defmodel view-source 'aestheticService:SensoryEnjoymentProvision
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
	  (bayesian 'aestheticService:SensoryEnjoymentProvision)
	  	:import "bn/SensoryEnjoyment.xsdl"
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

; presence/absence of highways
(defmodel view-highways 'aestheticService:Highways 
	(classification (ranking 'infrastructure:Highway)
			0          'aestheticService:HighwaysAbsent
			:otherwise 'aestheticService:HighwaysPresent))

(defmodel view-sink 'aestheticService:ViewSink

	(bayesian 'aestheticService:ViewSink)
	  	:import "bn/ViewSink.xsdl"
	  	:keep ('aestheticService:ViewSink)
	 	 	:context
  	 	 ((comment view-clearcuts) view-commercial-transportation view-highways))

;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
;(defmodel raycast-view-flow
;	
;		"Only the data that feed the raycast flow model in the gssm package. The actual computation 
;		 is integrated with gssm, and the observable class from this will select the raycasting 
;		 submodel in it."
;		
;		(identification 'aestheticService:LineOfSight)
;			:context 
;				((measurement 'geophysics:Altitude "m")))
;; --- this is what is should be - LIDAR data
;;				 (measurement 'geophysics:GroundElevation "m")))
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service model
;; ----------------------------------------------------------------------------------------------
			
;(defmodel aesthetic-views 'aestheticService:ViewService
;	
;		"Hypothetical for now. The GSSM connecting view provision to use of views, using
;		 raycasting to model the flows, influenced by athmospheric pollution."
;		 
;		(gssm 'aestheticService:AestheticView)
;					:source     view-source
;					:use        real-estate-use
;		 			:transport  raycast-view-flow
;		 			:sink       aesthetic-visual-blight)
		 			