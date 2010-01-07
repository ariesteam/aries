(ns aries/view
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; ----------------------------------------------------------------------------------------------
;; provision model
;; ----------------------------------------------------------------------------------------------

(defmodel lake 'aestheticService:Lake
		"Just being a lake. We may want to reclass lake area instead"
		(classification (ranking 'geofeatures:Lake)
				0          'aestheticService:LakeNotPresent
				:otherwise 'aestheticService:LakePresent))

;; TODO doesn't work
(defmodel ocean 'aestheticService:Ocean
		"Just being a lake. We may want to reclass lake area instead"
		(classification (ranking 'geofeatures:Ocean)
				0          'aestheticService:OceanNotPresent
				:otherwise 'aestheticService:OceanPresent))

(defmodel mountain 'aestheticService:Mountain
   "Classifies an elevation model into three levels of provision of beautiful mountains"
   (classification (measurement 'geophysics:Altitude "m")
   		[2000 2750]  'aestheticService:SmallMountain  ; 
   		[2750 8850]  'aestheticService:LargeMountain  ; no higher than mount Everest!
   		:otherwise   'aestheticService:NoMountain     ; will catch artifacts too
))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticEnjoymentProvision
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
	  (bayesian 'aestheticService:AestheticEnjoymentProvision)
	  	:import "aries.core::ViewSource.xdsl"
	  	:keep ('aestheticService:NaturalBeauty)
	 	 	:context (mountain lake))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

(defmodel housing 'aestheticService:PresenceOfHousing
	"Classifies land use from property data."
	; specific to Puget region, will not be used if data unavailable
	(classification (categorization 'puget:ParcelUseCategoryKing)
		#{"R" "K"}  'aestheticService:HousingPresent
		:otherwise  'aestheticService:HousingNotPresent)
	;; TODO add Grays Harbor (see flood.clj)
	;; TODO add generalized fall-back definitions using NCLD and/or other global lu/lc data
	)
	
(defmodel property-value 'aestheticService:HousingValue
	;; TODO we need this to become an actual valuation with currency and date, so we can 
	;; turn any values into these dollars
	(classification (ranking  'economics:AppraisedPropertyValue)
		[:< 100000]      'aestheticService:VeryLowHousingValue
		[100000 200000]  'aestheticService:LowHousingValue
		[200000 400000]  'aestheticService:ModerateHousingValue
		[400000 1000000] 'aestheticService:HighHousingValue
		[1000000 :>]     'aestheticService:VeryHighHousingValue))

;; bayesian model
(defmodel homeowners 'aestheticService:ViewUse
	"Property owners who can afford to pay for the view"
	(bayesian 'aestheticService:ViewUse)
	  	:import "aries.core::ViewUse.xdsl"
	  	:keep ('aestheticService:HomeownersEnjoyment)
	 	 	:context (property-value housing))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;; TODO errors
(defmodel clearcut 'aestheticService:Clearcuts 
	(classification (ranking 'geofeatures:Clearcut)
			0          'aestheticService:ClearcutsNotPresent
			:otherwise 'aestheticService:ClearcutsPresent))

; use NLCD layers to extract transportation infrastructure
(defmodel commercial-transportation 'aestheticService:CommercialIndustrialTransportation 
	(classification (ranking 'nlcd:NLCDNumeric)
			23         'aestheticService:TransportationInfrastructurePresent
			:otherwise 'aestheticService:TransportationInfrastructureAbsent))

; presence/absence of highways
(defmodel highway 'aestheticService:Highways 
	(classification (ranking 'infrastructure:Highway)
			0          'aestheticService:HighwaysAbsent
			:otherwise 'aestheticService:HighwaysPresent))

(defmodel sink 'aestheticService:ViewSink
	"Whatever is ugly enough to absorb our enjoyment"
	(bayesian 'aestheticService:ViewSink)
	  	:import "aries.core::ViewSink.xdsl"
	  	:keep ('aestheticService:ViewSink)
	 	 	:context
  	 	 (commercial-transportation highway))

;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m")) 	 								
 	 								
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
;		(span 'aestheticService:AestheticView)
;					:source     view-source
;					:use        real-estate-use
;		 			:transport  raycast-view-flow
;		 			:sink       aesthetic-visual-blight)

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'aestheticService:AestheticEnjoyment 
	(identification 'aestheticService:AestheticEnjoyment)
		:context (
			source :as source
			homeowners :as use
			sink :as sink
			altitude :as altitude))
		 			