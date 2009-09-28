(ns aries.models
	(:refer-clojure)
  (:use 
  	[modelling :only (defmodel measurement classification ranking
  			 							noisymax gssm flow bayesian)]
  	[aries     :only  (gssm)]))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-ph 'carbonService:Soilph
		 (classification (ranking 'carbonService:Soilph)
        1      'carbonService:HighPh
        2      'carbonService:LowPh
        {3 4}  'carbonService:ModeratePh))
    
(defmodel carbon-source 'carbonService:CarbonSourceValue
	
		"This one will harmonize the context, then retrieve and run the BN with the given
		evidence, and produce a new observation with distributions for the requested nodes."
		
	  (bayesian 'carbonService:CarbonSourceValue)
	  	:import   "bn/CarbonSourceValue.xsdl"
	  	:keep     ('carbonService:CarbonSourceValue)
	 	 	:context  (soil-ph))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------------------
;; TODO sink model
;; ----------------------------------------------------------------------------------------------

	 	 	
;; ----------------------------------------------------------------------------------------------
;; IMPLEMENT ME flow model
;; ----------------------------------------------------------------------------------------------
 	 								
 	 					
;; ----------------------------------------------------------------------------------------------
;; top-level service models
;; ----------------------------------------------------------------------------------------------
			
		 			