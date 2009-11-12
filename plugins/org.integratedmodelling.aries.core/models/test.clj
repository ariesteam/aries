;; --------------------------------------------------------------------------------------------------
;; models for testing of data, models and discretization
;; test namespace
;; fv Nov 09
;; --------------------------------------------------------------------------------------------------

(ns aries/test
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

(defmodel altitude-ft 'geophysics:Altitude
	(measurement 'geophysics:Altitude "mm"))

(defmodel conservation 'conservation:ProtectedStatus
	(ranking 'conservation:ProtectedStatus))
	
(defmodel zio 'floodService:PresenceOfHousing
	(categorization 'floodService:PresenceOfHousing))
	
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))
		 			
(defmodel testing 'geophysics:Altitude
	(identification 'floodService:PresenceOfHousing)
	 :context  
	 		((measurement 'geophysics:Altitude "m")   :as cazzo
	 		 (ranking 'habitat:PercentImperviousness) :as figa))
		 			