;; --------------------------------------------------------------------------------------------------
;; models for testing of data, models and discretization
;; test namespace
;; fv Nov 09
;; --------------------------------------------------------------------------------------------------

(ns aries/test
	(:refer-clojure)
  (:refer modelling :only (defmodel defagent defscenario measurement classification categorization ranking identification bayesian)))

(defmodel altitude-ft 'geophysics:Altitude
	(measurement 'geophysics:Altitude "mm"))

(defmodel conservation 'conservation:ProtectedStatus
	(ranking 'conservation:ProtectedStatus))
	
(defmodel zio 'floodService:PresenceOfHousing
	(categorization 'floodService:PresenceOfHousing))
	
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))

		 		
;; -------------------------------------------------------------------------
;; agents
;; -------------------------------------------------------------------------

(defagent rock-climber 'carbonService:Emitter 

  "A rock climber whose model of the world consists of altitude, slope and imperviousness (so
   s/he knows where to pee). It should also have a model of self. And it emits carbon for obvious
   reasons."
  (measurement 'geophysics:Altitude "m" :as altitude)
  (ranking 'habitat:PercentImperviousness :as imperviousness)
  (classification (ranking 'geophysics:DegreeSlope)
    :units       "degrees" 
    :as          slope
    [:< 1.15] 	 'floodService:Level
    [1.15 4.57]  'floodService:GentlyUndulating
    [4.57 16.70] 'floodService:RollingToHilly
    [16.70 :>] 	 'floodService:SteeplyDissectedToMountainous)
     
  ;; these are supposed to be rules                                                          
  :death    ()
  :movement ()
  :observe  ()
  
  ;; this is used to transform the representation of the context if we get
  ;; something that doesn't fit it.
  :resolves (:space "20 cm" :time "1 s"))	
		 			