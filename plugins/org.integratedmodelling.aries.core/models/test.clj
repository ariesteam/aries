;; --------------------------------------------------------------------------------------------------
;; models for testing of data, models and discretization
;; test namespace
;; fv Nov 09
;; --------------------------------------------------------------------------------------------------

(ns aries/test
	(:refer-clojure)
  (:refer modelling :only (defmodel defagent defscenario measurement classification categorization ranking identification bayesian)))

(defmodel altitude-mm 'geophysics:Altitude
	(measurement 'geophysics:Altitude "mm"))

(defmodel conservation-status 'conservation:ProtectedStatus
	(ranking 'conservation:ProtectedStatus))
	
(defmodel categories 'floodService:PresenceOfHousing
	(categorization 'floodService:PresenceOfHousing))
	
(defmodel altitude-m 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))

(defmodel altitude-ft 'geophysics:Altitude
  (measurement 'geophysics:Altitude "ft"))

(defmodel altitude-dyn 'geophysics:Altitude 

 "Test dynamic state computation. When the observation is mediating another
  model (in this case, implicitly, whatever compatible observation of altitude was seen), the 
  id of the model that contains the expression is bound to its value post-mediation, which
  is then redefined by the result of the state computation. 
  If the :as clause is not given, the state will be available as the local name of the observed
  concept (in this case, Altitude)."

  (measurement 'geophysics:Altitude "m"
    :as    altitude 
    :state #(+ 100000000 (:altitude %))
   )) 

;; test structural variability
(defmodel structest 'conservation:ProtectedStatus 

 [(measurement 'geophysics:Altitude "m" :as altitude)] 
 
	(measurement 'geophysics:Altitude "ft" :when #(> (:altitude %) 500))
  (measurement 'geophysics:Altitude "m") 
)
		 		
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
  :update    
       #(let [ state (.getState % 'geophysics:Altitude) ]
            (if (> state 4000)  
                (.die %)))

  ;; this is used to transform the representation of the context if we get
  ;; something that doesn't fit it.
  :resolves (:space "20 cm" :time "1 s"))	
		 			