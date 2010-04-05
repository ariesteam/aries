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

(defmodel conservation-status 'conservation:ProtectedStatus
	(ranking 'conservation:ProtectedStatus))
	
(defmodel zio 'floodService:PresenceOfHousing
	(categorization 'floodService:PresenceOfHousing))
	
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))

;; test clojure expressions for state computation
(defmodel cljtest 'geophysics:Altitude 
  (measurement 'geophysics:Altitude "m"
    :as    altitude 
    :state #(+ 100000000 (:altitude %))
   )) 

;; test structural variability
;; status of mountains always protected, otherwise lookup in PA data
(defmodel structest 'conservation:ProtectedStatus

;  "Computes the altitude over the observation context, then uses it to select the model of protection status
;   used. If altitude is higher than 1500m, protected status is true. Otherwise, we look up data to figure it
;   out. This could be used e.g. to substitute mere protected status to build a scenario." 

 [(measurement 'geophysics:Altitude "m") :as altitude] 

 (ranking 'conservation:ProtectedStatus :value 1) 
    :when #(> (:altitude %) 1500)

  (ranking 'conservation:ProtectedStatus) 
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
		 			