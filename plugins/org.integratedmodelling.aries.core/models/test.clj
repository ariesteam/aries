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

(defmodel altitude-computed 'geophysics:Altitude 

 "Test state computation. When the observation is mediating another
  model (in this case, implicitly, whatever compatible observation of altitude was seen), the 
  id of the model that contains the expression is bound to its value post-mediation, which
  is then redefined by the result of the state computation. 
  If the :as clause is not given, the state will be available as the local name of the observed
  concept (in this case, Altitude)."

  (measurement 'geophysics:Altitude "m"
    :as    altitude 
    :state #(+ 100000000 (:altitude %))
   )) 

(defmodel test-dynamic 'geophysics:Altitude 

  (measurement 'geophysics:Altitude "m"
    :value  50
    :update #(do  
                (print "time is " (:time %) ", altitude was " (:altitude %))
                (+ (:altitude %) 1.0)) 
   )) 

;; test structural variability
(defmodel structest 'conservation:ProtectedStatus 

 [(ranking 'nlcd:NLCDNumeric :as landuse)] 

	(measurement 'geophysics:Altitude "ft" 
     :when #(contains? #{41.0 42.0 43.0} (:landuse %)))

  ;; you can just say :value 40.0 (or any compatible object) or use a distribution from
  ;; corescience/ssj.clj
  (measurement 'geophysics:Altitude "m" :value (corescience/gaussian 150.0 3.75))
)

(defscenario eroded 'geophysics:Altitude 
  "Altitude is eroded by 100 m. Try it anywhere altitude is needed."
  (measurement 'geophysics:Altitude "m"
    :as    altitude 
    :state #(- (:altitude %) 100.0))) 

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
  :update #(if (> (:altitude %) 4000) (.die %))

  ;; this is used to transform the representation of the context if we get
  ;; something that doesn't fit it.
  :resolves (:space "20 cm" :time "1 s"))	
		 			