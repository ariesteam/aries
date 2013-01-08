;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; models for testing of data, models and discretization
;;; test namespace
;;; fv Nov 09
;;;
;;;-------------------------------------------------------------------

(ns core.models.test
  (:refer-clojure :rename {count length})
  (:refer modelling :only (defmodel defagent defscenario defcontext numeric-coding 
                            binary-coding count model
                            measurement classification categorization 
                            ranking identification bayesian))
  (:refer time :only (time-extent)))

;(defmodel altitude-mm Altitude
;	(measurement geophysics:Altitude "mm"))

(defcontext h12 
   "Twelwe hours starting now" 
  (time-extent "12h"))

(defmodel conservation-status conservation:ProtectedStatus
	(ranking conservation:ProtectedStatus))
	
(defmodel categories floodService:PresenceOfHousing
	(categorization floodService:PresenceOfHousing))
	
(defmodel altitude-m geophysics:Altitude
  (measurement geophysics:Altitude "m")
  :as a1)
  
(defmodel altitude-ft geophysics:Altitude
  (measurement geophysics:Altitude "ft"))

(defmodel highway aestheticService:Highways 
  (classification (categorization infrastructure:Road)
    #{"Primary Route" "Secondary Route" "Unknown"} aestheticService:HighwaysPresent
    :otherwise                                     aestheticService:HighwaysAbsent))

(defmodel railway aestheticService:Railways
  (classification (categorization infrastructure:Railway)
    #{"Operational" "Under Construction" "Unexamined/Unsurveyed"} aestheticService:RailwaysPresent
    :otherwise                                                    aestheticService:RailwaysAbsent))

(defmodel streams-nhd aestheticService:River
  (classification (ranking geofeatures:River)
    [2 10]     aestheticService:RiverPresent
    :otherwise aestheticService:RiverAbsent))

(defmodel slope geophysics:Slope
  (modelling.gis/slope (measurement geophysics:Altitude "m")))

(defmodel idall representation:GenericQuantifiable
	(identification representation:GenericQuantifiable
		:context(altitude-m slope))) 

(defmodel idran representation:GenericQuantifiable
	(ranking representation:GenericQuantifiable
		:state #(+ (:a1 %) (:a2 %))
		:context(altitude-m slope))) 

(defmodel idfuc representation:GenericQuantifiable
	(ranking representation:GenericQuantifiable
		:state #(+ (:a1 %) (:a2 %))
		:context (idall)))

(defmodel lulc lulc:LandClassificationNumericMapping
  (numeric-coding nlcd:NLCDNumeric)
  (numeric-coding corine:CORINENumeric)
  (numeric-coding mglulc:MGLULCNumeric)
  (numeric-coding domlulc:DOMLULCNumeric)
  (numeric-coding glc:GLCNumeric))

(defmodel altitude-computed geophysics:Altitude 

 "Test state computation. When the observation is mediating another
  model (in this case, implicitly, whatever compatible observation of altitude was seen), the 
  id of the model that contains the expression is bound to its value post-mediation, which
  is then redefined by the result of the state computation. 
  If the :as clause is not given, the state will be available as the local name of the observed
  concept (in this case, Altitude)."

  (measurement geophysics:Altitude "m"
    :as    altitude 
    :state #(+ 100000000 (:altitude %)))) 

;; simple test of dynamic updating
(defmodel dynamic representation:GenericQuantifiable 
  (measurement representation:Length "m/s"
    :value  (corescience/gaussian 150.0 3.75)
    :as     self
    :update #(do  
                (println "time is " (:time %) ", value was " (:self %))
                (+ (:self %) 1.0)))) 


;; simple test of ODE integration. This should grow exponentially.
 

(defmodel farmland floodService:Farmland
	(classification (numeric-coding nlcd:NLCDNumeric)
			82	       floodService:FarmlandPresent
			:otherwise floodService:FarmlandAbsent))
			
;; cellular automaton contagion model - farmland only survives to next generation if surrounded by farmland
;; on three sides, meaning it will erode at the edges at each update until all farmland patches are square.
(defmodel land-use-change floodService:Farmland
	(classification 
		(binary-coding nlcd:NLCDNumeric
      :as nlcd   
      :state #(if (= (:nlcd %) 82) 1 0))
   	0 floodService:FarmlandPresent
    1 floodService:FarmlandAbsent
    :update #(let [sum (tl/apply-not-nil + (:nlcd#n %) (:nlcd#s %) (:nlcd#e %) (:nlcd#w %))]
    			       (if (< sum 3) (tl/conc 'floodService:FarmlandAbsent) (tl/conc 'floodService:FarmlandPresent)))))

;; test structural variability
(defmodel structest conservation:ProtectedStatus 

 [(ranking nlcd:NLCDNumeric :as landuse)] 

	(measurement geophysics:Altitude "ft" 
     :when #(contains? #{41.0 42.0 43.0} (:landuse %)))

  ;; you can just say :value 40.0 (or any compatible object) or use a distribution from
  ;; corescience/ssj.clj
  (measurement geophysics:Altitude "m" :value (corescience/gaussian 150.0 3.75))
)

(defscenario eroded 
  "Altitude is eroded by 100 m. Try it anywhere altitude is needed."
  (model geophysics:Altitude
    (measurement geophysics:Altitude "m"
      :as    altitude 
      :state #(- (:altitude %) 100.0))))

;; -------------------------------------------------------------------------
;; bayesian node - sample specs
;; -------------------------------------------------------------------------

;(defmodel test-bayesian-node 'observation:GenericQuantifiable
;  (probabilistic-classification 'observation:ProbabilisticQuantifiable$
;     :context (farmland lulc)
;     :cpt-organization (farmland lulc : case1 case2 case3)
;     :cpt (
;         farmland-absent terrestrial : 20 35 17
;         farmland-absent marine : 20 35 17
;         farmland-present terrestrial : 20 35 17
;         farmland-present marine : 20 35 17)))
          

;; -------------------------------------------------------------------------
;; agents
;; -------------------------------------------------------------------------

(defagent test-agent carbonService:Emitter 

  (measurement geophysics:Altitude "m" :as altitude)
  (ranking habitat:PercentImperviousSurface :as imperviousness)
  (classification (ranking geophysics:DegreeSlope)
    :units       "degrees" 
    :as          slope
    [:< 1.15] 	 floodService:Level
    [1.15 4.57]  floodService:GentlyUndulating
    [4.57 16.70] floodService:RollingToHilly
    [16.70 :>] 	 floodService:SteeplyDissectedToMountainous)
     
  ;; these are supposed to be rules                                                          
  :update #(if (> (:altitude %) 4000) (.die %))

  ;; this is used to transform the representation of the context if we get
  ;; something that doesn't fit it.
  :resolves (:space "20 cm" :time "1 s"))	
		 			