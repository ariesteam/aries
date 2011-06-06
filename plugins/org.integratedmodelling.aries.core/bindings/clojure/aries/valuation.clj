(ns aries.valuation
  (:refer-clojure)
  (:use [span.interface :only (span-driver)])
  (:use [span.flow-model :only (simulate-service-flows)]))
  
(refer 'tl  :only '(listp))
(refer 'modelling :only '(transform-model process-observable))

(defn j-make-es-calculator
	[]
	(new org.integratedmodelling.aries.valuation.models.ESVCalculatorModel (str *ns*)))

(defmacro es-calculator
	"Create a stupid ecosystem services calculator that turns land use data into dollars."
	[observable & body]
	`(let [model# (j-make-es-calculator)] 
 	   (.setObservable model# (process-observable '~observable))
	   (if (not (nil? '~body)) 
				(doseq [classifier# (partition 2 '~body)]
		 	   	(if  (keyword? (first classifier#)) 
		 	   		  (transform-model model# classifier#))))
 	   model#))