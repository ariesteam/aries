(ns aries.valuation
  (:refer-clojure)
  (:use [span.interface :only (span-driver)])
  (:use [span.flow-model :only (simulate-service-flows)]))
  
(refer 'tl  :only '(listp))

(defn j-make-es-calculator
	[]
	(new org.integratedmodelling.aries.valuation.models.ESVCalculatorModel))

(defmacro es-calculator
	"Create a stupid ecosystem services calculator that turns land use data into dollars."
	[observable]
	`(let [model# (j-make-es-calculator)] 
 	   (.setObservable model# (if (seq? ~observable) (listp ~observable) ~observable))
 	   model#))