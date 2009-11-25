;; --------------------------------------------------------------------------------------
;; Basic ARIES structures and functions
;; All ARIES data structures have a counterpart in Java that can be initialized with them
;; 
;; --------------------------------------------------------------------------------------

(ns aries
  (:refer-clojure)
  (:use [span.flow-model :only (simulate-service-flows)]))
(refer 'tl          :only '(listp))
(refer 'corescience :only '(get-observable-class))

(defn j-make-gssm
	"Make a new instance of a GSSM model and return it. Should be private, but must be public to work 
	within the gssm macro. We need a compiled proxy because the Java classes aren't visible at runtime."
	[]
	(new org.integratedmodelling.aries.core.models.GSSMModel))

(defn get-data-for-observable
	"Returns a harmonized observation, which contains as dependencies all the data available 
	to observe the passed observable in the passed region of interest. Uses the passed kbox and the
	dependency tree for the observable extracted from the knowledge base. The last parameter (resolution) 
	is the number of pixels desired on the longest dimension of the resulting maps; the resolution in 
	the other dimension will be adjusted to obtain square pixels according to the aspect ratio."
	[observable region-of-interest kbox resolution exhaustive]
	(aries/harmonize-observations
		resolution
		(aries/retrieve-observations 
			(aries/make-dependency-tree observable) 
			exhaustive
			kbox
			region-of-interest)
		region-of-interest))

(defn get-scaling-parameters 
	""
	[observable region-of-interest]
	nil)

(defn get-scaled-observation
	""
	[data scaling-parameters]
	nil)
	
;; TODO bind the flow parameters before call to simulate-service-flows.
(defn get-gssm-proxy
	"Create a Java object to handle a GSSM run."
	[]
	(proxy [org.integratedmodelling.aries.core.gssm.GSSMProxy] []
		(runGSSM [source-obs use-obs sink-obs flow-obs flow-params] 
			(simulate-service-flows 
				(get-observable-class source-obs) source-obs 
				(get-observable-class use-obs)    use-obs 
				(get-observable-class sink-obs)   sink-obs 
				(get-observable-class flow-obs)   flow-obs))))
			
;; a static object will suffice, this is thread-safe to the point of boredom
(org.integratedmodelling.aries.core.implementations.observations.GSSMTransformer/setGSSMProxy (get-gssm-proxy))

(defmacro gssm
	"Create a gssm model. The observable must be a service. This one admits specification of dependencies
	and flow parameters using :source, :sink, :use, :flow clauses, plus all GSSM flow parameters."
	[observable]
	`(let [model# 
 	        	(modelling/j-make-gssm)]  ;; FIXME there is no modelling/j-make-gssm
 	   (.setObservable model# (if (seq? ~observable) (listp ~observable) ~observable))
 	   model#))
			