;; --------------------------------------------------------------------------------------
;; Basic ARIES structures and functions
;; All ARIES data structures have a counterpart in Java that can be initialized with them
;; 
;; --------------------------------------------------------------------------------------

(ns aries
  (:refer-clojure)
  (:use [misc.utils      :only (maphash count-distinct)]
	[gssm.flow-model :only (simulate-service-flows)]
	[gssm.analyzer   :only (*source-threshold*
				*sink-threshold*
				*use-threshold*
				theoretical-source
				theoretical-sink
				theoretical-use
				inaccessible-source
				inaccessible-sink
				inaccessible-use
				possible-flow
				possible-source
				possible-inflow
				possible-sink
				possible-use
				possible-outflow
				blocked-flow
				blocked-source
				blocked-inflow
				blocked-sink
				blocked-use
				blocked-outflow
				actual-flow
				actual-source
				actual-inflow
				actual-sink
				actual-use
				actual-outflow
				carriers-encountered)]))

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
	
;; TODO wrap in a binding form to bind the parameters when G tells us to what.
(defn get-gssm-proxy
	"Create a Java object to handle a GSSM run."
	[]
	(proxy [org.integratedmodelling.aries.core.gssm.GSSMProxy] []
		(runGSSM [source-obs use-obs sink-obs flow-obs] 
			(simulate-service-flows source-obs use-obs sink-obs flow-obs))))
			
;; a static object should suffice, this is thread-safe to the point of boredom
(org.integratedmodelling.aries.core.implementations.observations.GSSMTransformer/setGSSMProxy (get-gssm-proxy))

			