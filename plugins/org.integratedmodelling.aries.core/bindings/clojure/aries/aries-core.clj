;; --------------------------------------------------------------------------------------
;; Basic ARIES structures and functions
;; All ARIES data structures have a counterpart in Java that can be initialized with them
;; 
;; --------------------------------------------------------------------------------------

(ns aries)

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