;; -----------------------------------------------------------------------------------------------
;; Definition of simple ARIES workflow with default (demo) parameters when running w/o interface
;;
;; @author Ferdinando Villa
;; @date Nov 13, 2008 
;; -----------------------------------------------------------------------------------------------

(tl/load-bindings 'corescience)
(tl/load-bindings 'aries.core)

(defn get-demo-ecosystem-services 
	"Return the services used in the demo. Benefits for these are extracted from the ontologies.
	Data dependencies are predefined in the demo/models directory of the aries.core plugin."
	[]
	(list (tl/conc 'carbonService:CarbonSequestration)))

(defn get-demo-data-for-observable
	"Returns a harmonized observation, collecting (as dependencies) all the data available 
	to observe the passed observable in the passed region of interest. Uses the demo kbox and the
	demo dependency tree. The third parameter (resolution) is the number of pixels desired on
	the longest dimension of the resulting maps; the resolution in the other dimension will be 
	adjusted to obtain square pixels according to the aspect ratio."
	[observable region-of-interest resolution]
	(aries/harmonize-observations
		resolution
		observable
		(aries/retrieve-observations 
			(aries/make-demo-dependency-tree observable) 
			true
			(aries/get-demo-data-kbox)
			region-of-interest)
		region-of-interest))
	
;; ---------------------------------------------------------------------------------------------------
;; test: this should return the observation map of climate stability, ready for GSSM
;; ---------------------------------------------------------------------------------------------------

(corescience/map-dependent-states
	(get-demo-data-for-observable 
		(tl/conc 'carbonService:ClimateStability) 
		(aries/select-region-of-interest) 
		312))
		
(tl/alert "done")