;; -----------------------------------------------------------------------------------------------
;; ARIES demo functionalities
;;
;; @author Ferdinando Villa
;; @date Nov 13, 2008 
;; -----------------------------------------------------------------------------------------------

(ns aries.demo)

(tl/load-bindings 'corescience)
(tl/load-bindings 'geospace)

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
			false
			(aries/get-demo-data-kbox)
			region-of-interest)
		region-of-interest))

(defn run-gssm-demo
	"Run GSSM interactive interface on the harmonized demo dataset for the passed benefit (pass a 
	concept, a symbol or a string). The second parameter is an int specifying the desired grid 
	resolution on the longest dimension."
	[benefit max-resolution transition-threshold]
	(let [benf (tl/conc benefit)
	      data (get-demo-data-for-observable benf	   
						 	  (aries/select-region-of-interest) 
						     max-resolution)]
	  (district.gssm-interface/gssm-interface benf data transition-threshold)))
