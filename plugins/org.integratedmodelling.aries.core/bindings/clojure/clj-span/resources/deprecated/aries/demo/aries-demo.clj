;; -----------------------------------------------------------------------------------------------
;; ARIES demo functionalities
;;
;; @author Ferdinando Villa
;; @date Nov 13, 2008 
;; -----------------------------------------------------------------------------------------------

(ns aries.demo
  (:refer-clojure)
  (:use [span.interface :only (span-driver)]))

;(tl/load-bindings 'corescience)
;(tl/load-bindings 'geospace)

;(defn get-demo-ecosystem-services 
;	"Return the services used in the demo. Benefits for these are extracted from the ontologies.
;	Data dependencies are predefined in the demo/models directory of the aries.core plugin."
;	[]
	;(list (tl/conc 'carbonService:CarbonSequestration)))

;(defn get-demo-data-for-observable
;	"Returns a harmonized observation, collecting (as dependencies) all the data available 
;	to observe the passed observable in the passed region of interest. Uses the demo kbox and the
;	demo dependency tree. The third parameter (resolution) is the number of pixels desired on
;	the longest dimension of the resulting maps; the resolution in the other dimension will be 
;	adjusted to obtain square pixels according to the aspect ratio."
;	[observable region-of-interest resolution]
;	(if (nil? observable) nil (aries/harmonize-observations
;		resolution	
;		observable
;		(aries/retrieve-observations 
;			(aries/make-demo-dependency-tree observable) 
;			false
;			(aries/get-demo-data-kbox)
;			region-of-interest)
;		region-of-interest)))

;(defn select-region-of-interest-by-service
;	""
;	[service-id]
;	(tl/lit 'geospace:SpatialRecord
;		(cond (= service-id 'aestheticService:SensoryEnjoyment)
;					"POLYGON((-122.420446 47.464349, -121.759593 47.464349, -121.759593 46.85382, -122.420446 46.85382, -122.420446 47.464349))"
;					(= service-id 'aestheticService:ProximityToBeauty)
;					"POLYGON((-122.276913 47.421371, -122.202813 47.421371, -122.202813 47.373763, -122.276913 47.373763, -122.276913 47.421371))"
;					(= service-id 'carbonService:ClimateStability)
;					"POLYGON((-122.535334 47.76684, -121.409633 47.76684, -121.409633 47.110871, -122.535334 47.110871, -122.535334 47.76684))")));

;(defn make-dataset
;	"Create a netCDF file with all the harmonized data describing a concept"
;	[concept-id filename resolution]
;	(let [concept (tl/conc concept-id)
;			  study-region (select-region-of-interest-by-service concept-id)
;			  dataset (get-demo-data-for-observable concept
;							   study-region
;							   resolution)]
;			(modelling/write-netcdf dataset filename)))

;(defn run-gssm-demo
;	"Run GSSM interactive interface on the harmonized demo dataset for the passed benefit (pass a 
;	concept, a symbol or a string). The second parameter is an int specifying the desired grid 
;	resolution on the longest dimension."
;	[benefit-source benefit-use benefit-sink benefit-flow flow-params max-resolution]
;	(let [benf-source  (tl/conc benefit-source)
;	      benf-use     (tl/conc benefit-use)
;	      benf-sink    (tl/conc benefit-sink)
;	      benf-flow    (tl/conc benefit-flow)
;	      study-region (select-region-of-interest-by-service benefit-source)
;	      source-data (get-demo-data-for-observable benf-source
;							study-region
;							max-resolution)
;				use-data (get-demo-data-for-observable benf-use
;							study-region
;							max-resolution)
;	      sink-data   (get-demo-data-for-observable benf-sink	   
;							study-region
;							max-resolution)
;			  flow-data (get-demo-data-for-observable benf-flow
;							study-region
;							max-resolution)]
;	  (span-driver
;	  		 benf-source source-data
;	  		 benf-sink   sink-data
;	  		 benf-use    use-data
;	  		 benf-flow   flow-data
;				 flow-params)))
				
				
(defn run-span-interactive
	"Run GSSM interactive interface on the harmonized demo dataset for the passed benefit (pass a 
	concept, a symbol or a string). The second parameter is an int specifying the desired grid 
	resolution on the longest dimension."
	[model-id benefit-source benefit-use benefit-sink benefit-flow flow-params location max-resolution]
	(let [benf-source  (tl/conc benefit-source)
	      benf-use     (tl/conc benefit-use)
	      benf-sink    (tl/conc benefit-sink)
	      benf-flow    (tl/conc benefit-flow)
	      data-obs     (modelling/run-at-location model-id location max-resolution)
	      source-data  (corescience/find-observation data-obs benf-source)
				use-data     (corescience/find-observation data-obs benf-use) 
	      sink-data    (corescience/find-observation data-obs benf-sink)
			  flow-data    (corescience/find-observation data-obs benf-flow)]
	  (span-driver
	  		 benf-source source-data
	  		 benf-sink   sink-data
	  		 benf-use    use-data
	  		 benf-flow   flow-data
				 flow-params)))
				 
;(defn save-gssm-demo-data
;	"Run GSSM on the harmonized demo dataset for the passed benefit (pass a 
;	concept, a symbol or a string). The second parameter is an int specifying the desired grid 
;	resolution on the longest dimension. The third is a file name for NetCDF output."
;	[benefit-source benefit-use benefit-sink benefit-flow flow-params max-resolution output-file]
;	(let [benf-source  (tl/conc benefit-source)
;	      benf-use     (tl/conc benefit-use)
;	      benf-sink    (tl/conc benefit-sink)
;	      benf-flow    (tl/conc benefit-flow)
;	      study-region (select-region-of-interest-by-service benefit-source)
;	      source-data (get-demo-data-for-observable benf-source
;							study-region
;							max-resolution)
;				use-data (get-demo-data-for-observable benf-use
;							study-region
;							max-resolution)
;	      sink-data   (get-demo-data-for-observable benf-sink	   
;							study-region
;							max-resolution)
;			  flow-data (get-demo-data-for-observable benf-flow
;							study-region
;							max-resolution)]
;	  (org.integratedmodelling.aries.core.datastructures.demo.FlowObservationBuilder/buildObservation
;	   (span-driver
;	    benf-source source-data
;	    benf-sink   sink-data
;	    benf-use    use-data
;	    benf-flow   flow-data
;	    flow-params :matrix-list)
;	   source-data
;	   use-data
;	   sink-data
;	   flow-data
;	   output-file)))
