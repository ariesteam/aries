;; -----------------------------------------------------------------------------------------------
;; Definition of simple ARIES workflow with default (demo) parameters when running w/o interface
;;
;; @author Ferdinando Villa
;; @date Nov 13, 2008 
;; -----------------------------------------------------------------------------------------------

(tl/load-bindings 'aries.core)

(defn get-data-for-benefit
	"Returns a harmonized observation, collecting (as dependencies) all the data available 
	to observe the passed benefit in the passed region of interest. Uses the demo kbox and the
	demo dependency tree. The third parameter (resolution) is the number of pixels desired on
	the longest dimension of the resulting maps; the resolution in the other dimension will be 
	adjusted to obtain square pixels according to the aspect ratio."
	[benefit region-of-interest resolution]
	(aries/harmonize-observations
		(aries/retrieve-observations 
			(aries/get-demo-data-kbox)
			(aries/make-demo-dependency-tree benefit) 
			region-of-interest
			false)
		select-region-of-interest
		resolution))

(aries/generate-demo-dependency-tree (conc 'carbonService:ClimateStability))

(. javax.swing.JOptionPane (showMessageDialog nil "Hello Fuok"))		

;;(get-data-for-benefit (tl/conc 'carbonService:ClimateStability) (aries/get-region-of-interest))
(. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))