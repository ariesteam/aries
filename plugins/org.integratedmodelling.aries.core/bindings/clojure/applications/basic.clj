;; -----------------------------------------------------------------------------------------------
;; Definition of simple ARIES workflow with default (demo) parameters when running w/o interface
;;
;; @author Ferdinando Villa
;; @date Nov 13, 2008 
;; -----------------------------------------------------------------------------------------------

(ns aries)

(tl/load-bindings 'aries.core)

(defn get-data-for-benefit
	""
	[benefit]
	(harmonize-observations
		(retrieve-observations (generate-dependency-tree benefit) false)
		(select-region-of-interest)
		512))

(. javax.swing.JOptionPane (showMessageDialog nil "Hello Fuok"))		

;;(get-data-for-benefit (tl/conc 'carbonService:ClimateStability))
(. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))