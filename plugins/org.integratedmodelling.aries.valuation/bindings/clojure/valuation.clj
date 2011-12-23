;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.

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