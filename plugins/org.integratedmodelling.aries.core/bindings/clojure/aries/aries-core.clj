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

;; --------------------------------------------------------------------------------------
;; Basic ARIES structures and functions
;; All ARIES data structures have a counterpart in Java that can be initialized with them
;; 
;; --------------------------------------------------------------------------------------

(ns aries
  (:use [clj-span.aries-span-bridge :only (span-driver)]))
(refer 'tl        :only '(listp get-new-session))
(refer 'modelling :only '(transform-model process-observable annotate-concept))

(defn j-make-span
	"Make a new instance of a GSSM model and return it. Should be private, but must be public to work 
	within the gssm macro. We need a compiled proxy because the Java classes aren't visible at runtime."
	[]
	(new org.integratedmodelling.aries.core.models.SPANModel (str *ns*)))
	
(defn get-span-proxy
  "Create a Java object to handle a SPAN run."
  []
  (proxy [org.integratedmodelling.aries.core.span.SPANProxy] []
    (runSPAN [observation source-concept use-concept sink-concept flow-concepts flow-params]
             (println "We are inside runSPAN!")
             (binding [tl/*session* (get-new-session)]
               (time (span-driver observation source-concept sink-concept use-concept flow-concepts flow-params))))))

;; a static object will suffice, this is thread-safe to the point of boredom
(org.integratedmodelling.aries.core.implementations.observations.SPANTransformer/setSPANProxy (get-span-proxy))

(defmacro span
  "Create a SPAN model. The observable must be a flow concept. This
   one admits specification of all SPAN flow parameters inside the
   span form. The context models will be mapped to the source, use,
   and sink observables; any other dependents whose observable is not
   a source, sink or use type will be dependencies for the flow
   model."
  [observable source-obs use-obs sink-obs flow-obs flow-data-obs-seq & body]
  `(let [model# (j-make-span)] 
 	   (.setObservable model# (process-observable '~observable))
     (.setFlowObservables model#
                          (annotate-concept '~source-obs)
                          (annotate-concept '~use-obs)
                          (annotate-concept '~sink-obs)
                          (annotate-concept '~flow-obs)
                          '~flow-data-obs-seq)
     (if (not (nil? '~body))
       (doseq [classifier# (partition 2 '~body)]
         (if  (keyword? (first classifier#))
           (transform-model model# classifier#))))
     model#))
