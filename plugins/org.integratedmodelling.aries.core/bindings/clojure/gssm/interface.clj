;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-GSSM.
;;;
;;; CLJ-GSSM is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-GSSM is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-GSSM.  If not, see <http://www.gnu.org/licenses/>.

(ns gssm.interface
  (:refer-clojure)
  (:import (java.io OutputStreamWriter InputStreamReader PushbackReader))
  (:use
;;      [tl              :only (get-session)]
;;	[geospace        :only (build-coverage
;;				get-spatial-extent
;;				grid-extent?
;;				grid-rows
;;				grid-cols)]
;;	[corescience     :only (map-dependent-states)]
	[misc.utils      :only (maphash count-distinct)]
	[gssm.params     :only (set-global-params!)]
	[gssm.flow-model :only (simulate-service-flows)]
	[gssm.analyzer   :only (theoretical-source
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
				actual-outflow)]))

(defn select-menu-option
  "Prompts the user with a menu of choices and returns the label
   corresponding to their selection."
  [prompts num-prompts]
  (loop []
    (printf "%nOptions Menu:%n")
    (dotimes [i num-prompts]
	(printf " %d) %s%n" (inc i) (prompts i)))
    (print "Choice: ")
    (flush)
    (let [choice (read)]
      (if (and (integer? choice) (> choice 0) (<= choice num-prompts))
	(prompts (dec choice))
	(do (println "Invalid selection. Please choose a number from the menu.")
	    (recur))))))

(defn select-location
  "Prompts for coords and returns the cooresponding location object."
  [locations rows cols]
  (loop []
    (printf "%nInput location coords%n")
    (let [coords [(do (printf "Row [0-%d]: " (dec rows)) (flush) (read))
		  (do (printf "Col [0-%d]: " (dec cols)) (flush) (read))]
	  location (some #(and (= (:id %) coords) %) locations)]
      (if location
	location
	(do (printf "No location at %s. Enter another selection.%n" coords)
	    (recur))))))

(defn coord-map-to-matrix
  "Renders a map of {[i j] -> value} into a 2D matrix."
  [rows cols coord-map]
  (let [matrix (make-array Double/TYPE rows cols)]
    (doseq [[i j :as key] (keys coord-map)]
	(aset-double matrix i j (coord-map key)))
    matrix))

(defn select-map-by-feature
  [source-observation sink-observation use-observation flow-observation rows cols]
  (let [feature-states (maphash (memfn getLocalName) identity
				(merge (corescience/map-dependent-states source-observation)
				       (corescience/map-dependent-states sink-observation)
				       (corescience/map-dependent-states use-observation)
				       (if flow-observation
					 (corescience/map-dependent-states flow-observation))))
	feature-names  (vec (keys feature-states))
	num-features   (count feature-names)
	feature-name   (select-menu-option feature-names num-features)]
    (zipmap (for [i (range rows) j (range cols)] [i j])
	    (feature-states feature-name))))

(defn view-location-properties
  "Prints a summary of the post-simulation properties of the
  location."
  [location]
  (let [fmt-str (str
		 "%nLocation %s%n"
		 "--------------------%n"
		 "Neighbors:     %s%n"
		 "Source:        %.2f%n"
		 "Sink:          %.2f%n"
		 "Use:           %.2f%n"
		 "Flow Features: %s%n"
		 "Carriers Encountered: %d%n")]
    (printf fmt-str
	    (:id location)
	    (:neighbors location)
	    (force (:source location))
	    (force (:sink location))
	    (force (:use location))
	    (:flow-features location)
	    (count @(:carrier-cache location)))))

(defn gssm-autopilot
  "Takes the source, sink, use, and flow concepts along with
   observations of their dependent features, calculates the gssm
   flows, and returns a list of matrices representing the flow
   analysis results."
  [source-concept source-observation
   sink-concept   sink-observation
   use-concept    use-observation
   flow-concept   flow-observation
   flow-params]
  (assert (geospace/grid-extent? source-observation))
  (set-global-params! flow-params)
  (let [rows      (geospace/grid-rows    source-observation)
	cols      (geospace/grid-columns source-observation)
	locations (simulate-service-flows source-concept source-observation
					  sink-concept   sink-observation
					  use-concept    use-observation
					  flow-concept   flow-observation
					  rows           cols)]
    (doall (map (fn [coord-map] (coord-map-to-matrix rows cols coord-map))
		(list
		 (theoretical-source  locations)
		 (theoretical-sink    locations)
		 (theoretical-use     locations)
		 (inaccessible-source locations)
		 (inaccessible-sink   locations)
		 (inaccessible-use    locations)
		 (possible-flow       locations)
		 (possible-source     locations)
		 (possible-inflow     locations)
		 (possible-sink       locations)
		 (possible-use        locations)
		 (possible-outflow    locations)
		 (blocked-flow        locations)
		 (blocked-source      locations)
		 (blocked-inflow      locations)
		 (blocked-sink        locations)
		 (blocked-use         locations)
		 (blocked-outflow     locations)
		 (actual-flow         locations)
		 (actual-source       locations)
		 (actual-inflow       locations)
		 (actual-sink         locations)
		 (actual-use          locations)
		 (actual-outflow      locations))))))

(defn gssm-interface
  "Takes the source, sink, use, and flow concepts along with
   observations of their dependent features, calculates the gssm
   flows, and provides a simple menu-based interface to view the
   results."
  [source-concept source-observation
   sink-concept   sink-observation
   use-concept    use-observation
   flow-concept   flow-observation
   flow-params]
  (assert (geospace/grid-extent? source-observation))
  (set-global-params! flow-params)
  (binding [*out* (OutputStreamWriter. (.getOutputStream (tl/get-session)))
	    *in*  (PushbackReader. (InputStreamReader. (.getInputStream  (tl/get-session))))]
    (let [rows      (geospace/grid-rows          source-observation)
	  cols      (geospace/grid-columns       source-observation)
	  extent    (geospace/get-spatial-extent source-observation)
	  locations (simulate-service-flows source-concept source-observation
					    sink-concept   sink-observation
					    use-concept    use-observation
					    flow-concept   flow-observation
					    rows           cols)
	  menu (array-map
		"View Theoretical Source"   #(theoretical-source       locations)
		"View Theoretical Sink"     #(theoretical-sink         locations)
		"View Theoretical Use"      #(theoretical-use          locations)
		"View Inacessible Source"   #(inaccessible-source      locations)
		"View Inacessible Sink"     #(inaccessible-sink        locations)
		"View Inacessible Use"      #(inaccessible-use         locations)
		"View Possible Flow"        #(possible-flow            locations)
		"View Possible Source"      #(possible-source          locations)
		"View Possible Inflow"      #(possible-inflow          locations)
		"View Possible Sink"        #(possible-sink            locations)
		"View Possible Use"         #(possible-use             locations)
		"View Possible Outflow"     #(possible-outflow         locations)
		"View Blocked Flow"         #(blocked-flow             locations)
		"View Blocked Source"       #(blocked-source           locations)
		"View Blocked Inflow"       #(blocked-inflow           locations)
		"View Blocked Sink"         #(blocked-sink             locations)
		"View Blocked Use"          #(blocked-use              locations)
		"View Blocked Outflow"      #(blocked-outflow          locations)
		"View Actual Flow"          #(actual-flow              locations)
		"View Actual Source"        #(actual-source            locations)
		"View Actual Inflow"        #(actual-inflow            locations)
		"View Actual Sink"          #(actual-sink              locations)
		"View Actual Use"           #(actual-use               locations)
		"View Actual Outflow"       #(actual-outflow           locations)
		"View Location Properties"  #(view-location-properties (select-location locations rows cols))
		"View Feature Map"          #(select-map-by-feature    source-observation
								       sink-observation
								       use-observation
								       flow-observation
								       rows
								       cols)
		"Quit"                      nil)
	  prompts (vec (keys menu))
	  num-prompts (count prompts)]
      (println "Rows x Cols: " rows " x " cols)
      (println "Source-Concept-Name:" (.getLocalName source-concept))
      (println "Sink-Concept-Name:  " (.getLocalName sink-concept))
      (println "Use-Concept-Name:   " (.getLocalName use-concept))
      (println "Flow-Concept-Name:  " (.getLocalName flow-concept))
      (loop [choice (select-menu-option prompts num-prompts)]
	(let [action (menu choice)]
	  (when (fn? action)
	    (let [coord-map (apply action)]
	      (when (map? coord-map)
		(.show (geospace/build-coverage extent coord-map))
		(newline)
		(println "Distinct values: " (count-distinct (vals coord-map) 12)))
	      (recur (select-menu-option prompts num-prompts)))))))))
