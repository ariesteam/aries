;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.interface
  (:import (java.io OutputStreamWriter InputStreamReader PushbackReader))
  (:use	[misc.utils            :only (maphash count-distinct)]
	[span.params           :only (set-global-params!)]
	[span.flow-model       :only (simulate-service-flows)]
	[span.randvars         :only (unpack-datasource)]
	[span.analyzer         :only (theoretical-source
				      theoretical-sink
				      theoretical-use
				      inaccessible-source
				      inaccessible-sink
				      inaccessible-use
				      possible-flow
				      possible-source
				      possible-sink
				      possible-use
				      blocked-flow
				      blocked-source
				      blocked-sink
				      blocked-use
				      actual-flow
				      actual-source
				      actual-sink
				      actual-use)]))
(refer 'tl          :only '(get-session))
(refer 'corescience :only '(find-state
			    find-observation
			    get-state-map
			    get-observable-class))
(refer 'geospace    :only '(build-coverage
			    get-spatial-extent
			    grid-extent?
			    grid-rows
			    grid-columns))

(defn- select-menu-option
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

(defn- select-location
  "Prompts for coords and returns the corresponding location object."
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

(defn- coord-map-to-matrix
  "Renders a map of {[i j] -> value} into a 2D matrix, where value is
   either a double or a probability distribution."
  [rows cols coord-map]
  (let [matrix (make-array (class (val (first coord-map))) rows cols)]
    (doseq [[i j :as key] (keys coord-map)]
	(aset matrix i j (coord-map key)))
    matrix))

(defn- select-map-by-feature
  "Prompts for a concept available in the union of the source, sink,
   use, and flow observations, and returns a map of {[i j] -> value}
   for the one selected, where value is either a double or a
   probability distribution."
  [observation source-concept sink-concept use-concept flow-concept rows cols]
  (let [n              (* rows cols)
	feature-states (maphash (memfn getLocalName) #(unpack-datasource % n)
				(merge {source-concept (find-state observation source-concept)
					sink-concept   (find-state observation sink-concept)
					use-concept    (find-state observation use-concept)}
				       (when flow-concept
					 (get-state-map (find-observation observation flow-concept)))))
	feature-names  (vec (keys feature-states))
	num-features   (count feature-names)
	feature-name   (select-menu-option feature-names num-features)]
    (zipmap (for [i (range rows) j (range cols)] [i j])
	    (feature-states feature-name))))

(defn- view-location-properties
  "Prints a summary of the post-simulation properties of the
   location."
  [location]
  (let [fmt-str (str
		 "%nLocation %s%n"
		 "--------------------%n"
		 "Neighbors:     %s%n"
		 "Source:        %s%n"
		 "Sink:          %s%n"
		 "Use:           %s%n"
		 "Flow Features: %s%n"
		 "Carriers Encountered: %d%n")]
    (printf fmt-str
	    (:id location)
	    (:neighbors location)
	    (:source location)
	    (:sink location)
	    (:use location)
	    (:flow-features location)
	    (count @(:carrier-cache location)))))

(defn- observation-spaces-match?
  "Verifies that all observations have a grid extent and the same rows
   and cols."
  [& observations]
  (and (every? grid-extent? observations)
       (let [rows (map grid-rows observations)
	     cols (map grid-columns observations)]
         (not (or (some #(not= % (first rows)) (rest rows))
		  (some #(not= % (first cols)) (rest cols)))))))

(defn span-driver
  "Takes the source, sink, use, and flow concepts along with
   observations of their dependent features, calculates the span
   flows, and returns a map of keywords to closures, which when
   invoked, return coord-maps representing the flow analysis results."
  [observation source-concept use-concept sink-concept flow-concept flow-params]
  (set-global-params! flow-params)
  (let [rows              (grid-rows    observation)
	cols              (grid-columns observation)
	flow-concept-name (.getLocalName (get-observable-class observation))
	locations         (simulate-service-flows observation
						  source-concept
						  sink-concept
						  use-concept
						  flow-concept
						  flow-concept-name
						  rows cols)]
    {:theoretical-source  #(theoretical-source  locations)
     :theoretical-sink    #(theoretical-sink    locations)
     :theoretical-use     #(theoretical-use     locations)
     :inaccessible-source #(inaccessible-source locations)
     :inaccessible-sink   #(inaccessible-sink   locations)
     :inaccessible-use    #(inaccessible-use    locations)
     :possible-flow       #(possible-flow       locations flow-concept-name)
     :possible-source     #(possible-source     locations)
     :possible-sink       #(possible-sink       locations)
     :possible-use        #(possible-use        locations)
     :blocked-flow        #(blocked-flow        locations flow-concept-name)
     :blocked-source      #(blocked-source      locations flow-concept-name)
     :blocked-sink        #(blocked-sink        locations flow-concept-name)
     :blocked-use         #(blocked-use         locations flow-concept-name)
     :actual-flow         #(actual-flow         locations flow-concept-name)
     :actual-source       #(actual-source       locations flow-concept-name)
     :actual-sink         #(actual-sink         locations flow-concept-name)
     :actual-use          #(actual-use          locations flow-concept-name)}))

(defn span-autopilot
  "Takes the source, sink, use, and flow concepts along with
   observations of their dependent features, calculates the span
   flows, and returns a list of matrices representing the flow
   analysis results."
  [observation source-concept use-concept sink-concept flow-concept flow-params]
  (set-global-params! flow-params)
  (let [rows              (grid-rows    observation)
	cols              (grid-columns observation)
	flow-concept-name (.getLocalName (get-observable-class observation))
	locations         (simulate-service-flows observation
						  source-concept
						  sink-concept
						  use-concept
						  flow-concept
						  flow-concept-name
						  rows cols)]
    (doall (map (fn [coord-map] (coord-map-to-matrix rows cols coord-map))
		(list
		 (theoretical-source  locations)
		 (theoretical-sink    locations)
		 (theoretical-use     locations)
		 (inaccessible-source locations)
		 (inaccessible-sink   locations)
		 (inaccessible-use    locations)
		 (possible-flow       locations flow-concept-name)
		 (possible-source     locations)
		 (possible-sink       locations)
		 (possible-use        locations)
		 (blocked-flow        locations flow-concept-name)
		 (blocked-source      locations flow-concept-name)
		 (blocked-sink        locations flow-concept-name)
		 (blocked-use         locations flow-concept-name)
		 (actual-flow         locations flow-concept-name)
		 (actual-source       locations flow-concept-name)
		 (actual-sink         locations flow-concept-name)
		 (actual-use          locations flow-concept-name))))))

(defn span-interface
  "Takes the source, sink, use, and flow concepts along with
   observations of their dependent features, calculates the span
   flows, and provides a simple menu-based interface to view the
   results."
  [observation source-concept use-concept sink-concept flow-concept flow-params]
  (set-global-params! flow-params)
  (binding [*out* (OutputStreamWriter. (.getOutputStream (get-session)))
	    *in*  (PushbackReader. (InputStreamReader. (.getInputStream (get-session))))]
    (let [rows              (grid-rows          observation)
	  cols              (grid-columns       observation)
	  extent            (get-spatial-extent observation)
	  flow-concept-name (.getLocalName (get-observable-class observation))
	  locations         (simulate-service-flows observation
						    source-concept
						    sink-concept
						    use-concept
						    flow-concept
						    flow-concept-name
						    rows cols)
	  menu (array-map
		"View Theoretical Source"  #(theoretical-source       locations)
		"View Theoretical Sink"    #(theoretical-sink         locations)
		"View Theoretical Use"     #(theoretical-use          locations)
		"View Inaccessible Source" #(inaccessible-source      locations)
		"View Inaccessible Sink"   #(inaccessible-sink        locations)
		"View Inaccessible Use"    #(inaccessible-use         locations)
		"View Possible Flow"       #(possible-flow            locations flow-concept-name)
		"View Possible Source"     #(possible-source          locations)
		"View Possible Sink"       #(possible-sink            locations)
		"View Possible Use"        #(possible-use             locations)
		"View Blocked Flow"        #(blocked-flow             locations flow-concept-name)
		"View Blocked Source"      #(blocked-source           locations flow-concept-name)
		"View Blocked Sink"        #(blocked-sink             locations flow-concept-name)
		"View Blocked Use"         #(blocked-use              locations flow-concept-name)
		"View Actual Flow"         #(actual-flow              locations flow-concept-name)
		"View Actual Source"       #(actual-source            locations flow-concept-name)
		"View Actual Sink"         #(actual-sink              locations flow-concept-name)
		"View Actual Use"          #(actual-use               locations flow-concept-name)
		"View Location Properties" #(view-location-properties (select-location locations rows cols))
		"View Feature Map"         #(select-map-by-feature    observation
								      source-concept
								      sink-concept
								      use-concept
								      flow-concept
								      rows
								      cols)
		"Quit"                     nil)
	  prompts (vec (keys menu))
	  num-prompts (count prompts)]
      (println "Rows x Cols:" rows "x" cols)
      (println "Source-Concept-Name:" (.getLocalName source-concept))
      (println "Sink-Concept-Name:  " (.getLocalName sink-concept))
      (println "Use-Concept-Name:   " (.getLocalName use-concept))
      (println "Flow-Concept-Name:  " flow-concept-name)
      (loop [choice (select-menu-option prompts num-prompts)]
	(let [action (menu choice)]
	  (when (fn? action)
	    (let [coord-map (apply action)]
	      (when (map? coord-map)
		(if (instance? java.lang.Double (val (first coord-map)))
		  (.show (build-coverage extent coord-map)))
		(newline)
		(println "Distinct values:" (count-distinct (vals coord-map) 12)))
	      (recur (select-menu-option prompts num-prompts)))))))))
