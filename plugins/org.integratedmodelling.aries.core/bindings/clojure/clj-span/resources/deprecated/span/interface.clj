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
	[span.params           :only (set-global-params! *downscaling-factor*)]
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
				(merge (into {}
					     (map #(vector % (find-state observation %))
						  (remove nil? [source-concept sink-concept use-concept])))
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

(defn- store-params
  [flow-params]
  (with-open [foo (java.io.FileWriter. "/tmp/span-flow-params.txt")]
    (binding [*out* foo]
      (prn flow-params))))

(defn- upsample-results
  [result-map rows cols]
  (if (== *downscaling-factor* 1)
    result-map
    (let [offset-range  (range *downscaling-factor*)
	  row-remainder (rem  rows *downscaling-factor*)
	  col-remainder (rem  cols *downscaling-factor*)]
      (merge (into {} (mapcat (fn [[[i j] val]]
				(let [i-base (* i *downscaling-factor*)
				      j-base (* j *downscaling-factor*)]
				  (for [i-offset offset-range j-offset offset-range]
				    (let [idx [(+ i-base i-offset) (+ j-base j-offset)]]
				      [idx val]))))
			      result-map))
	     (into {} (for [i (range rows) j (range cols (+ cols col-remainder))] [[i j] nil]))
	     (into {} (for [i (range rows (+ rows row-remainder)) j (range cols)] [[i j] nil]))))))

(defmulti provide-results (fn [result-type flow-concept-name locations rows cols] result-type))

(defmethod provide-results :closure-map
  [_ flow-concept-name locations rows cols]
  {:theoretical-source  #(upsample-results (theoretical-source  locations)                   rows cols)
   :theoretical-sink    #(upsample-results (theoretical-sink    locations)                   rows cols)
   :theoretical-use     #(upsample-results (theoretical-use     locations)                   rows cols)
   :inaccessible-source #(upsample-results (inaccessible-source locations)                   rows cols)
   :inaccessible-sink   #(upsample-results (inaccessible-sink   locations)                   rows cols)
   :inaccessible-use    #(upsample-results (inaccessible-use    locations)                   rows cols)
   :possible-flow       #(upsample-results (possible-flow       locations flow-concept-name) rows cols)
   :possible-source     #(upsample-results (possible-source     locations)                   rows cols)
   :possible-sink       #(upsample-results (possible-sink       locations)                   rows cols)
   :possible-use        #(upsample-results (possible-use        locations)                   rows cols)
   :blocked-flow        #(upsample-results (blocked-flow        locations flow-concept-name) rows cols)
   :blocked-source      #(upsample-results (blocked-source      locations flow-concept-name) rows cols)
   :blocked-sink        #(upsample-results (blocked-sink        locations flow-concept-name) rows cols)
   :blocked-use         #(upsample-results (blocked-use         locations flow-concept-name) rows cols)
   :actual-flow         #(upsample-results (actual-flow         locations flow-concept-name) rows cols)
   :actual-source       #(upsample-results (actual-source       locations flow-concept-name) rows cols)
   :actual-sink         #(upsample-results (actual-sink         locations flow-concept-name) rows cols)
   :actual-use          #(upsample-results (actual-use          locations flow-concept-name) rows cols)})

(defmethod provide-results :matrix-list
  [_ flow-concept-name locations rows cols]
  (map (partial coord-map-to-matrix rows cols)
       (list
	(upsample-results (theoretical-source  locations)                   rows cols)
	(upsample-results (theoretical-sink    locations)                   rows cols)
	(upsample-results (theoretical-use     locations)                   rows cols)
	(upsample-results (inaccessible-source locations)                   rows cols)
	(upsample-results (inaccessible-sink   locations)                   rows cols)
	(upsample-results (inaccessible-use    locations)                   rows cols)
	(upsample-results (possible-flow       locations flow-concept-name) rows cols)
	(upsample-results (possible-source     locations)                   rows cols)
	(upsample-results (possible-sink       locations)                   rows cols)
	(upsample-results (possible-use        locations)                   rows cols)
	(upsample-results (blocked-flow        locations flow-concept-name) rows cols)
	(upsample-results (blocked-source      locations flow-concept-name) rows cols)
	(upsample-results (blocked-sink        locations flow-concept-name) rows cols)
	(upsample-results (blocked-use         locations flow-concept-name) rows cols)
	(upsample-results (actual-flow         locations flow-concept-name) rows cols)
	(upsample-results (actual-source       locations flow-concept-name) rows cols)
	(upsample-results (actual-sink         locations flow-concept-name) rows cols)
	(upsample-results (actual-use          locations flow-concept-name) rows cols))))

(defmethod provide-results :raw-locations
  [_ _ locations _ _]
  locations)

(defn span-driver
  "Takes the source, sink, use, and flow concepts along with the
   flow-params map and an observation containing the concepts'
   dependent features, calculates the SPAN flows, and returns the
   results using one of the following result-types:
   :closure-map :matrix-list :raw-locations"
  ([observation source-concept use-concept sink-concept flow-concept flow-params]
     (span-driver observation source-concept use-concept sink-concept flow-concept flow-params :closure-map))
  ([observation source-concept use-concept sink-concept flow-concept flow-params result-type]
     (set-global-params! flow-params)
     ;;(store-params flow-params)
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
       (provide-results result-type flow-concept-name locations rows cols))))

;; FIXME: enable printing to the thinklab shell
(defn span-cli-menu
  "Takes the source, sink, use, and flow concepts along with the
   flow-params map and an observation containing the concepts'
   dependent features, calculates the SPAN flows, and presents the
   results in a simple menu interface."
  [observation source-concept use-concept sink-concept flow-concept flow-params]
  (set-global-params! flow-params)
  ;;(store-params flow-params)
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
	prompts     (vec (keys menu))
	num-prompts (count prompts)]
    (binding [*out* (OutputStreamWriter. (.getOutputStream (get-session)))
	      *in*  (PushbackReader. (InputStreamReader. (.getInputStream (get-session))))]
      (println "Rows x Cols:" rows "x" cols)
      (println "Main-Concept-Name:  " flow-concept-name)
      (println "Source-Concept-Name:" (if source-concept (.getLocalName source-concept)))
      (println "Sink-Concept-Name:  " (if sink-concept   (.getLocalName sink-concept)))
      (println "Use-Concept-Name:   " (if use-concept    (.getLocalName use-concept)))
      (println "Flow-Concept-Name:  " (if flow-concept   (.getLocalName flow-concept)))
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
