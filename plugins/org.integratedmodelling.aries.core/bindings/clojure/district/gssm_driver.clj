;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-DISTRICT.
;;;
;;; CLJ-DISTRICT is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; CLJ-DISTRICT is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-DISTRICT.  If not, see
;;; <http://www.gnu.org/licenses/>.

(ns district.gssm-driver
  (:refer-clojure)
  (:use district.gssm
	[district.matrix-ops :only (print-matrix)]))

(defn view-provisionshed
  "Prints a matrix representation of the location's provisionshed."
  [location rows cols]
  (print-matrix
   (coord-map-to-matrix (find-provisionshed location) rows cols)
   "%5s "))

(defn view-benefitshed
  "Prints a matrix representation of the location's benefitshed."
  [location locations rows cols]
  (print-matrix
   (coord-map-to-matrix (find-benefitshed location locations) rows cols)
   "%5s "))

(defn view-location-properties
  "Prints a summary of the post-simulation properties of the
  location."
  [location]
  (let [fmt-str (concat
		 "Location %s\n"
		 "--------------------\n"
		 "Neighbors: %d\n"
		 "Sunk: %d\n"
		 "Used: %d\n"
		 "Consumed: %d\n"
		 "Carriers Encountered: %d\n"
		 "Source-Val: %d\n"
		 "Sink-Prob: %d\n"
		 "Use-Prob: %d\n"
		 "Consume-Prob: %d\n")
	flows (force (:flows location))]
    (printf fmt-str
	    (:coords location)
	    (count (:neighbors location))
	    @(:sunk location)
	    @(:used location)
	    @(:consumed location)
	    (count @(:carrier-bin location))
	    (:source location)
	    (:sink flows)
	    (:use flows)
	    (:consume flows))))

(defn select-menu-action
  "Prompts the user with a menu of choices and returns the number
  corresponding to their selection."
  []
  (println "\nAction Menu (0 quits):")
  (let [prompts ["View Provisionshed" "View Benefitshed"
		 "View Location Properties" "Count Locations"]]
    (dotimes [i (count prompts)]
	(printf " %d) %s\n" (inc i) (prompts i))))
  (print "Choice: ")
  (flush)
  (read))

(defn select-location
  "Prompts for coords and returns the cooresponding location object."
  [locations rows cols]
  (println "Input location coords")
  (let [coords [(do (printf "Row [0-%d]: " (dec rows)) (flush) (read))
		(do (printf "Col [0-%d]: " (dec cols)) (flush) (read))]]
    (some #(and (= (:coords %) coords) %) locations)))

(defn gssm-interface
  "Takes a benefit and an observation of the relevant features
   (currently a map of {feature -> 1D matrix}), calculates the gssm
   flows, and provides a simple menu-based interface to view the
   results."
  [benefit observation rows cols trans-threshold]
  (let [locations (simulate-service-flows benefit observation rows
					  cols trans-threshold)]
    (loop [choice (select-menu-action)]
      (when (not= choice 0)
	(cond (== choice 1) (view-provisionshed
			     (select-location locations rows cols) rows cols)
	      (== choice 2) (view-benefitshed
			     (select-location locations rows cols) locations rows cols)
	      (== choice 3) (view-location-properties
			     (select-location locations rows cols))
	      (== choice 4) (println (count locations))
	      :otherwise    (println "Invalid selection."))
	(recur (select-menu-action))))))
