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
  (:use district.gssm))

(defn view-provisionshed
  [location]
  (println "Provisionshed stuff"))

(defn view-benefitshed
  [location locations]
  (println "Benefitshed stuff"))

(defn view-location-properties
  [location]
  (println "Location property stuff"))

(defn select-menu-action
  "Prompts the user with a menu of choices and returns the number
  corresponding to their selection."
  []
  (println "Action Menu (0 quits):")
  (let [prompts ["View Provisionshed" "View Benefitshed"
		 "View Location Properties" "Count Locations"]]
    (dotimes [i (count prompts)]
	(printf " %d) %s\n" (inc i) (prompts i))))
  (print "Choice: ")
  (flush)
  (read))

(defn select-location
  [locations rows cols]
  (println "Input location coords")
  (let [coords [(or (printf "Row [0-%d]: " rows) (flush) (read))
		(or (printf "Col [0-%d]: " cols) (flush) (read))]]
    (some #(and (= (:coords %) coords) %) locations)))

(defn gssm-interface
  "Takes a benefit and an observation of the relevant features
   (currently a map of {feature -> 1D matrix}), calculates the gssm
   flows, and provides a simple menu-based interface to view the
   results."
  [benefit observation rows cols trans-threshold]
;;  (let [locations (simulate-service-flows benefit observation rows
;;					  cols trans-threshold)]
  (let [locations '({:coords [0 1]} {:coords [2 1]} {:coords [3 2]} {:coords [0 0]})]
    (loop [choice (select-menu-action)]
      (when (not= choice 0)
	(cond (== choice 1) (view-provisionshed (select-location locations rows cols))
	      (== choice 2) (view-benefitshed (select-location locations rows cols) locations)
	      (== choice 3) (view-location-properties (select-location locations rows cols))
	      (== choice 4) (println (count locations))
	      :otherwise    (println "Invalid selection."))
	(recur (select-menu-action))))))
