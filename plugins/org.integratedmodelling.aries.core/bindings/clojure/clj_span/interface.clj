;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-span.
;;;
;;; clj-span is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-span is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-span.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines the text-based menu interface for viewing
;;; the results of a SPAN model run as well as the raw data return
;;; functions.

(ns clj-span.interface
  (:use [clj-misc.varprop    :only (_0_)]
        [clj-misc.utils      :only (& p mapmap)]
        [clj-misc.matrix-ops :only (matrix2seq matrix2coord-map print-matrix get-rows get-cols in-bounds?)]))

(defn- select-location
  "Prompts for coords and returns the selected [i j] pair."
  [rows cols]
  (loop []
    (printf "%nInput location coords%n")
    (let [coords [(do (printf "Row [0-%d]: " (dec rows)) (flush) (read))
                  (do (printf "Col [0-%d]: " (dec cols)) (flush) (read))]]
      (if (in-bounds? rows cols coords)
        coords
        (do (printf "No location at %s. Enter another selection.%n" coords)
            (recur))))))

(defn- view-location-properties
  "Prints a summary of the post-simulation properties of the
   location."
  [coords source-layer sink-layer use-layer flow-layers]
  (let [fmt-str (str
                 "%nLocation %s%n"
                 "--------------------%n"
                 "Source:        %s%n"
                 "Sink:          %s%n"
                 "Use:           %s%n"
                 "Flow Features: %s%n")]
    (printf fmt-str
            coords
            (get-in source-layer coords)
            (get-in sink-layer   coords)
            (get-in use-layer    coords)
            (mapmap identity #(get-in % coords) flow-layers))))

(defn- select-menu-option
  "Prompts the user with a menu of choices and returns the label
   corresponding to their selection."
  [prompt-list]
  (let [prompts       (vec prompt-list)
        num-prompts   (count prompts)
        index-padding (count (str num-prompts))]
    (loop []
      (printf "%nOptions Menu:%n")
      (dotimes [i num-prompts]
        (printf (str " %" index-padding "d) %s%n") (inc i) (prompts i)))
      (print "Choice: ")
      (flush)
      (let [choice (read)]
        (if (and (integer? choice) (> choice 0) (<= choice num-prompts))
          (prompts (dec choice))
          (do (println "Invalid selection. Please choose a number from the menu.")
              (recur)))))))

(defn- select-map-by-feature
  "Prompts for a feature available in the union of the source, sink,
   use, and flow layers, and returns a map of {[i j] -> value} for the
   one selected, where value is either a double or a probability
   distribution."
  [source-layer sink-layer use-layer flow-layers]
  (let [feature-names    (list* "Source" "Sink" "Use" (keys flow-layers))
        selected-feature (select-menu-option feature-names)]
    ((-> flow-layers
         (assoc "Source" source-layer)
         (assoc "Sink"   sink-layer)
         (assoc "Use"    use-layer))
     selected-feature)))

(defmulti provide-results
  ;;"Returns the results to the caller according to the requested result-type:
  ;;
  ;;  :cli-menu = Provides a command-line driven menu system, which
  ;;              allows the user to view the result maps as matrices
  ;;              along with a report on their number of distinct
  ;;              values. Additional actions include viewing all
  ;;              properties of a particular location and viewing the
  ;;              input source, sink, use, and flow-layers as matrices.
  ;;
  ;;  :closure-map = Transforms the results-menu into the form expected
  ;;                 by ARIES.  That is, labels (keys) in the map are
  ;;                 converted from mixed-case strings to lowercase
  ;;                 keywords and matrices (vals) are transformed into
  ;;                 maps of {[i j] -> RV}, excluding all locations
  ;;                 whose values equal _0_."
  (fn [result-type source-layer sink-layer use-layer flow-layers results-menu] result-type))

(defmethod provide-results :cli-menu
  [_ source-layer sink-layer use-layer flow-layers results-menu]
  (let [rows        (get-rows source-layer)
        cols        (get-cols source-layer)
        menu-extras (array-map
                     "Location Properties"
                     #(view-location-properties (select-location rows cols) source-layer sink-layer use-layer flow-layers)
                     "Input Features"
                     #(select-map-by-feature source-layer sink-layer use-layer flow-layers)
                     "Quit"
                     nil)
        menu        (apply array-map (apply concat (concat results-menu menu-extras)))
        prompts     (keys menu)]
    (loop [action (menu (select-menu-option prompts))]
      (when action
        (when-let [matrix-result (action)]
          (newline)
          (print-matrix matrix-result)
          (newline)
          (println "Distinct values:" (count (distinct (matrix2seq matrix-result)))))
        (recur (menu (select-menu-option prompts)))))))

(defmethod provide-results :closure-map
  [_ _ _ _ _ results-menu]
  (println "Returning the results map to Ferd's code.")
  (mapmap
   (fn [label]
     (let [[_ word1 word2] (re-find #"(\w+)\s+-\s+(\w+)" label)]
       (keyword (str (.toLowerCase word2) \- (.toLowerCase word1)))))
   (fn [closure] (& (p matrix2coord-map _0_) closure))
   results-menu))
