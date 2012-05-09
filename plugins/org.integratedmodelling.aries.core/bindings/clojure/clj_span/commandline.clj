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
;;; This namespace is meant to be compiled as a Java class and called
;;; from the command line with its arguments passed as strings.  It
;;; validates all the inputs, and gives helpful usage messages if
;;; there are errors.  If everything checks out, it converts them to a
;;; map of {keywords -> native clojure types} and passes this to the
;;; clj-span.core/run-span function, requesting a command line menu
;;; for the results.

(ns clj-span.commandline
  (:gen-class)
  (:use [clj-span.core     :only (run-span)]
        [clj-misc.utils    :only (def- &)]
        [clj-span.worldgen :only (read-layer-from-file)]
        [clojure.string    :only (join)]
        [clojure.set       :only (difference) :as set]
        [clojure.java.io   :only (file) :as io]))

(def- usage-message
  (str
   "Usage: java -cp clj-span-standalone.jar clj_span.commandline \\ \n"
   "            -config-file        <filepath>                 \\ \n"
   "            -source-layer       <filepath>                 \\ \n"
   "            -sink-layer         <filepath>                 \\ \n"
   "            -use-layer          <filepath>                 \\ \n"
   "            -flow-layers        <filepath>                 \\ \n"
   "            -source-threshold   <double>                   \\ \n"
   "            -sink-threshold     <double>                   \\ \n"
   "            -use-threshold      <double>                   \\ \n"
   "            -trans-threshold    <double>                   \\ \n"
   "            -cell-width         <double>                   \\ \n"
   "            -cell-height        <double>                   \\ \n"
   "            -rv-max-states      <integer>                  \\ \n"
   "            -downscaling-factor <number>                   \\ \n"
   "            -source-type        <finite|infinite>          \\ \n"
   "            -sink-type          <finite|infinite>          \\ \n"
   "            -use-type           <finite|infinite>          \\ \n"
   "            -benefit-type       <rival|non-rival>          \\ \n"
   "            -value-type         <numbers|varprop|randvars> \\ \n"
   "            -animation?         <true|false>      \\ \n"
   "            -flow-model         <line-of-sight|proximity|carbon|flood-water|surface-water|sediment|coastal-storm-protection|subsistence-fisheries> \n"))

(def- param-tests
  [["-source-layer"       #(.canRead  (io/file (io/resource %))) " is not readable."                             ]
   ["-sink-layer"         #(.canRead  (io/file (io/resource %))) " is not readable."                             ]
   ["-use-layer"          #(.canRead  (io/file (io/resource %))) " is not readable."                             ]
   ["-flow-layers"        #(.canRead  (io/file (io/resource %))) " is not readable."                             ]
   ["-source-threshold"   (& float?   read-string)               " is not a double."                             ]
   ["-sink-threshold"     (& float?   read-string)               " is not a double."                             ]
   ["-use-threshold"      (& float?   read-string)               " is not a double."                             ]
   ["-trans-threshold"    (& float?   read-string)               " is not a double."                             ]
   ["-cell-width"         (& float?   read-string)               " is not a double."                             ]
   ["-cell-height"        (& float?   read-string)               " is not a double."                             ]
   ["-rv-max-states"      (& integer? read-string)               " is not an integer."                           ]
   ["-downscaling-factor" (& number?  read-string)               " is not a number."                             ]
   ["-source-type"        #{"finite" "infinite"}                 " must be one of finite or infinite."           ]
   ["-sink-type"          #{"finite" "infinite"}                 " must be one of finite or infinite."           ]
   ["-use-type"           #{"finite" "infinite"}                 " must be one of finite or infinite."           ]
   ["-benefit-type"       #{"rival" "non-rival"}                 " must be one of rival or non-rival."           ]
   ["-value-type"         #{"numbers" "varprop" "randvars"}      " must be one of numbers, varprop, or randvars."]
   ["-animation?"         #{"true" "false"}                      " must be one of true or false."                ]
   ["-flow-model"         #{"line-of-sight" "proximity" "carbon" "flood-water" "surface-water" "sediment" "coastal-storm-protection" "subsistence-fisheries"}
    " must be one of line-of-sight, proximity, carbon, flood-water, surface-water, sediment, coastal-storm-protection, or subsistence-fisheries."]])

(defn- non-existent-params
  [params param-tests]
  (map #(str % " is not a valid parameter name.")
       (set/difference (set (keys params))
                       (set (map first param-tests)))))

(defn- missing-or-bad-values
  [params param-tests]
  (remove nil?
          (for [[name test? error-suffix] param-tests :let [value (params name)]]
            (cond (nil? value)        (str "No value provided for " name)
                  (not (test? value)) (str value error-suffix)))))

(defn- collect-input-errors
  "Returns a seq of error messages if the params map does not:
     1) Include all parameter names in param-tests.
     2) Provide valid values for all parameters in param-tests.
     3) Contain parameter names not in param-tests."
  [params]
  (concat (non-existent-params   params param-tests)
          (missing-or-bad-values params param-tests)))

(defn- strings-to-better-types
  "Converts params from a map of {strings -> strings} into a map
   of {keywords -> native clojure types}."
  [params]
  {:source-layer       (read-layer-from-file (params "-source-layer"))
   :sink-layer         (read-layer-from-file (params "-sink-layer"))
   :use-layer          (read-layer-from-file (params "-use-layer"))
   :flow-layers        (read-layer-from-file (params "-flow-layers"))
   :source-threshold   (read-string (params "-source-threshold"))
   :sink-threshold     (read-string (params "-sink-threshold"))
   :use-threshold      (read-string (params "-use-threshold"))
   :trans-threshold    (read-string (params "-trans-threshold"))
   :cell-width         (read-string (params "-cell-width"))
   :cell-height        (read-string (params "-cell-height"))
   :rv-max-states      (read-string (params "-rv-max-states"))
   :downscaling-factor (read-string (params "-downscaling-factor"))
   :source-type        (keyword (params "-source-type"))
   :sink-type          (keyword (params "-sink-type"))
   :use-type           (keyword (params "-use-type"))
   :benefit-type       (keyword (params "-benefit-type"))
   :value-type         (keyword (params "-value-type"))
   :animation?         (read-string (params "-animation?"))
   :flow-model         ({"line-of-sight"            "LineOfSight"
                         "proximity"                "Proximity"
                         "carbon"                   "CO2Removed"
                         "flood-water"              "FloodWaterMovement"
                         "surface-water"            "SurfaceWaterMovement"
                         "sediment"                 "SedimentTransport"
                         "coastal-storm-protection" "CoastalStormMovement"
                         "subsistence-fisheries"    "SubsistenceFishAccessibility"}
                        (params "-flow-model"))})

(defn read-config-file
  [filename]
  (if filename
    (let [config-file-params (read-layer-from-file filename)]
      (if (and (map? config-file-params) (every? string? (concat (keys config-file-params) (vals config-file-params))))
        config-file-params
        (println (str "\nError: The config-file must contain a clojure map of quoted strings to quoted strings.\n\n" usage-message))))))

(defn -main
  "The compiled Java class' main method.  Pass it all the SPAN inputs
   as -key value argument pairs from the command line.  After
   validating your inputs and running the flow simulation, it will
   present a text-based menu to view the model results."
  [& args]
  (if (empty? args)
    (println "\n" usage-message)
    ;; args contains -key value pairs, so must have an even number of entries.
    (if (odd? (count args))
      (println (str "\nError: The number of input arguments must be even.\n\n" usage-message))
      ;; Store the args list in a map and validate it.
      (let [command-line-params (into {} (map vec (partition 2 args)))
            config-file-params  (read-config-file (command-line-params "-config-file"))
            params              (dissoc (merge config-file-params command-line-params) "-config-file")]
        (if-let [error-msgs (seq (collect-input-errors params))]
          (println (str "\nError: The parameter values that you entered are incorrect.\n\t"
                        (join "\n\t" error-msgs)
                        "\n\n"
                        usage-message))
          (do
            (println "\nAll inputs are valid.\n")
            (doseq [[name _ _] param-tests] (println (find params name)))
            (newline)
            ;; Run the SPAN simulation.
            (run-span (assoc (strings-to-better-types params) :result-type :cli-menu))
            ;; Exit cleanly.
            (shutdown-agents)
            (flush)
            (System/exit 0)))))))
