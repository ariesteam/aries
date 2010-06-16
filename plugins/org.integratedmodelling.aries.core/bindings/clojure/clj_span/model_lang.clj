;;; This is a multipart issue: 
;;;
;;;  - Factor SPAN out of ARIES/Thinklab to build and run as an independent project (clj-span.jar). 
;;;  - Re-envision map/observation preprocessing as a series of transformations on the input data 
;;;  - Use bottom-up programming to build a declarative agent-based modelling API to replace the current flow model system 
;;;  - Port existing models to new agent-based framework (this changes their concurrency story) 
;;;  - Let carriers move from source or use points and travel to the other (think recreation) 
;;;  - Allow agents to acquire more weight as they travel (or carry multiple weights: i.e. quantity + quality) 
;;;  - Allow multiple beneficiary types (rival and non-rival) and multiple sink types in a single model run 
;;;  - Complete algorithm optimizations (see dependent issue) 
;;;  - Update documentation (README, comments, API docs) 
;;;  - Add temporal dynamics (see dependent issue) 
;;;
;;; What I need is a new framework to express the SPAN dynamics,
;;; starting from scratch.  Here how it will work:
;;;
;;; 1) The jar is loaded by a JVM, making all the namespaces available
;;;
;;; 2) A top-level main function is called (clj-span.core/main) with a
;;; set of input parameters.  These describe the incoming data sources
;;; and the flow parameters that are exposed to the user.  This
;;; function can be called from the command line or by another
;;; Java/Clojure library.  The input parameters will allow the user to
;;; state whether information is coming in as GIS layers or via
;;; Observations from Thinklab.  Random world generation should also
;;; be allowed for testing purposes.
;;;
;;; 3) A set of data transformers will be applied to massage the input
;;; types into the necessary inputs for the SPAN model (in the form of
;;; a location network).  This is essentially the world generation
;;; phase.
;;;
;;; 4) The agents are constructed and initialized in each source or
;;; use location (this should be an input param).  These agents have a
;;; service description vector (e.g. <quantity,quality>) and a route
;;; description (preferably bitpacked for space efficiency).  Both
;;; will need to be in mutable refs or atoms.
;;;
;;; 5) The behavior of the carrier agents and the source, sink, and
;;; use locations will be described by declarative rules.  A language
;;; and parser for these must be devised.  Multiple types of sinks and
;;; rival/non-rival users will be allowed in a single run.
;;;
;;; 6) The movement (and decay) rules are applied to all agents in one
;;; pass (this is the first timestep).  This is repeated over and over
;;; again until all the carrier agents' weights have dropped below the
;;; transition threshold.  Weights may increase due to some rule
;;; effects.
;;;
;;; 7) The <weight,route> pairs stored on the sink and use locations
;;; are collected into a tripartite graph structure for analysis and
;;; storage of the results in a compact format.
;;;
;;; 8) Map results are made available via the analyzer API.  These may
;;; be called on the tripartite graph directly by other Java/Clojure
;;; code or can be requested at the beginning of the model run in the
;;; input parameters.  The output format should be specified as either
;;; GIS layers, Thinklab Observations, or something else useful to the
;;; end user.
;;
;;Questions I want to answer with my model M about the system S:
;;
;;1) How much service does each cell provide to each other cell?
;;   a) How much does each cell contribute to bringing the carrier to each other cell?
;;   b) How much does each cell contribute to blocking the carrier from each other cell?
;;2) Which cells are the greatest carrier producers?
;;3) Which cells have the greatest capacity for carrier blockage?
;;4) Which cells have the greatest capacity for carrier use?
;;5) Where does each cell actually provide service?
;;6) How much carrier weight does each cell actually block and who does this affect downstream?
;;7) How much carrier weight does each cell actually rivally use and who does this affect downstream?
;;8) How much carrier weight does each cell actually nonrivally use?
;;
;;
;;Theoretical/Inacessible/Possible/Blocked/Actual Source/Sink/Use/Flow
;;
;; Carbon Sequestration
;;
;;Value 1: Money to Landowners for Carbon Credits
;;Algorithm:
;;  1) Calculate Source (sequestration) value for all cells.
;;  2) Overlay land parcel map and add up all cells per parcel.
;;  3) Multiply sequestration value per parcel by $/ton sequestered.
;;  4) Return $s per parcel
;;
;;Value 2: CO2 Absorption for Polluters
;;Algorithm:
;;  1) Calculate Source (sequestration) value for all cells in study area.
;;  2) Calculate Use (emissions) value for all cells in study area.
;;  3) Subtract Use from Source (Source - Use) to get amount either unused or overused.
;;  4) If Source = Use, we are carbon neutral.
;;  5) If Source > Use:
;;    a) Map out (Source - Use)*(Source_i / Source) for each cell i to show the distribution of overproduction.
;;    b) Map out (Use - Source)*(Use_i / Use) for each cell i to show the distribution of overuse.

(ns clj-span.model-lang
  (:use [clj-misc.utils :only (constraints-1.0)]))

(defstruct service :source :sink :user :carrier)
(defstruct source  :source-type :source-rate :source-limit)
(defstruct sink    :sink-type   :sink-rate   :sink-limit   :sink-recovery-rate)
(defstruct user    :use-type    :use-rate    :use-limit    :use-recovery-rate :rival?)
(defstruct carrier :origin :weight :movement :decay :weight-branching :min-weight)

(def value-types? #{:absolute :relative})

;; FIXME: stub needs to be finished
(defn- valid-properties?
  [type properties]
  (let [properties (into {} (map vec (partition 2 properties)))]
    (condp = type
        :source
      (let [{:keys [source-type source-rate source-limit]} properties]
        (value-types? source-type))

      :sink
      (let [{:keys [sink-type sink-rate sink-limit sink-recovery-rate]} properties]
        (value-types? sink-type))

      :user
      (let [{:keys [use-type use-rate use-limit use-recovery-rate rival?]} properties]
        (value-types? use-type))
        
      :carrier
      (let [{:keys [origin weight movement decay weight-branching min-weight]} properties]
        true)

      :service
      (let [{:keys [source sink user carrier]} properties]
        true))))

(defmacro defspan
  "Define SPAN model components."
  [type varname & forms]
  (constraints-1.0 {:pre [(or (even? (count forms)) (string? (first forms)))]})
  (let [[doc-string properties] (if (odd? (count forms))
                                  [(first forms) (rest forms)]
                                  [nil forms])]
    (assert (and (#(or (nil? %) (string? %)) doc-string)
                 (valid-properties? type properties)))
    `(def #^{:doc ~doc-string :span-role ~type} ~varname
          (struct-map ~(symbol (name type)) ~@properties))))
