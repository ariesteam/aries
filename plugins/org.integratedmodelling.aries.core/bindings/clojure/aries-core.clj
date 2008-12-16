;; --------------------------------------------------------------------------------------
;; Basic ARIES structures and functions
;; All ARIES data structures have a counterpart in Java that can be initialized with them
;; 
;; --------------------------------------------------------------------------------------

(ns aries
	(:refer-clojure)
	(:use tl))

(load-bindings 'corescience 'geospace)

(defstruct 
	#^{:doc "to be documented"} 
	context :where :what)

(defstruct authority :username :saved-portfolio :default-where :default-what)

(defstruct scenario :evidence :models :results)

(defstruct portfolio :authority :context :scenarios)

(defn get-context 
	""
	[where what]
	(struct context where what))
	
(defn build-dependency-model 
	"Build a tree of dependencies for the specified benefit in the specified context"
	[benefit context] 
	nil)
	
(defn retrieve-evidence
	"Returns a set of available observations of the most general observables in 
	 the dependency tree"
	[dependency-model]
	nil)

(defn retrieve-all-benefits
	"Return a list with all the benefit concepts in the ontologies"
	[]
	())
	