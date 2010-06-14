(defproject org.clojars.lambdatronic/clj-span "1.0.0-alpha"
  :description "clj-span is an implementation of the Service Path
                Attribution Networks (Springer LNCS 2010 - Johnson et
                al.) framework for Ecosystem Service Assessment."
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [org.clojars.gjahad/debug-repl "0.3.0-SNAPSHOT"]]
  :dev-dependencies [[leiningen/lein-swank "1.2.0-SNAPSHOT"]
		     [lein-clojars "0.5.0-SNAPSHOT"]]
  :namespaces   [clj-span.commandline])
