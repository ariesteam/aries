;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-misc.
;;;
;;; clj-misc is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-misc is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-misc.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines miscellaneous functions.  These should be
;;; moved at some point into more specific utility libraries.

(ns clj-misc.utils
  (:import (java.util HashMap)))

;; Some useful abbreviations for point-free style.
(def p partial)
(def & comp)

(defn print-sysprops
  "Print out the result of System.getProperties()"
  []
  (doseq [[key val] (. System getProperties)]
    (printf "%s = %s\n" key val)))

(defmacro constraints-1.0
  [constraint-map]
  (cons 'do (for [pre-constraint (:pre constraint-map)]
              `(assert ~pre-constraint))))

(defmacro def-
  [name & args]
  (let [name# (with-meta name {:private true})]
    `(def ~name# ~@args)))

(defmacro defmulti-
  [name & args]
  (let [name# (with-meta name {:private true})]
    `(defmulti ~name# ~@args)))

(defn dissoc-seq
  [idx s]
  (constraints-1.0 {:pre [(>= idx 0) (< idx (count s))]})
  (let [split-seq (split-at idx s)]
    (concat (first split-seq) (rest (second split-seq)))))

(defn dissoc-vec
  [idx v]
  (vec (dissoc-seq idx v)))

(defn select-n-distinct
  [n coll]
  (if (>= n (count coll))
    (vec coll)
    (first
     (nth (iterate (fn [[picks opts]]
                     (let [idx (rand-int (count opts))]
                       [(conj picks (opts idx))
                        (dissoc-vec idx opts)]))
                   [[] (vec coll)])
          n))))

(defn select-n-summands
  "Returns a vector of n numbers which add up to total.  If total is a
   double, the summands will be doubles.  The same goes for integers."
  [n total]
  (if (< n 2)
    (list total)
    (let [rand-fn  (if (integer? total) rand-int rand)
          summands (nth (iterate #(conj % (rand-fn (reduce - total %)))
                                 [(rand-fn total)])
                        (- n 2))]
      (conj summands (reduce - total summands)))))

(defn my-partition-all
  [partition-size coll]
  (loop [remaining coll
         result    []]
    (if (empty? remaining)
      (seq result)
      (recur (drop partition-size remaining)
             (conj result (take partition-size remaining))))))

(defn add-anyway
  "Sums the non-nil argument values."
  [x y]
  (cond (nil? x) y
        (nil? y) x
        :otherwise (+ x y)))

(defn seq2map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element."
  [aseq keyvalfn]
  (into {} (map keyvalfn aseq)))

(defn seq2redundant-map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element.  If key is
   already in the map, its current value will be combined with the new
   val using (mergefn curval val)."
  [aseq keyvalfn mergefn]
  (reduce (fn [amap x]
            (let [[key val] (keyvalfn x)]
              (update-in amap [key] mergefn val)))
          {}
          aseq))

(defn mapmap
  "Creates a new map by applying keyfn to every key of in-map and
   valfn to every corresponding val."
  [keyfn valfn in-map]
  (into {} (map (fn [[k v]] [(keyfn k) (valfn v)]) in-map)))

(defn mapmap-java
  "Creates a new Java map by applying keyfn to every key of in-map and
   valfn to every corresponding val."
  [keyfn valfn in-map]
  (loop [keyvals (seq in-map)
         out-map (new HashMap)]
    (if (empty? keyvals)
      out-map
      (let [[key val] (first keyvals)]
        (recur (rest keyvals)
               (doto out-map (.put (keyfn key) (valfn val))))))))

(defn remove-nil-val-entries
  [amap]
  (into {} (remove (& nil? val) amap)))

(defn key-by-val
  "Returns the key from a map m whose corresponding value field is a
   sequence containing v."
  [m v]
  (some #(and (some #{v} (val %)) (key %)) m))

(defn linearize
  "Transforms a 2D matrix into a 1D vector."
  [matrix]
  (vec (mapcat identity matrix)))

(defn vectorize
  "Creates a vect of vects from a 2D Java array."
  [java-array]
  (into [] (map (p into []) java-array)))

(defn arrayify
  "Creates a 2D Java array (of Objects) from a vect of vects."
  [vect-of-vects]
  (into-array (map into-array vect-of-vects)))

(defn vectorize-map
  "Creates a map of {keywords -> vect-of-vects} from a Java
   HashMap<String,Array[]>."
  [java-map]
  (mapmap keyword vectorize java-map))

(defn arrayify-map
  "Creates a Java HashMap<String,Array[]> from a map of {keywords ->
   vect-of-vects}."
  [clojure-map]
  (mapmap-java name arrayify clojure-map))

(defn multi-conj
  "Conjoins an element multiple times onto a base-coll."
  [element times base-coll]
  (nth (iterate #(conj % element) base-coll) times))

(defn expand-runtime-encoded-vector
  "Expands a vector of the form [:foo 2 :bar 1 :baz 3] into
   [:foo :foo :bar :baz :baz :baz]."
  [avec]
  (constraints-1.0 {:pre [(even? (count avec))]})
  (loop [orig-vec avec
         expanded-vec []]
    (if (empty? orig-vec)
      expanded-vec
      (recur (drop 2 orig-vec)
             (multi-conj (first orig-vec) (second orig-vec) expanded-vec)))))

(defn contains-item?
  "Returns true if sequence contains item.  Otherwise nil."
  [sequence item]
  (some (p = item) sequence))

(defn breadth-first-search
  "The classic breadth-first-search.  Bread and butter of computer
   science.  Implemented using tail recursion, of course! ;)"
  [open-list closed-set successors goal?]
  (loop [open-list  open-list
         closed-set closed-set]
    (when-first [this-node open-list]
                (if (contains? closed-set this-node)
                  (recur (rest open-list) closed-set)
                  (if (goal? this-node)
                    this-node
                    (recur (concat (rest open-list)
                                   (filter (complement closed-set) (successors this-node)))
                           (conj closed-set this-node)))))))

(defn depth-first-tree-search
  "The classic depth-first-tree-search.  Bread and butter of computer
   science.  Implemented using tail recursion, of course! ;)"
  [open-list successors goal?]
  (loop [open-list open-list]
    (when-first [this-node open-list]
                (if (goal? this-node)
                  this-node
                  (recur (concat (successors this-node) (rest open-list)))))))

(defn between [val low high] (and (>= val low) (< val high)))

(defn manhattan-distance-2
  "Returns the manhattan distance between two 2-dimensional points."
  [[i1 j1] [i2 j2]]
  (+ (Math/abs (- i1 i2)) (Math/abs (- j1 j2))))

(defn manhattan-distance
  "Returns the manhattan distance between two n-dimensional points."
  [pointA pointB]
  (constraints-1.0 {:pre [pointA pointB (== (count pointA) (count pointB))]})
  (reduce + (map (fn [a b] (Math/abs (- a b))) pointA pointB)))

(defn square-distance
  [pointA pointB]
  (constraints-1.0 {:pre [pointA pointB (== (count pointA) (count pointB))]})
  (reduce + (map (fn [a b] (Math/pow (- a b) 2)) pointA pointB)))

(defn euclidean-distance
  "Returns the euclidean distance between two n-dimensional points."
  [pointA pointB]
  (Math/sqrt (square-distance pointA pointB)))

(defn count-distinct
  "Returns a map of {distinct-val -> num-instances, ...} for all the
   distinct values in a sequence.  If n is given, only count the first
   n distinct values and append {... -> num-distinct - n} to the map
   to indicate that more values were not examined."
  ([vals]
     (seq2map
      (distinct vals)
      (fn [val] [val (count (filter (p = val) vals))])))
  ([vals n]
     (let [d-vals       (distinct vals)
           num-distinct (count d-vals)]
       (assoc
           (seq2map
            (take n d-vals)
            (fn [val] [val (count (filter (p = val) vals))]))
         "..." (- d-vals n)))))

(defn memoize-by-first-arg
  [function]
  (let [cache (atom {})]
    (fn [& args]
      (or (@cache (first args))
          (let [result (apply function args)]
            (swap! cache assoc (first args) result)
            result)))))

(defn distribute-load-over-processors
  [action-fn arg-seq]
  (let [num-processors (.availableProcessors (Runtime/getRuntime))
        agents (map agent (replicate (+ 2 num-processors) ()))]
    (println "Sending Tasks to" (count agents) "Agents...")
    (dorun (map #(send %1 action-fn %2) (cycle agents) arg-seq))
    (println "Waiting for Agents to Finish...")
    (apply await agents)))
