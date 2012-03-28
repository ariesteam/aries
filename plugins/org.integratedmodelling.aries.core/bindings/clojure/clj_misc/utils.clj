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

(defn thrush [& args]
  (reduce #(%2 %1) args))
(def t> thrush)

(defn print-sysprops
  "Print out the result of System.getProperties()"
  []
  (doseq [[key val] (. System getProperties)]
    (printf "%s = %s\n" key val)))

(defmacro constraints-1.0
  [constraint-map]
  (cons 'do (for [pre-constraint (:pre constraint-map)]
              `(assert ~pre-constraint))))

;; FIXME: doesn't work because % is caught by the reader
(defmacro constraints-1.0-full
  [constraint-map & body]
  `(do
     ~@(for [pre-constraint# (:pre constraint-map)]
         `(assert ~pre-constraint#))
     (let [result# ~@body]
       ~@(for [post-constraint# (:post constraint-map)]
           `(#(assert ~post-constraint#) result#))
       result#)))

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

(defn my-partition-all
  [partition-size coll]
  (loop [remaining coll
         result    []]
    (if (empty? remaining)
      (seq result)
      (recur (drop partition-size remaining)
             (conj result (take partition-size remaining))))))

(defmacro my->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"} 
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~@(rest form)  ~x) (meta form))
              (list form x)))
  ([x form & more] `(my->> (my->> ~x ~form) ~@more)))

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
              (if-let [old-val (amap key)]
                (assoc amap key (mergefn old-val val))
                (assoc amap key val))))
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
  ([amap]
     (into {} (remove (& nil? val) amap)))
  ([nil-val amap]
     (into {} (remove (& (p = nil-val) val) amap))))

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

(defn shortest-path-bfgs
  "The classic breadth-first-graph-search.  Bread and butter of computer
   science.  Implemented using tail recursion, of course! ;)"
  [root successors goal? heuristic-filter]
  (loop [open-list  [[root]]
         closed-set #{}]
    (when-first [this-route open-list]
      (let [this-node (peek this-route)]
        (if (goal? this-node)
          this-route
          (recur (concat (rest open-list)
                         (map (p conj this-route) (heuristic-filter this-node (remove closed-set (successors this-node)))))
                 (conj closed-set this-node)))))))

(defn depth-first-tree-search
  "The classic depth-first-tree-search. Bread and butter of computer
   science. Implemented using tail recursion, of course! ;)"
  [root successors goal?]
  (loop [open-list [root]]
    (when-first [this-node open-list]
      (if (goal? this-node)
        this-node
        (recur (concat (successors this-node) (rest open-list)))))))

(defn depth-first-graph-search
  "The classic depth-first-graph-search. Bread and butter of computer
   science. Implemented using tail recursion, of course! ;)"
  [root successors goal?]
  (loop [open-list  [root]
         closed-set #{}]
    (when-first [this-node open-list]
      (if (goal? this-node)
        this-node
        (recur (concat (remove closed-set (successors this-node)) (rest open-list))
               (conj closed-set this-node))))))

(defn depth-limited-graph-search
  ([root successors goal? depth-limit]
     (depth-limited-graph-search successors goal? depth-limit [root] #{} 0))
  ([successors goal? depth-limit open-list closed-set current-depth]
     (if (seq open-list)
       (first
        (remove nil?
                (for [this-node open-list]
                  (if (goal? this-node)
                    this-node
                    (if (< current-depth depth-limit)
                      (depth-limited-graph-search successors
                                                  goal?
                                                  depth-limit
                                                  (remove closed-set (successors this-node))
                                                  (conj closed-set this-node)
                                                  (inc current-depth))))))))))

(defn iterative-deepening-graph-search
  [root successors goal?]
  (first (remove nil? (map (p depth-limited-graph-search root successors goal?) (iterate inc 1)))))

(defn shortest-path-dlgs
  ([root successors goal? depth-limit]
     (shortest-path-dlgs successors goal? depth-limit [root] #{} 0))
  ([successors goal? depth-limit open-list closed-set current-depth]
     (if (seq open-list)
       (first
        (remove nil?
                (for [this-node open-list]
                  (if (goal? this-node)
                    (list this-node)
                    (if (< current-depth depth-limit)
                      (if-let [tail (shortest-path-dlgs successors
                                                        goal?
                                                        depth-limit
                                                        (remove closed-set (successors this-node))
                                                        (conj closed-set this-node)
                                                        (inc current-depth))]
                        (cons this-node tail))))))))))

(defn shortest-path-dlgs2
  ([root successors goal? depth-limit]
     (shortest-path-dlgs successors goal? depth-limit [[root]] #{} 0))
  ([successors goal? depth-limit open-list closed-set current-depth]
     (if (seq open-list)
       (first
        (remove nil?
                (for [this-route open-list]
                  (let [this-node (peek this-route)]
                    (if (goal? this-node)
                      this-route
                      (if (< current-depth depth-limit)
                        (shortest-path-dlgs successors
                                            goal?
                                            depth-limit
                                            (map (p conj this-route) (remove closed-set (successors this-node)))
                                            (conj closed-set this-node)
                                            (inc current-depth)))))))))))

(defn shortest-path-idgs
  [root successors goal?]
  (first (remove nil? (map (p shortest-path-dlgs root successors goal?) (iterate inc 1)))))

(defn shortest-path-idgs2
  [root successors goal?]
  (let [[id depth-limit] (first
                          (remove nil?
                                  (map (fn [depth-limit]
                                         (if-let [id (depth-limited-graph-search root successors goal? depth-limit)]
                                           [id depth-limit]))
                                       (iterate inc 1))))]
    (shortest-path-dlgs root successors #{id} depth-limit)))

(defn between? [low high val] (and (>= val low) (<= val high)))

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

(defn metric-distance
  [cell-width cell-height [i1 j1] [i2 j2]]
  (euclidean-distance [(* cell-height i1) (* cell-width j1)]
                      [(* cell-height i2) (* cell-width j2)]))

(defn magnitude
  [A]
  (Math/sqrt (reduce + (map #(* % %) A))))

(defn inner-product
  [A B]
  (constraints-1.0 {:pre [A B (== (count A) (count B))]})
  (reduce + (map * A B)))

(defn to-degrees [r] (* r (/ 180.0 Math/PI)))

(defn to-radians [d] (* d (/ Math/PI 180.0)))

(defn angular-rotation
  "Given two vectors A and B, returns the directed angle between them."
  [[dy1 dx1 :as A] [dy2 dx2 :as B]]
  (- (Math/atan2 dy2 dx2) (Math/atan2 dy1 dx1)))

(defn angular-distance
  "Given two vectors A and B, returns the angle between them."
  [[dy1 dx1 :as A] [dy2 dx2 :as B]]
  (if (= A (map - B))
    ;; 180 degree turn
    Math/PI
    ;; Solve for angle C (angle opposite to side c):
    ;;   C   = acos(x.y/a2*b2)
    (Math/acos (/ (inner-product A B) (* (magnitude A) (magnitude B))))))

(defn angular-distance2
  "Given two vectors A and B, returns the angle between them."
  [[dy1 dx1 :as A] [dy2 dx2 :as B]]
  (if (= A (map - B))
    ;; 180 degree turn
    Math/PI
    (let [a2 (square-distance [0 0] B) ;; side a^2
          b2 (square-distance [0 0] A) ;; side b^2
          c2 (square-distance A B)]    ;; side c^2
      ;; Solve for angle C (angle opposite to side c):
      ;;   c^2 = a^2 + b^2 - 2ab*cosC
      ;;   C   = acos((a^2 + b^2 - c^2)/2ab)
      (Math/acos (/ (- (+ a2 b2) c2) (Math/sqrt (* 4 a2 b2)))))))

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

(defn sum [nums] (reduce + nums))

(defmacro with-progress-bar
  [body]
  `(let [result-seq# (delay ~body)]
     (doseq [_# (force result-seq#)]
       (print "*") (flush))
     (newline)
     (force result-seq#)))

(defmacro with-progress-bar*
  [step body]
  `(do
     (dorun (map (fn [v#] (print "*") (flush) v#) (take-nth ~step ~body)))
     (newline)))

;; Ex: (dotimes [i 100] (progress-bar (inc i) 100 25 \=) (Thread/sleep 500))
(defn progress-bar
  [got total width char]
  (let [width     (or width 25)
        char      (or char \=)
        num-width (count (str total))
        frac-done (/ got total)
        num-chars (Math/round (* (- width 1.0) frac-done))]
    (printf (str "|%-" width "s| Completed %" num-width "s of %s (%.1f%%)\r")
            (apply str (concat (take num-chars (repeat char)) ">")) got total (* 100.0 frac-done))
    (flush)))

(defmacro with-progress-bar-cool
  [return-behavior total body]
  (cond (= return-behavior :keep)
        `(if (pos? ~total)
           (let [step#       (max 1 (int (* 0.001 ~total)))
                 result-seq# (delay ~body)]
             (progress-bar 0 ~total 25 \=)
             (reduce (fn [done# work-chunk#]
                       (let [new-done# (+ done# (count work-chunk#))]
                         (progress-bar new-done# ~total 25 \=)
                         new-done#))
                     0
                     (my-partition-all step# (force result-seq#)))
             (force result-seq#)))
        (= return-behavior :drop)
        `(if (pos? ~total)
           (let [step#       (max 1 (int (* 0.001 ~total)))]
             (progress-bar 0 ~total 25 \=)
             (reduce (fn [done# work-chunk#]
                       (let [new-done# (+ done# (count work-chunk#))]
                         (progress-bar new-done# ~total 25 \=)
                         new-done#))
                     0
                     (my-partition-all step# ~body))
             nil))
        :otherwise
        (throw (Exception. "First input to with-progress-bar-cool must be one of :keep or :drop."))))

(defn iterate-while-seq
  [f x]
  ;;(take-while seq (iterate (& (p remove nil?) f) x)))
  (take-while seq (iterate #(remove nil? (f %)) x)))

(defmacro with-message
  [msg msg-done & body]
  `(do (print ~msg)
       (flush)
       (let [result# (do ~@body)]
         (println
          (if (fn? ~msg-done)
            (~msg-done result#)
            ~msg-done))
         result#)))

(defn reduce-true
  "Like reduce but short-circuits upon logical false"
  [f val coll]
  (when val
    (loop [val val, coll coll]
      (if (empty? coll)
        val
        (when-let [val* (f val (first coll))]
          (recur val* (rest coll)))))))

(defn successive-sums
  ([nums]
     (successive-sums (first nums) (rest nums)))
  ([total nums]
     (if (empty? nums)
       (list total)
       (lazy-cons total (successive-sums (+ total (first nums)) (rest nums))))))

(defn successive-differences
  [nums]
  (if (< (count nums) 2)
    nums
    (cons (first nums) (map - (rest nums) nums))))

(defn replace-all
  [smap form]
  (if (coll? form)
    (let [new-form (map (p replace-all smap) form)]
      (cond (list?   form) new-form
            (vector? form) (vec new-form)
            (map?    form) (into {} new-form)
            (set?    form) (set new-form)))
    (if-let [new-val (smap form)]
      new-val
      form)))

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
  "Returns a list of n numbers >= min-value, which add up to total.
   If total is a double, the summands will be doubles.  The same goes
   for integers."
  [n total min-value]
  (if (< n 2)
    (list total)
    (let [rand-fn   (if (integer? total) rand-int rand)
          min-value (if (integer? total) (int min-value) min-value)
          total     (- total (* n min-value))
          leftovers (take n (iterate rand-fn total))
          diffs     (map - leftovers (rest leftovers))]
      (map (p + min-value) (cons (reduce - total diffs) diffs)))))
