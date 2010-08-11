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
;;; This namespace defines functions for creating, querying, and
;;; manipulating random variables, which are defined to be maps of
;;; {state -> probability}.  Both discrete and continuous random
;;; variables are supported.  Continuous RVs are represented using
;;; samples from their cumulative distribution functions (CDFs).
;;;
;;; This library also provides some shorthand abbreviations for many
;;; of the arithmetic functions:
;;;
;;; _0_ rv-zero
;;; _+_ rv-add
;;; _-_ rv-subtract
;;; _*_ rv-multiply
;;; _d_ rv-divide
;;; _<_ rv-lt
;;; _>_ rv-gt
;;;  +_ scalar-rv-add
;;;  -_ scalar-rv-subtract
;;;  *_ scalar-rv-multiply
;;;  d_ scalar-rv-divide
;;; _+  rv-scalar-add
;;; _-  rv-scalar-subtract
;;; _*  rv-scalar-multiply
;;; _d  rv-scalar-divide

(ns clj-misc.randvars
  (:refer-clojure :exclude (type))
  (:use [clj-misc.utils :only (p my-partition-all constraints-1.0 mapmap seq2map dissoc-vec)]))

(def type (comp :type meta))

(def *rv-max-states* 10)
(defn reset-rv-max-states!
  [new-val]
  (if (and (pos? new-val) (integer? new-val))
    (alter-var-root #'*rv-max-states* (constantly new-val))))

(def cont-type {:type ::continuous-distribution})
(def disc-type {:type ::discrete-distribution})

(defn successive-sums
  ([nums]
     (successive-sums (first nums) (rest nums)))
  ([total nums]
     (if (empty? nums)
       (list total)
;;       (lazy-seq (cons total (successive-sums (+ total (first nums)) (rest nums)))))))
       (lazy-cons total (successive-sums (+ total (first nums)) (rest nums))))))

(defn successive-differences
  [nums]
  (if (< (count nums) 2)
    nums
    (cons (first nums) (map - (rest nums) nums))))

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

(defmulti to-discrete-randvar type)

(defmethod to-discrete-randvar ::continuous-distribution
  [continuous-RV]
  (with-meta
    (let [sorted-RV (sort continuous-RV)]
      (zipmap (keys sorted-RV)
              (successive-differences (vals sorted-RV))))
    disc-type))

(defmethod to-discrete-randvar ::discrete-distribution [discrete-RV] discrete-RV)

(defmulti to-continuous-randvar type)

(defmethod to-continuous-randvar ::discrete-distribution
  [discrete-RV]
  (with-meta
    (let [sorted-RV (sort discrete-RV)]
      (zipmap (keys sorted-RV)
              (successive-sums (vals sorted-RV))))
    cont-type))

(defmethod to-continuous-randvar ::continuous-distribution [continuous-RV] continuous-RV)
  
(defn make-randvar
  [rv-type num-states valid-states]
  (constraints-1.0 {:pre [(#{:discrete :continuous} rv-type)]})
  (let [discrete-RV (with-meta
                      (zipmap (map rationalize  (select-n-distinct num-states valid-states))
                              (map #(/ % 100.0) (select-n-summands num-states 100 1)))
                      disc-type)]
    (if (= rv-type :discrete)
      discrete-RV
      (to-continuous-randvar discrete-RV))))

(defmulti rv-cdf-lookup
  ;;"Return F_X(x) = P(X<x)."
  (fn [X x] (type X)))

(defmethod rv-cdf-lookup ::discrete-distribution
  [X x]
  (let [X* (to-continuous-randvar X)]
    (or (X* x)
        (if-let [below-x (seq (filter #(< % x) (keys X*)))]
          (X* (apply max below-x))
          0.0))))

(defmethod rv-cdf-lookup ::continuous-distribution
  [X x]
  (or (X x)
      (if-let [below-x (seq (filter #(< % x) (keys X)))]
        (X (apply max below-x))
        0.0)))

(defn- sum-discrepancy
  ([[p1 p2]]
     (Math/abs (- (apply + p1) (apply + p2))))
  ([f [p1 p2]]
     (Math/abs (- (apply + (map f p1)) (apply + (map f p2))))))

(defn minimum-discrepancy-partition
  "Given a sequence of sorted values, partition them into two
   sequences (preserving their order), so as to minimize the
   difference between their sums.  If an optional function f is passed
   it will be applied to the values before they are summed."
  ([values]
     (let [num-values (count values)]
       (if (< num-values 2)
         (list values)
         (let [partitions         (for [i (range 1 num-values)]
                                    [(take i values) (drop i values)])
               diffs-to-partitions (zipmap (map sum-discrepancy partitions)
                                           partitions)]
           (diffs-to-partitions (apply min (keys diffs-to-partitions)))))))
  ([f values]
     (let [num-values (count values)]
       (if (< num-values 2)
         (list values)
         (let [partitions          (for [i (range 1 num-values)] (split-at i values))
               diffs-to-partitions (zipmap (map (p sum-discrepancy f) partitions)
                                           partitions)]
           (diffs-to-partitions (apply min (keys diffs-to-partitions))))))))

(defn partition-by-probs
  "Given a random variable X, returns a partition of its states which
   attempts to minimize the difference between each partition's total
   probability."
  [max-partitions X]
  (if-not (> (count X) max-partitions)
    (map list X)
    ;; hmm...this is a variation on the Number Partitioning Problem
    ;; (NPP), which is NP-complete. Scheisse.  I'll apply the
    ;; differencing algorithm repeatedly to successively smaller
    ;; partitions until the total number is reached.
    (let [X*           (sort X)
          search-depth (int (/ (Math/log max-partitions) (Math/log 2)))]
      ;;(nth (iterate #(mapcat (fn [p] (minimum-discrepancy-partition val p)) %)
      (nth (iterate (p mapcat (p minimum-discrepancy-partition val))
                    (list X*))
           search-depth))))

(defmulti rv-resample
  ;;"Returns a new random variable with <=*rv-max-states* states sampled from X."
  type)

;;(defmethod rv-resample ::discrete-distribution
;;  [X]
;;  (if (<= (count X) *rv-max-states*)
;;    X
;;    (with-meta
;;      (seq2map (partition-by-probs *rv-max-states* X)
;;               #(vector (/ (apply + (keys %)) (count %))
;;                        (apply + (vals %))))
;;      (meta X))))

(defmethod rv-resample ::discrete-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [partition-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta
        (seq2map (my-partition-all partition-size (sort X))
                 #(vector (/ (apply + (keys %)) (count %))
                          (apply + (vals %))))
        (meta X)))))

(defmethod rv-resample ::continuous-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [step-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta (into {} (take-nth step-size (sort X))) (meta X)))))

(defmulti rv-mean
  ;;"Returns the mean value of a random variable X."
  type)

(defmethod rv-mean ::discrete-distribution
  [X]
  (reduce + (map (p apply *) X)))

;; This returns the average of the upper and lower bounds for the mean
(defmethod rv-mean ::continuous-distribution
  [X]
  (let [X*     (sort-by key X)
        states (keys X*)
        probs  (successive-differences (vals X*))
        upper  (reduce + (map * states probs))
        lower  (reduce + (map * states (rest probs)))]
    (/ (+ upper lower) 2)))

(def rv-zero (with-meta (array-map 0 1.0) disc-type))
(def _0_ rv-zero)

;;; testing functions below here

;; FIXME: Continuous convolutions always assume the lower bound
;; interpretation of the stepwise CDFs. We should probably average all
;; upper and lowers (or just use discrete values for everything?)

;; FIXME: These convolution functions also all assume independence
;; between X and Y. We need more information from ARIES to ascertain
;; this (or some way of representing this dependence/covariance).

(defmulti rv-convolute* (fn [f X Y] [(type X) (type Y)]))

;; P(X+Y=z) = \sum_{x \in X} \sum_{y \in Y} I(x+y=z) P(x) P(y)
(defmethod rv-convolute* [::discrete-distribution ::discrete-distribution]
  [f X Y]
  (apply merge-with + (for [[v1 p1] X [v2 p2] Y] {(f v1 v2) (* p1 p2)})))

;; F(Z) = P(X+Y<z) = \int_{-\infinity}^{\infinity} dy \sum_{x \in X} I(x+y<=z) P(x) p(y)
;;      = \sum_{x \in X} P_X(x) F_Y(z-x)
(defmethod rv-convolute* [::discrete-distribution ::continuous-distribution]
  [f X Y]
  (apply merge-with + (for [[v1 p1] X [v2 p2] Y] {(f v1 v2) (* p1 p2)})))

;; F(Z) = P(X+Y<z) = \int_{-\infinity}^{\infinity} dx \sum_{y \in Y} I(x+y<=z) p(x) P(y)
;;      = \sum_{y \in Y} P_Y(y) F_X(z-y)
(defmethod rv-convolute* [::continuous-distribution ::discrete-distribution]
  [f X Y]
  (apply merge-with + (for [[v1 p1] X [v2 p2] Y] {(f v1 v2) (* p1 p2)})))

;; F(Z) = P(X+Y<z) = \int_{-\infinity}^{\infinity} dx \int_{-\infinity}^{\infinity} dy I(x+y<=z) p(x) p(y)
;;      = \int_{-\infinity}^{\infinity} p(x) dx \int_{-\infinity}^{z-x} p(y) dy
;;      = \int_{-\infinity}^{\infinity} p(x) dx F_Y(z-x)
;;      = \int_{-\infinity}^{\infinity} F_Y(z-x) dF_X(x)
;;
;; where dF_X(x) for these stepwise interpretations of the CDFs can be
;; interpreted as 0 everywhere except where we have a sample value in
;; the RV map, in which case dF_X(x) = F_X(x_i)-F_X(x_i-1).  This also
;; breaks up the integral over Y into a discrete sum over the values
;; of Y where dF_X(x) is not 0. Thus:
;;
;; Let X* be the countably finite subset of X values for which dF_X(x) > 0.
;; F(Z) = \sum_{x \in X*} F_Y(z-x) dF_X(x)
(defmethod rv-convolute* [::continuous-distribution ::continuous-distribution]
  [f X Y]
  (apply merge-with + (for [[v1 p1] (to-discrete-randvar X) [v2 p2] Y] {(f v1 v2) (* p1 p2)})))

(defn rv-convolute-1
  [f X Y]
  (apply merge-with + (for [[v1 p1] X [v2 p2] Y] {(f v1 v2) (* p1 p2)})))

(defn rv-convolute-2
  [f X Y]
  (let [convolution (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)])
        all-unique  (reduce (fn [amap [v2 p2]]
                              (if-let [p1 (amap v2)]
                                (assoc amap v2 (+ p1 p2))
                                (assoc amap v2 p2)))
                            (apply array-map (first convolution))
                            (rest convolution))]
    all-unique))

(comment
  (defn rv-convolute-3
    [f X Y]
    (let [convolution (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)])
          all-unique  (reduce (fn [amap [v2 p2]]
                                (if-let [p1 (amap v2)]
                                  (assoc! amap v2 (+ p1 p2))
                                  (assoc! amap v2 p2)))
                              (transient (apply array-map (first convolution)))
                              (rest convolution))]
      (persistent! all-unique)))
  )

(defn rv-convolute-4
  [f X Y]
  (let [convolution (sort (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)]))
        all-unique  (reduce (fn [acc [v2 p2 :as n]]
                              (let [[v1 p1] (peek acc)]
                                (if (== v1 v2)
                                  (conj (pop acc) [v2 (+ p1 p2)])
                                  (conj acc n))))
                            (vector (first convolution))
                            (rest convolution))]
    (into {} all-unique)))

(comment
  (defn rv-convolute-5
    [f X Y]
    (let [convolution (sort (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)]))
          all-unique  (reduce (fn [acc [v2 p2 :as n]]
                                (let [last-idx (dec (count acc))
                                      [v1 p1]  (acc last-idx)]
                                  (if (== v1 v2)
                                    (assoc! acc last-idx [v2 (+ p1 p2)])
                                    (conj!  acc n))))
                              (transient (vector (first convolution)))
                              (rest convolution))]
      (into {} (persistent! all-unique))))
  )

(defn rv-convolute-6
  [f X Y]
  (loop [X* X, Y* Y, fXY {}]
    (if (empty? X*)
      fXY
      (if (empty? Y*)
        (recur (rest X*) Y fXY)
        (recur X* (rest Y*) (let [[v1 p1] (first X*)
                                  [v2 p2] (first Y*)
                                  v       (f v1 v2)
                                  p       (* p1 p2)]
                              ;;(update-in fXY [v] #(+ p (or % 0)))))))))
                              (if-let [old-p (fXY v)]
                                (assoc fXY v (+ old-p p))
                                (assoc fXY v p))))))))

(comment
  (defn rv-convolute-7
    [f X Y]
    (loop [X* X, Y* Y, fXY (transient {})]
      (if (empty? X*)
        (persistent! fXY)
        (if (empty? Y*)
          (recur (rest X*) Y fXY)
          (recur X* (rest Y*) (let [[v1 p1] (first X*)
                                    [v2 p2] (first Y*)
                                    v       (f v1 v2)
                                    p       (* p1 p2)]
                                (if-let [old-p (fXY v)]
                                  (assoc! fXY v (+ old-p p))
                                  (assoc! fXY v p))))))))
  )

;;; finish testing functions

(defn- rv-convolute
  "Returns the distribution of f of two random variables X and Y."
  [f X Y]
  (with-meta
    (rv-convolute* f X Y) ;; rv-convolute-6 was fastest before (without transients)
    (if (#{(type X) (type Y)} ::continuous-distribution)
      cont-type
      disc-type)))

(defn rv-add
  [X Y]
  ;;(println "Adding" (count X) "by" (count Y) "RVs (" (type X) (type Y) ")")
  (rv-resample (rv-convolute + X Y)))
(def _+_ rv-add)

(defn rv-subtract
  [X Y]
  (rv-resample (rv-convolute - X Y)))
(def _-_ rv-subtract)

(defn rv-multiply
  [X Y]
  (rv-resample (rv-convolute * X Y)))
(def _*_ rv-multiply)

(defn rv-divide
  [X Y]
  (rv-resample (rv-convolute / X (dissoc Y 0))))
(def _d_ rv-divide)

(defn rv-max
  [X Y]
  (if (> (rv-mean X) (rv-mean Y)) X Y))

(defn rv-min
  [X Y]
  (if (< (rv-mean X) (rv-mean Y)) X Y))

(defn rv-lt
  [X Y]
  (get (rv-convolute < X Y) true 0))
(def _<_ rv-lt)

;;(defn rv-gt
;;  [X Y]
;;  (get (rv-convolute > X Y) true 0))

(defn rv-gt
  [X Y]
  (= X (rv-max X Y)))
(def _>_ rv-gt)

(defn- rv-map
  "Returns the distribution of the random variable X with f applied to its range values."
  [f X]
  (with-meta (mapmap f identity X) (meta X)))
;;  (with-meta (into {} (map (fn [[s p]] [(f s) p]) X)) (meta X)))

(defn scalar-rv-add
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(+ x* %) Y)))
(def +_ scalar-rv-add)

(defn scalar-rv-subtract
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(- x* %) Y)))
(def -_ scalar-rv-subtract)

(defn scalar-rv-multiply
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(* x* %) Y)))
(def *_ scalar-rv-multiply)

(defn scalar-rv-divide
  [x Y]
  (let [x* (rationalize x)
        Y* (dissoc Y 0)]
    (rv-map #(/ x* %) Y*)))
(def d_ scalar-rv-divide)

(defn rv-scalar-add
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(+ % y*) X)))
(def _+ rv-scalar-add)

(defn rv-scalar-subtract
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(- % y*) X)))
(def _- rv-scalar-subtract)

(defn rv-scalar-multiply
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(* % y*) X)))
(def _* rv-scalar-multiply)

(defn rv-scalar-divide
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(/ % y*) X)))
(def _d rv-scalar-divide)

(defn rv-zero-above-scalar
  "Sets all values greater than y in the random variable X to 0."
  [X y]
  (rv-convolute #(if (> %2 %1) 0 %2) {(rationalize y) 1} X))

(defn rv-zero-below-scalar
  "Sets all values less than y in the random variable X to 0."
  [X y]
  (rv-convolute #(if (< %2 %1) 0 %2) {(rationalize y) 1} X))

(defn rv-pos
  "Sets all negative values in X to 0."
  [X]
  (rv-zero-below-scalar X 0))

(defn rv-average
  [RVs]
  (if (seq RVs)
;;    (rv-scalar-divide (reduce rv-add rv-zero RVs) (count RVs))))
    (do (println "Averaging" (count RVs) (type (first RVs)) "RVs...")
        (time (rv-scalar-divide (reduce rv-add rv-zero RVs) (count RVs))))))

(defmulti rv-scale
  (fn [rv scale-factor] (type rv)))

(defmethod rv-scale ::continuous-distribution
  [rv-unsorted scale-factor-double]
  (let [scale-factor (rationalize scale-factor-double)
        rv           (sort rv-unsorted)
        values       (vec (keys rv))
        probs        (vec (map (p * scale-factor) (successive-differences (vals rv))))
        zero-pos     (loop [i 0, max (count values)] (when (< i max) (if (zero? (values i)) i (recur (inc i) max))))
        pos-pos      (loop [i 0, max (count values)] (when (< i max) (if (pos?  (values i)) i (recur (inc i) max))))
        values2      (if zero-pos values (concat (take pos-pos values) [0] (drop pos-pos values)))
        probs2       (successive-sums (if zero-pos
                                        (assoc probs zero-pos (+ (probs zero-pos) (- 1 scale-factor)))
                                        (concat (take pos-pos probs) [(- 1 scale-factor)] (drop pos-pos probs))))]
    (with-meta (zipmap values2 probs2) cont-type)))

(defmethod rv-scale ::discrete-distribution
  [rv scale-factor-double]
  (let [scale-factor (rationalize scale-factor-double)
        values       (vec (keys rv))
        probs        (vec (map (p * scale-factor) (vals rv)))
        zero-pos     (loop [i 0, max (count values)] (when (< i max) (if (zero? (values i)) i (recur (inc i) max))))
        values2      (if zero-pos values (cons 0 values))
        probs2       (if zero-pos
                       (assoc probs zero-pos (+ (probs zero-pos) (- 1 scale-factor)))
                       (cons (- 1 scale-factor) probs))]
    (with-meta (zipmap values2 probs2) disc-type)))

;; Example profiling code
;;(use 'clj-misc.memtest)
;;(defmulti  make-rv (fn [states type] type))
;;(defmethod make-rv :double [states _] (into {} (take states (repeatedly #(vector (rand 100.0) (rand 1.0))))))
;;(defmethod make-rv :int    [states _] (into {} (take states (repeatedly #(vector (rand-int 10000) (rand-int 100))))))
;;(count (check-mem (time (doall (for [i (range 10000)] (rv-plus5_1 (make-rv 10 :int) (make-rv 10 :int)))))))
