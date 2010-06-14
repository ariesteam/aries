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
  (:use [clj-misc.utils :only (p select-n-distinct select-n-summands my-partition-all constraints-1.0)]))

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
       (lazy-seq (cons total (successive-sums (+ total (first nums)) (rest nums)))))))

(defn- successive-differences
  [nums]
  (if (< (count nums) 2)
    nums
    (cons (first nums) (map - (rest nums) nums))))

(defmulti to-continuous-randvar type)

(defmethod to-continuous-randvar ::discrete-distribution
  [discrete-RV]
  (with-meta
    (let [sorted-RV (sort discrete-RV)]
      (zipmap (keys sorted-RV)
              (successive-sums (vals sorted-RV))))
    cont-type))

(defmethod to-continuous-randvar ::continuous-distribution [continuous-RV] continuous-RV)
  
;; I should really make sure that all state values are rational.
(defn make-randvar
  [rv-type num-states valid-states]
  (constraints-1.0 {:pre [(#{:discrete :continuous} rv-type)]})
  (let [discrete-RV (with-meta
                      (zipmap (select-n-distinct num-states valid-states)
                              (map #(/ % 100.0) (select-n-summands num-states 100)))
                      disc-type)]
    (if (= rv-type :discrete)
      discrete-RV
      (to-continuous-randvar discrete-RV))))

(defmulti rv-resample
  "Returns a new random variable with <=*rv-max-states* states sampled from X."
  type)

(defmethod rv-resample ::discrete-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [partition-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta
        (into {}
              (map #(vector (/ (apply + (keys %)) (count %))
                            (apply + (vals %)))
                   (my-partition-all partition-size (sort-by key X))))
        (meta X)))))

(defmethod rv-resample ::continuous-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [step-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta (into {} (take-nth step-size (sort X))) (meta X)))))

(defmulti rv-mean
  "Returns the mean value of a random variable X."
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

(def rv-zero (with-meta (array-map 0 1) disc-type))
(def _0_ rv-zero)

;;; testing functions below here

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

#_(defn rv-convolute-3
    [f X Y]
    (let [convolution (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)])
          all-unique  (reduce (fn [amap [v2 p2]]
                                (if-let [p1 (amap v2)]
                                  (assoc! amap v2 (+ p1 p2))
                                  (assoc! amap v2 p2)))
                              (transient (apply array-map (first convolution)))
                              (rest convolution))]
      (persistent! all-unique)))

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

#_(defn rv-convolute-5
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

#_(defn rv-convolute-7
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

;;; finish testing functions

(defn- rv-convolute
  "Returns the distribution of f of two random variables X and Y."
  [f X Y]
  (with-meta
    (rv-convolute-6 f X Y)
    (if (#{(type X) (type Y)} ::continuous-distribution)
      cont-type
      disc-type)))

(defn rv-add
  [X Y]
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
  (with-meta (into {} (map (fn [[s p]] [(f s) p]) X)) (meta X)))

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
  (rv-scalar-divide (reduce rv-add RVs) (count RVs)))

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
