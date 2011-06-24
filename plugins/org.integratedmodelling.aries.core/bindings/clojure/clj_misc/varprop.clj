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
;;; manipulating fuzzy numbers, which are defined to be pairs of [mean
;;; var].

(ns clj-misc.varprop
  (:use [clj-misc.utils :only [my-partition-all replace-all]]))

;;(defrecord FuzzyNumber [mean var])
(defstruct FuzzyNumber :mean :var)

;; (defn fuzzy-number
;;   "Constructs a FuzzyNumber."
;;   [mean var]
;;   (FuzzyNumber. mean var))
(defn fuzzy-number
  "Constructs a FuzzyNumber."
  [mean var]
  (struct FuzzyNumber mean var))

(defn fuzzy-number-from-states
  "Constructs a FuzzyNumber from n states and n probs, corresponding
   to a finite discrete distribution."
  [states probs]
  (let [mean (reduce + (map * states probs))
        var  (reduce + (map (fn [x p] (* (Math/pow (- x mean) 2) p)) states probs))]
    (fuzzy-number mean var)))

(defn fuzzy-number-from-ranges
  "Constructs a FuzzyNumber from n bounds and n-1 probs corresponding
   to a piecewise continuous uniform distribution with
   discontinuities (i.e. jumps) at the bounds. prob i represents the
   probability of being between bound i and bound i+1."
  [bounds probs]
  (let [midpoints     (map (fn [next prev] (/ (+ next prev) 2.0)) (rest bounds) bounds)
        mean          (reduce + (map * midpoints probs))
        second-moment (* 1/3 (reduce + (map (fn [p1 p2 bp] (* (Math/pow bp 3) (- p1 p2)))
                                            (cons 0 probs)
                                            (concat probs [0])
                                            bounds)))
        var           (- second-moment (* mean mean))]
    (fuzzy-number mean var)))

(def ^{:doc "A fuzzy number with mean and variance of 0."} _0_ (fuzzy-number 0.0 0.0))

(defn _+_
  "Returns the sum of two or more FuzzyNumbers."
  ([X Y]
     (fuzzy-number (+ (:mean X) (:mean Y)) (+ (:var X) (:var Y))))
  ([X Y & more]
     (reduce _+_ (_+_ X Y) more)))

(defn _-_
  "Returns the difference of two or more FuzzyNumbers."
  ([X Y]
     (fuzzy-number (- (:mean X) (:mean Y)) (+ (:var X) (:var Y))))
  ([X Y & more]
     (reduce _-_ (_-_ X Y) more)))

(defn _*_
  "Returns the product of two or more FuzzyNumbers."
  ([{mx :mean, vx :var} {my :mean, vy :var}]
     (fuzzy-number (* mx my) (+ (* vx vy) (* mx mx vy) (* my my vx))))
  ([X Y & more]
     (reduce _*_ (_*_ X Y) more)))

(declare d_)

(defn _d_
  "Returns the quotient of two or more FuzzyNumbers."
  ([X Y]
     (_*_ X (d_ 1 Y)))
  ([X Y & more]
     (reduce _d_ (_d_ X Y) more)))

(defn _<_
  "Compares two or more FuzzyNumbers and returns true if P(X_i < X_i+1) > 0.5 for all i in [1,n]."
  ([X Y]
     (< (:mean X) (:mean Y)))
  ([X Y & more]
     (every? (fn [[X Y]] (_<_ X Y)) (partition 2 1 (list* X Y more)))))

(defn _>_
  "Compares two or more FuzzyNumbers and returns true if P(X_i > X_i+1) > 0.5 for all i in [1,n]."
  ([X Y]
     (> (:mean X) (:mean Y)))
  ([X Y & more]
     (every? (fn [[X Y]] (_>_ X Y)) (partition 2 1 (list* X Y more)))))

(defn _min_
  "Returns the smallest of two or more FuzzyNumbers using _<_."
  ([X Y]
     (if (_<_ X Y) X Y))
  ([X Y & more]
     (reduce _min_ (_min_ X Y) more)))

(defn _max_
  "Returns the greatest of two or more FuzzyNumbers using _>_."
  ([X Y]
     (if (_>_ X Y) X Y))
  ([X Y & more]
     (reduce _max_ (_max_ X Y) more)))

(defn _+
  "Returns the sum of a FuzzyNumber and one or more scalars."
  ([X y]
     (fuzzy-number (+ (:mean X) y) (:var X)))
  ([X y & more]
     (reduce _+ (_+ X y) more)))

(defn _-
  "Returns the difference of a FuzzyNumber and one or more scalars."
  ([X y]
     (fuzzy-number (- (:mean X) y) (:var X)))
  ([X y & more]
     (reduce _- (_- X y) more)))

(defn _*
  "Returns the product of a FuzzyNumber and one or more scalars."
  ([X y]
     (fuzzy-number (* (:mean X) y) (* (:var X) y y)))
  ([X y & more]
     (reduce _* (_* X y) more)))

(defn _d
  "Returns the quotient of a FuzzyNumber and one or more scalars."
  ([X y]
     (fuzzy-number (/ (:mean X) y) (/ (:var X) y y)))
  ([X y & more]
     (reduce _d (_d X y) more)))

(defn _<
  "Compares a FuzzyNumber and one or more scalars and returns true if
   P(X < y_1) > 0.5 and all ys are in monotonically increasing order."
  ([X y]
     (< (:mean X) y))
  ([X y & more]
     (and (_< X y)
          (apply < (cons y more)))))

(defn _>
  "Compares a FuzzyNumber and one or more scalars and returns true if
   P(X > y_1) > 0.5 and all ys are in monotonically decreasing order."
  ([X y]
     (> (:mean X) y))
  ([X y & more]
     (and (_> X y)
          (apply > (cons y more)))))

(defn _min
  "Returns the smallest of a FuzzyNumber and one or more scalars using _<."
  ([X y]
     (if (_< X y) X y))
  ([X y & more]
     (_min X (reduce min y more))))

(defn _max
  "Returns the greatest of a FuzzyNumber and one or more scalars using _>."
  ([X y]
     (if (_> X y) X y))
  ([X y & more]
     (_max X (reduce max y more))))

(defn +_
  "Returns the sum of a scalar and one or more FuzzyNumbers."
  ([x Y]
     (fuzzy-number (+ x (:mean Y)) (:var Y)))
  ([x Y & more]
     (reduce _+_ (+_ x Y) more)))

(defn -_
  "Returns the difference of a scalar and one or more FuzzyNumbers."
  ([x Y]
     (fuzzy-number (- x (:mean Y)) (:var Y)))
  ([x Y & more]
     (reduce _-_ (-_ x Y) more)))

(defn *_
  "Returns the product of a scalar and one or more FuzzyNumbers."
  ([x Y]
     (fuzzy-number (* x (:mean Y)) (* x x (:var Y))))
  ([x Y & more]
     (reduce _*_ (*_ x Y) more)))

(defn d_
  "Returns the quotient of a scalar and one or more FuzzyNumbers."
  ([x {:keys [mean var]}]
     (fuzzy-number (/ x mean) (/ (* x x var) (Math/pow mean 4))))
  ([x Y & more]
     (reduce _d_ (d_ x Y) more)))

(defn <_
  "Compares a scalar and one or more FuzzyNumbers and returns true if
   P(Y > x) > 0.5 and all Ys are in monotonically increasing order by
   _<_."
  ([x Y]
     (< x (:mean Y)))
  ([x Y & more]
     (and (<_ x Y)
          (apply _<_ (cons Y more)))))

(defn >_
  "Compares a scalar and one or more FuzzyNumbers and returns true if
   P(Y < x) > 0.5 and all Ys are in monotonically decreasing order by
   _>_."
  ([x Y]
     (> x (:mean Y)))
  ([x Y & more]
     (and (>_ x Y)
          (apply _>_ (cons Y more)))))

(defn min_
  "Returns the smallest of a scalar and one or more FuzzyNumbers using <_."
  ([x Y]
     (if (<_ x Y) x Y))
  ([x Y & more]
     (min_ x (reduce _min_ Y more))))

(defn max_
  "Returns the greatest of a scalar and one or more FuzzyNumbers using >_."
  ([x Y]
     (if (>_ x Y) x Y))
  ([x Y & more]
     (max_ x (reduce _max_ Y more))))

(defmulti ?+?
  "Returns the sum of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?+? [FuzzyNumber FuzzyNumber] [X Y & _] (_+_ X Y))
(defmethod ?+? [FuzzyNumber Number]      [X Y & _] (_+  X Y))
(defmethod ?+? [Number      FuzzyNumber] [X Y & _] ( +_ X Y))
(defmethod ?+? [Number      Number]      [X Y & _] ( +  X Y))
(defmethod ?+? :more [X Y & more] (reduce ?+? (?+? X Y) more))

(defmulti ?-?
  "Returns the difference of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?-? [FuzzyNumber FuzzyNumber] [X Y & _] (_-_ X Y))
(defmethod ?-? [FuzzyNumber Number]      [X Y & _] (_-  X Y))
(defmethod ?-? [Number      FuzzyNumber] [X Y & _] ( -_ X Y))
(defmethod ?-? [Number      Number]      [X Y & _] ( -  X Y))
(defmethod ?-? :more [X Y & more] (reduce ?-? (?-? X Y) more))

(defmulti ?*?
  "Returns the product of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?*? [FuzzyNumber FuzzyNumber] [X Y & _] (_*_ X Y))
(defmethod ?*? [FuzzyNumber Number]      [X Y & _] (_*  X Y))
(defmethod ?*? [Number      FuzzyNumber] [X Y & _] ( *_ X Y))
(defmethod ?*? [Number      Number]      [X Y & _] ( *  X Y))
(defmethod ?*? :more [X Y & more] (reduce ?*? (?*? X Y) more))

(defmulti ?d?
  "Returns the quotient of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?d? [FuzzyNumber FuzzyNumber] [X Y & _] (_d_ X Y))
(defmethod ?d? [FuzzyNumber Number]      [X Y & _] (_d  X Y))
(defmethod ?d? [Number      FuzzyNumber] [X Y & _] ( d_ X Y))
(defmethod ?d? [Number      Number]      [X Y & _] ( /  X Y))
(defmethod ?d? :more [X Y & more] (reduce ?d? (?d? X Y) more))

(defmulti ?<?
  "Compares two or more values, which may be FuzzyNumbers or
   scalars, and returns true if X_i < X_i+1 for all i in [1,n]. Uses
   reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?<? [FuzzyNumber FuzzyNumber] [X Y & _] (_<_ X Y))
(defmethod ?<? [FuzzyNumber Number]      [X Y & _] (_<  X Y))
(defmethod ?<? [Number      FuzzyNumber] [X Y & _] ( <_ X Y))
(defmethod ?<? [Number      Number]      [X Y & _] ( <  X Y))
(defmethod ?<? :more [X Y & more] (every? (fn [[X Y]] (?<? X Y)) (partition 2 1 (list* X Y more))))

(defmulti ?>?
  "Compares two or more values, which may be FuzzyNumbers or
   scalars, and returns true if X_i > X_i+1 for all i in [1,n]. Uses
   reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?>? [FuzzyNumber FuzzyNumber] [X Y & _] (_>_ X Y))
(defmethod ?>? [FuzzyNumber Number]      [X Y & _] (_>  X Y))
(defmethod ?>? [Number      FuzzyNumber] [X Y & _] ( >_ X Y))
(defmethod ?>? [Number      Number]      [X Y & _] ( >  X Y))
(defmethod ?>? :more [X Y & more] (every? (fn [[X Y]] (?>? X Y)) (partition 2 1 (list* X Y more))))

(defmulti ?min?
  "Returns the smallest of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?min? [FuzzyNumber FuzzyNumber] [X Y & _] (_min_ X Y))
(defmethod ?min? [FuzzyNumber Number]      [X Y & _] (_min  X Y))
(defmethod ?min? [Number      FuzzyNumber] [X Y & _] ( min_ X Y))
(defmethod ?min? [Number      Number]      [X Y & _] ( min  X Y))
(defmethod ?min? :more [X Y & more] (reduce ?min? (?min? X Y) more))

(defmulti ?max?
  "Returns the largest of two or more values, which may be FuzzyNumbers or scalars. Uses reflection."
  (fn [X Y & more] (if (seq more)
                     :more
                     [(type X) (type Y)])))

(defmethod ?max? [FuzzyNumber FuzzyNumber] [X Y & _] (_max_ X Y))
(defmethod ?max? [FuzzyNumber Number]      [X Y & _] (_max  X Y))
(defmethod ?max? [Number      FuzzyNumber] [X Y & _] ( max_ X Y))
(defmethod ?max? [Number      Number]      [X Y & _] ( max  X Y))
(defmethod ?max? :more [X Y & more] (reduce ?max? (?max? X Y) more))

(def fuzzy-arithmetic-mapping
  '{+   clj-misc.varprop/?+?
    -   clj-misc.varprop/?-?
    *   clj-misc.varprop/?*?
    /   clj-misc.varprop/?d?
    <   clj-misc.varprop/?<?
    >   clj-misc.varprop/?>?
    min clj-misc.varprop/?min?
    max clj-misc.varprop/?max?})

(defn fuzzify-fn
  "Transforms f into its fuzzy arithmetic equivalent, using the
   mappings defined in fuzzy-arithmetic-mapping."
  [f]
  (if-let [new-f (fuzzy-arithmetic-mapping f)]
    new-f
    (if (list? f)
      (let [[lambda args & body] f]
        (if (and (= lambda 'fn) (vector? args))
          `(~lambda ~args ~@(replace-all fuzzy-arithmetic-mapping body))))
      f)))

(defmacro rv-fn
  "Transforms f into its fuzzy arithmetic equivalent, fuzzy-f, and
   calls (apply fuzzy-f Xs). Uses reflection on the types of Xs as
   well as any numeric literal values used in f."
  [f & Xs]
  `(~(fuzzify-fn f) ~@Xs))

(defn rv-mean
  "Returns the mean of a FuzzyNumber."
  [X]
  (:mean X))

(defn rv-sum
  "Returns the sum of a sequence of FuzzyNumbers using _+_."
  [Xs]
  (cond (empty? Xs)
        _0_

        (== (count Xs) 1)
        (first Xs)

        (<= (count Xs) 20)
        (reduce _+_ Xs)

        :otherwise
        (recur (pmap rv-sum
                     (my-partition-all 20 Xs)))))

(defn rv-extensive-sampler
  "Returns the extensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered]). For use with
   clj-misc.matrix-ops/resample-matrix."
  [coverage]
  (rv-sum (map (fn [[val frac]] (_* val frac)) coverage)))

(defn rv-intensive-sampler
  "Returns the intensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered]). For use with
   clj-misc.matrix-ops/resample-matrix."
  [coverage]
  (let [frac-sum (reduce + (map second coverage))]
    (rv-sum (map (fn [[val frac]] (_* val (/ frac frac-sum))) coverage))))

(let [stored-val (atom nil)]
  (defn marsaglia-normal
    "Returns a value from X~N(0,1). Uses the Marsaglia polar
     method. Memoizes extra computed values for quicker lookups on
     even calls."
    []
    (when-let [normal-val @stored-val]
      (reset! stored-val nil)
      normal-val)
    (let [v1 (dec (* 2.0 (rand)))
          v2 (dec (* 2.0 (rand)))
          s  (+ (* v1 v1) (* v2 v2))]
      (if (and (not= s 0.0) (< s 1.0))
        (let [theta (Math/sqrt (/ (* -2.0 (Math/log s)) s))]
          (reset! stored-val (* v1 theta))
          (* v2 theta))
        (recur)))))

(let [stored-val (atom nil)]
  (defn box-muller-normal
    "Returns a value from X~N(0,1). Uses the Box-Muller
     transform. Memoizes extra computed values for quicker lookups on
     even calls."
    []
    (when-let [normal-val @stored-val]
      (reset! stored-val nil)
      normal-val)
    (let [u1    (+ (rand) 1e-6) ;; adding delta=1e-6 to prevent computing log(0) below
          u2    (rand)
          r     (Math/sqrt (* -2.0 (Math/log u1)))
          theta (* 2.0 Math/PI u2)
          n1    (* r (Math/cos theta))
          n2    (* r (Math/sin theta))]
      (reset! stored-val n1)
      n2)))

(defn draw
  "Extracts a deterministic value from a FuzzyNumber by modelling it
   as a normal distribution."
  [X]
  (+ (* (marsaglia-normal) (Math/sqrt (:var X))) (:mean X)))

(defn draw-repeatedly
  "Takes a fuzzy number X, and returns an infinite lazy sequence of
   normally-distributed, pseudo-random numbers that match the
   parameters of X, (or a finite sequence of length n, if an integer n
   is provided)."
  ([{:keys [mean var]}]
     (let [sigma (Math/sqrt var)]
       (map #(+ (* sigma %) mean) (repeatedly marsaglia-normal))))
  ([n X]
     (take n (draw-repeatedly X))))
