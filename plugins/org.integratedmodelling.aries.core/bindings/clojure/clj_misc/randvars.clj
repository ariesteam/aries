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

(ns clj-misc.randvars
  (:use [clj-misc.utils :only [p constraints-1.0 seq2map dissoc-vec
                               seq2redundant-map successive-sums
                               successive-differences my-partition-all
                               select-n-distinct select-n-summands]]))

;; -------------------- Begin utility functions --------------------

(def #^{:dynamic true} *rv-max-states* 10) ;; sensible default

(defn reset-rv-max-states!
  [new-val]
  (constraints-1.0 {:pre [(and (pos? new-val) (integer? new-val))]})
  (alter-var-root #'*rv-max-states* (constantly new-val)))

(def type (comp :type meta))

(def cont-type {:type ::continuous-distribution})

(def disc-type {:type ::discrete-distribution})

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

(defn create-from-states
  "Constructs a discrete Randvar from n states and n probs,
   corresponding to a finite discrete distribution."
  [states probs]
  (with-meta (zipmap states probs) disc-type))

;; FIXME: I should be using create-from-ranges-continous (see below)
;;        as the definition for this function, but continuous RV math
;;        is still buggy.
(defn create-from-ranges
  "Constructs a discrete Randvar from n bounds and n-1 probs
   corresponding to a piecewise continuous uniform distribution with
   discontinuities (i.e. jumps) at the bounds. prob i represents the
   probability of being between bound i and bound i+1."
  [bounds probs]
  (let [midpoints (map (fn [next prev] (/ (+ next prev) 2.0)) (rest bounds) bounds)]
    (with-meta (zipmap midpoints probs) disc-type)))

(defn create-from-ranges-continuous
  "Constructs a continuous Randvar from n bounds and n-1 probs
   corresponding to a piecewise continuous uniform distribution with
   discontinuities (i.e. jumps) at the bounds. prob i represents the
   probability of being between bound i and bound i+1."
  [bounds probs]
  (let [cdf-vals (successive-sums 0.0 probs)]
    (with-meta (zipmap bounds cdf-vals) cont-type)))

(defn make-randvar
  [rv-type num-states valid-states]
  (constraints-1.0 {:pre [(#{:discrete :continuous} rv-type)]})
  (let [discrete-RV (with-meta
                      (zipmap (map double (select-n-distinct num-states valid-states))
                              (map #(/ % 100.0) (select-n-summands num-states 100 1)))
                      disc-type)]
    (if (= rv-type :discrete)
      discrete-RV
      (to-continuous-randvar discrete-RV))))

(def _0_ (with-meta {0.0 1.0} disc-type))

;; -------------------- Begin rv-resample functions --------------------

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
         (let [partitions          (for [i (range 1 num-values)] (split-at i values))
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
  ;; "Returns a new random variable with <=*rv-max-states* states sampled from X."
  type)

(defmethod rv-resample ::discrete-distribution
  [X]
  (if (<= (count X) *rv-max-states*)
    X
    (with-meta
      (seq2map (partition-by-probs *rv-max-states* X)
               #(vector (/ (apply + (keys %)) (count %))
                        (apply + (vals %))))
      (meta X))))

;;(defmethod rv-resample ::discrete-distribution
;;  [X]
;;  (if-not (> (count X) *rv-max-states*)
;;    X
;;    (let [partition-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
;;      (with-meta
;;        (seq2map (partition-all partition-size (sort X))
;;                 #(vector (/ (apply + (keys %)) (count %))
;;                          (apply + (vals %))))
;;        (meta X)))))

(defmethod rv-resample ::continuous-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [step-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta (into {} (take-nth step-size (sort X))) (meta X)))))

;; -------------------- Begin rv-convolute functions --------------------

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

(comment (defn rv-convolute-3
    [f X Y]
    (let [convolution (for [[v1 p1] X [v2 p2] Y] [(f v1 v2) (* p1 p2)])
          all-unique  (reduce (fn [amap [v2 p2]]
                                (if-let [p1 (amap v2)]
                                  (assoc! amap v2 (+ p1 p2))
                                  (assoc! amap v2 p2)))
                              (transient (apply array-map (first convolution)))
                              (rest convolution))]
      (persistent! all-unique))))

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

(comment (defn rv-convolute-5
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
      (into {} (persistent! all-unique)))))

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
                              ;;(update-in fXY [v] #(+ p (or % 0.0)))))))))
                              (if-let [old-p (fXY v)]
                                (assoc fXY v (+ old-p p))
                                (assoc fXY v p))))))))

(comment (defn rv-convolute-7
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
                                  (assoc! fXY v p)))))))))

;;; finish testing functions
;; rv-convolute-6 was fastest before (without transients)
(defn- rv-convolute
  "Returns the distribution of f of two random variables X and Y."
  [f X Y]
  (with-meta
    (into {} (filter (fn [[_ p]] (pos? p)) (rv-convolute* f X Y)))
    (if (or (= ::continuous-distribution (type X))
            (= ::continuous-distribution (type Y)))
      cont-type
      disc-type)))

;; -------------------- Begin arithmetic functions --------------------

(defn rv-fn
  [f X Y]
  (rv-resample (rv-convolute (eval f) X Y)))

(defn _+_
  [X Y]
  (rv-fn '+ X Y))

(defn _-_
  [X Y]
  (rv-fn '- X Y))

(defn _*_
  [X Y]
  (rv-fn '* X Y))

(defn _d_
  [X Y]
  (rv-fn '/ X Y))

(defn _<_
  [X Y]
  (> (get (rv-convolute < X Y) true 0.0) 0.5))

(defn _>_
  [X Y]
  (> (get (rv-convolute > X Y) true 0.0) 0.5))

(defn _min_
  [X Y]
  (if (_<_ X Y) X Y))

(defn _max_
  [X Y]
  (if (_>_ X Y) X Y))

(defn- rv-map
  "Returns the distribution of the random variable X with f applied to its states."
  [f X]
  (with-meta (seq2redundant-map X (fn [[x p]] [(f x) p]) +) (meta X)))

(defn _+
  [X y]
  (rv-map #(+ % y) X))

(defn _-
  [X y]
  (rv-map #(- % y) X))

(defn _*
  [X y]
  (rv-map #(* % y) X))

(defn _d
  [X y]
  (rv-map #(/ % y) X))

(defmulti rv-cdf-lookup
  ;; "Return F_X(x) = P(X<x)."
  (fn [X y] (type X)))

(defmethod rv-cdf-lookup ::discrete-distribution
  [X y]
  (reduce + (map second (filter #(< (first %) y) X))))

;;; FIXME: stub
(defmethod rv-cdf-lookup ::continuous-distribution
  [X y]
  0.5)

(defn _<
  [X y]
  (> (rv-cdf-lookup X y) 0.5))

(defn _>
  [X y]
  (< (rv-cdf-lookup X y) 0.5))

(defn _min
  [X y]
  (if (_< X y) X y))

(defn _max
  [X y]
  (if (_> X y) X y))

(defn +_
  [x Y]
  (rv-map #(+ x %) Y))

(defn -_
  [x Y]
  (rv-map #(- x %) Y))

(defn *_
  [x Y]
  (rv-map #(* x %) Y))

(defn d_
  [x Y]
  (rv-map #(/ x %) Y))

(defn <_
  [x Y]
  (< (rv-cdf-lookup Y x) 0.5))

(defn >_
  [x Y]
  (> (rv-cdf-lookup Y x) 0.5))

(defn min_
  [x Y]
  (if (<_ x Y) x Y))

(defn max_
  [x Y]
  (if (>_ x Y) x Y))

;; -------------------- Begin arithmetic functions --------------------

(defmulti rv-mean
  ;; "Returns the mean value of a random variable X."
  type)

(defmethod rv-mean ::discrete-distribution
  [X]
  (reduce + (map (p apply *) X)))

;; This returns the average of the upper and lower bounds for the mean
(defmethod rv-mean ::continuous-distribution
  [X]
  (let [X*     (sort X)
        states (keys X*)
        probs  (successive-differences (vals X*))
        upper  (reduce + (map * states probs))
        lower  (reduce + (map * states (rest probs)))]
    (/ (+ upper lower) 2)))

;; FIXME: only works for discrete distributions
(defn rv-variance
  "Returns the variance of a random variable X."
  [X]
  (let [mean (rv-mean X)]
    (reduce + (map (fn [[x p]] (* (Math/pow (- x mean) 2) p)) X))))

;; FIXME: only works for discrete distributions
(defn rv-stdev
  "Returns the standard deviation of a random variable X."
  [X]
  (Math/sqrt (rv-variance X)))

(defn rv-sum
  [Xs]
  (cond (== (count Xs) 1)
        (first Xs)

        (<= (count Xs) 20)
        (reduce _+_ Xs)

        :otherwise
        (recur (pmap rv-sum
                     (my-partition-all 20 Xs)))))

(defn rv-extensive-sampler
  "Returns the extensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (rv-sum (map (fn [[val frac]] (_* val frac)) coverage)))

(defn rv-intensive-sampler
  "Returns the intensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (let [frac-sum (reduce + (map second coverage))]
    (rv-sum (map (fn [[val frac]] (_* val (/ frac frac-sum))) coverage))))

;; FIXME: only works for discrete distributions
(defn rv-distribution-sampler
  "Returns the distribution of the means of a coverage (i.e. a
   sequence of pairs of [value fraction-covered])."
  [coverage]
  (let [frac-sum (reduce + (map second coverage))
        states   (map (comp rv-mean first) coverage)
        probs    (map #(/ (second %) frac-sum) coverage)]
    (create-from-states states probs)))

(defn draw-repeatedly
  "Extracts values from X using a uniform distribution."
  ([X]
     (repeatedly #(loop [r (rand), X* X]
                    (let [[x p] (first X*)]
                      (if (<= r p)
                        x
                        (recur (- r p)
                               (rest X*)))))))
  ([n X]
     (take n (draw-repeatedly X))))

(defn draw
  [X]
  (first (draw-repeatedly X)))

;; -------------------- Begin unused functions --------------------

(defn rv-zero-above-scalar
  "Sets all values greater than y in the random variable X to 0."
  [X y]
  (rv-map #(if (> %1 y) 0.0 %1) X))

(defn rv-zero-below-scalar
  "Sets all values less than y in the random variable X to 0."
  [X y]
  (rv-map #(if (< %1 y) 0.0 %1) X))

(defn rv-pos
  "Sets all negative values in X to 0."
  [X]
  (rv-zero-below-scalar X 0.0))

(defn rv-average
  [RVs]
  (if (seq RVs)
;;    (_d (reduce _+_ _0_ RVs) (count RVs))))
    (do (println "Averaging" (count RVs) (type (first RVs)) "RVs...")
        (time (_d (reduce _+_ _0_ RVs) (count RVs))))))

(defn rv-convolutions
  [& Xs]
  (reduce (p rv-convolute conj) (rv-map list (first Xs)) (rest Xs)))

(defmulti rv-scale
  (fn [rv scale-factor] (type rv)))

(defmethod rv-scale ::discrete-distribution
  [X scale-factor]
  (let [states       (vec (keys X))
        probs        (vec (map (p * scale-factor) (vals X)))
        zero-pos     (first (filter #(zero? (states %)) (range (count states))))]
    (with-meta
      (zipmap (if zero-pos states (cons 0.0 states))
              (if zero-pos
                (assoc probs zero-pos (+ (probs zero-pos) (- 1.0 scale-factor)))
                (cons (- 1.0 scale-factor) probs)))
      disc-type)))

(defmethod rv-scale ::continuous-distribution
  [X scale-factor]
  (let [X*           (sort X)
        states       (vec (keys X*))
        probs        (vec (map (p * scale-factor) (successive-differences (vals X*))))
        zero-pos     (first (filter #(zero? (states %)) (range (count states))))
        pos-pos      (first (filter #(pos?  (states %)) (range (count states))))]
    (with-meta
      (zipmap (if zero-pos states (concat (take pos-pos states) [0.0] (drop pos-pos states)))
              (successive-sums (if zero-pos
                                 (assoc probs zero-pos (+ (probs zero-pos) (- 1.0 scale-factor)))
                                 (concat (take pos-pos probs) [(- 1.0 scale-factor)] (drop pos-pos probs)))))
      cont-type)))

(defn rv-zero-ish?
  [delta X]
  (<= (- 1.0 (X 0.0 0.0)) delta))

(defn rv=ish?
  [delta X Y]
  (every? (fn [[x px]] (if-let [py (Y x)] (<= (Math/abs (- py px)) delta))) X))



;; Example profiling code
;;(use 'clj-misc.memtest)
;;(defmulti  make-rv (fn [states type] type))
;;(defmethod make-rv :double [states _] (into {} (take states (repeatedly #(vector (rand 100.0) (rand 1.0))))))
;;(defmethod make-rv :int    [states _] (into {} (take states (repeatedly #(vector (rand-int 10000) (rand-int 100))))))
;;(count (check-mem (time (doall (for [i (range 10000)] (rv-plus5_1 (make-rv 10 :int) (make-rv 10 :int)))))))
