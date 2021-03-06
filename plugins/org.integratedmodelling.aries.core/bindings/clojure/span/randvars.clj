;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.randvars
  (:use [misc.utils  :only (maphash)]
	[span.params :only (*rv-max-states* *downscaling-factor*)]))
;;(comment
(refer 'modelling   :only '(probabilistic?
			    binary?
			    encodes-continuous-distribution?
			    get-dist-breakpoints
			    get-possible-states
			    get-probabilities
			    get-data))
;;)
(comment
  (declare probabilistic?
	   binary?
	   encodes-continuous-distribution?
	   get-dist-breakpoints
	   get-possible-states
	   get-probabilities
	   get-data)
)

(def #^{:private true} cont-type {:type ::continuous-distribution})
(def #^{:private true} disc-type {:type ::discrete-distribution})

(defn- successive-sums
  ([nums]
     (successive-sums (first nums) (rest nums)))
  ([total nums]
     (if (empty? nums)
       (list total)
       (lazy-cons total (successive-sums (+ total (first nums)) (rest nums))))))

(defn- successive-differences
  [nums]
  (if (< (count nums) 2)
    nums
    (cons (first nums) (map - (rest nums) nums))))

;; FIXME: upgrade clojure and change to type
(defmulti rv-resample
  ;;"Returns a new random variable with <=*rv-max-states* states sampled from X."
  (fn [X] (:type (meta X))))

;; FIXME: upgrade clojure and change to (lazy-seq (cons ...))
(defn- my-partition
  [size coll]
  (when (seq coll)
    (lazy-cons (take size coll) (my-partition size (drop size coll)))))
;;    (lazy-seq (cons (take size coll) (my-partition size (drop size coll))))))

(defmethod rv-resample ::discrete-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [partition-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta
	(into {}
	      (map #(vector (/ (apply + (keys %)) (count %))
			    (apply + (vals %)))
		   ;;(partition partition-size partition-size [] (sort X))))
		   (my-partition partition-size (sort X))))
	(meta X)))))

(defmethod rv-resample ::continuous-distribution
  [X]
  (if-not (> (count X) *rv-max-states*)
    X
    (let [step-size (Math/ceil (/ (dec (count X)) (dec *rv-max-states*)))]
      (with-meta (into {} (take-nth step-size (sort X))) (meta X)))))

;; FIXME change to type; upgrade clojure!
(defmulti rv-mean
  ;;"Returns the mean value of a random variable X."
  (fn [X] (:type (meta X))))

(defmethod rv-mean ::discrete-distribution
  [X]
  (reduce + (map (partial apply *) X)))

;; This returns the average of the upper and lower bounds for the mean
(defmethod rv-mean ::continuous-distribution
  [X]
  (let [X*     (sort X)
        states (keys X*)
	probs  (successive-differences (vals X*))
	upper  (reduce + (map * states probs))
	lower  (reduce + (map * states (rest probs)))]
    (/ (+ upper lower) 2)))

(def rv-zero (with-meta (array-map 0 1) disc-type))

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

;; FIXME upgrade clojure and change to type
(defn- rv-convolute
  "Returns the distribution of f of two random variables X and Y."
  [f X Y]
  (with-meta
    (rv-convolute-6 f X Y)
    (if (or (= (meta X) cont-type)
	    (= (meta Y) cont-type))
      cont-type
      disc-type)))

(defn rv-add
  [X Y]
  (rv-resample (rv-convolute + X Y)))

(defn rv-subtract
  [X Y]
  (rv-resample (rv-convolute - X Y)))

(defn rv-multiply
  [X Y]
  (rv-resample (rv-convolute * X Y)))

(defn rv-divide
  [X Y]
  (rv-resample (rv-convolute / X Y)))

(defn rv-lt
  [X Y]
  (get (rv-convolute < X Y) true 0))

(defn rv-gt
  [X Y]
  (get (rv-convolute > X Y) true 0))

(defn- rv-map
  "Returns the distribution of the random variable X with f applied to its range values."
  [f X]
  (with-meta (maphash f identity X) (meta X)))

(defn scalar-rv-add
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(+ x* %) Y)))

(defn scalar-rv-subtract
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(- x* %) Y)))

(defn scalar-rv-multiply
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(* x* %) Y)))

(defn scalar-rv-divide
  [x Y]
  (let [x* (rationalize x)]
    (rv-map #(/ x* %) Y)))

(defn rv-scalar-add
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(+ % y*) X)))

(defn rv-scalar-subtract
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(- % y*) X)))

(defn rv-scalar-multiply
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(* % y*) X)))

(defn rv-scalar-divide
  [X y]
  (let [y* (rationalize y)]
    (rv-map #(/ % y*) X)))

(defn rv-zero-above-scalar
  "Sets all values greater than y in the random variable X to 0."
  [X y]
  (rv-convolute #(if (> %2 %1) 0 %2) {(rationalize y) 1} X))

(defn rv-zero-below-scalar
  "Sets all values less than y in the random variable X to 0."
  [X y]
  (rv-convolute #(if (< %2 %1) 0 %2) {(rationalize y) 1} X))

(defmulti rv-scale
  (fn [rv scale-factor] (:type (meta rv))))

(defmethod rv-scale ::continuous-distribution
  [rv-unsorted scale-factor-double]
  (let [scale-factor (rationalize scale-factor-double)
	rv           (sort rv-unsorted)
	values       (vec (keys rv))
	probs        (vec (map #(* scale-factor %) (successive-differences (vals rv))))
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
	probs        (vec (map #(* scale-factor %) (vals rv)))
	zero-pos     (loop [i 0, max (count values)] (when (< i max) (if (zero? (values i)) i (recur (inc i) max))))
	values2      (if zero-pos values (cons 0 values))
	probs2       (if zero-pos
		       (assoc probs zero-pos (+ (probs zero-pos) (- 1 scale-factor)))
		       (cons (- 1 scale-factor) probs))]
    (with-meta (zipmap values2 probs2) disc-type)))

(defn- average-prob-dists
  [prob-dists]
  (rv-scalar-divide (reduce rv-add prob-dists) (count prob-dists)))

(defn- downsample-prob-dists
  "Takes a seq of prob-dists representing a linearized form of a rows
   x cols matrix of locations and generates a new seq of prob-dists by
   aggregating the values in the matrix according to the global
   *downscaling-factor* along each axis."
  [prob-dists rows cols]
  (if (== *downscaling-factor* 1)
    prob-dists
    (let [prob-dists-2D (vec (map vec (partition cols prob-dists)))
	  scaled-rows   (int (/ rows *downscaling-factor*))
	  scaled-cols   (int (/ cols *downscaling-factor*))
	  offset-range  (range *downscaling-factor*)]
      (for [scaled-i (range scaled-rows) scaled-j (range scaled-cols)]
	(let [i (* scaled-i *downscaling-factor*)
	      j (* scaled-j *downscaling-factor*)]
	  (average-prob-dists (for [i-offset offset-range j-offset offset-range]
				(get-in prob-dists-2D [(+ i i-offset) (+ j j-offset)]))))))))

(defn unpack-datasource
  "Returns a seq of length n of the values in ds,
   represented as probability distributions.  All values and
   probabilities are represented as rationals."
  [ds rows cols n]
  (println "DS:           " ds)
  (println "PROBABILISTIC?" (probabilistic? ds))
  (println "ENCODES?      " (encodes-continuous-distribution? ds))
  (let [to-rationals (partial map #(if (Double/isNaN %) 0 (rationalize %)))]
    (if (and (probabilistic? ds) (not (binary? ds)))
      (if (encodes-continuous-distribution? ds)
	;; sampled continuous distributions (FIXME: How is missing information represented?)
	(let [bounds                (get-dist-breakpoints ds)
	      unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
	      unbounded-from-above? (== Double/POSITIVE_INFINITY (last bounds))]
	  (println "BREAKPOINTS:    " bounds)
	  (println "UNBOUNDED-BELOW?" unbounded-from-below?)
	  (println "UNBOUNDED-ABOVE?" unbounded-from-above?)
	  (let [prob-dist             (apply create-struct (to-rationals
							    (if unbounded-from-below?
							      (if unbounded-from-above?
								(rest (butlast bounds))
								(rest bounds))
							      (if unbounded-from-above?
								(butlast bounds)
								bounds))))
		get-cdf-vals          (if unbounded-from-below?
					(if unbounded-from-above?
					  #(successive-sums (to-rationals (butlast (get-probabilities ds %))))
					  #(successive-sums (to-rationals (get-probabilities ds %))))
					(if unbounded-from-above?
					  #(successive-sums 0 (to-rationals (butlast (get-probabilities ds %))))
					  #(successive-sums 0 (to-rationals (get-probabilities ds %)))))
		extracted-prob-dists  (for [idx (range n)]
					(with-meta (apply struct prob-dist (get-cdf-vals idx)) cont-type))]
	    (downsample-prob-dists extracted-prob-dists rows cols)))
	;; discrete distributions (FIXME: How is missing information represented? Fns aren't setup for non-numeric values.)
	(let [prob-dist (apply create-struct (get-possible-states ds))
	      extracted-prob-dists (for [idx (range n)]
				     (with-meta (apply struct prob-dist (to-rationals (get-probabilities ds idx))) disc-type))]
	  (downsample-prob-dists extracted-prob-dists rows cols)))
      ;; binary distributions and deterministic values (FIXME: NaNs become 0s)
      (downsample-prob-dists (for [value (to-rationals (get-data ds))]
			       (with-meta (array-map value 1) disc-type)) rows cols))))

;; Example profiling code
;;(use 'misc.memtest)
;;(defmulti  make-rv (fn [states type] type))
;;(defmethod make-rv :double [states _] (into {} (take states (repeatedly #(vector (rand 100.0) (rand 1.0))))))
;;(defmethod make-rv :int    [states _] (into {} (take states (repeatedly #(vector (rand-int 10000) (rand-int 100))))))
;;(count (check-mem (time (doall (for [i (range 10000)] (rv-plus5_1 (make-rv 10 :int) (make-rv 10 :int)))))))
