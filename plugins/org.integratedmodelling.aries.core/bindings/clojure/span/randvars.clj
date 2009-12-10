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
  (:use [misc.utils :only (seq2map maphash)]))
(refer 'modelling   :only '(probabilistic?
			    encodes-continuous-distribution?
			    get-dist-breakpoints
			    get-possible-states
			    get-probabilities
			    get-data))

(defn- successive-sums
  ([nums]      (successive-sums (first nums) (rest nums)))
  ([init nums] (reduce #(conj %1 (+ (peek %1) %2)) [init] nums)))

(defn- successive-differences
  [nums]
  (if (< (count nums) 2)
    nums
    (cons (first nums) (map #(- %2 %1) nums (rest nums)))))

(defn unpack-datasource
  "Returns a seq of length n of the values in ds,
   represented as probability distributions.  All values and
   probabilities are represented as integers."
  [ds n]
  (let [to-ints (partial map #(int (* 100 %)))]
    (if (probabilistic? ds)
      (if (encodes-continuous-distribution? ds)
	;; sampled continuous distribution
	(let [type-info             {:type :continuous-distribution}
	      bounds                (get-dist-breakpoints ds)
	      unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
	      unbounded-from-above? (== Double/POSITIVE_INFINITY (last bounds))
	      prob-dist             (apply create-struct (to-ints 
							  (if unbounded-from-below?
							    (if unbounded-from-above?
							      (rest (butlast bounds))
							      (rest bounds))
							    (if unbounded-from-above?
							      (butlast bounds)
							      bounds))))]
	  (if unbounded-from-below?
	    (if unbounded-from-above?
	      (map (fn [idx] (with-meta
			       (apply struct prob-dist (successive-sums (to-ints (butlast (get-probabilities ds idx)))))
			       type-info))
		   (range n))
	      (map (fn [idx] (with-meta
			       (apply struct prob-dist (successive-sums (to-ints (get-probabilities ds idx))))
			       type-info))
		   (range n)))
	    (if unbounded-from-above?
	      (map (fn [idx] (with-meta
			       (apply struct prob-dist (successive-sums 0 (to-ints (butlast (get-probabilities ds idx)))))
			       type-info))
		   (range n))
	      (map (fn [idx] (with-meta
			       (apply struct prob-dist (successive-sums 0 (to-ints (get-probabilities ds idx))))
			       type-info))
		   (range n)))))
	;; discrete distribution
	(let [type-info {:type :discrete-distribution}
	      states    (get-possible-states ds)
	      prob-dist (apply create-struct states)]
	  (map (fn [idx] (with-meta
			   (apply struct prob-dist (get-probabilities ds idx))
			   type-info))
	       (range n))))
      ;; deterministic values
      (let [type-info {:type :discrete-distribution}]
	(map #(with-meta (array-map % 100) type-info) (to-ints (get-data ds)))))))

;; FIXME why is this causing a class cast exception?
;; FIXME change to type?
(defmulti rv-mean
  ;;"Returns the mean value of a random variable X."
  (fn [X] (:type (meta X))))

(defmethod rv-mean :discrete-distribution
  [X]
  (let [sum-of-squares (reduce + (map (partial apply *) X))]
    (if (zero? sum-of-squares) 0 (/ sum-of-squares 100))))

;; FIXME implement stub
(defmethod rv-mean :continuous-distribution
  [X]
  0)

(def rv-zero {0 100})

(comment
      rv-add
      rv-subtract
      rv-multiply
      rv-divide

      rv-scalar-divide
      rv-scalar-multiply
      scalar-rv-subtract

      rv-zero-above-scalar
      rv-zero-below-scalar

      rv-scale
      rv-lt
)
(defn rv-from-scalar
  "Returns a new distribution with the same vals as X with probability 1.0 on x."
  [X x]
  (seq2map X (fn [[v p]] (vector v (if (= v x) 1.0 0.0)))))

(defn rv-zero-above-scalar
  "Returns true if P(X=zero-val)=1.0 for the random variable X."
  [X zero-val]
  (== (X zero-val) 1.0))

(defn rv-zero-below-scalar
  "Returns true if P(X=zero-val)=1.0 for the random variable X."
  [X zero-val]
  (== (X zero-val) 1.0))

;; FIXME finish stub
(defn rv-add
  "Returns the distribution of the sum of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn rv-subtract
  "Returns the distribution of the difference of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn scalar-rv-subtract
  "Returns the distribution of the random variable Y with its range values subtracted from x."
  [x Y]
  (maphash #(- x %) identity Y))

;; FIXME finish stub
(defn rv-multiply
  "Returns the distribution of the product of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn rv-divide
  "Returns the distribution of the quotient of two random variables X and Y."
  [X Y]
  X)

(defn rv-scalar-multiply
  "Returns the distribution of the random variable X with its range values multiplied by y."
  [X y]
  (maphash #(* % y) identity X))

(defn scalar-rv-multiply
  "Returns the distribution of the random variable Y with its range values multiplied by x."
  [x Y]
  (maphash #(* x %) identity Y))

(defn rv-scalar-divide
  "Returns the distribution of the random variable X with its range values divided by y."
  [X y]
  (maphash #(/ % y) identity X))

(defn scalar-rv-divide
  "Returns the distribution of the random variable Y with its range values replaced by x/y."
  [x Y]
  (maphash #(/ x %) identity Y))

;; FIXME finish stub
(defn rv-gt
  ""
  [X Y]
  true)

;; FIXME finish stub
(defn rv-scalar-gt
  ""
  [X y]
  true)

;; FIXME finish stub
(defn scalar-rv-gt
  ""
  [x Y]
  true)

;; FIXME finish stub
(defn rv-min
  ""
  [X Y]
  X)

;; FIXME finish stub
(defn rv-eq
  ""
  [X Y]
  true)

;; FIXME finish stub
(defn rv-lt
  ""
  [X Y]
  true)

;; Here's the test:
;; 1) Vary data structures
;; 2) Vary floating point vs. integer representation

;; vector as p-list
(defn rv-plus1
  [X Y]
  (let [eps 0.001
	convolution (sort (for [[v1 p1] (partition 2 X) [v2 p2] (partition 2 Y)] [(+ v1 v2) (* p1 p2)]))
	all-unique  (reduce (fn [res [v2 p2 :as n]]
			      (let [[v1 p1] (peek res)]
				(if (< v2 (+ v1 eps))
				  (conj (pop res) [v2 (+ p1 p2)])
				  (conj res n))))
			    [(first convolution)]
			    (rest convolution))]
    (vec (concat all-unique))))

;; array-map
(defn rv-plus2
  [X Y]
  (let [eps 0.001
	convolution (sort (for [[v1 p1] X [v2 p2] Y] [(+ v1 v2) (* p1 p2)]))
	all-unique  (reduce (fn [res [v2 p2]]
			      (let [[v1 p1] (last res)]
				(if (< v2 (+ v1 eps))
				  (assoc (dissoc res v1) v2 (+ p1 p2))
				  (assoc res v2 p2))))
			    (apply array-map (first convolution))
			    (rest convolution))]
    all-unique))

(defstruct mydist :vals :probs)
;; struct of vectors
(defn rv-plus3
  [{Xvals :vals Xprobs :probs} {Yvals :vals Yprobs :probs}]
  (let [eps         0.001
	convolution (sort (for [i (range (count Xvals)) j (range (count Yvals))]
			    [(+ (Xvals i) (Yvals j)) (* (Xprobs i) (Yprobs j))]))]
    (loop [input (rest convolution)
	   vals  [(first  (first convolution))]
	   probs [(second (first convolution))]]
      (if (empty? input)
	(struct mydist vals probs)
	(let [v1      (peek vals)
	      p1      (peek probs)
	      [v2 p2] (first input)]
	  (if (< v2 (+ v1 eps))
	    (recur (rest input) (conj (pop vals) v2) (conj (pop probs) (+ p1 p2)))
	    (recur (rest input) (conj vals v2) (conj probs p2))))))))

(defn rv-plus3_1
  [{Xvals :vals Xprobs :probs} {Yvals :vals Yprobs :probs}]
  (let [eps  0.001
	lenX (count Xvals)
	lenY (count Yvals)
        [XYvals XYprobs] (loop [i 0, j 0, XYvals [], XYprobs []]
			   (if (== i lenX)
			     [XYvals XYprobs]
			     (if (== j lenY)
			       (recur (inc i) 0 XYvals XYprobs)
			       (recur i (inc j) (conj XYvals (+ (Xvals i) (Yvals j))) (conj XYprobs (* (Xprobs i) (Yprobs j)))))))
	convolution (sort (map vec (partition 2 (interleave XYvals XYprobs))))]
    (loop [input (rest convolution)
	   vals  [(first  (first convolution))]
	   probs [(second (first convolution))]]
      (if (empty? input)
	(struct mydist vals probs)
	(let [v1      (peek vals)
	      p1      (peek probs)
	      [v2 p2] (first input)]
	  (if (< v2 (+ v1 eps))
	    (recur (rest input) (conj (pop vals) v2) (conj (pop probs) (+ p1 p2)))
	    (recur (rest input) (conj vals v2) (conj probs p2))))))))

(defn rv-plus3_2
  [{Xvals :vals Xprobs :probs} {Yvals :vals Yprobs :probs}]
  (let [eps  0.001
	lenX (count Xvals)
	lenY (count Yvals)
        convolution (loop [i 0, j 0, XYsum (sorted-map)]
		      (if (== i lenX)
			XYsum
			(if (== j lenY)
			  (recur (inc i) 0 XYsum)
			  (recur i (inc j) (let [v (+ (Xvals  i) (Yvals  j))
						 p (* (Xprobs i) (Yprobs j))]
					     (if-let [old-p (XYsum v)]
					       (assoc XYsum v (+ old-p p))
					       (assoc XYsum v p)))))))]
    (struct mydist (keys convolution) (vals convolution))))

;; sorted map with doubles
(defn rv-plus5
  [X Y]
  (let [eps 0.001
	convolution (sort (for [[v1 p1] X [v2 p2] Y] [(+ v1 v2) (* p1 p2)]))
	all-unique  (reduce (fn [smap [v2 p2 :as n]]
			      (let [[v1 p1] (first (rseq smap))]
				(if (< v2 (+ v1 eps))
				  (assoc (dissoc smap v1) v2 (+ p1 p2))
				  (assoc smap v2 p2))))
			    (apply sorted-map (first convolution))
			    (rest convolution))]
    all-unique))

;; sorted map with integers
(defn rv-plus5_1
  [X Y]
  (apply merge-with + (sorted-map) (for [[v1 p1] X [v2 p2] Y] {(+ v1 v2) (* p1 p2)})))

;; sorted map with integers
(defn rv-plus5_2
  [X Y]
  (loop [X* X, Y* Y, XYsum (sorted-map)]
    (if (empty? X*)
      XYsum
      (if (empty? Y*)
	(recur (rest X*) Y XYsum)
	(recur X* (rest Y*) (let [[v1 p1] (first X*)
				  [v2 p2] (first Y*)
				  v       (+ v1 v2)
				  p       (* p1 p2)]
			      (if-let [old-p (XYsum v)]
				(assoc XYsum v (+ old-p p))
				(assoc XYsum v p))))))))

(defn rv-continuous?
  [X]
  (some #(> % 1.0) (successive-sums (vals (rseq X)))))

;; FIXME this will behave badly if rv contains negative values
(defn rv-scale
  [rv scale-factor]
  (let [values   (vec (keys rv))
	probs    (vec (map #(* % scale-factor) (if (rv-continuous? rv)
						 (successive-differences (vals rv))
						 (vals rv))))
	zero-pos (loop [i 0, max (count values)] (when (< i max) (if (== (values i) 0.0) i (recur (inc i) max))))
	probs2   (if zero-pos
		   (assoc probs zero-pos (+ (probs zero-pos) (- 1.0 scale-factor)))
		   (cons (- 1.0 scale-factor) probs))]
    (loop [values (if zero-pos values (cons 0.0 values))
	   probs  (if (rv-continuous? rv)
		    (successive-sums probs2)
		    probs2)
	   result (sorted-map)]
      (if (empty? values)
	result
	(recur (rest values)
	       (rest probs)
	       (assoc result (first values) (first probs)))))))
