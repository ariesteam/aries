(ns clj-span.core-test
  (:use clojure.contrib.test-is
        clj-span.core :reload-all
        [clj-misc.randvars   :only (_0_ cont-type disc-type to-continuous-randvar rv-average)]
        [clj-misc.utils      :only (& p mapmap seq2map)]
        [clj-span.params     :only (set-global-params!)]
        [clj-misc.matrix-ops :only (make-matrix map-matrix resample-matrix get-rows get-cols)]
        [clj-span.sediment-model :only (aggregate-flow-dirs)]))

(def attach-disc-metadata   (p map-matrix #(with-meta % disc-type)))
(def attach-cont-metadata   (p map-matrix #(with-meta % cont-type)))
(def rationalize-probs      (p map-matrix (p mapmap identity rationalize)))
(def to-continuous-randvars (p map-matrix to-continuous-randvar))
(def make-discrete-layer    (& attach-disc-metadata rationalize-probs))

(def source-layer-discrete   (make-discrete-layer
                              [[{8 0.08, 5 0.28, 0 0.64} {6 0.03, 3 0.23, 4 0.74} {8 0.06, 5 0.06, 0 0.88}]
                               [{6 0.46, 0 0.37, 2 0.17} {2 0.08, 0 0.12, 9 0.8}  {8 0.25, 2 0.19, 9 0.56}]
                               [{1 0.09, 4 0.54, 3 0.37} {6 0.07, 1 0.0,  9 0.93} {3 0.44, 0 0.24, 4 0.32}]]))

(def source-layer-continuous (to-continuous-randvars source-layer-discrete))

(def sink-layer-discrete     (make-discrete-layer
                              [[{0 0.1,  7 0.75, 5 0.15} {5 0.09, 4 0.05, 1 0.86} {6 0.26, 5 0.12, 1 0.62}]
                               [{0 0.14, 1 0.0,  5 0.86} {6 0.07, 8 0.07, 9 0.86} {5 0.09, 9 0.09, 1 0.82}]
                               [{9 0.83, 1 0.16, 5 0.01} {3 0.58, 4 0.01, 5 0.41} {2 0.26, 5 0.11, 3 0.63}]]))

(def sink-layer-continuous   (to-continuous-randvars sink-layer-discrete))

(def use-layer-discrete      (make-discrete-layer
                              [[{2 0.08, 4 0.06, 1 0.86} {1 0.12, 2 0.2,  3 0.68} {1 0.05, 9 0.17, 0 0.78}]
                               [{4 0.74, 0 0.11, 9 0.15} {4 0.03, 8 0.12, 6 0.85} {9 0.72, 5 0.04, 1 0.24}]
                               [{1 0.18, 0 0.52, 2 0.3}  {0 0.3,  6 0.4,  5 0.3}  {9 0.41, 8 0.05, 4 0.54}]]))

(def use-layer-continuous    (to-continuous-randvars use-layer-discrete))

(def flow-layers-discrete    (mapmap identity make-discrete-layer
                                     {"Foo"        [[{1 0.1,  6 0.06, 4 0.84} {9 0.05, 4 0.86, 7 0.09} {8 0.17, 1 0.24, 6 0.59}]
                                                    [{3 0.07, 0 0.8,  2 0.13} {5 0.67, 0 0.25, 4 0.08} {6 0.84, 1 0.01, 3 0.15}]
                                                    [{4 0.01, 0 0.53, 2 0.46} {4 0.55, 9 0.38, 8 0.07} {4 0.14, 3 0.61, 9 0.25}]],

                                      "Bar"        [[{0 0.14, 6 0.37, 3 0.49} {1 0.42, 3 0.54, 5 0.04} {8 0.22, 2 0.63, 7 0.15}]
                                                    [{3 0.33, 2 0.51, 1 0.16} {5 0.05, 2 0.44, 3 0.51} {8 0.02, 3 0.04, 9 0.94}]
                                                    [{3 0.1,  1 0.35, 2 0.55} {5 0.04, 7 0.01, 6 0.95} {8 0.03, 5 0.01, 0 0.96}]],

                                      "Baz"        [[{7 0.02, 6 0.07, 1 0.91} {8 0.19, 9 0.19, 6 0.62} {0 0.23, 7 0.14, 5 0.63}]
                                                    [{5 0.01, 1 0.4,  3 0.59} {6 0.07, 3 0.22, 5 0.71} {3 0.21, 5 0.2,  4 0.59}]
                                                    [{7 0.43, 4 0.46, 2 0.11} {1 0.11, 9 0.0,  0 0.89} {4 0.06, 7 0.37, 9 0.57}]]

                                      "Hydrosheds" [[{32 1.0} {128 1.0} {128 1.0}]
                                                    [{ 4 1.0} {  0 1.0} {  4 1.0}]
                                                    [{32 1.0} { 16 1.0} { 16 1.0}]]}))

(def flow-layers-continuous  (mapmap identity to-continuous-randvars flow-layers-discrete))

(deftest zero-layer-below-threshold-test
  (is (= (zero-layer-below-threshold 3.5 source-layer-discrete)
         [[_0_ {6 0.03, 3 0.23, 4 0.74} _0_]
          [_0_ {2 0.08, 0 0.12, 9 0.8}  {8 0.25, 2 0.19, 9 0.56}]
          [_0_ {6 0.07, 1 0.0,  9 0.93} _0_]]))
  (is (= (zero-layer-below-threshold 2.7 source-layer-continuous)
         [[_0_ {6 1.00, 4 0.97, 3 0.23} _0_]
          [_0_ {9 1.00, 2 0.2,  0 0.12} {9 1.0, 8 0.44, 2 0.19}]
          [_0_ {9 1.00, 6 0.07, 1 0.0}  _0_]])))

(defn run-discrete-with-params
  [source-threshold sink-threshold use-threshold downscaling-factor]
  (preprocess-data-layers source-layer-discrete source-threshold
                          sink-layer-discrete   sink-threshold
                          use-layer-discrete    use-threshold
                          flow-layers-discrete  downscaling-factor))

(defn run-continuous-with-params
  [source-threshold sink-threshold use-threshold downscaling-factor]
  (preprocess-data-layers source-layer-continuous source-threshold
                          sink-layer-continuous   sink-threshold
                          use-layer-continuous    use-threshold
                          flow-layers-continuous  downscaling-factor))

(defn resample-rv-matrix
  [downscaling-factor layer]
  (resample-matrix (int (/ (get-rows layer) downscaling-factor))
                   (int (/ (get-cols layer) downscaling-factor))
                   rv-average
                   layer))

(defn resample-rv-matrix-map
  [downscaling-factor layers-map]
  (let [first-matrix (val (first layers-map))
        scaled-rows  (int (/ (get-rows first-matrix) downscaling-factor))
        scaled-cols  (int (/ (get-cols first-matrix) downscaling-factor))]
    (seq2map layers-map (fn [[name matrix]]
                          [name (resample-matrix
                                 scaled-rows
                                 scaled-cols
                                 (if (= name "Hydrosheds") aggregate-flow-dirs rv-average)
                                 matrix)]))))

(deftest preprocess-data-layers-test
  ;; No zeroing or resampling.
  (is (= (run-discrete-with-params 0.0 0.0 0.0 1)
         [source-layer-discrete sink-layer-discrete use-layer-discrete flow-layers-discrete]))

  (is (= (run-continuous-with-params 0.0 0.0 0.0 1)
         [source-layer-continuous sink-layer-continuous use-layer-continuous flow-layers-continuous]))

  ;; Just resampling down by a factor of 3. Only zeroing negative values.
  (is (= (run-discrete-with-params 0.0 0.0 0.0 3)
         [(zero-layer-below-threshold 0.0 (resample-rv-matrix 3 source-layer-discrete))
          (zero-layer-below-threshold 0.0 (resample-rv-matrix 3 sink-layer-discrete))
          (zero-layer-below-threshold 0.0 (resample-rv-matrix 3 use-layer-discrete))
          (resample-rv-matrix-map 3 flow-layers-discrete)]))

  (is (= (run-continuous-with-params 0.0 0.0 0.0 3)
         [(zero-layer-below-threshold 0.0 (resample-rv-matrix 3 source-layer-continuous))
          (zero-layer-below-threshold 0.0 (resample-rv-matrix 3 sink-layer-continuous))
          (zero-layer-below-threshold 0.0 (resample-rv-matrix 3 use-layer-continuous))
          (resample-rv-matrix-map 3 flow-layers-continuous)]))

  ;; Just zeroing some values. No resampling.
  (is (= (run-discrete-with-params 4.0 5.0 6.0 1)
         [(zero-layer-below-threshold 4.0 source-layer-discrete)
          (zero-layer-below-threshold 5.0 sink-layer-discrete)
          (zero-layer-below-threshold 6.0 use-layer-discrete)
          flow-layers-discrete]))

  (is (= (run-continuous-with-params 4.0 5.0 6.0 1)
         [(zero-layer-below-threshold 4.0 source-layer-continuous)
          (zero-layer-below-threshold 5.0 sink-layer-continuous)
          (zero-layer-below-threshold 6.0 use-layer-continuous)
          flow-layers-continuous]))

  ;; Zeroing and resampling down by a factor of 3.
  (is (= (run-discrete-with-params 4.0 5.0 6.0 3)
         [(zero-layer-below-threshold 4.0 (resample-rv-matrix 3 source-layer-discrete))
          (zero-layer-below-threshold 5.0 (resample-rv-matrix 3 sink-layer-discrete))
          (zero-layer-below-threshold 6.0 (resample-rv-matrix 3 use-layer-discrete))
          (resample-rv-matrix-map 3 flow-layers-discrete)]))

  (is (= (run-continuous-with-params 4.0 5.0 6.0 3)
         [(zero-layer-below-threshold 4.0 (resample-rv-matrix 3 source-layer-continuous))
          (zero-layer-below-threshold 5.0 (resample-rv-matrix 3 sink-layer-continuous))
          (zero-layer-below-threshold 6.0 (resample-rv-matrix 3 use-layer-continuous))
          (resample-rv-matrix-map 3 flow-layers-continuous)]))

  ;; Making sure it works fine with nils for sink-layer and flow-layers.
  (is (= (preprocess-data-layers source-layer-discrete 0.0
                                 nil                   0.0
                                 use-layer-discrete    0.0
                                 nil                   1)
         [source-layer-discrete
          (make-matrix (get-rows source-layer-discrete) (get-cols source-layer-discrete) (constantly _0_))
          use-layer-discrete
          {}]))

  (is (= (preprocess-data-layers source-layer-continuous 0.0
                                 nil                     0.0
                                 use-layer-continuous    0.0
                                 nil                     1)
         [source-layer-continuous
          (make-matrix (get-rows source-layer-continuous) (get-cols source-layer-continuous) (constantly _0_))
          use-layer-continuous
          {}])))

(deftest generate-results-map-test
  (let [rows                       (get-rows source-layer-discrete)
        cols                       (get-cols source-layer-discrete)
        scaled-discrete-layers     (run-discrete-with-params   0.4 0.5 0.6 1)
        scaled-continuous-layers   (run-continuous-with-params 0.4 0.5 0.6 1)
        get-discrete-results-map   #(apply generate-results-map % rows cols scaled-discrete-layers)
        get-continuous-results-map #(apply generate-results-map % rows cols scaled-continuous-layers)]
    (set-global-params! {:trans-threshold 0.01})
    (doseq [flow-model ["Carbon" "LineOfSight" "Proximity" "Sediment"]]
      (let [discrete-results-map   (get-discrete-results-map flow-model)
            continuous-results-map (get-continuous-results-map flow-model)]
        ;; Are the return values maps?
        (is (every? map? [discrete-results-map continuous-results-map]))
        ;; Are their vals closures?
        (is (every? (& (p every? fn?) vals) [discrete-results-map continuous-results-map]))
        ;; Do their keys correspond to the ordered menu items?
        (is (every? (& (p = ["Source - Theoretical"
                             "Source - Inaccessible"
                             "Source - Possible"
                             "Source - Blocked"
                             "Source - Actual"
                             "Sink   - Theoretical"
                             "Sink   - Actual"
                             "Use    - Theoretical"
                             "Use    - Inaccessible"
                             "Use    - Possible"
                             "Use    - Blocked"
                             "Use    - Actual"
                             "Flow   - Possible"
                             "Flow   - Blocked"
                             "Flow   - Actual"])
                       keys)
                    [discrete-results-map continuous-results-map]))))))

(deftest run-span-arg-preds
  (are      (double>0?      _1)  1.0 10.5 100.0)
  (are (not (double>0?      _1)) -1.0 -1 0.0 0 1 100M 100.0M \a \  "foo" 'bar :baz)
  (are      (double>=0?     _1)  0.0 1.0 10.5 100.0)
  (are (not (double>=0?     _1)) -1.0 -1 0 1 100M 100.0M \a \  "foo" 'bar :baz)
  (are      (integer>=1?    _1)  1 2 3 10000)
  (are (not (integer>=1?    _1)) -1 -100.0 0 0.0 2.0 1.0 3.5 10000M \a \  "foo" 'bar :baz)
  (are      (number>=1?     _1)  1 1.0 2 2.5 100M (double 20) 17/15)
  (are (not (number>=1?     _1)) -1 -100.0 0 0.99 15/17 (double 1/3) \a \  "foo" 'bar :baz)
  (are      (nil-or-matrix? _1)  nil (seq {}) [[1 2 3][4 5 6][7 8 9]] source-layer-discrete sink-layer-continuous)
  (are (not (nil-or-matrix? _1)) () [] {} [1 2 3] [#{1 2 3} #{4 5 6} #{7 8 9}] (to-array-2d [[1 2 3][4 5 6][7 8 9]])))

(deftest run-span-test
  (let [run-span-with-layers #(run-span {:source-layer  %1                    :source-threshold   0.0
                                         :sink-layer    %2                    :sink-threshold     0.0
                                         :use-layer     %3                    :use-threshold      0.0
                                         :flow-layers   %4                    :trans-threshold    0.25
                                         :rv-max-states 10                    :downscaling-factor 1
                                         :source-type   :infinite             :sink-type          :infinite
                                         :use-type      :infinite             :benefit-type       :non-rival
                                         :flow-model    "LineOfSight"         :result-type        :closure-map})
        run-span-with-args   #(run-span {:source-layer  source-layer-discrete :source-threshold   %1
                                         :sink-layer    sink-layer-discrete   :sink-threshold     %2
                                         :use-layer     use-layer-discrete    :use-threshold      %3
                                         :flow-layers   flow-layers-discrete  :trans-threshold    %4
                                         :rv-max-states %5                    :downscaling-factor %6
                                         :source-type   %7                    :sink-type          %8
                                         :use-type      %9                    :benefit-type       %10
                                         :flow-model    %11                   :result-type        %12})
        ;; Correct invocation
        discrete-run-0  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad source-threshold
        discrete-run-1  #(run-span-with-args
                           -0.1 0.0 0.0 0.25 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad sink-threshold
        discrete-run-2  #(run-span-with-args
                           0.0 0 0.0 0.25 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad use-threshold
        discrete-run-3  #(run-span-with-args
                           0.0 0.0 2 0.25 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad trans-threshold
        discrete-run-4  #(run-span-with-args
                           0.0 0.0 0.0 0.0 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad rv-max-states
        discrete-run-5  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10.3 1 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad downscaling-factor
        discrete-run-6  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 0 :infinite :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad source-type
        discrete-run-7  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 1 :foo :infinite :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad sink-type
        discrete-run-8  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 1 :infinite :bar :infinite :non-rival "LineOfSight" :closure-map)
        ;; Bad use-type
        discrete-run-9  #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 1 :infinite :infinite :baz :non-rival "LineOfSight" :closure-map)
        ;; Bad benefit-type
        discrete-run-10  #(run-span-with-args
                            0.0 0.0 0.0 0.25 10 1 :infinite :infinite :infinite :buz "LineOfSight" :closure-map)
        ;; Bad flow-model
        discrete-run-11  #(run-span-with-args
                            0.0 0.0 0.0 0.25 10 1 :infinite :infinite :infinite :non-rival "Giampiero" :closure-map)
        ;; Bad result-type
        discrete-run-12 #(run-span-with-args
                           0.0 0.0 0.0 0.25 10 1 :infinite :infinite :infinite :non-rival "LineOfSight" :diopollo)
        ;; Bad source-layer
        discrete-run-13 #(run-span-with-layers nil sink-layer-discrete use-layer-discrete flow-layers-discrete)
        ;; Bad sink-layer
        discrete-run-14 #(run-span-with-layers source-layer-discrete "sink-layer" use-layer-discrete flow-layers-discrete)
        ;; Bad use-layer
        discrete-run-15 #(run-span-with-layers source-layer-discrete sink-layer-discrete nil flow-layers-discrete)
        ;; Bad flow-layers
        discrete-run-16 #(run-span-with-layers source-layer-discrete sink-layer-discrete use-layer-discrete {"Foobar" "not-a-matrix"})
        ;; Differently sized layers
        discrete-run-17 #(run-span-with-layers [[1 2][3 4]] [[1 2][3 4]] [[1 2][3 4]] {"Foobar" [[1 2 3][4 5 6][7 8 9]]})]

    ;; Testing the argument-checking constraints (they should be
    ;; invariant of whether I use discrete or continuous input layers,
    ;; so I'm just using the discrete ones to keep the tests shorter).

    ;; Should be no errors.
    (is (map? (discrete-run-0)))

    ;; Each should throw a different assertion exception.
    (are (thrown-with-msg? _2 _3 (_1))
         discrete-run-1  Exception #"Assert.*source-threshold.*"
         discrete-run-2  Exception #"Assert.*sink-threshold.*"
         discrete-run-3  Exception #"Assert.*use-threshold.*"
         discrete-run-4  Exception #"Assert.*trans-threshold.*"
         discrete-run-5  Exception #"Assert.*rv-max-states.*"
         discrete-run-6  Exception #"Assert.*downscaling-factor.*"
         discrete-run-7  Exception #"Assert.*source-type.*"
         discrete-run-8  Exception #"Assert.*sink-type.*"
         discrete-run-9  Exception #"Assert.*use-type.*"
         discrete-run-10 Exception #"Assert.*benefit-type.*"
         discrete-run-11 Exception #"Assert.*flow-model.*"
         discrete-run-12 Exception #"Assert.*result-type.*"
         discrete-run-13 Exception #"Assert.*is-matrix\?.*source-layer.*"
         discrete-run-14 Exception #"Assert.*nil-or-matrix\?.*sink-layer.*"
         discrete-run-15 Exception #"Assert.*is-matrix\?.*use-layer.*"
         discrete-run-16 Exception #"Assert.*nil-or-matrix\?.*flow-layers.*"
         discrete-run-17 Exception #"Assert.*grids-align\?.*")))

;;(run-tests)
