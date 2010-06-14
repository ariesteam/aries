;; Tests can be written in separate functions
(deftest add-test
  (is (= 4 (+ 2 2)))
  (is (= 2 (+ 2 0)) "adding zero doesn't change value"))

(deftest reverse-test
  (is (= [3 2 1] (reverse [1 2 3]))))

;; Tests can verify that a specific exception is thrown
(deftest division-test
  (is (thrown? ArithmeticException (/ 3.0 0))))

;; The with-test macro can be used to add tests to the functions they
;; test as metadata
(with-test
  (defn my-add [n1 n2] (+ n1 n2))
  (is (= 4 (my-add 2 2)))
  (is (= 2 (my-add 2 0)) "adding zero doesn't change value"))

;; The "are" macro takes a predicate template and multiple sets of
;; arguments to it, but no message.  Each set of arguments are
;; substituted one at a time into the predicate template and
;; evaluated.
(deftest multiplication
  (are (= (* _1 _2) _3) ; a template
       1 1 1,
       1 2 2,
       2 3 6))

;; When AOT compiling Clojure code to bytecode for production use,
;; bind the *load-tests* symbol to false to avoid compiling test code.

;; Fixture functions can be defined to setup and teardown an
;; environment in which the tests should be run.
(defn fixture-1 [test-function]
  ;; Perform setup here
  (println "Fixture-1 setup")
  ;; Run the test(s)
  (test-function)
  ;; Perform teardown here
  (println "Fixture-1 teardown"))

(defn fixture-2 [test-function]
  ;; Perform setup here
  (println "Fixture-2 setup")
  ;; Run the test(s)
  (test-function)
  ;; Perform teardown here
  (println "Fixture-2 teardown"))

;; To register fixtures to wrap each test method:
(use-fixtures :each fixture-1 fixture-2)

;; The order of execution will be:
;; 1. fixture-1 setup
;; 2. fixture-2 setup
;; 3. run the first test function
;; 4. fixture-2 teardown
;; 5. fixture-1 teardown
;; 6. Repeat for the next test function

;; To register fixtures to wrap the entire run:
(use-fixtures :once fixture-1 fixture-2)

;; The order of execution will be:
;; 1. fixture-1 setup
;; 2. fixture-2 setup
;; 3. run all the test functions
;; 4. fixture-2 teardown
;; 5. fixture-1 teardown

;; Run all the tests in the current namespace.  This includes tests
;; that were added as function metadata using with-test.  Other
;; namespaces can be specified as quoted arguments.
(run-tests)

;; If you've compiled your tests, you can run them with "ant test".
