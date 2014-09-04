(ns sicp-clojure.utils
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as m :refer (round floor expt abs)]))


(defn make-file-from-resource [name] (io/file (io/resource name)))


(defn to-bit-representation [^java.lang.Double d]
  (-> (Double/doubleToRawLongBits d) (java.lang.Long/toBinaryString)))


(defn methods-of [instance & [string]]
  "Returns the \"[...] public member methods of the class or interface represented by this Class
  object, including those declared by the class or interface and those inherited from superclasses
  and superinterfaces.\". See java.lang.Class/getMethods for details.
  If a second argument is provided as valid regex o string, the list is filtered according to it."
  {:pre [(not (nil? instance))]}
  (filter #(re-find (re-pattern (or string #".*")) %) (map #(.getName %) (-> instance class .getMethods))))


;; from www.bestinclass.dk/index.clj/2010/02/benchmarking-jvm-languages.html
(defmacro microbench [n expr]
  "Evaluates the expression n number of times, returning the average
  time spent in computation, removing highest and lowest values.

  If the body of expr returns nil, only the timing is returned otherwise
  the result is printed - does not affect timing.

  Before timings begin, a warmup is performed lasting either 1 minute or
  1 full computational cycle, depending on which comes first."
  {:pre [(> n 2)]}
  `(let [warm-up#  (let [start# (System/currentTimeMillis)]
                     (println "Warming up!")
                     (while (< (System/currentTimeMillis) (+ start# (* 60 1000)))
                       (with-out-str ~expr)
                       (System/gc))
                     (println "Benchmarking..."))
         timings#  (doall
                    (for [pass# (range ~n)]
                      (let [start#    (System/nanoTime)
                            retr#     ~expr
                            timing#   (/ (double (- (System/nanoTime) start#))
                                         1000000.0)]
                        (when retr# (println retr#))
                        (System/gc)
                        timing#)))
         runtime#  (reduce + timings#)
         highest#  (apply max timings#)
         lowest#   (apply min timings#)]
     (println "Total runtime: " runtime#)
     (println "Highest time : " highest#)
     (println "Lowest time  : " lowest#)
     (println "Average      : " (/ (- runtime# (+ highest# lowest#))
                                   (- (count timings#) 2)))
     timings#))


(defn round-to-p-decimals [x p]
  "Rounds a number x to a precision of p significant digits."
  (/ (m/round (* (double x) (m/expt 10 p))) (double (m/expt 10 p))))


(defn- equality-comparison-fn [epsilon]
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the input epsilon.
  With this implementation, the comparison fn doesn't work with big numbers."
  (fn [x y]
    (< (m/abs (- x y)) epsilon)))


(defn- equality-comparison-with-scale-fn [epsilon]
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the epsilon
  is used for comparing them."
  (fn [x y]
    (let [scale (if (and (not (zero? x)) (not (zero? y))) (m/abs x) 1)]
      (< (m/abs (- x y)) (* scale epsilon)))))


(def equal-to?
  "Comparing two numbers using equality-with-difference-fn with epsilon = 0.0001."
  (equality-comparison-with-scale-fn 0.001))


(defn square [x]
  "Calculates the square of x."
  (*' x x))

(defn cube [x]
  "Calculates the cube of x."
  (*' x x x))

(defn average
  "Calculates the average of two or more numbers."
  ([] 0)
  ([& xs] (/ (reduce + xs) (double (count xs)))))


(defn average-damp [f]
  "Returns a function that uses average damping to make the approximations (of x) converge.
  Namely, given a function f, we consider the function whose value at x is equal to
  the average of x and f(x). [sicp, 1.3.4 Procedures as Returned Values]"
  (fn [x] (average x (f x))))

(t/run-tests)
(t/deftest tests
  (t/is (empty? (filter #((not (.contains "Value")) %1) (methods-of java.lang.Double "Value"))))
  (t/is (= 1.342 (round-to-p-decimals 1.3415 3)))
  (t/is (= 1.341 (round-to-p-decimals 1.3412 3)))
  (t/is (= 0.0 (round-to-p-decimals 0 1)))
  (t/is (equal-to? 2.5 (average 3 2)) "Average of [3 2]")
  (t/is (equal-to? 2 (average 3 2 1)) "Average of [3 2 1]")
  (t/is (equal-to? 6.1 (average 3.7 4.1 9.3 12.4 1)) "Average of [3.7 4.1 9.3 12.4 1]")
  (t/is (= 55.0 ((average-damp (fn [x] (* x x))) 10)))
  (t/is (= 9 (square 3)))
  (t/is (= 27 (cube 3))))
