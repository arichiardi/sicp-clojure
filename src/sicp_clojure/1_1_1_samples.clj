(ns sicp-clojure.1-1-1-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]))

;;;; 1.1.7  Example: Square Roots by Newton's Method
;; No changes from the original
(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (m/abs (- (* guess guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))


;; My take to the algorithm looking at page 87 and at section "Comparing Floating-point Numbers"
;; of http://gettingclojure.wikidot.com/cookbook:numbers
(defn equality-comparison-fn [epsilon]
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the epsilon
  given to this one. The returned fn doesn't work with big numbers."
  (fn [x y]
    (< (m/abs (- x y)) epsilon)))

(defn equality-comparison-with-scale-fn [epsilon]
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the epsilon
  given to this one."
  (fn [x y]
    (let [scale (if (and (not (zero? x)) (not (zero? y))) (m/abs x) 1)]
      (< (m/abs (- x y)) (* scale epsilon)))))

(def equal-to?
  "Comparing two numbers using equality-with-difference-fn with epsilon = 0.0001."
  (equality-comparison-with-scale-fn 0.0001))

(defn- average*
  "Calculates the average of two or more numbers."
  ([] 0)
  ([xs] (/ (reduce + xs) (double (count xs)))))

(defn improve* [& {:keys [guess x]}]
  (average* [(/ x guess) guess]))

(defn- sqrt-iter* [& {:keys [guess x]}]
  "Guesses the sqrt of x."
  (cond (equal-to? x (m/expt guess 2)) guess
    (>= guess x) "Error, stack overflow foreseen."
    :else (sqrt-iter* :x x :guess (improve* :x x :guess guess))))

(defn sqrt* [x]
  (sqrt-iter* :x x :guess 1.0))

(t/deftest chap1-examples
  (t/is (equal-to? (+ 0.1 0.1 0.1 0.1 0.1 0.1) (* 6 0.1)) "Small number equality test.")
  (t/is (equal-to? 1.23456e38 (* 1.23456 (m/expt 10 38))) "Big number equality test.") ; doesn't work with big numbers
  (t/is (equal-to? 2.5 (average* [3 2])) "Average of [3 2]")
  (t/is (equal-to? 2 (average* [3 2 1])) "Average of [3 2 1]")
  (t/is (equal-to? 6.1 (average* [3.7 4.1 9.3 12.4 1])) "Average of [3.7 4.1 9.3 12.4 1]")
  (t/is (equal-to? 1.4142157 (sqrt 2)) "Sqrt of 2 should be equal to what?")
  (t/is (equal-to? 1.111107556e19 (sqrt* 1.23456e38)))
  (t/is (equal-to? 1.4142157 (sqrt* 2)) "Sqrt (custom) of 2 should be equal to what?"))
