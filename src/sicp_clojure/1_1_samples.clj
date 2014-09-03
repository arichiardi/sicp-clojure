(ns sicp-clojure.1-1-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (expt abs)]
            [sicp-clojure.utils :as u]))

;;; 1.1.7 Example: Square Roots by Newton's Method
;; No changes from the original
(defn- average [x y]
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

;; My take at the algorithm looking at page 87 and at section "Comparing Floating-point Numbers"
;; of http://gettingclojure.wikidot.com/cookbook:numbers

(defn improve* [& {:keys [guess x]}]
  (average (/ x guess) guess))

(defn- sqrt-iter* [& {:keys [guess x]}]
  "Guesses the sqrt of x."
  (cond (u/equal-to? x (m/expt guess 2)) guess
        (>= guess x) "Error, stack overflow foreseen."
        :else (sqrt-iter* :x x :guess (improve* :x x :guess guess))))

(defn sqrt* [x]
  (sqrt-iter* :x x :guess 1.0))


(t/deftest tests
  (t/is (u/equal-to? (+ 0.1 0.1 0.1 0.1 0.1 0.1) (* 6 0.1)) "Small number equality test.")
  (t/is (u/equal-to? 1.23456e38 (* 1.23456 (m/expt 10 38))) "Big number equality test.") ; doesn't work with big numbers
  (t/is (u/equal-to? 1.4142157 (sqrt 2)) "Sqrt of 2 should be equal to what?")
  (t/is (u/equal-to? 1.111107556e19 (sqrt* 1.23456e38)))
  (t/is (u/equal-to? 1.4142157 (sqrt* 2)) "Sqrt (custom) of 2 should be equal to what?"))
