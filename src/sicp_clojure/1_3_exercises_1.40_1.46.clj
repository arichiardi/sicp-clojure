(ns sicp-clojure.1-3-exercises
  (:require [clojure.test :as t]
            [clojure.tools.trace :as tr]
            [sicp-clojure.utils :as u]
            [sicp-clojure.1-3-samples :as s]))

;;; Exercise 1.40
;; Define a procedure cubic that can be used together with the newtons-method [...] to approximate
;; zeros of the cubic x3 + ax2 + bx + c.

(defn cubic [a b c]
  (fn [x] (+ (u/cube x) (* a (u/square x)) (* b x) c)))

; See tests at the bottom.


;;; Exercise 1.41
;; Define a procedure double that takes a procedure of one argument as argument and returns a procedure
;; that applies the original procedure twice.

(defn double* [f]
  (fn [x] (f (f x))))

;; The inner double* applies inc four times. The outer double* applies the inner twice resulting in
;; sixteen application of f. Applying inc sixteen times to 5.0 returns 21.0


;;; Exercise 1.42
;; Let f and g be two one-argument functions. The composition f after g is defined to be the function
;; x -> f(g(x)). Define a procedure compose that implements composition.

(defn compose [f g]
  (fn [x] (f (g x))))


;;; Exercise 1.43
;; Write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns
;; the procedure that computes the nth repeated application of f

(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))


(t/deftest tests
  (u/equal-to? 5.0 (s/newtons-method (cubic 3 -10 0) 1.0))
  (t/is (= 21 (((double* (double* double*)) inc) 5)))
  (t/is (= 49 ((compose u/square inc) 6)))
  (t/is (= 625 ((repeated u/square 2) 5))))
