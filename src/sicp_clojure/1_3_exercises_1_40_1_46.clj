(ns sicp-clojure.1-3-exercises-1-40-1-46
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
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
;; the procedure that computes the nth repeated application of f.

(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))


;;; Exercise 1.44
;; If f is a function and dx is some small number, then the smoothed version of f is the function whose
;; value at a point x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes
;; as input a procedure that computes f and returns a procedure that computes the smoothed f.

(defn smooth [f]
  (let [dx 0.00001]
    (fn [x] (u/average (f (- x dx)) (f x) (f (+ x dx))))))

(defn n-fold-smooth [f n]
  ((repeated smooth n) f))

;; Tests from http://danboykis.com/2009/07/exercise-1-44-of-sicp/
(def ^:private step-fn (fn [x] (if (< x 0) 0.0 1.0)))


;;; Exercise 1.45
;; Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point
;; search based upon repeated average damping of y -> x/y^n-1.
;; Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the
;; repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

(defn nth-damp [f n] ((repeated u/average-damp n) f))

(defn fourth-root [x]
  (s/fixed-point (nth-damp (fn [y] (/ x (u/cube y))) 2) 1.0))

;; Average damping is stable up to x^3.
;; Average damping twice is stable up to x^7.
;; Average damping three times is stable up to x^15.
;; Ergo:

(defn nth-root [x n]
  {:pre [(> n 0)]}
  (s/fixed-point (nth-damp (fn [y] (/ x (m/expt y (- n 1))))
                           (long (m/floor (u/log n 2)))) 1.0))


;;; Exercise 1.46
;; Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is
;; good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes
;; a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure and the fixed-point
;; procedure of section 1.3.3 in terms of iterative-improve.

(defn iterative-improve [good-enough? improve]
  (defn iterate-on-guess [guess]
    (if (good-enough? guess)
      guess
      (iterate-on-guess (improve guess))))
  iterate-on-guess)

;; Implementing sqrt

(defn sqrt [x]
  (def good-enough? (fn [guess] (u/equal-to? (u/square guess) x)))
  ((iterative-improve good-enough? (u/average-damp (fn [y] (/ x y)))) 1.0))

;; Implementing fixed-point

(def ^:private tolerance 0.00001)

(defn fixed-point [f first-guess]
  (def good-enough? (fn [guess] (u/equal-to? guess (f guess))))
  ((iterative-improve good-enough? f) first-guess))

(t/run-tests)
(t/deftest tests
  (u/equal-to? 5.0 (s/newtons-method (cubic 3 -10 0) 1.0))
  (t/is (= 21 (((double* (double* double*)) inc) 5)))
  (t/is (= 49 ((compose u/square inc) 6)))
  (t/is (= 37 ((compose inc u/square) 6)))
  (t/is (= 21 ((repeated inc 16) 5)))
  (t/is (= 625 ((repeated u/square 2) 5)))
  (t/is (u/equal-to? 0.6666666666666666 ((n-fold-smooth step-fn 1) 0.0)))
  (t/is (u/equal-to? 0.5409601581500249 ((n-fold-smooth step-fn 15) 0.0)))
  (t/is (u/equal-to? 2.0 (fourth-root 16)))
  (t/is (u/equal-to? 3.0 (fourth-root 81)))
  (t/is (u/equal-to? 2.0 (nth-root 1024 10)))
  (t/is (u/equal-to? 2.0 (nth-root 65536 16)))
  (t/is (u/equal-to? 3.0 (nth-root 243 5)))
  (t/is (u/equal-to? 4.0 (sqrt 16)))
  (t/is (u/equal-to? 0.7390822985224023 (fixed-point u/cos 1.0)))
  (t/is (u/equal-to? 1.2587315962971173 (fixed-point (fn [x] (+ (u/sin x) (u/cos x))) 1.0))))

