(ns sicp-clojure.1-1-1-exercises
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (expt)]
            [sicp-clojure.1-1-1-samples :as s]))

;;; Exercise 1.1
;; Below is a sequence of expressions. What is the result printed
;; by the interpreter in response to each expression? Assume that the
;; sequence is to be evaluated in the order in which it is presented.
; See tests at the bottom.


;;; Exercise 1.2
;; Translate the following expression into prefix form.
(defn prefix-expr []
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))


;;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.
;; Have a look at the excellent answer to my doubt on stackoverflow:
;; http://stackoverflow.com/questions/25096585/let-binding-sequence-as-input-to-map-exception-thrown/
(defn square-sum-largest-pair [a b c]
  "Sums the square of the largest two number of the three in input."
  (let [[fst snd] (sort > [a b c])]
        (+ (m/expt fst 2) (m/expt snd 2))))


;;; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure.
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; The result of the if is an primitive procedure which in turn, when evaluated, will be used
;; to sum a to b (if b is greater than zero) or subtract b from a (if b is less or
;; equal zero).


;;; Exercise 1.5
(defn p [] (p))

(defn ben-bitdiddle-test [x y]
  (if (= x 0) 0 y))

;; applicative-order would be:
;; (ben-bitdiddle-test 0 (p))
;; (if (= 0 0) 0 (p)) ; we retrieve the body of ben-bitdiddle-test and formal parameters are replaced.
;; (if (= 0 0) 0 (p)) ; p is evaluated because it is an expression and needs evaluation in order
                      ; to execute the next if
;; (if (= 0 0) 0 (p)) ; p evaluates to itself.
;; (if (= 0 0) 0 (p)) ; same
;; (stack overflow)

;; normal-order would be:
;; (ben-bitdiddle-test 0 (p))
;; (if (= 0 0) 0 (p)) ; just substitution of symbols
;; (if (= 0 0) 0 (p)) ; as per the special form if, the predicate is evaluted first.
;; (0)

; (ben-bitdiddle-test 0 (p)) ; Uncommenting this will cause a stack overflow


;;; Exercise 1.6
(defn new-if [predicate then-clause else-clause]
  (cond
   (do predicate) then-clause
   :else else-clause))

(defn sqrt-iter-alyssa [guess x]
  (new-if (s/good-enough? guess x)
          guess
          (sqrt-iter-alyssa (s/improve guess x) x)))

(defn sqrt-alyssa [x]
  (sqrt-iter-alyssa 1.0 x))

;; The new-if clauses are not evaluated only when the predicate is true, but right after
;; the substitution of the body of new-if in sqrt-iter-alyssa.
;; This is the normal behavior of an applicative-order interpreter as stated in section 1.1.3.
;; As a consequence, in this case the else-clause, which is a recursive call to sqrt-iter-alyssa,
;; gets evaluated with always the same parameters and will therefore produce a stack overflow.

; (sqrt-alyssa 2) ; Uncommenting this will cause a stack overflow


;;; Exercise 1.7
;; The good-enough? test used in computing square roots will not be very effective for finding
;; the square roots of very small numbers. Also, in real computers, arithmetic operations are
;; almost always performed with limited precision. This makes our test inadequate for very large
;; numbers. Explain these statements, with samples showing how the test fails for small and large
;; numbers.

;; The test with a tiny number fails because the guess is starting from a number that will
;; not be significantly changed by the subtraction of the tiny x.
;; As soon as the guess (squared) reaches the threshold (0.001 in the book), < will
;; evaluated to true.
;; Ex.: (At some point) (< (abs (- (m/expt 0.03125 2) 1.40e-30)) 0.001))
; (s/sqrt 1.4e-30) ; This won't return a correct result.

;; With such a big number (almost DOUBLE_MAX) the < test will never be true because there
;; is no space, in the floating point representation of the number, for decimals.
; (s/sqrt 1.79e+308) ; Uncommenting this will cause a stack overflow

(defn better-good-enough? [guess prev-guess]
  (< (m/abs (- guess prev-guess)) 1.0e-30))

(defn better-sqrt-iter [guess prev-guess x]
  (if (better-good-enough? guess prev-guess)
    guess
    (better-sqrt-iter (s/improve guess x) guess x)))

(defn better-sqrt [x]
  (better-sqrt-iter 1.0 0.0 x))

;; The better-good-enough? function should be better for small numbers, as the iteration
;; will go on and on until the new guess cannot be improved anymore and, consequently,
;; (m/abs (- guess prev-guess)) will be equal to more than the tiny threshold.
(better-sqrt 1.4e-30)

;; For big numbers it is the same.
(better-sqrt 1.7e+308)

;; An alternative strategy for implementing good-enough? is to watch how guess changes from one
;; iteration to the next and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test. Does this work better for
;; small and large numbers?


;;; Exercise 1.8
(defn cube-improve [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defn cube-root-iter [guess prev-guess x]
  (if (better-good-enough? guess prev-guess)
    guess
    (cube-root-iter (cube-improve guess x) guess x)))

(defn cube-root [x]
  (cube-root-iter 1.0 0.0 x))


(t/deftest tests
  (t/is (= 10 10) "1.1\\) Checking results.")
  (t/is (= 12 (+ 5 3 4)) "1.1\\) Checking results.")
  (t/is (= 8 (- 9 1)) "1.1\\) Checking results.")
  (t/is (= 3 (/ 6 2)) "1.1\\) Checking results.")
  (t/is (= 6 (+ (* 2 4) (- 4 6))) "1.1\\) Checking results.")
  (def a 3)
  (def b (+ a 1))
  (t/is (= 19 (+ a b (* a b))) "1.1\\) Checking results.")
  (t/is (= false (= a b)) "1.1\\) Checking results.")
  (t/is (= 4 (if (and (> b a) (< b (* a b)))
               b
               a)) "1.1\\) Checking results.")
  (t/is (= 16 (cond
               (= a 4) 6
               (= b 4) (+ 6 7 a)
               true 25)) "1.1\\) Checking results.")
  (t/is (= 6 (+ 2 (if (> b a) b a))))
  (t/is (= 16 (* (cond (> a b) a
                       (< a b) b
                       true -1)
                 (+ a 1))) "1.1\\) Checking results.")
  (t/is (= (prefix-expr) -37/150) "1.2\\) Should result in -37/150")
  (t/is (= (square-sum-largest-pair 1 4 5) 41) "1.3\\) Should correctly sum 1 4 5.")
  (t/is (= (square-sum-largest-pair 5 4 1) 41) "1.3\\) Should correctly sum 5 4 1.")
  (t/is (= (square-sum-largest-pair 4 5 1) 41) "1.3\\) Should correctly sum 4 5 1.")
  (t/is (= (square-sum-largest-pair 1 5 4) 41) "1.3\\) Should correctly sum 1 5 4.")
  (t/is (= (square-sum-largest-pair 4 1 5) 41) "1.3\\) Should correctly sum 4 1 5.")
  (t/is (= (square-sum-largest-pair 5 1 4) 41) "1.3\\) Should correctly sum 5 1 4.")
  (t/is (= 42 (a-plus-abs-b 40 -2)) "1.4\\) The answer is always 42.")
  (t/is (= 42 (a-plus-abs-b 40 2)) "1.4\\) The answer is always 42.")
  (t/is (= 5 (new-if (= 2 3) 0 5)) "1.6 \\) Alyssa test should be 5.")
  (t/is (= 0 (new-if (= 1 1) 0 5)) "1.6 \\) Alyssa test should be 0.")
  (t/is (s/equal-to? 3 (cube-root 27)) "1.8 \\) Cube root of 27.")
  (t/is (s/equal-to? 4.32674871092222 (cube-root 81)) "1.8 \\) Cube root of 81.")
  (t/is (s/equal-to? 1.1186889420813968E-10 (cube-root 1.4e-30)) "1.8 \\) Cube root of a small number.")
  (t/is (s/equal-to? 5.539658256754465E102 (cube-root 1.7e+308)) "1.8 \\) Cube root of a big number."))
