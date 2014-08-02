(ns sicp.chap1
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as p]))

; Exercise 1.1
;; Below is a sequence of expressions. What is the result printed
;; by the interpreter in response to each expression? Assume that the
;; sequence is to be evaluated in the order in which it is presented.

(t/deftest chap1-1.1
  (t/is (= 10 10))
  (t/is (= 12 (+ 5 3 4)))
  (t/is (= 8 (- 9 1)))
  (t/is (= 3 (/ 6 2)))
  (t/is (= 6 (+ (* 2 4) (- 4 6))))

  (t/is (= #'sicp.chap1/a (def a 3)))
  (t/is (= #'sicp.chap1/b (def b (+ a 1))))

  (t/is (= 19 (+ a b (* a b))))
  (t/is (= false (= a b)))

  (t/is (= 4 (if (and (> b a) (< b (* a b)))
                b
                a)))

  (t/is (= 16 (cond
               (= a 4) 6
               (= b 4) (+ 6 7 a)
               true 25)))

  (t/is (= 6 (+ 2 (if (> b a) b a))))

  (t/is (= 16 (* (cond (> a b) a
                     (< a b) b
                     true -1)
             (+ a 1)))))


; Exercise 1.2
;; Translate the following expression into prefix form.
(defn prefix-expr []
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))))
     (* 3 (- 6 2) (- 2 7)))


; Exercise 1.3
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.
;; Have a look at the excellent answer to my doubt on stackoverflow:
;; http://stackoverflow.com/questions/25096585/let-binding-sequence-as-input-to-map-exception-thrown/
(defn square-sum-largest-pair [a b c]
  "Sums the square of the largest two number of the three in input."
  (let [max-of-a-b (max a b)
        min-of-a-b (min a b)]
        (+ (m/expt max-of-a-b 2) (m/expt (max min-of-a-b c) 2))))


; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure.
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; The result of if is a function, which, being in first position after
;; the if, when it is evaluated it will be used to sum (or subtract) a and b.


; Exercise 1.5
(defn p [] (p))

(defn ben-bitdiddle-test [x y]
  (if (= x 0) 0 y))

;; applicative-order would be:
;;; (ben-bitdiddle-test 0 p)
;;; (if (= 0 0) 0 p)
;;; (if (= 0 0) 0 (p)) ; p is evaluated before the if.
;;; (if (= 0 0) 0 (p)) ; p evaluates to its own evaluation.
;;; (if (= 0 0) 0 (p)) ; same
;;; (stack overflow)

;; normal-order would be:
;;; (ben-bitdiddle-test 0 p)
;;; (if (= 0 0) 0 p)
;;; (if (= 0 0) 0 (p)) ; just substitution, not evaluation.
;;; (0)

;; (ben-bitdiddle-test 0 (p)) ; Uncommenting this will cause a stack overflow, which
;; means that Clojure's interpreter is evaluating in applicative-order.


(t/deftest chap1-others
  (t/is (= (prefix-expr) -37/150) "1.2\\) Should result in -37/150")
  (t/is (= (square-sum-largest-pair 1 4 5) 41) "1.3\\) Should correctly sum 1 4 5.")
  (t/is (= (square-sum-largest-pair 5 4 1) 41) "1.3\\) Should correctly sum 5 4 1.")
  (t/is (= (square-sum-largest-pair 4 5 1) 41) "1.3\\) Should correctly sum 4 5 1.")
  (t/is (= (square-sum-largest-pair 1 5 4) 41) "1.3\\) Should correctly sum 1 5 4.")
  (t/is (= (square-sum-largest-pair 4 1 5) 41) "1.3\\) Should correctly sum 4 1 5.")
  (t/is (= (square-sum-largest-pair 5 1 4) 41) "1.3\\) Should correctly sum 5 1 4.")
  (t/is (= 42 (a-plus-abs-b 40 -2)) "1.4\\) The answer is always 42")
  (t/is (= 42 (a-plus-abs-b 40 2)) "1.4\\) The answer is always 42"))
