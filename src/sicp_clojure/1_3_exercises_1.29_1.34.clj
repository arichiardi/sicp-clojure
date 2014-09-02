(ns sicp-clojure.1-3-exercises
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (expt sqrt abs gcd)]
            [sicp-clojure.1-3-samples :as s]))

;;; Exercise 1.29
;; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral,
;; computed using Simpson's Rule.

;; The following solution is not mathematically equivalent to the Simpson's approximation.
;; I am leaving it for the records, but it was a failed attempt to find a good implementation.

;; (defn simpson-integral [f a b n]
;;   {:pre? [(even? n)]}
;;   (def h (/ (- b a) (double n)))
;;   (def half-n (quot n 2))
;;   (defn term [a] (f a))
;;   (defn next-a [a] (+ a h))
;;   (/ (* (/ h 3) (+ (* 4 half-n) (* 2 (- half-n 1))) (s/sum term a next-a b)) n))

;; The correct solution quickly converges to the correct approximation.

(defn simpson-integral [f a b n]
  {:pre? [(even? n)]}
  (def h (/ (- b a) (double n)))
  (defn yk [a k] (f (+ a (* k h))))
  (defn term [k]
    (cond (or (= k 0) (= k n)) (yk a k)
          (even? k) (* 2 (yk a k))
          :else (* 4 (yk a k))))
  (* (/ h 3) (s/sum term 0 inc n)))


;;; Exercise 1.30
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that
;; the sum is performed iteratively. Show how to do this by filling in the missing expressions
;; in the following definition:

(defn sum* [term a next-a b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-a a) (+ result (term a)))))
  (iter a 0))

(defn triangular-sum [a b]
  (defn term [a] (quot (+ (* a a) a) 2))
  (sum* term a inc b))


;;; Exercise 1.31
;; a. Write an analogous procedure called product that returns the product of the values of a
;; function at points over a given range. Show how to define factorial in terms of product.
;; Also use product to compute approximations to pi.

(defn product [term a next-a b]
  (if (> a b)
    1
    (* (term a) (product term (next-a a) next-a b))))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi-approx [n]
  (defn term [n] (* (/ (double n) (- n 1)) (/ n (+ n 1))))
  (defn next-a [n] (+ n 2))
  (* 2 (product term 2 next-a n)))

;; b.  If your product procedure generates a recursive process, write one that generates an
;; iterative process. If it generates an iterative process, write one that generates a recursive
;; process.

(defn product* [term a next-a b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-a a) (* result (term a)))))
  (iter a 1))

(defn factorial-iter [n]
  (product* identity 1 inc n))


;;; Exercise 1.32
;; a. Show that sum and product (exercise 1.31) are both special cases of a still more general
;; notion called accumulate that combines a collection of terms, using some general accumulation
;; function that specifies how the current term is to be combined with the accumulation of the
;; preceding terms and a null-value that specifies what base value to use when the terms run out.
;; Write accumulate and show how sum and product can both be defined as simple calls to accumulate.

(defn accumulate [combiner null-value term a next-a b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next-a a) next-a b))))

(defn sum-acc [term a next-a b]
  (defn sum-combiner [term next-t] (+ term next-t))
  (accumulate sum-combiner 0 term a next-a b))

(defn product-acc [term a next-a b]
  (defn product-combiner [term next-t] (*' term next-t))
  (accumulate product-combiner 1 term a next-a b))

(defn geometric-sum [ratio a n]
  (defn term [n] (* (double a) (m/expt ratio n)))
  (sum-acc term 0 inc n))

(defn sine-product [x n]
  (defn term [n] (- 1.0 (/ (* x x) (* n n Math/PI Math/PI))))
  (* x (product-acc term 1 inc n)))

;; b. If your accumulate procedure generates a recursive process, write one that generates an
;; iterative process. If it generates an iterative process, write one that generates a recursive
;; process.

(defn accumulate* [combiner null-value term a next-a b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-a a) (combiner result (term a)))))
  (iter a null-value))

;; Using recur allows Clojure's interpreter/compiler to effectively optimize tail recursion.
;; For instance, (sine-product* (/ Math/PI 4) 100000000) will not work stack overflow.
(defn accumulate-recur [combiner null-value term a next-a b]
  (defn iter [a result]
    (if (> a b)
      result
      (recur (next-a a) (combiner result (term a)))))
  (iter a null-value))

(defn product-acc* [term a next-a b]
  (defn product-combiner [term next-t] (*' term next-t))
  (accumulate* product-combiner 1 term a next-a b))

(defn sine-product* [x n]
  (defn term [n] (- 1.0 (/ (* x x) (* n n Math/PI Math/PI))))
  (* x (product-acc* term 1 inc n)))


;;; Exercise 1.33

(defn filtered-accumulate [predicate combiner null-value term a next-a b]
  (defn iter [a result]
    (cond (> a b) result
          (predicate a) (iter (next-a a) (combiner result (term a)))
          :else (iter (next-a a) result)))
  (iter a null-value))

;; Using Java's BigInteger.isProbablePrime
(defn prime? [n]
  (.isProbablePrime (BigInteger/valueOf n) 5))

(defn sum-of-squares-of-primes [a b]
  (defn term [a] (* a a))
  (filtered-accumulate prime? + 0 term a inc b))

(defn product-of-positives-prime-to-n [n]
  (defn prime-to-n? [i] (= (m/gcd i n) 1))
  (filtered-accumulate prime-to-n? * 1 identity 0 inc n))


;;; Exercise 1.34

(defn f [g] (g 2))

;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
; (f f)

;; We will eventually get an error because of the following substitutions:
;; (f f)
;; (f 2)
;; (2 2) ; a number in first position causes an evaluation error.


(t/deftest tests
  (t/is (s/good-enough? 0.25 (simpson-integral s/cube 0 1 100)))
  (t/is (s/good-enough? 0.25 (simpson-integral s/cube 0 1 1000)))
  (t/is (= 4 (triangular-sum 1 2)))
  (t/is (= 56 (triangular-sum 1 6)))
  (t/is (= 1 (factorial 0)))
  (t/is (= 1 (factorial 1)))
  (t/is (= 2 (factorial 2)))
  (t/is (= 120 (factorial 5)))
  (t/is (s/good-enough? Math/PI (pi-approx 1000)))
  (t/is (= 1 (factorial-iter 0)))
  (t/is (= 1 (factorial-iter 1)))
  (t/is (= 2 (factorial-iter 2)))
  (t/is (= 120 (factorial-iter 5)))
  (t/is (s/good-enough? 0.707106781 (geometric-sum 1/2 1/2 50)))
  (t/is (s/good-enough? 1 (sine-product (/ Math/PI 2) 100)))
  (t/is (s/good-enough? 1 (sine-product (/ Math/PI 2) 1000)))
  (t/is (s/good-enough? 0.707106781 (sine-product* (/ Math/PI 4) 1000)))
  (t/is (= 87 (sum-of-squares-of-primes 1 10)))
  (t/is (= 105 (product-of-positives-prime-to-n 8)))
  (t/is (= 4 (f #(* %1 %1))))
  (t/is (= 6 (f #(* %1 (+ %1 1))))))
