(ns sicp-clojure.1-3-exercises
  (:require [clojure.test :as t]
            [clojure.java.javadoc :as j]
            [clojure.tools.trace :as tr]
            [sicp-clojure.utils :as u]
            [sicp-clojure.1-3-samples :as s]))

;;; Exercise 1.35
;; Show that the golden ratio  (section 1.2.2) is a fixed point of the transformation x = 1 + 1/x,
;; and use this fact to compute by means of the fixed-point procedure.

(defn golden-ratio []
  (s/fixed-point (fn [x] (+ 1 (/ 1.0 x))) 1.0))


;;; Exercise 1.36
;; [...] find a solution to x^x = 1000 by finding a fixed point of x  log(1000)/log(x).
;; Compare the number of steps this takes with and without average damping.

;; The following version unfolds 32 recursive calls.
(defn x-to-x-eq-one-thousand []
  (s/fixed-point (fn [x] (/ (Math/log10 1000) (Math/log10 x))) 10.0))

;; The version with average dumping takes just 9 steps.
(defn x-to-x-eq-one-thousand* []
  (s/fixed-point (fn [x] (/ (+ x (/ (Math/log10 1000) (Math/log10 x))) 2)) 10.0))


;;; Exercise 1.37
;; a. Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di
;; of the terms of the continued fraction. Define a procedure cont-frac such that evaluating
;; (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure
;; by approximating 1/golden ratio [...] for successive values of k. How large must you make k in order
;; to get an approximation that is accurate to 4 decimal places?

(defn cont-frac [n d k]
  (defn recurse [i]
    (if (> i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (recurse (inc i))))))
  (recurse 1))

;; With k >= 12 cont-frac is accurate to 4 decimal places (0.6180).
;; (cont-frac (fn [i] 1.0) (fn [i] 1.0) 12)

;; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process.
;; If it generates an iterative process, write one that generates a recursive process.

(defn cont-frac* [n d k]
  (defn iter [k result]
    (if (= k 0)
      result
      (iter (dec k) (/ (n k) (+ (d k) result)))))
  (iter (dec k) (/ (n k) (d k))))


;;; Exercise 1.38
;; Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e,
;; based on Euler's expansion.

(defn e-approx [k]
  (defn n [i] 1.0)
  (defn d [i] (if (= (rem (- i 2) 3) 0)
                (+ 2 (* 2 (quot i 3.0)))
                1.0))
  (+ 2 (cont-frac n d k)))

(defn e-approx* [k]
  (defn n [i] 1.0)
  (defn d [i] (if (= (rem (- i 2) 3) 0)
                (+ 2 (* 2 (quot i 3.0)))
                1.0))
  (+ 2 (cont-frac* n d k)))

;;; Exercise 1.39
;; Define a procedure (tan-cf x k) that computes an approximation to the tangent function based
;; on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.

(defn tan-cf [x k]
  (defn n [i] (if (= i 1) x
                       (- (*' x x))))
  (defn d [i] (- (* 2 i) 1))
  (cont-frac n d k))

(defn tan-cf* [x k]
  (defn n [i] (if (= i 1) x
                       (- (*' x x))))
  (defn d [i] (- (* 2 i) 1))
  (cont-frac* n d k))

(t/run-tests)
(t/deftest tests
  (t/is (u/equal-to? 1.6180339887498948482 (golden-ratio)))
  (t/is (u/equal-to? 4.555532257016376 (x-to-x-eq-one-thousand)))
  (t/is (u/equal-to? 4.555532257016376 (x-to-x-eq-one-thousand*)))
  (t/is (= (u/round-to-p-decimals (/ 1 1.6180339887498948482) 4)
           (u/round-to-p-decimals (cont-frac (fn [i] 1.0) (fn [i] 1.0) 12) 4)))
  (t/is (= (u/round-to-p-decimals (/ 1 1.6180339887498948482) 4)
           (u/round-to-p-decimals (cont-frac* (fn [i] 1.0) (fn [i] 1.0) 12) 4)))
  (t/is (u/equal-to? Math/E (e-approx 10)))
  (t/is (u/equal-to? Math/E (e-approx* 10)))
  (t/is (u/equal-to? 1.0 (tan-cf (/ Math/PI 4) 10)))
  (t/is (u/equal-to? 1.0 (tan-cf* (/ Math/PI 4) 10))))
