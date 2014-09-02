(ns sicp-clojure.1-3-exercises
  (:require :reload-all [clojure.test :as t]
;;             [clojure.math.numeric-tower :as m :refer (log)]
            [clojure.java.javadoc :as j]
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
  (if (= k 1)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (dec k))))))

;; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process.
;; If it generates an iterative process, write one that generates a recursive process.

;; (cont-frac (fn [i] 1.0) (fn [i] 1.0) 1000)



(t/run-tests)
(t/deftest tests
  (t/is (s/close-enough? 1.6180339887498948482 (golden-ratio)))
  (t/is (s/close-enough? 4.555532257016376 (x-to-x-eq-one-thousand)))
  (t/is (s/close-enough? 4.555532257016376 (x-to-x-eq-one-thousand*)))
  (t/is (s/close-enough? 1.6180339887498948482 (cont-frac (fn [i] 1.0) (fn [i] 1.0) 1000))))
