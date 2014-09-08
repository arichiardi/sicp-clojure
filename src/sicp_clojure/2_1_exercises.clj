(ns sicp-clojure.2-1-exercises
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (gcd)]
            [sicp-clojure.2-1-samples :as s]))

;;; Exercise 2.1
;; Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(defn make-rat* [n d]
  (let [g (m/gcd n d)])
  (if (neg? d)
    (cons (- n) (cons (- d) []))
    (cons n (cons d []))))


(t/deftest tests
  (t/is (s/equal-rat? (make-rat* 1 2) (make-rat* -1 -2)))
  (t/is (s/equal-rat? (make-rat* 1 2) (make-rat* 1 2)))
  (t/is (s/equal-rat? (make-rat* -1 2) (make-rat* -1 2)))
  (t/is (s/equal-rat? (make-rat* -1 2) (make-rat* 1 -2))))

