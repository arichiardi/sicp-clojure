(ns sicp-clojure.2-1-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (gcd)]
            [sicp-clojure.utils :as u]))

;;; 2.1.1  Example: Arithmetic Operations for Rational

(defn make-rat [n d]
  {:pre [(not= d 0)]}
  (let [g (m/gcd n d)]
    (cons (quot n g) (cons (quot d g) []))))

(defn numer [x]
  (first x))

(defn denom [x]
  (first (rest x)))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println (numer x) "/" (denom x)))


(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(t/deftest tests
  (t/is (equal-rat? (make-rat 5 6) (add-rat one-half one-third)))
  (t/is (equal-rat? (make-rat 1 6) (mul-rat one-half one-third)))
  (t/is (equal-rat? (make-rat 2 3) (add-rat one-third one-third))))

