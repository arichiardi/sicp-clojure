(ns sicp-clojure.1-3-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (abs)]
            [clojure.tools.trace :as tr]))

;;; 1.3.1  Procedures as Arguments

(defn cube [n] (* n n n))

;; common pattern in the following procedures
(defn- sum-integers [a b]
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defn- sum-cubes [a b]
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(defn- pi-sum [a b]
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; high-order function
(defn sum [term a next-a b]
  "Sums from a through b."
  (if (> a b)
    0
    (+ (term a) (sum term (next-a a) next-a b))))

(defn sum-cubes* [a b]
  (sum cube a inc b))

(defn sum-integers* [a b]
  (sum identity a inc b))

(defn pi-sum* [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


(t/deftest tests
  (t/is (= 3025 (sum-cubes 1 10)))
  (t/is (= 55 (sum-integers 1 10)))
  (t/is (< (m/abs (- (* 8 (pi-sum 1 1000)) 3.14)) 0.001))
  (t/is (= (sum-cubes 1 10) (sum-cubes* 1 10)))
  (t/is (= (sum-integers 1 10) (sum-integers* 1 10)))
  (t/is (= (pi-sum 1 1000) (pi-sum* 1 1000)))
  (t/is (< (m/abs (- (integral cube 0 1 0.01) 0.25)) 0.001))
  (t/is (< (m/abs (- (integral cube 0 1 0.001) 0.25)) 0.001)))
