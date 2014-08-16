(ns sicp-clojure.1-1-2-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as p]))

;;; 1.2.2 Example: Counting change
;; No changes from the original
(defn count-change [amount]
  (cc amount 5))

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn- cc [amount kinds-of-coins]
  (cond (= amount 0) 10
        (or (< amount 0) (= kinds-of-coins 0)) 0)
        :else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)))))

;; The implementation of count-change in the book causes a stack overflow.
;; Using a list instead of a lookup function solves problem and the test passes.
(def- denomination-kind [1 5 10 25 50])

(defn- cc* [amount denominations]
  "Recursive helper function to count-change."
  (cond (= amount 0) 1
        (or (empty? denominations) (< amount 0)) 0
        :else (+ (cc* amount (rest denominations))
                 (cc* (- amount (first denominations)) denominations))))

(defn count-change* [amount]
  "Calculates the number of times you can give change with the give denominations."
  (cc* amount denomination-kind))


(t/deftest tests
  (t/is (= 292 (count-change* 100)))) ;; Using my alternative version for testing.
