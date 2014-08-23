(ns sicp-clojure.1-1-2-samples
  (:require :reload-all [clojure.test :as t]
            [clojure.repl :as r]
            [clojure.java.javadoc :as j]))

;;; 1.2.2 Example: Counting change
;; No changes from the original.
(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn- cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5))

; (count-change 11) ; uncomment to evaluate

;; Implementation using a list for the denominations.
(def denomination-kind [1 5 10 25 50])

(defn- cc* [amount denominations]
  "Recursive helper function to count the change."
  (cond (= amount 0) 1
        (or (< amount 0) (empty? denominations)) 0
        :else (+ (cc* amount (rest denominations))
                 (cc* (- amount (first denominations)) denominations))))

(defn count-change* [amount]
  "Calculates the number of times you can give change with the give denominations."
  (cc* amount denomination-kind))

;;; 1.2.4  Exponentiation
;; recursive
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; iterative
(defn- expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b (- counter 1) (* b product))))

(defn expt* [b n]
  (expt-iter b n 1))

(defn- square [x]
  (* x x))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(t/deftest tests
  (t/is (= 292 (count-change 100)))
  (t/is (= 292 (count-change* 100)))
  (t/is (= 1 (expt 2 0)))
  (t/is (= 64 (expt 2 6)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (expt 2 62)))
  (t/is (= 1 (expt* 2 0)))
  (t/is (= 64 (expt* 2 6)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (expt* 2 62)))
  (t/is (= 1 (fast-expt 2 0)))
  (t/is (= 64 (fast-expt 2 6)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (fast-expt 2 62))))

(t/run-tests)
