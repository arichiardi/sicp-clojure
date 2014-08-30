(ns sicp-clojure.1-1-2-samples
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (round)]
            [clojure.tools.trace :as tr]))

;;; 1.2.2 Example: Counting change

;; No changes from the original
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

;; Recursive
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; Iterative
(defn- expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b (- counter 1) (* b product))))

(defn expt* [b n]
  (expt-iter b n 1))

(defn square [x]
  (*' x x))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (quot n 2)))
        :else (*' b (fast-expt b (- n 1)))))


;;; 1.2.6  Example: Testing for Primality

;; If the smallest divisor is n itself, the number is prime.
;; Order of growth is Theta(n) = O(sqrt 5).
(defn divides? [a b]
  (= (rem b a) 0))

(defn- find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= (smallest-divisor n) n))

;; Alternative implementation with Clojure's recur. It is necessary for the exercises,
;; as the other find-divisor easily causes stack overflows for big numbers.
(defn- find-divisor-recur [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (+ test-divisor 1))))

(defn smallest-divisor-recur [n]
  (find-divisor-recur n 2))

(defn prime-recur? [n]
  (= (smallest-divisor-recur n) n))

;; Fermat's test
(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (quot exp 2) m)) m)
        :else (rem (*' base (expmod base (- exp 1) m)) m)))

(defn- fermat-test [n]
  (defn try-it [a n]
    (= (expmod a n n) a))
  (try-it (+ 1 (m/round (m/floor (rand (- n 1)))) n)))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))


(t/deftest tests
  (t/is (= 4 (count-change 11)))
  (t/is (= 292 (count-change 100)))
  (t/is (= 4 (count-change* 11)))
  (t/is (= 292 (count-change* 100)))
  (t/is (= 1 (expt 2 0)))
  (t/is (= 64 (expt 2 6)))
  (t/is (= 512 (expt 2 9)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (expt 2 62)))
  (t/is (= 81 (expt 3 4)))
  (t/is (= 243 (expt 3 5)))
  (t/is (= 1 (expt* 2 0)))
  (t/is (= 64 (expt* 2 6)))
  (t/is (= 512 (expt* 2 9)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (expt* 2 62)))
  (t/is (= 81 (expt* 3 4)))
  (t/is (= 243 (expt* 3 5)))
  (t/is (= 1 (fast-expt 2 0)))
  (t/is (= 64 (fast-expt 2 6)))
  (t/is (= 512 (fast-expt 2 9)))
  (t/is (= (+ (bit-shift-right Long/MAX_VALUE 1) 1) (fast-expt 2 62)))
  (t/is (= 81 (fast-expt 3 4)))
  (t/is (= 243 (fast-expt 3 5)))
  (t/is (= true (prime? 29)))
  (t/is (= false (prime? 27)))
  (t/is (= true (fast-prime? 29 5)))
  (t/is (= false (fast-prime? 27 5))))

