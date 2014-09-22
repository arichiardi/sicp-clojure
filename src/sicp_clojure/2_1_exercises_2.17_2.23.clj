(ns sicp-clojure.2-1-exercises
  (:require :reload-all [clojure.test :as t]
            [sicp-clojure.utils :as u]))


;;; Exercise 2.17
;; Define a procedure last-pair that returns the list that contains only the last element of
;; a given (nonempty) list.

(defn last-pair [a]
  (let [cdr-a (u/cdr a)]
    (if (empty? cdr-a)
      (u/car a)
      (last-pair cdr-a))))


;;; Exercise 2.18
;; Define a procedure reverse that takes a list as argument and returns a list of the same elements
;; in reverse order.

(defn reverse* [a]
  (defn reverse-helper [a new-a]
    (if (empty? a)
      new-a
      (reverse-helper (u/cdr a) (cons (u/car a) new-a))))
  (reverse-helper a []))


;;; Exercise 2.19
;; Consider the change-counting program of section 1.2.2. It would be nice to be able to easily change
;; the currency used by the program [...]. We want to rewrite the procedure cc so that its second argument
;; is a list of the values of the coins to use rather than an integer specifying which coins to use.
;; We could then have lists that defined each kind of currency:

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

;; Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive
;; operations on list structures. Does the order of the list coin-values affect the answer produced by cc?
;; Why or why not?

(defn- first-denomination [cv] (u/car cv))
(defn- except-first-denomination [cv] (u/cdr cv))
(defn- no-more? [cv] (empty? cv))

(defn- cc
  "Recursive helper function to count the change, with added input denominations."
  [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values))))

(defn count-change
  "Calculates the number of times you can give change with the given coin values."
  [amount coin-values]
  (cc amount coin-values))


(t/run-tests)
(t/deftest tests
  (t/is (= 34 (last-pair (list 23 72 149 34))))
  (t/is (= (list 25 16 9 4 1) (reverse* (list 1 4 9 16 25))))
  (t/is (= 292 (cc 100 us-coins))))
