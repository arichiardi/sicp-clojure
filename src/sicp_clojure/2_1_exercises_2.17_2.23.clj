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
  (cond (u/equal-to? amount 0) 1 ; we need equal-to? for floating point comparisons.
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values))))

(defn count-change
  "Calculates the number of times you can give change with the given coin values."
  [amount coin-values]
  (cc amount coin-values))

;; As shown below, the order doesn't matter when counting change. The algorithm checks the coins against
;; the amount independently from the order, taking cdr and car in the branches of the tree recursion.

;; (def us-coins-shuffled (list 1 5 25 10 50))
;; (cc 100 us-coins-shuffled)


;;; Exercise 2.20
;; The procedures +, *, and list take arbitrary numbers of arguments. [...]
;; Use this notation to write a procedure same-parity that takes one or more integers and returns a list
;; of all the arguments that have the same even-odd parity as the first argument.

;; The notation for variable arguments is a little bit different in Clojure (& instead of .) but the
;; semantic is the same.

(defn same-parity [frst & args]
  (defn even-odd? [arg]
    (or (and (even? frst) (even? arg))
        (and (odd? frst) (odd? arg))))

  (defn helper [args result]
    (cond  (empty? args) result
           (even-odd? (u/car args)) (helper (u/cdr args) (cons (u/car args) result))
           :else (helper (u/cdr args) result)))

  (cons frst (reverse* (helper args []))))


(t/run-tests)
(t/deftest tests
  (t/is (= 34 (last-pair (list 23 72 149 34))))
  (t/is (= (list 25 16 9 4 1) (reverse* (list 1 4 9 16 25))))
  (t/is (= 292 (count-change 100 us-coins)))
  (t/is (= 104561 (count-change 100 uk-coins)))
  (t/is (= (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7)))
  (t/is (= (list 2 4 6) (same-parity 2 3 4 5 6 7)))
  (t/is (= (list 1 3 9 11 15) (same-parity 1 3 8 9 11 12 15))))
