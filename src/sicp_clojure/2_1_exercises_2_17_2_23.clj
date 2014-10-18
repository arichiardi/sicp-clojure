(ns sicp-clojure.2-1-exercises-2-17-2-23
  (:require [clojure.test :as t]
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
  (defn helper [a new-a]
    (if (empty? a)
      new-a
      (helper (u/cdr a) (cons (u/car a) new-a))))
  (helper a []))


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
    (cond (empty? args) result
          (even-odd? (u/car args)) (helper (u/cdr args) (cons (u/car args) result))
          :else (helper (u/cdr args) result)))

  (cons frst (reverse* (helper args []))))

;; The Clojure's way follows, using a Vector (and conj) to append at the right end and avoid the final reverse.

(defn same-parity* [frst & args]
  (defn even-odd? [arg]
    (or (and (even? frst) (even? arg))
        (and (odd? frst) (odd? arg))))

  (defn helper [args result]
    (cond (nil? args) result
          (even-odd? (first args)) (helper (next args) (conj result (first args)))
          :else (helper (next args) result)))

  (helper args [frst]))


;;; Exercise 2.21
;; Here are two different definitions of square-list. Complete both of them by filling in the missing
;; expressions:

(defn square-list [items]
  (if (empty? items)
    nil
    (let [head (u/car items)]
      (cons (* head head) (square-list (u/cdr items))))))

(defn square-list* [items]
  (u/map* u/square items))


;;; Exercise 2.22
;; Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves
;; an iterative process:

(defn square-list** [items]
  (defn iter [things answer]
    (if (empty? things)
        answer
        (iter (u/cdr things)
              (cons (u/square (u/car things)) answer))))
  (iter items nil))

;; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one
;; desired. Why?

;; The answer is reversed because cons always appends at the beginning of answer the car of things.
;; (square-list** (list 1 2 3 4))

;; Louis then tries to fix his bug by interchanging the arguments to cons:

(defn square-list*** [items]
  (defn iter [things answer]
    (if (empty? things)
        answer
        (iter (u/cdr things)
              (cons answer
                    (u/square (u/car things))))))
  (iter items nil))

;; This doesn't work either. Explain.

;; It won't work because cons expects an item as first parameter and a list as second. Louis' correction
;; will throw an error at runtime.
;; (square-list*** (list 1 2 3 4))


;;; Exercise 2.23
;; The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements.
;; However, rather than forming a list of the results, for-each just applies the procedure to each of the
;; elements in turn, from left to right. [...] Give an implementation of for-each.

(defn for-each [proc items]
  (if (empty? items)
    true
    (do (proc (u/car items)) (for-each proc (u/cdr items)))))

;; (for-each (fn [x] (newline) (print x)) (list 57 321 88))


(t/deftest tests
  (t/is (= 34 (last-pair (list 23 72 149 34))))
  (t/is (= (list 25 16 9 4 1) (reverse* (list 1 4 9 16 25))))
  (t/is (= (list (list 3 4) (list 1 2)) (reverse* (list (list 1 2) (list 3 4)))))
  (t/is (= 292 (count-change 100 us-coins)))
  (t/is (= 104561 (count-change 100 uk-coins)))
  (t/is (= (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7)))
  (t/is (= (list 2 4 6) (same-parity 2 3 4 5 6 7)))
  (t/is (= (list 1 3 9 11 15) (same-parity 1 3 8 9 11 12 15)))
  (t/is (= (list 1 3 5 7) (same-parity* 1 2 3 4 5 6 7)))
  (t/is (= (list 2 4 6) (same-parity* 2 3 4 5 6 7)))
  (t/is (= (list 1 3 9 11 15) (same-parity* 1 3 8 9 11 12 15)))
  (t/is (= (list 1 4 9 16) (square-list (list 1 2 3 4))))
  (t/is (= (list 1 4 9 16) (square-list* (list 1 2 3 4))))
  (t/is (= 7 (u/car (u/cdr (u/car (u/cdr (u/cdr (list 1 3 (list 5 7) 9))))))))
  (t/is (= 7 (u/car (u/car (list (list 7))))))
  (t/is (= 7 (u/car (u/cdr (u/car (u/cdr (u/car (u/cdr (u/car (u/cdr (u/car (u/cdr (u/car (u/cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))))
