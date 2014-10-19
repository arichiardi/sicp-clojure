(ns sicp-clojure.2-1-exercises-2-24-2-32
  (:require [clojure.test :as t]
            [sicp-clojure.utils :as u]))

;;; Exercise 2.24
;; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter,
;; the corresponding box-and-pointer structure, and the interpretation of this as a tree

;; Interpreter:
;;
;; (1 (2 (3 4)))

;; Box-and-pointer structure (note that the cdr always returns a pair wrapping the "rest" of the list,
;; try it in the REPL):
;;
;; (1(2(3 4)))  ((2(3 4)))
;;   |⚫|⚫|  →  |⚫|/|
;;    ↓           ↓
;;    1         (2(3 4))    ((3 4))
;;               |⚫|⚫|  →  |⚫|/|
;;                ↓           ↓
;;                2          (3 4)       (4)
;;                           |⚫|⚫|  →  |⚫|/|
;;                            ↓           ↓
;;                            3           4

;; Tree:
;;
;;   (1(2(3 4)))
;;    /       \
;;   1      (2(3 4))
;;            / \
;;           2  (3 4)
;;               /\
;;              3  4


;; Exercise 2.25
;; Give combinations of cars and cdrs that will pick 7 from each of the following lists:
;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

;; See tests.


;; Exercise 2.26
;; Suppose we define x and y to be two lists:

(def x (list 1 2 3))
(def y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of the following
;; expressions:

;; (u/append x y)
;; ; => (1 2 3 4 5 6)

;; (cons x y)
;; ; => ((1 2 3) 4 5 6)

;; (list x y)
;; ; => ((1 2 3) (4 5 6))


;; Exercise 2.27
;; Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes
;; a list as argument and returns as its value the list with its elements reversed and with all
;; sublists deep-reversed as well.

(defn deep-reverse [a]
  (defn helper [a new-a]
    (let [car (u/car a)
          cdr (u/cdr a)]
      (cond (empty? a) new-a
            (u/pair? car) (helper cdr (cons (helper car []) new-a))
            :else (helper cdr (cons car new-a)))))
  (helper a []))


;; Exercise 2.28
;; Write a procedure fringe that takes as argument a tree (represented as a list) and returns a
;; list whose elements are all the leaves of the tree arranged in left-to-right order.

(defn fringe [a]
  (defn helper [a res]
    (let [car (u/car a)
          cdr (u/cdr a)]
      (cond (empty? a) res
            (u/pair? car) (helper cdr (helper car res))
            :else (helper cdr (cons car res)))))
  (reverse(helper a [])))

;; For testing:

(def z (list (list 1 2) (list 3 4)))

;;; Exercise 2.29
;; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod
;; of a certain length, from which hangs either a weight or another binary mobile. We can represent
;; a binary mobile using compound data by constructing it from two branches (for example, using list):

(defn make-mobile [left right]
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure, which may
;; be either a number (representing a simple weight) or another mobile:

(defn make-branch [length structure]
  (list length structure))

;; a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile,
;; and branch-length and branch-structure, which return the components of a branch.

(defn left-branch [mobile]
  (u/car mobile))

(defn right-branch [mobile]
  (u/car (u/cdr mobile)))

(defn branch-length [branch]
  (u/car branch))

(defn branch-structure [branch]
  (u/car (u/cdr branch)))

;; b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

;; Recursive solution

(def mobile? u/pair?)

(declare total-weight)

(defn branch-weight [branch]
  (let [structure (branch-structure branch)]
    (if (mobile? structure)
      (total-weight structure)
      structure)))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


;; (defn total-weight* [mobile]

;; Continuation passing style solution, see the following thread for a mind-blowing implementation of
;; tree traversal using cps and trampoline:
;; http://stackoverflow.com/questions/19424327/printing-a-tree-lazily-in-newick-format

(defn total-weight-cps [mobile]
  (letfn
    [(branch-weight-cps
      [branch kont]
      (let [structure (branch-structure branch)]
        (if (mobile? (branch-structure branch))
          (kont (traverse-mobile-cps structure identity))
          (kont structure))))

     (traverse-mobile-cps
      [mobile kont]
      (branch-weight-cps (left-branch mobile)
                         (fn [left-weight]
                           (branch-weight-cps (right-branch mobile)
                                              (fn [right-weight] (kont (+ left-weight right-weight)))))))]
    (traverse-mobile-cps mobile identity)))

;; With clojure's constructs (and the non stack-consuming trampoline):
;; http://stackoverflow.com/questions/26450982/sicp-continuation-passing-style-and-clojures-trampoline

(defn total-weight* [mobile]
  (letfn
    [(branch-weight-cps
      [branch kont]
      (let [structure (branch-structure branch)]
        (if (mobile? (branch-structure branch))
          (do (println "then " structure) (fn [] (kont (trampoline traverse-mobile-cps structure identity))))
          (do (println "else " structure) (fn [] (kont structure))))))

     (traverse-mobile-cps
      [mobile kont]
      (branch-weight-cps (left-branch mobile)
                         (fn [left-weight]
                           (branch-weight-cps (right-branch mobile)
                                              (fn [right-weight] #(kont (+ left-weight right-weight)))))))]
    (trampoline traverse-mobile-cps mobile identity)))

;; Some def for testing:

;; Simple
(def branch11 (make-branch 1 1))
(def branch22 (make-branch 2 2))
(def branch36 (make-branch 3 6))
(def branch43 (make-branch 4 3))

(def mobile11-43 (make-mobile branch11 branch43))
(def mobile36-22 (make-mobile branch36 branch22))

;; Composite
(def branch5m1143 (make-branch 5 mobile11-43))
(def branch7m3622 (make-branch 7 mobile36-22))

(def mobile11-5m1143 (make-mobile branch11 branch5m1143))
(def mobile5m1143-7m3622 (make-mobile branch5m1143 branch7m3622))


(t/deftest tests
  (t/is (= (list 1 2 3 4 5 6) (u/append x y)))
  (t/is (= (list (list 1 2 3) 4 5 6) (cons x y)))
  (t/is (= (list (list 1 2 3) (list 4 5 6)) (list x y)))
  (t/is (= (list (list 4 3) (list 2 1)) (deep-reverse (list (list 1 2) (list 3 4)))))
  (t/is (= (list 7 (list (list 6 5) 4 3 2) 1) (deep-reverse (list 1 (list 2 3 4 (list 5 6)) 7))))
  (t/is (= (list 1 2 3 4) (fringe z)))
  (t/is (= (list 1 2 3 4 1 2 3 4) (fringe (list z z))))
  (t/is (= 4 (branch-length branch43)))
  (t/is (= 3 (branch-structure branch43)))
  (t/is (= mobile11-43 (branch-structure branch5m1143)))
  (t/is (= 1 (branch-length (left-branch mobile11-43))))
  (t/is (= mobile11-43 (branch-structure (right-branch mobile11-5m1143))))
  (t/is (= 6 (branch-weight branch36)))
  (t/is (= 8 (branch-weight branch7m3622)))
  (t/is (= 4 (total-weight mobile11-43)))
  (t/is (= 12 (total-weight mobile5m1143-7m3622)))
  (t/is (= 12 (total-weight-cps mobile5m1143-7m3622)))
  (t/is (= 12 (total-weight* mobile5m1143-7m3622))))
