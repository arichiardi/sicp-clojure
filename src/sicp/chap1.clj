(ns sicp.chap1
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as p]))

; Exercise 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))

(def a 3)
(def b (+ a 1))

(+ a b (* a b))
(= a b)

(if (and (> b a) (< b (* a b)))
  b
  a)

(cond
 (= a 4) 6
 (= b 4) (+ 6 7 a)
 true 25)

(+ 2 (if (> b a) b a))

(* (cond (> a b) a
         (< a b) b
         true -1)
   (+ a 1))


; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(defn square-sum-largest-pair [a b c]
  "Sums the square of the largest two number of the three in input.
  Maybe a little overkill to use a list here, but it's just for fun."
    (reduce + (map (fn [x] (m/expt x 2)) (take 2 (reverse (sort [a b c]))))))


; Exercise 1.4
;; (def (a-plus-abs-b a b)
;;   (if (< b 0) (+ )(- ))

; Exercise 1.5

(t/deftest chap1
  (t/is (= (square-sum-largest-pair 1 4 5) 41) "Should sum 4^2 + 5^2 = 41"))
