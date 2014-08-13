(ns sicp-clojure.1-1-2-exercises
  (:require :reload-all [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
            [sicp-clojure.1-1-2-samples :as samples]))

;;; Exercise 1.9
;; Using the substitution model, illustrate the process generated by each procedure
;; in evaluating (+ 4 5). Are these processes iterative or recursive?
(defn custom-plus [a b]
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

;; both in applicative-order:
;; (custom-plus 4 5)
;; (if (= 4 0) b (inc (+ (dec 4) 5)))
;; (if (= 4 0) b (inc (+ 3 5)))
;; (if (= 4 0) b (inc 8))
;; (if (= 4 0) b 9)
;; (9)
;; It looks like an iterative process because primitive expressions are just chained one
;; after another.

(defn custom-plus* [a b]
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

;; (custom-plus* 4 5)
;; (if (= 4 0) b (+ (dec 4) (inc 5)))
;; (if (= 4 0) b (+ (dec 4) 6)
;; (if (= 4 0) b (+ 3 6)
;; (if (= 4 0) b 9)
;; (9)
;; It looks like an iterative process because the interpreter needs to wait for the evaluation of
;; both inc and dec before proceeding with +, therefore keeping track and deferring them
;; once inside +.


;;; Exercise 1.10 - Ackermann's function
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(defn f [n] (A 0 n))
(defn g [n] (A 1 n))
(defn h [n] (A 2 n))
(defn k [n] (* 5 n n))

(t/deftest tests
  ;; (A 1 10) - substitution for relevant branches only
  ;; (: else (A (- 1 1) (A 1 (- 10 1)))))
  ;; (: else (A 0 (A 1 9))))
  ;;          |    |
  ;;          |   (:else (A 0 (A 1 8)))
  ;;          |           |    |
  ;;          |           |    |
  ;;          |           |    (:else (A 0 (A 1 1)))
  ;;          |           |            |    |
  ;;          |           |            |   ((= 1 1) 2)
  ;;          |           |           ((= 0 0) (* 2 2))
  ;;          |           ((= 0 0) (* 2 256))
  ;;          ((= 0 0) (* 2 512))
  ;;
  ;; If x is not zero, the first call goes to the :else branch, which recursively calls A with x-1
  ;; as first argument and another recursive call to A for calculating y as second argument.
  ;;
  ;; The calls with x-1 will all fall into (= x 0) (* 2 y) therefore needing the value of y and,
  ;; when obtained, returning it doubled. The stopping condition of the second-level
  ;; recursion is when (= y 1) is matched, which will return 2. Combining recursive doubling
  ;; with 2 as starting point leads to (A 1 y) = 2^y.
  (t/is (= (m/expt 2 10) (A 1 10)))

  ;; (A 2 4) - substitution for relevant branches only
  ;; (: else (A 1 (A 2 3)))
  ;;          |    |
  ;;          |   (:else (A 1 (A 2 2)))
  ;;          |           |    |
  ;;          |           |    (: else (A 1 (A 2 1)))
  ;;          |           |             |    |
  ;;          |           |             |    ((= 1 1) 2)
  ;;          |           |            see before 2^2
  ;;          |           see before 2^4
  ;;          see before 2^16
  ;;
  ;; The double recursion in this case results in the convolution of the exponential functions:
  ;; (A 2 y) = (A 1 (A 2 y-1)) = 2^(2^2^...) for y-1 times (see below)
  (t/is (= (m/expt 2 (m/expt 2 (m/expt 2 2))) (A 2 4)))
  (t/is (= (m/expt 2 (m/expt 2 2)) (A 2 3)))

  ;; (A 3 3)
  ;; One additional level of convolution is applied every time x is incremented:
  ;; (A 3 3) = (A 2 4) = 2^(2^2^...) -> for y times
  (t/is (= (m/expt 2 (m/expt 2 (m/expt 2 2))) (A 3 3)))

  ;; As per Ackermann's definition, f computes 2*n
  (t/is (= (* 2 15) (f 15)))

  ;; As seen above, g computes 2^n (Knuth single up-arrow)
  (t/is (= (m/expt 2 15) (g 15)))

  ;; As seen above, h computes 2^2^2^... n exponentations (Knuth double up-arrow)
  (t/is (= (m/expt 2 (m/expt 2 (m/expt 2 2))) (h 4))))