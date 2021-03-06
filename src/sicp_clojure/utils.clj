(ns sicp-clojure.utils
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as m]))


(defn make-file-from-resource [name] (io/file (io/resource name)))


(defn to-bit-representation [^java.lang.Double d]
  (-> (Double/doubleToRawLongBits d) (java.lang.Long/toBinaryString)))


(defn methods-of
  "Returns the \"[...] public member methods of the class or interface represented by this Class
  object, including those declared by the class or interface and those inherited from superclasses
  and superinterfaces.\". See java.lang.Class/getMethods for details.
  If a second argument is provided as valid regex o string, the list is filtered according to it."
  [instance & [string]]
  {:pre [(not (nil? instance))]}
  (filter #(re-find (re-pattern (or string #".*")) %) (map #(.getName %) (-> instance class .getMethods))))


;; from www.bestinclass.dk/index.clj/2010/02/benchmarking-jvm-languages.html
(defmacro microbench
  "Evaluates the expression n number of times, returning the average
   time spent in computation, removing highest and lowest values.
   If the body of expr returns nil, only the timing is returned otherwise
   the result is printed - does not affect timing.
   Before timings begin, a warmup is performed lasting either 1 minute or
   1 full computational cycle, depending on which comes first."
  [n expr]
  {:pre [(> n 2)]}
  `(let [warm-up#  (let [start# (System/currentTimeMillis)]
                     (println "Warming up!")
                     (while (< (System/currentTimeMillis) (+ start# (* 60 1000)))
                       (with-out-str ~expr)
                       (System/gc))
                     (println "Benchmarking..."))
         timings#  (doall
                    (for [pass# (range ~n)]
                      (let [start#    (System/nanoTime)
                            retr#     ~expr
                            timing#   (/ (double (- (System/nanoTime) start#))
                                         1000000.0)]
                        (when retr# (println retr#))
                        (System/gc)
                        timing#)))
         runtime#  (reduce + timings#)
         highest#  (apply max timings#)
         lowest#   (apply min timings#)]
     (println "Total runtime: " runtime#)
     (println "Highest time : " highest#)
     (println "Lowest time  : " lowest#)
     (println "Average      : " (/ (- runtime# (+ highest# lowest#))
                                   (- (count timings#) 2)))
     timings#))


(defn round-to-p-decimals
  "Rounds a number x to a precision of p significant digits."
  [x p] (/ (m/round (* (double x) (m/expt 10 p))) (double (m/expt 10 p))))


(defn- equality-comparison-fn
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the input epsilon.
  With this implementation, the comparison fn doesn't work with big numbers."
  [epsilon]
  (fn [x y]
    (< (m/abs (- x y)) epsilon)))


(defn- equality-comparison-with-scale-fn
  "Returns a function for evaluating the equality of its two arguments.
  The equality check is a simple comparison between their difference and the epsilon
  is used for comparing them."
  [epsilon]
  (fn [x y]
    (let [scale (if (and (not (zero? x)) (not (zero? y))) (m/abs x) 1)]
      (< (m/abs (- x y)) (* scale epsilon)))))


(def equal-to?
  "Comparing two numbers using equality-with-difference-fn with epsilon = 0.0001."
  (equality-comparison-with-scale-fn 0.001))


;; Math
(defn square
  "Calculates the square of x."
  [x] (*' x x))

(defn cube
  "Calculates the cube of x."
  [x] (*' x x x))

(defn sin
  "Calculates the sine of x."
  [x] (Math/sin x))

(defn cos
  "Calculates the cosine of x."
  [x] (Math/cos x))

(defn log
  "Calculates the log of x to the base n."
  [x n] (/ (Math/log10 x) (Math/log10 n)))

(defn average
  "Calculates the average of two or more numbers."
  ([] 0)
  ([& xs] (/ (reduce + xs) (count xs))))

(defn average-damp
  "Returns a function that uses average damping to make the approximations (of x) converge.
  Namely, given a function f, we consider the function whose value at x is equal to
  the average of x and f(x). [sicp, 1.3.4 Procedures as Returned Values]"
  [f]
  (fn [x] (average x (f x))))


;; Lists

(def car
  "We can think of car as selecting the first item in the list [sicp, 2.2.1]. The names car
  and cdr derive from the original implementation of Lisp on the IBM 704. [...] Car stands for
  Contents of Address part of Register [sicp, note 2 to 2.1.1]"
  first)

(def cdr
  "We can think of cdr as selecting the sublist consisting of all but the first item. [sicp, 2.2.1]
  The names car and cdr derive from the original implementation of Lisp on the IBM 704. [...]
  Cdr (pronounced ``could-er'') stands for ``Contents of Decrement part of Register. [sicp, note 2 to 2.1.1]"
  rest)

;; The following works but it can be improved.
;; (defn append
;;   "Appends list2 at the end of list1"
;;   [list1 list2]
;;   (cond (empty? list1) list2
;;         (empty? list2) list1
;;         :else (append (reverse (cons (car list2) (reverse list1))) (cdr list2))))

(defn append
  "Appends list2 at the end of list1"
  [list1 list2]
  (if (empty? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(defn map*
  "Map takes as arguments a procedure of one argument and a list, and returns a list of the results produced
  by applying the procedure to each element in the list. Returns nil (seq-like) if items is empty."
  [proc items]
  (if (empty? items)
    nil
    (cons (proc (car items)) (map* proc (cdr items)))))

(def pair?
  "Tests whether its argument is a pair."
  list?)


(t/deftest tests
  (t/is (empty? (filter #((not (.contains "Value")) %1) (methods-of java.lang.Double "Value"))))
  (t/is (= 1.342 (round-to-p-decimals 1.3415 3)))
  (t/is (= 1.341 (round-to-p-decimals 1.3412 3)))
  (t/is (= 0.0 (round-to-p-decimals 0 1)))
  (t/is (equal-to? 2.5 (average 3 2)) "Average of [3 2]")
  (t/is (equal-to? 2 (average 3 2 1)) "Average of [3 2 1]")
  (t/is (equal-to? 6.1 (average 3.7 4.1 9.3 12.4 1)) "Average of [3.7 4.1 9.3 12.4 1]")
  (t/is (== 55.0 ((average-damp (fn [x] (* x x))) 10)))
  (t/is (= 9 (square 3)))
  (t/is (= 27 (cube 3)))
  (t/is (equal-to? 2 (log 4 2)))
  (t/is (= 1 (car (list 1 2 3 4))))
  (t/is (= (list 2 3 4) (cdr (list 1 2 3 4))))
  (t/is (= (list 10 2.5 11.6 17) (map* m/abs (list -10 2.5 -11.6 17))))
  (t/is (= (list 1 4 9 16) (map* square (list 1 2 3 4))))
  (t/is (= (list 1 2 3 4 5 6) (append '(1 2 3) '(4 5 6))))
  (t/is (= (list 4 5 6) (append (list) '(4 5 6))))
  (t/is (= (list 1 2 3) (append (list 1 2 3) ()))))
