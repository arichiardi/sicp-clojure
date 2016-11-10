(ns sicp-clojure.1-2-exercises-1-21-1-28
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m]
            [sicp-clojure.utils :as u]
            [sicp-clojure.1-2-samples :as s]))

;;; Exercise 1.21
;; Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers:
;; 199, 1999, 19999.

; See tests at the bottom.


;;; Exercise 1.22

(defn- search-helper [start end n primes]
  (cond (= n 0) primes
        (= start end) primes
        (even? start) (search-helper (+ start 1) end n primes)
        (s/fast-prime? start 2) (search-helper (+ start 1) end (- n 1) (conj primes start))
        :else (search-helper (+ start 1) end n primes)))

(defn search-first-n-primes-in-range [start end n]
  "Searches for the first *n* primes in the range (both ends inclusive). Returns a vector."
  (search-helper start end n []))

(defn- report-prime [elapsed-time]
  (print " *** ")
  (print elapsed-time))

(defn- start-prime-test [n start-time]
  (if (s/prime? n)
      (report-prime (- (System/nanoTime) start-time))))

(defn timed-prime-test [n]
  (print "\n" n)
  (start-prime-test n (System/nanoTime)))

(defn search-for-primes [start end]
  (cond (>= start end) (println "\nDone!")
        (even? start) (search-for-primes (+ start 1) end)
        :else (do (timed-prime-test start)
                (search-for-primes (+ start 2) end))))

;;(search-first-n-primes-in-range 15 9999 3)
;;(search-first-n-primes-in-range 10000 99999 3)
;;(search-first-n-primes-in-range 100000 999999 3)
;;(search-first-n-primes-in-range 1000000 9999999 3)
;;(search-first-n-primes-in-range 10000000 99999999 3)

;;(search-for-primes 1008 1020) ; => ~ 2800
;;(search-for-primes 10006 10038) ; => ~ 8500
;;(search-for-primes 100002 100044) ; => ~ 17000
;;(search-for-primes 1000002 1000038) ; => ~ 55000

;; Even if the recursion causes stack overflow most of the times, the average timing (notice that
;; the time unit is nanoseconds) of each search kind of shows an increase of around (sqrt 10),
;; especially between > 100000 and > 10000000.
;; In any case, these kind of micro benchmarks are really difficult on the JVM in general,
;; see https://code.google.com/p/caliper/wiki/JavaMicrobenchmarks.

;;(u/microbench 100 (prime? 1009)) ; Average: 0.014561444444444441
;;(u/microbench 100 (prime? 1013)) ;
;;(u/microbench 100 (prime? 1019)) ;
;;(u/microbench 100 (prime? 10007)) ;
;;(u/microbench 100 (prime? 10009)) ;
;;(u/microbench 100 (prime? 10037)) ; Average: 0.015071555555555553
;;(u/microbench 100 (prime? 100003)) ;
;;(u/microbench 100 (prime? 100019)) ;
;;(u/microbench 100 (prime? 100043)) ; Average: 0.032170000000000004
;;(u/microbench 100 (prime? 1000003)) ;
;;(u/microbench 100 (prime? 1000033)) ; Average: 0.07697216666666665
;;(u/microbench 100 (prime? 1000037)) ;

;; Trying with another micro benchmark tool changes a little bit the result. The average time doesn't
;; really show a (sqrt 10) increase from one step to another.


;;; Exercise 1.23

(defn- next* [n]
  (if (= n 2) 3 (+ n 2)))

(defn- find-divisor* [n test-divisor]
  (cond (> (u/square test-divisor) n) n
        (s/divides? test-divisor n) test-divisor
        :else (find-divisor* n (next* test-divisor))))

(defn smallest-divisor* [n]
  (find-divisor* n 2))

(defn prime*? [n]
  (= (smallest-divisor* n) n))

(defn- start-prime-test* [n start-time]
  (if (prime*? n)
      (report-prime (- (System/nanoTime) start-time))))

(defn timed-prime-test* [n]
  (print "\n" n)
  (start-prime-test* n (System/nanoTime)))

(defn search-for-primes* [start end]
  (cond (>= start end) (println "\nDone!")
        (even? start) (search-for-primes* (+ start 1) end)
        :else (do (timed-prime-test* start)
                (search-for-primes* (+ start 2) end))))

;;(search-for-primes* 1008 1020) ; => ~ 2800
;;(search-for-primes* 10006 10038) ; => ~ 8500
;;(search-for-primes* 100002 100044) ; => ~ 17000
;;(search-for-primes* 1000002 1000038) ; => ~ 55000

;;(u/microbench 100 (prime*? 1009)) ; Average: 0.0106535
;;(u/microbench 100 (prime*? 1013)) ;
;;(u/microbench 100 (prime*? 1019)) ;
;;(u/microbench 100 (prime*? 10007)) ;
;;(u/microbench 100 (prime*? 10009)) ;
;;(u/microbench 100 (prime*? 10037)) ; Average: 0.012052333333333335
;;(u/microbench 100 (prime*? 100003)) ;
;;(u/microbench 100 (prime*? 100019)) ;
;;(u/microbench 100 (prime*? 100043)) ; Average: 0.02210261111111111
;;(u/microbench 100 (prime*? 1000003)) ;
;;(u/microbench 100 (prime*? 1000033)) ; Average: 0.05496777777777779
;;(u/microbench 100 (prime*? 1000037)) ;

;; The difference in average time between prime? and prime*? (using next*) is not exactly 2.
;; This can be explained noticing that next* introduces branching (if) that can have some
;; performance hit.


;;; Exercise 1.24

(defn- start-prime-test** [n start-time]
  (if (s/fast-prime? n 100)
      (report-prime (- (System/nanoTime) start-time))))

(defn timed-prime-test** [n]
  (print "\n" n)
  (start-prime-test** n (System/nanoTime)))

(defn search-for-primes** [start end]
  (cond (>= start end) (println "\nDone!")
        (even? start) (search-for-primes** (+ start 1) end)
        :else (do (timed-prime-test** start)
                (search-for-primes** (+ start 2) end))))

;;(search-for-primes** 1008 1020)
;;(search-for-primes** 10006 10038)
;;(search-for-primes** 100002 100044)
;;(search-for-primes** 1000002 1000038)

;; There is no noticeable difference in the benchmarks of fast-prime?, even using some more powerful
;; tools. The numbers are probably too small or some JVM hocus-pocus is acting weird behind the scene.
;; Furthermore, some Clojure's internal function produces stack overflow when using big numbers.


;;; Exercise 1.25
(defn expmod-alyssa [base exp m]
  (rem (s/fast-expt base exp) m))

;; As note 46 says, there is a huge difference in the expmod implementation of the book and the naive
;; version wrote by Alyssa. The book's implementation arrives at the result by applying mod m to
;; each partial value returned. This means that the values handled will not grow much more than m.
;; Alyssa's version will easily produce big numbers that will of course slow down the execution
;; (Clojure will convert to BigInt, which allows for arbitrary precision but is slower than Long/Integer).


;;; Exercise 1.26

;; By replacing (square ...) with (* ...), Louis has forced the applicative-order interpreter to
;; evaluate (expmod ...) twice per iteration, as soon as (remainder (* ...) ...) needs evaluation.
;; Therefore, now there are two recursive calls to expmod, which will unfold in exactly the same way,
;; duplicating espressions like in the Fibonacci tree-recursive implementation.


;;; Exercise 1.27
;; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test.

;; In order to avoid stack overflows we need to use Clojure's optimized recur construct.
(defn- f-helper [n a]
  (cond (= n a) true
        (= (s/expmod a n n) a) (recur n (inc a))
        :else false))

(defn fermat-test-check [n]
  (f-helper n 2))


;;; Exercise 1.28
;; Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use
;; this to implement the Miller-Rabin test with a procedure analogous to fermat-test.
(defn- check-and-square [a m]
  (def square-modulo (rem (u/square a) m))
  (if (or (= a 1) (= a (- m 1)) (not (= square-modulo 1)))
    square-modulo
    0))

(defn- expmod-mr [base exp m]
  (cond (= exp 0) 1
        (even? exp) (check-and-square (expmod-mr base (quot exp 2) m) m)
        :else (rem (*' base (expmod-mr base (- exp 1) m)) m)))

(defn miller-rabin-test [n]
  (defn try-it [a n]
    (= (expmod-mr a (- n 1) n) 1))
  (try-it (+ 1 (m/round (m/floor (rand (- n 1))))) n))

(defn fast-prime*? [n times]
  (cond (= times 0) true
        (miller-rabin-test n) (fast-prime*? n (- times 1))
        :else false))

;; Note that some of the tests below are probabilistic and they can produce a different
;; results (although with very low probability).
(t/deftest tests
  (t/is (= 199 (s/smallest-divisor 199)))
  (t/is (= 1999 (s/smallest-divisor 1999)))
  (t/is (= 7 (s/smallest-divisor 19999)))
  (t/is (= true (fermat-test-check 31)))
  (t/is (= false (fermat-test-check 32)))
  (t/is (= true (fermat-test-check 561))) ; Charmichael number
  (t/is (= true (fermat-test-check 1105))) ; Charmichael number
  (t/is (= true (fermat-test-check 1729))) ; Charmichael number
  (t/is (= true (fermat-test-check 2465))) ; Charmichael number
  (t/is (= true (fermat-test-check 2821))) ; Charmichael number
  (t/is (= true (fermat-test-check 6601))) ; Charmichael number
  (t/is (= true (fast-prime*? 31 50)))
  (t/is (= false (fast-prime*? 32 50)))
  (t/is (= false (fast-prime*? 561 50))) ; Charmichael number
  (t/is (= false (fast-prime*? 1105 50))) ; Charmichael number
  (t/is (= false (fast-prime*? 1729 50))) ; Charmichael number
  (t/is (= false (fast-prime*? 2465 50))) ; Charmichael number
  (t/is (= false (fast-prime*? 2821 50))) ; Charmichael number
  (t/is (= false (fast-prime*? 6601 50)))) ; Charmichael number

(t/run-tests)
