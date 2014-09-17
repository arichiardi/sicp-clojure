(ns sicp-clojure.2-1-exercises
  (:require [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (abs gcd expt)]
            [sicp-clojure.utils :as u]
            [sicp-clojure.2-1-samples :as s]))

;;; Exercise 2.1
;; Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(defn make-rat*
  [n d]
  {:pre [(not= d 0)]}
  (let [g (m/gcd n d)])
  (if (neg? d)
    (cons (- n) (cons (- d) []))
    (cons n (cons d []))))


;;;  Exercise 2.2
;; Consider the problem of representing line segments in a plane [...].

;; See the docstrings for the description of the functions (taken from the book).

(defn make-point
  "A point can be represented as a pair of numbers: the x coordinate and the y coordinate."
  [x y]
  (cons x (cons y [])))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn make-segment
  "Each segment is represented as a pair of points: a starting point and an ending point."
  [start-point end-point]
  (cons start-point (cons end-point [])))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn equal-segment? [a b]
  (and (= (start-segment a) (start-segment b)) (= (end-segment a) (end-segment b))))

(defn equal-point? [a b]
  (and (= (x-point a) (x-point b)) (= ( y-point a) ( y-point b))))

(defn print-point [p]
  (println  "(" (x-point p) "," (y-point p) ")"))

(defn midpoint-segment
  "Takes a line segment as argument and returns its midpoint (the point whose coordinates
  are the average of the coordinates of the endpoints)."
  [segment]
  (let [start (start-segment segment)
        end (end-segment segment)]
    (make-point (u/average (x-point start) (x-point end))
                (u/average (y-point start) (y-point end)))))

;;; Exercise 2.3
;; Implement a representation for rectangles in a plane. In terms of your constructors and selectors,
;; create procedures that compute the perimeter and the area of a given rectangle.

;; The following solution represents a rectangle using two opposites points.

(defn make-rectangle
  [top-left bottom-right]
  {:pre [(and (< (x-point top-left) (x-point bottom-right))
              (> (y-point top-left) (y-point bottom-right)))]}
  (cons top-left (cons bottom-right [])))

(defn top-left [rect]
  (first rect))

(defn bottom-right [rect]
  (second rect))

(defn area-rectangle* [rect]
  (let [tl (top-left rect)
        br (bottom-right rect)]
    (* (- (x-point br) (x-point tl))
       (- (y-point tl) (y-point br)))))

(defn perimeter-rectangle* [rect]
  (let [tl (top-left rect)
        br (bottom-right rect)]
    (* 2 (+ (- (x-point br) (x-point tl))
            (- (y-point tl) (y-point br))))))

;; If we use top-left and bottom-right to calculate perimeter and area (see area-rectangle*
;; and perimeter-rectangle*) we are obviously not erecting good abstraction barriers.
;; If the representation changes, area and perimeter need to change with it. The two functions
;; are too much related to the representation using points, but what if we will need to implement
;; make-rectangle using segments?
;; We have to use some other abstraction, something not linked to points or segments, which
;; can be interchangeably swapped in make-rectangle, some other property.
;; We will use width and height in order to achieve our goal.

(defn width [rect]
  (let [tl (first rect)
        br (second rect)]
    (* (- (x-point br) (x-point tl)))))

(defn height [rect]
  (let [tl (first rect)
        br (second rect)]
    (- (y-point tl) (y-point br))))

(defn area-rectangle [rect]
  (* (width rect) (height rect)))

(defn perimeter-rectangle [rect]
  (* 2 (+ (width rect) (height rect))))


(def rect1 (make-rectangle (make-point 0 2) (make-point 4 0)))
(def rect2 (make-rectangle (make-point -1 4) (make-point 3 -2)))
(def point1 (make-point 4 5))
(def point2 (make-point 1 -2))


;;; Exercise 2.4
;; What is the corresponding definition of cdr?
;; (Hint: To verify that this works, make use of the substitution model of section 1.1.5.)

(defn cons* [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

;; (car (cons* [x y]))
;; (car (fn [m] (m x y)))
;; ((fn [m] (m x y)) (fn [p q] p)) ; passes a lambda to the first lambda
;; ((fn [p q] p) x y)              ; substitutes the lambda to parameter m
;; (x)                             ; substitutes p q and therefore returns x

(defn cdr [z]
  (z (fn [p q] q)))

;; (cdr (cons* [x y]))
;; (cdr (fn [m] (m x y)))
;; ((fn [m] (m x y)) (fn [p q] q)) ; passes a lambda to the first lambda
;; ((fn [p q] q) x y)              ; substitutes the lambda to parameter m
;; (y)                             ; substitutes p q and therefore returns x


;;; Exercise 2.5
;; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
;; operations if we represent the pair a and b as the integer that is the product 2^a 3^b.
;; Give the corresponding definitions of the procedures cons, car, and cdr.

;; Playing around with the logarithm rules:
;; log2 (2^a 3^b) = log2 (2^a) + log2 (3^b) = a + b log2 (3)
;;
;; Similarly:
;; log3 (2^a 3^b) = log3 (2^a) + log3 (3^b) = a log3 (2) + b
;;
;; Therefore:
;; a = log2 (2^a 3^b) - b log2 (3)
;; b = log2 (2^a 3^b) - a log3 (2)

(defn cons** [a b]
  {:pre [(and (>= a 0) (>= b 0))]}
  (fn [f] (f a b (* (m/expt 2 a) (m/expt 3 b)))))

(defn car* [z]
  (z (fn [a b prod] (- (u/log prod 2) (* b (u/log 3 2))))))

(defn cdr* [z]
  (z (fn [a b prod] (- (u/log prod 3) (* a (u/log 2 3))))))


;;; Exercise 2.6
;; This representation is known as Church numerals, after its inventor, Alonzo Church,
;; the logician who invented the lambda calculus.
;; Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution
;; to evaluate (add-1 zero)). Give a direct definition of the addition procedure +
;; (not in terms of repeated application of add-1).

(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

;; (add-1 zero)
;; (fn [f] (fn [x] (f ((fn [f] (fn [x] x)) f) x)))
;; (fn [f] (fn [x] (f ((fn [x] x) x))))
;; (fn [f] (fn [x] (f x)))              ; f applied one time to x

;; Church numerals return two-parameter high-order functions which define the numeral "value"
;; by number of f (first parameter) function applications over x (second parameter).
;; In the case of zero, f is never applied to x. One applies f to x once. Two applies it twice.

(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

;; The sum of two numerals will follow the same principle: it will apply f to x n times,
;; where n is the sum of the f applications of the addends.
;; This can be observed using substitution in evaluating (add-1 zero) as suggested.
;; The returned high-order function eventually applies f to x just once (zero never applies it)
;; because we are incrementing by 1. Therefore (add-1 one) will result in applying f twice,
;; as shown below.

;; (add-1 zero)
;; (fn [f] (fn [x] (f ((fn [f] (fn [x] (f x))) f) x)))
;; (fn [f] (fn [x] (f ((fn [x] (f x)) x))))
;; (fn [f] (fn [x] (f (f x))))              ; f applied two times to x

(defn church+ [n m]
  (fn [f] (fn [x] ((m f) ((n f) x)))))

;; Definitions for testing commutativity, associativity and identity.
(def church-one+zero (church+ one zero))
(def church-one+two (church+ one two))
(def church-two+one (church+ two one))
(def church-one+zero+two (church+ church-one+zero two))
(def church-zero+two+one (church+ zero church-two+one))


(t/deftest tests
  (t/is (s/equal-rat? (make-rat* 1 2) (make-rat* -1 -2)))
  (t/is (s/equal-rat? (make-rat* 1 2) (make-rat* 1 2)))
  (t/is (s/equal-rat? (make-rat* -1 2) (make-rat* -1 2)))
  (t/is (s/equal-rat? (make-rat* -1 2) (make-rat* 1 -2)))
  (t/is (= 4 (x-point point1)))
  (t/is (= 5 (y-point point1)))
  (t/is (equal-point? point1 (start-segment (make-segment point1 point2))))
  (t/is (equal-point? point2 (end-segment (make-segment point1 point2))))
  (t/is (equal-point? point1 (midpoint-segment (make-segment (make-point 7 12) point2))))
  (t/is (= 8 (area-rectangle* rect1)))
  (t/is (= 24 (area-rectangle* rect2)))
  (t/is (= 12 (perimeter-rectangle* rect1)))
  (t/is (= 20 (perimeter-rectangle* rect2)))
  (t/is (= 8 (area-rectangle rect1)))
  (t/is (= 24 (area-rectangle rect2)))
  (t/is (= 12 (perimeter-rectangle rect1)))
  (t/is (= 20 (perimeter-rectangle rect2)))
  (t/is (= 1 (car (cons* 1 2))))
  (t/is (= 2 (cdr (cons* 1 2))))
  (t/is (u/equal-to? 1 (car* (cons** 1 2))))
  (t/is (u/equal-to? 2 (cdr* (cons** 1 2))))
  (t/is (= 3 ((zero inc) 3)))
  (t/is (= 4 (((add-1 zero) inc) 3)))
  (t/is (= 4 ((one inc) 3)))
  (t/is (= 5 ((two inc) 3)))
  (t/is (= 6 (((church+ two one) inc) 3)))
  (t/is (= 4 (((church+ zero one) inc) 3)))
  (t/is (= ((one inc) 3) ((church-one+zero inc) 3)))           ; Additive identity
  (t/is (= ((church-two+one inc) 3) ((church-one+two inc) 3))) ; Commutativity
  (t/is (= ((church-one+zero+two inc) 3) ((church-zero+two+one inc) 3))))
