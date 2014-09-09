(ns sicp-clojure.2-1-exercises
  (:require :reload-all [clojure.test :as t]
            [clojure.math.numeric-tower :as m :refer (abs gcd)]
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
  (t/is (= 20 (perimeter-rectangle rect2))))
