(ns sicp-clojure.2-1-exercises
  (:require [clojure.test :as t]
            [sicp-clojure.utils :as u]))

;;; Exercise 2.7
;; This exercise is part of 2.1.4  Extended Exercise: Interval Arithmetic.
;; Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction.
;; [...] Define selectors upper-bound and lower-bound to complete the implementation.

(defn make-interval [a b]
  {:pre [(<= a b)]}
  (cons a (cons b [])))

(defn lower-bound [x] (first x))

(defn upper-bound [x] (second x))

;; The following are defined by Alyssa:

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (upper-bound y))
        p4 (* (upper-bound x) (lower-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x (make-interval (/ (upper-bound y)) (/ (lower-bound y)))))

;; And for testing we will need:

(def i1 (make-interval 6.12 7.48)) ; 6.8 ohm 10% tolerance
(def i2 (make-interval 4.465 4.935)) ; 4.7 ohm 5% tolerance

(defn reciprocal-interval [x]
  (make-interval (/ (upper-bound x)) (/ (lower-bound x))))

(defn parallel-resistance [r1 r2]
  (reciprocal-interval (add-interval (reciprocal-interval r1)
                                     (reciprocal-interval r2))))

(defn equal-interval? [x y]
  (and (u/equal-to? (lower-bound x) (lower-bound y))
       (u/equal-to? (upper-bound x) (upper-bound y))))


;;; Exercise 2.8
;; Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed.
;; Define a corresponding subtraction procedure, called sub-interval.

(defn sub-interval [x y]
  (let [p1 (- (lower-bound x) (upper-bound y))
        p2 (- (upper-bound x) (lower-bound y))]
    (make-interval (min p1 p2) (max p1 p2))))


;;; Exercise 2.9
;; The width of an interval is half of the difference between its upper and lower bounds.
;; Show that the width of the sum (or difference) of two intervals is a function only of
;; the widths of the intervals being added (or subtracted). Give examples to show that this
;; is not true for multiplication or division.

(defn width-interval
  "Half of the difference between its upper and lower bounds."
  [x]
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; For addition (or subtraction), it is shown below how it is possible to get sum the two individual widths
;; in order to obtain the width of the sum (uncomment to evaluate).

;; (width-interval i1)
;; (width-interval i2)
;; (width-interval (add-interval i1 i2))

;; On the contrary, for multiplication (or division) the above statement doesn't hold true.

;; (width-interval (mul-interval i1 i2))


;;; Exercise 2.10
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear
;; what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition
;; and to signal an error if it occurs.

;; We want to avoid the case when, for instance, the reciprocal of the interval [-2,2] as defined by the
;; book (above) produces [1/2,1/-2] = [0.5 -0.5].

(defn div-interval* [x y]
  {:pre [(or (< (upper-bound y) 0)
             (> (lower-bound y) 0))]}
  (mul-interval x (reciprocal-interval y)))


;;; Exercise 2.11
;; In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals,
;; it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications."
;; Rewrite this procedure using Ben's suggestion.

(defn mul-interval* [x y]
  (let [lbx (lower-bound x)
        lby (lower-bound y)
        ubx (upper-bound x)
        uby (upper-bound y)]
    (cond (and (< lbx 0)  (< ubx 0)  (>= lby 0) (>= uby 0)) (make-interval (* lbx uby) (* ubx lby))
          (and (< lbx 0)  (< ubx 0)  (< lby 0)  (< uby 0))  (make-interval (* ubx uby) (* lbx lby))
          (and (< lbx 0)  (< ubx 0)  (< lby 0)  (>= uby 0)) (make-interval (* lbx uby) (* lbx lby))
          (and (>= lbx 0) (>= ubx 0) (>= lby 0) (>= uby 0)) (make-interval (* lbx lby) (* ubx uby))
          (and (>= lbx 0) (>= ubx 0) (< lby 0)  (< uby 0))  (make-interval (* ubx lby) (* lbx uby))
          (and (>= lbx 0) (>= ubx 0) (< lby 0)  (>= uby 0)) (make-interval (* ubx lby) (* ubx uby))
          (and (< lbx 0)  (>= ubx 0) (>= lby 0) (>= uby 0)) (make-interval (* lbx uby) (* ubx uby))
          (and (< lbx 0)  (>= ubx 0) (< lby 0)  (< uby 0))  (make-interval (* ubx lby) (* lbx lby))
          (and (< lbx 0)  (>= ubx 0) (< lby 0)  (>= uby 0)) (make-interval (min (* lbx uby) (* ubx lby))
                                                                           (max (* lbx lby) (* ubx uby))))))

(def i1-nn (make-interval -7.48 -6.12))
(def i1-np (make-interval -6.12 7.48))
(def i1-pp (make-interval 6.12 7.48))

(def i2-nn (make-interval -4.935 -4.465))
(def i2-np (make-interval -4.465 4.935))
(def i2-pp (make-interval 4.465 4.935))


;;; Exercise 2.12
;; Define a constructor make-center-percent that takes a center and a percentage tolerance
;; and produces the desired interval. You must also define a selector percent that produces
;; the percentage tolerance for a given interval. The center selector is the same as the one
;; shown above (below).

;; Alyssa's implementation:

;; (defn make-center-width [c w]
;;   (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; (defn width [i]
;;   (/ (- (upper-bound i) (lower-bound i)) 2))

;; Solution to the exercise:
(defn make-center-percent [c p]
  (let [w (* p (/ c 100))]
    (make-interval (- c w) (+ c w))))

(defn percent [i]
  (let [w (/ (- (upper-bound i) (lower-bound i)) 2)]
    (/ (* w 100) (+ w (lower-bound i)))))


;;; Exercise 2.13
;; Show that under the assumption of small percentage tolerances there is a simple formula
;; for the approximate percentage tolerance of the product of two intervals in terms of the
;; tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

;; In case of small percentages, the percentage of the result of multiplying two intervals is
;; equal to the sum of the percentages of the same two intervals. It is shown below:

(def i3 (make-interval 2.105 2.215))
(def i4 (make-interval 3.567 3.777))

;; (percent i3)
;; (percent i4)
;; (percent (mul-interval* i3 i4))

;; With bigger percentage tolerances the trick doesn't work. For formal proof see
;; http://www.billthelizard.com/2010/12/sicp-212-216-extended-exercise-interval.html

(def i5 (make-interval 2.105 4.305))
(def i6 (make-interval 3.567 4.867))

;; (percent i5)
;; (percent i6)
;; (percent (mul-interval* i5 i6))


;;; Exercise 2.14
;; Demonstrate that Lem is right. Investigate the behavior of the system on a variety of
;; arithmetic expressions. Make some intervals A and B, and use them in computing the
;; expressions A/A and A/B. . You will get the most insight by using intervals whose width
;; is a small percentage of the center value. Examine the results of the computation in
;; center-percent form.

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; From the following it can be observed that the div-interval function returns an approximation,
;; even when we divide an interval by itself, which should istead result in the interval (1 1).

;; (def i7 (make-center-percent 2.105 5))
;; (def i8 (make-center-percent 5.105 1))
;; (width-interval i8)
;; (width-interval i7)

;; (div-interval i7 i7)
;; (div-interval i7 i8)

;; Therefore Lem was right (expected interval: (2.58 2.97)).

;; (par1 i1 i2)
;; (par2 i1 i2) ; closer to the expected result


;;; Exercise 2.15
;; Eva Lu Ator, another user, has also noticed the different intervals computed by different
;; but algebraically equivalent expressions. She says that a formula to compute with intervals
;; using Alyssa's system will produce tighter error bounds if it can be written in such a form
;; that no variable that represents an uncertain number is repeated. Thus, she says, par2 is
;; a "better" program for parallel resistances than par1. Is she right? Why?

;; As it can be see below, each division/multiplication adds an error at every calculation.

;; (def one (div-interval i1 i1))
;; (def one2 (div-interval one one))
;; (def one3 (div-interval one2 one2))

;; (percent one) ; => 19.801...
;; (percent one2) ; => 38.109...
;; (percent one3) ; => 66.554...

;; (percent (mul-interval one one))
;; (percent (mul-interval one2 one2))

;; In par2 we are using an interval with no tolerance (one), which will produce tighter errors,
;; effectively reducing the amount of misbehaving expressions (add-interval at line 4 is the
;; only one vs the three of par1).
;; This hypothesis is proven true with the simple examples below:

;; (def true-one (make-interval 1 1))
;; (percent (div-interval one i1)) ; => 29.223...
;; (percent (div-interval true-one i1)) ; => 10.000...

;; Therefore Eva is right. Par2 returns a more accurate interval than par1.


;;; Exercise 2.16
;; The task is not impossible but very hard indeed, see the following link:
;; http://www.cs.utep.edu/interval-comp/main.html


(t/deftest tests
  (t/is (u/equal-to? 6.12 (lower-bound i1)))
  (t/is (u/equal-to? 7.48 (upper-bound i1)))
  (t/is (equal-interval? (make-interval 2.58 2.973) (parallel-resistance i1 i2)))
  (t/is (equal-interval? (make-interval 27.3258 36.9138) (mul-interval i1 i2)))
  (t/is (equal-interval? (make-interval 1.2401 1.6752) (div-interval i1 i2)))
  (t/is (equal-interval? (make-interval (- 3.015) (- 1.185)) (sub-interval i2 i1)))
  (t/is (equal-interval? (make-interval 1.185 3.015) (sub-interval i1 i2)))
  (t/is (u/equal-to? (+ (width-interval i1) (width-interval i2)) (width-interval (add-interval i1 i2))))
  (t/is (not (u/equal-to? (+ (width-interval i1) (width-interval i2)) (width-interval (mul-interval i1 i2)))))
  (t/is (equal-interval? (make-interval (- 36.9138) (- 27.3258)) (mul-interval* i1-nn i2-pp)))
  (t/is (equal-interval? (make-interval 27.3258 36.9138) (mul-interval* i1-nn i2-nn)))
  (t/is (equal-interval? (make-interval -36.9138 33.3982) (mul-interval* i1-nn i2-np)))
  (t/is (equal-interval? (make-interval 27.3258 36.9138) (mul-interval* i1-pp i2-pp)))
  (t/is (equal-interval? (make-interval -36.9138 (- 27.3258)) (mul-interval* i1-pp i2-nn)))
  (t/is (equal-interval? (make-interval (- 33.3982) 36.9138) (mul-interval* i1-pp i2-np)))
  (t/is (equal-interval? (make-interval (- 30.2022) 36.9138) (mul-interval* i1-np i2-pp)))
  (t/is (equal-interval? (make-interval (- 36.9138) 30.2022) (mul-interval* i1-np i2-nn)))
  (t/is (equal-interval? (make-interval (- 33.3982) 36.9138) (mul-interval* i1-np i2-np)))
  (t/is (equal-interval? i1 (make-center-percent 6.8 10)))
  (t/is (u/equal-to? 6.8 (center i1)))
  (t/is (u/equal-to? 10 (percent i1)))
  (t/is (u/equal-to? (+ (percent i3) (percent i4)) (percent (mul-interval* i3 i4))))
  (t/is (not (u/equal-to? (+ (percent i5) (percent i6)) (percent (mul-interval* i5 i6))))))
