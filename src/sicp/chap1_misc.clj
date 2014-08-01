(ns sicp.chap1-misc)

(def factorial
  ^{:user/comment "Easy iterative implementation"
   :doc "Calculates the factorial"}
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt))))))

(meta #'factorial)

(factorial 3)

(def t0 (System/currentTimeMillis))
(defn t1 [] (System/currentTimeMillis))
t0
t0
(t1)

(defn conditional-ops [a b]
  (if (< a b) + -))

((conditional-ops 5 4) 10 11)

(if   0 :t :f)  ; ⇒ :t
(if  "" :t :f)  ; ⇒ :t
(if  [] :t :f)  ; ⇒ :t
(if  {} :t :f)  ; ⇒ :t
(if #{} :t :f)  ; ⇒ :t
(if nil :t :f)

(= {:a  [1 2 3] :b [3]} {:b [3] :a [1 2 3]})

(def a-string "My name")
(.length a-string)
(defn to-lower-case [s]
  (.toLowerCase s))
(to-lower-case a-string)
(map to-lower-case '("Questa" "E'" "UNA" "proVa"))

(+ 1 (+ 2 3) 4)

(+ 1 (* 2 3) (+ 3 4))
