(ns sicp-clojure.utils
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))


(defn make-file-from-resource [name] (io/file (io/resource name)))


(defn to-bit-representation [^java.lang.Double d]
  (-> (Double/doubleToRawLongBits d) (java.lang.Long/toBinaryString)))


(defn methods-of [instance & [string]]
  "Returns the \"[...] public member methods of the class or interface represented by this Class
  object, including those declared by the class or interface and those inherited from superclasses
  and superinterfaces.\". See java.lang.Class/getMethods for details.
  If a second argument is provided as valid regex o string, the list is filtered according to it."
  {:pre [(nil? instance)]}
  (filter #(re-find (re-pattern (or string #".*")) %) (map #(.getName %) (-> instance class .getMethods))))


;; from www.bestinclass.dk/index.clj/2010/02/benchmarking-jvm-languages.html
(defmacro microbench [n expr]
  "Evaluates the expression n number of times, returning the average
  time spent in computation, removing highest and lowest values.

  If the body of expr returns nil, only the timing is returned otherwise
  the result is printed - does not affect timing.

  Before timings begin, a warmup is performed lasting either 1 minute or
  1 full computational cycle, depending on which comes first."
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


(t/deftest tests
  (t/is (empty? (filter #((not (.contains "Value")) %1) (methods-of java.lang.Double "Value")))))
