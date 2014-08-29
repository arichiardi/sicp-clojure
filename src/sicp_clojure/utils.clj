(ns sicp-clojure.utils)

(defmacro microbench [n expr]
  " Evaluates the expression n number of times, returning the average
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
