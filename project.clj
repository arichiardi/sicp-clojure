(defproject com.andrearichiardi.sicp-clojure "0.0.1-SNAPSHOT"
  :description "Structure and Interpretation of Computer Programs - Exercises and examples in Clojure."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [leiningen "2.4.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.trace "0.7.8"]]
  :global-vars { *warn-on-reflection* true }
  :repl-options {:init (set! *print-length* 200)})
