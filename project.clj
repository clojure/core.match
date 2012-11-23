(defproject match "0.2.0-alpha11-SNAPSHOT"
  :description "Optimized pattern matching and predicate dispatch for Clojure"

  ;; lein 1
  :source-path "src/main/clojure"
  :test-path "src/test/clojure"

  ;; lein 2
  :test-paths ["src/test/clojure"]
  :source-paths ["src/main/clojure"]

  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[lein-swank "1.4.4"]
                     [lein-cljsbuild "0.2.9"]])
