(defproject match "0.2.0-alpha11-SNAPSHOT"
  :description "Optimized pattern matching and predicate dispatch for Clojure"

  ;; lein 1
  :source-path "src/main/clojure"
  :test-path "src/test/clojure"

  ;; lein 2
  :test-paths ["src/test/clojure"]
  :source-paths ["src/main/clojure"]

  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojurescript "0.0-1576"]]
  :dev-dependencies [[nrepl "0.2.1"]]
  :plugins [[lein-cljsbuild "0.3.0"]]

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src/test/cljs"]
     :compiler {:output-js "test.js"
                :pretty-print true
                :static-fns true
                :optimizations :simple}}
    {:id "test-adv"
     :source-paths ["src/test/cljs"]
     :compiler {:output-js "test-adv.js"
                :pretty-print true
                :optimizations :advanced}}]})
