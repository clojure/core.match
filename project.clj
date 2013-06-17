(defproject org.clojure/core.match "0.2.0-beta2"
  :description "Optimized pattern matching and predicate dispatch for Clojure"

  :test-paths ["src/test/clojure"]
  :source-paths ["src/main/clojure"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1806"]]
  :dev-dependencies [[org.clojure/tools.nrepl "0.2.3"]]
  :plugins [[lein-cljsbuild "0.3.0"]]

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src/test/cljs"]
     :compiler {:output-to "out/test.js"
                :static-fns true
                :optimizations :simple}}
    {:id "test-adv"
     :source-paths ["src/test/cljs"]
     :compiler {:output-to "out/test_adv.js"
                :optimizations :advanced}}]})
