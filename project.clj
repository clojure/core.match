(defproject org.clojure/core.match "0.3.0-alpha4-SNAPSHOT"
  :description "Optimized pattern matching for Clojure"

  :jvm-opts ^:replace ["-Xmx512m" "-server"]

  :test-paths ["src/test/clojure"]
  :source-paths ["src/main/clojure"]

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.analyzer.jvm "0.6.5"]
                 [org.clojure/clojurescript "1.9.293" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.1.4"]]

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src/test/cljs"]
     :compiler {:output-to "out/test.js"
                :static-fns true
                :optimizations :simple}}
    {:id "test-adv"
     :source-paths ["src/test/cljs"]
     :compiler {:output-to "out/test.js"
                :optimizations :advanced}}]})
