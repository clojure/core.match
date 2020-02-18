(defproject org.clojure/core.match "1.1.0-SNAPSHOT"
  :description "Optimized pattern matching for Clojure"

  :jvm-opts ^:replace ["-Xmx512m" "-server"]

  :test-paths ["src/test/clojure"]
  :source-paths ["src/main/clojure"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2496" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]
            [cider/cider-nrepl "0.8.1"]]

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
