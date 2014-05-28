(defproject clozen "0.1.0-SNAPSHOT"
  :description "A general library of helper functions"
  :url "http://github.com/zenna/clozen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[com.keminglabs/cljx "0.3.2"]
            [lein-cljsbuild "1.0.2"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2030"]
                 [org.clojure/tools.macro "0.1.2"]
                 [incanter "1.4.1"]
                 ]
  :source-paths ["src/clj" "target/classes"]
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}]}

  :cljsbuild {:builds
              {:dev {:source-paths ["src/clj" "target/classes"]
                     :compiler {:output-to "target/main.js"
                                :optimizations :whitespace
                                :pretty-print true}}}}
  :hooks [cljx.hooks]
  :main clozen.core)
