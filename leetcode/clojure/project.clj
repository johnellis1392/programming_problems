(defproject leetcode "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name ""
            :url ""}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot .
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :test {:dependencies [#_[clojure.test/clojure.test ""]]}})
