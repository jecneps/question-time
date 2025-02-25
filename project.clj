(defproject question-time "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [enlive "1.1.6"]
                 [clj-http "3.12.3"]
                 [org.clojure/data.json "2.5.0"]
                 [clj-wrap-indent "1.0.0"]
                 [org.clojure/data.csv "1.1.0"]
                 [org.clojure/core.match "1.1.0"]]
  :main ^:skip-aot question-time.parsing
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
