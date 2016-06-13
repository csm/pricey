
(defproject pricey "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [amazonica "0.3.34"]
                 [hickory "0.5.4"]
                 [clj-http "2.0.0"]
                 [cheshire "5.5.0"]
                 [com.netflix.frigga/frigga "0.3"]
                 [org.clojure/tools.logging "0.3.1"]]
  :main ^:skip-aot pricey.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
