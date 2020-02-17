(defproject math_exp_evaluator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [instaparse "1.4.10"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-core "1.8.0"]
                 [ring-cors "0.1.13"]]
  :main math-exp-evaluator.server
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
