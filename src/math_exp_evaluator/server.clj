(ns math-exp-evaluator.server
  (:require [ring.adapter.jetty :as jetty]
            [math-exp-evaluator.core :refer :all]
            [ring.middleware.json :refer :all])
  (:gen-class))

(defn handler [request]
  (println request)
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (get-points ((:body request) "eq") (eval (read-string ((:body request) "range"))))})

(defn -main []
  (let [port (or (Integer/parseInt (System/getenv "PORT")) 3000)]
    (jetty/run-jetty (-> handler
                         wrap-json-body
                         wrap-json-response) {:port port})))
