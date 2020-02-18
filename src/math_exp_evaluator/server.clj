(ns math-exp-evaluator.server
  (:require [ring.adapter.jetty :as jetty]
            [math-exp-evaluator.core :refer :all]
            [ring.middleware.json :refer :all]
            [ring.middleware.cors :refer :all])
  (:gen-class))

(defn handler [request]
  (println request)
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [req-body (:body request)]
              (get-points (-> "eq"
                              req-body
                              remove-spaces) (eval (read-string ((:body request) "range")))))})

(defn -main []
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    (jetty/run-jetty (-> handler
                         wrap-json-body
                         wrap-json-response
                         (wrap-cors :access-control-allow-headers #{"accept"
                                                                    "accept-encoding"
                                                                    "accept-language"
                                                                    "authorization"
                                                                    "content-type"
                                                                    "origin"}
                                    :access-control-allow-origin [#".*"]
                                    :access-control-allow-methods [:get :put :patch :post :delete])) {:port port})))
