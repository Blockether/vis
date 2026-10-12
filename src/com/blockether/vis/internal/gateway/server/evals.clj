(ns com.blockether.vis.internal.gateway.server.evals
  "Evaluation routes. Each route runs one operation of the bundled SDK package
   `blockether.vis.evals` on the JSON body, so the logic exists only in Python.
   The operations read no files and keep no state. Only the leaderboard uses the
   network."
  (:require [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.python.extensions :as extensions]))

(defn- operation
  "The handler that runs evaluation `op` on the request body."
  [op]
  (fn [request]
    (let [body (http/body-json request)]
      (if-not (map? body)
        (http/error-response 400 :invalid-evals-request "The request body must be a JSON object")
        (try (let [{:strs [result error]} (extensions/evals-call op body)]
               (if error
                 (http/error-response 400 :invalid-evals-request error)
                 (http/json-response result)))
             (catch clojure.lang.ExceptionInfo e
               (http/error-response 500 :evals-failed (ex-message e))))))))

(def handlers
  {[:post "/v1/evals/agreement"] (operation "agreement")
   [:post "/v1/evals/compare"] (operation "compare")
   [:post "/v1/evals/diagnose"] (operation "diagnose")
   [:post "/v1/evals/labels"] (operation "labels")
   [:post "/v1/evals/leaderboard"] (operation "leaderboard")
   [:post "/v1/evals/report"] (operation "report")})
