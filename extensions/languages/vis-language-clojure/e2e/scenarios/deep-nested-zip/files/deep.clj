(ns deep)
(defn handler
  [req]
  (let [body (get req :body)]
    (when (valid? body) (process {:data {:items [{:id 1 :score (* base 2)}]}}))))
