(ns deep)

(defn a [n] (* n 2))

(defn handler [req]
  (let [n (get req :n)]
    (process (* n 2))))
