(ns com.blockether.vis.ext.language-clojure.nrepl-client-test
  "Integration test against a real, embedded nREPL server.

   We start one server per test, get the chosen port from `:port`,
   eval through our cached-conn client, then stop the server.
   `nrepl-client/close-all!` between tests prevents the cached
   connection from a previous run dialing into a dead socket."
  (:require
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
   [lazytest.core :refer [defdescribe expect it]]
   [nrepl.server :as server]))

(defn- with-server
  "Start an nREPL on an ephemeral port, run `f port`, stop the server.
   Always closes cached client connections so the next test sees a
   clean cache."
  [f]
  (let [srv (server/start-server :port 0)
        port (:port srv)]
    (try
      (f port)
      (finally
        (nc/close-all!)
        (server/stop-server srv)))))

(defdescribe eval-test
  (it "evaluates a single form and reports the value"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port :code "(+ 1 2)"})]
          (expect (= "3" (:value r)))
          (expect (contains? (:status r) "done"))
          (expect (false? (:timed-out? r)))
          (expect (number? (:ms r)))))))

  (it "captures stdout"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port :code "(println \"hi\")"})]
          (expect (re-find #"hi" (:out r)))
          (expect (contains? (:status r) "done"))))))

  (it "reports eval exceptions inside the response"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port :code "(/ 1 0)"})]
          (expect (contains? (:status r) "done"))
          (expect (or (re-find #"Divide" (str (:err r) ""))
                    (some? (:ex r)))))))))

(defdescribe connect-failure-test
  (it "throws :clj/nrepl-connect-failed on an obviously-closed port"
    (let [thrown? (try
                    (nc/eval! {:port 1 :code "(+ 1 1)"})
                    false
                    (catch clojure.lang.ExceptionInfo e
                      (= :clj/nrepl-connect-failed (:type (ex-data e)))))]
      (expect thrown?))))
