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
          (expect (= "3" (get r "value")))
          (expect (contains? (get r "status") "done"))
          (expect (false? (get r "timed_out")))
          (expect (number? (get r "ms")))))))

  (it "captures stdout"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port :code "(println \"hi\")"})]
          (expect (re-find #"hi" (get r "out")))
          (expect (contains? (get r "status") "done"))))))

  (it "reports eval exceptions inside the response"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port :code "(/ 1 0)"})]
          (expect (false? (get r "timed_out")))
          (expect (or (re-find #"Divide" (str (get r "err") ""))
                    (some? (get r "ex")))))))))

(defdescribe error-enrichment-test
  (it "enriches an eval error with a structured message + demunged user trace"
    (with-server
      (fn [port]
        ;; plain embedded nREPL (no cider-nrepl) → exercises the *e self-fetch
        (nc/eval! {:port port :code "(defn boom [x] (/ x 0)) (defn caller [] (boom 5)) nil"})
        (let [r (nc/eval! {:port port :code "(caller)"})]
          (expect (contains? (get r "status") "eval-error"))
          (expect (re-find #"ArithmeticException" (str (get r "error_message"))))
          (expect (re-find #"Divide by zero" (str (get r "error_message"))))
          ;; the trace names OUR frames, not JVM/nREPL plumbing
          (expect (vector? (get r "trace")))
          (expect (some #(re-find #"user/boom" %) (get r "trace")))
          (expect (some #(re-find #"user/caller" %) (get r "trace")))
          (expect (not-any? #(re-find #"clojure\.lang\." %) (get r "trace")))))))

  (it "surfaces ex-data on an ExceptionInfo"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port
                           :code "(throw (ex-info \"boom\" {:code 42}))"})]
          (expect (re-find #"ExceptionInfo" (str (get r "error_message"))))
          (expect (re-find #":code 42" (str (get r "error_data")))))))))

(defdescribe reader-error-test
  (it "returns reader syntax errors immediately instead of timing out"
    (with-server
      (fn [port]
        (let [r (nc/eval! {:port port
                           :code "(defn broken []])"
                           :timeout-ms 5000})]
          (expect (false? (get r "timed_out")))
          (expect (contains? (get r "status") "eval-error"))
          (expect (re-find #"Syntax error reading source" (get r "err"))))))))

(defdescribe connect-failure-test
  (it "throws :clj/nrepl-connect-failed on an obviously-closed port"
    (let [thrown? (try
                    (nc/eval! {:port 1 :code "(+ 1 1)"})
                    false
                    (catch clojure.lang.ExceptionInfo e
                      (= :clj/nrepl-connect-failed (:type (ex-data e)))))]
      (expect thrown?))))

(defdescribe probe-test
  (it "reports :up with versions, :clj dialect, and a string cwd against a live server"
    (with-server
      (fn [port]
        (let [r (nc/probe! {:port port :timeout-ms 1000})]
          (expect (= :up (:status r)))
          (expect (= :clj (:dialect r)))
          (expect (string? (get-in r [:versions :clojure])))
          ;; cwd is best-effort; when present it must be a non-blank string
          (expect (or (nil? (:cwd r)) (and (string? (:cwd r)) (seq (:cwd r)))))))))

  (it "reports :down on a closed port and never throws"
    (expect (= {:status :down} (nc/probe! {:port 1 :timeout-ms 200})))
    (expect (= {:status :down} (nc/probe! {:port nil})))))
