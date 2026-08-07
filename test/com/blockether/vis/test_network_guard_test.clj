(ns com.blockether.vis.test-network-guard-test
  (:require [com.blockether.vis.test-network-guard :as guard]
            [lazytest.core :refer [defdescribe expect it]]))

;; The suite used to dial `api.openai.com/v1/models` from provider and loop
;; tests: every fleet mutation rebuilt the router, and the router enumerates each
;; provider's live catalog. Tens of seconds of DNS/TLS/timeout for an assertion
;; nobody made, and a hard hang on a machine without network.
(defdescribe
  network-guard-test
  (it "refuses a public host"
      (expect (guard/refused? "api.openai.com"))
      (expect (guard/refused? (guard/request-host {:uri "https://api.openai.com/v1/models"}))))
  (it "lets the suite's own loopback sockets through"
      (expect (not (guard/refused? "127.0.0.1")))
      (expect (not (guard/refused? "localhost")))
      (expect (not (guard/refused? (guard/request-host {:uri "http://127.0.0.1:8080/healthz"}))))
      (expect (not (guard/refused? (guard/request-host {:uri {:host "localhost"}})))))
  (it "never refuses a request that names no host"
      (expect (nil? (guard/request-host {})))
      (expect (not (guard/refused? nil))))
  (it "opens on demand for a test that means to reach the wire"
      (binding [guard/*allow-network* true]
        (expect (not (guard/refused? "api.openai.com")))))
  (it "throws with the refused host instead of dialing it"
      (let
        [wrapped
         (guard/guard (fn [_]
                        :dialed))

         thrown
         (try (wrapped {:uri "https://api.openai.com/v1/models"})
              (catch clojure.lang.ExceptionInfo e (ex-data e)))]

        (expect (= :vis.test/network-refused (:type thrown)))
        (expect (= "api.openai.com" (:host thrown)))
        (expect (= :dialed (wrapped {:uri "http://127.0.0.1:1/x"}))))))
