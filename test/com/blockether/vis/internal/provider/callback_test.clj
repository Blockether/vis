(ns com.blockether.vis.internal.provider.callback-test
  (:require [babashka.http-client :as http]
            [com.blockether.vis.internal.provider.callback :as callback]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  callback-receiver-test
  (it "accepts one matched response and does not reflect callback values into HTML"
      (let [{:keys [redirect-uri result stop!]}
            (callback/listen! "http://127.0.0.1:0/callback" "test-state" 2000)

            input
            (str redirect-uri "?code=test-code&state=test-state")]

        (try (doseq [suffix ["?code=test-code" "?code=test-code&state=other"
                             "?code=test-code&state=test-state&state=test-state"
                             "/extra?code=test-code&state=test-state"
                             "?code=test-code&error=access_denied&state=test-state"]]
               (expect (= 400 (:status (http/get (str redirect-uri suffix) {:throw false})))))
             (expect (not (realized? result)))
             (let [response (http/get input)]
               (expect (= 200 (:status response)))
               (expect (= "no-store" (get-in response [:headers "cache-control"])))
               (expect (not (.contains ^String (:body response) "test-code")))
               (expect (= input (deref result 1000 :timeout))))
             (finally (stop!)))))
  (it "accepts a matched denial, without claiming authorization succeeded"
      (let [{:keys [redirect-uri result stop!]}
            (callback/listen! "http://127.0.0.1:0/callback" "test-state" 2000)

            input
            (str redirect-uri "?error=access_denied&state=test-state")]

        (try (expect (= 200 (:status (http/get input))))
             (expect (= input (deref result 1000 :timeout)))
             (finally (stop!)))))
  (it "releases an expired or cancelled receiver without any later auth traffic"
      (doseq [cancel? [true false]]
        (let [{:keys [result stop!]}
              (callback/listen! "http://127.0.0.1:0/callback" "test-state" 20)]
          (when cancel? (stop!))
          (expect (= (if cancel? :cancelled :expired) (deref result 2000 :timeout)))
          (stop!))))
  (it "never binds a public or user-info address"
      (doseq [uri ["http://10.0.0.5:1234/callback" "https://127.0.0.1:1234/callback"
                   "http://user@127.0.0.1:1234/callback" "http://127.0.0.1:1234/callback?q=1"]]
        (expect (try (callback/listen! uri "test-state" 20) false (catch Exception _ true)))))
  (it "requires the exact callback destination and unambiguous query"
      (expect (not (callback/response-uri?
                     "http://localhost:1455/auth/callback"
                     "test-state"
                     "http://127.0.0.1:1455/auth/callback?code=a&state=test-state")))
      (expect (not (callback/response-uri?
                     "http://localhost:1455/auth/callback"
                     "test-state"
                     "http://localhost:1455/auth/callback?code=a&state=test-state#fragment")))
      (expect (nil? (callback/query "state=%not-encoding")))))
