(ns com.blockether.vis.internal.provider.vendor.openai-codex-reset-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.internal.provider.vendor.openai-codex :as codex]
            [lazytest.core :refer [defdescribe expect it]]))

(def token {:token "test-access" :llm-headers {"chatgpt-account-id" "test-account"}})

(def attempt {:account-id "test-account" :idempotency-key "fdbe360a-21e0-4bca-86ef-cd8f49e78fa6"})

(defn- report
  [usage response]
  (with-redefs [codex/detect-credentials
                (constantly {:account-id "test-account"})

                codex/get-openai-codex-token!
                (constantly token)

                codex/fetch-usage!
                (fn [& _]
                  usage)

                http/get
                (fn [url opts]
                  (expect (= "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits" url))
                  (expect (= "Bearer test-access" (get-in opts [:headers "Authorization"])))
                  (expect (= "test-account" (get-in opts [:headers "chatgpt-account-id"])))
                  response)]

    (get-in (codex/limits) [:dynamic :reset-credits])))

(defdescribe reset-availability-is-account-scoped-and-never-inferred-from-quota
             (it "reset availability is account scoped and never inferred from quota"
                 (expect (= {:status :ok :available-count 2 :account-id "test-account"}
                            (report {:rate_limit_reset_credits {:available_count 2}} nil)))
                 (expect (= {:status :ok :available-count 0 :account-id "test-account"}
                            (report {} {:status 200 :body "{\"available_count\":0}"})))
                 ;; missing or malformed data is not zero credits
                 (doseq [response [{:status 404 :body "not supported"}
                                   {:status 500 :body "test-private-response"}
                                   {:status 200 :body "{\"available_count\":-1}"}
                                   {:status 200 :body "{\"available_count\":null}"}]]
                   (let [result (report {} response)]
                     (expect (contains? #{:unsupported :error} (:status result)))
                     (expect (not (contains? result :available-count)))
                     (expect (not (.contains (pr-str result) "test-private-response")))))))

(defdescribe consume-uses-the-confirmed-account-and-idempotency-key
             (it "consume uses the confirmed account and idempotency key"
                 (doseq [outcome ["reset" "nothing_to_reset" "no_credit" "already_redeemed"]]
                   (with-redefs
                     [codex/get-openai-codex-token! (constantly token)
                      http/post
                      (fn [url opts]
                        (expect
                          (= "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits/consume"
                             url))
                        (expect (= {"redeem_request_id" (:idempotency-key attempt)}
                                   (json/read-json (:body opts))))
                        (expect (= "test-account" (get-in opts [:headers "chatgpt-account-id"])))
                        (expect (false? (:throw opts)))
                        {:status 200 :body (json/write-json-str {:code outcome})})]

                     (expect (= {:outcome outcome} (codex/consume-reset-credit! attempt)))))))

(defdescribe account-switch-refuses-to-spend-a-different-accounts-credit
             (it "account switch refuses to spend a different accounts credit"
                 (let [posts (atom 0)]
                   (with-redefs [codex/get-openai-codex-token! (constantly (assoc-in token
                                                                             [:llm-headers
                                                                              "chatgpt-account-id"]
                                                                             "other-account"))
                                 http/post (fn [& _]
                                             (swap! posts inc))]

                     (expect (= :account-changed (:error (codex/consume-reset-credit! attempt)))))
                   (expect (zero? @posts)))))

(defdescribe reset-failures-are-sanitized-and-unknown-outcomes-stay-unknown
             (it "reset failures are sanitized and unknown outcomes stay unknown"
                 (doseq [response [{:status 503 :body "test-private-response"}
                                   {:status 200 :body "not-json test-private-response"}
                                   {:status 200 :body "{\"code\":\"future_outcome\"}"}]]
                   (with-redefs [codex/get-openai-codex-token! (constantly token)
                                 http/post (fn [& _]
                                             response)]

                     (let [result (codex/consume-reset-credit! attempt)]
                       (expect (= :reset-unconfirmed (:error result)))
                       (expect (not (.contains (pr-str result) "test-private-response")))
                       (expect (not (contains? result :outcome))))))))

(defdescribe authentication-refresh-retries-only-the-same-account-and-attempt
             (it "authentication refresh retries only the same account and attempt"
                 (let [bodies (atom [])]
                   (with-redefs [codex/get-openai-codex-token! (constantly token)
                                 codex/force-refresh-token! (fn [rejected]
                                                              (expect (= "test-access" rejected))
                                                              (assoc token :token "test-refreshed"))
                                 http/post (fn [_ opts]
                                             (swap! bodies conj (:body opts))
                                             (if (= 1 (count @bodies))
                                               {:status 401 :body "rejected"}
                                               {:status 200 :body "{\"code\":\"reset\"}"}))]

                     (expect (= {:outcome "reset"} (codex/consume-reset-credit! attempt))))
                   (expect (= 2 (count @bodies)))
                   (expect (apply = @bodies)))))
