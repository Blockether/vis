(ns com.blockether.vis.internal.provider.vendor.openai-codex-reset-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.internal.provider.vendor.openai-codex :as codex]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

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
                  (is (= "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits" url))
                  (is (= "Bearer test-access" (get-in opts [:headers "Authorization"])))
                  (is (= "test-account" (get-in opts [:headers "chatgpt-account-id"])))
                  response)]

    (get-in (codex/limits) [:dynamic :reset-credits])))

(deftest reset-availability-is-account-scoped-and-never-inferred-from-quota
  (is (= {:status :ok :available-count 2 :account-id "test-account"}
         (report {:rate_limit_reset_credits {:available_count 2}} nil)))
  (is (= {:status :ok :available-count 0 :account-id "test-account"}
         (report {} {:status 200 :body "{\"available_count\":0}"})))
  (testing "missing or malformed data is not zero credits"
    (doseq [response [{:status 404 :body "not supported"}
                      {:status 500 :body "test-private-response"}
                      {:status 200 :body "{\"available_count\":-1}"}
                      {:status 200 :body "{\"available_count\":null}"}]]
      (let [result (report {} response)]
        (is (contains? #{:unsupported :error} (:status result)))
        (is (not (contains? result :available-count)))
        (is (not (.contains (pr-str result) "test-private-response")))))))

(deftest consume-uses-the-confirmed-account-and-idempotency-key
  (doseq [outcome ["reset" "nothing_to_reset" "no_credit" "already_redeemed"]]
    (with-redefs [codex/get-openai-codex-token! (constantly token)
                  http/post
                  (fn [url opts]
                    (is (= "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits/consume"
                           url))
                    (is (= {"redeem_request_id" (:idempotency-key attempt)}
                           (json/read-json (:body opts))))
                    (is (= "test-account" (get-in opts [:headers "chatgpt-account-id"])))
                    (is (false? (:throw opts)))
                    {:status 200 :body (json/write-json-str {:code outcome})})]

      (is (= {:outcome outcome} (codex/consume-reset-credit! attempt))))))

(deftest account-switch-refuses-to-spend-a-different-accounts-credit
  (let [posts (atom 0)]
    (with-redefs [codex/get-openai-codex-token!
                  (constantly (assoc-in token [:llm-headers "chatgpt-account-id"] "other-account"))
                  http/post (fn [& _]
                              (swap! posts inc))]

      (is (= :account-changed (:error (codex/consume-reset-credit! attempt)))))
    (is (zero? @posts))))

(deftest reset-failures-are-sanitized-and-unknown-outcomes-stay-unknown
  (doseq [response [{:status 503 :body "test-private-response"}
                    {:status 200 :body "not-json test-private-response"}
                    {:status 200 :body "{\"code\":\"future_outcome\"}"}]]
    (with-redefs [codex/get-openai-codex-token! (constantly token)
                  http/post (fn [& _]
                              response)]

      (let [result (codex/consume-reset-credit! attempt)]
        (is (= :reset-unconfirmed (:error result)))
        (is (not (.contains (pr-str result) "test-private-response")))
        (is (not (contains? result :outcome)))))))

(deftest authentication-refresh-retries-only-the-same-account-and-attempt
  (let [bodies (atom [])]
    (with-redefs [codex/get-openai-codex-token! (constantly token)
                  codex/force-refresh-token! (fn [rejected]
                                               (is (= "test-access" rejected))
                                               (assoc token :token "test-refreshed"))
                  http/post (fn [_ opts]
                              (swap! bodies conj (:body opts))
                              (if (= 1 (count @bodies))
                                {:status 401 :body "rejected"}
                                {:status 200 :body "{\"code\":\"reset\"}"}))]

      (is (= {:outcome "reset"} (codex/consume-reset-credit! attempt))))
    (is (= 2 (count @bodies)))
    (is (apply = @bodies))))
