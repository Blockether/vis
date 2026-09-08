(ns com.blockether.vis.internal.gateway.provider-reset-test
  "Reset allowance and consumption across the real HTTP router and Clojure client.
   Only Codex credentials and upstream HTTP are fixtures; no real credits are spent."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.contract.provider :as contract-provider]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.provider-auth-test :as auth-test]
            [com.blockether.vis.internal.provider.limits :as limits]
            [com.blockether.vis.internal.provider.vendor.openai-codex :as codex]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- with-codex-backend
  [f]
  (let [descriptor
        (atom nil)

        remaining
        (atom 2)

        redeemed
        (atom #{})

        calls
        (atom [])

        lose-response?
        (atom false)

        account
        (atom "test-account")]

    (with-redefs [vis/register-extension! #(reset! descriptor (first (:ext/providers %)))]
      (codex/register!))
    (limits/flush-limits-cache! :openai-codex)
    (try
      (with-redefs [registry/provider-by-id
                    #(when (= % :openai-codex) @descriptor)

                    codex/detect-credentials
                    #(hash-map :account-id @account :access-token "test-access")

                    codex/get-openai-codex-token!
                    #(hash-map :token "test-access" :llm-headers {"chatgpt-account-id" @account})

                    http/get
                    (fn [url _]
                      (is (= "https://chatgpt.com/backend-api/wham/usage" url))
                      {:status 200
                       :body (json/write-json-str {:rate_limit_reset_credits {:available_count
                                                                              @remaining}})})

                    http/post
                    (fn [url opts]
                      (is (= "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits/consume"
                             url))
                      (let [key
                            (get (json/read-json (:body opts)) "redeem_request_id")

                            code
                            (if (contains? @redeemed key)
                              "already_redeemed"
                              (if (pos? @remaining)
                                (do (swap! redeemed conj key) (swap! remaining dec) "reset")
                                "no_credit"))]

                        (swap! calls conj key)
                        (if (compare-and-set! lose-response? true false)
                          (throw (java.io.IOException. "test-private-upstream-response"))
                          {:status 200 :body (json/write-json-str {:code code})})))]

        (#'auth-test/with-test-gateway
         #(f {:remaining remaining :calls calls :account account :lose-response? lose-response?})))
      (finally (limits/flush-limits-cache! :openai-codex)))))

(defn- request-reset
  [body]
  (let [response (client/request! :post
                                  "/v1/providers/openai-codex/reset-credits/consume"
                                  {:body body :timeout-ms 3000})]
    {:status (:status response) :json (wire/parse-json (:body response))}))

(deftest read-confirm-consume-refresh-and-lost-response-retry
  (with-codex-backend
    (fn [{:keys [remaining calls lose-response?]}]
      (let [report
            (client/provider-limits :openai-codex)

            key
            "a11fd916-28a8-4b38-842d-37cdb463d552"]

        (is (contract-provider/report-valid? report))
        (is (= {:status :ok :available-count 2 :account-id "test-account"}
               (get-in report [:dynamic :reset-credits])))
        (is (empty? @calls) "reading never consumes a reset")
        (reset! lose-response? true)
        (let [failed (request-reset {:account_id "test-account" :idempotency_key key})]
          (is (= 502 (:status failed)))
          (is (not (.contains (pr-str failed) "test-private-upstream-response"))))
        (is (= 1 @remaining))
        (is (= {:outcome "already_redeemed"}
               (client/consume-provider-reset-credit! :openai-codex "test-account" key)))
        (is (= [key key] @calls))
        (is (= 1 @remaining))
        (is (= 1
               (get-in (client/provider-limits :openai-codex)
                       [:dynamic :reset-credits :available-count])))
        (is (= {:outcome "reset"}
               (client/consume-provider-reset-credit! :openai-codex
                                                      "test-account"
                                                      "5df9a275-fde7-435e-977d-40b3a9a20de9")))
        (is (zero? (get-in (client/provider-limits :openai-codex)
                           [:dynamic :reset-credits :available-count])))))))

(deftest malformed-requests-and-account-switches-never-reach-codex
  (with-codex-backend
    (fn [{:keys [calls account]}]
      (doseq [body [{} {:account_id "test-account" :idempotency_key ""}
                    {:account_id [] :idempotency_key "attempt"}]]
        (is (= 400 (:status (request-reset body)))))
      (reset! account "other-account")
      (is (= 409 (:status (request-reset {:account_id "test-account" :idempotency_key "attempt"}))))
      (is (empty? @calls)))))

(deftest reset-summary-and-callback-contracts
  (let [base
        {:provider-id :openai-codex :status :ok :fetched-at-ms 1 :static {} :dynamic {:limits []}}

        descriptor
        {:provider/id :example :provider/label "Example"}]

    (doseq [credits [{:status :ok :account-id "account"}
                     {:status :ok :account-id "account" :available-count -1}
                     {:status :error :available-count 0}]]
      (is (not (contract-provider/report-valid?
                 (assoc-in base [:dynamic :reset-credits] credits)))))
    (is (registry/provider? (assoc descriptor :provider/consume-reset-credit-fn identity)))
    (is (not (registry/provider? (assoc descriptor :provider/consume-reset-credit-fn 1))))
    (is (not (#'extension/provider-entry?
              (assoc descriptor :provider/consume-reset-credit-fn 1))))))
