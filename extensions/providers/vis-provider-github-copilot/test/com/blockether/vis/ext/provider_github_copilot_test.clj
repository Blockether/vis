(ns com.blockether.vis.ext.provider-github-copilot-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-github-copilot :as sut]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  provider-registration-test
  (it
    "registers ONE transparent provider per Copilot account (no per-wire sub-providers)"
    (require 'com.blockether.vis.ext.provider-github-copilot :reload)
    (let [business
          (vis/provider-by-id :github-copilot-business)

          individual
          (vis/provider-by-id :github-copilot-individual)

          enterprise
          (vis/provider-by-id :github-copilot-enterprise)

          ext-nses
          (set (map :ext/name (vis/registered-extensions)))

          models
          (set (get-in individual [:provider/preset :default-models]))]

      (expect (= :github-copilot-business (:provider/id business)))
      (expect (= :github-copilot-individual (:provider/id individual)))
      (expect (= :github-copilot-enterprise (:provider/id enterprise)))
      (expect (contains? ext-nses "provider-github-copilot"))
      ;; One entry per account — the old `…-responses` / `…-chat` per-wire
      ;; sub-providers are gone; one base-url `/v1` carries both wires.
      (expect (nil? (vis/provider-by-id :github-copilot-individual-responses)))
      (expect (nil? (vis/provider-by-id :github-copilot-individual-chat)))
      (expect (= "https://api.business.githubcopilot.com/v1"
                 (get-in business [:provider/preset :base-url])))
      (expect (= "https://api.individual.githubcopilot.com/v1"
                 (get-in individual [:provider/preset :base-url])))
      (expect (= "https://api.business.githubcopilot.com/v1"
                 (get-in enterprise [:provider/preset :base-url])))
      (expect (= "/responses" (get-in individual [:provider/preset :responses-path])))
      ;; Catalog carries both cacheable wires; Gemini/Grok (chat-only) dropped.
      (expect (contains? models "claude-opus-4.8"))
      (expect (contains? models "claude-fable-5"))
      (expect (contains? models "gpt-5.4"))
      ;; Enterprise serves the SAME curated Claude catalog (dotted models.dev
      ;; ids → native Anthropic /v1/messages wire) so Copilot Enterprise users
      ;; can select Opus/Sonnet/Haiku.
      (expect (contains? (set (get-in enterprise [:provider/preset :default-models]))
                         "claude-opus-4.8"))
      (expect (contains? (set (get-in enterprise [:provider/preset :default-models]))
                         "claude-fable-5"))
      (expect (contains? (set (get-in enterprise [:provider/preset :default-models]))
                         "claude-sonnet-4.6"))
      (expect (contains? (set (get-in enterprise [:provider/preset :default-models]))
                         "claude-haiku-4.5"))
      (expect (not-any? #(re-find #"(?i)gemini|grok" %) models))
      (expect (ifn? (:provider/status-fn business)))
      (expect (ifn? (:provider/logout-fn business)))
      (expect (ifn? (:provider/detect-fn business)))
      (expect (ifn? (:provider/auth-fn business)))
      (expect (ifn? (:provider/get-token-fn business)))
      (expect (ifn? (:provider/limits-fn business)))))
  (it "returns Vis-owned static LLM headers with cached Copilot token"
      (reset! @#'sut/token-cache {:token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"
                                  :expires-at-ms (+ (System/currentTimeMillis) 600000)
                                  :account-type :individual
                                  :api-url "https://api.individual.githubcopilot.com/v1"})
      (let [token (sut/get-copilot-token!)]
        (expect (= "https://api.individual.githubcopilot.com/v1" (:api-url token)))
        (expect (= "GitHubCopilotChat/0.26.7" (get-in token [:llm-headers "User-Agent"])))
        (expect (= "vscode-chat" (get-in token [:llm-headers "Copilot-Integration-Id"]))))))
(it "honors refresh_in: a token past its proactive-refresh deadline is NOT re-served (issue #16)"
    ;; refresh_in expired but hard expires_at far in the future. The old code
    ;; trusted expires_at and re-served the proxy-rejected token forever (401
    ;; "IDE token expired" storm). cached-token-usable? must now say unusable so
    ;; get-copilot-token! re-mints instead of looping.
    (let [usable?
          @#'sut/cached-token-usable?

          now
          (System/currentTimeMillis)]

      (expect (false? (usable? {:token "t"
                                :account-type :individual
                                :expires-at-ms (+ now 1500000)
                                :refresh-at-ms (- now 1000)}
                               :individual
                               now)))
      ;; before refresh_in elapses the token is still served (no needless churn)
      (expect (true? (usable? {:token "t"
                               :account-type :individual
                               :expires-at-ms (+ now 1800000)
                               :refresh-at-ms (+ now 1200000)}
                              :individual
                              now)))
      ;; legacy cache with no :refresh-at-ms falls back to expires - margin
      (expect (true? (usable? {:token "t" :account-type :individual :expires-at-ms (+ now 1800000)}
                              :individual
                              now)))))

(defdescribe
  copilot-base-url-test
  (it "derives API base URL from Copilot token proxy endpoint"
      (expect (= "https://proxy.individual.githubcopilot.com"
                 (#'sut/copilot-base-url-from-token
                  "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"))))
  (it "ignores token proxy endpoints for chat and uses the account API host"
      (expect (= "https://api.business.githubcopilot.com"
                 (#'sut/copilot-api-base-url
                  "tid=x;exp=1"
                  {:proxy-ep "proxy.business.githubcopilot.com"}
                  nil
                  {:account-type :business}))))
  (it "falls back to selected business Copilot API when token has no endpoint"
      (expect (= "https://api.business.githubcopilot.com"
                 (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil {:account-type :business}))))
  (it "falls back to individual Copilot API by default"
      (expect (= "https://api.individual.githubcopilot.com"
                 (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))
  (it "uses business API fallback for Enterprise Cloud when no token endpoint is present"
      (expect (= "https://api.business.githubcopilot.com"
                 (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil {:account-type :enterprise}))))
  (it "uses GHE enterprise fallback when an enterprise domain is configured"
      (expect (= "https://copilot-api.ghe.example.com"
                 (#'sut/copilot-api-base-url "tid=x;exp=1" {} "ghe.example.com"))))
  (it "ensure-api-version appends /v1 to a bare host and is idempotent"
      (expect (= "https://api.business.githubcopilot.com/v1"
                 (#'sut/ensure-api-version "https://api.business.githubcopilot.com")))
      (expect (= "https://api.business.githubcopilot.com/v1"
                 (#'sut/ensure-api-version "https://api.business.githubcopilot.com/v1")))
      (expect (= "https://api.business.githubcopilot.com/v1"
                 (#'sut/ensure-api-version "https://api.business.githubcopilot.com/")))
      (expect (nil? (#'sut/ensure-api-version nil)))))

(defdescribe copilot-limits-test
             (it "normalizes Copilot quota snapshots"
                 (with-redefs [sut/detect-oauth-token
                               (fn []
                                 {:oauth-token "ghu_test"})

                               sut/fetch-user-usage!
                               (fn [_]
                                 {:copilot_plan "business"
                                  :quota_reset_date "2026-05-30T00:00:00Z"
                                  :quota_snapshots
                                  {:premium_interactions
                                   {:remaining 240 :entitlement 300 :percent_remaining 80}}})]

                   (let [report
                         (#'sut/dynamic-limits!)

                         row
                         (first (get-in report [:dynamic :limits]))]

                     (expect (= :ok (:status report)))
                     (expect (= :premium_interactions (:id row)))
                     (expect (= 240.0 (:remaining row)))
                     (expect (= 300.0 (:limit row)))
                     (expect (= 60.0 (:used row)))))))

(defdescribe
  copilot-refresh-margin-test
  (it "subtracts REFRESH_MARGIN_MS from the refresh_in soft deadline on mint (issue #21)"
      ;; GitHub's refresh_in (soft, proxy-reject) is shorter than expires_at (hard).
      ;; The mint must refresh a FULL margin BEFORE the soft deadline, never right
      ;; up to it — otherwise clock skew / round-trip lands us past the soft reject
      ;; ("IDE token expired") and the 401 recovery loop storms.
      (let [now
            (System/currentTimeMillis)

            refresh-in-s
            1500

            margin
            (* 5 60 1000)

            soft
            (+ now (* refresh-in-s 1000))]

        (with-redefs [sut/get-json
                      (fn [_ _]
                        {:token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"
                         :expires_at (long (/ (+ now 1800000) 1000))
                         :refresh_in refresh-in-s})

                      sut/copilot-llm-base-url
                      (fn [& _]
                        "https://api.individual.githubcopilot.com/v1")]

          (let [{:keys [refresh-at-ms expires-at-ms]} (#'sut/exchange-for-copilot-token!
                                                       "oauth-tok")]
            ;; refresh-at-ms == (min hard soft) - margin, and soft < hard here
            (expect (< (Math/abs (- (long refresh-at-ms) (- soft margin))) 2000))
            ;; always strictly before the hard expiry too
            (expect (< (long refresh-at-ms) (long expires-at-ms))))))))
