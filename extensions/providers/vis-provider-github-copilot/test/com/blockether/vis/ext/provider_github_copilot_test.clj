(ns com.blockether.vis.ext.provider-github-copilot-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-github-copilot :as sut]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers business and individual GitHub Copilot providers as separate extension entries"
    (require 'com.blockether.vis.ext.provider-github-copilot :reload)
    (let [business   (vis/provider-by-id :github-copilot-business)
          individual (vis/provider-by-id :github-copilot-individual)
          ext-nses   (set (map :ext/name (vis/registered-extensions)))]
      (expect (= :github-copilot-business (:provider/id business)))
      (expect (= :github-copilot-individual (:provider/id individual)))
      (expect (contains? ext-nses "provider-github-copilot"))
      (expect (= "https://api.business.githubcopilot.com" (get-in business [:provider/preset :base-url])))
      (expect (= "https://api.individual.githubcopilot.com" (get-in individual [:provider/preset :base-url])))
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
                                :api-url "https://api.individual.githubcopilot.com"})
    (let [token (sut/get-copilot-token!)]
      (expect (= "https://api.individual.githubcopilot.com" (:api-url token)))
      (expect (= "GitHubCopilotChat/0.26.7" (get-in token [:llm-headers "User-Agent"])))
      (expect (= "vscode-chat" (get-in token [:llm-headers "Copilot-Integration-Id"]))))))

(defdescribe copilot-base-url-test
  (it "derives API base URL from Copilot token proxy endpoint"
    (expect (= "https://proxy.individual.githubcopilot.com"
              (#'sut/copilot-base-url-from-token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"))))

  (it "ignores token proxy endpoints for chat and uses the account API host"
    (expect (= "https://api.business.githubcopilot.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {:proxy-ep "proxy.business.githubcopilot.com"} nil
                                          {:account-type :business}))))

  (it "falls back to selected business Copilot API when token has no endpoint"
    (expect (= "https://api.business.githubcopilot.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil {:account-type :business}))))

  (it "falls back to individual Copilot API by default"
    (expect (= "https://api.individual.githubcopilot.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))

  (it "uses enterprise fallback when no token endpoint is present"
    (expect (= "https://copilot-api.ghe.example.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {} "ghe.example.com")))))

(defdescribe copilot-limits-test
  (it "normalizes Copilot quota snapshots"
    (with-redefs [sut/detect-oauth-token (fn [] {:oauth-token "ghu_test"})
                  sut/fetch-user-usage! (fn [_]
                                          {:copilot_plan "business"
                                           :quota_reset_date "2026-05-30T00:00:00Z"
                                           :quota_snapshots {:premium_interactions {:remaining 240
                                                                                    :entitlement 300
                                                                                    :percent_remaining 80}}})]
      (let [report (#'sut/dynamic-limits!)
            row (first (get-in report [:dynamic :limits]))]
        (expect (= :ok (:status report)))
        (expect (= :premium_interactions (:id row)))
        (expect (= 240.0 (:remaining row)))
        (expect (= 300.0 (:limit row)))
        (expect (= 60.0 (:used row)))))))
