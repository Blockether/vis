(ns com.blockether.vis.ext.provider-github-copilot-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-github-copilot :as sut]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers GitHub Copilot through the provider extension contract"
    (let [provider (vis/provider-by-id :github-copilot)]
      (expect (= :github-copilot (:provider/id provider)))
      (expect (ifn? (:provider/status-fn provider)))
      (expect (ifn? (:provider/logout-fn provider)))
      (expect (ifn? (:provider/detect-fn provider)))
      (expect (ifn? (:provider/auth-fn provider)))
      (expect (ifn? (:provider/get-token-fn provider)))))

  (it "returns Vis-owned static LLM headers with cached Copilot token"
    (reset! @#'sut/token-cache {:token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"
                                :expires-at-ms (+ (System/currentTimeMillis) 600000)
                                :api-url "https://api.individual.githubcopilot.com"})
    (let [token (sut/get-copilot-token!)]
      (expect (= "GitHubCopilotChat/0.26.7" (get-in token [:llm-headers "User-Agent"])))
      (expect (= "vscode-chat" (get-in token [:llm-headers "Copilot-Integration-Id"]))))))

(defdescribe copilot-base-url-test
  (it "derives API base URL from Copilot token proxy endpoint"
    (expect (= "https://api.individual.githubcopilot.com"
              (#'sut/copilot-base-url-from-token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"))))

  (it "falls back to individual Copilot API when token has no proxy endpoint"
    (expect (= "https://api.individual.githubcopilot.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))

  (it "uses enterprise fallback when no token endpoint is present"
    (expect (= "https://copilot-api.ghe.example.com"
              (#'sut/copilot-api-base-url "tid=x;exp=1" {} "ghe.example.com")))))
