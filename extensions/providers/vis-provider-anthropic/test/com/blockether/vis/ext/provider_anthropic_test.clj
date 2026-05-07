(ns com.blockether.vis.ext.provider-anthropic-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-anthropic :as anthropic]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe provider-anthropic-test
  (it "registers separate Anthropic API-key and Claude subscription providers"
    (let [api-provider   (vis/provider-by-id :anthropic)
          oauth-provider (vis/provider-by-id :anthropic-coding-plan)]
      (expect (= :anthropic (:provider/id api-provider)))
      (expect (= "Anthropic (API Key)" (:provider/label api-provider)))
      (expect (= "claude-opus-4-6" (first (get-in api-provider [:provider/preset :default-models]))))
      (expect (nil? (:provider/auth-fn api-provider)))
      (expect (= :anthropic-coding-plan (:provider/id oauth-provider)))
      (expect (= "Anthropic (Claude Subscription)" (:provider/label oauth-provider)))
      (expect (= "https://api.anthropic.com/v1" (get-in oauth-provider [:provider/preset :base-url])))
      (expect (= :anthropic (get-in oauth-provider [:provider/preset :api-style])))
      (expect (ifn? (:provider/status-fn oauth-provider)))
      (expect (ifn? (:provider/logout-fn oauth-provider)))
      (expect (ifn? (:provider/detect-fn oauth-provider)))
      (expect (ifn? (:provider/auth-fn oauth-provider)))
      (expect (ifn? (:provider/get-token-fn oauth-provider)))))

  (it "parses callback URL, code#state, query string, and bare code"
    (expect (= {:code "abc" :state "s1"}
              (anthropic/parse-authorization-input "https://console.anthropic.com/oauth/code/callback?code=abc&state=s1")))
    (expect (= {:code "abc" :state "s1"}
              (anthropic/parse-authorization-input "abc#s1")))
    (expect (= {:code "abc" :state "s1"}
              (anthropic/parse-authorization-input "code=abc&state=s1")))
    (expect (= {:code "abc"}
              (anthropic/parse-authorization-input "abc"))))

  (it "creates Claude subscription authorization flow with PKCE"
    (let [flow (anthropic/create-authorization-flow)]
      (expect (string? (:verifier flow)))
      (expect (= (:verifier flow) (:state flow)))
      (expect (str/starts-with? (:url flow) "https://claude.ai/oauth/authorize?"))
      (expect (str/includes? (:url flow) "code_challenge="))
      (expect (str/includes? (:url flow) "scope=org%3Acreate_api_key"))))

  (it "login exchanges code and persists credentials"
    (let [saved (atom nil)
          lines (atom [])]
      (with-redefs-fn {#'http/post (fn [url opts]
                                     (reset! saved {:url url :opts opts})
                                     {:status 200
                                      :body (json/write-json-str {:access_token "sk-ant-oat01-access"
                                                                  :refresh_token "refresh"
                                                                  :expires_in 3600})})
                       #'anthropic/detect-credentials (constantly nil)
                       #'anthropic/save-auth-file! (fn [credentials]
                                                     (reset! saved (assoc @saved :credentials credentials))
                                                     credentials)}
        (fn []
          (let [result (anthropic/login! #(swap! lines conj %)
                         {:open-browser-fn (constantly true)
                          :manual-code-fn  (fn [_] "code123")})
                body (json/read-json (get-in @saved [:opts :body]) :key-fn keyword)]
            (expect (= :ok result))
            (expect (= "https://console.anthropic.com/v1/oauth/token" (:url @saved)))
            (expect (= "authorization_code" (:grant_type body)))
            (expect (= "code123" (:code body)))
            (expect (= "sk-ant-oat01-access" (get-in @saved [:credentials :access-token])))
            (expect (some #(str/includes? % "Authenticated") @lines)))))))

  (it "login rejects state mismatch before token exchange"
    (with-redefs-fn {#'http/post (fn [& _] (throw (ex-info "should not call" {})))
                     #'anthropic/detect-credentials (constantly nil)}
      (fn []
        (expect (throws? clojure.lang.ExceptionInfo
                  #(anthropic/login! (constantly nil)
                     {:open-browser-fn (constantly true)
                      :manual-code-fn  (fn [_] "code123#wrong")})))))))
