(ns com.blockether.vis.ext.provider-openai-codex-test
  (:require [charred.api :as json]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [com.blockether.vis.ext.provider-openai-codex.limits :as codex-limits]
            [com.blockether.vis.internal.external-opener :as opener]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- jwt
  [payload]
  (let [->b64 (fn [s]
                (#'codex/base64url (.getBytes ^String s java.nio.charset.StandardCharsets/UTF_8)))]
    (str (->b64 (json/write-json-str {:alg "none"}))
         "."
         (->b64 (json/write-json-str payload))
         ".sig")))

(defdescribe authorization-input-test
             (it "parses full callback URLs, query strings, code#state, and bare codes"
                 (expect (= {:code "abc" :state "s1"}
                            (codex/parse-authorization-input
                              "http://localhost:1455/auth/callback?code=abc&state=s1")))
                 (expect (= {:code "abc" :state "s1"}
                            (codex/parse-authorization-input "code=abc&state=s1")))
                 (expect (= {:code "abc" :state "s1"} (codex/parse-authorization-input "abc#s1")))
                 (expect (= {:code "abc"} (codex/parse-authorization-input "abc")))))

(defdescribe account-id-test
             (it "extracts the ChatGPT account id from Codex access-token JWTs"
                 (let [token (jwt {(keyword "https://api.openai.com/auth") {:chatgpt_account_id
                                                                            "acct_123"}})]
                   (expect (= "acct_123" (codex/account-id token))))))

(defdescribe browser-open-test
             (it "uses Vis's shared external opener"
                 (let [seen (atom nil)]
                   (with-redefs [opener/open! (fn [url]
                                                (reset! seen url)
                                                {:status :ok :target url})]
                     (expect (= true (#'codex/open-browser! "https://auth.openai.com/x")))
                     (expect (= "https://auth.openai.com/x" @seen)))))
             (it "surfaces opener failure as false"
                 (with-redefs [opener/open! (constantly {:status :spawn-failed :error "nope"})]
                   (expect (= false (#'codex/open-browser! "https://auth.openai.com/x"))))))

(defdescribe
  login-flow-test
  (it "fails fast when manual redirect collection is disabled"
      (with-redefs-fn {#'codex/detect-credentials (constantly nil)
                       #'codex/create-authorization-flow
                       (fn [_]
                         {:verifier "verifier" :state "state" :url "https://auth.openai.com/x"})
                       #'codex/open-browser! (constantly true)}
        (fn []
          (try (codex/login! (constantly nil) {:originator "vis-tui" :manual-code-fn nil})
               (expect false)
               (catch Exception e
                 (expect (= :vis/openai-codex-manual-entry-disabled
                            (-> e
                                ex-data
                                :type))))))))
  (it "accepts a pasted redirect URL and persists exchanged credentials"
      (let [saved (atom nil)]
        (with-redefs-fn {#'codex/detect-credentials (constantly nil)
                         #'codex/create-authorization-flow
                         (fn [_]
                           {:verifier "verifier" :state "state" :url "https://auth.openai.com/x"})
                         #'codex/open-browser! (constantly true)
                         #'codex/exchange-authorization-code! (fn [code verifier]
                                                                (expect (= "abc" code))
                                                                (expect (= "verifier" verifier))
                                                                {:access-token "tok"
                                                                 :refresh-token "ref"
                                                                 :account-id "acct_123"
                                                                 :expires-at-ms 42})
                         #'codex/save-auth-file! (fn [creds]
                                                   (reset! saved creds)
                                                   creds)}
          (fn []
            (expect (= :ok
                       (codex/login!
                         (constantly nil)
                         {:originator "vis-tui"
                          :manual-code-fn
                          (fn [_]
                            "http://localhost:1455/auth/callback?code=abc&state=state")})))
            (expect (= "acct_123" (:account-id @saved)))))))
  (it "starts a fresh OAuth flow when force is true and credentials already exist"
      (let [saved (atom nil)]
        (with-redefs-fn {#'codex/detect-credentials (constantly {:account-id "acct_old"})
                         #'codex/create-authorization-flow
                         (fn [_]
                           {:verifier "verifier" :state "state" :url "https://auth.openai.com/x"})
                         #'codex/open-browser! (constantly true)
                         #'codex/exchange-authorization-code! (fn [code verifier]
                                                                (expect (= "abc" code))
                                                                (expect (= "verifier" verifier))
                                                                {:access-token "tok"
                                                                 :refresh-token "ref"
                                                                 :account-id "acct_new"
                                                                 :expires-at-ms 42})
                         #'codex/save-auth-file! (fn [creds]
                                                   (reset! saved creds)
                                                   creds)}
          (fn []
            (expect (= :ok
                       (codex/login!
                         (constantly nil)
                         {:originator "vis-tui"
                          :force? true
                          :manual-code-fn
                          (fn [_]
                            "http://localhost:1455/auth/callback?code=abc&state=state")})))
            (expect (= "acct_new" (:account-id @saved))))))))

(defdescribe
  codex-token-test
  (it "returns the header shape svar's native Responses transport needs"
      (let [token (jwt {(keyword "https://api.openai.com/auth") {:chatgpt_account_id "acct_123"}})]
        (with-redefs-fn {#'codex/load-auth-file
                         (constantly {:access-token token
                                      :expires-at-ms (+ (System/currentTimeMillis) 600000)})}
          (fn []
            (expect (= {:token token
                        :api-url "https://chatgpt.com/backend-api"
                        :llm-headers {"chatgpt-account-id" "acct_123"}}
                       (codex/get-openai-codex-token!)))))))
  (it "forces refresh with the rejected access token so token cycling cannot reuse it"
      (let [seen (atom ::unset)]
        (with-redefs-fn {#'codex/refresh-and-persist! (fn [rejected-token]
                                                        (reset! seen rejected-token)
                                                        {:token "fresh"
                                                         :api-url "https://chatgpt.com/backend-api"
                                                         :llm-headers {"chatgpt-account-id"
                                                                       "acct_123"}})}
          (fn []
            (expect (= {:token "fresh"
                        :api-url "https://chatgpt.com/backend-api"
                        :llm-headers {"chatgpt-account-id" "acct_123"}}
                       (codex/force-refresh-token! "dead")))
            (expect (= "dead" @seen)))))))

(defdescribe
  codex-limits-test
  (it "reports dynamic usage windows when Codex credentials exist"
      (with-redefs-fn {#'codex/detect-credentials (constantly {:access-token "stale"
                                                               :account-id "acct_123"})
                       #'codex/get-openai-codex-token!
                       (constantly {:token "tok" :llm-headers {"chatgpt-account-id" "acct_123"}})
                       #'codex-limits/dynamic-limits! (fn [token account-id]
                                                        (expect (= "tok" token))
                                                        (expect (= "acct_123" account-id))
                                                        {:limits [{:id :codex-5h
                                                                   :label "Codex 5h quota (%)"
                                                                   :scope :account
                                                                   :kind :rate
                                                                   :precision :exact
                                                                   :source :provider-api
                                                                   :unlimited? false
                                                                   :used 20.0
                                                                   :limit 100.0
                                                                   :remaining 80.0}]})}
        (fn []
          (let [report (codex/limits)]
            (expect (= :openai-codex (:provider-id report)))
            (expect (= :ok (:status report)))
            (expect (= 80.0 (get-in report [:dynamic :limits 0 :remaining])))))))
  (it "reports :unauthenticated when Codex credentials are absent"
      (with-redefs-fn {#'codex/detect-credentials (constantly nil)}
        (fn []
          (let [report (codex/limits)]
            (expect (= :openai-codex (:provider-id report)))
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits])))))))
  (it "reports :error when the usage endpoint fails"
      (with-redefs-fn {#'codex/detect-credentials (constantly {:access-token "stale"
                                                               :account-id "acct_123"})
                       #'codex/get-openai-codex-token!
                       (constantly {:token "tok" :llm-headers {"chatgpt-account-id" "acct_123"}})
                       #'codex-limits/dynamic-limits! (fn [& _]
                                                        (throw (ex-info "boom" {})))}
        (fn []
          (let [report (codex/limits)]
            (expect (= :error (:status report)))
            (expect (= :provider/openai-codex-usage-error (get-in report [:error :type])))
            (expect (= [] (get-in report [:dynamic :limits]))))))))

(defdescribe provider-registration-test
             (it "registers the OpenAI Codex auth provider"
                 (let [provider (vis/provider-by-id :openai-codex)]
                   (expect (= :openai-codex (:provider/id provider)))
                   (expect (= "OpenAI Codex (ChatGPT OAuth)" (:provider/label provider)))
                   (expect (ifn? (:provider/get-token-fn provider)))
                   (expect (ifn? (:provider/limits-fn provider)))
                   (expect (contains? (set (get-in provider [:provider/preset :default-models]))
                                      "gpt-5.6-sol")))))

(defdescribe codex-extension-settings-test
             (it "does not declare legacy TUI settings metadata"
                 (let [ext (first (filter #(= "provider-openai-codex" (:ext/name %))
                                          (vis/registered-extensions)))]
                   (expect (= [] (:ext/settings ext))))))
