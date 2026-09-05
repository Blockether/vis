(ns com.blockether.vis.internal.provider.vendor.anthropic-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.provider.vendor.anthropic :as anthropic]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(anthropic/register!)

(defdescribe
  provider-anthropic-test
  (it "registers separate Anthropic API-key and Claude subscription providers"
      (let [api-provider
            (vis/provider-by-id :anthropic)

            oauth-provider
            (vis/provider-by-id :anthropic-coding-plan)]

        (expect (= :anthropic (:provider/id api-provider)))
        (expect (= "Anthropic (API Key)" (:provider/label api-provider)))
        (expect (= "claude-opus-5"
                   (first (get-in api-provider [:provider/preset :default-models]))))
        (expect (contains? (set (get-in api-provider [:provider/preset :default-models]))
                           "claude-fable-5-1"))
        (expect (nil? (:provider/auth-fn api-provider)))
        (expect (= :anthropic-coding-plan (:provider/id oauth-provider)))
        (expect (= "Anthropic (Claude Subscription)" (:provider/label oauth-provider)))
        (expect (= "https://api.anthropic.com/v1"
                   (get-in oauth-provider [:provider/preset :base-url])))
        (expect (= :anthropic (get-in oauth-provider [:provider/preset :api-style])))
        (expect (contains? (set (get-in oauth-provider [:provider/preset :default-models]))
                           "claude-fable-5-1"))
        (expect (ifn? (:provider/status-fn oauth-provider)))
        (expect (ifn? (:provider/logout-fn oauth-provider)))
        (expect (ifn? (:provider/detect-fn oauth-provider)))
        (expect (ifn? (:provider/auth-fn oauth-provider)))
        (expect (ifn? (:provider/get-token-fn oauth-provider)))
        (expect (ifn? (:provider/limits-fn oauth-provider)))))
  (it
    "parses callback URL, code#state, query string, and bare code"
    (expect (= {:code "abc" :state "s1"}
               (anthropic/parse-authorization-input
                 "http://localhost:53692/callback?code=abc&state=s1")))
    (expect (= {:code "abc" :state "s1"}
               (anthropic/parse-authorization-input "localhost:53692/callback?code=abc&state=s1")))
    (expect (= {:code "abc" :state "s1"} (anthropic/parse-authorization-input "abc#s1")))
    (expect (= {:code "abc" :state "s1"} (anthropic/parse-authorization-input "code=abc&state=s1")))
    (expect (= {:code "abc"} (anthropic/parse-authorization-input "abc"))))
  (it "creates Claude subscription authorization flow with PKCE"
      (let [flow (anthropic/create-authorization-flow)]
        (expect (string? (:verifier flow)))
        (expect (= (:verifier flow) (:state flow)))
        (expect (str/starts-with? (:url flow) "https://claude.ai/oauth/authorize?"))
        (expect (str/includes? (:url flow) "code_challenge="))
        (expect (str/includes? (:url flow)
                               "redirect_uri=http%3A%2F%2Flocalhost%3A53692%2Fcallback"))
        (expect (str/includes? (:url flow) "scope=org%3Acreate_api_key"))
        (expect (str/includes? (:url flow) "user%3Asessions%3Aclaude_code"))))
  (it
    "login exchanges code and persists credentials"
    (let [saved
          (atom nil)

          lines
          (atom [])]

      (with-redefs-fn {#'http/post (fn [url opts]
                                     (reset! saved {:url url :opts opts})
                                     {:status 200
                                      :body (json/write-json-str {:access_token
                                                                  "sk-ant-oat01-access"
                                                                  :refresh_token "refresh"
                                                                  :expires_in 3600})})
                       #'anthropic/detect-credentials (constantly nil)
                       #'anthropic/save-auth-file! (fn [credentials]
                                                     (reset! saved (assoc @saved
                                                                     :credentials credentials))
                                                     credentials)}
        (fn []
          (let [result
                (anthropic/login! #(swap! lines conj %)
                                  {:open-browser-fn (constantly true)
                                   :manual-code-fn (fn [_]
                                                     "code123")})

                body
                (json/read-json (get-in @saved [:opts :body]) :key-fn keyword)]

            (expect (= :ok result))
            (expect (= "https://platform.claude.com/v1/oauth/token" (:url @saved)))
            (expect (= "authorization_code" (:grant_type body)))
            (expect (= "code123" (:code body)))
            (expect (= "sk-ant-oat01-access" (get-in @saved [:credentials :access-token])))
            (expect (some #(str/includes? % "Authenticated") @lines)))))))
  (it "login rejects state mismatch before token exchange"
      (with-redefs-fn {#'http/post (fn [& _]
                                     (throw (ex-info "should not call" {})))
                       #'anthropic/detect-credentials (constantly nil)}
        (fn []
          (expect (throws? clojure.lang.ExceptionInfo
                           #(anthropic/login! (constantly nil)
                                              {:open-browser-fn (constantly true)
                                               :manual-code-fn (fn [_]
                                                                 "code123#wrong")}))))))
  (it "reports live Claude subscription usage limits from Anthropic OAuth endpoint"
      (let [provider
            (vis/provider-by-id :anthropic-coding-plan)

            called
            (atom nil)]

        (with-redefs-fn {#'anthropic/get-anthropic-token! (fn []
                                                            {:token "sk-ant-oat01-test"})
                         #'http/get (fn [url opts]
                                      (reset! called {:url url :opts opts})
                                      {:status 200
                                       :body (json/write-json-str
                                               {:fiveHour {:utilization 72.5
                                                           :resetsAt "2026-05-07T12:00:00Z"}
                                                :sevenDay {:utilization 25}
                                                :sevenDayOpus {:utilization 10}})})}
          (fn []
            (let [report
                  ((:provider/limits-fn provider))

                  rows
                  (get-in report [:dynamic :limits])]

              (expect (= "https://api.anthropic.com/api/oauth/usage" (:url @called)))
              (expect (= "Bearer sk-ant-oat01-test"
                         (get-in @called [:opts :headers "Authorization"])))
              (expect (= :ok (:status report)))
              (expect (= [:claude-5h :claude-7d :claude-opus-7d] (mapv :id rows)))
              (expect (= 72.5 (:used (first rows))))
              (expect (= 27.5 (:remaining (first rows))))
              (expect (= 100.0 (:limit (first rows))))
              (expect (= 1778155200000 (get-in (first rows) [:window :resets-at-ms]))))))))
  (it
    "force-refreshes the rotated OAuth token when the usage endpoint answers 401, then retries once"
    (let [provider
          (vis/provider-by-id :anthropic-coding-plan)

          refreshes
          (atom [])

          tokens
          (atom [])]

      (with-redefs-fn {#'anthropic/get-anthropic-token! (fn []
                                                          {:token "stale-token"})
                       #'anthropic/force-refresh-token! (fn [rejected]
                                                          (swap! refreshes conj rejected)
                                                          {:token "rotated-token"})
                       #'http/get (fn [_url opts]
                                    (let [tok (get-in opts [:headers "Authorization"])]
                                      (swap! tokens conj tok)
                                      (if (= tok "Bearer stale-token")
                                        {:status 401 :body "{}"}
                                        {:status 200
                                         :body (json/write-json-str {:fiveHour {:utilization
                                                                                5}})})))}
        (fn []
          (let [report ((:provider/limits-fn provider))]
            (expect (= :ok (:status report)))
            ;; ONE forced refresh, carrying the rejected token so single-flight
            ;; reuse cannot hand the dead one straight back.
            (expect (= ["stale-token"] @refreshes))
            (expect (= ["Bearer stale-token" "Bearer rotated-token"] @tokens)))))))
  (it "stops at unauthenticated when the forced refresh itself fails (no retry storm)"
      (let [provider
            (vis/provider-by-id :anthropic-coding-plan)

            calls
            (atom 0)]

        (with-redefs-fn {#'anthropic/get-anthropic-token! (fn []
                                                            {:token "stale-token"})
                         #'anthropic/force-refresh-token! (fn [_rejected]
                                                            (throw (ex-info "refresh failed" {})))
                         #'http/get (fn [_url _opts]
                                      (swap! calls inc)
                                      {:status 401 :body "{}"})}
          (fn []
            (let [report ((:provider/limits-fn provider))]
              (expect (= :unauthenticated (:status report)))
              (expect (= 1 @calls)))))))
  ;; User report, provider picker: a successful OAuth exchange kept painting the
  ;; pre-login rejection, because this provider remembered its own usage verdict
  ;; underneath the host cache, where the sign-in flush could not reach it.
  (it "keeps no usage cache of its own — the host owns the budget"
      (let [provider
            (vis/provider-by-id :anthropic-coding-plan)

            calls
            (atom 0)]

        (with-redefs-fn {#'anthropic/get-anthropic-token! (fn []
                                                            {:token "sk-ant-oat01-test"})
                         #'http/get (fn [_url _opts]
                                      (swap! calls inc)
                                      {:status 200
                                       :body (json/write-json-str {:five_hour {:utilization 8}})})}
          (fn []
            ((:provider/limits-fn provider))
            ((:provider/limits-fn provider))
            (expect (= 2 @calls))
            (expect (= 300000 (:provider/limits-cache-ms provider)))))))
  (it "returns unauthenticated limits report when Claude subscription OAuth is missing"
      (let [provider (vis/provider-by-id :anthropic-coding-plan)]
        (with-redefs-fn {#'anthropic/get-anthropic-token!
                         (fn []
                           (throw (ex-info "missing" {:type :vis/anthropic-not-authenticated})))
                         #'http/get (fn [& _]
                                      (throw (ex-info "should not call" {})))}
          (fn []
            (let [report ((:provider/limits-fn provider))]
              (expect (= :unauthenticated (:status report)))
              (expect (= [] (get-in report [:dynamic :limits])))
              (expect (str/includes? (get-in report [:dynamic :note])
                                     "providers auth anthropic-coding-plan"))))))))
