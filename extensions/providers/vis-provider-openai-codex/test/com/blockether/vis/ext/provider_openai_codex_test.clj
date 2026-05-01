(ns com.blockether.vis.ext.provider-openai-codex-test
  (:require [charred.api :as json]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [com.blockether.vis.internal.external-opener :as opener]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util Base64]))

(defn- base64url [s]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
    (.getBytes ^String s java.nio.charset.StandardCharsets/UTF_8)))

(defn- jwt [payload]
  (str (base64url (json/write-json-str {:alg "none"}))
    "."
    (base64url (json/write-json-str payload))
    ".sig"))

(defdescribe authorization-input-test
  (it "parses full callback URLs, query strings, code#state, and bare codes"
    (expect (= {:code "abc" :state "s1"}
              (codex/parse-authorization-input
                "http://localhost:1455/auth/callback?code=abc&state=s1")))
    (expect (= {:code "abc" :state "s1"}
              (codex/parse-authorization-input "code=abc&state=s1")))
    (expect (= {:code "abc" :state "s1"}
              (codex/parse-authorization-input "abc#s1")))
    (expect (= {:code "abc"}
              (codex/parse-authorization-input "abc")))))

(defdescribe account-id-test
  (it "extracts the ChatGPT account id from Codex access-token JWTs"
    (let [token (jwt {(keyword "https://api.openai.com/auth")
                      {:chatgpt_account_id "acct_123"}})]
      (expect (= "acct_123" (codex/account-id token))))))

(defdescribe browser-open-test
  (it "uses Vis's shared external opener instead of java.awt Desktop"
    (let [seen (atom nil)]
      (with-redefs [opener/open! (fn [url]
                                   (reset! seen url)
                                   {:status :ok :target url})]
        (expect (= true (#'codex/open-browser! "https://auth.openai.com/x")))
        (expect (= "https://auth.openai.com/x" @seen)))))

  (it "surfaces opener failure as false"
    (with-redefs [opener/open! (constantly {:status :spawn-failed :error "nope"})]
      (expect (= false (#'codex/open-browser! "https://auth.openai.com/x"))))))

(defdescribe login-flow-test
  (it "fails fast when manual redirect collection is disabled"
    (with-redefs-fn {#'codex/detect-credentials        (constantly nil)
                     #'codex/create-authorization-flow (fn [_]
                                                         {:verifier "verifier"
                                                          :state    "state"
                                                          :url      "https://auth.openai.com/x"})
                     #'codex/open-browser!            (constantly true)}
      (fn []
        (try
          (codex/login! (constantly nil)
            {:originator "vis-tui"
             :manual-code-fn nil})
          (expect false)
          (catch Exception e
            (expect (= :vis/openai-codex-manual-entry-disabled
                      (-> e ex-data :type))))))))

  (it "accepts a pasted redirect URL and persists exchanged credentials"
    (let [saved (atom nil)]
      (with-redefs-fn {#'codex/detect-credentials        (constantly nil)
                       #'codex/create-authorization-flow (fn [_]
                                                           {:verifier "verifier"
                                                            :state    "state"
                                                            :url      "https://auth.openai.com/x"})
                       #'codex/open-browser!            (constantly true)
                       #'codex/exchange-authorization-code! (fn [code verifier]
                                                              (expect (= "abc" code))
                                                              (expect (= "verifier" verifier))
                                                              {:access-token  "tok"
                                                               :refresh-token "ref"
                                                               :account-id    "acct_123"
                                                               :expires-at-ms 42})
                       #'codex/save-auth-file!          (fn [creds]
                                                          (reset! saved creds)
                                                          creds)}
        (fn []
          (expect (= :ok
                    (codex/login! (constantly nil)
                      {:originator     "vis-tui"
                       :manual-code-fn (fn [_]
                                         "http://localhost:1455/auth/callback?code=abc&state=state")})))
          (expect (= "acct_123" (:account-id @saved))))))))

(defdescribe codex-transport-test
  (it "resolves the Responses endpoint even when svar passes a chat URL"
    (expect (= "https://chatgpt.com/backend-api/codex/responses"
              (#'codex/codex-responses-url "https://chatgpt.com/backend-api/chat/completions")))
    (expect (= "https://chatgpt.com/backend-api/codex/responses"
              (#'codex/codex-responses-url "https://chatgpt.com/backend-api/codex"))))

  (it "builds a Codex Responses request body with required instructions"
    (let [body (#'codex/codex-request-body [{:role "user" :content "2+2"}]
                                           "gpt-5.5"
                                           {})]
      (expect (= "gpt-5.5" (:model body)))
      (expect (= "You are a helpful assistant." (:instructions body)))
      (expect (= "low" (get-in body [:text :verbosity])))
      (expect (= [{:role "user"
                   :content [{:type "input_text" :text "2+2"}]}]
                (:input body)))))

  (it "honors caller-specified Codex verbosity"
    (let [body (#'codex/codex-request-body [{:role "user" :content "2+2"}]
                                           "gpt-5.5"
                                           {:text {:verbosity :high}})]
      (expect (= "high" (get-in body [:text :verbosity])))))

  (it "extracts reasoning from response.completed when no streaming reasoning delta arrived"
    (let [content            (StringBuilder.)
          reasoning          (StringBuilder.)
          usage              (volatile! nil)
          completed-response (volatile! nil)
          chunks             (atom [])
          response           {:output [{:type "reasoning"
                                        :summary [{:type "summary_text"
                                                   :text "Need to inspect the TUI rendering path."}]}
                                       {:type "message"
                                        :content [{:type "output_text"
                                                   :text "(answer :ok)"}]}]
                              :usage {:input_tokens 11
                                      :output_tokens 17
                                      :total_tokens 28
                                      :output_tokens_details {:reasoning_tokens 9}}}]
      (#'codex/process-codex-event!
       {:type "response.completed" :response response}
       content reasoning usage completed-response
       #(swap! chunks conj %))
      (expect (= "Need to inspect the TUI rendering path." (str reasoning)))
      (expect (= "Need to inspect the TUI rendering path."
                (:reasoning (last @chunks))))
      (expect (= true (:done? (last @chunks))))
      (expect (= 9 (get-in @usage [:completion_tokens_details :reasoning_tokens]))))))

(defdescribe provider-registration-test
  (it "registers the OpenAI Codex auth provider"
    (let [provider (vis/provider-by-id :openai-codex)]
      (expect (= :openai-codex (:provider/id provider)))
      (expect (= "OpenAI Codex (ChatGPT OAuth)" (:provider/label provider)))
      (expect (ifn? (:provider/get-token-fn provider))))))
