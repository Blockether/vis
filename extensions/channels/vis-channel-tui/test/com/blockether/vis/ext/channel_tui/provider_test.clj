(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-dialog-namespace-test
  (it "loads the provider dialog namespace"
    (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

(defdescribe swap-items-test
  (it "swaps two positions without touching the others"
    (let [swap-items @#'provider/swap-items]
      (expect (= [:a :c :b :d]
                (swap-items [:a :b :c :d] 1 2))))))

(defdescribe move-model-to-front-test
  (it "moves the selected model to the first slot"
    (let [move-model-to-front @#'provider/move-model-to-front]
      (expect (= [{:name "beta"} {:name "alpha"} {:name "gamma"}]
                (move-model-to-front [{:name "alpha"} {:name "beta"} {:name "gamma"}] 1))))))

(defdescribe persisted-provider-config-test
  (it "strips transient OpenAI Codex runtime fields before saving config"
    (let [persisted-provider-config @#'provider/persisted-provider-config]
      (expect (= {:id :openai-codex
                  :models [{:name "gpt-5.5"}]
                  :base-url "https://chatgpt.com/backend-api"}
                (persisted-provider-config {:id :openai-codex
                                            :models [{:name "gpt-5.5"}]
                                            :base-url "https://chatgpt.com/backend-api"
                                            :api-key "tok"
                                            :api-style :openai-compatible-responses
                                            :llm-headers {"chatgpt-account-id" "acct_123"}}))))))

(defdescribe configured-provider-status-test
  (it "treats persisted api-key providers as authenticated from config"
    (expect (= {:authenticated? true
                :source :config
                :config-path vis/config-path}
              (select-keys (@#'provider/configured-provider-status {:id :openai
                                                                    :api-key "sk-test"
                                                                    :models [{:name "gpt-5"}]})
                [:authenticated? :source :config-path]))))

  (it "delegates to the registered provider status fn when no api-key is persisted"
    (with-redefs [vis/provider-by-id (constantly {:provider/status-fn (constantly {:authenticated? true
                                                                                   :source :local
                                                                                   :model-count 3})})]
      (expect (= {:authenticated? true
                  :source :local
                  :model-count 3}
                (select-keys (@#'provider/configured-provider-status {:id :ollama})
                  [:authenticated? :source :model-count]))))))

(defdescribe provider-action-items-test
  (it "offers auth actions for remote providers and only status for local providers"
    (with-redefs [vis/provider-by-id (fn [provider-id]
                                       (case provider-id
                                         :openai {:provider/status-fn (constantly {:authenticated? true})}
                                         :ollama {:provider/status-fn (constantly {:authenticated? true})}
                                         nil))]
      (expect (= [:models :authenticate :status :logout]
                (mapv :id (provider/provider-action-items {:id :openai
                                                           :api-key "sk-test"}))))
      (expect (= ["Configure Models" "Re-authenticate" "Show Status + Limits" "Log Out"]
                (mapv :label (provider/provider-action-items {:id :openai
                                                              :api-key "sk-test"}))))
      (expect (= [:models :status]
                (mapv :id (provider/provider-action-items {:id :ollama})))))))

(defdescribe provider-status-text-test
  (it "renders config path and catalog limits in the provider status dialog"
    (with-redefs [vis/provider-limits (constantly {:provider-id :openai-codex
                                                   :status :ok
                                                   :static {:rpm 500 :tpm 2000000}
                                                   :dynamic {:limits []
                                                             :note "Static-only for now."}})]
      (let [text (@#'provider/provider-status-text {:id :openai-codex
                                                    :base-url "https://chatgpt.com/backend-api"
                                                    :api-key "tok"})]
        (expect (str/includes? text "Base URL: https://chatgpt.com/backend-api"))
        (expect (str/includes? text "Authenticated: yes"))
        (expect (str/includes? text (str "Config path: " vis/config-path)))
        (expect (str/includes? text "Catalog RPM: 500"))
        (expect (str/includes? text "Catalog TPM: 2000000"))
        (expect (str/includes? text "Catalog RPM / TPM come from the provider catalog, not live account quota usage."))
        (expect (str/includes? text "Note: Static-only for now."))))))

(defdescribe codex-oauth-ready-test
  (it "returns true immediately when Codex credentials already exist"
    (let [login-called? (atom false)]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [& _]
                                         (reset! login-called? true)
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)]
        (expect (= true (@#'provider/codex-oauth-ready! nil)))
        (expect (= false @login-called?)))))

  (it "runs the shared Codex login flow from the TUI"
    (let [seen (atom nil)]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
                    codex/login!       (fn [printer-fn opts]
                                         (reset! seen {:printer-fn printer-fn :opts opts})
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)
                    dlg/text-input-dialog! (fn [& _] "http://localhost:1455/auth/callback?code=abc&state=s")]
        (expect (= true (@#'provider/codex-oauth-ready! nil)))
        (expect (= "vis-tui" (get-in @seen [:opts :originator])))
        (expect (ifn? (get-in @seen [:opts :manual-code-fn])))
        (expect (= "http://localhost:1455/auth/callback?code=abc&state=s"
                  ((get-in @seen [:opts :manual-code-fn]) nil))))))

  (it "returns false when the shared Codex login flow fails"
    (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
                  codex/login!       (fn [& _]
                                       (throw (ex-info "boom" {})))
                  dlg/confirm-dialog! (fn [& _] nil)]
      (expect (= false (@#'provider/codex-oauth-ready! nil))))))
