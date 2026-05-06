(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.provider-github-copilot :as copilot]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- eventually
  [pred]
  (loop [attempts 50]
    (cond
      (pred) true
      (pos? attempts) (do
                        (Thread/sleep 20)
                        (recur (dec attempts)))
      :else false)))

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

(defdescribe provider-card-scroll-test
  (it "keeps selected model cards inside a visible scroll window"
    (let [card-visible-count @#'provider/card-visible-count
          card-window-start  @#'provider/card-window-start]
      (expect (= 2 (card-visible-count 7)))
      (expect (= 3 (card-visible-count 8)))
      (expect (= 0 (card-window-start 0 0 8 20)))
      (expect (= 10 (card-window-start 12 0 8 20)))
      (expect (= 17 (card-window-start 19 10 8 20)))))

  (it "shows a scrollbar thumb for overflowing model/provider card lists"
    (let [card-scrollbar-geometry @#'provider/card-scrollbar-geometry]
      (expect (= {:track-h 8 :thumb-h 1 :thumb-top 0}
                (card-scrollbar-geometry 8 20 0)))
      (expect (= {:track-h 8 :thumb-h 1 :thumb-top 7}
                (card-scrollbar-geometry 8 20 17)))
      (expect (nil? (card-scrollbar-geometry 8 3 0))))))

(defdescribe persisted-provider-config-test
  (it "persists the dialog provider without runtime adapter coercion"
    (let [persisted-provider-config @#'provider/persisted-provider-config
          provider {:id :openai-codex
                    :models [{:name "gpt-5.5"}]
                    :base-url "https://chatgpt.com/backend-api"
                    :api-key "tok"
                    :api-style :openai-compatible-responses
                    :llm-headers {"chatgpt-account-id" "acct_123"}}]
      (expect (= provider (persisted-provider-config provider))))))

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

(defdescribe provider-dialog-async-diagnostics-test
  (it "seeds provider diagnostics without running blocking provider probes"
    (let [status-called? (atom false)
          limits-called? (atom false)]
      (with-redefs [vis/provider-by-id (constantly {:provider/status-fn (fn []
                                                                          (reset! status-called? true)
                                                                          {:authenticated? true})})
                    vis/provider-limits (fn [_]
                                          (reset! limits-called? true)
                                          {:status :ok})]
        (expect (= {:authenticated? nil
                    :loading? true}
                  (@#'provider/initial-provider-status {:id :slow})))
        (expect (= {:provider-id :slow
                    :status :loading
                    :static {}
                    :dynamic {:limits []}}
                  (@#'provider/initial-provider-limits {:id :slow})))
        (expect (= false @status-called?))
        (expect (= false @limits-called?)))))

  (it "refreshes provider diagnostics in the background after loading state is visible"
    (let [status-entered (promise)
          limits-entered (promise)
          release        (promise)
          statuses       (atom {})
          limits         (atom {})]
      (with-redefs [vis/provider-by-id (constantly {:provider/status-fn (fn []
                                                                          (deliver status-entered true)
                                                                          @release
                                                                          {:authenticated? true
                                                                           :source :test})})
                    vis/provider-limits (fn [provider-id]
                                          (deliver limits-entered provider-id)
                                          @release
                                          {:provider-id provider-id
                                           :status :ok
                                           :static {:rpm 1}
                                           :dynamic {:limits []}})]
        (@#'provider/refresh-provider-diagnostics! {:id :slow} statuses limits)
        (expect (= true (get-in @statuses [:slow :loading?])))
        (expect (= :loading (get-in @limits [:slow :status])))
        (expect (= true (deref status-entered 500 false)))
        (expect (= :slow (deref limits-entered 500 nil)))
        (expect (= true (@#'provider/provider-diagnostics-loading? @statuses @limits)))
        (deliver release true)
        (expect (eventually #(= true (get-in @statuses [:slow :authenticated?]))))
        (expect (eventually #(= :ok (get-in @limits [:slow :status]))))
        (expect (= false (@#'provider/provider-diagnostics-loading? @statuses @limits)))))))

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

(defdescribe api-key-auth-prompt-test
  (it "feeds static provider auth guidance into the API-key input dialog"
    (with-redefs [vis/provider-by-id (constantly {:provider/auth-fn (fn [print!]
                                                                      (print! "")
                                                                      (print! "  Z.ai (Coding Plan) requires a static API key.")
                                                                      (print! "")
                                                                      (print! "  Endpoint: https://api.z.ai/api/coding/paas/v4")
                                                                      :no-credentials)})]
      (expect (= ["  Z.ai (Coding Plan) requires a static API key."
                  ""
                  "  Endpoint: https://api.z.ai/api/coding/paas/v4"]
                (@#'provider/provider-auth-prompt-body {:id :zai-coding})))))

  (it "prefers pure prompt guidance over running the auth flow"
    (let [auth-called? (atom false)]
      (with-redefs [vis/provider-by-id (constantly {:provider/auth-prompt-fn (constantly ["static guidance"])
                                                    :provider/auth-fn (fn [_]
                                                                        (reset! auth-called? true))})]
        (expect (= ["static guidance"]
                  (@#'provider/provider-auth-prompt-body {:id :zai-coding})))
        (expect (= false @auth-called?)))))

  (it "treats Esc from the API-key prompt as cancel instead of showing guidance afterward"
    (let [input-args (atom nil)
          viewer-called? (atom false)]
      (with-redefs [vis/provider-by-id (constantly {:provider/auth-fn (fn [print!]
                                                                        (print! "  Z.ai (Coding Plan) requires a static API key.")
                                                                        :no-credentials)})
                    dlg/text-input-dialog! (fn [& args]
                                             (reset! input-args args)
                                             nil)
                    dlg/text-viewer-dialog! (fn [& _]
                                              (reset! viewer-called? true))]
        (expect (nil? (provider/authenticate-provider! nil {:id :zai-coding})))
        (let [opts (apply hash-map (drop 3 @input-args))]
          (expect (= ["  Z.ai (Coding Plan) requires a static API key."]
                    (:body opts))))
        (expect (= false @viewer-called?))))))

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
        (expect (str/includes? text "Note: Static-only for now.")))))

  (it "renders cached loading diagnostics without live provider probes"
    (let [provider-probed? (atom false)
          limits-probed?   (atom false)]
      (with-redefs [vis/provider-by-id (fn [_]
                                         (reset! provider-probed? true)
                                         nil)
                    vis/provider-limits (fn [_]
                                          (reset! limits-probed? true)
                                          {:status :ok})]
        (let [text (@#'provider/provider-status-text
                    {:id :slow}
                    {:authenticated? nil :loading? true}
                    {:provider-id :slow
                     :status :loading
                     :static {}
                     :dynamic {:limits []}})]
          (expect (str/includes? text "Authenticated: no"))
          (expect (str/includes? text "Loading?: true"))
          (expect (str/includes? text "Status: loading"))
          (expect (= false @provider-probed?))
          (expect (= false @limits-probed?)))))))

(defdescribe copilot-oauth-ready-test
  (it "does not start the device flow when Copilot credentials already exist"
    (let [start-called? (atom false)]
      (with-redefs [copilot/detect-oauth-token (constantly {:oauth-token "oauth"})
                    copilot/get-copilot-token! (constantly {:token "api-token"})
                    copilot/start-device-flow! (fn [& _]
                                                 (reset! start-called? true))]
        (expect (= "api-token" (@#'provider/copilot-oauth-flow! nil :individual)))
        (expect (= false @start-called?)))))

  (it "starts the device flow when Copilot re-authentication is requested"
    (let [start-called? (atom false)]
      (with-redefs [copilot/detect-oauth-token (constantly {:oauth-token "oauth"})
                    copilot/start-device-flow! (fn [& _]
                                                 (reset! start-called? true)
                                                 {:user-code "ABCD-EFGH"
                                                  :verification-uri "https://github.com/login/device"
                                                  :device-code "device"
                                                  :interval 5
                                                  :expires-in 900})
                    provider/copilot-auth-instructions! (fn [& _] nil)]
        (expect (nil? (@#'provider/copilot-oauth-flow! nil :individual true)))
        (expect (= true @start-called?))))))

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
                    dlg/confirm-dialog! (fn [& _] true)
                    dlg/text-view-dialog! (fn [& _] nil)
                    dlg/text-input-dialog! (fn [& _] "http://localhost:1455/auth/callback?code=abc&state=s")]
        (expect (= true (@#'provider/codex-oauth-ready! nil)))
        (expect (= "vis-tui" (get-in @seen [:opts :originator])))
        (expect (ifn? (get-in @seen [:opts :manual-code-fn])))
        (expect (= "http://localhost:1455/auth/callback?code=abc&state=s"
                  ((get-in @seen [:opts :manual-code-fn]) nil))))))

  (it "forces the shared Codex login flow when re-authenticating existing credentials"
    (let [seen (atom nil)]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [printer-fn opts]
                                         (reset! seen {:printer-fn printer-fn :opts opts})
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] true)
                    dlg/text-view-dialog! (fn [& _] nil)
                    dlg/text-input-dialog! (fn [& _] "http://localhost:1455/auth/callback?code=abc&state=s")]
        (expect (= true (@#'provider/codex-oauth-ready! nil true)))
        (expect (= true (get-in @seen [:opts :force?]))))))

  (it "does not force Codex login from a plain authenticate call when credentials exist"
    (let [login-called? (atom false)
          provider-config {:id :openai-codex :models [{:name "gpt-5.1"}]}]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [& _]
                                         (reset! login-called? true)
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)]
        (expect (= provider-config (provider/authenticate-provider! nil provider-config)))
        (expect (= false @login-called?)))))

  (it "does not force Codex login from the auth picker when credentials exist"
    (let [login-called? (atom false)
          provider-item {:provider-id :openai-codex
                         :provider {:provider/id :openai-codex
                                    :provider/label "OpenAI Codex"}}]
      (with-redefs [dlg/select-dialog! (fn [& _] provider-item)
                    vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [& _]
                                         (reset! login-called? true)
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] nil)]
        (expect (= true (provider/show-provider-auth-dialog! nil)))
        (expect (= false @login-called?)))))

  (it "forces Codex login only when re-authentication is requested"
    (let [seen (atom nil)
          provider-config {:id :openai-codex :models [{:name "gpt-5.1"}]}]
      (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})
                    codex/login!       (fn [printer-fn opts]
                                         (reset! seen {:printer-fn printer-fn :opts opts})
                                         :ok)
                    dlg/confirm-dialog! (fn [& _] true)
                    dlg/text-view-dialog! (fn [& _] nil)
                    dlg/text-input-dialog! (fn [& _] "http://localhost:1455/auth/callback?code=abc&state=s")]
        (expect (= provider-config (provider/authenticate-provider! nil provider-config true)))
        (expect (= true (get-in @seen [:opts :force?]))))))

  (it "returns false when the shared Codex login flow fails"
    (with-redefs [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
                  codex/login!       (fn [& _]
                                       (throw (ex-info "boom" {})))
                  dlg/confirm-dialog! (fn [& _] true)
                  dlg/text-view-dialog! (fn [& _] nil)]
      (expect (= false (@#'provider/codex-oauth-ready! nil))))))

(defdescribe add-provider-test
  (it "connects OpenAI Codex OAuth without forcing a single model selection"
    (let [model-picker-called? (atom false)]
      (with-redefs [vis/provider-presets (constantly [{:id :openai-codex
                                                       :label "OpenAI Codex"
                                                       :default-models ["gpt-5.1" "gpt-5.2"]}])
                    provider/codex-oauth-ready! (constantly true)
                    dlg/select-dialog! (fn [_ title items]
                                         (case title
                                           "Add Provider" (first items)
                                           "Select Model" (do
                                                            (reset! model-picker-called? true)
                                                            (first items))))]
        (expect (= {:id :openai-codex
                    :models [{:name "gpt-5.1"} {:name "gpt-5.2"}]}
                  (@#'provider/add-provider! nil #{})))
        (expect (= false @model-picker-called?))))))
