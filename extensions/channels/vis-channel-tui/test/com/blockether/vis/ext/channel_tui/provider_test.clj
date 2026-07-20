(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.provider-anthropic :as anthropic]
            [com.blockether.vis.ext.provider-github-copilot :as copilot]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(defn- eventually
  [pred]
  (loop [attempts 50]
    (cond (pred) true
          (pos? attempts) (do (Thread/sleep 20) (recur (dec attempts)))
          :else false)))

(defdescribe provider-dialog-namespace-test
             (it "loads the provider dialog namespace"
                 (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

(defdescribe swap-items-test
             (it "swaps two positions without touching the others"
                 (let [swap-items @#'provider/swap-items]
                   (expect (= [:a :c :b :d] (swap-items [:a :b :c :d] 1 2))))))

(defdescribe reorder-modifier-test
             (it "accepts Shift+arrow as macOS-friendly provider/model reorder modifier"
                 (expect (input/reorder-modifier? (KeyStroke. KeyType/ArrowUp false true false)))
                 (expect (input/reorder-modifier? (KeyStroke. KeyType/ArrowUp false false true)))
                 (expect (input/reorder-modifier? (KeyStroke. KeyType/ArrowUp false true true)))
                 (expect (not (input/reorder-modifier?
                                (KeyStroke. KeyType/ArrowUp false false false))))))

(defdescribe remove-provider-by-id-test
             (it "removes a logged-out provider from the router list"
                 (let [remove-provider-by-id @#'provider/remove-provider-by-id]
                   (expect (= [{:id :openai}]
                              (remove-provider-by-id [{:id :anthropic-coding-plan} {:id :openai}]
                                                     :anthropic-coding-plan))))))

(defdescribe move-model-to-front-test
             (it "moves the selected model to the first slot"
                 (let [move-model-to-front @#'provider/move-model-to-front]
                   (expect (= [{:name "beta"} {:name "alpha"} {:name "gamma"}]
                              (move-model-to-front [{:name "alpha"} {:name "beta"} {:name "gamma"}]
                                                   1))))))

(defdescribe provider-card-scroll-test
             (it "keeps selected model cards inside a visible scroll window"
                 (let
                   [card-visible-count
                    @#'provider/card-visible-count

                    card-window-start
                    @#'provider/card-window-start]

                   ;; Cards are 3 rows each with a 1-row gap, so an N-row pane fits
                   ;; (quot (+ N 1) 4) cards: 7 -> 2, 8 -> 2, 11 -> 3.
                   (expect (= 2 (card-visible-count 7)))
                   (expect (= 2 (card-visible-count 8)))
                   (expect (= 3 (card-visible-count 11)))
                   ;; 8-row pane fits 2 cards, so the window keeps the selected card at
                   ;; its bottom edge: select 12 -> start 11, select 19 -> start 18.
                   (expect (= 0 (card-window-start 0 0 8 20)))
                   (expect (= 11 (card-window-start 12 0 8 20)))
                   (expect (= 18 (card-window-start 19 10 8 20)))))
             (it "shows a scrollbar thumb for overflowing model/provider card lists"
                 ;; Cards drive the unified primitive directly now. The viewport size
                 ;; below is the count of cards a pane fits; the track stays 8 rows tall.
                 (let
                   [geom
                    (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)

                    ;; 20 cards, viewport fits 3, track 8 rows.
                    top
                    (geom 20 3 8 0)

                    bot
                    (geom 20 3 8 17)

                    flat
                    (geom 3 3 8 0)]

                   (expect (= 1 (:thumb-h top)))
                   (expect (= 0 (:thumb-top-rel top)))
                   (expect (= 1 (:thumb-h bot)))
                   (expect (= 7 (:thumb-top-rel bot))) ;; track-h(8) - thumb-h(1) = 7
                   (expect (nil? flat)))))

(defdescribe persisted-provider-config-test
             (it "persists the dialog provider without runtime adapter coercion"
                 (let
                   [persisted-provider-config
                    @#'provider/persisted-provider-config

                    provider
                    {:id :openai-codex
                     :models [{:name "gpt-5.5"}]
                     :base-url "https://chatgpt.com/backend-api"
                     :api-key "tok"
                     :api-style :openai-compatible-responses
                     :llm-headers {"chatgpt-account-id" "acct_123"}}]

                   (expect (= provider (persisted-provider-config provider))))))

(defdescribe
  configured-provider-status-test
  (it
    "routes configured provider status through the gateway"
    (with-redefs
      [vis/gateway-provider-status (fn [provider-id]
                                     {:authenticated? true
                                      :source :gateway
                                      :provider-id provider-id
                                      :config-path vis/config-path})]
      (expect
        (= {:authenticated? true :source :gateway :provider-id :openai :config-path vis/config-path}
           (select-keys (@#'provider/configured-provider-status
                         {:id :openai :api-key "sk-test" :models [{:name "gpt-5"}]})
                        [:authenticated? :source :provider-id :config-path])))))
  (it "routes local no-auth provider status through the gateway instead of probing locally"
      (let [local-probed? (atom false)]
        (with-redefs
          [providers/probe-local-reachable
           (fn [_]
             (reset! local-probed? true)
             {:authenticated? true :source :local :provider-id :ollama})
           vis/gateway-provider-status
           (fn [provider-id]
             {:authenticated? true :source :gateway :provider-id provider-id})]

          (expect (= {:authenticated? true :source :gateway :provider-id :ollama}
                     (select-keys (@#'provider/configured-provider-status {:id :ollama})
                                  [:authenticated? :source :provider-id])))
          (expect (= false @local-probed?))))))

(defdescribe
  provider-dialog-async-diagnostics-test
  (it "seeds provider diagnostics without running blocking provider probes"
      (let
        [status-called?
         (atom false)

         limits-called?
         (atom false)]

        (with-redefs
          [vis/gateway-provider-status
           (fn [_]
             (reset! status-called? true)
             {:authenticated? true})

           vis/gateway-provider-limits
           (fn [_]
             (reset! limits-called? true)
             {:status :ok})]

          (expect (= {:authenticated? nil :loading? true}
                     (@#'provider/initial-provider-status {:id :slow})))
          (expect (= {:provider-id :slow :status :loading :static {} :dynamic {:limits []}}
                     (@#'provider/initial-provider-limits {:id :slow})))
          (expect (= false @status-called?))
          (expect (= false @limits-called?)))))
  (it
    "refreshes provider diagnostics in the background after loading state is visible"
    (let
      [status-entered
       (promise)

       limits-entered
       (promise)

       release
       (promise)

       statuses
       (atom {})

       limits
       (atom {})]

      (with-redefs
        [vis/gateway-provider-status
         (fn [provider-id]
           (deliver status-entered provider-id)
           @release
           {:authenticated? true :source :gateway})

         vis/gateway-provider-limits
         (fn [provider-id]
           (deliver limits-entered provider-id)
           @release
           {:provider-id provider-id :status :ok :static {:rpm 1} :dynamic {:limits []}})]

        (@#'provider/refresh-provider-diagnostics! {:id :slow} statuses limits)
        (expect (= true (get-in @statuses [:slow :loading?])))
        (expect (= :loading (get-in @limits [:slow :status])))
        (expect (= :slow (deref status-entered 500 nil)))
        (expect (= :slow (deref limits-entered 500 nil)))
        (expect (= true (@#'provider/provider-diagnostics-loading? @statuses @limits)))
        (deliver release true)
        (expect (eventually #(= true (get-in @statuses [:slow :authenticated?]))))
        (expect (eventually #(= :ok (get-in @limits [:slow :status]))))
        (expect (= false (@#'provider/provider-diagnostics-loading? @statuses @limits)))))))

(defdescribe
  provider-action-items-test
  (it "offers auth actions for remote providers and only status for local providers"
      (with-redefs
        [vis/provider-by-id
         (fn [provider-id]
           (case provider-id
             :openai
             {:provider/status-fn (constantly {:authenticated? true})}

             :ollama
             {:provider/status-fn (constantly {:authenticated? true})}

             nil))

         vis/gateway-provider-status
         (fn [provider-id]
           (if (= :openai provider-id) {:authenticated? true} {:authenticated? false}))]

        (expect (= [:models :authenticate :status :logout]
                   (mapv :id (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
        (expect (= ["Configure Models" "Re-authenticate" "Show Status + Limits" "Log Out"]
                   (mapv :label (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
        (expect (= [:models :status] (mapv :id (provider/provider-action-items {:id :ollama})))))))

(defdescribe
  logout-provider-test
  (it
    "clears provider token storage and removes the persisted provider entry"
    (let
      [logout-called?
       (atom false)

       removed
       (atom nil)

       message
       (atom nil)]

      (with-redefs
        [vis/provider-by-id
         (fn [provider-id]
           (when (= :anthropic-coding-plan provider-id)
             {:provider/logout-fn #(reset! logout-called? true)}))

         vis/remove-config-provider!
         (fn [provider-id source]
           (reset! removed {:provider-id provider-id :source source})
           true)

         dlg/confirm-dialog!
         (fn [& _]
           true)

         dlg/text-view-dialog!
         (fn [& args]
           (reset! message args))]

        (expect (= true (provider/logout-provider! nil {:id :anthropic-coding-plan})))
        (expect (= true @logout-called?))
        (expect (= {:provider-id :anthropic-coding-plan :source :tui-provider-logout} @removed))
        (expect (str/includes? (str @message) "Provider removed from config"))))))

(defdescribe
  api-key-auth-prompt-test
  (it "feeds static provider auth guidance into the API-key input dialog"
      (with-redefs
        [vis/provider-by-id
         (constantly {:provider/auth-fn (fn [print!]
                                          (print! "")
                                          (print! "  Z.ai (Coding Plan) requires a static API key.")
                                          (print! "")
                                          (print! "  Endpoint: https://api.z.ai/api/coding/paas/v4")
                                          :no-credentials)})]
        (expect (= ["  Z.ai (Coding Plan) requires a static API key." ""
                    "  Endpoint: https://api.z.ai/api/coding/paas/v4"]
                   (@#'provider/provider-auth-prompt-body {:id :zai-coding-plan})))))
  (it "prefers pure prompt guidance over running the auth flow"
      (let [auth-called? (atom false)]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/auth-prompt-fn (constantly ["static guidance"])
                                           :provider/auth-fn (fn [_]
                                                               (reset! auth-called? true))})]
          (expect (= ["static guidance"]
                     (@#'provider/provider-auth-prompt-body {:id :zai-coding-plan})))
          (expect (= false @auth-called?)))))
  (it "treats Esc from the API-key prompt as cancel instead of showing guidance afterward"
      (let
        [input-args
         (atom nil)

         viewer-called?
         (atom false)]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [print!]
                                            (print!
                                              "  Z.ai (Coding Plan) requires a static API key.")
                                            :no-credentials)})

           dlg/text-input-dialog!
           (fn [& args]
             (reset! input-args args)
             nil)

           dlg/text-viewer-dialog!
           (fn [& _]
             (reset! viewer-called? true))]

          (expect (nil? (provider/authenticate-provider! nil {:id :zai-coding-plan})))
          (let [opts (apply hash-map (drop 3 @input-args))]
            (expect (= ["  Z.ai (Coding Plan) requires a static API key."] (:body opts))))
          (expect (= false @viewer-called?))))))

(defdescribe
  provider-status-text-test
  ;; The status report moved to the channel-neutral core service
  ;; (`internal.providers/status-text`); the TUI dialog renders the rich
  ;; markdown twin (`status-md`) but the text form stays the canonical
  ;; flat report. Redefs target the INTERNAL vars the core fns call —
  ;; the `vis.core` re-export vars are separate var objects.
  (it "renders config path and catalog limits in the provider status dialog"
      (with-redefs
        [provider-limits/provider-limits (constantly {:provider-id :openai-codex
                                                      :status :ok
                                                      :static {:rpm 500 :tpm 2000000}
                                                      :dynamic {:limits []
                                                                :note "Static-only for now."}})]
        (let
          [text (providers/status-text
                  {:id :openai-codex :base-url "https://chatgpt.com/backend-api" :api-key "tok"})]
          (expect (str/includes? text "Base URL: https://chatgpt.com/backend-api"))
          (expect (str/includes? text "Authenticated: yes"))
          (expect (str/includes? text (str "Config path: " vis/config-path)))
          (expect (str/includes? text "Catalog RPM: 500"))
          (expect (str/includes? text "Catalog TPM: 2000000"))
          (expect
            (str/includes?
              text
              "Catalog RPM / TPM come from the provider catalog, not live account quota usage."))
          (expect (str/includes? text "Note: Static-only for now.")))))
  (it "renders cached loading diagnostics without live provider probes"
      (let [limits-probed? (atom false)]
        ;; NOTE: `registry/provider-by-id` is NOT sentineled here —
        ;; `display-label` legitimately consults the registry for the
        ;; human label (cheap map lookup, no IO). The probe-free
        ;; guarantee is about the status/limits FETCHES.
        (with-redefs
          [provider-limits/provider-limits (fn [_]
                                             (reset! limits-probed? true)
                                             {:status :ok})]
          (let
            [text (providers/status-text
                    {:id :slow}
                    {:authenticated? nil :loading? true}
                    {:provider-id :slow :status :loading :static {} :dynamic {:limits []}})]
            (expect (str/includes? text "Authenticated: no"))
            (expect (str/includes? text "Loading?: true"))
            (expect (str/includes? text "Status: loading"))
            (expect (= false @limits-probed?)))))))

(defdescribe
  copilot-oauth-ready-test
  (it "does not start the device flow when Copilot credentials already exist"
      (let [start-called? (atom false)]
        (with-redefs
          [copilot/detect-oauth-token (constantly {:oauth-token "oauth"})
           copilot/get-copilot-token! (constantly {:token "api-token"})
           copilot/start-device-flow! (fn [& _]
                                        (reset! start-called? true))]

          (expect (= "api-token" (@#'provider/copilot-oauth-flow! nil :individual)))
          (expect (= false @start-called?)))))
  (it "starts the device flow when Copilot re-authentication is requested"
      (let [start-called? (atom false)]
        (with-redefs
          [copilot/detect-oauth-token (constantly {:oauth-token "oauth"})
           copilot/start-device-flow! (fn [& _]
                                        (reset! start-called? true)
                                        {:user-code "ABCD-EFGH"
                                         :verification-uri "https://github.com/login/device"
                                         :device-code "device"
                                         :interval 5
                                         :expires-in 900})
           provider/copilot-auth-instructions! (fn [& _]
                                                 nil)]

          (expect (nil? (@#'provider/copilot-oauth-flow! nil :individual true)))
          (expect (= true @start-called?)))))
  (it
    "times out pending device authorization instead of hanging the TUI"
    (let
      [cancelled?
       (atom false)

       exchange-called?
       (atom false)

       pending-result
       (reify
         java.util.concurrent.Future
           (cancel [_ _] (reset! cancelled? true) true)
           (isCancelled [_] @cancelled?)
           (isDone [_] false)
           (get [_] @(promise))
           (get [_ _ _] (throw (java.util.concurrent.TimeoutException.)))
         clojure.lang.IDeref
           (deref [_] @(promise))
         clojure.lang.IPending
           (isRealized [_] false))]

      (with-redefs
        [copilot/detect-oauth-token
         (constantly nil)

         copilot/start-device-flow!
         (fn [& _]
           {:user-code "ABCD-EFGH"
            :verification-uri "https://github.com/login/device"
            :device-code "device"
            :interval 5
            :expires-in 900})

         copilot/get-copilot-token!
         (fn [& _]
           (reset! exchange-called? true)
           {:token "api-token"})

         provider/copilot-auth-instructions!
         (fn [& _]
           true)

         provider/copilot-oauth-wait-poll-ms
         1

         provider/copilot-oauth-wait-timeout-ms
         1

         vis/worker-future
         (fn [& _]
           pending-result)]

        (expect (nil? (@#'provider/copilot-oauth-flow! nil :individual)))
        (expect (= true @cancelled?))
        (expect (= false @exchange-called?))))))

(defdescribe
  codex-oauth-ready-test
  (it "returns true immediately when Codex credentials already exist"
      (let [login-called? (atom false)]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id
                                                                            "acct_123"})})
           codex/login! (fn [& _]
                          (reset! login-called? true)
                          :ok)
           dlg/confirm-dialog! (fn [& _]
                                 nil)]

          (expect (= true (@#'provider/codex-oauth-ready! nil)))
          (expect (= false @login-called?)))))
  (it "runs the shared Codex login flow from the TUI"
      (let [seen (atom nil)]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
           codex/login! (fn [printer-fn opts]
                          (reset! seen {:printer-fn printer-fn :opts opts})
                          :ok)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-view-dialog! (fn [& _]
                                   nil)
           dlg/text-input-dialog! (fn [& _]
                                    "http://localhost:1455/auth/callback?code=abc&state=s")]

          (expect (= true (@#'provider/codex-oauth-ready! nil)))
          (expect (= "vis-tui" (get-in @seen [:opts :originator])))
          (expect (ifn? (get-in @seen [:opts :manual-code-fn])))
          (expect (= "http://localhost:1455/auth/callback?code=abc&state=s"
                     ((get-in @seen [:opts :manual-code-fn]) nil))))))
  (it "forces the shared Codex login flow when re-authenticating existing credentials"
      (let [seen (atom nil)]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/detect-fn (constantly {:account-id
                                                                            "acct_123"})})
           codex/login! (fn [printer-fn opts]
                          (reset! seen {:printer-fn printer-fn :opts opts})
                          :ok)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-view-dialog! (fn [& _]
                                   nil)
           dlg/text-input-dialog! (fn [& _]
                                    "http://localhost:1455/auth/callback?code=abc&state=s")]

          (expect (= true (@#'provider/codex-oauth-ready! nil true)))
          (expect (= true (get-in @seen [:opts :force?]))))))
  (it "does not force Codex login from a plain authenticate call when credentials exist"
      (let
        [login-called?
         (atom false)

         provider-config
         {:id :openai-codex :models [{:name "gpt-5.1"}]}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})

           codex/login!
           (fn [& _]
             (reset! login-called? true)
             :ok)

           dlg/confirm-dialog!
           (fn [& _]
             nil)]

          (expect (= provider-config (provider/authenticate-provider! nil provider-config)))
          (expect (= false @login-called?)))))
  (it "does not force Codex login from the auth picker when credentials exist"
      (let
        [login-called?
         (atom false)

         provider-item
         {:provider-id :openai-codex
          :provider {:provider/id :openai-codex :provider/label "OpenAI Codex"}}]

        (with-redefs
          [dlg/select-dialog!
           (fn [& _]
             provider-item)

           vis/provider-by-id
           (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})

           codex/login!
           (fn [& _]
             (reset! login-called? true)
             :ok)

           dlg/confirm-dialog!
           (fn [& _]
             nil)]

          (expect (= true (provider/show-provider-auth-dialog! nil)))
          (expect (= false @login-called?)))))
  (it "forces Codex login only when re-authentication is requested"
      (let
        [seen
         (atom nil)

         provider-config
         {:id :openai-codex :models [{:name "gpt-5.1"}]}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/detect-fn (constantly {:account-id "acct_123"})})

           codex/login!
           (fn [printer-fn opts]
             (reset! seen {:printer-fn printer-fn :opts opts})
             :ok)

           dlg/confirm-dialog!
           (fn [& _]
             true)

           dlg/text-view-dialog!
           (fn [& _]
             nil)

           dlg/text-input-dialog!
           (fn [& _]
             "http://localhost:1455/auth/callback?code=abc&state=s")]

          (expect (= provider-config (provider/authenticate-provider! nil provider-config true)))
          (expect (= true (get-in @seen [:opts :force?]))))))
  (it "returns false when the shared Codex login flow fails"
      (with-redefs
        [vis/provider-by-id
         (constantly {:provider/detect-fn (constantly nil)})

         codex/login!
         (fn [& _]
           (throw (ex-info "boom" {})))

         dlg/confirm-dialog!
         (fn [& _]
           true)

         dlg/text-view-dialog!
         (fn [& _]
           nil)]

        (expect (= false (@#'provider/codex-oauth-ready! nil))))))

(defdescribe add-provider-test
             (it "connects OpenAI Codex OAuth without forcing a single model selection"
                 (let [model-picker-called? (atom false)]
                   (with-redefs
                     [vis/provider-presets (constantly [{:id :openai-codex
                                                         :label "OpenAI Codex"
                                                         :default-models ["gpt-5.1" "gpt-5.2"]}])
                      provider/codex-oauth-ready! (constantly true)
                      dlg/select-dialog! (fn [_ title items]
                                           (case title
                                             "Add Provider"
                                             (first items)

                                             "Select Model"
                                             (do (reset! model-picker-called? true)
                                                 (first items))))]

                     (expect (= {:id :openai-codex :models [{:name "gpt-5.1"} {:name "gpt-5.2"}]}
                                (@#'provider/add-provider! nil #{})))
                     (expect (= false @model-picker-called?))))))

;; ---------------------------------------------------------------------------
;; Regression: success popups MUST stay silent.
;;
;; User feedback (Anthropic dialog session): the redundant
;; "✓ Authenticated!" / "<provider> authenticated." toast on top of an
;; already-closed auth dialog was confusing. Anthropic was fixed first; this
;; suite asserts the same silence for Copilot, Codex, and the generic
;; api-key auth path that providers like zai-coding use.
;; ---------------------------------------------------------------------------

(defn- text-view-recorder
  [sink]
  (fn [_ title lines]
    (swap! sink conj {:title title :lines (vec lines)})
    nil))

(defn- text-viewer-recorder
  [sink]
  (fn [_ title text]
    (swap! sink conj {:title title :text (str text)})
    nil))

(defdescribe
  silent-auth-success-test
  (it
    "copilot OAuth success closes silently (no ✓ Authenticated! popup)"
    (let [popups (atom [])]
      (with-redefs
        [copilot/detect-oauth-token (constantly nil)
         copilot/start-device-flow! (fn [& _]
                                      {:user-code "AAAA-BBBB"
                                       :verification-uri "https://github.com/login/device"
                                       :device-code "dev"
                                       :interval 0
                                       :expires-in 1})
         copilot/poll-for-token! (fn [& _]
                                   {:status :ok})
         copilot/get-copilot-token! (fn [& _]
                                      {:token "api-token"})
         copilot/logout! (fn []
                           nil)
         vis/worker-future (fn [_label thunk]
                             (let [v (thunk)]
                               (reify
                                 clojure.lang.IDeref
                                   (deref [_] v)
                                 clojure.lang.IPending
                                   (isRealized [_] true))))
         provider/copilot-auth-instructions! (fn [& _]
                                               true)
         dlg/text-view-dialog! (text-view-recorder popups)
         dlg/text-viewer-dialog! (text-viewer-recorder popups)]

        (expect (= "api-token" (@#'provider/copilot-oauth-flow! nil :individual)))
        (expect (empty? (filter #(some (fn [l]
                                         (str/includes? (str l) "Authenticated"))
                                       (or (:lines %) [(:text %)]))
                                @popups))))))
  (it "codex OAuth success closes silently (no ✓ Authenticated! popup)"
      (let [popups (atom [])]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
           codex/login! (fn [& _]
                          :ok)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-input-dialog! (fn [& _]
                                    "http://localhost:1455/auth/callback?code=abc&state=s")
           dlg/text-view-dialog! (text-view-recorder popups)
           dlg/text-viewer-dialog! (text-viewer-recorder popups)]

          (expect (= true (@#'provider/codex-oauth-ready! nil)))
          (expect (empty? (filter #(some (fn [l]
                                           (str/includes? (str l) "Authenticated"))
                                         (or (:lines %) [(:text %)]))
                                  @popups))))))
  (it "anthropic OAuth success closes silently (parity with copilot/codex)"
      (let [popups (atom [])]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/detect-fn (constantly nil)})
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-input-dialog! (fn [& _]
                                    "http://localhost:53692/callback?code=abc&state=s")
           dlg/text-view-dialog! (text-view-recorder popups)
           dlg/text-viewer-dialog! (text-viewer-recorder popups)]

          (with-redefs
            [anthropic/login! (fn [& _]
                                :ok)]
            (expect (= true (@#'provider/anthropic-oauth-ready! nil)))
            (expect (empty? (filter #(some (fn [l]
                                             (str/includes? (str l) "Authenticated"))
                                           (or (:lines %) [(:text %)]))
                                    @popups)))))))
  (it "generic api-key provider (zai-coding-style) shows no success toast when auth-fn is silent"
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [_print!]
                                            :ok)})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (= provider (@#'provider/run-generic-provider-auth! nil provider)))
          (expect (empty? @popups)))))
  (it "zai-coding-style :already-authenticated success stays silent even when auth-fn prints lines"
      ;; Real regression: zai's make-auth-fn prints \"Already authenticated with X.\"
      ;; on the success path. The previous \"silent unless lines collected\" rule
      ;; let those lines through as a popup - the exact \"success dialog\" the user
      ;; vetoed for typical/standard providers. Now success keywords suppress
      ;; printed output regardless.
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [print!]
                                            (print! "  Already authenticated with Z.AI Coding.")
                                            (print! "  Source: config.")
                                            :already-authenticated)})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (= provider (@#'provider/run-generic-provider-auth! nil provider)))
          (expect (empty? @popups)))))
  (it "zai-coding-style :ok success (env-var persisted) stays silent even with printed lines"
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [print!]
                                            (print! "  Persisted Z.ai key from env var.")
                                            (print! "  Z.AI Coding is ready.")
                                            :ok)})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (= provider (@#'provider/run-generic-provider-auth! nil provider)))
          (expect (empty? @popups)))))
  (it "action-required result (:no-credentials) DOES surface auth-fn instructions"
      ;; The mirror case: when auth-fn cannot complete on its own, the user must
      ;; read what to do next. Keep that path live.
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [print!]
                                            (print! "Set ZAI_CODING_API_KEY=... and re-run.")
                                            :no-credentials)})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (= provider (@#'provider/run-generic-provider-auth! nil provider)))
          (expect (= 1 (count @popups)))
          (expect (str/includes? (:text (first @popups)) "ZAI_CODING_API_KEY")))))
  (it "generic api-key provider failure still surfaces a dialog"
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/provider-by-id
           (constantly {:provider/auth-fn (fn [_print!]
                                            (throw (ex-info "boom" {})))})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (nil? (@#'provider/run-generic-provider-auth! nil provider)))
          (expect (= 1 (count @popups)))
          (expect (str/includes? (:text (first @popups)) "Authentication failed: boom"))))))
