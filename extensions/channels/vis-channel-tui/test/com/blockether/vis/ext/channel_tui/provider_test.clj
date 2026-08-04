(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- eventually
  [pred]
  (loop [attempts 50]
    (cond (pred) true
          (pos? attempts) (do (Thread/sleep 20) (recur (dec attempts)))
          :else false)))

(defn- add-keys!
  "Queue `keys` on the virtual terminal: `KeyType` members go through untouched,
   characters become plain keystrokes, so a magit transient's single-key commands
   are driven exactly as a user types them."
  [^DefaultVirtualTerminal terminal keys]
  (doseq [k keys]
    (.addInput terminal
               ^KeyStroke
               (if (instance? KeyType k)
                 (KeyStroke. ^KeyType k)
                 (KeyStroke. (Character/valueOf ^Character k) false false)))))

(defdescribe provider-dialog-namespace-test
             (it "loads the provider dialog namespace"
                 (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

(defdescribe provider-dialog-title-copy-test
             (it "uses the concise Providers title"
                 (expect (= "Providers" @#'provider/provider-dialog-title))))

(defdescribe default-first-provider-presentation-test
             (it "moves the explicit default provider to the top without disturbing peers"
                 (let
                   [order
                    @#'provider/default-first-providers

                    fleet
                    [{:id :anthropic-coding-plan} {:id :openai-codex} {:id :zai-coding-plan}]]

                   (expect (= [:openai-codex :anthropic-coding-plan :zai-coding-plan]
                              (mapv :id (order fleet "openai-codex"))))))
             (it "moves the explicit default model above the live catalog and keeps Show all last"
                 (with-redefs
                   [vis/gateway-provider-model-options
                    (fn [_ show-all?]
                      (if show-all?
                        {:models ["gpt-mini" "gpt-main" "gpt-main-2025-01-01"] :hidden-count 0}
                        {:models ["gpt-mini" "gpt-main"] :hidden-count 1}))]
                   (let
                     [build @#'provider/build-model-list
                      provider {:id :openai-codex}]

                     (expect (= ["gpt-main" "gpt-mini" :show-all]
                                (mapv :id (build provider ["gpt-main"] false))))
                     (expect (= ["gpt-main" "gpt-mini" "gpt-main-2025-01-01"]
                                (mapv :id (build provider ["gpt-main"] true))))))))

(defdescribe
  provider-inline-model-transient-test
  (it
    "selects the default model inside the provider dialog instead of opening another dialog"
    (let
      [terminal
       (DefaultVirtualTerminal. (TerminalSize. 80 30))

       screen
       (doto (TerminalScreen. terminal) (.startScreen))

       selected
       (atom nil)

       seed
       {:default-provider :beta
        :default-model "beta-default"
        :providers [{:id :alpha :models [{:name "alpha-1"} {:name "alpha-2"}]}
                    {:id :beta :models [{:name "beta-default"} {:name "beta-2"}]}]}]

      (try
        (with-redefs
          [vis/authenticated-preset-providers
           (constantly [])

           vis/gateway-provider-status
           (fn [_]
             {"is_authenticated" true "is_loading" false})

           vis/gateway-provider-limits
           (fn [provider-id]
             {:provider-id provider-id :status :ready :static {} :dynamic {:limits []}})

           vis/worker-future
           (fn [_ f]
             (f))

           vis/gateway-provider-model-options
           (fn [provider-id _]
             {:models (if (= provider-id :alpha) ["alpha-1" "alpha-2"] ["beta-default" "beta-2"])
              :hidden-count 0})

           vis/gateway-set-router-default!
           (fn [provider-id model]
             (reset! selected {:provider-id provider-id :model model}))

           vis/load-config-raw
           (constantly {})

           vis/configured-providers
           (constantly [])

           vis/save-config!
           (constantly nil)

           vis/load-config
           (constantly seed)

           dlg/select-dialog!
           (fn [& _]
             (throw (ex-info "nested model dialog opened" {})))]

          ;; Default :beta starts first. Choose :alpha, Enter opens its magit
          ;; transient, `d` runs "Set as Default...", `a` takes the first model.
          (add-keys! terminal [KeyType/ArrowDown KeyType/Enter \d \a KeyType/Escape KeyType/Escape])
          (provider/show-provider-dialog! screen seed)
          (expect (= {:provider-id :alpha :model "alpha-1"} @selected)))
        (finally (.stopScreen screen))))))

(defdescribe provider-card-scroll-test
             (it "keeps selected provider cards inside a visible scroll window"
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

(defdescribe provider-dialog-save-domain-config-test
             (it "returns the reloaded keyword-domain config after persisting raw YAML"
                 (let
                   [saved
                    (atom nil)

                    item
                    {:id :openai-codex :models [{:name "gpt-5.5"}]}

                    domain
                    {:providers [item]}

                    save-provider-config!
                    @#'provider/save-provider-config!]

                   (with-redefs
                     [vis/load-config-raw
                      (constantly {"theme" "dark"})

                      vis/configured-providers
                      (constantly [])

                      vis/save-config!
                      #(reset! saved %)

                      vis/load-config
                      (constantly domain)]

                     (expect (= domain (save-provider-config! [item])))
                     (expect (= "dark" (get @saved "theme")))
                     (expect (= [(vis/provider-persisted-config item)]
                                (get @saved "providers")))))))

(defdescribe provider-dialog-save-keeps-daemon-owned-credentials-test
             (it "merges each row onto the persisted entry so the gateway-written key survives"
                 (let
                   [saved
                    (atom nil)

                    ;; The dialog's in-memory row NEVER carries the credential: the daemon
                    ;; wrote it during `auth/complete`.
                    item
                    {:id :zai-coding-plan :models [{:name "glm-5.2"}]}

                    save-provider-config!
                    @#'provider/save-provider-config!]

                   (with-redefs
                     [vis/load-config-raw
                      (constantly {})

                      vis/configured-providers
                      (constantly [{:id :zai-coding-plan :api-key "sk-daemon-owned"}])

                      vis/save-config!
                      #(reset! saved %)

                      vis/load-config
                      (constantly {:providers [item]})]

                     (save-provider-config! [item])
                     (let [row (first (get @saved "providers"))]
                       (expect (= :zai-coding-plan (:id row)))
                       (expect (= "sk-daemon-owned" (:api-key row)))
                       (expect (= [{:name "glm-5.2"}] (:models row))))))))

(defdescribe
  configured-provider-status-test
  (it "routes configured provider status through the gateway"
      (with-redefs
        [vis/gateway-provider-status (fn [provider-id]
                                       {"is_authenticated" true
                                        "source" "gateway"
                                        "provider_id" (name provider-id)
                                        "config_path" (vis/state-path)})]
        (expect (= {"is_authenticated" true
                    "source" "gateway"
                    "provider_id" "openai"
                    "config_path" (vis/state-path)}
                   (select-keys (@#'provider/gateway-provider-status-safe
                                 {:id :openai :api-key "sk-test" :models [{:name "gpt-5"}]})
                                ["is_authenticated" "source" "provider_id" "config_path"])))))
  (it "routes local no-auth provider status through the gateway instead of probing locally"
      (let [local-probed? (atom false)]
        (with-redefs
          [providers/probe-local-reachable
           (fn [_]
             (reset! local-probed? true)
             {:is-authenticated true :source :local :provider-id :ollama})
           vis/gateway-provider-status
           (fn [provider-id]
             {"is_authenticated" true "source" "gateway" "provider_id" (name provider-id)})]

          (expect (= {"is_authenticated" true "source" "gateway" "provider_id" "ollama"}
                     (select-keys (@#'provider/gateway-provider-status-safe {:id :ollama})
                                  ["is_authenticated" "source" "provider_id"])))
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
             {:is-authenticated true})

           vis/gateway-provider-limits
           (fn [_]
             (reset! limits-called? true)
             {:status :ok})]

          (expect (= {"is_authenticated" nil "is_loading" true}
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
           {"is_authenticated" true "source" "gateway"})

         vis/gateway-provider-limits
         (fn [provider-id]
           (deliver limits-entered provider-id)
           @release
           {:provider-id provider-id :status :ok :static {:rpm 1} :dynamic {:limits []}})]

        (@#'provider/refresh-provider-diagnostics! {:id :slow} statuses limits)
        (expect (= true (get-in @statuses [:slow "is_loading"])))
        (expect (= :loading (get-in @limits [:slow :status])))
        (expect (= :slow (deref status-entered 500 nil)))
        (expect (= :slow (deref limits-entered 500 nil)))
        (expect (= true (@#'provider/provider-diagnostics-loading? @statuses @limits)))
        (deliver release true)
        (expect (eventually #(= true (get-in @statuses [:slow "is_authenticated"]))))
        (expect (eventually #(= :ok (get-in @limits [:slow :status]))))
        (expect (= false (@#'provider/provider-diagnostics-loading? @statuses @limits)))))))

(defdescribe
  provider-action-items-test
  (it
    "offers the default + fallback tags, auth actions for remote providers, and no model configuration"
    (with-redefs
      [vis/provider-by-id
       (fn [provider-id]
         (case provider-id
           :openai
           {:provider/status-fn (constantly {:is-authenticated true})}

           :ollama
           {:provider/status-fn (constantly {:is-authenticated true})}

           nil))

       vis/gateway-provider-status
       (fn [provider-id]
         (if (= :openai provider-id) {"is_authenticated" true} {"is_authenticated" false}))]

      (expect (= [:default :fallback :authenticate :status :logout]
                 (mapv :id (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
      (expect (= ["Set as Default..." "Set as Fallback..." "Re-authenticate" "Show Status + Limits"
                  "Log Out"]
                 (mapv :label (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
      ;; Only the row that ALREADY carries the fallback tag can drop it.
      (expect (= [:default :fallback :clear-fallback :authenticate :status :logout]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       true))))
      (expect (= [:default :fallback :status]
                 (mapv :id (provider/provider-action-items {:id :ollama}))))
      ;; The PRIMARY's own card never offers the fallback tag: the daemon refuses
      ;; a fallback naming the primary's provider, so the action could only ever
      ;; produce a rejection dialog.
      (expect (= [:default :authenticate :status :logout]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       false
                                                       true))))
      ;; A stale config naming ONE provider for both roles still drops `:fallback`
      ;; while keeping the escape hatch that clears the tag.
      (expect (= [:default :clear-fallback :authenticate :status :logout]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       true
                                                       true)))))))

(defdescribe
  logout-provider-test
  (it
    "clears the credential through the gateway and KEEPS the persisted provider"
    (let
      [logout-called?
       (atom false)

       removed
       (atom nil)

       message
       (atom nil)]

      (with-redefs
        [;; Logout is a GATEWAY call: the daemon owns the credential file.
         vis/gateway-provider-logout!
         (fn [provider-id]
           (reset! logout-called? (= :anthropic-coding-plan provider-id)))

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
        ;; Logging out forgets the CREDENTIAL, never the configuration: models,
        ;; base-url and tags have to survive so signing back in is one dialog away.
        (expect (nil? @removed))
        (expect (str/includes? (str @message) "stays configured")))))
  (it "reports a gateway refusal instead of letting it escape as a fatal error"
      (let
        [removed
         (atom nil)

         message
         (atom nil)]

        (with-redefs
          [vis/gateway-provider-logout!
           (fn [_]
             (throw (ex-info "provider logout failed: 400" {:status 400})))

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

          (expect (= false (provider/logout-provider! nil {:id :anthropic-coding-plan})))
          (expect (nil? @removed))
          (expect (str/includes? (str @message) "Logout failed"))))))

(defdescribe
  api-key-auth-prompt-test
  ;; API-key providers now take the SAME gateway road as the OAuth ones:
  ;; `auth/start` mints the flow and returns the provider's own guidance,
  ;; `auth/complete` persists the key DAEMON-side. The TUI never runs a
  ;; provider `:provider/auth-fn` and never writes a credential itself.
  (it
    "feeds gateway-issued auth guidance into the API-key input dialog"
    (let
      [input-args
       (atom nil)

       submitted
       (atom nil)]

      (with-redefs
        [vis/gateway-provider-auth-start!
         (constantly {"flow_id" "flow-1"
                      "kind" "api-key"
                      "instructions" ["  Z.ai (Coding Plan) requires a static API key."
                                      "  Endpoint: https://api.z.ai/api/coding/paas/v4"]})

         vis/gateway-provider-auth-submit-key!
         (fn [pid flow-id api-key]
           (reset! submitted [pid flow-id api-key])
           {"status" "ok"})

         dlg/transient-dialog!
         (fn [& args]
           (reset! input-args args)
           {:action :submit :options {:api-key "  sk-secret  "}})]

        ;; The row that comes back carries NO credential — the daemon owns it.
        (expect (= {:id :zai-coding-plan}
                   (provider/authenticate-provider! nil {:id :zai-coding-plan})))
        (expect (= [:zai-coding-plan "flow-1" "sk-secret"] @submitted))
        (let
          [[_ _ body spec]
           @input-args

           key-item
           (first (:items (first (:groups spec))))]

          (expect (= ["  Z.ai (Coding Plan) requires a static API key."
                      "  Endpoint: https://api.z.ai/api/coding/paas/v4"]
                     body))
          (expect (= :api-key (:id key-item)))
          (expect (= \* (:mask key-item)))
          (expect (= true (:secret? key-item)))))))
  (it "never runs the provider's in-process auth-fn"
      (let [auth-called? (atom false)]
        (with-redefs
          [vis/provider-by-id (constantly {:provider/auth-fn (fn [& _]
                                                               (reset! auth-called? true)
                                                               :ok)})
           vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-2" "kind" "api-key" "instructions" ["static guidance"]})
           vis/gateway-provider-auth-submit-key! (constantly {"status" "ok"})
           dlg/transient-dialog! (constantly {:action :submit :options {:api-key "sk-key"}})]

          (expect (= {:id :zai-coding-plan}
                     (provider/authenticate-provider! nil {:id :zai-coding-plan})))
          (expect (= false @auth-called?)))))
  (it
    "treats Esc from the API-key prompt as a gateway cancel, not a local write"
    (let
      [cancelled
       (atom nil)

       submitted?
       (atom false)

       viewer-called?
       (atom false)]

      (with-redefs
        [vis/gateway-provider-auth-start!
         (constantly {"flow_id" "flow-3" "kind" "api-key" "instructions" ["guidance"]})

         vis/gateway-provider-auth-cancel!
         (fn [pid flow-id]
           (reset! cancelled [pid flow-id])
           {"status" "cancelled"})

         vis/gateway-provider-auth-submit-key!
         (fn [& _]
           (reset! submitted? true))

         dlg/transient-dialog!
         (constantly nil)

         dlg/text-viewer-dialog!
         (fn [& _]
           (reset! viewer-called? true))]

        (expect (nil? (provider/authenticate-provider! nil {:id :zai-coding-plan})))
        (expect (= [:zai-coding-plan "flow-3"] @cancelled))
        (expect (= false @submitted?))
        (expect (= false @viewer-called?)))))
  (it "surfaces a gateway failure as a dialog instead of a credential write"
      (let [message (atom nil)]
        (with-redefs
          [vis/gateway-provider-auth-start! (fn [& _]
                                              (throw (ex-info "gateway down" {})))
           dlg/text-view-dialog! (fn [& args]
                                   (reset! message args))]

          (expect (nil? (provider/authenticate-provider! nil {:id :zai-coding-plan})))
          (expect (str/includes? (str @message) "Authentication failed"))))))

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
          (expect (str/includes? text (str "Config path: " (vis/state-path))))
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
                    {:is-authenticated nil :loading? true}
                    {:provider-id :slow :status :loading :static {} :dynamic {:limits []}})]
            (expect (str/includes? text "Authenticated: checking…"))
            (expect (not (str/includes? text "Authenticated: no")))
            (expect (not (str/includes? text "Is loading:")))
            (expect (str/includes? text "Status: loading"))
            (expect (= false @limits-probed?)))))))

(def ^:private copilot-device-flow
  {"flow_id" "flow-1"
   "kind" "device"
   "user_code" "ABCD-EFGH"
   "verification_uri" "https://github.com/login/device"
   "interval_ms" 1000})

(defdescribe
  copilot-device-login-test
  (it "does not start a device flow when the GATEWAY reports Copilot credentials"
      (let [start-called? (atom false)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" true})
           vis/gateway-provider-auth-start! (fn [& _]
                                              (reset! start-called? true)
                                              nil)]

          (expect
            (true?
              (@#'provider/gateway-device-login! nil :github-copilot-individual "GitHub Copilot")))
          (expect (= false @start-called?)))))
  (it "drives the device flow THROUGH THE GATEWAY, never the provider in-process"
      (let
        [started
         (atom nil)

         polled
         (atom 0)]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" false})

           vis/gateway-provider-auth-start!
           (fn [pid]
             (reset! started pid)
             copilot-device-flow)

           vis/gateway-provider-auth-poll!
           (fn [& _]
             (swap! polled inc)
             {"status" "ok"})

           provider/device-auth-instructions!
           (fn [& _]
             true)]

          (expect
            (true?
              (@#'provider/gateway-device-login! nil :github-copilot-individual "GitHub Copilot")))
          (expect (= :github-copilot-individual @started))
          (expect (= 1 @polled)))))
  (it "cancels the gateway flow when the user escapes the code dialog"
      (let [cancelled (atom nil)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" true})
           vis/gateway-provider-auth-start! (constantly copilot-device-flow)
           vis/gateway-provider-auth-cancel! (fn [_ flow-id]
                                               (reset! cancelled flow-id))
           provider/device-auth-instructions! (fn [& _]
                                                nil)]

          (expect (nil? (@#'provider/gateway-device-login!
                         nil
                         :github-copilot-individual
                         "GitHub Copilot"
                         true)))
          (expect (= "flow-1" @cancelled)))))
  (it "surfaces the gateway's error verdict instead of claiming success"
      (let [shown (atom nil)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly copilot-device-flow)
           vis/gateway-provider-auth-poll! (constantly {"status" "error"
                                                        "message" "device authorization failed"})
           provider/device-auth-instructions! (fn [& _]
                                                true)
           dlg/text-view-dialog! (fn [_ _ lines]
                                   (reset! shown lines)
                                   nil)]

          (expect
            (nil?
              (@#'provider/gateway-device-login! nil :github-copilot-individual "GitHub Copilot")))
          (expect (str/includes? (str @shown) "device authorization failed")))))
  (it
    "times out pending device authorization instead of hanging the TUI"
    (let
      [cancelled?
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
        [vis/gateway-provider-status
         (constantly {"is_authenticated" false})

         vis/gateway-provider-auth-start!
         (constantly copilot-device-flow)

         vis/gateway-provider-auth-cancel!
         (fn [& _]
           nil)

         provider/device-auth-instructions!
         (fn [& _]
           true)

         provider/device-wait-poll-ms
         1

         provider/device-wait-timeout-ms
         1

         vis/worker-future
         (fn [& _]
           pending-result)]

        (expect
          (nil?
            (@#'provider/gateway-device-login! nil :github-copilot-individual "GitHub Copilot")))
        (expect (= true @cancelled?))))))

(def ^:private codex-redirect "http://localhost:1455/auth/callback?code=abc&state=s")

(defdescribe
  codex-oauth-ready-test
  (it "returns true immediately when the GATEWAY reports Codex credentials"
      (let [start-called? (atom false)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" true})
           vis/gateway-provider-auth-start! (fn [& _]
                                              (reset! start-called? true)
                                              nil)
           dlg/confirm-dialog! (fn [& _]
                                 nil)]

          (expect (= true (@#'provider/codex-oauth-ready! nil)))
          (expect (= false @start-called?)))))
  (it "drives Codex login THROUGH THE GATEWAY, never in-process"
      (let [seen (atom {})]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start!
           (fn [provider-id]
             (swap! seen assoc :started provider-id)
             {"flow_id" "flow-1" "kind" "pkce" "url" "https://auth.openai.com/authorize?x=1"})
           vis/gateway-provider-auth-complete!
           (fn [provider-id flow-id input]
             (swap! seen assoc :completed [provider-id flow-id input])
             {"status" "ok"})
           opener/open! (fn [url]
                          (swap! seen assoc :opened url)
                          true)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-view-dialog! (fn [& _]
                                   nil)
           dlg/text-input-dialog! (fn [& _]
                                    codex-redirect)]

          (expect (= true (@#'provider/codex-oauth-ready! nil)))
          (expect (= :openai-codex (:started @seen)))
          (expect (= "https://auth.openai.com/authorize?x=1" (:opened @seen)))
          (expect (= [:openai-codex "flow-1" codex-redirect] (:completed @seen))))))
  (it "cancels the gateway flow when the user pastes nothing"
      (let [cancelled (atom nil)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-1" "kind" "pkce" "url" "https://auth"})
           vis/gateway-provider-auth-cancel! (fn [provider-id flow-id]
                                               (reset! cancelled [provider-id flow-id]))
           opener/open! (constantly true)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-view-dialog! (fn [& _]
                                   nil)
           dlg/text-input-dialog! (fn [& _]
                                    nil)]

          (expect (= false (@#'provider/codex-oauth-ready! nil)))
          (expect (= [:openai-codex "flow-1"] @cancelled)))))
  (it "forces a fresh gateway flow when re-authenticating existing credentials"
      (let [start-called? (atom false)]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" true})
           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             {"flow_id" "flow-1" "kind" "pkce" "url" "https://auth"})
           vis/gateway-provider-auth-complete! (constantly {"status" "ok"})
           opener/open! (constantly true)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-view-dialog! (fn [& _]
                                   nil)
           dlg/text-input-dialog! (fn [& _]
                                    codex-redirect)]

          (expect (= true (@#'provider/codex-oauth-ready! nil true)))
          (expect (= true @start-called?)))))
  (it "does not force Codex login from a plain authenticate call when credentials exist"
      (let
        [start-called?
         (atom false)

         provider-config
         {:id :openai-codex :models [{:name "gpt-5.1"}]}]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             nil)

           dlg/confirm-dialog!
           (fn [& _]
             nil)]

          (expect (= provider-config (provider/authenticate-provider! nil provider-config)))
          (expect (= false @start-called?)))))
  (it "does not force Codex login from the auth picker when credentials exist"
      (let
        [start-called?
         (atom false)

         provider-item
         {:provider-id :openai-codex
          :provider {:provider/id :openai-codex :provider/label "OpenAI Codex"}}]

        (with-redefs
          [dlg/select-dialog!
           (fn [& _]
             provider-item)

           vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             nil)

           dlg/confirm-dialog!
           (fn [& _]
             nil)]

          (expect (= true (provider/show-provider-auth-dialog! nil)))
          (expect (= false @start-called?)))))
  (it "returns false when the gateway auth flow fails"
      (with-redefs
        [vis/gateway-provider-status
         (constantly {"is_authenticated" false})

         vis/gateway-provider-auth-start!
         (fn [& _]
           (throw (ex-info "boom" {})))

         dlg/confirm-dialog!
         (fn [& _]
           true)

         dlg/text-view-dialog!
         (fn [& _]
           nil)]

        (expect (= false (@#'provider/codex-oauth-ready! nil))))))

(defdescribe
  add-provider-test
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
                                  (do (reset! model-picker-called? true) (first items))))]

          (expect (= {:id :openai-codex :models [{:name "gpt-5.1"} {:name "gpt-5.2"}]}
                     (@#'provider/add-provider! nil #{})))
          (expect (= false @model-picker-called?)))))
  (it
    "adds a plain API-key provider THROUGH THE GATEWAY, never writing the key"
    (let
      [submitted
       (atom [])

       started
       (atom [])]

      (with-redefs
        [vis/provider-presets
         (constantly [{:id :zai-coding-plan
                       :label "Z.AI Coding Plan"
                       :base-url "https://api.z.ai"
                       :default-models ["glm-5.2"]}])

         vis/gateway-provider-auth-start!
         (fn [pid]
           (swap! started conj pid)
           {"flow_id" "f-1" "kind" "api-key" "instructions" ["Paste your key"]})

         vis/gateway-provider-auth-submit-key!
         (fn [pid flow-id key]
           (swap! submitted conj [pid flow-id key])
           {"status" "ok"})

         vis/gateway-provider-auth-cancel!
         (fn [_ _]
           (throw (ex-info "cancelled" {})))

         vis/gateway-provider-model-options
         (constantly {:models ["glm-5.2"] :hidden-count 0})

         dlg/text-input-dialog!
         (fn [& _]
           "sk-typed-by-user")

         dlg/transient-dialog!
         (fn [& _]
           {:action :submit :options {:api-key "sk-typed-by-user"}})

         dlg/select-dialog!
         (fn [_ _ items]
           (first items))]

        (let [cfg (@#'provider/add-provider! nil #{})]
          ;; The gateway owns the credential: the daemon persisted it,
          ;; so the config the TUI hands back carries NO key.
          (expect (= [:zai-coding-plan] @started))
          (expect (= [[:zai-coding-plan "f-1" "sk-typed-by-user"]] @submitted))
          (expect (= :zai-coding-plan (:id cfg)))
          (expect (= [{:name "glm-5.2"}] (:models cfg)))
          (expect (nil? (:api-key cfg))))))))

;; ── Regression (user report, first run): "I want to connect provider — WHY IS
;; IT NOT OPENING THE SETTINGS? we should not duplicate the popups".
;; Enter on the welcome screen painted the "Add Provider" picker straight on top
;; of the welcome dialog, and the Base URL step on top of the picker: the outer
;; box's border, ✕ and hint bar still framed the inner one, so the user saw two
;; stacked popups instead of one wizard step. ───────────────────────────────────

(defdescribe add-provider-wizard-step-test
             (it "erases the parent dialog before each step, so only ONE popup is ever on screen"
                 (with-redefs
                   [vis/provider-presets
                    (constantly [{:id :ollama :label "Ollama" :base-url "http://localhost:11434"}])]
                   (let
                     [captured (cap/capture! {:keys [:enter :enter :esc :esc :esc]
                                              :paint!
                                              (fn [{:keys [screen]}]
                                                ;; The wizard lives behind Settings › Providers →
                                                ;; `a` now, so stand in for that opener: what this
                                                ;; test measures is the STEP erasure inside it.
                                                (provider/show-welcome!
                                                  screen
                                                  #(@#'provider/add-provider! screen #{})))})
                      frames (mapv #(cap/frame-text captured %) (range (count (:frames captured))))
                      picker (first (filter #(str/includes? % "Add Provider") frames))
                      base-url (first (filter #(str/includes? % "Base URL") frames))]

                     (expect (nil? (:error captured)))
                     (expect (some? picker))
                     (expect (some? base-url))
                     ;; Every ✕ is one dialog's close marker: two of them in a frame means
                     ;; the parent's chrome survived underneath the nested step.
                     (expect (every? #(<= (count (re-seq #"✕" %)) 1) frames))
                     ;; ...and the welcome body is gone while the picker is up.
                     (expect (not (str/includes? picker "Your terminal, now agentic.")))))))

;; ── Regression (user report, first run): "when we click add provider it should
;; open the settings and providers, like typically". Enter on the welcome screen
;; ran a PRIVATE add-provider picker, so first run had a provider UI of its own —
;; not Settings parked on `Providers`, the one dialog C-x o and the palette open.
;; ─────────────────────────────────────────────────────────────────────────────

(defdescribe
  welcome-opens-settings-providers-test
  (it
    "Enter hands off to the caller's Settings › Providers opener, never a picker of its own"
    (let
      [opened
       (atom 0)

       picked
       (atom 0)

       returned
       (atom :unset)]

      (with-redefs
        [vis/provider-presets
         (constantly [{:id :ollama :label "Ollama" :base-url "http://localhost:11434"}])

         dlg/select-dialog!
         (fn [& _]
           (swap! picked inc)
           nil)]

        (let
          [captured
           (cap/capture! {:keys [:enter]
                          :paint! (fn [{:keys [screen]}]
                                    (reset! returned (provider/show-welcome!
                                                       screen
                                                       (fn []
                                                         (swap! opened inc)
                                                         {:providers [{:id :ollama}]}))))})

           frames
           (mapv #(cap/frame-text captured %) (range (count (:frames captured))))]

          (expect (nil? (:error captured)))
          ;; ONE hand-off to Settings, and the welcome screen returns
          ;; the config that surface already persisted.
          (expect (= 1 @opened))
          (expect (= {:providers [{:id :ollama}]} @returned))
          ;; ...and the welcome screen never opens a provider picker itself.
          (expect (zero? @picked))
          (expect (not-any? #(str/includes? % "Add Provider") frames)))))))

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
  (it "copilot OAuth success closes silently (no ✓ Authenticated! popup)"
      (let [popups (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly copilot-device-flow)
           vis/gateway-provider-auth-poll! (constantly {"status" "ok"})
           vis/worker-future (fn [_label thunk]
                               (let [v (thunk)]
                                 (reify
                                   clojure.lang.IDeref
                                     (deref [_] v)
                                   clojure.lang.IPending
                                     (isRealized [_] true))))
           provider/device-auth-instructions! (fn [& _]
                                                true)
           dlg/text-view-dialog! (text-view-recorder popups)
           dlg/text-viewer-dialog! (text-viewer-recorder popups)]

          (expect
            (true?
              (@#'provider/gateway-device-login! nil :github-copilot-individual "GitHub Copilot")))
          (expect (empty? (filter #(some (fn [l]
                                           (str/includes? (str l) "Authenticated"))
                                         (or (:lines %) [(:text %)]))
                                  @popups))))))
  (it "codex OAuth success closes silently (no ✓ Authenticated! popup)"
      (let [popups (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-1" "kind" "pkce" "url" "https://auth"})
           vis/gateway-provider-auth-complete! (constantly {"status" "ok"})
           opener/open! (constantly true)
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
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           opener/open! (constantly true)
           dlg/confirm-dialog! (fn [& _]
                                 true)
           dlg/text-input-dialog! (fn [& _]
                                    "http://localhost:53692/callback?code=abc&state=s")
           dlg/text-view-dialog! (text-view-recorder popups)
           dlg/text-viewer-dialog! (text-viewer-recorder popups)]

          (with-redefs
            [vis/gateway-provider-auth-start!
             (constantly {"flow_id" "flow-1" "kind" "pkce" "url" "https://auth"})
             vis/gateway-provider-auth-complete! (constantly {"status" "ok"})]

            (expect (= true (@#'provider/anthropic-oauth-ready! nil)))
            (expect (empty? (filter #(some (fn [l]
                                             (str/includes? (str l) "Authenticated"))
                                           (or (:lines %) [(:text %)]))
                                    @popups)))))))
  (it "generic api-key provider (zai-coding-style) succeeds silently through the gateway"
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/gateway-provider-auth-start!
           (constantly {"flow_id" "f" "kind" "api-key"})

           vis/gateway-provider-auth-submit-key!
           (constantly {"status" "ok"})

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/transient-dialog!
           (constantly {:action :submit :options {:api-key "sk-key"}})

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          ;; No credential rides back into the TUI's own rows.
          (expect (= {:id :zai-coding-plan} (@#'provider/gateway-api-key-login! nil provider)))
          (expect (empty? @popups)))))
  (it "generic api-key provider failure still surfaces a dialog"
      (let
        [popups
         (atom [])

         provider
         {:id :zai-coding-plan :api-key nil}]

        (with-redefs
          [vis/gateway-provider-auth-start!
           (constantly {"flow_id" "f" "kind" "api-key"})

           vis/gateway-provider-auth-submit-key!
           (fn [& _]
             (throw (ex-info "boom" {})))

           vis/display-label
           (fn [_]
             "Z.AI Coding")

           dlg/transient-dialog!
           (constantly {:action :submit :options {:api-key "sk-key"}})

           dlg/text-view-dialog!
           (text-view-recorder popups)

           dlg/text-viewer-dialog!
           (text-viewer-recorder popups)]

          (expect (nil? (@#'provider/gateway-api-key-login! nil provider)))
          (expect (= 1 (count @popups)))
          (expect (str/includes? (str (:lines (first @popups))) "Authentication failed: boom"))))))

(defn- fallback-dialog-run
  "Drive the REAL provider dialog to the end: pick the second provider, open its
   actions, choose `Set as Fallback...`, take the first model. `tag!` stands in
   for the gateway call, so this exercises the same key path a user walks."
  [tag!]
  (let
    [terminal
     (DefaultVirtualTerminal. (TerminalSize. 80 30))

     screen
     (doto (TerminalScreen. terminal) (.startScreen))

     shown
     (atom [])

     primary
     (atom nil)

     seed
     {:default-provider :beta
      :default-model "beta-default"
      :providers [{:id :alpha :models [{:name "alpha-1"} {:name "alpha-2"}]}
                  {:id :beta :models [{:name "beta-default"} {:name "beta-2"}]}]}]

    (try
      (with-redefs
        [vis/authenticated-preset-providers
         (constantly [])

         vis/gateway-provider-status
         (fn [_]
           {"is_authenticated" true "is_loading" false})

         vis/gateway-provider-limits
         (fn [provider-id]
           {:provider-id provider-id :status :ready :static {} :dynamic {:limits []}})

         vis/worker-future
         (fn [_ f]
           (f))

         vis/gateway-provider-model-options
         (fn [provider-id _]
           {:models (if (= provider-id :alpha) ["alpha-1" "alpha-2"] ["beta-default" "beta-2"])
            :hidden-count 0})

         vis/gateway-set-router-default!
         (fn [provider-id model]
           (reset! primary {:provider-id provider-id :model model}))

         vis/gateway-set-router-fallback!
         tag!

         vis/load-config-raw
         (constantly {})

         vis/configured-providers
         (constantly [])

         vis/save-config!
         (constantly nil)

         vis/load-config
         (constantly seed)

         dlg/text-view-dialog!
         (fn [_ title lines]
           (swap! shown conj [title (vec lines)]))

         dlg/select-dialog!
         (fn [& _]
           (throw (ex-info "nested model dialog opened" {})))]

        ;; Default :beta sorts first. Down to :alpha, Enter opens its magit
        ;; transient, `f` runs "Set as Fallback...", `a` takes the first model.
        (add-keys! terminal [KeyType/ArrowDown KeyType/Enter \f \a KeyType/Escape KeyType/Escape])
        (provider/show-provider-dialog! screen seed)
        {:shown @shown :primary @primary})
      (finally (.stopScreen screen)))))

(defdescribe
  provider-inline-fallback-transient-test
  (it
    "tags the FALLBACK pair inline — same dialog, different role, and the primary tag is left alone"
    (let
      [tagged
       (atom nil)

       {:keys [shown primary]}
       (fallback-dialog-run (fn [provider-id model]
                              (reset! tagged {:provider-id provider-id :model model})))]

      (expect (= {:provider-id :alpha :model "alpha-1"} @tagged))
      (expect (nil? primary) "choosing the fallback role must never re-tag the primary")
      (expect (empty? shown))))
  (it
    "a daemon refusal (fallback on the primary's own provider) explains itself instead of killing the dialog"
    (let
      [{:keys [shown]} (fallback-dialog-run
                         (fn [_ _]
                           (throw (ex-info
                                    "Fallback provider must differ from the primary provider (beta)"
                                    {:http-status 400}))))]
      (expect (= 1 (count shown)))
      (expect (= "Fallback rejected" (ffirst shown)))
      (expect (some #(str/includes? % "must differ from the primary provider")
                    (second (first shown)))))))

(defdescribe
  provider-card-fallback-badge-test
  (it
    "paints the FALLBACK badge on the tagged provider and never on an untagged one"
    (let
      [terminal
       (DefaultVirtualTerminal. (TerminalSize. 80 30))

       screen
       (doto (TerminalScreen. terminal) (.startScreen))

       draw-card
       @#'provider/draw-provider-card!

       row-text
       (fn [^long y]
         (str/join (for [x (range 80)]
                     (.getCharacterString (.getBackCharacter screen (int x) (int y))))))

       status
       {"is_authenticated" true "is_loading" false}

       limits
       {:status :ready :static {} :dynamic {:limits []}}]

      (try (with-redefs [vis/provider-base-url (constantly "")]
             (let [g (.newTextGraphics screen)]
               (draw-card g
                          0
                          0
                          78
                          0
                          false
                          {:id :alpha}
                          status
                          limits
                          {:provider-id :beta :model "beta-default"}
                          {:provider-id :alpha :model "alpha-1"})
               (draw-card g
                          0
                          4
                          78
                          1
                          false
                          {:id :beta}
                          status
                          limits
                          {:provider-id :beta :model "beta-default"}
                          {:provider-id :alpha :model "alpha-1"})))
           (let
             [fallback-card
              (str (row-text 0) (row-text 1) (row-text 2))

              default-card
              (str (row-text 4) (row-text 5) (row-text 6))]

             (expect (str/includes? fallback-card "FALLBACK"))
             (expect (str/includes? fallback-card "alpha-1"))
             (expect (not (str/includes? fallback-card "DEFAULT")))
             (expect (str/includes? default-card "DEFAULT"))
             (expect (not (str/includes? default-card "FALLBACK"))))
           (finally (.stopScreen screen))))))

(defn- card-rows-text
  "Paint one provider card into a fresh virtual terminal and return its three
   rows as strings, so layout claims are read off the real back-buffer."
  [inner-w provider limits]
  (let
    [terminal
     (DefaultVirtualTerminal. (TerminalSize. 120 12))

     screen
     (doto (TerminalScreen. terminal) (.startScreen))]

    (try (with-redefs [vis/provider-base-url (constantly "")]
           ((deref #'provider/draw-provider-card!)
             (.newTextGraphics screen)
             0
             0
             inner-w
             0
             false
             provider
             {"is_authenticated" true "is_loading" false}
             limits
             {:provider-id :other :model "other-1"}
             nil))
         (mapv (fn [^long y]
                 (str/trimr (str/join (for [x (range 120)]
                                        (.getCharacterString
                                          (.getBackCharacter screen (int x) (int y)))))))
               [0 1 2])
         (finally (.stopScreen screen)))))

(defdescribe
  provider-card-account-line-test
  (it "gives the account limits the card's third row instead of chopping them onto the model line"
      (let
        [limits
         {:status :ready
          :static {}
          :dynamic {:limits [{:id :premium_interactions
                              :label "Premium interactions"
                              :remaining 92992.0
                              :limit 100000.0
                              :used 7008.0} {:id :chat :label "Chat" :is-unlimited true}]}}

         [_line1 line2 line3]
         (card-rows-text 78 {:id :alpha :models ["m1" "m2"]} limits)]

        (expect (str/includes? line2 "2 models available"))
        (expect (not (str/includes? line2 "Premium interactions"))
                "limits must no longer ride behind the model summary")
        (expect (str/includes? line3 "Premium interactions"))
        (expect (str/includes? line3 "(92992 left)"))
        (expect (str/includes? line3 "Chat unlimited")
                "the whole account line fits once it owns a row")))
  (it "ellipsizes the account line on a narrow dialog instead of amputating it mid-word"
      (let
        [limits
         {:status :ready
          :static {}
          :dynamic {:limits [{:id :premium_interactions
                              :label "Premium interactions"
                              :remaining 92992.0
                              :limit 100000.0
                              :used 7008.0} {:id :chat :label "Chat" :is-unlimited true}]}}

         [_line1 _line2 line3]
         (card-rows-text 36 {:id :alpha :models ["m1" "m2"]} limits)]

        (expect (str/ends-with? line3 "…"))
        (expect (<= (count line3) 36))))
  (it
    "keeps the account line under a tagged provider's model row"
    (let
      [limits
       {:status :ready :static {} :dynamic {:limits [{:id :chat :label "Chat" :is-unlimited true}]}}

       terminal
       (DefaultVirtualTerminal. (TerminalSize. 120 12))

       screen
       (doto (TerminalScreen. terminal) (.startScreen))

       row-text
       (fn [^long y]
         (str/join (for [x (range 120)]
                     (.getCharacterString (.getBackCharacter screen (int x) (int y))))))]

      (try (with-redefs [vis/provider-base-url (constantly "")]
             ((deref #'provider/draw-provider-card!)
               (.newTextGraphics screen)
               0
               0
               78
               0
               false
               {:id :alpha :models ["m1"]}
               {"is_authenticated" true "is_loading" false}
               limits
               {:provider-id :alpha :model "alpha-1"}
               nil))
           (expect (str/includes? (row-text 1) "alpha-1"))
           (expect (str/includes? (row-text 1) "DEFAULT"))
           (expect (not (str/includes? (row-text 1) "Chat unlimited")))
           (expect (str/includes? (row-text 2) "Chat unlimited"))
           (finally (.stopScreen screen))))))

(defdescribe
  provider-transient-spec-test
  (it "groups a provider's actions the way magit groups a popup, keeping their keys"
      (let
        [spec (provider/provider-transient-spec [{:id :default :label "Set as Default..." :key \d}
                                                 {:id :fallback :label "Set as Fallback..." :key \f}
                                                 {:id :authenticate :label "Authenticate" :key \a}
                                                 {:id :logout :label "Log Out" :key \l}])]
        (expect (= ["Routing" "Account"] (mapv :title (:groups spec))))
        (expect (= [["d" "f"] ["a" "l"]] (mapv #(mapv :key (:items %)) (:groups spec))))
        (expect (= [["Set as Default..." "Set as Fallback..."] ["Authenticate" "Log Out"]]
                   (mapv #(mapv :label (:items %)) (:groups spec))))
        (expect (= [:default :fallback :authenticate :logout]
                   (mapv :id (mapcat :items (:groups spec)))))
        (expect (every? #(= :action (:type %)) (mapcat :items (:groups spec))))))
  (it "drops a group no action landed in"
      (let
        [spec (provider/provider-transient-spec
                [{:id :default :label "Set as Default..." :key \d}])]
        (expect (= ["Routing"] (mapv :title (:groups spec)))))))

(defdescribe model-transient-spec-test
             (it "reserves the popup's own chrome and never outruns its single-key bindings"
                 (expect (= 1 (provider/model-transient-page-size 3)))
                 (expect (= 13 (provider/model-transient-page-size 20)))
                 (expect (= 24 (provider/model-transient-page-size 200))))
             (it "binds one letter per model and tags the routed pair"
                 (let
                   [entries
                    [{:id "m1" :label "m1"} {:id "m2" :label "m2"} {:id "m3" :label "m3"}
                     {:id :show-all :label "Show all"}]

                    spec
                    (provider/model-transient-spec entries 0 {:default "m1" :fallback "m2"} 2)

                    [models commands]
                    (:groups spec)]

                   (expect (= "Models  1/2" (:title models)))
                   (expect (= ["a" "b"] (mapv :key (:items models))))
                   (expect (= ["m1  (default)" "m2  (fallback)"] (mapv :label (:items models))))
                   (expect (= ["m1" "m2"] (mapv :id (:items models))))
                   (expect (= "Commands" (:title commands)))
                   (expect (= ["n" "p" "*"] (mapv :key (:items commands))))
                   (expect (= [::provider/next-page ::provider/prev-page :show-all]
                              (mapv :id (:items commands))))))
             (it "pages the rest of the catalog onto the same letters"
                 (let
                   [entries
                    [{:id "m1" :label "m1"} {:id "m2" :label "m2"} {:id "m3" :label "m3"}]

                    [models commands]
                    (:groups (provider/model-transient-spec entries 1 {} 2))]

                   (expect (= "Models  2/2" (:title models)))
                   (expect (= ["a"] (mapv :key (:items models))))
                   (expect (= ["m3"] (mapv :id (:items models))))
                   (expect (= ["n" "p"] (mapv :key (:items commands))))))
             (it "shows no paging or expansion commands when one page holds everything"
                 (let [spec (provider/model-transient-spec [{:id "m1" :label "m1"}] 0 {} 8)]
                   (expect (= ["Models"] (mapv :title (:groups spec))))
                   (expect (= ["a"] (mapv :key (:items (first (:groups spec)))))))))

;; ---------------------------------------------------------------------------
;; API-key sign-in is a TRANSIENT, not a full-screen prompt.
;;
;; The old dialog painted a huge vis logo and made the user tab through an
;; input box. Now the provider's own guidance stays on screen and the popup
;; advertises exactly two keys: `k` reads the key inline, `a` submits it.
;; ---------------------------------------------------------------------------

(defdescribe
  api-key-transient-spec-test
  (it "advertises one keystroke per step: k reads the key, a signs in"
      (expect (= "Sign in" (:title (provider/api-key-transient-spec))))
      (expect (= ["Credential" "Authenticate"]
                 (mapv :title (:groups (provider/api-key-transient-spec)))))
      (expect (= [["k"] ["a"]]
                 (mapv #(mapv :key (:items %)) (:groups (provider/api-key-transient-spec)))))
      (expect (= [[:option] [:action]]
                 (mapv #(mapv :type (:items %)) (:groups (provider/api-key-transient-spec))))))
  (it "the credential item is masked while typed and secret once armed"
      (let [item (first (:items (first (:groups (provider/api-key-transient-spec)))))]
        (expect (= :api-key (:id item)))
        (expect (= "API key" (:label item)))
        (expect (= "API key:" (:prompt item)))
        (expect (= \* (:mask item)))
        (expect (= true (:secret? item)))))
  (it "the only action submits the armed key"
      (let [item (first (:items (second (:groups (provider/api-key-transient-spec)))))]
        (expect (= :submit (:id item)))
        (expect (= :action (:type item))))))

(defdescribe command-minted-provider-auth-test
             ;; The screen that started this: a provider whose token is minted by
             ;; `api_key_command` was still offered an "enter your API key" transient,
             ;; right under guidance saying the token is minted automatically.
             (it "explains instead of prompting when config mints the credential itself"
                 (let
                   [message
                    (atom nil)

                    start-called?
                    (atom false)]

                   (with-redefs
                     [vis/gateway-provider-auth-start!
                      (fn [& _]
                        (reset! start-called? true)
                        nil)

                      dlg/text-view-dialog!
                      (fn [& args]
                        (reset! message args))

                      dlg/transient-dialog!
                      (fn [& _]
                        (throw (ex-info "must never prompt for a machine-minted key" {})))]

                     (expect (nil? (provider/authenticate-provider!
                                     nil
                                     {:id :corp :api-key-command "mint-token"})))
                     (expect (= false @start-called?))
                     (expect (str/includes? (str @message) "api_key_command"))))))

;; ── Regression (user report): "when I open the fucking providers and click on
;; active providers the transient is hiding the full render of the fucking
;; settings" ─────────────────────────────────────────────────────────────────
;; Enter on a provider row ran its magit band with `:clear-above? true`, so the
;; band wiped EVERY row from the settings list's top down: the Settings frame
;; was an empty box with one popup floating in it, sidebar rail included. A band
;; is magit chrome — the buffer it is about stays painted above it.
(defn- settings-provider-band-frames
  "Drive Settings (parked on Providers) → Enter on the provider row, off a fixed
   config so the capture never touches the machine's own providers or gateway."
  [^long cols ^long rows]
  (with-redefs
    [vis/load-config
     (constantly
       {:providers [{:id :openai-codex}] :default-provider "openai-codex" :default-model "gpt-5"})

     vis/authenticated-preset-providers
     (constantly [])

     provider/gateway-provider-status-safe
     (constantly nil)]

    (let
      [res (cap/capture! {:cols cols
                          :rows rows
                          :keys [:enter :esc :esc]
                          :paint!
                          (fn [{:keys [screen]}]
                            (dlg/settings-dialog!
                              screen
                              {}
                              {:focus-section "Providers"
                               :provider-transient
                               (fn [{:keys [g region provider-id]}]
                                 (provider/provider-transient! screen g region provider-id))}))})]
      (when-let [t (:error res)]
        (throw t))
      (mapv #(cap/frame-text res %) (range (count (:frames res)))))))

(defdescribe settings-provider-transient-keeps-settings-visible-test
             (it "paints the provider band INSIDE a Settings frame that still shows its own rows"
                 (let
                   [frames
                    (settings-provider-band-frames 100 30)

                    band
                    (first (filter #(str/includes? % "openai-codex — actions") frames))]

                   (expect (some? band))
                   ;; The band is there …
                   (expect (str/includes? band "Set as Default..."))
                   ;; … and so is the Settings frame it lives in: title, sidebar
                   ;; rail and the rows above the band, none of them wiped. The
                   ;; rail is the tell — it starts on the list's own top row, so
                   ;; a band that erased the pane erased it too.
                   (expect (str/includes? band "Settings"))
                   (expect (str/includes? band "Terminal UI"))
                   ;; Still ONE dialog on screen, never a second box.
                   (expect (= 1 (count (re-seq #"✕" band))))))
             (it "restores the whole settings list when the band closes"
                 (let
                   [frames
                    (settings-provider-band-frames 100 30)

                    final
                    (last frames)]

                   (expect (str/includes? final "── Providers"))
                   (expect (str/includes? final "openai-codex"))
                   (expect (not (str/includes? final "— actions"))))))
