(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.ext.channel-tui.terminals :as term]
            [com.blockether.vis.internal.providers :as providers]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))


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

(defn- band-stub
  "A `dlg/band-questions` stand-in. `answers` maps a question's label — or a nested
   transient spec's title — to what the user answers; every question asked lands
   in `log`. An unlisted read/transient IS an Esc, an unlisted confirm is a yes.

   A verb reached from a transient asks HERE: the dialog seams are redefined to
   THROW alongside it, which is how these tests prove no window was opened."
  [answers log]
  {:read! (fn read! ([label] (read! label {}))
            ([label opts] (swap! log conj [:read label opts]) (get answers label)))
   :choose! (fn [title choices]
              (swap! log conj [:choose title choices])
              (get answers title))
   :confirm! (fn confirm! ([question] (confirm! question nil))
               ([question opts] (swap! log conj [:confirm question opts])
                (get answers question true)))
   :note! (fn [title line]
            (swap! log conj [:note title line])
            nil)
   :wait! (fn [title _line-fn done?]
            (swap! log conj [:wait title])
            ;; The real one holds the band until the daemon answers or the clock
            ;; runs out; so does this, without a terminal.
            (loop [n 0]
              (cond (done?) true
                    (< n 300) (do (Thread/sleep 10) (recur (inc n)))
                    :else nil)))
   :transient! (fn [spec]
                 (swap! log conj [:transient spec])
                 (get answers (:title spec)))})

(defn- band-fn
  "`band-stub` as the `dlg/band-questions` redef itself."
  [answers log]
  (fn [& _]
    (band-stub answers log)))

(defn- notes "Every line the verb SAID back in the band." [log] (filterv #(= :note (first %)) @log))

(defdescribe provider-dialog-namespace-test
             (it "loads the provider dialog namespace"
                 (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

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

      (expect (= [:default :fallback :authenticate :status :remove]
                 (mapv :id (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
      ;; Log Out is GONE. Removal is the ONE teardown — the daemon runs the
      ;; provider's own logout AND drops the config entry — so a provider signed
      ;; out here can no longer linger as an authenticated preset nobody can get
      ;; rid of.
      (expect (= ["Set as Default..." "Set as Fallback..." "Re-authenticate" "Show Status + Limits"
                  "Remove Provider"]
                 (mapv :label (provider/provider-action-items {:id :openai :api-key "sk-test"}))))
      ;; Only the row that ALREADY carries the fallback tag can drop it.
      (expect (= [:default :fallback :clear-fallback :authenticate :status :remove]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       true))))
      (expect (= [:default :fallback :status :remove]
                 (mapv :id (provider/provider-action-items {:id :ollama}))))
      ;; The PRIMARY's own card never offers the fallback tag: the daemon refuses
      ;; a fallback naming the primary's provider, so the action could only ever
      ;; produce a rejection dialog.
      (expect (= [:default :authenticate :status :remove]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       false
                                                       true))))
      ;; A stale config naming ONE provider for both roles still drops `:fallback`
      ;; while keeping the escape hatch that clears the tag.
      (expect (= [:default :clear-fallback :authenticate :status :remove]
                 (mapv :id
                       (provider/provider-action-items {:id :openai :api-key "sk-test"}
                                                       {"is_authenticated" true}
                                                       true
                                                       true)))))))

(defdescribe
  remove-provider-test
  ;; Regression (user report, Settings -> Providers): the TUI offered LOG OUT as a
  ;; provider's only teardown while the companion offered Remove, so a provider
  ;; signed out here kept its config entry and came back as an authenticated
  ;; preset - and the confirmation stacked a DIALOG on top of the transient it
  ;; was fired from.
  (it
    "asks in the caller's own band and drops config entry AND credential"
    (let
      [removed
       (atom nil)

       asked
       (atom nil)]

      (with-redefs
        [vis/gateway-provider-remove!
         (fn [provider-id]
           (reset! removed provider-id)
           {"is_removed" true})

         dlg/band-questions
         (fn [_screen _g _region]
           {:confirm! (fn [question opts]
                        (reset! asked {:question question :opts opts})
                        true)
            :note! (fn [& args]
                     (throw (ex-info "a successful removal said nothing" {:args args})))})

         ;; A verb reached from a transient must never answer with a window.
         dlg/confirm-dialog!
         (fn [& _]
           (throw (ex-info "removal opened a dialog" {})))

         dlg/text-view-dialog!
         (fn [& _]
           (throw (ex-info "removal opened a dialog" {})))]

        (expect (= true (provider/remove-provider! nil nil nil {:id :github-copilot-individual})))
        (expect (= :github-copilot-individual @removed))
        ;; The band says what saying yes COSTS, the way the companion's
        ;; confirm row does - `Yes` alone never says what it agrees to.
        (expect (str/includes? (str (:question @asked)) "Remove"))
        (expect (str/includes? (str (:cost (:opts @asked))) "Signs out"))
        (expect (= "Yes, remove" (:yes-label (:opts @asked)))))))
  (it "keeps the row when the user declines"
      (let [removed (atom nil)]
        (with-redefs
          [vis/gateway-provider-remove! (fn [provider-id]
                                          (reset! removed provider-id))
           dlg/band-questions (fn [& _]
                                {:confirm! (fn [& _]
                                             false)
                                 :note! (fn [& _]
                                          nil)})]

          (expect (nil? (provider/remove-provider! nil nil nil {:id :openai})))
          (expect (nil? @removed)))))
  (it "reports a gateway refusal in the SAME band instead of a dialog"
      (let [note (atom nil)]
        (with-redefs
          [vis/gateway-provider-remove! (fn [_]
                                          (throw (ex-info "provider remove failed: 400"
                                                          {:status 400})))
           dlg/band-questions (fn [& _]
                                {:confirm! (fn [& _]
                                             true)
                                 :note! (fn [title line]
                                          (reset! note [title line]))})
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "refusal opened a dialog" {})))]

          (expect (= false (provider/remove-provider! nil nil nil {:id :openai})))
          (expect (str/includes? (str @note) "remove failed"))))))

(defdescribe
  api-key-auth-prompt-test
  ;; API-key providers now take the SAME gateway road as the OAuth ones:
  ;; `auth/start` mints the flow and returns the provider's own guidance,
  ;; `auth/complete` persists the key DAEMON-side. The TUI never runs a
  ;; provider `:provider/auth-fn` and never writes a credential itself — and it
  ;; reads the key in the BAND it was fired from, never in a window over it.
  (it
    "reads the key in the caller's own band and submits it to the gateway"
    (let
      [log
       (atom [])

       submitted
       (atom nil)]

      (with-redefs
        [dlg/host-band-region
         (fn [_screen region]
           region)

         dlg/band-questions
         (band-fn {"Sign in" {:action :submit :options {:api-key "sk-secret"}}} log)

         vis/gateway-provider-auth-start!
         (fn [_provider-id]
           {"flow_id" "flow-1"
            "kind" "api-key"
            "instructions" ["Z.ai (Coding Plan) requires a static API key."
                            "Create one at https://z.ai/manage-apikey/apikey-list"
                            "Then run: vis-agent providers auth zai-coding-plan"]})

         vis/gateway-provider-auth-submit-key!
         (fn [provider-id flow-id api-key]
           (reset! submitted [provider-id flow-id api-key])
           {"status" "ok"})

         dlg/transient-dialog!
         (fn [& _]
           (throw (ex-info "the key prompt opened a dialog" {})))

         dlg/text-view-dialog!
         (fn [& _]
           (throw (ex-info "the key prompt opened a dialog" {})))]

        ;; The row that comes back carries NO credential — the daemon owns it.
        (expect (= {:id :zai-coding-plan}
                   (provider/authenticate-provider! nil nil nil {:id :zai-coding-plan})))
        (expect (= [:zai-coding-plan "flow-1" "sk-secret"] @submitted))
        (let
          [spec
           (second (first (filter #(= :transient (first %)) @log)))

           key-item
           (first (:items (first (:groups spec))))]

          ;; A band has no room for three lines of prose, so the ONE line that
          ;; answers "where do I get a key?" becomes the field's own heading and
          ;; the CLI-only lines are dropped.
          (expect (= "Create one at https://z.ai/manage-apikey/apikey-list"
                     (:title (first (:groups spec)))))
          (expect (= :api-key (:id key-item)))
          (expect (= \* (:mask key-item)))
          (expect (= true (:secret? key-item)))))))
  (it "never runs the provider's in-process auth-fn"
      (let
        [auth-called?
         (atom false)

         log
         (atom [])]

        (with-redefs
          [dlg/host-band-region
           (fn [_screen region]
             region)

           dlg/band-questions
           (band-fn {"Sign in" {:action :submit :options {:api-key "sk-key"}}} log)

           vis/provider-by-id
           (constantly {:provider/auth-fn (fn [& _]
                                            (reset! auth-called? true)
                                            {:api-key "in-process"})})

           vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-2" "kind" "api-key"})

           vis/gateway-provider-auth-submit-key!
           (constantly {"status" "ok"})]

          (expect (= {:id :zai-coding-plan}
                     (provider/authenticate-provider! nil nil nil {:id :zai-coding-plan})))
          (expect (= false @auth-called?)))))
  (it
    "cancels the gateway flow when the user escapes the key band"
    (let
      [cancelled
       (atom nil)

       submitted?
       (atom false)

       log
       (atom [])]

      (with-redefs
        [dlg/host-band-region
         (fn [_screen region]
           region)

         ;; Nothing answers the "Sign in" band: that is an Esc.
         dlg/band-questions
         (band-fn {} log)

         vis/gateway-provider-auth-start!
         (constantly {"flow_id" "flow-3" "kind" "api-key"})

         vis/gateway-provider-auth-cancel!
         (fn [provider-id flow-id]
           (reset! cancelled [provider-id flow-id]))

         vis/gateway-provider-auth-submit-key!
         (fn [& _]
           (reset! submitted? true)
           {"status" "ok"})]

        (expect (nil? (provider/authenticate-provider! nil nil nil {:id :zai-coding-plan})))
        (expect (= [:zai-coding-plan "flow-3"] @cancelled))
        (expect (= false @submitted?))
        ;; A cancel is not an error: the band says nothing at all.
        (expect (empty? (notes log))))))
  (it "reports a gateway failure in the band instead of writing a credential"
      (let [log (atom [])]
        (with-redefs
          [dlg/host-band-region (fn [_screen region]
                                  region)
           dlg/band-questions (band-fn {} log)
           vis/gateway-provider-auth-start! (fn [& _]
                                              (throw (ex-info "gateway auth start failed: 503" {})))
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "a refusal opened a dialog" {})))]

          (expect (nil? (provider/authenticate-provider! nil nil nil {:id :zai-coding-plan})))
          (expect (str/includes? (str (notes log)) "Authentication failed"))))))

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
      (let
        [start-called?
         (atom false)

         log
         (atom [])]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             nil)]

          (expect (= true
                     (@#'provider/gateway-device-login!
                      (band-stub {} log)
                      :github-copilot-individual
                      "GitHub Copilot")))
          (expect (= false @start-called?)))))
  (it
    "drives the device flow THROUGH THE GATEWAY, never the provider in-process"
    (let
      [started
       (atom nil)

       polled
       (atom 0)

       log
       (atom [])]

      (with-redefs
        [vis/gateway-provider-status
         (constantly {"is_authenticated" false})

         vis/gateway-provider-auth-start!
         (fn [provider-id]
           (reset! started provider-id)
           copilot-device-flow)

         vis/gateway-provider-auth-poll!
         (fn [& _]
           (swap! polled inc)
           {"status" "ok"})]

        (expect (= true
                   (@#'provider/gateway-device-login!
                    ;; `w` — "I authorized in the browser" — on the code band.
                    (band-stub {"GitHub Copilot" {:action :wait}} log)
                    :github-copilot-individual
                    "GitHub Copilot")))
        (expect (= :github-copilot-individual @started))
        (expect (= 1 @polled))
        ;; The code and the URL are the two things the user must SEE, so they
        ;; are the band's own group headings — not a window over the list.
        (let [spec (second (first (filter #(= :transient (first %)) @log)))]
          (expect (str/includes? (str (mapv :title (:groups spec))) "ABCD-EFGH"))
          (expect (str/includes? (str (mapv :title (:groups spec)))
                                 "https://github.com/login/device"))))))
  (it "cancels the gateway flow when the user escapes the code band"
      (let
        [cancelled
         (atom nil)

         log
         (atom [])]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (constantly copilot-device-flow)

           vis/gateway-provider-auth-cancel!
           (fn [_ flow-id]
             (reset! cancelled flow-id))]

          (expect (nil? (@#'provider/gateway-device-login!
                         (band-stub {} log)
                         :github-copilot-individual
                         "GitHub Copilot"
                         true)))
          (expect (= "flow-1" @cancelled)))))
  (it "surfaces the gateway's error verdict in the band instead of claiming success"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly copilot-device-flow)
           vis/gateway-provider-auth-poll! (constantly {"status" "error"
                                                        "message" "device authorization failed"})
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "a refusal opened a dialog" {})))]

          (expect (nil? (@#'provider/gateway-device-login!
                         (band-stub {"GitHub Copilot" {:action :wait}} log)
                         :github-copilot-individual
                         "GitHub Copilot")))
          (expect (str/includes? (str (notes log)) "device authorization failed")))))
  (it
    "times out pending device authorization instead of holding the band forever"
    (let
      [cancelled?
       (atom false)

       log
       (atom [])

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
         (constantly nil)

         provider/device-wait-timeout-ms
         50

         vis/worker-future
         (fn [_ _]
           pending-result)]

        (expect (nil? (@#'provider/gateway-device-login!
                       (band-stub {"GitHub Copilot" {:action :wait}} log)
                       :github-copilot-individual
                       "GitHub Copilot")))
        (expect (= true @cancelled?))
        (expect (str/includes? (str (notes log)) "Timed out"))))))

(def ^:private codex-redirect "http://localhost:1455/auth/callback?code=abc&state=s")

(defdescribe
  codex-oauth-ready-test
  (it "returns true immediately when the GATEWAY reports Codex credentials"
      (let
        [start-called?
         (atom false)

         log
         (atom [])]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             nil)]

          (expect (= true (@#'provider/codex-oauth-ready! (band-stub {} log))))
          (expect (= false @start-called?))
          ;; Nothing to confirm, nothing to say.
          (expect (empty? @log)))))
  (it
    "drives Codex login THROUGH THE GATEWAY, never in-process"
    (let
      [seen
       (atom {})

       log
       (atom [])]

      (with-redefs
        [vis/gateway-provider-status
         (constantly {"is_authenticated" false})

         vis/gateway-provider-auth-start!
         (fn [provider-id]
           (swap! seen assoc :started provider-id)
           {"flow_id" "flow-1" "url" "https://auth.openai.com/authorize?x=1"})

         vis/gateway-provider-auth-complete!
         (fn [provider-id flow-id url]
           (swap! seen assoc :completed [provider-id flow-id url])
           {"status" "ok"})

         opener/open!
         (fn [url]
           (swap! seen assoc :opened url)
           true)

         dlg/confirm-dialog!
         (fn [& _]
           (throw (ex-info "the sign-in opened a dialog" {})))

         dlg/text-input-dialog!
         (fn [& _]
           (throw (ex-info "the paste opened a dialog" {})))]

        (expect (= true
                   (@#'provider/codex-oauth-ready!
                    (band-stub {"OpenAI Codex — paste the final browser URL:" codex-redirect}
                               log))))
        (expect (= :openai-codex (:started @seen)))
        (expect (= "https://auth.openai.com/authorize?x=1" (:opened @seen)))
        (expect (= [:openai-codex "flow-1" codex-redirect] (:completed @seen)))
        ;; The band says what saying yes COSTS before it opens a browser.
        (let [[_ question opts] (first (filter #(= :confirm (first %)) @log))]
          (expect (str/includes? question "Codex"))
          (expect (str/includes? (:cost opts) "browser"))
          (expect (= "Yes, open the browser" (:yes-label opts)))))))
  (it "cancels the gateway flow when the user pastes nothing"
      (let
        [cancelled
         (atom nil)

         log
         (atom [])]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" false})

           vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-1" "url" "https://auth.openai.com/x"})

           vis/gateway-provider-auth-cancel!
           (fn [provider-id flow-id]
             (reset! cancelled [provider-id flow-id]))

           opener/open!
           (constantly true)]

          (expect (= false (@#'provider/codex-oauth-ready! (band-stub {} log))))
          (expect (= [:openai-codex "flow-1"] @cancelled)))))
  (it "forces a fresh gateway flow when re-authenticating existing credentials"
      (let
        [start-called?
         (atom false)

         log
         (atom [])]

        (with-redefs
          [vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             {"flow_id" "flow-1" "url" "https://auth.openai.com/x"})

           vis/gateway-provider-auth-complete!
           (constantly {"status" "ok"})

           opener/open!
           (constantly true)]

          (expect (= true
                     (@#'provider/codex-oauth-ready!
                      (band-stub {"OpenAI Codex — paste the final browser URL:" codex-redirect} log)
                      true)))
          (expect (= true @start-called?)))))
  (it "does not force Codex login from a plain authenticate call when credentials exist"
      (let
        [start-called?
         (atom false)

         provider-config
         {:id :openai-codex}

         log
         (atom [])]

        (with-redefs
          [dlg/host-band-region
           (fn [_screen region]
             region)

           dlg/band-questions
           (band-fn {} log)

           vis/gateway-provider-status
           (constantly {"is_authenticated" true})

           vis/gateway-provider-auth-start!
           (fn [& _]
             (reset! start-called? true)
             nil)]

          (expect (= provider-config (provider/authenticate-provider! nil nil nil provider-config)))
          (expect (= false @start-called?)))))
  (it "returns false when the gateway auth flow fails"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (fn [& _]
                                              (throw (ex-info "gateway auth start failed: 500" {})))
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "a refusal opened a dialog" {})))]

          (expect (= false (@#'provider/codex-oauth-ready! (band-stub {} log))))
          (expect (str/includes? (str (notes log)) "Auth failed"))))))

;; ── Regression (user report): "right now the adding provider is a separate
;; dialogue — it should not be like this, it should be a sealed transient, it
;; doesn't really make sense to have it as a separate dialogue". `add-provider!`
;; opened one NESTED DIALOG per step (the preset picker, then the Base URL
;; prompt), and every nested dialog clears the frame it was opened from: the
;; list the user was reading vanished behind the wizard. Adding a provider is
;; now the same magit band as every other provider action.
;; ─────────────────────────────────────────────────────────────────────────────

(defdescribe preset-transient-spec-test
             (it "binds one keystroke per preset and pages the rest"
                 (let
                   [presets
                    (mapv (fn [i]
                            {:id (keyword (str "p" i)) :label (str "Preset " i)})
                          (range 5))

                    page-0
                    (provider/preset-transient-spec presets 0 2)

                    page-1
                    (provider/preset-transient-spec presets 1 2)

                    picks
                    (fn [spec]
                      (mapv (juxt :key :id) (:items (first (:groups spec)))))]

                   (expect (= [["a" :p0] ["b" :p1]] (picks page-0)))
                   (expect (= [["a" :p2] ["b" :p3]] (picks page-1)))
                   ;; …and magit's own paging keys, never bound to a preset.
                   (expect (= ["n" "p"] (mapv :key (:items (second (:groups page-0))))))
                   (expect (str/includes? (:title (first (:groups page-0))) "1/3"))))
             (it "asks for no paging when every preset fits on one page"
                 (let [spec (provider/preset-transient-spec [{:id :ollama :label "Ollama"}] 0 8)]
                   (expect (= 1 (count (:groups spec))))
                   (expect (= "Providers" (:title (first (:groups spec)))))
                   (expect (= [["a" :ollama]]
                              (mapv (juxt :key :id) (:items (first (:groups spec)))))))))

(defdescribe
  local-setup-transient-spec-test
  (it "reads a local endpoint inline and adds with it"
      (let
        [spec
         (provider/local-setup-transient-spec "Ollama" "http://localhost:11434")

         [endpoint commands]
         (:groups spec)]

        (expect (= "Ollama" (:title endpoint)))
        ;; `u` is an OPTION: the URL is typed on the host's own hint row, so no
        ;; second popup ever opens for it.
        (expect (= [{:key "u" :type :option :id :base-url :label "Base URL" :prompt "Base URL:"}]
                   (:items endpoint)))
        (expect (= :add (:id (first (:items commands)))))
        (expect (str/includes? (:label (first (:items commands))) "http://localhost:11434")))))

;; Regression: success popups MUST stay silent.
;;
;; User feedback (Anthropic dialog session): the redundant
;; "✓ Authenticated!" / "<provider> authenticated." toast on top of an
;; already-closed auth dialog was confusing. Anthropic was fixed first; this
;; suite asserts the same silence for Copilot, Codex, and the generic
;; api-key auth path that providers like zai-coding use.

(defdescribe
  silent-auth-success-test
  ;; A successful sign-in used to end with a "✓ Authenticated!" popup over the
  ;; list the user came from. Success is silent everywhere now: the band closes
  ;; and the row it was about is simply signed in.
  (it "copilot success closes the band silently"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly copilot-device-flow)
           vis/gateway-provider-auth-poll! (constantly {"status" "ok"})
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "success opened a dialog" {})))
           dlg/text-viewer-dialog! (fn [& _]
                                     (throw (ex-info "success opened a dialog" {})))]

          (expect (= true
                     (@#'provider/gateway-device-login!
                      (band-stub {"GitHub Copilot" {:action :wait}} log)
                      :github-copilot-individual
                      "GitHub Copilot")))
          (expect (empty? (notes log))))))
  (it "codex success closes the band silently"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly {"flow_id" "flow-1"
                                                         "url" "https://auth.openai.com/x"})
           vis/gateway-provider-auth-complete! (constantly {"status" "ok"})
           opener/open! (constantly true)
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "success opened a dialog" {})))]

          (expect (= true
                     (@#'provider/codex-oauth-ready!
                      (band-stub {"OpenAI Codex — paste the final browser URL:" codex-redirect}
                                 log))))
          (expect (empty? (notes log))))))
  (it "anthropic success closes the band silently (parity with copilot/codex)"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-status (constantly {"is_authenticated" false})
           vis/gateway-provider-auth-start! (constantly {"flow_id" "flow-9"
                                                         "url" "https://claude.ai/oauth"})
           vis/gateway-provider-auth-complete! (constantly {"status" "ok"})
           opener/open! (constantly true)
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "success opened a dialog" {})))]

          (expect (= true
                     (@#'provider/anthropic-oauth-ready!
                      (band-stub {"Anthropic — paste the final browser URL:" "https://x/?code=1"}
                                 log))))
          (expect (empty? (notes log))))))
  (it "an api-key provider succeeds silently through the gateway"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-1" "kind" "api-key" "instructions" ["Paste your key."]})
           vis/gateway-provider-auth-submit-key! (constantly {"status" "ok"})
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "success opened a dialog" {})))]

          ;; No credential rides back into the TUI's own rows.
          (expect (= {:id :zai-coding-plan}
                     (@#'provider/gateway-api-key-login!
                      (band-stub {"Sign in" {:action :submit :options {:api-key "sk-1"}}} log)
                      {:id :zai-coding-plan :api-key "sk-1"})))
          (expect (empty? (notes log))))))
  (it "an api-key failure still says so, in the band"
      (let [log (atom [])]
        (with-redefs
          [vis/gateway-provider-auth-start!
           (constantly {"flow_id" "flow-1" "kind" "api-key" "instructions" ["Paste your key."]})
           vis/gateway-provider-auth-submit-key! (fn [& _]
                                                   (throw (ex-info "boom" {})))
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "a refusal opened a dialog" {})))]

          (expect (nil? (@#'provider/gateway-api-key-login!
                         (band-stub {"Sign in" {:action :submit :options {:api-key "sk-1"}}} log)
                         {:id :zai-coding-plan})))
          (expect (= 1 (count (notes log))))
          (expect (str/includes? (str (notes log)) "Authentication failed: boom"))))))

(defdescribe
  provider-transient-spec-test
  (it "groups a provider's actions the way magit groups a popup, keeping their keys"
      (let
        [spec (provider/provider-transient-spec [{:id :default :label "Set as Default..." :key \d}
                                                 {:id :fallback :label "Set as Fallback..." :key \f}
                                                 {:id :authenticate :label "Authenticate" :key \a}
                                                 {:id :remove :label "Remove Provider" :key \x}])]
        (expect (= ["Routing" "Account"] (mapv :title (:groups spec))))
        (expect (= [["d" "f"] ["a" "x"]] (mapv #(mapv :key (:items %)) (:groups spec))))
        (expect (= [["Set as Default..." "Set as Fallback..."] ["Authenticate" "Remove Provider"]]
                   (mapv #(mapv :label (:items %)) (:groups spec))))
        (expect (= [:default :fallback :authenticate :remove]
                   (mapv :id (mapcat :items (:groups spec)))))
        (expect (every? #(= :action (:type %)) (mapcat :items (:groups spec))))))
  (it "drops a group no action landed in"
      (let
        [spec (provider/provider-transient-spec
                [{:id :default :label "Set as Default..." :key \d}])]
        (expect (= ["Routing"] (mapv :title (:groups spec)))))))

;; ── The model picker owns the rows it pages over ─────────────────────────────
;; A transient band wipes only the rows it paints, so the buffer above it stays
;; visible (the provider card, the settings list). The model picker is the one
;; host that repaints PAGES OF DIFFERENT HEIGHTS into the same rectangle, so it
;; erases the area it owns with `tr/clear-rows!` before each page: without that,
;; a short last page strands the previous page's models above its own title.
(defdescribe
  model-transient-paging-test
  (it
    "a shorter page leaves nothing of the previous page on screen"
    (let
      [terminal
       (DefaultVirtualTerminal. (TerminalSize. 80 30))

       screen
       (doto (TerminalScreen. terminal) (.startScreen))

       g
       (.newTextGraphics screen)

       entries
       (into (mapv (fn [i]
                     {:id (str "stale-model-" i) :label (str "stale-model-" i)})
                   (range 1 5))
             [{:id "final-model-a" :label "final-model-a"}
              {:id "final-model-b" :label "final-model-b"}])

       geom
       {:left 3 :inner-w 74 :hint-row 27 :text-w 70 :min-row 6}

       grid
       (try (add-keys! terminal [\n KeyType/Escape])
            (@#'provider/run-model-transient! screen g geom {:id :alpha} entries nil {} 4)
            (term/grid terminal)
            (finally (.stopScreen screen)))]

      ;; Page 2 is on screen …
      (expect (some #(str/includes? % "final-model-a") grid))
      ;; … and page 1 is GONE: the title is inked ON the band's opening rule, so
      ;; there is ONE title and TWO rules — the opening one and the one above the
      ;; hint bar. Without the picker's own wipe the taller page left a second
      ;; title and a third rule stranded above the band …
      (expect (= 1 (count (filter #(str/includes? % "alpha — models") grid))))
      (expect (= 2 (count (filter #(str/includes? % "────") grid))))
      ;; … and its longest row bled THROUGH the shorter page's own header,
      ;; which used to read `Models  2/2el-2`.
      (expect (= "Models  2/2"
                 (some #(when (str/includes? % "Models  ") (str/trim (str/replace % "│" "")))
                       grid)))
      (expect (not-any? #(str/includes? % "stale-model") grid))
      ;; The picker still never paints above the row it was given.
      (expect (every? str/blank? (take 6 grid))))))

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

;; API-key sign-in is a TRANSIENT, not a full-screen prompt.
;;
;; The old dialog painted a huge vis logo and made the user tab through an
;; input box. Now the provider's own guidance stays on screen and the popup
;; advertises exactly two keys: `k` reads the key inline, `a` submits it.

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
        (expect (= :action (:type item)))))
  (it "hangs the provider's own guidance over the field, in place of `Credential`"
      (expect (= ["Get one at https://z.ai/keys" "Authenticate"]
                 (mapv :title
                       (:groups (provider/api-key-transient-spec "Get one at https://z.ai/keys")))))
      ;; Same shape either way: a hint changes the heading, never the keys.
      (expect (= (mapv #(mapv :key (:items %)) (:groups (provider/api-key-transient-spec)))
                 (mapv #(mapv :key (:items %))
                       (:groups (provider/api-key-transient-spec
                                  "Get one at https://z.ai/keys")))))))

(defdescribe
  api-key-hint-test
  ;; The provider's guidance is written for the CLI; a band has room for ONE line.
  (it "keeps the line that says where to create the key"
      (expect (= "Create one at https://z.ai/manage-apikey/apikey-list"
                 (provider/api-key-hint ["Z.ai (Coding Plan) requires a static API key."
                                         "Create one at https://z.ai/manage-apikey/apikey-list"
                                         "Then run: vis-agent providers auth zai-coding-plan"]))))
  (it "falls back to the first line that says anything at all"
      (expect (= "Paste your key." (provider/api-key-hint ["" "  " "Paste your key." "More."]))))
  (it "has nothing to say about nothing"
      (expect (nil? (provider/api-key-hint nil)))
      (expect (nil? (provider/api-key-hint [])))))

(defdescribe command-minted-provider-auth-test
             ;; The screen that started this: a provider whose token is minted by
             ;; `api_key_command` was still offered an "enter your API key" transient,
             ;; right under guidance saying the token is minted automatically.
             (it "explains instead of prompting when config mints the credential itself"
                 (let
                   [log
                    (atom [])

                    start-called?
                    (atom false)]

                   (with-redefs
                     [dlg/host-band-region
                      (fn [_screen region]
                        region)

                      dlg/band-questions
                      (band-fn {} log)

                      vis/gateway-provider-auth-start!
                      (fn [& _]
                        (reset! start-called? true)
                        nil)

                      dlg/transient-dialog!
                      (fn [& _]
                        (throw (ex-info "must never prompt for a machine-minted key" {})))]

                     (expect (nil? (provider/authenticate-provider!
                                     nil
                                     nil
                                     nil
                                     {:id :corp :api-key-command "mint-token"})))
                     (expect (= false @start-called?))
                     (expect (str/includes? (str (notes log)) "api_key_command"))))))

;; ── Regression (user report): "when I open the providers and click on active
;; providers the transient is hiding the full render of the settings" ─────────────────────────────────────────────────────────────────
;; Enter on a provider row ran its magit band with `:clear-above? true`, so the
;; band wiped EVERY row from the settings list's top down: the Settings frame
;; was an empty box with one popup floating in it, sidebar rail included. A band
;; is magit chrome — the buffer it is about stays painted above it.
(def ^:private settings-router-writes
  "What the router was asked to write during the last `settings-provider-band-frames`
   run — `[:default provider-id model]` / `[:fallback provider-id model]`."
  (atom []))

(defn- settings-provider-band-frames
  "Drive Settings (parked on Providers) → `keystrokes` — Enter on the provider
   row, then whatever the flow under test needs — off a fixed config so the
   capture never touches the machine's own providers, gateway or model catalog.

   The fixture provider id is deliberately one NO provider extension registers:
   `display-label` hands a registered preset its own branding, so a preset id
   renders as `OpenAI Codex (ChatGPT OAuth)` in a run that loaded that extension
   and verbatim in one that did not — the whole aggregate suite versus this
   namespace alone."
  ([^long cols ^long rows] (settings-provider-band-frames cols rows [:enter :esc :esc]))
  ([^long cols ^long rows keystrokes]
   (reset! settings-router-writes [])
   (with-redefs
     [vis/load-config
      (constantly {:providers [{:id :acme-llm}]
                   :default-provider "acme-llm"
                   :default-model "acme-1"
                   :fallback-provider "acme-llm"
                   :fallback-model "acme-2"})

      vis/configured-providers
      (constantly [{:id :acme-llm}])

      vis/authenticated-preset-providers
      (constantly [])

      vis/gateway-provider-model-options
      (constantly {:models ["acme-1" "acme-2" "acme-3"] :hidden-count 0})

      vis/gateway-set-router-default!
      (fn [& args]
        (swap! settings-router-writes conj (into [:default] args))
        nil)

      vis/gateway-set-router-fallback!
      (fn [& args]
        (swap! settings-router-writes conj (into [:fallback] args))
        nil)

      provider/gateway-provider-status-safe
      (constantly nil)]

     (let
       [res (cap/capture! {:cols cols
                           :rows rows
                           :keys keystrokes
                           :paint!
                           (fn [{:keys [screen]}]
                             (dlg/settings-dialog!
                               screen
                               {}
                               {:focus-section "Providers"
                                :provider-add (fn [{:keys [g region]}]
                                                (provider/add-provider-transient! screen g region))
                                :provider-transient
                                (fn [{:keys [g region provider-id]}]
                                  (provider/provider-transient! screen g region provider-id))}))})]
       (when-let [t (:error res)]
         (throw t))
       (mapv #(cap/frame-text res %) (range (count (:frames res))))))))

(defdescribe settings-provider-transient-keeps-settings-visible-test
             (it "paints the provider band INSIDE a Settings frame that still shows its own rows"
                 (let
                   [frames
                    (settings-provider-band-frames 100 30)

                    band
                    (first (filter #(str/includes? % "acme-llm — actions") frames))]

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
                   (expect (str/includes? final "acme-llm"))
                   (expect (not (str/includes? final "— actions"))))))

;; ── Regression (user report): "there is still problem with the transients in
;; the settings when I'm setting default or fallback provider … why top is
;; hidden" ────────────────────────────────────────────────────────────────────
;; `d` / `f` on a provider's band open the MODEL picker, and the picker erased
;; every row from the settings list's TOP down to the hint bar before painting
;; each page: the Settings frame became one popup floating in an empty pane —
;; rows, section headers and the sidebar rail all wiped. A band owns the rows it
;; paints; the only rows it may erase above them are a PREVIOUS band's leftovers.
(defn- settings-model-band
  "The frame where the model picker (`d` on the provider's own band) is up."
  [^long cols ^long rows]
  (first (filter #(str/includes? % "acme-llm — models")
                 (settings-provider-band-frames cols rows [:enter \d :esc :esc]))))

(defdescribe settings-model-transient-keeps-settings-visible-test
             (it
               "keeps the settings list painted while the model picker band is up"
               (let
                 [band
                  (settings-model-band 100 30)

                  rows
                  (str/split-lines band)

                  band-top
                  (long (first (keep-indexed (fn [i s]
                                               (when (str/includes? s "acme-llm — models") i))
                                             rows)))

                  ;; What the settings dialog itself owns: the rows between the
                  ;; search box's rule — the one carrying the sidebar's `┬`
                  ;; junction — and the band's own opening rule.
                  list-rows
                  (->> (take band-top rows)
                       (drop-while #(not (str/includes? % "┬")))
                       rest
                       butlast)

                  ;; A settings row is SPLIT by the sidebar rail, so it carries
                  ;; a third `│`; a wiped row carries only the frame's two.
                  kept
                  (filter #(<= 3 (count (re-seq #"│" %))) list-rows)]

                 ;; The picker is up …
                 (expect (str/includes? band "Models"))
                 (expect (str/includes? band "acme-1"))
                 ;; … over a list that is still THERE. Before the fix every one
                 ;; of these rows was blank paper.
                 (expect (<= 2 (count kept)))
                 (expect (every? #(re-find #"\w" %) kept))
                 ;; One dialog, not a popup floating in an empty pane.
                 (expect (= 1 (count (re-seq #"✕" band))))))
             (it "leaves nothing of the actions band above a shorter model band"
                 (let [band (settings-model-band 100 30)]
                   (expect (not (str/includes? band "— actions")))
                   (expect (not (str/includes? band "Set as Default..."))))))

;; ── Regression (user report): "setting the default provider and model is not
;; making anything — there should be some visualization". The Providers list
;; built its rows from the config's PROVIDER entries alone, so the routing tags
;; the daemon had just persisted (`:default-provider`/`:default-model` and the
;; fallback pair) never reached the screen: picking a default looked like it did
;; nothing at all. ───────────────────────────────────────────────────────────
(defdescribe settings-router-tags-visible-test
             (it "names the routed model on the provider's own row"
                 (let [frame (first (settings-provider-band-frames 100 30 [:esc]))]
                   (expect (str/includes? frame "default → acme-1"))
                   (expect (str/includes? frame "fallback → acme-2")))))

;; ── Regression (user report): "the adding provider is a separate dialogue …
;; it should be a sealed transient". Every step of the old wizard was a NESTED
;; DIALOG, and a nested dialog clears the screen on the way in and on the way
;; out — the list the user was reading disappeared, and what came back was
;; whichever popup painted last. Adding a provider is now bands in the caller's
;; own frame, exactly like every other provider action. ──────────────────────
(defn- settings-add-provider-frames
  "Drive Settings → `Add provider…` → `keystrokes` off ONE fixed preset, and
   capture the config that WOULD have been written instead of touching the
   machine's own."
  [keystrokes]
  (let [saved (atom nil)]
    (with-redefs-fn {#'vis/provider-presets
                     (constantly [{:id :ollama :label "Ollama" :base-url "http://localhost:11434"}])
                     #'provider/save-provider-config! (fn [configs]
                                                        (reset! saved configs)
                                                        true)}
      (fn []
        {:frames (settings-provider-band-frames 100 30 (into [:down :enter] keystrokes))
         :saved @saved}))))

(defdescribe
  settings-add-provider-band-test
  (it "runs preset, endpoint and model as BANDS inside the settings frame"
      (let
        [{:keys [frames saved]}
         (settings-add-provider-frames [\a \a \a :esc])

         frame-with
         (fn [needle]
           (first (filter #(str/includes? % needle) frames)))

         picker
         (frame-with "a  Ollama")

         setup
         (frame-with "Ollama — setup")

         models
         (frame-with "Ollama — models")]

        (expect (some? picker))
        (expect (str/includes? picker "Add provider"))
        (expect (some? setup))
        (expect (str/includes? setup "Add with http://localhost:11434"))
        (expect (some? models))
        ;; Three steps, ONE frame: the settings list is still behind each of
        ;; them and there is never a second box's ✕ on screen.
        (doseq [frame [picker setup models]]
          (expect (str/includes? frame "Settings"))
          (expect (str/includes? frame "Terminal UI"))
          (expect (= 1 (count (re-seq #"✕" frame)))))
        ;; …and the provider really is added, with the model just picked.
        (expect (= {:id :ollama :models [{:name "acme-1"}] :base-url "http://localhost:11434"}
                   (last saved)))))
  (it "hands the rows a TALLER band covered back to the settings list"
      ;; The endpoint band is taller than the model band that follows it, and
      ;; blanking the rows between the two band tops punched a hole in the list
      ;; behind the popup — the host is not repainted while a flow runs, so the
      ;; band restores what it uncovers.
      (let
        [{:keys [frames]}
         (settings-add-provider-frames [\a \a \a :esc])

         rows
         (str/split-lines (first (filter #(str/includes? % "Ollama — models") frames)))

         band-top
         (long (first (keep-indexed #(when (str/includes? %2 "Ollama — models") %1) rows)))

         host-row
         (nth rows (- band-top 2))]

        ;; A settings row is SPLIT by the sidebar rail, so it carries a third
        ;; `│`; an uncovered-and-blanked row carries only the frame's two.
        (expect (re-find #"\w" host-row))
        (expect (<= 3 (count (re-seq #"│" host-row)))))))

;; ── Regression (user report): "Opening providers is slow" ────────────────────
;; Settings read the gateway MCP list AND every provider's auth status before it
;; painted its first frame, so `C-x o` — and the boot path that opens Settings ›
;; Providers when no provider is configured — sat on the old screen for the
;; length of those round trips (a cold daemon: seconds; a gateway on another
;; machine: one RTT per provider). The frame comes first, the fleet lands in it.
(defdescribe
  settings-opens-before-the-gateway-answers-test
  (it
    "paints the Settings frame first and fills the provider fleet in afterwards"
    (let
      [inventory
       (var-get #'dlg/provider-inventory)

       mcp
       (var-get #'dlg/mcp-inventory)

       original
       @inventory

       original-mcp
       @mcp

       gateway-calls
       (atom 0)]

      (try
        ;; Nothing cached: the very first open is the one that used to stall.
        (reset! inventory {:status :unloaded :providers [] :error nil})
        (reset! mcp {:status :unloaded :servers [] :error nil})
        (with-redefs
          [vis/load-config
           (constantly {:providers [{:id :acme-llm}]})

           vis/configured-providers
           (constantly [{:id :acme-llm}])

           vis/authenticated-preset-providers
           (constantly [])

           vis/gateway-mcp-servers
           (fn []
             (swap! gateway-calls inc)
             [])

           vis/gateway-router-fleet
           (fn []
             (swap! gateway-calls inc)
             [{"id" "acme-llm" "status" {"is_authenticated" true}}])]

          (let
            [res
             (cap/capture! {:cols 100
                            :rows 30
                            :keys [:esc]
                            :paint!
                            (fn [{:keys [screen]}]
                              (dlg/settings-dialog! screen {} {:focus-section "Providers"}))})

             _
             (when-let [t (:error res)]
               (throw t))

             frames
             (mapv #(cap/frame-text res %) (range (count (:frames res))))]

            ;; The dialog is on the terminal BEFORE the gateway is asked …
            (expect (<= 2 (count frames)))
            (expect (str/includes? (first frames) "Settings"))
            (expect (str/includes? (first frames) "Loading providers"))
            (expect (not (str/includes? (first frames) "acme-llm")))
            ;; … and the fleet arrives into that already-painted frame.
            (expect (str/includes? (last frames) "acme-llm"))
            (expect (pos? @gateway-calls))))
        (finally (reset! inventory original) (reset! mcp original-mcp))))))

;; The provider MANAGER is gone: `d` (set default), `c` (clear fallback) and the
;; model page they open are verbs of the band inside Settings, and they must
;; still reach the router — this is what the deleted manager's own test proved.
(defdescribe settings-provider-default-and-fallback-test
             (it "sets the default model from the provider band in Settings"
                 (settings-provider-band-frames 100 30 [:enter \d \a :esc :esc])
                 (expect (= [[:default :acme-llm "acme-1"]] @settings-router-writes)))
             (it "clears the fallback from the same band"
                 ;; The fixture row already holds the fallback tag, so the band
                 ;; offers `c` — Clear Fallback — in place of Set as Fallback.
                 (settings-provider-band-frames 100 30 [:enter \c :esc])
                 (expect (= [[:fallback]] @settings-router-writes))))
