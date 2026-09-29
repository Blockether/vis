(ns com.blockether.vis.internal.provider.service-test
  "Fleet snapshot cache behavior for `configured-providers-cached` (issue #29):
   the footer-frequency read must never re-run the full config enumeration on
   a warm caller, a stale snapshot must refresh OFF the calling thread, and
   every same-process fleet mutation must invalidate the snapshot."
  (:require [lazytest.core :as lt :refer [defdescribe expect it]]
            [com.blockether.vis.internal.session.cancellation :as cancel]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.vendor.github-copilot :as github-copilot]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(defn- rv
  "Resolve a (possibly private) var in the providers namespace."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.provider.service sym))

(defn- await-value
  "Wait for a background refresh to publish its expected snapshot."
  [read expected]
  (loop [attempts 100]
    (let [value (read)]
      (cond (= expected value) value
            (zero? attempts) nil
            :else (do (Thread/sleep 10) (recur (dec attempts)))))))

;; Every fleet mutation fires the router-rebuild hook `loop` registers at load,
;; and rebuilding the shared router enumerates each provider's LIVE `/models`
;; catalog over the network — 20s of real HTTP inside a unit test, and a
;; different 20s on a machine with no route out. The hook firing at all is what
;; `picking-a-default-rebuilds-the-shared-router` asserts, with its own counting
;; hook; everywhere else it is inert. A namespace-level `around-each` context
;; installs the inert hook around every test case.
(lt/set-ns-context! [(lt/around-each [f]
                                     (let [prev (providers/router-rebuild-hook-val)]
                                       (try (providers/set-router-rebuild-hook! (fn []))
                                            (f)
                                            (finally (providers/set-router-rebuild-hook! prev)))))])

(defdescribe
  provider-status-classifies-the-live-auth-verdict
  (it "provider status classifies the live auth verdict"
      (let [limits
            (atom {:provider-id :remote :status :ok :static {} :dynamic {:limits []}})

            registered
            {:provider/status-fn (constantly {:is-authenticated true :source :config})
             :provider/limits-fn (constantly nil)}]

        (with-redefs [registry/provider-by-id
                      (constantly registered)

                      provider-limits/provider-limits
                      (fn [_]
                        @limits)]

          ;; a successful live account check is verified
          (expect (= :verified (:auth-state (providers/provider-status {:id :remote}))))
          ;; an explicit credential rejection is red, not merely signed out
          (reset! limits {:provider-id :remote
                          :status :unauthenticated
                          :static {}
                          :dynamic {:limits [] :note "The provider rejected this token."}})
          (let [status (providers/provider-status {:id :remote})]
            (expect (false? (:is-authenticated status)))
            (expect (= :rejected (:auth-state status)))
            (expect (= "The provider rejected this token." (:error status))))
          ;; a transient limits failure keeps the usable credential but degrades its proof
          (reset! limits {:provider-id :remote
                          :status :error
                          :static {}
                          :dynamic {:limits [] :note "Limits are temporarily unavailable."}
                          :error {:message "upstream timeout"}})
          (let [status (providers/provider-status {:id :remote})]
            (expect (true? (:is-authenticated status)))
            (expect (= :degraded (:auth-state status)))
            (expect (= "Limits are temporarily unavailable." (:warning status))))
          ;; an endpoint that cannot verify credentials remains neutral
          (reset! limits
            {:provider-id :remote :status :unsupported :static {} :dynamic {:limits []}})
          (expect (= :unverified (:auth-state (providers/provider-status {:id :remote}))))))
      ;; a saved credential with no live check is neutral
      (with-redefs [registry/provider-by-id (constantly {:provider/status-fn
                                                         (constantly {:is-authenticated true
                                                                      :source :config})})]
        (let [status (providers/provider-status {:id :remote})]
          (expect (true? (:is-authenticated status)))
          (expect (= :unverified (:auth-state status)))))))

(defdescribe
  cached-provider-status-never-touches-the-network
  (it "cached provider status never touches the network"
      ;; Adding a provider answered only after re-probing every OTHER provider's
      ;; status and quota endpoints, so the API-key box appeared seconds after the
      ;; tap that asked for it. The non-probing read repeats what is already known.
      (let [probes
            (atom 0)

            registered
            {:provider/status-fn (fn []
                                   (swap! probes inc)
                                   {:is-authenticated true :source :config})
             :provider/limits-fn (constantly nil)}]

        (with-redefs [registry/provider-by-id
                      (constantly registered)

                      provider-limits/provider-limits
                      (fn [_]
                        (swap! probes inc)
                        {:provider-id :cached-status-test
                         :status :ok
                         :static {}
                         :dynamic {:limits []}})]

          (providers/forget-provider-status! :cached-status-test)
          ;; nothing known yet: config alone decides, and no callback runs
          (let [status (providers/provider-status-cached {:id :cached-status-test})]
            (expect (false? (:is-authenticated status)))
            (expect (= :unverified (:auth-state status))
                    "unchecked is neither verified nor rejected")
            (expect (zero? @probes)))
          ;; a configured key is trusted without a call, exactly as the live read trusts it
          (let [status (providers/provider-status-cached {:id :cached-status-test
                                                          :api-key "sk-test"})]
            (expect (true? (:is-authenticated status)))
            (expect (= :unverified (:auth-state status)))
            (expect (zero? @probes)))
          ;; the last live verdict stands in for the probe
          (expect (= :verified (:auth-state (providers/provider-status {:id :cached-status-test}))))
          (let [calls @probes]
            (expect (pos? calls))
            (expect (= :verified
                       (:auth-state (providers/provider-status-cached {:id :cached-status-test}))))
            (expect (= calls @probes) "repeating it costs nothing"))
          ;; an auth change drops it instead of repeating a verdict that is now a lie
          (providers/forget-provider-status! :cached-status-test)
          (let [calls @probes]
            (expect (= :unverified
                       (:auth-state (providers/provider-status-cached {:id :cached-status-test}))))
            (expect (= calls @probes)))))))

(defdescribe initial-provider-status-is-neutral-until-a-live-check
             (it "initial provider status is neutral until a live check"
                 (let [saved
                       (providers/initial-provider-status {:id :remote :api-key "saved"})

                       pending
                       (providers/initial-provider-status {:id :remote})]

                   (expect (= true (get saved "is_authenticated")))
                   (expect (= "unverified" (get saved "auth_state")))
                   (expect (= "unverified" (get pending "auth_state")))
                   (expect (= true (get pending "is_loading"))))))

(defdescribe configured-providers-cached-warm-reads-never-re-enumerate
             (it "configured providers cached warm reads never re enumerate"
                 (let [calls
                       (atom 0)

                       fleet
                       [{:id :fake :models [{:name "m1"}]}]]

                   (with-redefs [config/load-config (fn []
                                                      (swap! calls inc)
                                                      {:providers fleet})]
                     (providers/invalidate-configured-providers!)
                     (expect (= fleet (providers/configured-providers-cached))
                             "cold read enumerates synchronously ONCE and returns the real fleet")
                     (expect (= 1 @calls))
                     (dotimes [_ 10]
                       (providers/configured-providers-cached))
                     (expect (= 1 @calls) "warm reads are pure cache hits — no re-enumeration"))
                   (providers/invalidate-configured-providers!))))

(defdescribe
  configured-providers-cached-stale-serves-old-and-refreshes-in-background
  (it "configured providers cached stale serves old and refreshes in background"
      ;; REGRESSION: the TUI footer calls this on the render thread every ~80ms
      ;; frame. The enumeration (~200ms on machines with slow file IO) must NEVER
      ;; run synchronously on a warm caller — a stale snapshot is served as-is
      ;; while ONE background refresh replaces it.
      (let [;; The refresh is held OPEN on this gate instead of a sleep: "in flight" has
            ;; to be a FACT while the single-flight assertion runs. A 200ms bet loses on
            ;; a loaded runner — the refresh finished between the reads, a later read
            ;; found the snapshot stale again and enumerated a second time, and the
            ;; assertion failed for a race the product does not have (CI, macos-latest).
            gate
            (promise)

            calls
            (atom 0)

            fleet
            [{:id :fake :models [{:name "m1"}]}]

            cache
            (rv 'fleet-cache)]

        (with-redefs [config/load-config (fn []
                                           (swap! calls inc)
                                           (deref gate 10000 nil)
                                           {:providers fleet})]
          ;; plant a STALE snapshot
          (reset! @cache {:at 0 :val [{:id :old}]})
          (let [t0 (System/nanoTime)
                stale (providers/configured-providers-cached)
                stale-ms (/ (- (System/nanoTime) t0) 1e6)]

            (expect (= [{:id :old}] stale) "stale read serves the last-known snapshot immediately")
            (expect (< stale-ms 50.0) "stale read must NOT block on the enumeration")
            ;; wait for the ONE background refresh to actually reach the enumeration,
            ;; where the gate now holds it
            (loop [n 0]
              (when (and (zero? @calls) (< n 400)) (Thread/sleep 5) (recur (inc n))))
            ;; every stale read WHILE that refresh is in flight is single-flight
            (dotimes [_ 5]
              (providers/configured-providers-cached))
            (expect (= 1 @calls) "only ONE background refresh runs (single-flight)")
            (deliver gate true)
            ;; Single-flight is asserted ABOVE, while the refresh is provably in
            ;; flight. Once the gate opens that refresh is DONE, so any later read
            ;; is free to start a new one — counting calls here pinned a promise the
            ;; product never made, and lost the bet on a loaded runner.
            (await-value providers/configured-providers-cached fleet)
            (expect (= fleet (providers/configured-providers-cached))
                    "the refreshed snapshot lands")))
        (providers/invalidate-configured-providers!))))

(defdescribe fleet-mutations-invalidate-the-snapshot
             (it "fleet mutations invalidate the snapshot"
                 ;; The issue #29 follow-up: invalidate on change (long TTL stays safe), so a
                 ;; provider add/remove/reorder shows in the footer cycle count immediately.
                 (let [cache (rv 'fleet-cache)]
                   (with-redefs [config/load-global-config-raw (constantly {:providers []})
                                 config/load-config (constantly {:providers []})
                                 config/save-config! (fn [& _]
                                                       nil)
                                 config/reload-config! (constantly nil)]

                     (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
                     (providers/save-providers! [] nil)
                     (expect (nil? @@cache) "save-providers! drops the snapshot"))
                   (with-redefs [config/remove-config-provider! (fn [& _]
                                                                  true)
                                 config/load-global-config-raw (constantly {})
                                 config/load-config (constantly {:providers []})
                                 config/save-config! (fn [& _]
                                                       nil)
                                 config/reload-config! (constantly nil)]

                     (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
                     (providers/remove-provider! :warm nil)
                     (expect (nil? @@cache) "remove-provider! drops the snapshot")))))

;; Regression (user report): extension-owned providers must not be deleted,
;; including providers whose extension exposes an interactive sign-in flow.
(defdescribe removing-a-managed-provider-has-no-side-effects
             (it "removing a managed provider has no side effects"
                 (doseq [auth-fn [nil (fn [_])]]
                   (let [effects (atom [])
                         record! (fn [effect]
                                   (fn [& _]
                                     (swap! effects conj effect)))
                         registered (cond-> {:provider/id :extension-owned
                                             :provider/is-managed true
                                             :provider/logout-fn (record! :logout)}
                                      auth-fn
                                      (assoc :provider/auth-fn auth-fn))]

                     (with-redefs [registry/provider-by-id (constantly registered)
                                   config/remove-config-provider! (record! :remove)
                                   config/suppress-provider! (record! :suppress)
                                   config/reload-config! (record! :reload)
                                   providers/ensure-default-selection! (record! :retag)
                                   providers/rebuild-shared-router! (record! :rebuild)]

                       (let [result (try (providers/remove-provider! :extension-owned :gateway)
                                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                         (expect (= :provider/managed (:type result)))
                         (expect (= :extension-owned (:provider-id result)))
                         (expect (empty? @effects))))))))

(defn- with-machine-config
  "Run `f` against an in-memory machine config `raw` — the string-keyed shape
   `load-global-config-raw` answers — and return that config as it stands
   afterwards. The persisted providers are the WHOLE fleet: no preset detection,
   no registry, no disk, so a fleet mutation is observable as config alone."
  [raw f]
  (let [state (atom raw)]
    (with-redefs [config/load-global-config-raw (fn []
                                                  @state)
                  config/save-config! (fn [raw' & _]
                                        (reset! state raw')
                                        nil)
                  config/reload-config! (constantly nil)
                  config/load-config (fn []
                                       {:providers (vec (get @state "providers"))
                                        :default-provider (get @state "default_provider")
                                        :default-model (get @state "default_model")})
                  config/remove-config-provider!
                  (fn [provider-id & _]
                    (let [before (vec (get @state "providers"))
                          after (vec (remove #(= (keyword (name provider-id)) (:id %)) before))]

                      (swap! state assoc "providers" after)
                      (not= before after)))
                  providers/authenticated-preset-providers (constantly [])]

      (providers/invalidate-configured-providers!)
      (f)
      (let [result @state]
        (providers/invalidate-configured-providers!)
        result))))

;; Regression (user report): the machine's ONLY provider was not its default —
;; nothing was tagged until the user set it by hand — and removing the tagged
;; provider left `default_provider` naming what had just been deleted instead of
;; promoting whoever was left.
(defdescribe
  a-fleet-mutation-retags-the-primary-root
  (it
    "a fleet mutation retags the primary root"
    (let [acme
          {:id :acme :models [{:name "acme-1"}]}

          beta
          {:id :beta :models [{:name "beta-1"}]}

          tagged-acme
          {"providers" [acme] "default_provider" "acme" "default_model" "acme-1"}

          added
          (with-machine-config {} #(providers/save-providers! [acme] nil))

          second-added
          (with-machine-config (assoc tagged-acme "providers" [acme])
                               #(providers/save-providers! [acme beta] nil))

          promoted
          (with-machine-config (assoc tagged-acme "providers" [acme beta])
                               #(providers/remove-provider! :acme nil))

          emptied
          (with-machine-config tagged-acme #(providers/remove-provider! :acme nil))]

      (expect (= "acme" (get added "default_provider"))
              "the first provider a fleet gains IS the default root")
      (expect (= "acme-1" (get added "default_model"))
              "and the root names the model it can actually route to")
      (expect (= "acme" (get second-added "default_provider"))
              "a second provider never steals a default the user already has")
      (expect (= "beta" (get promoted "default_provider"))
              "removing the tagged provider promotes the survivor")
      (expect (= "beta-1" (get promoted "default_model")))
      (expect (nil? (get emptied "default_provider"))
              "and an emptied fleet names nobody rather than a ghost")
      (expect (nil? (get emptied "default_model"))))))

(defdescribe
  picker-fleet-appends-authenticated-but-unconfigured-oauth-providers
  (it "picker fleet appends authenticated but unconfigured oauth providers"
      ;; The model picker must list providers whose OAuth creds live OUTSIDE config
      ;; (token files / keychain) even before they're saved into `:providers` — the
      ;; whole point of `picker-fleet` vs `configured-providers`.
      (let [detected (atom true)]
        (with-redefs [config/load-config (constantly {:providers [{:id :openai
                                                                   :models [{:name "gpt-x"}]}]})
                      config/deleted-provider-ids (constantly #{})
                      registry/registered-providers
                      (constantly [{:provider/id :anthropic-coding-plan
                                    :provider/detect-fn (fn []
                                                          (when @detected {:access-token "tok"}))}
                                   {:provider/id :openai
                                    :provider/detect-fn (fn []
                                                          {:access-token "tok"})}])
                      catalog/template
                      (fn [pid]
                        (when (= pid :anthropic-coding-plan)
                          {:id pid :api-style :anthropic :default-models ["claude-opus-4-8"]}))]

          (providers/invalidate-configured-providers!)
          (let [extra (providers/authenticated-preset-providers)]
            (expect (= [:anthropic-coding-plan] (mapv :id extra))
                    "authenticated OAuth provider not in the fleet is surfaced")
            (expect (= [{:name "claude-opus-4-8"}] (:models (first extra)))
                    "its preset default catalog models are attached"))
          (expect (= [:openai :anthropic-coding-plan] (mapv :id (providers/picker-fleet)))
                  "picker-fleet = configured fleet first, authenticated extras appended")
          ;; No stored creds -> not surfaced.
          (reset! detected false)
          (expect (empty? (providers/authenticated-preset-providers))
                  "a provider with no detected creds is skipped")
          (expect (= [:openai] (mapv :id (providers/picker-fleet))))))
      (providers/invalidate-configured-providers!)))

(defdescribe
  picker-fleet-lists-authenticated-providers-in-canonical-order
  (it "picker fleet lists authenticated providers by preset rank, not registry order"
      ;; The registry keeps providers in a hash map, so pickers used to show authenticated
      ;; providers in hash order, with GitHub Copilot between unrelated providers.
      (let [registered
            (mapv (fn [[id label rank]]
                    (cond-> {:provider/id id
                             :provider/label label
                             :provider/detect-fn (constantly {:access-token "tok"})}
                      rank
                      (assoc :provider/policy {:preset-rank rank})))
                  [[:opencode-go "OpenCode Go" nil] [:zai-coding-plan "Z.ai Coding Plan" 6]
                   [:github-copilot "GitHub Copilot" 4] [:openrouter "OpenRouter" 9]
                   [:anthropic-coding-plan "Anthropic Coding Plan" 2]
                   [:openai-codex "OpenAI Codex" 3]])

            by-id
            (into {} (map (juxt :provider/id identity)) registered)]

        (with-redefs [config/load-config
                      (constantly {:providers [{:id :openrouter :models [{:name "router-x"}]}]})

                      config/deleted-provider-ids
                      (constantly #{})

                      registry/registered-providers
                      (constantly registered)

                      registry/provider-by-id
                      (fn [pid]
                        (get by-id pid))

                      catalog/template
                      (fn [pid]
                        {:id pid :default-models ["model-x"]})]

          (providers/invalidate-configured-providers!)
          (expect (= [:openrouter :anthropic-coding-plan :openai-codex :github-copilot
                      :zai-coding-plan :opencode-go]
                     (mapv :id (providers/picker-fleet)))
                  "configured providers first, then authenticated ones by rank, unranked last")))
      (providers/invalidate-configured-providers!)))

(defdescribe github-copilot-is-one-preset-in-add-provider-picker
             (it "github copilot is one preset in add provider picker"
                 ;; Issues #47/#48 once asked the three GitHub Copilot tiers to sit next to each
                 ;; other in the "Add Provider" picker. The tiers are gone — a seat is what the
                 ;; signed-in account reports, so ONE `:github-copilot` preset covers all of
                 ;; them and the withdrawn per-seat ids must never return as pickable rows.
                 (let [order
                       (do (github-copilot/register!) (mapv :id (catalog/presets)))

                       copilot-presets
                       (filterv #(str/starts-with? (name %) "github-copilot") order)]

                   (expect (= [:github-copilot] copilot-presets)
                           "exactly one GitHub Copilot preset, not one per seat tier")
                   (doseq [seat [:github-copilot-individual :github-copilot-business
                                 :github-copilot-enterprise]]
                     (expect (nil? (catalog/template seat))
                             (str (name seat) " is not offered as a preset"))))))

(defdescribe
  configured-provider-catalog-cannot-be-narrowed
  (it
    "configured provider catalog cannot be narrowed"
    (with-redefs [config/load-config-raw
                  (constantly {"providers" [{"id" "openai"
                                             "models" [{"name" "gpt-custom" "output_limit" 123}]}]})

                  catalog/template
                  (constantly {:id :openai
                               :default-models ["gpt-default" "gpt-custom" "gpt-extra"]})]

      (expect (= [{:name "gpt-custom" :output-limit 123} {:name "gpt-default"} {:name "gpt-extra"}]
                 (:models (first (:providers (config/load-config)))))
              "persisted metadata wins, while every preset model remains available"))))

(defdescribe
  explicit-default-selection-is-order-independent-and-persists-without-reordering
  (it
    "explicit default selection is order independent and persists without reordering"
    (let [fleet
          [{:id :openai :models [{:name "gpt-5"}]}
           {:id :anthropic-coding-plan
            :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]

          saved
          (atom nil)]

      (with-redefs [config/load-config (constantly {:default-provider "anthropic-coding-plan"
                                                    :default-model "claude-fable-5"
                                                    :providers fleet})]
        (expect (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
                   (providers/default-selection fleet))))
      (with-redefs [providers/picker-fleet
                    (constantly fleet)

                    config/load-global-config-raw
                    (constantly {"theme" "dark"
                                 "providers" [{"id" "openai" "models" [{"name" "gpt-5"}]}
                                              {"id" "anthropic-coding-plan"
                                               "models" [{"name" "claude-opus-4-8"}]}]})

                    config/save-config!
                    (fn [wire _]
                      (reset! saved wire))

                    config/reload-config!
                    (constantly nil)]

        (expect
          (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
             (providers/save-default-selection! :anthropic-coding-plan "claude-fable-5" :test)))
        (expect (= "anthropic-coding-plan" (get @saved "default_provider")))
        (expect (= "claude-fable-5" (get @saved "default_model")))
        (expect (= [:openai :anthropic-coding-plan] (mapv :id (get @saved "providers")))
                "choosing a default does not reorder providers")
        (expect (= ["claude-opus-4-8" "claude-fable-5"]
                   (mapv :name (get-in @saved ["providers" 1 :models])))
                "the complete selected-provider catalog is persisted")))))

(defdescribe
  a-live-catalog-model-can-become-the-default
  (it "a live catalog model can become the default"
      ;; The picker lists `model-options` (configured models PLUS the provider's
      ;; live catalog), but the save path validated the choice against the
      ;; configured catalog alone: picking any of the hundreds of live models a
      ;; provider exposes was refused with "Unknown model for provider" and the TUI
      ;; reported "Default rejected". Whatever the picker offers must be selectable,
      ;; and the saved pair must survive the next read.
      (let [fleet
            [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}
             {:id :openrouter :models [{:name "glm-5.2"}]}]

            saved
            (atom nil)]

        (with-redefs [providers/picker-fleet
                      (constantly fleet)

                      providers/fetch-models
                      (fn [provider]
                        (when (= :openrouter (:id provider)) ["z-ai/glm-4.6v"]))

                      catalog/template
                      (constantly nil)

                      config/load-config
                      (constantly {:default-provider "anthropic-coding-plan"
                                   :default-model "claude-fable-5"
                                   :providers fleet})

                      config/load-global-config-raw
                      (constantly {"providers" [{"id" "openrouter" "models" [{"name" "glm-5.2"}]}]})

                      config/save-config!
                      (fn [wire _]
                        (reset! saved wire))

                      config/reload-config!
                      (constantly nil)]

          (expect (= {:provider-id :openrouter :model "z-ai/glm-4.6v"}
                     (providers/save-default-selection! :openrouter "z-ai/glm-4.6v" :test))
                  "a model only the live catalog knows is still a valid default")
          (expect (= "openrouter" (get @saved "default_provider")))
          (expect (= "z-ai/glm-4.6v" (get @saved "default_model")))
          (expect (= ["glm-5.2" "z-ai/glm-4.6v"]
                     (mapv :name (:models (first (get @saved "providers")))))
                  "the chosen model joins the persisted catalog")
          (expect (= {:provider-id :openrouter :model "z-ai/glm-4.6v"}
                     (with-redefs [config/load-config (constantly {:default-provider "openrouter"
                                                                   :default-model "z-ai/glm-4.6v"})]
                       (providers/default-selection
                         [{:id :openrouter :models (:models (first (get @saved "providers")))}])))
                  "so the pair round-trips instead of reverting to the provider's first model")))))

(defdescribe
  model-options-uses-canonical-model-order
  (it "model options uses canonical model order"
      ;; Configured, live and preset ids share svar's canonical order, so every render
      ;; lists the best models first and dated snapshots last. Ids the order does not
      ;; rank keep vis.yml order, and the rest of the live catalog follows, sorted.
      (with-redefs [providers/fetch-models
                    (constantly ["zebra-live" "gpt-4o-2024-08-06" "alpha-live" "claude-opus-5-5"])

                    catalog/template
                    (constantly nil)

                    catalog/model-visible?
                    (constantly true)]

        (let [provider
              {:id :fake
               :models [{:name "zzz-first"} {:name "glm-4.7"} {:name "my-local"}
                        {:name "alpha-live"}]}

              {:keys [models hidden-count]}
              (providers/model-options provider (providers/default-model-names provider) false)]

          (expect
            (= ["claude-opus-5-5" "glm-4.7" "zzz-first" "my-local" "alpha-live" "zebra-live"]
               models)
            "ranked models lead; unranked ids keep vis.yml order, then the live catalog, sorted")
          (expect (= 1 hidden-count) "the dated snapshot is hidden")
          (expect (= ["zzz-first" "glm-4.7" "my-local" "alpha-live"]
                     (providers/configured-model-names provider))
                  "configured names come straight off the provider map, in its order")))))

(defdescribe model-options-accepts-mapped-provider-defaults
             (it "model options accepts mapped provider defaults"
                 (with-redefs [providers/fetch-models
                               (constantly ["zebra-live"])

                               catalog/template
                               (constantly nil)

                               catalog/model-visible?
                               (constantly true)]

                   (let [provider
                         {:id :fake
                          :default-models ["glm-5.2" {:name "minimax-m3" :api-style :anthropic}]}

                         defaults
                         (providers/default-model-names provider)]

                     (expect (= ["glm-5.2" "minimax-m3"] defaults))
                     (expect (= ["minimax-m3" "glm-5.2" "zebra-live"]
                                (:models (providers/model-options provider defaults true))))))))

(defdescribe model-options-leave-hidden-defaults-out
             (it "model options leave hidden defaults out"
                 (with-redefs [providers/fetch-models
                               (constantly nil)

                               catalog/template
                               (constantly nil)]

                   (let [provider {:id :fake
                                   :default-models ["glm-5.2" "mimo-v2.5-pro" "gemini-3-pro-preview"
                                                    "omen-alpha" "glm-4.7"
                                                    "claude-haiku-4-5-20251001" "mimo-v2.6-pro"]}]
                     (expect (= ["mimo-v2.6-pro" "glm-5.2"]
                                (:models (providers/model-options provider
                                                                  (providers/default-model-names
                                                                    provider)
                                                                  true)))
                             "stealth, preview and outdated defaults stay out of the picker")))))

;; Regression: a fleet stayed frozen at the build that added it. `:default-models`
;; is a hardcoded vendor list, adding a provider persisted exactly that list, and
;; every `/v1/router` row reads `:models` from CONFIG — so a model the vendor
;; released later was unreachable in the app for good.
(defdescribe
  refreshing-models-appends-only-what-config-does-not-name
  (it "refreshing models appends only what config does not name"
      (let [entry
            {:id :fake :models [{:name "glm-5.2"} {:name "kimi-k2.6"}]}

            written
            (atom nil)]

        (with-redefs [providers/configured-providers
                      (constantly [entry])

                      providers/fetch-model-catalog
                      (constantly {:identity "test-account"
                                   :models [{:name "glm-5.2"} {:name "glm-5.3"}
                                            {:name "minimax-m2.5"} {:name "minimax-m2.5"}]})

                      catalog/template
                      (constantly {:id :fake
                                   :default-models ["glm-5.3"
                                                    {:name "minimax-m2.5" :api-style :anthropic}]})

                      providers/update-config-provider!
                      (fn [provider-id f source]
                        (reset! written [provider-id (f entry) source]))]

          (let [added (providers/refresh-models! :fake :test)]
            (expect (= ["glm-5.3" "minimax-m2.5"] added)
                    "only the live ids config did not already name")
            (expect (= [:fake :test] [(first @written) (last @written)]))
            (expect
              (= [{:name "glm-5.2"} {:name "kimi-k2.6"} {:name "glm-5.3"}
                  {:name "minimax-m2.5" :api-style :anthropic}]
                 (:models (second @written)))
              "configured models keep their order; a new one carries the preset's own map"))))))

(defdescribe
  refreshing-models-drops-saved-models-svar-hides
  (it "refreshing models drops saved models svar hides"
      (let [entry
            {:id :fake
             :models [{:name "glm-5.2"} {:name "omen-alpha"} "mimo-v2.5-pro" {:name "hy4-preview"}
                      {:name "glm-4.7"} {:name "claude-haiku-4-5-20251001"}
                      {:name "mimo-v2.6-pro"}]}

            written
            (atom nil)]

        (with-redefs [providers/configured-providers
                      (constantly [entry])

                      providers/fetch-model-catalog
                      (constantly {:identity "test-account"
                                   :models [{:name "glm-5.2"} {:name "mimo-v2.6-pro"}]})

                      catalog/template
                      (constantly {:id :fake :default-models []})

                      providers/update-config-provider!
                      (fn [provider-id f source]
                        (reset! written [provider-id (f entry) source]))]

          (expect (= [] (providers/refresh-models! :fake :test)) "no live id is new")
          (expect
            (= [{:name "glm-5.2"} {:name "mimo-v2.6-pro"}] (:models (second @written)))
            "stealth, preview and outdated models leave the saved list; the rest keep order")))))

(defdescribe
  refreshing-a-local-provider-keeps-its-saved-models
  (it "refreshing a local provider keeps its saved models"
      (let [entry
            {:id :ollama :models [{:name "qwen3-coder-30b"} {:name "glm-4.7"}]}

            written
            (atom nil)]

        (with-redefs [providers/configured-providers
                      (constantly [entry])

                      providers/fetch-model-catalog
                      (constantly {:identity "local"
                                   :models [{:name "qwen3-coder-30b"} {:name "glm-4.7"}]})

                      catalog/template
                      (constantly {:id :ollama :default-models []})

                      providers/update-config-provider!
                      (fn [provider-id f source]
                        (reset! written [provider-id (f entry) source]))]

          (expect (= [] (providers/refresh-models! :ollama :test)) "no live id is new")
          (expect (= [{:name "qwen3-coder-30b"} {:name "glm-4.7"}] (:models (second @written)))
                  "Ollama lists every model it serves, so older versions stay saved")))))

(defdescribe a-failed-model-probe-leaves-the-fleet-alone
             (it "a failed model probe leaves the fleet alone"
                 (let [writes (atom 0)]
                   (with-redefs [providers/configured-providers
                                 (constantly [{:id :fake :models [{:name "glm-5.2"}]}])
                                 providers/fetch-model-catalog (constantly nil)
                                 providers/update-config-provider! (fn [& _]
                                                                     (swap! writes inc))]

                     (expect (nil? (providers/refresh-models! :fake))
                             "a probe that answered nothing is not an empty catalog")
                     ;; a provider the fleet does not carry is not written either
                     (expect (nil? (providers/refresh-models! :ghost)))
                     (expect (zero? @writes))))))

(defdescribe
  a-catalog-refresh-is-single-flight-and-windowed
  (it "a catalog refresh is single flight and windowed"
      (let [claim
            (rv 'claim-models-refresh!)

            release
            (rv 'release-models-refresh!)]

        (try
          (expect (true? (claim :fake)) "the first trigger takes the slot")
          (expect (false? (claim :fake)) "a second trigger while the probe is in flight is dropped")
          (release :fake false)
          (expect (true? (claim :fake)) "a probe that FAILED must not burn the window")
          (release :fake true)
          (expect (false? (claim :fake)) "an answered probe holds the window shut")
          (finally (reset! @(rv 'models-refreshing) #{}) (reset! @(rv 'last-models-refresh) {}))))))

(defdescribe
  fallback-selection-is-explicit-and-always-on-another-provider
  (it
    "fallback selection is explicit and always on another provider"
    (let [fleet
          [{:id :openai :models [{:name "gpt-5"}]}
           {:id :anthropic-coding-plan
            :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]

          primary
          {:provider-id :anthropic-coding-plan :model "claude-fable-5"}

          base
          {:default-provider "anthropic-coding-plan"
           :default-model "claude-fable-5"
           :providers fleet}

          saved
          (atom nil)]

      (with-redefs [config/load-config (constantly (assoc base
                                                     :fallback-provider "openai"
                                                     :fallback-model "gpt-5"))]
        (expect (= {:provider-id :openai :model "gpt-5"}
                   (providers/fallback-selection fleet primary))))
      (with-redefs [config/load-config (constantly base)]
        (expect (nil? (providers/fallback-selection fleet primary))
                "an unset tag never invents a second choice"))
      (with-redefs [config/load-config (constantly (assoc base
                                                     :fallback-provider "anthropic-coding-plan"
                                                     :fallback-model "claude-opus-4-8"))]
        (expect (nil? (providers/fallback-selection fleet primary))
                "a tag on the primary's own provider is no fallback at all"))
      (with-redefs [providers/picker-fleet
                    (constantly fleet)

                    config/load-config
                    (constantly base)

                    config/load-global-config-raw
                    (constantly {"default_provider" "anthropic-coding-plan"
                                 "default_model" "claude-fable-5"
                                 "fallback_provider" "stale"
                                 "fallback_model" "stale-1"})

                    config/save-config!
                    (fn [wire _]
                      (reset! saved wire))

                    config/reload-config!
                    (constantly nil)]

        (expect (= {:provider-id :openai :model "gpt-5"}
                   (providers/save-fallback-selection! :openai "gpt-5" :test)))
        (expect (= "openai" (get @saved "fallback_provider")))
        (expect (= "gpt-5" (get @saved "fallback_model")))
        (expect (= "anthropic-coding-plan" (get @saved "default_provider"))
                "tagging the fallback leaves the primary alone")
        (expect
          (= :vis/invalid-fallback-provider
             (try (providers/save-fallback-selection! :anthropic-coding-plan "claude-fable-5" :test)
                  nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
          "the primary's own provider is refused")
        (providers/clear-fallback-selection! :test)
        (expect (nil? (get @saved "fallback_provider")))
        (expect (nil? (get @saved "fallback_model")))))))

(defdescribe
  tagging-a-new-primary-drops-a-fallback-that-would-collide-with-it
  (it
    "tagging a new primary drops a fallback that would collide with it"
    (let [fleet
          [{:id :openai :models [{:name "gpt-5"}]}
           {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]

          saved
          (atom nil)]

      (with-redefs [providers/picker-fleet
                    (constantly fleet)

                    config/load-config
                    (constantly {:default-provider "anthropic-coding-plan"
                                 :default-model "claude-fable-5"
                                 :fallback-provider "openai"
                                 :fallback-model "gpt-5"
                                 :providers fleet})

                    config/load-global-config-raw
                    (constantly {"default_provider" "anthropic-coding-plan"
                                 "default_model" "claude-fable-5"
                                 "fallback_provider" "openai"
                                 "fallback_model" "gpt-5"})

                    config/save-config!
                    (fn [wire _]
                      (reset! saved wire))

                    config/reload-config!
                    (constantly nil)]

        (expect (= {:provider-id :openai :model "gpt-5"}
                   (providers/save-default-selection! :openai "gpt-5" :test)))
        (expect (= "openai" (get @saved "default_provider")))
        (expect (nil? (get @saved "fallback_provider"))
                "the fallback cannot stay on the provider that just became primary")
        (expect (nil? (get @saved "fallback_model")))))))

(defdescribe
  clear-provider-api-key-test
  (it "clear provider api key"
      ;; "Log out" for a key-only provider forgets the CREDENTIAL and nothing else:
      ;; the config entry — models, base-url, tags — has to survive so signing back in
      ;; is one key away (issue #80).
      (let [saved
            (atom nil)

            fleet
            [{:id :zai-coding-plan
              :api-key "sk-live"
              :base-url "https://example.invalid"
              :models [{:name "glm-4.7"}]} {:id :openai :api-key "sk-other"}]

            entry
            (fn [providers id]
              (some #(when (= id (:id %)) %) providers))]

        (with-redefs-fn {#'config/load-global-config-raw (constantly {:providers fleet})
                         (rv 'update-providers!) (fn [f _source]
                                                   ;; The fleet is now read INSIDE the
                                                   ;; locked update, so the stub hands
                                                   ;; it in and records only a change.
                                                   (let [next* (vec (f (vec fleet)))]
                                                     (when (not= (vec fleet) next*)
                                                       (reset! saved next*))
                                                     next*))}
          (fn []
            (expect (= true (providers/clear-provider-api-key! :zai-coding-plan :test)))
            (let [cleared (entry @saved :zai-coding-plan)]
              (expect (nil? (:api-key cleared)))
              (expect (= [{:name "glm-4.7"}] (:models cleared)))
              (expect (= "https://example.invalid" (:base-url cleared))))
            ;; Other providers are untouched…
            (expect (= "sk-other" (:api-key (entry @saved :openai))))
            ;; …and with nothing stored there is no write at all.
            (reset! saved nil)
            (expect (= false (providers/clear-provider-api-key! :unknown-provider :test)))
            (expect (nil? @saved)))))))

(defdescribe reprioritize-providers-renumbers-from-vector-position
             (it "reprioritize providers renumbers from vector position"
                 (let [renumbered (providers/reprioritize-providers
                                    [{:id :a :priority 7} {:id :b :priority 0} {:id :c}])]
                   (expect (= [:a :b :c] (mapv :id renumbered)))
                   (expect (= [0 1 2] (mapv :priority renumbered)))
                   (expect (vector? renumbered))
                   (expect (= [] (providers/reprioritize-providers nil))))))

(defdescribe demote-unreachable-providers-renumbers-the-demoted-provider
             (it "demote unreachable providers renumbers the demoted provider"
                 ;; svar sorts candidates by `:priority`, never by vector position, so a dead
                 ;; local endpoint that keeps `:priority 0` is still its FIRST pick — the health
                 ;; gate sank it in name only and the turn burned minutes against a dead port.
                 (with-redefs [providers/provider-reachable? (fn [provider]
                                                               (not= :lmstudio (:id provider)))]
                   (let [{:keys [router demoted]} (providers/demote-unreachable-providers
                                                    {:providers [{:id :lmstudio :priority 0}
                                                                 {:id :zai-coding-plan
                                                                  :priority 1}]})]
                     (expect (= [:lmstudio] demoted))
                     (expect (= [:zai-coding-plan :lmstudio] (mapv :id (:providers router))))
                     (expect (= [0 1] (mapv :priority (:providers router))))))))

(defdescribe demote-unreachable-providers-leaves-a-healthy-fleet-untouched
             (it "demote unreachable providers leaves a healthy fleet untouched"
                 (with-redefs [providers/provider-reachable? (constantly true)]
                   (let [router {:providers [{:id :lmstudio :priority 0}
                                             {:id :zai-coding-plan :priority 1}]}]
                     (expect (= {:router router :demoted []}
                                (providers/demote-unreachable-providers router)))))))

(defdescribe
  command-minted-provider-test
  (it "command minted provider"
      ;; A provider whose config carries `api_key_command` mints its OWN credential
      ;; on every request. Classifying it as `:api-key` made every channel offer a
      ;; "type your API key" prompt for a credential no human holds — and a typed
      ;; key then silently outranks the helper on the next request.
      (expect (= true (providers/command-minted? {:id :corp :api-key-command "mint-token"})))
      (expect (= false (providers/command-minted? {:id :corp :api-key "sk-1"})))
      (expect (= false (providers/command-minted? nil)))
      (expect (= :command (providers/auth-kind :corp {:id :corp :api-key-command "mint-token"})))
      (expect (= :api-key (providers/auth-kind :corp {:id :corp :api-key "sk-1"})))
      (expect (= :api-key (providers/auth-kind :corp)))
      (expect (= :oauth (providers/auth-kind (first providers/oauth-provider-ids))))
      (expect (= :none (providers/auth-kind (first providers/local-no-auth-provider-ids))))))

;; Regression: the shared static-API-key shape registers an interactive
;; `:provider/auth-fn` only to print key guidance, so inferring `:oauth` from its
;; presence classified every key-only provider as an OAuth sign-in — and the gateway
;; then refused the sign-in it had just advertised.
(defdescribe
  declared-auth-kind-outranks-the-interactive-auth-fn-test
  (it "declared auth kind outranks the interactive auth fn"
      (with-redefs [registry/provider-by-id (constantly {:provider/auth-kind :api-key
                                                         :provider/auth-fn (constantly
                                                                             :no-credentials)})]
        (expect (= :api-key (providers/auth-kind :acme-coding-plan)))
        ;; A machine-minted credential still outranks the declaration: nothing prompts.
        (expect (= :command
                   (providers/auth-kind :acme-coding-plan
                                        {:id :acme-coding-plan :api-key-command "mint-token"}))))
      (with-redefs [registry/provider-by-id (constantly {:provider/auth-kind :oauth
                                                         :provider/is-managed true})]
        (expect (= :oauth (providers/auth-kind :acme-managed-oauth))))
      ;; Nothing declared: the inference underneath it is unchanged.
      (with-redefs [registry/provider-by-id (constantly {:provider/auth-fn (constantly :ok)})]
        (expect (= :oauth (providers/auth-kind :acme-interactive))))))

(defdescribe
  status-report-uses-the-four-state-auth-verdict
  (it "status report uses the four state auth verdict"
      (let [limits
            {:provider-id :slow :status :loading :static {} :dynamic {:limits []}}

            report
            (fn [status]
              [(providers/status-text {:id :slow} status limits)
               (providers/status-md {:id :slow} status limits)])

            [neutral-text neutral-md]
            (report {:is-authenticated true :auth-state :unverified :loading? true})]

        (expect (str/includes? neutral-text "Authenticated: saved, not verified"))
        (expect (not (str/includes? neutral-text "checking")))
        (expect (str/includes? neutral-md "**Authenticated:** saved, not verified ○"))
        (expect (str/includes? (first (report {:is-authenticated true :auth-state :verified}))
                               "Authenticated: verified"))
        (expect (str/includes? (first (report {:is-authenticated false :auth-state :rejected}))
                               "Authenticated: rejected"))
        (expect (str/includes? (first (report {:is-authenticated true :auth-state :degraded}))
                               "Authenticated: usable; live check unavailable")))))

;; Regression, issue #113: a provider lifecycle callback that never returned ran
;; unbounded on the caller's thread, so one wedged extension held the gateway's
;; provider-status request — and the card behind it — open until the HTTP client
;; gave up 30s later.
(defdescribe
  provider-probe-never-runs-unbounded-test
  (it
    "provider probe never runs unbounded"
    (let [gate
          (promise)

          probe
          ;; The contract is the wall, not its production length. A promise keeps the
          ;; callback provably wedged, so 100ms exercises the same timeout without
          ;; adding two seconds to every suite run.
          (fn [provider]
            (with-redefs [providers/probe-timeout-ms 100]
              (deref (cancel/worker-future "provider-probe-test"
                                           #(providers/safe-provider-status provider))
                     8000
                     ::still-running)))

          status
          (probe {:id :wedged
                  :provider/status-fn (fn []
                                        @gate
                                        {:is-authenticated true})})

          detected
          (probe {:id :wedged
                  :provider/detect-fn (fn []
                                        @gate
                                        true)})]

      (deliver gate true)
      (expect (not= ::still-running status))
      (expect (false? (:is-authenticated status)))
      (expect (str/includes? (str (:error status)) "timed out"))
      (expect (not= ::still-running detected))
      (expect (false? (:is-authenticated detected)))
      (expect (str/includes? (str (:error detected)) "timed out")))))

;; Regression, issue 9cc1d0a0-2836-4518-b504-bc9f70eae7c4: `/v1/router` asks EVERY
;; provider for its account limits before the model picker can paint, and that
;; probe had no wall — a single hung endpoint held the whole payload until the
;; app's own 30s request bound aborted it, so changing the model in a session took
;; minutes and often just failed.
(defdescribe
  provider-limits-probe-never-runs-unbounded-test
  (it "provider limits probe never runs unbounded"
      (let [;; The contract is the wall, not its production length. The stand-in
            ;; outlives the test ceiling fivefold without parking the suite for seconds.
            outcome (with-redefs [providers/limits-probe-timeout-ms 100
                                  provider-limits/provider-limits (fn [provider-id]
                                                                    (Thread/sleep 500)
                                                                    {:provider-id provider-id
                                                                     :status :ok
                                                                     :static {}
                                                                     :dynamic {:limits []}})]

                      (let [started (System/nanoTime)
                            value (providers/provider-limits-safe {:id :wedged})]

                        {:value value :elapsed-ms (quot (- (System/nanoTime) started) 1000000)}))]
        (expect (= :error (:status (:value outcome))))
        (expect (str/includes? (str (get-in outcome [:value :error :message])) "timed out"))
        (expect (= [] (get-in outcome [:value :dynamic :limits])))
        (expect (< (long (:elapsed-ms outcome)) 1000)))))

;; Regression, issue #113: bounding the probe moved the callback onto a bare
;; worker thread with no binding conveyance, so a provider callback invoked from
;; inside a LIVE session saw no session at all — `vis.ask` refused with "available
;; only while handling a session", `vis.state`
;; fell back to the process-wide DB, and a jailed spawn was scoped to the process
;; cwd instead of the caller's workspace.
(defdescribe
  provider-probe-keeps-the-callers-session-context-test
  (it "provider probe keeps the callers session context"
      (let [env
            {:session-id "s-probe" :workspace {:root (str (workspace/cwd))}}

            seen
            (extension/with-context {:env env}
                                    (providers/safe-provider-status
                                      {:id :ctx
                                       :provider/status-fn
                                       (fn []
                                         {:is-authenticated true
                                          :session (:session-id extension/*current-environment*)
                                          :root workspace/*workspace-root*})}))

            detected
            (extension/with-context {:env env}
                                    (providers/safe-provider-status
                                      {:id :ctx
                                       :provider/detect-fn #(:session-id
                                                              extension/*current-environment*)}))]

        (expect (= "s-probe" (:session seen)))
        (expect (= (workspace/workspace-root env) (:root seen)))
        (expect (true? (:is-authenticated detected))))))

;; Regression, issue #118: format-status-value fell back to `(str v)` for a
;; nested map status_fn value, so `status-text`/`status-md` printed a raw
;; Clojure map literal (`{"max_budget" 100.0, "spend" 25.49}`) instead of a
;; readable "key: value" line.
(defdescribe status-text-formats-nested-usage-map-readably
             (it "status text formats nested usage map readably"
                 (let [status
                       {"is_authenticated" true
                        "usage" {"max_budget" 100.0 "spend" 25.49 "remaining_requests" 42}}

                       text
                       (providers/status-text {:id :anthropic-coding-plan}
                                              status
                                              {:status :ok :dynamic {:limits []}})]

                   (expect (str/includes?
                             text
                             "Usage: max_budget: 100.0, remaining_requests: 42, spend: 25.49"))
                   (expect (not (str/includes? text "{\"max_budget\""))))))

(defdescribe
  picking-a-default-rebuilds-the-shared-router
  (it "picking a default rebuilds the shared router"
      ;; Regression: changing the default model via the picker persisted config and the
      ;; picker showed the new model, but the shared router-atom (and every session env
      ;; that snapshotted it) kept the OLD root — a new session's first turn ran the
      ;; previous model until the user re-pinned it on the session. A config-affecting
      ;; save must rebuild the shared router the same turn a new session will snapshot.
      (let [fleet
            [{:id :openai :models [{:name "gpt-5"}]}
             {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]

            rebuilt
            (atom 0)

            prev
            (providers/router-rebuild-hook-val)]

        (try (providers/set-router-rebuild-hook! (fn []
                                                   (swap! rebuilt inc)))
             (with-redefs [providers/picker-fleet
                           (constantly fleet)

                           providers/fetch-models
                           (constantly nil)

                           config/load-global-config-raw
                           (constantly {"providers" [{"id" "openai" "models" [{"name" "gpt-5"}]}
                                                     {"id" "anthropic-coding-plan"
                                                      "models" [{"name" "claude-fable-5"}]}]})

                           config/save-config!
                           (fn [_ _])

                           config/reload-config!
                           (constantly nil)]

               (expect (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
                          (providers/save-default-selection! :anthropic-coding-plan
                                                             "claude-fable-5"
                                                             :test)))
               (expect (= 1 @rebuilt) "a default pick rebuilds the shared router")
               (providers/clear-fallback-selection! :test)
               (expect (= 2 @rebuilt) "clearing the fallback rebuilds the shared router")
               (providers/save-providers! fleet :test)
               (expect (= 3 @rebuilt) "a fleet mutation rebuilds the shared router"))
             (finally (providers/set-router-rebuild-hook! prev))))))

;; Regression, issue #165: `is_managed` overrode a declared auth function, so an
;; automatically bound extension provider could never obtain its initial credential.
(defdescribe
  managed-provider-binds-itself-and-is-never-an-add-provider-row
  (it "managed provider binds itself and is never an add provider row"
      ;; MANAGED owns binding and configuration, not necessarily credential issuance.
      ;; Runtime-issued and provider-authenticated variants both bind without a config
      ;; entry and stay out of Add Provider; only the latter may invoke its auth function.
      (let [auth-calls
            (atom 0)

            registered
            [{:provider/id :acme-managed :provider/label "Acme (Managed)" :provider/is-managed true}
             {:provider/id :acme-managed-oauth
              :provider/label "Acme (Managed OAuth)"
              :provider/is-managed true
              :provider/auth-fn (fn [_]
                                  (swap! auth-calls inc))}
             {:provider/id :acme-byo :provider/label "Acme (Own key)"}]]

        (with-redefs [config/load-config
                      (constantly {:providers [{:id :openai :models [{:name "gpt-x"}]}]})

                      registry/registered-providers
                      (constantly registered)

                      registry/provider-by-id
                      (into {} (map (juxt :provider/id identity)) registered)

                      catalog/template
                      (fn [pid]
                        {:id pid :api-style :openai :default-models ["acme-1"]})

                      catalog/presets
                      (constantly [{:id :acme-managed :label "Acme (Managed)"}
                                   {:id :acme-managed-oauth :label "Acme (Managed OAuth)"}
                                   {:id :acme-byo :label "Acme (Own key)"}])]

          (providers/invalidate-configured-providers!)
          (expect (= true (providers/managed? :acme-managed)))
          (expect (= false (providers/managed? :acme-byo)))
          (expect (= :managed (providers/auth-kind :acme-managed)))
          (expect (= :oauth (providers/auth-kind :acme-managed-oauth)))
          (expect (= :api-key (providers/auth-kind :acme-byo)))
          ;; NEITHER has a detect-fn: the managed one binds because it is managed.
          (expect (= [:acme-managed-oauth :acme-managed]
                     (mapv :id (providers/authenticated-preset-providers)))
                  "managed providers bind themselves regardless of authentication kind")
          (expect (= [:openai :acme-managed-oauth :acme-managed]
                     (mapv :id (providers/picker-fleet)))
                  "so the model picker holds them with no Add Provider step")
          (expect (= [:acme-byo] (mapv :id (providers/available-presets)))
                  "and Add Provider only offers the provider a human can actually add")
          (expect (zero? @auth-calls) "fleet, picker, and Add Provider reads never start OAuth")))
      (providers/invalidate-configured-providers!)))

(defdescribe status-label-reads-the-same-for-a-wire-key-and-an-engine-key
             (it "status label reads the same for a wire key and an engine key"
                 ;; The CLI table had its own copy of this label that replaced "-" only, so the
                 ;; SAME status map printed "Plan_name:" in `providers status` and "Plan name:"
                 ;; in the dialog and the markdown card.
                 (let [label @#'providers/status-entry-label]
                   (expect (= "Plan name" (label "plan_name")))
                   (expect (= "Plan name" (label :plan-name))))))

(defdescribe refresh-updates-existing-model-metadata-without-overwriting-config
             (it "refresh updates existing model metadata without overwriting config"
                 (let [entry
                       {:id :custom
                        :api-key "test"
                        :base-url "https://gateway.example.com/v1"
                        :models [{:name "m" :input-limit 50000}]}

                       written
                       (atom nil)]

                   (with-redefs [providers/configured-providers
                                 (constantly [entry])

                                 svar/models!
                                 (constantly [{:id "m"
                                               :context 100000
                                               :input-limit 70000
                                               :output-limit 20000
                                               :tokenizer "cl100k_base"}])

                                 providers/update-config-provider!
                                 (fn [_ f _]
                                   (reset! written (f entry)))]

                     (expect (= [] (providers/refresh-models! :custom :test)))
                     (expect (= (:models entry) (:models @written)))
                     (expect (= 70000 (get-in @written [:model-metadata :models 0 :input-limit])))
                     (expect (= "cl100k_base"
                                (get-in @written [:model-metadata :models 0 :tokenizer])))
                     (expect (string? (get-in @written [:model-metadata :identity])))))))

(defdescribe
  metadata-refresh-retains-last-good-fields-only-for-the-same-account
  (it
    "metadata refresh retains last good fields only for the same account"
    (let [entry
          (atom {:id :custom
                 :models [{:name "m" :input-limit 50000}]
                 :model-metadata
                 {:identity "a"
                  :models
                  [{:name "m" :context 100000 :input-limit 70000 :tokenizer "cl100k_base"}]}})

          catalog
          (atom {:identity "a" :models [{:name "m" :input-limit 60000}]})

          writes
          (atom 0)]

      (with-redefs [providers/configured-providers
                    #(vector @entry)

                    providers/fetch-model-catalog
                    (fn [_]
                      @catalog)

                    providers/update-config-provider!
                    (fn [_ f _]
                      (swap! writes inc)
                      (swap! entry f))]

        (providers/refresh-models! :custom)
        (expect (= {:name "m" :context 100000 :input-limit 60000 :tokenizer "cl100k_base"}
                   (get-in @entry [:model-metadata :models 0])))
        (reset! catalog nil)
        (expect (nil? (providers/refresh-models! :custom)))
        (expect (= 1 @writes))
        (expect (= 100000 (get-in @entry [:model-metadata :models 0 :context])))
        (reset! catalog {:identity "b" :models [{:name "m" :input-limit 80000}]})
        (providers/refresh-models! :custom)
        (expect (= {:name "m" :input-limit 80000} (get-in @entry [:model-metadata :models 0])))
        (expect (= [{:name "m" :input-limit 50000}] (:models @entry)))))))
