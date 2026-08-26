(ns com.blockether.vis.internal.providers-test
  "Fleet snapshot cache behavior for `configured-providers-cached` (issue #29):
   the footer-frequency read must never re-run the full config enumeration on
   a warm caller, a stale snapshot must refresh OFF the calling thread, and
   every same-process fleet mutation must invalidate the snapshot."
  (:require [lazytest.core :as lt]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.cancellation :as cancel]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.workspace :as workspace]))

(defn- rv
  "Resolve a (possibly private) var in the providers namespace."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.providers sym))

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
;; hook; everywhere else it is inert. Lazytest's clojure.test shim has no
;; `use-fixtures`; a namespace-level `around-each` context is the same wrapping.
(lt/set-ns-context! [(lt/around-each [f]
                                     (let [prev (providers/router-rebuild-hook-val)]
                                       (try (providers/set-router-rebuild-hook! (fn []))
                                            (f)
                                            (finally (providers/set-router-rebuild-hook! prev)))))])

(deftest provider-status-classifies-the-live-auth-verdict
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

      (testing "a successful live account check is verified"
        (is (= :verified (:auth-state (providers/provider-status {:id :remote})))))
      (testing "an explicit credential rejection is red, not merely signed out"
        (reset! limits {:provider-id :remote
                        :status :unauthenticated
                        :static {}
                        :dynamic {:limits [] :note "The provider rejected this token."}})
        (let [status (providers/provider-status {:id :remote})]
          (is (false? (:is-authenticated status)))
          (is (= :rejected (:auth-state status)))
          (is (= "The provider rejected this token." (:error status)))))
      (testing "a transient limits failure keeps the usable credential but degrades its proof"
        (reset! limits {:provider-id :remote
                        :status :error
                        :static {}
                        :dynamic {:limits [] :note "Limits are temporarily unavailable."}
                        :error {:message "upstream timeout"}})
        (let [status (providers/provider-status {:id :remote})]
          (is (true? (:is-authenticated status)))
          (is (= :degraded (:auth-state status)))
          (is (= "Limits are temporarily unavailable." (:warning status)))))
      (testing "an endpoint that cannot verify credentials remains neutral"
        (reset! limits {:provider-id :remote :status :unsupported :static {} :dynamic {:limits []}})
        (is (= :unverified (:auth-state (providers/provider-status {:id :remote})))))))
  (testing "a saved credential with no live check is neutral"
    (with-redefs [registry/provider-by-id (constantly {:provider/status-fn (constantly
                                                                             {:is-authenticated true
                                                                              :source :config})})]
      (let [status (providers/provider-status {:id :remote})]
        (is (true? (:is-authenticated status)))
        (is (= :unverified (:auth-state status)))))))

(deftest initial-provider-status-is-neutral-until-a-live-check
  (let [saved
        (providers/initial-provider-status {:id :remote :api-key "saved"})

        pending
        (providers/initial-provider-status {:id :remote})]

    (is (= true (get saved "is_authenticated")))
    (is (= "unverified" (get saved "auth_state")))
    (is (= "unverified" (get pending "auth_state")))
    (is (= true (get pending "is_loading")))))

(deftest configured-providers-cached-warm-reads-never-re-enumerate
  (let [calls
        (atom 0)

        fleet
        [{:id :fake :models [{:name "m1"}]}]]

    (with-redefs [config/load-config (fn []
                                       (swap! calls inc)
                                       {:providers fleet})]
      (providers/invalidate-configured-providers!)
      (is (= fleet (providers/configured-providers-cached))
          "cold read enumerates synchronously ONCE and returns the real fleet")
      (is (= 1 @calls))
      (dotimes [_ 10]
        (providers/configured-providers-cached))
      (is (= 1 @calls) "warm reads are pure cache hits — no re-enumeration"))
    (providers/invalidate-configured-providers!)))

(deftest configured-providers-cached-stale-serves-old-and-refreshes-in-background
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

        (is (= [{:id :old}] stale) "stale read serves the last-known snapshot immediately")
        (is (< stale-ms 50.0) "stale read must NOT block on the enumeration")
        ;; wait for the ONE background refresh to actually reach the enumeration,
        ;; where the gate now holds it
        (loop [n 0]
          (when (and (zero? @calls) (< n 400)) (Thread/sleep 5) (recur (inc n))))
        ;; every stale read WHILE that refresh is in flight is single-flight
        (dotimes [_ 5]
          (providers/configured-providers-cached))
        (is (= 1 @calls) "only ONE background refresh runs (single-flight)")
        (deliver gate true)
        (await-value providers/configured-providers-cached fleet)
        (is (= 1 @calls) "and the refresh that lands is that same one")
        (is (= fleet (providers/configured-providers-cached)) "the refreshed snapshot lands")))
    (providers/invalidate-configured-providers!)))

(deftest fleet-mutations-invalidate-the-snapshot
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
      (is (nil? @@cache) "save-providers! drops the snapshot"))
    (with-redefs [config/remove-config-provider! (fn [& _]
                                                   true)
                  config/load-global-config-raw (constantly {})
                  config/load-config (constantly {:providers []})
                  config/save-config! (fn [& _]
                                        nil)
                  config/reload-config! (constantly nil)]

      (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
      (providers/remove-provider! :warm nil)
      (is (nil? @@cache) "remove-provider! drops the snapshot"))))

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
(deftest a-fleet-mutation-retags-the-primary-root
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

    (is (= "acme" (get added "default_provider"))
        "the first provider a fleet gains IS the default root")
    (is (= "acme-1" (get added "default_model"))
        "and the root names the model it can actually route to")
    (is (= "acme" (get second-added "default_provider"))
        "a second provider never steals a default the user already has")
    (is (= "beta" (get promoted "default_provider"))
        "removing the tagged provider promotes the survivor")
    (is (= "beta-1" (get promoted "default_model")))
    (is (nil? (get emptied "default_provider"))
        "and an emptied fleet names nobody rather than a ghost")
    (is (nil? (get emptied "default_model")))))

(deftest picker-fleet-appends-authenticated-but-unconfigured-oauth-providers
  ;; The model picker must list providers whose OAuth creds live OUTSIDE config
  ;; (token files / keychain) even before they're saved into `:providers` — the
  ;; whole point of `picker-fleet` vs `configured-providers`.
  (let [detected (atom true)]
    (with-redefs [config/load-config (constantly {:providers [{:id :openai
                                                               :models [{:name "gpt-x"}]}]})
                  registry/registered-providers
                  (constantly [{:provider/id :anthropic-coding-plan
                                :provider/detect-fn (fn []
                                                      (when @detected {:access-token "tok"}))}
                               {:provider/id :openai
                                :provider/detect-fn (fn []
                                                      {:access-token "tok"})}])
                  config/provider-template
                  (fn [pid]
                    (when (= pid :anthropic-coding-plan)
                      {:id pid :api-style :anthropic :default-models ["claude-opus-4-8"]}))]

      (providers/invalidate-configured-providers!)
      (let [extra (providers/authenticated-preset-providers)]
        (is (= [:anthropic-coding-plan] (mapv :id extra))
            "authenticated OAuth provider not in the fleet is surfaced")
        (is (= [{:name "claude-opus-4-8"}] (:models (first extra)))
            "its preset default catalog models are attached"))
      (is (= [:openai :anthropic-coding-plan] (mapv :id (providers/picker-fleet)))
          "picker-fleet = configured fleet first, authenticated extras appended")
      ;; No stored creds -> not surfaced.
      (reset! detected false)
      (is (empty? (providers/authenticated-preset-providers))
          "a provider with no detected creds is skipped")
      (is (= [:openai] (mapv :id (providers/picker-fleet))))))
  (providers/invalidate-configured-providers!))

(deftest github-copilot-presets-are-contiguous-in-add-provider-picker
  ;; Issue #47/#48: the three GitHub Copilot tiers must sit next to each other
  ;; in the "Add Provider" picker. `:github-copilot-enterprise` was missing from
  ;; PRESET_ORDER, so it sorted to Long/MAX_VALUE at the end — split from
  ;; business/individual by zai/mistral (top, middle, then stranded at the
  ;; bottom). Guard the whole family stays a single contiguous run.
  (let [order
        @(ns-resolve 'com.blockether.vis.internal.config 'PRESET_ORDER)

        idxs
        (mapv #(.indexOf ^java.util.List order %)
              [:github-copilot-business :github-copilot-individual :github-copilot-enterprise])]

    (is (every? nat-int? idxs) "all three Copilot tiers are listed in PRESET_ORDER")
    (let [sorted (sort idxs)]
      (is (= sorted (range (first sorted) (inc (last sorted))))
          "the three Copilot tiers form one contiguous run — no other preset splits them"))))

(deftest configured-provider-catalog-cannot-be-narrowed
  (with-redefs [config/load-config-raw
                (constantly {"providers" [{"id" "openai"
                                           "models" [{"name" "gpt-custom" "output_limit" 123}]}]})

                config/provider-template
                (constantly {:id :openai :default-models ["gpt-default" "gpt-custom" "gpt-extra"]})]

    (is (= [{:name "gpt-custom" :output-limit 123} {:name "gpt-default"} {:name "gpt-extra"}]
           (:models (first (:providers (config/load-config)))))
        "persisted metadata wins, while every preset model remains available")))

(deftest explicit-default-selection-is-order-independent-and-persists-without-reordering
  (let [fleet
        [{:id :openai :models [{:name "gpt-5"}]}
         {:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]

        saved
        (atom nil)]

    (with-redefs [config/load-config (constantly {:default-provider "anthropic-coding-plan"
                                                  :default-model "claude-fable-5"
                                                  :providers fleet})]
      (is (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
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

      (is (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
             (providers/save-default-selection! :anthropic-coding-plan "claude-fable-5" :test)))
      (is (= "anthropic-coding-plan" (get @saved "default_provider")))
      (is (= "claude-fable-5" (get @saved "default_model")))
      (is (= [:openai :anthropic-coding-plan] (mapv :id (get @saved "providers")))
          "choosing a default does not reorder providers")
      (is (= ["claude-opus-4-8" "claude-fable-5"]
             (mapv :name (get-in @saved ["providers" 1 :models])))
          "the complete selected-provider catalog is persisted"))))

(deftest a-live-catalog-model-can-become-the-default
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

                  config/provider-template
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

      (is (= {:provider-id :openrouter :model "z-ai/glm-4.6v"}
             (providers/save-default-selection! :openrouter "z-ai/glm-4.6v" :test))
          "a model only the live catalog knows is still a valid default")
      (is (= "openrouter" (get @saved "default_provider")))
      (is (= "z-ai/glm-4.6v" (get @saved "default_model")))
      (is (= ["glm-5.2" "z-ai/glm-4.6v"] (mapv :name (:models (first (get @saved "providers")))))
          "the chosen model joins the persisted catalog")
      (is (= {:provider-id :openrouter :model "z-ai/glm-4.6v"}
             (with-redefs [config/load-config (constantly {:default-provider "openrouter"
                                                           :default-model "z-ai/glm-4.6v"})]
               (providers/default-selection
                 [{:id :openrouter :models (:models (first (get @saved "providers")))}])))
          "so the pair round-trips instead of reverting to the provider's first model"))))

(deftest model-options-keeps-vis-yml-order
  ;; A hand-written `models:` list is an ORDER, not a set. Sorting every id
  ;; alphabetically reshuffled the user's fleet on every render (and moved the
  ;; intended default off the top). Configured models lead, in file order; the
  ;; live catalog is appended after them, sorted.
  (with-redefs [providers/fetch-models
                (constantly ["zebra-live" "alpha-live"])

                config/provider-template
                (constantly nil)

                config/provider-model-visible?
                (constantly true)]

    (let [provider
          {:id :fake :models [{:name "zzz-first"} {:name "my-local"} {:name "alpha-live"}]}

          {:keys [models]}
          (providers/model-options provider (providers/default-model-names provider) true)]

      (is (= ["zzz-first" "my-local" "alpha-live" "zebra-live"] models)
          "vis.yml order is preserved verbatim; catalog-only ids follow, sorted")
      (is (= ["zzz-first" "my-local" "alpha-live"] (providers/configured-model-names provider))
          "configured names come straight off the provider map, in file order"))))

(deftest model-options-accepts-mapped-provider-defaults
  (with-redefs [providers/fetch-models
                (constantly ["ox-alpha-free"])

                config/provider-template
                (constantly nil)

                config/provider-model-visible?
                (constantly true)]

    (let [provider
          {:id :fake :default-models ["glm-5.2" {:name "minimax-m3" :api-style :anthropic}]}

          defaults
          (providers/default-model-names provider)]

      (is (= ["glm-5.2" "minimax-m3"] defaults))
      (is (= ["glm-5.2" "minimax-m3" "ox-alpha-free"]
             (:models (providers/model-options provider defaults true)))))))
(deftest fallback-selection-is-explicit-and-always-on-another-provider
  (let [fleet
        [{:id :openai :models [{:name "gpt-5"}]}
         {:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]

        primary
        {:provider-id :anthropic-coding-plan :model "claude-fable-5"}

        base
        {:default-provider "anthropic-coding-plan" :default-model "claude-fable-5" :providers fleet}

        saved
        (atom nil)]

    (with-redefs [config/load-config (constantly (assoc base
                                                   :fallback-provider "openai"
                                                   :fallback-model "gpt-5"))]
      (is (= {:provider-id :openai :model "gpt-5"} (providers/fallback-selection fleet primary))))
    (with-redefs [config/load-config (constantly base)]
      (is (nil? (providers/fallback-selection fleet primary))
          "an unset tag never invents a second choice"))
    (with-redefs [config/load-config (constantly (assoc base
                                                   :fallback-provider "anthropic-coding-plan"
                                                   :fallback-model "claude-opus-4-8"))]
      (is (nil? (providers/fallback-selection fleet primary))
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

      (is (= {:provider-id :openai :model "gpt-5"}
             (providers/save-fallback-selection! :openai "gpt-5" :test)))
      (is (= "openai" (get @saved "fallback_provider")))
      (is (= "gpt-5" (get @saved "fallback_model")))
      (is (= "anthropic-coding-plan" (get @saved "default_provider"))
          "tagging the fallback leaves the primary alone")
      (is (= :vis/invalid-fallback-provider
             (try (providers/save-fallback-selection! :anthropic-coding-plan "claude-fable-5" :test)
                  nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
          "the primary's own provider is refused")
      (providers/clear-fallback-selection! :test)
      (is (nil? (get @saved "fallback_provider")))
      (is (nil? (get @saved "fallback_model"))))))

(deftest tagging-a-new-primary-drops-a-fallback-that-would-collide-with-it
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

      (is (= {:provider-id :openai :model "gpt-5"}
             (providers/save-default-selection! :openai "gpt-5" :test)))
      (is (= "openai" (get @saved "default_provider")))
      (is (nil? (get @saved "fallback_provider"))
          "the fallback cannot stay on the provider that just became primary")
      (is (nil? (get @saved "fallback_model"))))))

(deftest clear-provider-api-key-test
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
        (is (= true (providers/clear-provider-api-key! :zai-coding-plan :test)))
        (let [cleared (entry @saved :zai-coding-plan)]
          (is (nil? (:api-key cleared)))
          (is (= [{:name "glm-4.7"}] (:models cleared)))
          (is (= "https://example.invalid" (:base-url cleared))))
        ;; Other providers are untouched…
        (is (= "sk-other" (:api-key (entry @saved :openai))))
        ;; …and with nothing stored there is no write at all.
        (reset! saved nil)
        (is (= false (providers/clear-provider-api-key! :unknown-provider :test)))
        (is (nil? @saved))))))

(deftest reprioritize-providers-renumbers-from-vector-position
  (let [renumbered (providers/reprioritize-providers [{:id :a :priority 7} {:id :b :priority 0}
                                                      {:id :c}])]
    (is (= [:a :b :c] (mapv :id renumbered)))
    (is (= [0 1 2] (mapv :priority renumbered)))
    (is (vector? renumbered))
    (is (= [] (providers/reprioritize-providers nil)))))

(deftest demote-unreachable-providers-renumbers-the-demoted-provider
  ;; svar sorts candidates by `:priority`, never by vector position, so a dead
  ;; local endpoint that keeps `:priority 0` is still its FIRST pick — the health
  ;; gate sank it in name only and the turn burned minutes against a dead port.
  (with-redefs [providers/provider-reachable? (fn [provider]
                                                (not= :lmstudio (:id provider)))]
    (let [{:keys [router demoted]} (providers/demote-unreachable-providers
                                     {:providers [{:id :lmstudio :priority 0}
                                                  {:id :zai-coding-plan :priority 1}]})]
      (is (= [:lmstudio] demoted))
      (is (= [:zai-coding-plan :lmstudio] (mapv :id (:providers router))))
      (is (= [0 1] (mapv :priority (:providers router)))))))

(deftest demote-unreachable-providers-leaves-a-healthy-fleet-untouched
  (with-redefs [providers/provider-reachable? (constantly true)]
    (let [router {:providers [{:id :lmstudio :priority 0} {:id :zai-coding-plan :priority 1}]}]
      (is (= {:router router :demoted []} (providers/demote-unreachable-providers router))))))

(deftest command-minted-provider-test
  ;; A provider whose config carries `api_key_command` mints its OWN credential
  ;; on every request. Classifying it as `:api-key` made every channel offer a
  ;; "type your API key" prompt for a credential no human holds — and a typed
  ;; key then silently outranks the helper on the next request.
  (is (= true (providers/command-minted? {:id :corp :api-key-command "mint-token"})))
  (is (= false (providers/command-minted? {:id :corp :api-key "sk-1"})))
  (is (= false (providers/command-minted? nil)))
  (is (= :command (providers/auth-kind :corp {:id :corp :api-key-command "mint-token"})))
  (is (= :api-key (providers/auth-kind :corp {:id :corp :api-key "sk-1"})))
  (is (= :api-key (providers/auth-kind :corp)))
  (is (= :oauth (providers/auth-kind (first providers/oauth-provider-ids))))
  (is (= :none (providers/auth-kind (first providers/local-no-auth-provider-ids)))))

(deftest status-report-uses-the-four-state-auth-verdict
  (let [limits
        {:provider-id :slow :status :loading :static {} :dynamic {:limits []}}

        report
        (fn [status]
          [(providers/status-text {:id :slow} status limits)
           (providers/status-md {:id :slow} status limits)])

        [neutral-text neutral-md]
        (report {:is-authenticated true :auth-state :unverified :loading? true})]

    (is (str/includes? neutral-text "Authenticated: saved, not verified"))
    (is (not (str/includes? neutral-text "checking")))
    (is (str/includes? neutral-md "**Authenticated:** saved, not verified ○"))
    (is (str/includes? (first (report {:is-authenticated true :auth-state :verified}))
                       "Authenticated: verified"))
    (is (str/includes? (first (report {:is-authenticated false :auth-state :rejected}))
                       "Authenticated: rejected"))
    (is (str/includes? (first (report {:is-authenticated true :auth-state :degraded}))
                       "Authenticated: usable; live check unavailable"))))

;; Regression, issue #113: a provider lifecycle callback that never returned ran
;; unbounded on the caller's thread, so one wedged extension held the gateway's
;; provider-status request — and the card behind it — open until the HTTP client
;; gave up 30s later.
(deftest provider-probe-never-runs-unbounded-test
  (let [gate
        (promise)

        probe
        ;; The production ceiling is 5s, and proving a wedged callback is walled off
        ;; costs that ceiling in wall-clock — twice, once per callback kind. The
        ;; contract under test is the WALL, not its length, so shrink it: 10s of
        ;; sleeping became 2s.
        (fn [provider]
          (with-redefs [providers/probe-timeout-ms 1000]
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
    (is (not= ::still-running status))
    (is (false? (:is-authenticated status)))
    (is (str/includes? (str (:error status)) "timed out"))
    (is (not= ::still-running detected))
    (is (false? (:is-authenticated detected)))
    (is (str/includes? (str (:error detected)) "timed out"))))

;; Regression, issue 9cc1d0a0-2836-4518-b504-bc9f70eae7c4: `/v1/router` asks EVERY
;; provider for its account limits before the model picker can paint, and that
;; probe had no wall — a single hung endpoint held the whole payload until the
;; app's own 30s request bound aborted it, so changing the model in a session took
;; minutes and often just failed.
(deftest provider-limits-probe-never-runs-unbounded-test
  (let [;; The contract under test is the WALL, not its length, so shrink the
        ;; production ceiling instead of sleeping through it. The stand-in still
        ;; outlives that ceiling by an order of magnitude, which is what an
        ;; unbounded probe used to hand straight to the client.
        outcome
        (with-redefs [providers/limits-probe-timeout-ms 250
                      provider-limits/provider-limits
                      (fn [provider-id]
                        (Thread/sleep 3000)
                        {:provider-id provider-id :status :ok :static {} :dynamic {:limits []}})]

          (let [started (System/nanoTime)
                value (providers/provider-limits-safe {:id :wedged})]

            {:value value :elapsed-ms (quot (- (System/nanoTime) started) 1000000)}))]
    (is (= :error (:status (:value outcome))))
    (is (str/includes? (str (get-in outcome [:value :error :message])) "timed out"))
    (is (= [] (get-in outcome [:value :dynamic :limits])))
    (is (< (long (:elapsed-ms outcome)) 2000))))

;; Regression, issue #113: bounding the probe moved the callback onto a bare
;; worker thread with no binding conveyance, so a provider callback invoked from
;; inside a LIVE session saw no session at all — `vis.jailed_shell_session` and
;; `vis.ask` refused with "available only while handling a session", `vis.state`
;; fell back to the process-wide DB, and a jailed spawn was scoped to the process
;; cwd instead of the caller's workspace.
(deftest provider-probe-keeps-the-callers-session-context-test
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

    (is (= "s-probe" (:session seen)))
    (is (= (workspace/workspace-root env) (:root seen)))
    (is (true? (:is-authenticated detected)))))

;; Regression, issue #118: format-status-value fell back to `(str v)` for a
;; nested map status_fn value, so `status-text`/`status-md` printed a raw
;; Clojure map literal (`{"max_budget" 100.0, "spend" 25.49}`) instead of a
;; readable "key: value" line.
(deftest status-text-formats-nested-usage-map-readably
  (let [status
        {"is_authenticated" true "usage" {"max_budget" 100.0 "spend" 25.49 "remaining_requests" 42}}

        text
        (providers/status-text {:id :anthropic-coding-plan}
                               status
                               {:status :ok :dynamic {:limits []}})]

    (is (str/includes? text "Usage: max_budget: 100.0, remaining_requests: 42, spend: 25.49"))
    (is (not (str/includes? text "{\"max_budget\"")))))

(deftest picking-a-default-rebuilds-the-shared-router
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

           (is (=
                 {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
                 (providers/save-default-selection! :anthropic-coding-plan "claude-fable-5" :test)))
           (is (= 1 @rebuilt) "a default pick rebuilds the shared router")
           (providers/clear-fallback-selection! :test)
           (is (= 2 @rebuilt) "clearing the fallback rebuilds the shared router")
           (providers/save-providers! fleet :test)
           (is (= 3 @rebuilt) "a fleet mutation rebuilds the shared router"))
         (finally (providers/set-router-rebuild-hook! prev)))))

(deftest managed-provider-binds-itself-and-is-never-an-add-provider-row
  ;; A MANAGED provider (`:provider/is-managed`) has its credential ISSUED by the
  ;; runtime — a corporate gateway, a device policy — so there is nothing local
  ;; to detect and nothing for a human to type. Without the flag such a provider
  ;; was invisible until someone ran "Add provider", whose only second act is to
  ;; collect an API key that every seam below has to refuse.
  (let [registered
        [{:provider/id :acme-managed :provider/label "Acme (Managed)" :provider/is-managed true}
         {:provider/id :acme-byo :provider/label "Acme (Own key)"}]]
    (with-redefs [config/load-config (constantly {:providers [{:id :openai
                                                               :models [{:name "gpt-x"}]}]})
                  registry/registered-providers (constantly registered)
                  registry/provider-by-id (into {} (map (juxt :provider/id identity)) registered)
                  config/provider-template
                  (fn [pid]
                    {:id pid :api-style :openai :default-models ["acme-1"]})
                  config/provider-presets (constantly [{:id :acme-managed :label "Acme (Managed)"}
                                                       {:id :acme-byo :label "Acme (Own key)"}])]

      (providers/invalidate-configured-providers!)
      (is (= true (providers/managed? :acme-managed)))
      (is (= false (providers/managed? :acme-byo)))
      (is (= :managed (providers/auth-kind :acme-managed)))
      (is (= :api-key (providers/auth-kind :acme-byo)))
      ;; NEITHER has a detect-fn: the managed one binds because it is managed.
      (is (= [:acme-managed] (mapv :id (providers/authenticated-preset-providers)))
          "a managed provider binds itself, an unmanaged one without creds does not")
      (is (= [:openai :acme-managed] (mapv :id (providers/picker-fleet)))
          "so the model picker holds it with no Add Provider step")
      (is (= [:acme-byo] (mapv :id (providers/available-presets)))
          "and Add Provider only offers the provider a human can actually add")))
  (providers/invalidate-configured-providers!))

(deftest status-label-reads-the-same-for-a-wire-key-and-an-engine-key
  ;; The CLI table had its own copy of this label that replaced "-" only, so the
  ;; SAME status map printed "Plan_name:" in `providers status` and "Plan name:"
  ;; in the dialog and the markdown card.
  (let [label @#'providers/status-entry-label]
    (is (= "Plan name" (label "plan_name")))
    (is (= "Plan name" (label :plan-name)))))
