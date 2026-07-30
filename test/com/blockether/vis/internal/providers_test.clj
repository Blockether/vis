(ns com.blockether.vis.internal.providers-test
  "Fleet snapshot cache behavior for `configured-providers-cached` (issue #29):
   the footer-frequency read must never re-run the full config enumeration on
   a warm caller, a stale snapshot must refresh OFF the calling thread, and
   every same-process fleet mutation must invalidate the snapshot."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.registry :as registry]))

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

(deftest configured-providers-cached-warm-reads-never-re-enumerate
  (let
    [calls
     (atom 0)

     fleet
     [{:id :fake :models [{:name "m1"}]}]]

    (with-redefs
      [config/load-config (fn []
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
  (let
    [slow-ms
     200

     calls
     (atom 0)

     fleet
     [{:id :fake :models [{:name "m1"}]}]

     cache
     (rv 'fleet-cache)]

    (with-redefs
      [config/load-config (fn []
                            (swap! calls inc)
                            (Thread/sleep slow-ms)
                            {:providers fleet})]
      ;; plant a STALE snapshot
      (reset! @cache {:at 0 :val [{:id :old}]})
      (let
        [t0 (System/nanoTime)
         stale (providers/configured-providers-cached)
         stale-ms (/ (- (System/nanoTime) t0) 1e6)]

        (is (= [{:id :old}] stale) "stale read serves the last-known snapshot immediately")
        (is (< stale-ms 50.0) "stale read must NOT block on the enumeration")
        ;; several stale reads while the refresh is in flight stay single-flight
        (dotimes [_ 5]
          (providers/configured-providers-cached))
        (await-value providers/configured-providers-cached fleet)
        (is (= 1 @calls) "only ONE background refresh runs (single-flight)")
        (is (= fleet (providers/configured-providers-cached)) "the refreshed snapshot lands")))
    (providers/invalidate-configured-providers!)))

(deftest fleet-mutations-invalidate-the-snapshot
  ;; The issue #29 follow-up: invalidate on change (long TTL stays safe), so a
  ;; provider add/remove/reorder shows in the footer cycle count immediately.
  (let [cache (rv 'fleet-cache)]
    (with-redefs
      [config/load-global-config-raw (constantly {:providers []})
       config/save-config! (fn [& _]
                             nil)
       config/reload-config! (constantly nil)]

      (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
      (providers/save-providers! [] nil)
      (is (nil? @@cache) "save-providers! drops the snapshot"))
    (with-redefs
      [config/remove-config-provider! (fn [& _]
                                        true)
       config/reload-config! (constantly nil)]

      (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
      (providers/remove-provider! :warm nil)
      (is (nil? @@cache) "remove-provider! drops the snapshot"))))

(deftest picker-fleet-appends-authenticated-but-unconfigured-oauth-providers
  ;; The model picker must list providers whose OAuth creds live OUTSIDE config
  ;; (token files / keychain) even before they're saved into `:providers` — the
  ;; whole point of `picker-fleet` vs `configured-providers`.
  (let [detected (atom true)]
    (with-redefs
      [config/load-config (constantly {:providers [{:id :openai :models [{:name "gpt-x"}]}]})
       registry/registered-providers (constantly [{:provider/id :anthropic-coding-plan
                                                   :provider/detect-fn (fn []
                                                                         (when @detected
                                                                           {:access-token "tok"}))}
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
  (let
    [order
     @(ns-resolve 'com.blockether.vis.internal.config 'PRESET_ORDER)

     idxs
     (mapv #(.indexOf ^java.util.List order %)
           [:github-copilot-business :github-copilot-individual :github-copilot-enterprise])]

    (is (every? nat-int? idxs) "all three Copilot tiers are listed in PRESET_ORDER")
    (let [sorted (sort idxs)]
      (is (= sorted (range (first sorted) (inc (last sorted))))
          "the three Copilot tiers form one contiguous run — no other preset splits them"))))

(deftest configured-provider-catalog-cannot-be-narrowed
  (with-redefs
    [config/load-config-raw
     (constantly {"providers" [{"id" "openai"
                                "models" [{"name" "gpt-custom" "output_limit" 123}]}]})

     config/provider-template
     (constantly {:id :openai :default-models ["gpt-default" "gpt-custom" "gpt-extra"]})]

    (is (= [{:name "gpt-custom" :output-limit 123} {:name "gpt-default"} {:name "gpt-extra"}]
           (:models (first (:providers (config/load-config)))))
        "persisted metadata wins, while every preset model remains available")))

(deftest explicit-default-selection-is-order-independent-and-persists-without-reordering
  (let
    [fleet
     [{:id :openai :models [{:name "gpt-5"}]}
      {:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]

     saved
     (atom nil)]

    (with-redefs
      [config/load-config (constantly {:default-provider "anthropic-coding-plan"
                                       :default-model "claude-fable-5"
                                       :providers fleet})]
      (is (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
             (providers/default-selection fleet))))
    (with-redefs
      [providers/picker-fleet
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

(deftest model-options-keeps-vis-yml-order
  ;; A hand-written `models:` list is an ORDER, not a set. Sorting every id
  ;; alphabetically reshuffled the user's fleet on every render (and moved the
  ;; intended default off the top). Configured models lead, in file order; the
  ;; live catalog is appended after them, sorted.
  (with-redefs
    [providers/fetch-models
     (constantly ["zebra-live" "alpha-live"])

     config/provider-template
     (constantly nil)

     config/provider-model-visible?
     (constantly true)]

    (let
      [provider
       {:id :fake :models [{:name "zzz-first"} {:name "my-local"} {:name "alpha-live"}]}

       {:keys [models]}
       (providers/model-options provider (providers/default-model-names provider) true)]

      (is (= ["zzz-first" "my-local" "alpha-live" "zebra-live"] models)
          "vis.yml order is preserved verbatim; catalog-only ids follow, sorted")
      (is (= ["zzz-first" "my-local" "alpha-live"] (providers/configured-model-names provider))
          "configured names come straight off the provider map, in file order"))))
