(ns com.blockether.vis.internal.config-test
  "Coverage for the Vis-side config helpers. Pure helpers (`router-opts`) plus a
   disk-isolated round-trip for `save-config!`/`load-config` — the persistence
   the first-run welcome and the provider manager both rely on (a connected
   provider must survive a restart)."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.config-spec :as config-spec]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe it expect]]
            [taoensso.telemere :as tel]
            [taoensso.trove :as trove]))

(defdescribe
  router-opts-test
  "`router-opts` extracts the `:router` block from a Vis config map and
   trims it to the keys `svar/make-router`'s opts arity understands.
   Without this passthrough, every Vis-built router would silently fall
   back to svar defaults — the user's `:rate-limit` config block would
   be inert."
  (it "returns {} when no `:router` block is present"
      (expect (= {} (config/router-opts {})))
      (expect (= {} (config/router-opts {:providers []}))))
  (it "returns {} when `:router` is non-map"
      (expect (= {} (config/router-opts {:router nil})))
      (expect (= {} (config/router-opts {:router "string"}))))
  (it "passes through `:rate-limit` verbatim"
      (let
        [block {:same-provider-delays-ms [2000 3000 6000]
                :fallback-after-ms 30000
                :respect-retry-after? true
                :fallback-provider? true}]
        (expect (= {:rate-limit block} (config/router-opts {:router {:rate-limit block}})))))
  (it "passes through `:network`, `:budget`, `:tokens`, and CB knobs"
      (let
        [cfg {:router {:network {:timeout-ms 600000 :idle-timeout-ms 60000}
                       :budget {:max-tokens 1000000 :max-cost 5.0}
                       :tokens {:check-context? false}
                       :failure-threshold 10
                       :recovery-ms 30000}}]
        (expect (= (:router cfg) (config/router-opts cfg)))))
  (it "drops unknown keys so future config additions don't crash make-router"
      (let
        [cfg {:router
              {:rate-limit {:fallback-after-ms 1} :totally-made-up-key :whatever :another :nope}}]
        (expect (= {:rate-limit {:fallback-after-ms 1}} (config/router-opts cfg)))))
  (it "ignores top-level config keys outside `:router`"
      (let
        [cfg {:providers [{:id :p1}]
              :db-spec {:backend :sqlite}
              :router {:rate-limit {:fallback-after-ms 1}}}]
        (expect (= {:rate-limit {:fallback-after-ms 1}} (config/router-opts cfg))))))

(defdescribe
  ->svar-provider-test
  "`->svar-provider` coerces a vis provider map to svar-native shape. Local
   no-auth presets (ollama, lmstudio) carry no user credential, but svar's
   `models!` sends the catalog api-key as an HTTP header — a nil value throws
   a null-header NPE, which surfaced as empty LM Studio / Ollama model lists.
   So the coercion must forward svar's catalog dummy key for these presets."
  (it "forwards svar's catalog api-key for local no-auth presets (lmstudio/ollama)"
      (expect (= "lmstudio"
                 (:api-key (config/->svar-provider {:id :lmstudio
                                                    :base-url "http://localhost:1234/v1"
                                                    :models [{:name "probe"}]}))))
      (expect (= "ollama"
                 (:api-key (config/->svar-provider {:id :ollama
                                                    :base-url "http://localhost:11434/v1"
                                                    :models [{:name "probe"}]})))))
  (it "prefers an explicitly configured api-key over the catalog fallback"
      (expect (= "user-key"
                 (:api-key (config/->svar-provider
                             {:id :lmstudio :api-key "user-key" :models [{:name "probe"}]})))))
  (it "leaves cloud presets keyless when none is configured (no catalog dummy)"
      ;; Hermetic: a DEVELOPER machine may hold a real OpenRouter credential
      ;; (env/keychain), and the registry token fn would resolve it — the claim
      ;; under test is only that no CATALOG dummy key is invented for a cloud
      ;; preset, so the registry lookup is stubbed out.
      (with-redefs [registry/provider-by-id (constantly nil)]
        (expect (nil? (:api-key (config/->svar-provider {:id :openrouter
                                                         :models [{:name "probe"}]})))))))

(defdescribe
  svar-model-metadata-test
  (it "lets svar retain GLM-5.2's catalog-native effort style and values"
      (let
        [provider
         (config/->svar-provider {:id :zai-coding-plan :api-key "test" :models [{:name "glm-5.2"}]})

         model
         (-> (svar/make-router [provider])
             :providers
             first
             :models
             first)]

        (expect (= :zai-effort (:reasoning-style model)))
        (expect (= [{:type "effort" :values ["high" "max"]}] (:reasoning-options model)))))
  (it "does not stamp a broad Z.ai reasoning override in Vis"
      (expect (= {:name "glm-5.2"} (config/->svar-model :zai-coding-plan {:name "glm-5.2"})))))

(defdescribe
  display-label-casing-test
  "A `vis.yml` provider entry has NO `label` key (see `config-spec/provider-keys`),
   so its `id` is the only casing signal its author has — `display-label` must echo
   that id verbatim. The old `str/capitalize` fallback force-uppercased the first
   letter AND lowercased the rest, so an authored `openAI` surfaced as `Openai` in
   the TUI, in the gateway `/v1/providers` label, and in the companion."
  (it "echoes an unregistered vis.yml id verbatim, whatever its casing"
      (with-redefs [registry/provider-by-id (constantly nil)]
        (expect (= "openAI" (config/display-label :openAI)))
        (expect (= "ACME" (config/display-label :ACME)))
        (expect (= "GPT4All" (config/display-label :GPT4All)))
        (expect (= "my-custom-llm" (config/display-label :my-custom-llm)))
        (expect (= "ollama" (config/display-label :ollama)))))
  (it "changes no character of the id — never capitalizes, never lowercases"
      (with-redefs [registry/provider-by-id (constantly nil)]
        (doseq [id [:openAI :zAI-coding :OpenRouter :lmStudio :ACME :x]]
          (expect (= (name id) (config/display-label id))))))
  (it "a registered provider extension still owns its own branding"
      (with-redefs
        [registry/provider-by-id (fn [pid]
                                   (when (= pid :openai)
                                     {:provider/id pid :provider/label "OpenAI"}))]
        (expect (= "OpenAI" (config/display-label :openai)))))
  (it "falls back to `Provider` only when there is no id at all"
      (with-redefs [registry/provider-by-id (constantly nil)]
        (expect (= "Provider" (config/display-label nil))))))

(defn- rm-rf! [^java.io.File f] (when (.exists f) (run! rm-rf! (.listFiles f)) (.delete f)))

(defdescribe
  provider-persistence-test
  "save-config! / load-config round-trip backing onboarding. The first-run
   welcome and the provider manager BOTH write ~/.vis/state.yml so a connected
   provider survives a restart; adding a second provider must preserve the first
   and any unrelated global keys (e.g. :router). Isolated to a temp config dir."
  (it
    "first-run connect persists; adding a second provider keeps both + globals"
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-test-" (System/nanoTime))]
      (try (with-redefs
             [config/config-dir (constantly tmp)
              config/state-path (constantly (str tmp "/state.yml"))
              ;; isolate from any real project-local overlay / root YAML
              config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
              config/project-root-yaml-paths (constantly [])]

             ;; (0) genuine first run — nothing on disk
             (expect (config/first-run?))
             (expect (not (config/provider-configured?)))
             ;; (1) welcome connects provider A (mirrors show-welcome!'s persist:
             ;;     merge into raw global config, then save-config!)
             (config/save-config! (assoc (or (config/load-config-raw) {})
                                    "router" {"budget" {"max_cost" 5.0}}
                                    "providers" [{"id" "prov-a" "api_key" "key-a"}]))
             (expect (not (config/first-run?)))
             (expect (config/provider-configured?))
             (expect (= [:prov-a] (mapv :id (:providers (config/load-config)))))
             ;; (2) provider manager adds provider B (mirrors manage-providers:
             ;;     seed from existing config, append, save the full list)
             (let [raw (config/load-config-raw)]
               (config/save-config! (assoc raw
                                      "providers" (conj (vec (get raw "providers"))
                                                        {"id" "prov-b" "api_key" "key-b"}))))
             ;; (3) reload from disk: BOTH providers survive (in order) and the
             ;;     unrelated global :router key is preserved
             (let [loaded (config/load-config)]
               (expect (= [:prov-a :prov-b] (mapv :id (:providers loaded))))
               (expect (= 5.0 (get-in loaded [:router :budget :max-cost])))))
           (finally (rm-rf! (io/file tmp)))))))

(defdescribe
  remove-config-provider-test
  "`remove-config-provider!` is the provider REMOVE write path (logout only clears
   the credential). Dropping the provider entry alone is not enough: a FALLBACK
   tag naming it would stay behind in
   state.yml. The read path hides such a tag (the provider is no longer in the
   fleet), so no UI can show or clear it — and it silently resurrects the moment
   that provider is authenticated again. The tag must go with the provider."
  (it "drops a flat `fallback_provider`/`fallback_model` tag naming the removal"
      (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-fb-" (System/nanoTime))]
        (try (with-redefs
               [config/config-dir (constantly tmp)
                config/state-path (constantly (str tmp "/state.yml"))
                config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
                config/project-root-yaml-paths (constantly [])]

               (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}
                                                  {"id" "prov-b" "api_key" "key-b"}]
                                     "default_provider" "prov-a"
                                     "fallback_provider" "prov-b"
                                     "fallback_model" "beta-1"})
               (expect (config/remove-config-provider! :prov-b))
               (let [raw (config/load-config-raw)]
                 (expect (= ["prov-a"] (mapv #(get % "id") (get raw "providers"))))
                 (expect (nil? (get raw "fallback_provider")))
                 (expect (nil? (get raw "fallback_model")))
                 ;; the PRIMARY tag is unrelated and must survive untouched
                 (expect (= "prov-a" (get raw "default_provider")))))
             (finally (rm-rf! (io/file tmp))))))
  (it "drops a qualified `provider/model` fallback tag with no `fallback_provider`"
      (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-fb-" (System/nanoTime))]
        (try (with-redefs
               [config/config-dir (constantly tmp)
                config/state-path (constantly (str tmp "/state.yml"))
                config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
                config/project-root-yaml-paths (constantly [])]

               (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}
                                                  {"id" "prov-b" "api_key" "key-b"}]
                                     "fallback_model" "prov-b/beta-1"})
               (expect (config/remove-config-provider! "prov-b"))
               (let [raw (config/load-config-raw)]
                 (expect (nil? (get raw "fallback_model")))))
             (finally (rm-rf! (io/file tmp))))))
  (it "keeps a fallback tag that names a DIFFERENT provider"
      (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-fb-" (System/nanoTime))]
        (try (with-redefs
               [config/config-dir (constantly tmp)
                config/state-path (constantly (str tmp "/state.yml"))
                config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
                config/project-root-yaml-paths (constantly [])]

               (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}
                                                  {"id" "prov-b" "api_key" "key-b"}
                                                  {"id" "prov-c" "api_key" "key-c"}]
                                     "fallback_provider" "prov-c"
                                     "fallback_model" "gamma-1"})
               (expect (config/remove-config-provider! :prov-b))
               (let [raw (config/load-config-raw)]
                 (expect (= ["prov-a" "prov-c"] (mapv #(get % "id") (get raw "providers"))))
                 (expect (= "prov-c" (get raw "fallback_provider")))
                 (expect (= "gamma-1" (get raw "fallback_model")))))
             (finally (rm-rf! (io/file tmp))))))
  (it "a slash INSIDE a fallback model id is not a provider tag"
      (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-fb-" (System/nanoTime))]
        (try (with-redefs
               [config/config-dir (constantly tmp)
                config/state-path (constantly (str tmp "/state.yml"))
                config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
                config/project-root-yaml-paths (constantly [])]

               (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}
                                                  {"id" "openrouter" "api_key" "key-b"}]
                                     "fallback_provider" "openrouter"
                                     "fallback_model" "z-ai/glm-4.6v"})
               ;; `z-ai` is a model-id prefix, NOT a configured provider: the tag
               ;; belongs to `openrouter` and must survive removing someone else
               (expect (config/remove-config-provider! :prov-a))
               (let [raw (config/load-config-raw)]
                 (expect (= "openrouter" (get raw "fallback_provider")))
                 (expect (= "z-ai/glm-4.6v" (get raw "fallback_model"))))
               ;; ...and must go when its real owner is removed
               (expect (config/remove-config-provider! :openrouter))
               (let [raw (config/load-config-raw)]
                 (expect (nil? (get raw "fallback_provider")))
                 (expect (nil? (get raw "fallback_model")))))
             (finally (rm-rf! (io/file tmp))))))
  (it "reports no change when the provider is absent and holds no tag"
      (let [tmp (str (System/getProperty "java.io.tmpdir") "/vis-cfg-fb-" (System/nanoTime))]
        (try (with-redefs
               [config/config-dir (constantly tmp)
                config/state-path (constantly (str tmp "/state.yml"))
                config/project-config-yaml-paths (constantly [(str tmp "/none/.vis/config.yml")])
                config/project-root-yaml-paths (constantly [])]

               (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}]
                                     "fallback_provider" "prov-a"
                                     "fallback_model" "alpha-1"})
               (expect (nil? (config/remove-config-provider! :prov-z)))
               (let [raw (config/load-config-raw)]
                 (expect (= "prov-a" (get raw "fallback_provider")))))
             (finally (rm-rf! (io/file tmp)))))))

(defdescribe
  yaml-config-test
  "YAML parsing keeps canonical keys and scalar values as strings; the finite
   internal adapter runs only after validation."
  (it "adapts only known schema keys while preserving user-owned keys"
      (let
        [wire
         {"environment" {"ANTHROPIC_API_KEY" "tok"}
          "providers"
          [{"id" "anthropic" "api_style" "anthropic" "llm_headers" {"X-Custom-Header" "v"}}]}

         runtime
         (config/runtime-config wire)]

        (expect (= {:environment {"ANTHROPIC_API_KEY" "tok"}
                    :providers
                    [{:id :anthropic :api-style :anthropic :llm-headers {"X-Custom-Header" "v"}}]}
                   runtime))))
  (it "maps svar's is_* YAML keys to their ?-suffixed keyword contracts both ways"
      (let
        [wire
         {"providers" [{"id" "p" "models" [{"name" "m" "is_tool_call" true}]}]
          "router" {"rate_limit" {"is_respect_retry_after" true "is_fallback_provider" false}
                    "tokens" {"is_check_context" true}}
          "system_prompt" {"text" "x" "is_replace" true}}

         runtime
         (config/runtime-config wire)]

        (expect (true? (get-in runtime [:providers 0 :models 0 :tool-call?])))
        (expect (true? (get-in runtime [:router :rate-limit :respect-retry-after?])))
        (expect (false? (get-in runtime [:router :rate-limit :fallback-provider?])))
        (expect (true? (get-in runtime [:router :tokens :check-context?])))
        (expect (true? (get-in runtime [:system-prompt :is-replace])))
        (expect (= "is_respect_retry_after"
                   (first (keys (#'config/->yaml-safe {:respect-retry-after? true})))))
        (expect (= "is_replace" (first (keys (#'config/->yaml-safe {:is-replace true})))))))
  (it "parses vis.yml directly into the string-keyed clojure.spec shape"
      (let
        [read-yaml
         @#'config/read-yaml-config-map

         dir
         (io/file "target/config-yaml-test")

         yml
         (io/file dir "vis.yml")]

        (try (.mkdirs dir)
             (spit yml
                   (str "system_prompt: Prefer RST.\n"
                        "grep:\n  include_gitignored_paths:\n    - repositories/\n"))
             (expect (= {"system_prompt" "Prefer RST."
                         "grep" {"include_gitignored_paths" ["repositories/"]}}
                        (read-yaml (.getPath yml))))
             (spit yml "{{{{: not yaml")
             (expect (nil? (read-yaml (.getPath yml))))
             (finally (rm-rf! dir)))))
  (it "search-overlay adapts the validated grep config block"
      (expect (nil? (with-redefs
                      [config/load-config-raw (fn []
                                                {})]
                      (config/search-overlay))))
      (let
        [overlay (with-redefs
                   [config/load-config-raw (fn []
                                             {"grep" {"include_gitignored_paths"
                                                      ["repositories/"]}})]
                   (config/search-overlay))]
        (expect (= ["repositories/"] (:include-gitignored-paths overlay)))
        (expect (= config/default-search-always-exclude (:always-exclude overlay))))
      (expect (= ["*.log"]
                 (:always-exclude (with-redefs
                                    [config/load-config-raw (fn []
                                                              {"grep"
                                                               {"include_gitignored_paths" ["r/"]
                                                                "always_exclude" ["*.log"]}})]
                                    (config/search-overlay)))))))

(defdescribe
  toggles-yaml-config-test
  "Feature toggles are plain snake_case string ids end to end: hand-written
   `vis.yml` preserves the keys verbatim, and the machine YAML writer emits
   the same strings without keyword or namespace conversion."
  (it "a hand-written vis.yml toggles block preserves plain string ids"
      (let
        [dir
         (io/file "target/toggles-yaml-read-test")

         root-yml
         (io/file dir "vis.yml")]

        (try (.mkdirs dir)
             (spit root-yml (str "toggles:\n" "  reasoning_level: deep\n" "  auto_commit: true\n"))
             (with-redefs
               [config/global-config-yaml-paths
                (fn []
                  [])

                config/state-path
                (constantly "/nonexistent/state.yml")

                config/project-root-yaml-paths
                (fn []
                  [(.getPath root-yml)])

                config/project-config-yaml-paths
                (fn []
                  [])]

               (let [cfg (config/load-config-raw)]
                 (expect (= {"reasoning_level" "deep" "auto_commit" true} (get cfg "toggles")))
                 (expect (every? string? (keys (get cfg "toggles"))))))
             (finally (some-> root-yml
                              .delete)
                      (some-> dir
                              .delete)))))
  (it "->yaml-safe emits string toggle ids verbatim and keeps ordinary config keys"
      (let
        [safe (#'config/->yaml-safe
               {:toggles {"reasoning_level" :deep "auto_commit" true} :base-url "x" :providers []})]
        (expect (= {"reasoning_level" "deep" "auto_commit" true} (get safe "toggles")))
        (expect (contains? safe "base_url")))))

(defdescribe
  config-tier-precedence-test
  "`load-config-raw` deep-merges FOUR tiers, later wins: global `~/.vis` YAML
   base < global `~/.vis/state.yml` < root `vis.*` < nested `.vis/config.*`.
   Two contracts under test: the NESTED hidden overlay overrides the root
   file (personal beats committed), and the global `~/.vis` YAML tier loads
   UNDER the machine-written `state.yml` (state.yml wins per key, disjoint keys
   merge — the file Vis itself writes can never be shadowed by hand YAML)."
  (it
    "nested .vis/config.yml overrides root vis.yml; disjoint keys from every tier survive"
    (let
      [dir
       (io/file "target/config-precedence-test")

       gdir
       (io/file dir "global")

       gyml
       (io/file gdir "config.yml")

       gstate
       (io/file gdir "state.yml")

       root-yml
       (io/file dir "vis.yml")

       nested-yml
       (io/file dir ".vis" "config.yml")]

      (try (.mkdirs (io/file dir ".vis"))
           (.mkdirs gdir)
           (spit gyml
                 (str "system_prompt: FROM-GLOBAL-YAML\n"
                      "router:\n  budget:\n    max_cost: 1.0\n"))
           (spit gstate "providers:\n  - id: prov-a\n")
           (spit root-yml
                 (str "system_prompt: FROM-ROOT\n"
                      "grep:\n  include_gitignored_paths:\n    - repositories/\n"))
           (spit nested-yml "system_prompt: FROM-NESTED\n")
           (with-redefs
             [config/state-path
              (constantly (.getPath gstate))

              config/global-config-yaml-paths
              (fn []
                [(.getPath gyml)])

              config/project-root-yaml-paths
              (fn []
                [(.getPath root-yml)])

              config/project-config-yaml-paths
              (fn []
                [(.getPath nested-yml)])]

             (let [cfg (config/load-config-raw)]
               ;; the nested overlay wins the conflicting key
               (expect (= "FROM-NESTED" (get cfg "system_prompt")))
               ;; disjoint keys from every tier survive the merge
               (expect (= ["repositories/"] (get-in cfg ["grep" "include_gitignored_paths"])))
               (expect (= ["prov-a"] (mapv #(get % "id") (get cfg "providers"))))
               (expect (= 1.0 (get-in cfg ["router" "budget" "max_cost"]))))
             ;; drop the nested overlay entirely -> root wins
             (.delete nested-yml)
             (expect (= "FROM-ROOT" (get (config/load-config-raw) "system_prompt")))
             ;; drop root too -> the global YAML base shows through
             (.delete root-yml)
             (expect (= "FROM-GLOBAL-YAML" (get (config/load-config-raw) "system_prompt"))))
           (finally (rm-rf! dir)))))
  (it
    "global ~/.vis: hand-written YAML merges UNDER machine-written state.yml (state wins per key)"
    (let
      [dir
       (io/file "target/config-global-yaml-test")

       gyml
       (io/file dir "config.yml")

       gstate
       (io/file dir "state.yml")

       none
       (fn []
         [])]

      (try (.mkdirs dir)
           (spit gyml
                 (str "system_prompt: FROM-YAML\n"
                      "grep:\n  include_gitignored_paths:\n    - repositories/\n"))
           (spit gstate "system_prompt: FROM-STATE\n")
           (with-redefs
             [config/state-path
              (constantly (.getPath gstate))

              config/global-config-yaml-paths
              (fn []
                [(.getPath gyml)])

              config/project-root-yaml-paths
              none

              config/project-config-yaml-paths
              none]

             (let [cfg (config/load-config-raw)]
               ;; conflicting key: the machine-written state.yml wins
               (expect (= "FROM-STATE" (get cfg "system_prompt")))
               ;; YAML-only keys still land (merged, not ignored)
               (expect (= ["repositories/"] (get-in cfg ["grep" "include_gitignored_paths"])))))
           ;; ~/.vis accepts vis.yml / vis.yaml spellings as fallbacks
           (expect (= ["config.yml" "config.yaml" "vis.yml" "vis.yaml"]
                      (mapv #(.getName (io/file ^String %)) (@#'config/global-config-yaml-paths))))
           (finally (rm-rf! dir))))))

(defdescribe
  load-config-raw-cache-test
  "`load-config-raw` parses four YAML tiers (~60ms) and is called on EVERY
   tool call — `search-overlay` alone made it the dominant cost of a warm
   grep. It is memoized against the sources' mtime+size, so the memo must be
   INVISIBLE: an edit, a delete, and a fresh file all still show through."
  (it
    "reuses the parsed value until a source changes, then reparses"
    (let
      [dir
       (io/file "target/config-cache-test")

       gstate
       (io/file dir "state.yml")

       none
       (fn []
         [])]

      (try (.mkdirs dir)
           (spit gstate "system_prompt: ONE\n")
           (with-redefs
             [config/state-path
              (constantly (.getPath gstate))

              config/global-config-yaml-paths
              none

              config/project-root-yaml-paths
              none

              config/project-config-yaml-paths
              none]

             (config/invalidate-config-cache!)
             (let
               [a
                (config/load-config-raw)

                b
                (config/load-config-raw)]

               ;; cache HIT: the very same parsed map, no re-read
               (expect (identical? a b))
               (expect (= "ONE" (get a "system_prompt")))
               ;; an edit is picked up live (no /reload needed)
               (spit gstate "system_prompt: TWO\n")
               (expect (= "TWO" (get (config/load-config-raw) "system_prompt")))
               ;; so is a delete
               (.delete gstate)
               (expect (nil? (config/load-config-raw)))
               ;; and an explicit invalidation never breaks the value
               (spit gstate "system_prompt: THREE\n")
               (config/invalidate-config-cache!)
               (expect (= "THREE" (get (config/load-config-raw) "system_prompt")))))
           (finally (config/invalidate-config-cache!) (rm-rf! dir))))))

(defdescribe
  env-interpolation-test
  "`${NAME}` references in config values. An UNSET var is deliberately NOT a load
   failure: Vis is a long-lived gateway whose config is re-read live and on
   `/reload`, so aborting would let one unused provider's missing key kill a
   session running on a healthy provider. The reference is left VERBATIM, which
   makes the resolved map self-describing — the provider verdict, `vis doctor`,
   and the hard error on explicit selection all read it back."
  (it "reads distinct references out of strings only, and never sees bare $NAME"
      (expect (= ["HOME"] (config/env-refs "${HOME}")))
      (expect (= ["A" "B"] (config/env-refs "${A}/x/${B}/${A}")))
      (expect (= [] (config/env-refs "plain value")))
      (expect (= [] (config/env-refs "$HOME")))
      (expect (nil? (config/env-refs :not-a-string))))
  (it "resolves set vars, leaves unset ones verbatim, and never touches map keys"
      (let [home (System/getenv "HOME")]
        (expect (= home (config/interpolate-env "${HOME}")))
        (expect (= (str "https://" home "/v1") (config/interpolate-env "https://${HOME}/v1")))
        (expect (= "${VIS_TEST_UNSET_A}" (config/interpolate-env "${VIS_TEST_UNSET_A}")))
        (expect (= (str "a" home "b${VIS_TEST_UNSET_A}")
                   (config/interpolate-env "a${HOME}b${VIS_TEST_UNSET_A}")))
        ;; the key set is finite and snake_case, so a `${}` KEY is a typo
        (expect (= {"${HOME}" home} (config/interpolate-env {"${HOME}" "${HOME}"})))
        (expect (= {"a" [{"b" home}]} (config/interpolate-env {"a" [{"b" "${HOME}"}]})))
        (expect (= 7 (config/interpolate-env 7)))))
  (it "writes a whole-value reference back so a re-save cannot bake the secret in"
      (let
        [home
         (System/getenv "HOME")

         resolved
         (config/interpolate-env {"providers" [{"api_key" "${HOME}"
                                                "base_url" "https://x/${HOME}/v1"}]})]

        (expect (= [{"api_key" home "base_url" (str "https://x/" home "/v1")}]
                   (get resolved "providers")))
        ;; only the WHOLE-value reference maps back unambiguously; an embedded one
        ;; stays resolved, which is why `${NAME}` belongs on the key itself
        (expect (= [{"api_key" "${HOME}" "base_url" (str "https://x/" home "/v1")}]
                   (get (config/restore-env-refs resolved) "providers")))))
  (it "reports, sorted, exactly the unset vars each provider still needs"
      (let
        [gapped
         {:id :gapped :api-key "${VIS_TEST_UNSET_B}"}

         two
         {:id :two :api-key "${VIS_TEST_UNSET_B}" :base-url "https://${VIS_TEST_UNSET_A}/v1"}

         fine
         {:id :fine :api-key "sk-literal"}]

        (expect (= ["VIS_TEST_UNSET_B"] (config/provider-env-gap gapped)))
        (expect (= ["VIS_TEST_UNSET_A" "VIS_TEST_UNSET_B"] (config/provider-env-gap two)))
        (expect (nil? (config/provider-env-gap fine)))
        (expect (= {:gapped ["VIS_TEST_UNSET_B"] :two ["VIS_TEST_UNSET_A" "VIS_TEST_UNSET_B"]}
                   (config/provider-env-gaps {:providers [gapped fine two]})))
        (expect (= {} (config/provider-env-gaps {:providers [fine]})))))
  (it "names the provider and the vars, and carries no config VALUE that could leak"
      (expect (= "can't use rbi-genai: RBI_GENAI_API_KEY is not set"
                 (config/provider-env-message :rbi-genai ["RBI_GENAI_API_KEY"])))
      (expect (= "can't use rbi-genai: A_KEY, B_KEY are not set"
                 (config/provider-env-message "rbi-genai" ["A_KEY" "B_KEY"]))))
  (it "resolves through `load-config`, marking the gapped provider without failing"
      (try (config/invalidate-config-cache!)
           (with-redefs
             [config/load-config-raw
              (constantly {"providers"
                           [{"id" "gapped" "api_key" "${VIS_TEST_UNSET_A}" "models" [{"name" "m1"}]}
                            {"id" "fine" "api_key" "${HOME}" "models" [{"name" "m2"}]}]})]
             (let
               [cfg (config/load-config)
                [gapped fine] (:providers cfg)]

               (expect (= "${VIS_TEST_UNSET_A}" (:api-key gapped)))
               (expect (= (System/getenv "HOME") (:api-key fine)))
               (expect (= {:gapped ["VIS_TEST_UNSET_A"]} (config/provider-env-gaps cfg)))))
           (finally (config/invalidate-config-cache!)))))

(defdescribe
  provider-compatibility-test
  "`compatibility:` is the user-facing wire dialect (anthropic / openai /
   openai-responses). It resolves to svar's low-level `:api-style`, so a custom
   or self-hosted endpoint needs one obvious word instead of an internal svar
   enum. Raw `api_style` stays the escape hatch and wins when both are set."
  (it "maps every accepted value onto an svar api-style"
      (expect (= :anthropic (config/compatibility-api-style :anthropic)))
      (expect (= :openai-compatible-chat (config/compatibility-api-style :openai)))
      (expect (= :openai-compatible-responses (config/compatibility-api-style :openai-responses)))
      (expect (= :openai-compatible-responses (config/compatibility-api-style :openai_responses))
              "underscore spelling from YAML keywordization resolves identically"))
  (it "returns nil for absent or unknown values"
      (expect (nil? (config/compatibility-api-style nil)))
      (expect (nil? (config/compatibility-api-style :gemini))))
  (it "keywordizes `compatibility` off the YAML surface"
      (expect (= :openai
                 (:compatibility
                   (first (:providers (config/runtime-config
                                        {"providers" [{"id" "gw" "compatibility" "openai"}]})))))))
  (it "forwards the implied api-style to svar"
      (expect (= :openai-compatible-chat
                 (:api-style (config/->svar-provider {:id :gw
                                                      :api-key "k"
                                                      :base-url "https://llm.internal/v1"
                                                      :compatibility :openai
                                                      :models [{:name "m"}]})))))
  (it "lets an explicit api_style win over compatibility"
      (expect (= :gemini
                 (config/provider-api-style {:id :gw :compatibility :openai :api-style :gemini}
                                            nil))))
  (it "carries per-model context and output limits through to svar"
      (expect (= [{:name "m" :context 262144 :output-limit 32768 :tool-call? true}]
                 (:models (config/->svar-provider {:id :gw
                                                   :api-key "k"
                                                   :models [{:name "m"
                                                             :context 262144
                                                             :output-limit 32768
                                                             :tool-call? true}]})))))
  (it "carries `is_stateless` through to svar as :stateless-items?"
      ;; A gateway load-balancing several Azure OpenAI resources 400s on any
      ;; replayed server-minted item id (Blockether/vis#59); this flag is how a
      ;; user turns that replay off for one provider.
      (expect (true? (:stateless-items?
                       (config/->svar-provider
                         {:id :gw :api-key "k" :models [{:name "m"}] :stateless-items? true}))))
      (expect (true? (:stateless-items?
                       (first (:providers (config/runtime-config
                                            {"providers" [{"id" "gw" "is_stateless" true}]}))))))
      (expect (not (contains? (config/->svar-provider {:id :gw :api-key "k" :models [{:name "m"}]})
                              :stateless-items?)))))

(defn- with-declared-environment
  "Run `f` with `block` DECLARED as the active config's `environment:` block."
  [block f]
  (let [previous @config/active-config]
    (try (reset! config/active-config {:environment block})
         (f)
         (finally (reset! config/active-config previous)))))

(defn- with-dotenv
  "Write a temporary `.env` (and `.env.local`), bind both paths and call `f`."
  [env-text local-text f]
  (let
    [env-path
     (str (System/getProperty "java.io.tmpdir") "/vis-extension-env-" (System/nanoTime))

     local-path
     (str env-path ".local")]

    (try (spit env-path (or env-text ""))
         (when local-text (spit local-path local-text))
         (binding
           [config/*extension-dotenv-path*
            env-path

            config/*extension-dotenv-local-path*
            (when local-text local-path)]

           (f))
         (finally (.delete (io/file env-path)) (.delete (io/file local-path))))))

(defn- dotenv-status
  "Status for `name` DECLARED as `dotenv:` — the RENAME form of the same file."
  [name]
  (with-declared-environment {name {"dotenv" name}}
                             (fn []
                               (config/extension-env-status name))))

(defdescribe
  workspace-dotenv-test
  "The workspace's `.env` is loaded by DEFAULT, whole, with nothing declared: the
   project file is part of the project, and a confined child can read it out of
   the workspace it was granted anyway."
  (it "answers an undeclared name from `.env`"
      (with-dotenv "# comment\nexport VIS_TEST_EXTENSION_TOKEN = quoted\nVIS_TEST_EMPTY=\n"
                   nil
                   (fn []
                     (expect (= {:name "VIS_TEST_EXTENSION_TOKEN" :source :dotenv :value "quoted"}
                                (config/extension-env-status "VIS_TEST_EXTENSION_TOKEN")))
                     ;; A blank assignment is no value: the name stays UNSET.
                     (expect (= {:name "VIS_TEST_EMPTY" :source :unset :value nil}
                                (config/extension-env-status "VIS_TEST_EMPTY")))
                     ;; …and the same file, as the whole map a child is handed.
                     (expect (= {"VIS_TEST_EXTENSION_TOKEN" "quoted"}
                                (config/workspace-environment-values))))))
  (it "uses the final .env assignment, including an explicit blank override"
      (with-dotenv (str "VIS_TEST_OVERRIDE=first\nVIS_TEST_OVERRIDE=second\n"
                        "VIS_TEST_BLANK=first\nVIS_TEST_BLANK=\n")
                   nil
                   (fn []
                     (expect (= {:name "VIS_TEST_OVERRIDE" :source :dotenv :value "second"}
                                (config/extension-env-status "VIS_TEST_OVERRIDE")))
                     (expect (= {:name "VIS_TEST_BLANK" :source :unset :value nil}
                                (config/extension-env-status "VIS_TEST_BLANK")))
                     (expect (= {"VIS_TEST_OVERRIDE" "second"}
                                (config/workspace-environment-values))))))
  (it ".env overrides .env.local, including with an explicit blank value"
      (with-dotenv (str "VIS_TEST_ENV_PRIORITY=from-env\n" "VIS_TEST_ENV_BLANK=\n")
                   (str "VIS_TEST_ENV_PRIORITY=from-local\n"
                        "VIS_TEST_ENV_BLANK=from-local\n"
                        "VIS_TEST_LOCAL_ONLY=from-local\n")
                   (fn []
                     (expect (= {"VIS_TEST_ENV_PRIORITY" "from-env"
                                 "VIS_TEST_LOCAL_ONLY" "from-local"}
                                (config/workspace-environment-values)))
                     (expect (= {:name "VIS_TEST_ENV_BLANK" :source :unset :value nil}
                                (config/extension-env-status "VIS_TEST_ENV_BLANK")))
                     (expect (= {:name "VIS_TEST_LOCAL_ONLY" :source :dotenv :value "from-local"}
                                (config/extension-env-status "VIS_TEST_LOCAL_ONLY"))))))
  (it "layers `.env` ON TOP of the ambient value, and a declaration on top of both"
      (binding
        [config/*extension-getenv* {"VIS_TEST_LAYER" "from-ambient" "VIS_TEST_OUTER" "from-outer"}]
        (with-dotenv "VIS_TEST_LAYER=from-dotenv\n"
                     nil
                     (fn []
                       (expect (= {:name "VIS_TEST_LAYER" :source :dotenv :value "from-dotenv"}
                                  (config/extension-env-status "VIS_TEST_LAYER")))
                       ;; A name no project file mentions still comes from the process.
                       (expect (= {:name "VIS_TEST_OUTER" :source :env :value "from-outer"}
                                  (config/extension-env-status "VIS_TEST_OUTER")))
                       ;; A declaration NAMES its source, so it beats the file.
                       (with-declared-environment
                         {"VIS_TEST_LAYER" {"env" "VIS_TEST_OUTER"}}
                         (fn []
                           (expect (= {:name "VIS_TEST_LAYER" :source :env :value "from-outer"}
                                      (config/extension-env-status "VIS_TEST_LAYER")))))
                       ;; …and `dotenv:` is now only worth writing for a RENAME.
                       (expect (= "from-dotenv" (:value (dotenv-status "VIS_TEST_LAYER"))))))))
  (it "handles a BOM, an inline comment and a quoted value"
      (with-dotenv (str "\uFEFFVIS_TEST_BOM=ok\r\n"
                        "VIS_TEST_INLINE=bare # comment\n"
                        "VIS_TEST_QUOTED=\"quoted # value\" # comment\n")
                   nil
                   (fn []
                     (expect (= {"VIS_TEST_BOM" "ok"
                                 "VIS_TEST_INLINE" "bare"
                                 "VIS_TEST_QUOTED" "quoted # value"}
                                (config/workspace-environment-values)))))))

(defdescribe
  declared-environment-test
  "`environment:` says WHERE each variable's value comes from, and every surface
   resolves it through `extension-env-status`. The key used to be spec'd,
   documented and completely unread; wiring it to a hidden precedence chain would
   have left the same hole — a file that still does not state a name's source."
  (it "requires exactly one named source and refuses a literal"
      (expect (s/valid? ::config-spec/environment {"A" {"env" "B"}}))
      (expect (s/valid? ::config-spec/environment {"A" {"dotenv" "A"}}))
      (expect (s/valid? ::config-spec/environment {"A" {"keychain" "vis-exa" "account" "alice"}}))
      (expect (s/valid? ::config-spec/environment {"A" {"command" ["gh" "auth" "token"]}}))
      (expect (not (s/valid? ::config-spec/environment {"A" "a-literal-value"})))
      (expect (not (s/valid? ::config-spec/environment {"A" "${B}"})))
      (expect (not (s/valid? ::config-spec/environment {"A" {}})))
      (expect (not (s/valid? ::config-spec/environment {"A" {"env" "B" "dotenv" "B"}})))
      (expect (not (s/valid? ::config-spec/environment {"A" {"keychain" "k" "command" ["x"]}})))
      (expect (not (s/valid? ::config-spec/environment {"A" {"command" ["x"] "account" "alice"}})))
      (expect (not (s/valid? ::config-spec/environment {"9BAD" {"env" "B"}}))))
  (it "`env:` passes an outer variable through under a new name"
      (binding [config/*extension-getenv* {"WORK_OPENAI_KEY" "outer" "VIS_TEST_BLANK_OUTER" ""}]
        (with-declared-environment {"OPENAI_API_KEY" {"env" "WORK_OPENAI_KEY"}
                                    "VIS_TEST_BLANK" {"env" "VIS_TEST_BLANK_OUTER"}}
                                   (fn []
                                     (expect (= {:name "OPENAI_API_KEY" :source :env :value "outer"}
                                                (config/extension-env-status "OPENAI_API_KEY")))
                                     (expect (= {:name "VIS_TEST_BLANK" :source :unset :value nil}
                                                (config/extension-env-status "VIS_TEST_BLANK")))))))
  (it "resolves a helper command's stdout and names the source"
      (binding [config/*extension-getenv* (constantly nil)]
        (with-declared-environment
          {"VIS_TEST_DECLARED_CMD" {"command" ["/bin/echo" "token-from-helper"]}}
          (fn []
            (expect (= {:name "VIS_TEST_DECLARED_CMD" :source :command :value "token-from-helper"}
                       (config/extension-env-status "VIS_TEST_DECLARED_CMD")))
            (expect (= ["VIS_TEST_DECLARED_CMD"] (config/declared-environment-names)))))))
  (it "a declared source that produces nothing leaves the name unset"
      (binding [config/*extension-getenv* (constantly nil)]
        (with-declared-environment
          {"VIS_TEST_DECLARED_MISSING" {"env" "VIS_TEST_NEVER_SET_42"}}
          (fn []
            (expect (= {:name "VIS_TEST_DECLARED_MISSING" :source :unset :value nil}
                       (config/extension-env-status "VIS_TEST_DECLARED_MISSING")))))))
  ;; `jail.env` used to have to REPEAT every declared name before a confined child
  ;; saw one, and repeating a `dotenv:`/`keychain:` name there did nothing at all:
  ;; that list could only re-admit an AMBIENT variable. One resolution, one map.
  (it "resolves the whole block into resolved values"
      (binding [config/*extension-getenv* {"WORK_OPENAI_KEY" "outer" "VIS_TEST_BLANK_OUTER" ""}]
        (with-declared-environment {"OPENAI_API_KEY" {"env" "WORK_OPENAI_KEY"}
                                    "FROM_HELPER" {"command" ["/bin/echo" "helper-value"]}
                                    "VIS_TEST_BLANK" {"env" "VIS_TEST_BLANK_OUTER"}}
                                   (fn []
                                     ;; An unset name is ABSENT, never an empty string.
                                     (expect (= {"OPENAI_API_KEY" "outer"
                                                 "FROM_HELPER" "helper-value"}
                                                (config/declared-environment-values)))))))
  (it "hands a child the workspace `.env` with the declarations layered on top"
      (binding [config/*extension-getenv* {"WORK_OPENAI_KEY" "outer"}]
        (with-dotenv (str "FROM_DOTENV=file-value\n" "OPENAI_API_KEY=stale-file-value\n")
                     nil
                     (fn []
                       (with-declared-environment
                         {"OPENAI_API_KEY" {"env" "WORK_OPENAI_KEY"}}
                         (fn []
                           (expect (= {"FROM_DOTENV" "file-value" "OPENAI_API_KEY" "outer"}
                                      (config/child-environment-values)))))))))
  (it "never persists a value: the block survives the write round trip unchanged"
      (let [block {"environment" {"P" {"env" "PATH"} "Q" {"keychain" "vis-exa"}}}]
        (expect (= block (config/interpolate-env block)))
        (expect (= block (config/restore-env-refs (config/interpolate-env block)))))))

(defdescribe
  route-svar-logs-test
  "`route-svar-logs!` points svar's Trove facade at Telemere.

   Before this, Trove kept its default console backend, which writes to
   `*out*` — and `init!`/`init-cli!` rebind `*out*` to a BUFFERED writer
   over `vis.log` that is never flushed. Every svar log, including the
   `-Dsvar.stream.trace=true` SSE trace, was swallowed: `vis.log` held
   zero svar signals."
  (it "delivers a Trove signal to Telemere"
      ;; Trove's default: a console fn that never reaches Telemere.
      (expect (nil? (tel/with-signal (binding
                                       [trove/*log-fn* ((requiring-resolve
                                                          'taoensso.trove.console/get-log-fn))]
                                       (trove/log! {:level :info :id ::console :msg "sse line"})))))
      (config/route-svar-logs!)
      (let
        [signal (tel/with-signal (trove/log! {:level :info
                                              :id ::stream-line-trace
                                              :data {:reasoning-acc-len 13}
                                              :msg "sse line"}))]
        (expect (some? signal))
        (expect (= "sse line" (force (:msg_ signal))))
        (expect (= 13
                   (-> signal
                       :data
                       :reasoning-acc-len))))))

;; Regression: the TUI and the gateway daemon both wrote `~/.vis/vis.log`, and
;; the Telemere rolling handler rotates by renaming — the process that did not
;; rotate kept appending into a deleted inode.
(defdescribe log-path-test
             "`config/log-path` is this process's own file, never a shared one."
             (it "is `paths/log-file`, pid-stamped, under ~/.vis/logs"
                 (expect (= (paths/log-file) (config/log-path)))
                 (expect (str/ends-with? (paths/unixify (config/log-path))
                                         (str "/.vis/logs/vis-" (paths/process-id) ".log")))))

;; Regression, issue #140: `<cwd>/vis.yml` is the COMMITTED, team-shared project
;; file, yet it was deep-merged whole and merges over `~/.vis` — so one author's
;; `default_provider`/`default_model` was silently forced on every clone, a
;; teammate without that entitlement got a broken session, and validation
;; reported no problem at all.
(defdescribe
  project-root-user-only-keys-test
  "Provider and model selection is a per-user entitlement, so the VISIBLE project
   file cannot decide it: `load-project-root-config-raw` drops
   `config/user-only-config-keys` from `<cwd>/vis.yml` and warns once, naming the
   files a PERSON owns. Everything else in that file is untouched, and every
   file a PERSON owns still sets the pair: the hand-written
   `~/.vis/config.yml` and the gitignored `.vis/` overlay."
  (it
    "drops the routing keys from a committed vis.yml, keeps the rest, warns once"
    (let
      [dir
       (io/file (str (System/getProperty "java.io.tmpdir") "/vis-root-routing-" (System/nanoTime)))

       root-yml
       (io/file dir "vis.yml")]

      (try (.mkdirs dir)
           (spit root-yml
                 (str "default_provider: team-forced\n"
                      "default_model: team-model\n" "fallback_provider: team-backup\n"
                      "fallback_model: team-backup-model\n" "system_prompt: Prefer RST.\n"))
           (with-redefs
             [config/project-root-yaml-paths (fn []
                                               [(.getPath root-yml)])]
             (let
               [signal (tel/with-signal (config/load-project-root-config-raw))
                raw (config/load-project-root-config-raw)]

               ;; the project keeps its own config, and only its own
               (expect (= {"system_prompt" "Prefer RST."} raw))
               (expect (every? #(nil? (get raw %)) config/user-only-config-keys))
               ;; the developer is TOLD, by file and by key, and is not failed
               (expect (= :com.blockether.vis.internal.config/project-root-routing-keys-ignored
                          (:id signal)))
               (expect (= (.getPath root-yml)
                          (-> signal
                              :data
                              :source)))
               (expect (= ["default_model" "default_provider" "fallback_model" "fallback_provider"]
                          (-> signal
                              :data
                              :keys)))
               ;; once per file: the per-turn load path must not repeat it
               (expect (nil? (tel/with-signal (config/load-project-root-config-raw))))))
           (finally (rm-rf! dir)))))
  (it
    "a hand-written ~/.vis/config.yml still decides the pair"
    (let
      [dir
       (io/file (str (System/getProperty "java.io.tmpdir") "/vis-global-pair-" (System/nanoTime)))

       global-yml
       (io/file dir "config.yml")]

      (try (.mkdirs dir)
           (spit global-yml
                 (str "default_provider: mine\n"
                      "default_model: my-model\n"
                      "fallback_provider: my-backup\n"))
           (with-redefs
             [config/global-config-yaml-paths
              (fn []
                [(.getPath global-yml)])

              config/state-path
              (constantly (.getPath (io/file dir "absent-state.yml")))

              config/project-root-yaml-paths
              (fn []
                [])

              config/project-config-yaml-paths
              (fn []
                [])]

             ;; the personal hand-written tier is read WHOLE — the pair is the
             ;; selection the TUI, the gateway and the companion remember, and a
             ;; person may equally write their own
             (expect (= {"default_provider" "mine"
                         "default_model" "my-model"
                         "fallback_provider" "my-backup"}
                        (config/load-global-yaml-config-raw)))
             (config/invalidate-config-cache!)
             (let [merged (config/load-config-raw)]
               (expect (= "mine" (get merged "default_provider")))
               (expect (= "my-model" (get merged "default_model")))
               (expect (= "my-backup" (get merged "fallback_provider")))))
           (finally (config/invalidate-config-cache!) (rm-rf! dir)))))
  (it
    "the gitignored .vis/ overlay still decides the pair"
    (let
      [dir
       (io/file (str (System/getProperty "java.io.tmpdir") "/vis-root-overlay-" (System/nanoTime)))

       root-yml
       (io/file dir "vis.yml")

       overlay-yml
       (io/file dir ".vis" "config.yml")]

      (try (.mkdirs (io/file dir ".vis"))
           (spit root-yml (str "default_provider: team-forced\n" "system_prompt: Prefer RST.\n"))
           (spit overlay-yml (str "default_provider: mine\n" "default_model: my-model\n"))
           (with-redefs
             [config/global-config-yaml-paths
              (fn []
                [])

              config/state-path
              (constantly (.getPath (io/file dir "absent-state.yml")))

              config/project-root-yaml-paths
              (fn []
                [(.getPath root-yml)])

              config/project-config-yaml-paths
              (fn []
                [(.getPath overlay-yml)])]

             (config/invalidate-config-cache!)
             (let [merged (config/load-config-raw)]
               (expect (= "mine" (get merged "default_provider")))
               (expect (= "my-model" (get merged "default_model")))
               (expect (= "Prefer RST." (get merged "system_prompt")))))
           (finally (config/invalidate-config-cache!) (rm-rf! dir))))))
