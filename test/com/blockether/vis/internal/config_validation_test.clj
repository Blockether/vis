(ns com.blockether.vis.internal.config-validation-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config-validation :as config-validation]
            [lazytest.core :refer [defdescribe expect it]]
            [yamlstar.core :as yamlstar]))

(defdescribe per-model-api-style-config-test
             (it "accepts provider-generated wire routing metadata for a model"
                 (expect (config-validation/valid? {"providers" [{"id" "opencode-go"
                                                                  "models" [{"name" "minimax-m3"
                                                                             "api_style"
                                                                             "anthropic"}]}]}))))

(def full-config
  {"default_provider" "anthropic"
   "default_model" "claude"
   "fallback_provider" "openai"
   "fallback_model" "gpt"
   ;; Provider ids the operator DELETED. Needed for a provider that never had a
   ;; `providers` entry to delete — one synthesized from an env var or a stored
   ;; credential — since absence from config cannot express "removed" for those.
   "removed_providers" ["removed-provider"]
   "providers" [{"id" "anthropic"
                 "api_key" "secret"
                 ;; Command-backed credential: argv whose trimmed stdout is the key.
                 "api_key_command" ["vault-token" "--env" "sbox"]
                 "models" [{"name" "claude"
                            "context" 200000
                            "output_limit" 8192
                            "is_tool_call" true
                            "api_style" "anthropic"}]
                 "base_url" "https://api.anthropic.com"
                 "compatibility" "anthropic"
                 "api_style" "anthropic"
                 "responses_path" "/v1/messages"
                 "llm_headers" {"X-Test" "yes"}
                 "extra_body" {"temperature" 0}
                 "is_stateless" true
                 ;; Provider-level veto: this endpoint refuses image content parts.
                 "is_image_input" false
                 ;; Per-provider transport budget, narrowing the root `network`
                 ;; block for this one endpoint.
                 "network" {"timeout_ms" 120000
                            "ttft_timeout_ms" 30000
                            "first_byte_timeout_ms" 30000
                            "idle_timeout_ms" 60000
                            "semantic_timeout_ms" 90000}}]
   "router" {"rate_limit" {"same_provider_delays_ms" [1000 2000]
                           "fallback_after_ms" 30000
                           "is_respect_retry_after" true
                           "is_fallback_provider" true}
             "network" {"timeout_ms" 300000
                        "ttft_timeout_ms" 30000
                        "first_byte_timeout_ms" 30000
                        "idle_timeout_ms" 45000
                        "semantic_timeout_ms" 120000
                        "max_retries" 5
                        "initial_delay_ms" 1000
                        "max_delay_ms" 60000
                        "multiplier" 2.0}
             "budget" {"max_tokens" 1000000 "max_cost" 5.0}
             "tokens" {"is_check_context" true
                       "pricing" {"claude" {"input" 1.0 "output" 2.0}}
                       "context_limits" {"claude" 200000}
                       "output_reserve" 8192}
             "failure_threshold" 5
             "recovery_ms" 60000
             "transient_status_codes" [429 500 503]
             "window_ms" 60000
             "cooldown_ms" 60000
             "max_wait_ms" 30000}
   "system_prompt" {"text" "Project rules" "is_replace" false}
   "workspace" {"filesystem"
                [{"id" "svar"
                  "path" "/opt/svar"
                  "description" "a sibling repo"
                  "access" "read-write"
                  "search" true
                  ;; #89: a root may be gated on the host OS and/or an existing path.
                  "when" {"os" ["macos" "linux" "wsl" "windows"] "exists" "/"}
                  "optional" false
                  "draft" "copy-and-apply"} {"id" "ref" "path" "~/reference" "access" "read-only"}
                 {"id" "gen" "path" "~/generated"}
                 {"id" "cache" "path" "~/.m2" "search" false "description" "maven cache"}]}
   "jail" {"enabled" true
           ;; Ambient-environment mode: `declared` (the default) or `inherit`.
           "environment" "declared"
           "filesystem" {"allow" ["svar" "ref" "gen" "cache"]}
           ;; #90: macOS Mach lookups — an explicit allow list plus the keychain bundle.
           "mach_services" {"allow" ["com.example.agent"] "keychain" false}
           "deny_exec" ["definitely-not-a-real-binary-xyz"]
           "network" {"inbound_ports" [5273 8080]
                      "allowed_domains" ["github.com"]
                      "denied_domains" ["example.invalid"]
                      "exclude_domains" ["opaque.example"]
                      "allow_private" false
                      "rules" [{"host" "api.example.com"
                                "access" "read-only"
                                "methods" ["POST"]
                                "ports" [443]
                                "allow" [{"method" "POST" "path" "/v1/**"}]}]}}
   "environment" {"ANTHROPIC_API_KEY" {"env" "ANTHROPIC_API_KEY"}
                  "EXA_API_KEY" {"keychain" "vis-exa" "account" "alice"}
                  "GITHUB_TOKEN" {"command" ["gh" "auth" "token"]}}
   "db_spec" {"backend" "sqlite" "path" "/tmp/vis.db"}
   "grep" {"include_gitignored_paths" ["repositories/"] "always_exclude" ["target/"]}
   "toggles" {"reasoning_level" "deep"}
   "python"
   {"source_paths" ["src" "lib/vendor"] "interpreter" [".venv/bin/python"] "runner" "project"}
   "tui_settings" {"theme_name" "dark" "contributors_disabled" ["voice"]}
   "mcp" {"servers" {"local" {"transport" "stdio"
                              "command" "npx"
                              "args" ["-y" "server"]
                              "cwd" "/tmp"
                              "env" {"TOKEN" "secret"}
                              "enabled" true
                              "timeout_ms" 60000}
                     "remote" {"transport" "streamable_http"
                               "url" "https://mcp.example.com"
                               "headers" {"Authorization" "Bearer secret"}
                               "listen" true
                               "auth" {"client_id" "vis"
                                       "scope" "openid profile offline_access"
                                       "authorization_timeout_ms" 300000}}}}
   "titling" {"mode" "llm" "provider" "rbi_genai" "model" "gpt-5.4-mini"}
   ;; Machine-written, never hand-authored: what the wire itself answered about images,
   ;; each row stamped so it can expire.
   "vision_memory"
   {"blind_providers" {"console-go" {"learned_at" "2026-01-05T09:12:00Z"}}
    "blind_models" {"small-coder" {"learned_at" "2026-01-05T09:12:00Z" "providers" ["console-go"]}}
    "working_eye" {"provider" "seeing" "model" "mimo-v2.5" "learned_at" "2026-01-05T09:14:00Z"}}})

(defdescribe
  config-contract-test
  (it "validates the complete string-keyed JSON contract"
      (expect (config-validation/valid? full-config)))
  (it
    "rejects keyword keys, aliases, unknown keys, and invalid security values"
    (expect (not (config-validation/valid? {:filesystem {}})))
    (expect (not (config-validation/valid? {"filesystem" {}})))
    ;; `jail` is the confinement block; `sandbox` is not accepted.
    (expect (not (config-validation/valid? {"sandbox" false})))
    (expect (not (config-validation/valid? (assoc full-config "sandbox" true))))
    ;; Unknown top-level blocks are rejected.
    (expect (not (config-validation/valid? {"search" {"include_gitignored_paths"
                                                      ["repositories/"]}})))
    ;; Configuration keys use snake_case.
    (expect (not (config-validation/valid? (assoc-in full-config ["providers" 0 "api-key"] "k"))))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["providers" 0 "base-url"] "https://x"))))
    (expect (not (config-validation/valid? (assoc full-config "system-prompt" "hi"))))
    (expect (not (config-validation/valid? (assoc full-config "db-spec" {"backend" "sqlite"}))))
    (expect (not (config-validation/valid? (assoc full-config "default_provider" "  "))))
    (expect (not (config-validation/valid? (assoc full-config "default_model" ""))))
    (expect (not (config-validation/valid? (assoc full-config "fallback_provider" "  "))))
    (expect (not (config-validation/valid? (assoc full-config "fallback_model" ""))))
    ;; Workspace entries: a rooted path is required and unknown keys are rejected.
    (expect (not (config-validation/valid? (assoc-in full-config
                                             ["workspace" "filesystem"]
                                             [{"id" "x" "path" "./relative"}]))))
    (expect (not (config-validation/valid? (assoc-in full-config
                                             ["workspace" "filesystem"]
                                             [{"id" "x" "path" "~/ok" "note" "unknown-key"}]))))
    ;; `access` is a closed enum and `description` may not be blank.
    (expect (not (config-validation/valid? (assoc-in full-config
                                             ["workspace" "filesystem"]
                                             [{"id" "x" "path" "~/ok" "access" "sideways"}]))))
    (expect (not (config-validation/valid? (assoc-in full-config
                                             ["workspace" "filesystem"]
                                             [{"id" "x" "path" "~/ok" "description" ""}]))))
    ;; `draft` is a closed policy enum: shared / copy-only / copy-and-apply / not-allowed.
    (expect (= #{"shared" "copy-only" "copy-and-apply" "not-allowed"}
               config-validation/workspace-draft-values))
    (doseq [policy config-validation/workspace-draft-values]
      (expect (config-validation/valid? (assoc-in full-config
                                          ["workspace" "filesystem"]
                                          [{"id" "x" "path" "~/ok" "draft" policy}]))))
    (expect (not (config-validation/valid? (assoc-in full-config
                                             ["workspace" "filesystem"]
                                             [{"id" "x" "path" "~/ok" "draft" "sideways"}]))))
    ;; jail.filesystem is pure id admission — only an `allow` STRING VECTOR, nothing else.
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "filesystem" "allow"] "svar"))))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "filesystem" "deny"] ["svar"]))))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "network" "inbound_ports"] [0]))))
    (expect (config-validation/valid?
              (assoc-in full-config
                ["workspace" "filesystem"]
                [{"id" "ok" "path" "~/home-ok" "description" "why" "search" false}])))
    ;; `environment` is the only block that declares variable sources.
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "env"] ["CI"]))))
    ;; What replaced it is a MODE over the operator's ambient environment, not a list:
    ;; `declared` (default) or `inherit`, and nothing else.
    (expect (config-validation/valid? (assoc-in full-config ["jail" "environment"] "declared")))
    (expect (config-validation/valid? (assoc-in full-config ["jail" "environment"] "inherit")))
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "environment"] "all"))))
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "environment"] true))))
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "environment"] ["CI"]))))
    ;; deny-exec: a list of executable names (or rooted paths) to block by read.
    (expect (config-validation/valid? (assoc-in full-config ["jail" "deny_exec"] ["curl" "ssh"])))
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "deny_exec"] "curl"))))
    (expect (not (config-validation/valid? (assoc-in full-config ["jail" "deny_exec"] [""]))))
    ;; Descriptions of ADMITTED roots flow into the derived policy, keyed by grant path.
    (expect (= {"/opt/svar" "a sibling repo"
                "~/.m2" "maven cache"
                "~/.vis" (get config-validation/vis-home-entry "description")}
               (:path-descriptions (config-validation/process-jail-config full-config))))
    ;; Network is policy data, never an independent on/off escape hatch.
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "network" "enabled"] false))))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "network" "rules" 0 "oops"] true))))
    ;; :ports is a list of valid port integers.
    (expect (config-validation/valid?
              (assoc-in full-config ["jail" "network" "rules" 0 "ports"] [22 443])))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "network" "rules" 0 "ports"] [70000]))))
    (expect (not (config-validation/valid?
                   (assoc-in full-config ["jail" "network" "rules" 0 "ports"] ["443"]))))
    ;; Python block: closed, so an unknown key is refused.
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "cache"] "/x"))))
    ;; Extra import roots: a list of strings, never a bare string or a number.
    (expect (config-validation/valid? (assoc-in full-config ["python" "source_paths"] [])))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "source_paths"] "src"))))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "source_paths"] [1]))))
    ;; Interpreter pin: an argv vector or a bare program, never empty or numeric.
    (expect (config-validation/valid? (assoc-in full-config ["python" "interpreter"] "python3.12")))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "interpreter"] []))))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "interpreter"] [1]))))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "interpreter"] ""))))
    ;; Runner: a closed enum of the two execution backends.
    (expect (config-validation/valid? (assoc-in full-config ["python" "runner"] "vispython")))
    (expect (not (config-validation/valid? (assoc-in full-config ["python" "runner"] "uv"))))
    ;; Titling has no scheduling option.
    (expect (config-validation/valid? (assoc-in full-config ["titling" "mode"] "first_sentence")))
    (expect (not (config-validation/valid? (assoc-in full-config ["titling" "mode"] "clever"))))
    (expect (not (config-validation/valid? (assoc-in full-config ["titling" "scheduling"] "idle"))))
    (expect (not (config-validation/valid? (assoc-in full-config ["titling" "provider"] ""))))
    (expect (not (config-validation/valid? (assoc-in full-config ["titling" "unknown"] 1))))
    (doseq [server [{"transport" "stdio" "url" "https://x"}
                    {"transport" "stdio" "command" "server" "headers" {}}
                    {"transport" "streamable_http" "command" "server"}
                    {"transport" "streamable_http" "url" "https://x" "args" []}
                    {"transport" "http" "url" "https://x"} {"command" "server"}
                    {"url" "https://x"}]]
      (expect (not (config-validation/valid? {"mcp" {"servers" {"invalid" server}}})))))
  (it
    "derives process-jail and network maps from the same string contract"
    (expect (= {:disabled? false
                :inherit-host-env? false
                :allow-read-write ["/opt/svar" "~/generated" "~/.m2" "~/.vis"]
                :allow-read ["~/reference"]
                :allow-write []
                :deny-read []
                :deny-write []
                :deny-exec []
                :no-search ["~/.m2" "~/.vis"]
                :inbound-ports [5273 8080]
                :mach-services ["com.example.agent"]
                :path-descriptions {"/opt/svar" "a sibling repo"
                                    "~/.m2" "maven cache"
                                    "~/.vis" (get config-validation/vis-home-entry "description")}}
               (config-validation/process-jail-config full-config)))
    (expect (true? (:disabled? (config-validation/process-jail-config
                                 (assoc-in full-config ["jail" "enabled"] false)))))
    (expect (false? (:disabled? (config-validation/process-jail-config
                                  (assoc-in full-config ["jail" "enabled"] true)))))
    ;; The jail is opt-in.
    (expect (true? (:disabled? (config-validation/process-jail-config
                                 (update full-config "jail" dissoc "enabled")))))
    ;; A confined child receives only declared environment values by default.
    (expect (false? (:inherit-host-env? (config-validation/process-jail-config full-config))))
    (expect (false? (:inherit-host-env?
                      (config-validation/process-jail-config
                        (assoc-in full-config ["jail" "environment"] "declared")))))
    (expect (true? (:inherit-host-env? (config-validation/process-jail-config
                                         (assoc-in full-config ["jail" "environment"] "inherit")))))
    (expect (= {:allowed-domains ["github.com"]
                :denied-domains ["example.invalid"]
                :exclude-domains ["opaque.example"]
                :allow-private false
                :rules [{:host "api.example.com"
                         :access "read-only"
                         :methods ["POST"]
                         :ports [443]
                         :allow [{:method "POST" :path "/v1/**"}]}]}
               (config-validation/network-config full-config)))
    ;; jail.enabled is the SINGLE gate: off => egress policy is empty (open).
    (expect (= {}
               (config-validation/network-config (assoc-in full-config ["jail" "enabled"] false))))
    (expect (= {} (config-validation/network-config (update full-config "jail" dissoc "enabled")))))
  (it
    "resolves jail.deny-exec into a separate deny-exec list (rooted passthrough, drops unresolvable)"
    (let [pol
          (config-validation/process-jail-config (assoc-in full-config
                                                   ["jail" "deny_exec"]
                                                   ["/opt/nope/curl"
                                                    "definitely-not-a-real-binary-xyz"]))

          denied
          (set (:deny-exec pol))]

      ;; absolute/home entries pass through verbatim (deny fails safe)
      (expect (contains? denied "/opt/nope/curl"))
      ;; an unresolvable bare name contributes nothing
      (expect (not (contains? denied "definitely-not-a-real-binary-xyz")))
      ;; deny-exec is exec-only; the filesystem deny-read list stays empty.
      (expect (= [] (:deny-read pol)))))
  (it "a disabled jail admits the catalog; an enabled jail admits named roots"
      ;; The disabled policy ignores the allow list.
      (let [with-ghost
            (assoc-in full-config ["jail" "filesystem" "allow"] ["svar" "ghost-id"])

            disabled
            (assoc-in with-ghost ["jail" "enabled"] false)

            enabled
            (assoc-in with-ghost ["jail" "enabled"] true)]

        ;; disabled => full catalog RW roots (svar, gen, cache), allow ignored.
        (expect (= ["/opt/svar" "~/generated" "~/.m2" "~/.vis"]
                   (:allow-read-write (config-validation/process-jail-config disabled))))
        (expect (= ["~/reference"] (:allow-read (config-validation/process-jail-config disabled))))
        ;; enabled => allow narrows the jail, and the ghost id is a hard error.
        (expect (= :vis/invalid-config
                   (try (config-validation/process-jail-config enabled)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it "always grants Vis's own session folder (~/.vis), whatever the catalog and the jail say"
      ;; ENGINE-LEVEL, not a feature toggle: `~/.vis` holds state.yml, the session
      ;; DB, the gateway event journals and the logs, so Vis must reach its own
      ;; state even with an empty catalog or a live deny-by-omission jail.
      (let [vis-home
            (get config-validation/vis-home-entry "path")

            bare
            (config-validation/process-jail-config {})

            jailed
            (config-validation/process-jail-config (-> full-config
                                                       (assoc-in ["jail" "enabled"] true)
                                                       (assoc-in ["jail" "filesystem" "allow"]
                                                                 ["svar"])))

            explicit
            (config-validation/process-jail-config
              {"workspace" {"filesystem" [{"id" "vh" "path" "~/.vis/" "access" "read-only"}]}})]

        (expect (= [vis-home] (:allow-read-write bare)))
        ;; Explicit paths remain reachable, but default search excludes this root.
        (expect (= [vis-home] (:no-search bare)))
        (expect (contains? (:path-descriptions bare) vis-home))
        ;; A live jail narrows to `allow`, but never away from the session folder.
        (expect (= ["/opt/svar" vis-home] (:allow-read-write jailed)))
        ;; An explicit catalog entry for the same path wins (here: read-only).
        (expect (= [] (:allow-read-write explicit)))
        (expect (= ["~/.vis/"] (:allow-read explicit)))))
  (it "redacts credentials from validation failures"
      (let [bad
            (assoc-in full-config ["providers" 0 "unknown"] true)

            data
            (try (config-validation/assert-config! bad "vis.yml")
                 nil
                 (catch Exception e (ex-data e)))]

        (expect (= :vis/invalid-config (:type data)))
        (expect (not (.contains (pr-str data) "secret")))
        (expect (.contains (pr-str data) "<redacted>"))))
  (it "validates parser output before any runtime adaptation"
      (require 'com.blockether.vis.internal.config :reload)
      (let [file
            (io/file "target/invalid-vis-config.yml")

            read-yaml
            (var-get (ns-resolve 'com.blockether.vis.internal.config 'read-yaml-config-map))]

        (try (.mkdirs (.getParentFile file))
             (spit file "jail:\n  filesystem:\n    allow_reed:\n      - ../escape\n")
             (let [data (try (read-yaml (.getPath file)) nil (catch Exception e (ex-data e)))]
               (expect (= :vis/invalid-config (:type data)))
               (expect (= (.getPath file) (:source data))))
             (finally (io/delete-file file true)))))
  ;; A top-level `sandbox:` / `filesystem:` used to be REWRITTEN into `jail:`
  ;; before the schema ever saw the file, so an operator's key silently became a
  ;; different one — and `sandbox: false` next to `jail:` was quietly ignored.
  ;; The parser normalizes NOTHING now; the closed schema refuses both by name.
  (it "refuses a top-level sandbox: / filesystem: instead of folding it into jail:"
      (require 'com.blockether.vis.internal.config :reload)
      (let [file
            (io/file "target/invalid-key-vis-config.yml")

            read-yaml
            (var-get (ns-resolve 'com.blockether.vis.internal.config 'read-yaml-config-map))

            refused
            (fn [yaml]
              (spit file yaml)
              (try (read-yaml (.getPath file)) nil (catch Exception e (ex-data e))))]

        (try (.mkdirs (.getParentFile file))
             (expect (= :vis/invalid-config (:type (refused "sandbox: false\n"))))
             (expect (= :vis/invalid-config
                        (:type (refused "sandbox: true\njail:\n  enabled: false\n"))))
             (expect (= :vis/invalid-config (:type (refused "filesystem:\n  allow: [vis]\n"))))
             (finally (io/delete-file file true))))))

(defdescribe
  config-schema-ownership-test
  (it "validates the repository vis.yml through the contract schema"
      (let [wire (yamlstar/load (slurp (io/file "vis.yml")))]
        ;; The repository file is a commented template and therefore may parse to nil.
        (expect (or (nil? wire)
                    (and (every? string? (keys wire)) (config-validation/valid? wire))))))
  (it "checks recursively user-owned request and pricing maps without keywordizing"
      (expect (config-validation/valid? (assoc-in full-config
                                          ["providers" 0 "extra_body"]
                                          {"thinking" {"type" "enabled" "budget_tokens" 2048}
                                           "stop" ["DONE" nil]})))
      (expect (not (config-validation/valid? (assoc-in full-config
                                               ["providers" 0 "extra_body"]
                                               {:keyword-key "not YAML wire data"}))))
      (expect (not (config-validation/valid? (assoc-in full-config
                                               ["router" "tokens" "pricing"]
                                               {"claude" {:input 1.0}})))))
  (it "takes the machine's learned vision facts only in the shape it writes"
      (expect (config-validation/valid? (assoc-in full-config
                                          ["vision_memory" "blind_providers" "other"]
                                          {"learned_at" "2026-01-06T00:00:00Z"})))
      ;; A row with no stamp could never expire, so it is refused at the write boundary.
      (expect (not (config-validation/valid?
                     (assoc-in full-config ["vision_memory" "blind_providers" "other"] {}))))
      (expect (not (config-validation/valid?
                     (assoc-in full-config ["vision_memory" "working_eye" "prefer"] true))))
      (expect (not (config-validation/valid? (assoc-in full-config
                                               ["vision_memory" "blind_models" "small-coder"
                                                "providers"]
                                               "console-go")))))
  (it "explain-problems names each offending top-level key, [] when valid"
      (expect (= []
                 (config-validation/explain-problems {"providers" [{"id" "a"
                                                                    "models" [{"name" "m"}]}]})))
      (expect (= [] (config-validation/explain-problems nil)))
      (expect (= ["config: expected a YAML map with string keys"]
                 (config-validation/explain-problems 7)))
      (expect (= ["nope: unknown top-level config key (config is closed)"]
                 (config-validation/explain-problems {"nope" 1})))
      (expect (= ["jail: expected object, got integer"]
                 (config-validation/explain-problems {"jail" 5}))))
  (it "explain-problems pinpoints the nested field path, with a did-you-mean"
      (expect (= [(str "grep.include-gitignored-paths: unknown key (config is closed)"
                       " — did you mean \"grep.include_gitignored_paths\"?")]
                 (config-validation/explain-problems {"grep" {"include-gitignored-paths" ["a"]}})))
      (expect (= ["jail.filesystem.allow_reed: unknown key (config is closed)"]
                 (config-validation/explain-problems {"jail" {"filesystem" {"allow_reed" ["x"]}}})))
      (expect (= ["providers[0].models[0].context: -1 is less than the minimum 1"]
                 (config-validation/explain-problems
                   {"providers" [{"id" "p" "models" [{"name" "m" "context" -1}]}]})))
      (expect (= ["providers[0].id: required key is missing"]
                 (config-validation/explain-problems {"providers" [{"base_url" "https://x"}]})))
      (expect (= ["mcp.servers.docs.transport: the instance is not one of the enumerated values"]
                 (config-validation/explain-problems
                   {"mcp" {"servers" {"docs" {"transport" "stdioo" "command" "x"}}}}))))
  (it "assert-config! names the offending fields in the thrown message"
      (let [e (try (config-validation/assert-config! {"grep" {"include-gitignored-paths" ["a"]}}
                                                     "vis.yml")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (expect (.contains (ex-message e) "grep.include-gitignored-paths"))
        (expect (.contains (ex-message e) "did you mean"))
        (expect (= ["vis.yml"] [(:source (ex-data e))]))
        (expect (seq (:fields (ex-data e))))
        ;; A mistyped key is a USER error: entry points print `:vis/panel` and
        ;; skip the fatal path, so no Java stack trace ever reaches the user.
        (expect (true? (:vis/user-error (ex-data e))))
        (let [panel (:vis/panel (ex-data e))]
          (expect (every? string? panel))
          (expect (some #(.contains ^String % "Invalid Vis configuration in vis.yml") panel))
          (expect (some #(.contains ^String % "grep.include-gitignored-paths") panel))))))

;; ── Provider wire dialect (#152) ─────────────────────────────────────────────
;; Regression, issue #152: `api_style` was typed as "any non-blank string", so a
;; near-miss like `openai_responses` validated and then reached svar as a dialect
;; its `case` does not know — which silently means `/chat/completions`. Forgiving
;; on every accepted spelling, refused with the vocabulary named otherwise.

(defdescribe
  provider-dialect-vocabulary-test
  (it "accepts every schema-declared spelling of a dialect"
      (doseq [v ["anthropic" "claude" "openai" "chat" "openai-responses" "openai_responses"
                 "responses" "openai-compatible-responses" "gemini"]]
        (expect (= []
                   (config-validation/explain-problems {"providers"
                                                        [{"id" "p"
                                                          "api_style" v
                                                          "compatibility" v
                                                          "models" [{"name" "m" "api_style" v}]}]}))
                (str v " must name a dialect"))))
  (it "normalizes an accepted spelling onto svar's own api-style"
      (expect (= :openai-compatible-responses
                 (config-validation/normalize-api-style "openai_responses")))
      (expect (= :openai-compatible-chat (config-validation/normalize-api-style :openai)))
      (expect (= :anthropic (config-validation/normalize-api-style "anthropic")))
      (expect (nil? (config-validation/normalize-api-style "openai-responses-v2")))
      (expect (nil? (config-validation/normalize-api-style nil))))
  (it "refuses an unknown dialect and names the accepted vocabulary"
      (let [[problem :as problems] (config-validation/explain-problems
                                     {"providers" [{"id" "p" "api_style" "openai-responses-v2"}]})]
        (expect (= 1 (count problems)))
        (expect (str/starts-with? problem "providers[0].api_style: "))
        (expect (str/includes? problem "is not a wire dialect"))
        (doseq [v config-validation/api-style-values]
          (expect (str/includes? problem v) (str "the message must name " v))))
      (expect (str/includes? (first (config-validation/explain-problems
                                      {"providers" [{"id" "p" "compatibility" "openai-ish"}]}))
                             "is not a wire dialect"))
      (expect (str/includes? (first (config-validation/explain-problems
                                      {"providers" [{"id" "p"
                                                     "models" [{"name" "m" "api_style" "gpt"}]}]}))
                             "is not a wire dialect"))))
;; ── Conditional mounts (#89) ─────────────────────────────────────────────────
;; One catalog is shared by every machine: a mac laptop, a Linux box, and a host
;; where half the sibling repos are simply not checked out.

(def ^:private cross-machine-config
  {"workspace" {"filesystem" [{"id" "here" "path" "/" "description" "always present"}
                              {"id" "mac-only" "path" "/opt/mac" "when" {"os" "macos"}}
                              {"id" "nix" "path" "/opt/nix" "when" {"os" ["linux" "windows"]}}
                              {"id" "gated" "path" "/opt/gated" "when" {"exists" "/opt/toolchain"}}
                              {"id" "maybe" "path" "/opt/maybe" "optional" true "draft" "copy-only"}
                              {"id" "stale" "path" "/opt/stale"}]}})

(def ^:private mac-env
  "A macOS host on which only `/` and `/opt/mac` exist."
  {:os "macos" :exists? #{"/" "/opt/mac"}})

(defdescribe
  workspace-conditional-mount-test
  (it "the OS token set is closed and this host reports one of its members"
      (expect (= #{"macos" "linux" "wsl" "windows"} config-validation/workspace-os-values))
      (expect (contains? (conj config-validation/workspace-os-values "unknown")
                         (config-validation/host-os)))
      (expect (string? (:os (config-validation/mount-env))))
      (expect (ifn? (:exists? (config-validation/mount-env)))))
  (it "every mount status is decided against an explicit env, never this machine"
      (let [status #(config-validation/entry-mount-status % mac-env)]
        (expect (= :mounted (status {"id" "a" "path" "/" "when" {"os" "macos"}})))
        (expect (= :os-mismatch (status {"id" "a" "path" "/" "when" {"os" ["linux" "windows"]}})))
        (expect (= :when-absent (status {"id" "a" "path" "/" "when" {"exists" "/opt/nope"}})))
        (expect (= :optional-absent (status {"id" "a" "path" "/opt/nope" "optional" true})))
        ;; A missing unconditional root remains mounted and is reported.
        (expect (= :missing (status {"id" "a" "path" "/opt/nope"})))
        (expect (config-validation/entry-mounted? {"id" "a" "path" "/opt/nope"} mac-env))))
  (it "a linux clause covers WSL, but a wsl clause is not plain linux"
      (let [wsl
            {:os "wsl" :exists? #{"/"}}

            entry
            (fn [os]
              {"id" "a" "path" "/" "when" {"os" os}})]

        (expect (config-validation/entry-mounted? (entry "linux") wsl))
        (expect (config-validation/entry-mounted? (entry ["wsl" "macos"]) wsl))
        (expect (not (config-validation/entry-mounted? (entry "macos") wsl)))
        (expect (not (config-validation/entry-mounted? (entry "wsl")
                                                       {:os "linux" :exists? #{"/"}})))))
  (it "the jail catalog keeps declaration order and drops what this host lacks"
      (let [pol (config-validation/process-jail-config cross-machine-config mac-env)]
        (expect (= ["/" "/opt/mac" "/opt/stale" (get config-validation/vis-home-entry "path")]
                   (:allow-read-write pol)))
        (expect (= ["here" "mac-only" "stale"]
                   (mapv #(get % "id")
                         (config-validation/applicable-entries (get-in cross-machine-config
                                                                       ["workspace" "filesystem"])
                                                               mac-env))))))
  (it "draft isolation follows the same host filter"
      (expect (= {} (config-validation/workspace-draft-policies cross-machine-config mac-env)))
      (expect (= {"/opt/maybe" :copy-only}
                 (config-validation/workspace-draft-policies cross-machine-config
                                                             {:os "macos"
                                                              :exists? (constantly true)}))))
  (it "an allow list may name a root this host does not mount, but never an unknown id"
      (let [jailed (assoc cross-machine-config
                     "jail" {"enabled" true "filesystem" {"allow" ["here" "nix" "maybe"]}})]
        ;; `nix`/`maybe` are declared but unmounted here: skipped, not fatal — the
        ;; same vis.yml has to work on every machine.
        (expect (= ["/" (get config-validation/vis-home-entry "path")]
                   (:allow-read-write (config-validation/process-jail-config jailed mac-env))))
        ;; An id NO entry ever declared is still a hard config error.
        (expect (= :vis/invalid-config
                   (try (config-validation/process-jail-config
                          (assoc-in jailed ["jail" "filesystem" "allow"] ["ghost"])
                          mac-env)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it "diagnostics explain every root that did not mount, and stay empty when all do"
      (let [msgs
            (config-validation/workspace-mount-diagnostics cross-machine-config mac-env)

            by-id
            (into {} (map (juxt :id identity)) msgs)]

        (expect (= #{"nix" "gated" "maybe" "stale"} (set (keys by-id))))
        (expect (= :info (:level (by-id "nix"))))
        (expect (= :os-mismatch (:reason (by-id "nix"))))
        (expect (str/includes? (:message (by-id "nix")) "this host is macos"))
        (expect (= :when-absent (:reason (by-id "gated"))))
        (expect (str/includes? (:message (by-id "gated")) "/opt/toolchain"))
        (expect (= :optional-absent (:reason (by-id "maybe"))))
        (expect (= :info (:level (by-id "maybe"))))
        ;; Only a root that was NOT gated at all is a warning: dead grant.
        (expect (= :warn (:level (by-id "stale"))))
        (expect (str/includes? (:message (by-id "stale")) "/opt/stale"))
        (expect (every? #(seq (:remediation %)) msgs))
        (expect (= []
                   (config-validation/workspace-mount-diagnostics
                     {"workspace" {"filesystem" [{"id" "here" "path" "/"}]}}
                     mac-env)))))
  (it "`when` and `optional` are part of the closed entry contract"
      (let [entry (fn [m]
                    (assoc-in full-config ["workspace" "filesystem"] [m]))]
        (expect (config-validation/valid? (entry {"id" "x" "path" "~/ok" "optional" true})))
        (expect (config-validation/valid? (entry {"id" "x" "path" "~/ok" "when" {"os" "macos"}})))
        (expect (config-validation/valid?
                  (entry {"id" "x" "path" "~/ok" "when" {"os" ["linux" "wsl"] "exists" "/opt/x"}})))
        (expect (not (config-validation/valid? (entry {"id" "x" "path" "~/ok" "optional" "yes"}))))
        (expect (not (config-validation/valid? (entry
                                                 {"id" "x" "path" "~/ok" "when" {"os" "plan9"}}))))
        (expect (not (config-validation/valid? (entry
                                                 {"id" "x" "path" "~/ok" "when" {"exists" ""}}))))
        (expect (not (config-validation/valid? (entry
                                                 {"id" "x" "path" "~/ok" "when" {"nope" true}}))))
        (expect (= ["workspace.filesystem[0].when.nope: unknown key (config is closed)"]
                   (config-validation/explain-problems
                     {"workspace" {"filesystem"
                                   [{"id" "x" "path" "~/ok" "when" {"nope" true}}]}}))))))

;; ── macOS Mach services / Keychain (#90) ─────────────────────────────────────

(defdescribe
  jail-mach-services-test
  (it "keychain: true grants the three Security services and the keychain databases"
      (let [pol (config-validation/process-jail-config (assoc-in full-config
                                                         ["jail" "mach_services"]
                                                         {"keychain" true
                                                          "allow" ["com.example.agent"]}))]
        (expect (= ["com.apple.SecurityServer" "com.apple.ocspd" "com.apple.trustd.agent"]
                   config-validation/keychain-mach-services))
        (expect (= ["com.apple.SecurityServer" "com.apple.ocspd" "com.apple.trustd.agent"
                    "com.example.agent"]
                   (:mach-services pol)))
        ;; Reading the databases is what actually completes a lookup; they stay
        ;; out of the search sweep so credentials never surface in results.
        (expect (= ["~/Library/Keychains" "/Library/Keychains"]
                   config-validation/keychain-read-paths))
        (expect (every? (set (:allow-read pol)) config-validation/keychain-read-paths))
        (expect (every? (set (:no-search pol)) config-validation/keychain-read-paths))))
  (it "without the opt-in nothing is granted and nothing is added to the filesystem"
      (let [pol (config-validation/process-jail-config
                  (update full-config "jail" dissoc "mach_services"))]
        (expect (= [] (:mach-services pol)))
        (expect (= ["~/reference"] (:allow-read pol)))
        (expect (not-any? (set (:no-search pol)) config-validation/keychain-read-paths))))
  (it "mach_services is a closed block: a string list and a boolean"
      (expect (config-validation/valid?
                (assoc-in full-config ["jail" "mach_services"] {"allow" [] "keychain" true})))
      (expect (not (config-validation/valid? (assoc-in full-config
                                               ["jail" "mach_services"]
                                               {"allow" "com.apple.SecurityServer"}))))
      (expect (not (config-validation/valid?
                     (assoc-in full-config ["jail" "mach_services"] {"keychain" "true"}))))
      (expect (not (config-validation/valid?
                     (assoc-in full-config ["jail" "mach_services"] {"allow" [""]}))))
      (expect (not (config-validation/valid?
                     (assoc-in full-config ["jail" "mach_services"] {"mach" ["x"]}))))))
