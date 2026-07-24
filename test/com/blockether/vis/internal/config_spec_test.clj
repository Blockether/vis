(ns com.blockether.vis.internal.config-spec-test
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.config-spec :as config-spec]
            [lazytest.core :refer [defdescribe expect it]]
            [yamlstar.core :as yamlstar]))

(def full-config
  {"providers" [{"id" "anthropic"
                 "api_key" "secret"
                 "models"
                 [{"name" "claude" "context" 200000 "output_limit" 8192 "is_tool_call" true}]
                 "base_url" "https://api.anthropic.com"
                 "api_style" "anthropic"
                 "responses_path" "/v1/messages"
                 "llm_headers" {"X-Test" "yes"}
                 "extra_body" {"temperature" 0}}]
   "router" {"rate_limit" {"same_provider_delays_ms" [1000 2000]
                           "fallback_after_ms" 30000
                           "is_respect_retry_after" true
                           "is_fallback_provider" true}
             "network" {"timeout_ms" 300000
                        "ttft_timeout_ms" 30000
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
                  "search" true} {"id" "ref" "path" "~/reference" "access" "read-only"}
                 {"id" "gen" "path" "~/generated"}
                 {"id" "cache" "path" "~/.m2" "search" false "description" "maven cache"}]}
   "jail" {"enabled" true
           "filesystem" {"allow" ["svar" "ref" "gen" "cache"]}
           "inbound_ports" [5273 8080]
           "env" ["CI" "MY_TOKEN"]
           "deny_exec" ["definitely-not-a-real-binary-xyz"]}
   "network" {"allowed_domains" ["github.com"]
              "denied_domains" ["example.invalid"]
              "exclude_domains" ["opaque.example"]
              "allow_private" false
              "rules" [{"host" "api.example.com"
                        "access" "read-only"
                        "methods" ["POST"]
                        "ports" [443]
                        "allow" [{"method" "POST" "path" "/v1/**"}]}]}
   "environment" {"ANTHROPIC_API_KEY" "secret"}
   "db_spec" {"backend" "sqlite" "path" "/tmp/vis.db"}
   "search" {"include_gitignored_paths" ["repositories/"] "always_exclude" ["target/"]}
   "toggles" {"reasoning_level" "deep"}
   "python" {"resource_cache" "~/.vis/cache/graal-resources"}
   "tui_settings" {"theme_name" "dark" "contributors_disabled" ["voice"]}
   "mcp" {"servers" {"local" {"transport" "stdio"
                              "command" "npx"
                              "args" ["-y" "server"]
                              "cwd" "/tmp"
                              "env" {"TOKEN" "secret"}}
                     "remote" {"transport" "http"
                               "url" "https://mcp.example.com"
                               "headers" {"Authorization" "Bearer secret"}}}}
   "message_queue" {"breaker_threshold" 3
                    "retry_backoff_ms" [2000 8000 30000]
                    "halfopen_probe_ms" 60000
                    "retry_after_cap_ms" 120000}})

(defdescribe
  config-contract-test
  (it "registers a complete string-keyed clojure.spec contract"
      (expect (s/get-spec :com.blockether.vis.internal.config-spec/config))
      (expect (config-spec/valid? full-config)))
  (it
    "rejects keyword keys, aliases, unknown keys, and invalid security values"
    (expect (not (config-spec/valid? {:filesystem {}})))
    (expect (not (config-spec/valid? {"filesystem" {}})))
    ;; #50: snake_case is the ONLY accepted spelling — kebab-case keys are rejected.
    (expect (not (config-spec/valid? (assoc-in full-config ["providers" 0 "api-key"] "k"))))
    (expect (not (config-spec/valid?
                   (assoc-in full-config ["providers" 0 "base-url"] "https://x"))))
    (expect (not (config-spec/valid? (assoc full-config "system-prompt" "hi"))))
    (expect (not (config-spec/valid? (assoc full-config "db-spec" {"backend" "sqlite"}))))
    ;; Workspace entries: a rooted path is required and unknown keys are rejected.
    (expect (not (config-spec/valid? (assoc-in full-config
                                       ["workspace" "filesystem"]
                                       [{"id" "x" "path" "./relative"}]))))
    (expect (not (config-spec/valid? (assoc-in full-config
                                       ["workspace" "filesystem"]
                                       [{"id" "x" "path" "~/ok" "note" "unknown-key"}]))))
    ;; `access` is a closed enum and `description` may not be blank.
    (expect (not (config-spec/valid? (assoc-in full-config
                                       ["workspace" "filesystem"]
                                       [{"id" "x" "path" "~/ok" "access" "sideways"}]))))
    (expect (not (config-spec/valid? (assoc-in full-config
                                       ["workspace" "filesystem"]
                                       [{"id" "x" "path" "~/ok" "description" ""}]))))
    ;; jail.filesystem is pure id admission — only an `allow` STRING VECTOR, nothing else.
    (expect (not (config-spec/valid? (assoc-in full-config ["jail" "filesystem" "allow"] "svar"))))
    (expect (not (config-spec/valid? (assoc-in full-config ["jail" "filesystem" "deny"] ["svar"]))))
    (expect (not (config-spec/valid? (assoc-in full-config ["jail" "inbound_ports"] [0]))))
    (expect (config-spec/valid?
              (assoc-in full-config
                ["workspace" "filesystem"]
                [{"id" "ok" "path" "~/home-ok" "description" "why" "search" false}])))
    ;; deny-exec: a list of executable names (or rooted paths) to block by read.
    (expect (config-spec/valid? (assoc-in full-config ["jail" "deny_exec"] ["curl" "ssh"])))
    (expect (not (config-spec/valid? (assoc-in full-config ["jail" "deny_exec"] "curl"))))
    (expect (not (config-spec/valid? (assoc-in full-config ["jail" "deny_exec"] [""]))))
    ;; Descriptions of ADMITTED roots flow into the derived policy, keyed by grant path.
    (expect (= {"/opt/svar" "a sibling repo" "~/.m2" "maven cache"}
               (:path-descriptions (config-spec/process-jail-config full-config))))
    ;; Network is policy data, never an independent on/off escape hatch.
    (expect (not (config-spec/valid? (assoc-in full-config ["network" "enabled"] false))))
    (expect (not (config-spec/valid? (assoc-in full-config ["network" "rules" 0 "oops"] true))))
    ;; :ports is a list of valid port integers.
    (expect (config-spec/valid? (assoc-in full-config ["network" "rules" 0 "ports"] [22 443])))
    (expect (not (config-spec/valid? (assoc-in full-config ["network" "rules" 0 "ports"] [70000]))))
    (expect (not (config-spec/valid? (assoc-in full-config ["network" "rules" 0 "ports"] ["443"]))))
    ;; GraalPy resource cache: closed block, non-blank path only.
    (expect (not (config-spec/valid? (assoc-in full-config ["python" "cache"] "/x"))))
    (expect (not (config-spec/valid? (assoc-in full-config ["python" "resource_cache"] ""))))
    ;; Queue tuning: a closed block of positive numbers; the backoff is a non-empty list.
    (expect (config-spec/valid? (assoc-in full-config ["message_queue" "breaker_threshold"] 5)))
    (expect (not (config-spec/valid?
                   (assoc-in full-config ["message_queue" "breaker_threshold"] 0))))
    (expect (not (config-spec/valid?
                   (assoc-in full-config ["message_queue" "retry_backoff_ms"] []))))
    (expect (not (config-spec/valid?
                   (assoc-in full-config ["message_queue" "retry_backoff_ms"] ["2s"]))))
    (expect (not (config-spec/valid? (assoc-in full-config ["message_queue" "unknown"] 1)))))
  (it "derives process-jail and network maps from the same string contract"
      (expect (= {:disabled? false
                  :allow-read-write ["/opt/svar" "~/generated" "~/.m2"]
                  :allow-read ["~/reference"]
                  :allow-write []
                  :deny-read []
                  :deny-write []
                  :deny-exec []
                  :no-search ["~/.m2"]
                  :inbound-ports [5273 8080]
                  :env-passthrough ["CI" "MY_TOKEN"]
                  :path-descriptions {"/opt/svar" "a sibling repo" "~/.m2" "maven cache"}}
                 (config-spec/process-jail-config full-config)))
      (expect (true? (:disabled? (config-spec/process-jail-config
                                   (assoc-in full-config ["jail" "enabled"] false)))))
      (expect (false? (:disabled? (config-spec/process-jail-config
                                    (assoc-in full-config ["jail" "enabled"] true)))))
      ;; DEFAULT is OFF: absent `jail.enabled` key => jail disabled (opt-in).
      (expect (true? (:disabled? (config-spec/process-jail-config
                                   (update full-config "jail" dissoc "enabled")))))
      (expect (= {:allowed-domains ["github.com"]
                  :denied-domains ["example.invalid"]
                  :exclude-domains ["opaque.example"]
                  :allow-private false
                  :rules [{:host "api.example.com"
                           :access "read-only"
                           :methods ["POST"]
                           :ports [443]
                           :allow [{:method "POST" :path "/v1/**"}]}]}
                 (config-spec/network-config full-config))))
  (it
    "resolves jail.deny-exec into a separate deny-exec list (rooted passthrough, drops unresolvable)"
    (let
      [pol
       (config-spec/process-jail-config (assoc-in full-config
                                          ["jail" "deny_exec"]
                                          ["/opt/nope/curl" "definitely-not-a-real-binary-xyz"]))

       denied
       (set (:deny-exec pol))]

      ;; absolute/home entries pass through verbatim (deny fails safe)
      (expect (contains? denied "/opt/nope/curl"))
      ;; an unresolvable bare name contributes nothing
      (expect (not (contains? denied "definitely-not-a-real-binary-xyz")))
      ;; deny-exec is exec-only; the filesystem deny-read list stays empty.
      (expect (= [] (:deny-read pol)))))
  (it "redacts credentials from validation failures"
      (let
        [bad
         (assoc-in full-config ["providers" 0 "unknown"] true)

         data
         (try (config-spec/assert-config! bad "vis.yml") nil (catch Exception e (ex-data e)))]

        (expect (= :vis/invalid-config (:type data)))
        (expect (not (.contains (pr-str data) "secret")))
        (expect (.contains (pr-str data) "<redacted>"))))
  (it "validates parser output before any runtime adaptation"
      (require 'com.blockether.vis.internal.config :reload)
      (let
        [file
         (io/file "target/invalid-vis-config.yml")

         read-yaml
         (var-get (ns-resolve 'com.blockether.vis.internal.config 'read-yaml-config-map))]

        (try (.mkdirs (.getParentFile file))
             (spit file "jail:\n  filesystem:\n    allow_reed:\n      - ../escape\n")
             (let [data (try (read-yaml (.getPath file)) nil (catch Exception e (ex-data e)))]
               (expect (= :vis/invalid-config (:type data)))
               (expect (= (.getPath file) (:source data))))
             (finally (io/delete-file file true))))))

(defdescribe
  config-completeness-test
  (it "registers specs for every fixed and dynamic configuration block"
      (doseq
        [spec-name [:config :model-map :model :models :provider :providers :rate-limit
                    :router-network :budget :tokens :router :workspace-entry :workspace-entries
                    :workspace :jail-filesystem :jail :network-rule-allow :network-rule-allows
                    :network-rule :network-rules :network :prompt-map :system-prompt :search
                    :db-spec :tui-settings :mcp-server :mcp-servers :mcp]]
        (expect (s/get-spec (keyword "com.blockether.vis.internal.config-spec" (name spec-name))))))
  (it
    "keeps every declared key set, schema, and exhaustive fixture in sync"
    (let
      [provider
       (first (get full-config "providers"))

       model
       (first (get provider "models"))

       router
       (get full-config "router")

       jail
       (get full-config "jail")

       filesystem
       (get jail "filesystem")

       network
       (get full-config "network")

       rule
       (first (get network "rules"))

       rule-allow
       (first (get rule "allow"))

       mcp
       (get full-config "mcp")

       servers
       (vals (get mcp "servers"))

       workspace
       (get full-config "workspace")

       ws-entry
       (first (get workspace "filesystem"))

       cases
       [[config-spec/config-keys config-spec/config-schema (set (keys full-config))]
        [config-spec/model-keys config-spec/model-schema (set (keys model))]
        [config-spec/provider-keys config-spec/provider-schema (set (keys provider))]
        [config-spec/rate-limit-keys config-spec/rate-limit-schema
         (set (keys (get router "rate_limit")))]
        [config-spec/router-network-keys config-spec/router-network-schema
         (set (keys (get router "network")))]
        [config-spec/budget-keys config-spec/budget-schema (set (keys (get router "budget")))]
        [config-spec/token-keys config-spec/token-schema (set (keys (get router "tokens")))]
        [config-spec/router-keys config-spec/router-schema (set (keys router))]
        [config-spec/workspace-entry-keys config-spec/workspace-entry-schema (set (keys ws-entry))]
        [config-spec/workspace-keys config-spec/workspace-schema (set (keys workspace))]
        [config-spec/jail-filesystem-keys config-spec/jail-filesystem-schema
         (set (keys filesystem))] [config-spec/jail-keys config-spec/jail-schema (set (keys jail))]
        [config-spec/network-rule-allow-keys config-spec/network-rule-allow-schema
         (set (keys rule-allow))]
        [config-spec/network-rule-keys config-spec/network-rule-schema (set (keys rule))]
        [config-spec/network-keys config-spec/network-schema (set (keys network))]
        [config-spec/prompt-keys config-spec/prompt-schema
         (set (keys (get full-config "system_prompt")))]
        [config-spec/search-keys config-spec/search-schema (set (keys (get full-config "search")))]
        [config-spec/db-keys config-spec/db-schema (set (keys (get full-config "db_spec")))]
        [config-spec/tui-keys config-spec/tui-schema (set (keys (get full-config "tui_settings")))]
        [config-spec/mcp-keys config-spec/mcp-schema (set (keys mcp))]
        [config-spec/python-keys config-spec/python-schema (set (keys (get full-config "python")))]
        [config-spec/message-queue-keys config-spec/message-queue-schema
         (set (keys (get full-config "message_queue")))]
        [config-spec/mcp-server-keys config-spec/mcp-server-schema
         (into #{} (mapcat keys) servers)]]]

      (doseq [[declared schema fixture] cases]
        (expect (= declared (set (keys schema))))
        (expect (= declared fixture)))))
  (it "validates the repository vis.yml through the same root spec"
      (let [wire (yamlstar/load (slurp (io/file "vis.yml")))]
        ;; The repo vis.yml ships fully commented (a documented template), so it
        ;; parses to nil. When it carries content it must satisfy the root spec.
        (expect (or (nil? wire) (and (every? string? (keys wire)) (config-spec/valid? wire))))))
  (it "checks recursively user-owned request and pricing maps without keywordizing"
      (expect (config-spec/valid? (assoc-in full-config
                                    ["providers" 0 "extra_body"]
                                    {"thinking" {"type" "enabled" "budget_tokens" 2048}
                                     "stop" ["DONE" nil]})))
      (expect (not (config-spec/valid? (assoc-in full-config
                                         ["providers" 0 "extra_body"]
                                         {:keyword-key "not YAML wire data"}))))
      (expect (not (config-spec/valid? (assoc-in full-config
                                         ["router" "tokens" "pricing"]
                                         {"claude" {:input 1.0}})))))
  (it "explain-problems names each offending top-level key, [] when valid"
      (expect (= []
                 (config-spec/explain-problems {"providers" [{"id" "a" "models" [{"name" "m"}]}]})))
      (expect (= [] (config-spec/explain-problems nil)))
      (expect (= ["config: expected a YAML map with string keys"] (config-spec/explain-problems 7)))
      (expect (= ["nope: unknown top-level config key (config is closed)"]
                 (config-spec/explain-problems {"nope" 1})))
      (expect (= ["network: value rejected by the network contract"]
                 (config-spec/explain-problems {"network" 5})))))
