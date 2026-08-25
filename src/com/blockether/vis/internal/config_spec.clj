(ns com.blockether.vis.internal.config-spec
  "The executable contract for the YAML representation of Vis configuration.

   YAMLStar returns maps with string keys. This namespace validates that exact
   representation: snake_case string keys ONLY (kebab-case is rejected), no
   recursive keywordization, and no acceptance of keyword-keyed lookalikes.
   Maps are closed unless their keys are
   deliberately user-defined (environment variables, headers, toggle ids, MCP
   server names, pricing/model tables, and provider request bodies).

   Security consumers derive their internal policy maps through the adapters at
   the end of this namespace, so validation and enforcement share one contract."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- positive-int? [x] (and (integer? x) (pos? (long x))))
(defn- non-negative-number? [x] (and (number? x) (not (neg? (double x)))))
(defn- port? [x] (and (integer? x) (<= 1 x 65535)))
(defn- port-list? [x] (and (or (vector? x) (set? x)) (every? port? x)))
(defn- scalar? [x] (or (string? x) (boolean? x) (number? x) (nil? x)))
(defn- string-list? [x] (and (vector? x) (every? non-blank-string? x)))
(defn- non-empty-string-list? [x] (and (string-list? x) (seq x)))
(def ^:private env-var-name-re #"[A-Za-z_][A-Za-z0-9_]*")
(defn- env-var-name? [x] (and (string? x) (boolean (re-matches env-var-name-re x))))
(defn- rooted-path?
  "A filesystem grant must be absolute (\"/\") or home-relative (\"~\", \"~/\") —
   a bare-relative path resolves against the gateway process cwd, silently
   wrong for a multi-session gateway (allow) or an invalid subpath (deny)."
  [x]
  (and (non-blank-string? x) (or (str/starts-with? x "/") (= x "~") (str/starts-with? x "~/"))))
(defn- rooted-path-list? [x] (and (vector? x) (every? rooted-path? x)))
(defn- string-map? [m] (and (map? m) (every? string? (keys m)) (every? string? (vals m))))
(defn- named-scalar-map?
  [m]
  (and (map? m) (every? non-blank-string? (keys m)) (every? scalar? (vals m))))
(defn- number-map? [m] (and (map? m) (every? non-blank-string? (keys m)) (every? number? (vals m))))

(declare yaml-value?)

(defn- yaml-map?
  "A user-defined YAML map: string keys and recursively YAML-safe values only."
  [m]
  (and (map? m) (every? string? (keys m)) (every? yaml-value? (vals m))))

(defn- yaml-value?
  "The exact recursive value domain YAML config may pass through verbatim."
  [x]
  (or (scalar? x) (and (vector? x) (every? yaml-value? x)) (yaml-map? x)))

(defn- named-yaml-map? [m] (and (yaml-map? m) (every? non-blank-string? (keys m))))

(defn- one-of [values] #(contains? values %))
(defn- spec-pred [spec] #(s/valid? spec %))

(defn- closed-map?
  "Validate a string-keyed map against key -> predicate schema."
  ([schema m] (closed-map? schema #{} m))
  ([schema required m]
   (and (map? m)
        (every? string? (keys m))
        (every? #(contains? m %) required)
        (every? (fn [[k v]]
                  (when-let [pred (get schema k)]
                    (pred v)))
                m))))

(def ^:private sensitive-keys #{"api_key" "environment" "env" "headers" "llm_headers" "extra_body"})

(defn- redact
  [x]
  (cond (map? x) (into {}
                       (map (fn [[k v]]
                              [k (if (sensitive-keys k) "<redacted>" (redact v))]))
                       x)
        (vector? x) (mapv redact x)
        (set? x) (set (map redact x))
        :else x))

;; Provider/model contract -----------------------------------------------------

(def model-keys #{"name" "context" "output_limit" "is_tool_call" "api_style"})
(def network-timeout-schema
  {"timeout_ms" non-negative-number?
   "ttft_timeout_ms" non-negative-number?
   "first_byte_timeout_ms" non-negative-number?
   "idle_timeout_ms" non-negative-number?
   "semantic_timeout_ms" non-negative-number?})

(defn- network-timeout-map? [m] (closed-map? network-timeout-schema m))

(def provider-keys
  #{"id" "api_key" "api_key_command" "models" "base_url" "compatibility" "api_style"
    "responses_path" "llm_headers" "extra_body" "network" "is_stateless" "is_image_input"})

(def api-style-aliases
  "Every spelling Vis accepts for a wire dialect -> the `:api-style` svar
   dispatches on. Forgiving in, exact out: a value is normalized (trimmed,
   lower-cased, `_`/whitespace -> `-`) and must land in this table.

   svar `case`s the api-style and every value it does not recognise falls
   through to `/chat/completions`, so a near-miss like `openai_responses` would
   otherwise SILENTLY post a Responses history to the chat wire. This table is
   how a near-miss becomes the right dialect, and [[api-style?]] is how anything
   else becomes a refused config instead of a wrong endpoint.

   ONE vocabulary for both keys: `compatibility` is the word a user is taught,
   `api_style` the same value under svar's own name."
  {"anthropic" :anthropic
   "anthropic-messages" :anthropic
   "claude" :anthropic
   "messages" :anthropic
   "openai" :openai-compatible-chat
   "openai-chat" :openai-compatible-chat
   "openai-compatible" :openai-compatible-chat
   "openai-compatible-chat" :openai-compatible-chat
   "chat" :openai-compatible-chat
   "chat-completions" :openai-compatible-chat
   "openai-responses" :openai-compatible-responses
   "openai-compatible-responses" :openai-compatible-responses
   "responses" :openai-compatible-responses
   "gemini" :gemini
   "google" :gemini
   "google-gemini" :gemini})

(def api-style-values
  "The dialects a config may name, in the spelling the documentation teaches.
   Every other key of [[api-style-aliases]] is an accepted alias of one of these."
  ["anthropic" "openai" "openai-responses" "gemini"])

(defn normalize-api-style
  "`v` (string or keyword, in any accepted spelling) -> svar's `:api-style`,
   or nil when it names no dialect."
  [v]
  (when (or (string? v) (keyword? v) (symbol? v))
    (get api-style-aliases
         (-> (if (or (keyword? v) (symbol? v)) (name v) v)
             str/trim
             str/lower-case
             (str/replace #"[_\s]+" "-")))))

(defn api-style?
  "True when `x` names a wire dialect Vis can hand svar."
  [x]
  (and (non-blank-string? x) (some? (normalize-api-style x))))

(def model-schema
  {"name" non-blank-string?
   "context" positive-int?
   "output_limit" positive-int?
   "is_tool_call" boolean?
   "api_style" api-style?})

(s/def ::api-key-command
  ;; Structured argv, never a shell string: the helper is exec'd directly, so a
  ;; bare string is ONE argument and is never word-split. Both spellings are the
  ;; same value shape; there is no shell to quote for.
  (s/or :argv non-empty-string-list?
        :program non-blank-string?))

(s/def ::model-map #(closed-map? model-schema #{"name"} %))
(s/def ::model
  (s/or :name non-blank-string?
        :map ::model-map))
(s/def ::models (s/coll-of ::model :kind vector?))

(def provider-schema
  {"id" non-blank-string?
   "api_key" string?
   ;; Command-backed credential: argv whose trimmed stdout IS the API key.
   ;; Resolved live (short-lived SSO/vault helpers) and NEVER persisted.
   "api_key_command" (spec-pred ::api-key-command)
   "models" (spec-pred ::models)
   "base_url" non-blank-string?
   "compatibility" api-style?
   "api_style" api-style?
   "responses_path" non-blank-string?
   "llm_headers" string-map?
   "extra_body" yaml-map?
   "network" network-timeout-map?
   ;; Gateways that load-balance across several Azure OpenAI resources cannot
   ;; resolve an item id minted by another replica; replaying one is a hard
   ;; HTTP 400. `is_stateless: true` stops sending server-minted item ids
   ;; (reasoning id + encrypted_content, function_call id) for this provider.
   "is_stateless" boolean?
   ;; A gateway can proxy a multimodal model and still refuse an image content
   ;; part itself (HTTP 400 `unknown variant image_url, expected text`).
   ;; `is_image_input: false` vetoes vision for every model it serves.
   "is_image_input" boolean?})

(s/def ::provider #(closed-map? provider-schema #{"id"} %))
(s/def ::providers (s/coll-of ::provider :kind vector?))

;; Router contract -------------------------------------------------------------

(def rate-limit-keys
  #{"same_provider_delays_ms" "fallback_after_ms" "is_respect_retry_after" "is_fallback_provider"})
(def router-network-keys
  #{"timeout_ms" "ttft_timeout_ms" "first_byte_timeout_ms" "idle_timeout_ms" "semantic_timeout_ms"
    "max_retries" "initial_delay_ms" "max_delay_ms" "multiplier"})
(def budget-keys #{"max_tokens" "max_cost"})
(def token-keys #{"is_check_context" "pricing" "context_limits" "output_reserve"})
(def router-keys
  #{"rate_limit" "network" "budget" "tokens" "failure_threshold" "recovery_ms"
    "transient_status_codes" "window_ms" "cooldown_ms" "max_wait_ms"})

(def rate-limit-schema
  {"same_provider_delays_ms" #(and (vector? %) (every? positive-int? %))
   "fallback_after_ms" positive-int?
   "is_respect_retry_after" boolean?
   "is_fallback_provider" boolean?})
(s/def ::rate-limit #(closed-map? rate-limit-schema %))

(def router-network-schema
  {"timeout_ms" positive-int?
   "ttft_timeout_ms" positive-int?
   "idle_timeout_ms" positive-int?
   "semantic_timeout_ms" positive-int?
   "max_retries" positive-int?
   "initial_delay_ms" positive-int?
   "max_delay_ms" positive-int?
   "multiplier" number?})
(s/def ::router-network #(closed-map? router-network-schema %))

(def budget-schema {"max_tokens" positive-int? "max_cost" non-negative-number?})
(s/def ::budget #(closed-map? budget-schema %))

(def token-schema
  {"is_check_context" boolean?
   "pricing" named-yaml-map?
   "context_limits" number-map?
   "output_reserve" positive-int?})
(s/def ::tokens #(closed-map? token-schema %))

(def router-schema
  {"rate_limit" (spec-pred ::rate-limit)
   "network" (spec-pred ::router-network)
   "budget" (spec-pred ::budget)
   "tokens" (spec-pred ::tokens)
   "failure_threshold" positive-int?
   "recovery_ms" positive-int?
   "transient_status_codes" #(and (or (vector? %) (set? %))
                                  (every? (fn [n]
                                            (and (integer? n) (<= 100 n 599)))
                                          %))
   "window_ms" positive-int?
   "cooldown_ms" positive-int?
   "max_wait_ms" positive-int?})
(s/def ::router #(closed-map? router-schema %))

;; Sandbox contracts -----------------------------------------------------------

;; ── Workspace filesystem catalog ─────────────────────────────────────────────
;; ONE documented catalog of every filesystem root. Each entry is
;; `{id, path, description, access?, search?, draft?, when?, optional?}`. `access`
;; (default read-write) picks RW vs read-only; `search: false` keeps the root OUT
;; of the default grep sweep (explicit paths still reach it); `draft` (default
;; `shared`) is the root's ISOLATION policy for a drafted session; `when` mounts
;; the root CONDITIONALLY (host OS and/or an existing path) and `optional: true`
;; drops it silently when its own path is absent. The catalog is the sole source
;; of truth; `jail.filesystem.allow` references entries by id.
(def workspace-entry-keys #{"id" "path" "description" "access" "search" "draft" "when" "optional"})
(def workspace-access-values #{"read-only" "readonly" "ro" "read-write" "readwrite" "rw"})
(def workspace-draft-values
  "Per-root DRAFT isolation vocabulary.

   `shared`         — the draft writes THROUGH to the real root (default).
   `copy-only`      — the draft gets a private copy; `apply!` never lands it back.
   `copy-and-apply` — private copy, landed back into the real root on `apply!`.
   `not-allowed`    — the root is withheld from a drafted session entirely."
  #{"shared" "copy-only" "copy-and-apply" "not-allowed"})
(def workspace-os-values
  "Host tokens `when.os` may name. `wsl` is Linux under WSL: a WSL host also
   matches a `linux` clause, but a plain Linux host never matches `wsl`."
  #{"macos" "linux" "wsl" "windows"})
(def workspace-when-keys #{"os" "exists"})
(def workspace-when-schema
  {"os" #(or (contains? workspace-os-values %)
             (and (vector? %) (seq %) (every? workspace-os-values %)))
   "exists" rooted-path?})
(s/def ::workspace-when #(closed-map? workspace-when-schema %))
(def workspace-entry-schema
  {"id" non-blank-string?
   "path" rooted-path?
   "description" non-blank-string?
   "access" (one-of workspace-access-values)
   "search" boolean?
   "draft" (one-of workspace-draft-values)
   "when" (spec-pred ::workspace-when)
   "optional" boolean?})
(s/def ::workspace-entry #(closed-map? workspace-entry-schema #{"id" "path"} %))
(s/def ::workspace-entries (s/coll-of ::workspace-entry :kind vector?))
(def workspace-keys #{"filesystem"})
(def workspace-schema {"filesystem" (spec-pred ::workspace-entries)})
(s/def ::workspace #(closed-map? workspace-schema %))

;; ── Jail filesystem admission ────────────────────────────────────────────────
;; Pure id references into the workspace catalog (deny-by-omission): a catalog
;; root is OUTSIDE the OS jail unless its id appears in `allow`.
(def jail-filesystem-keys #{"allow"})
(def jail-filesystem-schema {"allow" #(and (vector? %) (every? non-blank-string? %))})
(s/def ::jail-filesystem #(closed-map? jail-filesystem-schema %))

;; ── Jail Mach services (macOS) ───────────────────────────────────────────────
;; Seatbelt denies every Mach lookup by default, which is what breaks Keychain
;; reads (`security`, `gh auth token`, `git credential-osxkeychain`) inside the
;; jail. `keychain: true` grants EXACTLY the services and keychain directories a
;; Keychain read needs; `allow` is the escape hatch for anything else.
(def jail-mach-services-keys #{"allow" "keychain"})
(def jail-mach-services-schema
  {"allow" #(and (vector? %) (every? non-blank-string? %)) "keychain" boolean?})
(s/def ::jail-mach-services #(closed-map? jail-mach-services-schema %))

(def keychain-mach-services
  "The Mach services a confined child must reach to read the macOS Keychain:
   the security server itself plus the trust/revocation daemons it calls into.
   Verified against `security`, `gh auth token` and `git credential-osxkeychain`
   under `sandbox-exec`; without them the lookup fails with
   `SecKeychainSearchCreateFromAttributes: … parameters … not valid`."
  ["com.apple.SecurityServer" "com.apple.ocspd" "com.apple.trustd.agent"])

(def keychain-read-paths
  "Keychain databases the same lookup reads. Kept OUT of the search sweep — they
   are credentials, never grep fodder."
  ["~/Library/Keychains" "/Library/Keychains"])

;; ── Jail environment mode ────────────────────────────────────────────────────
;; `jail.environment` is the ONE knob over the OPERATOR's ambient environment —
;; whatever was exported into the shell that launched Vis. It is deliberately a
;; MODE and not a list of names: the PROJECT names its variables (the
;; workspace's `.env`, plus `environment:` for what a dotenv file cannot say),
;; and every one of them reaches Vis' children — confined or not — with the
;; value its own source produced. A second list of the same names (the retired
;; `jail.env`) bought nothing: it could only re-admit an ambient variable, never
;; a `dotenv:`/`keychain:`/`command:` value.
;;
;;   "declared" (default) — the child gets the project's variables plus a fixed
;;                          non-secret basics allowlist (`PATH`, `HOME`, `LANG`
;;                          …). Nothing the operator exported comes along;
;;                          re-admit one BY NAME with `environment: {CI: {env:
;;                          CI}}`.
;;   "inherit"            — the child additionally inherits the whole ambient
;;                          environment. Filesystem, network, exec and Mach
;;                          confinement are untouched, but every exported API key
;;                          and token is then readable by anything the agent
;;                          runs. The escape hatch for a toolchain that needs a
;;                          pile of host variables (`JAVA_HOME`, `ANDROID_HOME`,
;;                          `SSH_AUTH_SOCK`, …), never a default.
;;
;; Both modes still refuse every pre-exec hijack name (`LD_*`, `DYLD_*`,
;; `PERL*`, `BASH_ENV`, `IFS` …): those execute in the UNCONFINED detacher /
;; enforcer hops, so no mode and no declaration can buy them back
;; (`process-jail/jailed-child-env`).
(def jail-environment-values #{"declared" "inherit"})
(def jail-keys #{"enabled" "environment" "filesystem" "network" "deny_exec" "mach_services"})
(def jail-schema
  {"enabled" boolean?
   "environment" (one-of jail-environment-values)
   "filesystem" (spec-pred ::jail-filesystem)
   "network" (spec-pred ::network)
   "deny_exec" string-list?
   "mach_services" (spec-pred ::jail-mach-services)})
(s/def ::jail #(closed-map? jail-schema %))

(def network-rule-allow-keys #{"method" "path"})
(def network-rule-keys #{"host" "access" "methods" "allow" "ports"})
(def network-keys
  #{"allowed_domains" "denied_domains" "exclude_domains" "allow_private" "inbound_ports" "rules"})
(def network-rule-allow-schema {"method" non-blank-string? "path" non-blank-string?})
(s/def ::network-rule-allow #(closed-map? network-rule-allow-schema #{"method"} %))
(s/def ::network-rule-allows (s/coll-of ::network-rule-allow :kind vector?))

(def network-rule-schema
  {"host" non-blank-string?
   "access" (one-of #{"read-only" "readonly" "ro" "read-write" "readwrite" "rw" "full" "all" "none"
                      "deny" "closed"})
   "methods" #(and (or (vector? %) (set? %)) (every? non-blank-string? %))
   "ports" port-list?
   "allow" (spec-pred ::network-rule-allows)})
(s/def ::network-rule #(closed-map? network-rule-schema #{"host"} %))
(s/def ::network-rules (s/coll-of ::network-rule :kind vector?))

(def network-schema
  {"allowed_domains" string-list?
   "denied_domains" string-list?
   "exclude_domains" string-list?
   "allow_private" boolean?
   "inbound_ports" #(and (vector? %) (= (count %) (count (distinct %))) (every? port? %))
   "rules" (spec-pred ::network-rules)})
(s/def ::network #(closed-map? network-schema %))

;; Remaining top-level blocks --------------------------------------------------

(def prompt-keys #{"text" "is_replace"})
(def grep-keys #{"include_gitignored_paths" "always_exclude"})
(def db-keys #{"backend" "path"})
(def tui-keys #{"theme_name" "contributors_disabled"})
(def mcp-keys #{"servers"})
(def mcp-server-keys
  #{"transport" "command" "args" "cwd" "env" "url" "headers" "enabled" "timeout_ms" "listen"
    "auth"})
(def python-keys #{"resource_cache" "source_paths" "interpreter" "runner"})
(def titling-keys #{"mode" "provider" "model"})
(def vision-fact-keys #{"learned_at" "providers"})
(def vision-eye-keys #{"provider" "model" "learned_at"})
(def vision-memory-keys #{"blind_providers" "blind_models" "working_eye"})
(def config-keys
  #{"providers" "removed_providers" "default_provider" "default_model" "fallback_provider"
    "fallback_model" "router" "system_prompt" "workspace" "jail" "environment" "db_spec" "grep"
    "toggles" "tui_settings" "mcp" "python" "titling" "vision_memory"})

(def prompt-schema {"text" string? "is_replace" boolean?})
(s/def ::prompt-map #(closed-map? prompt-schema #{"text"} %))
(s/def ::system-prompt
  (s/or :text string?
        :map ::prompt-map))

(def grep-schema {"include_gitignored_paths" string-list? "always_exclude" string-list?})
(s/def ::grep #(closed-map? grep-schema %))

(def db-schema {"backend" non-blank-string? "path" non-blank-string?})
(s/def ::db-spec #(closed-map? db-schema #{"backend"} %))

(def tui-schema
  {"theme_name" non-blank-string?
   "contributors_disabled" #(and (or (vector? %) (set? %)) (every? non-blank-string? %))})
(s/def ::tui-settings #(closed-map? tui-schema %))

(s/def ::python-interpreter
  ;; Same value shape as a provider's `api_key_command`: an argv vector, or a
  ;; bare program/path that is ONE argument and is never word-split.
  (s/or :argv non-empty-string-list?
        :program non-blank-string?))

(def python-schema
  ;; `resource_cache`: GraalPy internal-resource cache root (where the Python
  ;; stdlib extracts at runtime). Read ONCE per process at polyglot-engine boot;
  ;; the explicit `-Dpolyglot.engine.userResourceCache` system property wins over
  ;; this key.
  ;; `source_paths`: extra import roots prepended to `sys.path` for `vis-agent python`,
  ;; on top of what the project's own packaging metadata declares -- the escape
  ;; hatch for a layout vis cannot infer. Relative to the working directory; `~`
  ;; expands.
  ;; `interpreter`: the argv PREFIX that launches this project's Python for
  ;; `repl_start` / `repl_eval` and the `project` test runner, pinned instead of
  ;; detected (uv / poetry / .venv / python3). A path-like entry resolves against
  ;; the project dir; `~` expands; a bare name is looked up on PATH.
  ;; `runner`: default `run_tests({"language": "python"})` backend -- the hermetic `graalpy`
  ;; sandbox or the `project` interpreter's own pytest. Explicit call arguments
  ;; still win.
  {"resource_cache" non-blank-string?
   "source_paths" string-list?
   "interpreter" (spec-pred ::python-interpreter)
   "runner" (one-of #{"graalpy" "project"})})
(s/def ::python #(closed-map? python-schema %))


(def mcp-auth-schema
  {"client_id" non-blank-string?
   "scope" non-blank-string?
   "authorization_timeout_ms" positive-int?})
(s/def ::mcp-auth #(closed-map? mcp-auth-schema %))

(def mcp-server-schema
  {"transport" (one-of #{"stdio" "streamable_http" "http"})
   "command" non-blank-string?
   "args" #(and (vector? %) (every? string? %))
   "cwd" non-blank-string?
   "env" string-map?
   "url" non-blank-string?
   "headers" string-map?
   "enabled" boolean?
   "timeout_ms" positive-int?
   "listen" boolean?
   "auth" (spec-pred ::mcp-auth)})
(s/def ::mcp-server
  #(and (closed-map? mcp-server-schema %)
        ;; `command`/`args` and `url`/`headers` are the standard MCP client
        ;; configuration shapes. `transport` is optional so a standard config
        ;; can omit it; `http` remains a read-compatible alias for pre-canonical
        ;; Vis state and is normalized to `streamable_http` on a gateway save.
        (let [transport
              (case (get % "transport")
                "http"
                "streamable_http"

                (get % "transport"))

              has-cmd?
              (non-blank-string? (get % "command"))

              has-url?
              (non-blank-string? (get % "url"))]

          (case transport
            "stdio"
            (and has-cmd? (not has-url?))

            "streamable_http"
            (and has-url? (not has-cmd?))

            nil
            (and (or has-cmd? has-url?) (not (and has-cmd? has-url?)))

            false))))
(s/def ::mcp-servers
  #(and (map? %) (every? non-blank-string? (keys %)) (every? (spec-pred ::mcp-server) (vals %))))
(def mcp-schema {"servers" (spec-pred ::mcp-servers)})
(s/def ::mcp #(closed-map? mcp-schema %))

(def titling-modes
  "How a session gets its name. Only `llm` spends a provider call; the other
   two derive the title locally from the request itself, and `disabled` leaves
   the session unnamed (Blockether/vis#71)."
  #{"llm" "first_sentence" "first_words" "disabled"})
(def titling-schema {"mode" titling-modes "provider" non-blank-string? "model" non-blank-string?})
(s/def ::titling #(closed-map? titling-schema %))

;; What the WIRE answered about images ------------------------------------------
;;
;; MACHINE-WRITTEN, never hand-authored: `vision-describe` records which endpoint
;; refused an image content part outright, which model NAME answered that it cannot
;; read pixels, and which provider/model pair actually DESCRIBED one, so a fresh
;; process does not re-pay that discovery on the user's first image.
;;
;; Every row carries `learned_at`, because none of these facts is permanent: a
;; provider ships the image variant it was missing, a plan gains a vision tier. The
;; timestamp is what lets the reader EXPIRE a row and offer that endpoint an image
;; again, instead of blinding it for good on one refusal.

(def vision-fact-schema {"learned_at" non-blank-string? "providers" string-list?})
(s/def ::vision-fact #(closed-map? vision-fact-schema #{"learned_at"} %))
(s/def ::vision-facts
  #(and (map? %) (every? non-blank-string? (keys %)) (every? (spec-pred ::vision-fact) (vals %))))

(def vision-eye-schema
  {"provider" non-blank-string? "model" non-blank-string? "learned_at" non-blank-string?})
(s/def ::vision-eye #(closed-map? vision-eye-schema #{"provider" "learned_at"} %))

(def vision-memory-schema
  {"blind_providers" (spec-pred ::vision-facts)
   "blind_models" (spec-pred ::vision-facts)
   "working_eye" (spec-pred ::vision-eye)})
(s/def ::vision-memory #(closed-map? vision-memory-schema %))

;; ── `environment:` — what the workspace's `.env` CANNOT say ───────────────────
;;
;; The working directory's `.env`/`.env.local` are loaded by default, whole, with
;; no declaration at all (`config/workspace-environment-values`). This block is
;; for everything a dotenv file cannot express, and every entry NAMES ITS SOURCE,
;; exactly one: `env:` (the process environment, optionally under a different
;; name — also the ONLY way to re-admit an ambient variable into a CONFINED
;; child), `dotenv:` (a dotenv name, for a RENAME), `keychain:` (the OS
;; credential store) or `command:` (a helper's stdout). A declaration WINS over
;; the dotenv files and over the ambient environment; below it there is one fixed
;; order and no per-entry precedence chain to remember.
;;
;; A SECRET never appears here. A keychain item and a helper command hold no value
;; in the file at all, which is what `${NAME}` and `api_key_command` exist for, and
;; `~/.vis/state.yml` never sees this block: `environment:` is project-scoped, so a
;; whole-store write drops it (`project-scoped-config-keys`).
;;
;; `literal:` is the ONE entry that carries its own value, and it is spelled out so
;; it can never be a slip: a bare scalar (`VIS_MANAGED: "true"`) stays refused, and
;; a literal under a credential-looking name (`*_KEY`, `*_TOKEN`, `*_SECRET`,
;; `*_PASSWORD`) is refused too. It is for a non-secret process marker a child reads
;; to know Vis started it — the one thing a project's `.env` cannot own, because the
;; marker belongs to Vis' configuration and not to the project's file.
;;
;; The project IS the exposure decision: a name in `.env` or written here is
;; resolved and handed to Vis' own children — Python extensions, the shell's
;; subprocesses (confined or not) and managed language processes. Nothing else
;; of the OPERATOR's environment reaches a CONFINED child; the names that run
;; code during another program's startup (`LD_*`, `DYLD_*`, `PERL*`, `BASH_ENV`
;; …) are refused from either source (`process-jail/jailed-child-env`).

(defn- env-literal?
  "A literal `environment:` value: a non-blank string, a number or a boolean — the
   same scalar domain one call's own `env` delta takes."
  [v]
  (or (non-blank-string? v) (number? v) (boolean? v)))

(def ^:private credential-name-pattern
  "A variable name that reads like a credential. A `literal:` is refused under one:
   the wrapper exists for a non-secret marker, and the heuristic keeps a secret from
   being typed into a file that is committed."
  #"(?i)(.*_)?(KEY|TOKEN|SECRET|PASSWORD|PASSPHRASE|CREDENTIAL|CREDENTIALS)")

(def environment-source-schema
  {;; The process environment, read under this name — also the rename knob.
   "env" env-var-name?
   ;; The working directory's `.env`, then `.env.local`, read under this name.
   "dotenv" env-var-name?
   ;; macOS Keychain / freedesktop secret service item name.
   "keychain" non-blank-string?
   ;; Optional account qualifying the keychain item; meaningless without one.
   "account" non-blank-string?
   ;; Structured argv whose trimmed stdout IS the value, same shape and same
   ;; no-shell contract as `api_key_command`.
   "command" (spec-pred ::api-key-command)
   ;; The value itself, written down on purpose — never a bare scalar.
   "literal" env-literal?})

(def ^:private environment-sources
  "The five spellings of WHERE; exactly one of them makes an entry."
  #{"env" "dotenv" "keychain" "command" "literal"})

(s/def ::environment-source
  #(and (closed-map? environment-source-schema %)
        ;; Exactly ONE source — a second one would make the source invisible again.
        (= 1 (count (filter environment-sources (keys %))))
        (or (contains? % "keychain") (not (contains? % "account")))))

(s/def ::environment
  #(and (map? %)
        (every? env-var-name? (keys %))
        (every? (fn [[name entry]]
                  (and (s/valid? ::environment-source entry)
                       ;; A credential-looking name may not carry its own value.
                       (or (not (contains? entry "literal"))
                           (not (re-matches credential-name-pattern (str name))))))
                %)))

(def config-schema
  {"providers" (spec-pred ::providers)
   ;; Provider ids the operator DELETED. A provider can enter the fleet without
   ;; ever being in `providers` — synthesized from an env var or a stored
   ;; credential — so "deleted" cannot be expressed by absence and is recorded
   ;; here instead.
   "removed_providers" string-list?
   "default_provider" non-blank-string?
   "default_model" non-blank-string?
   "fallback_provider" non-blank-string?
   "fallback_model" non-blank-string?
   "router" (spec-pred ::router)
   "system_prompt" (spec-pred ::system-prompt)
   "workspace" (spec-pred ::workspace)
   "jail" (spec-pred ::jail)
   "environment" (spec-pred ::environment)
   "db_spec" (spec-pred ::db-spec)
   "grep" (spec-pred ::grep)
   "toggles" named-scalar-map?
   "tui_settings" (spec-pred ::tui-settings)
   "mcp" (spec-pred ::mcp)
   "python" (spec-pred ::python)
   "titling" (spec-pred ::titling)
   "vision_memory" (spec-pred ::vision-memory)})

(s/def ::config #(closed-map? config-schema %))

(defn explain-data [config] (s/explain-data ::config config))
(defn valid? [config] (s/valid? ::config config))

(def ^:private nested-schemas
  "Which closed-map schema a parent key nests, so a failure is attributed to the
   exact dotted field path instead of the whole opaque config map. Each entry is
   `[schema required nesting]`; nesting is `:map` (the value IS that map),
   `:vector` (a vector of such maps) or `:map-of` (a user-named map whose VALUES
   are such maps)."
  {config-schema {"providers" [provider-schema #{"id"} :vector]
                  "system_prompt" [prompt-schema #{"text"} :map]
                  "router" [router-schema #{} :map]
                  "workspace" [workspace-schema #{} :map]
                  "jail" [jail-schema #{} :map]
                  "db_spec" [db-schema #{"backend"} :map]
                  "grep" [grep-schema #{} :map]
                  "tui_settings" [tui-schema #{} :map]
                  "mcp" [mcp-schema #{} :map]
                  "python" [python-schema #{} :map]
                  "titling" [titling-schema #{} :map]
                  "vision_memory" [vision-memory-schema #{} :map]}
   provider-schema {"models" [model-schema #{"name"} :vector]}
   router-schema {"rate_limit" [rate-limit-schema #{} :map]
                  "network" [router-network-schema #{} :map]
                  "budget" [budget-schema #{} :map]
                  "tokens" [token-schema #{} :map]}
   workspace-schema {"filesystem" [workspace-entry-schema #{"id" "path"} :vector]}
   workspace-entry-schema {"when" [workspace-when-schema #{} :map]}
   jail-schema {"filesystem" [jail-filesystem-schema #{} :map]
                "network" [network-schema #{} :map]
                "mach_services" [jail-mach-services-schema #{} :map]}
   network-schema {"rules" [network-rule-schema #{"host"} :vector]}
   network-rule-schema {"allow" [network-rule-allow-schema #{"method"} :vector]}
   mcp-schema {"servers" [mcp-server-schema #{} :map-of]}
   mcp-server-schema {"auth" [mcp-auth-schema #{} :map]}
   vision-memory-schema {"blind_providers" [vision-fact-schema #{"learned_at"} :map-of]
                         "blind_models" [vision-fact-schema #{"learned_at"} :map-of]
                         "working_eye" [vision-eye-schema #{"provider" "learned_at"} :map]}})

(defn- edit-distance
  "Levenshtein distance — small inputs only (config key names)."
  [a b]
  (let [b
        (vec b)

        n
        (count b)]

    (peek (reduce (fn [prev [i ca]]
                    (reduce (fn [row j]
                              (conj row
                                    (min (inc (long (nth row j)))
                                         (inc (long (nth prev (inc (long j)))))
                                         (+ (long (nth prev j)) (if (= ca (nth b j)) 0 1)))))
                            [(inc (long i))]
                            (range n)))
                  (vec (range (inc (long n))))
                  (map-indexed vector a)))))

(defn- closest-key
  "The known key a typo most likely meant, or nil when nothing is close enough."
  [k known]
  (let [k
        (str/lower-case k)

        [best distance]
        (first (sort-by second
                        (map (fn [c]
                               [c (edit-distance k (str/lower-case c))])
                             known)))]

    (when (and best (pos? (count k)) (<= (long distance) (max 1 (quot (count k) 3)))) best)))

(defn- field-label [path] (str/join "." path))

(defn- value-rejection
  "Why one leaf value was rejected. Generic by default; a key whose vocabulary
   cannot be guessed from the key name spells its accepted values out, because
   \"value rejected by the api_style contract\" is exactly the message that leaves
   a near-miss dialect to be re-typed at random."
  [k v]
  (case k
    ("api_style" "compatibility")
    (str (pr-str v)
         " is not a wire dialect - use one of "
         (str/join ", " api-style-values)
         " (aliases such as openai_responses, responses, chat or claude are accepted too)")

    "environment"
    (str "every entry names ONE source - {env: NAME}, {dotenv: NAME}, "
         "{keychain: item} or {command: [argv ...]} - or carries a non-secret "
         "value explicitly as {literal: value}, which a credential-looking name "
         "(*_KEY, *_TOKEN, *_SECRET, *_PASSWORD) may not use")

    (str "value rejected by the " k " contract")))

(defn- schema-problems
  "Every reason a string-keyed `m` fails `schema`, each naming its dotted field
   path. Recurses through `nested-schemas` so the offending LEAF is reported."
  [schema required path m]
  (into
    (into []
          (comp (remove #(contains? m %))
                (map #(str (field-label (conj path %)) ": required key is missing")))
          (sort required))
    (mapcat
      (fn [[k v]]
        (let [label (field-label (conj path (str k)))]
          (cond (not (string? k)) [(str (pr-str k) ": every config key must be a string")]
                (not (contains? schema k))
                (let [hint (closest-key k (keys schema))]
                  [(str label
                        ": unknown " (if (empty? path) "top-level config key" "key")
                        " (config is closed)"
                        (when hint
                          (str " — did you mean \"" (field-label (conj path hint)) "\"?")))])
                ((get schema k) v) nil
                :else
                (let [[child child-required nesting] (get-in nested-schemas [schema k])]
                  (or (when child
                        (seq
                          (case nesting
                            :map
                            (when (map? v) (schema-problems child child-required (conj path k) v))

                            :vector
                            (when (vector? v)
                              (into []
                                    (mapcat (fn [[i x]]
                                              (when (map? x)
                                                (schema-problems child
                                                                 child-required
                                                                 (conj path (str k "[" i "]"))
                                                                 x))))
                                    (map-indexed vector v)))

                            :map-of
                            (when (map? v)
                              (into
                                []
                                (mapcat
                                  (fn [[ck cv]]
                                    (when (and (string? ck) (map? cv))
                                      (schema-problems child child-required (conj path k ck) cv))))
                                v))

                            nil)))
                      [(str label ": " (value-rejection k v))])))))
      m)))

(defn explain-problems
  "Best-effort, model-readable reasons a string-keyed YAML `config` fails the
   contract — one line per offending FIELD, named by its dotted path
   (`grep.include_gitignored_paths`, `providers[1].models[0].context`,
   `mcp.servers.docs.transport`): a non-string key, an unknown key (maps are
   closed, with a did-you-mean when a known key is close), a missing required
   key, or a value the schema rejects. Returns [] for a valid or nil map, so a
   caller surfaces a `config_error` hint ONLY when the live config is actually
   denied. This points a fix straight at the field rather than dumping the whole
   opaque spec problem."
  [config]
  (cond (nil? config) []
        (not (map? config)) ["config: expected a YAML map with string keys"]
        :else (vec (schema-problems config-schema #{} [] config))))

(def derived-machine-keys
  "Top-level keys of the machine store `~/.vis/state.yml` that Vis DERIVES and can
   rebuild from nothing — a cache of what earlier sessions learned about the
   fleet, never something a person typed."
  #{"vision_memory"})

(def project-scoped-config-keys
  "The ACCESS surface of a checkout — its filesystem grants, its jail and its
   environment — and never a property of a person or a machine, so these keys may
   never live in the machine store `~/.vis/state.yml`. Each belongs to the checkout
   it was read from (`<cwd>/vis.yml` or the `<cwd>/.vis/config.yml` overlay), and
   the machine tier merges OVER both. A prompt, a model or a toggle is a person's
   and stays legal here.

   A build that handed the MERGED config to a whole-store write copied these blocks
   out of a project's own files into the machine store, where the machine tier then
   OUTRANKED the files they came from: one repository's grants became the grants of
   every other checkout on the machine, and editing that repository's own YAML
   stopped changing anything. Writers now touch one key each, and these keys are
   dropped on read and on write so a store poisoned earlier heals itself."
  #{"workspace" "jail" "environment"})

(defn without-project-scoped
  "`config` with every `project-scoped-config-keys` block dropped, plus the set that
   was dropped: `[config dropped]`. Nothing else is touched — a person-owned key
   stays exactly as it was written."
  [config]
  (if-not (map? config)
    [config #{}]
    (let [dropped (into #{} (filter #(contains? config %)) project-scoped-config-keys)]
      [(apply dissoc config dropped) dropped])))
(defn- problem-key
  "The top-level key one `explain-problems` line blames: the first segment of its
   dotted field path."
  [line]
  (-> (str line)
      (str/split #":" 2)
      first
      (str/split #"[.\[]" 2)
      first
      str/trim))

(defn without-invalid-derived
  "`config` with every DERIVED machine key the contract rejects dropped, plus the
   set that was dropped: `[config dropped]`.

   Every writer of the machine store read-modify-writes the WHOLE map and
   `assert-config!` judges the whole map, so ONE malformed row inside a cache Vis
   wrote itself would also refuse the human's next provider, toggle or model pick
   — the store would stay read-only until somebody hand-edited YAML. A derived
   cache is discardable by definition, and dropping it here heals the file on the
   next write. A key a person authored is never dropped: that one has to keep
   refusing loudly, because silently deleting a provider is worse than refusing
   to write."
  [config]
  (if (or (not (map? config)) (valid? config))
    [config #{}]
    (let [dropped (into #{}
                        (comp (map problem-key)
                              (filter derived-machine-keys)
                              (filter #(contains? config %)))
                        (explain-problems config))]
      [(apply dissoc config dropped) dropped])))
(defn config-error-panel
  "Caller-facing screen for an invalid config: the offending field lines and
   nothing else. Entry points print `:vis/panel` verbatim, so a bad YAML key
   must never reach the fatal path (a Java stack trace tells the user nothing
   about which key they mistyped)."
  [fields source]
  (into ["" (str "  Invalid Vis configuration" (when source (str " in " source)) ":") ""]
        (concat (map #(str "  - " %) (or (seq fields) ["config: does not match the Vis contract"]))
                ["" "  Fix the entries above and run vis-agent again." ""])))

(defn assert-config!
  "Return a string-keyed YAML config when it satisfies the complete contract.
   The thrown message names each offending field path, because the raw spec
   problem for a closed map is an unreadable dump of the entire config.

   The violation is a USER error (`:vis/user-error`) carrying a rendered
   `:vis/panel`: a mistyped key deserves the field list, never a stack trace."
  ([config] (assert-config! config nil))
  ([config source]
   (if (valid? config)
     config
     (let [fields (explain-problems config)]
       (throw (ex-info (str "Invalid Vis configuration"
                            (when source (str " in " source))
                            (when (seq fields) (str ":\n  - " (str/join "\n  - " fields))))
                       {:type :vis/invalid-config
                        :vis/user-error true
                        :vis/panel (config-error-panel fields source)
                        :source source
                        :fields fields
                        :problems (mapv #(update % :val redact)
                                        (::s/problems (explain-data config)))}))))))

(def process-jail-config-keys
  #{:disabled? :inherit-host-env? :allow-read-write :allow-read :allow-write :deny-read :deny-write
    :deny-exec :no-search :inbound-ports :path-descriptions :mach-services})

(s/def ::process-jail-config
  (s/and map?
         #(every? process-jail-config-keys (keys %))
         #(boolean? (:disabled? %))
         #(boolean? (:inherit-host-env? %))
         #(every? rooted-path-list?
                  ((juxt :allow-read-write :allow-read :allow-write :deny-read :deny-write) %))
         #(rooted-path-list? (or (:no-search %) []))
         #(rooted-path-list? (or (:deny-exec %) []))
         #(s/valid? (get network-schema "inbound_ports") (:inbound-ports %))
         #(string-list? (or (:mach-services %) []))
         #(let [d (:path-descriptions %)] (or (nil? d) (string-map? d)))))

(defn assert-process-jail-config!
  "Validate and return the exact internal policy consumed by process-jail."
  [policy]
  (if (s/valid? ::process-jail-config policy)
    policy
    (throw (ex-info "Invalid process-jail configuration"
                    {:type :vis/invalid-process-jail-config
                     :problems (mapv #(update % :val redact)
                                     (::s/problems (s/explain-data ::process-jail-config
                                                                   policy)))}))))

(defn- resolve-exec-denies
  "Resolve `jail.deny-exec` entries into absolute executable paths that the jail
   forbids from being EXECUTED (a Seatbelt `(deny process-exec* ...)`, which
   overrides the blanket exec allow — kernel-enforced, no leaky argv parsing).
   A bare name is looked up on every PATH directory (all matches denied); an
   absolute/home path is denied verbatim."
  [names]
  (let [dirs (some-> (System/getenv "PATH")
                     (str/split (re-pattern java.io.File/pathSeparator)))]
    (into []
          (comp (mapcat (fn [n]
                          (let [n (str n)]
                            (if (or (str/starts-with? n "/") (str/starts-with? n "~"))
                              [n]
                              (into []
                                    (comp (map #(str % java.io.File/separator n))
                                          (filter #(.canExecute (java.io.File. ^String %))))
                                    dirs)))))
                (distinct))
          names)))

(defn entry-read-only?
  "True when the catalog entry declares READ-ONLY access. `access` defaults to
   read-write, so only an explicit `read-only` / `readonly` / `ro` opts out."
  [entry]
  (contains? #{"read-only" "readonly" "ro"}
             (some-> (get entry "access")
                     str/lower-case)))

(defn- entry-no-search?
  "Search visibility defaults to true; only an explicit `search: false` opts out."
  [entry]
  (false? (get entry "search")))

(defn entry-draft-policy
  "The catalog entry's DRAFT isolation policy as a keyword. Absent/unknown →
   `:shared` (write through to the real root), the historical behaviour."
  [entry]
  (case (some-> (get entry "draft")
                str
                str/lower-case)
    "copy-only"
    :copy-only

    "copy-and-apply"
    :copy-and-apply

    "not-allowed"
    :not-allowed

    :shared))

;; ── Conditional mounts ───────────────────────────────────────────────────────
;; A catalog shared across machines declares roots that only exist on some of
;; them. `when` gates an entry on the host OS and/or an existing path; `optional`
;; drops an entry whose own path is absent. Both are decided against an explicit
;; `env` map, so the decision stays a pure function.

(defn host-os
  "This host's `when.os` token: `macos`, `windows`, `wsl` (Linux under WSL),
   `linux`, or `unknown` when the platform can't be identified."
  []
  (let [n (str/lower-case (str (System/getProperty "os.name")))]
    (cond (str/includes? n "mac") "macos"
          (str/includes? n "win") "windows"
          (str/includes? n "linux") (if (try (str/includes? (str/lower-case (slurp "/proc/version"))
                                                            "microsoft")
                                             (catch Throwable _ false))
                                      "wsl"
                                      "linux")
          :else "unknown")))

(defn- path-present?
  [p]
  (boolean (when (and (string? p) (seq p))
             (.exists (java.io.File. ^String (paths/expand-home p))))))

(defn mount-env
  "The LIVE host facts a `when` clause is evaluated against:
   `{:os \"macos\" :exists? <pred>}`. Passed explicitly everywhere so mounting is
   testable without touching this machine."
  []
  {:os (host-os) :exists? path-present?})

(defn- when-os-match?
  [declared os]
  (let [wanted (cond (string? declared) #{declared}
                     (coll? declared) (set (map str declared)))]
    (or (nil? wanted)
        (contains? wanted os)
        ;; WSL *is* Linux: a `linux` clause covers it, never the other way round.
        (and (= "wsl" os) (contains? wanted "linux")))))

(defn entry-mount-status
  "Why a catalog entry does or does not mount on this host:

   `:mounted`         — declared, present, admitted.
   `:os-mismatch`     — `when.os` names other platforms.
   `:when-absent`     — `when.exists` names a path that is not there.
   `:optional-absent` — `optional: true` and the root itself is missing.
   `:missing`         — admitted, but the root does not exist yet (a warning,
                        not a removal: the historical behaviour is preserved)."
  ([entry] (entry-mount-status entry (mount-env)))
  ([entry {:keys [os exists?]}]
   (let [clause
         (get entry "when")

         exists?
         (or exists? path-present?)]

     (cond (not (when-os-match? (get clause "os") os)) :os-mismatch
           (and (contains? clause "exists") (not (exists? (get clause "exists")))) :when-absent
           (exists? (get entry "path")) :mounted
           (true? (get entry "optional")) :optional-absent
           :else :missing))))

(defn entry-mounted?
  "True when the entry belongs in THIS host's catalog."
  ([entry] (entry-mounted? entry (mount-env)))
  ([entry env] (contains? #{:mounted :missing} (entry-mount-status entry env))))

(defn applicable-entries
  "The catalog entries that apply to this host, in declaration order: a `when`
   that does not match and an `optional` root whose path is absent are dropped."
  ([entries] (applicable-entries entries (mount-env)))
  ([entries env] (into [] (filter #(entry-mounted? % env)) entries)))

(defn- os-clause-str [declared] (if (string? declared) declared (str/join ", " (map str declared))))

(defn workspace-mount-diagnostics
  "One message per declared root that did NOT mount as written: conditional roots
   the host skipped (`:info`) and admitted roots whose path is missing (`:warn`,
   or `:info` when `optional: true`). Empty when every root is present, so it
   doubles as the startup hint and the `doctor` check."
  ([config] (workspace-mount-diagnostics config (mount-env)))
  ([config env]
   (into
     []
     (keep
       (fn [entry]
         (let [id
               (get entry "id")

               path
               (get entry "path")

               base
               {:id id :path path}]

           (case (entry-mount-status entry env)
             :os-mismatch
             (assoc base
               :level :info
               :reason :os-mismatch
               :message (str "workspace root '"
                             id
                             "' is not mounted: when.os is "
                             (os-clause-str (get-in entry ["when" "os"]))
                             " and this host is "
                             (:os env)
                             ".")
               :remediation "Nothing to do — the root is meant for another platform.")

             :when-absent
             (assoc base
               :level :info
               :reason :when-absent
               :message (str "workspace root '"
                             id
                             "' is not mounted: when.exists path "
                             (get-in entry ["when" "exists"])
                             " does not exist.")
               :remediation "Create that path, or drop the when.exists clause.")

             :optional-absent
             (assoc base
               :level :info
               :reason :optional-absent
               :message
               (str "optional workspace root '" id "' is not mounted: " path " does not exist.")
               :remediation "Create the path to mount it, or leave it optional.")

             :missing
             (assoc base
               :level :warn
               :reason :missing
               :message (str "workspace root '" id "' points at " path ", which does not exist.")
               :remediation (str "Create it, mark the entry optional: true, or gate it with a "
                                 "when: clause."))

             nil))))
     (get-in config ["workspace" "filesystem"] []))))

(defn workspace-draft-policies
  "`{catalog-path -> policy}` for every declared root that opts OUT of the default
   `:shared` behaviour. Independent of `jail.filesystem.allow`: the policy governs
   draft isolation, which applies whether or not the OS jail is enabled. Roots
   this host does not mount never appear."
  ([config] (workspace-draft-policies config (mount-env)))
  ([config env]
   (assert-config! config)
   (into {}
         (keep (fn [entry]
                 (let [policy (entry-draft-policy entry)]
                   (when (not= :shared policy) [(get entry "path") policy]))))
         (applicable-entries (get-in config ["workspace" "filesystem"] []) env))))

(def vis-home-entry
  "Vis's OWN session folder — `~/.vis`: `state.yml`, the session DB, the gateway
   event journals, drafts and logs. An IMPLICIT `workspace.filesystem` catalog
   entry the engine always grants, independent of what a project declares and of
   `jail.filesystem.allow`: Vis must reach its own state even inside a live jail,
   and that reach is engine-level — never a feature toggle (the `introspection`
   toggle governs the read_session/get_session/list_sessions TOOLS, not this grant). Kept out of
   the DEFAULT search sweep (`search: false`); explicit paths still reach it."
  {"id" "vis-home"
   "path" "~/.vis"
   "description"
   "Vis' own session folder — session DB, gateway event journals, state.yml, drafts, logs."
   "search" false})

(defn- same-root?
  "True when two catalog paths denote the same directory once `~` is expanded and
   a trailing separator dropped."
  [a b]
  (letfn [(norm [p]
            (some-> p
                    str
                    paths/expand-home
                    paths/unixify
                    (str/replace #"/+$" "")))]
    (= (norm a) (norm b))))

(defn- with-vis-home
  "Append the implicit `~/.vis` entry unless the catalog already declares that
   path — an explicit entry stays the operator's (its id, description and access
   win)."
  [entries]
  (if (some #(same-root? (get % "path") (get vis-home-entry "path")) entries)
    (vec entries)
    (conj (vec entries) vis-home-entry)))

(defn process-jail-config
  "Derive the internal process-jail policy from validated string-keyed config.
   The `workspace.filesystem` catalog is the single source of roots. When the
   jail is DISABLED (the default) nothing is confined, so every catalog root is
   available and the `allow` list is ignored. When ENABLED,
   `jail.filesystem.allow` selects which catalog ids enter the OS jail
   (deny-by-omission). Each admitted entry's `access` sets RW vs read-only and
   `search: false` marks it out of the default search sweep.

   Roots this host does not mount are dropped FIRST (`applicable-entries`), so a
   `when`-gated id may be listed in `allow` on every machine; pass an explicit
   `env` to resolve against something other than the live host.

   `jail.environment` is the OPERATOR-environment mode: `declared` (the default)
   confines the child to the project's own variables plus the non-secret basics
   allowlist, `inherit` hands it the ambient environment too. It only applies to
   a live jail — with the jail off nothing is confined, so nothing is withheld
   either.

   `jail.mach_services` opens macOS Mach lookups. `keychain: true` additionally
   grants read access to the keychain databases (kept out of the search sweep),
   which is what makes `security`, `gh auth token` and
   `git credential-osxkeychain` work inside the jail.

   Vis's own session folder (`vis-home-entry`, `~/.vis`) is ALWAYS appended to the
   admitted set — engine-level, so it survives both an undeclared catalog and a
   live jail's deny-by-omission. A catalog entry for the same path wins."
  ([config] (process-jail-config config (mount-env)))
  ([config env]
   (assert-config! config)
   (let [jail
         (get config "jail" {})

         entries
         (applicable-entries (get-in config ["workspace" "filesystem"] []) env)

         by-id
         (reduce (fn [m e]
                   (assoc m (get e "id") e))
                 {}
                 entries)

         allowed
         ;; The workspace catalog is the single source of roots. When the jail is
         ;; DISABLED it confines nothing, so the whole catalog is available and must
         ;; still appear in the session — `jail.filesystem.allow` is irrelevant and a
         ;; stale/renamed id in it can never deny-safe the config. Only a live
         ;; (enabled) jail narrows to the `allow` subset. An id this host does not
         ;; mount is skipped; an id no catalog entry ever declared stays a hard
         ;; config error.
         (if (true? (get jail "enabled"))
           (let [declared
                 (into #{} (map #(get % "id")) (get-in config ["workspace" "filesystem"] []))]
             (into []
                   (keep (fn [id]
                           (or (get by-id id)
                               (when-not (contains? declared id)
                                 (throw
                                   (ex-info
                                     (str "jail.filesystem.allow references unknown workspace id: "
                                          id)
                                     {:type :vis/invalid-config :id id}))))))
                   (get-in jail ["filesystem" "allow"] [])))
           entries)

         allowed
         (with-vis-home allowed)

         descriptions
         (into {}
               (keep (fn [e]
                       (when-let [d (get e "description")]
                         [(get e "path") d])))
               allowed)

         mach
         (get jail "mach_services" {})

         keychain?
         (true? (get mach "keychain"))

         mach-services
         (into [] (distinct) (concat (when keychain? keychain-mach-services) (get mach "allow" [])))

         read-only
         (into [] (comp (filter entry-read-only?) (map #(get % "path"))) allowed)

         no-search
         (into [] (comp (filter entry-no-search?) (map #(get % "path"))) allowed)]

     (assert-process-jail-config!
       {:disabled? (not (true? (get jail "enabled")))
        :inherit-host-env? (= "inherit" (get jail "environment"))
        :allow-read-write (into [] (comp (remove entry-read-only?) (map #(get % "path"))) allowed)
        :allow-read (into [] (distinct) (concat read-only (when keychain? keychain-read-paths)))
        :allow-write []
        :deny-read []
        :deny-write []
        :deny-exec (resolve-exec-denies (get jail "deny_exec"))
        :no-search (into [] (distinct) (concat no-search (when keychain? keychain-read-paths)))
        :inbound-ports (vec (get-in jail ["network" "inbound_ports"]))
        :mach-services mach-services
        :path-descriptions descriptions}))))

(defn- network-allow->runtime
  [allow]
  (cond-> {:method (get allow "method")}
    (contains? allow "path")
    (assoc :path (get allow "path"))))

(defn- network-rule->runtime
  [rule]
  (cond-> {:host (get rule "host")}
    (contains? rule "access")
    (assoc :access (get rule "access"))

    (contains? rule "methods")
    (assoc :methods (get rule "methods"))

    (contains? rule "ports")
    (assoc :ports (get rule "ports"))

    (contains? rule "allow")
    (assoc :allow (mapv network-allow->runtime (get rule "allow")))))

(defn network-config
  "Derive the keyword-keyed internal egress policy from validated YAML config.
   Egress filtering is one facet of the process jail: `jail.enabled` is the single
   gate. When the jail is off the policy is empty (egress open); when on, the
   `jail.network` block (allowed/denied/exclude domains, `allow_private`, rules)
   is enforced alongside the filesystem and inbound-port confinement."
  [config]
  (assert-config! config)
  (let [jail
        (get config "jail" {})

        net
        (get jail "network" {})]

    (if-not (true? (get jail "enabled"))
      {}
      (cond-> {}
        (contains? net "allowed_domains")
        (assoc :allowed-domains (get net "allowed_domains"))

        (contains? net "denied_domains")
        (assoc :denied-domains (get net "denied_domains"))

        (contains? net "exclude_domains")
        (assoc :exclude-domains (get net "exclude_domains"))

        (contains? net "allow_private")
        (assoc :allow-private (get net "allow_private"))

        (contains? net "rules")
        (assoc :rules (mapv network-rule->runtime (get net "rules")))))))
