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
            [clojure.string :as str]))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- positive-int? [x] (and (integer? x) (pos? (long x))))
(defn- non-negative-number? [x] (and (number? x) (not (neg? (double x)))))
(defn- port? [x] (and (integer? x) (<= 1 x 65535)))
(defn- port-list? [x] (and (or (vector? x) (set? x)) (every? port? x)))
(defn- scalar? [x] (or (string? x) (boolean? x) (number? x) (nil? x)))
(defn- string-list? [x] (and (vector? x) (every? non-blank-string? x)))
(def ^:private env-var-name-re #"[A-Za-z_][A-Za-z0-9_]*")
(defn- env-var-name? [x] (and (string? x) (boolean (re-matches env-var-name-re x))))
(defn- env-var-name-list? [x] (and (vector? x) (every? env-var-name? x)))
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

(def model-keys #{"name" "context" "output_limit" "is_tool_call"})
(def provider-keys
  #{"id" "api_key" "models" "base_url" "compatibility" "api_style" "responses_path" "llm_headers"
    "extra_body"})

(def compatibility-values
  "The wire dialects a provider may declare. `api_style` remains the raw svar
   escape hatch for anything outside this two-dialect world (e.g. `gemini`)."
  #{"anthropic" "openai" "openai-responses"})

(def model-schema
  {"name" non-blank-string?
   "context" positive-int?
   "output_limit" positive-int?
   "is_tool_call" boolean?})

(s/def ::model-map #(closed-map? model-schema #{"name"} %))
(s/def ::model
  (s/or :name non-blank-string?
        :map ::model-map))
(s/def ::models (s/coll-of ::model :kind vector?))

(def provider-schema
  {"id" non-blank-string?
   "api_key" string?
   "models" (spec-pred ::models)
   "base_url" non-blank-string?
   "compatibility" (one-of compatibility-values)
   "api_style" non-blank-string?
   "responses_path" non-blank-string?
   "llm_headers" string-map?
   "extra_body" yaml-map?})

(s/def ::provider #(closed-map? provider-schema #{"id"} %))
(s/def ::providers (s/coll-of ::provider :kind vector?))

;; Router contract -------------------------------------------------------------

(def rate-limit-keys
  #{"same_provider_delays_ms" "fallback_after_ms" "is_respect_retry_after" "is_fallback_provider"})
(def router-network-keys
  #{"timeout_ms" "ttft_timeout_ms" "idle_timeout_ms" "semantic_timeout_ms" "max_retries"
    "initial_delay_ms" "max_delay_ms" "multiplier"})
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
;; `{id, path, description, access?, search?}`. `access` (default read-write)
;; picks RW vs read-only; `search: false` keeps the root OUT of the default
;; grep sweep (explicit paths still reach it). The catalog is the sole
;; source of truth; `jail.filesystem.allow` references entries by id.
(def workspace-entry-keys #{"id" "path" "description" "access" "search"})
(def workspace-access-values #{"read-only" "readonly" "ro" "read-write" "readwrite" "rw"})
(def workspace-entry-schema
  {"id" non-blank-string?
   "path" rooted-path?
   "description" non-blank-string?
   "access" (one-of workspace-access-values)
   "search" boolean?})
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

(def jail-keys #{"enabled" "filesystem" "network" "env" "deny_exec"})
(def jail-schema
  {"enabled" boolean?
   "filesystem" (spec-pred ::jail-filesystem)
   "network" (spec-pred ::network)
   "env" env-var-name-list?
   "deny_exec" string-list?})
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
(def python-keys #{"resource_cache"})
(def message-queue-keys
  #{"breaker_threshold" "retry_backoff_ms" "halfopen_probe_ms" "retry_after_cap_ms"})
(def config-keys
  #{"providers" "default_provider" "default_model" "router" "system_prompt" "workspace" "jail"
    "environment" "db_spec" "grep" "toggles" "tui_settings" "mcp" "python" "message_queue"})

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

(def python-schema
  ;; GraalPy internal-resource cache root (where the Python stdlib extracts at
  ;; runtime). Read ONCE per process at polyglot-engine boot; the explicit
  ;; `-Dpolyglot.engine.userResourceCache` system property wins over this key.
  {"resource_cache" non-blank-string?})
(s/def ::python #(closed-map? python-schema %))

(def message-queue-schema
  ;; Gateway turn-queue failure handling. Every value is a plain number so it
  ;; round-trips through ->yaml-safe/runtime-config as a snake_case string key.
  {"breaker_threshold" positive-int?
   "retry_backoff_ms" #(and (vector? %) (seq %) (every? positive-int? %))
   "halfopen_probe_ms" positive-int?
   "retry_after_cap_ms" positive-int?})
(s/def ::message-queue #(closed-map? message-queue-schema %))

(def mcp-auth-schema
  {"client_id" non-blank-string?
   "scope" non-blank-string?
   "authorization_timeout_ms" positive-int?})
(s/def ::mcp-auth #(closed-map? mcp-auth-schema %))

(def mcp-server-schema
  {"transport" (one-of #{"stdio" "http"})
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
        ;; Exactly ONE transport-defining key. Prevents a stdio spec with a
        ;; stray :url silently switching to http via inference.
        (let
          [has-cmd?
           (non-blank-string? (get % "command"))

           has-url?
           (non-blank-string? (get % "url"))]

          (and (or has-cmd? has-url?) (not (and has-cmd? has-url?))))))
(s/def ::mcp-servers
  #(and (map? %) (every? non-blank-string? (keys %)) (every? (spec-pred ::mcp-server) (vals %))))
(def mcp-schema {"servers" (spec-pred ::mcp-servers)})
(s/def ::mcp #(closed-map? mcp-schema %))

(def config-schema
  {"providers" (spec-pred ::providers)
   "default_provider" non-blank-string?
   "default_model" non-blank-string?
   "router" (spec-pred ::router)
   "system_prompt" (spec-pred ::system-prompt)
   "workspace" (spec-pred ::workspace)
   "jail" (spec-pred ::jail)
   "environment" string-map?
   "db_spec" (spec-pred ::db-spec)
   "grep" (spec-pred ::grep)
   "toggles" named-scalar-map?
   "tui_settings" (spec-pred ::tui-settings)
   "mcp" (spec-pred ::mcp)
   "python" (spec-pred ::python)
   "message_queue" (spec-pred ::message-queue)})

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
                  "message_queue" [message-queue-schema #{} :map]}
   provider-schema {"models" [model-schema #{"name"} :vector]}
   router-schema {"rate_limit" [rate-limit-schema #{} :map]
                  "network" [router-network-schema #{} :map]
                  "budget" [budget-schema #{} :map]
                  "tokens" [token-schema #{} :map]}
   workspace-schema {"filesystem" [workspace-entry-schema #{"id" "path"} :vector]}
   jail-schema {"filesystem" [jail-filesystem-schema #{} :map] "network" [network-schema #{} :map]}
   network-schema {"rules" [network-rule-schema #{"host"} :vector]}
   network-rule-schema {"allow" [network-rule-allow-schema #{"method"} :vector]}
   mcp-schema {"servers" [mcp-server-schema #{} :map-of]}
   mcp-server-schema {"auth" [mcp-auth-schema #{} :map]}})

(defn- edit-distance
  "Levenshtein distance — small inputs only (config key names)."
  [a b]
  (let
    [b
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
  (let
    [k
     (str/lower-case k)

     [best distance]
     (first (sort-by second
                     (map (fn [c]
                            [c (edit-distance k (str/lower-case c))])
                          known)))]

    (when (and best (pos? (count k)) (<= (long distance) (max 1 (quot (count k) 3)))) best)))

(defn- field-label [path] (str/join "." path))

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
                      [(str label ": value rejected by the " k " contract")])))))
      m)))

(defn explain-problems
  "Best-effort, model-readable reasons a string-keyed YAML `config` fails the
   contract — one line per offending FIELD, named by its dotted path
   (`search.include_gitignored_paths`, `providers[1].models[0].context`,
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

(defn config-error-panel
  "Caller-facing screen for an invalid config: the offending field lines and
   nothing else. Entry points print `:vis/panel` verbatim, so a bad YAML key
   must never reach the fatal path (a Java stack trace tells the user nothing
   about which key they mistyped)."
  [fields source]
  (into ["" (str "  Invalid Vis configuration" (when source (str " in " source)) ":") ""]
        (concat (map #(str "  - " %) (or (seq fields) ["config: does not match the Vis contract"]))
                ["" "  Fix the entries above and run vis again." ""])))

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
  #{:disabled? :allow-read-write :allow-read :allow-write :deny-read :deny-write :deny-exec
    :no-search :inbound-ports :env-passthrough :path-descriptions})

(s/def ::process-jail-config
  (s/and map?
         #(every? process-jail-config-keys (keys %))
         #(boolean? (:disabled? %))
         #(every? rooted-path-list?
                  ((juxt :allow-read-write :allow-read :allow-write :deny-read :deny-write) %))
         #(rooted-path-list? (or (:no-search %) []))
         #(rooted-path-list? (or (:deny-exec %) []))
         #(s/valid? (get network-schema "inbound_ports") (:inbound-ports %))
         #(env-var-name-list? (or (:env-passthrough %) []))
         #(let
            [d
             (:path-descriptions %)]

            (or (nil? d) (string-map? d)))))

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
  (let
    [dirs (some-> (System/getenv "PATH")
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

(defn- entry-read-only?
  [entry]
  (contains? #{"read-only" "readonly" "ro"}
             (some-> (get entry "access")
                     str/lower-case)))

(defn- entry-no-search?
  "Search visibility defaults to true; only an explicit `search: false` opts out."
  [entry]
  (false? (get entry "search")))

(defn process-jail-config
  "Derive the internal process-jail policy from validated string-keyed config.
   The `workspace.filesystem` catalog is the single source of roots. When the
   jail is DISABLED (the default) nothing is confined, so every catalog root is
   available and the `allow` list is ignored. When ENABLED,
   `jail.filesystem.allow` selects which catalog ids enter the OS jail
   (deny-by-omission). Each admitted entry's `access` sets RW vs read-only and
   `search: false` marks it out of the default search sweep."
  [config]
  (assert-config! config)
  (let
    [jail
     (get config "jail" {})

     entries
     (get-in config ["workspace" "filesystem"] [])

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
     ;; (enabled) jail narrows to the `allow` subset and treats an unknown id as
     ;; a hard config error.
     (if (true? (get jail "enabled"))
       (into []
             (keep (fn [id]
                     (or (get by-id id)
                         (throw (ex-info
                                  (str "jail.filesystem.allow references unknown workspace id: " id)
                                  {:type :vis/invalid-config :id id})))))
             (get-in jail ["filesystem" "allow"] []))
       entries)

     descriptions
     (into {}
           (keep (fn [e]
                   (when-let [d (get e "description")]
                     [(get e "path") d])))
           allowed)]

    (assert-process-jail-config!
      {:disabled? (not (true? (get jail "enabled")))
       :allow-read-write (into [] (comp (remove entry-read-only?) (map #(get % "path"))) allowed)
       :allow-read (into [] (comp (filter entry-read-only?) (map #(get % "path"))) allowed)
       :allow-write []
       :deny-read []
       :deny-write []
       :deny-exec (resolve-exec-denies (get jail "deny_exec"))
       :no-search (into [] (comp (filter entry-no-search?) (map #(get % "path"))) allowed)
       :inbound-ports (vec (get-in jail ["network" "inbound_ports"]))
       :env-passthrough (vec (get jail "env"))
       :path-descriptions descriptions})))

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
  (let
    [jail
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
