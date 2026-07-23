(ns com.blockether.vis.internal.config-spec
  "The executable contract for the YAML representation of Vis configuration.

   YAMLStar returns maps with string keys. This namespace validates that exact
   representation: no recursive keywordization, no snake_case aliases, and no
   acceptance of keyword-keyed lookalikes. Maps are closed unless their keys are
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

(def ^:private sensitive-keys #{"api-key" "environment" "env" "headers" "llm-headers" "extra-body"})

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

(def model-keys #{"name" "context" "output-limit" "tool-call?"})
(def provider-keys
  #{"id" "api-key" "models" "base-url" "api-style" "responses-path" "llm-headers" "extra-body"})

(def model-schema
  {"name" non-blank-string?
   "context" positive-int?
   "output-limit" positive-int?
   "tool-call?" boolean?})

(s/def ::model-map #(closed-map? model-schema #{"name"} %))
(s/def ::model
  (s/or :name non-blank-string?
        :map ::model-map))
(s/def ::models (s/coll-of ::model :kind vector?))

(def provider-schema
  {"id" non-blank-string?
   "api-key" string?
   "models" (spec-pred ::models)
   "base-url" non-blank-string?
   "api-style" non-blank-string?
   "responses-path" non-blank-string?
   "llm-headers" string-map?
   "extra-body" yaml-map?})

(s/def ::provider #(closed-map? provider-schema #{"id"} %))
(s/def ::providers (s/coll-of ::provider :kind vector?))

;; Router contract -------------------------------------------------------------

(def rate-limit-keys
  #{"same-provider-delays-ms" "fallback-after-ms" "respect-retry-after?" "fallback-provider?"})
(def router-network-keys
  #{"timeout-ms" "ttft-timeout-ms" "idle-timeout-ms" "semantic-timeout-ms" "max-retries"
    "initial-delay-ms" "max-delay-ms" "multiplier"})
(def budget-keys #{"max-tokens" "max-cost"})
(def token-keys #{"check-context?" "pricing" "context-limits" "output-reserve"})
(def router-keys
  #{"rate-limit" "network" "budget" "tokens" "failure-threshold" "recovery-ms"
    "transient-status-codes" "window-ms" "cooldown-ms" "max-wait-ms"})

(def rate-limit-schema
  {"same-provider-delays-ms" #(and (vector? %) (every? positive-int? %))
   "fallback-after-ms" positive-int?
   "respect-retry-after?" boolean?
   "fallback-provider?" boolean?})
(s/def ::rate-limit #(closed-map? rate-limit-schema %))

(def router-network-schema
  {"timeout-ms" positive-int?
   "ttft-timeout-ms" positive-int?
   "idle-timeout-ms" positive-int?
   "semantic-timeout-ms" positive-int?
   "max-retries" positive-int?
   "initial-delay-ms" positive-int?
   "max-delay-ms" positive-int?
   "multiplier" number?})
(s/def ::router-network #(closed-map? router-network-schema %))

(def budget-schema {"max-tokens" positive-int? "max-cost" non-negative-number?})
(s/def ::budget #(closed-map? budget-schema %))

(def token-schema
  {"check-context?" boolean?
   "pricing" named-yaml-map?
   "context-limits" number-map?
   "output-reserve" positive-int?})
(s/def ::tokens #(closed-map? token-schema %))

(def router-schema
  {"rate-limit" (spec-pred ::rate-limit)
   "network" (spec-pred ::router-network)
   "budget" (spec-pred ::budget)
   "tokens" (spec-pred ::tokens)
   "failure-threshold" positive-int?
   "recovery-ms" positive-int?
   "transient-status-codes" #(and (or (vector? %) (set? %))
                                  (every? (fn [n]
                                            (and (integer? n) (<= 100 n 599)))
                                          %))
   "window-ms" positive-int?
   "cooldown-ms" positive-int?
   "max-wait-ms" positive-int?})
(s/def ::router #(closed-map? router-schema %))

;; Sandbox contracts -----------------------------------------------------------

(def cache-keys #{"path" "access"})
(def filesystem-keys
  #{"allow-read-write" "allow-read" "allow-write" "deny-read" "deny-write" "language-caches"})
(def jail-keys #{"filesystem" "inbound-ports" "env" "deny-exec"})
(def network-rule-allow-keys #{"method" "path"})
(def network-rule-keys #{"host" "access" "methods" "allow" "ports"})
(def network-keys #{"allowed-domains" "denied-domains" "exclude-domains" "allow-private" "rules"})

(def cache-schema
  {"path" rooted-path?
   "access" (one-of #{"read-only" "readonly" "ro" "read-write" "readwrite" "rw"})})
(s/def ::cache-map #(closed-map? cache-schema #{"path"} %))
(s/def ::cache
  (s/or :path rooted-path?
        :map ::cache-map))
(s/def ::caches (s/coll-of ::cache :kind vector?))

(def fs-grant-keys #{"path" "description"})
(def fs-grant-schema {"path" rooted-path? "description" non-blank-string?})
(s/def ::fs-grant-map #(closed-map? fs-grant-schema #{"path"} %))
(s/def ::fs-grant
  (s/or :path rooted-path?
        :map ::fs-grant-map))
(s/def ::fs-grants (s/coll-of ::fs-grant :kind vector?))

(defn- grant-path
  "The rooted path of a filesystem grant entry (a bare string or a
   `{path, description}` map). Enforcement consumes paths only."
  [entry]
  (if (map? entry) (get entry "path") entry))

(defn- grant-descriptions
  "Path -> human description for the grant entries that carry one. Feeds the
   model-facing session view so the agent knows why each root is granted."
  [grants]
  (into {}
        (keep (fn [entry]
                (when (map? entry)
                  (when-let [d (get entry "description")]
                    [(get entry "path") d]))))
        grants))

(def filesystem-schema
  {"allow-read-write" (spec-pred ::fs-grants)
   "allow-read" (spec-pred ::fs-grants)
   "allow-write" (spec-pred ::fs-grants)
   "deny-read" rooted-path-list?
   "deny-write" rooted-path-list?
   "language-caches" (spec-pred ::caches)})
(s/def ::filesystem #(closed-map? filesystem-schema %))

(def jail-schema
  {"filesystem" (spec-pred ::filesystem)
   "inbound-ports" #(and (vector? %) (= (count %) (count (distinct %))) (every? port? %))
   "env" env-var-name-list?
   "deny-exec" string-list?})
(s/def ::jail #(closed-map? jail-schema %))

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
  {"allowed-domains" string-list?
   "denied-domains" string-list?
   "exclude-domains" string-list?
   "allow-private" boolean?
   "rules" (spec-pred ::network-rules)})
(s/def ::network #(closed-map? network-schema %))

;; Remaining top-level blocks --------------------------------------------------

(def prompt-keys #{"text" "replace?"})
(def search-keys #{"include-gitignored-paths" "always-exclude"})
(def db-keys #{"backend" "path"})
(def tui-keys #{"theme-name" "contributors-disabled"})
(def mcp-keys #{"servers"})
(def mcp-server-keys #{"transport" "command" "args" "cwd" "env" "url" "headers"})
(def python-keys #{"resource-cache"})
(def config-keys
  #{"providers" "router" "system-prompt" "sandbox" "jail" "network" "environment" "db-spec" "search"
    "toggles" "tui-settings" "mcp" "python"})

(def prompt-schema {"text" string? "replace?" boolean?})
(s/def ::prompt-map #(closed-map? prompt-schema #{"text"} %))
(s/def ::system-prompt
  (s/or :text string?
        :map ::prompt-map))

(def search-schema {"include-gitignored-paths" string-list? "always-exclude" string-list?})
(s/def ::search #(closed-map? search-schema %))

(def db-schema {"backend" non-blank-string? "path" non-blank-string?})
(s/def ::db-spec #(closed-map? db-schema #{"backend"} %))

(def tui-schema
  {"theme-name" non-blank-string?
   "contributors-disabled" #(and (or (vector? %) (set? %)) (every? non-blank-string? %))})
(s/def ::tui-settings #(closed-map? tui-schema %))

(def python-schema
  ;; GraalPy internal-resource cache root (where the Python stdlib extracts at
  ;; runtime). Read ONCE per process at polyglot-engine boot; the explicit
  ;; `-Dpolyglot.engine.userResourceCache` system property wins over this key.
  {"resource-cache" non-blank-string?})
(s/def ::python #(closed-map? python-schema %))

(def mcp-server-schema
  {"transport" (one-of #{"stdio" "http"})
   "command" non-blank-string?
   "args" #(and (vector? %) (every? string? %))
   "cwd" non-blank-string?
   "env" string-map?
   "url" non-blank-string?
   "headers" string-map?})
(s/def ::mcp-server
  #(and (closed-map? mcp-server-schema %)
        (or (non-blank-string? (get % "command")) (non-blank-string? (get % "url")))))
(s/def ::mcp-servers
  #(and (map? %) (every? non-blank-string? (keys %)) (every? (spec-pred ::mcp-server) (vals %))))
(def mcp-schema {"servers" (spec-pred ::mcp-servers)})
(s/def ::mcp #(closed-map? mcp-schema %))

(def config-schema
  {"providers" (spec-pred ::providers)
   "router" (spec-pred ::router)
   "system-prompt" (spec-pred ::system-prompt)
   "sandbox" boolean?
   "jail" (spec-pred ::jail)
   "network" (spec-pred ::network)
   "environment" string-map?
   "db-spec" (spec-pred ::db-spec)
   "search" (spec-pred ::search)
   "toggles" named-scalar-map?
   "tui-settings" (spec-pred ::tui-settings)
   "mcp" (spec-pred ::mcp)
   "python" (spec-pred ::python)})

(s/def ::config #(closed-map? config-schema %))

(defn explain-data [config] (s/explain-data ::config config))
(defn valid? [config] (s/valid? ::config config))

(defn assert-config!
  "Return a string-keyed YAML config when it satisfies the complete contract."
  ([config] (assert-config! config nil))
  ([config source]
   (if (valid? config)
     config
     (throw (ex-info (str "Invalid Vis configuration" (when source (str " in " source)))
                     {:type :vis/invalid-config
                      :source source
                      :problems (mapv #(update % :val redact)
                                      (::s/problems (explain-data config)))})))))

(def process-jail-config-keys
  #{:disabled? :allow-read-write :allow-read :allow-write :deny-read :deny-write :deny-exec
    :language-cache-dirs :inbound-ports :env-passthrough :path-descriptions})

(s/def ::process-jail-config
  (s/and map?
         #(every? process-jail-config-keys (keys %))
         #(boolean? (:disabled? %))
         #(every? rooted-path-list?
                  ((juxt :allow-read-write :allow-read :allow-write :deny-read :deny-write) %))
         #(s/valid? ::caches (:language-cache-dirs %))
         #(rooted-path-list? (or (:deny-exec %) []))
         #(s/valid? (get jail-schema "inbound-ports") (:inbound-ports %))
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

(defn process-jail-config
  "Derive the internal process-jail policy from validated string-keyed config."
  [config]
  (assert-config! config)
  (let
    [jail
     (get config "jail" {})

     fs
     (get jail "filesystem" {})

     descriptions
     (merge (grant-descriptions (get fs "allow-read-write"))
            (grant-descriptions (get fs "allow-read"))
            (grant-descriptions (get fs "allow-write")))]

    (assert-process-jail-config! {:disabled? (not (true? (get config "sandbox")))
                                  :allow-read-write
                                  (into [] (keep grant-path) (get fs "allow-read-write"))
                                  :allow-read (into [] (keep grant-path) (get fs "allow-read"))
                                  :allow-write (into [] (keep grant-path) (get fs "allow-write"))
                                  :deny-read (vec (get fs "deny-read"))
                                  :deny-write (vec (get fs "deny-write"))
                                  :deny-exec (resolve-exec-denies (get jail "deny-exec"))
                                  :language-cache-dirs (vec (get fs "language-caches"))
                                  :inbound-ports (vec (get jail "inbound-ports"))
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
  "Derive the keyword-keyed internal egress policy from validated YAML config."
  [config]
  (assert-config! config)
  (let [net (get config "network" {})]
    (cond-> {}
      (contains? net "allowed-domains")
      (assoc :allowed-domains (get net "allowed-domains"))

      (contains? net "denied-domains")
      (assoc :denied-domains (get net "denied-domains"))

      (contains? net "exclude-domains")
      (assoc :exclude-domains (get net "exclude-domains"))

      (contains? net "allow-private")
      (assoc :allow-private (get net "allow-private"))

      (contains? net "rules")
      (assoc :rules (mapv network-rule->runtime (get net "rules"))))))
