(ns com.blockether.vis.internal.config
  "Configuration: paths, JVM lifecycle, provider presets, svar-native
   coercion, config file I/O, and the active-provider state every
   channel reads through.

   Two halves:

     - On-disk config under `~/.vis/`: `config.edn`, `vis.mdb/`, `vis.log`.
       `init!` / `init-cli!` / `shutdown!` redirect stdout/stderr into
       the log file and bring up Telemere's file handler.
     - Live process state: the `active-config` atom holds the
       currently-selected provider config; `current-config`,
       `active-provider`, `active-model`, `provider-ids`,
       `has-provider?` are the read API. `reload-config!` re-reads
       from disk.

   The `->svar-provider` helper resolves `:api-key` lazily by calling
   the registered provider's `:provider/get-token-fn`, so the
   token-refresh policy stays inside each provider implementation
   instead of leaking up here."
  (:require
   [clojure+.error]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.registry :as registry]
   [taoensso.telemere :as tel])
  (:import
   (java.io FileInputStream FileOutputStream)))

(def config-dir  (str (System/getProperty "user.home") "/.vis"))
(def config-path (str config-dir "/config.edn"))
(def db-path     (str config-dir "/vis.mdb"))
(def default-db-spec {:backend :sqlite :path db-path})

(def ^:private ^String log-path (str config-dir "/vis.log"))

(def tty-in  (delay (FileInputStream.  "/dev/tty")))
(def tty-out (delay ^java.io.OutputStream (FileOutputStream. "/dev/tty")))

(def ^java.io.PrintStream original-stdout System/out)

(defn init!
  "Redirect System/out and System/err to the log file. Lanterna uses
   tty-in / tty-out for terminal I/O. Call from the TUI entry point."
  []
  (clojure+.error/install!)
  (set! *print-level* 10)
  (set! *print-length* 100)
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir)))
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (java.io.PrintStream. raw-out true)]
    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  (tel/add-handler! :file/vis
    (tel/handler:file {:path              log-path
                       :interval          :monthly
                       :max-file-size     4000000
                       :max-num-parts     8
                       :max-num-intervals 6}))
  (tel/call-on-shutdown! (fn [] (tel/stop-handlers!))))

(defn init-cli!
  "Logging init for non-TUI processes. Same redirects as init! but
   without the shutdown hook (CLI commands run to completion and exit)."
  []
  (clojure+.error/install!)
  (set! *print-level* 10)
  (set! *print-length* 100)
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir)))
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (java.io.PrintStream. raw-out true)]
    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  (tel/add-handler! :file/vis
    (tel/handler:file {:path              log-path
                       :interval          :monthly
                       :max-file-size     4000000
                       :max-num-parts     8
                       :max-num-intervals 6})))

(defn shutdown!
  "Flush and stop all telemere handlers. Call after the TUI screen
   stops."
  []
  (tel/stop-handlers!))

;;; ── Provider presets ──────────────────────────────────────────────────────

(def ^:private removed-provider-ids
  #{:blockether :openrouter :github-models :github-copilot})

(def ^:private PRESET_ORDER
  "Stable display order in the 'Add Provider' picker. Most-likely-used
   first. Anything not in this vec lands at the end."
  [:openai :anthropic :anthropic-coding-plan :openai-codex
   :github-copilot-business :github-copilot-individual
   :zai :zai-coding :ollama :lmstudio])

(defn- registered-provider-metadata
  "Provider-owned preset metadata. First-party provider extensions put
   labels, base URLs, default models, and transport overrides here so
   internal config stays provider-agnostic."
  [pid]
  (when-let [provider (registry/provider-by-id pid)]
    (merge (:provider/preset provider)
      (when-let [label (:provider/label provider)]
        {:label label}))))

(defn- known-provider-base-url
  "Base URL for a provider id: provider extension first, svar table last."
  [pid]
  (or (:base-url (registered-provider-metadata pid))
    (:base-url (get svar-router/KNOWN_PROVIDERS pid))))

(defn provider-template
  "Preset descriptor for a provider id, merged from a provider
   extension's metadata and svar's catalog. Returns nil for unknown or
   intentionally removed ids."
  [pid]
  (when-not (contains? removed-provider-ids pid)
    (let [provider-md (registered-provider-metadata pid)
          svar-md     (get svar-router/KNOWN_PROVIDERS pid)]
      (when (or provider-md svar-md (registry/provider-by-id pid))
        (cond-> {:id pid}
          (:label provider-md)             (assoc :label (:label provider-md))
          (known-provider-base-url pid)    (assoc :base-url (known-provider-base-url pid))
          (or (:api-style provider-md)
            (:api-style svar-md))          (assoc :api-style (or (:api-style provider-md)
                                                               (:api-style svar-md)))
          (:default-models provider-md)    (assoc :default-models (:default-models provider-md))
          (:hidden? provider-md)           (assoc :hidden? true))))))

(defn provider-presets
  "All known provider presets, sorted for the 'Add Provider' picker."
  []
  (let [order-rank (zipmap PRESET_ORDER (range))
        ids        (into #{} (concat (keys svar-router/KNOWN_PROVIDERS)
                               (map :provider/id (registry/registered-providers))))]
    (->> ids
      (remove removed-provider-ids)
      (keep provider-template)
      (remove :hidden?)
      (sort-by #(or (order-rank (:id %)) Long/MAX_VALUE))
      vec)))

(defn display-label
  "Human-readable label for a provider id. Never persisted."
  [pid]
  (or (:label (registered-provider-metadata pid))
    (some-> pid name str/capitalize)
    "Provider"))

(defn- trim-trailing-slashes
  [s]
  (str/replace (or s "") #"/+$" ""))

(defn- catalog-base-url?
  "True when `url` is just Vis/svar catalog metadata for `provider-id`,
   not a caller-owned custom endpoint. OAuth providers may receive a
   fresher LLM endpoint from token exchange (for Copilot, the proxy host),
   and catalog defaults must not pin traffic to the stale bootstrap host."
  [provider-id url]
  (= (some-> url trim-trailing-slashes)
    (some-> (known-provider-base-url provider-id) trim-trailing-slashes)))

(defn- provider-token-base-url
  [provider-id explicit-url api-url]
  (cond
    (and api-url (or (nil? explicit-url)
                   (catalog-base-url? provider-id explicit-url)))
    api-url

    explicit-url explicit-url
    :else api-url))

(defn- github-copilot-provider-id? [provider-id]
  (contains? #{:github-copilot-individual :github-copilot-business} provider-id))

(defn provider-model-visible?
  "True when svar's provider-scoped model filters allow this model id."
  [provider-id model-id]
  (let [catalog-id (if (github-copilot-provider-id? provider-id)
                     :github-copilot
                     provider-id)]
    (if-let [visible? (ns-resolve 'com.blockether.svar.internal.router 'provider-model-visible?)]
      (boolean (visible? catalog-id model-id))
      true)))

(defn provider-base-url
  "Resolve base-url for a provider: explicit field on the provider
   map first (so user-supplied URLs win), then the merged catalog."
  [provider]
  (or (:base-url provider) (known-provider-base-url (:id provider))))

;;; ── Svar-native data helpers ────────────────────────────────────────────

(defn model-name
  "Extract the model name string from a model (string or `{:name str}`)."
  [model]
  (cond
    (string? model) model
    (map? model)    (:name model)
    :else           nil))

(def ^:private github-copilot-legacy-model-aliases
  {"claude-sonnet-4-6" "claude-sonnet-4.6"
   "claude-sonnet-4-5" "claude-sonnet-4.5"
   "claude-haiku-4-5"  "claude-haiku-4.5"
   "claude-opus-4-7"   "claude-opus-4.7"
   "claude-opus-4-6"   "claude-opus-4.6"
   "claude-opus-4-5"   "claude-opus-4.5"})

(defn- github-copilot-claude-model?
  [model-name]
  (boolean (re-find #"^claude-(opus|sonnet|haiku)-4(?:\.\d+)?$" model-name)))

(defn- zai-provider-id?
  [provider-id]
  (contains? #{:zai :zai-coding} provider-id))

(defn- zai-thinking-model?
  [model-name]
  (boolean (re-find #"(?i)^glm-(?:5(?:[.-].*)?|4\.[5-9].*)$" (or model-name ""))))

(defn- provider-model-name
  [provider-id name]
  (if (github-copilot-provider-id? provider-id)
    (get github-copilot-legacy-model-aliases name name)
    name))

(defn ->svar-model
  "Coerce a model representation to svar-native `{:name str}`."
  ([model]
   (->svar-model nil model))
  ([provider-id model]
   (when-let [n (some-> (model-name model) str str/trim not-empty)]
     (let [n (provider-model-name provider-id n)]
       (cond-> {:name n}
         (and (github-copilot-provider-id? provider-id)
           (github-copilot-claude-model? n))
         (assoc :api-style :openai-compatible-chat
           :reasoning? true
           :reasoning-style :openai-effort
           :reasoning-effort? true)

         (and (zai-provider-id? provider-id)
           (zai-thinking-model? n))
         (assoc :reasoning? true
           :reasoning-style :zai-thinking
           :reasoning-effort? false))))))

(defn ->svar-provider
  "Coerce a provider map to svar-native shape (`:id`, `:api-key`,
   `:base-url`, `:api-style`, `:models`, optional `:responses-path`,
   optional `:llm-headers`).

   svar's `make-router` calls `normalize-provider` which auto-resolves
   `:base-url` from svar's `KNOWN_PROVIDERS` table for built-in
   providers, so we forward `:base-url` ONLY when the provider map
   has one explicitly (vis-only providers like `:github-models`,
   user overrides, or OAuth-supplied URLs). For known providers
   svar fills in the URL itself — stop fighting it.

   When `:api-key` is nil, look the provider up in the global
   provider registry (registry.clj) and call its
   `:provider/get-token-fn` to resolve a usable token. Each provider
   implementation handles its own auth lifecycle (OAuth refresh,
   env-var fallback, provider-specific headers, …) so this fn stays
   provider-agnostic and never references a concrete provider ns by
   name."
  [provider]
  (let [pid                   (:id provider)
        template              (provider-template pid)
        api-key               (:api-key provider)
        models                (->> (:models provider) (keep #(->svar-model pid %)) vec)
        explicit-url          (:base-url provider)
        explicit-api-style    (or (:api-style provider) (:api-style template))
        explicit-headers      (:llm-headers provider)
        explicit-responses    (:responses-path provider)
        get-token-fn          (when (nil? api-key)
                                (some-> (registry/provider-by-id pid) :provider/get-token-fn))]
    (if get-token-fn
      (let [{:keys [token api-url llm-headers responses-path]} (get-token-fn)
            url             (provider-token-base-url pid explicit-url api-url)
            merged-headers  (or explicit-headers llm-headers)
            merged-response (or explicit-responses responses-path)]
        (cond-> {:id pid :models models :api-key token}
          url               (assoc :base-url url)
          explicit-api-style (assoc :api-style explicit-api-style)
          merged-response   (assoc :responses-path merged-response)
          merged-headers    (assoc :llm-headers merged-headers)))
      (cond-> {:id pid :models models}
        api-key             (assoc :api-key api-key)
        explicit-url        (assoc :base-url explicit-url)
        explicit-api-style  (assoc :api-style explicit-api-style)
        explicit-responses  (assoc :responses-path explicit-responses)
        explicit-headers    (assoc :llm-headers explicit-headers)))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(defn load-config-raw
  "Load raw `config.edn` map (or nil on read/parse error)."
  []
  (let [f (io/file config-path)]
    (when (.exists f)
      (try
        (let [raw (edn/read-string (slurp f))]
          (when (map? raw) raw))
        (catch Exception _ nil)))))

(defn- apply-provider-metadata
  "Attach catalog metadata needed by the runtime while preserving the
   user's provider map exactly otherwise."
  [provider]
  (let [template (provider-template (:id provider))]
    (cond-> provider
      (and (nil? (:base-url provider)) (:base-url template))
      (assoc :base-url (:base-url template))

      (and (nil? (:api-style provider)) (:api-style template))
      (assoc :api-style (:api-style template)))))

(defn- apply-config-metadata [config]
  (update config :providers #(mapv apply-provider-metadata %)))

(defn load-config
  "Load provider config in svar-native syntax from `~/.vis/config.edn`."
  []
  (some-> (load-config-raw)
    ((fn [raw] (when (seq (:providers raw)) raw)))
    apply-config-metadata))

(defn- active-provider-entry [config]
  (first (:providers config)))

(defn- provider-selection-changed?
  [previous-provider selected-provider]
  (and selected-provider
    (not= (:id previous-provider) (:id selected-provider))))

(defn- emit-provider-selected!
  [{:keys [previous-provider provider config source]}]
  (when-let [hook (some-> (:id provider) registry/provider-by-id :provider/on-selected-fn)]
    (try
      (hook {:previous-provider previous-provider
             :provider          provider
             :config            config
             :source            source})
      (catch Throwable t
        (tel/log! {:level :warn :id ::provider-on-selected-failed
                   :data  {:provider (:id provider)
                           :source   source
                           :error    (ex-message t)
                           :ex-class (.getName (class t))}
                   :msg   (str "Provider on-selected hook for " (:id provider)
                            " threw; selection continues")})))))

(defn save-config!
  "Persist provider config to `~/.vis/config.edn`.

   When the first provider (the active provider) changes, the newly
   selected provider's optional `:provider/on-selected-fn` is invoked
   after the file write. Hook failures are logged and never prevent
   config persistence."
  ([config] (save-config! config nil))
  ([config source]
   (let [previous-provider (active-provider-entry (load-config-raw))
         selected-provider (active-provider-entry config)
         dir (io/file config-dir)]
     (when-not (.exists dir) (.mkdirs dir))
     (spit config-path (pr-str config))
     (when (provider-selection-changed? previous-provider selected-provider)
       (emit-provider-selected! {:previous-provider previous-provider
                                 :provider          selected-provider
                                 :config            config
                                 :source            source})))))

(defn resolve-config
  "Resolve provider config: explicit → `~/.vis/config.edn`.
   Throws when nothing is available."
  ([] (resolve-config nil))
  ([explicit-config]
   (or explicit-config
     (load-config)
     (throw (ex-info "No provider config. Create ~/.vis/config.edn or add one through the provider dialog."
              {})))))

(def ^:private extension-env-config-key :environment)

(defn extension-env-overrides
  "Persisted extension environment overrides from `~/.vis/config.edn`
   under `:environment`. Keys are environment variable names as
   strings. Values are strings. This does NOT mutate the process
   environment; extension code should call `extension-env-value` when
   it wants config-over-env resolution."
  []
  (let [raw (load-config-raw)
        m   (when (map? raw) (get raw extension-env-config-key))]
    (if (map? m)
      (into {}
        (keep (fn [[k v]]
                (when (and (string? k) (string? v) (not (str/blank? v)))
                  [k v])))
        m)
      {})))

(defn extension-env-status
  "Return source and value metadata for an extension-declared env var.
   `:source` is one of `:config`, `:env`, or `:unset`."
  [name]
  (let [name'     (str name)
        overrides (extension-env-overrides)]
    (if-let [configured (get overrides name')]
      {:name name' :source :config :value configured}
      (if-let [from-env (not-empty (str/trim (or (System/getenv name') "")))]
        {:name name' :source :env :value from-env}
        {:name name' :source :unset :value nil}))))

(defn extension-env-value
  "Resolve an extension-declared env var as config override → real env.
   Blank/missing values return nil."
  [name]
  (:value (extension-env-status name)))

(defn save-extension-env-var!
  "Persist or clear one extension env override in `~/.vis/config.edn`.
   Blank/nil `value` removes the override, revealing the process env
   value again if one exists. Preserves all other config keys."
  [name value]
  (let [name' (str name)
        raw   (or (load-config-raw) {})
        value' (when (string? value) (not-empty (str/trim value)))
        envs  (cond-> (or (get raw extension-env-config-key) {})
                value' (assoc name' value')
                (not value') (dissoc name'))]
    (save-config! (if (seq envs)
                    (assoc raw extension-env-config-key envs)
                    (dissoc raw extension-env-config-key))
      :environment)
    (extension-env-status name')))

(defn resolve-db-spec
  "Resolve DB spec: explicit → VIS_DB_PATH env → `:db-spec` from
   config.edn → default sqlite at `~/.vis/vis.mdb`."
  ([] (resolve-db-spec nil))
  ([explicit-db-spec]
   (or explicit-db-spec
     (when-let [env-path (System/getenv "VIS_DB_PATH")]
       {:backend :sqlite :path env-path})
     (:db-spec (load-config-raw))
     default-db-spec)))

;; =============================================================================
;; Active provider state
;;
;; The active provider config is mirrored from disk into the
;; `active-config` atom for fast reads. Every mutation goes through
;; the iteration loop's `set-provider!` (which writes to disk AND
;; rebuilds the global router AND reseats it on every cached
;; conversation env so long-lived envs stop talking to the previous
;; model). The router-rebuild and cached-env reseat are owned by
;; the runtime; this namespace owns the on-disk + atom state.
;; =============================================================================

(defonce active-config
  ;; Public atom (no #'private guard) so the iteration loop's
  ;; `set-provider!` can update it directly without going through a
  ;; setter; everyone else reads through `current-config`.
  (atom nil))

(defn current-config
  "Return the current provider config. Loads from disk on first call."
  []
  (or @active-config
    (let [cfg (load-config)]
      (reset! active-config cfg)
      cfg)))

(defn active-provider
  "Return the first (primary) provider from config, or nil."
  []
  (first (:providers (current-config))))

(defn active-model
  "Return the primary model name string, or nil."
  []
  (some-> (active-provider) :models first model-name))

(defn provider-ids
  "Set of configured provider `:id` keywords."
  []
  (into #{} (map :id) (:providers (or (current-config) {:providers []}))))

(defn has-provider? [provider-id]
  (contains? (provider-ids) provider-id))

(defn reload-config! []
  (reset! active-config (load-config)))

