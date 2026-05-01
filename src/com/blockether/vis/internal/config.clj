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

;;; ── Provider catalog overlay ──────────────────────────────────────────────

(def ^:private VIS_PROVIDER_METADATA
  "Vis-side provider metadata layered on top of svar's KNOWN_PROVIDERS.
   Slots:
     :label          — human display name (svar tracks no strings).
     :default-models — curated short list for the TUI model-picker.
                       Optional; when missing the picker queries the
                       provider's `/v1/models` directly.
     :base-url       — ONLY for vis-only providers svar doesn't know
                       (`:github-models`, `:github-copilot`). Every
                       other id leaves base-url to svar's catalog.
     :api-style      — Optional vis override when svar's catalog does not
                       already provide the transport metadata.

   Adding a new provider svar already knows: list it here with
   `:label` (and optionally `:default-models`). Do NOT re-list its
   `:base-url` — svar owns that."
  {:openai         {:label "OpenAI"
                    :default-models ["gpt-5" "gpt-5-mini" "gpt-4o" "gpt-4o-mini" "o3-mini"]}
   :anthropic      {:label "Anthropic"
                    :default-models ["claude-opus-4-6" "claude-sonnet-4-6" "claude-haiku-4-5"]}
   :zai            {:label "Z.ai (Pass)"
                    :default-models ["glm-5-turbo" "glm-5.1" "glm-4.7" "glm-4.6v"]}
   :zai-coding     {:label "Z.ai (Coding Plan)"
                    :default-models ["glm-5-turbo" "glm-4.7" "glm-5.1"]}
   :openrouter     {:label "OpenRouter"
                    :default-models ["openai/gpt-4o" "google/gemini-2.5-pro"
                                     "anthropic/claude-sonnet-4-5"
                                     "meta-llama/llama-3.1-70b-instruct"]}
   :ollama         {:label "Ollama"
                    :default-models ["llama3.1" "mistral" "codellama" "phi3"]}
   :lmstudio       {:label "LM Studio"}
   :blockether     {:label "Blockether"
                    :default-models ["glm-5-turbo" "glm-5.1" "gpt-5-mini" "gpt-4o"
                                     "minimax-m2.5" "gemini-2.5-pro"]}
   ;; ── vis-owned provider metadata (vis-only + UI overlays) ───────────────
   :github-models  {:label "GitHub Models"
                    :base-url "https://models.github.ai/inference"
                    :default-models ["openai/gpt-4o" "openai/gpt-4o-mini" "openai/o3-mini"
                                     "meta/llama-4-scout-17b-16e-instruct"
                                     "deepseek/DeepSeek-R1"
                                     "mistralai/mistral-small-2503"]}
   :github-copilot {:label "GitHub Copilot"
                    :base-url "https://api.githubcopilot.com"
                    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini" "gemini-2.0-flash-001"]}
   :openai-codex   {:label "OpenAI Codex (ChatGPT OAuth)"
                    :default-models ["gpt-5.1" "gpt-5.1-codex-mini" "gpt-5.1-codex-max"
                                     "gpt-5.2" "gpt-5.2-codex" "gpt-5.3-codex"
                                     "gpt-5.4" "gpt-5.5"]}})

(def ^:private PRESET_ORDER
  "Stable display order in the 'Add Provider' picker. Most-likely-used
   first. Anything not in this vec lands at the end."
  [:openai :anthropic :openai-codex :openrouter :github-copilot :github-models
   :zai :zai-coding :blockether :ollama :lmstudio])

(defn- blockether-env-api-key []
  (System/getenv "BLOCKETHER_OPENAI_API_KEY"))

(defn- known-provider-base-url
  "Base URL for a provider id, vis override first, svar table second."
  [pid]
  (or (:base-url (get VIS_PROVIDER_METADATA pid))
    (:base-url (get svar-router/KNOWN_PROVIDERS pid))))

(defn provider-template
  "Preset descriptor for a provider id, merged from svar's catalog
   and vis's overlay. Returns nil for unknown ids."
  [pid]
  (let [vis-md  (get VIS_PROVIDER_METADATA pid)
        svar-md (get svar-router/KNOWN_PROVIDERS pid)]
    (when (or vis-md svar-md)
      (cond-> {:id pid}
        (:label vis-md)                   (assoc :label (:label vis-md))
        (known-provider-base-url pid)     (assoc :base-url (known-provider-base-url pid))
        (or (:api-style vis-md)
          (:api-style svar-md))           (assoc :api-style (or (:api-style vis-md)
                                                              (:api-style svar-md)))
        (:default-models vis-md)          (assoc :default-models (:default-models vis-md))))))

(defn provider-presets
  "All known provider presets, sorted for the 'Add Provider' picker.
   `:blockether` is hidden when no `BLOCKETHER_OPENAI_API_KEY` env
   var is present so the row doesn't show up empty for users without
   it."
  []
  (let [order-rank (zipmap PRESET_ORDER (range))
        ids        (cond-> (into #{} (concat (keys svar-router/KNOWN_PROVIDERS)
                                       (keys VIS_PROVIDER_METADATA)))
                     (not (blockether-env-api-key)) (disj :blockether))]
    (->> ids
      (sort-by #(or (order-rank %) Long/MAX_VALUE))
      (mapv provider-template))))

(defn display-label
  "Human-readable label for a provider id. Never persisted."
  [pid]
  (or (:label (get VIS_PROVIDER_METADATA pid))
    (some-> pid name str/capitalize)
    "Provider"))

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

(defn ->svar-model
  "Coerce a model representation to svar-native `{:name str}`."
  [model]
  (when-let [n (some-> (model-name model) str str/trim not-empty)]
    {:name n}))

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
        models                (->> (:models provider) (keep ->svar-model) vec)
        explicit-url          (:base-url provider)
        explicit-api-style    (or (:api-style provider) (:api-style template))
        explicit-headers      (:llm-headers provider)
        explicit-responses    (:responses-path provider)
        get-token-fn          (when (nil? api-key)
                                (some-> (registry/provider-by-id pid) :provider/get-token-fn))]
    (if get-token-fn
      (let [{:keys [token api-url llm-headers responses-path]} (get-token-fn)
            url             (or explicit-url api-url)
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

(defn- blockether-env-config
  "Bootstrap a `:blockether` provider config from `BLOCKETHER_*` env
   vars. Base URL prefers `BLOCKETHER_OPENAI_BASE_URL` (back-compat
   override), falling back to whatever svar's catalog has for
   `:blockether`. Models seed from the curated `:default-models` so
   first-boot UX has a working list without a config edit."
  []
  (when-let [api-key (blockether-env-api-key)]
    (let [be      (provider-template :blockether)
          env-url (System/getenv "BLOCKETHER_OPENAI_BASE_URL")
          url     (or env-url (:base-url be))]
      (when url
        {:providers [(cond-> {:id       :blockether
                              :api-key  api-key
                              :base-url url}
                       (:default-models be)
                       (assoc :models (mapv (fn [m] {:name m})
                                        (:default-models be))))]}))))

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
  "Backfill runtime-only metadata for provider configs already stored on disk."
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
  "Load provider config in svar-native syntax. `~/.vis/config.edn`
   takes priority, falling back to BLOCKETHER_* env vars."
  []
  (or (some-> (load-config-raw)
        ((fn [raw] (when (seq (:providers raw)) raw)))
        apply-config-metadata)
    (blockether-env-config)))

(defn save-config!
  "Persist svar-native config to `~/.vis/config.edn`."
  [config]
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir))
    (spit config-path (pr-str config))))

(defn resolve-config
  "Resolve provider config: explicit → `~/.vis/config.edn` → BLOCKETHER_* env.
   Throws when nothing is available."
  ([] (resolve-config nil))
  ([explicit-config]
   (or explicit-config
     (load-config)
     (throw (ex-info (str "No provider config. Create ~/.vis/config.edn "
                       "or set BLOCKETHER_OPENAI_API_KEY env var.")
              {})))))

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
