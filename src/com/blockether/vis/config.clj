(ns com.blockether.vis.config
  "Shared config I/O, provider presets, logging bootstrap, and svar-native data helpers.
   Single source of truth for ~/.vis/config.edn reading/writing.
   Used by both agent.clj (programmatic) and provider.clj (TUI)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure+.error]
            [com.blockether.vis.provider :as provider-registry]
            [taoensso.telemere :as t])
  (:import [java.io FileInputStream FileOutputStream]))

;;; ── Version ─────────────────────────────────────────────────────────────

(def version
  "Vis version string.

   Resolution order:
   1. `META-INF/vis/VERSION` on the classpath — written into the jar by
      `build.clj` at packaging time from the repo-root `VERSION` file.
   2. `VERSION` in the current working directory — the dev / REPL
      fallback used by `clojure -M:run` from the monorepo root.
   3. Literal \"dev\" — last-resort fallback so the namespace always loads."
  (or (some-> (io/resource "META-INF/vis/VERSION") slurp str/trim not-empty)
    (try (let [v (str/trim (slurp "VERSION"))]
           (when-not (str/blank? v) v))
      (catch Throwable _ nil))
    "dev"))

;;; ── Paths ───────────────────────────────────────────────────────────────

(def config-dir  (str (System/getProperty "user.home") "/.vis"))
(def config-path (str config-dir "/config.edn"))

;; One SQLite DB for everything — TUI sessions, web sessions, telegram chats,
;; CLI agent runs. Each is a named :conversation inside. Replaces the old
;; per-entrypoint dirs (~/.vis/sessions/<uuid>/, ~/.vis/telegram/<chat-id>/,
;; ~/.vis/agents/<name>/, ~/.vis/rlm/) — all collapsed here.
(def db-path (str config-dir "/vis.mdb"))
(def default-db-spec {:backend :sqlite :path db-path})

;; Logging path shared by TUI, CLI, Telegram, and web.
(def ^:private ^String log-path (str config-dir "/vis.log"))

;; Open /dev/tty directly for Lanterna — independent of System/out.
;; Delayed so non-TUI entrypoints do not fail at class-load time.
(def tty-in  (delay (FileInputStream.  "/dev/tty")))
(def tty-out (delay ^java.io.OutputStream (FileOutputStream. "/dev/tty")))

;; Capture original stdout before redirection. CLI prints user-facing output here.
(def ^java.io.PrintStream original-stdout System/out)

(defn init!
  "Redirect System/out and System/err to log file. Lanterna uses tty-in/tty-out."
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

  (t/remove-handler! :default/console)
  (t/add-handler! :file/vis
    (t/handler:file {:path              log-path
                     :interval          :monthly
                     :max-file-size     4000000
                     :max-num-parts     8
                     :max-num-intervals 6}))

  (t/call-on-shutdown! (fn [] (t/stop-handlers!))))

(defn init-cli!
  "Logging init for CLI and non-TUI commands.
   Redirects System.out/System.err and Clojure *out*/*err* to log file."
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

  (t/remove-handler! :default/console)
  (t/add-handler! :file/vis
    (t/handler:file {:path              log-path
                     :interval          :monthly
                     :max-file-size     4000000
                     :max-num-parts     8
                     :max-num-intervals 6})))
(defn shutdown!
  "Flush and stop all handlers. Call after screen stops."
  []
  (t/stop-handlers!))

;;; ── Provider presets ────────────────────────────────────────────────────

(def ^:private base-providers
  ;; Anthropic preset deliberately removed — the Blockether gateway
  ;; rejects Claude requests with a plaintext \"no access\" response
  ;; that pretends to be a 200 OK, and we don't want any model picker
  ;; to surface Claude until that's fixed gateway-side. Same reason for
  ;; the Claude entries dropped from the other presets below.
  [{:id :openai     :label "OpenAI"     :base-url "https://api.openai.com/v1"
    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini" "gpt-4-turbo" "gpt-4"]}
   {:id :github-models :label "GitHub Models" :base-url "https://models.github.ai/inference"
    :default-models ["openai/gpt-4o" "openai/gpt-4o-mini" "openai/o3-mini"
                     "meta/llama-4-scout-17b-16e-instruct"
                     "deepseek/DeepSeek-R1" "mistralai/mistral-small-2503"]}
   {:id :github-copilot :label "GitHub Copilot" :base-url "https://api.githubcopilot.com"
    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini"
                     "gemini-2.0-flash-001"]}
   {:id :openrouter :label "OpenRouter" :base-url "https://openrouter.ai/api/v1"
    :default-models ["openai/gpt-4o" "google/gemini-2.0-flash-001"
                     "meta-llama/llama-3.1-70b-instruct"]}
   {:id :ollama     :label "Ollama"     :base-url "http://localhost:11434/v1"
    :default-models ["llama3.1" "mistral" "codellama" "phi3"]}])

(defn- blockether-provider
  "Build Blockether provider preset from env vars. Returns nil when not configured."
  []
  (let [be-key (System/getenv "BLOCKETHER_OPENAI_API_KEY")
        be-url (System/getenv "BLOCKETHER_OPENAI_BASE_URL")]
    (when (and be-key be-url)
      {:id :blockether
       :label "Blockether"
       :base-url be-url
       :api-key be-key
        ;; Claude entries deliberately removed — the Blockether gateway
        ;; returns a plaintext auth-rejection on a 200 OK for those
        ;; models, which svar's spec layer correctly classifies as
        ;; `not-a-map` and the iteration loop has nowhere to go from
        ;; there. Reinstate only after the gateway side is fixed.
       :default-models ["glm-5-turbo" "glm-5.1" "gpt-5-mini" "gpt-4o"
                        "minimax-m2.5" "gemini-2.5-pro"]})))

(defn provider-presets
  "All known provider presets. Includes Blockether when BLOCKETHER_* env vars are set."
  []
  (if-let [be (blockether-provider)]
    (into [be] base-providers)
    base-providers))

(defn provider-template
  "Find preset for a provider ID."
  [pid]
  (some #(when (= (:id %) pid) %) (provider-presets)))

;;; ── Display helpers (computed at render time, never stored) ─────────────

(defn display-label
  "Human-readable label for a provider ID. Never stored in config."
  [pid]
  (or (:label (provider-template pid))
    (some-> pid name str/capitalize)
    "Provider"))

(defn provider-base-url
  "Resolve base-url for a provider, falling back to preset."
  [provider]
  (or (:base-url provider)
    (:base-url (provider-template (:id provider)))))

;;; ── Svar-native data helpers ────────────────────────────────────────────

(defn model-name
  "Extract model name string from a model (string or {:name ...} map)."
  [model]
  (cond
    (string? model) model
    (map? model)    (:name model)
    :else           nil))

(defn ->svar-model
  "Coerce any model representation to svar-native {:name str}."
  [model]
  (when-let [n (some-> (model-name model) str str/trim not-empty)]
    {:name n}))

(defn ->svar-provider
  "Coerce a provider map to svar-native shape (only :id, :api-key,
   :base-url, :models).

   When `:api-key` is nil, look the provider up in the
   `vis-provider` registry and call its `:provider/get-token-fn` to
   resolve a usable token. Each provider implementation handles its
   own auth lifecycle (OAuth refresh, env-var fallback, etc.) so this
   namespace stays provider-agnostic and never references a concrete
   provider namespace by name."
  [provider]
  (let [pid       (:id provider)
        api-key   (:api-key provider)
        models    (->> (:models provider) (keep ->svar-model) vec)
        base-url  (provider-base-url provider)
        get-token-fn (when (nil? api-key)
                       (some-> (provider-registry/by-id pid) :provider/get-token-fn))]
    (if get-token-fn
      (let [{:keys [token api-url]} (get-token-fn)]
        (cond-> {:id pid :models models :api-key token}
          (some? (or base-url api-url)) (assoc :base-url (or base-url api-url))))
      (cond-> {:id pid :models models}
        (some? api-key)  (assoc :api-key api-key)
        (some? base-url) (assoc :base-url base-url)))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(defn- blockether-env-config
  "Build svar-native config from BLOCKETHER_* env vars. Returns nil when not set."
  []
  (when-let [be (blockether-provider)]
    {:providers [(cond-> {:id       :blockether
                          :base-url (:base-url be)
                          :models   (mapv (fn [m] {:name m}) (:default-models be))}
                   (:api-key be) (assoc :api-key (:api-key be)))]}))

(defn load-config-raw
  "Load raw config.edn map (or nil on read/parse errors)."
  []
  (let [f (io/file config-path)]
    (when (.exists f)
      (try
        (let [raw (edn/read-string (slurp f))]
          (when (map? raw) raw))
        (catch Exception _ nil)))))

(defn load-config
  "Load provider config in svar-native syntax.
   ~/.vis/config.edn takes priority. Falls back to BLOCKETHER_* env vars."
  []
  (or (some-> (load-config-raw)
        ((fn [raw] (when (seq (:providers raw)) raw))))
    (blockether-env-config)))

(defn save-config!
  "Save svar-native config to ~/.vis/config.edn."
  [config]
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir))
    (spit config-path (pr-str config))))

(defn resolve-config
  "Resolve provider config in svar-native syntax.
   Checks: explicit config → ~/.vis/config.edn → BLOCKETHER_* env vars.
   Throws when nothing is available."
  ([] (resolve-config nil))
  ([explicit-config]
   (or explicit-config
     (load-config)
     (throw (ex-info (str "No provider config. Create ~/.vis/config.edn "
                       "(see SAMPLE_CONFIG.edn) or set BLOCKETHER_OPENAI_API_KEY env var.")
              {})))))

(defn resolve-db-spec
  "Resolve DB spec.

   Priority:
   1) explicit-db-spec arg
   2) VIS_DB_PATH env var
   3) :db-spec in ~/.vis/config.edn
   4) default sqlite path (~/.vis/vis.mdb)

   Returns a db-spec accepted by persistance/create-rlm-conn."
  ([] (resolve-db-spec nil))
  ([explicit-db-spec]
   (or explicit-db-spec
     (when-let [env-path (System/getenv "VIS_DB_PATH")]
       {:backend :sqlite :path env-path})
     (:db-spec (load-config-raw))
     default-db-spec)))
