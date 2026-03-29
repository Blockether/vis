(ns com.blockether.vis.config
  "Shared config I/O, provider presets, and svar-native data helpers.
   Single source of truth for ~/.vis/config.edn reading/writing.
   Used by both agent.clj (programmatic) and provider.clj (TUI)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; ── Paths ───────────────────────────────────────────────────────────────

(def config-dir  (str (System/getProperty "user.home") "/.vis"))
(def config-path (str config-dir "/config.edn"))

;;; ── Provider presets ────────────────────────────────────────────────────

(def ^:private base-providers
  [{:id :anthropic  :label "Anthropic"  :base-url "https://api.anthropic.com/v1"
    :default-models ["claude-sonnet-4-20250514" "claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022"
                     "claude-3-opus-20240229"]}
   {:id :openai     :label "OpenAI"     :base-url "https://api.openai.com/v1"
    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini" "gpt-4-turbo" "gpt-4"]}
   {:id :openrouter :label "OpenRouter" :base-url "https://openrouter.ai/api/v1"
    :default-models ["openai/gpt-4o" "anthropic/claude-sonnet-4-20250514" "google/gemini-2.0-flash-001"
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
       :default-models ["glm-5.1" "claude-opus-4-6" "minimax-m2.5" "gpt-5-mini"
                        "claude-sonnet-4-6" "gemini-2.5-pro"]})))

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
  "Coerce a provider map to svar-native shape (only :id, :api-key, :base-url, :models)."
  [provider]
  (let [pid      (:id provider)
        api-key  (:api-key provider)
        models   (->> (:models provider) (keep ->svar-model) vec)
        base-url (provider-base-url provider)]
    (cond-> {:id pid :models models}
      (some? api-key)  (assoc :api-key api-key)
      (some? base-url) (assoc :base-url base-url))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(defn- blockether-env-config
  "Build svar-native config from BLOCKETHER_* env vars. Returns nil when not set."
  []
  (when-let [be (blockether-provider)]
    {:providers [(cond-> {:id       :blockether
                          :base-url (:base-url be)
                          :models   (mapv (fn [m] {:name m}) (:default-models be))}
                   (:api-key be) (assoc :api-key (:api-key be)))]}))

(defn load-config
  "Load config in svar-native syntax.
   ~/.vis/config.edn takes priority. Falls back to BLOCKETHER_* env vars."
  []
  (let [f (io/file config-path)]
    (or (when (.exists f)
          (try (let [raw (edn/read-string (slurp f))]
                 (when (and (map? raw) (vector? (:providers raw)))
                   raw))
               (catch Exception _ nil)))
        (blockether-env-config))))

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
  [explicit-config]
  (or explicit-config
      (load-config)
      (throw (ex-info (str "No provider config. Create ~/.vis/config.edn "
                           "(see SAMPLE_CONFIG.edn) or set BLOCKETHER_OPENAI_API_KEY env var.")
                      {}))))
