(ns com.blockether.vis.ext.provider-opencode-go
  "OpenCode Go (https://opencode.ai/go) static-API-key provider.

  OpenCode Go is a flat-rate ($10/month) subscription gateway serving a curated
  set of open-source coding models from ONE endpoint
  (`https://opencode.ai/zen/go/v1`) over TWO wire dialects. Vis surfaces them
  as a SINGLE first-class provider — `:opencode-go` — that routes each model to
  the correct wire automatically:

    OpenAI chat wire (/chat/completions, svar default):
      GLM, Kimi, DeepSeek, MiMo, Hy3.

    Anthropic Messages wire (/messages, per-model `:api-style :anthropic`):
      MiniMax, Qwen.

  svar reads `(or (:api-style model-map) (:api-style provider))` at request
  build time, so a per-model `:api-style` override on the Anthropic models
  inside `:default-models` is all that is needed — one provider, one key, one
  endpoint, two wires.

  Auth lifecycle:
    1. `vis-agent providers auth opencode-go` persists the key once under
       `~/.vis/opencode-auth.json` as canonical snake_case JSON
       (`api_key` / `saved_at`) — every JSON this repo writes uses
       snake_case string keys.
    2. Later runs resolve the key from config (`~/.vis/config.edn`, written by
       the TUI), then `OPENCODE_API_KEY`, then the auth file. Config wins so
       status/limits report the key that model calls actually use; the env var
       beats the file so CI stays home-directory-free.
    3. `vis-agent providers status opencode-go` shows the source without
       exposing the key.
    4. `vis-agent providers logout opencode-go` deletes the persisted key."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-opencode-go.limits :as go-limits]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Constants
;; =============================================================================

(defn- auth-file
  "Persisted auth state for the single OpenCode Go key. A FUNCTION: native-image
   folds top-level `def`s at build time, which would bake the builder's home
   into the binary."
  ^String []
  (str (System/getProperty "user.home") "/.vis/opencode-auth.json"))

(def ^:private PROVIDER_ID :opencode-go)

(def ^:private LABEL "OpenCode Go")

(def ^:private BASE_URL "https://opencode.ai/zen/go/v1")

(def ^:private ENV_KEYS "Env-var chain, highest priority first." ["OPENCODE_API_KEY"])

(def ^:private ANTHROPIC_PATTERN
  "Model IDs served over the Anthropic Messages wire (/messages). Everything else
   rides svar's default OpenAI chat wire (/chat/completions)."
  #"(?i)(minimax|qwen)")

(defn- anthropic-model? [^String model-id] (boolean (re-find ANTHROPIC_PATTERN model-id)))

(def ^:private DEFAULT_MODELS
  "The catalog this build ships: a bare string rides svar's default OpenAI chat
   wire, a `{:name … :api-style :anthropic}` map routes to `/messages`.

   Nothing is fetched here. `native-image` (graal-build-time) initializes this
   namespace at BUILD time, so a `/models` call at load ran on the BUILDER — it
   froze that machine's catalog into the binary and left its `HttpClient` in the
   image heap, which failed every native build after v0.1.32. The LIVE catalog
   still reaches the picker, at runtime, through `providers/fetch-models` (svar
   `/models!` against this same public endpoint), and `enrich-models` below
   stamps the wire on whatever comes back."
  ["kimi-k2.7-code" "glm-5.2" "deepseek-v4-flash" "deepseek-v4-pro" "kimi-k2.6" "mimo-v2.5-pro"
   "mimo-v2.5" "hy3" {:name "minimax-m3" :api-style :anthropic}
   {:name "minimax-m2.7" :api-style :anthropic} {:name "qwen3.7-max" :api-style :anthropic}
   {:name "qwen3.7-plus" :api-style :anthropic}])

(defn- enrich-models
  "`:provider/enrich-models-fn`: `(svar-provider router-opts) -> models-vec`, run
   when the router is built — at runtime, in the process that will make the call.

   The wire is a pure function of the model id, so a MiniMax/Qwen model the live
   `/models` catalog gained after this release still reaches `/messages`. An
   explicit `:api-style` from config wins."
  [svar-provider _router-opts]
  (mapv (fn [model]
          (if (and (nil? (:api-style model)) (anthropic-model? (str (:name model))))
            (assoc model :api-style :anthropic)
            model))
        (:models svar-provider)))

;; =============================================================================
;; Persistence
;; =============================================================================

(defn- auth-json-key
  "JSON key -> engine keyword. What we write is snake_case (`api_key`); the
   kebab spelling older builds persisted still reads back onto the same key."
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn- load-auth-file
  "Load `~/.vis/opencode-auth.json` or nil. Never throws."
  []
  (let [f (io/file (auth-file))]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn auth-json-key) (catch Exception _ nil)))))

(defn- save-auth-file!
  "Persist through `vis/wire-json-str`, the ONE JSON boundary: keyword keys
   become snake_case strings (`:api-key` -> `api_key`) and encoding is total."
  [auth-state]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit (auth-file) (vis/wire-json-str auth-state))))

(defn- clear-auth-file!
  []
  (let [f (io/file (auth-file))]
    (when (.exists f) (.delete f))))

;; =============================================================================
;; Token detection / resolution
;; =============================================================================

(defn- env-key
  []
  (some (fn [name*]
          (let [v (System/getenv name*)]
            (when-not (str/blank? v) v)))
        ENV_KEYS))

(defn- configured-key
  "API key from Vis provider config (`~/.vis/config.edn`), or nil. This is
   the TUI flow, which stores static keys in the normal provider config."
  []
  (try (when-let [current-config-fn (requiring-resolve 'com.blockether.vis.core/current-config)]
         (some (fn [provider]
                 (when (= PROVIDER_ID (:id provider))
                   (when-let [k (:api-key provider)]
                     (when-not (str/blank? k) k))))
               (:providers (current-config-fn))))
       (catch Throwable _ nil)))

(defn- auth-file-key
  []
  (when-let [k (:api-key (load-auth-file))]
    (when-not (str/blank? k) k)))

(defn- detect-key
  "Lookup priority: config -> env var -> auth file. Returns
   `{:api-key str :source kw}` or nil. Never throws."
  []
  (or (when-let [k (configured-key)]
        {:api-key k :source :config})
      (when-let [k (env-key)]
        {:api-key k :source :env-var})
      (when-let [k (auth-file-key)]
        {:api-key k :source :auth-file})))

(defn- get-token
  "Uniform token envelope for the central router adapter. Throws with a
   pointer at `vis-agent providers auth opencode-go` instead of letting the
   call die on an opaque 401."
  []
  (if-let [{:keys [api-key]} (detect-key)]
    {:token api-key :api-url BASE_URL}
    (throw (ex-info (str "No OpenCode Go API key. Run `vis-agent providers auth opencode-go`"
                         " to authenticate, or set "
                         (str/join " / " ENV_KEYS)
                         ".")
                    {:type :vis/opencode-go-not-authenticated :provider-id PROVIDER_ID}))))

;; =============================================================================
;; Status / logout
;; =============================================================================

(defn- key-preview
  "Short non-secret preview."
  [api-key]
  (let [n (count api-key)]
    (if (<= n 12) (str (subs api-key 0 (min 4 n)) "...") (str (subs api-key 0 8) "..."))))

(defn- status-fn
  []
  (let [detected (detect-key)]
    (cond-> {:is-authenticated (some? detected) :provider-id PROVIDER_ID :label LABEL}
      detected
      (assoc :source
        (:source detected) :api-key-preview
        (key-preview (:api-key detected))))))

(defn- logout-fn
  []
  (clear-auth-file!)
  (tel/log! {:level :info :id ::opencode-go-logout :msg "Cleared persisted OpenCode Go key"})
  :logged-out)

;; =============================================================================
;; Limits
;; =============================================================================

(defn- usage-error-note
  [status]
  (case status
    401
    (str LABEL " rejected the API key; re-run `vis-agent providers auth opencode-go`.")

    403
    (str LABEL " reports no active subscription behind this key.")

    (str LABEL " usage is unavailable.")))

(defn- usage-error-report
  [^Throwable t]
  (let [status (:status (ex-data t))]
    {:provider-id PROVIDER_ID
     ;; 401 is a key problem the user fixes by re-authenticating; 403 means the
     ;; key is real but carries no Go subscription, which is not an auth failure.
     :status (if (= 401 status) :unauthenticated :error)
     :fetched-at-ms (System/currentTimeMillis)
     :dynamic {:limits [] :note (usage-error-note status)}
     :error {:type :provider/opencode-go-usage-error
             :message (or (ex-message t) (.getName (class t)))}}))

(defn- limits-fn
  []
  (let [detected (detect-key)]
    (if (nil? detected)
      {:provider-id PROVIDER_ID
       :status :unauthenticated
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic {:limits [] :note (str LABEL " is not authenticated.")}}
      ;; OpenCode Go meters three dollar budgets (5h / week / month) and
      ;; publishes them at /usage, so the plan is reported from the live
      ;; counters rather than assumed unmetered.
      (try {:provider-id PROVIDER_ID
            :status :ok
            :fetched-at-ms (System/currentTimeMillis)
            :dynamic (go-limits/dynamic-limits! (:api-key detected))}
           (catch Throwable t (usage-error-report t))))))

;; =============================================================================
;; Interactive auth
;; =============================================================================

(defn- auth-instruction-lines
  []
  (vec (concat ["" (str "  " LABEL " requires a static API key.") ""
                "  Subscribe and create a key at https://opencode.ai/go." ""
                "  Two ways to authenticate:" ""
                "    1. Set the env var, then re-run `vis-agent providers auth opencode-go`:"]
               (mapv (fn [name*]
                       (str "         export " name* "=<your-opencode-go-api-key>"))
                     ENV_KEYS)
               ["" "    2. Add the provider through the TUI (Ctrl+K -> Providers)."
                "       The TUI prompts for the key directly and writes it to the config." ""
                (str "  Endpoint: " BASE_URL) ""
                "  One key covers both wire dialects (OpenAI chat + Anthropic Messages)."])))

(defn- auth-fn
  "Non-blocking auth flow: the CLI dispatcher captures stdin/stdout, so we persist
   an env-var key when present and otherwise print instructions."
  [printer-fn]
  (let
    [print!
     (or printer-fn (constantly nil))

     existing
     (detect-key)]

    (cond
      ;; Configured or already on disk -> no-op so re-running auth doesn't
      ;; require re-typing the key.
      (and existing (contains? #{:config :auth-file} (:source existing)))
      (do (print! (str "  Already authenticated with " LABEL "."))
          (print! (str "  Source: " (name (:source existing)) "."))
          (print! (str "  Run `vis-agent providers status " (name PROVIDER_ID) "` for details."))
          (print!
            (str "  Run `vis-agent providers logout " (name PROVIDER_ID) "` first to switch keys."))
          :already-authenticated)
      ;; Env var is set but not persisted -> write it through to the file so
      ;; subsequent runs read the persisted key directly.
      (and existing (= :env-var (:source existing)))
      (do (save-auth-file!
            {:api-key (:api-key existing) :saved-at (System/currentTimeMillis) :from :env-var})
          (print! (str "  Persisted OpenCode Go key from env var (" (str/join " / " ENV_KEYS) ")."))
          (print! (str "  " LABEL " is ready (endpoint: " BASE_URL ")."))
          :ok)
      ;; Nothing anywhere -> tell the user how to provide one.
      :else (do (doseq [line (auth-instruction-lines)]
                  (print! line))
                :no-credentials))))

;; =============================================================================
;; Public helpers
;; =============================================================================

(defn authenticated?
  "True when a usable key is resolvable from any source."
  []
  (some? (detect-key)))

(defn status "Status map. Convenience for the REPL / doctor probes." [] (status-fn))

(defn logout! "Clear the persisted key." [] (logout-fn))

;; =============================================================================
;; Provider registration
;; =============================================================================

(vis/register-extension!
  (vis/extension
    {:ext/name "provider-opencode-go"
     :ext/description
     "OpenCode Go subscription gateway — OpenAI chat + Anthropic Messages wires in one provider."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/providers [{:provider/id PROVIDER_ID
                      :provider/label LABEL
                      :provider/preset {:base-url BASE_URL :default-models DEFAULT_MODELS}
                      :provider/status-fn status-fn
                      :provider/logout-fn logout-fn
                      :provider/detect-fn detect-key
                      :provider/auth-fn auth-fn
                      :provider/auth-prompt-fn auth-instruction-lines
                      :provider/get-token-fn get-token
                      :provider/enrich-models-fn #'enrich-models
                      :provider/limits-fn limits-fn}]}))
