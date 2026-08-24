(ns com.blockether.vis.ext.provider-openrouter
  "OpenRouter static-API-key provider (https://openrouter.ai/api/v1).

   OpenRouter is a multi-provider gateway speaking the OpenAI chat wire,
   so no `:api-style` override is needed - svar's default OpenAI
   transport handles it. Model names are `vendor/model` slugs
   (`anthropic/claude-sonnet-4.5`, `openai/gpt-5.1`, ...).

   Auth lifecycle:
     1. `vis-agent providers auth openrouter` persists the key once under
        `~/.vis/openrouter-auth.json` as canonical snake_case JSON
        (`api_key` / `saved_at`) - every JSON this repo writes uses
        snake_case string keys.
     2. Later runs resolve the key from config (`~/.vis/config.edn`,
        written by the TUI), then `OPENROUTER_API_KEY`, then the auth
        file. Config wins so status/limits report the key that model
        calls actually use; the env var beats the file so CI stays
        home-directory-free.
     3. `vis-agent providers status openrouter` shows the source without
        exposing the key.
     4. `vis-agent providers logout openrouter` deletes the persisted key.

   Limits come from OpenRouter's `GET /api/v1/key`, which reports the
   credits consumed by the key and, for capped keys, the credit limit."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [taoensso.telemere :as tel]))

;; Constants

(defn- auth-file
  "Persisted auth state for the single OpenRouter key. A FUNCTION: native-image folds
   top-level `def`s at build time, which would bake the builder's home into the binary."
  ^String []
  (str (System/getProperty "user.home") "/.vis/openrouter-auth.json"))

(def ^:private PROVIDER_ID :openrouter)

(def ^:private LABEL "OpenRouter")

(def ^:private ENV_KEYS
  "Env-var chain, highest priority first. Matches svar's `KNOWN_PROVIDERS`
   entry for `:openrouter` so both layers read the same variable."
  ["OPENROUTER_API_KEY"])

(def ^:private DEFAULT_MODELS
  "Starter model NAMES for the 'Add Provider' picker. svar's catalog carries no
   `:default-models` for the gateway (it fronts every vendor), so the preset
   seeds a small cross-vendor set of `vendor/model` slugs; users edit the list
   in config afterwards. svar stays the source of truth if it ever curates one."
  (or (not-empty (svar/provider-default-models PROVIDER_ID))
      ["anthropic/claude-sonnet-4.5" "openai/gpt-5.1" "google/gemini-2.5-pro" "z-ai/glm-4.6"
       "deepseek/deepseek-chat-v3.1"]))

(def ^:private BASE_URL
  ;; Single source of truth is svar `KNOWN_PROVIDERS`; override here only if
  ;; this provider ever needs to diverge.
  (svar/provider-base-url PROVIDER_ID))

(def ^:private key-info-url "https://openrouter.ai/api/v1/key")

;; Persistence

(defn- auth-json-key
  "JSON key -> engine keyword. What we write is snake_case (`api_key`); the
   kebab spelling older builds persisted still reads back onto the same key."
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn- load-auth-file
  "Load `~/.vis/openrouter-auth.json` or nil. Never throws."
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

;; Token detection / resolution

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
   pointer at `vis-agent providers auth openrouter` instead of letting the call
   die on an opaque 401."
  []
  (if-let [{:keys [api-key]} (detect-key)]
    {:token api-key :api-url BASE_URL}
    (throw (ex-info (str "No OpenRouter API key. Run `vis-agent providers auth "
                         (name PROVIDER_ID)
                         "` to authenticate, or set "
                         (str/join " / " ENV_KEYS)
                         ".")
                    {:type :vis/openrouter-not-authenticated :provider-id PROVIDER_ID}))))

;; Status / logout

(defn- key-preview
  "Short non-secret preview. OpenRouter keys look like `sk-or-v1-...`."
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
  (tel/log! {:level :info :id ::openrouter-logout :msg "Cleared persisted OpenRouter key"})
  :logged-out)

;; Limits (credits)

(defn- object-map [value] (when (and (map? value) (not (record? value))) value))

(defn- field
  [m k]
  (when-let [m* (object-map m)]
    (cond (contains? m* k) (get m* k)
          (contains? m* (name k)) (get m* (name k)))))

(defn- fetch-key-info!
  "GET /api/v1/key -> `{:data {:label .. :usage .. :limit .. :is_free_tier ..}}`."
  [api-key]
  (let [response
        (http/get key-info-url
                  {:headers {"Accept" "application/json" "Authorization" (str "Bearer " api-key)}
                   :timeout 30000
                   :throw false})

        status
        (:status response)

        body
        (:body response)]

    (if (<= 200 status 299)
      (json/read-json body :key-fn keyword)
      (throw (ex-info (str "OpenRouter key info request failed: HTTP " status)
                      {:type :provider/openrouter-key-info-error
                       :status status
                       :body body
                       :url key-info-url})))))

(defn- key-info->dynamic-limits
  "Map the key report onto the shared dynamic-limits shape. A key with no
   `limit` is uncapped (account credits govern it), so it is reported as an
   unlimited row carrying the usage figure."
  [info]
  (let [data
        (or (field info :data) info)

        usage
        (field data :usage)

        limit
        (field data :limit)

        free?
        (boolean (field data :is_free_tier))

        capped?
        (number? limit)

        row
        (cond-> {:id :openrouter-credits
                 :label "OpenRouter credits"
                 :scope :account
                 :kind :credits
                 :precision :exact
                 :source :provider-api
                 :is-unlimited (not capped?)}
          (number? usage)
          (assoc :used (double usage))

          capped?
          (assoc :limit
            (double limit) :remaining
            (double (max 0.0 (- (double limit) (double (or usage 0)))))))]

    (cond-> {:limits [row]}
      free?
      (assoc :note "OpenRouter key is on the free tier.")

      (not capped?)
      (assoc :note "OpenRouter key has no spend cap; usage is billed against account credits."))))

(defn- limits-fn
  []
  (let [detected (detect-key)]
    {:provider-id PROVIDER_ID
     :status (if detected :ok :unauthenticated)
     :fetched-at-ms (System/currentTimeMillis)
     :dynamic (if (nil? detected)
                {:limits [] :note (str LABEL " is not authenticated.")}
                (key-info->dynamic-limits (fetch-key-info! (:api-key detected))))}))

;; Interactive auth

(defn- auth-instruction-lines
  []
  (vec (concat ["" (str "  " LABEL " requires a static API key.") ""
                "  Create one at https://openrouter.ai/keys." "" "  Two ways to authenticate:" ""
                (str "    1. Set the env var, then re-run `vis-agent providers auth "
                     (name PROVIDER_ID)
                     "`:")]
               (mapv (fn [name*]
                       (str "         export " name* "=<your-openrouter-api-key>"))
                     ENV_KEYS)
               ["" "    2. Add the provider through the TUI (Ctrl+K -> Providers)."
                "       The TUI prompts for the key directly and writes it to the config." ""
                (str "  Endpoint: " BASE_URL)])))

(defn- auth-fn
  "Non-blocking auth flow: the CLI dispatcher captures stdin/stdout, so we
   persist an env-var key when present and otherwise print instructions."
  [printer-fn]
  (let [print!
        (or printer-fn (constantly nil))

        existing
        (detect-key)]

    (cond (and existing (contains? #{:config :auth-file} (:source existing)))
          (do (print! (str "  Already authenticated with " LABEL "."))
              (print! (str "  Source: " (name (:source existing)) "."))
              (print!
                (str "  Run `vis-agent providers status " (name PROVIDER_ID) "` for details."))
              (print! (str "  Run `vis-agent providers logout "
                           (name PROVIDER_ID)
                           "` first to switch stored keys."))
              :already-authenticated)
          (and existing (= :env-var (:source existing)))
          (do (save-auth-file!
                {:api-key (:api-key existing) :saved-at (System/currentTimeMillis) :from :env-var})
              (print!
                (str "  Persisted OpenRouter key from env var (" (str/join " / " ENV_KEYS) ")."))
              (print! (str "  " LABEL " is ready (endpoint: " BASE_URL ")."))
              :ok)
          :else (do (doseq [line (auth-instruction-lines)]
                      (print! line))
                    :no-credentials))))

;; Live model catalog

(def ^:private catalog-url "https://openrouter.ai/api/v1/models")

(defn- catalog-entry
  [m]
  (let [top
        (field m :top_provider)

        params
        (set (field m :supported_parameters))

        context
        (or (field top :context_length) (field m :context_length))

        output
        (field top :max_completion_tokens)]

    (cond-> {}
      (number? context)
      (assoc :context (long context))

      (number? output)
      (assoc :output-limit (long output))

      (seq params)
      (assoc :tool-call?
        (contains? params "tools") :reasoning?
        (contains? params "reasoning")))))

(defn- fetch-catalog!
  "GET /api/v1/models -> `{slug {:context .. :output-limit .. :tool-call? ..}}`.
   Public endpoint, no key needed. OpenRouter fronts hundreds of models, so the
   catalog stays the source of truth for context windows - nothing is pinned in
   this file. Never throws; an unreachable catalog yields `{}`."
  []
  (try (let [response (http/get
                        catalog-url
                        {:headers {"Accept" "application/json"} :timeout 15000 :throw false})]
         (if-not (<= 200 (:status response) 299)
           {}
           (into {}
                 (keep (fn [m]
                         (when-let [slug (field m :id)]
                           [slug (catalog-entry m)])))
                 (field (json/read-json (:body response) :key-fn keyword) :data))))
       (catch Throwable _ {})))

(defn- enrich-models
  "`:provider/enrich-models-fn` - resolve each CONFIGURED model's real context
   window, output limit and tool/reasoning support from the live catalog. So
   adding a model is just writing its `vendor/model` slug in config (or picking
   it in the TUI): no per-model constants here, no vendored 300-entry catalog.
   Explicit config values still win per key; unknown slugs and an unreachable
   catalog leave the models untouched. Runs once per router build, not per turn."
  [svar-provider _router-opts]
  (let [models (:models svar-provider)]
    (if (empty? models)
      models
      (let [by-slug (fetch-catalog!)]
        (if (empty? by-slug)
          models
          (mapv (fn [m]
                  (merge (get by-slug (:name m)) m))
                models))))))

;; Public helpers

(defn authenticated?
  "True when a usable key is resolvable from any source."
  []
  (some? (detect-key)))

(defn status "Status map. Convenience for the REPL / doctor probes." [] (status-fn))

(defn logout! "Clear the persisted key." [] (logout-fn))

;; Provider registration

(defn register!
  []
  (vis/register-extension!
    (vis/extension
      {:ext/name "provider-openrouter"
       :ext/description "OpenRouter multi-provider gateway (static API key)."
       :ext/version "0.1.0"
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
                        :provider/limits-fn limits-fn
                        :provider/enrich-models-fn enrich-models}]})))
