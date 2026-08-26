(ns com.blockether.vis.ext.provider-openrouter
  "OpenRouter static-API-key provider (https://openrouter.ai/api/v1).

   OpenRouter is a multi-provider gateway speaking the OpenAI chat wire, so no
   `:api-style` override is needed - svar's default OpenAI transport handles it.
   Model names are `vendor/model` slugs (`anthropic/claude-sonnet-4.5`,
   `openai/gpt-5.1`, ...).

   Authentication is the shared static-API-key shape, owned by
   `com.blockether.vis.internal.provider-key-store` and declared by `BOOK` below:
   lookup order, status, logout, the token envelope and the interactive
   `vis-agent providers auth openrouter` flow all come from there.

   What stays here is what only OpenRouter knows: the starter catalog, the live
   model enrichment, and the credit report from `GET /api/v1/key` - the credits
   this key has consumed and, for capped keys, its limit."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]))

;; Constants

(def ^:private PROVIDER_ID :openrouter)

(def ^:private LABEL "OpenRouter")

(def ^:private DEFAULT_MODELS
  "Starter model NAMES for the 'Add Provider' picker. svar's catalog carries no
   `:default-models` for the gateway (it fronts every vendor), so the preset
   seeds a small cross-vendor set of `vendor/model` slugs; users edit the list
   in config afterwards. svar stays the source of truth if it ever curates one."
  (or (not-empty (svar/provider-default-models PROVIDER_ID))
      ["anthropic/claude-sonnet-4.5" "openai/gpt-5.1" "google/gemini-2.5-pro" "z-ai/glm-4.6"
       "deepseek/deepseek-chat-v3.1"]))

(def ^:private BOOK
  "OpenRouter's slice of the shared static-API-key shape
   (`com.blockether.vis.internal.provider-key-store`).

   ONE credential, so `:file-shape :flat`: `~/.vis/openrouter-auth.json` carries
   `api_key` / `saved_at` at its root, which is exactly where every build that
   ever persisted a key wrote it.

   `:base-url` and `:env-keys` come from svar's `KNOWN_PROVIDERS`, so the
   provider layer and the transport read the same endpoint and the same
   variable."
  {:vendor "OpenRouter"
   :file "openrouter-auth.json"
   :file-shape :flat
   :key-hint "<your-openrouter-api-key>"
   :error-type :vis/openrouter-not-authenticated
   :auth-notes ["  Create one at https://openrouter.ai/keys." ""]
   :plans {:openrouter {:provider-id PROVIDER_ID
                        :label LABEL
                        :base-url (svar/provider-base-url PROVIDER_ID)
                        :default-models DEFAULT_MODELS
                        :env-keys ["OPENROUTER_API_KEY"]}}})

;; Limits (credits)

(def ^:private key-info-url "https://openrouter.ai/api/v1/key")

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

(defn- make-limits-fn
  "`:provider/limits-fn` per plan. OpenRouter publishes the key's own credit
   report, so the row carries measured numbers rather than an assumption."
  [plan-tag]
  (fn []
    (let [detected (vis/provider-key-detect BOOK plan-tag)]
      {:provider-id PROVIDER_ID
       :status (if detected :ok :unauthenticated)
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic (if (nil? detected)
                  {:limits [] :note (str LABEL " is not authenticated.")}
                  (key-info->dynamic-limits (fetch-key-info! (:api-key detected))))})))

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

;; Provider registration

(defn register!
  []
  (vis/register-extension!
    (vis/extension {:ext/name "provider-openrouter"
                    :ext/description "OpenRouter multi-provider gateway (static API key)."
                    :ext/version "0.1.0"
                    :ext/author "Blockether"
                    :ext/owner "vis"
                    :ext/license "Apache-2.0"
                    ;; The key store owns auth, status, logout and the token envelope; the
                    ;; live catalog enrichment is this gateway's own business and is stamped
                    ;; onto the entry it produces.
                    :ext/providers (mapv #(assoc % :provider/enrich-models-fn enrich-models)
                                         (vis/provider-key-entries BOOK make-limits-fn))})))
