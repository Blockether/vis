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

  Authentication is the shared static-API-key shape, owned by
  `com.blockether.vis.internal.provider-key-store` and declared by `BOOK` below:
  lookup order, status, logout, the token envelope and the interactive
  `vis-agent providers auth opencode-go` flow all come from there.

  What stays here is what only OpenCode Go knows: which model rides which wire,
  and the live `/usage` quota report (see `limits.clj`)."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-opencode-go.limits :as go-limits]))

;; Constants

(def ^:private PROVIDER_ID :opencode-go)

(def ^:private LABEL "OpenCode Go")

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
   "mimo-v2.5" "hy3" "ox-alpha-free" {:name "minimax-m3" :api-style :anthropic}
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

(def ^:private BOOK
  "OpenCode Go's slice of the shared static-API-key shape
   (`com.blockether.vis.internal.provider-key-store`).

   ONE subscription key covers both wire dialects, so `:file-shape :flat`:
   `~/.vis/opencode-auth.json` carries `api_key` / `saved_at` at its root, which
   is exactly where every build that ever persisted a key wrote it."
  {:vendor "OpenCode Go"
   :file "opencode-auth.json"
   :file-shape :flat
   :key-hint "<your-opencode-go-api-key>"
   :error-type :vis/opencode-go-not-authenticated
   :auth-notes ["  Subscribe and create a key at https://opencode.ai/go."
                "  One key covers both wire dialects (OpenAI chat + Anthropic Messages)." ""]
   :plans {:opencode-go {:provider-id PROVIDER_ID
                         :label LABEL
                         :base-url "https://opencode.ai/zen/go/v1"
                         :default-models DEFAULT_MODELS
                         :env-keys ["OPENCODE_API_KEY"]}}})

;; Limits

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

(defn- make-limits-fn
  "`:provider/limits-fn` per plan. OpenCode Go meters three dollar budgets
   (5h / week / month) and publishes them at `/usage`, so the plan is reported
   from the live counters rather than assumed unmetered."
  [plan-tag]
  (fn []
    (let [detected (vis/provider-key-detect BOOK plan-tag)]
      (if (nil? detected)
        {:provider-id PROVIDER_ID
         :status :unauthenticated
         :fetched-at-ms (System/currentTimeMillis)
         :dynamic {:limits [] :note (str LABEL " is not authenticated.")}}
        (try {:provider-id PROVIDER_ID
              :status :ok
              :fetched-at-ms (System/currentTimeMillis)
              :dynamic (go-limits/dynamic-limits! (:api-key detected))}
             (catch Throwable t (usage-error-report t)))))))

;; Provider registration

(defn register!
  []
  (vis/register-extension!
    (vis/extension
      {:ext/name "provider-opencode-go"
       :ext/description
       "OpenCode Go subscription gateway — OpenAI chat + Anthropic Messages wires in one provider."
       :ext/version "0.2.0"
       :ext/author "Blockether"
       :ext/owner "vis"
       :ext/license "Apache-2.0"
       ;; The key store owns auth, status, logout and the token envelope; which
       ;; wire a model rides is this provider's own business and is stamped onto
       ;; the entry it produces.
       :ext/providers (mapv #(assoc % :provider/enrich-models-fn #'enrich-models)
                            (vis/provider-key-entries BOOK make-limits-fn))})))
