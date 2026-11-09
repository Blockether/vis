(ns com.blockether.vis.internal.provider.vendor.alibaba
  "Alibaba Model Studio static-API-key providers. Each plan is registered as its own extension:

     :alibaba-coding-plan -> Coding Plan subscription
                   (https://coding-intl.dashscope.aliyuncs.com/v1).
                   Env var: `ALIBABA_CODING_PLAN_API_KEY`.

     :alibaba-token-plan  -> Token Plan prepaid token bundle
                   (https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1).
                   Env var: `ALIBABA_TOKEN_PLAN_API_KEY`.

   Both endpoints speak the OpenAI-compatible chat wire (`/chat/completions`,
   SSE streaming with `stream_options.include_usage`, native `tool_calls`,
   `reasoning_content` for thinking models), so svar's default
   `:openai-compatible-chat` api-style drives them unchanged.

   The plans are SEPARATE credentials: a Token Plan key is rejected by the
   Coding Plan endpoint and vice versa (HTTP 401 `InvalidApiKey`). Hence one
   provider id, one env var and one auth-file slice per plan - never a shared
   `ALIBABA_API_KEY` fallback that would silently authenticate as the wrong
   plan.

   The provider ids match their models.dev slugs, which is what lets svar
   resolve pricing, context windows and capabilities for the catalog models
   listed in each preset; svar's `KNOWN_PROVIDERS` has no Alibaba entry, so
   the preset here owns `:base-url` (svar accepts an unknown provider id
   whenever a base URL is supplied).

   Auth lifecycle:
     1. `vis-agent providers auth alibaba-coding-plan` (or
        `vis-agent providers auth alibaba-token-plan`) takes the API key once
        and persists it under `~/.vis/alibaba-auth.json`, as canonical
        snake_case JSON - top-level plan tag, then `api_key` / `saved_at`
        (never kebab, never keyword keys).
     2. Subsequent runs read the configured provider key, env var, or
        persisted key. A TUI/config `:api-key` wins so status/limits match the
        key used for model calls; the env vars override the auth file when
        present so CI / scripted setups stay home-directory-free.
     3. `vis-agent providers status alibaba-coding-plan` reports the source
        (config / env / file) without exposing the full key.
     4. `vis-agent providers logout alibaba-coding-plan` clears the persisted
        key for that plan only; the other plan stays intact."
  (:require [babashka.http-client :as http]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.provider.key-store :as key-store]))

(def ^:private BOOK
  "Alibaba's slice of the shared static-API-key shape
   (`com.blockether.vis.internal.provider.key-store`): one file, one plan table,
   and the strings a message needs.

   The map key is the LOCAL plan tag used inside the persisted auth file
   (`:coding` / `:token`); `:provider-id` is the svar/vis catalog id surfaced to
   the rest of the system. Keep the local tag stable so a future schema upgrade
   skips a file-key migration.

   `:env-keys` is plan-specific and matches the models.dev `env` entry for the
   same slug. Keep the two credentials separate so one plan never silently
   authenticates as the other.

   `:base-url` and `:default-models` live here rather than in svar because
   svar's `KNOWN_PROVIDERS` carries no Alibaba entry; the model ids are the
   ones both `/models` endpoints actually serve, and every one except
   `qwen3.8-max` resolves its limits from the models.dev catalog under the
   matching provider slug. `qwen3.8-max` ships as `qwen3.8-max-preview` in the
   catalog, so its window is declared inline."
  {:vendor "Alibaba"
   :file "alibaba-auth.json"
   :key-hint "<your-alibaba-api-key>"
   :error-type :vis/alibaba-not-authenticated
   :auth-notes ["  The key is plan-scoped - a key issued for the other Alibaba plan"
                "  is rejected by this endpoint." ""]
   :plans
   {:coding {:provider-id :alibaba-coding-plan
             :label "Alibaba (Coding Plan)"
             :base-url "https://coding-intl.dashscope.aliyuncs.com/v1"
             :default-models ["qwen3.7-max" "qwen3.7-plus" "qwen3.6-flash" "qwen3.6-plus" "glm-5"]
             :env-keys ["ALIBABA_CODING_PLAN_API_KEY"]
             ;; A flat-fee plan: the second choice for session titles.
             :policy {:preset-rank 7 :title-rank 1}}
    :token {:provider-id :alibaba-token-plan
            :label "Alibaba (Token Plan)"
            :base-url "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
            :default-models
            [{:name "qwen3.8-max" :context 1000000 :output-limit 131072 :tool-call? true}
             "qwen3.7-max" "qwen3.7-plus" "qwen3.6-flash" "glm-5.3" "deepseek-v4-pro"]
            :env-keys ["ALIBABA_TOKEN_PLAN_API_KEY"]
            ;; This `/models` route answers 401 `InvalidApiKey` to a bad key, so
            ;; it proves a key without spending tokens. The Coding Plan route
            ;; answers 200 to any key, so that plan has no key check.
            :key-check-path "/models"
            :policy {:preset-rank 8}}}})

(def ^:private console-note
  " Alibaba exposes no quota endpoint; check the remaining allowance in the Model Studio console.")

(defn- key-check-status
  "HTTP status of the plan's key check, or nil when the request fails."
  [base-url path api-key]
  (try (:status (http/get (str base-url path)
                          {:headers {"Accept" "application/json"
                                     "Authorization" (str "Bearer " api-key)}
                           :timeout 30000
                           :throw false}))
       (catch Exception _ nil)))

(defn- make-limits-fn
  "Neither plan exposes a quota/usage endpoint: `/usage`, `/quota` and the
   OpenAI-compatible `/dashboard/billing/*` routes all answer 404 on both
   base URLs. So the report carries no numbers. A plan with a
   `:key-check-path` proves its key live (`:ok` or `:unauthenticated`);
   the other plan stays `:unsupported`."
  [plan-tag]
  (fn []
    (let [{:keys [provider-id label base-url key-check-path]}
          (get-in BOOK [:plans plan-tag])

          detected
          (key-store/detect-key BOOK plan-tag)

          status
          (when (and detected key-check-path)
            (key-check-status base-url key-check-path (:api-key detected)))

          [verdict note]
          (cond (nil? detected) [:unauthenticated (str label " is not authenticated.")]
                (nil? key-check-path) [:unsupported (str label " has no key check." console-note)]
                (and (number? status) (<= 200 status 299))
                [:ok (str label " accepted the API key." console-note)]
                (contains? #{401 403} status) [:unauthenticated
                                               (str label " rejected the current API key.")]
                :else [:error
                       (str label " key check failed" (when status (str ": HTTP " status)) ".")])]

      {:provider-id provider-id
       :status verdict
       :fetched-at-ms (util/now-ms)
       :dynamic {:limits [] :note note}})))

;; Provider registration
;;
;; Loading this namespace registers ONE extension entry per plan.
;; `:alibaba-coding-plan` and `:alibaba-token-plan` are independent
;; first-class providers - `vis-agent providers auth alibaba-coding-plan`,
;; `vis-agent providers status alibaba-token-plan`, per-plan logout, etc. all
;; work. The TUI's add-provider picker shows them as two separate cards driven
;; by each provider's preset metadata.

(defn register!
  []
  (extension/register-extension!
    (extension/extension {:ext/name "provider-alibaba"
                          :ext/description
                          "Alibaba Model Studio coding-plan + token-plan static-API-key providers."
                          :ext/version "0.1.0"
                          :ext/author "Blockether"
                          :ext/owner "vis"
                          :ext/license "Apache-2.0"
                          :ext/providers (key-store/provider-entries BOOK make-limits-fn)})))
