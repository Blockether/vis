(ns com.blockether.vis.ext.provider-alibaba
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
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel]))

;; Constants

(defn- auth-file
  "Persisted auth state. Shared file for both plans, keyed by
   `:coding` / `:token` so logging out of one plan doesn't clobber
   the other. A FUNCTION: native-image folds top-level `def`s at build
   time, which would bake the builder's home into the binary."
  ^String []
  (str (System/getProperty "user.home") "/.vis/alibaba-auth.json"))

(def ^:private PLANS
  "Per-plan metadata. The map key is the LOCAL plan tag used inside
   the persisted auth file (`:coding` / `:token`); `:provider-id` is
   the svar/vis catalog id surfaced to the rest of the system. Keep
   the local tag stable so future schema upgrades skip a file-key
   migration.

   `:env-keys` is plan-specific and matches the models.dev `env` entry for the
   same slug. Keep the two credentials separate so one plan never silently
   authenticates as the other.

   `:base-url` and `:default-models` live here rather than in svar because
   svar's `KNOWN_PROVIDERS` carries no Alibaba entry; the model ids are the
   ones both `/models` endpoints actually serve, and every one except
   `qwen3.8-max` resolves its limits from the models.dev catalog under the
   matching provider slug. `qwen3.8-max` ships as `qwen3.8-max-preview` in the
   catalog, so its window is declared inline."
  {:coding {:provider-id :alibaba-coding-plan
            :label "Alibaba (Coding Plan)"
            :base-url "https://coding-intl.dashscope.aliyuncs.com/v1"
            :default-models ["qwen3-coder-plus" "qwen3-coder-next" "qwen3.7-plus" "qwen3.6-plus"
                             "qwen3.5-plus" "qwen3-max-2026-01-23" "glm-5" "glm-4.7" "kimi-k2.5"
                             "MiniMax-M2.5"]
            :env-keys ["ALIBABA_CODING_PLAN_API_KEY"]}
   :token {:provider-id :alibaba-token-plan
           :label "Alibaba (Token Plan)"
           :base-url "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
           :default-models
           [{:name "qwen3.8-max" :context 1000000 :output-limit 131072 :tool-call? true}
            "qwen3.7-max" "qwen3.7-plus" "qwen3.6-flash" "glm-5.2" "deepseek-v4-pro"]
           :env-keys ["ALIBABA_TOKEN_PLAN_API_KEY"]}})

;; Persistence

(defn- auth-json-key
  "JSON key -> engine keyword. What we write is snake_case (`api_key`); the
   kebab spelling reads back onto the same key."
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn- load-auth-file
  "Load `~/.vis/alibaba-auth.json` or nil. Returns the WHOLE map (both
   plans) so a single read serves callers querying any sibling plan."
  []
  (let [f (io/file (auth-file))]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn auth-json-key) (catch Exception _ nil)))))

(defn- save-auth-file!
  "Persist the WHOLE auth map through the ONE JSON boundary (`wire/json-str`):
   snake_case string keys, total encoding. Caller is responsible for merging
   the per-plan slice into the existing file via `update-plan!`."
  [auth-state]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit (auth-file) (wire/json-str auth-state))))

(defn- update-plan!
  "Merge `slice` into the existing auth file under `plan-tag`. When
   `slice` is nil, REMOVE the plan entry. Returns the new map."
  [plan-tag slice]
  (let [current
        (or (load-auth-file) {})

        next-state
        (if (nil? slice) (dissoc current plan-tag) (assoc current plan-tag slice))]

    (if (seq next-state)
      (save-auth-file! next-state)
      ;; Empty file -> remove on disk so the file's existence stays
      ;; truthy with `detect-fn` semantics.
      (let [f (io/file (auth-file))]
        (when (.exists f) (.delete f))))
    next-state))

;; Token detection / resolution

(defn- env-key-for-plan
  "First non-blank env var from the plan's `:env-keys` priority
   list, or nil."
  [plan-tag]
  (some (fn [name]
          (let [v (System/getenv name)]
            (when-not (str/blank? v) v)))
        (:env-keys (get PLANS plan-tag))))

(defn- configured-key-for-plan
  "API key from Vis provider config (`~/.vis/config.edn`) for this
   plan, or nil. This covers the TUI flow, which stores static API keys
   in the normal provider config instead of the provider auth file."
  [plan-tag]
  (let [provider-id (:provider-id (get PLANS plan-tag))]
    (try (when-let [current-config-fn (requiring-resolve 'com.blockether.vis.core/current-config)]
           (some (fn [provider]
                   (when (= provider-id (:id provider))
                     (when-let [k (:api-key provider)]
                       (when-not (str/blank? k) k))))
                 (:providers (current-config-fn))))
         (catch Throwable _ nil))))

(defn- auth-file-key-for-plan
  [plan-tag]
  (when-let [from-file (get (load-auth-file) plan-tag)]
    (when-let [k (:api-key from-file)]
      (when-not (str/blank? k) k))))

(defn- detect-key
  "Lookup priority for one plan:
     1. TUI/config provider `:api-key` for this plan.
     2. The plan's env-var chain.
     3. `~/.vis/alibaba-auth.json` slice for this plan.
   Returns `{:api-key str :source kw}` or nil. Never throws.

   `:source` is `:config`, `:env-var`, or `:auth-file` so the status fn
   can show the user where the key came from."
  [plan-tag]
  (or (when-let [k (configured-key-for-plan plan-tag)]
        {:api-key k :source :config})
      (when-let [k (env-key-for-plan plan-tag)]
        {:api-key k :source :env-var})
      (when-let [k (auth-file-key-for-plan plan-tag)]
        {:api-key k :source :auth-file})))

(defn- get-token
  "Resolve a usable API key for the given plan. Throws when no
   source has one so the runtime fails fast with a clear pointer at
   `vis-agent providers auth <plan>` instead of a confusing 401
   `InvalidApiKey` from Model Studio."
  [plan-tag]
  (let [{:keys [provider-id base-url]} (get PLANS plan-tag)]
    (if-let [{:keys [api-key]} (detect-key plan-tag)]
      ;; Provider extensions expose runtime credentials as a uniform
      ;; token envelope consumed by the central router adapter.
      {:token api-key :api-url base-url}
      (throw (ex-info
               (str "No Alibaba API key for plan "
                    plan-tag
                    ". Run `vis-agent providers auth "
                    (name provider-id)
                    "` to authenticate, "
                    "or set "
                    (str/join " / " (:env-keys (get PLANS plan-tag)))
                    ".")
               {:type :vis/alibaba-not-authenticated :plan plan-tag :provider-id provider-id})))))

;; Per-plan provider fns

(defn- make-detect-fn
  [plan-tag]
  (fn []
    (detect-key plan-tag)))

(defn- make-get-token-fn
  [plan-tag]
  (fn []
    (get-token plan-tag)))

(defn- key-preview
  "Short non-secret preview for the status output. Model Studio keys are
   long opaque `sk-` tokens; show the first 8 chars + ellipsis."
  [api-key]
  (let [n (count api-key)]
    (if (<= n 12) (str (subs api-key 0 (min 4 n)) "...") (str (subs api-key 0 8) "..."))))

(defn- make-status-fn
  [plan-tag]
  (fn []
    (let [{:keys [provider-id label]}
          (get PLANS plan-tag)

          detected
          (detect-key plan-tag)]

      (cond-> {:is-authenticated (some? detected) :provider-id provider-id :label label}
        detected
        (assoc :source
          (:source detected) :api-key-preview
          (key-preview (:api-key detected)))))))

(defn- make-logout-fn
  [plan-tag]
  (fn []
    (update-plan! plan-tag nil)
    (tel/log! {:level :info
               :id ::alibaba-logout
               :data {:plan plan-tag}
               :msg (str "Cleared persisted Alibaba key for plan " plan-tag)})
    :logged-out))

(defn- make-limits-fn
  "Neither plan exposes a quota/usage endpoint: `/usage`, `/quota` and the
   OpenAI-compatible `/dashboard/billing/*` routes all answer 404 on both
   base URLs. So the report carries authentication state plus a note pointing
   at the Model Studio console instead of inventing numbers."
  [plan-tag]
  (fn []
    (let [{:keys [provider-id label]}
          (get PLANS plan-tag)

          detected
          (detect-key plan-tag)]

      {:provider-id provider-id
       :status (if detected :unsupported :unauthenticated)
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic {:limits []
                 :note (if detected
                         (str label
                              " does not expose a quota endpoint; check the remaining"
                              " allowance in the Model Studio console.")
                         (str label " is not authenticated."))}})))

(defn- auth-instruction-lines
  [plan-tag]
  (let [{:keys [provider-id label env-keys base-url]} (get PLANS plan-tag)]
    (vec (concat ["" (str "  " label " requires a static API key.") ""
                  "  The key is plan-scoped - a key issued for the other Alibaba plan"
                  "  is rejected by this endpoint." "" "  Two ways to authenticate:" ""
                  (str "    1. Set the env var, then re-run `vis-agent providers auth "
                       (name provider-id)
                       "`:")]
                 (mapv (fn [name*]
                         (str "         export " name* "=<your-alibaba-api-key>"))
                       env-keys)
                 ["" "    2. Add the provider through the TUI (Ctrl+K -> Providers)."
                  "       The TUI prompts for the key directly and writes it to the config." ""
                  (str "  Endpoint: " base-url)]))))

(defn- make-auth-fn
  "Interactive auth flow. The runtime invokes this with a single
   `printer-fn` arg (an `(fn [line] ...)` that writes one line of
   user-visible output). We can't use `read-line` directly because
   the CLI dispatcher captures stdout/stderr to a log file; the
   shared pattern is to print instructions and accept the key from
   the env var the user set in the shell that ran
   `vis-agent providers auth ...`. If the env var is already populated we
   just persist it; otherwise we instruct the user to set it and re-run."
  [plan-tag]
  (fn [printer-fn]
    (let [print!
          (or printer-fn (constantly nil))

          {:keys [provider-id label env-keys base-url]}
          (get PLANS plan-tag)

          existing
          (detect-key plan-tag)]

      (cond
        ;; Configured or already on disk -> no-op so re-running auth doesn't
        ;; require re-typing the key.
        (and existing (contains? #{:config :auth-file} (:source existing)))
        (do (print! (str "  Already authenticated with " label "."))
            (print! (str "  Source: " (name (:source existing)) "."))
            (print! (str "  Run `vis-agent providers status " (name provider-id) "` for details."))
            (print! (str "  Run `vis-agent providers logout "
                         (name provider-id)
                         "` first to switch stored keys."))
            :already-authenticated)
        ;; Env var is set but not persisted -> write it through to
        ;; the file so subsequent runs read the persisted key
        ;; directly, independent of the user's shell env.
        (and existing (= :env-var (:source existing)))
        (do (update-plan!
              plan-tag
              {:api-key (:api-key existing) :saved-at (System/currentTimeMillis) :from :env-var})
            (print! (str "  Persisted Alibaba key from env var (" (str/join " / " env-keys) ")."))
            (print! (str "  " label " is ready (endpoint: " base-url ")."))
            :ok)
        ;; Nothing anywhere -> tell the user how to provide one.
        :else (do (doseq [line (auth-instruction-lines plan-tag)]
                    (print! line))
                  :no-credentials)))))

;; Public CLI helpers (used by both auth-fn and `vis-agent providers`)

(defn authenticated?
  "True if any plan has a usable key from any source. Convenience for
   doctor-style probes; not part of the provider contract."
  []
  (some #(detect-key %) (keys PLANS)))

(defn status
  "Aggregate status across both plans. Useful at the REPL."
  []
  (into {}
        (map (fn [plan-tag]
               [plan-tag ((make-status-fn plan-tag))]))
        (keys PLANS)))

(defn logout!
  "Clear BOTH persisted plan keys. Plan-specific logout goes through
   `vis-agent providers logout <plan>` which dispatches to the per-plan
   logout-fn registered below."
  []
  (let [f (io/file (auth-file))]
    (when (.exists f) (.delete f)))
  :logged-out)

;; Provider registration
;;
;; Loading this namespace registers ONE extension entry per plan.
;; `:alibaba-coding-plan` and `:alibaba-token-plan` are independent
;; first-class providers - `vis-agent providers auth alibaba-coding-plan`,
;; `vis-agent providers status alibaba-token-plan`, per-plan logout, etc. all
;; work. The TUI's add-provider picker shows them as two separate cards driven
;; by each provider's preset metadata.

(require '[com.blockether.vis.core :as vis])

(defn- provider-entry
  [plan-tag]
  (let [{:keys [provider-id label base-url default-models]} (get PLANS plan-tag)]
    {:provider/id provider-id
     :provider/label label
     :provider/preset {:base-url base-url :default-models default-models}
     :provider/status-fn (make-status-fn plan-tag)
     :provider/logout-fn (make-logout-fn plan-tag)
     :provider/detect-fn (make-detect-fn plan-tag)
     :provider/auth-fn (make-auth-fn plan-tag)
     :provider/auth-prompt-fn #(auth-instruction-lines plan-tag)
     :provider/get-token-fn (make-get-token-fn plan-tag)
     :provider/limits-fn (make-limits-fn plan-tag)}))

(defn register!
  []
  (vis/register-extension!
    (vis/extension {:ext/name "provider-alibaba"
                    :ext/description
                    "Alibaba Model Studio coding-plan + token-plan static-API-key providers."
                    :ext/version "0.1.0"
                    :ext/author "Blockether"
                    :ext/owner "vis"
                    :ext/license "Apache-2.0"
                    :ext/providers (mapv provider-entry (keys PLANS))})))
