(ns com.blockether.vis.ext.provider-zai
  "Z.ai (ZhipuAI) static-API-key provider helpers. Each plan is registered as its own extension:

     :zai-coding-plan -> coding-plan subscription
                   (https://api.z.ai/api/coding/paas/v4).
                   Env var: `ZAI_CODING_API_KEY`.

     :zai        -> pay-as-you-go / `Pass` gateway
                   (https://api.z.ai/api/paas/v4).
                   Env var: `ZAI_API_KEY`.

   Both endpoints serve the same GLM model family (`glm-5-turbo`,
   `glm-5.1`, `glm-4.7`, `glm-4.6`, `glm-4.6v`, ...) with binary
   thinking (`:zai-thinking` reasoning-style - handled by svar). They
   share helper code, but the runtime extension registry sees one
   extension entry per provider id.

   Auth lifecycle:
     1. `vis providers auth zai-coding` (or `vis providers auth zai`) prompts for the API
        key once and persists it under `~/.vis/zai-auth.json`,
        `{:coding {:api-key str :saved-at long}
          :pass   {:api-key str :saved-at long}}`.
     2. Subsequent runs read the configured provider key, env var, or
        persisted key. A TUI/config `:api-key` wins so status/limits
        match the key used for model calls; env vars
        (`ZAI_CODING_API_KEY`, `ZAI_API_KEY`) override the auth file when
        present so CI / scripted setups stay home-directory-free.
     3. `vis providers status zai-coding` reports the source
        (config / env / file) without exposing the full key.
     4. `vis providers logout zai-coding` clears the persisted key for
        that plan only; the other plan stays intact."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private AUTH_FILE
  "Persisted auth state. Shared file for both plans, keyed by
   `:coding` / `:pass` so logging out of one plan doesn't clobber
   the other."
  (str (System/getProperty "user.home") "/.vis/zai-auth.json"))

(def ^:private PLANS
  "Per-plan metadata. The map key is the LOCAL plan tag used inside
   the persisted auth file (`:coding` / `:pass`); `:provider-id` is
   the svar/vis catalog id surfaced to the rest of the system. Keep
   the local tag stable so future schema upgrades skip a file-key
   migration.

   `:env-keys` is plan-specific. Keep the coding and pass credentials
   separate so one plan never silently authenticates as the other."
  ;; base-url + default-models come from svar (single source of truth — see
  ;; svar `KNOWN_PROVIDERS`). Override here only if this provider ever needs to
  ;; diverge from svar's sane defaults.
  {:coding {:provider-id :zai-coding-plan
            :label "Z.ai (Coding Plan)"
            :base-url (svar/provider-base-url :zai-coding-plan)
            :default-models (svar/provider-default-models :zai-coding-plan)
            :env-keys ["ZAI_CODING_API_KEY"]}
   :pass {:provider-id :zai
          :label "Z.ai (Pass)"
          :base-url (svar/provider-base-url :zai)
          :default-models (svar/provider-default-models :zai)
          :env-keys ["ZAI_API_KEY"]}})

;; =============================================================================
;; Persistence
;; =============================================================================

(defn- load-auth-file
  "Load `~/.vis/zai-auth.json` or nil. Returns the WHOLE map (both
   plans) so a single read serves callers querying any sibling plan."
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f) (try (json/read-json (slurp f) :key-fn keyword) (catch Exception _ nil)))))

(defn- save-auth-file!
  "Persist the WHOLE auth map. Caller is responsible for merging the
   per-plan slice into the existing file via `update-plan!`."
  [auth-state]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit AUTH_FILE (json/write-json-str auth-state))))

(defn- update-plan!
  "Merge `slice` into the existing auth file under `plan-tag`. When
   `slice` is nil, REMOVE the plan entry. Returns the new map."
  [plan-tag slice]
  (let
    [current
     (or (load-auth-file) {})

     next-state
     (if (nil? slice) (dissoc current plan-tag) (assoc current plan-tag slice))]

    (if (seq next-state)
      (save-auth-file! next-state)
      ;; Empty file -> remove on disk so the file's existence stays
      ;; truthy with `detect-fn` semantics.
      (let [f (io/file AUTH_FILE)]
        (when (.exists f) (.delete f))))
    next-state))

;; =============================================================================
;; Token detection / resolution
;; =============================================================================

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
     3. `~/.vis/zai-auth.json` slice for this plan.
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
   `vis providers auth <plan>` instead of a confusing 401 from Z.ai."
  [plan-tag]
  (let [{:keys [provider-id base-url]} (get PLANS plan-tag)]
    (if-let [{:keys [api-key]} (detect-key plan-tag)]
      ;; Provider extensions expose runtime credentials as a uniform
      ;; token envelope consumed by the central router adapter.
      {:token api-key :api-url base-url}
      (throw (ex-info
               (str "No Z.ai API key for plan "
                    plan-tag
                    ". Run `vis providers auth "
                    (name provider-id)
                    "` to authenticate, "
                    "or set "
                    (str/join " / " (:env-keys (get PLANS plan-tag)))
                    ".")
               {:type :vis/zai-not-authenticated :plan plan-tag :provider-id provider-id})))))

;; =============================================================================
;; Per-plan provider fns
;; =============================================================================

(defn- make-detect-fn
  [plan-tag]
  (fn []
    (detect-key plan-tag)))

(defn- make-get-token-fn
  [plan-tag]
  (fn []
    (get-token plan-tag)))

(defn- key-preview
  "Short non-secret preview for the status output. Z.ai keys are
   long opaque tokens; show the first 8 chars + ellipsis."
  [api-key]
  (let [n (count api-key)]
    (if (<= n 12) (str (subs api-key 0 (min 4 n)) "...") (str (subs api-key 0 8) "..."))))

(defn- make-status-fn
  [plan-tag]
  (fn []
    (let
      [{:keys [provider-id label]}
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
               :id ::zai-logout
               :data {:plan plan-tag}
               :msg (str "Cleared persisted Z.ai key for plan " plan-tag)})
    :logged-out))

(def ^:private coding-quota-url "https://api.z.ai/api/monitor/usage/quota/limit")

(defn- object-map [value] (when (and (map? value) (not (record? value))) value))

(defn- field
  [m k]
  (when-let [m* (object-map m)]
    (cond (contains? m* k) (get m* k)
          (contains? m* (name k)) (get m* (name k)))))

(defn- limit-kind
  [limit]
  (case
    (some-> (field limit :type)
            str/upper-case)
    "TOKENS_LIMIT"
    :tokens

    "TIME_LIMIT"
    :requests

    :rate))

(defn- limit-label
  [limit]
  (let
    [kind
     (limit-kind limit)

     unit
     (field limit :unit)

     number
     (field limit :number)]

    (cond (and (= :tokens kind) (= 3 unit) (= 5 number)) "Z.ai coding plan 5h token quota"
          (and (= :tokens kind) (= 6 unit) (= 7 number)) "Z.ai coding plan 7d token quota"
          (= :tokens kind) "Z.ai coding plan token quota"
          (= :requests kind) "Z.ai coding plan request quota"
          :else "Z.ai coding plan quota")))

(defn- limit-id
  [limit idx]
  (let
    [kind
     (limit-kind limit)

     unit
     (field limit :unit)

     number
     (field limit :number)]

    (cond (and (= :tokens kind) (= 3 unit) (= 5 number)) :zai-coding-plan-5h
          (and (= :tokens kind) (= 6 unit) (= 7 number)) :zai-coding-plan-7d
          :else (keyword (str "zai-coding-limit-" idx)))))

(defn- limit-window
  [limit]
  (let
    [unit
     (field limit :unit)

     number
     (field limit :number)

     reset-at-ms
     (field limit :nextResetTime)

     base
     (case unit
       3
       {:kind :rolling :unit :hour :size (or number 1)}

       5
       {:kind :calendar :unit :month :size (or number 1)}

       6
       {:kind :rolling :unit :day :size (or number 1)}

       nil)]

    (cond-> base
      (and base (number? reset-at-ms))
      (assoc :resets-at-ms (long reset-at-ms)))))

(defn- quota-limit-row
  [idx limit]
  (let
    [kind
     (limit-kind limit)

     usage
     (field limit :usage)

     current
     (field limit :currentValue)

     remaining
     (field limit :remaining)

     percentage
     (field limit :percentage)

     window
     (limit-window limit)

     token-pct?
     (and (= :tokens kind) (number? percentage))]

    (cond->
      {:id (limit-id limit idx)
       :label (limit-label limit)
       :scope :plan
       :kind kind
       :precision :exact
       :source :provider-api
       :is-unlimited false}
      window
      (assoc :window window)

      token-pct?
      (assoc :used
        (double percentage) :limit
        100.0 :remaining
        (double (max 0.0 (- 100.0 (double percentage)))))

      (and (not token-pct?) (number? current))
      (assoc :used (double current))

      (and (not token-pct?) (number? usage))
      (assoc :limit (double usage))

      (and (not token-pct?) (number? remaining))
      (assoc :remaining (double remaining))

      (and (not token-pct?) (not (number? current)) (number? percentage))
      (assoc :used
        (double percentage) :limit
        100.0 :remaining
        (double (max 0.0 (- 100.0 (double percentage))))))))

(defn- quota->dynamic-limits
  [quota]
  (let
    [data
     (or (field quota :data) quota)

     limits
     (field data :limits)

     rows
     (if (sequential? limits) (mapv quota-limit-row (range) limits) [])

     level
     (field data :level)]

    (cond-> {:limits rows}
      (empty? rows)
      (assoc :note "Z.ai coding plan quota endpoint did not return quota windows.")

      (and (seq rows) (some? level))
      (assoc :note (str "Z.ai coding plan level: " level ".")))))

(defn- fetch-quota!
  [api-key]
  (let
    [response
     (http/get coding-quota-url
               {:headers {"Accept" "application/json" "Authorization" (str "Bearer " api-key)}
                :timeout 30000
                :throw false})

     status
     (:status response)

     body
     (:body response)]

    (if (<= 200 status 299)
      (json/read-json body :key-fn keyword)
      (throw (ex-info (str "Z.ai coding plan quota request failed: HTTP " status)
                      {:type :provider/zai-coding-quota-error
                       :status status
                       :body body
                       :url coding-quota-url})))))

(defn- coding-dynamic-limits! [api-key] (quota->dynamic-limits (fetch-quota! api-key)))

(defn- make-limits-fn
  [plan-tag]
  (fn []
    (let
      [{:keys [provider-id label]}
       (get PLANS plan-tag)

       detected
       (detect-key plan-tag)]

      {:provider-id provider-id
       :status (if detected :ok :unauthenticated)
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic (cond (nil? detected) {:limits [] :note (str label " is not authenticated.")}
                      (= :coding plan-tag) (coding-dynamic-limits! (:api-key detected))
                      :else {:limits []
                             :note (str label
                                        " does not expose a dynamic quota endpoint yet.")})})))

(defn- auth-instruction-lines
  [plan-tag]
  (let [{:keys [provider-id label env-keys base-url]} (get PLANS plan-tag)]
    (vec
      (concat
        ["" (str "  " label " requires a static API key.") "" "  Two ways to authenticate:" ""
         (str "    1. Set the env var, then re-run `vis providers auth " (name provider-id) "`:")]
        (mapv (fn [name*]
                (str "         export " name* "=<your-zai-api-key>"))
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
   the env var the user set in the shell that ran `vis providers auth ...`. If
   the env var is already populated we just persist it; otherwise we
   instruct the user to set it and re-run."
  [plan-tag]
  (fn [printer-fn]
    (let
      [print!
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
            (print! (str "  Run `vis providers status " (name provider-id) "` for details."))
            (print! (str "  Run `vis providers logout "
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
            (print! (str "  Persisted Z.ai key from env var (" (str/join " / " env-keys) ")."))
            (print! (str "  " label " is ready (endpoint: " base-url ")."))
            :ok)
        ;; Nothing anywhere -> tell the user how to provide one.
        :else (do (doseq [line (auth-instruction-lines plan-tag)]
                    (print! line))
                  :no-credentials)))))

;; =============================================================================
;; Public CLI helpers (used by both auth-fn and `vis providers`)
;; =============================================================================

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
   `vis providers logout <plan>` which dispatches to the per-plan
   logout-fn registered below."
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f) (.delete f)))
  :logged-out)

;; =============================================================================
;; Provider registration
;;
;; Loading this namespace registers ONE extension entry per plan.
;; `:zai-coding-plan` and `:zai` are independent first-class providers -
;; `vis providers auth zai-coding`, `vis providers status zai`,
;; per-plan logout, etc. all work. The TUI's add-provider picker shows
;; them as two separate cards driven by each provider's preset metadata.
;; =============================================================================

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

(vis/register-extension! (vis/extension {:ext/name "provider-zai"
                                         :ext/description
                                         "Z.ai coding-plan + pass static-API-key providers."
                                         :ext/version "0.2.0"
                                         :ext/author "Blockether"
                                         :ext/owner "vis"
                                         :ext/license "Apache-2.0"
                                         :ext/providers (mapv provider-entry (keys PLANS))}))
