(ns com.blockether.vis.ext.provider-zai
  "Z.ai (ZhipuAI) static-API-key provider helpers. Each plan is registered as its own extension:

     :zai-coding → coding-plan subscription
                   (https://api.z.ai/api/coding/paas/v4).
                   Env var: `ZAI_CODING_API_KEY`.

     :zai        → pay-as-you-go / `Pass` gateway
                   (https://api.z.ai/api/paas/v4).
                   Env var: `ZAI_API_KEY`.

   Both endpoints serve the same GLM model family (`glm-5-turbo`,
   `glm-5.1`, `glm-4.7`, `glm-4.6`, `glm-4.6v`, …) with binary
   thinking (`:zai-thinking` reasoning-style — handled by svar). They
   share helper code, but the runtime extension registry sees one
   extension entry per provider id.

   Auth lifecycle:
     1. `vis providers auth zai-coding` (or `vis providers auth zai`) prompts for the API
        key once and persists it under `~/.vis/zai-auth.json`,
        `{:coding {:api-key str :saved-at long}
          :pass   {:api-key str :saved-at long}}`.
     2. Subsequent runs read the persisted key. Env vars
        (`ZAI_CODING_API_KEY`, `ZAI_API_KEY`) override the file when
        present so CI / scripted setups stay home-directory-free.
     3. `vis providers status zai-coding` reports the source
        (file / env) without exposing the full key.
     4. `vis providers logout zai-coding` clears the persisted key for
        that plan only; the other plan stays intact."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
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
  {:coding {:provider-id :zai-coding
            :label       "Z.ai (Coding Plan)"
            :base-url    "https://api.z.ai/api/coding/paas/v4"
            :default-models ["glm-5-turbo" "glm-4.7" "glm-5.1"]
            :env-keys    ["ZAI_CODING_API_KEY"]}
   :pass   {:provider-id :zai
            :label       "Z.ai (Pass)"
            :base-url    "https://api.z.ai/api/paas/v4"
            :default-models ["glm-5-turbo" "glm-5.1" "glm-4.7" "glm-4.6v"]
            :env-keys    ["ZAI_API_KEY"]}})

;; =============================================================================
;; Persistence
;; =============================================================================

(defn- load-auth-file
  "Load `~/.vis/zai-auth.json` or nil. Returns the WHOLE map (both
   plans) so a single read serves callers querying any sibling plan."
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f)
      (try
        (json/read-json (slurp f) :key-fn keyword)
        (catch Exception _ nil)))))

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
  (let [current (or (load-auth-file) {})
        next-state (if (nil? slice)
                     (dissoc current plan-tag)
                     (assoc current plan-tag slice))]
    (if (seq next-state)
      (save-auth-file! next-state)
      ;; Empty file → remove on disk so the file's existence stays
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

(defn- detect-key
  "Lookup priority for one plan:
     1. `~/.vis/zai-auth.json` slice for this plan.
     2. The plan's env-var chain.
   Returns `{:api-key str :source kw}` or nil. Never throws.

   `:source` is `:auth-file` or `:env-var` so the status fn can
   show the user where the key came from."
  [plan-tag]
  (or (when-let [from-file (get (load-auth-file) plan-tag)]
        (when-let [k (:api-key from-file)]
          (when-not (str/blank? k)
            {:api-key k :source :auth-file})))
    (when-let [k (env-key-for-plan plan-tag)]
      {:api-key k :source :env-var})))

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
      (throw (ex-info (str "No Z.ai API key for plan " plan-tag
                        ". Run `vis providers auth " (name provider-id) "` to authenticate, "
                        "or set " (str/join " / " (:env-keys (get PLANS plan-tag))) ".")
               {:type        :vis/zai-not-authenticated
                :plan        plan-tag
                :provider-id provider-id})))))

;; =============================================================================
;; Per-plan provider fns
;; =============================================================================

(defn- make-detect-fn [plan-tag]
  (fn [] (detect-key plan-tag)))

(defn- make-get-token-fn [plan-tag]
  (fn [] (get-token plan-tag)))

(defn- key-preview
  "Short non-secret preview for the status output. Z.ai keys are
   long opaque tokens; show the first 8 chars + ellipsis."
  [api-key]
  (let [n (count api-key)]
    (if (<= n 12)
      (str (subs api-key 0 (min 4 n)) "...")
      (str (subs api-key 0 8) "..."))))

(defn- make-status-fn [plan-tag]
  (fn []
    (let [{:keys [provider-id label]} (get PLANS plan-tag)
          detected (detect-key plan-tag)]
      (cond-> {:authenticated? (some? detected)
               :provider-id    provider-id
               :label          label}
        detected
        (assoc :source            (:source detected)
          :api-key-preview   (key-preview (:api-key detected)))))))

(defn- make-logout-fn [plan-tag]
  (fn []
    (update-plan! plan-tag nil)
    (tel/log! {:level :info :id ::zai-logout
               :data  {:plan plan-tag}
               :msg   (str "Cleared persisted Z.ai key for plan " plan-tag)})
    :logged-out))

(defn- make-limits-fn [plan-tag]
  (fn []
    (let [{:keys [provider-id label]} (get PLANS plan-tag)
          detected (detect-key plan-tag)]
      {:provider-id   provider-id
       :status        (if detected :ok :unauthenticated)
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic       {:limits []
                       :note (if detected
                               (str label " does not expose a dynamic quota endpoint yet.")
                               (str label " is not authenticated."))}})))

(defn- auth-instruction-lines
  [plan-tag]
  (let [{:keys [provider-id label env-keys base-url]} (get PLANS plan-tag)]
    (vec
      (concat [""
               (str "  " label " requires a static API key.")
               ""
               "  Two ways to authenticate:"
               ""
               (str "    1. Set the env var, then re-run `vis providers auth " (name provider-id) "`:")]
        (mapv (fn [name*]
                (str "         export " name* "=<your-zai-api-key>"))
          env-keys)
        [""
         "    2. Add the provider through the TUI (Ctrl+K → Providers)."
         "       The TUI prompts for the key directly and writes it to the config."
         ""
         (str "  Endpoint: " base-url)]))))

(defn- make-auth-fn
  "Interactive auth flow. The runtime invokes this with a single
   `printer-fn` arg (an `(fn [line] ...)` that writes one line of
   user-visible output). We can't use `read-line` directly because
   the CLI dispatcher captures stdout/stderr to a log file; the
   shared pattern is to print instructions and accept the key from
   the env var the user set in the shell that ran `vis providers auth …`. If
   the env var is already populated we just persist it; otherwise we
   instruct the user to set it and re-run."
  [plan-tag]
  (fn [printer-fn]
    (let [print! (or printer-fn (constantly nil))
          {:keys [provider-id label env-keys base-url]} (get PLANS plan-tag)
          existing (detect-key plan-tag)]
      (cond
        ;; Already on disk → no-op so re-running auth doesn't
        ;; require re-typing the key.
        (and existing (= :auth-file (:source existing)))
        (do
          (print! (str "  Already authenticated with " label "."))
          (print! (str "  Run `vis providers status " (name provider-id) "` for details."))
          (print! (str "  Run `vis providers logout " (name provider-id) "` first to switch keys."))
          :already-authenticated)

        ;; Env var is set but not persisted → write it through to
        ;; the file so subsequent runs read the persisted key
        ;; directly, independent of the user's shell env.
        (and existing (= :env-var (:source existing)))
        (do
          (update-plan! plan-tag {:api-key   (:api-key existing)
                                  :saved-at  (System/currentTimeMillis)
                                  :from      :env-var})
          (print! (str "  Persisted Z.ai key from env var ("
                    (str/join " / " env-keys) ")."))
          (print! (str "  " label " is ready (endpoint: " base-url ")."))
          :ok)

        ;; Nothing anywhere → tell the user how to provide one.
        :else
        (do
          (doseq [line (auth-instruction-lines plan-tag)]
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
;; `:zai-coding` and `:zai` are independent first-class providers —
;; `vis providers auth zai-coding`, `vis providers status zai`,
;; per-plan logout, etc. all work. The TUI's add-provider picker shows
;; them as two separate cards driven by each provider's preset metadata.
;; =============================================================================

(require '[com.blockether.vis.core :as vis])

(defn- provider-entry [plan-tag]
  (let [{:keys [provider-id label base-url default-models]} (get PLANS plan-tag)]
    {:provider/id             provider-id
     :provider/label          label
     :provider/preset         {:base-url       base-url
                               :default-models default-models}
     :provider/status-fn      (make-status-fn plan-tag)
     :provider/logout-fn      (make-logout-fn plan-tag)
     :provider/detect-fn      (make-detect-fn plan-tag)
     :provider/auth-fn        (make-auth-fn plan-tag)
     :provider/auth-prompt-fn #(auth-instruction-lines plan-tag)
     :provider/get-token-fn   (make-get-token-fn plan-tag)
     :provider/limits-fn      (make-limits-fn plan-tag)}))

(defn- register-plan-extension! [plan-tag]
  (let [{:keys [label]} (get PLANS plan-tag)]
    (vis/register-extension!
      (vis/extension
        {:ext/namespace (symbol (str "com.blockether.vis.ext.provider-zai." (name plan-tag)))
         :ext/nses      ['com.blockether.vis.ext.provider-zai]
         :ext/doc       (str label " static-API-key provider.")
         :ext/version   "0.2.0"
         :ext/author    "Blockether"
         :ext/owner     "vis"
         :ext/license   "Apache-2.0"
         :ext/providers [(provider-entry plan-tag)]}))))

(doseq [plan-tag (keys PLANS)]
  (register-plan-extension! plan-tag))
