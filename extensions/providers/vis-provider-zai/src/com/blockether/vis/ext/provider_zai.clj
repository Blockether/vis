(ns com.blockether.vis.ext.provider-zai
  "Z.ai (ZhipuAI) static-API-key provider helpers. Each plan is registered as its own extension:

     :zai-coding-plan -> coding-plan subscription
                   (https://api.z.ai/api/coding/paas/v4).
                   Env var: `ZAI_CODING_API_KEY`.

     :zai        -> pay-as-you-go / `Pass` gateway
                   (https://api.z.ai/api/paas/v4).
                   Env var: `ZAI_API_KEY`.

   Both endpoints serve the same GLM model family (`glm-5.3-flash`,
   `glm-5.3`, `glm-5-turbo`, `glm-5.1`, ...) with effort-based reasoning
   on GLM-5.3 models and binary thinking on older models (handled by svar). They
   share helper code, but the runtime extension registry sees one
   extension entry per provider id.

   Auth lifecycle:
     1. `vis-agent providers auth zai-coding` (or `vis-agent providers auth zai`) prompts for the API
        key once and persists it under `~/.vis/zai-auth.json`,
        as canonical snake_case JSON - top-level plan tag, then
        `api_key` / `saved_at` (never kebab, never keyword keys).
     2. Subsequent runs read the configured provider key, env var, or
        persisted key. A TUI/config `:api-key` wins so status/limits
        match the key used for model calls; env vars
        (`ZAI_CODING_API_KEY`, `ZAI_API_KEY`) override the auth file when
        present so CI / scripted setups stay home-directory-free.
     3. `vis-agent providers status zai-coding` reports the source
        (config / env / file) without exposing the full key.
     4. `vis-agent providers logout zai-coding` clears the persisted key for
        that plan only; the other plan stays intact."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]))

(def ^:private BOOK
  "Z.ai's slice of the shared static-API-key shape
   (`com.blockether.vis.internal.provider-key-store`): one file, one plan table,
   and the strings a message needs.

   The map key is the LOCAL plan tag used inside the persisted auth file
   (`:coding` / `:pass`); `:provider-id` is the svar/vis catalog id surfaced to
   the rest of the system. Keep the local tag stable so a future schema upgrade
   skips a file-key migration.

   `:env-keys` is plan-specific. Keep the coding and pass credentials separate
   so one plan never silently authenticates as the other."
  ;; base-url + default-models come from svar (single source of truth — see
  ;; svar `KNOWN_PROVIDERS`). Override here only if this provider ever needs to
  ;; diverge from svar's sane defaults.
  {:vendor "Z.ai"
   :file "zai-auth.json"
   :key-hint "<your-zai-api-key>"
   ;; Where the key comes from. Both the TUI band and the companion prune these
   ;; instructions down to their one URL-bearing line, so whatever else this
   ;; shape prints stays terminal-only.
   :auth-notes ["  Create one at https://z.ai/manage-apikey/apikey-list." ""]
   :error-type :vis/zai-not-authenticated
   :plans {:coding {:provider-id :zai-coding-plan
                    :label "Z.ai (Coding Plan)"
                    :base-url (svar/provider-base-url :zai-coding-plan)
                    :default-models (svar/provider-default-models :zai-coding-plan)
                    :env-keys ["ZAI_CODING_API_KEY"]}
           :pass {:provider-id :zai
                  :label "Z.ai (Pass)"
                  :base-url (svar/provider-base-url :zai)
                  :default-models (svar/provider-default-models :zai)
                  :env-keys ["ZAI_API_KEY"]}}})

;; Coding-plan quota (the one plan with a live usage endpoint)

(def ^:private coding-quota-url "https://api.z.ai/api/monitor/usage/quota/limit")

(defn- object-map [value] (when (and (map? value) (not (record? value))) value))

(defn- field
  [m k]
  (when-let [m* (object-map m)]
    (cond (contains? m* k) (get m* k)
          (contains? m* (name k)) (get m* (name k)))))

(defn- limit-kind
  [limit]
  (case (some-> (field limit :type)
                str/upper-case)
    "TOKENS_LIMIT"
    :tokens

    "TIME_LIMIT"
    :requests

    :rate))

(defn- limit-label
  [limit]
  (let [kind
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
  (let [kind
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
  (let [unit
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
  (let [kind
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

    (cond-> {:id (limit-id limit idx)
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
  (let [data
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

(def ^:private quota-auth-statuses #{401 403})

(defn- application-quota-error
  "The Z.ai monitor endpoint can answer HTTP 200 while its JSON body rejects
   the request. Return that application-level failure without retaining the
   response body or credential."
  [quota]
  (let [success
        (field quota :success)

        code
        (field quota :code)

        status
        (when (number? code) (long code))]

    (when (or (false? success) (and status (<= 400 status 599)))
      {:status status
       :message (some-> (or (field quota :msg) (field quota :message))
                        str
                        str/trim
                        not-empty)})))

(defn- quota-request-error
  [status message]
  (ex-info (str "Z.ai coding plan quota request failed" (when status (str ": HTTP " status)))
           (cond-> {:type :provider/zai-coding-quota-error :url coding-quota-url}
             status
             (assoc :status status)

             message
             (assoc :upstream-message message))))

(defn- fetch-quota!
  [api-key]
  (let [response
        (http/get coding-quota-url
                  {:headers {"Accept" "application/json" "Authorization" (str "Bearer " api-key)}
                   :timeout 30000
                   :throw false})

        status
        (:status response)

        body
        (:body response)]

    (if (and (number? status) (<= 200 status 299))
      (let [quota (json/read-json body :key-fn keyword)]
        (if-let [{:keys [status message]} (application-quota-error quota)]
          (throw (quota-request-error status message))
          quota))
      (throw (quota-request-error status nil)))))

(defn- quota-error-note
  [label status message]
  (str label
       (if (contains? quota-auth-statuses status)
         " rejected the current API key"
         " quota check failed")
       (when message (str ": " (str/replace message #"[.!?]+$" "")))
       "."))

(defn- coding-limits-report!
  [provider-id label api-key]
  (try {:provider-id provider-id
        :status :ok
        :fetched-at-ms (System/currentTimeMillis)
        :dynamic (quota->dynamic-limits (fetch-quota! api-key))}
       (catch Throwable t
         (let [{:keys [status upstream-message]}
               (ex-data t)

               auth-rejected?
               (contains? quota-auth-statuses status)]

           {:provider-id provider-id
            :status (if auth-rejected? :unauthenticated :error)
            :fetched-at-ms (System/currentTimeMillis)
            :dynamic {:limits [] :note (quota-error-note label status upstream-message)}
            :error {:type :provider/zai-coding-quota-error
                    :message (or (ex-message t) "Z.ai coding plan quota check failed")}}))))

(defn- make-limits-fn
  [plan-tag]
  (fn []
    (let [{:keys [provider-id label]}
          (get-in BOOK [:plans plan-tag])

          detected
          (vis/provider-key-detect BOOK plan-tag)]

      (cond (nil? detected) {:provider-id provider-id
                             :status :unauthenticated
                             :fetched-at-ms (System/currentTimeMillis)
                             :dynamic {:limits [] :note (str label " is not authenticated.")}}
            (= :coding plan-tag) (coding-limits-report! provider-id label (:api-key detected))
            :else {:provider-id provider-id
                   :status :unsupported
                   :fetched-at-ms (System/currentTimeMillis)
                   :dynamic {:limits []
                             :note (str label
                                        " does not expose a dynamic quota endpoint yet.")}}))))

;; Provider registration
;;
;; Loading this namespace registers ONE extension entry per plan.
;; `:zai-coding-plan` and `:zai` are independent first-class providers -
;; `vis-agent providers auth zai-coding`, `vis-agent providers status zai`,
;; per-plan logout, etc. all work. The TUI's add-provider picker shows
;; them as two separate cards driven by each provider's preset metadata.

(defn register!
  []
  (vis/register-extension! (vis/extension
                             {:ext/name "provider-zai"
                              :ext/description "Z.ai coding-plan + pass static-API-key providers."
                              :ext/version "0.2.0"
                              :ext/author "Blockether"
                              :ext/owner "vis"
                              :ext/license "Apache-2.0"
                              :ext/providers (vis/provider-key-entries BOOK make-limits-fn)})))
