(ns com.blockether.vis.ext.provider-github-copilot
  "GitHub Copilot OAuth provider — device flow authentication + token lifecycle.

   Auth flow:
   1. Device flow → user visits github.com/login/device, enters code
   2. Poll until authorized → receive OAuth token (`ghu_...`)
   3. Exchange OAuth token for short-lived Copilot API token
      via `api.github.com/copilot_internal/v2/token`
   4. Auto-refresh the API token before expiry

   Token detection priority (same as Copilot CLI):
   1. Persisted OAuth token in `~/.vis/github-copilot-auth.json`
   2. `COPILOT_GITHUB_TOKEN` env var
   3. `GH_TOKEN` env var
   4. `GITHUB_TOKEN` env var
   5. macOS Keychain (`copilot-cli` service) — if `security` CLI available

   Works with both Individual and Business/Enterprise plans.
   Enterprise users can pass `:enterprise-domain` for GHE."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private CLIENT_ID
  "GitHub App client ID for VS Code Copilot Chat extension.
   This is the public client ID used by all Copilot integrations."
  "Iv1.b507a08c87ecfe98")

(def ^:private DEVICE_CODE_URL  "https://github.com/login/device/code")
(def ^:private ACCESS_TOKEN_URL "https://github.com/login/oauth/access_token")
(def ^:private COPILOT_TOKEN_URL "https://api.github.com/copilot_internal/v2/token")
(def ^:private COPILOT_API_FALLBACK_URL "https://api.individual.githubcopilot.com")

(def ^:private COPILOT_ACCOUNT_BASE_URLS
  {:individual "https://api.individual.githubcopilot.com"
   :business   "https://api.business.githubcopilot.com"
   :enterprise "https://api.enterprise.githubcopilot.com"})

(def ^:private COPILOT_ACCOUNT_TYPES #{:individual :business :enterprise})

(def ^:private COPILOT_HEADERS
  "Required headers for Copilot API calls."
  {"Editor-Version"       "vscode/1.100.0"
   "Editor-Plugin-Version" "copilot-chat/0.26.7"
   "Copilot-Integration-Id" "vscode-chat"
   "User-Agent"           "GitHubCopilotChat/0.26.7"})

(def ^:private AUTH_FILE
  "Persisted OAuth token file."
  (str (System/getProperty "user.home") "/.vis/github-copilot-auth.json"))

(def ^:private REFRESH_MARGIN_MS
  "Refresh the Copilot API token 5 minutes before expiry."
  (* 5 60 1000))

;; =============================================================================
;; HTTP helpers
;; =============================================================================

(defn- json-body
  [body]
  (json/read-json (or body "") :key-fn keyword))

(defn- post-form
  "POST application/x-www-form-urlencoded through babashka.http-client;
   return parsed JSON map on HTTP 2xx, nil otherwise."
  [url params & [extra-headers]]
  (let [body   (str/join "&"
                 (map (fn [[k v]] (str (java.net.URLEncoder/encode (str k) "UTF-8")
                                    "="
                                    (java.net.URLEncoder/encode (str v) "UTF-8")))
                   params))
        resp   (http/post url
                 {:headers (merge COPILOT_HEADERS
                             {"Accept" "application/json"
                              "Content-Type" "application/x-www-form-urlencoded"}
                             (or extra-headers {}))
                  :body    body
                  :timeout 30000
                  :throw   false})
        status (:status resp)]
    (when (<= 200 status 299)
      (json-body (:body resp)))))

(defn- get-json
  "GET with Bearer auth through babashka.http-client, return parsed JSON map."
  [url bearer-token]
  (let [resp   (http/get url
                 {:headers (merge COPILOT_HEADERS
                             {"Accept" "application/json"
                              "Authorization" (str "Bearer " bearer-token)})
                  :timeout 30000
                  :throw   false})
        status (:status resp)]
    (if (<= 200 status 299)
      (json-body (:body resp))
      (throw (ex-info (str "GitHub API returned " status)
               {:status status
                :body   (:body resp)
                :url    url})))))

;; =============================================================================
;; Token persistence
;; =============================================================================

(defn- load-auth-file
  "Load persisted auth state from ~/.vis/github-copilot-auth.json.
   Returns map or nil."
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f)
      (try
        (json/read-json (slurp f) :key-fn keyword)
        (catch Exception _ nil)))))

(defn- save-auth-file!
  "Persist auth state to ~/.vis/github-copilot-auth.json."
  [auth-state]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit AUTH_FILE (json/write-json-str auth-state))))

(defn- normalize-account-type [value]
  (let [raw (cond
              (keyword? value) (name value)
              (string? value)  value
              :else nil)]
    (when-let [s (some-> raw str/lower-case str/trim not-empty)]
      (let [account-type (keyword s)]
        (when (contains? COPILOT_ACCOUNT_TYPES account-type)
          account-type)))))

(defn- env-account-type []
  (or (normalize-account-type (System/getenv "VIS_GITHUB_COPILOT_ACCOUNT_TYPE"))
    (normalize-account-type (System/getenv "GITHUB_COPILOT_ACCOUNT_TYPE"))))

(defn- auth-account-type []
  (normalize-account-type (:account-type (load-auth-file))))

(defn- configured-account-type [opts]
  (or (normalize-account-type (:account-type opts))
    (env-account-type)
    (auth-account-type)
    :individual))

(defn- delete-auth-file!
  "Remove persisted auth state."
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f) (.delete f))))

;; =============================================================================
;; Token detection — env vars, keychain, persisted file
;; =============================================================================

(defn- env-token
  "Check env vars in Copilot CLI priority order."
  []
  (or (System/getenv "COPILOT_GITHUB_TOKEN")
    (System/getenv "GH_TOKEN")
    (System/getenv "GITHUB_TOKEN")))

(defn- keychain-token
  "Try to read OAuth token from macOS Keychain (copilot-cli service).
   Returns token string or nil. Silent on non-macOS or missing entry."
  []
  (when (= "Mac OS X" (System/getProperty "os.name"))
    (try
      (let [proc (-> (ProcessBuilder. ["security" "find-generic-password"
                                       "-s" "copilot-cli"
                                       "-a" "github.com"
                                       "-w"])
                   (.redirectErrorStream true)
                   (.start))
            out  (str/trim (slurp (.getInputStream proc)))
            ok   (.waitFor proc 5 java.util.concurrent.TimeUnit/SECONDS)]
        (when (and ok (zero? (.exitValue proc)) (not (str/blank? out)))
          out))
      (catch Exception _ nil))))

(defn detect-oauth-token
  "Detect an existing OAuth token from all known sources.
   Returns {:oauth-token str :source keyword} or nil."
  []
  (or
    ;; 1. Persisted auth file
    (when-let [auth (load-auth-file)]
      (when-let [t (:oauth-token auth)]
        (cond-> {:oauth-token t :source :auth-file}
          (normalize-account-type (:account-type auth))
          (assoc :account-type (normalize-account-type (:account-type auth))))))
    ;; 2. Env vars
    (when-let [t (env-token)]
      {:oauth-token t :source :env-var})
    ;; 3. macOS Keychain
    (when-let [t (keychain-token)]
      {:oauth-token t :source :keychain})))

;; =============================================================================
;; OAuth Device Flow
;; =============================================================================

(defn start-device-flow!
  "Start the GitHub OAuth device flow.
   Returns {:user-code :verification-uri :device-code :interval :expires-in}.
   The caller must display user-code and verification-uri to the user."
  ([] (start-device-flow! nil))
  ([{:keys [enterprise-domain]}]
   (let [url (if enterprise-domain
               (str "https://" enterprise-domain "/login/device/code")
               DEVICE_CODE_URL)
         resp (post-form url {"client_id" CLIENT_ID
                              "scope"     "read:user"})]
     (when-not resp
       (throw (ex-info "Failed to start device flow — no response from GitHub"
                {:url url})))
     {:user-code        (:user_code resp)
      :verification-uri (:verification_uri resp)
      :device-code      (:device_code resp)
      :interval         (or (:interval resp) 5)
      :expires-in       (or (:expires_in resp) 900)})))

(defn poll-for-token!
  "Poll GitHub for the OAuth access token after user has authorized.
   Blocks until authorized, denied, or expired.
   Returns {:oauth-token str} on success, throws on failure.

   `device-code` — from start-device-flow!
   `interval`    — poll interval in seconds (from start-device-flow!)
   `expires-in`  — max wait in seconds"
  ([device-code interval] (poll-for-token! device-code interval 900 nil))
  ([device-code interval expires-in] (poll-for-token! device-code interval expires-in nil))
  ([device-code interval expires-in {:keys [enterprise-domain] :as opts}]
   (let [account-type (configured-account-type opts)
         url       (if enterprise-domain
                     (str "https://" enterprise-domain "/login/oauth/access_token")
                     ACCESS_TOKEN_URL)
         deadline  (+ (System/currentTimeMillis) (* (long (or expires-in 900)) 1000))
         interval-ms (long (* (max 5 (long (or interval 5))) 1000))]
     (loop []
       (when (> (System/currentTimeMillis) deadline)
         (throw (ex-info "Device flow expired — user did not authorize in time" {})))
       (Thread/sleep interval-ms)
       (let [resp (post-form url {"client_id"  CLIENT_ID
                                  "device_code" device-code
                                  "grant_type" "urn:ietf:params:oauth:grant-type:device_code"})]
         (cond
           (:access_token resp)
           (let [oauth-token (:access_token resp)]
             (save-auth-file! {:oauth-token      oauth-token
                               :refresh-token    (:refresh_token resp)
                               :account-type     (name account-type)
                               :created-at       (System/currentTimeMillis)})
             {:oauth-token oauth-token})

           (= "authorization_pending" (:error resp))
           (recur)

           (= "slow_down" (:error resp))
           (do (Thread/sleep 5000) (recur))

           :else
           (throw (ex-info (str "Device flow failed: " (or (:error_description resp)
                                                         (:error resp)
                                                         "unknown error"))
                    {:response resp}))))))))

;; =============================================================================
;; Copilot API Token Exchange + Refresh
;; =============================================================================

(def ^:private COPILOT_POLICY_MODELS
  ["claude-haiku-4.5" "claude-sonnet-4" "claude-sonnet-4.5" "claude-sonnet-4.6"
   "claude-opus-4.5" "claude-opus-4.6" "claude-opus-4.7"
   "gpt-5" "gpt-5-mini" "gpt-5.1" "gpt-5.1-codex" "gpt-5.1-codex-max"
   "gpt-5.1-codex-mini" "gpt-5.2" "gpt-5.2-codex" "gpt-5.3-codex"
   "gpt-5.4" "gpt-5.4-mini"
   "gpt-4.1" "gpt-4o" "gemini-2.5-pro" "gemini-3-flash-preview"
   "gemini-3-pro-preview" "gemini-3.1-pro-preview" "grok-code-fast-1"])

(defn- valid-copilot-host? [host]
  (and (string? host)
    (re-matches #"(?i)[a-z0-9][a-z0-9.-]*\.githubcopilot\.com" host)
    (not (str/includes? host "/"))
    (not (str/includes? host "@"))))

(defn- host-url [host]
  (when (valid-copilot-host? host)
    (str "https://" host)))

(defn- response-field [m k]
  (or (get m k) (get m (name k))))

(defn- endpoint-api-url [response]
  (or (get-in response [:endpoints :api])
    (get-in response ["endpoints" "api"])))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- proxy-endpoint [response]
  (or (response-field response :proxy-ep)
    (response-field response :proxy_ep)
    (response-field response :proxyEndpoint)))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- copilot-base-url-from-token [token]
  (when-let [[_ proxy-host] (re-find #"(?:^|;)proxy-ep=([^;]+)" (or token ""))]
    (host-url proxy-host)))

(defn- copilot-api-base-url
  ([token response enterprise-domain]
   (copilot-api-base-url token response enterprise-domain nil))
  ([_token response enterprise-domain opts]
   (let [account-type (configured-account-type opts)]
     ;; Token `proxy-ep` is NES/inline-completion traffic. Chat models use
     ;; account API host; `/messages` and proxy chat return 404/model-missing.
     (or (endpoint-api-url response)
       (when-not (str/blank? enterprise-domain)
         (str "https://copilot-api." enterprise-domain))
       (get COPILOT_ACCOUNT_BASE_URLS account-type)
       COPILOT_API_FALLBACK_URL))))

(defn- enable-copilot-model! [token api-url model-id]
  (try
    (let [resp (http/post (str api-url "/models/" model-id "/policy")
                 {:headers (merge COPILOT_HEADERS
                             {"Content-Type" "application/json"
                              "Authorization" (str "Bearer " token)
                              "openai-intent" "chat-policy"
                              "x-interaction-type" "chat-policy"})
                  :body    "{\"state\":\"enabled\"}"
                  :timeout 30000
                  :throw   false})]
      (<= 200 (:status resp) 299))
    (catch Throwable _ false)))

(defn- enable-known-copilot-models! [token api-url]
  (let [results (doall (pmap #(enable-copilot-model! token api-url %) COPILOT_POLICY_MODELS))]
    {:attempted (count COPILOT_POLICY_MODELS)
     :enabled   (count (filter true? results))}))

;; Cached Copilot API token. Shape: {:token str :expires-at-ms long :oauth-token str}
(defonce ^:private token-cache (atom nil))

(defn- exchange-for-copilot-token!
  "Exchange an OAuth token for a short-lived Copilot API token.
   Returns {:token str :expires-at-ms long :api-url str}."
  [oauth-token & [{:keys [enterprise-domain] :as opts}]]
  (let [account-type (configured-account-type opts)
        url (if enterprise-domain
              (str "https://api." enterprise-domain "/copilot_internal/v2/token")
              COPILOT_TOKEN_URL)
        resp (get-json url oauth-token)]
    (when-not (:token resp)
      (throw (ex-info "Copilot token exchange failed — no token in response"
               {:response resp :url url})))
    (let [token (:token resp)]
      {:token        token
       :expires-at-ms (* (long (:expires_at resp)) 1000)
       :api-url      (copilot-api-base-url token resp enterprise-domain {:account-type account-type})
       :account-type account-type
       :sku          (or (response-field resp :sku) (response-field resp :access_type_sku))
       :oauth-token  oauth-token})))

(defn get-copilot-token!
  "Get a valid Copilot API token, refreshing if needed.
   Uses cached token if still valid, otherwise exchanges the OAuth token.
   Returns {:token str :api-url str :llm-headers map} or throws.

   Opts:
     :enterprise-domain — for GHE (e.g. \"github.mycompany.com\")"
  ([] (get-copilot-token! nil))
  ([opts]
   (let [account-type (configured-account-type opts)
         cached @token-cache
         now    (System/currentTimeMillis)]
     (if (and cached
           (:token cached)
           (> (:expires-at-ms cached) (+ now REFRESH_MARGIN_MS))
           (= account-type (or (normalize-account-type (:account-type cached)) :individual)))
       ;; Cached token is still valid. Recompute chat API host so older
       ;; caches carrying token `proxy-ep` do not keep routing chat there.
       {:token (:token cached)
        :api-url (copilot-api-base-url (:token cached) {} nil {:account-type account-type})
        :account-type account-type
        :llm-headers COPILOT_HEADERS}
       ;; Need to refresh
       (let [oauth-token (or (:oauth-token cached)
                           (:oauth-token (detect-oauth-token))
                           (throw (ex-info "No GitHub Copilot OAuth token found. Run `vis providers auth github-copilot` to authenticate."
                                    {:type :vis/copilot-not-authenticated})))
             fresh (exchange-for-copilot-token! oauth-token (assoc opts :account-type account-type))]
         (reset! token-cache (assoc fresh :oauth-token oauth-token :account-type account-type))
         (tel/log! {:level :info :id ::copilot-token-refreshed
                    :data {:expires-in-ms (- (:expires-at-ms fresh) now)
                           :api-url (:api-url fresh)
                           :account-type account-type}
                    :msg "Copilot API token refreshed"})
         {:token (:token fresh) :api-url (:api-url fresh) :account-type account-type :llm-headers COPILOT_HEADERS})))))

;; =============================================================================
;; CLI helpers
;; =============================================================================

(defn authenticated?
  "True if we have a usable OAuth token from any source."
  []
  (some? (detect-oauth-token)))

(defn status
  "Return a status map describing the current auth state.
   {:authenticated? bool :source keyword :oauth-token-preview str
    :account-type keyword :copilot-token-valid? bool :expires-in-ms long}"
  []
  (let [detected (detect-oauth-token)
        cached   @token-cache
        now      (System/currentTimeMillis)]
    (cond-> {:authenticated? (some? detected)
             :account-type (configured-account-type nil)}
      detected
      (assoc :source (:source detected)
        :oauth-token-preview (let [t (:oauth-token detected)]
                               (str (subs t 0 (min 8 (count t))) "...")))
      (and cached (:token cached))
      (assoc :copilot-token-valid? (> (:expires-at-ms cached) now)
        :expires-in-ms (- (:expires-at-ms cached) now)
        :api-url (:api-url cached)))))

(defn logout!
  "Clear all cached and persisted tokens."
  []
  (reset! token-cache nil)
  (delete-auth-file!)
  :logged-out)

;; =============================================================================
;; Dynamic quota / limits
;; =============================================================================

(defn- parse-epoch-ms [value]
  (cond
    (number? value)
    (long (if (> (double value) 100000000000.0)
            value
            (* 1000.0 (double value))))

    (string? value)
    (try
      (.toEpochMilli (java.time.Instant/parse value))
      (catch Throwable _
        (try
          (.toEpochMilli (.toInstant (.atStartOfDay (java.time.LocalDate/parse value) java.time.ZoneOffset/UTC)))
          (catch Throwable _ nil))))

    :else nil))

(defn- quota-row [reset-ms [quota-key quota]]
  (let [id        (keyword (name quota-key))
        label     (-> (name quota-key) (str/replace #"[_-]" " ") str/capitalize)
        remaining (response-field quota :remaining)
        limit     (or (response-field quota :entitlement)
                    (response-field quota :limit))
        pct       (response-field quota :percent_remaining)]
    (cond-> {:id         id
             :label      label
             :scope      :account
             :kind       :requests
             :precision  :exact
             :source     :provider-api
             :unlimited? false}
      (number? remaining) (assoc :remaining (double remaining))
      (number? limit)     (assoc :limit (double limit))
      (and (number? remaining) (number? limit))
      (assoc :used (double (- (double limit) (double remaining))))
      (number? pct)       (assoc :note (String/format java.util.Locale/ROOT "%.1f%% remaining" (object-array [(double pct)])))
      reset-ms            (assoc :window {:kind :calendar
                                          :unit :month
                                          :resets-at-ms reset-ms}))))

(defn- quota-map-rows [reset-ms quotas]
  (if (map? quotas)
    (mapv #(quota-row reset-ms %) quotas)
    []))

(defn- limited-quota-rows [reset-ms usage]
  (let [remaining (response-field usage :limited_user_quotas)
        monthly   (response-field usage :monthly_quotas)]
    (if (and (map? remaining) (map? monthly))
      (mapv (fn [[k rem]]
              (quota-row reset-ms [k {:remaining rem :entitlement (get monthly k)}]))
        remaining)
      [])))

(defn- fetch-user-usage! [oauth-token]
  (let [resp   (http/get "https://api.github.com/copilot_internal/user"
                 {:headers (merge COPILOT_HEADERS
                             {"Accept" "application/json"
                              "Authorization" (str "Bearer " oauth-token)
                              "X-GitHub-Api-Version" "2025-04-01"})
                  :timeout 30000
                  :throw   false})
        status (:status resp)
        body   (:body resp)]
    (if (<= 200 status 299)
      (json-body body)
      (throw (ex-info (str "GitHub Copilot usage request failed: HTTP " status)
               {:type :provider/github-copilot-usage-error
                :status status
                :body body})))))

(defn- dynamic-limits!
  []
  (if-let [{:keys [oauth-token]} (detect-oauth-token)]
    (let [usage    (fetch-user-usage! oauth-token)
          reset-ms (or (parse-epoch-ms (response-field usage :quota_reset_date))
                     (parse-epoch-ms (response-field usage :limited_user_reset_date)))
          rows     (let [snapshots (response-field usage :quota_snapshots)]
                     (if (seq snapshots)
                       (quota-map-rows reset-ms snapshots)
                       (limited-quota-rows reset-ms usage)))]
      {:status :ok
       :dynamic {:limits rows
                 :note (str "Copilot plan: " (or (response-field usage :copilot_plan)
                                               (response-field usage :access_type_sku)
                                               "unknown"))}})
    {:status :unauthenticated
     :dynamic {:limits []
               :note "Authenticate GitHub Copilot to fetch dynamic quota data."}}))

;; =============================================================================
;; Provider registration
;;
;; Loading this namespace plugs the GitHub Copilot provider into the
;; generic registry. The CLI's `vis providers` tree lists every registered
;; provider; the runtime's token-resolution path looks providers up
;; by id. Drop this jar (or pick another `vis-provider-*` package)
;; to swap providers without touching vis-runtime.
;; =============================================================================

(require '[com.blockether.vis.core :as vis])

(defn- interactive-auth!
  "Wrap the multi-step device flow into one fn the CLI / TUI can
   call uniformly. `printer-fn` is invoked for every status line so
   the caller controls the output channel (stdout, TUI dialog, …).
   Opts may include `:account-type` = :individual, :business, or :enterprise."
  ([printer-fn] (interactive-auth! printer-fn nil))
  ([printer-fn opts]
   (let [print! (or printer-fn (constantly nil))
         account-type (configured-account-type opts)
         opts (assoc opts :account-type account-type)]
     (if (detect-oauth-token)
       (do (print! "  Already authenticated with GitHub Copilot.")
         (print! (str "  Account type: " (name account-type)))
         (print! "  Run `vis providers status github-copilot` for details.")
         (print! "  Run `vis providers logout github-copilot` first to re-authenticate.")
         :already-authenticated)
       (let [{:keys [user-code verification-uri device-code interval expires-in]}
             (start-device-flow! opts)]
         (print! "")
         (print! (str "  Account type: " (name account-type)))
         (print! (str "  1. Open: " verification-uri))
         (print! (str "  2. Enter code: " user-code))
         (print! "")
         (print! "  Waiting for authorization...")
         (poll-for-token! device-code interval expires-in opts)
         (let [{:keys [token api-url]} (get-copilot-token! opts)
               {:keys [attempted enabled]} (enable-known-copilot-models! token api-url)]
           (print! (str "  Enabled " enabled "/" attempted " known Copilot model policies.")))
         (print! "  ✓ Authenticated! GitHub Copilot is ready.")
         :ok)))))

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-github-copilot
     :ext/doc       "GitHub Copilot OAuth + token-exchange provider."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers
     [{:provider/id           :github-copilot
       :provider/label        "GitHub Copilot"
       :provider/status-fn    #'status
       :provider/logout-fn    #'logout!
       :provider/detect-fn    #'detect-oauth-token
       :provider/auth-fn      #'interactive-auth!
       :provider/get-token-fn #'get-copilot-token!
       :provider/limits-fn    #'dynamic-limits!}]}))
