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
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

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

(def ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofSeconds 15))
      (.followRedirects java.net.http.HttpClient$Redirect/NORMAL)
      (.build))))

(defn- post-form
  "POST application/x-www-form-urlencoded, return parsed JSON map."
  [url params & [extra-headers]]
  (let [body (str/join "&"
               (map (fn [[k v]] (str (java.net.URLEncoder/encode (str k) "UTF-8")
                                  "="
                                  (java.net.URLEncoder/encode (str v) "UTF-8")))
                 params))
        ^java.net.http.HttpRequest$Builder builder
        (-> (HttpRequest/newBuilder)
          (.uri (URI/create url))
          (.timeout (Duration/ofSeconds 30))
          (.header "Accept" "application/json")
          (.header "Content-Type" "application/x-www-form-urlencoded")
          (.POST (HttpRequest$BodyPublishers/ofString body)))
        ^java.net.http.HttpRequest$Builder builder
        (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b k v)) builder
          (merge COPILOT_HEADERS (or extra-headers {})))
        resp    (.send ^java.net.http.HttpClient @http-client
                  (.build builder) (HttpResponse$BodyHandlers/ofString))
        status  (.statusCode resp)]
    (when (<= 200 status 299)
      (json/read-json (.body resp) :key-fn keyword))))

(defn- get-json
  "GET with Bearer auth, return parsed JSON map."
  [url bearer-token]
  (let [^java.net.http.HttpRequest$Builder builder
        (-> (HttpRequest/newBuilder)
          (.uri (URI/create url))
          (.timeout (Duration/ofSeconds 30))
          (.header "Accept" "application/json")
          (.header "Authorization" (str "Bearer " bearer-token))
          (.GET))
        ^java.net.http.HttpRequest$Builder builder
        (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b k v)) builder COPILOT_HEADERS)
        resp    (.send ^java.net.http.HttpClient @http-client
                  (.build builder) (HttpResponse$BodyHandlers/ofString))
        status  (.statusCode resp)]
    (if (<= 200 status 299)
      (json/read-json (.body resp) :key-fn keyword)
      (throw (ex-info (str "GitHub API returned " status)
               {:status status
                :body   (.body resp)
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
        {:oauth-token t :source :auth-file}))
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
  ([device-code interval expires-in {:keys [enterprise-domain]}]
   (let [url       (if enterprise-domain
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

;; Cached Copilot API token. Shape: {:token str :expires-at-ms long :oauth-token str}
(defonce ^:private token-cache (atom nil))

(defn- exchange-for-copilot-token!
  "Exchange an OAuth token for a short-lived Copilot API token.
   Returns {:token str :expires-at-ms long :api-url str}."
  [oauth-token & [{:keys [enterprise-domain]}]]
  (let [url (if enterprise-domain
              (str "https://api." enterprise-domain "/copilot_internal/v2/token")
              COPILOT_TOKEN_URL)
        resp (get-json url oauth-token)]
    (when-not (:token resp)
      (throw (ex-info "Copilot token exchange failed — no token in response"
               {:response resp :url url})))
    {:token        (:token resp)
     :expires-at-ms (* (long (:expires_at resp)) 1000)
     :api-url      (get-in resp [:endpoints :api] "https://api.githubcopilot.com")
     :oauth-token  oauth-token}))

(defn get-copilot-token!
  "Get a valid Copilot API token, refreshing if needed.
   Uses cached token if still valid, otherwise exchanges the OAuth token.
   Returns {:token str :api-url str} or throws.

   Opts:
     :enterprise-domain — for GHE (e.g. \"github.mycompany.com\")"
  ([] (get-copilot-token! nil))
  ([opts]
   (let [cached @token-cache
         now    (System/currentTimeMillis)]
     (if (and cached
           (:token cached)
           (> (:expires-at-ms cached) (+ now REFRESH_MARGIN_MS)))
       ;; Cached token is still valid
       {:token (:token cached) :api-url (:api-url cached)}
       ;; Need to refresh
       (let [oauth-token (or (:oauth-token cached)
                           (:oauth-token (detect-oauth-token))
                           (throw (ex-info "No GitHub Copilot OAuth token found. Run `vis providers auth github-copilot` to authenticate."
                                    {:type :vis/copilot-not-authenticated})))
             fresh (exchange-for-copilot-token! oauth-token opts)]
         (reset! token-cache (assoc fresh :oauth-token oauth-token))
         (tel/log! {:level :info :id ::copilot-token-refreshed
                    :data {:expires-in-ms (- (:expires-at-ms fresh) now)
                           :api-url (:api-url fresh)}
                    :msg "Copilot API token refreshed"})
         {:token (:token fresh) :api-url (:api-url fresh)})))))

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
    :copilot-token-valid? bool :expires-in-ms long}"
  []
  (let [detected (detect-oauth-token)
        cached   @token-cache
        now      (System/currentTimeMillis)]
    (cond-> {:authenticated? (some? detected)}
      detected
      (assoc :source (:source detected)
        :oauth-token-preview (let [t (:oauth-token detected)]
                               (str (subs t 0 (min 8 (count t))) "...")))
      (and cached (:token cached))
      (assoc :copilot-token-valid? (> (:expires-at-ms cached) now)
        :expires-in-ms (- (:expires-at-ms cached) now)))))

(defn logout!
  "Clear all cached and persisted tokens."
  []
  (reset! token-cache nil)
  (delete-auth-file!)
  :logged-out)

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
   the caller controls the output channel (stdout, TUI dialog, …)."
  [printer-fn]
  (let [print! (or printer-fn (constantly nil))]
    (if (detect-oauth-token)
      (do (print! "  Already authenticated with GitHub Copilot.")
        (print! "  Run `vis providers status github-copilot` for details.")
        (print! "  Run `vis providers logout github-copilot` first to re-authenticate.")
        :already-authenticated)
      (let [{:keys [user-code verification-uri device-code interval expires-in]}
            (start-device-flow!)]
        (print! "")
        (print! (str "  1. Open: " verification-uri))
        (print! (str "  2. Enter code: " user-code))
        (print! "")
        (print! "  Waiting for authorization...")
        (poll-for-token! device-code interval expires-in)
        (get-copilot-token!)
        (print! "  ✓ Authenticated! GitHub Copilot is ready.")
        :ok))))

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
       :provider/get-token-fn #'get-copilot-token!}]}))
