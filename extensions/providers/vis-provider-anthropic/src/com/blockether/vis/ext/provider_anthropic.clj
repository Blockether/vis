(ns com.blockether.vis.ext.provider-anthropic
  "Anthropic providers.

   Providers:
   - `:anthropic` - normal Anthropic API key provider. API key lives in Vis config.
   - `:anthropic-coding-plan` - Claude subscription OAuth provider. OAuth
     credentials live in `~/.vis/anthropic-auth.json`.

   Runtime calls hand the OAuth access token to svar; svar handles only the
   Anthropic Messages API wire differences for subscription tokens."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.oauth :as oauth])
  (:import [java.net URLDecoder URLEncoder]
           [java.security MessageDigest SecureRandom]
           [java.time Instant]
           [java.util Base64]))

(def ^:private client-id
  (String. (.decode (Base64/getDecoder) "OWQxYzI1MGEtZTYxYi00NGQ5LTg4ZWQtNTk0NGQxOTYyZjVl")
           java.nio.charset.StandardCharsets/UTF_8))

(def ^:private authorize-url "https://claude.ai/oauth/authorize")
(def ^:private token-url "https://platform.claude.com/v1/oauth/token")
(def ^:private usage-url "https://api.anthropic.com/api/oauth/usage")
(def ^:private redirect-uri "http://localhost:53692/callback")
(def ^:private scopes
  "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload")
(def ^:private auth-file (str (System/getProperty "user.home") "/.vis/anthropic-auth.json"))
;; base-url + default-models come from svar (single source of truth).
(def ^:private ^:const refresh-margin-ms 300000)
(def ^:private secure-random (delay (SecureRandom.)))

(defn- random-bytes
  [n]
  (let [bytes (byte-array n)]
    (.nextBytes ^SecureRandom @secure-random bytes)
    bytes))

(defn- base64url [^bytes bytes] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn- sha256
  [^String s]
  (.digest (MessageDigest/getInstance "SHA-256")
           (.getBytes s java.nio.charset.StandardCharsets/UTF_8)))

(defn- url-encode [v] (URLEncoder/encode (str v) "UTF-8"))

(defn- url-decode [^String v] (URLDecoder/decode v "UTF-8"))

(defn- query-string
  [params]
  (str/join "&"
            (map (fn [[k v]]
                   (str (url-encode (name k)) "=" (url-encode v)))
                 params)))

(defn- parse-query-string
  [^String qs]
  (when-not (str/blank? qs)
    (into {}
          (keep (fn [part]
                  (let [[k v] (str/split part #"=" 2)]
                    (when-not (str/blank? k) [(keyword (url-decode k)) (url-decode (or v ""))]))))
          (str/split qs #"&"))))

(defn parse-authorization-input
  "Parse a pasted Anthropic OAuth callback URL, query string, `code#state`, or bare code."
  [input]
  (let [value (str/trim (or input ""))]
    (cond (str/blank? value) {}
          (str/starts-with? value "http")
          (try (let
                 [[head fragment] (str/split value #"#" 2)
                  q-idx (.indexOf ^String head "?")
                  query (when (<= 0 q-idx) (subs head (inc q-idx)))
                  params (merge (parse-query-string query)
                                (if (and fragment (str/includes? fragment "="))
                                  (parse-query-string fragment)
                                  (when fragment {:state fragment})))]

                 (select-keys params [:code :state]))
               (catch Exception _ {:code value}))
          (str/includes? value "#") (let [[code state] (str/split value #"#" 2)]
                                      (cond-> {:code code}
                                        (not (str/blank? state))
                                        (assoc :state state)))
          (str/includes? value "code=") (select-keys (parse-query-string value) [:code :state])
          :else {:code value})))

(defn create-authorization-flow
  "Create Anthropic Claude subscription OAuth PKCE flow data."
  []
  (let
    [verifier
     (base64url (random-bytes 32))

     challenge
     (base64url (sha256 verifier))

     query
     (query-string {:code "true"
                    :client_id client-id
                    :response_type "code"
                    :redirect_uri redirect-uri
                    :scope scopes
                    :code_challenge challenge
                    :code_challenge_method "S256"
                    :state verifier})]

    {:verifier verifier :state verifier :url (str authorize-url "?" query)}))

(defn- read-json-body
  [text]
  (try (json/read-json (or text "") :key-fn keyword) (catch Exception _ nil)))

(defn- post-token
  [body]
  (let
    [resp
     (http/post token-url
                {:headers {"Accept" "application/json" "Content-Type" "application/json"}
                 :body (json/write-json-str body)
                 :timeout 30000
                 :throw false})

     text
     (:body resp)]

    {:status (:status resp) :body text :json (read-json-body text)}))

(defn- credentials-result
  [json]
  (let
    [access-token
     (:access_token json)

     refresh-token
     (:refresh_token json)

     expires-in
     (:expires_in json)]

    (when (or (str/blank? access-token) (str/blank? refresh-token) (not (number? expires-in)))
      (throw (ex-info "Anthropic token response missing required fields"
                      {:response (dissoc json :access_token :refresh_token)})))
    {:access-token access-token
     :refresh-token refresh-token
     :expires-at-ms (- (+ (System/currentTimeMillis) (* (long expires-in) 1000))
                       refresh-margin-ms)}))

(defn- exchange-authorization-code!
  [code {:keys [verifier state]} returned-state]
  (when (str/blank? code)
    (throw (ex-info "Missing Anthropic authorization code" {:type :vis/anthropic-missing-code})))
  (when (str/blank? verifier)
    (throw (ex-info "Missing Anthropic PKCE verifier" {:type :vis/anthropic-missing-verifier})))
  (when (and (not (str/blank? returned-state)) (not= state returned-state))
    (throw (ex-info "Anthropic OAuth state mismatch"
                    {:type :vis/anthropic-state-mismatch :expected state :actual returned-state})))
  (let
    [{:keys [status body json]} (post-token {:grant_type "authorization_code"
                                             :client_id client-id
                                             :code code
                                             :state (or returned-state state verifier)
                                             :redirect_uri redirect-uri
                                             :code_verifier verifier})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "Anthropic token exchange failed: HTTP " status)
                      {:type :vis/anthropic-token-exchange-failed :status status :body body})))
    (credentials-result json)))

(defn- refresh-access-token!
  [refresh-token]
  (let
    [{:keys [status body json]}
     (post-token {:grant_type "refresh_token" :client_id client-id :refresh_token refresh-token})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "Anthropic token refresh failed: HTTP " status)
                      {:type :vis/anthropic-token-refresh-failed :status status :body body})))
    (credentials-result json)))

(defn- auth-dir [] (io/file (str (System/getProperty "user.home") "/.vis")))

(defn- load-auth-file
  []
  (let [f (io/file auth-file)]
    (when (.exists f) (try (json/read-json (slurp f) :key-fn keyword) (catch Exception _ nil)))))

(defn- save-auth-file!
  [credentials]
  (let [^java.io.File dir (auth-dir)]
    (when-not (.exists dir) (.mkdirs dir))
    (spit auth-file
          (json/write-json-str (assoc credentials :saved-at-ms (System/currentTimeMillis))))
    credentials))

(defn- delete-auth-file!
  []
  (let [f (io/file auth-file)]
    (when (.exists f) (.delete f))))

(defn detect-credentials
  "Detect persisted Anthropic OAuth credentials without network validation."
  []
  (when-let [auth (load-auth-file)]
    (when-not (str/blank? (:access-token auth))
      {:access-token (:access-token auth)
       :source :auth-file
       :expires-at-ms (:expires-at-ms auth)})))

(def ^:private refresh-and-persist!
  "Single-flight refresh for the rotating Anthropic refresh_token (see
   `internal.oauth/make-file-refresher`): serialized per credential file,
   reuses a just-persisted token instead of racing another exchange into
   HTTP 400. Returns `{:token <access-token>}`."
  (oauth/make-file-refresher
    {:load load-auth-file
     :saved-at :saved-at-ms
     :refresh-token :refresh-token
     :exchange! refresh-access-token!
     :persist! save-auth-file!
     :->token (fn [auth]
                {:token (:access-token auth)})
     :no-token!
     #(throw
        (ex-info
          "No Anthropic refresh token on file. Run `vis providers auth anthropic-coding-plan` to re-authenticate."
          {:type :vis/anthropic-not-authenticated}))}))

(defn get-anthropic-token!
  "Return a fresh Anthropic Claude subscription access token for Vis runtime."
  []
  (let
    [auth
     (load-auth-file)

     now
     (System/currentTimeMillis)]

    (if (and (:access-token auth)
             (:expires-at-ms auth)
             (> (long (:expires-at-ms auth)) (+ now refresh-margin-ms)))
      {:token (:access-token auth)}
      ;; Locally expired (or no token): refresh under the single-flight lock
      ;; so concurrent callers don't each run a rotating exchange.
      (refresh-and-persist!))))

(defn force-refresh-token!
  "Force an OAuth refresh-token exchange, persist the rotated credentials,
   and return `{:token <new-access-token>}`.

   `get-anthropic-token!` only refreshes when the stored token is locally
   expired, so a token that is locally-valid but has been invalidated
   server-side (refresh-token rotation by another client/process) would
   otherwise never be replaced. The runtime's 401 recovery path calls this
   to break that deadlock. Goes through the single-flight
   `refresh-and-persist!`, so a STORM of 401s (every iteration's retry, the
   usage poll) collapses into one exchange instead of racing the rotating
   refresh token into HTTP 400.

   `rejected-token` (optional) is the access token the server just 401'd:
   the single-flight reuse step will NOT hand it back, forcing a real
   exchange when the on-file token is still the dead one. Throws when there
   is no refresh token."
  ([] (force-refresh-token! nil))
  ([rejected-token] (refresh-and-persist! rejected-token)))

(defn- parse-instant-ms
  [s]
  (when-not (str/blank? (str s))
    (try (.toEpochMilli (Instant/parse (str s))) (catch Exception _ nil))))

(defn- usage-field
  [m k]
  (when (map? m)
    (let
      [snake
       (name k)

       camel
       (str/replace snake #"_([a-zA-Z])" #(str/upper-case (second %)))

       kebab
       (str/replace snake #"_" "-")]

      (reduce (fn [_ k*]
                (when (contains? m k*) (reduced (get m k*))))
              nil
              [k snake (keyword camel) camel (keyword kebab) kebab]))))

(defn- clamp-percent
  [n]
  (when (number? n)
    (-> (double n)
        (max 0.0)
        (min 100.0))))

(defn- percentage-limit-row
  [id label window-unit window-size usage]
  (when-let [used (clamp-percent (usage-field usage :utilization))]
    (cond->
      {:id id
       :label label
       :scope :plan
       :kind :rate
       :precision :estimate
       :source :provider-api
       :unlimited? false
       :used used
       :limit 100.0
       :remaining (- 100.0 (double used))
       :window {:kind :rolling :unit window-unit :size window-size}}
      (usage-field usage :resets_at)
      (assoc-in [:window :resets-at-ms] (parse-instant-ms (usage-field usage :resets_at))))))

(defn- usage-limit-rows
  [usage]
  (->> [(percentage-limit-row :claude-5h "Claude 5h" :hour 5 (usage-field usage :five_hour))
        (percentage-limit-row :claude-7d "Claude 7d" :day 7 (usage-field usage :seven_day))
        (percentage-limit-row :claude-sonnet-7d
                              "Claude Sonnet 7d" :day
                              7 (usage-field usage :seven_day_sonnet))
        (percentage-limit-row :claude-opus-7d
                              "Claude Opus 7d" :day
                              7 (usage-field usage :seven_day_opus))]
       (remove nil?)
       vec))

(defn- fetch-usage!
  [token]
  (let
    [resp
     (http/get usage-url
               {:headers {"Accept" "application/json"
                          "Authorization" (str "Bearer " token)
                          "anthropic-beta" "oauth-2025-04-20"
                          "Content-Type" "application/json"}
                :timeout 10000
                :throw false})

     text
     (:body resp)]

    {:status (:status resp) :body text :json (read-json-body text)}))

(defn- limits-error-report
  [type message data]
  {:provider-id :anthropic-coding-plan
   :status :error
   :dynamic {:limits []}
   :error {:type type :message message :data data}})

(def ^:private limits-success-cache-ms (* 5 60 1000))

(def ^:private limits-transient-error-cache-ms (* 10 60 1000))

(def ^:private limits-error-cache-ms (* 2 60 1000))

(defonce ^:private limits-cache (atom nil))

(defonce ^:private limits-lock (Object.))

(defn clear-limits-cache!
  "Clear the in-process Anthropic usage cache. Intended for tests and
   forced provider refreshes; normal callers should use `limits`."
  []
  (reset! limits-cache nil))

(defn- transient-usage-status? [status] (contains? #{409 429} status))

(defn- transient-limits-report?
  [report]
  (and (= :error (:status report))
       (transient-usage-status? (get-in report [:error :data :status]))))

(defn- cached-limits-report
  [now-ms]
  (let [{:keys [report expires-at-ms]} @limits-cache]
    (when (and report expires-at-ms (< (long now-ms) (long expires-at-ms))) report)))

(defn- annotate-stale-limits-report
  [report status]
  (update report
          :dynamic assoc
          :note (str
                  "Using cached Claude subscription usage; Anthropic usage endpoint returned HTTP "
                  status
                  ", so Vis is backing off.")))

(defn- limits-cache-ttl-ms
  [report transient?]
  (cond transient? limits-transient-error-cache-ms
        (= :ok (:status report)) limits-success-cache-ms
        :else limits-error-cache-ms))

(defn- remember-limits-report!
  [report now-ms]
  (let
    [prev
     @limits-cache

     transient?
     (transient-limits-report? report)

     stale-ok
     (:stale-ok-report prev)

     report*
     (if (and transient? stale-ok)
       (annotate-stale-limits-report stale-ok (get-in report [:error :data :status]))
       report)

     ttl-ms
     (limits-cache-ttl-ms report* transient?)

     stale-ok*
     (if (= :ok (:status report*)) report* stale-ok)]

    (reset! limits-cache {:report report*
                          :expires-at-ms (+ (long now-ms) (long ttl-ms))
                          :stale-ok-report stale-ok*})
    report*))

(defn- usage-throttle-report
  [status]
  (limits-error-report
    :vis/anthropic-usage-rate-limited
    (str "Anthropic usage endpoint temporarily rejected the limits check (HTTP " status ").")
    {:status status}))

(defn- fetch-limits-report
  []
  (try
    (let
      [{:keys [token]}
       (get-anthropic-token!)

       {:keys [status body json]}
       (fetch-usage! token)]

      (cond
        (= 200 status)
        (let [rows (usage-limit-rows json)]
          (if (seq rows)
            {:provider-id :anthropic-coding-plan
             :status :ok
             :dynamic {:limits rows
                       :note "Live Claude subscription usage from Anthropic OAuth usage API."}}
            {:provider-id :anthropic-coding-plan
             :status :unsupported
             :dynamic {:limits []
                       :note
                       "Anthropic usage endpoint returned no Claude subscription limit fields."}}))
        (contains? #{401 403} status)
        {:provider-id :anthropic-coding-plan
         :status :unauthenticated
         :dynamic
         {:limits []
          :note
          "Anthropic OAuth token was rejected. Run `vis providers auth anthropic-coding-plan --force` to re-authenticate."}}
        (transient-usage-status? status) (usage-throttle-report status)
        :else (limits-error-report :vis/anthropic-usage-failed
                                   (str "Anthropic usage endpoint failed: HTTP " status)
                                   {:status status :body body})))
    (catch clojure.lang.ExceptionInfo e
      (if (= :vis/anthropic-not-authenticated (:type (ex-data e)))
        {:provider-id :anthropic-coding-plan
         :status :unauthenticated
         :dynamic
         {:limits []
          :note
          "Run `vis providers auth anthropic-coding-plan` to authenticate with Claude subscription."}}
        (limits-error-report :vis/anthropic-limits-error
                             (or (ex-message e) "Anthropic limits check failed")
                             (dissoc (ex-data e) :access-token :refresh-token :token))))
    (catch Throwable t
      (limits-error-report :vis/anthropic-limits-error
                           (or (ex-message t) (.getName (class t)))
                           {:class (.getName (class t))}))))

(defn- limits
  []
  (let [now-ms (System/currentTimeMillis)]
    (or (cached-limits-report now-ms)
        (locking limits-lock
          (let [now-ms (System/currentTimeMillis)]
            (or (cached-limits-report now-ms)
                (remember-limits-report! (fetch-limits-report) now-ms)))))))

(defn authenticated? [] (some? (detect-credentials)))

(defn status
  []
  (let
    [detected
     (detect-credentials)

     now
     (System/currentTimeMillis)]

    (cond-> {:authenticated? (some? detected)}
      detected
      (assoc :source
        (:source detected) :oauth-token-preview
        (let [t (:access-token detected)]
          (str (subs t 0 (min 8 (count t))) "...")))

      (:expires-at-ms detected)
      (assoc :expires-in-ms (- (long (:expires-at-ms detected)) now)))))

(defn logout! [] (delete-auth-file!) :logged-out)

(defn auth-instruction-lines
  []
  ["Anthropic Claude subscription OAuth." "" "CLI OAuth command:"
   "  vis providers auth anthropic-coding-plan"])

(defn- open-browser! [url] (= :ok (:status (opener/open! url))))

(defn- prompt-for-code!
  [printer-fn]
  (printer-fn "")
  (printer-fn "  Paste the authorization code or full redirect URL, then press Enter:")
  (read-line))

(defn login!
  "Run Anthropic Claude subscription OAuth and persist credentials.

   Options:
   - `:open-browser-fn` `(fn [url] boolean)` override for tests/frontends.
   - `:manual-code-fn` `(fn [printer-fn] string|nil)` collector.
   - `:force?` starts fresh even if credentials already exist."
  ([printer-fn] (login! printer-fn {}))
  ([printer-fn
    {:keys [open-browser-fn manual-code-fn force?]
     :or {open-browser-fn open-browser! manual-code-fn prompt-for-code!}}]
   (let [print! (or printer-fn (constantly nil))]
     (if (and (not force?) (detect-credentials))
       (do (print! "  Already authenticated with Anthropic Claude subscription.")
           (print! "  Run `vis providers status anthropic-coding-plan` for details.")
           (print! "  Run `vis providers logout anthropic-coding-plan` first to re-authenticate.")
           :already-authenticated)
       (let [{:keys [url] :as flow} (create-authorization-flow)]
         (print! "")
         (print! "  Anthropic Claude subscription authentication")
         (print! "  ---------------------------------------------")
         (print! "  Open this URL if your browser does not open automatically:")
         (print! (str "  " url))
         (print! "")
         (if (open-browser-fn url)
           (print! "  Browser opened. After login, copy the final browser URL and paste it here.")
           (print!
             "  Browser auto-open failed; open the URL manually, then paste the final browser URL here."))
         (when-not manual-code-fn
           (throw
             (ex-info
               "Manual code entry is disabled for this flow. Run `vis providers auth anthropic-coding-plan` in a terminal or use a frontend that can collect the redirect URL."
               {:type :vis/anthropic-manual-entry-disabled})))
         (let
           [input (manual-code-fn print!)
            parsed (parse-authorization-input input)
            code (:code parsed)]

           (when (str/blank? (or input ""))
             (throw (ex-info "Missing authorization input" {:type :vis/anthropic-missing-input})))
           (let
             [credentials (save-auth-file!
                            (exchange-authorization-code! code flow (:state parsed)))]
             (print! "  ✓ Authenticated! Anthropic Claude subscription is ready.")
             (when (str/blank? (:access-token credentials))
               (throw (ex-info "Anthropic credentials missing access token"
                               {:type :vis/anthropic-missing-access-token})))
             :ok)))))))

(vis/register-extension!
  (vis/extension
    {:ext/name "provider-anthropic"
     :ext/description "Anthropic API-key and Claude subscription OAuth providers."
     :ext/version "0.3.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/providers [{:provider/id :anthropic
                      :provider/label "Anthropic (API Key)"
                      :provider/preset {:default-models (svar/provider-default-models :anthropic)}}
                     {:provider/id :anthropic-coding-plan
                      :provider/label "Anthropic (Claude Subscription)"
                      :provider/preset {:base-url (svar/provider-base-url :anthropic-coding-plan)
                                        :api-style :anthropic
                                        :default-models (svar/provider-default-models
                                                          :anthropic-coding-plan)}
                      :provider/status-fn #'status
                      :provider/logout-fn #'logout!
                      :provider/detect-fn #'detect-credentials
                      :provider/auth-fn #'login!
                      :provider/auth-prompt-fn #'auth-instruction-lines
                      :provider/get-token-fn #'get-anthropic-token!
                      :provider/refresh-token-fn #'force-refresh-token!
                      :provider/limits-fn #'limits}]}))
