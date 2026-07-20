(ns com.blockether.vis.ext.provider-openai-codex
  "OpenAI Codex (ChatGPT OAuth) provider.

   This mirrors Codex CLI / ChatGPT OAuth:
   1. Generate PKCE verifier + S256 challenge.
   2. Open auth.openai.com with Codex's public client id.
   3. Let the browser redirect to the registered localhost callback,
      then paste the final redirect URL/code back into Vis.
   4. Exchange the code for ChatGPT access/refresh tokens.
   5. Refresh the access token before expiry and expose it to Vis as
      the provider token.

   Tokens are persisted at `~/.vis/openai-codex-auth.json`. The access
   token is a JWT; Codex requests require the embedded ChatGPT account
   id, so this namespace validates/extracts it during login/refresh."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.provider-openai-codex.limits :as codex-limits]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.oauth :as oauth]
            [taoensso.telemere :as tel])
  (:import [java.net URLDecoder URLEncoder]
           [java.security MessageDigest SecureRandom]
           [java.util Base64]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private CLIENT_ID "app_EMoamEEZ73f0CkXaXp7hrann")

(def ^:private AUTHORIZE_URL "https://auth.openai.com/oauth/authorize")

(def ^:private TOKEN_URL "https://auth.openai.com/oauth/token")

(def ^:private REDIRECT_URI "http://localhost:1455/auth/callback")

(def ^:private SCOPE "openid profile email offline_access")

(def ^:private JWT_CLAIM_PATH "https://api.openai.com/auth")

(def ^:private CODEX_BASE_URL "https://chatgpt.com/backend-api")

(def ^:private AUTH_FILE (str (System/getProperty "user.home") "/.vis/openai-codex-auth.json"))

(def ^:private ^:const REFRESH_MARGIN_MS (* 5 60 1000))

;; =============================================================================
;; Encoding / crypto helpers
;; =============================================================================

(def ^:private secure-random (delay (SecureRandom.)))

(defn- random-bytes
  [n]
  (let [bytes (byte-array n)]
    (.nextBytes ^SecureRandom @secure-random bytes)
    bytes))

(defn- bytes->hex
  [^bytes bytes]
  (let [sb (StringBuilder. (* 2 (alength bytes)))]
    (doseq [b bytes]
      (let [v (bit-and 0xff (int b))]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toHexString v))))
    (str sb)))

(defn- base64url [^bytes bytes] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn- sha256
  [^String s]
  (.digest (MessageDigest/getInstance "SHA-256")
           (.getBytes s java.nio.charset.StandardCharsets/UTF_8)))

(defn- generate-pkce
  []
  (let
    [verifier
     (base64url (random-bytes 32))

     challenge
     (base64url (sha256 verifier))]

    {:verifier verifier :challenge challenge}))

(defn- create-state [] (bytes->hex (random-bytes 16)))

(defn- url-encode [v] (URLEncoder/encode (str v) "UTF-8"))

(defn- url-decode [^String v] (URLDecoder/decode v "UTF-8"))

(defn- form-encode
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
  "Parse a pasted OAuth callback URL, raw query string, `code#state`,
   or bare code. Returns `{:code string? :state string?}`."
  [input]
  (let [value (str/trim (or input ""))]
    (cond (str/blank? value) {}
          (str/starts-with? value "http") (try (let
                                                 [[head fragment] (str/split value #"#" 2)
                                                  q-idx (.indexOf ^String head "?")
                                                  query (when (<= 0 q-idx) (subs head (inc q-idx)))
                                                  params (merge (parse-query-string query)
                                                                (parse-query-string fragment))]

                                                 (select-keys params [:code :state]))
                                               (catch Exception _ {:code value}))
          (str/includes? value "#") (let [[code state] (str/split value #"#" 2)]
                                      (cond-> {:code code}
                                        (not (str/blank? state))
                                        (assoc :state state)))
          (str/includes? value "code=") (select-keys (parse-query-string value) [:code :state])
          :else {:code value})))

(defn- jwt-payload
  [token]
  (try (let
         [parts
          (str/split token #"\.")

          payload
          (second parts)]

         (when (= 3 (count parts))
           (json/read-json (String. (.decode (Base64/getUrlDecoder) ^String payload)
                                    java.nio.charset.StandardCharsets/UTF_8)
                           :key-fn
                           keyword)))
       (catch Exception _ nil)))

(defn account-id
  "Extract the ChatGPT account id from a Codex access-token JWT."
  [access-token]
  (let
    [payload
     (jwt-payload access-token)

     auth
     (get payload (keyword JWT_CLAIM_PATH))

     id
     (:chatgpt_account_id auth)]

    (when-not (str/blank? id) id)))

;; =============================================================================
;; HTTP helpers
;; =============================================================================

(defn- post-form
  [url params]
  (let
    [resp
     (http/post url
                {:headers {"Accept" "application/json"
                           "Content-Type" "application/x-www-form-urlencoded"}
                 :body (form-encode params)
                 :timeout 30000
                 :throw false})

     text
     (:body resp)]

    {:status (:status resp)
     :body text
     :json (try (json/read-json text :key-fn keyword) (catch Exception _ nil))}))

(defn- token-result
  [json]
  (let
    [access-token
     (:access_token json)

     refresh-token
     (:refresh_token json)

     expires-in
     (:expires_in json)

     account-id*
     (account-id access-token)]

    (when (or (str/blank? access-token)
              (str/blank? refresh-token)
              (not (number? expires-in))
              (str/blank? account-id*))
      (throw (ex-info "OpenAI Codex token response missing required fields"
                      {:response (dissoc json :access_token :refresh_token)})))
    {:access-token access-token
     :refresh-token refresh-token
     :expires-at-ms (+ (System/currentTimeMillis) (* (long expires-in) 1000))
     :account-id account-id*}))

(defn- exchange-authorization-code!
  [code verifier]
  (let
    [{:keys [status body json]} (post-form TOKEN_URL
                                           {:grant_type "authorization_code"
                                            :client_id CLIENT_ID
                                            :code code
                                            :code_verifier verifier
                                            :redirect_uri REDIRECT_URI})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "OpenAI Codex token exchange failed: HTTP " status)
                      {:status status :body body})))
    (token-result json)))

(defn- refresh-access-token!
  [refresh-token]
  (let
    [{:keys [status body json]}
     (post-form TOKEN_URL
                {:grant_type "refresh_token" :refresh_token refresh-token :client_id CLIENT_ID})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "OpenAI Codex token refresh failed: HTTP " status)
                      {:status status :body body})))
    (token-result json)))

;; =============================================================================
;; Token persistence
;; =============================================================================

(defn- load-auth-file
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f) (try (json/read-json (slurp f) :key-fn keyword) (catch Exception _ nil)))))

(defn- save-auth-file!
  [credentials]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit AUTH_FILE (json/write-json-str (assoc credentials :saved-at (System/currentTimeMillis))))
    credentials))

(defn- delete-auth-file!
  []
  (let [f (io/file AUTH_FILE)]
    (when (.exists f) (.delete f))))

(defn detect-credentials
  "Detect persisted OpenAI Codex credentials. Returns a status-friendly
   map or nil; does not validate with the network."
  []
  (when-let [auth (load-auth-file)]
    (when-let [access-token (:access-token auth)]
      (when-not (str/blank? access-token)
        {:access-token access-token
         :source :auth-file
         :account-id (or (:account-id auth) (account-id access-token))
         :expires-at-ms (:expires-at-ms auth)}))))

(defn- token-map
  "Provider-token shape for a creds map. Resolves the ChatGPT account id
   (embedded in the JWT when not stored) and throws if absent."
  [auth]
  (let
    [token
     (:access-token auth)

     acct
     (or (:account-id auth) (account-id token))]

    (when (str/blank? acct)
      (throw (ex-info "OpenAI Codex token is missing a ChatGPT account id"
                      {:type :vis/openai-codex-missing-account-id})))
    {:token token :api-url CODEX_BASE_URL :llm-headers {"chatgpt-account-id" acct}}))

(def ^:private refresh-and-persist!
  "Single-flight refresh for the rotating Codex refresh_token (see
   `internal.oauth/make-file-refresher`): serialized per credential file,
   reuses a just-persisted token instead of racing another exchange into
   HTTP 400. Returns the provider-token map."
  (oauth/make-file-refresher
    {:load load-auth-file
     :saved-at :saved-at
     :refresh-token :refresh-token
     :exchange! refresh-access-token!
     :persist! (fn [fresh]
                 (save-auth-file! fresh)
                 (tel/log! {:level :info
                            :id ::codex-token-refreshed
                            :data {:account-id (:account-id fresh)}
                            :msg "OpenAI Codex token refreshed"})
                 fresh)
     :->token token-map
     :no-token!
     #(throw
        (ex-info
          "No OpenAI Codex refresh token on file. Run `vis providers auth openai-codex` to re-authenticate."
          {:type :vis/openai-codex-not-authenticated}))}))

(defn get-openai-codex-token!
  "Return a fresh Codex access token in the provider-token shape used by
   Vis: `{:token access-token :api-url CODEX_BASE_URL :llm-headers {...}}`."
  []
  (let
    [auth
     (load-auth-file)

     now
     (System/currentTimeMillis)]

    (cond
      (and (:access-token auth)
           (:expires-at-ms auth)
           (> (long (:expires-at-ms auth)) (+ now REFRESH_MARGIN_MS)))
      (token-map auth)
      (:refresh-token auth) (refresh-and-persist!)
      :else
      (throw
        (ex-info
          "No OpenAI Codex credentials found. Run `vis providers auth openai-codex` to authenticate."
          {:type :vis/openai-codex-not-authenticated})))))

(defn force-refresh-token!
  "Force an OAuth refresh-token exchange, persist the rotated credentials,
   and return the provider-token map.

   `get-openai-codex-token!` only refreshes when the stored token is locally
   expired, so a token that is locally-valid but invalidated server-side
   (refresh-token rotation by another client/process) would otherwise never
   be replaced. The runtime's 401 recovery path calls this. Routes through
   the single-flight `refresh-and-persist!`, so a STORM of 401s collapses
   into one exchange instead of racing the rotating refresh token into
   HTTP 400.

   `rejected-token` (optional) is the access token the server just 401'd:
   the single-flight reuse step will NOT hand it back, forcing a real
   exchange when the on-file token is still the dead one. Throws when there
   is no refresh token on file."
  ([] (force-refresh-token! nil))
  ([rejected-token] (refresh-and-persist! rejected-token)))

;; =============================================================================
;; OAuth authorization flow
;; =============================================================================

(defn create-authorization-flow
  "Create PKCE verifier, CSRF state, and OpenAI authorization URL."
  ([] (create-authorization-flow "vis"))
  ([originator]
   (let
     [{:keys [verifier challenge]}
      (generate-pkce)

      state
      (create-state)

      query
      (form-encode {:response_type "code"
                    :client_id CLIENT_ID
                    :redirect_uri REDIRECT_URI
                    :scope SCOPE
                    :code_challenge challenge
                    :code_challenge_method "S256"
                    :state state
                    :id_token_add_organizations "true"
                    :codex_cli_simplified_flow "true"
                    :originator originator})]

     {:verifier verifier :state state :url (str AUTHORIZE_URL "?" query)})))

(defn- open-browser! [url] (= :ok (:status (opener/open! url))))

(defn- prompt-for-code!
  [printer-fn]
  (printer-fn "")
  (printer-fn "  Paste the authorization code or full redirect URL, then press Enter:")
  (read-line))

(defn login!
  "Run the Codex OAuth flow and persist fresh credentials.

   Options:
   - `:originator`      value forwarded to OpenAI's OAuth request.
   - `:open-browser-fn` `(fn [url] boolean)` override for tests or
     alternate frontends.
   - `:manual-code-fn`  `(fn [printer-fn] string|nil)` collector for the
     final redirect URL or bare authorization code. CLI uses `read-line`;
     the TUI injects a dialog-backed collector. Pass nil to disable manual
     entry entirely.
   - `:force?`          when true, starts a fresh OAuth flow even if
     persisted credentials already exist."
  ([printer-fn] (login! printer-fn {}))
  ([printer-fn
    {:keys [originator open-browser-fn manual-code-fn force?]
     :or {originator "vis" open-browser-fn open-browser! manual-code-fn prompt-for-code!}}]
   (let [print! (or printer-fn (constantly nil))]
     (if (and (not force?) (detect-credentials))
       (do (print! "  Already authenticated with OpenAI Codex.")
           (print! "  Run `vis providers status openai-codex` for details.")
           (print! "  Run `vis providers logout openai-codex` first to re-authenticate.")
           :already-authenticated)
       (let [{:keys [verifier state url]} (create-authorization-flow originator)]
         (print! "")
         (print! "  OpenAI Codex authentication")
         (print! "  -----------------------------")
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
               "Manual code entry is disabled for this flow. Run `vis providers auth openai-codex` in a terminal or use a frontend that can collect the redirect URL."
               {:type :vis/openai-codex-manual-entry-disabled})))
         (let
           [input (manual-code-fn print!)
            parsed (parse-authorization-input input)
            code (:code parsed)]

           (when (str/blank? (or input ""))
             (throw (ex-info "Missing authorization input"
                             {:type :vis/openai-codex-missing-input})))
           (when (and (:state parsed) (not= state (:state parsed)))
             (throw (ex-info "State mismatch" {:expected state :actual (:state parsed)})))
           (when (str/blank? code) (throw (ex-info "Missing authorization code" {})))
           (let [credentials (save-auth-file! (exchange-authorization-code! code verifier))]
             (print! (str "  ✓ Authenticated! OpenAI Codex is ready (account "
                          (:account-id credentials)
                          ")."))
             :ok)))))))

;; =============================================================================
;; Public CLI helpers
;; =============================================================================

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
        (:source detected) :account-id
        (:account-id detected) :oauth-token-preview
        (let [t (:access-token detected)]
          (str (subs t 0 (min 8 (count t))) "...")))

      (:expires-at-ms detected)
      (assoc :copilot-token-valid?
        (> (long (:expires-at-ms detected)) now) :expires-in-ms
        (- (long (:expires-at-ms detected)) now)))))

(defn logout! [] (delete-auth-file!) :logged-out)

(defn- usage-auth-error? [^Throwable t] (contains? #{401 403} (:status (ex-data t))))

(defn- usage-error-report
  [^Throwable t]
  {:provider-id :openai-codex
   :status :error
   :fetched-at-ms (System/currentTimeMillis)
   :dynamic {:limits [] :note "OpenAI Codex usage is unavailable."}
   :error {:type :provider/openai-codex-usage-error
           :message (or (ex-message t) (.getName (class t)))}})

(defn- usage-report-from-token!
  [{:keys [token llm-headers]}]
  (let [account-id (get llm-headers "chatgpt-account-id")]
    (if (or (str/blank? token) (str/blank? account-id))
      {:provider-id :openai-codex
       :status :error
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic {:limits [] :note "OpenAI Codex credentials are missing usage request fields."}
       :error {:type :provider/openai-codex-missing-usage-credentials
               :message "OpenAI Codex credentials are missing access token or account id"}}
      {:provider-id :openai-codex
       :status :ok
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic (codex-limits/dynamic-limits! token account-id)})))

(defn- authenticated-limits-report!
  []
  (let [provider-token (get-openai-codex-token!)]
    (try (usage-report-from-token! provider-token)
         (catch Throwable t
           (if (usage-auth-error? t)
             ;; The usage endpoint rejected a locally-valid token. That is the
             ;; same refresh-token rotation failure mode as a mid-turn 401: force
             ;; refresh with the rejected token so single-flight cannot hand it
             ;; back, then retry the usage request once.
             (try (usage-report-from-token! (force-refresh-token! (:token provider-token)))
                  (catch Throwable retry-t (usage-error-report retry-t)))
             (usage-error-report t))))))

(defn limits
  "Normalized limits envelope for the OpenAI Codex provider.

   Static RPM/TPM metadata comes from svar's provider catalog; this fn
   reports authentication state and live ChatGPT/Codex quota windows
   when credentials are available. A usage-endpoint 401/403 force-refreshes
   the rotating OAuth token and retries once so TUI/gateway/iOS status panels
   do not get stuck on a server-rotated access token."
  []
  (let [detected (detect-credentials)]
    (if (nil? detected)
      {:provider-id :openai-codex
       :status :unauthenticated
       :fetched-at-ms (System/currentTimeMillis)
       :dynamic {:limits [] :note "OpenAI Codex is not authenticated."}}
      (authenticated-limits-report!))))

(require '[com.blockether.vis.core :as vis])

(require '[com.blockether.svar.core :as svar])

;; ---------------------------------------------------------------------------
;; A new Codex model svar's pinned catalog doesn't know yet can still be offered
;; with its real window declared INLINE on `:default-models` as
;; `{:name "…" :context N}`: `default-model-configs` carries the map through,
;; `->svar-model` forwards `:context` to svar, and svar's `provider-model-context`
;; honors a caller-supplied `:context` over its own catalog / the 8192
;; `DEFAULT_CONTEXT_LIMIT`. gpt-5.6-terra now rides as a BARE name — svar's
;; catalog (>= 0.7.59) supplies its 272k window, so no inline `:context` needed.

;; Provider-specific Settings knob — registered HERE, next to the
;; backend it tunes (not in internal/toggles.clj), and visible in the
;; settings dialogs only while a Codex provider is actually configured.
(vis/register-toggle! {:id :openai-codex/verbosity
                       :label "Verbosity"
                       :description "Output detail hint passed to the OpenAI Codex backend."
                       :type :enum
                       :choices [:low :medium :high]
                       :default :low
                       :owner :vis
                       :group :provider
                       :persist? true
                       ;; Cycled on its own control (TUI keymap 'l'), not the Settings dialog.
                       :settings? false
                       :visible-fn (fn []
                                     (boolean (vis/has-provider? :openai-codex)))})

(vis/register-extension!
  (vis/extension
    {:ext/name "provider-openai-codex"
     :ext/description "OpenAI Codex / ChatGPT OAuth provider."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/providers [{:provider/id :openai-codex
                      :provider/label "OpenAI Codex (ChatGPT OAuth)"
                      :provider/preset {:default-models (distinct (concat
                                                                    (svar/provider-default-models
                                                                      :openai-codex)
                                                                    ["gpt-5.6-terra"]))}
                      :provider/status-fn #'status
                      :provider/logout-fn #'logout!
                      :provider/detect-fn #'detect-credentials
                      :provider/auth-fn #'login!
                      :provider/get-token-fn #'get-openai-codex-token!
                      :provider/refresh-token-fn #'force-refresh-token!
                      :provider/limits-fn #'limits}]}))
