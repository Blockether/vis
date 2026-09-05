(ns com.blockether.vis.internal.provider.openai-codex
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
   id, so this namespace validates/extracts it during login/refresh.

   The dynamic quota report lives here too (`dynamic-limits!`): it fetches
   `https://chatgpt.com/backend-api/wham/usage`, selects the regular Codex
   bucket (or the nested Codex Spark bucket) and exposes the 5h and 7d
   percentage windows as normalized Vis limit rows."
  (:require [com.blockether.vis.internal.util :as util]
            [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.oauth :as oauth]
            [taoensso.telemere :as tel])
  (:import [java.net URLDecoder URLEncoder]
           [java.security SecureRandom]
           [java.util Base64]))

;; Constants

(def ^:private CLIENT_ID "app_EMoamEEZ73f0CkXaXp7hrann")

(def ^:private AUTHORIZE_URL "https://auth.openai.com/oauth/authorize")

(def ^:private TOKEN_URL "https://auth.openai.com/oauth/token")

(def ^:private REDIRECT_URI "http://localhost:1455/auth/callback")

(def ^:private SCOPE "openid profile email offline_access")

(def ^:private JWT_CLAIM_PATH "https://api.openai.com/auth")

(def ^:private CODEX_BASE_URL "https://chatgpt.com/backend-api")

(defn- auth-file
  "Persisted OAuth credentials. A FUNCTION: native-image folds top-level `def`s at
   build time, which would bake the builder's home directory into the binary."
  ^String []
  (str (System/getProperty "user.home") "/.vis/openai-codex-auth.json"))

(def ^:private ^:const REFRESH_MARGIN_MS (* 5 60 1000))

;; Encoding / crypto helpers

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

(defn- sha256 [^String s] (util/sha256 (util/utf8 s)))

(defn- generate-pkce
  []
  (let [verifier
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
          ;; Mobile browsers sometimes copy a loopback callback without `http://`.
          ;; The query carries the OAuth response, so parse any URL-shaped value with
          ;; a query rather than treating `localhost:…/callback?code=…` as the code.
          (or (str/starts-with? value "http") (str/includes? value "?"))
          (try (let [[head fragment] (str/split value #"#" 2)
                     q-idx (.indexOf ^String head "?")
                     query (when (<= 0 q-idx) (subs head (inc q-idx)))
                     params (merge (parse-query-string query) (parse-query-string fragment))]

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
  (try (let [parts
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
  (let [payload
        (jwt-payload access-token)

        auth
        (get payload (keyword JWT_CLAIM_PATH))

        id
        (:chatgpt_account_id auth)]

    (when-not (str/blank? id) id)))

;; HTTP helpers

(defn- post-form
  [url params]
  (let [resp
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
  (let [access-token
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
     :expires-at-ms (+ (util/now-ms) (* (long expires-in) 1000))
     :account-id account-id*}))

(defn- exchange-authorization-code!
  [code verifier]
  (let [{:keys [status body json]} (post-form TOKEN_URL
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
  (let [{:keys [status body json]}
        (post-form TOKEN_URL
                   {:grant_type "refresh_token" :refresh_token refresh-token :client_id CLIENT_ID})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "OpenAI Codex token refresh failed: HTTP " status)
                      {:status status :body body})))
    (token-result json)))

;; Token persistence

(defn- auth-json-key
  "JSON key -> engine keyword. What we write is snake_case (`refresh_token`);
   the kebab spelling older builds persisted reads back onto the same key."
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn- load-auth-file
  []
  (let [f (io/file (auth-file))]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn auth-json-key) (catch Exception _ nil)))))

(defn- save-auth-file!
  "Persist credentials through the ONE JSON boundary (`oauth/auth-json-str`):
   snake_case string keys, total encoding."
  [credentials]
  (let [dir (io/file (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists dir) (.mkdirs dir))
    (spit (auth-file) (oauth/auth-json-str (assoc credentials :saved-at (util/now-ms))))
    credentials))

(defn- delete-auth-file!
  []
  (let [f (io/file (auth-file))]
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
  (let [token
        (:access-token auth)

        acct
        (or (:account-id auth) (account-id token))]

    (when (str/blank? acct)
      (throw (ex-info "OpenAI Codex token is missing a ChatGPT account id"
                      {:type :vis/openai-codex-missing-account-id})))
    {:token token :api-url CODEX_BASE_URL :llm-headers {"chatgpt-account-id" acct}}))

(def ^:private file-refresher
  "Single-flight refresh for the rotating Codex refresh_token (see
   `internal.oauth/make-file-refresher`): serialized per credential file,
   reuses a just-persisted token instead of racing another exchange into
   HTTP 400. Returns the provider-token map.

   A `delay`, not a value: `:lock-path` is derived from the user's home,
   and native-image runs namespace init at BUILD time - a top-level value
   would bake the build machine's lock path into the shipped binary."
  (delay
    (oauth/make-file-refresher
      {:load load-auth-file
       :lock-path (str (auth-file) ".lock")
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
            "No OpenAI Codex refresh token on file. Run `vis-agent providers auth openai-codex` to re-authenticate."
            {:type :vis/openai-codex-not-authenticated}))})))

(defn ^:private refresh-and-persist!
  "Run the single-flight refresher, building it on first call."
  ([] (@file-refresher))
  ([rejected-token] (@file-refresher rejected-token)))

(defn get-openai-codex-token!
  "Return a fresh Codex access token in the provider-token shape used by
   Vis: `{:token access-token :api-url CODEX_BASE_URL :llm-headers {...}}`."
  []
  (let [auth
        (load-auth-file)

        now
        (util/now-ms)]

    (cond
      (and (:access-token auth)
           (:expires-at-ms auth)
           (> (long (:expires-at-ms auth)) (+ now (long REFRESH_MARGIN_MS))))
      (token-map auth)
      (:refresh-token auth) (refresh-and-persist!)
      :else
      (throw
        (ex-info
          "No OpenAI Codex credentials found. Run `vis-agent providers auth openai-codex` to authenticate."
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

;; OAuth authorization flow

(defn create-authorization-flow
  "Create PKCE verifier, CSRF state, and OpenAI authorization URL."
  ([] (create-authorization-flow "vis"))
  ([originator]
   (let [{:keys [verifier challenge]}
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
           (print! "  Run `vis-agent providers status openai-codex` for details.")
           (print! "  Run `vis-agent providers logout openai-codex` first to re-authenticate.")
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
               "Manual code entry is disabled for this flow. Run `vis-agent providers auth openai-codex` in a terminal or use a frontend that can collect the redirect URL."
               {:type :vis/openai-codex-manual-entry-disabled})))
         (let [input (manual-code-fn print!)
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

(defn auth-start
  "Headless leg 1 of OpenAI Codex OAuth — the wire-drivable twin of `login!`.
   Mints a fresh PKCE flow and returns the authorization URL plus the OPAQUE
   `:flow` the daemon hands back to `auth-complete`.

   `:flow` carries the PKCE verifier and CSRF state: daemon-side secrets that
   must NEVER be emitted onto the wire."
  ([] (auth-start "vis"))
  ([originator]
   (let [{:keys [url] :as flow} (create-authorization-flow originator)]
     {:kind :pkce
      :url url
      :instructions ["Sign in to OpenAI in the browser."
                     "Copy the FULL redirect URL from the address bar."
                     "Paste it back here to finish."]
      :flow flow})))

(defn auth-complete
  "Headless leg 2: verify CSRF state, exchange the pasted redirect URL (or bare
   `code#state`) for credentials, and PERSIST them in the daemon's auth file."
  [{:keys [verifier state]} input]
  (when (str/blank? (or input ""))
    (throw (ex-info "Missing authorization input" {:type :vis/openai-codex-missing-input})))
  (let [parsed
        (parse-authorization-input input)

        code
        (:code parsed)]

    (when (and (:state parsed) (not= state (:state parsed)))
      (throw (ex-info "State mismatch" {:expected state :actual (:state parsed)})))
    (when (str/blank? code) (throw (ex-info "Missing authorization code" {})))
    (save-auth-file! (exchange-authorization-code! code verifier))
    {:status :ok}))

;; Public CLI helpers

(defn authenticated? [] (some? (detect-credentials)))

(defn status
  []
  (let [detected
        (detect-credentials)

        now
        (util/now-ms)]

    (cond-> {:is-authenticated (some? detected)}
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
  (if (usage-auth-error? t)
    {:provider-id :openai-codex
     :status :unauthenticated
     :fetched-at-ms (util/now-ms)
     :dynamic {:limits [] :note "OpenAI Codex credentials were rejected."}
     :error {:type :provider/openai-codex-usage-unauthenticated
             :message (or (ex-message t) (.getName (class t)))}}
    {:provider-id :openai-codex
     :status :error
     :fetched-at-ms (util/now-ms)
     :dynamic {:limits [] :note "OpenAI Codex usage is unavailable."}
     :error {:type :provider/openai-codex-usage-error
             :message (or (ex-message t) (.getName (class t)))}}))

;;; ── Dynamic quota (`/wham/usage`) ─────────────────────────────────────────

(def ^:private usage-url "https://chatgpt.com/backend-api/wham/usage")

(def ^:private spark-limit-name "GPT-5.3-Codex-Spark")

(def ^:private spark-model-id "gpt-5.3-codex-spark")

(def ^:private fallback-window-specs
  [{:bucket-key :primary_window :id :codex-5h :label "Codex 5h quota (%)" :unit :hour :size 5}
   {:bucket-key :secondary_window :id :codex-7d :label "Codex 7d quota (%)" :unit :day :size 7}])

(def ^:private known-window-specs-by-seconds
  {(* 5 60 60) {:id :codex-5h :label "Codex 5h quota (%)" :unit :hour :size 5}
   (* 7 24 60 60) {:id :codex-7d :label "Codex 7d quota (%)" :unit :day :size 7}})

(defn- object-map [value] (when (and (map? value) (not (record? value))) value))

(defn- camel-key
  [k]
  (let [s (name k)]
    (str/replace s #"_([a-zA-Z])" #(str/upper-case (second %)))))

(defn- kebab-key [k] (str/replace (name k) #"_" "-"))

(defn- field
  [m k]
  (when-let [m* (object-map m)]
    (let [ks [k (name k) (keyword (camel-key k)) (camel-key k) (keyword (kebab-key k))
              (kebab-key k)]]
      (reduce (fn [_ k*]
                (when (contains? m* k*) (reduced (get m* k*))))
              nil
              ks))))

(defn- clamp-percent
  [value]
  (-> (double value)
      (max 0.0)
      (min 100.0)))

(defn- used->left-percent
  [used-percent]
  (when (number? used-percent) (clamp-percent (- 100.0 (double used-percent)))))

(defn- model-id
  [model-ref]
  (cond (keyword? model-ref) (name model-ref)
        (string? model-ref) model-ref
        (map? model-ref) (or (field model-ref :id)
                             (field model-ref :name)
                             (some-> (field model-ref :model)
                                     model-id))
        :else nil))

(defn- spark-model?
  [model-ref]
  (= spark-model-id
     (some-> (model-id model-ref)
             str/lower-case)))

(defn- normalize-rate-limit-bucket [value] (object-map value))

(defn- spark-rate-limit-entry
  [value]
  (let [entry (object-map value)]
    (when (= spark-limit-name
             (some-> (field entry :limit_name)
                     str/trim))
      (normalize-rate-limit-bucket (field entry :rate_limit)))))

(defn- spark-rate-limit-bucket
  [usage]
  (let [additional (field usage :additional_rate_limits)]
    (or (when (sequential? additional) (some spark-rate-limit-entry additional))
        (when-let [additional-map (object-map additional)]
          (some spark-rate-limit-entry (vals additional-map))))))

(defn- select-rate-limit-bucket
  [usage model-ref]
  (if (spark-model? model-ref)
    (spark-rate-limit-bucket usage)
    (normalize-rate-limit-bucket (field usage :rate_limit))))

(defn- epoch-ms
  [value]
  (when (number? value)
    (long (if (> (double value) 100000000000.0) value (* 1000.0 (double value))))))

(defn- reset-at-ms
  [window now-ms]
  (or (epoch-ms (field window :reset_at))
      (when-let [seconds (field window :reset_after_seconds)]
        (when (number? seconds) (+ (long now-ms) (long (* 1000.0 (double seconds))))))))

(defn- generated-window-spec
  [seconds fallback]
  (let [seconds
        (long seconds)

        day?
        (zero? (long (mod seconds (* 24 60 60))))

        hour?
        (zero? (long (mod seconds (* 60 60))))

        [unit size suffix]
        (cond day? [:day (quot seconds (* 24 60 60)) "d"]
              hour? [:hour (quot seconds (* 60 60)) "h"]
              :else [:second seconds "s"])]

    (assoc fallback
      :id (keyword (str "codex-" size suffix))
      :label (str "Codex " size suffix " quota (%)")
      :unit unit
      :size size)))

(defn- window-spec
  [fallback window]
  (let [seconds (field window :limit_window_seconds)]
    (if (number? seconds)
      (merge fallback
             (or (get known-window-specs-by-seconds (long seconds))
                 (generated-window-spec seconds fallback)))
      fallback)))

(defn- window-row
  [now-ms {:keys [bucket-key] :as fallback} bucket]
  (when-let [window (not-empty (object-map (field bucket bucket-key)))]
    (let [{:keys [id label unit size]} (window-spec fallback window)
          used-percent (field window :used_percent)
          left-percent (used->left-percent used-percent)
          reset-ms (reset-at-ms window now-ms)]

      (cond-> {:id id
               :label label
               :scope :account
               :kind :rate
               :precision :exact
               :source :provider-api
               :is-unlimited false
               :window (cond-> {:kind :rolling :unit unit :size size}
                         reset-ms
                         (assoc :resets-at-ms reset-ms))}
        (number? used-percent)
        (assoc :used
          (clamp-percent used-percent) :limit
          100.0)

        (number? left-percent)
        (assoc :remaining left-percent)))))

(defn- window-sort-seconds
  [row]
  (let [{:keys [unit size]}
        (:window row)

        seconds
        (case unit
          :minute
          60

          :hour
          (* 60 60)

          :day
          (* 24 60 60)

          :week
          (* 7 24 60 60)

          :month
          (* 30 24 60 60)

          :year
          (* 365 24 60 60)

          Long/MAX_VALUE)]

    (* (long (or size 1)) (long seconds))))

(defn- missing-window-row
  [{:keys [id label unit size]}]
  {:id id
   :label label
   :scope :account
   :kind :rate
   :precision :unknown
   :source :provider-api
   :is-unlimited false
   :window {:kind :rolling :unit unit :size size}
   :note "OpenAI Codex did not report this quota window."})

(defn usage->dynamic-limits
  "Convert ChatGPT/Codex `/wham/usage` JSON into Vis dynamic limit rows.

   `model-ref` may be a model id string/keyword or a map with `:id` /
   `:name`. It is used only for Codex Spark, whose bucket is nested in
   `additional_rate_limits`, matching Codex/ChatGPT's usage payload. Rows are
   sorted by explicit window size (5h before 7d) rather than trusting provider
   bucket names (`primary_window` / `secondary_window`)."
  ([usage] (usage->dynamic-limits usage nil))
  ([usage model-ref] (usage->dynamic-limits usage model-ref (util/now-ms)))
  ([usage model-ref now-ms]
   (let [bucket
         (select-rate-limit-bucket usage model-ref)

         rows
         (if bucket
           (let [actual-rows
                 (keep #(window-row now-ms % bucket) fallback-window-specs)

                 present-ids
                 (set (map :id actual-rows))]

             (->> fallback-window-specs
                  (remove #(contains? present-ids (:id %)))
                  (map missing-window-row)
                  (concat actual-rows)
                  (sort-by window-sort-seconds)
                  vec))
           [])

         limited?
         (or (true? (field bucket :limit_reached)) (false? (field bucket :allowed)))]

     (cond-> {:limits rows}
       (and bucket (empty? rows))
       (assoc :note "OpenAI Codex usage endpoint did not return quota windows.")

       (nil? bucket)
       (assoc :note "OpenAI Codex usage endpoint did not return a matching quota bucket.")

       limited?
       (assoc :note "OpenAI Codex reports that the selected quota bucket is currently limited.")))))

(defn fetch-usage!
  "Fetch raw ChatGPT/Codex usage JSON from
   `https://chatgpt.com/backend-api/wham/usage`."
  [access-token account-id]
  (let [response
        (http/get usage-url
                  {:headers {"Accept" "*/*"
                             "Authorization" (str "Bearer " access-token)
                             "chatgpt-account-id" account-id}
                   :timeout 30000
                   :throw false})

        status
        (:status response)

        body
        (:body response)]

    (if (<= 200 status 299)
      (json/read-json body :key-fn keyword)
      (throw
        (ex-info
          (str "OpenAI Codex usage request failed: HTTP " status)
          {:type :provider/openai-codex-usage-error :status status :body body :url usage-url})))))

(defn dynamic-limits!
  "Fetch and normalize OpenAI Codex dynamic quota data for an access
   token/account id pair. Optional `model-ref` selects the Codex Spark
   nested bucket when applicable."
  ([access-token account-id] (dynamic-limits! access-token account-id nil))
  ([access-token account-id model-ref]
   (usage->dynamic-limits (fetch-usage! access-token account-id) model-ref)))

(defn- usage-report-from-token!
  [{:keys [token llm-headers]}]
  (let [account-id (get llm-headers "chatgpt-account-id")]
    (if (or (str/blank? token) (str/blank? account-id))
      {:provider-id :openai-codex
       :status :error
       :fetched-at-ms (util/now-ms)
       :dynamic {:limits [] :note "OpenAI Codex credentials are missing usage request fields."}
       :error {:type :provider/openai-codex-missing-usage-credentials
               :message "OpenAI Codex credentials are missing access token or account id"}}
      {:provider-id :openai-codex
       :status :ok
       :fetched-at-ms (util/now-ms)
       :dynamic (dynamic-limits! token account-id)})))

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
       :fetched-at-ms (util/now-ms)
       :dynamic {:limits [] :note "OpenAI Codex is not authenticated."}}
      (authenticated-limits-report!))))

(require '[com.blockether.vis.core :as vis])
(require '[com.blockether.svar.core :as svar])

(vis/register-toggle! {:id "codex_fast_mode"
                       :label "Fast mode"
                       :description
                       "Route eligible OpenAI Codex turns through the priority service tier."
                       :type :boolean
                       :settings? false
                       :default false
                       :owner "openai-codex"
                       :group :provider
                       :persist? true})

;; A new Codex model svar's pinned catalog doesn't know yet can still be offered
;; with its real window declared INLINE on `:default-models` as
;; `{:name "…" :context N}`: `default-model-configs` carries the map through,
;; `->svar-model` forwards `:context` to svar, and svar's `provider-model-context`
;; honors a caller-supplied `:context` over its own catalog / the 8192
;; `DEFAULT_CONTEXT_LIMIT`. gpt-5.6-terra now rides as a BARE name — svar's
;; catalog (>= 0.7.59) supplies its 272k window, so no inline `:context` needed.

;; NOTE: verbosity is NOT registered here. `text.verbosity` is a knob of the
;; OpenAI RESPONSES wire, and Codex is one of several providers that ride it
;; (GitHub Copilot's GPT tier is another), so the toggle lives in
;; `internal/toggles.clj` and its visibility follows svar's `:verbosity-style`
;; capability instead of this provider's id.

(defn register!
  []
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
                        :provider/auth-start-fn #'auth-start
                        :provider/auth-complete-fn #'auth-complete
                        :provider/get-token-fn #'get-openai-codex-token!
                        :provider/refresh-token-fn #'force-refresh-token!
                        :provider/limits-fn #'limits}]})))
