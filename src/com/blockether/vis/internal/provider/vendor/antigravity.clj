(ns com.blockether.vis.internal.provider.vendor.antigravity
  "Google Antigravity provider: Gemini and Claude through Google Cloud Code
   Assist, signed in with the Antigravity Google OAuth client.

   Vis owns sign-in and quota reports. Svar owns the Cloud Code wire, the
   managed project, and the model list.

   Credentials live in `~/.vis/antigravity-auth.json`. Google does not rotate
   the refresh token, so a refresh keeps the stored one."
  (:require [com.blockether.vis.internal.util :as util]
            [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.provider.oauth :as oauth])
  (:import [java.io IOException]
           [java.net URLDecoder URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.time Instant]
           [java.time.format DateTimeParseException]
           [java.util Base64]))

(def ^:private PROVIDER_ID :antigravity)

(defn- base64-text
  ^String [^String s]
  (String. (.decode (Base64/getDecoder) s) StandardCharsets/UTF_8))

;; The public Antigravity desktop client.
(def ^:private client-id
  (base64-text
    "MTA3MTAwNjA2MDU5MS10bWhzc2luMmgyMWxjcmUyMzV2dG9sb2poNGc0MDNlcC5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbQ=="))

(defn- client-secret
  "The secret of the Antigravity client, from the environment. A FUNCTION: native-image
   folds top-level `def`s at build time. The repository keeps no secret."
  ^String []
  (or (not-empty (System/getenv "VIS_ANTIGRAVITY_CLIENT_SECRET"))
      (throw (ex-info "Set VIS_ANTIGRAVITY_CLIENT_SECRET to the Antigravity OAuth client secret."
                      {:type :vis/antigravity-client-secret-missing}))))

(def ^:private AUTHORIZE_URL "https://accounts.google.com/o/oauth2/v2/auth")

(def ^:private TOKEN_URL "https://oauth2.googleapis.com/token")

(def ^:private USERINFO_URL "https://www.googleapis.com/oauth2/v1/userinfo?alt=json")

(def ^:private REDIRECT_URI "http://localhost:51121/oauth-callback")

(def ^:private SCOPES
  (str/join " "
            (map #(str "https://www.googleapis.com/auth/" %)
                 ["cloud-platform" "userinfo.email" "userinfo.profile" "cclog"
                  "experimentsandconfigs"])))

(def ^:private ^:const REFRESH_MARGIN_MS (* 5 60 1000))

(defn- auth-file
  "Persisted OAuth credentials. A FUNCTION: native-image folds top-level `def`s at
   build time, which would bake the builder's home directory into the binary."
  ^String []
  (str (paths/vis-home) "/antigravity-auth.json"))

;; Encoding

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
  "Parse a pasted callback URL, query string or bare code into `{:code :state}`."
  [input]
  (let [value (str/trim (or input ""))]
    (cond (str/blank? value) {}
          (str/includes? value "?")
          (let [[head fragment] (str/split value #"#" 2)]
            (select-keys (merge (parse-query-string (subs head (inc (str/index-of head "?"))))
                                (parse-query-string fragment))
                         [:code :state]))
          (str/includes? value "code=") (select-keys (parse-query-string value) [:code :state])
          :else {:code value})))

;; Token endpoint

(defn- post-form
  [url params]
  (let [resp (http/post url
                        {:headers {"Accept" "application/json"
                                   "Content-Type" "application/x-www-form-urlencoded"}
                         :body (form-encode params)
                         :timeout 30000
                         :throw false})]
    {:status (:status resp)
     :body (:body resp)
     :json (try (json/read-json (:body resp) :key-fn keyword) (catch Exception _ nil))}))

(defn- fetch-email
  "Account email for status, or nil: sign-in does not depend on it."
  [access-token]
  (try (let [resp (http/get USERINFO_URL
                            {:headers {"Authorization" (str "Bearer " access-token)}
                             :timeout 15000
                             :throw false})]
         (when (<= 200 (long (:status resp)) 299) (get (json/read-json (:body resp)) "email")))
       (catch Exception _ nil)))

(defn- token-result
  "Credentials from a token response. A refresh response has no refresh token,
   so `previous` supplies it."
  [json previous]
  (let [access-token
        (:access_token json)

        refresh-token
        (or (:refresh_token json) (:refresh-token previous))

        expires-in
        (:expires_in json)]

    (when (or (str/blank? access-token) (str/blank? refresh-token) (not (number? expires-in)))
      (throw (ex-info "Antigravity token response missing required fields"
                      {:type :vis/antigravity-token-invalid
                       :response (dissoc json :access_token :refresh_token :id_token)})))
    {:access-token access-token
     :refresh-token refresh-token
     :expires-at-ms (+ (util/now-ms) (* (long expires-in) 1000))
     :email (or (fetch-email access-token) (:email previous))}))

(defn- token-request!
  [params previous what]
  (let [{:keys [status body json]}
        (post-form TOKEN_URL (merge {:client_id client-id :client_secret (client-secret)} params))]
    (when-not (<= 200 (long status) 299)
      (throw (ex-info (str "Antigravity token " what " failed: HTTP " status)
                      {:type :vis/antigravity-token-failed :status status :body body})))
    (token-result json previous)))

(defn- exchange-authorization-code!
  [code {:keys [verifier state]} returned-state]
  (when (str/blank? code)
    (throw (ex-info "Missing Antigravity authorization code"
                    {:type :vis/antigravity-missing-code})))
  (when (and (not (str/blank? returned-state)) (not= state returned-state))
    (throw (ex-info "Antigravity OAuth state mismatch" {:type :vis/antigravity-state-mismatch})))
  (token-request!
    {:grant_type "authorization_code" :code code :redirect_uri REDIRECT_URI :code_verifier verifier}
    nil
    "exchange"))

;; Token persistence

(defn- auth-json-key [k] (keyword (str/replace (name k) "_" "-")))

(defn- load-auth-file
  []
  (let [f (io/file (auth-file))]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn auth-json-key) (catch Exception _ nil)))))

(defn- save-auth-file!
  [credentials]
  (paths/spit-private! (auth-file)
                       (oauth/auth-json-str (assoc credentials :saved-at (util/now-ms))))
  (when (:reauth-required credentials) (provider-limits/auth-changed! PROVIDER_ID))
  credentials)

(defn- delete-auth-file!
  []
  (try (Files/deleteIfExists (.toPath (io/file (auth-file))))
       (catch IOException e
         (throw (ex-info "Could not delete the Antigravity sign-in file."
                         {:type :vis/antigravity-logout-failed}
                         e)))))

(defn- refresh-access-token!
  [refresh-token]
  (token-request! {:grant_type "refresh_token" :refresh_token refresh-token}
                  (assoc (load-auth-file) :refresh-token refresh-token)
                  "refresh"))

(defn detect-credentials
  "Persisted Antigravity credentials as a status map, or nil. No network call."
  []
  (when-let [auth (load-auth-file)]
    (or (oauth/reauthentication-status auth)
        (when-not (str/blank? (:access-token auth))
          {:access-token (:access-token auth)
           :source :auth-file
           :email (:email auth)
           :expires-at-ms (:expires-at-ms auth)}))))

(defn- token-map [auth] {:token (:access-token auth) :api-url (svar/provider-base-url PROVIDER_ID)})

(def ^:private file-refresher
  "Single-flight refresh. A `delay`: native-image runs namespace init at build
   time, and the lock path comes from the user's home."
  (delay (oauth/make-file-refresher
           {:load load-auth-file
            :lock-path (str (auth-file) ".lock")
            :saved-at :saved-at
            :refresh-token :refresh-token
            :exchange! refresh-access-token!
            :store PROVIDER_ID
            :persist! save-auth-file!
            :->token token-map
            :no-token!
            #(throw
               (ex-info
                 "No Antigravity refresh token on file. Run `vis-agent providers auth antigravity`."
                 {:type :vis/antigravity-not-authenticated}))})))

(defn get-antigravity-token!
  "A usable access token as `{:token :api-url}`; refreshes when it expires soon."
  []
  (let [auth (oauth/require-usable-credentials! (load-auth-file))]
    (cond (and (:access-token auth)
               (:expires-at-ms auth)
               (> (long (:expires-at-ms auth)) (+ (util/now-ms) (long REFRESH_MARGIN_MS))))
          (token-map auth)
          (:refresh-token auth) (@file-refresher)
          :else (throw
                  (ex-info
                    "No Antigravity credentials found. Run `vis-agent providers auth antigravity`."
                    {:type :vis/antigravity-not-authenticated})))))

(defn force-refresh-token!
  "Refresh after the server rejected `rejected-token`; returns the token map."
  ([] (force-refresh-token! nil))
  ([rejected-token] (@file-refresher rejected-token)))

;; Quota reports

(defn- quota-pool
  [id]
  (cond (str/starts-with? id "gemini-") ["gemini" "Gemini"]
        (str/starts-with? id "claude-") ["claude" "Claude"]
        (str/starts-with? id "gpt-oss-") ["gpt-oss" "GPT-OSS"]
        :else [id id]))

(defn- quota-sample
  [{:keys [id] :as model}]
  (let [fraction (get-in model ["quotaInfo" "remainingFraction"])
        reset (get-in model ["quotaInfo" "resetTime"])]
    (when (and (string? id) (not (str/blank? id))
               (not (true? (get model "disabled")))
               (not (str/ends-with? id "-disabled"))
               (not (str/starts-with? id "tab_"))
               (number? fraction) (<= 0.0 (double fraction) 1.0))
      {:pool (quota-pool id)
       :remaining (* 100.0 (double fraction))
       :resets-at-ms (when (string? reset)
                       (try (.toEpochMilli (Instant/parse reset))
                            (catch DateTimeParseException _ nil)))})))

(defn- pool-row
  [[[id label] samples]]
  ;; If the readings differ, show the lowest balance with its own reset time.
  (let [{:keys [remaining resets-at-ms]}
        (first (sort-by (juxt :remaining #(or (:resets-at-ms %) Long/MAX_VALUE)) samples))]
    (cond-> {:id (str "antigravity-" id)
             :label (str label " quota (%)")
             :scope :account
             :kind :rate
             :precision :exact
             :source :provider-api
             :is-unlimited false
             :limit 100.0
             :remaining remaining
             :used (- 100.0 (double remaining))}
      resets-at-ms
      (assoc :window {:kind :rolling :resets-at-ms resets-at-ms})

      (zero? (double remaining))
      (assoc :note (str label " quota is exhausted.")))))

(defn- fetch-limits-report
  []
  (try (let [{:keys [token api-url]} (get-antigravity-token!)
             ;; Svar preserves quotaInfo on model entries. Its model cache lasts 60s.
             models (svar/models! nil {:base-url api-url
                                       :api-key token
                                       :provider-id PROVIDER_ID
                                       :api-style :cloud-code
                                       :strict? true})
             rows (->> models (keep quota-sample) (group-by :pool) (map pool-row)
                       (sort-by :id) vec)]
         (if (seq rows)
           {:status :ok :dynamic {:limits rows}}
           {:status :unsupported
            :dynamic {:limits [] :note "Antigravity did not report any model quota."}}))
       (catch Exception e
         (let [status (:status (ex-data e))]
           (if (or (= :vis/antigravity-not-authenticated (:type (ex-data e)))
                   (oauth/reauthentication-error? e)
                   (contains? #{401 403} status))
             {:status :unauthenticated
              :dynamic {:limits []
                        :note "Run `vis-agent providers auth antigravity` to sign in."}}
             {:status :error
              :dynamic {:limits []}
              :error {:type :vis/antigravity-limits-error
                      :message (if status
                                 (str "Antigravity quota check failed: HTTP " status ".")
                                 "Could not check Antigravity quota.")
                      :data (cond-> {} status (assoc :status status))}})))))

;; Authorization flow

(defn create-authorization-flow
  "PKCE verifier, CSRF state and the Google authorization URL."
  []
  (let [verifier
        (util/base64url (util/random-bytes 32))

        state
        (util/base64url (util/random-bytes 32))

        challenge
        (util/base64url (util/sha256 (util/utf8 verifier)))]

    {:verifier verifier
     :state state
     :url (str AUTHORIZE_URL
               "?"
               (form-encode {:client_id client-id
                             :response_type "code"
                             :redirect_uri REDIRECT_URI
                             :scope SCOPES
                             :code_challenge challenge
                             :code_challenge_method "S256"
                             :state state
                             :access_type "offline"
                             :prompt "consent"}))}))

(defn auth-start
  "Headless leg 1. `:flow` holds the PKCE verifier and state: a daemon-side
   secret that never goes on the wire."
  []
  (let [{:keys [url] :as flow} (create-authorization-flow)]
    {:kind :pkce
     :url url
     :redirect-uri REDIRECT_URI
     :instructions ["Sign in with Google in the browser, then return to Vis."
                    "If automatic return is unavailable, paste the final browser URL."]
     :flow flow}))

(defn auth-complete
  "Headless leg 2: exchange the callback URL or code and persist the credentials."
  [flow input]
  (when (str/blank? (or input ""))
    (throw (ex-info "Missing authorization input" {:type :vis/antigravity-missing-input})))
  (let [{:keys [code state]} (parse-authorization-input input)]
    (save-auth-file! (exchange-authorization-code! code flow state))
    {:status :ok}))

(defn auth-instruction-lines
  []
  ["Google Antigravity OAuth." "" "CLI OAuth command:" "  vis-agent providers auth antigravity"])

(defn authenticated? [] (boolean (:access-token (detect-credentials))))

(defn- open-browser! [url] (= :ok (:status (opener/open! url))))

(defn- prompt-for-code!
  [printer-fn]
  (printer-fn "")
  (printer-fn "  Paste the full redirect URL (http://localhost:51121/...), then press Enter:")
  (read-line))

(defn login!
  "Terminal sign-in: open the Google page, then read the pasted redirect URL.
   Options: `:open-browser-fn`, `:manual-code-fn`, `:force?`."
  ([printer-fn] (login! printer-fn {}))
  ([printer-fn
    {:keys [open-browser-fn manual-code-fn force?]
     :or {open-browser-fn open-browser! manual-code-fn prompt-for-code!}}]
   (let [print! (or printer-fn (constantly nil))]
     (if (and (not force?) (authenticated?))
       (do (print! "  Already authenticated with Google Antigravity.")
           (print! "  Run `vis-agent providers logout antigravity` first to re-authenticate.")
           :already-authenticated)
       (let [{:keys [url] :as flow} (create-authorization-flow)]
         (print! "")
         (print! "  Google Antigravity authentication")
         (print! "  ---------------------------------")
         (print! "  Open this URL if your browser does not open automatically:")
         (print! (str "  " url))
         (print! "")
         (print! (if (open-browser-fn url)
                   "  Browser opened. After sign-in, copy the final browser URL and paste it here."
                   "  Open the URL manually, then paste the final browser URL here."))
         (when-not manual-code-fn
           (throw (ex-info "Manual code entry is disabled for this flow."
                           {:type :vis/antigravity-manual-entry-disabled})))
         (auth-complete flow (manual-code-fn print!))
         (print! (str "  ✓ Authenticated! Google Antigravity is ready"
                      (some->> (:email (load-auth-file))
                               (str " (")
                               (#(str % ")")))
                      "."))
         :ok)))))

(defn status
  []
  (let [detected (detect-credentials)]
    (if (= :rejected (:auth-state detected))
      detected
      (cond-> {:is-authenticated (some? detected)}
        detected
        (assoc :source
          (:source detected) :account-id
          (:email detected))

        (:expires-at-ms detected)
        (assoc :expires-in-ms (- (long (:expires-at-ms detected)) (util/now-ms)))))))

(defn logout! [] (delete-auth-file!) :logged-out)

(defn register!
  []
  (extension/register-extension!
    (extension/extension
      {:ext/name "provider-antigravity"
       :ext/description
       "Google Antigravity OAuth provider (Gemini and Claude via Cloud Code Assist)."
       :ext/version "0.1.0"
       :ext/author "Blockether"
       :ext/owner "vis"
       :ext/license "Apache-2.0"
       :ext/providers [{:provider/id PROVIDER_ID
                        :provider/label "Google Antigravity (OAuth)"
                        :provider/preset {:default-models (svar/provider-default-models
                                                            PROVIDER_ID)}
                        :provider/policy {:preset-rank 12}
                        :provider/status-fn #'status
                        :provider/logout-fn #'logout!
                        :provider/detect-fn #'detect-credentials
                        :provider/auth-fn #'login!
                        :provider/auth-start-fn #'auth-start
                        :provider/auth-complete-fn #'auth-complete
                        :provider/auth-prompt-fn #'auth-instruction-lines
                        :provider/limits-fn #'fetch-limits-report
                        :provider/get-token-fn #'get-antigravity-token!
                        :provider/refresh-token-fn #'force-refresh-token!}]})))
