(ns com.blockether.vis.ext.provider-anthropic
  "Anthropic providers.

   Providers:
   - `:anthropic` — normal Anthropic API key provider. API key lives in Vis config.
   - `:anthropic-coding-plan` — Claude subscription OAuth provider. OAuth
     credentials live in `~/.vis/anthropic-auth.json`.

   Runtime calls hand the OAuth access token to svar; svar handles only the
   Anthropic Messages API wire differences for subscription tokens."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [java.net URLDecoder URLEncoder]
           [java.security MessageDigest SecureRandom]
           [java.util Base64]))

(def ^:private client-id
  (String. (.decode (Base64/getDecoder)
             "OWQxYzI1MGEtZTYxYi00NGQ5LTg4ZWQtNTk0NGQxOTYyZjVl")
    java.nio.charset.StandardCharsets/UTF_8))

(def ^:private authorize-url "https://claude.ai/oauth/authorize")
(def ^:private token-url "https://platform.claude.com/v1/oauth/token")
(def ^:private redirect-uri "http://localhost:53692/callback")
(def ^:private scopes "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload")
(def ^:private auth-file (str (System/getProperty "user.home") "/.vis/anthropic-auth.json"))
(def ^:private anthropic-base-url "https://api.anthropic.com/v1")
(def ^:private default-models ["claude-opus-4-7" "claude-opus-4-6" "claude-sonnet-4-6" "claude-haiku-4-5"])
(def ^:private ^:const refresh-margin-ms 300000)
(def ^:private secure-random (delay (SecureRandom.)))

(defn- random-bytes [n]
  (let [bytes (byte-array n)]
    (.nextBytes ^SecureRandom @secure-random bytes)
    bytes))

(defn- base64url [^bytes bytes]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn- sha256 [^String s]
  (.digest (MessageDigest/getInstance "SHA-256")
    (.getBytes s java.nio.charset.StandardCharsets/UTF_8)))

(defn- url-encode [v]
  (URLEncoder/encode (str v) "UTF-8"))

(defn- url-decode [^String v]
  (URLDecoder/decode v "UTF-8"))

(defn- query-string [params]
  (str/join "&"
    (map (fn [[k v]] (str (url-encode (name k)) "=" (url-encode v)))
      params)))

(defn- parse-query-string [^String qs]
  (when-not (str/blank? qs)
    (into {}
      (keep (fn [part]
              (let [[k v] (str/split part #"=" 2)]
                (when-not (str/blank? k)
                  [(keyword (url-decode k)) (url-decode (or v ""))]))))
      (str/split qs #"&"))))

(defn parse-authorization-input
  "Parse a pasted Anthropic OAuth callback URL, query string, `code#state`, or bare code."
  [input]
  (let [value (str/trim (or input ""))]
    (cond
      (str/blank? value) {}

      (str/starts-with? value "http")
      (try
        (let [[head fragment] (str/split value #"#" 2)
              q-idx           (.indexOf ^String head "?")
              query           (when (<= 0 q-idx) (subs head (inc q-idx)))
              params          (merge (parse-query-string query)
                                (if (and fragment (str/includes? fragment "="))
                                  (parse-query-string fragment)
                                  (when fragment {:state fragment})))]
          (select-keys params [:code :state]))
        (catch Exception _ {:code value}))

      (str/includes? value "#")
      (let [[code state] (str/split value #"#" 2)]
        (cond-> {:code code}
          (not (str/blank? state)) (assoc :state state)))

      (str/includes? value "code=")
      (select-keys (parse-query-string value) [:code :state])

      :else {:code value})))

(defn create-authorization-flow
  "Create Anthropic Claude subscription OAuth PKCE flow data."
  []
  (let [verifier  (base64url (random-bytes 32))
        challenge (base64url (sha256 verifier))
        query     (query-string {:code                  "true"
                                 :client_id             client-id
                                 :response_type         "code"
                                 :redirect_uri          redirect-uri
                                 :scope                 scopes
                                 :code_challenge        challenge
                                 :code_challenge_method "S256"
                                 :state                 verifier})]
    {:verifier verifier
     :state    verifier
     :url      (str authorize-url "?" query)}))

(defn- post-token [body]
  (let [resp (http/post token-url
               {:headers {"Accept" "application/json"
                          "Content-Type" "application/json"}
                :body    (json/write-json-str body)
                :timeout 30000
                :throw   false})
        text (:body resp)]
    {:status (:status resp)
     :body   text
     :json   (try (json/read-json text :key-fn keyword)
               (catch Exception _ nil))}))

(defn- credentials-result [json]
  (let [access-token  (:access_token json)
        refresh-token (:refresh_token json)
        expires-in    (:expires_in json)]
    (when (or (str/blank? access-token)
            (str/blank? refresh-token)
            (not (number? expires-in)))
      (throw (ex-info "Anthropic token response missing required fields"
               {:response (dissoc json :access_token :refresh_token)})))
    {:access-token  access-token
     :refresh-token refresh-token
     :expires-at-ms (- (+ (System/currentTimeMillis) (* (long expires-in) 1000))
                      refresh-margin-ms)}))

(defn- exchange-authorization-code! [code {:keys [verifier state]} returned-state]
  (when (str/blank? code)
    (throw (ex-info "Missing Anthropic authorization code" {:type :vis/anthropic-missing-code})))
  (when (str/blank? verifier)
    (throw (ex-info "Missing Anthropic PKCE verifier" {:type :vis/anthropic-missing-verifier})))
  (when (and (not (str/blank? returned-state)) (not= state returned-state))
    (throw (ex-info "Anthropic OAuth state mismatch"
             {:type :vis/anthropic-state-mismatch
              :expected state
              :actual returned-state})))
  (let [{:keys [status body json]} (post-token {:grant_type    "authorization_code"
                                                :client_id     client-id
                                                :code          code
                                                :state         (or returned-state state verifier)
                                                :redirect_uri  redirect-uri
                                                :code_verifier verifier})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "Anthropic token exchange failed: HTTP " status)
               {:type :vis/anthropic-token-exchange-failed
                :status status
                :body body})))
    (credentials-result json)))

(defn- refresh-access-token! [refresh-token]
  (let [{:keys [status body json]} (post-token {:grant_type    "refresh_token"
                                                :client_id     client-id
                                                :refresh_token refresh-token})]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "Anthropic token refresh failed: HTTP " status)
               {:type :vis/anthropic-token-refresh-failed
                :status status
                :body body})))
    (credentials-result json)))

(defn- auth-dir []
  (io/file (str (System/getProperty "user.home") "/.vis")))

(defn- load-auth-file []
  (let [f (io/file auth-file)]
    (when (.exists f)
      (try (json/read-json (slurp f) :key-fn keyword)
        (catch Exception _ nil)))))

(defn- save-auth-file! [credentials]
  (let [dir (auth-dir)]
    (when-not (.exists dir) (.mkdirs dir))
    (spit auth-file (json/write-json-str (assoc credentials :saved-at-ms (System/currentTimeMillis))))
    credentials))

(defn- delete-auth-file! []
  (let [f (io/file auth-file)]
    (when (.exists f) (.delete f))))

(defn detect-credentials
  "Detect persisted Anthropic OAuth credentials without network validation."
  []
  (when-let [auth (load-auth-file)]
    (when-not (str/blank? (:access-token auth))
      {:access-token  (:access-token auth)
       :source        :auth-file
       :expires-at-ms (:expires-at-ms auth)})))

(defn get-anthropic-token!
  "Return a fresh Anthropic Claude subscription access token for Vis runtime."
  []
  (let [auth (load-auth-file)
        now  (System/currentTimeMillis)]
    (cond
      (and (:access-token auth)
        (:expires-at-ms auth)
        (> (long (:expires-at-ms auth)) (+ now refresh-margin-ms)))
      {:token (:access-token auth)}

      (:refresh-token auth)
      (let [fresh (refresh-access-token! (:refresh-token auth))]
        (save-auth-file! fresh)
        {:token (:access-token fresh)})

      :else
      (throw (ex-info "No Anthropic OAuth credentials found. Run `vis providers auth anthropic-coding-plan` to authenticate."
               {:type :vis/anthropic-not-authenticated})))))

(defn authenticated? []
  (some? (detect-credentials)))

(defn status []
  (let [detected (detect-credentials)
        now      (System/currentTimeMillis)]
    (cond-> {:authenticated? (some? detected)}
      detected (assoc :source (:source detected)
                 :oauth-token-preview (let [t (:access-token detected)]
                                        (str (subs t 0 (min 8 (count t))) "...")))
      (:expires-at-ms detected) (assoc :expires-in-ms (- (long (:expires-at-ms detected)) now)))))

(defn logout! []
  (delete-auth-file!)
  :logged-out)

(defn auth-instruction-lines []
  ["Anthropic Claude subscription OAuth."
   ""
   "CLI OAuth command:"
   "  vis providers auth anthropic-coding-plan"])

(defn- open-browser! [url]
  (= :ok (:status (opener/open! url))))

(defn- prompt-for-code! [printer-fn]
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
  ([printer-fn {:keys [open-browser-fn manual-code-fn force?]
                :or   {open-browser-fn open-browser!
                       manual-code-fn  prompt-for-code!}}]
   (let [print! (or printer-fn (constantly nil))]
     (if (and (not force?) (detect-credentials))
       (do
         (print! "  Already authenticated with Anthropic Claude subscription.")
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
           (print! "  Browser auto-open failed; open the URL manually, then paste the final browser URL here."))
         (when-not manual-code-fn
           (throw (ex-info "Manual code entry is disabled for this flow. Run `vis providers auth anthropic-coding-plan` in a terminal or use a frontend that can collect the redirect URL."
                    {:type :vis/anthropic-manual-entry-disabled})))
         (let [input  (manual-code-fn print!)
               parsed (parse-authorization-input input)
               code   (:code parsed)]
           (when (str/blank? (or input ""))
             (throw (ex-info "Missing authorization input" {:type :vis/anthropic-missing-input})))
           (let [credentials (save-auth-file! (exchange-authorization-code! code flow (:state parsed)))]
             (print! "  ✓ Authenticated! Anthropic Claude subscription is ready.")
             (when (str/blank? (:access-token credentials))
               (throw (ex-info "Anthropic credentials missing access token" {:type :vis/anthropic-missing-access-token})))
             :ok)))))))

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-anthropic
     :ext/doc       "Anthropic API-key and Claude subscription OAuth providers."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/providers [{:provider/id     :anthropic
                      :provider/label  "Anthropic (API Key)"
                      :provider/preset {:default-models default-models}}
                     {:provider/id             :anthropic-coding-plan
                      :provider/label          "Anthropic (Claude Subscription)"
                      :provider/preset         {:base-url anthropic-base-url
                                                :api-style :anthropic
                                                :default-models default-models}
                      :provider/status-fn      #'status
                      :provider/logout-fn      #'logout!
                      :provider/detect-fn      #'detect-credentials
                      :provider/auth-fn        #'login!
                      :provider/auth-prompt-fn #'auth-instruction-lines
                      :provider/get-token-fn   #'get-anthropic-token!}]}))
