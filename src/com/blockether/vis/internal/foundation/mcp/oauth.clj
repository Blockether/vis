(ns com.blockether.vis.internal.foundation.mcp.oauth
  "OAuth 2.1 client for Model Context Protocol servers (spec `2025-06-18`).

   Flow — on HTTP 401 from an MCP server, we:
     1. read `WWW-Authenticate: Bearer resource_metadata=\"...\"` (RFC 9728),
        fall back to `${origin}/.well-known/oauth-protected-resource`;
     2. GET the resource-metadata JSON → pick an `authorization_servers[0]`;
     3. GET its `.well-known/oauth-authorization-server` (RFC 8414) or
        `.well-known/openid-configuration` for endpoints + capabilities;
     4. dynamic-client-register (RFC 7591) if the AS supports it, or use
        the caller-supplied `client_id`;
     5. run PKCE S256 authorization-code with a loopback redirect
        (`http://127.0.0.1:<ephemeral>/mcp-callback`) — spawn a one-shot
        `com.sun.net.httpserver.HttpServer`, print the URL, best-effort open
        it in the user's browser, wait for `?code=`;
     6. exchange code → access + refresh tokens; persist to
        `~/.vis/mcp-tokens/<server>.edn`;
     7. on later expiry / 401, refresh the token single-flight through
        `com.blockether.vis.internal.oauth/make-file-refresher`.

   The returned `bearer-fn` is a 0/1-arg function: 0-arg yields the current
   bearer token (running the auth flow on first use); 1-arg (with the token the
   server just rejected) forces a refresh."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.http :as mcp-http]
            [com.blockether.vis.internal.oauth :as oauth]
            [taoensso.telemere :as tel])
  (:import
    (com.sun.net.httpserver HttpHandler HttpServer)
    (java.awt Desktop Desktop$Action)
    (java.net InetSocketAddress URI URLDecoder URLEncoder)
    (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
    (java.security MessageDigest SecureRandom)
    (java.time Duration)
    (java.util Base64)))

(def ^:private protocol-version-header "MCP-Protocol-Version")
(def ^:private protocol-version "2025-06-18")

;; HTTP client is shared with `client.clj` via `mcp-http/client` — see
;; `mcp/http.clj` for the perf/threading rationale (one JDK HttpClient, one
;; selector pool, virtual-thread executor).

(defn- b64url
  ^String [^bytes bs]
  (-> (Base64/getUrlEncoder)
      .withoutPadding
      (.encodeToString bs)))

(defn- sha256
  ^bytes [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(defn- rand-bytes
  ^bytes [n]
  (let [b (byte-array n)]
    (.nextBytes (SecureRandom.) b)
    b))

(defn- form-encode
  ^String [m]
  (->> m
       (map (fn [[k v]]
              (str (URLEncoder/encode (str k) "UTF-8") "=" (URLEncoder/encode (str v) "UTF-8"))))
       (str/join "&")))

(defn- query-parse
  [^String q]
  (when (seq q)
    (into {}
          (for
            [p
             (str/split q #"&")

             :let [[k v]
                   (str/split p #"=" 2)

                   ^String kk
                   (str k)

                   ^String vv
                   (str (or v ""))]
             :when (seq k)]

            [(URLDecoder/decode kk "UTF-8") (URLDecoder/decode vv "UTF-8")]))))

(defn- http-get-json
  [^String url]
  (let
    [req
     (-> (HttpRequest/newBuilder (URI/create url))
         (.header "Accept" "application/json")
         (.header protocol-version-header protocol-version)
         (.timeout (Duration/ofSeconds 15))
         (.GET)
         .build)

     resp
     (.send ^HttpClient @mcp-http/client req (HttpResponse$BodyHandlers/ofString))]

    (when (< (.statusCode resp) 400) (try (json/read-json (.body resp)) (catch Throwable _ nil)))))

(defn- http-post-form
  [^String url form]
  (let
    [req
     (-> (HttpRequest/newBuilder (URI/create url))
         (.header "Content-Type" "application/x-www-form-urlencoded")
         (.header "Accept" "application/json")
         (.header protocol-version-header protocol-version)
         (.timeout (Duration/ofSeconds 30))
         (.POST (HttpRequest$BodyPublishers/ofString (form-encode form)))
         .build)

     resp
     (.send ^HttpClient @mcp-http/client req (HttpResponse$BodyHandlers/ofString))]

    {:status (.statusCode resp)
     :body (try (json/read-json (.body resp)) (catch Throwable _ (.body resp)))}))

(defn- http-post-json
  [^String url body]
  (let
    [req
     (-> (HttpRequest/newBuilder (URI/create url))
         (.header "Content-Type" "application/json")
         (.header "Accept" "application/json")
         (.header protocol-version-header protocol-version)
         (.timeout (Duration/ofSeconds 30))
         (.POST (HttpRequest$BodyPublishers/ofString (json/write-json-str body)))
         .build)

     resp
     (.send ^HttpClient @mcp-http/client req (HttpResponse$BodyHandlers/ofString))]

    {:status (.statusCode resp)
     :body (try (json/read-json (.body resp)) (catch Throwable _ (.body resp)))}))

;; ---------------------------------------------------------------------------
;; Discovery (RFC 9728 + RFC 8414 fallbacks)
;; ---------------------------------------------------------------------------

(defn- parse-www-authenticate
  "Extract `resource_metadata` URL from a `WWW-Authenticate: Bearer …` header."
  [h]
  (when (seq h)
    (some->> (re-find #"(?i)resource_metadata\s*=\s*\"([^\"]+)\"" h)
             second)))

(defn- origin-of
  [^String url]
  (let [u (URI/create url)]
    (str (.getScheme u) "://" (.getAuthority u))))

(defn- discover-protected-resource
  [server-url www-auth]
  (let
    [candidates (->> [(parse-www-authenticate www-auth)
                      (str (origin-of server-url) "/.well-known/oauth-protected-resource")]
                     (remove str/blank?)
                     distinct)]
    (some (fn [u]
            (when-let [meta (http-get-json u)]
              (assoc meta ::url u)))
          candidates)))

(defn- discover-authorization-server
  [as-url]
  (let
    [candidates [(str as-url "/.well-known/oauth-authorization-server")
                 (str as-url "/.well-known/openid-configuration")]]
    (some (fn [u]
            (some-> (http-get-json u)
                    (assoc ::url u)))
          candidates)))

;; ---------------------------------------------------------------------------
;; Dynamic Client Registration (RFC 7591)
;; ---------------------------------------------------------------------------

(defn- register-client!
  [{registration-url "registration_endpoint"} redirect-uri]
  (when registration-url
    (let
      [{:keys [status body]} (http-post-json registration-url
                                             {"client_name" "vis MCP client"
                                              "redirect_uris" [redirect-uri]
                                              "grant_types" ["authorization_code" "refresh_token"]
                                              "response_types" ["code"]
                                              "token_endpoint_auth_method" "none"
                                              "scope" "openid profile offline_access"})]
      (when (and (< (long status) 400) (map? body) (get body "client_id")) body))))

;; ---------------------------------------------------------------------------
;; Loopback callback (PKCE authorization-code)
;; ---------------------------------------------------------------------------

(defn- open-browser!
  [^String url]
  (try (let [dt (Desktop/getDesktop)]
         (when (.isSupported dt Desktop$Action/BROWSE) (.browse dt (URI/create url)) true))
       (catch Throwable _ false)))

(defn- await-loopback-code!
  "Bind a one-shot HTTP server on 127.0.0.1, print the authorization URL, best-effort
   open it, and block up to `timeout-ms` for the OAuth `code`. Returns
   `{:code :state}` or throws `:mcp/oauth-timeout`."
  [server-name authorize-url-fn timeout-ms]
  (let
    [addr
     (InetSocketAddress. "127.0.0.1" 0)

     srv
     (HttpServer/create addr 0)

     port
     (.getPort (.getAddress srv))

     redirect-uri
     (str "http://127.0.0.1:" port "/mcp-callback")

     result
     (promise)]

    (.createContext
      srv
      "/mcp-callback"
      (reify
        HttpHandler
          (handle [_ ex]
            (let
              [q
               (query-parse (.getRawQuery (.getRequestURI ex)))

               html
               (str "<!doctype html><meta charset=utf-8>"
                    "<title>MCP auth</title><body style=\""
                    "font-family:sans-serif;padding:2em\">"
                    (if (get q "code")
                      (str "<h2>Authorized " server-name
                           "</h2>" "<p>You can close this tab and return to vis.</p>")
                      (str "<h2>Auth failed</h2><pre>" (pr-str q) "</pre>"))
                    "</body>")

               bs
               (.getBytes html "UTF-8")]

              (.set (.getResponseHeaders ex) "Content-Type" "text/html; charset=utf-8")
              (.sendResponseHeaders ex 200 (alength bs))
              (with-open [os (.getResponseBody ex)]
                (.write os bs))
              (.close ex)
              (deliver result q)))))
    (.setExecutor srv nil)
    (.start srv)
    (try (let [url (authorize-url-fn redirect-uri)]
           (tel/log!
             {:level :info :id ::authorize :data {:server server-name :redirect_uri redirect-uri}}
             (str "MCP OAuth: open this URL to authorize `" server-name "`:\n  " url))
           (open-browser! url)
           (let [q (deref result timeout-ms ::timeout)]
             (cond (= q ::timeout)
                   (throw (ex-info (str "MCP " server-name " OAuth authorization timed out")
                                   {:type :mcp/oauth-timeout :server server-name}))
                   (get q "error")
                   (throw (ex-info (str "MCP " server-name " OAuth error: " (get q "error"))
                                   {:type :mcp/oauth-error :server server-name :query q}))
                   :else {:code (get q "code") :state (get q "state") :redirect-uri redirect-uri})))
         (finally (.stop srv 0)))))

;; ---------------------------------------------------------------------------
;; Token store — one EDN file per server under ~/.vis/mcp-tokens/
;; ---------------------------------------------------------------------------

(defn- token-file
  ^java.io.File [server-name]
  (io/file (System/getProperty "user.home") ".vis" "mcp-tokens" (str server-name ".edn")))

(defn- read-tokens
  [server-name]
  (let [f (token-file server-name)]
    (when (.exists f)
      (try (with-open [r (io/reader f)]
             (read (java.io.PushbackReader. r)))
           (catch Throwable _ nil)))))

(defn- write-tokens!
  [server-name m]
  (let [f (token-file server-name)]
    (io/make-parents f)
    (spit f (pr-str m))
    (try (.setReadable f false false)
         (.setReadable f true true)
         (.setWritable f false false)
         (.setWritable f true true)
         (catch Throwable _ nil))
    m))

(defn- expired?
  "Skew the reported expiry by 60s so we refresh proactively, never at the
   exact instant of the 401."
  [{:keys [expires-at-ms]}]
  (or (nil? expires-at-ms) (< (- (long expires-at-ms) 60000) (System/currentTimeMillis))))

(defn- ->tokens
  "Normalize a token endpoint response into `{:token :refresh-token :expires-at-ms
   :saved-at-ms :client-id}`."
  [body extra]
  (let
    [now
     (System/currentTimeMillis)

     expires-in
     (some-> (get body "expires_in")
             long)]

    (merge extra
           {:token (get body "access_token")
            :refresh-token (or (get body "refresh_token") (:refresh-token extra))
            :token-type (or (get body "token_type") "Bearer")
            :scope (get body "scope")
            :expires-at-ms (when expires-in (+ now (long (* 1000 (long expires-in)))))
            :saved-at-ms now})))

;; ---------------------------------------------------------------------------
;; Auth-code flow (first time)
;; ---------------------------------------------------------------------------

(defn- authorize-code!
  "Run the full RFC 9728 → RFC 8414 → PKCE authorization-code dance for `server-name`
   against `server-url`. `www-auth` is the raw `WWW-Authenticate` header from the
   401 (may be nil — we still try the well-known URL). `auth-hint` is optional user
   config: `{:client-id :scope :authorization-timeout-ms}`. Persists tokens and
   returns the token map."
  [server-name server-url www-auth auth-hint]
  (let
    [rmeta
     (or (discover-protected-resource server-url www-auth)
         (throw (ex-info
                  (str "MCP " server-name ": no OAuth protected-resource metadata discoverable")
                  {:type :mcp/oauth-discovery :server server-name})))

     as-url
     (or (first (get rmeta "authorization_servers"))
         (throw (ex-info (str "MCP "
                              server-name
                              ": protected-resource metadata has no authorization_servers")
                         {:type :mcp/oauth-discovery :server server-name})))

     asmeta
     (or (discover-authorization-server as-url)
         (throw (ex-info (str "MCP " server-name ": AS metadata not discoverable at " as-url)
                         {:type :mcp/oauth-discovery :server server-name})))

     verifier
     (b64url (rand-bytes 32))

     challenge
     (b64url (sha256 verifier))

     state
     (b64url (rand-bytes 16))

     resource
     (get rmeta "resource")

     scope
     (or (:scope auth-hint)
         (some->> (get rmeta "scopes_supported")
                  seq
                  (str/join " "))
         "openid profile offline_access")]

    (letfn
      [(authorize-url [redirect-uri]
         (let
           [reg
            (when-not (:client-id auth-hint) (register-client! asmeta redirect-uri))

            client-id
            (or (:client-id auth-hint) (get reg "client_id"))

            params
            (cond->
              {"response_type" "code"
               "client_id" client-id
               "redirect_uri" redirect-uri
               "state" state
               "code_challenge" challenge
               "code_challenge_method" "S256"
               "scope" scope}
              resource
              (assoc "resource" resource))]

           (assert
             client-id
             "MCP OAuth: no client_id (auth server does not support DCR; set :auth :client_id in config)")
           (str (get asmeta "authorization_endpoint")
                (if (str/includes? (get asmeta "authorization_endpoint") "?") "&" "?")
                (form-encode params))))]
      (let
        [{:keys [code state redirect-uri]}
         (await-loopback-code! server-name
                               authorize-url
                               (or (:authorization-timeout-ms auth-hint) 300000))

         _
         (when-not (seq code)
           (throw (ex-info "MCP OAuth: no code in callback"
                           {:type :mcp/oauth-error :server server-name :state state})))

         reg
         (when-not (:client-id auth-hint) (register-client! asmeta redirect-uri))

         client-id
         (or (:client-id auth-hint) (get reg "client_id"))

         token-url
         (get asmeta "token_endpoint")

         {:keys [status body]}
         (http-post-form token-url
                         (cond->
                           {"grant_type" "authorization_code"
                            "code" code
                            "redirect_uri" redirect-uri
                            "client_id" client-id
                            "code_verifier" verifier}
                           resource
                           (assoc "resource" resource)))]

        (when (>= (long status) 400)
          (throw (ex-info (str "MCP " server-name " token exchange failed: " status)
                          {:type :mcp/oauth-token :server server-name :status status :body body})))
        (write-tokens! server-name
                       (->tokens body
                                 {:client-id client-id
                                  :token-endpoint token-url
                                  :authorization-server as-url
                                  :resource resource}))))))

(defn- refresh-token-exchange!
  "Refresh an access token via the token endpoint. May rotate the refresh token."
  [server-name creds]
  (let
    [{:keys [status body]} (http-post-form (:token-endpoint creds)
                                           (cond->
                                             {"grant_type" "refresh_token"
                                              "refresh_token" (:refresh-token creds)
                                              "client_id" (:client-id creds)}
                                             (:resource creds)
                                             (assoc "resource" (:resource creds))))]
    (when (>= (long status) 400)
      (throw (ex-info (str "MCP " server-name " token refresh failed: " status)
                      {:type :mcp/oauth-refresh :server server-name :status status :body body})))
    (write-tokens! server-name (->tokens body creds))))

;; ---------------------------------------------------------------------------
;; Public: a 0/1-arg `bearer-fn` for the HTTP transport
;; ---------------------------------------------------------------------------

(defn make-bearer-fn
  "Build a 0/1-arg fn returning the current Bearer token string for `server-name`.
   On first use (no cached tokens) OR when called with the just-rejected token,
   it runs the OAuth flow / refresh under a single-flight lock and persists the
   new tokens. `server-url` is the MCP endpoint. `www-auth-atom` is an atom the
   HTTP transport keeps updated with the latest `WWW-Authenticate` header (so we
   discover from the LIVE 401). `auth-hint` is user config from `:mcp :servers
   <name> :auth`."
  [server-name server-url www-auth-atom auth-hint]
  (oauth/refresher
    (fn [rejected]
      (let [creds (read-tokens server-name)]
        (when (and creds (:token creds) (not (expired? creds)) (not= rejected (:token creds)))
          (:token creds))))
    (fn []
      (let
        [creds
         (read-tokens server-name)

         fresh
         (cond (and creds (:refresh-token creds))
               (try (refresh-token-exchange! server-name creds)
                    (catch Throwable _
                      (authorize-code! server-name server-url @www-auth-atom auth-hint)))
               :else (authorize-code! server-name server-url @www-auth-atom auth-hint))]

        (:token fresh)))))

(defn forget!
  "Drop persisted tokens for `server-name` (e.g. on `mcp__disconnect`, or when a
   401 recurs after refresh)."
  [server-name]
  (let [f (token-file server-name)]
    (when (.exists f) (.delete f))))
