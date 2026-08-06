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
        (`http://127.0.0.1:<ephemeral>/mcp-callback`) — HEADLESS: `start-authorization!`
        binds the one-shot listener and RETURNS the URL for whoever is
        authorizing (often on a different device than the daemon), and
        `finish-authorization!` lands the code either from that listener or from
        a redirect URL pasted back. Nothing blocks waiting for a human;
     6. exchange code → access + refresh tokens; persist to
        `~/.vis/mcp-tokens/<server>.edn`;
     7. on later expiry / 401, refresh the token single-flight through
        `com.blockether.vis.internal.oauth/make-file-refresher`.

   The returned `bearer-fn` is a 0/1-arg function: 0-arg yields the current
   bearer token; 1-arg (with the token the server just rejected) forces a
   refresh. It never opens a browser and never waits — with nothing to refresh
   it throws `:mcp/oauth-required`, which callers turn into `sign in`."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.http :as mcp-http]
            [com.blockether.vis.internal.oauth :as oauth]
            [taoensso.telemere :as tel])
  (:import (com.sun.net.httpserver HttpHandler HttpServer)
           (java.net InetSocketAddress URI URLDecoder URLEncoder)
           (java.security MessageDigest SecureRandom)
           (java.util Base64)))

(def ^:private protocol-version-header "MCP-Protocol-Version")

(def ^:private protocol-version "2025-06-18")

;; The lazy babashka.http-client instance is shared with `client.clj` through
;; `mcp-http/client`.

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
    [resp
     (http/request {:uri url
                    :method :get
                    :client @mcp-http/client
                    :headers {"Accept" "application/json" protocol-version-header protocol-version}
                    :timeout 15000
                    :throw false
                    :as :string})

     status
     (long (:status resp))]

    (when (< status 400) (try (json/read-json (:body resp)) (catch Throwable _ nil)))))

(defn- http-post-form
  [^String url form]
  (let
    [resp (http/request {:uri url
                         :method :post
                         :client @mcp-http/client
                         :headers {"Content-Type" "application/x-www-form-urlencoded"
                                   "Accept" "application/json"
                                   protocol-version-header protocol-version}
                         :body (form-encode form)
                         :timeout 30000
                         :throw false
                         :as :string})]
    {:status (:status resp)
     :body (try (json/read-json (:body resp)) (catch Throwable _ (:body resp)))}))

(defn- http-post-json
  [^String url body]
  (let
    [resp (http/request {:uri url
                         :method :post
                         :client @mcp-http/client
                         :headers {"Content-Type" "application/json"
                                   "Accept" "application/json"
                                   protocol-version-header protocol-version}
                         :body (json/write-json-str body)
                         :timeout 30000
                         :throw false
                         :as :string})]
    {:status (:status resp)
     :body (try (json/read-json (:body resp)) (catch Throwable _ (:body resp)))}))

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


(defn- start-loopback!
  "Bind a one-shot loopback callback listener on 127.0.0.1. Returns
   `{:server :port :redirect-uri :result}`; `result` is a promise delivered with
   the parsed callback query the moment a browser ON THIS HOST reaches it.

   Kept apart from the token exchange because authorization must never block a
   request path: the listener goes up, the URL goes out, and the person
   authorizing may be on a phone somewhere else."
  [server-name]
  (let
    [srv
     (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

     port
     (.getPort (.getAddress srv))

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
    {:server srv
     :port port
     :redirect-uri (str "http://127.0.0.1:" port "/mcp-callback")
     :result result}))


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

(defn- auth-context
  "Discovery (RFC 9728 → RFC 8414) plus fresh PKCE material for `server-name`.
   Shared by the loopback flow and the headless flow so both speak exactly the
   same protocol and only the redirect leg differs. `www-auth` is the raw
   `WWW-Authenticate` header from the 401 (may be nil — we still try the
   well-known URL). `auth-hint` is optional user config:
   `{:client-id :scope :authorization-timeout-ms}`."
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
     (b64url (rand-bytes 32))]

    {:server server-name
     :as-url as-url
     :asmeta asmeta
     :verifier verifier
     :challenge (b64url (sha256 verifier))
     :state (b64url (rand-bytes 16))
     :resource (get rmeta "resource")
     :scope (or (:scope auth-hint)
                (some->> (get rmeta "scopes_supported")
                         seq
                         (str/join " "))
                "openid profile offline_access")}))

(defn- authorize-url
  "The authorization-endpoint URL for `ctx` at `redirect-uri`, plus the `client_id`
   it was built for. Dynamic client registration happens HERE, once, and the id is
   returned: registering again before the token exchange can hand back a DIFFERENT
   client, which the authorization server then rejects the code for."
  [{:keys [asmeta challenge state resource scope server]} auth-hint redirect-uri]
  (let
    [reg
     (when-not (:client-id auth-hint) (register-client! asmeta redirect-uri))

     client-id
     (or (:client-id auth-hint) (get reg "client_id"))

     _
     (when-not client-id
       (throw (ex-info (str "MCP " server
                            ": the authorization server supports no dynamic client registration; "
                            "set `auth: {client_id: …}` on the server")
                       {:type :mcp/oauth-client :server server})))

     endpoint
     (get asmeta "authorization_endpoint")

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

    {:client-id client-id
     :url (str endpoint (if (str/includes? endpoint "?") "&" "?") (form-encode params))}))

(defn- exchange-code!
  "Spend `code` at the token endpoint and PERSIST the resulting tokens."
  [{:keys [server as-url asmeta verifier resource]} client-id code redirect-uri]
  (let
    [token-url
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
      (throw (ex-info (str "MCP " server " token exchange failed: " status)
                      {:type :mcp/oauth-token :server server :status status :body body})))
    (write-tokens! server
                   (->tokens body
                             {:client-id client-id
                              :token-endpoint token-url
                              :authorization-server as-url
                              :resource resource}))))


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

(defn- oauth-required
  "The one refusal for `server-name` when only a HUMAN can move things forward:
   nothing is persisted yet, or the refresh token stopped working. Callers turn
   this into `sign in`, never into `the server is broken` - and never into a
   five-minute wait."
  [server-name server-url www-auth cause]
  (ex-info (str "MCP server '" server-name "' is not authorized - start the OAuth flow to sign in")
           {:type :mcp/oauth-required
            :server server-name
            :server-url server-url
            :www-authenticate www-auth}
           cause))

(defn make-bearer-fn
  "Build a 0/1-arg fn returning the current Bearer token string for `server-name`.
   0-arg yields the persisted token; 1-arg (the token the server just rejected)
   forces a single-flight refresh. `server-url` is the MCP endpoint and
   `www-auth-atom` is the atom the HTTP transport keeps pointed at the latest
   `WWW-Authenticate` header, so a refusal can carry the LIVE 401 details.

   It never runs an interactive authorization. Every caller sits on a path a
   human is waiting on - a tool call, a reconcile tick, the health loop - and the
   browser dance blocked each of them for up to five minutes on a server nobody
   had signed into yet, which looked exactly like a hung daemon. With nothing to
   refresh it throws `:mcp/oauth-required` immediately; authorizing is the
   explicit headless flow below, which hands back a URL instead of waiting for
   one."
  [server-name server-url www-auth-atom]
  (oauth/refresher
    (fn [rejected]
      (let [creds (read-tokens server-name)]
        (when (and creds (:token creds) (not (expired? creds)) (not= rejected (:token creds)))
          (:token creds))))
    (fn []
      (let [creds (read-tokens server-name)]
        (if (:refresh-token creds)
          (try (:token (refresh-token-exchange! server-name creds))
               (catch Throwable t (throw (oauth-required server-name server-url @www-auth-atom t))))
          (throw (oauth-required server-name server-url @www-auth-atom nil)))))))

(defn forget!
  "Drop persisted tokens for `server-name` (e.g. on a gateway sign-out, or when a
   401 recurs after refresh)."
  [server-name]
  (let [f (token-file server-name)]
    (when (.exists f) (.delete f))))

(defn token-status
  "Non-secret view of the persisted OAuth tokens for `server-name`. Never returns
   the token itself: a client may render it, and a client is never trusted with
   credentials."
  [server-name]
  (let [creds (read-tokens server-name)]
    {"server" server-name
     "is_authorized" (boolean (:token creds))
     "is_expired" (boolean (and (:token creds) (expired? creds)))
     "has_refresh_token" (boolean (:refresh-token creds))
     "expires_at_ms" (:expires-at-ms creds)
     "scope" (:scope creds)}))

;; ---------------------------------------------------------------------------
;; Headless flows — authorizing from a client that is NOT this daemon's terminal
;;
;; This is the ONLY authorization path. The Companion app and the TUI are
;; clients, possibly on another device entirely, and the daemon may have no
;; browser at all, so the flow is taken apart: START returns the URL to show, the
;; user authorizes in THEIR browser, and the flow lands either by itself (the
;; browser did reach the loopback listener on this host) or by POSTing back the
;; redirect URL they were dumped on. Nothing on a request path waits for a human.
;; ---------------------------------------------------------------------------

(def ^:private flow-ttl-ms
  "How long an unfinished headless flow stays resumable. Long enough to unlock a
   phone, log in, and approve; short enough that an abandoned flow's loopback
   listener does not sit on a port forever."
  600000)

(defonce ^:private flows (atom {})) ; {flow-id flow}

(defn- new-flow-id [] (b64url (rand-bytes 12)))

(defn- stop-loopback!
  [{:keys [^HttpServer loopback]}]
  (when loopback (try (.stop loopback 0) (catch Throwable _ nil)))
  nil)

(defn- drop-flow!
  [flow-id]
  (when-let [flow (get @flows flow-id)]
    (swap! flows dissoc flow-id)
    (stop-loopback! flow))
  nil)

(defn- sweep!
  "Drop expired flows and release their listeners. Runs on every public op, so a
   gateway nobody is authorizing against keeps no timers and no open ports."
  []
  (doseq [[id {:keys [expires-at-ms]}] @flows]
    (when (< (long expires-at-ms) (System/currentTimeMillis)) (drop-flow! id)))
  nil)

(defn- flow-view
  "The ONLY fields that may cross the wire, string-keyed like every MCP surface.
   The PKCE verifier, the state nonce and the discovery context stay in this
   process."
  [{:keys [id server url redirect-uri expires-at-ms state]}]
  (merge {"flow_id" id
          "server" server
          "kind" "pkce"
          "url" url
          "redirect_uri" redirect-uri
          "expires_at_ms" expires-at-ms}
         @state))

(defn- code-of
  "The authorization code inside `input`: a bare code, or the whole redirect URL
   the browser landed on (which is all a user on another device can hand back).
   Throws the server's own `error` when the callback carried one."
  [input]
  (let
    [s
     (str/trim (str input))

     q
     (when (str/includes? s "?") (query-parse (subs s (inc (long (str/index-of s "?"))))))]

    (when-let [err (get q "error")]
      (throw (ex-info
               (str "MCP OAuth error: " err)
               {:type :mcp/oauth-error :error err :description (get q "error_description")})))
    (or (get q "code")
        (when-not (or (str/blank? s) (str/includes? s "?")) s)
        (throw (ex-info "No authorization code in the pasted value" {:type :mcp/oauth-error})))))

(defn- finish-flow!
  "Spend `code` for `flow` and record the verdict on the flow itself, so a client
   that started the flow on one device can read the outcome from another."
  [{:keys [ctx client-id redirect-uri state] :as flow} code]
  (try (exchange-code! ctx client-id code redirect-uri)
       (reset! state {"status" "ok"})
       (catch Throwable t
         (reset! state {"status" "error" "error" (or (ex-message t) (str t))})
         (throw t))
       (finally (stop-loopback! flow)))
  (flow-view flow))

(defn start-authorization!
  "Begin a HEADLESS OAuth flow for `server-name` at `server-url` and return its
   public view `{flow_id, server, kind, url, redirect_uri, expires_at_ms,
   status}` — string-keyed, like every MCP surface.

   The caller shows `url`; the user authorizes in their own browser. NO browser
   is opened here — the user may be nowhere near this machine. The flow completes
   by itself when that browser can reach the loopback listener on this host (the
   local TUI case), otherwise the client posts the redirect URL back through
   `complete-authorization!`."
  [server-name server-url {:keys [www-auth auth-hint]}]
  (sweep!)
  (let
    [ctx
     (auth-context server-name server-url www-auth auth-hint)

     {:keys [^HttpServer server redirect-uri result]}
     (start-loopback! server-name)

     {:keys [url client-id]}
     (try (authorize-url ctx auth-hint redirect-uri)
          (catch Throwable t (try (.stop server 0) (catch Throwable _ nil)) (throw t)))

     flow
     {:id (new-flow-id)
      :server server-name
      :ctx ctx
      :client-id client-id
      :url url
      :redirect-uri redirect-uri
      :loopback server
      :expires-at-ms (+ (System/currentTimeMillis) (long flow-ttl-ms))
      :state (atom {"status" "pending"})}]

    (swap! flows assoc (:id flow) flow)
    (future (let [q (deref result flow-ttl-ms ::timeout)]
              (when (map? q)
                (try (finish-flow! flow (code-of (str "?" (form-encode q))))
                     (catch Throwable _ nil)))))
    (tel/log!
      {:level :info :id ::headless-authorize :data {:server server-name :flow_id (:id flow)}}
      (str "MCP OAuth: headless flow started for `" server-name "`"))
    (flow-view flow)))

(defn complete-authorization!
  "Finish flow `flow-id` with what the user pasted back: the redirect URL their
   browser landed on, or a bare authorization code."
  [flow-id input]
  (sweep!)
  (let [flow (get @flows flow-id)]
    (when-not flow
      (throw (ex-info "Unknown or expired MCP auth flow"
                      {:type :mcp/oauth-flow-not-found :flow-id flow-id})))
    (finish-flow! flow (code-of input))))

(defn poll-authorization!
  "Read a flow's verdict without blocking: `pending`, `ok`, or `error`. This is how
   a client learns that the loopback listener already finished the flow for it."
  [flow-id]
  (sweep!)
  (let [flow (get @flows flow-id)]
    (when-not flow
      (throw (ex-info "Unknown or expired MCP auth flow"
                      {:type :mcp/oauth-flow-not-found :flow-id flow-id})))
    (flow-view flow)))

(defn cancel-authorization!
  "Forget an abandoned flow and release its loopback listener now."
  [flow-id]
  (sweep!)
  (drop-flow! flow-id)
  {"flow_id" flow-id "is_cancelled" true})
