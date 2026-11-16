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
     5. prepare an allowed callback and register it, then hand the adapter to
        `provider.flow`, the SAME lifecycle used by model-provider authentication.
        The initiating client opens the browser and returns directly to its gateway;
     6. exchange code → access + refresh tokens; persist to
        `~/.vis/mcp-tokens/<server>.edn`;
     7. on later expiry / 401, refresh the token single-flight through
        `com.blockether.vis.internal.provider.oauth/make-file-refresher`.

   The returned `bearer-fn` is a 0/1-arg function: 0-arg yields the current
   bearer token; 1-arg (with the token the server just rejected) forces a
   refresh. It never opens a browser and never waits — with nothing to refresh
   it throws `:mcp/oauth-required`, which callers turn into `sign in`."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.http :as mcp-http]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.provider.flow :as auth-flow]
            [com.blockether.vis.internal.provider.oauth :as oauth]
            [com.blockether.vis.internal.util :as util])
  (:import (java.net URI URLDecoder URLEncoder)
           (java.security KeyFactory KeyPairGenerator PrivateKey)
           (java.security.interfaces ECPublicKey RSAPrivateKey)
           (java.security.spec ECGenParameterSpec InvalidKeySpecException PKCS8EncodedKeySpec)
           (java.util Base64)))

(def ^:private protocol-version-header "MCP-Protocol-Version")

(def ^:private protocol-version "2025-06-18")

;; The lazy babashka.http-client instance is shared with `client.clj` through
;; `mcp-http/client`.

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
          (for [p
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
  (let [resp
        (http/request {:uri url
                       :method :get
                       :client @mcp-http/client
                       :headers {"Accept" "application/json"
                                 protocol-version-header protocol-version}
                       :timeout 15000
                       :throw false
                       :as :string})

        status
        (long (:status resp))]

    (when (< status 400) (try (json/read-json (:body resp)) (catch Throwable _ nil)))))

(defn- http-post-form
  [^String url form headers]
  (let [resp (http/request {:uri url
                            :method :post
                            :client @mcp-http/client
                            :headers (merge {"Content-Type" "application/x-www-form-urlencoded"
                                             "Accept" "application/json"
                                             protocol-version-header protocol-version}
                                            headers)
                            :body (form-encode form)
                            :timeout 30000
                            :throw false
                            :as :string})]
    {:status (:status resp)
     :headers (:headers resp)
     :body (try (json/read-json (:body resp)) (catch Throwable _ (:body resp)))}))

(defn- http-post-json
  [^String url body]
  (let [resp (http/request {:uri url
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

(defn- oauth-failure
  "A typed sign-in refusal for `server` that names the failed `step` (`discovery`,
   `registration` or `authorize`) and, when there is one, the upstream answer as
   `:upstream-status` and `:upstream-url`. The message is safe to show as is."
  [type server step message & {:as upstream}]
  (ex-info (str "MCP server '" server "': " message)
           (merge {:type type :server server :step step} upstream)))

;; Discovery (RFC 9728 + RFC 8414 fallbacks)

(defn challenge-param
  "The value of parameter `k` in a `WWW-Authenticate: Bearer …` header, quoted or
   not (RFC 6750 section 3)."
  [h k]
  (when (seq h)
    (some->> (re-find (re-pattern (str "(?i)(?:^|[\\s,])" k "\\s*=\\s*(?:\"([^\"]*)\"|([^\\s,]+))"))
                      h)
             rest
             (some not-empty))))

(defn- origin-of
  [^String url]
  (let [u (URI/create url)]
    (str (.getScheme u) "://" (.getAuthority u))))

(defn- path-of
  "The path of `url` without a trailing slash; empty for a bare origin."
  [^String url]
  (str/replace (or (.getRawPath (URI/create url)) "") #"/+$" ""))

(defn- well-known
  "The well-known URL of `suffix` for `url`. The suffix goes between the origin and
   the path (RFC 8414 section 3.1, RFC 9728 section 3.1)."
  [url suffix]
  (str (origin-of url) "/.well-known/" suffix (path-of url)))

(defn- same-resource?
  "True when `resource`, from the protected-resource metadata, is `server-url` or a
   parent path of it on the same origin (RFC 9728 section 3.3). A token for another
   resource can go to another party."
  [server-url resource]
  (try (let [server-path
             (path-of server-url)

             path
             (path-of resource)]

         (and (= (str/lower-case (origin-of server-url)) (str/lower-case (origin-of resource)))
              (or (= path server-path) (str/starts-with? server-path (str path "/")))))
       (catch Exception _ false)))

(defn- same-issuer?
  "True when the metadata `issuer` is the issuer `as-url` that gave its URL (RFC 8414
   section 3.3). Only a trailing slash can differ: some servers publish one in the
   resource metadata and not in the issuer."
  [as-url issuer]
  (= (str/replace (str as-url) #"/+$" "") (str/replace (str issuer) #"/+$" "")))

(defn- discover-protected-resource
  "The protected-resource metadata of `server-url` (RFC 9728). The URL that the
   server names in its challenge comes first, then the URL for the path of the
   server, then the URL for its host."
  [server-url www-auth]
  (let [candidates (->> [(challenge-param www-auth "resource_metadata")
                         (well-known server-url "oauth-protected-resource")
                         (str (origin-of server-url) "/.well-known/oauth-protected-resource")]
                        (remove str/blank?)
                        distinct)]
    (some (fn [u]
            (when-let [meta (http-get-json u)]
              (assoc meta ::url u)))
          candidates)))

(defn- discover-authorization-server
  "The metadata of the authorization server `as-url` (RFC 8414, OpenID Connect
   Discovery). An issuer with a path never gets the URLs of its bare host: another
   tenant can answer there."
  [as-url]
  (let [issuer
        (str (origin-of as-url) (path-of as-url))

        candidates
        (distinct [(well-known as-url "oauth-authorization-server")
                   (well-known as-url "openid-configuration")
                   (str issuer "/.well-known/openid-configuration")
                   (str issuer "/.well-known/oauth-authorization-server")])]

    (some (fn [u]
            (some-> (http-get-json u)
                    (assoc ::url u)))
          candidates)))

(defn- default-endpoints
  "The endpoints of protocol 2025-03-26 for a server that publishes no metadata at
   all: fixed paths on the host of the MCP server."
  [origin]
  {"authorization_endpoint" (str origin "/authorize")
   "token_endpoint" (str origin "/token")
   "registration_endpoint" (str origin "/register")
   ::assumed? true})

;; Dynamic Client Registration (RFC 7591)

(defn- register-client!
  "Register a public client with the configured name (RFC 7591), or throw the
   typed refusal with the upstream status."
  [server {registration-url "registration_endpoint"} redirect-uri client-name scope]
  (when registration-url
    (let [{:keys [status body]}
          (http-post-json registration-url
                          (cond-> {"client_name" (or client-name "Claude Code")
                                   ;; `web`, the default, may not redirect to loopback.
                                   "application_type" "native"
                                   "redirect_uris" [redirect-uri]
                                   "grant_types" ["authorization_code" "refresh_token"]
                                   "response_types" ["code"]
                                   "token_endpoint_auth_method" "none"}
                            scope
                            (assoc "scope" scope)))

          status
          (long status)]

      (if (and (< status 400) (map? body) (get body "client_id"))
        body
        (throw
          (oauth-failure :mcp/oauth-registration server
                         "registration"
                         (if (< status 400)
                           (str "the authorization server answered client registration "
                                "without a client_id (HTTP "
                                status
                                " from "
                                registration-url
                                ").")
                           (str "the authorization server refused client registration (HTTP "
                                status
                                " from "
                                registration-url
                                "). It accepts only clients "
                                "that it registered. Set auth.client_id and auth.client_secret "
                                "on the server."))
                         :upstream-status status
                         :upstream-url registration-url))))))

;; Token store — one EDN file per server under ~/.vis/mcp-tokens/

(defn- token-file
  ^java.io.File [server-name]
  (io/file (paths/vis-home) "mcp-tokens" (str server-name ".edn")))

(defn- read-tokens
  [server-name]
  (let [f (token-file server-name)]
    (when (.exists f)
      (try (with-open [r (io/reader f)]
             (read (java.io.PushbackReader. r)))
           (catch Throwable _ nil)))))

(defn- write-tokens!
  [server-name m]
  (paths/spit-private! (token-file server-name) (util/content-str m))
  m)

(defn- expired?
  "Skew the reported expiry by 60s so we refresh proactively, never at the
   exact instant of the 401."
  [{:keys [expires-at-ms]}]
  (or (nil? expires-at-ms) (< (- (long expires-at-ms) 60000) (util/now-ms))))

(defn- ->tokens
  "Normalize a token endpoint response into `{:token :refresh-token :expires-at-ms
   :saved-at-ms :client-id}`."
  [body extra]
  (let [now
        (util/now-ms)

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

;; DPoP (RFC 9449, SEP-1932) and signed assertions (RFC 7523)

(defn- coordinate
  "The 32 big-endian bytes of the P-256 coordinate `n`, in base64url."
  [^java.math.BigInteger n]
  (let [bytes
        (.toByteArray n)

        size
        (alength bytes)

        length
        (min size 32)

        output
        (byte-array 32)]

    (System/arraycopy bytes (- size length) output (- 32 length) length)
    (util/base64url output)))

(defn- new-dpop-key
  "A new P-256 key for DPoP proofs: the private key as PKCS#8 text and the public
   key as a JWK."
  []
  (let [pair
        (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                            (.initialize (ECGenParameterSpec. "secp256r1"))))

        ^ECPublicKey public
        (.getPublic pair)

        point
        (.getW public)]

    {:private-key (.encodeToString (Base64/getEncoder) (.getEncoded (.getPrivate pair)))
     :jwk {"kty" "EC"
           "crv" "P-256"
           "x" (coordinate (.getAffineX point))
           "y" (coordinate (.getAffineY point))}}))

(defn- private-key
  "The EC or RSA private key of the PKCS#8 text `text`, with or without the PEM
   lines."
  ^PrivateKey [^String text]
  (let [der
        (try (.decode (Base64/getDecoder)
                      (-> text
                          (str/replace #"-----[^-]+-----" "")
                          (str/replace #"\s" "")))
             (catch IllegalArgumentException _ nil))

        read-as
        (fn [^String algorithm]
          (try (.generatePrivate (KeyFactory/getInstance algorithm) (PKCS8EncodedKeySpec. der))
               (catch InvalidKeySpecException _ nil)))]

    (or (when der (or (read-as "EC") (read-as "RSA")))
        (throw (ex-info "The private key is not a PKCS#8 key (`BEGIN PRIVATE KEY`)."
                        {:type :mcp/oauth-error})))))

(defn- dpop-proof
  "The DPoP proof of the key `dpop` for one `method` request to `url`. `token`
   binds the proof to an access token. `nonce` is the last nonce of that server."
  [dpop method ^String url token nonce]
  (util/compact-jws (private-key (:private-key dpop))
                    (json/write-json-str {"typ" "dpop+jwt" "alg" "ES256" "jwk" (:jwk dpop)})
                    (json/write-json-str (cond-> {"jti" (util/base64url (util/random-bytes 16))
                                                  "htm" method
                                                  ;; Without the query and the fragment (section 4.2).
                                                  "htu" (first (str/split url #"[?#]" 2))
                                                  "iat" (quot (long (util/now-ms)) 1000)}
                                           token
                                           (assoc "ath"
                                             (util/base64url (util/sha256 (util/utf8 token))))

                                           nonce
                                           (assoc "nonce" nonce)))))

(defn- dpop-bound?
  "True when the token endpoint answer `body` has a DPoP-bound token."
  [body]
  (= "dpop"
     (some-> (get body "token_type")
             str/lower-case)))

(defn- wants-dpop?
  "Whether the sign-in asks for a DPoP-bound token. `auth.dpop` decides when it is
   set. Else the MCP server must ask for DPoP, in its challenge or its metadata,
   and the authorization server must accept ES256 proofs."
  [auth-hint www-auth rmeta asmeta]
  (let [configured (:dpop auth-hint)]
    (if (boolean? configured)
      configured
      (boolean (and (some #{"ES256"} (get asmeta "dpop_signing_alg_values_supported"))
                    (or (some->> www-auth
                                 (re-find #"(?i)(?:^|,)\s*DPoP(?:\s|$)"))
                        (true? (get rmeta "dpop_bound_access_tokens_required"))
                        (seq (get rmeta "dpop_signing_alg_values_supported"))))))))

(defonce ^:private dpop-nonces (atom {})) ; {token endpoint -> its last DPoP-Nonce}

(defn- token-request!
  "POST `form` to the token endpoint `url`. With the key `dpop` the request has a
   DPoP proof, and one `use_dpop_nonce` refusal is sent again with the nonce of
   the server (RFC 9449 section 8)."
  [url form headers dpop]
  (let [send
        (fn []
          (let [response (http-post-form
                           url
                           form
                           (cond-> headers
                             dpop
                             (assoc "DPoP"
                               (dpop-proof dpop "POST" url nil (get @dpop-nonces url)))))]
            (when-let [nonce (get-in response [:headers "dpop-nonce"])]
              (swap! dpop-nonces assoc url nonce))
            response))

        response
        (send)]

    (if (and dpop
             (map? (:body response))
             (= "use_dpop_nonce" (get (:body response) "error"))
             (get-in response [:headers "dpop-nonce"]))
      (send)
      response)))

(defn- token-failure
  "The typed refusal of a token endpoint to `step` of `server`."
  [server step {:keys [status body]}]
  (let [oauth-error (when (map? body) (get body "error"))]
    (ex-info (str "MCP " server " " step " failed: " status)
             {:type :mcp/oauth-token
              :server server
              :status status
              :oauth-error oauth-error
              :body body
              :refusal (str "the token endpoint answered HTTP "
                            status
                            (when (string? oauth-error) (str " (" oauth-error ")"))
                            ".")})))

;; Auth-code flow (first time)

(defn- with-offline-access
  "`scope` with `offline_access` added when the authorization server lists that scope
   (SEP-2207). Many servers issue a refresh token only for it."
  [scope asmeta]
  (if (and (some #{"offline_access"} (get asmeta "scopes_supported"))
           (not (some #{"offline_access"} (str/split scope #"\s+"))))
    (str scope " offline_access")
    scope))

(defn- with-previous-scope
  "`scope` after the scopes of `previous` that it does not have. A server names
   only the scope of the refused operation in a step-up (SEP-2350)."
  [scope previous]
  (if (seq previous)
    (str/join " " (distinct (concat (str/split previous #"\s+") (str/split scope #"\s+"))))
    scope))

(defn- auth-context
  "Discovery (RFC 9728 → RFC 8414) plus fresh PKCE material for `server-name`.
   Shared by the loopback flow and the headless flow so both speak exactly the
   same protocol and only the redirect leg differs. `www-auth` is the raw
   `WWW-Authenticate` header from the 401 (may be nil — we still try the
   well-known URL). `auth-hint` is optional user config:
   `{:client-id :scope :authorization-timeout-ms}`."
  [server-name server-url www-auth auth-hint]
  (let [rmeta
        (discover-protected-resource server-url www-auth)

        _
        (when-let [resource (get rmeta "resource")]
          (when-not (same-resource? server-url resource)
            (throw (oauth-failure
                     :mcp/oauth-discovery server-name
                     "discovery"
                     (str "the protected-resource metadata is for another resource (" resource ").")
                     :upstream-url (::url rmeta)))))

        ;; Protocol 2025-03-26 has no resource metadata: the host of the MCP server
        ;; is the authorization server, with fixed endpoints when it publishes none.
        as-url
        (cond (nil? rmeta) (origin-of server-url)
              (first (get rmeta "authorization_servers")) (first (get rmeta
                                                                      "authorization_servers"))
              :else (throw (oauth-failure
                             :mcp/oauth-discovery server-name
                             "discovery"
                             "the protected-resource metadata names no authorization server."
                             :upstream-url (::url rmeta))))

        asmeta
        (or (discover-authorization-server as-url)
            (when-not rmeta (default-endpoints as-url))
            (throw (oauth-failure :mcp/oauth-discovery server-name
                                  "discovery"
                                  (str "found no authorization server metadata at " as-url ".")
                                  :upstream-url as-url)))

        ;; Protocol 2025-03-26 servers often publish the metadata of another host.
        _
        (when-let [issuer (and rmeta (get asmeta "issuer"))]
          (when-not (same-issuer? as-url issuer)
            (throw (oauth-failure :mcp/oauth-discovery server-name
                                  "discovery" (str "the authorization server metadata of "
                                                   as-url
                                                   " names another issuer ("
                                                   issuer
                                                   ").")
                                  :upstream-url (::url asmeta)))))

        ;; A step-up keeps what the last sign-in at this authorization server asked for.
        previous-scope
        (when (= "insufficient_scope" (challenge-param www-auth "error"))
          (let [saved (read-tokens server-name)]
            (when (= as-url (:authorization-server saved)) (:requested-scope saved))))

        verifier
        (util/base64url (util/random-bytes 32))]

    {:server server-name
     :as-url as-url
     :asmeta asmeta
     :verifier verifier
     :challenge (util/base64url (util/sha256 (util/utf8 verifier)))
     :state (util/base64url (util/random-bytes 16))
     :resource (get rmeta "resource")
     :dpop? (wants-dpop? auth-hint www-auth rmeta asmeta)
     ;; The configured scope, then the scope that the server asked for, then every
     ;; scope of the resource. With none of them the request names no scope.
     :scope (or (:scope auth-hint)
                (some-> (or (challenge-param www-auth "scope")
                            (some->> (get rmeta "scopes_supported")
                                     seq
                                     (str/join " ")))
                        (with-previous-scope previous-scope)
                        (with-offline-access asmeta)))}))

(def ^:private client-document-url
  "Where the Client ID Metadata Document of Vis is published. The document is
   `vis-contract/oauth/mcp-client.json`; the documentation site serves it."
  "https://vis.blockether.com/oauth/mcp-client.json")

(defn- without-loopback-port
  "`redirect-uri` without its port when it is a loopback address. An authorization
   server accepts each port of a registered loopback redirect (RFC 8252 section 7.3)."
  [redirect-uri]
  (str/replace-first (str redirect-uri) #"^(http://(?:127\.0\.0\.1|localhost|\[::1\])):\d+" "$1"))

(defn- published-client?
  "Whether the document at `url` is what an authorization server reads for a
   sign-in that returns to `redirect-uri`: it is there, it names `url` as its
   `client_id` and it lists the redirect."
  [url redirect-uri]
  (let [document (try (http-get-json url) (catch Exception _ nil))]
    (boolean (and (= url (get document "client_id"))
                  ;; Not a set literal: a redirect without a port is twice the same.
                  (some (hash-set redirect-uri (without-loopback-port redirect-uri))
                        (get document "redirect_uris"))))))

(defn- document-client-id
  "The Client ID Metadata Document URL that is the `client_id` at this
   authorization server, or nil when registration must give one. A configured
   `auth.client_metadata_url` is used as it is and `false` turns documents off. The
   document of Vis itself is used only when a read shows that it is published:
   a server that cannot read the document refuses the person, with no way back."
  [asmeta {configured :client-metadata-url} redirect-uri]
  (when (true? (get asmeta "client_id_metadata_document_supported"))
    (cond (false? configured) nil
          (string? configured) configured
          (published-client? client-document-url redirect-uri) client-document-url)))

(defn- authorize-url
  "The authorization-endpoint URL for `ctx` at `redirect-uri`, plus the `client_id`
   it was built for. The client is the configured `auth.client_id`, else a Client ID
   Metadata Document when the server reads them, else a dynamic registration.
   Registration happens HERE, once, and the id is returned: registering again
   before the token exchange can hand back a DIFFERENT client, which the
   authorization server then rejects the code for. A secret that registration
   issued comes back as `:registered-secret`."
  [{:keys [asmeta challenge state resource scope server]} auth-hint redirect-uri]
  (let [document-id
        (when-not (:client-id auth-hint) (document-client-id asmeta auth-hint redirect-uri))

        reg
        (when-not (or (:client-id auth-hint) document-id)
          (try (register-client! server asmeta redirect-uri (:client-name auth-hint) scope)
               (catch clojure.lang.ExceptionInfo e
                 ;; Nothing said that this host is an authorization server at all.
                 (throw (if (::assumed? asmeta)
                          (oauth-failure :mcp/oauth-discovery server
                                         "discovery" (str "found no OAuth metadata for the server, "
                                                          "and no authorization server at "
                                                          (get asmeta "registration_endpoint")
                                                          ".")
                                         :upstream-status (:upstream-status (ex-data e))
                                         :upstream-url (get asmeta "registration_endpoint"))
                          e)))))

        client-id
        (or (:client-id auth-hint) document-id (get reg "client_id"))

        _
        (when-not client-id
          (throw (oauth-failure :mcp/oauth-client server
                                "registration"
                                (str
                                  "the authorization server offers no dynamic client registration. "
                                  "Set auth.client_id on the server."))))

        endpoint
        (or (get asmeta "authorization_endpoint")
            (throw (oauth-failure :mcp/oauth-discovery server
                                  "authorize"
                                  "the authorization server metadata has no authorization_endpoint."
                                  :upstream-url (::url asmeta))))

        params
        (cond-> {"response_type" "code"
                 "client_id" client-id
                 "redirect_uri" redirect-uri
                 "state" state
                 "code_challenge" challenge
                 "code_challenge_method" "S256"}
          scope
          (assoc "scope" scope)

          resource
          (assoc "resource" resource))]

    {:client-id client-id
     :registered-secret (get reg "client_secret")
     :url (str endpoint (if (str/includes? endpoint "?") "&" "?") (form-encode params))}))

(defn- token-auth-method
  "How the client authenticates at the token endpoint (RFC 6749 section 2.3.1). A
   client without a secret is public: `none`. A client with a secret uses
   `client_secret_basic`, unless the server lists only `client_secret_post`."
  [asmeta client-secret]
  (let [methods (set (get asmeta "token_endpoint_auth_methods_supported"))]
    (cond (not client-secret) "none"
          (and (contains? methods "client_secret_post")
               (not (contains? methods "client_secret_basic")))
          "client_secret_post"
          :else "client_secret_basic")))

(defn- client-credentials
  "The `client_id` form field plus the secret for `method`, as `[form headers]`."
  [method client-id client-secret]
  (let [enc #(URLEncoder/encode (str %) "UTF-8")]
    (case method
      "client_secret_post"
      [{"client_id" client-id "client_secret" client-secret} nil]

      "client_secret_basic"
      [{"client_id" client-id}
       {"Authorization" (str "Basic "
                             (.encodeToString (Base64/getEncoder)
                                              (util/utf8
                                                (str (enc client-id) ":" (enc client-secret)))))}]

      [{"client_id" client-id} nil])))

(defn- exchange-code!
  "Spend `code` at the token endpoint and PERSIST the resulting tokens. A configured
   secret is never persisted; only the method that a refresh must use again. A
   secret that registration issued exists nowhere else, so it stays with the tokens.
   So does the key of a DPoP-bound token: each later request needs its proof."
  [{:keys [server as-url asmeta verifier resource registered-secret scope dpop?]} client-id
   client-secret code redirect-uri]
  (let [token-url
        (get asmeta "token_endpoint")

        client-secret
        (or client-secret registered-secret)

        method
        (token-auth-method asmeta client-secret)

        [form headers]
        (client-credentials method client-id client-secret)

        dpop
        (when dpop? (new-dpop-key))

        {:keys [status body] :as response}
        (token-request! token-url
                        (cond-> (merge form
                                       {"grant_type" "authorization_code"
                                        "code" code
                                        "redirect_uri" redirect-uri
                                        "code_verifier" verifier})
                          resource
                          (assoc "resource" resource))
                        headers
                        dpop)]

    (when (>= (long status) 400) (throw (token-failure server "token exchange" response)))
    (write-tokens! server
                   (->tokens body
                             (cond-> {:client-id client-id
                                      :token-auth-method method
                                      :token-endpoint token-url
                                      :authorization-server as-url
                                      :resource resource}
                               registered-secret
                               (assoc :client-secret registered-secret)

                               scope
                               (assoc :requested-scope scope)

                               (and dpop (dpop-bound? body))
                               (assoc :dpop dpop))))))

(defn- refresh-exchange!
  "Spend `refresh-token` at the token endpoint of `creds` and return the new
   tokens. The server may rotate the refresh token. A confidential client sends
   `client-secret`, or the secret that registration issued, with the method that
   the first exchange used. A DPoP-bound token keeps its key."
  [server-name creds client-secret refresh-token]
  (let [[form headers]
        (client-credentials (:token-auth-method creds)
                            (:client-id creds)
                            (or client-secret (:client-secret creds)))

        {:keys [status body]}
        (token-request! (:token-endpoint creds)
                        (cond-> (merge form
                                       {"grant_type" "refresh_token" "refresh_token" refresh-token})
                          (:resource creds)
                          (assoc "resource" (:resource creds)))
                        headers
                        (:dpop creds))]

    (when (>= (long status) 400)
      (throw (ex-info (str "MCP " server-name " token refresh failed: " status)
                      {:type :mcp/oauth-refresh :server server-name :status status :body body})))
    (->tokens body creds)))

;; Grants that need no person (SEP-1046, SEP-1933, SEP-990)

(def ^:private jwt-bearer-grant "urn:ietf:params:oauth:grant-type:jwt-bearer")

(defn machine-grant?
  "True when `auth-hint` names a grant that needs no person: the client gets the
   token by itself, with no browser."
  [auth-hint]
  (contains? #{"client_credentials" "jwt_bearer" "enterprise_managed"} (:grant auth-hint)))

(defn- configured-text
  "The text under `k` of `auth-hint`, or the text of the file under `<k>-file`. A
   file is read for each grant: a platform replaces a workload token while Vis runs."
  [server auth-hint k]
  (or (get auth-hint k)
      (some-> (get auth-hint (keyword (str (name k) "-file")))
              slurp
              str/trim)
      (throw (oauth-failure :mcp/oauth-error server
                            "configuration" (str "`auth` has no `"
                                                 (str/replace (name k) "-" "_")
                                                 "` for the grant `"
                                                 (:grant auth-hint)
                                                 "`.")))))

(defn- client-assertion
  "The signed JWT that authenticates `client-id` at the authorization server
   `issuer` (RFC 7523 section 3). The key selects ES256 or RS256; a configured
   `algorithm` must name the same one."
  [server client-id issuer pem algorithm]
  (let [key
        (private-key pem)

        alg
        (if (instance? RSAPrivateKey key) "RS256" "ES256")

        now
        (quot (long (util/now-ms)) 1000)]

    (when (and algorithm (not= algorithm alg))
      (throw
        (oauth-failure
          :mcp/oauth-error server
          "configuration"
          (str "`auth.signing_algorithm` is " algorithm ", but the private key signs " alg "."))))
    (util/compact-jws key
                      (json/write-json-str {"alg" alg "typ" "JWT"})
                      (json/write-json-str {"iss" client-id
                                            "sub" client-id
                                            "aud" issuer
                                            "jti" (util/base64url (util/random-bytes 16))
                                            "iat" now
                                            "exp" (+ now 300)}))))

(defn- id-jag!
  "Exchange the identity token of the person at the identity provider for an
   ID-JAG: a grant for the authorization server `issuer` and the MCP server
   `resource` (SEP-990, RFC 8693)."
  [server auth-hint issuer resource scope]
  (let [endpoint
        (or (:idp-token-endpoint auth-hint)
            (some-> (:idp-issuer auth-hint)
                    (str/replace #"/+$" "")
                    (str "/.well-known/openid-configuration")
                    http-get-json
                    (get "token_endpoint"))
            (throw (oauth-failure :mcp/oauth-discovery server
                                  "discovery" "found no token endpoint of the identity provider.")))

        {:keys [idp-client-id idp-client-secret]}
        auth-hint

        [form headers]
        (if idp-client-id
          (client-credentials (if idp-client-secret "client_secret_basic" "none")
                              idp-client-id
                              idp-client-secret)
          [{} nil])

        {:keys [status body] :as response}
        (http-post-form endpoint
                        (cond-> (merge
                                  form
                                  {"grant_type" "urn:ietf:params:oauth:grant-type:token-exchange"
                                   "requested_token_type" "urn:ietf:params:oauth:token-type:id-jag"
                                   "subject_token" (configured-text server auth-hint :idp-id-token)
                                   "subject_token_type" "urn:ietf:params:oauth:token-type:id_token"
                                   "audience" issuer
                                   "resource" resource})
                          scope
                          (assoc "scope" scope))
                        headers)]

    (when-not (and (< (long status) 400) (map? body) (string? (get body "access_token")))
      (throw (token-failure server "identity token exchange" response)))
    (get body "access_token")))

(defn- machine-grant!
  "Get a token for `server-name` with the grant of `auth-hint` and PERSIST it. One
   token request: a refusal is the answer, with no second try and no other grant."
  [server-name server-url www-auth auth-hint]
  (let [{:keys [as-url asmeta resource scope dpop?]}
        (auth-context server-name server-url www-auth auth-hint)

        {:keys [grant client-id client-secret]}
        auth-hint

        issuer
        (or (get asmeta "issuer") as-url)

        pem
        (when (or (:private-key auth-hint) (:private-key-file auth-hint))
          (configured-text server-name auth-hint :private-key))

        [form headers]
        (cond pem
              [{"client_assertion_type" "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
                "client_assertion"
                (client-assertion server-name client-id issuer pem (:signing-algorithm auth-hint))}
               nil]
              client-id
              (client-credentials (token-auth-method asmeta client-secret) client-id client-secret)
              :else [{} nil])

        grant-form
        (case grant
          "client_credentials"
          {"grant_type" "client_credentials"}

          "jwt_bearer"
          {"grant_type" jwt-bearer-grant
           "assertion" (configured-text server-name auth-hint :assertion)}

          "enterprise_managed"
          {"grant_type" jwt-bearer-grant
           "assertion" (id-jag! server-name auth-hint issuer (or resource server-url) scope)})

        dpop
        (when dpop? (new-dpop-key))

        {:keys [status body] :as response}
        (token-request! (get asmeta "token_endpoint")
                        (cond-> (merge form grant-form)
                          scope
                          (assoc "scope" scope)

                          resource
                          (assoc "resource" resource))
                        headers
                        dpop)]

    (when-not (and (< (long status) 400) (map? body) (string? (get body "access_token")))
      (throw (token-failure server-name "token request" response)))
    (write-tokens!
      server-name
      (->tokens
        (dissoc body "refresh_token")
        (cond-> {:grant grant :client-id client-id :authorization-server as-url :resource resource}
          (and dpop (dpop-bound? body))
          (assoc :dpop dpop))))))

;; Public: a 0/1-arg `bearer-fn` for the HTTP transport

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

(defn forget!
  "Drop persisted tokens for `server-name` after sign-out or a revoked refresh grant."
  [server-name]
  (auth-flow/cancel-owner! [:mcp server-name])
  (let [f (token-file server-name)]
    (when (.exists f) (.delete f))))

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
   one. `client-secret` is the configured secret of a pre-registered client, sent
   only when the persisted tokens came from a confidential exchange.

   With a grant of `auth-hint` that needs no person (`machine-grant?`), the fn
   gets the token by itself after the server refused a request: a 401 is the proof
   that the server wants a token. A refusal of that grant is `:mcp/oauth-token`."
  ([server-name server-url www-auth-atom] (make-bearer-fn server-name server-url www-auth-atom nil))
  ([server-name server-url www-auth-atom client-secret]
   (make-bearer-fn server-name server-url www-auth-atom client-secret nil))
  ([server-name server-url www-auth-atom client-secret auth-hint]
   (let [required
         #(oauth-required server-name server-url @www-auth-atom %)

         usable?
         #(and (:token %) (not (expired? %)))

         refresh
         (oauth/make-file-refresher
           {:store (str "mcp/" server-name)
            :load #(read-tokens server-name)
            :usable? usable?
            :refresh-token :refresh-token
            :exchange! #(refresh-exchange! server-name (read-tokens server-name) client-secret %)
            :persist! #(write-tokens! server-name %)
            :on-rejected! (fn [_]
                            (forget! server-name))
            :->token #(select-keys % [:token])
            :no-token! #(throw (required nil))
            :lock-path (str (token-file server-name) ".lock")})

         bearer
         (fn [rejected]
           ;; Nothing on file is a server that nobody signed in to, often a public
           ;; one: no exchange and no refresh record.
           (let [creds (or (read-tokens server-name) (throw (required nil)))]
             (if (and (usable? creds) (not= rejected (:token creds)))
               (:token creds)
               (try (:token (refresh rejected))
                    (catch Throwable t
                      (throw (if (= :mcp/oauth-required (:type (ex-data t))) t (required t))))))))

         grant-lock
         (Object.)

         machine
         (fn [rejected refused?]
           (locking grant-lock
             (let [creds (read-tokens server-name)]
               (cond (and (usable? creds) (not= rejected (:token creds))) (:token creds)
                     refused? (:token
                                (machine-grant! server-name server-url @www-auth-atom auth-hint))
                     :else (throw (required nil))))))]

     (if (machine-grant? auth-hint)
       (fn ([] (machine nil false)) ([rejected] (machine rejected true)))
       (fn ([] (bearer nil)) ([rejected] (bearer rejected)))))))

(defn make-proof-fn
  "Build `(fn [method url token nonce])` for the HTTP transport of `server-name`:
   the DPoP proof for one request with `token`, or nil when that token is not
   DPoP-bound."
  [server-name]
  (fn [method url token nonce]
    (let [creds (read-tokens server-name)]
      (when (and (:dpop creds) (= token (:token creds)))
        (dpop-proof (:dpop creds) method url token nonce)))))

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

(defn- mcp-view!
  [{:keys [ok? flow message error]}]
  (when-not ok?
    (throw (ex-info message
                    {:type
                     (if (= :unknown-flow error) :mcp/oauth-flow-not-found :mcp/oauth-error)})))
  (cond-> {"flow_id" (:flow-id flow)
           "server" (:subject flow)
           "kind" (:kind flow)
           "url" (:url flow)
           "redirect_uri" (:redirect-uri flow)
           "callback_mode" (:callback-mode flow)
           "expires_at_ms" (:expires-at flow)
           "status" (:status flow)}
    (:message flow)
    (assoc "error" (:message flow))))

(defn- callback-query
  "The query parameters of the redirect URL `input`, or nil for a bare code."
  [input]
  (let [s (str/trim (str input))]
    (when (str/includes? s "?") (query-parse (subs s (inc (long (str/index-of s "?"))))))))

(defn- check-issuer!
  "Refuse an authorization response that another authorization server sent: the
   mix-up attack (RFC 9207). The `iss` parameter must be the exact `issuer` of the
   metadata, and must be there when the metadata says that the server sends it. A
   bare pasted code names no issuer and passes."
  [{:keys [asmeta as-url server]} input]
  (let [q
        (callback-query input)

        iss
        (get q "iss")

        issuer
        (or (get asmeta "issuer") as-url)

        refusal
        (cond
          (and iss (not= iss issuer)) (str "the authorization response came from another issuer ("
                                           iss
                                           "), not from "
                                           issuer
                                           ".")
          (and q (nil? iss) (true? (get asmeta "authorization_response_iss_parameter_supported")))
          (str "the authorization response names no issuer, but "
               issuer
               " says that it sends one."))]

    (when refusal
      (throw (ex-info (str "MCP server '" server "': " refusal)
                      {:type :mcp/oauth-error :server server :refusal refusal})))))

(defn- code-of
  "The authorization code inside `input`: a bare code, or the whole redirect URL
   the browser landed on (which is all a user on another device can hand back).
   Throws the server's own `error` when the callback carried one."
  [input]
  (let [s
        (str/trim (str input))

        q
        (callback-query input)]

    (when-let [err (get q "error")]
      (let [description (get q "error_description")]
        (throw (ex-info (str "MCP OAuth error: " err)
                        {:type :mcp/oauth-error
                         :error err
                         :description description
                         :refusal (str "the authorization server answered "
                                       err
                                       (when description (str " (" description ")"))
                                       ".")}))))
    (or (get q "code")
        (when-not (or (str/blank? s) (str/includes? s "?")) s)
        (throw (ex-info "No authorization code in the pasted value" {:type :mcp/oauth-error})))))

(defn start-authorization!
  "Discover/register the MCP protocol adapter; the shared engine owns its lifecycle.
   App returns go directly through the paired client, never through a relay."
  [server-name server-url {:keys [www-auth auth-hint callback-mode]}]
  (when-not (contains? #{nil "loopback" "app"} callback-mode)
    (throw (ex-info "Unsupported OAuth callback mode" {:type :mcp/oauth-error})))
  (when (machine-grant? auth-hint)
    (throw (ex-info (str "MCP server '"
                         server-name
                         "' signs in without a browser: its `auth.grant` is "
                         (:grant auth-hint)
                         ".")
                    {:type :mcp/oauth-error})))
  (mcp-view!
    (auth-flow/start!
      [:mcp server-name]
      {:start (fn []
                (let [ctx
                      (auth-context server-name server-url www-auth auth-hint)

                      transport
                      (auth-flow/callback-transport! callback-mode
                                                     (if (= "app" callback-mode)
                                                       auth-flow/app-callback-uri
                                                       "http://127.0.0.1:0/mcp-callback")
                                                     (:state ctx)
                                                     600000)]

                  (try (let [{:keys [url client-id registered-secret]}
                             (authorize-url ctx auth-hint (:redirect-uri transport))]
                         (merge transport
                                {:kind :pkce
                                 :url url
                                 :expires-in-ms 600000
                                 :flow (assoc ctx
                                         :client-id client-id
                                         :registered-secret registered-secret
                                         :redirect-uri (:redirect-uri transport))}))
                       (catch Throwable t
                         (when-let [stop! (get-in transport [:callback :stop!])]
                           (stop!))
                         (throw t)))))
       :complete (fn [ctx input]
                   ;; A configured secret belongs to the configured client id, never to a
                   ;; client that dynamic registration made.
                   (check-issuer! ctx input)
                   (exchange-code! ctx
                                   (:client-id ctx)
                                   (when (:client-id auth-hint) (:client-secret auth-hint))
                                   (code-of input)
                                   (:redirect-uri ctx)))})))

(defn complete-authorization!
  "Validate and complete via the shared engine; app flows require the full callback."
  [flow-id input]
  (mcp-view! (auth-flow/complete! :mcp flow-id input)))

(defn poll-authorization!
  "Read the common retained browser/app verdict."
  [flow-id]
  (mcp-view! (auth-flow/poll! :mcp flow-id)))

(defn cancel-authorization!
  "Forget an MCP attempt and release its callback worker."
  [flow-id]
  (auth-flow/cancel! :mcp flow-id)
  {"flow_id" flow-id "is_cancelled" true})
