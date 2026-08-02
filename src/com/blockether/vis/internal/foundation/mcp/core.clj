(ns com.blockether.vis.internal.foundation.mcp.core
  "Built-in Model Context Protocol (MCP) surface. The gateway daemon owns ONE
   shared pool of MCP connections (`{server {:conn spec}}`); every session sees
   the same live tools. Always on: MCP is core infrastructure, not a droppable
   plug-in and not gated by any toggle. The pool is empty (and costs nothing)
   until at least one server is declared in config.

   Servers are declared natively in `~/.vis/state.yml`:

     {:mcp {:servers {\"filesystem\" {:transport :stdio :command \"npx\"
                                    :args [\"-y\" \"@modelcontextprotocol/server-filesystem\" \"/path\"]}
                      \"remote\"     {:transport :streamable-http :url \"https://.../mcp\"
                                    :headers {\"Authorization\" \"Bearer ${MY_TOKEN}\"}
                                    :timeout_ms 60000}
                      \"stale\"      {:enabled false :url \"https://.../mcp\"}}}}

   Every string in `:headers` / `:env` / `:args` / `:url` / `:command` / `:cwd`
   supports `${ENV_VAR}` interpolation from the host environment. `:enabled
   false` skips the server without deleting the entry. HTTP servers with no
   static bearer transparently negotiate OAuth 2.1 on first 401 (RFC 9728
   discovery + RFC 7591 dynamic client registration + PKCE loopback).

   Five model-facing verbs under alias `mcp` (flat sandbox renders `alias_name`):
     mcp__servers()                — configured servers + status + tool counts
     mcp__tools(server)            — a server's tools (auto-connects)
     mcp__call(server, tool, args) — call a tool (auto-connects)
     mcp__connect(server) / mcp__disconnect(server) — manage the connection

   Live connections + tool counts also ride in ctx under `env.mcp`."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.foundation.mcp.oauth :as mcp-oauth]
            [com.blockether.vis.internal.strutil :as strutil]
            [taoensso.telemere :as tel]))

(defn- now-ms [] (System/currentTimeMillis))

;; ---------------------------------------------------------------------------
;; Config — declared servers from ~/.vis/state.yml :mcp :servers
;; ---------------------------------------------------------------------------

(defn- transport-of
  "Canonical internal transport. `http` is accepted only as a legacy persisted
   spelling; new gateway saves write the standard `streamable_http` form."
  [spec]
  (case
    (some-> (:transport spec)
            name)
    ("streamable_http" "streamable-http" "http")
    :streamable-http

    "stdio"
    :stdio

    (if (:url spec) :streamable-http :stdio)))

(defn- canonicalize-server-spec
  "Persist the standard external spelling while retaining legacy read support."
  [spec]
  (case (get spec "transport")
    ("http" "streamable-http")
    (assoc spec "transport" "streamable_http")

    spec))

(defn- interpolate-env
  "Substitute `${VAR}` in `s` from the host environment; leave unknowns intact.
   Non-strings pass through unchanged so a numeric header/arg stays numeric."
  [s]
  (if-not (string? s)
    s
    (str/replace s
                 #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}"
                 (fn [[whole var-name]]
                   (or (System/getenv var-name) whole)))))

(defn- deep-interpolate
  "Walk `x`, interpolating `${VAR}` in every string leaf."
  [x]
  (cond (map? x) (into (empty x)
                       (map (fn [[k v]]
                              [k (deep-interpolate v)]))
                       x)
        (vector? x) (mapv deep-interpolate x)
        (seq? x) (mapv deep-interpolate x)
        (set? x) (into #{} (map deep-interpolate) x)
        :else (interpolate-env x)))

(defn- enabled?
  "Per-server on/off switch — default true. `false` skips reconciliation
   without deleting the entry."
  [spec]
  (let [e (:enabled spec)]
    (if (nil? e) true (boolean e))))

(defn- ->client-spec
  "Coerce a user spec (already `runtime-config`'d) into what `mcp/connect`
   wants: env-interpolated, `:bearer-fn` synthesised for HTTP servers without a
   static `Authorization` header, `:timeout-ms` / `:listen?` forwarded."
  [server-name spec]
  (let
    [s
     (deep-interpolate spec)

     transport
     (transport-of s)

     headers
     (or (:headers s) {})

     has-static-auth?
     (some (fn [[k _]]
             (= "authorization" (str/lower-case (name k))))
           headers)

     bearer-fn
     (when (and (= :streamable-http transport) (not has-static-auth?))
       (mcp-oauth/make-bearer-fn server-name (:url s) (atom nil) (:auth s)))]

    (cond-> (assoc s :transport transport)
      (:timeout_ms s)
      (assoc :timeout-ms (:timeout_ms s))

      (:listen s)
      (assoc :listen? true)

      bearer-fn
      (assoc :bearer-fn bearer-fn))))

(defonce ^:private servers-cache
  ;; {:hash <hash-of-raw-mcp-map> :value {server-name coerced-spec}}
  (atom {:hash ::none :value {}}))

(defn- configured-servers
  "Enabled `{server-name spec}` from config, coerced for the client. Cached
   behind the hash of the raw `:mcp` block so a hot loop (per-turn ctx-fn) does
   not re-parse or re-wrap on every call. `${ENV_VAR}` is interpolated."
  []
  (let
    [raw
     (get-in (or (config/load-config-raw) {}) ["mcp"])

     h
     (hash raw)]

    (if (= h (:hash @servers-cache))
      (:value @servers-cache)
      (let
        [m
         (get raw "servers")

         coerced
         (if (map? m)
           (into {}
                 (keep (fn [[k v]]
                         (let
                           [nm
                            (str k)

                            rt
                            (config/runtime-config v)]

                           (when (enabled? rt) [nm (->client-spec nm rt)]))))
                 m)
           {})]

        (reset! servers-cache {:hash h :value coerced})
        coerced))))

;; ---------------------------------------------------------------------------
;; Live connections — ONE daemon-wide pool shared across every session. MCP
;; servers are gateway infrastructure, not per-session backgrounds: the pool is
;; reconciled to `:mcp :servers` proactively per turn and on every `/reload`.
;; ---------------------------------------------------------------------------

(defonce ^:private conns (atom {})) ; {server {:conn conn :spec spec}}

(defonce ^:private reconciling? (atom false)) ; single-flight guard

;; Servers a client explicitly KILLED. `reconcile!` runs on every turn and every
;; `/reload`, so simply closing a connection is not a stop: the very next turn
;; respawns the stdio child the user just killed. A kill is therefore REMEMBERED
;; until someone starts the server again (connect, save, enable). Deliberately
;; in-memory and never persisted: this is a runtime brake for a runaway or
;; unwanted server, not an edit to the user's config, so a gateway restart brings
;; the server back exactly as declared.
(defonce ^:private killed (atom #{})) ; #{server}

(defn- killed? [server] (contains? @killed server))

(defn- revive!
  "Release the kill brake on `server` — every explicit start path calls this, or
   the start would be undone by the next reconcile."
  [server]
  (swap! killed disj server)
  server)

;; Session-scoped servers — servers a CLIENT attaches to ONE session (an ACP
;; editor's `session/new` / `session/load` `mcpServers`). They are NEVER written
;; to config and never leak into another session: an editor wiring a server for
;; its own workspace must not mutate the daemon for the TUI, the phone, and every
;; other session. Their pool is keyed by `[session-id server]`, so a session
;; server SHADOWS a global one of the same name without disturbing it.
(defonce ^:private session-specs (atom {})) ; {session-id {server client-spec}}

(defonce ^:private session-conns (atom {})) ; {[session-id server] {:conn :spec}}

(defn- close-in-pool!
  "Close the conn cached in `pool` at `k` (if any) and drop the entry."
  [pool k]
  (when-let [conn (get-in @pool [k :conn])]
    (try (mcp/close conn) (catch Throwable _ nil))
    (swap! pool dissoc k))
  nil)

(defn- ensure-in-pool!
  "Idempotently connect `server` under `spec`, caching the conn (with the spec it
   was connected under) in `pool` at `k`. Returns the conn, or nil when the
   connection failed. A cached-but-dead conn is reaped and respawned."
  [pool k server spec]
  (let [existing (get @pool k)]
    (cond (and existing (mcp/alive? (:conn existing))) (:conn existing)
          existing (do (close-in-pool! pool k) (recur pool k server spec))
          :else (try (let [conn (mcp/connect server spec)]
                       (swap! pool assoc k {:conn conn :spec spec})
                       conn)
                     (catch Throwable t
                       (tel/log! {:level :warn
                                  :id ::connect-failed
                                  :data {:server server :error (ex-message t)}}
                                 "MCP connect failed")
                       nil)))))

(defn- session-spec-of [session-id server] (get-in @session-specs [session-id server]))

(defn- conn-of
  "The live conn for `server` as SEEN BY `session-id` (nil = global scope only).
   A server the session brought itself is answered from the session pool ONLY, so
   a same-named global server can never be handed to it by accident."
  ([server] (conn-of nil server))
  ([session-id server]
   (if (session-spec-of session-id server)
     (get-in @session-conns [[session-id server] :conn])
     (get-in @conns [server :conn]))))

(defn- disconnect!
  "Close `server` in `session-id`'s scope and drop it from that pool."
  ([server] (disconnect! nil server))
  ([session-id server]
   (if (session-spec-of session-id server)
     (close-in-pool! session-conns [session-id server])
     (close-in-pool! conns server))
   server))

(defn- ensure-connected!
  "Idempotently connect `server` as `session-id` sees it, caching the conn in the
   matching pool. Returns the conn or nil (unknown / disabled / failed)."
  ([server] (ensure-connected! nil server))
  ([session-id server]
   (if-let [spec (session-spec-of session-id server)]
     (ensure-in-pool! session-conns [session-id server] server spec)
     (when-let [spec (get (configured-servers) server)]
       (ensure-in-pool! conns server server spec)))))

(defn- visible-servers
  "`{server spec}` visible to `session-id`: the configured (daemon-wide) servers
   plus that session's own, which win on a name clash."
  [session-id]
  (merge (configured-servers) (get @session-specs session-id)))

(defn- reconcile!
  "Reconcile the daemon-wide pool to config: connect newly-enabled servers,
   close entries whose server is gone / disabled / spec-changed, and reap dead
   conns (crashed stdio child, closed HTTP session). Runs on every `/reload`
   and proactively per turn. Session-scoped servers have no config to drift
   from, so they are only reaped when DEAD."
  []
  (let [cfg (configured-servers)]
    (doseq [[server {:keys [conn spec]}] @conns]
      (when (or (killed? server) (not= spec (get cfg server)) (not (mcp/alive? conn)))
        (disconnect! server)))
    (doseq [server (keys cfg)]
      (when-not (killed? server) (ensure-connected! server)))
    (doseq [[k {:keys [conn]}] @session-conns]
      (when-not (mcp/alive? conn) (close-in-pool! session-conns k)))
    nil))

(defn- reconcile-async!
  "Single-flight background `reconcile!` — safe to call from a turn's ctx-fn
   without blocking it on a slow stdio spawn."
  []
  (when (compare-and-set! reconciling? false true)
    (future (try (reconcile!) (finally (reset! reconciling? false))))))

(defn clear-session-servers!
  "Drop and CLOSE every session-scoped server attached to `session-id`."
  [session-id]
  (doseq
    [k
     (keys @session-conns)

     :when (= session-id (first k))]

    (close-in-pool! session-conns k))
  (swap! session-specs dissoc session-id)
  nil)

(defn session-servers
  "The session-scoped servers attached to `session-id` as
   `[{:name :transport :is-connected}]`, in name order."
  [session-id]
  (mapv (fn [[nm spec]]
          {:name nm
           :transport (name (transport-of spec))
           :is-connected (boolean (get-in @session-conns [[session-id nm] :conn]))})
        (sort-by key (get @session-specs session-id))))

;; ---------------------------------------------------------------------------
;; Gateway management — persisted on the gateway, never in a Companion client.
;; ---------------------------------------------------------------------------

(defn- raw-servers [] (or (get-in (or (config/load-config-raw) {}) ["mcp" "servers"]) {}))

(defn- machine-servers
  "The servers THIS GATEWAY owns: the `:mcp :servers` block of the machine-written
   `~/.vis/state.yml` it read-modify-writes. Every other server in the merged view
   comes from a hand-written tier (`~/.vis/config.yml`, `vis.yml`,
   `.vis/config.yml`) that belongs to the user, so this API lists those but never
   rewrites them."
  []
  (or (get-in (or (config/load-global-config-raw) {}) ["mcp" "servers"]) {}))

(defn- ensure-managed!
  "Guard on every gateway write: `name` must be unknown, or owned by the machine
   tier. Writing a hand-written entry from here is never what the caller means —
   the project tier WINS on merge, so the edit is either silently shadowed or
   forks a stale duplicate of the user's own spec into `state.yml`."
  [name]
  (when (and (not (contains? (machine-servers) name)) (contains? (raw-servers) name))
    (throw (ex-info (str
                      "MCP server '" name
                      "' is declared in a hand-written config file, not in this gateway's state; "
                      "edit it there.")
                    {:type :mcp/not-managed :server name}))))

(defn- with-preserved-secrets
  "Carry `env`/`headers` forward from the persisted spec when the incoming one
   OMITS that key. The sanitized inventory a client reads never carries those
   values, so a save that round-tripped through a UI would otherwise wipe the
   server's credentials. An explicit key — including `{}` — still replaces."
  [previous next]
  (reduce (fn [spec k]
            (if (or (contains? next k) (not (contains? previous k)))
              spec
              (assoc spec k (get previous k))))
          next
          ["env" "headers"]))

(defn- server-name
  [name]
  (let
    [name (some-> name
                  str
                  str/trim)]
    (when-not (seq name)
      (throw (ex-info "MCP server name must be a non-blank string" {:type :mcp/invalid-name})))
    name))

(defn set-session-servers!
  "Attach `servers` — `{name raw-spec}`, each raw spec shaped exactly like a
   `:mcp :servers` config entry — to `session-id`, REPLACING whatever that
   session had, and connect each one EAGERLY so the caller learns now whether the
   client's servers actually work. Nothing is persisted. Returns
   `{:connected [name…] :failed [{:server … :error …}…]}`."
  [session-id servers]
  (when-not (and (string? session-id) (seq session-id))
    (throw (ex-info "MCP session id must be a non-blank string" {:type :mcp/invalid-session})))
  (clear-session-servers! session-id)
  (let
    [coerced (into {}
                   (map (fn [[k v]]
                          (let
                            [nm (server-name k)
                             spec (->> (dissoc v "name")
                                       canonicalize-server-spec
                                       config/runtime-config
                                       (->client-spec nm))]

                            [nm spec])))
                   servers)]
    (when (seq coerced) (swap! session-specs assoc session-id coerced))
    (reduce (fn [acc [nm spec]]
              (try (let [conn (mcp/connect nm spec)]
                     (swap! session-conns assoc [session-id nm] {:conn conn :spec spec})
                     (update acc :connected conj nm))
                   (catch Throwable t
                     (tel/log! {:level :warn
                                :id ::session-connect-failed
                                :data {:server nm :session session-id :error (ex-message t)}}
                               "MCP session server connect failed")
                     (update acc :failed conj {:server nm :error (or (ex-message t) (str t))}))))
            {:connected [] :failed []}
            coerced)))

(defn- tool-count
  "How many tools `conn` exposes. `mcp/list-tools` CACHES into the conn, so the
   first read pays one RPC and every later read is free. Reading the raw `:tools`
   atom instead reports 0 for every freshly connected server — the cache is only
   filled on demand — which is what made a healthy gateway server look empty in
   the Companion inventory and in the model's own `env.mcp` view."
  [conn]
  (if-not conn
    0
    (count (or (some-> (:tools conn)
                       deref)
               (try (mcp/list-tools conn) (catch Throwable _ nil))
               []))))

(defn- server-summary
  [name raw-spec is-managed]
  (let
    [spec
     (config/runtime-config raw-spec)

     conn
     (conn-of name)]

    (cond->
      {:name name
       :transport (case (transport-of spec)
                    :streamable-http
                    "streamable_http"

                    "stdio")
       :enabled (enabled? spec)
       :is-connected (boolean conn)
       ;; Whether the GATEWAY owns this entry. A server declared in a hand-written
       ;; tier is the user's file: listed, never rewritten from here.
       :is-managed (boolean is-managed)
       ;; Stopped by a client and HELD down until explicitly started again.
       :is-killed (killed? name)
       :tools (tool-count conn)}
      (:command spec)
      (assoc :command (:command spec))

      (:cwd spec)
      (assoc :cwd (:cwd spec))

      (:url spec)
      ;; OAuth is only ever an HTTP concern; `:is-authorized` lets a client offer
      ;; "Sign in" instead of letting the user stare at a server that 401s.
      (assoc :url
        (:url spec) :is-authorized
        (:is-authorized (mcp-oauth/token-status name))))))

(defn gateway-servers
  "Sanitized MCP inventory for gateway management. Secrets (env and header values)
   deliberately never cross this boundary. Every configured server is listed —
   gateway-owned and hand-written alike — and `:is-managed` says which of them
   this API may write."
  []
  (let [machine (machine-servers)]
    {:servers (->> (raw-servers)
                   (map (fn [[name spec]]
                          [(str name) spec]))
                   (sort-by first)
                   (mapv (fn [[name spec]]
                           (server-summary name spec (contains? machine name)))))}))

(defn- reset-server-cache! [] (reset! servers-cache {:hash ::none :value {}}))

(defn save-gateway-server!
  "Validate and persist a complete string-keyed server spec in this gateway's
   machine state, then reconnect it in the background. Returns its sanitized row.

   `env` and `headers` survive a save that omits them: see `with-preserved-secrets`."
  [name raw-spec]
  (when-not (map? raw-spec)
    (throw (ex-info "MCP server must be an object" {:type :mcp/invalid-server})))
  (let
    [name
     (server-name name)

     raw-spec
     (-> raw-spec
         (dissoc "name")
         canonicalize-server-spec)]

    (ensure-managed! name)
    (let
      [machine
       (or (config/load-global-config-raw) {})

       spec
       (with-preserved-secrets (get-in machine ["mcp" "servers" name]) raw-spec)

       next-config
       (assoc-in machine ["mcp" "servers" name] spec)]

      ;; save-config! is the strict schema and secret-preserving write boundary.
      (config/save-config! next-config :gateway-mcp)
      (reset-server-cache!)
      ;; Saving a server is an explicit start: never leave it pinned down by a
      ;; kill the user has plainly moved on from.
      (revive! name)
      (disconnect! name)
      (reconcile-async!)
      (server-summary name spec true))))

(defn set-gateway-server-enabled!
  "Persist an enabled/disabled override without exposing or accepting secrets."
  [name enabled]
  (let
    [name
     (server-name name)

     current
     (get (machine-servers) name)]

    (when-not current
      (ensure-managed! name)
      (throw (ex-info "Unknown MCP server" {:type :mcp/not-found :server name})))
    (save-gateway-server! name (assoc current "enabled" (boolean enabled)))))

(defn delete-gateway-server!
  "Remove a server from this gateway's machine-owned state and stop it now."
  [name]
  (let
    [name
     (server-name name)

     machine
     (or (config/load-global-config-raw) {})]

    (when-not (get-in machine ["mcp" "servers" name])
      ;; A hand-written server LOOKS deletable in the inventory; deleting the
      ;; machine tier would appear to work and the entry would come straight
      ;; back on the next merge. Say so instead.
      (ensure-managed! name)
      (throw (ex-info "Unknown MCP server in gateway state" {:type :mcp/not-found :server name})))
    (let
      [servers
       (dissoc (get-in machine ["mcp" "servers"]) name)

       machine*
       (if (seq servers)
         (assoc-in machine ["mcp" "servers"] servers)
         (update machine "mcp" dissoc "servers"))

       next-config
       (if (seq (get machine* "mcp")) machine* (dissoc machine* "mcp"))]

      (config/save-config! next-config :gateway-mcp)
      (reset-server-cache!)
      (revive! name)
      (disconnect! name)
      {:name name :is-deleted true})))

(defn kill-gateway-server!
  "Stop `name` NOW and keep it stopped: close the connection (for stdio that
   destroys the child process, forcibly if it will not go) and set the kill brake
   so the per-turn reconcile does not respawn it. Nothing is persisted — use
   `set-gateway-server-enabled!` for a durable off switch. Works for hand-written
   servers too: killing a runaway process is not editing the user's file."
  [name]
  (let
    [name
     (server-name name)

     spec
     (get (raw-servers) name)]

    (when-not spec (throw (ex-info "Unknown MCP server" {:type :mcp/not-found :server name})))
    (swap! killed conj name)
    (disconnect! name)
    (tel/log! {:level :info :id ::killed :data {:server name}} (str "MCP server killed: " name))
    (server-summary name spec (contains? (machine-servers) name))))

(defn start-gateway-server!
  "Undo a kill: release the brake and connect `name` right now. A disabled server
   stays down — `enabled false` is the user's decision, not a stale brake."
  [name]
  (let
    [name
     (server-name name)

     spec
     (get (raw-servers) name)]

    (when-not spec (throw (ex-info "Unknown MCP server" {:type :mcp/not-found :server name})))
    (revive! name)
    (ensure-connected! name)
    (server-summary name spec (contains? (machine-servers) name))))

;; ---------------------------------------------------------------------------
;; Headless OAuth — the Companion app and the TUI are CLIENTS of this gateway,
;; possibly on another device, so they cannot use the loopback browser dance the
;; daemon runs for itself. They start a flow, show the URL, and either the flow
;; lands by itself (the browser did reach this host's listener) or they hand back
;; the redirect URL the user was dumped on.
;; ---------------------------------------------------------------------------

(defn- oauth-server-spec
  "The live client spec for an HTTP `name`, or a typed refusal."
  [name]
  (let [spec (get (configured-servers) name)]
    (when-not spec
      (throw (ex-info "Unknown or disabled MCP server" {:type :mcp/not-found :server name})))
    (when-not (:url spec)
      (throw (ex-info (str "MCP server '" name "' is a stdio server; OAuth applies to HTTP servers")
                      {:type :mcp/invalid-server :server name})))
    spec))

(defn start-gateway-server-auth!
  "Begin a headless OAuth 2.1 flow for HTTP server `name`. Returns
   `{:flow-id :server :kind :url :redirect-uri :expires-at-ms :status}`; the
   caller shows `:url` and the user authorizes in their own browser."
  [name]
  (let
    [name
     (server-name name)

     spec
     (oauth-server-spec name)]

    (mcp-oauth/start-authorization! name (:url spec) {:auth-hint (:auth spec)})))

(defn- settle-auth!
  "A flow that landed leaves the server connected with a STALE 401'd session (or
   not connected at all), so reconnect it once the tokens exist."
  [{:keys [status server] :as row}]
  (when (and (= "ok" status) server (not (conn-of server))) (revive! server) (reconcile-async!))
  row)

(defn complete-gateway-server-auth!
  "Finish flow `flow-id` with the redirect URL the user pasted back (or a bare
   authorization code) and reconnect the server."
  [flow-id input]
  (settle-auth! (mcp-oauth/complete-authorization! flow-id input)))

(defn poll-gateway-server-auth!
  "Non-blocking verdict for `flow-id`: `pending`, `ok`, or `error`. This is how a
   client learns the loopback listener already finished the flow for it."
  [flow-id]
  (settle-auth! (mcp-oauth/poll-authorization! flow-id)))

(defn cancel-gateway-server-auth!
  "Forget an abandoned flow and release its listener."
  [flow-id]
  (mcp-oauth/cancel-authorization! flow-id))

(defn gateway-server-auth-status
  "Non-secret OAuth state for `name`: whether tokens exist, whether they expired."
  [name]
  (mcp-oauth/token-status (server-name name)))

(defn logout-gateway-server-auth!
  "Forget the persisted OAuth tokens for `name` and drop the connection that was
   using them."
  [name]
  (let [name (server-name name)]
    (mcp-oauth/forget! name)
    (disconnect! name)
    (mcp-oauth/token-status name)))

(defn test-gateway-server!
  "Connect a candidate spec without saving it. The connection is always closed;
   only non-secret tool metadata is returned."
  [name raw-spec]
  (let
    [name
     (server-name name)

     spec
     (->> (dissoc raw-spec "name")
          canonicalize-server-spec
          config/runtime-config
          (->client-spec name))

     conn
     (mcp/connect name spec)]

    (try {:name name
          :is-connected true
          :tools (mapv (fn [tool]
                         {:name (get tool "name") :description (get tool "description")})
                       (mcp/list-tools conn))}
         (finally (mcp/close conn)))))

;; ---------------------------------------------------------------------------
;; Verb implementations (env injected by the gate as the first arg)
;; ---------------------------------------------------------------------------

(defn- ok [op result] (extension/success {:op op :result result}))

(defn- err
  [op message & {:as extra}]
  (extension/failure {:result nil
                      :op op
                      :metadata {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                      :error (merge {:message message} extra)}))

(defn- mcp-servers-impl
  [env]
  (let [sid (:session-id env)]
    (ok :mcp/servers
        {"servers" (mapv (fn [[nm spec]]
                           (let
                             [session? (some? (session-spec-of sid nm))
                              conn (conn-of sid nm)]

                             (cond->
                               {"name" nm
                                "transport" (name (transport-of spec))
                                "scope" (if session? "session" "global")
                                "connected" (boolean conn)
                                "enabled" true}
                               (and (not session?) (killed? nm))
                               (assoc "killed" true)

                               conn
                               (assoc "tools" (tool-count conn))

                               (:command spec)
                               (assoc "command" (:command spec))

                               (:url spec)
                               (assoc "url" (:url spec)))))
                         (visible-servers sid))})))

(defn- mcp-tools-impl
  [env server]
  (if-let [conn (ensure-connected! (:session-id env) server)]
    (ok :mcp/tools
        {"server" server
         "tools" (mapv (fn [t]
                         {"name" (get t "name")
                          "description" (get t "description")
                          "input_schema" (get t "inputSchema")})
                       (mcp/list-tools conn))})
    (err :mcp/tools (str "MCP server '"
                         server
                         "' is not configured or is disabled (see ~/.vis/state.yml :mcp :servers).")
         :hint (str "Enabled servers: "
                    (pr-str (vec (keys (visible-servers (:session-id env)))))))))

(defn- mcp-call-impl
  ([env server tool] (mcp-call-impl env server tool {}))
  ([env server tool args]
   (if-let [conn (ensure-connected! (:session-id env) server)]
     (let [r (mcp/call-tool conn tool (if (map? args) args {}))]
       (ok :mcp/call
           {"server" server
            "tool" tool
            "content" (get r "content")
            "is_error" (boolean (get r "isError"))}))
     (err :mcp/call (str "MCP server '"
                         server
                         "' is not configured or is disabled (see ~/.vis/state.yml :mcp :servers).")
          :hint (str "Enabled servers: "
                     (pr-str (vec (keys (visible-servers (:session-id env))))))))))

(defn- mcp-connect-impl
  [env server]
  (revive! server)
  (if-let [conn (ensure-connected! (:session-id env) server)]
    (ok :mcp/connect
        {"server" server
         "connected" true
         "tools" (count (try (mcp/list-tools conn) (catch Throwable _ [])))})
    (err :mcp/connect (str "Could not connect to MCP server '" server "'."))))

(defn- mcp-disconnect-impl
  "Stop `server` for real. A daemon-wide server is also KILLED — closing it alone
   is undone by the next per-turn reconcile, which is not what anyone asking to
   disconnect means. `mcp__connect` starts it again. A session-scoped server has
   no config to be reconciled from, so closing it is enough."
  [env server]
  (let
    [sid
     (:session-id env)

     session?
     (some? (session-spec-of sid server))

     connected
     (some? (conn-of sid server))]

    (when-not session? (swap! killed conj server))
    (disconnect! sid server)
    (ok :mcp/disconnect {"server" server "result" (if connected "disconnected" "not_connected")})))

;; ---------------------------------------------------------------------------
;; Error envelope
;; ---------------------------------------------------------------------------

(defn- mcp-on-error
  [op]
  (fn [err* _env _f _args]
    {:result (extension/failure {:result nil
                                 :op op
                                 :metadata
                                 {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                                 :throwable err*})}))

;; ---------------------------------------------------------------------------
;; Native op-card renderers
;; ---------------------------------------------------------------------------

(defn- mcp-fence [s] (when (seq (str s)) (strutil/fenced s)))

(defn- render-mcp-servers-result
  [r]
  (let [servers (get r "servers")]
    {:summary (str (count servers) " MCP server" (when (not= 1 (count servers)) "s"))
     :body (when (seq servers)
             (str/join
               "\n"
               (map (fn [s]
                      (str "- `"
                           (get s "name")
                           "` "
                           (get s "transport")
                           (if (get s "connected")
                             (str " ✓" (when (get s "tools") (str " (" (get s "tools") " tools)")))
                             " ·")))
                    servers)))}))

(defn- render-mcp-tools-result
  [r]
  (let [tools (get r "tools")]
    {:summary
     (str "`" (get r "server") "` — " (count tools) " tool" (when (not= 1 (count tools)) "s"))
     :body (when (seq tools)
             (str/join "\n"
                       (map (fn [t]
                              (str "- `" (get t "name")
                                   "`" (when (seq (str (get t "description")))
                                         (str " — " (get t "description")))))
                            tools)))}))

(defn- render-mcp-call-result
  [r]
  (let
    [blocks
     (get r "content")

     text
     (->> blocks
          (keep (fn [b]
                  (get b "text")))
          (str/join "\n"))]

    {:summary (str "`" (get r "server") "`/" (get r "tool") (when (get r "is_error") " — error"))
     :body (mcp-fence (if (seq text) text (pr-str blocks)))}))

(defn- render-mcp-connect-result
  [r]
  {:summary (str "connected `" (get r "server")
                 "`" (when (get r "tools") (str " (" (get r "tools") " tools)")))})

(defn- render-mcp-disconnect-result
  [r]
  {:summary (str "disconnected `" (get r "server") "` — " (get r "result"))})

;; ---------------------------------------------------------------------------
;; Public vars retain developer examples and fallback docs. Native symbols
;; below own compact model-facing semantics and exact schemas. Under alias
;; `mcp` the Python names use one underscore; direct native names use two.
;; ---------------------------------------------------------------------------

(def
  ^{:doc
    "List configured MCP servers and status: {\"servers\": [{\"name\": S, \"transport\": \"stdio\"|\"http\", \"connected\": bool, \"enabled\": bool, \"tools\": N (when connected), \"command\"/\"url\": S}]}. Connections are lazy via mcp__tools/mcp__call, or explicit via mcp__connect. Config: ~/.vis/state.yml :mcp :servers."
    :arglists '([])}
  mcp-servers
  mcp-servers-impl)

(def
  ^{:doc
    "Connect if needed and list a server's tools: {\"server\": S, \"tools\": [{\"name\": S, \"description\": S, \"input_schema\": <JSON schema dict>}]}. Use input_schema for mcp__call args."
    :arglists '([server])}
  mcp-tools
  mcp-tools-impl)

(def
  ^{:doc
    "Connect if needed and invoke `tool` with `args` matching its input_schema (omit or {} for none). Returns {\"server\": S, \"tool\": S, \"content\": [<MCP content blocks>], \"is_error\": bool}; text is at content[i][\"text\"]."
    :arglists '([server tool] [server tool args])}
  mcp-call
  mcp-call-impl)

(def
  ^{:doc
    "Connect a configured server into the daemon-wide pool. Usually unnecessary: mcp__tools/mcp__call connect lazily and /reload reconciles config. Returns {\"server\": S, \"connected\": bool, \"tools\": N}."
    :arglists '([server])}
  mcp-connect
  mcp-connect-impl)

(def
  ^{:doc
    "Disconnect from the daemon-wide pool, closing the connection and any stdio child. Returns {\"server\": S, \"result\": \"disconnected\"|\"not_connected\"}. A later /reload may reconnect configured servers."
    :arglists '([server])}
  mcp-disconnect
  mcp-disconnect-impl)

;; ---------------------------------------------------------------------------
;; Symbols + ctx + extension
;; ---------------------------------------------------------------------------

;; Tool NAMES use a `mcp__` (DOUBLE-underscore) prefix, never `mcp_`.
;; Anthropic's Claude-subscription OAuth endpoint reserves the
;; single-underscore `mcp_<x>` namespace for its own managed MCP-connector
;; tools; a CLIENT tool named `mcp_<x>` makes the whole request classify as a
;; third-party MCP integration and 400s. Do NOT revert to single underscore.
(def ^:private mcp-symbols
  [(vis/symbol
     #'mcp-servers
     {:symbol 'servers
      :name "mcp__servers"
      :native-tool? true
      :result
      "String-keyed `{op,servers:[{name,transport,connected,enabled,tools?,command?,url?}]}`."
      :description
      "List configured MCP servers and connection state. In `python_execution`, call `await mcp_servers()`."
      :render render-mcp-servers-result
      :call {:pos []}
      :color-role :tool-color/meta
      :schema {:type "object" :properties {} :required [] :additionalProperties false}
      :tag :observation
      :on-error-fn (mcp-on-error :mcp/servers)})
   (vis/symbol
     #'mcp-tools
     {:symbol 'tools
      :name "mcp__tools"
      :native-tool? true
      :result
      "String-keyed `{op,server,tools:[{name,description,input_schema}]}`; descriptions may be null."
      :description
      "Discover a server's live tools and input schemas; auto-connects. In `python_execution`, call `await mcp_tools(...)`."
      :render render-mcp-tools-result
      :call {:pos ["server"]}
      :color-role :tool-color/meta
      :schema {:type "object"
               :properties {"server" {:type "string"
                                      :description "Configured server; auto-connects."}}
               :required ["server"]
               :additionalProperties false}
      :tag :observation
      :on-error-fn (mcp-on-error :mcp/tools)})
   (vis/symbol
     #'mcp-call
     {:symbol 'call
      :name "mcp__call"
      :native-tool? true
      :result
      "String-keyed `{op,server,tool,content,is_error}`; `content` is MCP blocks, with text at `block[\"text\"]`."
      :description
      "Call a discovered server tool using its input schema; auto-connects. In `python_execution`, call `await mcp_call(...)`."
      :render render-mcp-call-result
      :call {:pos ["server" "tool"] :opt-pos ["args"]}
      :color-role :tool-color/shell
      :schema {:type "object"
               :properties
               {"server" {:type "string" :description "Configured server; auto-connects."}
                "tool" {:type "string" :description "Discovered tool name."}
                "args" {:type "object" :description "Input-schema args; omit or `{}` for none."}}
               :required ["server" "tool"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/call)})
   (vis/symbol
     #'mcp-connect
     {:symbol 'connect
      :name "mcp__connect"
      :native-tool? true
      :result "String-keyed `{op,server,connected:true,tools}`."
      :description
      "Explicitly connect a configured server; tools/calls usually auto-connect and `/reload` reconciles the pool. In `python_execution`, call `await mcp_connect(...)`."
      :render render-mcp-connect-result
      :call {:pos ["server"]}
      :color-role :tool-color/create
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Configured server."}}
               :required ["server"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/connect)})
   (vis/symbol
     #'mcp-disconnect
     {:symbol 'disconnect
      :name "mcp__disconnect"
      :native-tool? true
      :result "String-keyed `{op,server,result}`, where result is `disconnected|not_connected`."
      :description
      "Disconnect from the pool, closing the connection and terminating any stdio child; `/reload` reconciles config. In `python_execution`, call `await mcp_disconnect(...)`."
      :render render-mcp-disconnect-result
      :call {:pos ["server"]}
      :color-role :tool-color/delete
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Configured server."}}
               :required ["server"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/disconnect)})])

(defn- contribute
  "`:ext/ctx-fn` — proactively reconcile the daemon-wide MCP pool, then surface
   the CONNECTED servers (+ tool counts) so the model sees what's reachable at
   `session[\"env\"][\"mcp\"][\"servers\"]`. The daemon-wide pool is shared across
   every session; servers a client attached to THIS session (ACP `mcpServers`)
   are listed alongside it, marked `\"scope\": \"session\"`."
  [env]
  (reconcile-async!)
  (let
    [sid
     (:session-id env)

     row
     (fn [scope nm conn]
       {"name" nm "scope" scope "transport" (name (:transport conn)) "tools" (tool-count conn)})

     session-rows
     (keep (fn [[[s nm] {:keys [conn]}]]
             (when (and (= s sid) conn) (row "session" nm conn)))
           @session-conns)

     shadowed
     (set (map #(get % "name") session-rows))

     global-rows
     (keep (fn [[nm {:keys [conn]}]]
             (when-not (contains? shadowed nm) (row "global" nm conn)))
           @conns)

     rows
     (vec (concat session-rows global-rows))]

    (when (seq rows) {"session_env" {"mcp" {"servers" rows}}})))

(defn- activation-fn
  "Active when at least one MCP server is configured, or a client attached one to
   this session."
  [env]
  (boolean (or (seq (configured-servers)) (seq (get @session-specs (:session-id env))))))

(defonce ^:private _mcp-reload-hook (extension/register-reload-hook! ::reconcile reconcile!))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-mcp"
     :ext/description
     "MCP client: one gateway-wide pool auto-connects and `/reload`-reconciles configured (`:mcp :servers`) stdio/Streamable HTTP servers; `mcp__servers`, `mcp__tools`, and `mcp__call` reach every session. Supports remote OAuth 2.1 discovery + PKCE (2025-06-18). Always on; active with servers."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/engine {:ext.engine/alias 'mcp :ext.engine/symbols mcp-symbols}
     :ext/ctx-fn contribute
     :ext/kind "foundation"}))

(vis/register-extension! vis-extension)
