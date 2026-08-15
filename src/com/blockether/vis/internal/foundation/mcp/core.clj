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

   ONE model-facing verb under alias `mcp` (flat sandbox renders `alias_name`):
     mcp__call(server, tool, args) - call a tool
     mcp__call(server)             - that server's descriptions + input schemas

   There is deliberately NO connect/disconnect verb. The daemon connects every
   enabled server, health-checks the pool on its own clock, and reaps/respawns a
   dead one; a tool call self-heals its connection too. Starting or stopping a
   server is a human admin action on the gateway API (save/enable/kill/start),
   never something one session does to a resource every other session shares.

   Every visible server - its status and the NAMES of the tools it exposes -
   rides in ctx under `env.mcp`, keyed by server name so a change diffs per
   server. That IS the inventory: no listing verb spends a turn re-fetching what
   the session object already carries."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.foundation.mcp.oauth :as mcp-oauth]
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

(defn- wire-transport
  "The transport as every wire surface spells it: snake_case, matching what a
   client PUTs back. `transport-of` is the internal kebab keyword; rendering it
   with `name` leaked `streamable-http` into ctx while `/v1/mcp/servers` said
   `streamable_http`, so one MCP server appeared to have two transports."
  [spec]
  (case (transport-of spec)
    :streamable-http
    "streamable_http"

    "stdio"))

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
   static `Authorization` header, `:timeout-ms` / `:listen?` forwarded.

   The `WWW-Authenticate` atom is SHARED with the transport instead of staying
   private to the token provider: every HTTP server without a static header gets
   a `:bearer-fn`, so having one says nothing about whether the server wants
   OAuth - a recorded Bearer challenge does."
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

     www-auth
     (atom nil)

     bearer-fn
     (when (and (= :streamable-http transport) (not has-static-auth?))
       (mcp-oauth/make-bearer-fn server-name (:url s) www-auth))]

    (cond-> (assoc s :transport transport)
      (:timeout_ms s)
      (assoc :timeout-ms (:timeout_ms s))

      (:listen s)
      (assoc :listen? true)

      bearer-fn
      (assoc :bearer-fn
        bearer-fn :www-auth-atom
        www-auth))))

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
;; until someone starts the server again (start, save, enable). Deliberately
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

;; Session-scoped servers — servers a CLIENT attaches to ONE session when it
;; opens or loads that session. They are NEVER written
;; to config and never leak into another session: an editor wiring a server for
;; its own workspace must not mutate the daemon for the TUI, the phone, and every
;; other session. Their pool is keyed by `[session-id server]`, so a session
;; server SHADOWS a global one of the same name without disturbing it.
(defonce ^:private session-specs (atom {})) ; {session-id {server client-spec}}

(defonce ^:private session-conns (atom {})) ; {[session-id server] {:conn :spec}}

(defn- close-in-pool!
  "Atomically drop the conn cached in `pool` at `k` (if any), then close it.

   The DROP takes the pool monitor — publication uses the same monitor, so a
   connection finishing concurrently cannot slip past a stop/config change and
   become an untracked live process. The CLOSE runs AFTER the monitor is
   released: tearing down an stdio tree waits up to two seconds for SIGTERM
   before SIGKILL, and an HTTP close waits on a `DELETE` round trip. Holding the
   daemon-wide monitor across that made one slow kill freeze every other
   server's connect — in every session — for the same window. Once dropped the
   conn is unreachable from the pool, so closing it late is still exclusive."
  [pool k]
  (let
    [conn (locking pool
            (when-let [conn (get-in @pool [k :conn])]
              (swap! pool dissoc k)
              conn))]
    (when conn (try (mcp/close conn) (catch Throwable _ nil))))
  nil)

(defn- ensure-in-pool!
  "Idempotently connect `server` under `spec`, caching the conn (with the spec it
   was connected under) in `pool` at `k`. `accept?` is checked while publishing:
   if a kill, disable, delete, or spec change won during the handshake, the new
   connection is closed instead of leaking past that decision. Concurrent handshakes
   also converge on one cached conn; every losing process is closed. Returns the
   accepted conn, or nil when connection failed / became stale.

   Publication decides under the monitor and closes the losers OUTSIDE it, for the
   same reason `close-in-pool!` does: a loser's teardown can block for seconds and
   must not stall the pool it has already been excluded from."
  [pool k server spec accept?]
  (let [existing (get @pool k)]
    (cond (and existing (mcp/alive? (:conn existing))) (:conn existing)
          existing (do (close-in-pool! pool k) (recur pool k server spec accept?))
          :else (try (let
                       [conn (mcp/connect server spec)
                        [accepted stale]
                        (locking pool
                          (let [winner (get @pool k)]
                            (cond (not (accept?)) [nil [conn]]
                                  (and winner (mcp/alive? (:conn winner))) [(:conn winner) [conn]]
                                  :else (do (swap! pool assoc k {:conn conn :spec spec})
                                            [conn (when winner [(:conn winner)])]))))]

                       (run! (fn [c]
                               (try (mcp/close c) (catch Throwable _ nil)))
                             stale)
                       accepted)
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
   matching pool. Returns the conn or nil (unknown / disabled / killed / failed).

   The kill brake is enforced HERE, not only in `reconcile!`: a TOOL CALL is a
   connect path of its own, so checking only in the reconcile loop let the first
   `mcp_call` after a kill respawn the very stdio child the user just stopped.
   The publication predicate also closes a handshake that loses a race with kill,
   disable, detach, delete, or a spec replacement. Session-scoped servers are never
   governed by the daemon-wide kill brake."
  ([server] (ensure-connected! nil server))
  ([session-id server]
   (if-let [spec (session-spec-of session-id server)]
     (ensure-in-pool! session-conns
                      [session-id server]
                      server
                      spec
                      #(= spec (session-spec-of session-id server)))
     (when-not (killed? server)
       (when-let [spec (get (configured-servers) server)]
         (ensure-in-pool! conns
                          server
                          server
                          spec
                          #(and (not (killed? server))
                                (= spec (get (configured-servers) server)))))))))

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
    ;; `ensure-connected!` owns the kill brake, so this loop stays a plain
    ;; "connect what config declares" — one place decides, not two.
    (run! ensure-connected! (keys cfg))
    (doseq [[k {:keys [conn]}] @session-conns]
      (when-not (mcp/alive? conn) (close-in-pool! session-conns k)))
    nil))

(defn- reconcile-once!
  "Single-flight `reconcile!`: one already in flight wins, and a throwing one is
   logged rather than propagated to whoever nudged it."
  []
  (when (compare-and-set! reconciling? false true)
    (try (reconcile!)
         (catch Throwable t
           (tel/log! {:level :warn :id ::reconcile-failed :data {:error (ex-message t)}}
                     "MCP reconcile failed"))
         (finally (reset! reconciling? false)))))

(defonce ^:private supervisor (atom nil))

(def ^:private health-interval-ms 30000)

(defn- ensure-supervisor!
  "Arm the daemon-wide MCP health loop, once. Every `health-interval-ms` it
   reconciles the pool, so a crashed stdio child, a dropped HTTP session, or an
   edited spec is repaired on the GATEWAY's clock instead of waiting for a turn —
   which is what lets sessions have no connect/disconnect verb at all. Idempotent,
   armed only once a server is configured, and daemon-threaded so it can never
   hold the JVM open."
  []
  (when (and (nil? @supervisor) (seq (configured-servers)))
    (let
      [^java.util.concurrent.ScheduledExecutorService ex
       (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
         (reify
           java.util.concurrent.ThreadFactory
             (newThread [_ r] (doto (Thread. ^Runnable r "vis-mcp-health") (.setDaemon true)))))]
      (if (compare-and-set! supervisor nil ex)
        (.scheduleWithFixedDelay ex
                                 ^Runnable
                                 (fn []
                                   (reconcile-once!))
                                 (long health-interval-ms)
                                 (long health-interval-ms)
                                 java.util.concurrent.TimeUnit/MILLISECONDS)
        (.shutdownNow ex))))
  nil)

(defn- reconcile-async!
  "Nudge the pool toward config in the background — safe from a turn's ctx-fn or
   an HTTP handler without blocking either on a slow stdio spawn — and arm the
   health loop, so the first look at MCP puts the gateway in charge of keeping
   those servers alive from then on."
  []
  (ensure-supervisor!)
  (when-not @reconciling? (future (reconcile-once!)))
  nil)

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
  "The session-scoped servers attached to `session-id` as string-keyed rows
   `[{name, transport, is_connected}]`, in name order — like every MCP surface."
  [session-id]
  (mapv (fn [[nm spec]]
          {"name" nm
           "transport" (name (transport-of spec))
           "is_connected" (boolean (get-in @session-conns [[session-id nm] :conn]))})
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
   `{connected [name…], failed [{server …, error …}…]}` — STRING-keyed, like every
   MCP surface. A detach/replacement
   racing a handshake wins: the just-opened transport is closed, never leaked."
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
              (if (ensure-in-pool! session-conns
                                   [session-id nm]
                                   nm
                                   spec
                                   #(= spec (session-spec-of session-id nm)))
                (update acc "connected" conj nm)
                (update acc
                        "failed"
                        conj
                        {"server" nm "error" "MCP session server was detached while connecting"})))
            {"connected" [] "failed" []}
            coerced)))

(defn- conn-tools
  "The tools `conn` exposes, as raw `{\"name\" \"description\" \"inputSchema\"}` maps.
   `mcp/list-tools` CACHES into the conn, so the first read pays one RPC and every
   later read is free. Reading the raw `:tools` atom instead reports NOTHING for a
   freshly connected server — that cache is only filled on demand — which is what
   made a healthy gateway server look empty in the Companion inventory and in the
   model's own `env.mcp` view. Never throws: a server that dies mid-read is simply
   toolless until the next reconcile."
  [conn]
  (if-not conn
    []
    (or (some-> (:tools conn)
                deref)
        (try (mcp/list-tools conn) (catch Throwable _ nil))
        [])))

(defn- tool-count
  "How many tools `conn` exposes; 0 when it is not connected."
  [conn]
  (count (conn-tools conn)))

(def ^:private max-ctx-tools
  "Cap on the tool NAMES listed per server in ctx, so one enormous server cannot
   eat the context budget. The full catalog - descriptions and input schemas -
   stays one `mcp__call` (server alone, no tool) away."
  40)

(defn- ctx-tool-names
  "`{:names [...] :omitted n}` — the SORTED, capped tool names of `conn` for the
   ctx block. Sorted on purpose: ctx is diffed structurally, so a stable order is
   what lets an unchanged server re-render with no delta at all."
  [conn]
  (let
    [cap
     (long max-ctx-tools)

     names
     (sort (keep #(get % "name") (conn-tools conn)))

     n
     (long (count names))]

    {:names (vec (take cap names)) :omitted (max 0 (- n cap))}))

(defn- server-summary
  "One sanitized inventory row. STRING-KEYED, snake_case — exactly what a client
   reads off the wire. Nothing here is a keyword: this map is JSON the moment it
   leaves the gateway, and the TUI/Companion read it back by string key."
  [name raw-spec is-managed]
  (let
    [spec
     (config/runtime-config raw-spec)

     conn
     (conn-of name)]

    (cond->
      {"name" name
       "transport" (wire-transport spec)
       "enabled" (enabled? spec)
       "is_connected" (boolean conn)
       ;; Whether the GATEWAY owns this entry. A server declared in a hand-written
       ;; tier is the user's file: listed, never rewritten from here.
       "is_managed" (boolean is-managed)
       ;; Stopped by a client and HELD down until explicitly started again.
       "is_killed" (killed? name)
       "tools" (tool-count conn)}
      (:command spec)
      (assoc "command" (:command spec))

      (:cwd spec)
      (assoc "cwd" (:cwd spec))

      ;; `args` and `timeout_ms` are NOT secrets — they are the rest of the spec a
      ;; client needs to render an edit form. Without them an edit round-trip
      ;; would save back a server that had silently lost its arguments; `env` and
      ;; `headers` stay out and are instead preserved by omission on save.
      (seq (:args spec))
      (assoc "args" (mapv str (:args spec)))

      ;; `runtime-config` has already kebab-cased the wire key `timeout_ms`.
      (:timeout-ms spec)
      (assoc "timeout_ms" (:timeout-ms spec))

      (:url spec)
      ;; OAuth is only ever an HTTP concern; `is_authorized` lets a client offer
      ;; "Sign in" instead of letting the user stare at a server that 401s.
      (assoc "url"
        (:url spec) "is_authorized"
        (get (mcp-oauth/token-status name) "is_authorized")))))

(defn gateway-servers
  "Sanitized MCP inventory for gateway management. Secrets (env and header values)
   deliberately never cross this boundary. Every configured server is listed —
   gateway-owned and hand-written alike — and `is_managed` says which of them
   this API may write. Reading the inventory also nudges the pool toward config
   and arms the health loop, so a client that has not run a turn yet still gets —
   not merely sees — the connections the gateway owes it.

   String-keyed throughout, like every MCP surface."
  []
  (reconcile-async!)
  (let [machine (machine-servers)]
    {"servers" (->> (raw-servers)
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
      {"name" name "is_deleted" true})))

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
   `{flow_id, server, kind, url, redirect_uri, expires_at_ms, status}` —
   string-keyed, like every MCP surface. The caller shows `url` and the user
   authorizes in their own browser."
  [name]
  (let
    [name
     (server-name name)

     spec
     (oauth-server-spec name)]

    (mcp-oauth/start-authorization! name (:url spec) {:auth-hint (:auth spec)})))

(defn- settle-auth!
  "A flow that landed leaves the server connected with a STALE 401'd session (or
   not connected at all), so reconnect it once the tokens exist. `row` is the
   string-keyed wire view of the flow."
  [row]
  (let
    [status
     (get row "status")

     server
     (get row "server")]

    (when (and (= "ok" status) server (not (conn-of server))) (revive! server) (reconcile-async!)))
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
   only non-secret tool metadata is returned, string-keyed like the inventory."
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

    (try {"name" name
          "is_connected" true
          "tools" (mapv (fn [tool]
                          {"name" (get tool "name") "description" (get tool "description")})
                        (mcp/list-tools conn))}
         (finally (mcp/close conn)))))

;; ---------------------------------------------------------------------------
;; Verb implementations (env injected by the gate as the first arg)
;; ---------------------------------------------------------------------------

(defn- ok [op result] (extension/success {:op op :result result}))

(defn- err
  "Failure envelope. `:message` and `:hint` are the ENGINE's error contract (and
   the only keywords allowed through here); every MCP fact carried alongside them
   — server, tool, catalog, target — is snake_case STRING-keyed, so nothing
   keyword-shaped can reach the model or a client."
  [op message & {:as extra}]
  (extension/failure {:result nil
                      :op op
                      :metadata {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                      :error (reduce-kv
                               (fn [m k v]
                                 (assoc m (if (= :hint k) :hint (str/replace (name k) "-" "_")) v))
                               {:message message}
                               extra)}))

(defn- needs-auth?
  "True when `server` is an OAuth server nobody has signed into: it answered a
   request with a `WWW-Authenticate` challenge and no usable token is persisted.

   The challenge is the load-bearing half. Every HTTP server without a static
   `Authorization` header gets a synthesised `:bearer-fn`, so keying off that
   alone would label a plain unauthenticated server that is merely DOWN as
   \"signed out\" and send its user hunting for a login screen that does not
   exist."
  [session-id server]
  (let
    [spec
     (get (visible-servers session-id) server)

     challenged?
     (boolean (some-> ^clojure.lang.IDeref (:www-auth-atom spec)
                      deref))

     status
     (when challenged? (mcp-oauth/token-status server))]

    (boolean (and challenged?
                  (or (not (get status "is_authorized"))
                      (and (get status "is_expired") (not (get status "has_refresh_token"))))))))

(defn- authorize-hint
  "Where a human goes to authorize `server`. The gateway route is headless: it
   answers with a URL to open, so it works from a phone as well as from the
   machine running the daemon."
  [server]
  (str "Authorize it in Settings -> MCP Servers, or POST /v1/mcp/servers/"
       server
       "/auth/start, which answers with the URL to open."))

(defn- unavailable-err
  "The refusal for a server ctx advertises but the pool cannot reach. Four very
   different instructions hide behind one failure, so they are told apart: an
   OAuth server nobody signed into needs a sign-in; a KILLED server is configured
   and enabled, so pointing at `:mcp :servers` would send its user editing a file
   that is already right; a configured server that simply will not connect is a
   transport problem, and telling its user it \"is not configured\" while listing
   it as enabled in the same breath is a lie; only an unknown name is config."
  [env server]
  (let [spec (get (visible-servers (:session-id env)) server)]
    (cond
      (needs-auth? (:session-id env) server)
      (err :mcp/call (str "MCP server '" server "' is not authorized yet - nobody has signed in.")
           :server server
           :hint (authorize-hint server))
      (killed? server)
      (err :mcp/call
           (str "MCP server '" server "' was stopped and is held down until it is started again.")
           :hint (str "Start it from Settings -> MCP Servers, or POST /v1/mcp/servers/"
                      server
                      "/actions/start"))
      spec
      (err
        :mcp/call
        (str "MCP server '" server "' is configured but unreachable - the connection failed.")
        :server server
        :target (or (:url spec) (:command spec))
        :hint
        "Check its url/command and that the server is up; the daemon retries on its own, so a later call may work.")
      :else (err :mcp/call
                 (str "MCP server '"
                      server
                      "' is not configured or is disabled (see ~/.vis/state.yml :mcp :servers).")
                 :hint (str "Enabled servers: "
                            (pr-str (vec (keys (visible-servers (:session-id env))))))))))

(defn- tool-rows
  "`conn`'s catalog in wire shape. Reads the conn's CACHED listing, so asking for
   schemas twice in a turn costs one RPC, not two."
  [conn]
  (mapv (fn [t]
          {"name" (get t "name")
           "description" (get t "description")
           "input_schema" (get t "inputSchema")})
        (conn-tools conn)))

(defn- doc-corpus-entries
  "Every tool of every VISIBLE MCP server as a `doc-corpus` entry, so
   `apropos(\"jira\")` reaches a server-supplied description — the only place
   that text survives now that `mcp__call` carries no schema.

   Reads the conn's CACHED listing ONLY: assembling the corpus must never cost
   an RPC, so a server nobody has listed yet contributes nothing until it does."
  []
  (into []
        (mapcat
          (fn [[server _spec]]
            (let
              [cached (or (some-> (:tools (conn-of server))
                                  deref)
                          [])]
              (keep (fn [t]
                      (when-let [nm (not-empty (str (get t "name")))]
                        {:name (str server "/" nm)
                         :kind "mcp"
                         :call (str "mcp__call(" (pr-str (str server)) ", " (pr-str nm) ", {})")
                         :text (str "MCP tool `"
                                    nm
                                    "` on server `"
                                    server
                                    "`."
                                    (when-let [d (not-empty (str (get t "description")))]
                                      (str "\n\n" d))
                                    (when-let [sch (get t "inputSchema")]
                                      (str "\n\nInput schema: " (pr-str sch))))}))
                    cached))))
        (visible-servers nil)))

(defn- doc-corpus-stamp
  "Which servers are visible and how many tools each has ALREADY listed. Cheap
   and RPC-free, exactly like the entries it guards: a listing that lands later
   changes a count, and the corpus rebuilds then."
  []
  (mapv (fn [[server _spec]]
          [server
           (count (or (some-> (:tools (conn-of server))
                              deref)
                      []))])
        (visible-servers nil)))

(doc-corpus/register-source! :mcp-tools #'doc-corpus-stamp #'doc-corpus-entries)

(defn- call-failed-err
  "Turn anything thrown below this point into a typed refusal. A tool call
   crosses a process boundary or a network, so it fails in ways no argument check
   predicts: a token that expired mid-turn, a server that died between the
   catalog and the call, a payload the peer rejects. None of it may escape as a
   raw exception - a throwing extension ends the turn, while an error envelope
   leaves the model free to fix its arguments or the user free to sign in."
  [env server tool ^Throwable t]
  (if (= :mcp/oauth-required (:type (ex-data t)))
    (err :mcp/call
         (str "MCP server '" server "' is not authorized - its OAuth token is missing or expired.")
         :server server
         :hint (authorize-hint server))
    (err :mcp/call (str "MCP call to '" server
                        "'" (when (string? tool) (str " tool '" tool "'"))
                        " failed: " (or (ex-message t) (str t)))
         :server server
         :tool (when (string? tool) tool)
         :hint (str "Call mcp__call with `server` alone to re-read the tool's input schema"
                    (when-not (needs-auth? (:session-id env) server)
                      ", or check the server's status in session env.mcp")
                    "."))))

(defn- mcp-call-impl
  "The whole model-facing surface. `server` alone answers with that server's tools,
   descriptions and input schemas; `server` + `tool` invokes. There is no inventory
   verb because the inventory - every visible server, its status, its tool NAMES -
   already rides in ctx under `env.mcp`, so a call only has to name the server.

   Every failure leaves here as an error envelope, never as a throw: connecting,
   listing and calling all reach a remote peer."
  ([env server] (mcp-call-impl env server nil nil))
  ([env server tool] (mcp-call-impl env server tool nil))
  ([env server tool args]
   (try
     (if-let [conn (ensure-connected! (:session-id env) server)]
       (let
         [rows (tool-rows conn)
          row (when (string? tool) (first (filter #(= tool (get % "name")) rows)))]

         (cond
           ;; No tool named - or an args map drifted into its slot. Answer with the
           ;; catalog rather than guessing at a call: this IS the schema lookup.
           (or (not (string? tool)) (str/blank? tool)) (ok :mcp/call {"server" server "tools" rows})
           ;; Unknown name, judged only against a catalog we actually have: a
           ;; momentarily empty listing must never refuse a real call.
           (and (seq rows) (nil? row))
           (err :mcp/call (str "MCP server '" server "' exposes no tool '" tool "'.")
                :server server
                :tools (mapv #(get % "name") rows)
                :hint
                "Call mcp__call with `server` alone for every tool's description and input schema.")
           :else (let
                   [r (mcp/call-tool conn tool (if (map? args) args {}))
                    is-error (boolean (get r "isError"))]

                   (ok
                     :mcp/call
                     (cond->
                       {"server" server "tool" tool "content" (get r "content") "is_error" is-error}
                       ;; A refused call is nearly always an argument mismatch, and the
                       ;; schema is already cached: ship it WITH the refusal so the retry
                       ;; costs no extra round trip.
                       (and is-error row)
                       (assoc "input_schema" (get row "input_schema")))))))
       (unavailable-err env server))
     (catch Throwable t (call-failed-err env server tool t)))))

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
;; Public vars retain developer examples and fallback docs. Native symbols
;; below own compact model-facing semantics and exact schemas. Under alias
;; `mcp` the Python names use one underscore; direct native names use two.
;; ---------------------------------------------------------------------------



(def
  ^{:doc
    "Invoke `tool` on configured MCP server `server` with `args` matching its input schema (omit or {} for none): {\"server\": S, \"tool\": S, \"content\": [<MCP content blocks>], \"is_error\": bool}; text is at content[i][\"text\"], and a refused call carries \"input_schema\" back with it. Called with `server` alone it lists that server's catalog instead: {\"server\": S, \"tools\": [{\"name\": S, \"description\": S, \"input_schema\": <JSON schema dict>}]}. Server names, status and tool names already ride in ctx under env.mcp; the gateway connects and heals every enabled server on its own clock, so there is no connect/disconnect verb. Config: ~/.vis/state.yml :mcp :servers."
    :arglists '([server] [server tool] [server tool args])}
  mcp-call
  mcp-call-impl)

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
     #'mcp-call
     {:symbol 'call
      :name "mcp__call"
      :result
      "String-keyed `{op,server,tool,content,is_error,input_schema?}`; text at `block[\"text\"]`. With `tool` omitted: `{op,server,tools:[{name,description,input_schema}]}`."
      :description
      "Call a tool on an MCP server; auto-connects. Servers and their tool names are already in `session[\"env\"][\"mcp\"]`, so just name them. Omit `tool` for that server's input schemas. In `python_execution`, call `await mcp_call(...)`."
      :call {:pos ["server"] :opt-pos ["tool" "args"]}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/call)})])

(defn- contribute
  "`:ext/ctx-fn` - proactively reconcile the daemon-wide MCP pool, then surface
   every VISIBLE server and the tool NAMES it exposes at
   `session[\"env\"][\"mcp\"][\"servers\"]`. This IS the MCP inventory: with it in the
   session object there is no listing verb, and `mcp__call` only ever has to name
   a server (and, to invoke, a tool).

   Keyed BY SERVER NAME (a sorted map, never a list) on purpose: the ctx delta is
   a recursive structural diff, so one server connecting, dying, or gaining a tool
   emits exactly `session[\"env\"][\"mcp\"][\"servers\"][\"<name>\"][...] = ...` instead
   of re-sending the whole inventory. A list would re-send every server on any
   change.

   Servers that are not usable are listed too, with `\"status\"`: the model must be
   able to tell \"no such server\" from \"stopped - start it again\" from \"signed
   out\" (`\"needs_auth\"` - an OAuth server with no live token, where asking for
   tools can only produce a refusal). The daemon-wide pool is shared across every
   session; servers a client attached to THIS session are
   listed alongside it and shadow a global one of the same name, marked
   `\"scope\": \"session\"`."
  [env]
  (reconcile-async!)
  (let
    [sid
     (:session-id env)

     entry
     (fn [[nm spec]]
       (let
         [session?
          (some? (session-spec-of sid nm))

          conn
          (conn-of sid nm)

          {:keys [names omitted]}
          (ctx-tool-names conn)]

         [nm
          (cond->
            {"scope" (if session? "session" "global")
             "transport" (wire-transport spec)
             "status" (cond conn "connected"
                            (and (not session?) (killed? nm)) "killed"
                            (needs-auth? sid nm) "needs_auth"
                            :else "disconnected")}
            conn
            (assoc "tools" names)

            (pos? (long omitted))
            (assoc "tools_omitted" omitted))]))

     rows
     (into (sorted-map) (map entry) (visible-servers sid))]

    (when (seq rows) {"session_env" {"mcp" {"servers" rows}}})))

(defn- activation-fn
  "Active when at least one MCP server is configured, or a client attached one to
   this session."
  [env]
  (boolean (or (seq (configured-servers)) (seq (get @session-specs (:session-id env))))))

;; `/reload` re-reads config, so it is also where a gateway that has never run a
;; turn first learns it owns servers: reconcile synchronously (so the reply
;; reflects the new pool) and arm the health loop that keeps it that way.
(defonce ^:private _mcp-reload-hook
  (extension/register-reload-hook! ::reconcile
                                   (fn []
                                     (ensure-supervisor!)
                                     (reconcile!))))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-mcp"
     :ext/description
     "MCP client: one gateway-wide pool connects every enabled (`:mcp :servers`) stdio/Streamable HTTP server, health-checks it on the daemon's own clock, and `/reload`-reconciles it. The inventory rides in ctx under `env.mcp`, so the single verb `mcp__call` reaches every session (server alone lists schemas) and there is no per-session connect/disconnect. Supports remote OAuth 2.1 discovery + PKCE (2025-06-18). Always on; active with servers."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/engine {:ext.engine/alias 'mcp :ext.engine/symbols mcp-symbols}
     :ext/ctx-fn contribute
     :ext/kind "foundation"}))

(vis/register-extension! vis-extension)
