(ns com.blockether.vis.internal.foundation.mcp.core
  "Built-in Model Context Protocol (MCP) surface. The gateway daemon owns ONE
   shared pool of MCP connections (`{server {:conn spec}}`); every session sees
   the same live tools. Always on: MCP is core infrastructure, not a droppable
   plug-in and not gated by any toggle. The pool is empty (and costs nothing)
   until at least one server is declared in config.

   Servers are declared natively in `~/.vis/state.yml`:

     {:mcp {:servers {\"filesystem\" {:transport :stdio :command \"npx\"
                                    :args [\"-y\" \"@modelcontextprotocol/server-filesystem\" \"/path\"]}
                      \"remote\"     {:transport :http :url \"https://...\"
                                    :headers {\"Authorization\" \"Bearer ${MY_TOKEN}\"}
                                    :timeout_ms 60000
                                    :listen true
                                    :auth {:client_id \"...\" :scope \"...\"}}
                      \"stale\"      {:enabled false :url \"...\"}}}}

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
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.foundation.mcp.oauth :as mcp-oauth]
            [taoensso.telemere :as tel]))

(defn- now-ms [] (System/currentTimeMillis))

;; ---------------------------------------------------------------------------
;; Config — declared servers from ~/.vis/state.yml :mcp :servers
;; ---------------------------------------------------------------------------

(defn- transport-of [spec] (or (:transport spec) (if (:url spec) :http :stdio)))

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
     (when (and (= :http transport) (not has-static-auth?))
       (mcp-oauth/make-bearer-fn server-name (:url s) (atom nil) (:auth s)))]

    (cond-> s
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
     (get-in (or (vis/load-config-raw) {}) ["mcp"])

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
                            (vis/runtime-config v)]

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

(defn- conn-of [server] (get-in @conns [server :conn]))

(defn- disconnect!
  "Close `server`'s connection (if any) and drop it from the daemon-wide pool."
  [server]
  (when-let [conn (conn-of server)]
    (try (mcp/close conn) (catch Throwable _ nil))
    (swap! conns dissoc server))
  server)

(defn- ensure-connected!
  "Idempotently connect the configured `server`, caching the conn (with the
   spec it was connected under) in the daemon-wide pool. Returns the conn or
   nil (unknown / disabled / failed). A cached-but-dead conn is reaped and
   respawned on the next call."
  [server]
  (let [existing (get @conns server)]
    (cond (and existing (mcp/alive? (:conn existing))) (:conn existing)
          existing (do (disconnect! server) (recur server))
          :else (when-let [spec (get (configured-servers) server)]
                  (try (let [conn (mcp/connect server spec)]
                         (swap! conns assoc server {:conn conn :spec spec})
                         conn)
                       (catch Throwable t
                         (tel/log! {:level :warn
                                    :id ::connect-failed
                                    :data {:server server :error (ex-message t)}}
                                   "MCP connect failed")
                         nil))))))

(defn- reconcile!
  "Reconcile the daemon-wide pool to config: connect newly-enabled servers,
   close entries whose server is gone / disabled / spec-changed, and reap dead
   conns (crashed stdio child, closed HTTP session). Runs on every `/reload`
   and proactively per turn."
  []
  (let [cfg (configured-servers)]
    (doseq [[server {:keys [conn spec]}] @conns]
      (when (or (not= spec (get cfg server)) (not (mcp/alive? conn))) (disconnect! server)))
    (doseq [server (keys cfg)]
      (ensure-connected! server))
    nil))

(defn- reconcile-async!
  "Single-flight background `reconcile!` — safe to call from a turn's ctx-fn
   without blocking it on a slow stdio spawn."
  []
  (when (compare-and-set! reconciling? false true)
    (future (try (reconcile!) (finally (reset! reconciling? false))))))

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
  [_env]
  (ok :mcp/servers
      {"servers" (mapv (fn [[nm spec]]
                         (let [conn (conn-of nm)]
                           (cond->
                             {"name" nm
                              "transport" (name (transport-of spec))
                              "connected" (boolean conn)
                              "enabled" true}
                             conn
                             (assoc "tools"
                               (count (or (some-> (:tools conn)
                                                  deref)
                                          [])))

                             (:command spec)
                             (assoc "command" (:command spec))

                             (:url spec)
                             (assoc "url" (:url spec)))))
                       (configured-servers))}))

(defn- mcp-tools-impl
  [_env server]
  (if-let [conn (ensure-connected! server)]
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
         :hint (str "Enabled servers: " (pr-str (vec (keys (configured-servers))))))))

(defn- mcp-call-impl
  ([env server tool] (mcp-call-impl env server tool {}))
  ([_env server tool args]
   (if-let [conn (ensure-connected! server)]
     (let [r (mcp/call-tool conn tool (if (map? args) args {}))]
       (ok :mcp/call
           {"server" server
            "tool" tool
            "content" (get r "content")
            "is_error" (boolean (get r "isError"))}))
     (err :mcp/call (str "MCP server '"
                         server
                         "' is not configured or is disabled (see ~/.vis/state.yml :mcp :servers).")
          :hint (str "Enabled servers: " (pr-str (vec (keys (configured-servers)))))))))

(defn- mcp-connect-impl
  [_env server]
  (if-let [conn (ensure-connected! server)]
    (ok :mcp/connect
        {"server" server
         "connected" true
         "tools" (count (try (mcp/list-tools conn) (catch Throwable _ [])))})
    (err :mcp/connect (str "Could not connect to MCP server '" server "'."))))

(defn- mcp-disconnect-impl
  [_env server]
  (let [connected (some? (conn-of server))]
    (disconnect! server)
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

(defn- mcp-fence [s] (when (seq (str s)) (str "```\n" s "\n```")))

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
    "List the Model Context Protocol servers declared in ~/.vis/state.yml (:mcp :servers) with their connection status. Returns {\"servers\": [{\"name\": S, \"transport\": \"stdio\"|\"http\", \"connected\": bool, \"enabled\": bool, \"tools\": N (when connected), \"command\"/\"url\": S}]}. Connecting happens lazily on mcp__tools/mcp__call, or explicitly via mcp__connect."
    :arglists '([])}
  mcp-servers
  mcp-servers-impl)

(def
  ^{:doc
    "List a server's tools. mcp__tools(server) connects to the configured server if needed, then returns {\"server\": S, \"tools\": [{\"name\": S, \"description\": S, \"input_schema\": <JSON schema dict>}]}. Use the input_schema to shape the args for mcp__call."
    :arglists '([server])}
  mcp-tools
  mcp-tools-impl)

(def
  ^{:doc
    "Call a tool on an MCP server. mcp__call(server, tool, args) connects if needed and invokes `tool` with `args` (a dict matching that tool's input_schema; omit or {} for no args). Returns {\"server\": S, \"tool\": S, \"content\": [<MCP content blocks>], \"is_error\": bool}. Read text blocks via content[i][\"text\"]."
    :arglists '([server tool] [server tool args])}
  mcp-call
  mcp-call-impl)

(def
  ^{:doc
    "Explicitly connect a configured MCP server into the daemon-wide pool (usually unnecessary because mcp__tools/mcp__call auto-connect, and every /reload reconciles the pool from config). Returns {\"server\": S, \"connected\": bool, \"tools\": N}."
    :arglists '([server])}
  mcp-connect
  mcp-connect-impl)

(def
  ^{:doc
    "Disconnect an MCP server from the daemon-wide pool: closes the connection and, for stdio, terminates the child process. Returns {\"server\": S, \"result\": \"disconnected\"|\"not_connected\"}. On the next /reload (or when config still lists the server) the pool reconciles and may reconnect."
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
      :description
      "List configured MCP servers and connection state before choosing one. In `python_execution`, call `await mcp_servers()`."
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
      :description
      "Discover one MCP server's live tools and input schemas before calling them; auto-connects when needed. In `python_execution`, call `await mcp_tools(...)`."
      :render render-mcp-tools-result
      :call {:pos ["server"]}
      :color-role :tool-color/meta
      :schema {:type "object"
               :properties {"server" {:type "string"
                                      :description "Configured MCP server name (auto-connects)."}}
               :required ["server"]
               :additionalProperties false}
      :tag :observation
      :on-error-fn (mcp-on-error :mcp/tools)})
   (vis/symbol
     #'mcp-call
     {:symbol 'call
      :name "mcp__call"
      :native-tool? true
      :description
      "Invoke a discovered MCP tool with arguments matching its returned input schema; auto-connects when needed. In `python_execution`, call `await mcp_call(...)`."
      :render render-mcp-call-result
      :call {:pos ["server" "tool"] :opt-pos ["args"]}
      :color-role :tool-color/shell
      :schema {:type "object"
               :properties
               {"server" {:type "string" :description "Configured MCP server name (auto-connects)."}
                "tool" {:type "string" :description "Tool name on that server (see mcp__tools)."}
                "args" {:type "object"
                        :description "Args matching the tool's input_schema; omit or {} for none."}}
               :required ["server" "tool"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/call)})
   (vis/symbol
     #'mcp-connect
     {:symbol 'connect
      :name "mcp__connect"
      :native-tool? true
      :description
      "Explicitly connect a configured MCP server into the daemon-wide pool; normally unnecessary because discovery and calls auto-connect, and /reload reconciles the pool. In `python_execution`, call `await mcp_connect(...)`."
      :render render-mcp-connect-result
      :call {:pos ["server"]}
      :color-role :tool-color/create
      :schema {:type "object"
               :properties {"server"
                            {:type "string"
                             :description
                             "Configured MCP server to connect into the daemon-wide pool."}}
               :required ["server"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/connect)})
   (vis/symbol
     #'mcp-disconnect
     {:symbol 'disconnect
      :name "mcp__disconnect"
      :native-tool? true
      :description
      "Disconnect an MCP server from the daemon-wide pool (closes the connection; terminates the child process for stdio). The next /reload will reconcile the pool from config. In `python_execution`, call `await mcp_disconnect(...)`."
      :render render-mcp-disconnect-result
      :call {:pos ["server"]}
      :color-role :tool-color/delete
      :schema {:type "object"
               :properties {"server"
                            {:type "string"
                             :description
                             "Configured MCP server to disconnect from the daemon-wide pool."}}
               :required ["server"]
               :additionalProperties false}
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/disconnect)})])

(defn- contribute
  "`:ext/ctx-fn` — proactively reconcile the daemon-wide MCP pool, then surface
   the CONNECTED servers (+ tool counts) so the model sees what's reachable at
   `session[\"env\"][\"mcp\"][\"servers\"]`. The pool is shared across every
   session."
  [_env]
  (reconcile-async!)
  (let [live @conns]
    (when (seq live)
      {"session_env" {"mcp" {"servers" (mapv (fn [[nm {:keys [conn]}]]
                                               {"name" nm
                                                "transport" (name (:transport conn))
                                                "tools" (count (or (some-> (:tools conn)
                                                                           deref)
                                                                   []))})
                                             live)}}})))

(defn- activation-fn
  "Active when at least one MCP server is configured."
  [_env]
  (boolean (seq (configured-servers))))

(defonce ^:private _mcp-reload-hook (extension/register-reload-hook! ::reconcile reconcile!))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-mcp"
     :ext/description
     "Model Context Protocol (MCP) client: the gateway daemon auto-connects the stdio/HTTP MCP servers declared in config (:mcp :servers) into ONE shared pool, reconciled on every /reload, and exposes their tools (mcp__servers/mcp__tools/mcp__call) to every session. Supports stdio, remote HTTP (Streamable), and OAuth 2.1 (spec 2025-06-18) with automatic discovery + PKCE. Always on; activates when servers are configured."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/engine {:ext.engine/alias 'mcp :ext.engine/symbols mcp-symbols}
     :ext/ctx-fn contribute
     :ext/kind "foundation"}))

(vis/register-extension! vis-extension)
