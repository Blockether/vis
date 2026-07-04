(ns com.blockether.vis.ext.foundation-mcp.core
  "`mcp/` extension — connect to Model Context Protocol servers and expose their
   tools to the agent, with each live connection registered as a session
   RESOURCE (footer count, F4 dialog, `resource_stop`).

   DROPPABLE classpath plug-in. Gated behind the user-owned `:mcp/enabled`
   toggle (OFF by default — connecting spawns processes / opens network calls,
   so every verb short-circuits into a refusal until the user flips it).

   Servers are declared natively in `~/.vis/config.edn`:

     {:mcp {:servers {\"filesystem\" {:transport :stdio :command \"npx\"
                                      :args [\"-y\" \"@modelcontextprotocol/server-filesystem\" \"/path\"]}
                      \"remote\"     {:transport :http :url \"https://...\"
                                      :headers {\"Authorization\" \"Bearer ...\"}}}}}

   Five model-facing verbs under alias `mcp` (flat sandbox renders `alias_name`):
     mcp_servers()                — configured servers + status + tool counts
     mcp_tools(server)            — a server's tools (auto-connects)
     mcp_call(server, tool, args) — call a tool (auto-connects)
     mcp_connect(server) / mcp_disconnect(server) — manage the connection

   Live connections + tool counts also ride in ctx under `env.mcp`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-mcp.client :as mcp]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.resources :as resources]
   [com.blockether.vis.internal.toggles :as toggles]
   [taoensso.telemere :as tel]))

(defn- now-ms [] (System/currentTimeMillis))

;; ---------------------------------------------------------------------------
;; Toggle (extension-owned, OFF by default)
;; ---------------------------------------------------------------------------

(def ^:private disabled-hint
  "MCP is OFF. Enable it in settings (toggle :mcp/enabled) to connect to MCP servers and call their tools.")

(toggles/register-toggle!
  {:id          :mcp/enabled
   :label       "MCP servers"
   :description (str "When ON the agent can connect to the Model Context Protocol"
                  " servers declared in ~/.vis/config.edn (:mcp :servers) and call"
                  " their tools (mcp_servers / mcp_tools / mcp_call). Each live"
                  " connection is a session resource (footer count, F4, resource_stop)."
                  " OFF by default — connecting spawns processes / network calls.")
   :default     false
   :owner       "foundation-mcp"
   :group       :tools})

;; ---------------------------------------------------------------------------
;; Config — declared servers from ~/.vis/config.edn :mcp :servers
;; ---------------------------------------------------------------------------

(defn- transport-of [spec]
  (or (:transport spec) (if (:url spec) :http :stdio)))

(defn- configured-servers
  "Map of `{server-name spec}` from config; keys normalised to strings."
  []
  (let [m (some-> (vis/load-config-raw) :mcp :servers)]
    (if (map? m)
      (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v])) m)
      {})))

(defn- parse-args [s]
  (vec (remove str/blank? (str/split (str s) #"\s+"))))

(defn- add-configured-server!
  [name spec]
  (let [server (some-> name str str/trim not-empty)
        raw    (or (vis/load-config-raw) {})]
    (when-not server
      (throw (ex-info "MCP server name is required" {:type :mcp/config})))
    (vis/save-config!
      (assoc-in raw [:mcp :servers server] spec)
      :mcp)
    server))

(defn- add-stdio-server!
  [{:keys [name command args cwd]}]
  (let [cmd (some-> command str str/trim not-empty)]
    (when-not cmd
      (throw (ex-info "Command is required for a local MCP server" {:type :mcp/config})))
    (add-configured-server! name
      (cond-> {:transport :stdio
               :command cmd
               :args (parse-args args)}
        (some-> cwd str str/trim not-empty) (assoc :cwd (str/trim cwd))))))

(defn- add-http-server!
  [{:keys [name url authorization]}]
  (let [u (some-> url str str/trim not-empty)]
    (when-not u
      (throw (ex-info "URL is required for a remote MCP server" {:type :mcp/config})))
    (add-configured-server! name
      (cond-> {:transport :http
               :url u}
        (some-> authorization str str/trim not-empty)
        (assoc :headers {"Authorization" (str/trim authorization)})))))

;; ---------------------------------------------------------------------------
;; Live connections, partitioned by session: { session-id { server conn } }
;; ---------------------------------------------------------------------------

(defonce ^:private conns (atom {}))

(defn- get-conn [session server] (get-in @conns [session server]))

(defn- resource-id [server] (str "mcp:" server))

(defn- connect-server!
  "Connect (idempotently) to configured `server` for `session`, register it as a
   resource, cache the conn. Returns the conn or nil (unknown / failed)."
  [session server]
  (or (get-conn session server)
    (when-let [spec (get (configured-servers) server)]
      (try
        (let [conn  (mcp/connect server spec)
              tools (try (mcp/list-tools conn) (catch Throwable _ []))]
          (swap! conns assoc-in [session server] conn)
          (resources/register! session
            {:id     (resource-id server)
             :kind   :mcp
             :label  (str "MCP " server)
             :status :up
             :detail (str (name (transport-of spec)) " · " (count tools) " tools")
             :pid    (:pid conn)
             :owner  "foundation-mcp"}
            {:stop-fn  (fn [] (mcp/close conn) (swap! conns update session dissoc server))
             :alive-fn (fn [] (mcp/alive? conn))})
          conn)
        (catch Throwable t
          (tel/log! {:level :warn :id ::connect-failed
                     :data {:server server :error (ex-message t)}}
            "MCP connect failed")
          nil)))))

;; ---------------------------------------------------------------------------
;; Verb implementations (env injected by the gate as the first arg)
;; ---------------------------------------------------------------------------

(defn- ok [op result]
  (extension/success {:op op :result result}))

(defn- err [op message & {:as extra}]
  (extension/failure {:result nil :op op
                      :metadata {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                      :error (merge {:message message} extra)}))

(defn- mcp-servers-impl
  [env]
  (let [session (:session-id env)
        live    (get @conns session)]
    (ok :mcp/servers
      {"servers" (mapv (fn [[nm spec]]
                         (let [conn (get live nm)]
                           (cond-> {"name" nm
                                    "transport" (name (transport-of spec))
                                    "connected" (boolean conn)}
                             conn            (assoc "tools" (count (or (some-> (:tools conn) deref) [])))
                             (:command spec) (assoc "command" (:command spec))
                             (:url spec)     (assoc "url" (:url spec)))))
                   (configured-servers))})))

(defn- mcp-tools-impl
  [env server]
  (if-let [conn (connect-server! (:session-id env) server)]
    (ok :mcp/tools
      {"server" server
       "tools"  (mapv (fn [t] {"name"         (get t "name")
                               "description"  (get t "description")
                               "input_schema" (get t "inputSchema")})
                  (mcp/list-tools conn))})
    (err :mcp/tools (str "MCP server '" server "' is not configured (see ~/.vis/config.edn :mcp :servers).")
      :hint (str "Configured servers: " (pr-str (vec (keys (configured-servers))))))))

(defn- mcp-call-impl
  ([env server tool] (mcp-call-impl env server tool {}))
  ([env server tool args]
   (if-let [conn (connect-server! (:session-id env) server)]
     ;; `args` arrives from Python string-keyed (verbatim, matching the tool's
     ;; input_schema); the MCP JSON client wants string keys, so pass straight
     ;; through — no keyword→string coercion.
     (let [r (mcp/call-tool conn tool (if (map? args) args {}))]
       (ok :mcp/call
         {"server"   server
          "tool"     tool
          "content"  (get r "content")
          "is_error" (boolean (get r "isError"))}))
     (err :mcp/call (str "MCP server '" server "' is not configured (see ~/.vis/config.edn :mcp :servers).")
       :hint (str "Configured servers: " (pr-str (vec (keys (configured-servers)))))))))

(defn- mcp-connect-impl
  [env server]
  (if-let [conn (connect-server! (:session-id env) server)]
    (ok :mcp/connect {"server" server "connected" true
                      "tools" (count (try (mcp/list-tools conn) (catch Throwable _ [])))})
    (err :mcp/connect (str "Could not connect to MCP server '" server "'."))))

(defn- mcp-disconnect-impl
  [env server]
  (let [res (resources/stop! (:session-id env) (resource-id server))]
    (ok :mcp/disconnect {"server" server "result" (name (or (:result res) :unknown))})))

;; ---------------------------------------------------------------------------
;; Gate + error envelopes (mirror the shell extension)
;; ---------------------------------------------------------------------------

(defn- mcp-gate-before-fn
  [op]
  (fn [env f args]
    (if (toggles/enabled? :mcp/enabled)
      {:env env :fn f :args (into [env] args)}
      {:result (err op (str (name op) " blocked: " disabled-hint)
                 :type ::disabled :reason :mcp-disabled
                 :hint disabled-hint :loop-hint disabled-hint)})))

(defn- mcp-on-error
  [op]
  (fn [err* _env _f _args]
    {:result (extension/failure
               {:result nil :op op
                :metadata {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                :throwable err*})}))

;; ---------------------------------------------------------------------------
;; Native op-card renderers — read the tool's string-keyed `:result` and return
;; the internal `{:summary :body}` IR (keyword-keyed by contract). Keys arrive
;; as VERBATIM STRINGS (the boundary is strings-only); the injected env is gone.
;; ---------------------------------------------------------------------------

(defn- mcp-fence [s]
  (when (seq (str s)) (str "```\n" s "\n```")))

(defn- render-mcp-servers-result
  [r]
  (let [servers (get r "servers")]
    {:summary (str (count servers) " MCP server" (when (not= 1 (count servers)) "s"))
     :body    (when (seq servers)
                (str/join "\n"
                  (map (fn [s]
                         (str "- `" (get s "name") "` " (get s "transport")
                           (if (get s "connected")
                             (str " ✓" (when (get s "tools") (str " (" (get s "tools") " tools)")))
                             " ·")))
                    servers)))}))

(defn- render-mcp-tools-result
  [r]
  (let [tools (get r "tools")]
    {:summary (str "`" (get r "server") "` — " (count tools) " tool" (when (not= 1 (count tools)) "s"))
     :body    (when (seq tools)
                (str/join "\n"
                  (map (fn [t]
                         (str "- `" (get t "name") "`"
                           (when (seq (str (get t "description")))
                             (str " — " (get t "description")))))
                    tools)))}))

(defn- render-mcp-call-result
  [r]
  (let [blocks (get r "content")
        text   (->> blocks
                 (keep (fn [b] (get b "text")))
                 (str/join "\n"))]
    {:summary (str "`" (get r "server") "`/" (get r "tool") (when (get r "is_error") " — error"))
     :body    (mcp-fence (if (seq text) text (pr-str blocks)))}))

(defn- render-mcp-connect-result
  [r]
  {:summary (str "connected `" (get r "server") "`"
              (when (get r "tools") (str " (" (get r "tools") " tools)")))})

(defn- render-mcp-disconnect-result
  [r]
  {:summary (str "disconnected `" (get r "server") "` — " (get r "result"))})

;; ---------------------------------------------------------------------------
;; Public, doc-bearing vars (model-facing surface). Under alias `mcp` they bind
;; as mcp_servers / mcp_tools / mcp_call / mcp_connect / mcp_disconnect.
;; ---------------------------------------------------------------------------

(def ^{:doc "List the Model Context Protocol servers declared in ~/.vis/config.edn (:mcp :servers) with their connection status. Returns {\"servers\": [{\"name\": S, \"transport\": \"stdio\"|\"http\", \"connected\": bool, \"tools\": N (when connected), \"command\"/\"url\": S}]}. Connecting happens lazily on mcp_tools/mcp_call, or explicitly via mcp_connect."
       :arglists '([])}
  mcp-servers mcp-servers-impl)

(def ^{:doc "List a server's tools. mcp_tools(server) connects to the configured server if needed, then returns {\"server\": S, \"tools\": [{\"name\": S, \"description\": S, \"input_schema\": <JSON schema dict>}]}. Use the input_schema to shape the args for mcp_call."
       :arglists '([server])}
  mcp-tools mcp-tools-impl)

(def ^{:doc "Call a tool on an MCP server. mcp_call(server, tool, args) connects if needed and invokes `tool` with `args` (a dict matching that tool's input_schema; omit or {} for no args). Returns {\"server\": S, \"tool\": S, \"content\": [<MCP content blocks>], \"is_error\": bool}. Read text blocks via content[i][\"text\"]."
       :arglists '([server tool] [server tool args])}
  mcp-call mcp-call-impl)

(def ^{:doc "Explicitly connect to a configured MCP server and register it as a session resource (footer count, F4, resource_stop). Returns {\"server\": S, \"connected\": bool, \"tools\": N}. Usually unnecessary — mcp_tools/mcp_call auto-connect."
       :arglists '([server])}
  mcp-connect mcp-connect-impl)

(def ^{:doc "Disconnect an MCP server: stop its session resource (closes the connection / kills the stdio process) via the canonical resource_stop path. Returns {\"server\": S, \"result\": \"stopped\"|\"unknown\"|...}."
       :arglists '([server])}
  mcp-disconnect mcp-disconnect-impl)

;; ---------------------------------------------------------------------------
;; Symbols + ctx + prompt + extension
;; ---------------------------------------------------------------------------

(def ^:private mcp-symbols
  [(vis/symbol #'mcp-servers
     {:symbol 'servers :name "mcp_servers"
      :native-tool? true :render render-mcp-servers-result
                ;; mcp verbs bind positionally under the wire name: mcp_servers(),
                ;; mcp_tools(server), mcp_call(server, tool, args?), mcp_(dis)connect(server).
      :call {:pos []}
      :color-role :tool-color/meta
      :schema {:type "object" :properties {} :required []}
      :before-fn (mcp-gate-before-fn :mcp/servers)
      :tag :observation :on-error-fn (mcp-on-error :mcp/servers)})
   (vis/symbol #'mcp-tools
     {:symbol 'tools :name "mcp_tools"
      :native-tool? true :render render-mcp-tools-result
      :call {:pos ["server"]}
      :color-role :tool-color/meta
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Configured MCP server name (auto-connects)."}}
               :required ["server"]}
      :before-fn (mcp-gate-before-fn :mcp/tools)
      :tag :observation :on-error-fn (mcp-on-error :mcp/tools)})
   (vis/symbol #'mcp-call
     {:symbol 'call :name "mcp_call"
      :native-tool? true :render render-mcp-call-result
      :call {:pos ["server" "tool"] :opt-pos ["args"]}
      :color-role :tool-color/shell
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Configured MCP server name (auto-connects)."}
                            "tool"   {:type "string" :description "Tool name on that server (see mcp_tools)."}
                            "args"   {:type "object" :description "Args matching the tool's input_schema; omit or {} for none."}}
               :required ["server" "tool"]}
      :before-fn (mcp-gate-before-fn :mcp/call)
      :tag :mutation :on-error-fn (mcp-on-error :mcp/call)})
   (vis/symbol #'mcp-connect
     {:symbol 'connect :name "mcp_connect"
      :native-tool? true :render render-mcp-connect-result
      :call {:pos ["server"]}
      :color-role :tool-color/create
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Configured MCP server to connect + register as a resource."}}
               :required ["server"]}
      :before-fn (mcp-gate-before-fn :mcp/connect)
      :tag :mutation :on-error-fn (mcp-on-error :mcp/connect)})
   (vis/symbol #'mcp-disconnect
     {:symbol 'disconnect :name "mcp_disconnect"
      :native-tool? true :render render-mcp-disconnect-result
      :call {:pos ["server"]}
      :color-role :tool-color/delete
      :schema {:type "object"
               :properties {"server" {:type "string" :description "Connected MCP server to disconnect (stops its resource)."}}
               :required ["server"]}
      :before-fn (mcp-gate-before-fn :mcp/disconnect)
      :tag :mutation :on-error-fn (mcp-on-error :mcp/disconnect)})])

(defn- contribute
  "`:ext/ctx-fn` — surface this session's CONNECTED MCP servers (+ tool counts) so
   the model sees what's reachable at `ctx[\"env\"][\"mcp\"][\"servers\"]`."
  [env]
  (let [session (:session-id env)
        live    (get @conns session)]
    (when (seq live)
      ;; `"session_env"` is the merge directive read STRING-KEYED by ctx_loop /
      ;; env-digest; the whole contribution (key + value) is string-keyed with no
      ;; keyword values, so it survives the merge and crosses the boundary.
      {"session_env"
       {"mcp" {"servers" (mapv (fn [[nm conn]]
                                 {"name" nm
                                  "transport" (name (:transport conn))
                                  "tools" (count (or (some-> (:tools conn) deref) []))})
                           live)}}})))

(def ^:private prompt-text
  (str "MCP servers available. Connect to Model Context Protocol servers (declared in\n"
    "~/.vis/config.edn under :mcp :servers) and call their tools. Verbs (alias mcp):\n"
    "  mcp_servers()                  — configured servers + connection status + tool counts\n"
    "  mcp_tools(server)              — a server's tools (name/description/input_schema); auto-connects\n"
    "  mcp_call(server, tool, args)   — call a tool; args is a dict matching its input_schema; auto-connects\n"
    "  mcp_connect(server) / mcp_disconnect(server) — manage the connection explicitly\n"
    "Each live connection is a session RESOURCE (footer count, F4 dialog, resource_stop(\"mcp:<server>\")).\n"
    "Connected servers + tool counts also ride in ctx under ctx[\"env\"][\"mcp\"][\"servers\"].\n"
    "Workflow: mcp_servers() to see what's there → mcp_tools(server) to learn a tool's input_schema →\n"
    "mcp_call(server, tool, {...}) to invoke it. Read text results via content[i][\"text\"]."))

(defn- activation-fn
  "Active when at least one MCP server is configured."
  [_env]
  (boolean (seq (configured-servers))))

(defn- mcp-enabled?
  "True when the user-owned `:mcp/enabled` toggle is ON. The MCP surface in any
   Resources UI (web modal + TUI dialog) is gated on this — nothing MCP shows
   until the user opts in."
  []
  (toggles/enabled? :mcp/enabled))

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-mcp"
     :ext/description    "Model Context Protocol (MCP) client: connect to stdio/HTTP MCP servers declared in config (:mcp :servers), call their tools (mcp_servers/mcp_tools/mcp_call), each live connection a session resource. Gated by :mcp/enabled (OFF by default). Activates when servers are configured."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/engine         {:ext.engine/alias 'mcp
                          :ext.engine/symbols mcp-symbols}
     :ext/prompt-fn         (fn [_env] prompt-text)
     :ext/ctx-fn            contribute
     :ext/startable-resources
     [{:kind          :mcp-configured
       :label         "configured MCP client"
       :options-label "client"
       ;; Only when MCP is ON *and* there's at least one configured server to
       ;; connect to — an empty "connect to configured client" row is noise.
       :visible-fn    (fn [] (and (mcp-enabled?) (boolean (seq (configured-servers)))))
       :options-fn    (fn [_env] (vec (keys (configured-servers))))
       :start-fn      (fn [env selected]
                        (let [session (:session-id env)
                              names   (if (sequential? selected) selected [selected])]
                          (doseq [s names :when s] (connect-server! session (str s)))))}
      {:kind     :mcp-stdio
       :label    "local command MCP client"
       :visible-fn mcp-enabled?
       :fields   [{:name :name :label "Client name" :placeholder "filesystem" :required true}
                  {:name :command :label "Command" :placeholder "npx" :required true}
                  {:name :args :label "Arguments" :placeholder "-y @modelcontextprotocol/server-filesystem /path"}
                  {:name :cwd :label "Working directory" :placeholder "optional"}]
       :start-fn (fn [env fields]
                   (let [server (add-stdio-server! fields)]
                     (connect-server! (:session-id env) server)))}
      {:kind     :mcp-http
       :label    "remote HTTP MCP client"
       :visible-fn mcp-enabled?
       :fields   [{:name :name :label "Client name" :placeholder "remote" :required true}
                  {:name :url :label "URL" :placeholder "https://example.com/mcp" :required true}
                  {:name :authorization :label "Authorization header" :placeholder "Bearer …"}]
       :start-fn (fn [env fields]
                   (let [server (add-http-server! fields)]
                     (connect-server! (:session-id env) server)))}]
     :ext/kind           "common"}))

(vis/register-extension! vis-extension)
