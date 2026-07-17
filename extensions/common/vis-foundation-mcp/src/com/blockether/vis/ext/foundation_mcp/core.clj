(ns com.blockether.vis.ext.foundation-mcp.core
  "`mcp/` extension — connect to Model Context Protocol servers and expose their
   tools to the agent, with each live connection registered as a session
   RESOURCE (footer count, F4 dialog, `resource_stop`).

   DROPPABLE classpath plug-in. Gated behind the user-owned `:mcp/enabled`
   toggle (ON by default — connects automatically when MCP servers are
   configured; can still be turned off to short-circuit every verb).

   Servers are declared natively in `~/.vis/config.edn`:

     {:mcp {:servers {\"filesystem\" {:transport :stdio :command \"npx\"
                                      :args [\"-y\" \"@modelcontextprotocol/server-filesystem\" \"/path\"]}
                      \"remote\"     {:transport :http :url \"https://...\"
                                      :headers {\"Authorization\" \"Bearer ...\"}}}}}

   Five model-facing verbs under alias `mcp` (flat sandbox renders `alias_name`):
     mcp__servers()                — configured servers + status + tool counts
     mcp__tools(server)            — a server's tools (auto-connects)
     mcp__call(server, tool, args) — call a tool (auto-connects)
     mcp__connect(server) / mcp__disconnect(server) — manage the connection

   Live connections + tool counts also ride in ctx under `env.mcp`."
  (:require [clojure.string :as str]
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
  {:id :mcp/enabled
   :label "MCP servers"
   :description (str "When ON the agent can connect to the Model Context Protocol"
                     " servers declared in ~/.vis/config.edn (:mcp :servers) and call"
                     " their tools (mcp__servers / mcp__tools / mcp__call). Each live"
                     " connection is a session resource (footer count, F4, resource_stop)."
                     " ON by default — connects automatically when MCP servers are configured.")
   :default true
   :owner "foundation-mcp"
   :group :tools})

;; ---------------------------------------------------------------------------
;; Config — declared servers from ~/.vis/config.edn :mcp :servers
;; ---------------------------------------------------------------------------

(defn- transport-of [spec] (or (:transport spec) (if (:url spec) :http :stdio)))

(defn- configured-servers
  "Map of `{server-name spec}` from config; keys normalised to strings."
  []
  (let [m (some-> (vis/load-config-raw)
                  :mcp
                  :servers)]
    (if (map? m)
      (into {}
            (map (fn [[k v]]
                   [(if (keyword? k) (name k) (str k)) v]))
            m)
      {})))

(defn- parse-args [s] (vec (remove str/blank? (str/split (str s) #"\s+"))))

(defn- parse-env
  "Parse a textarea of `KEY=VALUE` lines into a `{\"KEY\" \"VALUE\"}` map.
   Blank lines and lines without `=` are ignored; keys/values are trimmed."
  [s]
  (into {}
        (keep (fn [line]
                (let [line (str/trim line)]
                  (when-let [i (and (seq line) (str/index-of line "="))]
                    (let [k (str/trim (subs line 0 i))]
                      (when (seq k) [k (str/trim (subs line (inc (long i))))]))))))
        (str/split-lines (str s))))

(defn- add-configured-server!
  [name spec]
  (let [server
        (some-> name
                str
                str/trim
                not-empty)

        raw
        (or (vis/load-config-raw) {})]

    (when-not server (throw (ex-info "MCP server name is required" {:type :mcp/config})))
    (vis/save-config! (assoc-in raw [:mcp :servers server] spec) :mcp)
    server))

(defn- add-stdio-server!
  [{:strs [name command args cwd env]}]
  (let [cmd (some-> command
                    str
                    str/trim
                    not-empty)]
    (when-not cmd
      (throw (ex-info "Command is required for a local MCP server" {:type :mcp/config})))
    (add-configured-server! name
                            (cond-> {:transport :stdio :command cmd :args (parse-args args)}
                              (some-> cwd
                                      str
                                      str/trim
                                      not-empty)
                              (assoc :cwd (str/trim cwd))

                              (seq (parse-env env))
                              (assoc :env (parse-env env))))))

(defn- add-http-server!
  [{:strs [name url authorization token]}]
  (let [u
        (some-> url
                str
                str/trim
                not-empty)

        ;; A bare `token` is the friendly path — sent as `Bearer <token>`.
        ;; A full `authorization` header value (any scheme) still wins if given.
        auth
        (or (some-> authorization
                    str
                    str/trim
                    not-empty)
            (some-> token
                    str
                    str/trim
                    not-empty
                    (->> (str "Bearer "))))]

    (when-not u (throw (ex-info "URL is required for a remote MCP server" {:type :mcp/config})))
    (add-configured-server! name
                            (cond-> {:transport :http :url u}
                              auth
                              (assoc :headers {"Authorization" auth})))))

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
        (try (let [conn (mcp/connect server spec)
                   tools (try (mcp/list-tools conn) (catch Throwable _ []))]

               (swap! conns assoc-in [session server] conn)
               (resources/register! session
                                    {:id (resource-id server)
                                     :kind :mcp
                                     :label (str "MCP " server)
                                     :status :up
                                     :detail
                                     (str (name (transport-of spec)) " · " (count tools) " tools")
                                     :pid (:pid conn)
                                     :owner "foundation-mcp"}
                                    {:stop-fn (fn []
                                                (mcp/close conn)
                                                (swap! conns update session dissoc server))
                                     :alive-fn (fn []
                                                 (mcp/alive? conn))})
               conn)
             (catch Throwable t
               (tel/log!
                 {:level :warn :id ::connect-failed :data {:server server :error (ex-message t)}}
                 "MCP connect failed")
               nil)))))

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
  (let [session
        (:session-id env)

        live
        (get @conns session)]

    (ok :mcp/servers
        {"servers" (mapv (fn [[nm spec]]
                           (let [conn (get live nm)]
                             (cond-> {"name" nm
                                      "transport" (name (transport-of spec))
                                      "connected" (boolean conn)}
                               conn
                               (assoc "tools"
                                 (count (or (some-> (:tools conn)
                                                    deref)
                                            [])))

                               (:command spec)
                               (assoc "command" (:command spec))

                               (:url spec)
                               (assoc "url" (:url spec)))))
                         (configured-servers))})))

(defn- mcp-tools-impl
  [env server]
  (if-let [conn (connect-server! (:session-id env) server)]
    (ok :mcp/tools
        {"server" server
         "tools" (mapv (fn [t]
                         {"name" (get t "name")
                          "description" (get t "description")
                          "input_schema" (get t "inputSchema")})
                       (mcp/list-tools conn))})
    (err :mcp/tools
         (str "MCP server '" server "' is not configured (see ~/.vis/config.edn :mcp :servers).")
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
           {"server" server
            "tool" tool
            "content" (get r "content")
            "is_error" (boolean (get r "isError"))}))
     (err :mcp/call
          (str "MCP server '" server "' is not configured (see ~/.vis/config.edn :mcp :servers).")
          :hint (str "Configured servers: " (pr-str (vec (keys (configured-servers)))))))))

(defn- mcp-connect-impl
  [env server]
  (if-let [conn (connect-server! (:session-id env) server)]
    (ok :mcp/connect
        {"server" server
         "connected" true
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
      {:result (err op
                    (str (name op) " blocked: " disabled-hint)
                    :type ::disabled
                    :reason :mcp-disabled
                    :hint disabled-hint
                    :loop-hint disabled-hint)})))

(defn- mcp-on-error
  [op]
  (fn [err* _env _f _args]
    {:result (extension/failure {:result nil
                                 :op op
                                 :metadata
                                 {:started-at-ms (now-ms) :finished-at-ms (now-ms) :duration-ms 0}
                                 :throwable err*})}))

;; ---------------------------------------------------------------------------
;; Native op-card renderers — read the tool's string-keyed `:result` and return
;; the internal `{:summary :body}` IR (keyword-keyed by contract). Keys arrive
;; as VERBATIM STRINGS (the boundary is strings-only); the injected env is gone.
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
  (let [blocks
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
;; Public, doc-bearing vars (model-facing surface). Under alias `mcp` they bind
;; as mcp__servers / mcp__tools / mcp__call / mcp__connect / mcp__disconnect.
;; ---------------------------------------------------------------------------

(def
  ^{:doc
    "List the Model Context Protocol servers declared in ~/.vis/config.edn (:mcp :servers) with their connection status. Returns {\"servers\": [{\"name\": S, \"transport\": \"stdio\"|\"http\", \"connected\": bool, \"tools\": N (when connected), \"command\"/\"url\": S}]}. Connecting happens lazily on mcp__tools/mcp__call, or explicitly via mcp__connect."
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
    "Explicitly connect to a configured MCP server and register it as a session resource (footer count, F4, resource_stop). Returns {\"server\": S, \"connected\": bool, \"tools\": N}. Usually unnecessary — mcp__tools/mcp__call auto-connect."
    :arglists '([server])}
  mcp-connect
  mcp-connect-impl)

(def
  ^{:doc
    "Disconnect an MCP server: stop its session resource (closes the connection / kills the stdio process) via the canonical resource_stop path. Returns {\"server\": S, \"result\": \"stopped\"|\"unknown\"|...}."
    :arglists '([server])}
  mcp-disconnect
  mcp-disconnect-impl)

;; ---------------------------------------------------------------------------
;; Symbols + ctx + prompt + extension
;; ---------------------------------------------------------------------------

;; Tool NAMES use a `mcp__` (DOUBLE-underscore) prefix, never `mcp_`. Anthropic's
;; Claude-subscription OAuth endpoint reserves the single-underscore `mcp_<x>`
;; namespace for its own managed MCP-connector tools; a CLIENT tool named
;; `mcp_<x>` makes the whole request classify as a third-party MCP integration
;; and 400 with "third-party apps now draw from your extra usage" — every turn
;; with MCP enabled died on OAuth. Verified by wire probe: `mcp_call` → 400,
;; `mcp__call` → 200. Double underscore keeps the family grouping, stays a valid
;; Python identifier (verbs bind positionally under the wire name), and dodges
;; the reservation. Do NOT revert to a single underscore.
(def ^:private mcp-symbols
  [(vis/symbol #'mcp-servers
               {:symbol 'servers
                :name "mcp__servers"
                :native-tool? true
                :render render-mcp-servers-result
                ;; mcp verbs bind positionally under the wire name: mcp__servers(),
                ;; mcp__tools(server), mcp__call(server, tool, args?), mcp_(dis)connect(server).
                :call {:pos []}
                :color-role :tool-color/meta
                :schema {:type "object" :properties {} :required []}
                :before-fn (mcp-gate-before-fn :mcp/servers)
                :tag :observation
                :on-error-fn (mcp-on-error :mcp/servers)})
   (vis/symbol #'mcp-tools
               {:symbol 'tools
                :name "mcp__tools"
                :native-tool? true
                :render render-mcp-tools-result
                :call {:pos ["server"]}
                :color-role :tool-color/meta
                :schema {:type "object"
                         :properties {"server" {:type "string"
                                                :description
                                                "Configured MCP server name (auto-connects)."}}
                         :required ["server"]}
                :before-fn (mcp-gate-before-fn :mcp/tools)
                :tag :observation
                :on-error-fn (mcp-on-error :mcp/tools)})
   (vis/symbol
     #'mcp-call
     {:symbol 'call
      :name "mcp__call"
      :native-tool? true
      :render render-mcp-call-result
      :call {:pos ["server" "tool"] :opt-pos ["args"]}
      :color-role :tool-color/shell
      :schema {:type "object"
               :properties
               {"server" {:type "string" :description "Configured MCP server name (auto-connects)."}
                "tool" {:type "string" :description "Tool name on that server (see mcp__tools)."}
                "args" {:type "object"
                        :description "Args matching the tool's input_schema; omit or {} for none."}}
               :required ["server" "tool"]}
      :before-fn (mcp-gate-before-fn :mcp/call)
      :tag :mutation
      :on-error-fn (mcp-on-error :mcp/call)})
   (vis/symbol #'mcp-connect
               {:symbol 'connect
                :name "mcp__connect"
                :native-tool? true
                :render render-mcp-connect-result
                :call {:pos ["server"]}
                :color-role :tool-color/create
                :schema {:type "object"
                         :properties
                         {"server" {:type "string"
                                    :description
                                    "Configured MCP server to connect + register as a resource."}}
                         :required ["server"]}
                :before-fn (mcp-gate-before-fn :mcp/connect)
                :tag :mutation
                :on-error-fn (mcp-on-error :mcp/connect)})
   (vis/symbol #'mcp-disconnect
               {:symbol 'disconnect
                :name "mcp__disconnect"
                :native-tool? true
                :render render-mcp-disconnect-result
                :call {:pos ["server"]}
                :color-role :tool-color/delete
                :schema {:type "object"
                         :properties {"server"
                                      {:type "string"
                                       :description
                                       "Connected MCP server to disconnect (stops its resource)."}}
                         :required ["server"]}
                :before-fn (mcp-gate-before-fn :mcp/disconnect)
                :tag :mutation
                :on-error-fn (mcp-on-error :mcp/disconnect)})])

(defn- contribute
  "`:ext/ctx-fn` — surface this session's CONNECTED MCP servers (+ tool counts) so
   the model sees what's reachable at `ctx[\"env\"][\"mcp\"][\"servers\"]`."
  [env]
  (let [session
        (:session-id env)

        live
        (get @conns session)]

    (when (seq live)
      ;; `"session_env"` is the merge directive read STRING-KEYED by ctx_loop /
      ;; env-digest; the whole contribution (key + value) is string-keyed with no
      ;; keyword values, so it survives the merge and crosses the boundary.
      {"session_env" {"mcp" {"servers" (mapv (fn [[nm conn]]
                                               {"name" nm
                                                "transport" (name (:transport conn))
                                                "tools" (count (or (some-> (:tools conn)
                                                                           deref)
                                                                   []))})
                                             live)}}})))

(def ^:private prompt-text
  (str
    "MCP servers available. Connect to Model Context Protocol servers (declared in\n"
    "~/.vis/config.edn under :mcp :servers) and call their tools. Verbs (alias mcp):\n"
    "  mcp__servers()                  — configured servers + connection status + tool counts\n"
    "  mcp__tools(server)              — a server's tools (name/description/input_schema); auto-connects\n"
    "  mcp__call(server, tool, args)   — call a tool; args is a dict matching its input_schema; auto-connects\n"
    "  mcp__connect(server) / mcp__disconnect(server) — manage the connection explicitly\n"
    "Each live connection is a session RESOURCE (footer count, F4 dialog, resource_stop(\"mcp:<server>\")).\n"
    "Connected servers + tool counts also ride in ctx under ctx[\"env\"][\"mcp\"][\"servers\"].\n"
    "Workflow: mcp__servers() to see what's there → mcp__tools(server) to learn a tool's input_schema →\n"
    "mcp__call(server, tool, {...}) to invoke it. Read text results via content[i][\"text\"].\n"
    "Results come back to you IN FULL — filter / shape them in python_execution (content is a list: loop, slice, json.loads a text block), never paste a raw tool dump back."))

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
    {:ext/name "foundation-mcp"
     :ext/description
     "Model Context Protocol (MCP) client: connect to stdio/HTTP MCP servers declared in config (:mcp :servers), call their tools (mcp__servers/mcp__tools/mcp__call), each live connection a session resource. Gated by :mcp/enabled (ON by default). Activates when servers are configured."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/engine {:ext.engine/alias 'mcp :ext.engine/symbols mcp-symbols}
     :ext/prompt-fn (fn [_env]
                      prompt-text)
     :ext/ctx-fn contribute
     :ext/startable-resources
     [{:kind :mcp-configured
       :label "Connect configured MCP"
       :options-label "client"
       ;; Only when MCP is ON *and* there's at least one configured server to
       ;; connect to — an empty "connect to configured client" row is noise.
       :visible-fn (fn []
                     (and (mcp-enabled?) (boolean (seq (configured-servers)))))
       :options-fn (fn [_env]
                     (vec (keys (configured-servers))))
       :start-fn (fn [env selected]
                   (let [session
                         (:session-id env)

                         names
                         (if (sequential? selected) selected [selected])]

                     (doseq [s
                             names

                             :when s]

                       (connect-server! session (str s)))))}
      {:kind :mcp-stdio
       :label "MCP server"
       ;; Grouped with :mcp-http under one "MCP server" row; the web shows a
       ;; Local/Remote transport chooser that swaps between the two variants.
       :group "MCP server"
       :variant {:id "local" :label "Local" :hint "Runs a command on this machine (stdio)"}
       :visible-fn mcp-enabled?
       :fields [{:name :name :label "Name" :placeholder "filesystem" :required true}
                {:name :command :label "Command" :placeholder "npx" :required true}
                {:name :args
                 :label "Arguments"
                 :placeholder "-y @modelcontextprotocol/server-filesystem /path"}
                {:name :env
                 :label "Environment"
                 :type :textarea
                 :placeholder "KEY=VALUE (one per line)"
                 :hint "secrets like GITHUB_TOKEN=… — stored in config.edn"}
                {:name :cwd :label "Directory" :placeholder "optional"}]
       :start-fn (fn [env fields]
                   (let [server (add-stdio-server! fields)]
                     (connect-server! (:session-id env) server)))}
      {:kind :mcp-http
       :label "MCP server"
       :group "MCP server"
       :variant {:id "remote" :label "Remote" :hint "Connects to a URL over HTTP"}
       :visible-fn mcp-enabled?
       :fields [{:name :name :label "Name" :placeholder "linear" :required true}
                {:name :url :label "URL" :placeholder "https://mcp.linear.app/sse" :required true}
                {:name :token
                 :label "Auth token"
                 :type :password
                 :placeholder "paste token"
                 :hint "stored in config.edn, sent as Bearer"}]
       :start-fn (fn [env fields]
                   (let [server (add-http-server! fields)]
                     (connect-server! (:session-id env) server)))}]
     :ext/kind "common"}))

(vis/register-extension! vis-extension)
