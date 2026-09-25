(ns com.blockether.vis.internal.foundation.mcp.cli
  "`vis-agent gateway mcp` commands for gateway-owned MCP servers: list, add,
   test, remove, enable, disable, kill, start and OAuth sign-in."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.client :as gateway-client]))

(defn- with-mcp-db!
  [parsed]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db)))

(defn- parse-kv-list
  "Parse a comma-separated K=V,K2=V2 flag value into a string-keyed map, or nil
   when blank."
  [s]
  (when-not (str/blank? (str s))
    (into {}
          (map (fn [pair]
                 (let [[k v] (str/split pair #"=" 2)]
                   [(str/trim k) (str/trim (or v ""))])))
          (str/split s #","))))

(defn- mcp-spec-from-parsed
  "Build a wire-shaped MCP server spec (string-keyed) from parsed CLI flags.
   `url` implies Streamable HTTP; `command` implies stdio -- exactly what
   `mcp.core/transport-of` infers when no explicit `transport` is given."
  [parsed]
  (cond-> {"enabled" (not (boolean (get parsed "disabled")))}
    (get parsed "url")
    (assoc "url" (get parsed "url"))

    (parse-kv-list (get parsed "headers"))
    (assoc "headers" (parse-kv-list (get parsed "headers")))

    (get parsed "command")
    (assoc "command" (get parsed "command"))

    (not (str/blank? (str (get parsed "args"))))
    (assoc "args" (vec (str/split (get parsed "args") #"\s+")))

    (get parsed "cwd")
    (assoc "cwd" (get parsed "cwd"))

    (parse-kv-list (get parsed "env"))
    (assoc "env" (parse-kv-list (get parsed "env")))

    (get parsed "timeout-ms")
    (assoc "timeout_ms" (get parsed "timeout-ms"))))

(defn- require-mcp-name!
  [parsed]
  (let [n (get parsed "name")]
    (when (str/blank? n) (throw (ex-info "A server NAME is required." {:vis/user-error true})))
    n))

(defn- cli-mcp-list!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [rows (gateway-client/mcp-servers)]
    (if (empty? rows)
      (commandline/stdout!
        "No MCP servers configured. Add one: vis-agent gateway mcp add <NAME> --url <URL>")
      (commandline/print-table! [{:key "name" :label "Name" :width 16}
                                 {:key "transport" :label "Transport" :width 15}
                                 {:key "enabled" :label "Enabled" :width 7}
                                 {:key "is_connected" :label "Connected" :width 9}
                                 {:key "is_authorized" :label "Authorized" :width 10}
                                 {:key "tools" :label "Tools" :width 5}]
                                rows))))

(defn- cli-mcp-add!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        spec
        (mcp-spec-from-parsed parsed)

        saved
        (gateway-client/mcp-save-server! name spec)]

    (commandline/stdout! (str "Saved MCP server \"" name "\" (" (get saved "transport") ")."))
    (when (= "streamable_http" (get saved "transport"))
      (if (get saved "is_authorized")
        (commandline/stdout! "  Already authorized.")
        (do (commandline/stdout! "  This server needs OAuth sign-in before it can be used:")
            (commandline/stdout! (str "    vis-agent gateway mcp auth-start " name)))))))

(defn- cli-mcp-test!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        spec
        (mcp-spec-from-parsed parsed)

        result
        (gateway-client/mcp-test-server! name spec)]

    (commandline/stdout! (str "connected=" (boolean (get result "is_connected"))
                              "  tools=" (count (get result "tools"))))))

(defn- cli-mcp-remove!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-delete-server! name)
    (commandline/stdout! (str "Removed MCP server \"" name "\"."))))

(defn- cli-mcp-enable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-set-server-enabled! name true)
    (commandline/stdout! (str "Enabled MCP server \"" name "\"."))))

(defn- cli-mcp-disable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-set-server-enabled! name false)
    (commandline/stdout! (str "Disabled MCP server \"" name "\"."))))

(defn- cli-mcp-kill!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-kill-server! name)
    (commandline/stdout! (str "Killed MCP server \"" name "\" (held down until started again)."))))

(defn- cli-mcp-start!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-start-server! name)
    (commandline/stdout! (str "Started MCP server \"" name "\"."))))

(defn- cli-mcp-auth-start!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        {:strs [flow_id url redirect_uri]}
        (gateway-client/mcp-auth-start! name)]

    (commandline/stdout!
      (str "Step 1. Open this URL in a browser and approve access for \"" name "\":"))
    (commandline/stdout! (str "  " url))
    (commandline/stdout! "")
    (commandline/stdout!
      (str "Step 2. The provider redirects to a loopback URL (" redirect_uri "?code=...)."))
    (commandline/stdout!
      "        Copy that FULL redirect URL (it works even if the page shows an error --")
    (commandline/stdout! "        the code is in the URL bar) and run:")
    (commandline/stdout! (str "  vis-agent gateway mcp auth-complete "
                              name
                              " --flow-id "
                              flow_id
                              " --input \"<PASTED_URL>\""))
    (commandline/stdout! "")
    (commandline/stdout! (str "flow_id=" flow_id))))

(defn- cli-mcp-auth-complete!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")

        input
        (get parsed "input")

        {:strs [status] :as result}
        (gateway-client/mcp-auth-complete! name flow-id input)]

    (commandline/stdout! (str "auth status: " (or status "unknown")))
    (when (= "error" status)
      (commandline/stdout! (str "  " (or (get result "message") "Authorization failed."))))))

(defn- cli-mcp-auth-poll!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")

        {:strs [status] :as result}
        (gateway-client/mcp-auth-poll! name flow-id)]

    (commandline/stdout! (str "auth status: " (or status "unknown")))
    (when (= "error" status)
      (commandline/stdout! (str "  " (or (get result "message") "Authorization failed."))))))

(defn- cli-mcp-auth-cancel!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")]

    (gateway-client/mcp-auth-cancel! name flow-id)
    (commandline/stdout! (str "Cancelled auth flow for \"" name "\"."))))

(defn- cli-mcp-auth-logout!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-auth-logout! name)
    (commandline/stdout! (str "Signed out of MCP server \"" name "\" (tokens forgotten)."))))

(def subcommands
  "Subcommands registered under `vis-agent gateway mcp`."
  [{:cmd/name "list"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "List MCP servers configured on this gateway (transport, enabled, connected, authorized, tool count)."
    :cmd/usage "vis-agent gateway mcp list [--db PATH]"
    :cmd/args
    [{:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be queried."}]
    :cmd/examples ["vis-agent gateway mcp list"]
    :cmd/run-fn cli-mcp-list!}
   {:cmd/name "add"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "Add (or replace) a gateway-managed MCP server. A `--url` makes it Streamable HTTP; a `--command` makes it stdio. OAuth is never configured here -- save the server, then run `auth-start`."
    :cmd/usage
    "vis-agent gateway mcp add <NAME> (--url URL [--headers K=V,...] | --command CMD [--args \"a b\"] [--cwd DIR] [--env K=V,...]) [--timeout-ms MS] [--disabled] [--db PATH]"
    :cmd/args
    [{:name "name"
      :kind :positional
      :type :string
      :required true
      :doc "Server name, e.g. \"linear\"."}
     {:name "url"
      :kind :flag
      :type :string
      :doc "Streamable HTTP endpoint, e.g. https://mcp.linear.app/mcp."}
     {:name "headers"
      :kind :flag
      :type :string
      :doc
      "Static request headers as K=V,K2=V2 (e.g. Authorization=Bearer TOKEN) -- the bearer-token alternative to OAuth. Leave unset for OAuth servers; sign in with auth-start instead."}
     {:name "command" :kind :flag :type :string :doc "Executable for a stdio server."}
     {:name "args" :kind :flag :type :string :doc "Space-separated stdio arguments."}
     {:name "cwd" :kind :flag :type :string :doc "Working directory for a stdio server."}
     {:name "env" :kind :flag :type :string :doc "stdio environment as K=V,K2=V2."}
     {:name "timeout-ms" :kind :flag :type :int :doc "Per-call timeout override in milliseconds."}
     {:name "disabled" :kind :flag :type :boolean :doc "Save the server switched off."}
     {:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be updated."}]
    :cmd/examples
    ["vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp"
     "vis-agent gateway mcp add linear-ro --url https://mcp.linear.app/mcp/readonly"
     "vis-agent gateway mcp add local-fs --command npx --args \"-y @modelcontextprotocol/server-filesystem /tmp\""]
    :cmd/run-fn cli-mcp-add!}
   {:cmd/name "test"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "Connect a candidate server spec without saving it, and report whether it connected and how many tools it exposes."
    :cmd/usage "vis-agent gateway mcp test <NAME> (--url URL | --command CMD ...) [--db PATH]"
    :cmd/args
    [{:name "name" :kind :positional :type :string :required true :doc "Server name to test as."}
     {:name "url" :kind :flag :type :string :doc "Streamable HTTP endpoint."}
     {:name "headers" :kind :flag :type :string :doc "Static request headers as K=V,K2=V2."}
     {:name "command" :kind :flag :type :string :doc "Executable for a stdio server."}
     {:name "args" :kind :flag :type :string :doc "Space-separated stdio arguments."}
     {:name "cwd" :kind :flag :type :string :doc "Working directory for a stdio server."}
     {:name "env" :kind :flag :type :string :doc "stdio environment as K=V,K2=V2."}
     {:name "timeout-ms" :kind :flag :type :int :doc "Per-call timeout override in milliseconds."}
     {:name "db"
      :kind :flag
      :type :string
      :doc "SQLite DB path whose gateway should run the test."}]
    :cmd/examples ["vis-agent gateway mcp test linear --url https://mcp.linear.app/mcp"]
    :cmd/run-fn cli-mcp-test!}
   {:cmd/name "remove"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Delete a gateway-managed MCP server and stop it now."
    :cmd/usage "vis-agent gateway mcp remove <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples ["vis-agent gateway mcp remove linear"]
    :cmd/run-fn cli-mcp-remove!}
   {:cmd/name "enable"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Turn a configured server back on."
    :cmd/usage "vis-agent gateway mcp enable <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-enable!}
   {:cmd/name "disable"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Turn a configured server off without deleting it."
    :cmd/usage "vis-agent gateway mcp disable <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-disable!}
   {:cmd/name "kill"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "Stop a server now and hold it down until started again (runtime only, config unchanged)."
    :cmd/usage "vis-agent gateway mcp kill <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-kill!}
   {:cmd/name "start"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Undo a kill and reconnect a server now."
    :cmd/usage "vis-agent gateway mcp start <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-start!}
   {:cmd/name "auth-start"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "Begin the headless OAuth 2.1 flow (RFC 9728/8414 discovery, dynamic client registration, PKCE) for an HTTP server. Prints the authorize URL to open and the auth-complete command to run after."
    :cmd/usage "vis-agent gateway mcp auth-start <NAME> [--db PATH]"
    :cmd/args [{:name "name"
                :kind :positional
                :type :string
                :required true
                :doc "Server name (must be Streamable HTTP)."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples ["vis-agent gateway mcp auth-start linear"]
    :cmd/run-fn cli-mcp-auth-start!}
   {:cmd/name "auth-complete"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc
    "Finish an OAuth flow with the redirect URL the browser landed on (or its bare `code=` value)."
    :cmd/usage
    "vis-agent gateway mcp auth-complete <NAME> --flow-id ID --input URL_OR_CODE [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "flow-id"
                :kind :flag
                :type :string
                :required true
                :doc "flow_id printed by auth-start."}
               {:name "input"
                :kind :flag
                :type :string
                :required true
                :doc "The pasted redirect URL, or its bare authorization code."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples
    ["vis-agent gateway mcp auth-complete linear --flow-id abc123 --input \"http://127.0.0.1:5555/callback?code=xyz&state=...\""]
    :cmd/run-fn cli-mcp-auth-complete!}
   {:cmd/name "auth-poll"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Read an in-flight OAuth flow's verdict without blocking: pending, ok, or error."
    :cmd/usage "vis-agent gateway mcp auth-poll <NAME> --flow-id ID [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "flow-id"
                :kind :flag
                :type :string
                :required true
                :doc "flow_id printed by auth-start."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-auth-poll!}
   {:cmd/name "auth-cancel"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Abandon an in-flight OAuth flow and release its loopback listener."
    :cmd/usage "vis-agent gateway mcp auth-cancel <NAME> --flow-id ID [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "flow-id" :kind :flag :type :string :required true :doc "flow_id to abandon."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-auth-cancel!}
   {:cmd/name "auth-logout"
    :cmd/parent ["gateway" "mcp"]
    :cmd/doc "Forget the gateway's persisted OAuth tokens for a server."
    :cmd/usage "vis-agent gateway mcp auth-logout <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples ["vis-agent gateway mcp auth-logout linear"]
    :cmd/run-fn cli-mcp-auth-logout!}])
