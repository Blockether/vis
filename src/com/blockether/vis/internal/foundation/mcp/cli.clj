(ns com.blockether.vis.internal.foundation.mcp.cli
  "`vis-agent mcp` commands. `add`, `add-json`, `add-from-claude-desktop`, `get`,
   `list` and `remove` take the arguments of `claude mcp` and use its scopes:
   `local` writes the private `.vis/config.yml` of the current directory, `project`
   writes the shared `.mcp.json` and `user` writes the gateway's machine state.
   `group` and `session` with `--target-id` write a scoped definition through the
   gateway. `test`, `enable`, `disable`, `kill`, `start` and the OAuth commands
   manage gateway servers."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.mcp-approval :as mcp-approval]
            [com.blockether.vis.internal.config.mcp-json :as mcp-json]
            [com.blockether.vis.internal.config.validation :as config-validation]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp-core]
            [com.blockether.vis.internal.foundation.mcp.oauth :as oauth]
            [com.blockether.vis.internal.foundation.mcp.serve :as serve]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.io Console]))

(defn- with-mcp-db!
  [parsed]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db)))

;; Claude Code ends a refused `claude mcp` command with exit code 1.
(defn- refuse! [message] (throw (ex-info message {:vis/user-error true :vis/exit-code 1})))

(defn- same-name?
  "True when the engine keys `a` and `b` as one server: `Figma_Dev` is `figma-dev`."
  [a b]
  (= (mcp-core/canonical-name a) (mcp-core/canonical-name b)))

(defn- by-canonical-name
  "`servers` keyed by `mcp-core/canonical-name`, like the engine reads them."
  [servers]
  (into {}
        (keep (fn [[k v]]
                (when-let [nm (mcp-core/canonical-name k)]
                  [nm v])))
        servers))

(defn- without-name
  "`servers` without each spelling of `name`."
  [servers name]
  (into {} (remove #(same-name? name (key %))) servers))

;;; ── Claude Code arguments ─────────────────────────────────────────────────────

(def ^:private option-keys
  "Each option spelling of `claude mcp`, then the Vis options, to its key."
  {"-s" "scope"
   "--scope" "scope"
   "-t" "transport"
   "--transport" "transport"
   "-e" "env"
   "--env" "env"
   "-H" "header"
   "--header" "header"
   "--client-id" "client-id"
   "--client-secret" "client-secret"
   "--callback-port" "callback-port"
   "--target-id" "target-id"
   "-d" "debug"
   "--debug" "debug"
   "--verbose" "verbose"
   "--db" "db"})

(def ^:private boolean-options "Options that take no value." #{"client-secret" "debug" "verbose"})

(def ^:private repeated-options
  "Options that take each next word of their shape too, as `-e A=1 B=2` does."
  {"env" #"^[A-Za-z_][A-Za-z0-9_]*=" "header" #"^[\w-]+:(?!//)"})

(defn parse-words
  "Parse `claude mcp` words. Options of `allowed` come in any order before `--`, also
   as `--name=value`. `-e` and `-H` repeat. Each word after `--` is a positional.
   Returns `{:options {key value} :positionals [word ...]}`, with vectors for
   `env` and `header`."
  [allowed words]
  (loop [words
         (seq words)

         options
         {}

         positionals
         []]

    (if-not words
      {:options options :positionals positionals}
      (let [[word & more] words]
        (cond
          (= "--" word) {:options options :positionals (into positionals more)}
          (and (str/starts-with? word "-") (not= "-" word))
          (let [[spelling inline]
                (if (str/starts-with? word "--") (str/split word #"=" 2) [word nil])
                k (get option-keys spelling)]

            (when-not (contains? allowed k)
              (refuse!
                (str "Unknown option '" spelling
                     "'. Put the server command and its"
                     " arguments after --, for example: vis-agent mcp add NAME -- npx -y PACKAGE")))
            (if (boolean-options k)
              (recur more (assoc options k true) positionals)
              (let [[value more] (if inline [inline more] [(first more) (next more)])]
                (when (nil? value) (refuse! (str "Option '" spelling "' needs a value.")))
                (if-let [shape (get repeated-options k)]
                  (let [[extra more] (split-with #(re-find shape %) more)]
                    (recur (seq more)
                           (update options k (fnil into []) (cons value extra))
                           positionals))
                  (recur more (assoc options k value) positionals)))))
          :else (recur more options (conj positionals word)))))))

(defn- env-map
  [values]
  (into
    {}
    (map (fn [value]
           (let [[k v] (str/split value #"=" 2)]
             (when (or (str/blank? k) (nil? v))
               (refuse!
                 (str "Invalid environment variable format: "
                      value
                      ", environment variables should be added as: -e KEY1=value1 -e KEY2=value2")))
             [k v])))
    values))

(defn- header-map
  [values]
  (into {}
        (map (fn [value]
               (let [[k v] (str/split value #":" 2)]
                 (when (or (str/blank? k) (nil? v))
                   (refuse! (str "Invalid header format: \""
                                 value
                                 "\". Expected format: \"Header-Name: value\"")))
                 [(str/trim k) (str/trim v)])))
        values))

(defn- check-name!
  [name]
  (when (str/blank? name) (refuse! "Missing required argument 'name'."))
  (when-not (re-matches #"[A-Za-z0-9_-]+" name)
    (refuse! (str "Invalid name "
                  name
                  ". Names can only contain letters, numbers, hyphens, and underscores.")))
  name)

(defn- check-auth-options!
  [options]
  (when-let [port (get options "callback-port")]
    (when-not (and (re-matches #"\d{1,5}" port) (<= 1 (parse-long port) 65535))
      (refuse! (str "Invalid callback port: " port ". Must be a number from 1 to 65535.")))))

(defn- client-secret!
  "The secret of `--client-secret`: `MCP_CLIENT_SECRET`, else asked without echo, as
   `claude mcp add` does. It is for the OAuth client `auth.client_id` of `spec`."
  [options spec]
  (when (get options "client-secret")
    (when-not (get-in spec ["auth" "client_id"]) (refuse! "--client-secret requires --client-id."))
    ;; The OAuth flow runs in the gateway and reads the secret from its own machine.
    (when (gateway-client/remote-gateway)
      (refuse! "--client-secret needs the gateway on this machine, not a remote gateway."))
    (or (not-empty (System/getenv "MCP_CLIENT_SECRET"))
        (if-let [^Console console (System/console)]
          (or (not-empty (some->
                           (.readPassword console "Enter OAuth client secret: " (object-array 0))
                           (String.)))
              (refuse! "The client secret cannot be empty."))
          (refuse! "No terminal can ask for the client secret. Set MCP_CLIENT_SECRET.")))))

(defn- save-client-secret!
  "Keep `secret` for the OAuth client of `name` in the token store of this machine."
  [name secret]
  (when secret (oauth/save-client-secret! (mcp-core/canonical-name name) secret)))

(defn add-spec
  "Vis server spec of the parsed words of `vis-agent mcp add`, which takes the
   arguments of `claude mcp add`: NAME, then a command and its arguments, or a URL
   with `--transport http` or `sse`."
  [{:keys [options positionals]}]
  (let [[name target & args]
        positionals

        transport
        (or (get options "transport") "stdio")

        env
        (env-map (get options "env"))

        headers
        (header-map (get options "header"))]

    (check-name! name)
    (when (str/blank? target) (refuse! "Missing required argument 'commandOrUrl'."))
    (when-not (#{"stdio" "sse" "http"} transport)
      (refuse! (str "Invalid transport type: " transport ". Must be one of: stdio, sse, http")))
    (check-auth-options! options)
    (if (= "stdio" transport)
      (do
        (when (seq headers) (refuse! "Headers apply only to --transport http or sse."))
        (when (get options "client-id")
          (refuse! "--client-id applies only to --transport http or sse."))
        (when (get options "callback-port")
          (refuse! "--callback-port applies only to --transport http or sse."))
        (when (and (nil? (get options "transport")) (re-find #"^https?://" target))
          (commandline/stderr!
            (str
              "Warning: The command \""
              target
              "\" looks like a URL, but is being interpreted as a stdio server as --transport was not specified.\n"
              "If this is an HTTP server use: vis-agent mcp add --transport http "
              name
              " "
              target
              "\n"
              "If this is an SSE server use: vis-agent mcp add --transport sse " name
              " " target)))
        (cond-> {"transport" "stdio" "command" target}
          (seq args)
          (assoc "args" (vec args))

          (seq env)
          (assoc "env" env)))
      (do (when (seq args)
            (refuse! (str "Unexpected arguments after the URL: " (str/join " " args))))
          (when (seq env) (refuse! "--env applies only to stdio servers."))
          (cond-> {"transport" (if (= "sse" transport) "sse" "streamable_http") "url" target}
            (seq headers)
            (assoc "headers" headers)

            (get options "client-id")
            (assoc-in ["auth" "client_id"] (get options "client-id"))

            (get options "callback-port")
            (assoc-in ["auth" "callback_port"] (parse-long (get options "callback-port"))))))))

;;; ── Scopes ────────────────────────────────────────────────────────────────────

(def ^:private scope-labels
  {"local" "Local config (private to you in this project)"
   "project" "Project config (shared via .mcp.json)"
   "user" "User config (available in all your projects)"})

(def ^:private file-scopes ["local" "project" "user"])

(defn- scope-of
  "The scope of the parsed `options`, `default` when there is none."
  [options default]
  (let [scope (or (get options "scope") default)]
    (when-not (#{"local" "project" "user" "group" "session"} scope)
      (refuse!
        (str "Invalid scope: " scope ". Must be one of: local, user, project, group, session")))
    (when (and (#{"group" "session"} scope) (str/blank? (get options "target-id")))
      (refuse! (str "Scope " scope " needs --target-id with the " scope " id.")))
    (when (and (get options "target-id") (not (#{"group" "session"} scope)))
      (refuse! "--target-id applies only to --scope group or session."))
    scope))

(defn- gateway-target [options scope] {:scope scope :target-id (get options "target-id")})

(defn- root [] (workspace/cwd-root))

(defn- local-file
  []
  (let [dir (io/file (root) ".vis")]
    (str (io/file dir
                  (or (first (filter #(.exists (io/file dir %)) ["config.yml" "config.yaml"]))
                      "config.yml")))))

(defn- project-file
  []
  (binding [workspace/*workspace-root* (root)]
    (mcp-json/path)))

(defn- servers-in
  "Definitions of `scope` in the files of this machine, by name. `user` is the
   hand-written `~/.vis/config.yml` under the gateway's `state.yml`, `project` is
   `.mcp.json` under `vis.yml` and `local` is `.vis/config.yml`."
  [scope]
  (binding [workspace/*workspace-root* (root)]
    (by-canonical-name (case scope
                         "user"
                         (merge (get-in (config/load-global-yaml-config-raw) ["mcp" "servers"])
                                (get-in (config/load-global-config-raw) ["mcp" "servers"]))

                         ;; Also the `.mcp.json` servers that wait for approval.
                         "project"
                         (merge (mcp-json/servers)
                                (get-in (config/load-project-root-config-raw) ["mcp" "servers"]))

                         "local"
                         (get-in (config/load-project-config-raw) ["mcp" "servers"])))))

(defn- in-mcp-json?
  [name]
  (some #(same-name? name %) (keys (get (mcp-json/read-file (project-file)) "mcpServers"))))

(defn- without-server
  [raw name]
  (let [raw (update-in raw ["mcp" "servers"] without-name name)]
    (cond-> raw
      (empty? (get-in raw ["mcp" "servers"]))
      (update "mcp" dissoc "servers")

      (empty? (get (update raw "mcp" dissoc "servers") "mcp"))
      (dissoc "mcp"))))

(defn- write-server!
  "Save or, with a nil `spec`, delete `name` in `scope`. Returns the changed file,
   or nil for a gateway scope."
  [options scope name spec]
  (when spec
    (when-not (config-validation/valid? {"mcp" {"servers" {name spec}}})
      (refuse! (str "Invalid MCP server configuration for " name "."))))
  (case scope
    "local"
    (binding [workspace/*workspace-root* (root)]
      (config/update-project-config!
        #(if spec (assoc-in % ["mcp" "servers" name] spec) (without-server % name)))
      (local-file))

    "project"
    (let [file
          (project-file)

          raw
          (mcp-json/read-file file)]

      (mcp-json/write-file! file
                            (if spec
                              (assoc-in raw ["mcpServers" name] (mcp-json/->entry spec))
                              (update raw "mcpServers" without-name name)))
      file)

    "user"
    (do
      (if spec (gateway-client/mcp-save-server! name spec) (gateway-client/mcp-delete-server! name))
      (config/state-path))

    (do (if spec
          (gateway-client/mcp-save-server! name spec (gateway-target options scope))
          (gateway-client/mcp-delete-server! name (gateway-target options scope)))
        nil)))

(defn- own-names
  "Names that `scope` itself defines: for `project` only `.mcp.json`."
  [options scope]
  (case scope
    "project"
    (set (keys (by-canonical-name (get (mcp-json/read-file (project-file)) "mcpServers"))))

    ("local" "user")
    (set (keys (servers-in scope)))

    (into #{}
          (keep #(when (= scope (get % "source")) (mcp-core/canonical-name (get % "name"))))
          (gateway-client/mcp-servers (gateway-target options scope)))))

(defn- save-new!
  "Add `name` to `scope` like `claude mcp add`: refuse a name that the scope has.
   Returns the changed file or nil."
  [options scope name spec]
  (when (contains? (own-names options scope) (mcp-core/canonical-name name))
    (refuse! (str "MCP server " name " already exists in " scope " config")))
  (write-server! options scope name spec))

(defn- choose!
  "Keep the approval `choice` (`mcp-approval/choose`) of the `.mcp.json` server
   `name` in the private `.vis/config.yml`."
  [name choice]
  (binding [workspace/*workspace-root* (root)]
    (config/update-project-config! #(mcp-approval/choose % name choice))))

(defn- forget!
  "Drop each approval choice of a `.mcp.json` server that has the engine name of
   `name`. A choice for `My_Srv` must not stay after `remove my-srv`."
  [name]
  (binding [workspace/*workspace-root* (root)]
    (config/update-project-config! (fn [raw]
                                     (reduce #(mcp-approval/choose %1 %2 :forget)
                                             raw
                                             (filter #(same-name? name %)
                                                     (mapcat #(get-in raw ["mcp" %])
                                                             ["enabled_mcpjson_servers"
                                                              "disabled_mcpjson_servers"])))))))

(defn- approve-own!
  "A person who adds a server to `.mcp.json` approves it for this person."
  [scope name]
  (when (= "project" scope) (choose! name :approve)))

(defn- approval
  "`pending` or `rejected` when `name` of `scope` is a `.mcp.json` server that Vis
   does not start, else nil."
  [scope name]
  (when (= "project" scope)
    (binding [workspace/*workspace-root* (root)]
      (let [started (by-canonical-name (get-in (config/load-project-root-config-raw)
                                               ["mcp" "servers"]))]
        (when-not (contains? started (mcp-core/canonical-name name))
          (when-let [raw (first (filter #(same-name? name %) (keys (mcp-json/servers))))]
            (mcp-approval/status (config/mcp-approval-choices) raw)))))))

(defn- print-file! [file] (when file (commandline/stdout! (str "File modified: " file))))

(defn- transport-label
  [spec]
  (case (get spec "transport")
    "stdio"
    "stdio"

    "sse"
    "SSE"

    "HTTP"))

;;; ── Commands of claude mcp ────────────────────────────────────────────────────

(def ^:private add-options
  #{"scope" "transport" "env" "header" "client-id" "client-secret" "callback-port" "target-id"
    "db"})

(defn- cli-mcp-add!
  [_ words]
  (let [{:keys [options positionals] :as parsed}
        (parse-words add-options words)

        _
        (with-mcp-db! options)

        scope
        (scope-of options "local")

        spec
        (add-spec parsed)

        name
        (first positionals)

        secret
        (client-secret! options spec)

        file
        (save-new! options scope name spec)]

    (save-client-secret! name secret)
    (approve-own! scope name)
    (commandline/stdout!
      (if (= "stdio" (get spec "transport"))
        (str "Added stdio MCP server "
             name
             " with command: "
             (str/join " " (cons (get spec "command") (get spec "args")))
             " to "
             scope
             " config")
        (str "Added "
             (transport-label spec)
             " MCP server "
             name
             " with URL: "
             (get spec "url")
             " to "
             scope
             " config")))
    (when-let [headers (get spec "headers")]
      (commandline/stdout!
        (str "Headers: " (json/write-json-str headers :indent-str "  " :escape-slash false))))
    (print-file! file)))

(defn- cli-mcp-add-json!
  [_ words]
  (let [{:keys [options positionals]}
        (parse-words #{"scope" "client-secret" "target-id" "db"} words)

        _
        (with-mcp-db! options)

        [name text & extra]
        positionals

        scope
        (scope-of options "local")]

    (check-name! name)
    (check-auth-options! options)
    (when (str/blank? text) (refuse! "Missing required argument 'json'."))
    (when (seq extra) (refuse! (str "Unexpected arguments: " (str/join " " extra))))
    (let [entry
          (try (json/read-json text)
               (catch Exception e (refuse! (str "Invalid JSON: " (ex-message e)))))

          spec
          (or (mcp-json/->spec entry)
              (refuse! "Invalid MCP server configuration: give type stdio, http or sse."))

          secret
          (client-secret! options spec)

          file
          (save-new! options scope name spec)]

      (save-client-secret! name secret)
      (approve-own! scope name)
      (commandline/stdout!
        (str "Added "
             (get {"streamable_http" "http"} (get spec "transport") (get spec "transport"))
             " MCP server "
             name
             " to "
             scope
             " config"))
      (print-file! file))))

(defn- claude-desktop-file
  "Claude Desktop's configuration file on this operating system."
  []
  (let [home
        (System/getProperty "user.home")

        os
        (str/lower-case (System/getProperty "os.name"))]

    (cond (str/includes? os "mac")
          (io/file home "Library" "Application Support" "Claude" "claude_desktop_config.json")
          (str/includes? os "win") (io/file (or (System/getenv "APPDATA")
                                                (str home "\\AppData\\Roaming"))
                                            "Claude"
                                            "claude_desktop_config.json")
          :else (io/file home ".config" "Claude" "claude_desktop_config.json"))))

(defn- choose-servers
  "The names that the person selects in a terminal; every name without a terminal."
  [names]
  (if-let [^Console console (System/console)]
    (do (commandline/stdout! (str "Found " (count names) " MCP servers in Claude Desktop:"))
        (doseq [[i name] (map-indexed vector names)]
          (commandline/stdout! (str "  " (inc i) ". " name)))
        (let [answer (str/trim (str (.readLine
                                      console
                                      "Servers to import (numbers such as 1,3; Enter imports all): "
                                      (object-array 0))))]
          (if (str/blank? answer)
            names
            (vec (distinct (for [n (str/split answer #"[,\s]+")
                                 :let [i (some-> (parse-long n)
                                                 dec)]]

                             (if (and i (< -1 i (count names)))
                               (nth names i)
                               (refuse! (str "No server has the number " n ".")))))))))
    names))

(defn- free-name
  "`name`, or Claude Code's `name_1`, `name_2` when `taken` has it."
  [taken name]
  (first (remove #(contains? taken (mcp-core/canonical-name %))
           (cons name (map #(str name "_" %) (iterate inc 1))))))

(defn- cli-mcp-add-from-claude-desktop!
  [_ words]
  (let [{:keys [options positionals]}
        (parse-words #{"scope" "target-id" "db"} words)

        _
        (with-mcp-db! options)

        _
        (when (seq positionals) (refuse! (str "Unexpected arguments: " (str/join " " positionals))))

        scope
        (scope-of options "local")

        entries
        (get (mcp-json/read-file (claude-desktop-file)) "mcpServers")]

    (if-not (and (map? entries) (seq entries))
      (commandline/stdout!
        "No MCP servers found in Claude Desktop configuration or configuration file does not exist.")
      (let [chosen
            (choose-servers (vec (sort (map str (keys entries)))))

            [_ imported]
            (reduce (fn [[taken n] name]
                      (if-let [spec (mcp-json/->spec (get entries name))]
                        (let [name (free-name taken name)]
                          (write-server! options scope name spec)
                          [(conj taken (mcp-core/canonical-name name)) (inc n)])
                        (do (commandline/stderr!
                              (str "Skipped " name ": Vis cannot use its configuration."))
                            [taken n])))
                    [(own-names options scope) 0]
                    chosen)]

        (commandline/stdout!
          (str "Successfully imported " imported " MCP server(s) to " scope " config."))))))

(defn- effective-servers
  "Every server of the file scopes, the most specific definition of each name,
   as `[name spec scope]` in Claude Code's order: user, project, local."
  []
  (let [by-scope
        (into {} (map (juxt identity servers-in)) file-scopes)

        order
        (distinct (mapcat #(keys (get by-scope %)) ["user" "project" "local"]))]

    (for [name
          order

          :let [scope
                (first (filter #(contains? (get by-scope %) name) ["local" "project" "user"]))]]

      [name (get-in by-scope [scope name]) scope])))

(defn- health
  "Claude Code's status mark of `name` of `scope` after the gateway tries to connect
   it. A `.mcp.json` server without approval does not connect: a stdio server runs
   a command of the repository."
  [name spec scope]
  (let [approval (when-not (false? (get spec "enabled")) (approval scope name))]
    (cond (false? (get spec "enabled")) "✗ Disabled"
          (= "pending" approval) "! Needs approval"
          (= "rejected" approval) "✗ Rejected"
          :else (try (if (get (gateway-client/mcp-test-server! name spec) "is_connected")
                       "✓ Connected"
                       "! Needs authentication")
                     ;; Only the gateway's answer about this server is a failed connect. An
                     ;; unreachable gateway is not, and must not mark every server failed.
                     (catch clojure.lang.ExceptionInfo e
                       (let [status (:http-status (ex-data e))]
                         (if (and status (<= 400 (long status) 499) (not= 401 (long status)))
                           "✗ Failed to connect"
                           (throw e))))))))

(defn- target-text
  [spec]
  (if (= "stdio" (get spec "transport"))
    (str/join " " (cons (get spec "command") (get spec "args")))
    (str (get spec "url") " (" (transport-label spec) ")")))

(defn- cli-mcp-list!
  [_ words]
  (let [{:keys [options positionals]} (parse-words #{"scope" "target-id" "db"} words)]
    (with-mcp-db! options)
    (when (seq positionals) (refuse! (str "Unexpected arguments: " (str/join " " positionals))))
    (if (#{"group" "session"} (get options "scope"))
      (let [scope (scope-of options nil)]
        (doseq [row (gateway-client/mcp-servers (gateway-target options scope))]
          (commandline/stdout! (str (get row "name")
                                    ": "
                                    (target-text row)
                                    " - "
                                    (if (get row "is_connected") "✓ Connected" "✗ Not connected")
                                    " ("
                                    (get row "source")
                                    ")"))))
      (let [servers (effective-servers)]
        (if (empty? servers)
          (commandline/stdout!
            "No MCP servers configured. Use `vis-agent mcp add` to add a server.")
          (do (commandline/stdout! "Checking MCP server health...\n")
              ;; Check the servers at the same time, and print them in order. A
              ;; failed future gives its own error, not a wrapped one.
              (doseq [line (mapv (fn [[name spec scope]]
                                   (future (str name
                                                ": " (target-text spec)
                                                " - " (health name spec scope))))
                                 servers)]
                (commandline/stdout! (try @line
                                          (catch java.util.concurrent.ExecutionException e
                                            (throw (.getCause e))))))))))))

(defn- cli-mcp-get!
  [_ words]
  (let [{:keys [options positionals]}
        (parse-words #{"db"} words)

        _
        (with-mcp-db! options)

        [name & extra]
        positionals

        _
        (when (str/blank? name) (refuse! "Missing required argument 'name'."))

        _
        (when (seq extra) (refuse! (str "Unexpected arguments: " (str/join " " extra))))

        [_ spec scope]
        (or (first (filter #(same-name? name (first %)) (effective-servers)))
            (refuse! (str "No MCP server found with name: " name)))

        status
        (health name spec scope)]

    (commandline/stdout! (str name ":"))
    (commandline/stdout! (str "  Scope: " (scope-labels scope)))
    (commandline/stdout! (str "  Status: " status))
    (commandline/stdout! (str "  Type: " (str/lower-case (transport-label spec))))
    (if (= "stdio" (get spec "transport"))
      (do (commandline/stdout! (str "  Command: " (get spec "command")))
          (commandline/stdout! (str "  Args: " (str/join " " (get spec "args"))))
          (when-let [env (not-empty (get spec "env"))]
            (commandline/stdout! "  Environment:")
            (doseq [[k v] env]
              (commandline/stdout! (str "    " k "=" v)))))
      (do (commandline/stdout! (str "  URL: " (get spec "url")))
          (when-let [headers (not-empty (get spec "headers"))]
            (commandline/stdout! "  Headers:")
            (doseq [[k v] headers]
              (commandline/stdout! (str "    " k ": " v))))))
    (when (and (= "! Needs authentication" status) (= "user" scope))
      (commandline/stdout! (str "\nTo sign in, run: vis-agent mcp auth-start " name)))
    (commandline/stdout! (str "\nTo remove this server, run: vis-agent mcp remove \"" name
                              "\" -s " scope))))

(defn- remove-from!
  [options scope name]
  (cond (and (= "project" scope)
             (not (in-mcp-json? name))
             (contains? (servers-in "project") (mcp-core/canonical-name name)))
        (refuse! (str "MCP server " name " is declared in vis.yml; edit it there."))
        (not (contains? (if (#{"group" "session"} scope)
                          (own-names options scope)
                          (set (keys (servers-in scope))))
                        (mcp-core/canonical-name name)))
        (refuse! (str "No MCP server found with name: \"" name "\" in " scope " config")))
  (let [file (write-server! options scope name nil)]
    (when (= "project" scope) (forget! name))
    (when-not (some #(contains? (servers-in %) (mcp-core/canonical-name name)) file-scopes)
      (oauth/delete-client-secret! (mcp-core/canonical-name name)))
    (commandline/stdout! (str "Removed MCP server " name " from " scope " config"))
    (print-file! file)))

(defn- cli-mcp-remove!
  [_ words]
  (let [{:keys [options positionals]}
        (parse-words #{"scope" "target-id" "db"} words)

        _
        (with-mcp-db! options)

        [name & extra]
        positionals]

    (when (str/blank? name) (refuse! "Missing required argument 'name'."))
    (when (seq extra) (refuse! (str "Unexpected arguments: " (str/join " " extra))))
    (if (get options "scope")
      (remove-from! options (scope-of options nil) name)
      (let [found (filter #(contains? (servers-in %) (mcp-core/canonical-name name)) file-scopes)]
        (case (count found)
          0
          (refuse! (str "No MCP server found with name: \"" name "\""))

          1
          (remove-from! options (first found) name)

          (refuse! (str "MCP server \"" name
                        "\" exists in multiple scopes:\n"
                        (str/join (map #(str "  - " (scope-labels %) "\n") found))
                        "\nTo remove from a specific scope, use:\n"
                        (str/join "\n"
                                  (map #(str "  vis-agent mcp remove \"" name "\" -s " %)
                                       found)))))))))

;;; ── Approval of .mcp.json servers ──────────────────────────────────────────

(defn- pending-project-servers
  "The `.mcp.json` names that wait for the approval of the person, sorted."
  []
  (let [started
        (by-canonical-name (get-in (config/load-project-root-config-raw) ["mcp" "servers"]))

        choices
        (config/mcp-approval-choices)]

    (vec (sort (filter #(and (= "pending" (mcp-approval/status choices %))
                             (not (contains? started (mcp-core/canonical-name %))))
                       (map str (keys (mcp-json/servers))))))))

(defn- ask-line
  [^Console console prompt]
  (str/trim (str (.readLine console prompt (object-array 0)))))

(defn- ask-one!
  "Claude Code's question about one new `.mcp.json` server."
  [^Console console name]
  (commandline/stdout! (str "New MCP server found in .mcp.json: " name "\n"))
  (commandline/stdout! "MCP servers may execute code or access system resources.\n")
  (commandline/stdout! "  1. Use this and all future MCP servers in this project")
  (commandline/stdout! "  2. Use this MCP server")
  (commandline/stdout! "  3. Continue without using this MCP server")
  (loop []

    (if-let [choice (get {"" :approve-all "1" :approve-all "2" :approve "3" :reject}
                         (ask-line console "Choice (Enter = 1): "))]
      (choose! name choice)
      (recur))))

(defn- ask-many!
  "Claude Code's question about several new `.mcp.json` servers: the person selects
   the servers to use, and Vis rejects the others."
  [^Console console names]
  (commandline/stdout! (str (count names) " new MCP servers found in .mcp.json\n"))
  (commandline/stdout! "MCP servers may execute code or access system resources.\n")
  (doseq [[i name] (map-indexed vector names)]
    (commandline/stdout! (str "  " (inc i) ". " name)))
  (let [chosen (loop []

                 (let [answer (ask-line
                                console
                                "Servers to use (numbers such as 1,3; Enter = all; n = none): ")
                       picks (cond (str/blank? answer) names
                                   (#{"n" "N"} answer) []
                                   :else (for [n (str/split answer #"[,\s]+")
                                               :let [i (some-> (parse-long n)
                                                               dec)]]

                                           (when (and i (< -1 i (count names))) (nth names i))))]

                   (if (every? some? picks) (set picks) (recur))))]
    (doseq [name names]
      (choose! name (if (chosen name) :approve :reject)))))

(defn approve-project-servers!
  "Ask about each new `.mcp.json` server of the current directory, as Claude Code
   does when it starts. The terminal client calls it before it takes the screen.
   Without a terminal, the servers stay pending, and Vis does not start them."
  []
  (when-let [console (System/console)]
    (binding [workspace/*workspace-root* (root)]
      (when (workspace/source-enabled? "project")
        (let [names (pending-project-servers)]
          (case (count names)
            0
            nil

            1
            (ask-one! console (first names))

            (ask-many! console names)))))))

(defn- cli-mcp-reset-project-choices!
  [_ words]
  (let [{:keys [options positionals]} (parse-words #{"db"} words)]
    (with-mcp-db! options)
    (when (seq positionals) (refuse! (str "Unexpected arguments: " (str/join " " positionals))))
    (binding [workspace/*workspace-root* (root)]
      (config/update-project-config! mcp-approval/reset))
    (commandline/stdout!
      "All project-scoped (.mcp.json) server approvals and rejections have been reset.")
    (commandline/stdout! "You will be prompted for approval next time you start Vis.")))

(defn- cli-mcp-serve!
  [_ words]
  (let [{:keys [options positionals]}
        (parse-words #{"debug" "verbose" "db"} words)

        ;; The launcher takes `--debug`, `--verbose` and `-v` for itself and adds the
        ;; console log handler; only `-d` arrives here. Read the handler before
        ;; `config/init-cli!` removes it.
        debug?
        (boolean (or (get options "debug")
                     (get options "verbose")
                     (contains? (tel/get-handlers) :default/console)))]

    (with-mcp-db! options)
    (when (seq positionals) (refuse! (str "Unexpected arguments: " (str/join " " positionals))))
    ;; `config/init-cli!` points System/out and System/err at the log file.
    (let [exit (serve/serve! {:in System/in
                              :out config/original-stdout
                              :err config/original-stderr
                              :cwd (root)
                              :tools (transcript/model-facing-tools)
                              :debug? debug?})]
      (shutdown-agents)
      (System/exit (int exit)))))

;;; ── Vis gateway commands ──────────────────────────────────────────────────────

(defn- cli-mcp-test!
  [_ words]
  (let [{:keys [options positionals] :as parsed}
        (parse-words #{"transport" "env" "header" "client-id" "db"} words)

        _
        (with-mcp-db! options)

        result
        (gateway-client/mcp-test-server! (first positionals) (add-spec parsed))]

    (commandline/stdout! (str "connected=" (boolean (get result "is_connected"))
                              "  tools=" (count (get result "tools"))))))

(defn- require-mcp-name!
  [parsed]
  (let [n (get parsed "name")]
    (when (str/blank? n) (throw (ex-info "A server NAME is required." {:vis/user-error true})))
    n))

(defn- settings-target
  [parsed]
  (let [scope
        (get parsed "scope")

        id
        (get parsed "target-id")]

    (when (and id (nil? scope))
      (throw (ex-info "--target-id requires --scope" {:vis/user-error true})))
    (when scope {:scope scope :target-id id})))

(defn- cli-mcp-enable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-set-server-enabled! name true (settings-target parsed))
    (commandline/stdout! (str "Enabled MCP server \"" name "\"."))))

(defn- cli-mcp-disable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    (gateway-client/mcp-set-server-enabled! name false (settings-target parsed))
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
    (commandline/stdout!
      (str "  vis-agent mcp auth-complete " name " --flow-id " flow_id " --input \"<PASTED_URL>\""))
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

(def ^:private scope-arg
  {:name "scope"
   :kind :flag
   :type :string
   :doc
   "-s. local (default): private .vis/config.yml of this directory; project: shared .mcp.json; user: all your projects. Also group or session with --target-id."})

(def ^:private target-arg
  {:name "target-id"
   :kind :flag
   :type :string
   :doc "Group id or session id for --scope group or session."})

(def ^:private db-arg
  {:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be used."})

(def command
  {:cmd/name "mcp"
   :cmd/doc
   "Configure MCP servers with the commands and arguments of `claude mcp`, and manage gateway servers."
   :cmd/usage
   "vis-agent mcp <add|add-json|add-from-claude-desktop|get|list|remove|test|enable|disable|kill|start|auth-start|auth-complete|auth-poll|auth-cancel|auth-logout>"
   :cmd/usage-exit-code 1
   :cmd/subcommands #(registry/registered-under ["mcp"])})

(def ^:private subcommand-specs
  "The commands under `vis-agent mcp`."
  [{:cmd/name "add"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Add an MCP server, with the arguments of `claude mcp add`. Options come before the server command; put the command and its arguments after --."
    :cmd/usage "vis-agent mcp add [options] <name> <commandOrUrl> [args...]"
    :cmd/raw-args? true
    :cmd/args
    [{:name "name" :kind :positional :type :string :doc "Server name: letters, numbers, - and _."}
     {:name "commandOrUrl"
      :kind :positional
      :type :string
      :doc "The command of a stdio server, or the URL of an http or sse server."}
     {:name "args" :kind :positional :type :string :doc "Arguments of the command, after --."}
     scope-arg
     {:name "transport" :kind :flag :type :string :doc "-t. stdio (default), http or sse."}
     {:name "env"
      :kind :flag
      :type :string
      :doc "-e KEY=value. Environment of a stdio server; repeat it."}
     {:name "header"
      :kind :flag
      :type :string
      :doc "-H \"Name: value\". Request header of an http or sse server; repeat it."}
     {:name "client-id" :kind :flag :type :string :doc "OAuth client id of an http or sse server."}
     {:name "client-secret"
      :kind :flag
      :type :boolean
      :doc
      "Ask for the OAuth client secret, or read MCP_CLIENT_SECRET. Vis keeps it in its token store."}
     {:name "callback-port"
      :kind :flag
      :type :string
      :doc "Fixed port of the OAuth callback, http://localhost:PORT/callback."} target-arg db-arg]
    :cmd/examples
    ["vis-agent mcp add --transport http notion https://mcp.notion.com/mcp"
     "vis-agent mcp add --transport http secure-api https://api.example.com/mcp --header \"Authorization: Bearer your-token\""
     "vis-agent mcp add --transport sse asana https://mcp.asana.com/sse"
     "vis-agent mcp add --transport stdio --env AIRTABLE_API_KEY=YOUR_KEY airtable -- npx -y airtable-mcp-server"
     "vis-agent mcp add --scope project --transport http paypal https://mcp.paypal.com/mcp"]
    :cmd/run-fn cli-mcp-add!}
   {:cmd/name "add-json"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Add an MCP server from a JSON string in the format of .mcp.json, like `claude mcp add-json`."
    :cmd/usage "vis-agent mcp add-json [options] <name> <json>"
    :cmd/raw-args? true
    :cmd/args
    [{:name "name" :kind :positional :type :string :doc "Server name."}
     {:name "json" :kind :positional :type :string :doc "The server entry as JSON."} scope-arg
     {:name "client-secret"
      :kind :flag
      :type :boolean
      :doc "Ask for the OAuth client secret of oauth.clientId, or read MCP_CLIENT_SECRET."}
     target-arg db-arg]
    :cmd/examples
    ["vis-agent mcp add-json weather-api '{\"type\":\"http\",\"url\":\"https://api.weather.com/mcp\",\"headers\":{\"Authorization\":\"Bearer token\"}}'"
     "vis-agent mcp add-json local-weather '{\"type\":\"stdio\",\"command\":\"/path/to/weather-cli\",\"args\":[\"--api-key\",\"abc123\"],\"env\":{\"CACHE_DIR\":\"/tmp\"}}'"]
    :cmd/run-fn cli-mcp-add-json!}
   {:cmd/name "add-from-claude-desktop"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Import the MCP servers of Claude Desktop. In a terminal, you select the servers; a name that the scope has gets the suffix _1."
    :cmd/usage "vis-agent mcp add-from-claude-desktop [--scope SCOPE]"
    :cmd/raw-args? true
    :cmd/args [scope-arg target-arg db-arg]
    :cmd/examples ["vis-agent mcp add-from-claude-desktop"
                   "vis-agent mcp add-from-claude-desktop -s user"]
    :cmd/run-fn cli-mcp-add-from-claude-desktop!}
   {:cmd/name "get"
    :cmd/parent ["mcp"]
    :cmd/doc "Show the scope, status and configuration of one MCP server."
    :cmd/usage "vis-agent mcp get <name>"
    :cmd/raw-args? true
    :cmd/args [{:name "name" :kind :positional :type :string :doc "Server name."} db-arg]
    :cmd/examples ["vis-agent mcp get notion"]
    :cmd/run-fn cli-mcp-get!}
   {:cmd/name "list"
    :cmd/parent ["mcp"]
    :cmd/doc
    "List the MCP servers of the user, project and local scopes, and check that each one connects."
    :cmd/usage "vis-agent mcp list"
    :cmd/raw-args? true
    :cmd/args
    [{:name "scope" :kind :flag :type :string :doc "-s group or session, with --target-id."}
     target-arg db-arg]
    :cmd/examples ["vis-agent mcp list" "vis-agent mcp list -s session --target-id <session-id>"]
    :cmd/run-fn cli-mcp-list!}
   {:cmd/name "remove"
    :cmd/parent ["mcp"]
    :cmd/doc "Remove an MCP server. Without --scope, the server must be in one scope only."
    :cmd/usage "vis-agent mcp remove [--scope SCOPE] <name>"
    :cmd/raw-args? true
    :cmd/args [{:name "name" :kind :positional :type :string :doc "Server name."} scope-arg
               target-arg db-arg]
    :cmd/examples ["vis-agent mcp remove notion" "vis-agent mcp remove notion -s user"]
    :cmd/run-fn cli-mcp-remove!}
   {:cmd/name "serve"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Run Vis as an MCP server on standard input and output, like `claude mcp serve`. Its tools are the tools of the Vis agent."
    :cmd/usage "vis-agent mcp serve [-d|--debug] [--verbose]"
    :cmd/raw-args? true
    :cmd/args [{:name "debug" :kind :flag :type :boolean :doc "-d. Write each message to stderr."}
               {:name "verbose" :kind :flag :type :boolean :doc "Write each message to stderr."}
               db-arg]
    :cmd/examples ["vis-agent mcp serve"]
    :cmd/run-fn cli-mcp-serve!}
   {:cmd/name "reset-project-choices"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Reset all approved and rejected project-scoped (.mcp.json) servers within this project."
    :cmd/usage "vis-agent mcp reset-project-choices"
    :cmd/raw-args? true
    :cmd/args [db-arg]
    :cmd/examples ["vis-agent mcp reset-project-choices"]
    :cmd/run-fn cli-mcp-reset-project-choices!}
   {:cmd/name "test"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Connect a server through the gateway without saving it, and show the number of its tools. Takes the arguments of add."
    :cmd/usage "vis-agent mcp test [options] <name> <commandOrUrl> [args...]"
    :cmd/raw-args? true
    :cmd/args
    [{:name "transport" :kind :flag :type :string :doc "-t. stdio (default), http or sse."}
     {:name "env" :kind :flag :type :string :doc "-e KEY=value."}
     {:name "header" :kind :flag :type :string :doc "-H \"Name: value\"."} db-arg]
    :cmd/examples ["vis-agent mcp test --transport http linear https://mcp.linear.app/mcp"]
    :cmd/run-fn cli-mcp-test!}
   {:cmd/name "enable"
    :cmd/parent ["mcp"]
    :cmd/doc "Turn a configured server back on."
    :cmd/usage "vis-agent mcp enable <NAME> [--scope SCOPE --target-id ID] [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}
               {:name "scope"
                :kind :flag
                :type :string
                :doc "Settings scope: global, project, group or session."}
               {:name "target-id"
                :kind :flag
                :type :string
                :doc "Project id/root, group id or session id; required outside global."}]
    :cmd/run-fn cli-mcp-enable!}
   {:cmd/name "disable"
    :cmd/parent ["mcp"]
    :cmd/doc "Turn a configured server off without deleting it."
    :cmd/usage "vis-agent mcp disable <NAME> [--scope SCOPE --target-id ID] [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}
               {:name "scope"
                :kind :flag
                :type :string
                :doc "Settings scope: global, project, group or session."}
               {:name "target-id"
                :kind :flag
                :type :string
                :doc "Project id/root, group id or session id; required outside global."}]
    :cmd/run-fn cli-mcp-disable!}
   {:cmd/name "kill"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Stop a server now and hold it down until started again (runtime only, config unchanged)."
    :cmd/usage "vis-agent mcp kill <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-kill!}
   {:cmd/name "start"
    :cmd/parent ["mcp"]
    :cmd/doc "Undo a kill and reconnect a server now."
    :cmd/usage "vis-agent mcp start <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-start!}
   {:cmd/name "auth-start"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Begin the headless OAuth 2.1 flow (RFC 9728/8414 discovery, dynamic client registration, PKCE) for an HTTP server. Prints the authorize URL to open and the auth-complete command to run after."
    :cmd/usage "vis-agent mcp auth-start <NAME> [--db PATH]"
    :cmd/args [{:name "name"
                :kind :positional
                :type :string
                :required true
                :doc "Server name (must be Streamable HTTP)."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples ["vis-agent mcp auth-start linear"]
    :cmd/run-fn cli-mcp-auth-start!}
   {:cmd/name "auth-complete"
    :cmd/parent ["mcp"]
    :cmd/doc
    "Finish an OAuth flow with the redirect URL the browser landed on (or its bare `code=` value)."
    :cmd/usage "vis-agent mcp auth-complete <NAME> --flow-id ID --input URL_OR_CODE [--db PATH]"
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
    ["vis-agent mcp auth-complete linear --flow-id abc123 --input \"http://127.0.0.1:5555/callback?code=xyz&state=...\""]
    :cmd/run-fn cli-mcp-auth-complete!}
   {:cmd/name "auth-poll"
    :cmd/parent ["mcp"]
    :cmd/doc "Read an in-flight OAuth flow's verdict without blocking: pending, ok, or error."
    :cmd/usage "vis-agent mcp auth-poll <NAME> --flow-id ID [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "flow-id"
                :kind :flag
                :type :string
                :required true
                :doc "flow_id printed by auth-start."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-auth-poll!}
   {:cmd/name "auth-cancel"
    :cmd/parent ["mcp"]
    :cmd/doc "Abandon an in-flight OAuth flow and release its loopback listener."
    :cmd/usage "vis-agent mcp auth-cancel <NAME> --flow-id ID [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "flow-id" :kind :flag :type :string :required true :doc "flow_id to abandon."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/run-fn cli-mcp-auth-cancel!}
   {:cmd/name "auth-logout"
    :cmd/parent ["mcp"]
    :cmd/doc "Forget the gateway's persisted OAuth tokens for a server."
    :cmd/usage "vis-agent mcp auth-logout <NAME> [--db PATH]"
    :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
               {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
    :cmd/examples ["vis-agent mcp auth-logout linear"]
    :cmd/run-fn cli-mcp-auth-logout!}])

(def subcommands
  "Subcommands registered under `vis-agent mcp`. As in `claude mcp`, a usage error
   ends with exit code 1."
  (mapv #(assoc % :cmd/usage-exit-code 1) subcommand-specs))
