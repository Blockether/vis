(ns com.blockether.vis.internal.config.mcp-json
  "Claude Code's shared project file `.mcp.json`. The engine reads its `mcpServers`
   as part of the committed project tier, below `vis.yml`. `vis-agent mcp add
   --scope project` writes the file in the Claude Code format."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.validation :as config-validation]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files LinkOption StandardCopyOption]
           [java.nio.file.attribute BasicFileAttributes]
           [java.util.concurrent TimeUnit]))

(defn path
  "`.mcp.json` of the current workspace root."
  ^String []
  (str (io/file (workspace/cwd) ".mcp.json")))

(def ^:private default-ref
  "Claude Code's `${NAME:-default}` form. Vis resolves a plain `${NAME}` later."
  #"\$\{([A-Za-z_][A-Za-z0-9_]*):-([^}]*)\}")

(defn- expand-defaults
  [v]
  (cond (string? v) (str/replace v
                                 default-ref
                                 (fn [[_ k d]]
                                   (or (not-empty (util/env-val k)) d)))
        (map? v) (update-vals v expand-defaults)
        (sequential? v) (mapv expand-defaults v)
        :else v))

(def ^:private transports {"stdio" "stdio" "http" "streamable_http" "sse" "sse"})

(defn- strings [m] (when (map? m) (not-empty (update-vals m str))))

(defn ->spec
  "Vis server spec of one Claude Code server entry, or nil when the entry names
   no transport that Vis has. `type` defaults to stdio for a `command` and to
   Streamable HTTP for a `url`."
  [entry]
  (when (map? entry)
    (let [entry
          (expand-defaults entry)

          kind
          (get entry "type")

          transport
          (cond (some? kind) (get transports kind)
                (get entry "command") "stdio"
                (get entry "url") "streamable_http")

          client-id
          (get-in entry ["oauth" "clientId"])]

      (cond (nil? transport) nil
            (= "stdio" transport) (cond-> {"transport" "stdio" "command" (get entry "command")}
                                    (seq (get entry "args"))
                                    (assoc "args" (mapv str (get entry "args")))

                                    (strings (get entry "env"))
                                    (assoc "env" (strings (get entry "env"))))
            :else (cond-> {"transport" transport "url" (get entry "url")}
                    (strings (get entry "headers"))
                    (assoc "headers" (strings (get entry "headers")))

                    client-id
                    (assoc "auth" {"client_id" (str client-id)}))))))

(defn ->entry
  "Claude Code server entry of a Vis server spec, in the shape that `claude mcp add` writes."
  [spec]
  (if (= "stdio" (get spec "transport"))
    {"type" "stdio"
     "command" (get spec "command")
     "args" (vec (get spec "args"))
     "env" (or (get spec "env") {})}
    (cond-> {"type" (if (= "sse" (get spec "transport")) "sse" "http") "url" (get spec "url")}
      (seq (get spec "headers"))
      (assoc "headers" (get spec "headers"))

      (get-in spec ["auth" "client_id"])
      (assoc "oauth" {"clientId" (get-in spec ["auth" "client_id"])}))))

(defn read-file
  "The parsed JSON object at `file`, or {} when the file does not exist."
  [file]
  (let [f (io/file file)]
    (if-not (.isFile f)
      {}
      (let [value (json/read-json (slurp f))]
        (when-not (map? value)
          (throw (ex-info (str file " does not hold a JSON object") {:path (str file)})))
        value))))

(defn write-file!
  "Atomically replace `file` with `value` as JSON, indented by two spaces like Claude Code."
  [file value]
  (let [target
        (.toPath (io/file file))

        temp
        (.toPath (io/file (str file ".tmp")))]

    (spit (.toFile temp)
          (str
            (json/write-json-str value :indent-str "  " :escape-slash false :escape-unicode false)
            "\n"))
    (Files/move temp
                target
                (into-array StandardCopyOption
                            [StandardCopyOption/REPLACE_EXISTING StandardCopyOption/ATOMIC_MOVE]))
    nil))

(defn- file-stamp
  [^String file]
  (let [f (io/file file)]
    (when (.isFile f)
      (let [attrs ^BasicFileAttributes
                  (Files/readAttributes (.toPath f) BasicFileAttributes ^"[Ljava.nio.file.LinkOption;" (make-array LinkOption 0))]
        [file (.to (.lastModifiedTime attrs) TimeUnit/NANOSECONDS) (.size attrs)]))))

(defn- load-servers
  [file]
  (try (let [entries (get (read-file file) "mcpServers")]
         (into {}
               (keep (fn [[name entry]]
                       (let [name (str name)
                             spec (->spec entry)]

                         (if (and spec (config-validation/valid? {"mcp" {"servers" {name spec}}}))
                           [name spec]
                           (do (tel/log! {:level :warn
                                          :id ::server-skipped
                                          :data {:source file :server name}}
                                         "Vis cannot use this .mcp.json server; it is skipped")
                               nil)))))
               (when (map? entries) entries)))
       (catch Exception e
         (tel/log! {:level :warn :id ::file-unreadable :data {:source file} :error e}
                   "Vis cannot read .mcp.json; its servers are skipped")
         {})))

(defonce ^:private servers-cache (atom nil))

(defn servers
  "Vis specs of the usable `.mcp.json` servers of the workspace, by name. A broken
   file or entry logs a warning and gives no server, so it never stops a session."
  []
  (let [file (path)]
    (if-let [stamp (file-stamp file)]
      (let [cached @servers-cache]
        (if (= stamp (:stamp cached))
          (:value cached)
          (let [value (load-servers file)]
            (reset! servers-cache {:stamp stamp :value value})
            value)))
      {})))
