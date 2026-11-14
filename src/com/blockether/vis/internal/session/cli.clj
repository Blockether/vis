(ns com.blockether.vis.internal.session.cli
  "`vis-agent sessions` commands: list, show, fork, delete, search, export and import
   persisted sessions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.transcript-export :as transcript-export]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.session.snapshot :as snapshot]
            [com.blockether.vis.internal.workspace.core :as workspace])
  (:import (java.io File)
           (java.nio.charset StandardCharsets)
           (java.net URLEncoder)
           (java.nio.file CopyOption Files StandardCopyOption)))

(def ^:private known-channels #{"tui" "cli" "api"})

(def ^:private known-channel-filters (conj known-channels "all"))

(defn- resolve-session-by-prefix
  "Resolve a user-supplied session reference (full UUID or an
   unambiguous prefix) to the canonical UUID. Scans every channel
   because forks are channel-agnostic; the user typed an id, we find
   it. Returns nil on miss or ambiguous prefix. Existence-checks full
   UUID strings; backend `db-resolve-session-id` only parses them."
  [d input]
  (let [s (some-> input
                  str
                  str/trim)]
    (when (seq s)
      (letfn [(existing-id [id]
                (when (and id (try (persistance/db-get-session d id) (catch Throwable _ nil))) id))]
        (or (try (existing-id (persistance/db-resolve-session-id d s)) (catch Throwable _ nil))
            (let [matches (->> (or (persistance/db-list-sessions d :all) [])
                               (filter #(str/starts-with? (str (:id %)) s))
                               (map :id)
                               distinct
                               vec)]
              (when (= 1 (count matches)) (existing-id (first matches)))))))))

(defn- cli-fork-session!
  "Fork a session by id. Creates a new `session_state` row
   that points at the latest state as its parent, optionally with a
   user-supplied title. Prints the new state UUID; the session
   id (soul-id) stays the same so `vis-agent tui --session-id <ID>` keeps
   working and now resumes from the fork."
  [cid-input title]
  (let [d
        (lp/db-info)

        resolved
        (resolve-session-by-prefix d cid-input)]

    (cond (nil? resolved) (do (commandline/stdout! (str "Session not found: " cid-input))
                              (commandline/stdout! "")
                              (commandline/stdout! "List existing sessions with:")
                              (commandline/stdout! "  vis-agent sessions")
                              (shutdown-agents)
                              (System/exit 1))
          :else
          (let [;; Fork = new session_state = new workspace pin (1:1).
                ;; Mint a fresh workspace row for the fork.
                ws-id
                (:id (workspace/ensure-workspace! d {}))

                opts
                (cond-> {:workspace-id ws-id}
                  (and title (not (str/blank? title)))
                  (assoc :title title))

                new-state
                (persistance/db-fork-session! d resolved opts)]

            (if new-state
              (do (commandline/stdout! "")
                  (commandline/stdout! (str "  Forked session " resolved))
                  (when title (commandline/stdout! (str "  Title:        " title)))
                  (commandline/stdout! (str "  New state-id: " new-state))
                  (commandline/stdout! "")
                  (commandline/stdout! (str "  Resume with: vis-agent tui --session-id " resolved))
                  (commandline/stdout! ""))
              (do (commandline/stdout!
                    (str "Failed to fork session " resolved "; no existing state to fork from."))
                  (shutdown-agents)
                  (System/exit 1)))
            (shutdown-agents)))))

(defn- session-sort-key
  [{:keys [last-turn-at created-at id]}]
  [(- (long (or (some-> last-turn-at
                        inst-ms)
                0)))
   (- (long (or (some-> created-at
                        inst-ms)
                0))) (str id)])

(defn- session-row
  [d c]
  (let [turns
        (or (persistance/db-list-session-turns d (:id c)) [])

        last-turn
        (last turns)

        channel-name
        (name (or (:channel c) :unknown))]

    {:id (str (:id c))
     :title (or (:title c) "-")
     :last-channel channel-name
     :turns (count turns)
     :forks (long (or (:fork-count c) 0))
     :last-turn-at (:created-at last-turn)
     :last-turn (or (some-> last-turn
                            :created-at
                            fmt/format-date)
                    "-")
     :created-at (:created-at c)
     :created (or (fmt/format-date (:created-at c)) "-")}))

(defn- session-rows
  [d sessions]
  (->> sessions
       (mapv #(session-row d %))
       (sort-by session-sort-key)
       vec))

(defn- sessions-for-listing
  [channel-input]
  (if channel-input (lp/by-channel (keyword channel-input)) (lp/by-channel :all)))

(defn- cli-list-sessions!
  "List persisted sessions. `channel-input` filters to one channel;
   nil lists every known channel. Rows sort by most recent turn first,
   with empty sessions after sessions that have turns."
  [channel-input]
  (let [channel-label
        (or channel-input "all")

        sessions
        (sessions-for-listing channel-input)

        d
        (lp/db-info)]

    (if (empty? sessions)
      (commandline/stdout!
        (if channel-input (str "No " channel-input " sessions found.") "No sessions found."))
      (let [rows (session-rows d sessions)]
        (commandline/stdout!
          (str "\n  " (if channel-input (str/upper-case channel-label) "All") " Sessions\n"))
        (commandline/print-table! [{:key :id :label "ID" :width 36 :align :left}
                                   {:key :title :label "Title" :width 24 :align :left :grow? true}
                                   {:key :last-channel :label "Last Channel" :width 12 :align :left}
                                   {:key :turns :label "Turns" :width 5 :align :right}
                                   {:key :forks :label "Forks" :width 5 :align :right}
                                   {:key :last-turn :label "Last Turn" :width 16 :align :left}
                                   {:key :created :label "Created" :width 16 :align :left}]
                                  rows)
        (commandline/stdout! (str "\n  " (count rows) " session(s)\n"))
        (commandline/stdout! "  Resume with: vis-agent tui --session-id <ID>  (full or short)")
        (commandline/stdout! "  Pick latest: vis-agent tui --continue")
        (commandline/stdout! "  Browse:      vis-agent tui --resume")
        (commandline/stdout! "  Show:        vis-agent sessions show <ID>")
        (commandline/stdout! "  Fork:        vis-agent sessions fork <ID> [--title TITLE]")
        (commandline/stdout! "  Export:      vis-agent sessions export <ID> --md")
        (commandline/stdout! "  Move:        vis-agent sessions export <ID> --snapshot FILE"))))
  (shutdown-agents))

(defn- cli-sessions-list!
  [parsed _residual]
  (config/init-cli!)
  (let [channel
        (get parsed "channel")

        ch
        (when (and channel (not= "all" channel)) (when (contains? known-channels channel) channel))]

    (when (and channel (not (contains? known-channel-filters channel)))
      (commandline/stdout! (str "Unknown channel: "
                                channel
                                ". Expected one of: "
                                (str/join ", " (sort known-channel-filters))
                                ". Showing all sessions."))
      (commandline/stdout! ""))
    (cli-list-sessions! ch)))

(defn- session-or-exit!
  [d cid-input]
  (let [resolved (resolve-session-by-prefix d cid-input)]
    (if-let [session (when resolved (persistance/db-get-session d resolved))]
      (assoc session :id resolved)
      (do (commandline/stdout! (str "Session not found: " cid-input))
          (commandline/stdout! "")
          (commandline/stdout! "List existing sessions with:")
          (commandline/stdout! "  vis-agent sessions list")
          (shutdown-agents)
          (System/exit 1)))))

(defn- session-detail-row [d session] (session-row d session))

(defn- cli-show-session!
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))

        row
        (session-detail-row d session)

        states
        (persistance/db-list-session-states d (:id session))]

    (commandline/stdout! (str "\n  Session " (:id session)))
    (commandline/stdout! "  ─────────────────────────────────")
    (commandline/stdout! (str "  Title:        " (:title row)))
    (commandline/stdout! (str "  Channel:      " (:last-channel row)))
    (commandline/stdout! (str "  Turns:        " (:turns row)))
    (commandline/stdout! (str "  Forks:        " (:forks row)))
    (commandline/stdout! (str "  Created:      " (:created row)))
    (commandline/stdout! (str "  Last turn:    " (:last-turn row)))
    (when-let [model (:model session)]
      (commandline/stdout! (str "  Model:        " model)))
    (when-let [provider (:provider session)]
      (commandline/stdout! (str "  Provider:     " (name provider))))
    (when-let [ws (when-let [sid (persistance/db-latest-session-state-id d (:id session))]
                    (workspace/for-session d sid))]
      (commandline/stdout! (str "  Root:         " (:root ws))))
    (when (seq states)
      (commandline/stdout! "")
      (commandline/stdout! "  States")
      (commandline/print-table! [{:key :version :label "Version" :width 7 :align :right}
                                 {:key :state-id :label "State ID" :width 36 :align :left}
                                 {:key :parent :label "Parent" :width 8 :align :left}
                                 {:key :turns :label "Turns" :width 5 :align :right}
                                 {:key :created :label "Created" :width 16 :align :left}]
                                (mapv (fn [state]
                                        {:version (:version state)
                                         :state-id (str (:state-id state))
                                         :parent (if-let [p (:parent-state-id state)]
                                                   (subs (str p) 0 8)
                                                   "-")
                                         :turns (:turn-count state)
                                         :created (or (fmt/format-date (:created-at state)) "-")})
                                      states)))
    (commandline/stdout! "")
    (commandline/stdout! (str "  Resume:  vis-agent tui --session-id " (:id session)))
    (commandline/stdout!
      (str "  Export:  vis-agent sessions export " (subs (str (:id session)) 0 8) " --md"))
    (commandline/stdout! "")
    (shutdown-agents)))

(defn- resolve-out-path
  "Resolve a user-supplied output path against the invocation directory.
   `bin/vis-agent` runs the JVM source runtime from its source root (so
   `clojure -M:vis` finds deps.edn) but passes the real invocation cwd as
   `-Duser.dir`. Java resolves relative `File` paths against the OS cwd, so a
   bare `out.html` would silently land in the source root while the printed
   path (from `user.dir`) said otherwise. Anchor relatives to `user.dir`;
   absolute paths pass through."
  [path]
  (let [f (io/file path)]
    (.getPath (if (.isAbsolute f) f (io/file (System/getProperty "user.dir") path)))))

(defn- ensure-ext
  "Append `.ext` to `path` when it doesn't already end with it (case-insensitive),
   so a bare `siema` given to `--html` lands as `siema.html`."
  [path ext]
  (let [dot (str "." ext)]
    (if (str/ends-with? (str/lower-case path) (str/lower-case dot)) path (str path dot))))

(def ^:private snapshot-timeout-ms
  "How long a snapshot export or import may take. A snapshot can hold up to 1 GiB."
  600000)

(defn- refuse!
  "Stop the command with `message` as a user error, without a stack trace."
  [message]
  (throw (ex-info message {:type :vis.cli/session-snapshot :vis/user-error true})))

(defn- gateway-error
  "The message of the refused gateway `response`."
  [{:keys [status body]}]
  (let [text
        (if (bytes? body) (String. ^bytes body StandardCharsets/UTF_8) (str body))

        error
        (get (try (wire/parse-json text) (catch Exception _ nil)) "error")]

    (or (when (map? error) (not-empty (str (get error "message"))))
        (when (string? error) (not-empty error))
        (str "The gateway answered HTTP " status))))

(defn- write-atomically!
  "Write `data` to `target` in one step. An interrupted write leaves no partial file."
  [^File target ^bytes data]
  (let [dir
        (.getAbsoluteFile (or (.getParentFile (.getAbsoluteFile target)) (io/file ".")))

        _
        (.mkdirs dir)

        tmp
        (File/createTempFile ".vissnap-" ".tmp" dir)]

    (try (io/copy data tmp)
         (Files/move (.toPath tmp)
                     (.toPath target)
                     (into-array CopyOption
                                 [StandardCopyOption/ATOMIC_MOVE
                                  StandardCopyOption/REPLACE_EXISTING]))
         (finally (.delete tmp)))))

(defn- export-snapshot!
  "Write the `.vissnap` snapshot of session `sid` and its children to `path` through the
   gateway. Answers the snapshot manifest."
  [sid path]
  (let [response (gateway-client/request! :get
                                          (str "/v1/sessions/" sid "/snapshot")
                                          {:as :bytes :timeout-ms snapshot-timeout-ms})]
    (when-not (= 200 (:status response)) (refuse! (gateway-error response)))
    (write-atomically! (io/file path) (:body response))
    (snapshot/info (:body response))))

(defn- import-query
  "The query string of a snapshot import, with a leading `?` or empty. Without `root`, the
   restore uses the folder `cwd`. With `group` and no `root`, the gateway uses the folder of
   the project of that group."
  [{:keys [root group key]} cwd]
  (let [params (cond-> []
                 (or root (when-not group cwd))
                 (conj ["root" (or root cwd)])

                 group
                 (conj ["group_id" group])

                 key
                 (conj ["key" key]))]
    (if (empty? params)
      ""
      (str "?"
           (str/join "&"
                     (map (fn [[k v]]
                            (str k "=" (URLEncoder/encode (str v) StandardCharsets/UTF_8)))
                          params))))))

(defn- import-snapshot!
  "Restore the snapshot file at `path` through the gateway as a new session tree. Answers the
   wire session of its root. `opts` and `cwd` are as in [[import-query]]."
  [path opts cwd]
  (let [f (io/file path)]
    (when-not (.isFile f)
      (refuse! (str "No snapshot file at " (paths/abbreviate-home (.getPath f)))))
    (let [response (gateway-client/request! :post
                                            (str "/v1/sessions/actions/import"
                                                 (import-query opts cwd))
                                            {:body (Files/readAllBytes (.toPath f))
                                             :raw-body? true
                                             :timeout-ms snapshot-timeout-ms})]
      (when-not (<= 200 (long (:status response)) 299) (refuse! (gateway-error response)))
      (wire/parse-json
        (let [body (:body response)]
          (if (bytes? body) (String. ^bytes body StandardCharsets/UTF_8) (str body)))))))

(defn- counted [n word] (str n " " word (when-not (= 1 n) "s")))

(defn- cli-export-session!
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))

        md?
        (boolean (get parsed "md"))

        html-path
        (some-> (get parsed "html")
                str/trim
                not-empty
                (ensure-ext "html")
                resolve-out-path)

        snapshot-path
        (some-> (get parsed "snapshot")
                str/trim
                not-empty
                (ensure-ext "vissnap")
                resolve-out-path)

        chosen
        (filterv some? [(when md? :md) (when html-path :html) (when snapshot-path :snapshot)])]

    (when (> (count chosen) 1)
      (commandline/stdout! "Choose exactly one of --md, --html PATH or --snapshot PATH.")
      (shutdown-agents)
      (System/exit 2))
    (cond snapshot-path (let [counts (get (export-snapshot! (:id session) snapshot-path) "counts")]
                          (commandline/stdout! (str "Exported snapshot: "
                                                    (paths/abbreviate-home snapshot-path)
                                                    " ("
                                                    (counted (get counts "sessions") "session")
                                                    ", "
                                                    (counted (get counts "turns") "turn")
                                                    ")"))
                          (commandline/stdout! (str "  Import:  vis-agent sessions import "
                                                    (.getName (io/file snapshot-path)))))
          html-path (let [target (io/file html-path)]
                      (when-let [parent (.getParentFile ^java.io.File target)]
                        (.mkdirs parent))
                      (spit target
                            (or (try (transcript-export/html (:id session))
                                     (catch clojure.lang.ExceptionInfo e (refuse! (ex-message e))))
                                (refuse! (str "No session " (:id session)))))
                      (commandline/stdout! (str "Exported HTML: "
                                                (paths/abbreviate-home (.getPath target)))))
          :else (commandline/write-stdout! (transcript-export/md (:id session))))
    (shutdown-agents)))

(defn- cli-import-session!
  [parsed residual]
  (config/init-cli!)
  (let [path
        (some-> (or (get parsed "file") (first residual))
                str/trim
                not-empty
                resolve-out-path)

        root
        (some-> (get parsed "root")
                str/trim
                not-empty
                resolve-out-path
                (as-> p (.getCanonicalPath ^File (io/file p))))

        group
        (some-> (get parsed "group")
                str/trim
                not-empty)

        key
        (some-> (get parsed "key")
                str/trim
                not-empty)]

    (when-not path (refuse! "Name the snapshot file: vis-agent sessions import FILE"))
    (when (and root (not (.isDirectory (io/file root))))
      (refuse! (str "No folder at " (paths/abbreviate-home root))))
    (let [restored
          (import-snapshot! path {:root root :group group :key key} (System/getProperty "user.dir"))

          id
          (get restored "id")]

      (commandline/stdout! (str "Imported session "
                                id
                                (when-let [title (not-empty (str (get restored "title")))]
                                  (str " \"" title "\""))))
      (commandline/stdout! (str "  Resume:  vis-agent tui --session-id " id)))
    (shutdown-agents)))

(defn- cli-delete-session!
  [parsed _residual]
  (config/init-cli!)
  (let [session
        (session-or-exit! (lp/db-info) (get parsed "session-id"))

        ;; The gateway deletes the session and its sandbox state. A
        ;; gateway that has the session loaded must stop it: a running turn wrote the
        ;; files of a session that this process had deleted.
        response
        (gateway-client/request! :delete (str "/v1/sessions/" (:id session)))]

    (when-not (<= 200 (long (:status response)) 299) (refuse! (gateway-error response)))
    (commandline/stdout! (str "Deleted session " (:id session)))
    (shutdown-agents)))

(defn- cli-fork-session-command!
  [parsed _residual]
  (config/init-cli!)
  (cli-fork-session! (get parsed "session-id") (get parsed "title")))

(defn- cli-sessions-search!
  "`vis-agent sessions search <query>` handler. Uses the same transcript search as the
   TUI session navigator: token-prefix matching across user requests and assistant
   replies (answer + thinking), ordered newest-first. Hits print one per line:

     <session-id-prefix>  <side>     <snippet>

   Snippets carry `[match]` markers around hit terms. `--limit N` caps the
   result count (default 25)."
  [parsed _residual]
  (config/init-cli!)
  (let [query
        (or (get parsed "query") "")

        limit
        (max 1
             (long (or (some-> (get parsed "limit")
                               str/trim
                               Long/parseLong)
                       25)))]

    (cond (str/blank? query)
          (do (commandline/stdout! "vis-agent sessions search <query> [--limit N]")
              (commandline/stdout! "")
              (commandline/stdout! "Searches transcripts exactly like the TUI session navigator.")
              (shutdown-agents)
              (System/exit 1))
          :else (let [d
                      (lp/db-info)

                      hits
                      (->> (persistance/db-search-session-matches d :all query)
                           (mapcat (fn [{:keys [id hits]}]
                                     (map #(assoc % :session-id id) hits)))
                           (take limit)
                           vec)]

                  (cond (empty? hits) (do (commandline/stdout! (str "No matches for: " query))
                                          (shutdown-agents))
                        :else (do
                                (commandline/stdout! (str (count hits)
                                                          " match" (when (not= 1 (count hits)) "es")
                                                          " for: " query))
                                (commandline/stdout! "")
                                (doseq [{:keys [session-id side snippet]} hits]
                                  (let [id-pref (let [s (str session-id)]
                                                  (subs s 0 (min 8 (count s))))
                                        snippet (str/replace (or snippet "") #"\s+" " ")]

                                    (commandline/stdout!
                                      (str id-pref "  " (format "%-8s" (name side)) "  " snippet))))
                                (shutdown-agents)))))))

(defn- cli-sessions!
  "`vis-agent sessions` default handler. Bare `vis-agent sessions` lists all
   sessions; every other operation is a canonical subcommand."
  [_parsed residual]
  (config/init-cli!)
  (if (seq residual)
    (do (commandline/stdout! (str "Unknown sessions command: " (first residual)))
        (commandline/stdout! "")
        (commandline/stdout! "Run: vis-agent sessions --help")
        (shutdown-agents)
        (System/exit 2))
    (cli-list-sessions! nil)))

;;; ── `vis-agent projects` ──────────────────────────────────────────────────────

(def command
  {:cmd/name "sessions"
   :cmd/doc "List, show, fork, delete, search, export or import persisted sessions."
   :cmd/usage "vis-agent sessions <list|show|fork|delete|search|export|import> [...]"
   :cmd/examples ["vis-agent sessions" "vis-agent sessions list" "vis-agent sessions show 3a7b2c1d"
                  "vis-agent sessions fork 3a7b2c1d --title \"Branch A\""
                  "vis-agent sessions export 3a7b2c1d --md"
                  "vis-agent sessions export 3a7b2c1d --html out.html"
                  "vis-agent sessions export 3a7b2c1d --snapshot work.vissnap"
                  "vis-agent sessions import work.vissnap" "vis-agent sessions search \"foo bar\""]
   :cmd/subcommands #(registry/registered-under ["sessions"])
   :cmd/run-fn cli-sessions!})

(def subcommands
  "Subcommands registered under `vis-agent sessions`."
  [{:cmd/name "list"
    :cmd/parent ["sessions"]
    :cmd/doc "List persisted sessions."
    :cmd/usage "vis-agent sessions list [all|tui|cli]"
    :cmd/args [{:name "channel"
                :kind :positional
                :type :string
                :doc "Optional channel filter (all|tui|cli; default all)."}]
    :cmd/examples ["vis-agent sessions list" "vis-agent sessions list tui"]
    :cmd/run-fn cli-sessions-list!}
   {:cmd/name "show"
    :cmd/parent ["sessions"]
    :cmd/doc "Show one session's metadata, turns, and fork states."
    :cmd/usage "vis-agent sessions show <SESSION-ID>"
    :cmd/args [{:name "session-id"
                :kind :positional
                :type :string
                :required true
                :doc "Session id (full UUID or unambiguous prefix)."}]
    :cmd/examples ["vis-agent sessions show 3a7b2c1d"]
    :cmd/run-fn cli-show-session!}
   {:cmd/name "fork"
    :cmd/parent ["sessions"]
    :cmd/doc "Fork a session from its latest state."
    :cmd/usage "vis-agent sessions fork <SESSION-ID> [--title TITLE]"
    :cmd/args [{:name "session-id"
                :kind :positional
                :type :string
                :required true
                :doc "Session id (full UUID or unambiguous prefix)."}
               {:name "title" :kind :flag :type :string :doc "Title to set on the new fork."}]
    :cmd/examples ["vis-agent sessions fork 3a7b2c1d"
                   "vis-agent sessions fork 3a7b2c1d --title \"Branch A\""]
    :cmd/run-fn cli-fork-session-command!}
   {:cmd/name "delete"
    :cmd/parent ["sessions"]
    :cmd/doc "Delete a session tree from persistent storage."
    :cmd/usage "vis-agent sessions delete <SESSION-ID>"
    :cmd/args [{:name "session-id"
                :kind :positional
                :type :string
                :required true
                :doc "Session id (full UUID or unambiguous prefix)."}]
    :cmd/examples ["vis-agent sessions delete 3a7b2c1d"]
    :cmd/run-fn cli-delete-session!}
   {:cmd/name "export"
    :cmd/parent ["sessions"]
    :cmd/doc
    "Export a session: Markdown on stdout, styled HTML to a file, or a snapshot to import elsewhere."
    :cmd/usage "vis-agent sessions export <SESSION-ID> [--md | --html PATH | --snapshot PATH]"
    :cmd/args [{:name "session-id"
                :kind :positional
                :type :string
                :required true
                :doc "Session id (full UUID or unambiguous prefix)."}
               {:name "md" :kind :flag :type :boolean :doc "Print Markdown to stdout (default)."}
               {:name "html" :kind :flag :type :string :doc "Write styled HTML export to PATH."}
               {:name "snapshot"
                :kind :flag
                :type :string
                :doc "Write a .vissnap snapshot of the session and its subagents to PATH."}]
    :cmd/examples ["vis-agent sessions export 3a7b2c1d --md"
                   "vis-agent sessions export 3a7b2c1d --html out.html"
                   "vis-agent sessions export 3a7b2c1d --snapshot work.vissnap"]
    :cmd/run-fn cli-export-session!}
   {:cmd/name "import"
    :cmd/parent ["sessions"]
    :cmd/doc "Restore a .vissnap snapshot as a new session, for example on another computer."
    :cmd/usage "vis-agent sessions import <FILE> [--root DIR] [--group ID] [--key KEY]"
    :cmd/args [{:name "file"
                :kind :positional
                :type :string
                :required true
                :doc "Snapshot file from `vis-agent sessions export <ID> --snapshot PATH`."}
               {:name "root"
                :kind :flag
                :type :string
                :doc "Project folder of the restored session. Default: the current folder."}
               {:name "group"
                :kind :flag
                :type :string
                :doc "Session group of the restored session. Without --root, its project folder."}
               {:name "key"
                :kind :flag
                :type :string
                :doc "Restore once: a second import with the same KEY returns the first session."}]
    :cmd/examples ["vis-agent sessions import work.vissnap"
                   "vis-agent sessions import work.vissnap --root ~/code/shop --key shop-1"]
    :cmd/run-fn cli-import-session!}
   {:cmd/name "search"
    :cmd/parent ["sessions"]
    :cmd/doc "Transcript search with the same matching semantics as the TUI session navigator."
    :cmd/usage "vis-agent sessions search <query> [--limit N]"
    :cmd/args [{:name "query"
                :kind :positional
                :type :string
                :variadic true
                :doc "Words to search for (case-insensitive token prefixes, as in the TUI)."}
               {:name "limit" :kind :flag :type :string :doc "Max hits to print (default 25)."}]
    :cmd/examples ["vis-agent sessions search \"provider credentials\""
                   "vis-agent sessions search \"authentication failed\" --limit 100"]
    :cmd/run-fn cli-sessions-search!}])
