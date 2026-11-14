(ns com.blockether.vis.internal.workspace.core
  "Session workspaces, DB-pinned to session_state 1:1.

   A workspace is the directory in which a session works: the folder where the user
   started `vis`, or the folder that the session moved to. Vis does not require it
   to be a git repository.

   Vis never mutates JVM user.dir. Channels rebind *workspace-root* per
   turn from the active workspace; tools resolve paths via
   (workspace/cwd). There is NO process-cwd fallback in production -
   the env carries `:workspace/root` from `create-environment` onward."
  (:refer-clojure :exclude [get])
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io File]))

;; Dynamic cwd binding

(def ^:dynamic *workspace-root*
  "Canonical workspace root for the current tool call. Bound per-turn
   by the channel layer; never `nil` in normal operation."
  nil)

(def ^:dynamic *filesystem-roots*
  "Configured filesystem catalog entries available to the current tool call, as
   canonical `[{:root}]` entries. These are rebuilt from `vis.yml` on
   `/reload`; they are never session-persisted or mutable through a command."
  nil)

(def config-sources
  "Configuration tiers a session can read: `~/.vis` (global) and the workspace (project)."
  #{"global" "project"})

(def ^:dynamic *config-sources*
  "Configuration tiers the current session reads, a subset of `config-sources`. Nil
   reads every tier. Bound from the session environment beside `*workspace-root*`.
   Provider and sign-in settings in `~/.vis` load in every case."
  nil)

(def config-sources-setting
  "Session setting that keeps the tiers a session was created with. The colon keeps
   it apart from snake_case toggle ids."
  "launch:config_sources")

(defn source-enabled?
  "True when the current session reads configuration tier `tier`."
  [tier]
  (or (nil? *config-sources*) (contains? *config-sources* tier)))

(defn normalize-config-sources
  "Validate a tier collection. Return nil for every tier, else the set of tiers.
   Throw a 400 error for an unknown tier."
  [sources]
  (when (some? sources)
    (let [chosen
          (set (map str sources))

          unknown
          (remove config-sources chosen)]

      (when (seq unknown)
        (throw (ex-info (str "Unknown configuration source: "
                             (str/join ", " (sort unknown))
                             ". Use global or project.")
                        {:status 400 :type :config/unknown-source :sources (vec (sort unknown))})))
      (when-not (= chosen config-sources) chosen))))

(defn session-config-sources
  "Tiers that session `session-id` reads, from its saved setting. Nil reads every tier."
  [db-info session-id]
  (when (and db-info session-id)
    (some-> (clojure.core/get (p/db-scoped-settings db-info "session" session-id)
                              config-sources-setting)
            normalize-config-sources)))

(defn normalize-root
  "Canonicalize a workspace root string/File. Blank/nil → nil. Expand a leading
   `~` or `~/` against the current user's home directory."
  [root]
  (let [s (some-> root
                  str
                  str/trim)]
    (when (seq s)
      (let [s (cond (= s "~") (System/getProperty "user.home")
                    (or (str/starts-with? s "~/") (str/starts-with? s "~\\"))
                    (str (System/getProperty "user.home") (subs s 1))
                    :else s)]
        (.getCanonicalPath (io/file s))))))

(defonce ^:private canonical-roots
  ;; Raw root value -> `[[raw owner] canonical path]` for [[canonical-root]].
  (atom {}))

(defn- canonical-root
  "[[normalize-root]] of `raw`, reused while `raw` and `owner`, the value that holds it,
   stay the same objects. A workspace change brings a new owner, so it resolves again."
  [raw owner]
  (util/cached canonical-roots 256 raw [raw owner] #(normalize-root raw)))

(defn workspace-root
  "Extract a canonical workspace root from an env map or raw root value.

   For an env map, the live `:workspace-atom` wins over the static `:workspace/root`:
   it always holds the session's saved workspace (#364)."
  [env-or-root]
  (if (map? env-or-root)
    (let [ws (some-> (:workspace-atom env-or-root)
                     deref)]
      (if-let [root (:root ws)]
        (canonical-root root ws)
        (canonical-root (:workspace/root env-or-root) env-or-root)))
    (canonical-root env-or-root nil)))

(defn ancestor-roots
  "Canonical workspace root followed by each parent through the nearest ancestor
   containing `.git` (inclusive). Outside a Git repository, continue through the
   filesystem root. This is topology, not Git indexing: explicit project config
   remains visible even when its directory is gitignored."
  [env-or-root]
  (if-let [root (workspace-root env-or-root)]
    (let [start (.getCanonicalFile (io/file root))
          stop (paths/nearest-ancestor #(.exists (io/file % ".git")) start)
          chain (paths/ancestor-chain start)]

      (if stop (conj (vec (take-while #(not= stop %) chain)) stop) (vec chain)))
    []))

(defn- resolve-filesystem-roots
  "The entries of [[env-filesystem-roots]] for `env-or-roots`."
  [env-or-roots]
  (let [environment?
        (map? env-or-roots)

        host-roots
        (when (and environment? (false? (get-in env-or-roots [:security-policy :jail-enabled])))
          (keep (fn [^File root]
                  (.getCanonicalPath root))
                (File/listRoots)))

        configured
        (if environment? (concat (:security/filesystem-roots env-or-roots) host-roots) env-or-roots)

        no-search
        (when environment?
          (into #{}
                (keep normalize-root)
                (concat host-roots (:security/no-search-roots env-or-roots))))]

    (into []
          (comp (keep normalize-root)
                (distinct)
                (map (fn [root]
                       (cond-> {:root root}
                         (contains? no-search root)
                         (assoc :no-search? true)))))
          configured)))

(defonce ^:private filesystem-roots-cache
  ;; Raw root strings -> `[input objects entries]` for [[env-filesystem-roots]].
  (atom {}))

(defn env-filesystem-roots
  "Canonical `[{:root}]` entries available to the current tool call: the configured
   workspace catalog entries and the immutable read/write roots from the environment
   security snapshot. Catalog `search: false` roots are marked `:no-search?` with the
   jail on or off. With the jail disabled, host filesystem roots are also granted and
   marked no-search so explicit paths are unrestricted without making default
   searches crawl the machine.

   Reuse the entries while each input stays the same object. A workspace change
   swaps the workspace value, so the roots resolve again on the next call."
  [env-or-roots]
  (if (map? env-or-roots)
    (util/cached filesystem-roots-cache
                 256
                 [::environment (:workspace/root env-or-roots)]
                 [(some-> (:workspace-atom env-or-roots)
                          deref) (:security-policy env-or-roots)
                  (:security/filesystem-roots env-or-roots)
                  (:security/no-search-roots env-or-roots)]
                 #(resolve-filesystem-roots env-or-roots))
    (util/cached filesystem-roots-cache
                 256
                 ::roots
                 [env-or-roots]
                 #(resolve-filesystem-roots env-or-roots))))

(defn cwd
  "Resolve the current workspace cwd. In production the channel
   wrapper binds `*workspace-root*` per turn, so the process-cwd
   fallback only fires from REPL / test / one-off CLI paths that have
   no session context."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir"))))

(defn cwd-root
  "Canonical path of `cwd`: the key of the current project's scoped state."
  ^String []
  (canonical-root (str (cwd)) nil))

(defn allowed-roots
  "Canonical absolute paths the current tool call may operate under: the primary
   cwd FIRST, then each bound filesystem root. Deduped; the primary is always
   present. The confinement set the editing layer's `safe-path` checks the target
   against."
  []
  (vec (distinct (cons (.getCanonicalPath (cwd)) (keep :root *filesystem-roots*)))))

(defn no-search-roots
  "Canonical paths of bound filesystem roots flagged `search: false` in the
   workspace catalog. `resolve-search-roots` prunes these from the DEFAULT grep
   sweep; explicit paths still reach them."
  []
  (into #{}
        (keep (fn [e]
                (when (:no-search? e) (:root e))))
        *filesystem-roots*))

(def ^:private temp-roots
  "System temp dirs (`/tmp` and the JVM `java.io.tmpdir`, e.g. `$TMPDIR`) the file
   tools may ALWAYS reach, independent of the workspace roots. Canonical (symlinks
   resolved, so macOS `/tmp` -> `/private/tmp`), computed once on first use; a
   non-existent/unresolvable entry is dropped."
  (delay (->> [(System/getProperty "java.io.tmpdir") "/tmp"]
              (keep (fn [s]
                      (when-not (str/blank? (str s))
                        (try (.toPath (.getCanonicalFile (java.io.File. ^String (str s))))
                             (catch Throwable _ nil)))))
              distinct
              vec)))

(def ^:private vis-always-roots
  "The `~/.vis` directory tree that file tools may always reach, independent of
   workspace roots. Canonical paths are computed once on first use; entries
   are dropped when `user.home` is unset."
  (delay (->> [".vis"]
              (keep (fn [^String sub]
                      (some-> (System/getProperty "user.home")
                              (java.io.File. sub))))
              (keep (fn [^java.io.File f]
                      (try (.toPath (.getCanonicalFile f)) (catch Throwable _ nil))))
              distinct
              vec)))

(defn resolve-file-path
  "Resolve a host file path with the same canonical root and always-access rules as the file tools.
   Bind *workspace-root* and *filesystem-roots* for the session before calling it."
  ^File [p]
  ;; Resolve `p` and confine it to the union of ALLOWED ROOTS: the primary
  ;; workspace cwd plus any extra filesystem roots bound for this turn. Relative
  ;; paths resolve against the primary root; an absolute path is taken as-is so
  ;; it may land under an added filesystem root. The confinement check runs on
  ;; CANONICAL paths (symlinks resolved, e.g. macOS /tmp -> /private/tmp) so it
  ;; matches the canonical allowed roots AND a symlink that points outside every
  ;; root is rejected. `..` traversal that escapes all roots is rejected too.
  (when (str/blank? (str p))
    (throw (ex-info "Path must be a non-blank string" {:type :workspace/blank-path :path p})))
  (let [cwd
        (cwd)

        canon
        (fn ^java.nio.file.Path [x]
          (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path (str x))))))))

        ^java.nio.file.Path cwd-canon
        (.toPath (.getCanonicalFile (.toFile (.normalize (.toAbsolutePath (fs/path cwd))))))

        ;; relative → under cwd; absolute → as-is. Canonical throughout so
        ;; symlinks (/tmp→/private/tmp) and `..` resolve before confinement.
        ^java.nio.file.Path canonical
        (.toPath (.getCanonicalFile (.toFile (.normalize
                                               (.toAbsolutePath
                                                 (fs/path cwd (paths/expand-home (str p))))))))

        ^java.nio.file.Path resolved
        (or (when (.startsWith canonical cwd-canon) canonical)
            (some (fn [root]
                    (let [^java.nio.file.Path rp (canon root)]
                      (when (.startsWith canonical rp) canonical)))
                  (keep :root *filesystem-roots*))
            ;; system temp dirs (/tmp, $TMPDIR) + Vis's own ~/.vis tree are ALWAYS
            ;; reachable, independent of workspace roots — config and diagnostics work.
            (some (fn [^java.nio.file.Path tr]
                    (when (.startsWith canonical tr) canonical))
                  (concat @temp-roots @vis-always-roots)))]

    (when-not resolved
      (throw (ex-info (str "Path '" p "' escapes the allowed workspace roots")
                      {:type :workspace/path-escape :path (str p)})))
    ;; A live database (or claim) this process locks: one read from a file tool
    ;; would release its locks (see `paths/held-file?`).
    (when (paths/held-file? (.toFile resolved))
      (throw
        (ex-info
          (str
            "Path '"
            p
            "' is a file this Vis process keeps locked while it runs; opening it here would release that lock")
          {:type :workspace/path-denied :path (str p) :reason :held-lock})))
    (.toFile resolved)))

(defn- file-path ^String [f] (.getCanonicalPath (io/file f)))

(defn trunk-root
  "The user's real working directory — where they launched `vis`.
   Canonical absolute path, never required to be a git repo.
   (`bin/vis-agent` preserves the invocation cwd
   as JVM user.dir even though it cd's to the repo for deps.)"
  ^String []
  (file-path (System/getProperty "user.dir")))

(defn- sanitize-id
  [s]
  (let [s (-> (str (or s "ws"))
              str/lower-case
              (str/replace #"[^a-z0-9._-]+" "-")
              (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? s) "ws" s)))

(defn repo-id-for
  "Stable per-root grouping id (sanitized basename + path hash).
   Groups the workspaces of one root together in listings; no git involved."
  [root]
  (let [root
        (file-path root)

        name
        (sanitize-id (.getName (io/file root)))

        hash
        (Long/toUnsignedString (Integer/toUnsignedLong (hash root)) 36)]

    (str name "-" hash)))

(defn- git*
  "Run `git <args>` inside `dir` and return `{:exit :out}` — `:exit` is nil when
   git could not be spawned or timed out. `env` entries are added to the child's
   environment (used to point git at a PRIVATE index). Deliberately LOCAL:
   `internal.git` already requires this namespace, so requiring it back would
   close a load cycle — but it keeps `internal.git/git-argv`'s contract:
   `--no-optional-locks`, so a workspace read never takes `.git/index.lock`
   away from a concurrent git."
  ([^File dir args] (git* dir args nil nil))
  ([^File dir args env] (git* dir args env nil))
  ([^File dir args env input]
   (try (let [pb
              (doto (ProcessBuilder. ^java.util.List
                                     (into ["git" "--no-optional-locks"] (map str) args))
                (.directory dir)
                (.redirectErrorStream true))

              _
              (doseq [[k v] env]
                (if (nil? v)
                  (.remove (.environment pb) (str k))
                  (.put (.environment pb) (str k) (str v))))

              p
              (.start pb)

              _
              (with-open [stream (.getOutputStream p)]
                (when input
                  (.write stream
                          (.getBytes ^String input java.nio.charset.StandardCharsets/UTF_8))))

              out
              (slurp (.getInputStream p))

              done
              (.waitFor p 120 java.util.concurrent.TimeUnit/SECONDS)]

          (when-not done (.destroyForcibly p))
          {:exit (when done (.exitValue p)) :out out})
        (catch Throwable t
          (cancellation/preserve-interrupt! t)
          {:exit nil :out (str (ex-message t))}))))

(def ^:private workspace-files-ttl-ms
  "How long one listing of a root answers later callers. The language guards of a
   session list the same root at the start of the same block, so they share one."
  2000)

(defonce ^:private workspace-file-listings
  ;; canonical root -> {:at epoch-ms :files (delay [absolute-path ...] or nil)}
  (atom {}))

(defn- listed-files
  "Absolute paths of the files that git sees under `root`, or nil without git."
  [^String root]
  (let [{:keys [exit out]} (git* (io/file root)
                                 ["ls-files" "-z" "--cached" "--others" "--exclude-standard"])]
    (when (= 0 exit)
      ;; git* merges stderr into stdout; a warning line never is a NUL-separated name.
      (into []
            (comp (remove str/blank?)
                  (remove #(str/includes? % "\n"))
                  (map #(str root File/separator %)))
            (str/split (str out) #"\u0000")))))

(defn workspace-files
  "Absolute paths of the files under `root` that git sees: tracked or untracked,
   never ignored. nil when `root` is not in a Git working tree or git fails.

   One listing answers every caller of the same root for `workspace-files-ttl-ms`,
   and concurrent callers wait for the same listing. This is the one place where
   extensions list a workspace, instead of a child process for each extension."
  [root]
  (let [root
        (.getAbsolutePath (io/file (str root)))

        now
        (util/now-ms)

        fresh?
        #(and % (< (- now (long (:at %))) (long workspace-files-ttl-ms)))

        listings
        (swap! workspace-file-listings (fn [m]
                                         (if (fresh? (clojure.core/get m root))
                                           m
                                           (-> (into {} (filter (comp fresh? val)) m)
                                               (assoc root {:at now
                                                            :files (delay (listed-files
                                                                            root))})))))]

    @(:files (clojure.core/get listings root))))

;; Lookup

(defn get
  "Return the workspace with `workspace-id`, or nil."
  [db-info workspace-id]
  (p/db-workspace-get db-info workspace-id))

(defn list-active
  "Active workspaces for `repo-id`, newest first."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:active}))

(defn list-finished
  "Discarded workspaces for `repo-id`, newest first."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:discarded}))

(defn for-session
  "Workspace pinned to `session-state-id`, or nil."
  [db-info session-state-id]
  (p/db-workspace-for-session db-info session-state-id))

;; Label + focus + hydration

(defn set-label!
  "Set the workspace's human-friendly `:label`. Empty/nil clears it."
  [db-info {:keys [workspace-id label]}]
  (let [trimmed (some-> label
                        str
                        str/trim
                        not-empty)]
    (p/db-workspace-update-label! db-info workspace-id trimmed)))

(defn subdirs
  "Child directory names (non-hidden) of `path`, case-insensitively sorted.
   Empty vec when `path` is blank, not a directory, or unreadable."
  [path]
  (let [dir (some-> path
                    normalize-root
                    io/file)]
    (->> (when (and dir (.isDirectory ^File dir)) (.listFiles ^File dir))
         (filter (fn [^File f]
                   (and (.isDirectory f) (not (.isHidden f)))))
         (map (fn [^File f]
                (.getName f)))
         (sort String/CASE_INSENSITIVE_ORDER)
         vec)))

(defn create-dir!
  "Create a single child directory `name` under existing directory `parent`.
   Returns the canonical path of the (possibly already-existing) child. Throws
   when `parent` is not a directory or `name` is not a single safe path segment.
   `name` may not contain a separator, be blank, or be `.`/`..`."
  [parent name]
  (let [base
        (some-> parent
                normalize-root
                io/file)

        seg
        (some-> name
                str
                str/trim)]

    (cond (or (nil? base) (not (.isDirectory ^File base)))
          (throw (ex-info (str "Not a directory: " parent)
                          {:type :workspace/not-a-directory :path parent}))
          (or (str/blank? seg)
              (= seg ".")
              (= seg "..")
              (str/includes? seg "/")
              (str/includes? seg "\\"))
          (throw (ex-info (str "Invalid folder name: " name)
                          {:type :workspace/invalid-name :name name}))
          :else (let [child (io/file base seg)]
                  (when (and (not (.exists child)) (not (.mkdir child)))
                    (throw (ex-info (str "Could not create folder: " seg)
                                    {:type :workspace/mkdir-failed :name name})))
                  (.getCanonicalPath child)))))

(defn focus!
  "Stamp `last_focused_at_ms` and upsert the per-repo `repo_focus`
   pointer. Returns the updated workspace record."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-touch-focus! db-info workspace-id)]
    (when (:repo-id ws) (p/db-repo-focus-set! db-info (:repo-id ws) workspace-id))
    ws))

(defn last-focused
  "Workspace id from `repo_focus` for `repo-id`, or nil."
  [db-info repo-id]
  (some-> (p/db-repo-focus-get db-info repo-id)
          :workspace-id))

(defn display-label
  "Human-facing label for `workspace`. Order: explicit `:label` →
   pinned session title → id prefix."
  ([workspace] (display-label nil workspace nil))
  ([db-info workspace session]
   (let [hydrated (or session
                      (when (and db-info (:id workspace))
                        (some->> (:id workspace)
                                 (p/db-session-state-list-for-workspace db-info)
                                 first)))]
     (or (some-> (:label workspace)
                 str/trim
                 not-empty)
         (some-> hydrated
                 :title
                 str/trim
                 not-empty)
         (some-> (:id workspace)
                 str
                 (subs 0 (min 8 (count (str (:id workspace))))))))))

(defn workspace-with-session
  "Hydrate `workspace-id` with its pinned `session_state`. Returns
   `{:workspace <ws> :session-state <ss>}`."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-get db-info workspace-id)]
    {:workspace ws
     :session-state (some->> workspace-id
                             (p/db-session-state-list-for-workspace db-info)
                             first)}))

(defn list-active-with-sessions
  "Like `list-active` but each entry is the `{:workspace :session-state}`
   pair, sorted by `last_focused_at_ms` DESC NULLS LAST, then
   `created_at` DESC."
  [db-info repo-id]
  (let [rows
        (list-active db-info repo-id)

        cmp
        (fn [a b]
          (let [recency-of
                #(or (:last-focused-at-ms %) Long/MIN_VALUE)

                ra
                (recency-of a)

                rb
                (recency-of b)]

            (cond (not= ra rb) (compare rb ra)
                  :else (compare (str (:created-at b)) (str (:created-at a))))))]

    (mapv (fn [ws]
            (let [pair (workspace-with-session db-info (:id ws))]
              (assoc pair :workspace ws)))
          (sort cmp rows))))

;; Mutations

(defn- insert-trunk!
  "Insert a fresh workspace row at `root` (default: the bound project folder, then
   the real cwd) and pin it to `session-state-id` when given."
  ([db-info session-state-id] (insert-trunk! db-info session-state-id (cwd-root)))
  ([db-info session-state-id root]
   (let [root
         (normalize-root root)

         ws
         (p/db-workspace-insert! db-info {:repo-id (repo-id-for root) :root root :state :active})]

     (when session-state-id (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
     ws)))

(defn ensure-workspace!
  "Find-or-create the session's workspace. The DEFAULT is the bound project
   folder (`*workspace-root*`), else the user's real cwd.
   Resume returns whatever workspace the session was pinned to. Idempotent per
   session-state."
  [db-info {:keys [session-state-id]}]
  (or (for-session db-info session-state-id) (insert-trunk! db-info session-state-id)))

(defn create-trunk-at!
  "Mint an unpinned workspace at `root`. Channels may pass a directory other than
   the launch directory; returns the workspace row whose `:id` is passed as
   `:workspace-id` at session creation."
  [db-info root]
  (insert-trunk! db-info nil root))

(defonce ^:private root-changes
  ;; How many times a session root moved in this process. A reader that groups
  ;; sessions by root, such as the gateway's project overview, keys its cache on it.
  (atom 0))

(defn root-change-count
  "How many session roots moved in this process. Include it in a cache key that
   depends on session roots."
  []
  @root-changes)

(defn change-root!
  "Repoint `session-state-id`'s primary workspace root to `path`. Relative paths
   resolve from the session's current root."
  [db-info session-state-id path]
  (let [current
        (for-session db-info session-state-id)

        input
        (some-> path
                str
                str/trim
                not-empty
                paths/expand-home)

        file
        (some-> input
                io/file)]

    (when-not file (throw (ex-info "Path is blank" {:type :workspace/blank-path :path path})))
    (when (and (not (.isAbsolute ^File file)) (nil? (:root current)))
      (throw (ex-info "Session workspace is unavailable"
                      {:type :workspace/no-root :session-state-id session-state-id})))
    (let [canon
          (normalize-root (if (.isAbsolute ^File file) file (io/file (:root current) file)))

          dir
          (io/file canon)]

      (when-not (.isDirectory ^File dir)
        (throw (ex-info (str "Not a directory: " path)
                        {:type :workspace/not-a-directory :path path})))
      (if (= canon (normalize-root (:root current)))
        current
        (let [ws (insert-trunk! db-info session-state-id canon)]
          (swap! root-changes inc)
          ws)))))

(defn- refile-session-project!
  "Put session `session-id` into the project bound to the root of `ws`. A session
   in no project stays in no project. With no project bound to the new root, the
   session leaves its project. A move out of a project also leaves its group."
  [db-info session-id ws]
  (when-let [session (p/db-get-session db-info session-id)]
    (when-let [current (:project-id session)]
      (let [target (some->> (:root ws)
                            (p/db-get-project-by-root db-info (or (:owner-id session) "local"))
                            :id)]
        (when (not= (str current) (str target))
          (p/db-set-session-project! db-info session-id target))))))

(defn change-session-root!
  "Repoint session `session-id` at `path` with `change-root!`, then keep its project
   with its root. Clients list a session under the project of its root, so a session
   filed in the old project and its group was in no list."
  [db-info session-id session-state-id path]
  (let [before
        (for-session db-info session-state-id)

        ws
        (change-root! db-info session-state-id path)]

    (when (and session-id (not= (:id before) (:id ws)))
      (refile-session-project! db-info session-id ws))
    ws))
