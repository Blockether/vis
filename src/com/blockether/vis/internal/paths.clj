(ns com.blockether.vis.internal.paths
  "Cross-platform path helpers. A LEAF namespace (no project deps) so any
   layer — core, extensions, tests — can normalize without a require cycle."
  (:import [java.io File]
           [java.nio.channels FileChannel FileLock]
           [java.nio.file Files OpenOption Path Paths StandardOpenOption]
           [java.time Instant LocalDate ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.util Locale]))

(defn unixify
  "Normalize a path string to `/` separators on every OS. Java's `File`/`Path`
   APIs can hand back platform-native separators — so this is the single
   canonical normalizer.

   Use it ONLY where a path is DATA: compared, glob-matched, shown to the model,
   or embedded in a URL / wire / DB. NEVER for real filesystem I/O — `io/file`,
   `.exists`, JGit, nio all take native paths fine. Returns nil for nil input."
  ^String [s]
  (when s (.replace (str s) "\\" "/")))

(defn expand-home
  "Expand a leading `~` path segment to the user's home directory for filesystem
   I/O. Bare `~` becomes home; `~/…` and `~\\…` use native separators; `~user`,
   mid-path tildes, and ordinary paths pass through unchanged. Nil-safe and a
   no-op when home is unavailable."
  (^String [path] (expand-home path (System/getProperty "user.home")))
  (^String [path home]
   (let [^String path
         (some-> path
                 str)

         ^String home
         (some-> home
                 str
                 not-empty)]

     (cond (nil? path) nil
           (nil? home) path
           (= path "~") home
           (or (.startsWith path "~/") (.startsWith path "~\\"))
           (.getPath (java.io.File. home (subs path 2)))
           :else path))))

(defn abbreviate-home
  "Shorten an absolute path for DISPLAY by replacing the user's home dir with
   `~`, matching the footer/navigator/dialogs. Only rewrites when `path` is at
   or under home (so `/etc/x` and relative paths stay unchanged). Rendered
   descendants always use `/` separators; nil-safe."
  (^String [path] (abbreviate-home path (System/getProperty "user.home")))
  (^String [path home]
   (let [path
         (some-> path
                 str)

         home
         (some-> home
                 str
                 not-empty)]

     (if-not (and path home)
       path
       (try (let [^Path raw-path
                  (Paths/get path (make-array String 0))

                  ^Path normalized-path
                  (.normalize (.toAbsolutePath raw-path))

                  ^Path normalized-home
                  (.normalize (.toAbsolutePath (Paths/get home (make-array String 0))))]

              (cond (not (.isAbsolute raw-path)) path
                    (= normalized-path normalized-home) "~/"
                    (.startsWith normalized-path normalized-home)
                    (str "~/" (unixify (.toString (.relativize normalized-home normalized-path))))
                    :else path))
            (catch Throwable _ path))))))

(defn logs-dir
  "Root for diagnostic logs and reports: `~/.vis/logs`. Writers use UTC date
   directories below it. This dedicated root is accessible to the file tools and
   sandbox without exposing configuration, session databases or gateway tokens."
  ^String []
  (str (System/getProperty "user.home") "/.vis/logs"))

(defn ensure-logs-dir!
  "Create `~/.vis/logs` (and parents) when absent; return its path string.
   Never throws."
  ^String []
  (let [d (logs-dir)]
    (try (.mkdirs (java.io.File. d)) (catch Throwable _ nil))
    d))

(defn log-date-dir
  "Diagnostic directory for an instant's UTC date: `~/.vis/logs/YYYY-MM-DD`.
   Defaults to now; does not create directories. Long-lived writers retain the
   path chosen at startup rather than switching files at midnight."
  (^String [] (log-date-dir (Instant/now)))
  (^String [^Instant instant]
   (str (logs-dir)
        "/"
        (.format DateTimeFormatter/ISO_LOCAL_DATE (.atOffset instant ZoneOffset/UTC)))))

(defn ensure-log-date-dir!
  "Create the UTC date directory and return its path. Filesystem errors propagate."
  (^String [] (ensure-log-date-dir! (Instant/now)))
  (^String [^Instant instant]
   (let [dir (log-date-dir instant)]
     (Files/createDirectories (.toPath (File. dir))
                              (make-array java.nio.file.attribute.FileAttribute 0))
     dir)))

(defn log-date-dirs
  "Existing UTC date directories, newest first. Ignores files, invalid dates and
   symlinks; retention and session log lookup share this directory boundary."
  []
  (->> (.listFiles (File. (logs-dir)))
       (filter (fn [^File dir]
                 (and (.isDirectory dir)
                      (not (Files/isSymbolicLink (.toPath dir)))
                      (re-matches #"\d{4}-\d{2}-\d{2}" (.getName dir))
                      (try (LocalDate/parse (.getName dir)) (catch DateTimeParseException _ nil)))))
       (sort-by #(.getName ^File %) #(compare %2 %1))))

(defn sandbox-defs-dir
  "Directory for persisted Python sandbox helper definitions — `~/.vis/sandbox`.
   Its own subdir (not `~/.vis`) for the same reason as `logs-dir`: nothing here
   needs to sit beside the session DB or the gateway token."
  ^String []
  (str (System/getProperty "user.home") "/.vis/sandbox"))

(defn sandbox-defs-file
  "File holding ONE session's persisted sandbox helper definitions —
   `~/.vis/sandbox/<session-id>.py`. The sandbox dies with the process, so this
   is what re-creates a session's own `def`s in a fresh one. The id is reduced
   to a safe file name; every other character becomes `_`."
  ^String [session-id]
  (str (sandbox-defs-dir) "/" (.replaceAll (str session-id) "[^A-Za-z0-9_.-]" "_") ".py"))

(defn process-id
  "This JVM's OS process id. Read fresh so native-image never bakes the builder's
   pid into the installed binary."
  ^long []
  (.pid (java.lang.ProcessHandle/current)))

(defonce ^:private lock-holders
  ;; Files whose OS locks this process depends on, by owner: `{owner #{path}}`,
  ;; canonical path strings. See `held-file?` for why they are off limits.
  (atom {}))

(defn- canonical-path
  "Canonical path string of `f` (a `File`, `Path` or string); its absolute path when
   the file system cannot canonicalize it."
  ^String [f]
  (let [file (if (instance? File f) ^File f (File. (str f)))]
    (try (.getCanonicalPath file) (catch java.io.IOException _ (.getAbsolutePath file)))))

(defn hold-files!
  "Record that `owner` holds OS locks on `files` (they need not exist yet), so the
   rest of this process leaves them alone (see `held-file?`). Replaces what
   `owner` held before. Answers nil."
  [owner files]
  (swap! lock-holders assoc owner (into #{} (map canonical-path) files))
  nil)

(defn release-held-files!
  "Forget `owner`'s files once it holds no more locks on them. Answers nil."
  [owner]
  (swap! lock-holders dissoc owner)
  nil)

(defn held-files
  "Canonical path strings of every file this process holds OS locks on."
  []
  (into #{} cat (vals @lock-holders)))

(defn held-file?
  "True when `f` names a file this process holds OS locks on, such as a live SQLite
   database with its `-wal` and `-shm` companions. POSIX record locks belong to the
   PROCESS, not to a descriptor: closing ANY descriptor of the file drops every
   lock the process holds on it. One read of a live `vis.db-shm` by a file tool
   therefore releases SQLite's wal-index locks; the next process to open the
   database then truncates the `-shm` this process still has mapped, and its next
   write dies with SIGBUS. Code that opens user-chosen files must refuse or skip
   held ones."
  [f]
  (let [held (held-files)]
    (and (boolean (seq held)) (contains? held (canonical-path f)))))

(defonce ^:private held-claims
  ;; Claims THIS process holds, by absolute path: `{path [channel lock]}`. The
  ;; channel stays open deliberately — the OS drops its lock when the process
  ;; dies, including a kill that runs no shutdown hook, and that is what makes a
  ;; claim a liveness signal instead of one more stale marker file.
  (atom {}))

(def ^:private claim-file-name
  "Lock token inside a claimed directory. It carries no content: holding the lock
   is the whole signal."
  ".vis-live")

(defn- claim-file ^File [^File dir] (File. dir ^String claim-file-name))

(defn claim-dir!
  "Claim `dir` as in use by this process for as long as it runs, so cleanup in
   another process can tell a directory that is still served from one a dead
   process left behind. Idempotent per directory and never throws: a directory
   that cannot be claimed is simply left to be judged by age. Answers `dir`."
  ^File [^File dir]
  (let [path (.getAbsolutePath dir)]
    (when-not (contains? @held-claims path)
      (try (let [channel (FileChannel/open (.toPath (claim-file dir))
                                           (into-array OpenOption
                                                       [StandardOpenOption/CREATE
                                                        StandardOpenOption/READ
                                                        StandardOpenOption/WRITE]))]
             (if-let [^FileLock lock (.tryLock channel 0 Long/MAX_VALUE true)]
               (do (swap! held-claims assoc path [channel lock])
                   (hold-files! [::claim path] [(claim-file dir)]))
               (.close channel)))
           (catch Throwable _ nil)))
    dir))

(defn claim-state
  "How `dir`'s in-use claim stands right now: `:held` while some process still
   serves it, `:free` once every claimant is gone, and `:none` for a directory
   that carries no claim at all — one written before claims existed, or one
   nobody ever used. Never throws; a claim that cannot be tested reads as
   `:held`, because being wrong about a live directory deletes work in use."
  [^File dir]
  (let [file (claim-file dir)]
    (cond (contains? @held-claims (.getAbsolutePath dir)) :held
          (not (.isFile file)) :none
          :else (try (with-open [channel (FileChannel/open (.toPath file)
                                                           (into-array OpenOption
                                                                       [StandardOpenOption/READ
                                                                        StandardOpenOption/WRITE]))]
                       (if-let [^FileLock lock (.tryLock channel 0 Long/MAX_VALUE false)]
                         (do (.release lock) :free)
                         :held))
                     (catch Throwable _ :held)))))

(def ^:private process-roles
  "What a vis PROCESS may call itself: the TUI, the gateway daemon, or the
   short-lived CLI."
  #{"gateway" "tui" "vis"})

(def ^:private log-roles "Every vis writer whose diagnostics land in `~/.vis/logs`." process-roles)

(def ^:private process-start-time
  ;; Delay the clock read so native-image does not bake in its builder's date.
  (delay (Instant/now)))

(defn set-log-role!
  "Set this process's diagnostic role before its first log path is opened.
   Accepted roles are `tui`, `gateway`, and `vis` (short-lived CLI work)."
  [role]
  (let [role (name role)]
    (when-not (contains? process-roles role)
      (throw (ex-info (str "unknown log role: " role) {:role role :allowed process-roles})))
    (System/setProperty "vis.log.role" role)
    role))

(defn- current-log-role
  []
  (let [role (System/getProperty "vis.log.role")]
    (if (contains? process-roles role) role "vis")))

(defn log-file
  "Diagnostic log file for this process. The name carries its role, UTC start
   time, and pid: `~/.vis/logs/YYYY-MM-DD/<role>-<yyyyMMddTHHmmssZ>-pid<pid>.log`.

   TUI and gateway are separate writers because Telemere rotates by renaming its
   file; sharing a path lets the non-rotating process keep writing to an orphaned
   descriptor. Embedded Python belongs to the gateway stream rather than a third
   file. The active `.log` remains tail-able and Telemere gzip-compresses rotated
   parts; housekeeping removes stale generations by age."
  (^String [] (log-file (current-log-role)))
  (^String [role]
   (let [role (name role)]
     (when-not (contains? log-roles role)
       (throw (ex-info (str "unknown log role: " role) {:role role :allowed log-roles})))
     (let [instant @process-start-time
           stamp (.format (.withZone (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'"
                                                                  Locale/ROOT)
                                     ZoneOffset/UTC)
                          instant)]

       (str (ensure-log-date-dir! instant) "/" role "-" stamp "-pid" (process-id) ".log")))))
