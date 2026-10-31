(ns com.blockether.vis.internal.paths
  "Cross-platform path helpers. A LEAF namespace (no project deps) so any
   layer — core, extensions, tests — can normalize without a require cycle."
  (:require [clojure.string :as str])
  (:import [java.io File IOException]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel FileLock]
           [java.nio.charset StandardCharsets]
           [java.nio.file AccessDeniedException AtomicMoveNotSupportedException CopyOption
            FileAlreadyExistsException Files InvalidPathException LinkOption OpenOption Path Paths
            StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermission PosixFilePermissions]
           [java.time Instant LocalDate ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.util Locale]
           [java.util.regex Pattern]))

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

(defn compact-path-text
  "Shorten absolute path prefixes inside DISPLAY text: `root` (and its `~/` spelling)
   becomes relative and the user's home dir becomes `~`, the spelling the footer,
   navigator and dialogs use. Embedded URL paths and sibling names that merely START
   with a prefix stay unchanged. A nil `root` shortens home alone; non-string `text`
   passes through."
  ([root text] (compact-path-text root text (System/getProperty "user.home")))
  ([root text home]
   (if-not (string? text)
     text
     (reduce (fn [text [prefix replacement]]
               (if (str/blank? prefix)
                 text
                 (str/replace
                   text
                   (re-pattern
                     (str "(?<![\\p{L}\\p{N}_./\\\\:~%-])"
                          (str/join "[/\\\\]"
                                    (map #(Pattern/quote %) (str/split (unixify prefix) #"/")))
                          "(?:([/\\\\])(?=[^\\s\"'`),;\\]}])|[/\\\\]?(?=$|[\\s\"'`),;\\]}]))"))
                   (fn [[_ separator]]
                     (if separator (if (= replacement ".") "" "~/") replacement)))))
             text
             (distinct [[root "."] [(when root (abbreviate-home root home)) "."] [home "~"]])))))

(defn shell-path
  "Render `path` for DISPLAY as one copyable POSIX shell word. A path at or under
   home reads `~/…` with `~/` left unquoted, so the shell still expands it; a
   remainder holding anything beyond `[A-Za-z0-9_@%+=:,./-]` is single-quoted.
   Nil-safe."
  (^String [path] (shell-path path (System/getProperty "user.home")))
  (^String [path home]
   (when-let [display (abbreviate-home path home)]
     (let [tilde? (str/starts-with? display "~/")
           word (if tilde? (subs display 2) display)]

       (str (when tilde? "~/")
            (if (re-matches #"[A-Za-z0-9_@%+=:,./-]*" word)
              word
              (str "'" (str/replace word "'" "'\\''") "'")))))))

(defn ancestor-chain
  "`dir` and each parent up to the filesystem root, nearest first, as a lazy seq.
   `dir` is a `File` or a `Path`, and each item has the same type. Nil gives an empty
   seq. Make `dir` absolute or canonical first, because a relative path stops at its
   first segment."
  [dir]
  (if (instance? Path dir)
    (take-while some?
                (iterate (fn [^Path p]
                           (.getParent p))
                         dir))
    (take-while some?
                (iterate (fn [^File f]
                           (.getParentFile f))
                         dir))))

(defn nearest-ancestor
  "The first item of [[ancestor-chain]] of `dir` for which `pred` is true, or nil.
   For example, `(nearest-ancestor #(.exists (File. ^File % \".git\")) dir)`."
  [pred dir]
  (some #(when (pred %) %) (ancestor-chain dir)))

(defn runtime-dir-setting
  "The task runtime directory: JVM property `vis.runtime.dir`, then `VIS_RUNTIME_DIR`.
   Empty is unset. A set value holds worker control endpoints and temporary files (#423)."
  []
  (or (not-empty (System/getProperty "vis.runtime.dir"))
      (not-empty (System/getenv "VIS_RUNTIME_DIR"))))

(defn- refuse-runtime-dir!
  [setting reason]
  (throw (ex-info (str "VIS_RUNTIME_DIR " (pr-str (str setting)) " " reason ".")
                  {:type :vis/runtime-dir :runtime-dir (str setting) :reason reason})))

(defn- shared-permissions
  "The group and other permissions of `path`, without following a symbolic link."
  [^Path path]
  (filter #{PosixFilePermission/GROUP_READ PosixFilePermission/GROUP_WRITE
            PosixFilePermission/GROUP_EXECUTE PosixFilePermission/OTHERS_READ
            PosixFilePermission/OTHERS_WRITE PosixFilePermission/OTHERS_EXECUTE}
          (Files/getPosixFilePermissions path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))))

(defn runtime-dir
  "The checked task runtime directory, or nil when it is unset. A set value fails
   closed and never falls back to `/tmp`: it must be an existing absolute directory
   without symbolic links that only its owner can write. A refusal is
   `:vis/runtime-dir` and names its reason."
  ^Path []
  (when-let [setting (runtime-dir-setting)]
    (let [^Path path (try (Path/of ^String setting (make-array String 0))
                          (catch InvalidPathException _
                            (refuse-runtime-dir! setting "is not a path")))]
      (when-not (.isAbsolute path) (refuse-runtime-dir! setting "is not an absolute path"))
      (let [^Path real (try (.toRealPath path (make-array LinkOption 0))
                            (catch IOException _ (refuse-runtime-dir! setting "does not exist")))]
        (when-not (= real (.normalize path))
          (refuse-runtime-dir! setting (str "contains a symbolic link; use " real)))
        (when-not (Files/isDirectory real (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
          (refuse-runtime-dir! setting "is not a directory"))
        (when (some #{PosixFilePermission/GROUP_WRITE PosixFilePermission/OTHERS_WRITE}
                    (shared-permissions real))
          (refuse-runtime-dir! setting "is writable by other users"))
        real))))

(defn install-temp-dir!
  "Point `java.io.tmpdir` at the private `tmp` directory in the task runtime
   directory, and return it. Return nil and change nothing when the setting is
   unset. Run it at startup: the one-argument `createTempFile` forms keep the
   startup value, so their callers pass [[temp-dir]]. A second run keeps the same
   directory."
  ^Path []
  (when-let [^Path root (runtime-dir)]
    (let [dir (.resolve root "tmp")
          no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])]

      (try (Files/createDirectory dir
                                  (into-array FileAttribute
                                              [(PosixFilePermissions/asFileAttribute
                                                 (PosixFilePermissions/fromString "rwx------"))]))
           (catch FileAlreadyExistsException _ nil))
      (when-not (and (Files/isDirectory dir no-follow)
                     (= (Files/getOwner dir no-follow) (Files/getOwner root no-follow))
                     (empty? (shared-permissions dir)))
        (refuse-runtime-dir! root
                             (str "has a " dir " that is not a private directory of its owner")))
      (System/setProperty "java.io.tmpdir" (str dir))
      dir)))

(defn temp-dir
  "The directory for temporary files: `java.io.tmpdir`, which [[install-temp-dir!]]
   moves into the task runtime directory. Give it to every `createTempFile` and
   `createTempDirectory` call."
  ^File []
  (File. (System/getProperty "java.io.tmpdir")))

(defn temp-env
  "The `TMPDIR` of a child process when the task runtime directory is set, else an
   empty map, so the child keeps its inherited value."
  []
  (if (runtime-dir-setting) {"TMPDIR" (str (temp-dir))} {}))

(defn logs-dir
  "Root for diagnostic logs and reports: the `vis.logs.dir` system property, else
   `~/.vis/logs`. Writers use UTC date directories below it. This dedicated root
   is accessible to the file tools and sandbox without exposing configuration,
   session databases or gateway tokens. The `:test` alias points it at `target/`."
  ^String []
  (or (not-empty (System/getProperty "vis.logs.dir"))
      (str (System/getProperty "user.home") "/.vis/logs")))

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

(defn sandbox-spill-dir
  "Directory for ONE session's spilled sandbox values — `~/.vis/sandbox/<session-id>.spill`.
   It holds large text and bytes values that the snapshot budget left out, beside
   [[sandbox-defs-file]]. The id is reduced to a safe file name like there."
  ^String [session-id]
  (str (sandbox-defs-dir) "/" (.replaceAll (str session-id) "[^A-Za-z0-9_.-]" "_") ".spill"))

(defn- windows?
  "True on Windows. Read at run time: a native image must not bake the build host in."
  []
  (str/starts-with? (System/getProperty "os.name" "") "Windows"))

(defn- replace-file!
  "Move `tmp` onto `target` in one atomic step. On Windows, a reader that holds
   `target` open makes the move fail for a short time, so retry it a few times.
   Never fall back to a write in place."
  [^Path tmp ^Path target]
  (loop [attempt 1]
    (let [moved?
          (try (try (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
                    (catch AtomicMoveNotSupportedException _
                      (Files/move tmp
                                  target
                                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
               true
               (catch AccessDeniedException e
                 (if (and (windows?) (< attempt 10)) false (throw e))))]
      (when-not moved? (Thread/sleep (* 20 attempt)) (recur (inc attempt))))))

(defn- force-dir!
  "Force the entry of a replaced file in `dir` to the disk, so a power loss cannot
   bring back the old file. POSIX only. A failure is ignored: the content is
   already safe."
  [^Path dir]
  (when-not (windows?)
    (try (with-open [ch (FileChannel/open dir (into-array OpenOption [StandardOpenOption/READ]))]
           (.force ch true))
         (catch Exception _ nil))))

(defn spit-private!
  "Write `content` to `path` as an owner-only (600) file, ATOMICALLY. The ONE
   writer for every file that holds a secret: `~/.vis/state.yml` and each OAuth
   credential store.

     - The bytes are never even briefly world-readable: the temporary file is
       CREATED with the restrictive mode (create-with-attribute, never
       write-then-chmod).
     - No reader ever observes a missing or half-written file, and an interrupted
       write never destroys the previous one. The content lands in a sibling
       temporary file, is forced to the disk and is then MOVED onto `path`, so the
       file that was there survives every failure before the move.
     - A `path` that is a symbolic link keeps its link: the content replaces the
       file that the link names.

   Falls back to a creation without the attribute, and to a replacing move, on a
   filesystem that supports neither. On Windows, the move is retried for a short
   time while another process holds the file open. Any other failure propagates, and the
   temporary file is removed. The parent directory must exist."
  [path ^String content]
  (let [^Path given
        (.toPath (File. (str path)))

        ^Path target
        (if (Files/isSymbolicLink given) (.toRealPath given (make-array LinkOption 0)) given)

        ^Path dir
        (or (.getParent target) (.toPath (File. ".")))

        ^Path tmp
        (.resolve dir (str "." (.getFileName target) ".tmp-" (java.util.UUID/randomUUID)))

        attr
        (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]

    (try (try (Files/createFile tmp (into-array FileAttribute [attr]))
              (catch UnsupportedOperationException _
                (Files/createFile tmp (make-array FileAttribute 0))))
         (with-open [ch (FileChannel/open tmp (into-array OpenOption [StandardOpenOption/WRITE]))]
           (let [buf (ByteBuffer/wrap (.getBytes content StandardCharsets/UTF_8))]
             (while (.hasRemaining buf) (.write ch buf)))
           (.force ch true))
         (replace-file! tmp target)
         (force-dir! dir)
         nil
         (finally (try (Files/deleteIfExists tmp) (catch Throwable _ nil))))))

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

(defonce ^:private claim-lock
  ;; ONE monitor for the claim decisions of this process. See `with-claim-lock`.
  (Object.))

(defn with-claim-lock
  "Call `f` while this process holds its claim lock, and answer what `f` answers.
   A sweep holds the lock while it judges and deletes a directory. A publisher
   holds it while it creates and claims a directory, before it fills it. So a
   sweep of this process deletes a directory before the claim or not at all. A
   sweep in another process can still judge a directory free before the claim."
  [f]
  (locking claim-lock (f)))

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

(defn process-role
  "This process's role: `gateway`, `tui` or `vis` (short-lived CLI work, the default)."
  ^String []
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
  (^String [] (log-file (process-role)))
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
