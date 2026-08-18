(ns com.blockether.vis.internal.paths
  "Cross-platform path helpers. A LEAF namespace (no project deps) so any
   layer — core, extensions, tests — can normalize without a require cycle."
  (:import [java.nio.file Path Paths]))

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
  "Directory for vis diagnostic logs — `~/.vis/logs`. A DEDICATED subdir (not
   `~/.vis` itself) so the native file tools and the Python sandbox can be
   granted always-on access to logs without exposing `config.edn`, the session
   DB, or gateway tokens. Returns the path string (native separators are fine
   for real I/O)."
  ^String []
  (str (System/getProperty "user.home") "/.vis/logs"))

(defn ensure-logs-dir!
  "Create `~/.vis/logs` (and parents) when absent; return its path string.
   Never throws."
  ^String []
  (let [d (logs-dir)]
    (try (.mkdirs (java.io.File. d)) (catch Throwable _ nil))
    d))

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
  "This JVM's OS process id. Its only job is to make `log-file` unique per
   process, so it is read fresh (never a load-time `def`): `native-image`
   would otherwise bake the BUILDER's pid into the image."
  ^long []
  (.pid (java.lang.ProcessHandle/current)))

(defn log-file
  "Diagnostic log file for THIS process — `~/.vis/logs/vis-<pid>.log`, or
   `~/.vis/logs/<prefix>-<pid>.log` for a second sink in the same process.

   ONE WRITER PER FILE, by construction. Telemere's rolling file handler owns
   the file it writes and rotates by RENAMING it; with several vis processes
   (TUI, gateway daemon, CLI) on a single shared path, whichever one rotates
   pulls the file out from under the others, and they go on appending into a
   deleted inode — observed as a gateway writing 13.8 MB of stream traces into
   a file nobody could read. Stamping the pid into the name makes that
   impossible instead of merely unlikely.

   Two sinks in the same process must NOT share one name either (a raw
   `FileOutputStream` survives the rename with a stale fd), hence `prefix` —
   GraalPy's polyglot log takes its own.

   Growth is bounded by `foundation.housekeeping/sweep-stale!`, which prunes
   `~/.vis/logs` by age — including the per-command `shell` subdirectories; the
   rolling handler bounds each live file."
  (^String [] (log-file "vis"))
  (^String [prefix] (str (ensure-logs-dir!) "/" prefix "-" (process-id) ".log")))
