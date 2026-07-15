(ns com.blockether.vis.ext.language-clojure.repl-manager
  "Owned, session-scoped nREPL lifecycle for the Clojure pack.

   OWNERSHIP: each vis SESSION owns its own nREPL subprocess(es). The `processes`
   atom is keyed by `[session-id dir]`, so two sessions in the same directory get
   two independent REPLs and neither can see or stop the other's. A managed REPL
   lives and dies with THIS vis process — there is NO persistent registry and NO
   PID re-attach across a vis restart. Restarting vis means a fresh REPL, exactly
   like the Python pack.

   PORT: we PICK a free ephemeral port ourselves and pass it to the launcher
   EXPLICITLY (`nrepl.cmdline --port N`, `lein repl :headless :port N`,
   `bb nrepl-server N`), so we always KNOW our port without ever reading a
   `.nrepl-port` file back. Any stray `.nrepl-port` a tool drops in the project is
   deleted after boot — vis never depends on it and never leaves it behind.

   ALIASES: a REPL is ALWAYS booted with the project's `:dev :test` deps + paths
   on its classpath (full dependency spec), with the user's `:main-opts` dropped
   (our synthetic `:vis/nrepl-launch` alias appends last so `-m nrepl.cmdline`
   wins). Unknown `:dev`/`:test` aliases are silently ignored by tools.deps, so
   this is safe in any project.

   Starting/stopping is CORE and ALWAYS allowed — never gated behind a flag."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client])
  (:import (java.io RandomAccessFile)
           (java.net ServerSocket)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent TimeUnit)))

;; { [session-id dir] -> {:id :process ^Process :port int :cmd [..] :tool kw
;;                        :aliases [kw..] :pid long :dir str :started-at ms} }
;; defonce so a `(require :reload)` during dev never orphans a live child.
;; NO on-disk registry: a managed REPL is bound to THIS vis process + session.
(defonce ^:private processes (atom {}))

(defn- alive? [^Process p] (boolean (and p (.isAlive p))))

(defn- proc-alive? [info] (alive? (:process info)))

;; { [session-id dir] -> {"exit" int? "at" ms "log" path "log_tail" [lines]} }
;; Written when a managed launcher dies UNEXPECTEDLY (a startup failure or a
;; later crash) — never by an intentional `stop!` (it deregisters first). Read
;; by `health` (→ :failed) and `last-failure` so status/eval can surface the
;; REAL boot error. Cleared on a successful start and on `stop!`.
(defonce ^:private last-failures (atom {}))

;; ── Idle reaping ────────────────────────────────────────────────────────────
;; A managed REPL is a FULL project JVM (0.5–2 GB resident: the whole :dev:test
;; classpath + every loaded namespace). ONE is spawned per distinct working `dir`
;; an eval/test targets and — without this — lived for the ENTIRE session, so a
;; long agent run touching several monorepo subdirs piled up several idle GB of
;; heavyweight JVMs. Each REPL carries a `:last-touch` ms stamp, bumped on every
;; eval/test that targets it; a single daemon thread stops any REPL untouched for
;; `idle-reap-ms`. Reaping is transparent — the next eval autostarts a fresh REPL
;; on demand. Set VIS_CLJ_REPL_IDLE_MS=0 to disable (or to a custom ms budget).
(def ^:private idle-reap-ms
  (let [env (some-> (System/getenv "VIS_CLJ_REPL_IDLE_MS")
                    str/trim
                    not-empty)]
    (or (when env (try (Long/parseLong env) (catch Exception _ nil))) (* 20 60 1000))))

(def ^:private reaper-tick-ms 60000)

(defn- touch!
  "Stamp `[session-id dir]`'s REPL as used just now, so the idle reaper spares an
   actively-worked REPL. No-op when the session owns no REPL for `dir`."
  [session-id dir]
  (let [k [session-id dir]]
    (swap! processes (fn [m]
                       (cond-> m
                         (contains? m k)
                         (assoc-in [k :last-touch] (System/currentTimeMillis)))))))

(declare ensure-reaper!)

(defn last-failure
  "The last UNEXPECTED launcher death recorded for `[session-id dir]`, or nil.
   STRING-keyed (\"exit\" \"at\" \"log\" \"log_tail\") — safe to splice into
   model-facing results."
  [session-id dir]
  (get @last-failures [session-id dir]))

(defn- clear-failure! [session-id dir] (swap! last-failures dissoc [session-id dir]))

;; Kept in sync with the nrepl/nrepl version pinned in this extension's deps.edn.
;; Injected via `-Sdeps` so the launcher works even in target projects that don't
;; declare nREPL themselves.
(def nrepl-version "1.3.0")

;; How long `start!` waits for OUR port to answer. The deadline only matters
;; while the launcher is STILL ALIVE — a dead launcher short-circuits to a
;; :failed result in ≤ ~250ms via `wait-until-up` — so it can be generous
;; enough for a cold-cache deps resolve without making real failures slow.
(def ^:private start-deadline-ms 120000)

(def ^:private default-aliases
  "Every managed REPL boots with the project's dev + test deps/paths. Unknown
   aliases are silently ignored by tools.deps, so this is safe everywhere."
  [:dev :test])

(defn id-of "Stable session-resource id for the REPL rooted at `dir`." [dir] (str "nrepl:" dir))

(defn- as-keywords [aliases] (mapv #(if (keyword? %) % (keyword (name %))) aliases))

(defn- free-port!
  "Grab a free ephemeral TCP port from the OS, then release it so the launcher can
   bind it. A tiny race window (the port taken between close and bind) is
   acceptable — `start!` probes our OWN port and reports :starting/failure if it
   never comes up."
  []
  (with-open [s (ServerSocket. 0)]
    (.setReuseAddress s true)
    (.getLocalPort s)))

(defn- alias-suffix
  "deps.edn alias suffix, e.g. [:dev :test] -> \":dev:test\". nil when none."
  [aliases]
  (when (seq aliases) (apply str (map #(str ":" (name %)) aliases))))

(defn- read-deps-edn
  "Parse `dir`/deps.edn to an EDN map, or nil when absent/unreadable."
  [^java.io.File dir]
  (try (let [f (io/file dir "deps.edn")]
         (when (.isFile f) (edn/read-string (slurp f))))
       (catch Throwable _ nil)))

(defn- alias-jvm-opts
  "The `:jvm-opts` `aliases` declare in a parsed deps.edn map, concatenated in the
   order the aliases are given (nil-safe, distinct)."
  [deps aliases]
  (->> aliases
       (mapcat (fn [a]
                 (get-in deps [:aliases a :jvm-opts])))
       distinct
       vec))

(defn inherited-jvm-opts
  "JVM options a nested project should INHERIT from an ancestor deps.edn.

   The nREPL is launched with `-M:dev:test:vis/nrepl-launch`, so any `:jvm-opts`
   `dir`'s OWN deps.edn declares for those aliases already reach the JVM — in that
   case nothing is inherited (returns nil, keeping the top-level project unchanged).

   But a NESTED project whose deps.edn declares no such aliases (e.g. an extension
   with a bare `{:deps …}` map) would otherwise boot a BARE JVM — missing the
   workspace's flags (`--enable-native-access`, `--enable-preview`,
   `--sun-misc-unsafe-memory-access=allow`, …) that its code needs, so tests crash
   before they run. For that case we walk UP from `dir` to the nearest ancestor
   whose deps.edn declares `:jvm-opts` for `aliases` and return them, so the nested
   nREPL inherits the workspace's JVM options."
  [^java.io.File dir aliases]
  (when (seq aliases)
    (let [own (alias-jvm-opts (read-deps-edn dir) aliases)]
      (when (empty? own)
        (loop [d (.getParentFile (.getAbsoluteFile dir))]
          (when d
            (let [opts (alias-jvm-opts (read-deps-edn d) aliases)]
              (if (seq opts) opts (recur (.getParentFile d))))))))))

(defn launcher-for
  "Subprocess command to boot a project nREPL in `dir` on the EXPLICIT `port`,
   honouring `aliases` (deps.edn aliases / lein profiles). Returns
   `{:tool kw :cmd [strings]}` or nil when no known Clojure build file is present."
  [dir aliases port]
  (let [present? (fn [n]
                   (.isFile (io/file dir n)))]
    (cond (present? "deps.edn")
          ;; Inject nREPL + the `nrepl.cmdline` main via a synthetic alias we append
          ;; LAST. tools.deps resolves `:main-opts` last-alias-wins (while
          ;; `:extra-deps`/`:extra-paths` accumulate), so a user alias that carries
          ;; its OWN `:main-opts` still contributes its deps + source paths to the
          ;; classpath, but our `-m nrepl.cmdline --port N` is what actually runs. We
          ;; want the user aliases' deps/paths, never their main.
          (let [jvm (seq (inherited-jvm-opts (io/file dir) aliases))]
            {:tool :clj
             :cmd ["clojure" "-Sdeps"
                   (str "{:aliases {:vis/nrepl-launch "
                        "{:extra-deps {nrepl/nrepl {:mvn/version \""
                        nrepl-version
                        "\"}} "
                        ;; A NESTED project whose own deps.edn declares no :jvm-opts
                        ;; for the launch aliases inherits the workspace's — so its
                        ;; nREPL never boots a bare JVM missing --enable-native-access
                        ;; / --enable-preview / --sun-misc-unsafe-memory-access.
                        (when jvm (str ":jvm-opts " (pr-str (vec jvm)) " "))
                        ":main-opts [\"-m\" \"nrepl.cmdline\" \"--port\" \""
                        port
                        "\"]}}}") (str "-M" (alias-suffix aliases) ":vis/nrepl-launch")]})
          (present? "project.clj") {:tool :lein
                                    :cmd (if (seq aliases)
                                           ["lein" "with-profile"
                                            (str/join "," (map #(str "+" (name %)) aliases)) "repl"
                                            ":headless" ":port" (str port)]
                                           ["lein" "repl" ":headless" ":port" (str port)])}
          (present? "bb.edn") {:tool :bb :cmd ["bb" "nrepl-server" (str port)]}
          :else nil)))

(defn- log-file
  "Subprocess log path under `~/.vis/logs` (never inside the user's project
   tree, never the OS temp dir), namespaced by the target dir so each managed
   REPL gets its own log. The logs dir is created on demand."
  ^java.io.File [dir]
  (let [safe
        (-> (str dir)
            (str/replace #"[^A-Za-z0-9]+" "_")
            (str/replace #"(^_+|_+$)" ""))

        logs-dir
        (io/file (System/getProperty "user.home") ".vis" "logs")]

    (.mkdirs logs-dir)
    (io/file logs-dir (str "vis-nrepl-" safe ".log"))))

(def ^:private default-log-line-limit 500)

;; Read at most this many bytes off the END of a launcher log per tail. The log
;; captures the subprocess's FULL stdout and can grow to hundreds of MB over a
;; session — a tail must be O(tail), never O(file), or every log view hangs.
(def ^:private tail-read-bytes (* 256 1024))

(defn tail-log
  "Tail a managed nREPL launcher log as line strings, reading ONLY the last
   `tail-read-bytes` of the file (never the whole thing). Returns [] when the
   log does not exist yet or cannot be read; resource viewers treat that as an
   empty but still log-capable resource."
  ([log-path] (tail-log log-path default-log-line-limit))
  ([log-path n]
   (let [f
         (when (seq (str log-path)) (io/file (str log-path)))

         n
         (max 1 (long (or n default-log-line-limit)))]

     (if (and f (.isFile f))
       (try (with-open [raf (RandomAccessFile. f "r")]
              (let [len (.length raf)
                    start (max 0 (- len (long tail-read-bytes)))
                    size (int (- len start))]

                (if (zero? size)
                  []
                  (let [buf (byte-array size)]
                    (.seek raf start)
                    (.readFully raf buf)
                    (let [lines (vec (str/split-lines (String. buf StandardCharsets/UTF_8)))
                          ;; a mid-file start means the first line is partial — drop it
                          lines (if (and (pos? start) (seq lines)) (subvec lines 1) lines)
                          c (count lines)]

                      (if (> c n) (subvec lines (- c n)) lines))))))
            (catch Throwable _ []))
       []))))

(defn- record-failure!
  "Stamp the UNEXPECTED death of a managed launcher (exit code + log tail) so
   `health` keeps answering :failed and `last-failure` can explain WHY after
   the process is gone."
  [session-id dir ^Process proc log-path]
  (let [exit
        (try (.exitValue proc) (catch Throwable _ nil))

        tail
        (tail-log log-path 80)]

    (swap! last-failures assoc
      [session-id dir]
      (cond-> {"at" (System/currentTimeMillis)}
        exit
        (assoc "exit" exit)

        log-path
        (assoc "log" log-path)

        (seq tail)
        (assoc "log_tail" tail)))))

(defn- watch-process!
  "Attach an `.onExit` watcher to a freshly-launched REPL process. If it dies
   while STILL the registered process for `[session-id dir]` — i.e. not
   replaced and not intentionally stopped (`stop!` deregisters BEFORE
   destroying) — drop the entry and record the failure, so a boot that limps
   past `start!`'s deadline and THEN dies still flips to :failed instead of
   hanging in \"starting\" forever."
  [session-id dir ^Process proc log-path]
  (let [k [session-id dir]]
    (.thenAccept (.onExit proc)
                 (reify
                   java.util.function.Consumer
                     (accept [_ _]
                       (try (when (identical? proc (:process (get @processes k)))
                              (swap! processes dissoc k)
                              (record-failure! session-id dir proc log-path))
                            (catch Throwable _ nil)))))))

(defn- delete-stray-port-file!
  "The launcher may still drop a `.nrepl-port` in `dir` even though we passed the
   port explicitly. We never read it, so delete it so nothing downstream (or a
   human) mistakes it for the source of truth. Best-effort."
  [dir]
  (try (io/delete-file (io/file dir ".nrepl-port") true) (catch Throwable _ nil)))

(defn- wait-until-up
  "Poll our OWN chosen `port` until the nREPL accepts a describe round-trip, up
   to `deadline-ms`, while ALSO watching the launcher process itself. Returns
   :up (port answers), :died the moment `proc` exits before binding (a fast
   startup failure never burns the deadline), or :starting (deadline passed
   with the process still alive — a slow cold boot). `proc` may be nil (pure
   port probe)."
  [^Process proc port deadline-ms]
  (let [deadline (+ (System/currentTimeMillis) (long deadline-ms))]
    (loop []

      (let [st (:status (nrepl-client/probe! {:host "localhost" :port port :timeout-ms 500}))]
        (cond (= :up st) :up
              (and proc (not (.isAlive proc))) :died
              (< (System/currentTimeMillis) deadline) (do (Thread/sleep 250) (recur))
              :else :starting)))))

(defn status
  "Live view of THIS session's managed REPL for `dir`. Always safe. Model-facing:
   STRING keys + STRING enum values (crosses as a tool `:result`)."
  [session-id dir]
  (let [{:keys [id port tool aliases pid] :as info}
        (get @processes [session-id dir])

        running?
        (proc-alive? info)]

    (cond-> {"result" "status"
             "id" (or id (id-of dir))
             "dir" dir
             "status" (if running? "up" "down")}
      running?
      (assoc "running" true)

      (and running? port)
      (assoc "port" port)

      (and running? tool)
      (assoc "tool" (name tool))

      (and running? (seq aliases))
      (assoc "aliases" (mapv name aliases))

      (and running? pid)
      (assoc "pid" pid)

      running?
      (assoc "log" (or (:log info) (.getAbsolutePath (log-file dir)))))))

(defn health
  "Coarse LIVE health of THIS session's managed REPL for `dir`:
     :up       — process alive AND the port answers an nREPL describe
     :starting — process alive, port not answering yet
     :failed   — no live process but an UNEXPECTED death is on record
     :down     — nothing managed (or an intentional stop)
   Used as the resource registry's `:health-fn`, so footer/F4/ctx status tracks
   reality instead of the status frozen at registration time."
  [session-id dir]
  (let [info (get @processes [session-id dir])]
    (cond (proc-alive? info) (if (= :up
                                    (:status (nrepl-client/probe! {:host "localhost"
                                                                   :port (:port info)
                                                                   :timeout-ms 250})))
                               :up
                               :starting)
          (last-failure session-id dir) :failed
          :else :down)))

(defn start!
  "Self-start a project nREPL subprocess OWNED by `session-id` in `dir`.
   Always allowed — never flag-gated. Default `:aliases` are [:dev :test] (merged
   with any explicitly passed). We pick a FREE port, pass it to the launcher, drop
   any stray `.nrepl-port`, and wait for OUR port — SYNCHRONOUSLY: the wait ends
   the moment the launcher dies (fast :failed with exit + log tail), and only a
   still-alive-but-slow boot can outlive `start-deadline-ms` (then :starting,
   with an `.onExit` watcher recording any later death as a failure).

   - Already ours + alive for `[session-id dir]` → :already-running.
   - No known build file → :no-launcher.
   - Launcher exits before binding → :failed with exit code + log tail.
   - Else :started (port up) or :starting (still coming up; ctx will show it).

   Model-facing: STRING keys + STRING enum values (crosses as a tool `:result`)."
  ([session-id dir] (start! session-id dir nil))
  ([session-id dir {:keys [aliases]}]
   (let [k
         [session-id dir]

         aliases
         (as-keywords (if (seq aliases) aliases default-aliases))]

     (if (proc-alive? (get @processes k))
       (assoc (status session-id dir) "result" "already-running")
       (let [port (free-port!)]
         (if-let [{:keys [tool cmd]} (launcher-for dir aliases port)]
           (try
             (let [log (log-file dir)
                   pb (doto (ProcessBuilder. ^java.util.List cmd)
                        (.directory (io/file dir))
                        (.redirectErrorStream true)
                        (.redirectOutput log))
                   proc (.start pb)
                   pid (try (.pid proc) (catch Throwable _ nil))
                   info {:id (id-of dir)
                         :process proc
                         :port port
                         :cmd cmd
                         :tool tool
                         :aliases (vec aliases)
                         :pid pid
                         :dir dir
                         :log (.getAbsolutePath log)
                         :started-at (System/currentTimeMillis)
                         :last-touch (System/currentTimeMillis)}]

               (swap! processes assoc k info)
               (ensure-reaper!)
               (watch-process! session-id dir proc (.getAbsolutePath log))
               (let [st (wait-until-up proc port start-deadline-ms)]
                 ;; We passed --port explicitly; never depend on the file the tool
                 ;; may still write. Remove it so it can't mislead anything.
                 (delete-stray-port-file! dir)
                 (when-not (= :up st)
                   ;; If the launcher died quickly, give the OS one beat to publish
                   ;; the exit code before deciding whether this is "still starting"
                   ;; or a real startup failure.
                   (try (.waitFor proc 100 TimeUnit/MILLISECONDS) (catch Throwable _ nil)))
                 (let [alive? (alive? proc)
                       exit (when-not alive? (try (.exitValue proc) (catch Throwable _ nil)))
                       log-path (.getAbsolutePath log)
                       tail (tail-log log-path 80)
                       base {"id" (id-of dir)
                             "dir" dir
                             "port" port
                             "tool" (name tool)
                             "aliases" (mapv name aliases)
                             "pid" pid
                             "cmd" cmd
                             "log" log-path}]

                   (cond
                     (= :up st) (do (clear-failure! session-id dir)
                                    (assoc base
                                      "result" "started"
                                      "status" "up"))
                     (not alive?) (do (swap! processes dissoc k)
                                      (record-failure! session-id dir proc log-path)
                                      (cond->
                                        (assoc base
                                          "result" "failed"
                                          "status" "failed"
                                          "message"
                                          (str "nREPL launcher exited before accepting connections"
                                               (when exit (str " (exit " exit ")"))
                                               ". See log for details.")
                                          "exit" exit)
                                        (seq tail)
                                        (assoc "log_tail" tail)))
                     :else
                     (cond->
                       (assoc base
                         "result" "starting"
                         "status" "starting"
                         "message"
                         "nREPL launching; not accepting connections yet. Check the log if it stays in this state.")
                       (seq tail)
                       (assoc "log_tail" tail))))))
             (catch java.io.IOException e
               {"result" "failed"
                "status" "failed"
                "id" (id-of dir)
                "dir" dir
                "port" port
                "tool" (name tool)
                "aliases" (mapv name aliases)
                "cmd" cmd
                "log" (.getAbsolutePath (log-file dir))
                "message" (str "Could not start nREPL launcher: " (.getMessage e))}))
           {"result" "no-launcher"
            "status" "down"
            "dir" dir
            "message"
            "No deps.edn / project.clj / bb.edn in this directory to start an nREPL."}))))))

(defn stop!
  "Stop THIS session's managed nREPL for `dir` (graceful, then forced). The
   entry is DEREGISTERED FIRST so the `.onExit` watcher reads the death as an
   intentional stop, never a failure; any remembered failure for `dir` is
   cleared too. No-op-safe. Model-facing STRING-keyed result."
  [session-id dir]
  (let [k
        [session-id dir]

        {:keys [^Process process]}
        (get @processes k)]

    (clear-failure! session-id dir)
    (if process
      (do (swap! processes dissoc k)
          (.destroy process)
          (when-not (.waitFor process 3 TimeUnit/SECONDS) (.destroyForcibly process))
          {"result" "stopped" "id" (id-of dir) "dir" dir})
      {"result" "not-managed"
       "id" (id-of dir)
       "dir" dir
       "message" "No Vis-managed nREPL for this directory in this session."})))

(defonce ^:private reaper (atom nil))

(defn- reap-idle!
  "Stop every managed REPL untouched for `idle-reap-ms`. Best-effort per entry —
   one wedged stop never blocks reaping the rest. The session's resource mirror
   self-prunes once `stop!` drops the process (its `:alive-fn` flips to false)."
  []
  (when (pos? idle-reap-ms)
    (let [now
          (System/currentTimeMillis)

          stale
          (for [[[sid dir] info]
                @processes

                :let [t
                      (or (:last-touch info) (:started-at info) 0)]
                :when (> (- now t) idle-reap-ms)]

            [sid dir])]

      (doseq [[sid dir] stale]
        (try (stop! sid dir) (catch Throwable _ nil))))))

(defn- ensure-reaper!
  "Lazily start the ONE daemon thread that idle-reaps managed REPLs. Idempotent;
   a no-op when idle reaping is disabled (`idle-reap-ms` <= 0). The thread is a
   daemon so it never keeps the JVM alive on shutdown."
  []
  (when (and (pos? idle-reap-ms) (compare-and-set! reaper nil ::starting))
    (let [t (Thread. ^Runnable
                     (fn []
                       (loop []

                         (try (Thread/sleep (long reaper-tick-ms))
                              (reap-idle!)
                              (catch InterruptedException _ nil)
                              (catch Throwable _ nil))
                         (recur)))
                     "vis-clj-repl-idle-reaper")]
      (.setDaemon t true)
      (.start t)
      (reset! reaper t))))

(defn- prune-dead!
  "Drop this session's dead entries from the process atom, best-effort."
  [session-id]
  (let [dead (for [[[sid _dir :as k] info] @processes
                   :when (and (= sid session-id) (not (proc-alive? info)))]

               k)]
    (when (seq dead) (apply swap! processes dissoc dead))))

(defn session-repls
  "Live REPLs OWNED by `session-id`, as a vec of
   `{:id :dir :port :tool :aliases :pid}` sorted by dir. Prunes dead entries as a
   side effect. This is the SINGLE source of truth for ctx + eval/test targeting —
   there is no external-port discovery."
  [session-id]
  (prune-dead! session-id)
  (->> @processes
       (keep (fn [[[sid _dir] info]]
               (when (and (= sid session-id) (proc-alive? info))
                 {:id (:id info)
                  :dir (:dir info)
                  :port (:port info)
                  :tool (:tool info)
                  :aliases (:aliases info)
                  :pid (:pid info)
                  :log (or (:log info) (.getAbsolutePath (log-file (:dir info))))})))
       (sort-by :dir)
       vec))

(defn repl-by-id
  "The session's live REPL info matching resource `id`, or nil."
  [session-id id]
  (first (filter #(= (:id %) id) (session-repls session-id))))

(defn ensure-repl-for-dir!
  "Return the live REPL info for `[session-id dir]`, AUTOSTARTING one (with the
   default :dev :test aliases) when the session owns none for `dir` — OR when the
   recorded process is alive but UNREACHABLE (a boot that never bound its port, or
   a wedged server thread that `proc-alive?` alone cannot see): such a stale
   process is stopped and REPLACED instead of silently swallowing every eval on a
   dead socket (the failure that stalls `run_tests` past the tool budget). A
   still-present process gets a brief `wait-until-up` grace window to finish a slow
   cold boot before it is judged wedged, so a legitimately-booting REPL is never
   needlessly restarted. When the (re)start does NOT yield a live process, returns
   start!'s STRING-keyed lifecycle result (\"failed\"/\"no-launcher\"… with exit +
   log_tail) instead of swallowing it — callers tell the cases apart by `:port`
   (live keyword-keyed info) vs `\"result\"` (string-keyed lifecycle map)."
  [session-id dir]
  (let [k
        [session-id dir]

        info
        (get @processes k)]

    (if (and (proc-alive? info)
             (:port info)
             (= :up (wait-until-up (:process info) (:port info) 5000)))
      (do (touch! session-id dir) info)
      ;; Dead, or alive-but-wedged (port never answers): drop the stale process
      ;; first, then autostart — surfacing start!'s lifecycle result when no live
      ;; process results, exactly as the plain-autostart path does.
      (do (when (proc-alive? info) (stop! session-id dir))
          (let [r (start! session-id dir nil)]
            (or (get @processes k) r))))))

(defn restart-for-dir!
  "Recover THIS session's REPL for `dir` when its recorded process is dead OR
   alive-but-UNREACHABLE (a boot that never bound its port, or a wedged server
   thread) — the failure `proc-alive?` alone cannot see. Gives a still-present
   process a brief grace window to finish booting before killing it, so a slow
   cold-deps start is not needlessly restarted; otherwise stops the stale
   process and autostarts a fresh one. Returns the live REPL info, or nil when
   `dir` has no launchable Clojure build file."
  [session-id dir]
  (let [info (get @processes [session-id dir])]
    (if (and (proc-alive? info)
             (:port info)
             (= :up (wait-until-up (:process info) (:port info) 5000)))
      info
      (do (stop! session-id dir) (start! session-id dir nil) (get @processes [session-id dir])))))

(defn resolve-target!
  "Resolve — and AUTOSTART when needed — the REPL an eval should hit for
   `session-id`. `id` is an optional explicit resource id; `default-dir` is where
   we autostart when the session owns no REPL yet. Returns `{:id :dir :port}`.

   Rules (the ownership contract):
     - explicit `id` → that REPL (throws if no such live REPL in this session);
     - `id` = `default` (any case) → sentinel, treated as no explicit id (below);
     - 0 REPLs       → autostart `default-dir` with [:dev :test], use it;
     - 1 REPL        → use it (it's the implicit default);
     - >1 REPLs      → use the DEFAULT: the REPL owning `default-dir` (the
                       workspace root) when present, else the first (dir-sorted).
                       Never throws on ambiguity — eval always resolves and the
                       result reports which REPL ran it, so the model can pass an
                       explicit `id` to override."
  [session-id id default-dir]
  (let [id
        (some-> id
                str
                str/trim
                not-empty)

        ;; "default" is a sentinel, not a real resource id — treat it as "no
        ;; explicit id" so it falls through to the implicit-default resolution
        ;; (autostart when the session owns none, else the default REPL).
        id
        (when-not (some-> id
                          str/lower-case
                          (= "default"))
          id)]

    (if id
      (if-let [r (repl-by-id session-id id)]
        (do (touch! session-id (:dir r)) r)
        (throw (ex-info (str "no nREPL registered under id '"
                             id
                             "' in this session — check ctx / session_resources for live REPL ids")
                        {:type :clj/unknown-repl-id :id id})))
      (let [repls (session-repls session-id)]
        (if (zero? (count repls))
          (let [r (ensure-repl-for-dir! session-id default-dir)]
            (cond (:port r) (select-keys r [:id :dir :port])
                  (= "no-launcher" (get r "result"))
                  (throw (ex-info
                           (str "no Clojure build file in " default-dir " to autostart an nREPL")
                           {:type :clj/no-launcher :dir default-dir}))
                  :else
                  ;; The start RAN and failed — surface the launcher's own story
                  ;; (exit + log tail), never a bogus \"no build file\" guess, so
                  ;; the model sees WHY instead of blindly restarting forever.
                  (throw (ex-info (str "nREPL autostart failed for "
                                       default-dir
                                       (when-let [e (get r "exit")]
                                         (str " (exit " e ")"))
                                       (when-let [m (get r "message")]
                                         (str ": " m))
                                       (when-let [tail (seq (get r "log_tail"))]
                                         (str "\nlauncher log tail:\n  "
                                              (str/join "\n  " (take-last 15 tail)))))
                                  {:type :clj/start-failed
                                   :dir default-dir
                                   :exit (get r "exit")
                                   :log (get r "log")}))))
          ;; 1+ REPLs: the implicit default is the one owning `default-dir`
          ;; (the workspace root) when live, else the first (dir-sorted).
          (let [r (or (first (filter #(= (:dir %) default-dir) repls)) (first repls))]
            (touch! session-id (:dir r))
            (select-keys r [:id :dir :port])))))))
