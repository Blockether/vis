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

   ATTACHMENTS: `connect!` registers an EXTERNAL nREPL the user already runs in a
   SEPARATE `attachments` atom, never in `processes`. They are different kinds:
   one is a process we own and must eventually kill, the other is an address we
   were invited to use. Keeping them apart is what lets ONE project have both at
   once — the managed JVM REPL `repl start` booted for its `.clj`, and the
   shadow-cljs nREPL its own `watch` runs for the `.cljs` — instead of the second
   `connect` answering \"already-running\" about the first and handing back a JVM
   REPL nobody asked for.

   Starting/stopping is CORE and ALWAYS allowed — never gated behind a flag."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.shadow-repl :as shadow-repl]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.core :as vis])
  (:import (java.io RandomAccessFile)
           (java.net ServerSocket)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent TimeUnit)))

;; { [session-id dir] -> {:id :process ^Process :port int :cmd [..] :tool kw
;;                        :aliases [kw..] :pid long :dir str :started-at ms} }
;; defonce so a `(require :reload)` during dev never orphans a live child.
;; NO on-disk registry: a managed REPL is bound to THIS vis process + session.
(defonce ^:private processes (atom {}))

;; { [session-id dir] -> {:id :dir :host :port :dialect kw :build str? :target kw?
;;                        :session-token str? :started-at ms :last-touch ms} }
;; EXTERNAL nREPLs this session was explicitly told to attach to. Deliberately a
;; SECOND registry rather than a flag inside `processes`: an attachment owns no
;; process (nothing to reap, kill or watch for exit) and must be able to coexist
;; with the managed REPL for the SAME dir — a project's JVM REPL and its
;; shadow-cljs watch are two live REPLs, not two claims on one slot.
(defonce ^:private attachments (atom {}))

;; { [session-id dir] -> monitor Object }. A stable per-key lock so concurrent
;; `start!` calls for the SAME [session-id dir] SERIALIZE: the check-then-spawn
;; is made atomic, so a racing second caller sees the first REPL as
;; :already-running instead of spawning + orphaning a DUPLICATE JVM.
(defonce ^:private start-locks (atom {}))

(defn- start-lock
  "The monitor Object guarding `start!` for `k` = `[session-id dir]`, created
   once and reused so all starts for that key lock on the SAME object."
  [k]
  (or (get @start-locks k) (get (swap! start-locks update k #(or % (Object.))) k)))

(defn- alive? [^Process p] (boolean (and p (.isAlive p))))

(defn- proc-alive?
  "Registry-entry liveness. `processes` holds ONLY REPLs vis spawned, so an entry
   is alive exactly while its subprocess is. Attachments have no process to ask —
   their liveness is the port probe in `health` / `live-repl-for-dir`."
  [info]
  (alive? (:process info)))

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
;; `idle-reap-ms`. Set VIS_CLJ_REPL_IDLE_MS=0 to disable (or to a custom ms budget).
(def ^:private idle-reap-ms
  ;; A `delay`, never an eager read: `native-image` initializes this namespace at
  ;; BUILD time, so a top-level `getenv` would ship the BUILDER's answer.
  (delay (let
           [env (some-> (System/getenv "VIS_CLJ_REPL_IDLE_MS")
                        str/trim
                        not-empty)]
           (or (when env (try (Long/parseLong env) (catch Exception _ nil))) (* 20 60 1000)))))

(def ^:private reaper-tick-ms 60000)

(defn- touch!
  "Stamp `[session-id dir]`'s REPL as used just now, so the idle reaper spares an
   actively-worked REPL. Stamps whichever registry holds it — a managed process,
   an attachment, or both. No-op when the session has neither for `dir`."
  [session-id dir]
  (let
    [k
     [session-id dir]

     stamp
     (fn [m]
       (cond-> m
         (contains? m k)
         (assoc-in [k :last-touch] (System/currentTimeMillis))))]

    (swap! processes stamp)
    (swap! attachments stamp)))

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
(def nrepl-version "1.7.0")

;; How long `start!` waits for OUR port to answer. The deadline only matters
;; while the launcher is STILL ALIVE — a dead launcher short-circuits to a
;; :failed result in ≤ ~250ms via `wait-until-up` — so it can be generous
;; enough for a cold-cache deps resolve without making real failures slow.
(def ^:private start-deadline-ms 120000)

(defn- booting?
  "True when `info`'s process is alive and still inside its cold-boot window
   (`start-deadline-ms` since `:started-at`). Such a REPL is a slow-but-healthy
   boot we must WAIT for — never stop+restart it: a restart throws away real
   boot progress and, repeated across evals, spins an endless restart cycle."
  [info]
  (boolean (and (proc-alive? info)
                (:started-at info)
                (< (- (System/currentTimeMillis) (long (:started-at info)))
                   (long start-deadline-ms)))))

(defn- health-probe-ms
  "How long to wait for a recorded REPL to answer a describe before judging it
   wedged. A still-booting process gets the REMAINING cold-boot window (so a
   slow legit boot is never killed mid-flight); anything else gets a short grace."
  [info]
  (if (booting? info)
    (max 5000 (- (long start-deadline-ms) (- (System/currentTimeMillis) (long (:started-at info)))))
    5000))

(def ^:private default-aliases
  "Every managed REPL boots with the project's dev + test deps/paths. Unknown
   aliases are silently ignored by tools.deps, so this is safe everywhere."
  [:dev :test])

(defn home-relativize
  "Collapse a leading user-home prefix to `~`, so a REPL id reads `~/vis` instead
   of the noisy machine-absolute `/Users/you/vis`. Paths outside home (and blanks)
   pass through unchanged."
  [^String dir]
  (let [shown (paths/abbreviate-home dir)]
    ;; `paths/abbreviate-home` renders the home dir as `~/` for general display;
    ;; REPL resource ids have historically used the compact `~` spelling.
    (if (= shown "~/") "~" shown)))

(defn id-of
  "Stable session-resource id for the REPL rooted at `dir`. The dir is CANONICALIZED
   first — so `..`, a trailing slash, and symlinks all collapse to ONE id per
   physical dir (no `nrepl:.../vis` vs `nrepl:.../vis/..` near-duplicates spawning
   twin REPLs) — then its home prefix is homogenized to `~`, so a REPL always
   addresses as `nrepl:~/vis`."
  [dir]
  (let [canon (try (.getCanonicalPath (io/file (str dir))) (catch Throwable _ (str dir)))]
    (str "nrepl:" (home-relativize canon))))

(defn- attachment-id
  "Session-resource id for an EXTERNAL attachment in `dir`. Suffixed so it can
   never collide with the MANAGED REPL's id for the same dir (both are live at
   once), and suffixed with the shadow-cljs BUILD when there is one, because that
   is what the caller thinks it is talking to: `nrepl:~/proj#app`."
  [dir build]
  (str (id-of dir) "#" (or build "external")))

(defn- attachment-entry
  "Keyword-keyed registry view of an attachment — the shape `session-repls`,
   `live-repl-for-dir` and `resolve-target!` speak."
  [att]
  (cond->
    {:id (:id att)
     :dir (:dir att)
     :port (:port att)
     :host (or (:host att) "localhost")
     :external? true
     :dialect (or (:dialect att) :clj)}
    (:build att)
    (assoc :build
      (:build att) :target
      (:target att) :session-token
      (:session-token att))))

(defn- attachment-view
  "Model-facing STRING-keyed view of an attachment: what it is, where it is, and
   — for shadow-cljs — which build an eval lands in and what that build targets."
  [att]
  (cond->
    {"id" (:id att)
     "cwd" (:dir att)
     "status" "up"
     "external" true
     "host" (or (:host att) "localhost")
     "port" (:port att)
     "dialect" (name (or (:dialect att) :clj))}
    (:build att)
    (assoc "build" (:build att))

    (:target att)
    (assoc "target" (name (:target att)))))

(defn- as-keywords [aliases] (mapv #(if (keyword? %) % (keyword (name %))) aliases))

(defn- launch-aliases
  "The final alias vector used for every managed REPL launch. Defaults are
   mandatory; caller aliases add to them, never replace them."
  [aliases]
  (->> (concat default-aliases aliases)
       as-keywords
       distinct
       vec))

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
  (let
    [present? (fn [n]
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
  "MINT a fresh subprocess log path under `~/.vis/logs` (never inside the user's
   project tree, never the OS temp dir) for ONE nREPL start.

   Called exactly once per spawn, and every call returns a DIFFERENT file: the
   dir-derived stem keeps the log greppable, the random suffix makes it the
   private log of that one process. A name keyed by the directory alone was a
   shared file — `ProcessBuilder`'s output redirect TRUNCATES, so a second
   session (or a plain restart) in the same directory erased the log the live
   REPL was still writing into, and every resource view pointed at that one
   path. The `~/.vis/logs` sweep (`foundation.housekeeping`) is what bounds the
   resulting one-file-per-start growth. The logs dir is created on demand."
  ^java.io.File [dir]
  (let
    [home
     (System/getProperty "user.home")

     ;; Relativize against ~ so the log name reflects the project's path
     ;; RELATIVE to home (e.g. `vis`), not the whole absolute path
     ;; (`Users_fierycod_vis`). Paths outside home fall back to as-is.
     rel
     (let [d (str dir)]
       (if (and (seq home) (str/starts-with? d home)) (subs d (count home)) d))

     safe
     (-> rel
         (str/replace #"[^A-Za-z0-9]+" "_")
         (str/replace #"(^_+|_+$)" ""))

     logs-dir
     (io/file home ".vis" "logs")]

    (.mkdirs logs-dir)
    (io/file logs-dir (str "vis-nrepl-" safe "-" (java.util.UUID/randomUUID) ".log"))))

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
   (let
     [f
      (when (seq (str log-path)) (io/file (str log-path)))

      n
      (max 1 (long (or n default-log-line-limit)))]

     (if (and f (.isFile f))
       (try (with-open [raf (RandomAccessFile. f "r")]
              (let
                [len (.length raf)
                 start (max 0 (- len (long tail-read-bytes)))
                 size (int (- len start))]

                (if (zero? size)
                  []
                  (let [buf (byte-array size)]
                    (.seek raf start)
                    (.readFully raf buf)
                    (let
                      [lines (vec (str/split-lines (String. buf StandardCharsets/UTF_8)))
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
  (let
    [exit
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

(def ^:private wait-poll-ms
  ;; Pause between port probes in `wait-until-up`. A plain var (NOT ^:const —
  ;; that inlines and silently breaks with-redefs) so tests can shrink it and
  ;; run deadline paths in milliseconds.
  250)

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
              (< (System/currentTimeMillis) deadline) (do (Thread/sleep (long wait-poll-ms))
                                                          (recur))
              :else :starting)))))

(defn status
  "Live view of THIS session's REPLs for `dir`. Always safe. Model-facing: STRING
   keys + STRING enum values (crosses as a tool `:result`).

   The MANAGED REPL is the subject; an EXTERNAL attachment for the same dir rides
   along under \"attached\" — and IS the subject when there is no managed REPL,
   because then it is the only REPL this dir has."
  [session-id dir]
  (let
    [{:keys [id port tool aliases pid] :as info}
     (get @processes [session-id dir])

     running?
     (proc-alive? info)

     att
     (get @attachments [session-id dir])]

    (if (and att (not running?))
      (assoc (attachment-view att) "result" "status")
      (cond->
        {"result" "status" "id" (or id (id-of dir)) "cwd" dir "status" (if running? "up" "down")}
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

        ;; The log path is the one MINTED for this very process at spawn, never
        ;; recomputed: `log-file` mints a fresh name per call, so a derived path
        ;; would name a file nothing has ever written.
        (and running? (:log info))
        (assoc "log" (:log info))

        att
        (assoc "attached" (attachment-view att))))))

(defn attachment-health
  "Coarse LIVE health of the EXTERNAL attachment for `dir`: :up while its address
   answers, :down once it stops (or when nothing is attached). An attachment owns
   no process to watch, so the probe IS its health — never :starting or :failed."
  [session-id dir]
  (let [att (get @attachments [session-id dir])]
    (if (and att
             (= :up
                (:status (nrepl-client/probe! {:host (or (:host att) "localhost")
                                               :port (:port att)
                                               :timeout-ms 250}))))
      :up
      :down)))

(defn health
  "Coarse LIVE health of THIS session's REPL for `dir`:
     :up       — managed process alive AND the port answers
     :starting — managed process alive, port not answering yet
     :failed   — no live process but an UNEXPECTED death is on record
     :down     — nothing managed (intentional stop) and nothing attached
   With no managed REPL the answer is the ATTACHMENT's health, because for a dir
   whose only REPL is attached that is the REPL being asked about. Used as the
   resource registry's `:health-fn`, so footer/F4/ctx status tracks reality
   instead of the status frozen at registration time."
  [session-id dir]
  (let [info (get @processes [session-id dir])]
    (cond (proc-alive? info) (if (= :up
                                    (:status (nrepl-client/probe! {:host "localhost"
                                                                   :port (:port info)
                                                                   :timeout-ms 250})))
                               :up
                               :starting)
          (last-failure session-id dir) :failed
          (get @attachments [session-id dir]) (attachment-health session-id dir)
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
   (let
     [k
      [session-id dir]

      aliases
      (launch-aliases aliases)]

     ;; SERIALIZE the check-then-spawn per [session-id dir]: without this a
     ;; racing second start! (e.g. the repl tool + an eval-autostart)
     ;; could both pass the alive? check and both spawn, orphaning a duplicate
     ;; JVM. Under the lock the loser re-checks and returns :already-running.
     ;; (`start-lock` returns a SHARED, atom-stored monitor — not a fresh/local
     ;; object — so clj-kondo's suspicious-lock heuristic is a false positive.)
     #_{:clj-kondo/ignore [:locking-suspicious-lock]}
     (locking (start-lock k)
       (if (proc-alive? (get @processes k))
         (assoc (status session-id dir) "result" "already-running")
         (let [port (free-port!)]
           (if-let [{:keys [tool cmd]} (launcher-for dir aliases port)]
             (try
               (let
                 [log (log-file dir)
                  ;; Resolve argv + proxy env atomically through the shared,
                  ;; fail-closed language-process contract. nREPL alone may bind a
                  ;; loopback listener; direct outbound traffic remains jailed.
                  launch (vis/session-process-launch session-id cmd {:loopback-port port})
                  jailed-cmd (:argv launch)
                  pb (doto (ProcessBuilder. ^java.util.List jailed-cmd)
                       (.directory (io/file dir))
                       (.redirectErrorStream true)
                       (.redirectOutput log))
                  _env (let [^java.util.Map e (.environment ^ProcessBuilder pb)]
                         (when (:replace-env? launch) (.clear e))
                         (doseq [[k v] (:env launch)]
                           (.put e ^String k ^String v)))
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
                 ;; Clean-exit teardown of managed REPLs is owned by vis core:
                 ;; every spawn is registered as a session resource, and the
                 ;; gateway server's JVM shutdown hook runs resources/shutdown!
                 ;; which stops them all before the JVM exits. (kill -9 runs no
                 ;; hooks anywhere — only a child-side parent watchdog could
                 ;; cover that.)
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
                   (let
                     [alive? (alive? proc)
                      exit (when-not alive? (try (.exitValue proc) (catch Throwable _ nil)))
                      log-path (.getAbsolutePath log)
                      tail (tail-log log-path 80)
                      base {"id" (id-of dir)
                            "cwd" dir
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
                       (not alive?)
                       (do (swap! processes dissoc k)
                           (record-failure! session-id dir proc log-path)
                           (cond->
                             (assoc base
                               "result" "failed"
                               "status" "failed"
                               "message" (str "nREPL launcher exited before accepting connections"
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
                  "cwd" dir
                  "port" port
                  "tool" (name tool)
                  "aliases" (mapv name aliases)
                  "cmd" cmd
                  "message" (str "Could not start nREPL launcher: " (.getMessage e))}))
             {"result" "no-launcher"
              "status" "down"
              "cwd" dir
              "message"
              "No deps.edn / project.clj / bb.edn in this directory to start an nREPL."})))))))

(defn detach!
  "DETACH THIS session's external attachment for `dir`: vis never kills a process
   it did not spawn, so this drops the address — and evicts the client connection
   whose socket would otherwise leak — while the user's server keeps running
   exactly as it was. No-op-safe. Model-facing STRING-keyed result."
  [session-id dir]
  (let
    [k
     [session-id dir]

     {:keys [host port build id]}
     (get @attachments k)]

    (if-not port
      {"result" "not-attached"
       "id" (id-of dir)
       "cwd" dir
       "message" "No external nREPL attached to this directory in this session."}
      (do (swap! attachments dissoc k)
          ;; The client-side connection cache is keyed by [host port]; a REPL we
          ;; stop targeting MUST evict it or its transport thread + socket leak
          ;; for the life of the process.
          (nrepl-client/evict! (or host "localhost") port)
          {"result" "detached"
           "id" id
           "cwd" dir
           "message" (str "Detached from external nREPL at "
                          (or host "localhost")
                          ":"
                          port
                          (when build (str " (shadow-cljs build " build ")"))
                          " — the process keeps running (vis does not own it).")}))))

(defn stop!
  "Stop THIS session's REPL for `dir`. A MANAGED subprocess is destroyed (graceful,
   then forced); the entry is DEREGISTERED FIRST so the `.onExit` watcher reads
   that death as an intentional stop, never a failure, and any remembered
   failure/crash history for `dir` is cleared too.

   With no managed REPL but an attachment for `dir`, this DETACHES it: `stop` on
   the only REPL a dir has must never answer `not-managed` about the one REPL it
   can see. No-op-safe. Model-facing STRING-keyed result."
  [session-id dir]
  (let
    [k
     [session-id dir]

     {:keys [^Process process port]}
     (get @processes k)]

    (clear-failure! session-id dir)
    (cond process (do
                    ;; The client-side connection cache is keyed by [host port]; a REPL
                    ;; that goes away MUST evict it or its transport thread + socket leak
                    ;; for the life of the process (ports are per-start, never reused).
                    (when port (nrepl-client/evict! "localhost" port))
                    (swap! processes dissoc k)
                    (.destroy process)
                    (when-not (.waitFor process 3 TimeUnit/SECONDS) (.destroyForcibly process))
                    {"result" "stopped" "id" (id-of dir) "cwd" dir})
          (get @attachments k) (detach! session-id dir)
          :else {"result" "not-managed"
                 "id" (id-of dir)
                 "cwd" dir
                 "message" "No Vis-managed nREPL for this directory in this session."})))

(defn connect!
  "Attach THIS session to an EXTERNAL nREPL the USER already runs (their editor
   jack-in, a `clj -M:nrepl`, a `shadow-cljs watch`) — the OPT-IN inverse of
   `start!`: vis never spawns, never kills and never reaps that process; it only
   registers the address so eval targets it like a managed REPL. Explicit consent
   only — nothing ever auto-connects and no port is ever scanned.

   Opts `{:host :port :build}`:
   - `:build` names a shadow-cljs build (\"app\"). It makes this a ClojureScript
     attachment: the build is SELECTED in the nREPL session every eval reuses, so
     `repl_eval` lands in that build's JS runtime instead of the JVM the very same
     server also serves. With no `:port`, the port is read from the project's own
     `.shadow-cljs/nrepl.port` — the file that watch published, under the dir the
     caller named.
   - The address is PROBED first (bounded): a dead host:port is REFUSED
     (\"unreachable\") instead of registered.
   - An attachment is INDEPENDENT of the MANAGED REPL for the same dir: both stay
     live, each under its own id. A second connect for the same dir REPLACES the
     attachment (\"reconnected\"), so changing build or port needs no detach.
   - `stop!` / `detach!` on it only detaches.

   Every refusal names what to run next: \"no-port\", \"unreachable\",
   \"not-shadow\" (a plain JVM nREPL has no build to select), \"unknown-build\"
   (with the ids that server loaded), \"no-watch\" (a build's REPL needs its
   RUNNING worker), \"select-failed\". Model-facing: STRING keys + STRING enums."
  [session-id dir {:keys [host port build]}]
  (let
    [host
     (or (some-> host
                 str/trim
                 not-empty)
         "localhost")

     build
     (some-> build
             str
             str/trim
             not-empty)

     port
     (or (some-> port
                 long)
         (when build (shadow-repl/nrepl-port dir)))

     k
     [session-id dir]

     existing
     (get @attachments k)]

    (cond
      (nil? port) {"result" "no-port"
                   "status" "down"
                   "cwd" dir
                   "message" (str "No \"port\" given and no "
                                  (str/join "/" shadow-repl/port-file-path)
                                  " under "
                                  (home-relativize (str dir))
                                  " — start `shadow-cljs watch "
                                  build
                                  "` (it publishes its nREPL port there), or pass {\"port\": N}.")}
      (not= :up (:status (nrepl-client/probe! {:host host :port port :timeout-ms 3000})))
      {"result" "unreachable"
       "status" "down"
       "cwd" dir
       "host" host
       "port" port
       "message"
       (str "No nREPL answering at " host ":" port " — is it running? Nothing was registered.")}
      :else
      (let
        [;; Attaching DEFINES this session's starting state, so the cached
         ;; connection goes first. A connection already SELECTED on a build
         ;; evaluates everything — including the shadow probe, whose `resolve`
         ;; does not exist in ClojureScript — inside that build, so a re-connect
         ;; would misread its own server as \"not shadow-cljs\". A fresh nREPL
         ;; session is always CLJ, which is exactly what probing and selecting
         ;; need.
         _
         (nrepl-client/evict! host port)

         shadow
         (when build (shadow-repl/probe! {:host host :port port :build build}))]

        (cond (and build (not (:shadow? shadow)))
              {"result" "not-shadow"
               "status" "down"
               "cwd" dir
               "host" host
               "port" port
               "message"
               (str "The nREPL at "
                    host
                    ":"
                    port
                    " is a plain Clojure one — shadow-cljs is not loaded in it, so there is no"
                    " build to select. Connect without \"build\" for a JVM REPL, or point at the"
                    " port `shadow-cljs watch "
                    build
                    "` published in "
                    (str/join "/" shadow-repl/port-file-path)
                    ".")}
              (and build (not (some #{build} (:builds shadow))))
              {"result" "unknown-build"
               "status" "down"
               "cwd" dir
               "host" host
               "port" port
               "build" build
               "builds" (:builds shadow)
               "message" (str "shadow-cljs at " host
                              ":" port
                              " knows no build \"" build
                              "\" — it loaded: " (str/join ", " (:builds shadow))
                              ". Build ids come from :builds in the shadow-cljs.edn THAT server"
                              " started with, which need not be the one under this cwd.")}
              (and build (not (:worker? shadow)))
              {"result" "no-watch"
               "status" "down"
               "cwd" dir
               "host" host
               "port" port
               "build" build
               "message" (str "shadow-cljs build \""
                              build
                              "\" has no watch running. A REPL selects a build's RUNNING worker, so"
                              " start `shadow-cljs watch "
                              build
                              "` first, then connect again.")}
              :else
              (let [sel (when build (shadow-repl/select! {:host host :port port :build build}))]
                (if (and build (not (:selected? sel)))
                  {"result" "select-failed"
                   "status" "down"
                   "cwd" dir
                   "host" host
                   "port" port
                   "build" build
                   "message" (str "shadow-cljs refused to select build \"" build
                                  "\": " (:message sel))}
                  (let
                    [att {:id (attachment-id dir build)
                          :dir dir
                          :host host
                          :port port
                          :build build
                          :target (:target shadow)
                          :dialect (if build :cljs :clj)
                          :session-token (:session-token sel)
                          :started-at (System/currentTimeMillis)
                          :last-touch (System/currentTimeMillis)}
                     ;; ONE cheap eval, so the ANSWER to connect already says whether
                     ;; this build can evaluate at all — a `watch` with no runtime
                     ;; joined is a perfectly healthy attachment that evaluates
                     ;; nothing, and finding that out on the next repl_eval reads as
                     ;; a broken REPL instead of a missing `node`/browser.
                     ping (when build (shadow-repl/eval! att {:code "1" :timeout-ms 5000}))
                     att (cond-> att
                           (:session-token ping)
                           (assoc :session-token (:session-token ping)))]

                    (swap! attachments assoc k att)
                    (cond->
                      (assoc (attachment-view att) "result" (if existing "reconnected" "connected"))
                      build
                      (assoc "runtime" (if (:message ping) "none" "connected"))

                      (:message ping)
                      (assoc "message" (:message ping)))))))))))

(defonce ^:private reaper (atom nil))

(defn- reap-idle!
  "Stop every managed REPL untouched for `idle-reap-ms`. Best-effort per entry —
   one wedged stop never blocks reaping the rest. The session's resource mirror
   self-prunes once `stop!` drops the process (its `:alive-fn` flips to false)."
  []
  (when (pos? (long @idle-reap-ms))
    (let
      [now
       (System/currentTimeMillis)

       stale
       (for
         [[[sid dir] info]
          @processes

          ;; Only REPLs vis SPAWNED are in here; attachments are the user's own
          ;; processes and are never reaped, only detached on request.
          :let [t
                (long (or (:last-touch info) (:started-at info) 0))]
          :when (> (- now t) (long @idle-reap-ms))]

         [sid dir])]

      (doseq [[sid dir] stale]
        (try (stop! sid dir) (catch Throwable _ nil))))))

(defn- ensure-reaper!
  "Lazily start the ONE daemon thread that idle-reaps managed REPLs. Idempotent;
   a no-op when idle reaping is disabled (`idle-reap-ms` <= 0). The thread is a
   daemon so it never keeps the JVM alive on shutdown."
  []
  (when (and (pos? (long @idle-reap-ms)) (compare-and-set! reaper nil ::starting))
    (let
      [t (Thread. ^Runnable
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
  (let
    [dead (for
            [[[sid _dir :as k] info] @processes
             :when (and (= sid session-id) (not (proc-alive? info)))]

            k)]
    (when (seq dead) (apply swap! processes dissoc dead))))

(defn session-repls
  "Live REPLs OWNED by (or ATTACHED to) `session-id`, as a vec of
   `{:id :dir :port :tool :aliases :pid}` (+ `:log` for managed; `:external? :host
   :dialect` and, for a shadow-cljs one, `:build :target :session-token` for
   attached) sorted by dir. Prunes dead managed entries as a side effect.
   This is the SINGLE source of truth for ctx + eval/test targeting — external
   REPLs enter it ONLY via an explicit `connect!`, never by discovery.

   Within one dir the MANAGED REPL sorts FIRST, so a dir that has both keeps the
   JVM REPL as its implicit default and attaching a ClojureScript build never
   silently redirects an eval that named no target."
  [session-id]
  (prune-dead! session-id)
  (->> (concat (keep (fn [[[sid _dir] info]]
                       (when (and (= sid session-id) (proc-alive? info))
                         (cond->
                           {:id (:id info)
                            :dir (:dir info)
                            :port (:port info)
                            :tool (:tool info)
                            :aliases (:aliases info)
                            :pid (:pid info)}
                           (:log info)
                           (assoc :log (:log info)))))
                     @processes)
               (keep (fn [[[sid _dir] att]]
                       (when (= sid session-id) (attachment-entry att)))
                     @attachments))
       (sort-by (juxt :dir #(if (:external? %) 1 0)))
       vec))

(defn repl-by-id
  "The session's live REPL info matching resource `id`, or nil."
  [session-id id]
  (first (filter #(= (:id %) id) (session-repls session-id))))

(defn live-repl-for-dir
  "The REPL `session-id` ALREADY has for `dir`, and only while it ANSWERS — else nil.
   NEVER starts, stops or replaces a server: `run_tests` reuses a REPL the session
   deliberately keeps up, and with none it runs the suite in a clean JVM instead of
   spawning one behind the caller's back. `repl` `start` is the ONE way a managed
   REPL comes into existence.

   The MANAGED REPL wins: it needs a live process AND a describe round-trip inside
   its remaining cold-boot window (`health-probe-ms`), so a still-booting server
   counts and a wedged one does not. An ATTACHMENT is offered only when it is a
   JVM one and only while its own probe answers — a session SELECTED on a
   shadow-cljs build cannot load a `.clj` test namespace, and handing it to a JVM
   test run would fail as if the tests were broken."
  [session-id dir]
  (let
    [info
     (get @processes [session-id dir])

     managed-live?
     (and info
          (proc-alive? info)
          (:port info)
          (= :up (wait-until-up (:process info) (:port info) (health-probe-ms info))))]

    (if managed-live?
      (do (touch! session-id dir) info)
      (let [att (get @attachments [session-id dir])]
        (when (and att
                   (not= :cljs (:dialect att))
                   (= :up
                      (:status (nrepl-client/probe! {:host (or (:host att) "localhost")
                                                     :port (:port att)
                                                     :timeout-ms 2000}))))
          (touch! session-id dir)
          (attachment-entry att))))))


(defn resolve-target!
  "Resolve the RUNNING REPL an eval should hit for `session-id`.
   `id` is an optional explicit resource id; `default-dir`
   picks the implicit default among several live REPLs. Returns `{:id :dir :port}`.

   Rules (the ownership contract):
     - explicit `id` → that REPL (throws :clj/unknown-repl-id if no such live REPL);
     - `id` = `default` (any case) → sentinel, treated as no explicit id (below);
     - 0 REPLs       → throw :clj/no-repl (start one with repl, e.g. repl(\"clojure\"));
     - 1 REPL        → use it (the implicit default);
     - >1 REPLs      → use the DEFAULT: the REPL owning `default-dir` (the
                       workspace root) when present, else the first (dir-sorted).
                       Never throws on ambiguity — eval always resolves and the
                       result reports which REPL ran it, so the model can pass an
                       explicit `id` to override."
  [session-id id default-dir]
  (let
    [id
     (some-> id
             str
             str/trim
             not-empty)

     ;; "default" is a sentinel, not a real resource id — treat it as "no
     ;; explicit id" so it falls through to the implicit-default resolution
     ;; (the single owned REPL, else the default REPL among several).
     id
     (when-not (some-> id
                       str/lower-case
                       (= "default"))
       id)]

    (if id
      (if-let [r (repl-by-id session-id id)]
        (do (touch! session-id (:dir r)) r)
        (throw (ex-info
                 (str "no nREPL registered under id '"
                      id
                      "' in this session — check session[\"resources\"][\"repls\"][\"clojure\"]")
                 {:type :clj/unknown-repl-id :id id})))
      (let [repls (session-repls session-id)]
        (if (zero? (count repls))
          (throw (ex-info (str "no running nREPL in this session — start one with "
                               "repl(\"clojure\"), "
                               "then retry the eval")
                          {:type :clj/no-repl :dir default-dir}))
          ;; 1+ REPLs: the implicit default is the one owning `default-dir`
          ;; (the workspace root) when live, else the first (dir-sorted).
          (let [r (or (first (filter #(= (:dir %) default-dir) repls)) (first repls))]
            (touch! session-id (:dir r))
            (select-keys r
                         [:id :dir :port :host :external? :dialect :build :target
                          :session-token])))))))

(defn eval!
  "Evaluate `opts` (an `nrepl-client/eval!` map minus its address) over `target` —
   `resolve-target!`'s map, or any `{:host :port}`. Returns nrepl-client's
   STRING-keyed result, and never throws for a shadow-cljs condition.

   A target carrying a shadow-cljs `:build` is the whole reason this exists. The
   build is (re)SELECTED in the nREPL session whenever that session is no longer
   the one it was selected in — an evicted socket or a restarted watch otherwise
   leaves the very same code silently answering as JVM Clojure — and the fresh
   session token is written back to the attachment. The result carries \"build\",
   and a build whose JS runtime is not connected answers with the instruction that
   starts one instead of shadow's bare `No available JS runtime`."
  [session-id {:keys [host port build dir] :as target} opts]
  (if-not build
    (nrepl-client/eval! (assoc opts
                          :host (or host "localhost")
                          :port port))
    (let [r (shadow-repl/eval! target opts)]
      (when-let [token (:session-token r)]
        (swap! attachments (fn [m]
                             (cond-> m
                               (contains? m [session-id dir])
                               (assoc-in [[session-id dir] :session-token] token)))))
      (if (:selected? r)
        (cond-> (assoc (:result r) "build" build)
          (:message r)
          (assoc "message" (:message r)))
        {"build" build
         "error_message" (str "shadow-cljs build \"" build
                              "\" could not be selected: " (:message r))
         "message" (str "Is `shadow-cljs watch "
                        build
                        "` still running? Reattach with"
                        " repl(\"clojure\", \"connect\", {\"build\": \""
                        build
                        "\"}) once it is.")}))))
