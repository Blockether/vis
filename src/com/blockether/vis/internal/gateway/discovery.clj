(ns com.blockether.vis.internal.gateway.discovery
  "Gateway daemon discovery + registry (build order step 1).

   One long-lived gateway per DB owns execution; every TUI/web/whatever is a
   thin client of it. This namespace answers the boot-time question: \"is a
   gateway already running for my DB — attach; else spawn one, DETACHED (nobody's
   child), then hand back where to connect.\"

   Registry: one EDN file per DB at `~/.vis/gateway/registry/<sha256(db)>.edn`
   holding `{:pid :port :host :secret :db :created-at}`. Freshness = the recorded
   `:pid` is still alive AND a caller-supplied `probe` confirms the port+secret
   are really OUR daemon (guards OS pid reuse — see D4/Q3 in TODO-gateway-daemon).

   Design decisions (locked, see TODO-gateway-daemon.md):
   - Q2 registry key = the DB path (two dirs sharing `--db` share one daemon).
   - Q3 race = port-bind winner is the daemon; the loser attaches. The daemon
     SELF-REGISTERS on startup (via [[register-self!]] from `serve-main!`), so a
     spawner never needs the child pid — it just polls for a fresh registry.
   - Q5 `:memory` never registers/spawns (headless one-shot stays in-process).

   Effects (`spawn`, `probe`, pid-liveness) are injectable so the orchestration
   in [[discover-or-start!]] is unit-testable without a real process."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io RandomAccessFile)
           (java.lang ProcessBuilder$Redirect ProcessHandle)
           (java.lang.management ManagementFactory)
           (java.nio.channels FileChannel FileLock)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

;; =============================================================================
;; Paths + registry key
;; =============================================================================

(defn- vis-home ^java.io.File [] (io/file (System/getProperty "user.home") ".vis"))

(defn registry-dir
  "Directory holding the per-DB registry files. Not created here."
  ^java.io.File []
  (io/file (vis-home) "gateway" "registry"))

(defn- sha256-hex
  ^String [^String s]
  (let [md
        (MessageDigest/getInstance "SHA-256")

        bs
        (.digest md (.getBytes s StandardCharsets/UTF_8))]

    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bs))))

(defn db-target
  "Normalize a DB target/spec to the value that identifies a daemon. SQLite specs
   use their `:path`; `:memory` stays ephemeral; other values pass through."
  [db]
  (cond (map? db) (or (:path db) db)
        :else db))

(defn memory-db?
  "True for the ephemeral in-memory DB target — it never spawns/attaches a
   daemon (Q5). Accepts raw `--db` values and resolved DB specs."
  [db]
  (let [target (db-target db)]
    (or (nil? target) (= target :memory) (contains? #{":memory" "memory"} (str target)))))

(defn- canonical-db
  "Canonical string identity of a DB path for keying the registry. A real path is
   canonicalized (so `./x` and an absolute `x` collapse); a non-path token is
   used verbatim. Resolved SQLite specs key by their `:path`."
  ^String [db]
  (let [s (str (db-target db))]
    (try (.getCanonicalPath (io/file s)) (catch Throwable _ s))))

(defn registry-key
  "Stable key for a DB target: `sha256` of its canonical path (Q2)."
  ^String [db]
  (sha256-hex (canonical-db db)))

(defn registry-file
  "The registry EDN file for DB target `db`."
  ^java.io.File [db]
  (io/file (registry-dir) (str (registry-key db) ".edn")))

;; =============================================================================
;; pid liveness
;; =============================================================================

(defn current-pid ^long [] (.pid (ProcessHandle/current)))

(defn pid-alive?
  "Best-effort: is OS process `pid` still running? A nil pid is NOT alive (an
   entry with no pid can't own a live daemon)."
  [pid]
  (if (nil? pid)
    false
    (let [^ProcessHandle ph (.orElse (ProcessHandle/of (long pid)) nil)]
      (boolean (and ph (.isAlive ph))))))

;; =============================================================================
;; Registry read / write / delete
;; =============================================================================

(defn read-registry
  "Read the registry entry for `db`, or nil when absent/unreadable/garbage."
  [db]
  (let [f (registry-file db)]
    (when (.exists f)
      (try (let [m (edn/read-string (slurp f))]
             (when (map? m) m))
           (catch Throwable _ nil)))))

(defn write-registry!
  "Write `entry` (a map, `:db`/`:created-at` filled in if absent) as the registry
   for `db`. Creates the registry dir. Returns the written entry."
  [db entry]
  (let [f
        (registry-file db)

        entry
        (merge {:db (canonical-db db) :created-at (System/currentTimeMillis)} entry)]

    (.mkdirs (registry-dir))
    (spit f (pr-str entry))
    entry))

(defn delete-registry!
  "Remove the registry file for `db`. Never throws. Returns true when a file was
   removed."
  [db]
  (try (let [f (registry-file db)]
         (boolean (and (.exists f) (.delete f))))
       (catch Throwable _ false)))

;; =============================================================================
;; Freshness
;; =============================================================================

(defn registry-fresh?
  "True when `entry` describes a LIVE daemon: it has a pid, that pid is alive,
   AND `probe` confirms it (defaults to trusting pid-liveness alone). The client
   adapter injects a real `probe` that hits `http://host:port` with `:secret` to
   defeat pid reuse (Q3)."
  ([entry] (registry-fresh? entry (constantly true)))
  ([{:keys [pid] :as entry} probe]
   (boolean (and (map? entry) pid (pid-alive? pid) (try (probe entry) (catch Throwable _ false))))))

;; =============================================================================
;; Self-registration (called by the daemon at startup)
;; =============================================================================

(defn register-self!
  "Called by the daemon (`serve-main!`) once it is listening: write this process
   as the owner of `db`. `secret` should be the gateway bearer token (or any
   nonce a client can echo back on [[registry-fresh?]]'s probe). Returns the
   entry, or nil for a `:memory` DB (which never registers)."
  [db {:keys [port host secret]}]
  (when-not (memory-db? db)
    (write-registry! db {:pid (current-pid) :port port :host host :secret secret})))

(defn deregister-self!
  "Called from the daemon's shutdown hook: delete OUR registry entry for `db`,
   but only if it still points at us (never clobber a successor that took over)."
  [db]
  (when-not (memory-db? db)
    (when (= (:pid (read-registry db)) (current-pid)) (delete-registry! db))))

;; =============================================================================
;; Spawn argv + detached launch
;; =============================================================================

(defn native-image?
  "True when running as the compiled GraalVM binary rather than on the JVM."
  []
  (some? (System/getProperty "org.graalvm.nativeimage.imagecode")))

(defn- java-bin
  ^String []
  (str (System/getProperty "java.home") java.io.File/separator "bin" java.io.File/separator "java"))

(defn- self-executable
  "The command that launched THIS process (the `vis` native binary), best-effort."
  ^String []
  (or (some-> (ProcessHandle/current)
              .info
              .command
              (.orElse nil))
      "vis"))

(defn base-argv
  "Prefix argv that re-launches vis in a fresh process: the native binary as-is,
   or `java -cp … clojure.main -m com.blockether.vis.core` on the JVM (mirrors the
   telegram self-restart trick)."
  []
  (if (native-image?)
    [(self-executable)]
    (vec (concat [(java-bin)]
                 (.getInputArguments (ManagementFactory/getRuntimeMXBean))
                 ["-cp" (System/getProperty "java.class.path") "clojure.main" "-m"
                  "com.blockether.vis.core"]))))

(defn spawn-argv
  "Full argv to launch a fresh gateway daemon for `db` on `port`. Serve flags
   follow the `gateway start` subcommand. `:base` lets a caller override the
   re-exec prefix (tests do)."
  [{:keys [db port host token-file require-token? base]}]
  (let [target (db-target db)]
    (-> (vec (or base (base-argv)))
        (into ["gateway" "start"])
        (cond->
          (and target (not (memory-db? target)))
          (into ["--db" (str target)])

          port
          (into ["--port" (str port)])

          host
          (into ["--host" (str host)])

          token-file
          (into ["--token-file" (str token-file)])

          require-token?
          (conj "--require-token")))))

(defn- unix?
  []
  (not (str/starts-with? (str/lower-case (System/getProperty "os.name" "")) "windows")))

(defn- sh-quote
  "Single-quote `s` for a POSIX shell."
  ^String [s]
  (str "'" (str/replace (str s) "'" "'\\''") "'"))

(defn spawn-detached!
  "Fire-and-forget launch of a gateway daemon for `db`, fully DETACHED so closing
   the spawner's terminal (SIGHUP) does NOT kill it (D4). On unix this runs the
   argv under `nohup … &` inside a throwaway `sh` (the daemon is reparented to
   init); elsewhere it falls back to a plain detached ProcessBuilder. The daemon
   SELF-REGISTERS its pid/port on startup, so we never need its pid here. Its
   stdout+stderr are captured to a per-DB boot log under the registry dir so a
   daemon that dies on startup is diagnosable instead of vanishing. Returns nil."
  [{:keys [db] :as opts}]
  (let [argv
        (spawn-argv opts)

        _
        (.mkdirs (registry-dir))

        ;; Per-DB boot log so a daemon that dies on startup (route conflict,
        ;; bad flag, port clash, …) leaves a diagnosable trace instead of only
        ;; surfacing to clients as a generic :gateway/start-timeout. Truncated
        ;; on each spawn, so it always reflects the latest boot.
        log
        (io/file (registry-dir) (str (registry-key db) ".log"))

        pb
        (if (unix?)
          (let [cmd (str "nohup "
                         (str/join " " (map sh-quote argv))
                         " >"
                         (sh-quote (.getPath log))
                         " 2>&1 &")]
            (ProcessBuilder. ^java.util.List (vec ["sh" "-c" cmd])))
          (ProcessBuilder. ^java.util.List (vec argv)))]

    (if (unix?)
      ;; The daemon's own stdio is redirected to `log` by the shell command;
      ;; discard only the throwaway `sh` wrapper's stdio.
      (doto pb
        (.redirectOutput ProcessBuilder$Redirect/DISCARD)
        (.redirectError ProcessBuilder$Redirect/DISCARD))
      ;; No shell wrapper here: send the daemon's own stdout+stderr to `log`.
      (doto pb (.redirectErrorStream true) (.redirectOutput (ProcessBuilder$Redirect/to log))))
    ;; Marks this `vis gateway start` as client-managed rather than a user-owned
    ;; foreground daemon. The daemon should self-reap when the last client dies;
    ;; a manually-run `vis gateway start` must not.
    (.put (.environment pb) "VIS_GATEWAY_MANAGED" "1")
    (.start pb)
    nil))

;; =============================================================================
;; Cross-process spawn lock (thundering-herd guard)
;; =============================================================================

(defn lock-file
  "OS advisory-lock file guarding daemon SPAWN for `db`. Sits beside the registry
   file so it shares the per-DB key; a stale lockfile is harmless (the OS lock,
   not the file's existence, is what's held)."
  ^java.io.File [db]
  (io/file (registry-dir) (str (registry-key db) ".lock")))

(defn acquire-spawn-lock!
  "Try to grab the EXCLUSIVE cross-process spawn lock for `db` WITHOUT blocking.
   Returns a `{:channel :lock}` holder when THIS process won the right to spawn,
   or nil when another process already holds it (that process is the designated
   spawner — the caller should just await the registry instead of launching a
   competing daemon). Never throws."
  [db]
  (try (.mkdirs (registry-dir))
       (let [ch (.getChannel (RandomAccessFile. (lock-file db) "rw"))]
         (if-let [^FileLock lk (try (.tryLock ch) (catch Throwable _ nil))]
           {:channel ch :lock lk}
           (do (.close ch) nil)))
       (catch Throwable _ nil)))

(defn release-spawn-lock!
  "Release a holder from [[acquire-spawn-lock!]]. Never throws."
  [{:keys [^FileLock lock ^FileChannel channel]}]
  (when lock (try (.release lock) (catch Throwable _ nil)))
  (when channel (try (.close channel) (catch Throwable _ nil))))


;; Orchestration
;; =============================================================================

(defn await-registry!
  "Poll for a FRESH registry entry for `db` up to `timeout-ms`, checking every
   `poll-ms`. `:on-tick` (optional) is called before each sleep with the elapsed
   millis so a caller can render live 'still waiting…' feedback; it never breaks
   the poll loop. Returns the entry or nil on timeout."
  ([db probe] (await-registry! db probe {}))
  ([db probe {:keys [timeout-ms poll-ms on-tick] :or {timeout-ms 8000 poll-ms 100}}]
   (let [start
         (System/currentTimeMillis)

         deadline
         (+ start timeout-ms)]

     (loop []

       (let [entry (read-registry db)]
         (cond (registry-fresh? entry probe) entry
               (< (System/currentTimeMillis) deadline)
               (do (when on-tick
                     (try (on-tick (- (System/currentTimeMillis) start)) (catch Throwable _ nil)))
                   (Thread/sleep (long poll-ms))
                   (recur))
               :else nil))))))

(defn discover-or-start!
  "Resolve a gateway for `db`. Returns:
   - `{:mode :none}`                          for a `:memory` DB (Q5, never spawns);
   - `{:mode :attach  :entry {…}}`            a fresh daemon already runs — connect;
   - `{:mode :spawned :entry {…}}`            WE spawned one and it self-registered;
   - `{:mode :awaited :entry {…}}`            ANOTHER process was spawning — we waited
                                              on its daemon instead of piling on;
   - `{:mode :timeout}`                       nobody came up in time.

   `probe` is the port+secret liveness check (defaults to pid-liveness alone).
   `spawn` (default [[spawn-detached!]]) and `now` are injectable for tests.

   `:on-event` (optional) is a side-effecting callback the caller uses to surface
   live progress so waiters are NEVER left staring at a frozen screen. It fires
   only on the SLOW path (never on a plain `:attach`) with maps:
   `{:phase :spawning}`           WE won the lock and are launching the daemon;
   `{:phase :awaiting}`           another vis is starting it — we wait, not spawn;
   `{:phase :tick :elapsed-ms n}` a poll heartbeat while awaiting either of those;
   `{:phase :ready :mode m :entry e}` the daemon came up;
   `{:phase :timeout}`            nobody came up in time. It never throws upward.

   THUNDERING-HERD GUARD (see [[acquire-spawn-lock!]]): when the registry is not
   fresh, only the ONE process that wins the cross-process spawn lock actually
   launches a daemon; every other concurrent starter finds the lock held, learns
   'someone is already spawning', and just awaits the winner's self-registration.
   That replaces the old blind-port-bind race (N daemons launched, N-1 crashing)
   with a single spawn + N-1 cheap waiters. A stale registry is deleted before
   spawning; the lock is re-checked against a double-read so a daemon that came up
   between our read and the lock is attached, not re-spawned."
  [{:keys [db] :as opts} &
   {:keys [probe spawn timeout-ms poll-ms on-event]
    :or {probe (constantly true) spawn spawn-detached!}}]
  (let [emit (fn [ev]
               (when on-event (try (on-event ev) (catch Throwable _ nil))))]
    (if (memory-db? db)
      {:mode :none}
      (let [existing (read-registry db)]
        (if (registry-fresh? existing probe)
          {:mode :attach :entry existing}
          (let [await-opts (cond-> {:on-tick (fn [elapsed-ms]
                                               (emit {:phase :tick :elapsed-ms elapsed-ms}))}
                             timeout-ms
                             (assoc :timeout-ms timeout-ms)

                             poll-ms
                             (assoc :poll-ms poll-ms))]
            (if-let [holder (acquire-spawn-lock! db)]
              ;; We are the designated spawner for this DB.
              (try
                ;; Double-check under the lock: another spawner may have finished
                ;; between our read above and winning the lock.
                (let [again (read-registry db)]
                  (if (registry-fresh? again probe)
                    {:mode :attach :entry again}
                    (do (when again (delete-registry! db))
                        (emit {:phase :spawning})
                        (spawn opts)
                        (if-let [entry (await-registry! db probe await-opts)]
                          (do (emit {:phase :ready :mode :spawned :entry entry})
                              {:mode :spawned :entry entry})
                          (do (emit {:phase :timeout}) {:mode :timeout})))))
                (finally (release-spawn-lock! holder)))
              ;; Lock is held by another process that's already spawning — don't
              ;; pile on a competing daemon, just wait for it to self-register.
              (do (emit {:phase :awaiting})
                  (if-let [entry (await-registry! db probe await-opts)]
                    (do (emit {:phase :ready :mode :awaited :entry entry})
                        {:mode :awaited :entry entry})
                    (do (emit {:phase :timeout}) {:mode :timeout}))))))))))
