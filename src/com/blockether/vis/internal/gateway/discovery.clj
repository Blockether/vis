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
  (:import (java.lang ProcessBuilder$Redirect ProcessHandle)
           (java.lang.management ManagementFactory)
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
  (boolean (some-> pid
                   long
                   ProcessHandle/of
                   (.orElse nil)
                   .isAlive)))

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
   follow the `serve` subcommand. `:base` lets a caller override the re-exec
   prefix (tests do)."
  [{:keys [db port host token-file require-token? base]}]
  (let [target (db-target db)]
    (-> (vec (or base (base-argv)))
        (conj "serve")
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
   SELF-REGISTERS its pid/port on startup, so we never need its pid here. Output
   is discarded (the daemon logs through Telemere). Returns nil."
  [opts]
  (let [argv
        (spawn-argv opts)

        _
        (.mkdirs (registry-dir))

        pb
        (if (unix?)
          (let [cmd (str "nohup " (str/join " " (map sh-quote argv)) " >/dev/null 2>&1 &")]
            (ProcessBuilder. ^java.util.List ["sh" "-c" cmd]))
          (ProcessBuilder. ^java.util.List (vec argv)))]

    (doto pb
      (.redirectOutput ProcessBuilder$Redirect/DISCARD)
      (.redirectError ProcessBuilder$Redirect/DISCARD))
    (.start pb)
    nil))

;; =============================================================================
;; Orchestration
;; =============================================================================

(defn await-registry!
  "Poll for a FRESH registry entry for `db` up to `timeout-ms`, checking every
   `poll-ms`. Returns the entry or nil on timeout."
  ([db probe] (await-registry! db probe {}))
  ([db probe {:keys [timeout-ms poll-ms] :or {timeout-ms 8000 poll-ms 100}}]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []

       (let [entry (read-registry db)]
         (cond (registry-fresh? entry probe) entry
               (< (System/currentTimeMillis) deadline) (do (Thread/sleep (long poll-ms)) (recur))
               :else nil))))))

(defn discover-or-start!
  "Resolve a gateway for `db`. Returns:
   - `{:mode :none}`                          for a `:memory` DB (Q5, never spawns);
   - `{:mode :attach  :entry {…}}`            a fresh daemon already runs — connect;
   - `{:mode :spawned :entry {…}}`            we spawned one and it self-registered;
   - `{:mode :timeout}`                       we spawned but it never came up in time.

   `probe` is the port+secret liveness check (defaults to pid-liveness alone).
   `spawn` (default [[spawn-detached!]]) and `now` are injectable for tests. A
   stale registry is deleted before spawning; a bind-race loser naturally attaches
   because the winner's self-registration is what [[await-registry!]] observes."
  [{:keys [db] :as opts} &
   {:keys [probe spawn timeout-ms poll-ms] :or {probe (constantly true) spawn spawn-detached!}}]
  (if (memory-db? db)
    {:mode :none}
    (let [existing (read-registry db)]
      (if (registry-fresh? existing probe)
        {:mode :attach :entry existing}
        (do (when existing (delete-registry! db))
            (spawn opts)
            (if-let [entry (await-registry! db
                                            probe
                                            (cond-> {}
                                              timeout-ms
                                              (assoc :timeout-ms timeout-ms)

                                              poll-ms
                                              (assoc :poll-ms poll-ms)))]
              {:mode :spawned :entry entry}
              {:mode :timeout}))))))
