(ns com.blockether.vis.ext.language-clojure.repl-manager
  "nREPL lifecycle for the Clojure pack.

   Self-starting a REPL spawns a real subprocess in the workspace. Starting,
   restarting and stopping a project nREPL is core and ALWAYS allowed — never
   gated behind a flag or toggle.

   The REPL runs as a SUBPROCESS in the workspace root — never in-process —
   so `clj/eval` hits the project's own classpath (deps.edn / project.clj /
   bb.edn deps), not Vis's JVM. The chosen launcher writes `.nrepl-port` on
   boot, which `nrepl-ctx` discovery then surfaces in context automatically.

   Managed REPLs SURVIVE A VIS RESTART: each one is recorded by PID in a
   persistent registry (`~/.vis/clj-nrepl/managed.edn`). The in-memory atom
   only caches the `Process` handle for clean teardown in the same session;
   across a restart we re-attach to the PID via `ProcessHandle`, so a
   self-started REPL still reads as `:managed` and is still stoppable.

   `:status`/`:stop` read + clean up a Vis-managed proc; `:start`/`:restart`
   spawn one. All four are always allowed."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.ports :as ports])
  (:import
   (java.lang ProcessHandle)
   (java.util.concurrent TimeUnit)))

;; In-memory cache of THIS session's spawns:
;; { dir -> {:process ^Process :cmd [..] :tool kw :aliases [..] :started-at ms} }
;; defonce so a `(require :reload)` during dev never orphans a live child.
;; The cross-restart source of truth is the on-disk registry below.
(defonce ^:private processes (atom {}))

(defn- alive? [^Process p] (boolean (and p (.isAlive p))))

;; ---------------------------------------------------------------------------
;; PID re-attach — recognise our own REPLs across a vis restart
;; ---------------------------------------------------------------------------

(defn- handle-of ^ProcessHandle [pid]
  (when pid (.orElse (ProcessHandle/of (long pid)) nil)))

(defn- pid-alive? [pid]
  (boolean (some-> (handle-of pid) .isAlive)))

(defn- kill-pid!
  "Best-effort terminate a process by PID (used when we only have the PID from
   the registry, e.g. after a vis restart — no `Process` handle to waitFor)."
  [pid]
  (when-let [h (handle-of pid)]
    (try (.destroy h) (catch Throwable _ nil))
    (try (when (.isAlive h) (.destroyForcibly h)) (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Persistent registry — survives a vis/TUI restart
;; { dir -> {:pid long :tool kw :aliases [..] :started-at ms} }
;; ---------------------------------------------------------------------------

(def ^:private state-dir
  (io/file (System/getProperty "user.home") ".vis" "clj-nrepl"))

(def ^:private registry-file (io/file state-dir "managed.edn"))

(defonce ^:private registry-lock (Object.))

(defn- read-registry []
  (locking registry-lock
    (try
      (when (.isFile registry-file)
        (let [m (edn/read-string (slurp registry-file))]
          (when (map? m) m)))
      (catch Throwable _ nil))))

(defn- write-registry! [m]
  (locking registry-lock
    (try
      (.mkdirs state-dir)
      (spit registry-file (pr-str (or m {})))
      (catch Throwable _ nil))))

(defn- register! [dir info]
  (locking registry-lock
    (write-registry! (assoc (or (read-registry) {}) dir info))))

(defn- unregister! [dir]
  (locking registry-lock
    (write-registry! (dissoc (or (read-registry) {}) dir))))

(defn- dir-alive?
  "Is the REPL for `dir` still running? Prefer this session's `Process`; fall
   back to the registry PID via `ProcessHandle` (the cross-restart path)."
  [dir reg-info]
  (let [proc (:process (get @processes dir))]
    (if proc (alive? proc) (pid-alive? (:pid reg-info)))))

;; Kept in sync with the nrepl/nrepl version pinned in this extension's
;; deps.edn. Injected via `-Sdeps` so the launcher works even in target
;; projects that don't declare nREPL themselves.
(def nrepl-version "1.3.0")

(defn- alias-suffix
  "deps.edn alias suffix, e.g. [:dev :test] -> \":dev:test\". nil when none."
  [aliases]
  (when (seq aliases)
    (apply str (map #(str ":" (name %)) aliases))))

(defn launcher-for
  "Pick a subprocess command to boot a project nREPL in `dir`, by build file,
   honouring `aliases` (deps.edn aliases / lein profiles). Returns
   `{:tool kw :cmd [strings]}` or nil when no known Clojure build file is
   present. Each command writes `.nrepl-port` in its working dir on boot."
  ([dir] (launcher-for dir nil))
  ([dir aliases]
   (let [present? (fn [n] (.isFile (io/file dir n)))]
     (cond
       (present? "deps.edn")
       ;; Inject nREPL + the `nrepl.cmdline` main via a synthetic alias we
       ;; append LAST. tools.deps resolves `:main-opts` last-alias-wins (while
       ;; `:extra-deps`/`:extra-paths` accumulate), so a user alias that carries
       ;; its OWN `:main-opts` — e.g. a `:test` alias whose main is the lazytest
       ;; runner — still contributes its deps + source paths to the classpath,
       ;; but our `-m nrepl.cmdline` is what actually runs. We want the user
       ;; aliases' deps/paths, never their main.
       {:tool :clj
        :cmd  ["clojure"
               "-Sdeps" (str "{:aliases {:vis/nrepl-launch "
                          "{:extra-deps {nrepl/nrepl {:mvn/version \"" nrepl-version "\"}} "
                          ":main-opts [\"-m\" \"nrepl.cmdline\"]}}}")
               (str "-M" (alias-suffix aliases) ":vis/nrepl-launch")]}

       (present? "project.clj")
       {:tool :lein
        :cmd  (if (seq aliases)
                ["lein" "with-profile"
                 (str/join "," (map #(str "+" (name %)) aliases))
                 "repl" ":headless"]
                ["lein" "repl" ":headless"])}

       (present? "bb.edn")
       {:tool :bb :cmd ["bb" "nrepl-server"]}

       :else nil))))

(defn- port-file ^java.io.File [dir] (io/file dir ".nrepl-port"))

(defn- log-file
  "Subprocess log path under the OS temp dir (never inside the user's project
   tree), namespaced by the target dir so each managed REPL gets its own log."
  ^java.io.File [dir]
  (let [safe (-> (str dir)
               (str/replace #"[^A-Za-z0-9]+" "_")
               (str/replace #"(^_+|_+$)" ""))]
    (io/file (System/getProperty "java.io.tmpdir") (str "vis-nrepl-" safe ".log"))))

(defn- read-port-file
  "Read the port written DIRECTLY in `dir` (not ancestors — the launcher
   writes `.nrepl-port` in its own cwd). Returns an int or nil."
  [dir]
  (let [f (port-file dir)]
    (when (.isFile f)
      (try
        (let [n (Long/parseLong (str/trim (slurp f)))]
          (when (< 0 n 65536) (int n)))
        (catch Throwable _ nil)))))

(defn- wait-for-port
  "Poll for `dir`'s OWN freshly-written `.nrepl-port` up to `deadline-ms`.
   Returns the port int or nil on timeout."
  [dir deadline-ms]
  (let [deadline (+ (System/currentTimeMillis) (long deadline-ms))]
    (loop []
      (or (read-port-file dir)
        (when (< (System/currentTimeMillis) deadline)
          (Thread/sleep 250)
          (recur))))))

(defn managed-ports
  "Map of `{port {:managed true :tool :aliases :pid :dir}}` for every currently
   ALIVE Vis-managed REPL, keyed by the live port each one wrote — sourced from
   the persistent registry so it survives a vis restart (re-attach by PID).
   Prunes dead entries from the registry as a side effect. Lets ctx mark which
   ports vis owns and surface subdir REPLs discovery would otherwise miss."
  []
  (let [reg   (or (read-registry) {})
        alive (into {} (filter (fn [[dir info]] (dir-alive? dir info)) reg))]
    (when (not= alive reg)
      (write-registry! alive))
    (into {}
      (keep (fn [[dir {:keys [pid tool aliases]}]]
              (when-let [port (read-port-file dir)]
                [port (cond-> {:managed true :dir dir}
                        tool          (assoc :tool tool)
                        (seq aliases) (assoc :aliases (vec aliases))
                        pid           (assoc :pid pid))])))
      alive)))

(defn status
  "Current managed-process + discovered-port view for `dir`. Always safe.
   Reflects the persistent registry, so a REPL vis started before a restart
   still reports as managed + running (re-attached by PID). Model-facing:
   STRING keys + STRING enum values (crosses the strings-only boundary as a
   tool `:result`)."
  [dir]
  (let [reg-info (get (read-registry) dir)
        running? (dir-alive? dir reg-info)
        tool     (:tool reg-info)
        aliases  (:aliases reg-info)
        pid      (or (some-> ^Process (:process (get @processes dir)) .pid)
                   (:pid reg-info))]
    {"result"        "status"
     "dir"           dir
     "managed"       (cond-> {"running" running?}
                       tool          (assoc "tool" (name tool))
                       (seq aliases) (assoc "aliases" (mapv name aliases))
                       (and running? pid) (assoc "pid" pid))
     "ports"         (mapv (fn [{:keys [port source]}] {"port" port "source" source})
                       (ports/discover-all dir))}))

(defn start!
  "Self-start a project nREPL subprocess in `dir` with optional `:aliases`.
   Always allowed — starting a project nREPL is core, never flag-gated.

   - If we already manage a live process for `dir` → :already-running.
   - If `dir` already has a LIVE external nREPL (its own `.nrepl-port` probes
     :up) → :already-running with `:is_external true` (we don't double-start).
   - A stale `.nrepl-port` is cleared so we wait for the fresh one.

   Waits briefly for the fresh port; returns :started (with port + liveness)
   when it appears, else :starting (ctx surfaces it next turn). Model-facing:
   STRING keys + STRING enum values (crosses as a tool `:result`)."
  ([dir] (start! dir nil))
  ([dir {:keys [aliases]}]
   (let [reg-info (get (read-registry) dir)]
     (cond
       ;; already ours and alive — same session, or re-attached across restart
       (or (alive? (:process (get @processes dir)))
         (and reg-info (pid-alive? (:pid reg-info))))
       (assoc (status dir) "result" "already-running")

       :else
       (let [existing (read-port-file dir)
             live?    (and existing
                        (= :up (:status (nrepl-client/probe! {:port existing :timeout-ms 500}))))]
         (cond
           ;; a live REPL we don't own (no registry entry) — don't double-start
           live?
           {"result" "already-running" "is_external" true "dir" dir "port" existing
            "message" "An nREPL is already running in this directory."}

           :else
           (if-let [{:keys [tool cmd]} (launcher-for dir aliases)]
             (do
               ;; drop a stale port file so wait-for-port only sees the fresh one
               (when existing (io/delete-file (port-file dir) true))
               (let [log  (log-file dir)
                     pb   (doto (ProcessBuilder. ^java.util.List cmd)
                            (.directory (io/file dir))
                            (.redirectErrorStream true)
                            (.redirectOutput log))
                     proc (.start pb)
                     pid  (try (.pid proc) (catch Throwable _ nil))]
                 (swap! processes assoc dir
                   {:process proc :cmd cmd :tool tool :aliases (vec aliases)
                    :started-at (System/currentTimeMillis)})
                 ;; persist so we still recognise this REPL after a vis restart
                 (register! dir {:pid pid :tool tool :aliases (vec aliases)
                                 :started-at (System/currentTimeMillis)})
                 (let [port  (wait-for-port dir 15000)
                       probe (when port (nrepl-client/probe! {:port port :timeout-ms 500}))]
                   (cond-> {"tool" (name tool) "dir" dir "aliases" (mapv name aliases) "cmd" cmd
                            "log"  (.getAbsolutePath log) "pid" pid}
                     port       (assoc "result" "started" "port" port
                                  "status" (some-> (:status probe) name))
                     (not port) (assoc "result" "starting"
                                  "message" "nREPL launching; the port will appear in ctx shortly.")))))
             {"result"  "no-launcher" "dir" dir
              "message" "No deps.edn / project.clj / bb.edn in this directory to start an nREPL."})))))))

(defn stop!
  "Stop the Vis-managed nREPL for `dir` (graceful, then forced). Uses this
   session's `Process` when present, else the registry PID (cross-restart
   path). Always clears the in-memory cache and the registry. No-op-safe."
  [dir]
  (let [{:keys [^Process process]} (get @processes dir)
        reg-info (get (read-registry) dir)]
    (if (or process reg-info)
      (do
        (cond
          process (do (.destroy process)
                    (when-not (.waitFor process 3 TimeUnit/SECONDS)
                      (.destroyForcibly process)))
          (:pid reg-info) (kill-pid! (:pid reg-info)))
        (swap! processes dissoc dir)
        (unregister! dir)
        {"result" "stopped" "dir" dir})
      {"result"  "not-managed" "dir" dir
       "message" "No Vis-managed nREPL for this directory."})))
