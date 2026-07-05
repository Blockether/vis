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
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client])
  (:import
   (java.net ServerSocket)
   (java.util.concurrent TimeUnit)))

;; { [session-id dir] -> {:id :process ^Process :port int :cmd [..] :tool kw
;;                        :aliases [kw..] :pid long :dir str :started-at ms} }
;; defonce so a `(require :reload)` during dev never orphans a live child.
;; NO on-disk registry: a managed REPL is bound to THIS vis process + session.
(defonce ^:private processes (atom {}))

(defn- alive? [^Process p] (boolean (and p (.isAlive p))))

(defn- proc-alive? [info] (alive? (:process info)))

;; Kept in sync with the nrepl/nrepl version pinned in this extension's deps.edn.
;; Injected via `-Sdeps` so the launcher works even in target projects that don't
;; declare nREPL themselves.
(def nrepl-version "1.3.0")

(def ^:private default-aliases
  "Every managed REPL boots with the project's dev + test deps/paths. Unknown
   aliases are silently ignored by tools.deps, so this is safe everywhere."
  [:dev :test])

(defn id-of
  "Stable session-resource id for the REPL rooted at `dir`."
  [dir]
  (str "nrepl:" dir))

(defn- as-keywords [aliases]
  (mapv #(if (keyword? %) % (keyword (name %))) aliases))

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
  (when (seq aliases)
    (apply str (map #(str ":" (name %)) aliases))))

(defn launcher-for
  "Subprocess command to boot a project nREPL in `dir` on the EXPLICIT `port`,
   honouring `aliases` (deps.edn aliases / lein profiles). Returns
   `{:tool kw :cmd [strings]}` or nil when no known Clojure build file is present."
  [dir aliases port]
  (let [present? (fn [n] (.isFile (io/file dir n)))]
    (cond
      (present? "deps.edn")
      ;; Inject nREPL + the `nrepl.cmdline` main via a synthetic alias we append
      ;; LAST. tools.deps resolves `:main-opts` last-alias-wins (while
      ;; `:extra-deps`/`:extra-paths` accumulate), so a user alias that carries
      ;; its OWN `:main-opts` still contributes its deps + source paths to the
      ;; classpath, but our `-m nrepl.cmdline --port N` is what actually runs. We
      ;; want the user aliases' deps/paths, never their main.
      {:tool :clj
       :cmd  ["clojure"
              "-Sdeps" (str "{:aliases {:vis/nrepl-launch "
                         "{:extra-deps {nrepl/nrepl {:mvn/version \"" nrepl-version "\"}} "
                         ":main-opts [\"-m\" \"nrepl.cmdline\" \"--port\" \"" port "\"]}}}")
              (str "-M" (alias-suffix aliases) ":vis/nrepl-launch")]}

      (present? "project.clj")
      {:tool :lein
       :cmd  (if (seq aliases)
               ["lein" "with-profile"
                (str/join "," (map #(str "+" (name %)) aliases))
                "repl" ":headless" ":port" (str port)]
               ["lein" "repl" ":headless" ":port" (str port)])}

      (present? "bb.edn")
      {:tool :bb :cmd ["bb" "nrepl-server" (str port)]}

      :else nil)))

(defn- log-file
  "Subprocess log path under the OS temp dir (never inside the user's project
   tree), namespaced by the target dir so each managed REPL gets its own log."
  ^java.io.File [dir]
  (let [safe (-> (str dir)
               (str/replace #"[^A-Za-z0-9]+" "_")
               (str/replace #"(^_+|_+$)" ""))]
    (io/file (System/getProperty "java.io.tmpdir") (str "vis-nrepl-" safe ".log"))))

(defn- delete-stray-port-file!
  "The launcher may still drop a `.nrepl-port` in `dir` even though we passed the
   port explicitly. We never read it, so delete it so nothing downstream (or a
   human) mistakes it for the source of truth. Best-effort."
  [dir]
  (try (io/delete-file (io/file dir ".nrepl-port") true) (catch Throwable _ nil)))

(defn- wait-until-up
  "Poll our OWN chosen `port` until the nREPL accepts a describe round-trip, up to
   `deadline-ms`. Returns :up or :starting (timed out)."
  [port deadline-ms]
  (let [deadline (+ (System/currentTimeMillis) (long deadline-ms))]
    (loop []
      (let [st (:status (nrepl-client/probe! {:host "localhost" :port port :timeout-ms 500}))]
        (cond
          (= :up st)                                   :up
          (< (System/currentTimeMillis) deadline)      (do (Thread/sleep 250) (recur))
          :else                                        :starting)))))

(defn status
  "Live view of THIS session's managed REPL for `dir`. Always safe. Model-facing:
   STRING keys + STRING enum values (crosses as a tool `:result`)."
  [session-id dir]
  (let [{:keys [id port tool aliases pid] :as info} (get @processes [session-id dir])
        running? (proc-alive? info)]
    (cond-> {"result" "status"
             "id"     (or id (id-of dir))
             "dir"    dir
             "status" (if running? "up" "down")}
      running?                     (assoc "running" true)
      (and running? port)          (assoc "port" port)
      (and running? tool)          (assoc "tool" (name tool))
      (and running? (seq aliases)) (assoc "aliases" (mapv name aliases))
      (and running? pid)           (assoc "pid" pid))))

(defn start!
  "Self-start a project nREPL subprocess OWNED by `session-id` in `dir`.
   Always allowed — never flag-gated. Default `:aliases` are [:dev :test] (merged
   with any explicitly passed). We pick a FREE port, pass it to the launcher, drop
   any stray `.nrepl-port`, and wait briefly for OUR port to come up.

   - Already ours + alive for `[session-id dir]` → :already-running.
   - No known build file → :no-launcher.
   - Else :started (port up) or :starting (still coming up; ctx will show it).

   Model-facing: STRING keys + STRING enum values (crosses as a tool `:result`)."
  ([session-id dir] (start! session-id dir nil))
  ([session-id dir {:keys [aliases]}]
   (let [k       [session-id dir]
         aliases (as-keywords (if (seq aliases) aliases default-aliases))]
     (if (proc-alive? (get @processes k))
       (assoc (status session-id dir) "result" "already-running")
       (let [port (free-port!)]
         (if-let [{:keys [tool cmd]} (launcher-for dir aliases port)]
           (let [log  (log-file dir)
                 pb   (doto (ProcessBuilder. ^java.util.List cmd)
                        (.directory (io/file dir))
                        (.redirectErrorStream true)
                        (.redirectOutput log))
                 proc (.start pb)
                 pid  (try (.pid proc) (catch Throwable _ nil))
                 info {:id         (id-of dir)
                       :process    proc
                       :port       port
                       :cmd        cmd
                       :tool       tool
                       :aliases    (vec aliases)
                       :pid        pid
                       :dir        dir
                       :started-at (System/currentTimeMillis)}]
             (swap! processes assoc k info)
             (let [st (wait-until-up port 20000)]
               ;; We passed --port explicitly; never depend on the file the tool
               ;; may still write. Remove it so it can't mislead anything.
               (delete-stray-port-file! dir)
               (cond-> {"result"  (if (= :up st) "started" "starting")
                        "id"      (id-of dir)
                        "dir"     dir
                        "port"    port
                        "tool"    (name tool)
                        "aliases" (mapv name aliases)
                        "pid"     pid
                        "cmd"     cmd
                        "log"     (.getAbsolutePath log)
                        "status"  (name st)}
                 (not= :up st)
                 (assoc "message" "nREPL launching; it will report :up in ctx shortly."))))
           {"result"  "no-launcher" "dir" dir
            "message" "No deps.edn / project.clj / bb.edn in this directory to start an nREPL."}))))))

(defn stop!
  "Stop THIS session's managed nREPL for `dir` (graceful, then forced). Clears the
   in-memory cache. No-op-safe. Model-facing STRING-keyed result."
  [session-id dir]
  (let [k [session-id dir]
        {:keys [^Process process]} (get @processes k)]
    (if process
      (do
        (.destroy process)
        (when-not (.waitFor process 3 TimeUnit/SECONDS)
          (.destroyForcibly process))
        (swap! processes dissoc k)
        {"result" "stopped" "id" (id-of dir) "dir" dir})
      {"result"  "not-managed" "id" (id-of dir) "dir" dir
       "message" "No Vis-managed nREPL for this directory in this session."})))

(defn- prune-dead!
  "Drop this session's dead entries from the process atom, best-effort."
  [session-id]
  (let [dead (for [[[sid _dir :as k] info] @processes
                   :when (and (= sid session-id) (not (proc-alive? info)))]
               k)]
    (when (seq dead)
      (apply swap! processes dissoc dead))))

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
              {:id      (:id info)
               :dir     (:dir info)
               :port    (:port info)
               :tool    (:tool info)
               :aliases (:aliases info)
               :pid     (:pid info)})))
    (sort-by :dir)
    vec))

(defn repl-by-id
  "The session's live REPL info matching resource `id`, or nil."
  [session-id id]
  (first (filter #(= (:id %) id) (session-repls session-id))))

(defn ensure-repl-for-dir!
  "Return the live REPL info for `[session-id dir]`, AUTOSTARTING one (with the
   default :dev :test aliases) when the session owns none for `dir`. Returns nil
   only when there is no launchable Clojure build file in `dir`. Used by eval /
   test to guarantee a REPL is up for the target dir."
  [session-id dir]
  (let [k [session-id dir]]
    (if (proc-alive? (get @processes k))
      (get @processes k)
      (do (start! session-id dir nil)
        (get @processes k)))))

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
          (= :up (wait-until-up (:port info) 5000)))
      info
      (do
        (stop! session-id dir)
        (start! session-id dir nil)
        (get @processes [session-id dir])))))

(defn resolve-target!
  "Resolve — and AUTOSTART when needed — the REPL an eval should hit for
   `session-id`. `id` is an optional explicit resource id; `default-dir` is where
   we autostart when the session owns no REPL yet. Returns `{:id :dir :port}`.

   Rules (the ownership contract):
     - explicit `id` → that REPL (throws if no such live REPL in this session);
     - 0 REPLs       → autostart `default-dir` with [:dev :test], use it;
     - 1 REPL        → use it (it's the implicit default);
     - >1 REPLs      → throw, listing the ids (the model MUST pass one)."
  [session-id id default-dir]
  (let [id (some-> id str str/trim not-empty)]
    (if id
      (or (repl-by-id session-id id)
        (throw (ex-info (str "no nREPL registered under id '" id
                          "' in this session — check ctx / session_resources for live REPL ids")
                 {:type :clj/unknown-repl-id :id id})))
      (let [repls (session-repls session-id)]
        (case (count repls)
          0 (let [r (ensure-repl-for-dir! session-id default-dir)]
              (when-not r
                (throw (ex-info (str "no Clojure build file in " default-dir
                                  " to autostart an nREPL")
                         {:type :clj/no-launcher :dir default-dir})))
              (select-keys r [:id :dir :port]))
          1 (select-keys (first repls) [:id :dir :port])
          (throw (ex-info (str (count repls) " nREPLs are live in this session — pass an id to pick one: "
                            (str/join ", " (map :id repls)))
                   {:type :clj/ambiguous-repl :ids (mapv :id repls)})))))))
