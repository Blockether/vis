(ns com.blockether.vis.ext.language-clojure.nrepl-ctx
  "Per-turn `:ext/ctx-fn` contribution for the Clojure pack.

   Mirrors foundation-core's `workspace-ctx`: instead of forcing the model
   to call `clj_repl()` over and over, the engine injects live nREPL state
   into context as standing knowledge, nested UNDER the active language so a
   polyglot repo accumulates `:languages {:clojure {...} :typescript {...}}`:

     {\"session_env\" {\"languages\" {\"clojure\" {\"nrepl\" {\"default\" <int|nil>
                                                 \"ports\" [{\"port\" \"source\" \"via\"
                                                          \"status\" \"managed\"
                                                          [\"dialect\" \"cwd\" \"versions\"]
                                                          [\"tool\" \"pid\" \"aliases\"]} ...]}}}}}

   `:via` is the SOURCE/scope of the port (not the build tool we can't know for
   an external REPL): `:project` (a `.nrepl-port` in the project tree),
   `:lein-home` (`~/.lein/repl-port`), `:clojure-home` (`~/.clojure/.nrepl-port`).
   `:managed` says whether VIS started this REPL; managed entries also carry the
   handle to act on them — `:tool` (`:clj`/`:lein`/`:bb`), `:pid`, `:aliases` —
   and are surfaced even when they live in a subdir that workspace-root
   discovery wouldn't reach.

   Discovery (cheap `.nrepl-port` file reads) runs every render so a REPL
   started mid-turn shows up immediately. The liveness probe (`describe`
   round-trip) is the only network cost, so it is cached per turn AND per
   port-set — re-probing only when the turn advances or the set of ports
   changes. All best-effort: any failure degrades to discovery with
   `:status :unknown` (or an empty contribution) and never blocks the render."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
   [com.blockether.vis.internal.paths :as paths]
   [taoensso.telemere :as tel]))

(def ^:private probe-timeout-ms 100)

;; Per-turn liveness cache. `{:key [host turn sorted-ports] :statuses {port status-map}}`.
;; defonce so it survives `(require :reload)` during REPL-driven dev.
(defonce ^:private liveness-cache (atom {:key nil :statuses {}}))

(defn- via-of
  "Where the port was discovered (scope), derived from the `.nrepl-port` source
   path. NOT the build tool — for an external REPL we can't know that. `:tool`
   is stamped separately only for REPLs vis itself manages."
  [source]
  (let [s (str source)]
    (cond
      (re-find #"\.lein[/\\]repl-port$" s)        :lein-home
      (re-find #"\.clojure[/\\]\.nrepl-port$" s)   :clojure-home
      (re-find #"\.nrepl-port$" s)                 :project
      :else                                        :unknown)))

(defn- current-turn
  "Read the live turn off the engine ctx-atom for per-turn cache keying.
   Falls back to 0 when no ctx-atom is on env (e.g. tests / bare calls)."
  [env]
  (or (some-> (:ctx-atom env) deref :session/turn) 0))

(defn- probe-all
  "Probe every port in parallel, each under a hard deadline so one
   slow/firewalled host can never stall the render. Returns
   `{port {:status .. [:versions ..]}}`."
  [host ports]
  (let [futs (mapv (fn [p]
                     [p (future
                          (nrepl-client/probe!
                            {:host host :port p :timeout-ms probe-timeout-ms}))])
               ports)]
    (into {}
      (map (fn [[p f]] [p (deref f (+ probe-timeout-ms 50) {:status :down})]))
      futs)))

(defn- liveness-for
  "Statuses for `ports`, reusing the per-turn cache when the
   `[host turn port-set]` key is unchanged; otherwise probe and store."
  [host turn ports]
  (let [k [host turn (vec (sort ports))]]
    (if (= k (:key @liveness-cache))
      (:statuses @liveness-cache)
      (let [statuses (probe-all host ports)]
        (reset! liveness-cache {:key k :statuses statuses})
        statuses))))

(defn- source-dir
  "Parent directory of a `.nrepl-port` source file — a free working-directory
   fallback for project-rooted ports when the server can't be queried."
  [source]
  ;; `.getParent` (not `.getAbsolutePath`) so a POSIX-absolute source isn't
  ;; re-rooted onto the current drive on Windows; `/`-normalized for display.
  (paths/unixify (some-> source io/file .getParent)))

(defn- nrepl-block
  "Build the `:nrepl` map from discovery hits + liveness statuses + the
   vis-managed index. Each port carries `:via` (scope), `:status`, `:dialect`,
   `:cwd`, and `:managed` (did vis start it). Managed ports also carry the
   handle to act on them: `:tool`, `:pid`, `:aliases`."
  [hits statuses managed]
  ;; This block crosses the strings-only Clojure->Python boundary (as
  ;; session["env"]["languages"]["clojure"]["nrepl"]), so it is built with STRING
  ;; keys + STRING enum values. Its SOURCES (`statuses` from probe!, `managed`
  ;; from managed-ports, `hits` from discover-all) stay idiomatic keyword maps —
  ;; internal — and are stringified here at the crossing seam.
  {"default" (some-> hits first :port)
   "ports"   (mapv (fn [{:keys [port source]}]
                     (let [st  (get statuses port {:status :unknown})
                           via (via-of source)
                           m   (get managed port)
                           cwd (or (:cwd st)
                                 (when (= :project via) (source-dir source)))]
                       (cond-> {"port"    port
                                "source"  source
                                "via"     (name via)
                                "status"  (name (:status st))
                                "managed" (boolean m)}
                         (seq (:versions st)) (assoc "versions" (update-keys (:versions st) name))
                         (:dialect st)        (assoc "dialect" (name (:dialect st)))
                         cwd                  (assoc "cwd" cwd)
                         (:tool m)            (assoc "tool" (name (:tool m)))
                         (:pid m)             (assoc "pid" (:pid m))
                         (seq (:aliases m))   (assoc "aliases" (mapv name (:aliases m))))))
               hits)})

(defn- under-root?
  "True when `dir` is the workspace `root` or nested beneath it. Keeps one
   workspace's ctx from surfacing another workspace's managed REPL (the
   registry is global)."
  [root dir]
  (try
    (let [r (.getCanonicalPath (io/file root))
          d (.getCanonicalPath (io/file dir))]
      (or (= r d) (str/starts-with? d (str r java.io.File/separator))))
    (catch Throwable _ false)))

(defn- managed-under
  "The vis-managed index restricted to REPLs at or below `root` (the registry
   is global; this keeps workspaces isolated)."
  [root]
  (into {}
    (filter (fn [[_ info]] (under-root? root (:dir info))))
    (repl-manager/managed-ports)))

(defn- all-hits
  "Union of file-discovered ports and vis-managed ports (already scoped to this
   root). Managed REPLs in a subdir aren't reachable by workspace-root discovery
   (which only walks UP), so we add them explicitly with a synthesized source so
   they still surface — including after a vis restart."
  [root managed]
  (let [discovered (vec (ports/discover-all root))
        seen       (set (map :port discovered))]
    (into discovered
      (for [[port info] managed
            :when       (not (seen port))]
        {:port port
         :source (paths/unixify (io/file (:dir info) ".nrepl-port"))}))))

(defn- repl-pid
  "Ask the nREPL on `port` for its OWN OS pid - pure JVM, exact, no lsof. The
   server evaluates `(.pid (ProcessHandle/current))` for us, so it works for any
   nREPL we can reach (managed, `./bin/dev nrepl`, a Calva jack-in). nil on
   failure; never throws."
  [port]
  (try
    (let [r (nrepl-client/eval! {:host "localhost" :port port
                                 :code "(.pid (java.lang.ProcessHandle/current))"
                                 :timeout-ms 1000})
          v (some-> (:value r) str str/trim not-empty)]
      (when v (Long/parseLong v)))
    (catch Throwable _ nil)))

(defn- shutdown-repl!
  "Pure-JVM stop for a REPL we discovered but did NOT spawn: ask the nREPL to
   exit its own JVM (deferred a beat so the eval response still returns first).
   No lsof, no kill, cross-platform - works wherever we can open the nREPL
   socket. Best-effort; never throws."
  [port]
  (try
    (nrepl-client/eval! {:host "localhost" :port port
                         :code "(do (future (Thread/sleep 150) (System/exit 0)) :vis/stopping)"
                         :timeout-ms 1500})
    true
    (catch Throwable _ nil)))

(defn- repl-resource-id
  "Stable session-resource id for the REPL rooted at `dir` - matches the id
   `core/register-repl-resource!` uses on start, so vis-managed and synced
   entries never double up."
  [dir]
  (str "nrepl:" dir))

(defn- ensure-resource!
  "Idempotently mirror ONE live nREPL hit into `session`'s resource registry so
   the footer badge + stop/restart dialog see it regardless of which session
   started the REPL. No-op unless the port is live (`:up`) and not already
   registered. Vis-managed REPLs (`mgd` from `managed-ports`) get stop+restart
   thunks via repl-manager; externally-started ones (e.g. `./bin/dev nrepl`, a
   Calva jack-in) get a PURE-JVM stop-only thunk that tells the nREPL to exit
   its own JVM - no restart, since we don't know their launch command."
  [session {:keys [port source]} mgd status]
  (when (and session port (= :up (:status status)))
    (let [dir (or (:dir mgd) (source-dir source))
          id  (repl-resource-id dir)]
      (when (and dir (nil? (vis/get-resource session id)))
        (if mgd
          (let [aliases (:aliases mgd)]
            (vis/register-resource! session
              {:id     id
               :kind   :nrepl
               :language :clojure
               :label  (str "nREPL " (.getName (io/file dir))
                         (when (seq aliases) (apply str (map #(str " :" (name %)) aliases))))
               :status :up
               ;; STRING-keyed `:detail` — resources.clj/->data passes it through
               ;; verbatim, so it must be boundary-safe already.
               :detail (cond-> {"dir" dir "port" port}
                         (seq aliases) (assoc "aliases" (mapv name aliases)))
               :pid    (:pid mgd)
               :owner  :ext/language-clojure}
              {:stop-fn    (fn [] (repl-manager/stop! dir))
               :restart-fn (fn []
                             (repl-manager/stop! dir)
                             (let [r (repl-manager/start! dir {:aliases aliases})]
                               (vis/unregister-resource! session id)
                               r))}))
          (let [pid (repl-pid port)]
            (vis/register-resource! session
              {:id     id
               :kind   :nrepl
               :language :clojure
               :label  (str "nREPL " (.getName (io/file dir)) " (external)")
               :status :up
               :detail {"dir" dir "port" port "external" true}
               :pid    pid
               :owner  :ext/language-clojure}
              {:stop-fn (fn [] (shutdown-repl! port))})))))))

(defn- sync-session-resources!
  "Best-effort: mirror every live workspace nREPL into the current session's
   resource registry so another session's (or an external) REPL still shows the
   footer badge + stop/restart dialog HERE. Idempotent - skips ports already
   registered - so it is cheap to call every render. Never throws."
  [session hits statuses managed]
  (when session
    (doseq [{:keys [port] :as hit} hits]
      (try
        (ensure-resource! session hit (get managed port) (get statuses port {:status :unknown}))
        (catch Throwable e
          (tel/log! {:level :warn :id ::sync-resource-failed
                     :data {:port port :error (ex-message e)}}
            "Failed to mirror nREPL port into the session resource registry"))))))

(defn contribute
  "`:ext/ctx-fn` fn. Returns the `{\"session_env\" {\"languages\" {\"clojure\"
   {\"nrepl\" ...}}}}` slice (STRING-keyed — the contribution contract key is
   \"session_env\", and the env tree crosses the strings-only boundary), and
   MIRRORS every live nREPL into the session resource registry so the TUI footer
   / F4 dialog show externally-started REPLs too, or `{}` when no workspace root
   is on env. Never throws — degrades to an empty contribution."
  [env]
  (try
    (if-let [root (:workspace/root env)]
      (let [managed  (managed-under root)
            hits     (all-hits root managed)
            host     "localhost"
            statuses (when (seq hits)
                       (liveness-for host (current-turn env) (map :port hits)))]
        ;; Register the live REPLs as session resources → footer badge + F4 dialog,
        ;; so a REPL the user (or another session) started shows in the footer here.
        (sync-session-resources! (:session-id env) hits statuses managed)
        ;; The contribution is STRING-keyed all the way from the top:
        ;; ctx_loop/enrich-ctx + env_digest/extension-contributions now read the
        ;; top-level key as "session_env" (a keyword :session/env is silently
        ;; dropped). Its value is the env tree that crosses via ->py, so
        ;; "languages"/"clojure"/"nrepl" are strings too — matching the
        ;; string-keyed env tree the core workspace-ctx already landed.
        {"session_env" {"languages" {"clojure" {"nrepl" (nrepl-block hits statuses managed)}}}})
      {})
    (catch Throwable e
      (tel/log! {:level :warn :id ::contribute-failed :data {:error (ex-message e)}}
        "Clojure nREPL ctx contribution failed; degrading to empty")
      {})))
