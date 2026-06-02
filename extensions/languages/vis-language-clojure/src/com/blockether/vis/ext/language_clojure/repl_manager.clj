(ns com.blockether.vis.ext.language-clojure.repl-manager
  "Flag-gated nREPL lifecycle for the Clojure pack.

   Self-starting a REPL spawns a real subprocess in the workspace. It is ON by
   default; set the `VIS_CLJ_REPL_AUTOSTART` flag (declared as `:ext/env` on the
   extension; resolvable from Vis config or the OS env) to a falsy value
   (0/false/no/off) to opt OUT.

   The REPL runs as a SUBPROCESS in the workspace root — never in-process —
   so `clj/eval` hits the project's own classpath (deps.edn / project.clj /
   bb.edn deps), not Vis's JVM. The chosen launcher writes `.nrepl-port` on
   boot, which `nrepl-ctx` discovery then surfaces in context automatically.

   `:status`/`:stop` are always allowed (read + cleanup of a Vis-managed proc).
   Only `:start`/`:restart` require the flag."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.ports :as ports])
  (:import
   (java.util.concurrent TimeUnit)))

(def flag-env "VIS_CLJ_REPL_AUTOSTART")

(def ^:private falsy #{"0" "false" "no" "off"})

(defn flag-enabled?
  "Self-start is ON by default. Set `VIS_CLJ_REPL_AUTOSTART` to a falsy value
   (0/false/no/off) via Vis config or OS env to disable it."
  []
  (let [raw (or (some-> (vis/extension-env-value flag-env) str)
              (some-> (System/getenv flag-env) str))]
    (if (str/blank? (str raw))
      true
      (not (contains? falsy (str/lower-case (str/trim raw)))))))

;; { workspace-root -> {:process ^Process :cmd [..] :tool kw :started-at ms} }
;; defonce so a `(require :reload)` during dev never orphans a live child.
(defonce ^:private processes (atom {}))

(defn- alive? [^Process p] (boolean (and p (.isAlive p))))

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
       {:tool :clj
        :cmd  ["clojure"
               "-Sdeps" (str "{:deps {nrepl/nrepl {:mvn/version \"" nrepl-version "\"}}}")
               (str "-M" (alias-suffix aliases)) "-m" "nrepl.cmdline"]}

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
   ALIVE Vis-managed REPL, keyed by the live port each one wrote. Lets ctx mark
   which ports vis owns (and surface subdir REPLs that workspace-root discovery
   would otherwise miss). Dead entries are skipped."
  []
  (into {}
    (keep (fn [[dir {:keys [^Process process tool aliases]}]]
            (when (alive? process)
              (when-let [port (read-port-file dir)]
                [port (cond-> {:managed true :dir dir}
                        tool          (assoc :tool tool)
                        (seq aliases) (assoc :aliases aliases)
                        process       (assoc :pid (try (.pid process) (catch Throwable _ nil))))])))
      @processes)))

(defn status
  "Current managed-process + discovered-port view for `dir`. Always safe."
  [dir]
  (let [{:keys [^Process process tool aliases]} (get @processes dir)]
    {:result        :status
     :dir           dir
     :flag-enabled? (flag-enabled?)
     :managed       (cond-> {:running (alive? process)}
                      tool         (assoc :tool tool)
                      (seq aliases) (assoc :aliases aliases)
                      process      (assoc :pid (try (.pid process) (catch Throwable _ nil))))
     :ports         (vec (ports/discover-all dir))}))

(defn start!
  "Self-start a project nREPL subprocess in `dir` with optional `:aliases`.
   Caller must enforce the flag.

   - If we already manage a live process for `dir` → :already-running.
   - If `dir` already has a LIVE external nREPL (its own `.nrepl-port` probes
     :up) → :already-running with `:external? true` (we don't double-start).
   - A stale `.nrepl-port` is cleared so we wait for the fresh one.

   Waits briefly for the fresh port; returns :started (with port + liveness)
   when it appears, else :starting (ctx surfaces it next turn)."
  ([dir] (start! dir nil))
  ([dir {:keys [aliases]}]
   (cond
     (alive? (:process (get @processes dir)))
     (assoc (status dir) :result :already-running)

     :else
     (let [existing (read-port-file dir)
           live?    (and existing
                      (= :up (:status (nrepl-client/probe! {:port existing :timeout-ms 500}))))]
       (cond
         live?
         {:result :already-running :external? true :dir dir :port existing
          :message "An nREPL is already running in this directory."}

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
                   proc (.start pb)]
               (swap! processes assoc dir
                 {:process proc :cmd cmd :tool tool :aliases (vec aliases)
                  :started-at (System/currentTimeMillis)})
               (let [port  (wait-for-port dir 15000)
                     probe (when port (nrepl-client/probe! {:port port :timeout-ms 500}))]
                 (cond-> {:tool tool :dir dir :aliases (vec aliases) :cmd cmd
                          :log  (.getAbsolutePath log)
                          :pid  (try (.pid proc) (catch Throwable _ nil))}
                   port       (assoc :result :started :port port :status (:status probe))
                   (not port) (assoc :result :starting
                                :message "nREPL launching; the port will appear in ctx shortly.")))))
           {:result  :no-launcher :dir dir
            :message "No deps.edn / project.clj / bb.edn in this directory to start an nREPL."}))))))

(defn stop!
  "Stop the Vis-managed nREPL subprocess for `dir` (graceful, then forced).
   No-op-safe when nothing is managed."
  [dir]
  (if-let [{:keys [^Process process]} (get @processes dir)]
    (do
      (when (alive? process)
        (.destroy process)
        (when-not (.waitFor process 3 TimeUnit/SECONDS)
          (.destroyForcibly process)))
      (swap! processes dissoc dir)
      {:result :stopped :dir dir})
    {:result  :not-managed :dir dir
     :message "No Vis-managed nREPL for this directory."}))
