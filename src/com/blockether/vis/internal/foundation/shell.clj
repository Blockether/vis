(ns com.blockether.vis.internal.foundation.shell
  "`shell/` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature), gated behind the user-owned `:shell/enabled` toggle
   (OFF by default; every call short-circuits into a refusal envelope until the
   user flips it in settings).

   Three model-facing bindings under alias `shell` (the flat Python sandbox
   renders `alias/name` as `<alias>_<name>`, so the calls are
   `shell_run` / `shell_bg` / `shell_logs` — same shape as `git_status`,
   `clj_eval`, `search_web`):

   1. SYNC `shell_run(cmd)` / `shell_run(cmd, opts)` — `bash -lc` in the
      workspace root, waits up to a timeout, and returns a LEAN payload
      {:cmd :stdout :duration_ms} + conditional keys (:exit when finished;
      :timed_out/:timeout_secs on timeout; :stderr when non-empty; truncation
      flags when true; :cwd when narrowed) — results ride every later prompt
      as frozen <results> pins, so dead fields never ship. Output is bounded
      at read time to a head+tail budget per stream — only the MIDDLE of a huge
      stream is dropped, never its start or end (memory can't balloon on a
      chatty-then-killed command). A non-zero exit is DATA the
      model reads, not a tool error.

   2. BACKGROUND `shell_bg(id, cmd)` — spawns the process, pumps its merged
      output into a bounded ring buffer, and registers it as a session
      RESOURCE in `internal.resources`: it shows up in the footer count, the
      F4 dialog, and the `resources` ctx block, and the ONE stop path
      is `resource_stop(id)` (model) / the footer dialog (user) — both land
      on `resources/stop!`, which runs our `:stop-fn` (process-tree kill +
      buffer drop). An exited process is NOT auto-pruned (its `:alive-fn`
      reports true while the buffer entry exists) so `shell_logs(id)` can
      still read its output + exit code until the resource is stopped.

   3. `shell_logs(id)` / `shell_logs(id, n)` — tail of a background shell's
      captured output as `[seq, line]` tuples plus status/exit/uptime.

   The `:shell/enabled` toggle is registered HERE, extension-owned under the
   extension's own namespace."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.resources :as resources]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.internal.paths :as paths]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.io File)
   (java.lang ProcessHandle)
   (java.util.concurrent TimeUnit)))

;; =============================================================================
;; Toggle (extension-owned)
;; =============================================================================

(toggles/register-toggle!
 {:id          :shell/enabled
  :label       "Shell commands (compatibility layer)"
  :description (str "When ON the agent can run shell commands from the"
                    " sandbox: sync shell_run(cmd) plus background"
                    " shell_bg(id, cmd) registered as session resources"
                    " (footer count, F4 dialog, resource_stop). OFF by"
                    " default - every shell call is refused with a hint"
                    " until you enable it.")
  :default     false
  :owner       "foundation-shell"
  :group       :tools
  :persist?    true})

;; =============================================================================
;; Limits
;; =============================================================================

(def ^:private default-timeout-secs 120)
(def ^:private max-timeout-secs 600)
(def ^:private max-sync-head-chars
  "Prefix of a SYNC stream always kept: the command's OPENING context —
   compile errors, the first failing assertion, the banner that says WHAT ran."
  4000)
(def ^:private max-sync-tail-chars
  "Suffix of a SYNC stream always kept: build / test failures and the final
   summary live at the END. Only the MIDDLE is dropped when a stream is huge —
   never the head, never the tail — so nothing important silently disappears."
  12000)
(def ^:private max-bg-lines
  "Ring-buffer cap per background shell; older lines are dropped (counted)."
  2000)
(def ^:private max-line-chars
  "Per-line char cap in the background pump. A newline-free stream (e.g.
   `cat big.bin`) would otherwise let a line builder grow one unbounded line
   in memory; we force a break at this width instead."
  16000)
(def ^:private default-log-tail 200)

(defn- now-ms [] (System/currentTimeMillis))

;; =============================================================================
;; Small helpers
;; =============================================================================

(defn- opt-get
  "Read an option from a GraalPy-crossed dict. The boundary keywordizes dict
   keys snake-verbatim (:timeout_secs), but be lenient to raw string keys."
  [opts k sk]
  (when (map? opts) (or (get opts k) (get opts sk))))

(defn- read-capped
  "Drain a Reader keeping the HEAD and the TAIL of the stream, dropping only the
   MIDDLE when output exceeds `head-limit`+`tail-limit` — so neither the opening
   context nor the closing failure/summary is ever silently lost (the old
   tail-only cap swallowed everything before the last N chars). When truncated a
   visible omitted-count marker is spliced in at the boundary. Bounded memory:
   the middle is collapsed at read time, so a megabyte-then-killed command can't
   balloon the heap. Returns {:text :truncated}. Never throws: a stream closed
   mid-read (the timeout/stop path closes it) just ends the drain."
  [^java.io.Reader r head-limit tail-limit]
  (let [sb    (StringBuilder.)
        buf   (char-array 8192)
        cap   (+ head-limit tail-limit)
        total (volatile! 0)
        trunc (volatile! false)]
    (try
      (loop []
        (let [n (.read r buf 0 (alength buf))]
          (when (pos? n)
            (vswap! total + n)
            (.append sb buf 0 n)
            (when (> (.length sb) cap)
              (vreset! trunc true)
              ;; keep the first `head-limit` chars + the last `tail-limit`;
              ;; excise the run between them so memory stays at ~cap.
              (.delete sb head-limit (- (.length sb) tail-limit)))
            (recur))))
      (catch Throwable _ nil))
    {:text      (if @trunc
                  (str (subs (.toString sb) 0 head-limit)
                       "\n\n…[" (- @total cap) " chars omitted]…\n\n"
                       (subs (.toString sb) head-limit))
                  (.toString sb))
     :truncated @trunc}))

(defn- ->pos-long
  "Coerce a GraalPy-crossed numeric option to a long (floats round), or throw a
   typed error. nil passes through (caller supplies the default). Rejects
   strings/other types with a clean message instead of a raw ClassCastException
   surfacing as an opaque throwable envelope."
  [x what]
  (cond
    (nil? x)    nil
    (number? x) (long (Math/round (double x)))
    :else       (throw (ex-info (str what " must be a number, got " (pr-str x) ".")
                                {:type ::bad-option :option what :value x}))))

(defn- one-line
  "Collapse a command to a single display line capped at `limit` chars."
  [s limit]
  (let [s (-> (str s) (str/replace #"\s+" " ") str/trim)]
    (if (> (count s) limit) (str (subs s 0 limit) "…") s)))

(defn- resolve-cwd
  "Directory the command runs in: the workspace root (bound per-call by the
   extension wrapper), optionally narrowed by a RELATIVE opts `cwd` that must
   stay inside the root — same containment rule the editing tools enforce."
  ^File [opts]
  (let [root (.getCanonicalFile (workspace/cwd))
        rel  (let [c (opt-get opts :cwd "cwd")]
               (when-not (str/blank? (str (or c ""))) (str c)))]
    (if-not rel
      root
      (let [dir (.getCanonicalFile (io/file root rel))]
        (when-not (or (= dir root)
                      (str/starts-with? (.getPath dir)
                                        (str (.getPath root) File/separator)))
          (throw (ex-info (str "shell cwd '" rel "' escapes the workspace root;"
                               " relative paths must stay inside it.")
                          {:type ::cwd-outside-workspace :cwd rel :root (.getPath root)})))
        (when-not (.isDirectory dir)
          (throw (ex-info (str "shell cwd '" rel "' "
                               (if (.exists dir) "is a file, not a directory."
                                   "does not exist under the workspace root."))
                          {:type ::cwd-not-a-directory :cwd rel :exists (.exists dir)})))
        dir))))

(defn- lf
  "Normalize CRLF to LF so captured output is byte-identical on every OS."
  ^String [^String s]
  (when s (.replace s "\r\n" "\n")))

(defn- windows?* []
  (str/starts-with? (str/lower-case (System/getProperty "os.name" "")) "win"))

(defn- find-git-bash
  "Absolute path to a REAL bash on Windows (Git for Windows), or nil. NEVER the
   WSL launcher at System32\\bash.exe — with no distro installed it merely
   prints \"Windows Subsystem for Linux has no installed distributions\" and
   exits, so a bare `bash` lookup on PATH is a trap. Resolves a `VIS_BASH`
   override first, then the standard Git install roots, then bash alongside a
   `git.exe` found on PATH (Git\\cmd\\git.exe → Git\\bin\\bash.exe)."
  []
  (let [path-sep  (System/getProperty "path.separator" ";")
        from-path (for [dir (str/split (or (System/getenv "PATH") "")
                                       (re-pattern (java.util.regex.Pattern/quote path-sep)))
                        :when (not (str/blank? dir))
                        :let  [git (io/file dir "git.exe")]
                        :when (.isFile git)
                        :let  [root (some-> git .getParentFile .getParentFile)
                               bash (when root (io/file root "bin" "bash.exe"))]
                        :when (and bash (.isFile bash))]
                    (.getPath bash))
        roots     (keep identity [(System/getenv "ProgramFiles")
                                  (System/getenv "ProgramW6432")
                                  (System/getenv "ProgramFiles(x86)")
                                  (some-> (System/getenv "LOCALAPPDATA") (str "\\Programs"))])
        candidates (concat
                    (when-let [o (System/getenv "VIS_BASH")] [o])
                    (map #(str % "\\Git\\bin\\bash.exe") roots)
                    from-path)]
    (some #(when (and (not (str/blank? %)) (.isFile (io/file %))) %) candidates)))

(defn- bash-command
  "Bash executable to run commands with — bash on EVERY platform, so the model
   writes one command dialect everywhere.

   - Under WSL the JVM reports `os.name=Linux`, so this takes the POSIX branch
     and uses WSL's own real `bash` (correct — we're a Linux process there).
   - On native Windows, resolve Git Bash and NEVER the `System32\\bash.exe` WSL
     stub: with no distro it just errors, and even with one it would switch us
     into a WSL filesystem context and break Windows path handling.
   - Everywhere else, a bare `bash` on PATH is correct."
  []
  (if (windows?*) (or (find-git-bash) "bash") "bash"))

(defn- spawn!
  ^Process [cmd ^File dir merge-err?]
  (let [pb (ProcessBuilder. ^java.util.List [(bash-command) "-lc" (str cmd)])]
    (.directory pb dir)
    (when merge-err? (.redirectErrorStream pb true))
    (.start pb)))

(defn- kill-tree!
  "Destroy a process and every descendant the JVM can still REACH via
   `ProcessHandle.descendants`: polite destroy first, then a forced pass after
   a 2s grace. Never throws (teardown path). NOTE: a deliberately-detaching
   child (`setsid`/double-fork/`nohup … &`) reparents to init and escapes this
   reach — for those the registry still drops cleanly and the pump is unblocked
   by closing the pipe in the stop-fn, but the orphan keeps running."
  [^Process p]
  (try
    (let [h (.toHandle p)]
      (run! (fn [^ProcessHandle d] (.destroy d))
            (-> h .descendants .iterator iterator-seq))
      (.destroy h)
      (when-not (try (.waitFor p 2 TimeUnit/SECONDS)
                     (catch InterruptedException _
                       (.interrupt (Thread/currentThread))
                       false))
        (run! (fn [^ProcessHandle d] (.destroyForcibly d))
              (-> h .descendants .iterator iterator-seq))
        (.destroyForcibly h)))
    (catch Throwable _ nil))
  nil)

;; =============================================================================
;; SYNC shell_run
;; =============================================================================

(defn- clamp-timeout-secs
  "Effective sync timeout from the opts value: default 120, floor 1, cap 600."
  ^long [v]
  (-> (or (->pos-long v "timeout_secs") default-timeout-secs)
      (max 1)
      (min max-timeout-secs)))

(defn- shell-run-impl
  ([env cmd] (shell-run-impl env cmd nil))
  ([_env cmd opts]
   (let [cmd (str cmd)]
     (when (str/blank? cmd)
       (throw (ex-info "shell_run needs a non-blank command string."
                       {:type ::blank-command})))
     (let [timeout-secs (clamp-timeout-secs (opt-get opts :timeout_secs "timeout_secs"))
           cwd-opt?     (not (str/blank? (str (or (opt-get opts :cwd "cwd") ""))))
           dir   (resolve-cwd opts)
           t0    (now-ms)
           p     (spawn! cmd dir false)
           empty-tail {:text "" :truncated false}
           ;; Separate reader futures per stream — avoids the classic full-pipe
           ;; deadlock on chatty commands. `read-capped` bounds memory to the
           ;; head+tail budget per stream at READ time (dropping only the MIDDLE
           ;; of a huge stream, not its start), so a megabyte-then-killed command
           ;; can't balloon the heap yet the opening context survives.
           out-f (future (read-capped (io/reader (.getInputStream p)) max-sync-head-chars max-sync-tail-chars))
           err-f (future (read-capped (io/reader (.getErrorStream p)) max-sync-head-chars max-sync-tail-chars))
           finished? (try (.waitFor p timeout-secs TimeUnit/SECONDS)
                          (catch InterruptedException ie
                         ;; Turn cancellation: kill the spawned tree before
                         ;; the interrupt propagates to the loop.
                            (kill-tree! p)
                            (throw ie)))]
       (when-not finished?
         (kill-tree! p)
         ;; Closing the streams unblocks the reader futures on a wedged child
         ;; so their threads don't linger past our 5s deref ceiling.
         (doseq [^java.io.InputStream s [(.getInputStream p) (.getErrorStream p)]]
           (try (.close s) (catch Throwable _ nil))))
       (let [out  (deref out-f 5000 empty-tail)
             err  (deref err-f 5000 empty-tail)
             exit (when finished? (.exitValue p))
             t1   (now-ms)]
         (extension/success
           ;; Lean result: this map rides every later prompt as a frozen
           ;; <results> pin, so optional keys appear ONLY when they carry
           ;; signal (model reads them with .get). :op / echoes of the call
           ;; args (cwd default, timeout default) never ship.
          {:result (cond-> {:cmd cmd
                            :stdout (lf (:text out))
                            :duration_ms (- t1 t0)}
                     finished?         (assoc :exit exit)
                     (not finished?)   (assoc :timed_out true
                                              :timeout_secs timeout-secs)
                     (:truncated out)  (assoc :stdout_truncated true)
                     (not (str/blank? (:text err))) (assoc :stderr (lf (:text err)))
                     (:truncated err)  (assoc :stderr_truncated true)
                      ;; Relative cwd is `/`-separated on every OS (Windows `\`).
                     cwd-opt?          (assoc :cwd (paths/unixify (.getPath dir))))
           :op :shell/run
           :metadata {:command cmd
                      :exit exit
                      :timed-out? (not finished?)
                      :started-at-ms t0
                      :finished-at-ms t1
                      :duration-ms (- t1 t0)}}))))))

;; =============================================================================
;; BACKGROUND shell_bg — session resources
;; =============================================================================

(defonce ^:private bg-procs
  ;; { session-key -> { id -> {:proc :buffer :exit :pump :stopped? :cmd :cwd
  ;; :started-at} } }. defonce so a dev `:reload` never orphans live processes.
  (atom {}))

(defn- bg-entry [session id]
  (get-in @bg-procs [(str session) (str id)]))

(defn- drop-bg-entry! [session id]
  (let [sk (str session) id (str id)]
    (swap! bg-procs (fn [m]
                      (let [m (update m sk dissoc id)]
                        (if (empty? (get m sk)) (dissoc m sk) m))))
    nil))

(defn- push-line! [buffer line]
  ;; A char-pump split on `\n` leaves the `\r` of a CRLF line behind; strip it
  ;; so a Windows-emitted line reads identically to a POSIX one.
  (let [line (if (and (string? line) (str/ends-with? line "\r"))
               (subs line 0 (dec (count line)))
               line)]
    (swap! buffer
           (fn [{:keys [lines next-seq dropped]}]
             (let [lines (conj lines [next-seq line])
                   over  (- (count lines) max-bg-lines)]
               {:lines    (if (pos? over) (subvec lines over) lines)
                :next-seq (inc next-seq)
                :dropped  (+ dropped (max over 0))})))))

(defn- start-pump!
  "Daemon thread: drain the process's merged output into the ring buffer,
   then record the exit code and flip the registered resource to :exited.
   The resource stays listed (logs + exit readable) until resource_stop.

   `stopped?` is the cooperative-shutdown flag the stop-fn sets BEFORE it
   unregisters the resource: once set, the pump does NOT call
   `resources/update!`. That matters because `resources/update!` guards with
   a non-atomic `get-in` then `update-in` — calling it after `unregister!`
   would resurrect a partial `:data`-only entry (TOCTOU). The stop-fn also
   joins this thread, so on a manual stop the pump has fully drained before
   the resource is removed. Returns the started Thread."
  ^Thread [session id ^Process p buffer exit-atom stopped?]
  (doto (Thread.
         (fn []
            ;; Char-level drain (not `line-seq`) so a newline-free stream
            ;; (`cat big.bin`) can't grow one unbounded line in memory: a line
            ;; is force-flushed at `max-line-chars`.
           (try
             (with-open [r (io/reader (.getInputStream p))]
               (let [sb (StringBuilder.)]
                 (loop []
                   (let [c (.read r)]
                     (cond
                       (= c -1)
                       (when (pos? (.length sb)) (push-line! buffer (str sb)))

                       (= c (int \newline))
                       (do (push-line! buffer (str sb)) (.setLength sb 0) (recur))

                       :else
                       (do (.append sb (char c))
                           (when (>= (.length sb) max-line-chars)
                             (push-line! buffer (str sb " …[line truncated]"))
                             (.setLength sb 0))
                           (recur)))))))
             (catch Throwable _ nil))
           (let [code (try (.waitFor p) (catch Throwable _ nil))]
             (reset! exit-atom code)
             (when-not @stopped?
               (try
                 (resources/update! session id
                                    {:status :exited
                                     :detail (str "exit " code
                                                  " — logs retained until resource_stop")})
                 (catch Throwable _ nil))))))
    (.setName (str "vis-shell-bg-" id))
    (.setDaemon true)
    (.start)))

(defn- shell-bg-impl
  [env id cmd]
  (let [session (:session-id env)
        id      (str id)
        cmd     (str cmd)]
    (when (str/blank? id)
      (throw (ex-info "shell_bg needs a non-blank resource id (first arg)."
                      {:type ::blank-id})))
    (when (str/blank? cmd)
      (throw (ex-info "shell_bg needs a non-blank command string (second arg)."
                      {:type ::blank-command})))
    (when-let [existing (bg-entry session id)]
      (if (.isAlive ^Process (:proc existing))
        (throw (ex-info (str "Background shell '" id "' is already running (pid "
                             (.pid ^Process (:proc existing))
                             "); resource_stop it first or pick a new id.")
                        {:type ::bg-id-in-use :id id}))
        ;; Exited-but-unread entry under the same id: replacing it discards
        ;; its retained logs by intent (the model chose to reuse the id).
        (do (resources/unregister! session id)
            (drop-bg-entry! session id))))
    (let [dir       (resolve-cwd nil)
          p         (spawn! cmd dir true)
          buffer    (atom {:lines [] :next-seq 1 :dropped 0})
          exit-atom (atom nil)
          stopped?  (atom false)
          t0        (now-ms)
          pump      (start-pump! session id p buffer exit-atom stopped?)]
      (swap! bg-procs assoc-in [(str session) id]
             {:proc p :buffer buffer :exit exit-atom :pump pump :stopped? stopped?
              :cmd cmd :cwd (.getPath dir) :started-at t0})
      (resources/register! session
                           {:id id
                            :kind :shell
                            :label (one-line cmd 48)
                            :detail cmd
                            :pid (.pid p)
                            :owner "foundation-shell"
                            :status :running}
                           {:stop-fn  (fn []
                     ;; Tell the pump to stop touching the registry, kill the
                     ;; tree, then wait for the pump to finish draining BEFORE
                     ;; the registry drops the resource — so the pump can never
                     ;; resurrect a partial entry after unregister.
                                        (reset! stopped? true)
                                        (kill-tree! p)
                     ;; Close the read end so the pump's blocking `.read`
                     ;; returns even if a detached grandchild still holds the
                     ;; write end — the pump thread can't outlive the stop.
                                        (try (.close (.getInputStream p)) (catch Throwable _ nil))
                                        (try (.join pump 3000) (catch InterruptedException _ nil))
                                        (drop-bg-entry! session id))
         ;; Alive while the buffer entry exists — an EXITED process is kept
         ;; (status :exited) so its logs stay readable; only resource_stop
         ;; (or replacing the id) lets the registry drop it.
                            :alive-fn (fn [] (some? (bg-entry session id)))})
      (extension/success
        ;; No :op / :cwd — shell_bg always runs at the workspace root and the
        ;; result rides every later prompt as a frozen <results> pin.
       {:result {:id id
                 :pid (.pid p)
                 :cmd cmd
                 :status "running"}
        :op :shell/bg
        :metadata {:command cmd
                   :pid (.pid p)
                   :started-at-ms t0
                   :finished-at-ms t0
                   :duration-ms 0}}))))

(defn- shell-logs-impl
  ([env id] (shell-logs-impl env id default-log-tail))
  ([env id n]
   (let [session (:session-id env)
         id      (str id)
         entry   (bg-entry session id)]
     (when-not entry
       (throw (ex-info (str "No background shell '" id "' in this session —"
                            " start one with shell_bg(id, cmd); live ids are"
                            " listed in resources.")
                       {:type ::unknown-bg-id :id id})))
     (let [n     (-> (or (->pos-long n "n") default-log-tail) (max 1) (min max-bg-lines))
           {:keys [lines dropped next-seq]} @(:buffer entry)
           total (dec next-seq)
           shown (if (> (count lines) n) (subvec lines (- (count lines) n)) lines)
           exit  @(:exit entry)
           t     (now-ms)]
       (extension/success
         ;; Lean result: no :op / :cmd / :cwd / :pid (the shell_bg result
         ;; already carries process identity) and no :shown_count (it's
         ;; len(lines)). :exit only once exited, :dropped only when the ring
         ;; buffer actually evicted — absent keys read as None via .get.
        {:result (cond-> {:id id
                          :status (if (some? exit) "exited" "running")
                          :lines shown
                          :line_count total
                          :uptime_ms (- t (:started-at entry))}
                   (some? exit)    (assoc :exit exit)
                   (pos? (long (or dropped 0))) (assoc :dropped dropped))
         :op :shell/logs
         :metadata {:id id
                    :started-at-ms t
                    :finished-at-ms t
                    :duration-ms 0}})))))

;; =============================================================================
;; Toggle gate + env injection (one before-fn does both)
;; =============================================================================

(defn- op-label
  "Human call name from a namespaced op keyword: :shell/run -> \"shell_run\"."
  [op]
  (if (namespace op) (str (namespace op) "_" (name op)) (name op)))

(def ^:private disabled-hint
  (str "The shell layer is OFF. Only the USER can enable it: settings dialog ->"
       " 'Shell commands (compatibility layer)' toggle. Tell the user instead of"
       " retrying; use cat/ls/rg/patch/write for file work meanwhile."))

(defn- shell-gate-before-fn
  "Compose the `:shell/enabled` toggle gate with env injection: when the
   toggle is ON the underlying impl receives `env` as its first arg (the
   model never sees it); when OFF the call short-circuits into a refusal
   envelope the loop surfaces as a readable tool error."
  [op]
  (fn [env f args]
    (if (toggles/enabled? :shell/enabled)
      {:env env :fn f :args (into [env] args)}
      (let [t (now-ms)]
        {:result (extension/failure
                  {:result nil
                   :op op
                   :metadata {:started-at-ms t :finished-at-ms t :duration-ms 0}
                   :error {:message (str (op-label op) " blocked: " disabled-hint)
                           :type ::disabled
                           :reason :shell-disabled
                           :hint disabled-hint
                           :loop-hint disabled-hint}})}))))

(defn- shell-on-error
  "Failure envelope for thrown impl errors; mirrors editing's interrupted-vs-
   throwable split so turn cancellation renders as a clean interruption."
  [op]
  (fn [err _env _f _args]
    (let [interrupted? (instance? InterruptedException err)
          t (now-ms)]
      {:result (extension/failure
                {:result nil
                 :op op
                 :metadata (cond-> {:started-at-ms t
                                    :finished-at-ms t
                                    :duration-ms 0}
                             interrupted? (assoc :interrupted? true
                                                 :status :interrupted))
                 :error (when interrupted?
                          {:message (str (op-label op) " interrupted while running;"
                                         " the spawned process tree was killed.")})
                 :throwable (when-not interrupted? err)})})))

;; =============================================================================
;; Public, doc-bearing vars — `:doc`/`:arglists` are the model-facing surface
;; (read by `vis/symbol` straight off the var); the injected `env` first arg
;; is hidden from both. Under alias `shell` they bind as `shell_run` /
;; `shell_bg` / `shell_logs`.
;; =============================================================================

(def ^{:doc "await shell_run(\"git status\")
await shell_run(\"npm run build\", {\"timeout_secs\": 300, \"cwd\": \"web\"})

Run a command via bash -lc in the workspace root.
Returns {\"cmd\", \"stdout\", \"duration_ms\"} plus, only when meaningful (use .get): \"exit\", \"timed_out\"+\"timeout_secs\", \"stderr\", \"stdout_truncated\"/\"stderr_truncated\", \"cwd\".
opts: {\"timeout_secs\": N (default 120, max 600), \"cwd\": rel-dir-inside-workspace}.
Gotcha: a non-zero \"exit\" is DATA to read, not a tool failure. On timeout there is NO \"exit\" key."
       :arglists '([cmd] [cmd opts])}
  shell-run shell-run-impl)

(def ^{:doc "await shell_bg(\"dev-server\", \"npm run dev\")

Start a background command (bash -lc, workspace root) as a session resource `id`; no timeout — use for daemons / watchers / long builds.
Returns {\"id\", \"pid\", \"cmd\", \"status\": \"running\"}.
Read output with shell_logs(id); stop and discard logs with resource_stop(id) — the ONLY stop path.
Gotcha: `id` must be unique among RUNNING shells; reusing an exited id discards its retained logs."
       :arglists '([id cmd])}
  shell-bg shell-bg-impl)

(def ^{:doc "await shell_logs(\"dev-server\")
await shell_logs(\"dev-server\", 500)

Tail a background shell's captured output. shell_logs(id) keeps the last 200 lines, shell_logs(id, n) the last n (max 2000).
Returns {\"id\", \"status\": \"running\"|\"exited\", \"lines\": [[seq, text], ...], \"line_count\", \"uptime_ms\"} plus, only when meaningful (use .get): \"exit\", \"dropped\".
Gotcha: \"lines\" is [seq, text] pairs (not strings); shown count is len(lines), \"line_count\" is total-ever."
       :arglists '([id] [id n])}
  shell-logs shell-logs-impl)

;; =============================================================================
;; Native op-card renderers — `:result` → `{:summary :body}`. Keys arrive
;; keywordized snake_case; the injected env first arg is already gone.
;; =============================================================================

(defn- fence
  "Wrap `s` in a plain code fence, or nil when blank."
  [s]
  (when (seq (str s)) (str "```\n" s "\n```")))

(defn- render-shell-run-result
  "shell_run → `$ <cmd>` headline (with an exit/timeout note) + stdout / stderr
   code blocks."
  [r]
  (let [exit (:exit r)
        note (cond (:timed_out r)               " (timed out)"
                   (and exit (not (zero? exit))) (str " (exit " exit ")")
                   :else                         "")
        body (->> [(fence (:stdout r))
                   (when-let [e (fence (:stderr r))] (str "stderr:\n" e))]
                  (remove nil?)
                  (str/join "\n\n"))]
    {:summary (str "$ " (:cmd r) note)
     :body    (when (seq body) body)}))

(defn- render-shell-bg-result
  "shell_bg → `bg `<id>` started (pid N)` headline only."
  [r]
  {:summary (str "bg `" (:id r) "` " (or (:status r) "started")
                 (when-let [pid (:pid r)] (str " (pid " pid ")")))})

(defn- render-shell-logs-result
  "shell_logs → `<id> <status> — N lines` headline + the captured lines."
  [r]
  (let [lines (:lines r)
        text  (->> lines (map (fn [pair] (if (sequential? pair) (second pair) pair)))
                   (str/join "\n"))]
    {:summary (str "`" (:id r) "` " (or (:status r) "?") " — " (count lines) " lines"
                   (when-let [d (:dropped r)] (str " (" d " dropped)")))
     :body    (fence text)}))

;; =============================================================================
;; Symbols + prompt + extension. Alias `shell` → `shell_run` / `shell_bg` /
;; `shell_logs` in the flat Python sandbox.
;; =============================================================================

(def shell-run-symbol
  (vis/symbol #'shell-run
              {:symbol 'run
               :native-tool? true
               :name "shell_run"
               ;; shell_run(cmd, {opts}) — cmd positional, the rest an options dict.
               :call {:pos ["cmd"] :rest :opt}
               :render render-shell-run-result
               :color-role :tool-color/shell
               :schema {:type "object"
                        :properties {"cmd"          {:type "string" :description "Command line, run via bash -lc in the workspace root."}
                                     "timeout_secs" {:type "integer" :description "Sync timeout seconds (default 120, max 600)."}
                                     "cwd"          {:type "string" :description "Relative directory inside the workspace to run in."}}
                        :required ["cmd"]}
               :before-fn (shell-gate-before-fn :shell/run)
               :tag :mutation
               :on-error-fn (shell-on-error :shell/run)}))

(def shell-bg-symbol
  (vis/symbol #'shell-bg
              {:symbol 'bg
               :native-tool? true
               :name "shell_bg"
               ;; shell_bg(id, cmd) — both positional.
               :call {:pos ["id" "cmd"]}
               :render render-shell-bg-result
               :color-role :tool-color/shell
               :schema {:type "object"
                        :properties {"id"  {:type "string" :description "Unique resource id among RUNNING shells; read logs / stop by it."}
                                     "cmd" {:type "string" :description "Background command (bash -lc, workspace root); no timeout."}}
                        :required ["id" "cmd"]}
               :before-fn (shell-gate-before-fn :shell/bg)
               :tag :mutation
               :on-error-fn (shell-on-error :shell/bg)}))

(def shell-logs-symbol
  (vis/symbol #'shell-logs
              {:symbol 'logs
               :native-tool? true
               :name "shell_logs"
               ;; shell_logs(id, n?) — id positional, optional trailing n.
               :call {:pos ["id"] :opt-pos ["n"]}
               :render render-shell-logs-result
               :color-role :tool-color/shell
               :schema {:type "object"
                        :properties {"id" {:type "string" :description "The background shell's resource id."}
                                     "n"  {:type "integer" :description "Tail the last n lines (default 200, max 2000)."}}
                        :required ["id"]}
               :before-fn (shell-gate-before-fn :shell/logs)
               :tag :observation
               :on-error-fn (shell-on-error :shell/logs)}))

(def shell-symbols
  [shell-run-symbol shell-bg-symbol shell-logs-symbol])

(defn shell-prompt
  "Prompt fragment advertising the shell surface — ONLY while the toggle is
   ON (a blank string is filtered out of the extensions prompt block), so a
   disabled layer costs zero prompt tokens and the model never sees it."
  [_env]
  (if (toggles/enabled? :shell/enabled)
    (str/join "\n"
              ["Shell layer ENABLED. To run any command / build / test, call shell_run(...) — it returns a dict {exit, stdout, stderr, ...}. The callable is `shell_run`, NOT `shell` (plain `shell(...)` does not exist). Python subprocess.run / subprocess.Popen / os.system ALSO work (bridged to this tool — Popen runs in the BACKGROUND like shell_bg), but shell_run / shell_bg are cleaner — no exception dance."
               "  shell_run(\"make test\")                              # sync, bash -lc, workspace root"
               "  shell_run(\"npm run build\", {\"timeout_secs\": 300, \"cwd\": \"web\"})  # timeout default 120s, max 600s"
               "  r[\"stdout\"] always; r.get(\"exit\") / r.get(\"stderr\") / r.get(\"timed_out\") — optional keys ship only when meaningful; non-zero exit is DATA: read it, don't treat it as a tool failure."
               "  Background tasks are session RESOURCES (no timeout — use these for daemons / watch / long builds):"
               "  shell_bg(\"dev-server\", \"npm run dev\")              # registers resource 'dev-server' (see resources)"
               "  shell_logs(\"dev-server\")                            # tail captured output, [seq, line] pairs"
               "  resource_stop(\"dev-server\")                         # the ONE stop path (also discards logs)"
               "  Prefer cat/ls/rg/patch/write for file work — shell_run never replaces them."
               "  Commands run inside the workspace root only."])
    ""))

(def vis-extension
  (vis/extension
   {:ext/name        "foundation-shell"
    :ext/description "Shell compatibility layer (toggle :shell/enabled, OFF by default): sync shell_run(cmd) via bash -lc; background shell_bg(id, cmd) registered as a session resource (stop via resource_stop, footer + resources visibility); shell_logs(id) output tail."
    :ext/version     "0.1.0"
    :ext/author      "Blockether"
    :ext/owner       "vis"
    :ext/license     "Apache-2.0"
    :ext/kind        "foundation"
     ;; The toggle IS the activation gate: when OFF the extension is
     ;; inactive, so `sync-active-extension-symbols!` (per-env install +
     ;; every turn start) REMOVES shell_run/shell_bg/shell_logs from the
     ;; sandbox globals — `apropos` doesn't list them, calling raises a
     ;; plain NameError, and the prompt fragment is gone. The before-fn
     ;; refusal below stays as defense-in-depth for the same-turn window
     ;; where the toggle flips after symbols were already bound.
    :ext/activation-fn (fn [_env] (toggles/enabled? :shell/enabled))
    :ext/engine      {:ext.engine/alias   'shell
                      :ext.engine/symbols shell-symbols}
    :ext/prompt-fn      shell-prompt}))

(vis/register-extension! vis-extension)
