(ns com.blockether.vis.internal.foundation.shell
  "Shell compatibility layer — bare `shell` / `shell_bg` / `shell_logs` Python
   functions, gated behind the user-owned `:vis/shell-tool` toggle (OFF by
   default; every call short-circuits into a refusal envelope until the user
   flips it in settings).

   Two execution modes:

   1. SYNC `shell(cmd)` / `shell(cmd, opts)` — `bash -lc` in the workspace
      root, waits (bounded by a timeout), returns the canonical payload
      {:exit :stdout :stderr :timed_out :duration_ms ...}. A non-zero exit
      is DATA the model reads, not a tool error.

   2. BACKGROUND `shell_bg(id, cmd)` — spawns the process, pumps its merged
      output into a bounded ring buffer, and registers it as a session
      RESOURCE in `internal.resources`: it shows up in the footer count, the
      F4 dialog, and the `session_resources` ctx block, and the ONE stop path
      is `resource_stop(id)` (model) / the footer dialog (user) — both land
      on `resources/stop!`, which runs our `:stop-fn` (process-tree kill +
      buffer drop). An exited process is NOT auto-pruned (its `:alive-fn`
      reports true while the buffer entry exists) so `shell_logs(id)` can
      still read its output + exit code until the resource is stopped.

   The module ships as the BUILT-IN extension `foundation-shell`
   (`builtin-extension-nses` in `internal.extension`), so symbols bind BARE
   into the sandbox and kebab names snake-case: `shell-bg` -> `shell_bg`."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.resources :as resources]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.io File)
   (java.lang ProcessHandle)
   (java.util.concurrent TimeUnit)))

;; =============================================================================
;; Limits
;; =============================================================================

(def ^:private default-timeout-secs 120)
(def ^:private max-timeout-secs 600)
(def ^:private max-sync-chars
  "Per-stream cap on what a SYNC result carries back to the model. The TAIL is
   kept (build/test failures live at the end of output)."
  16000)
(def ^:private max-bg-lines
  "Ring-buffer cap per background shell; older lines are dropped (counted)."
  2000)
(def ^:private max-line-chars
  "Per-line char cap in the background pump. A newline-free stream (e.g.
   `cat big.bin`) would otherwise let `line-seq`/a line builder grow a single
   unbounded line in memory; we force a break at this width instead."
  16000)
(def ^:private default-log-tail 200)
(def ^:private render-preview-chars
  "Display-pane cap per output block; the model payload keeps `max-sync-chars`."
  4000)

(defn- now-ms [] (System/currentTimeMillis))

;; =============================================================================
;; Small helpers
;; =============================================================================

(defn- opt-get
  "Read an option from a GraalPy-crossed dict. The boundary keywordizes dict
   keys snake-verbatim (:timeout_secs), but be lenient to raw string keys."
  [opts k sk]
  (when (map? opts) (or (get opts k) (get opts sk))))

(defn- tail-str
  "Keep at most `limit` TAIL chars of `s`. Returns {:text :truncated}."
  [s limit]
  (let [s (str (or s ""))]
    (if (<= (count s) limit)
      {:text s :truncated false}
      {:text (subs s (- (count s) limit)) :truncated true})))

(defn- read-tail
  "Drain a Reader keeping only the LAST `limit` chars — where build / test
   failures live — so memory stays bounded regardless of how much the command
   emits before it's killed. Returns {:text :truncated}. Never throws: a stream
   closed mid-read (the timeout/stop path closes it to unblock us) just ends
   the drain."
  [^java.io.Reader r limit]
  (let [sb  (StringBuilder.)
        buf (char-array 8192)
        trunc (volatile! false)]
    (try
      (loop []
        (let [n (.read r buf 0 (alength buf))]
          (when (pos? n)
            (.append sb buf 0 n)
            (when (> (.length sb) limit)
              (vreset! trunc true)
              (.delete sb 0 (- (.length sb) limit)))
            (recur))))
      (catch Throwable _ nil))
    {:text (.toString sb) :truncated @trunc}))

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

(defn- spawn!
  ^Process [cmd ^File dir merge-err?]
  (let [pb (ProcessBuilder. ^java.util.List ["bash" "-lc" (str cmd)])]
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
;; SYNC shell
;; =============================================================================

(defn- shell-impl
  ([env cmd] (shell-impl env cmd nil))
  ([_env cmd opts]
   (let [cmd (str cmd)]
     (when (str/blank? cmd)
       (throw (ex-info "shell needs a non-blank command string."
                {:type ::blank-command})))
     (let [timeout-secs (-> (or (->pos-long (opt-get opts :timeout_secs "timeout_secs")
                                  "timeout_secs")
                              default-timeout-secs)
                          (max 1)
                          (min max-timeout-secs))
           dir   (resolve-cwd opts)
           t0    (now-ms)
           p     (spawn! cmd dir false)
           empty-tail {:text "" :truncated false}
           ;; Separate reader futures per stream — avoids the classic full-pipe
           ;; deadlock on chatty commands. `read-tail` bounds memory to the
           ;; last `max-sync-chars` per stream at READ time (not just trimmed
           ;; after), so a megabyte-then-killed command can't balloon the heap.
           out-f (future (read-tail (io/reader (.getInputStream p)) max-sync-chars))
           err-f (future (read-tail (io/reader (.getErrorStream p)) max-sync-chars))
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
           {:result {:op :shell
                     :cmd cmd
                     :cwd (.getPath dir)
                     :exit exit
                     :timed_out (not finished?)
                     :timeout_secs timeout-secs
                     :stdout (:text out)
                     :stdout_truncated (:truncated out)
                     :stderr (:text err)
                     :stderr_truncated (:truncated err)
                     :duration_ms (- t1 t0)}
            :op :shell
            :metadata {:command cmd
                       :exit exit
                       :timed-out? (not finished?)
                       :started-at-ms t0
                       :finished-at-ms t1
                       :duration-ms (- t1 t0)}}))))))

;; =============================================================================
;; BACKGROUND shell — session resources
;; =============================================================================

(defonce ^:private bg-procs
  ;; { session-key -> { id -> {:proc :buffer :exit :cmd :cwd :started-at} } }
  ;; defonce so a dev `:reload` never orphans live background processes.
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
  (swap! buffer
    (fn [{:keys [lines next-seq dropped]}]
      (let [lines (conj lines [next-seq line])
            over  (- (count lines) max-bg-lines)]
        {:lines    (if (pos? over) (subvec lines over) lines)
         :next-seq (inc next-seq)
         :dropped  (+ dropped (max over 0))}))))

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
        {:result {:op :shell-bg
                  :id id
                  :pid (.pid p)
                  :cmd cmd
                  :cwd (.getPath dir)
                  :status "running"}
         :op :shell-bg
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
                         " listed in session_resources.")
                {:type ::unknown-bg-id :id id})))
     (let [n     (-> (or (->pos-long n "n") default-log-tail) (max 1) (min max-bg-lines))
           {:keys [lines dropped next-seq]} @(:buffer entry)
           total (dec next-seq)
           shown (if (> (count lines) n) (subvec lines (- (count lines) n)) lines)
           exit  @(:exit entry)
           t     (now-ms)]
       (extension/success
         {:result {:op :shell-logs
                   :id id
                   :cmd (:cmd entry)
                   :cwd (:cwd entry)
                   :status (if (some? exit) "exited" "running")
                   :exit exit
                   :pid (.pid ^Process (:proc entry))
                   :uptime_ms (- t (:started-at entry))
                   :lines shown
                   :shown_count (count shown)
                   :line_count total
                   :dropped dropped}
          :op :shell-logs
          :metadata {:id id
                     :started-at-ms t
                     :finished-at-ms t
                     :duration-ms 0}})))))

;; =============================================================================
;; Toggle gate + env injection (one before-fn does both)
;; =============================================================================

(def ^:private disabled-hint
  (str "The shell layer is OFF. Only the USER can enable it: settings dialog ->"
    " 'Shell commands (compatibility layer)' toggle. Tell the user instead of"
    " retrying; use cat/ls/rg/patch/write for file work meanwhile."))

(defn- shell-gate-before-fn
  "Compose the `:vis/shell-tool` toggle gate with env injection: when the
   toggle is ON the underlying impl receives `env` as its first arg (the
   model never sees it); when OFF the call short-circuits into a refusal
   envelope the loop surfaces as a readable tool error."
  [op]
  (fn [env f args]
    (if (toggles/enabled? :vis/shell-tool)
      {:env env :fn f :args (into [env] args)}
      (let [t (now-ms)]
        {:result (extension/failure
                   {:result nil
                    :op op
                    :metadata {:started-at-ms t :finished-at-ms t :duration-ms 0}
                    :error {:message (str (name op) " blocked: " disabled-hint)
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
                           {:message (str (name op) " interrupted while running;"
                                       " the spawned process tree was killed.")})
                  :throwable (when-not interrupted? err)})})))

;; =============================================================================
;; Channel renderers — {:summary :display} contract
;; =============================================================================

(defn- ir-code [s] [:c {} (str s)])
(defn- ir-strong [s] [:strong {} (str s)])
(defn- ir-code-block [lang body] [:code (cond-> {} lang (assoc :lang lang)) (str body)])
(defn- ir-p [& parts] (into [:p {}] parts))
(defn- ir-root [& blocks] (into [:ir {}] (remove nil? blocks)))

(defn- channel-render-shell
  [{:keys [cmd exit timed_out timeout_secs stdout stderr duration_ms]}]
  (let [status (if timed_out
                 (str "timeout after " timeout_secs "s")
                 (str "exit " exit))
        head   (str (one-line cmd 60) "  →  " status "  (" duration_ms " ms)")
        out    (:text (tail-str stdout render-preview-chars))
        err    (:text (tail-str stderr render-preview-chars))]
    {:summary {:left  (ir-strong "SHELL")
               :right (ir-code head)}
     :display (ir-root
                (ir-p (ir-strong "SHELL") "  " (ir-code head))
                (when-not (str/blank? out) (ir-code-block nil out))
                (when-not (str/blank? err) (ir-p (ir-strong "stderr")))
                (when-not (str/blank? err) (ir-code-block nil err)))}))

(defn- channel-render-shell-bg
  [{:keys [id pid cmd status]}]
  (let [head (str id " · pid " pid " · " status)]
    {:summary {:left  (ir-strong "SHELL-BG")
               :right (ir-code head)}
     :display (ir-root
                (ir-p (ir-strong "SHELL-BG") "  " (ir-code head))
                (ir-code-block "bash" (one-line cmd 200)))}))

(defn- channel-render-shell-logs
  [{:keys [id status exit lines shown_count line_count dropped]}]
  (let [head (str id " · " status (when (some? exit) (str " (exit " exit ")"))
               " · " shown_count "/" line_count " lines"
               (when (pos? (long (or dropped 0))) (str " · " dropped " dropped")))
        body (:text (tail-str (str/join "\n" (map (fn [[n text]] (str n "| " text))
                                               lines))
                      render-preview-chars))]
    {:summary {:left  (ir-strong "LOGS")
               :right (ir-code head)}
     :display (ir-root
                (ir-p (ir-strong "LOGS") "  " (ir-code head))
                (when-not (str/blank? body) (ir-code-block nil body)))}))

;; =============================================================================
;; Public, doc-bearing vars — `:doc`/`:arglists` are the model-facing surface
;; (read by `vis/symbol` straight off the var); the injected `env` first arg
;; is hidden from both.
;; =============================================================================

(def ^{:doc "Run a shell command synchronously via bash -lc in the workspace root. Returns {\"exit\": N|None, \"stdout\": S, \"stderr\": S, \"timed_out\": B, \"duration_ms\": N, ...} — a NON-ZERO exit is a result to read, not an error. Options dict (snake_case): {\"timeout_secs\": N (default 120, max 600), \"cwd\": rel-dir-inside-workspace}. On timeout the whole process tree is killed and timed_out is True (exit None). Output keeps the LAST 16k chars per stream (stdout_truncated/stderr_truncated flag it). Prefer cat/ls/rg/patch/write for file work; shell covers builds, tests, git, package managers. Long-running daemons belong in shell_bg."
       :arglists '([cmd] [cmd opts])}
  shell shell-impl)

(def ^{:doc "Start a BACKGROUND shell command as a session RESOURCE: shell_bg(id, cmd) spawns bash -lc in the workspace root, captures merged stdout+stderr into a ring buffer (last 2000 lines), and registers resource `id` (kind shell) — it appears in session_resources and the footer. Read output with shell_logs(id); stop (and discard logs) with resource_stop(id) — the ONLY stop path. When the process exits on its own the resource flips to status exited and the logs + exit code stay readable until resource_stop. `id` must be unique among this session's RUNNING shells; an exited id can be reused (its logs are discarded). Returns {\"id\": S, \"pid\": N, \"status\": \"running\", ...}."
       :arglists '([id cmd])}
  shell-bg shell-bg-impl)

(def ^{:doc "Tail the captured output of a background shell started with shell_bg. shell_logs(id) returns the last 200 lines, shell_logs(id, n) the last n (max 2000). Result: {\"status\": \"running\"|\"exited\", \"exit\": N|None, \"lines\": [[seq, text], ...], \"line_count\": total-ever, \"shown_count\": N, \"dropped\": ring-buffer-evictions}. `lines` mirrors cat's [line-number, text] tuple shape."
       :arglists '([id] [id n])}
  shell-logs shell-logs-impl)

;; =============================================================================
;; Symbols + prompt + extension
;; =============================================================================

(def shell-symbol
  (vis/symbol #'shell
    {:symbol 'shell
     :before-fn (shell-gate-before-fn :shell)
     :tag :mutation
     :render-fn channel-render-shell
     :on-error-fn (shell-on-error :shell)}))

(def shell-bg-symbol
  (vis/symbol #'shell-bg
    {:symbol 'shell-bg
     :before-fn (shell-gate-before-fn :shell-bg)
     :tag :mutation
     :render-fn channel-render-shell-bg
     :on-error-fn (shell-on-error :shell-bg)}))

(def shell-logs-symbol
  (vis/symbol #'shell-logs
    {:symbol 'shell-logs
     :before-fn (shell-gate-before-fn :shell-logs)
     :tag :observation
     :render-fn channel-render-shell-logs
     :on-error-fn (shell-on-error :shell-logs)}))

(defn available-shell-symbols
  []
  [shell-symbol shell-bg-symbol shell-logs-symbol])

(defn shell-prompt
  "Prompt fragment advertising the shell surface — ONLY while the toggle is
   ON (a blank string is filtered out of the extensions prompt block), so a
   disabled layer costs zero prompt tokens and the model never sees it."
  [_env]
  (if (toggles/enabled? :vis/shell-tool)
    (str/join "\n"
      ["Shell compatibility layer — ENABLED by the user. Bare Python functions: shell / shell_bg / shell_logs."
       "  shell(\"make test\")                                  # sync, bash -lc, workspace root"
       "  shell(\"npm run build\", {\"timeout_secs\": 300, \"cwd\": \"web\"})"
       "  r[\"exit\"] / r[\"stdout\"] / r[\"stderr\"] / r[\"timed_out\"] — non-zero exit is DATA: read it, don't treat it as a tool failure."
       "  Background tasks are session RESOURCES:"
       "  shell_bg(\"dev-server\", \"npm run dev\")              # registers resource 'dev-server' (see session_resources)"
       "  shell_logs(\"dev-server\")                            # tail captured output, [seq, line] pairs"
       "  resource_stop(\"dev-server\")                         # the ONE stop path (also discards logs)"
       "  Prefer cat/ls/rg/patch/write for file work — shell never replaces them."
       "  Side effect in its own reply; commands run inside the workspace root only."])
    ""))

(def vis-extension
  (vis/extension
    {:ext/name        "foundation-shell"
     :ext/description "Shell compatibility layer (toggle :vis/shell-tool, OFF by default): sync shell(cmd) via bash -lc; background shell_bg(id, cmd) registered as a session resource (stop via resource_stop, footer + session_resources visibility); shell_logs(id) output tail."
     :ext/version     "0.1.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/kind        "foundation"
     :ext/engine      {:ext.engine/builtin? true
                       :ext.engine/symbols  (available-shell-symbols)}
     :ext/prompt      shell-prompt}))

(vis/register-extension! vis-extension)
