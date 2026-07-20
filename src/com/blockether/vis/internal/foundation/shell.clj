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
      workspace root, waits up to a timeout, and returns a LEAN payload with
      string keys cmd/stdout/duration_ms + conditional keys (exit when finished;
      timed_out/timeout_secs on timeout; stderr when non-empty; truncation
      flags when true; cwd when narrowed) — results ride every later prompt
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
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.pty :as pty]
            [com.blockether.vis.internal.foundation.pty-bridge :as pty-bridge])
  (:import (java.io File)
           (java.lang ProcessHandle)
           (java.util HashMap)
           (java.util.concurrent TimeUnit)))

;; =============================================================================
;; Toggle (extension-owned)
;; =============================================================================

(toggles/register-toggle! {:id :shell/enabled
                           :label "Shell commands (compatibility layer)"
                           :description (str "When ON the agent can run shell commands from the"
                                             " sandbox: sync shell_run(cmd) plus background"
                                             " shell_bg(id, cmd) registered as session resources"
                                             " (footer count, F4 dialog, resource_stop). OFF by"
                                             " default - every shell call is refused with a hint"
                                             " until you enable it.")
                           :default false
                           :owner "foundation-shell"
                           :group :tools
                           :persist? true})

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

(defn- now-ms ^long [] (System/currentTimeMillis))

;; =============================================================================
;; Small helpers
;; =============================================================================

(defn- read-capped
  "Drain a Reader keeping the HEAD and the TAIL of the stream, dropping only the
   MIDDLE when output exceeds `head-limit`+`tail-limit` — so neither the opening
   context nor the closing failure/summary is ever silently lost (the old
   tail-only cap swallowed everything before the last N chars). When truncated a
   visible omitted-count marker is spliced in at the boundary. Bounded memory:
   the middle is collapsed at read time, so a megabyte-then-killed command can't
   balloon the heap. Returns {:text :truncated}. Never throws: a stream closed
   mid-read (the timeout/stop path closes it) just ends the drain."
  [^java.io.Reader r ^long head-limit ^long tail-limit]
  (let
    [sb
     (StringBuilder.)

     buf
     (char-array 8192)

     cap
     (+ head-limit tail-limit)

     total
     (volatile! 0)

     trunc
     (volatile! false)]

    (try (loop []

           (let [n (.read r buf 0 (alength buf))]
             (when (pos? n)
               (vswap! total
                       (fn [t]
                         (+ (long t) n)))
               (.append sb buf 0 n)
               (when (> (.length sb) cap)
                 (vreset! trunc true)
                 ;; keep the first `head-limit` chars + the last `tail-limit`;
                 ;; excise the run between them so memory stays at ~cap.
                 (.delete sb (int head-limit) (int (- (.length sb) tail-limit))))
               (recur))))
         (catch Throwable _ nil))
    {:text (if @trunc
             (str (subs (.toString sb) 0 head-limit)
                  "\n\n…[" (- (long @total) cap)
                  " chars omitted]…\n\n" (subs (.toString sb) head-limit))
             (.toString sb))
     :truncated @trunc}))

(defn- ->pos-long
  "Coerce a GraalPy-crossed numeric option to a long (floats round), or throw a
   typed error. nil passes through (caller supplies the default). Rejects
   strings/other types with a clean message instead of a raw ClassCastException
   surfacing as an opaque throwable envelope."
  [x what]
  (cond (nil? x) nil
        (number? x) (long (Math/round (double x)))
        :else (throw (ex-info (str what " must be a number, got " (pr-str x) ".")
                              {:type ::bad-option :option what :value x}))))

(defn- one-line
  "Collapse a command to a single display line capped at `limit` chars."
  [s ^long limit]
  (let
    [s (-> (str s)
           (str/replace #"\s+" " ")
           str/trim)]
    (if (> (count s) limit) (str (subs s 0 limit) "…") s)))

(defn- contains-dir?
  "When `rel` names an existing directory under canonical `root` (or IS the
   root), the canonical File for it; otherwise nil. `nil` too when it resolves
   OUTSIDE the root (containment guard, same rule the editing tools enforce).
   An ABSOLUTE `rel` is taken as-is (not joined onto root), so a path the model
   can already see/edit — e.g. the workspace root itself — is runnable verbatim."
  ^File [^File root rel]
  (let
    [f
     (io/file rel)

     dir
     (.getCanonicalFile (if (.isAbsolute f) f (io/file root rel)))]

    (when (and (or (= dir root)
                   (str/starts-with? (.getPath dir) (str (.getPath root) File/separator)))
               (.isDirectory dir))
      dir)))

(defn- resolve-cwd
  "Directory the command runs in: the workspace root (bound per-call by the
   extension wrapper), optionally narrowed by a RELATIVE opts `cwd` that must
   stay inside a root — same containment rule the editing tools enforce.

   The relative `cwd` is resolved against the primary root FIRST, then each
   bound filesystem-root `:trunk` (the real dirs the model addresses), so a
   path the model can see/edit is also runnable here even when the primary
   root differs (forked/trunk-clone workspaces, or an unbound root falling
   back to the process cwd)."
  ^File [opts]
  (let
    [root
     (.getCanonicalFile (workspace/cwd))

     roots
     (into [root]
           (comp (keep :trunk) (map #(.getCanonicalFile (io/file %))))
           workspace/*filesystem-roots*)

     rel
     (let [c (get opts "cwd")]
       (when-not (str/blank? (str (or c ""))) (str c)))]

    (if-not rel
      root
      (or (some #(contains-dir? % rel) roots)
          ;; Nothing matched — build the most useful diagnostic we can: report
          ;; the base(s) tried and the absolute path we looked for, and say
          ;; WHY (escapes / is-a-file / missing) against the primary root.
          (let
            [dir
             (let [f (io/file rel)]
               (.getCanonicalFile (if (.isAbsolute f) f (io/file root rel))))

             bases
             (str/join ", " (distinct (map #(.getPath ^File %) roots)))]

            (throw
              (ex-info
                (cond (not (or (= dir root)
                               (str/starts-with? (.getPath dir)
                                                 (str (.getPath root) File/separator))))
                      (str "shell cwd '"
                           rel
                           "' escapes the workspace root;"
                           " relative paths must stay inside it (base "
                           bases
                           ").")
                      (.exists dir)
                      (str "shell cwd '" rel "' is a file, not a directory (" (.getPath dir) ").")
                      :else (str "shell cwd '"
                                 rel
                                 "' does not exist under the workspace"
                                 " root — looked for "
                                 (.getPath dir)
                                 " (base "
                                 bases
                                 ")."))
                {:type ::cwd-unresolved
                 :cwd rel
                 :resolved (.getPath dir)
                 :exists (.exists dir)
                 :roots (mapv #(.getPath ^File %) roots)})))))))

(defn- lf
  "Normalize CRLF to LF so captured output is byte-identical on every OS."
  ^String [^String s]
  (when s (.replace s "\r\n" "\n")))

(defn- windows?* [] (str/starts-with? (str/lower-case (System/getProperty "os.name" "")) "win"))

(defn- find-git-bash
  "Absolute path to a REAL bash on Windows (Git for Windows), or nil. NEVER the
   WSL launcher at System32\\bash.exe — with no distro installed it merely
   prints \"Windows Subsystem for Linux has no installed distributions\" and
   exits, so a bare `bash` lookup on PATH is a trap. Resolves a `VIS_BASH`
   override first, then the standard Git install roots, then bash alongside a
   `git.exe` found on PATH (Git\\cmd\\git.exe → Git\\bin\\bash.exe)."
  []
  (let
    [path-sep
     (System/getProperty "path.separator" ";")

     from-path
     (for
       [dir
        (str/split (or (System/getenv "PATH") "")
                   (re-pattern (java.util.regex.Pattern/quote path-sep)))

        :when (not (str/blank? dir))
        :let [git
              (io/file dir "git.exe")]
        :when (.isFile git)
        :let [root
              (let [^java.io.File p (.getParentFile ^java.io.File git)]
                (when p (.getParentFile p)))

              bash
              (when root (io/file root "bin" "bash.exe"))]
        :when (and bash (.isFile bash))]

       (.getPath bash))

     roots
     (keep identity
           [(System/getenv "ProgramFiles") (System/getenv "ProgramW6432")
            (System/getenv "ProgramFiles(x86)")
            (some-> (System/getenv "LOCALAPPDATA")
                    (str "\\Programs"))])

     candidates
     (concat (when-let [o (System/getenv "VIS_BASH")]
               [o])
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
  (let
    [^java.util.List args
     [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)]

     pb
     (ProcessBuilder. args)]

    (.directory pb dir)
    (when merge-err? (.redirectErrorStream pb true))
    (.start pb)))

(defn- process->handle
  "Adapt a plain `java.lang.Process` (the no-PTY ProcessBuilder path) into the
   same handle map the FFM PTY backend returns, so the background pump /
   kill-tree! / shell_send all consume ONE shape regardless of backend. Used as
   the native-Windows fallback (see pty-spawn!)."
  [^Process p]
  {:pid (.pid p)
   :in (.getInputStream p)
   :send (fn [^bytes b]
           (let [^java.io.OutputStream os (.getOutputStream p)]
             (.write os b)
             (.flush os)))
   :wait (fn []
           (.waitFor p))
   :alive? (fn []
             (.isAlive p))
   :destroy (fn [force?]
              (if force? (.destroyForcibly p) (.destroy p)))})

(defn- pty-spawn!
  "Spawn `cmd` under a REAL pseudo-terminal (internal.foundation.pty — pure Java
   FFM, no JNA and no extracted native helper): isatty() is TRUE, $TERM is set,
   and stdin is writable (shell_send) — so interactive CLIs that refuse a dumb
   pipe (browser-auth prompts, password `read`, REPLs) actually run. Returns the
   pty HANDLE MAP (`:pid :in :send :wait :alive? :destroy`) that the pump /
   kill-tree! / wait path below consume. stdout+stderr share the one PTY stream
   (a real terminal has no separate error channel), so no merge-err? knob."
  [cmd ^File dir]
  (if (windows?*)
    ;; pty is POSIX-only (openpty/posix_spawnp). On native Windows fall back to
    ;; a plain merged-output ProcessBuilder wrapped in the same handle shape —
    ;; no real TTY (isatty() false, no shell_send interactivity, no attach
    ;; bridge), but shell_bg still runs/captures/stops cleanly instead of
    ;; throwing. (ConPTY could restore a real TTY here later.)
    (process->handle (spawn! cmd dir true))
    (pty/spawn! {:command [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)]
                 :dir (.getPath dir)
                 :env (doto (HashMap. ^java.util.Map (System/getenv))
                        (.put "TERM" "xterm-256color"))
                 :cols 120
                 :rows 40})))

(defn- kill-tree!
  "Destroy a spawned process + every descendant reachable via `ProcessHandle.of
   pid`: polite SIGTERM first, then a forced SIGKILL after a 2s grace. Accepts
   EITHER a `java.lang.Process` (the sync `shell_run` ProcessBuilder path) or the
   pty HANDLE MAP (the `shell_bg` path); both spawn genuine OS processes reachable
   via `ProcessHandle`. Never throws (teardown path). NOTE: a deliberately-
   detaching child (`setsid`/double-fork/`nohup … &`) reparents to init and
   escapes this reach — the registry still drops cleanly and the pump is unblocked
   by closing the stream in the stop-fn, but the orphan keeps running."
  [p]
  (try
    (let
      [pid
       (if (map? p) (:pid p) (.pid ^Process p))

       destroy
       (if (map? p)
         (:destroy p)
         (fn [force?]
           (if force? (.destroyForcibly ^Process p) (.destroy ^Process p))))

       ^ProcessHandle ph
       (try (.orElse (ProcessHandle/of pid) nil) (catch Throwable _ nil))

       descendants
       (fn []
         (if ph
           (-> ph
               .descendants
               .iterator
               iterator-seq)
           []))]

      (run! (fn [^ProcessHandle d]
              (try (.destroy d) (catch Throwable _ nil)))
            (descendants))
      (destroy false)
      (let [deadline (+ (System/currentTimeMillis) 2000)]
        (loop []

          (when (and ph (.isAlive ph) (< (System/currentTimeMillis) deadline))
            (Thread/sleep 50)
            (recur))))
      (when (and ph (.isAlive ph))
        (run! (fn [^ProcessHandle d]
                (try (.destroyForcibly d) (catch Throwable _ nil)))
              (descendants))
        (destroy true)))
    (catch Throwable _ nil))
  nil)

;; =============================================================================
;; SYNC shell_run
;; =============================================================================

(defn- clamp-timeout-secs
  "Effective sync timeout from the opts value: default 120, floor 1, cap 600."
  ^long [v]
  (-> (long (or (->pos-long v "timeout_secs") default-timeout-secs))
      (max 1)
      long
      (min (long max-timeout-secs))))

(defn- shell-run-impl
  ([env cmd] (shell-run-impl env cmd nil))
  ([_env cmd opts]
   (let [cmd (str cmd)]
     (when (str/blank? cmd)
       (throw (ex-info "shell_run needs a non-blank command string." {:type ::blank-command})))
     (let
       [timeout-secs (clamp-timeout-secs (get opts "timeout_secs"))
        cwd-opt? (not (str/blank? (str (or (get opts "cwd") ""))))
        dir (resolve-cwd opts)
        t0 (now-ms)
        p (spawn! cmd dir false)
        empty-tail {:text "" :truncated false}
        ;; Separate reader futures per stream — avoids the classic full-pipe
        ;; deadlock on chatty commands. `read-capped` bounds memory to the
        ;; head+tail budget per stream at READ time (dropping only the MIDDLE
        ;; of a huge stream, not its start), so a megabyte-then-killed command
        ;; can't balloon the heap yet the opening context survives.
        out-f (future (read-capped (io/reader (.getInputStream p))
                                   max-sync-head-chars
                                   max-sync-tail-chars))
        err-f (future (read-capped (io/reader (.getErrorStream p))
                                   max-sync-head-chars
                                   max-sync-tail-chars))
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
       (let
         [out (deref out-f 5000 empty-tail)
          err (deref err-f 5000 empty-tail)
          exit (when finished? (.exitValue p))
          t1 (now-ms)]

         (extension/success
           ;; Lean result: this map rides every later prompt as a frozen
           ;; <results> pin, so optional keys appear ONLY when they carry
           ;; signal (model reads them with .get). :op / echoes of the call
           ;; args (cwd default, timeout default) never ship.
           {:result (cond-> {"cmd" cmd "stdout" (lf (:text out)) "duration_ms" (- t1 t0)}
                      finished?
                      (assoc "exit" exit)

                      (not finished?)
                      (assoc "timed_out"
                        true "timeout_secs"
                        timeout-secs)

                      (:truncated out)
                      (assoc "stdout_truncated" true)

                      (not (str/blank? (:text err)))
                      (assoc "stderr" (lf (:text err)))

                      (:truncated err)
                      (assoc "stderr_truncated" true)

                      ;; Relative cwd is `/`-separated on every OS (Windows `\`).
                      cwd-opt?
                      (assoc "cwd" (paths/unixify (.getPath dir))))
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

(defonce ^:private _bridge-sweep
  ;; One-time GC at extension load: a prior vis crash/kill never ran serve!'s
  ;; :stop (the JVM held the AF_UNIX server), so stale attach sockets pile up in
  ;; bridge-dir. sweep-orphans! connect-probes each and unlinks the dead ones.
  (do (try (pty-bridge/sweep-orphans!) (catch Throwable _ nil)) true))

(defn- bg-entry [session id] (get-in @bg-procs [(str session) (str id)]))

(defn- drop-bg-entry!
  [session id]
  (let
    [sk
     (str session)

     id
     (str id)]

    (swap! bg-procs (fn [m]
                      (let [m (update m sk dissoc id)]
                        (if (empty? (get m sk)) (dissoc m sk) m))))
    nil))

(defn- push-line!
  [buffer line]
  ;; A char-pump split on `\n` leaves the `\r` of a CRLF line behind; strip it
  ;; so a Windows-emitted line reads identically to a POSIX one.
  (let
    [line
     (if (and (string? line) (str/ends-with? line "\r")) (subs line 0 (dec (count line))) line)]
    (swap! buffer (fn [{:keys [lines next-seq dropped]}]
                    (let
                      [lines (conj lines [next-seq line])
                       over (- (count lines) (long max-bg-lines))]

                      {:lines (if (< 0 over) (subvec lines over) lines)
                       :next-seq (inc (long next-seq))
                       :dropped (+ (long dropped) (long (max over 0)))})))))

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
  ^Thread [session id p buffer exit-atom stopped? bridge-atom]
  (doto
    (Thread.
      (fn []
        ;; Char-level drain (not `line-seq`) so a newline-free stream
        ;; (`cat big.bin`) can't grow one unbounded line in memory: a line
        ;; is force-flushed at `max-line-chars`.
        (try (with-open [r (io/reader ^java.io.InputStream (:in p))]
               (let [sb (StringBuilder.)]
                 (loop []

                   (let [c (.read r)]
                     (cond (= c -1) (when (pos? (.length sb)) (push-line! buffer (str sb)))
                           (= c (int \newline))
                           (do (push-line! buffer (str sb)) (.setLength sb 0) (recur))
                           :else (do (.append sb (char c))
                                     (when (>= (.length sb) (long max-line-chars))
                                       (push-line! buffer (str sb " …[line truncated]"))
                                       (.setLength sb 0))
                                     (recur)))))))
             (catch Throwable _ nil))
        (let [code (try ((:wait p)) (catch Throwable _ nil))]
          (reset! exit-atom code)
          (when-not @stopped?
            ;; Natural child exit (not resource_stop): tear down the attach
            ;; bridge so its AF_UNIX socket doesn't linger — otherwise a human
            ;; could `attach` a dead shell and find-socket could pick the
            ;; stale .sock. On a manual stop the stop-fn owns this teardown.
            (when-let [b @bridge-atom]
              (try ((:stop b)) (catch Throwable _ nil)))
            (try (resources/update! session
                                    id
                                    {:status :exited
                                     :detail
                                     (str "exit " code " — logs retained until resource_stop")})
                 (catch Throwable _ nil))))))
    (.setName (str "vis-shell-bg-" id))
    (.setDaemon true)
    (.start)))

(defn- shell-bg-impl
  [env id cmd]
  (let
    [session
     (:session-id env)

     id
     (str id)

     cmd
     (str cmd)]

    (when (str/blank? id)
      (throw (ex-info "shell_bg needs a non-blank resource id (first arg)." {:type ::blank-id})))
    (when (str/blank? cmd)
      (throw (ex-info "shell_bg needs a non-blank command string (second arg)."
                      {:type ::blank-command})))
    (when-let [existing (bg-entry session id)]
      (if ((:alive? (:proc existing)))
        (throw (ex-info (str "Background shell '"
                             id
                             "' is already running (pid "
                             (:pid (:proc existing))
                             "); resource_stop it first or pick a new id.")
                        {:type ::bg-id-in-use :id id}))
        ;; Exited-but-unread entry under the same id: replacing it discards
        ;; its retained logs by intent (the model chose to reuse the id).
        (do (resources/unregister! session id) (drop-bg-entry! session id))))
    (let
      [dir
       (resolve-cwd nil)

       p
       (pty-spawn! cmd dir)

       buffer
       (atom {:lines [] :next-seq 1 :dropped 0})

       exit-atom
       (atom nil)

       stopped?
       (atom false)

       bridge-atom
       (atom nil)

       t0
       (now-ms)

       pump
       (start-pump! session id p buffer exit-atom stopped? bridge-atom)

       ;; Passthrough bridge: expose this PTY over a per-shell AF_UNIX socket
       ;; so a HUMAN can `vis ext shell attach <id>` into the live terminal
       ;; (browser OAuth, a prompt only a person can answer) and detach again,
       ;; child untouched. Best-effort — if AF_UNIX bind fails the shell still
       ;; runs, just without human attach.
       bridge
       (try (pty-bridge/serve! {:pty p
                                :path (pty-bridge/socket-path session id)
                                :replay-fn (fn []
                                             (let [ls (:lines @buffer)]
                                               (when (seq ls)
                                                 (.getBytes
                                                   (str (str/join "\n" (map second ls)) "\n")
                                                   java.nio.charset.StandardCharsets/UTF_8))))})
            (catch Throwable _ nil))]

      (reset! bridge-atom bridge)
      (swap! bg-procs assoc-in
        [(str session) id]
        {:proc p
         :buffer buffer
         :exit exit-atom
         :pump pump
         :stopped? stopped?
         :send (:send p)
         :bridge bridge
         :cmd cmd
         :cwd (.getPath dir)
         :started-at t0})
      (resources/register! session
                           {:id id
                            :kind :shell
                            :label (one-line cmd 48)
                            :detail cmd
                            :pid (:pid p)
                            :owner "foundation-shell"
                            :status :running}
                           {:stop-fn (fn []
                                       ;; Tell the pump to stop touching the registry, kill the
                                       ;; tree, then wait for the pump to finish draining BEFORE
                                       ;; the registry drops the resource — so the pump can never
                                       ;; resurrect a partial entry after unregister.
                                       (reset! stopped? true)
                                       (kill-tree! p)
                                       ;; Close the read end so the pump's blocking `.read`
                                       ;; returns even if a detached grandchild still holds the
                                       ;; write end — the pump thread can't outlive the stop.
                                       (try (.close ^java.io.InputStream (:in p))
                                            (catch Throwable _ nil))
                                       (try (.join pump 3000) (catch InterruptedException _ nil))
                                       ;; Tear down the attach socket last: no more human attachers
                                       ;; once the child is gone.
                                       (when bridge (try ((:stop bridge)) (catch Throwable _ nil)))
                                       (drop-bg-entry! session id))
                            ;; Alive while the buffer entry exists — an EXITED process is kept
                            ;; (status :exited) so its logs stay readable; only resource_stop
                            ;; (or replacing the id) lets the registry drop it.
                            :alive-fn (fn []
                                        (some? (bg-entry session id)))
                            ;; Ring-buffer tail so TUI/web can VIEW a background's
                            ;; output (same lines as shell_logs), not just stop it.
                            :logs-fn (fn []
                                       (mapv second (:lines @buffer)))
                            ;; "alive, but is it WORKING?" for the registry's
                            ;; per-render health probe: running / exited-clean /
                            ;; failed (non-zero exit).
                            :health-fn (fn []
                                         (cond (nil? (bg-entry session id)) :down
                                               (nil? @exit-atom) :running
                                               (zero? (long @exit-atom)) :exited
                                               :else :failed))})
      (extension/success
        ;; No :op / :cwd — shell_bg always runs at the workspace root and the
        ;; result rides every later prompt as a frozen <results> pin.
        {:result (cond-> {"id" id "pid" (:pid p) "cmd" cmd "status" "running"}
                   bridge
                   (assoc "attach"
                     (str "vis ext shell attach " id) "socket"
                     (:path bridge)))
         :op :shell/bg
         :metadata
         {:command cmd :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}}))))

(defn- shell-logs-impl
  ([env id] (shell-logs-impl env id default-log-tail))
  ([env id n]
   (let
     [session
      (:session-id env)

      id
      (str id)

      entry
      (bg-entry session id)]

     (when-not entry
       (throw (ex-info (str "No background shell '"
                            id
                            "' in this session —"
                            " start one with shell_bg(id, cmd); live ids are"
                            " listed in resources.")
                       {:type ::unknown-bg-id :id id})))
     (let
       [n
        (-> (long (or (->pos-long n "n") default-log-tail))
            (max 1)
            long
            (min (long max-bg-lines)))

        {:keys [lines dropped next-seq]}
        @(:buffer entry)

        total
        (dec (long next-seq))

        shown
        (if (> (count lines) n) (subvec lines (- (count lines) n)) lines)

        exit
        @(:exit entry)

        t
        (now-ms)]

       (extension/success
         ;; Lean result: no :op / :cmd / :cwd / :pid (the shell_bg result
         ;; already carries process identity) and no :shown_count (it's
         ;; len(lines)). :exit only once exited, :dropped only when the ring
         ;; buffer actually evicted — absent keys read as None via .get.
         {:result (cond->
                    {"id" id
                     "status" (if (some? exit) "exited" "running")
                     "lines" shown
                     "line_count" total
                     "uptime_ms" (- t (long (:started-at entry)))}
                    (some? exit)
                    (assoc "exit" exit)

                    (pos? (long (or dropped 0)))
                    (assoc "dropped" dropped))
          :op :shell/logs
          :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))))

(defn- shell-send-impl
  "Write `text` to a background shell's STDIN (its PTY master). With enter (default
   true) a trailing newline SUBMITS the line — exactly what an interactive prompt
   (password, `read`, a REPL, a y/N confirm) waits for. The send-keys equivalent:
   the agent drives an interactive program whose output the pump captured. Read the
   response with shell_logs(id). Returns {id, sent, status}."
  ([env id text] (shell-send-impl env id text nil))
  ([env id text opts]
   (let
     [session
      (:session-id env)

      id
      (str id)

      entry
      (bg-entry session id)

      enter?
      (let [e (get opts "enter" (get opts :enter true))]
        (if (nil? e) true (boolean e)))]

     (when-not entry
       (throw (ex-info (str "No background shell '"
                            id
                            "' in this session — start"
                            " one with shell_bg(id, cmd); live ids are listed in"
                            " resources.")
                       {:type ::unknown-bg-id :id id})))
     (when-not ((:alive? (:proc entry)))
       (throw (ex-info (str "Background shell '" id
                            "' has exited — nothing to send"
                            " to. Its logs stay readable until resource_stop.")
                       {:type ::bg-exited :id id})))
     (let [send-fn (:send entry)]
       (when (nil? send-fn)
         (throw (ex-info (str "Background shell '" id "' has no writable stdin.")
                         {:type ::no-stdin :id id})))
       (let
         [payload (str text (when enter? "\n"))
          t (now-ms)]

         (send-fn (.getBytes payload java.nio.charset.StandardCharsets/UTF_8))
         (extension/success {:result {"id" id "sent" (count payload) "status" "running"}
                             :op :shell/send
                             :metadata
                             {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}}))))))

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
        {:result (extension/failure {:result nil
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
    (let
      [interrupted?
       (instance? InterruptedException err)

       t
       (now-ms)]

      {:result (extension/failure
                 {:result nil
                  :op op
                  :metadata (cond-> {:started-at-ms t :finished-at-ms t :duration-ms 0}
                              interrupted?
                              (assoc :interrupted?
                                true :status
                                :interrupted))
                  :error (when interrupted?
                           {:message (str (op-label op)
                                          " interrupted while running;"
                                          " the spawned process tree was killed.")})
                  :throwable (when-not interrupted? err)})})))

;; =============================================================================
;; Public, doc-bearing vars retain developer examples and fallback docs. Native
;; symbols below provide compact model-facing semantics; their schemas provide
;; exact inputs. The injected `env` first arg is hidden from both.
;; =============================================================================

(def
  ^{:doc
    "await shell_run(\"git status\")
await shell_run(\"npm run build\", {\"timeout_secs\": 300, \"cwd\": \"web\"})

Run a command via bash -lc in the workspace root.
Returns {\"cmd\", \"stdout\", \"duration_ms\"} plus, only when meaningful (use .get): \"exit\", \"timed_out\"+\"timeout_secs\", \"stderr\", \"stdout_truncated\"/\"stderr_truncated\", \"cwd\".
opts: {\"timeout_secs\": N (default 120, max 600), \"cwd\": rel-dir-inside-workspace}.
Gotcha: a non-zero \"exit\" is DATA to read, not a tool failure. On timeout there is NO \"exit\" key."
    :arglists '([cmd] [cmd opts])}
  shell-run
  shell-run-impl)

(def
  ^{:doc
    "await shell_bg(\"dev-server\", \"npm run dev\")

Start a background command (bash -lc, workspace root) as a session resource `id`; no timeout — use for daemons / watchers / long builds.
Returns {\"id\", \"pid\", \"cmd\", \"status\": \"running\"}.
Read output with shell_logs(id); stop and discard logs with resource_stop(id) — the ONLY stop path.
Gotcha: `id` must be unique among RUNNING shells; reusing an exited id discards its retained logs."
    :arglists '([id cmd])}
  shell-bg
  shell-bg-impl)

(def
  ^{:doc
    "await shell_logs(\"dev-server\")
await shell_logs(\"dev-server\", 500)

Tail a background shell's captured output. shell_logs(id) keeps the last 200 lines, shell_logs(id, n) the last n (max 2000).
Returns {\"id\", \"status\": \"running\"|\"exited\", \"lines\": [[seq, text], ...], \"line_count\", \"uptime_ms\"} plus, only when meaningful (use .get): \"exit\", \"dropped\".
Gotcha: \"lines\" is [seq, text] pairs (not strings); shown count is len(lines), \"line_count\" is total-ever."
    :arglists '([id] [id n])}
  shell-logs
  shell-logs-impl)

(def
  ^{:doc
    "await shell_send(\"dev-server\", \"y\")
await shell_send(\"slack-auth\", \"xoxp-my-token\", {\"enter\": true})

Type into a background shell's STDIN — the send-keys equivalent. The process runs under a REAL pty (isatty() true, $TERM set), so interactive prompts (password / `read` / REPL / y-N confirm) accept input. `text` is written to its stdin; with enter (default true) a trailing newline SUBMITS the line. Read what came back with shell_logs(id).
Returns {\"id\", \"sent\", \"status\"}.
Gotcha: only a RUNNING background shell accepts input; an exited one raises. A step only a HUMAN can finish (browser OAuth, a device-code prompt) can't be typed by the agent — tell the user to run `vis ext shell attach <id>` in their own terminal to take over the live session, then detach with Ctrl-] (the child keeps running). The shell_bg result carries the exact `attach` command."
    :arglists '([id text] [id text opts])}
  shell-send
  shell-send-impl)

;; =============================================================================
;; Native op-card renderers — `:result` → `{:summary :body}`. The result arrives
;; string-keyed snake_case (strings-only boundary); the injected env first arg is
;; already gone. Renderers read string keys but still RETURN the keyword `{:summary
;; :body}` IR (that part is internal).
;; =============================================================================

(def ^:private shell-chip-max
  "Display-width-ish budget for the command on the collapsed shell chip. Keep this
   tighter than the TUI width; the op-card label already says SHELL RUN."
  72)

(defn- present-str
  "Stringify `x`, trim only the right edge (so logs keep indentation), and return
   nil when blank."
  [x]
  (let [s (str/trimr (str x))]
    (when-not (str/blank? s) s)))

(defn- shell-one-line
  "Collapse whitespace to one trimmed line for a shell card preview."
  [s]
  (some-> (present-str s)
          (str/replace #"\s+" " ")
          str/trim
          not-empty))

(defn- clip-chip
  "Clip a single-line preview with an ellipsis so shell commands cannot blow out
   collapsed cards."
  [s n]
  (let
    [s
     (str s)

     n
     (long n)]

    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

(defn- duration-label
  "Human duration for shell card chips/status sections."
  [ms]
  (when (number? ms)
    (cond (< (long ms) 1000) (str (long ms) "ms")
          (< (long ms) 60000) (str/replace (format "%.1fs" (/ (double ms) 1000.0)) "," ".")
          :else (str/replace (format "%.1fm" (/ (double ms) 60000.0)) "," "."))))

(defn- format-shell-command
  "Pretty-print a shell command for the COMMAND card so a compound one-liner
   reads as separated statements instead of one crammed blob. Break onto its
   own line at TOP-LEVEL `;`, `&&`, `||` operators, keeping the operator at the
   end of its line. Quote- AND paren-aware: separators inside `'…'` / `\"…\"`
   or nested `$(…)` / `(…)` stay put (so `$(f || g)` and `2>&1 &` are never
   split), and a simple command comes back unchanged."
  [s]
  (let
    [s
     (str s)

     n
     (count s)

     sb
     (StringBuilder.)]

    (loop
      [i
       0

       sq
       false

       dq
       false

       depth
       0]

      (if (>= i n)
        (let
          [out (->> (str/split-lines (str sb))
                    (map str/trim)
                    (remove str/blank?)
                    (str/join "\n"))]
          (if (str/blank? out) (str/trim s) out))
        (let
          [c
           (.charAt s i)

           nxt
           (when (< (inc i) n) (.charAt s (inc i)))]

          (cond
            ;; backslash escape (not inside single quotes): copy the pair verbatim
            (and (not sq) (= c \\))
            (do (.append sb c) (when nxt (.append sb nxt)) (recur (+ i 2) sq dq depth))
            (and (not dq) (= c \')) (do (.append sb c) (recur (inc i) (not sq) dq depth))
            (and (not sq) (= c \")) (do (.append sb c) (recur (inc i) sq (not dq) depth))
            (or sq dq) (do (.append sb c) (recur (inc i) sq dq depth))
            (= c \() (do (.append sb c) (recur (inc i) sq dq (inc depth)))
            (= c \)) (do (.append sb c) (recur (inc i) sq dq (max 0 (dec depth))))
            (and (zero? depth) (= c \&) (= nxt \&)) (do (.append sb "&&\n")
                                                        (recur (+ i 2) sq dq depth))
            (and (zero? depth) (= c \|) (= nxt \|)) (do (.append sb "||\n")
                                                        (recur (+ i 2) sq dq depth))
            (and (zero? depth) (= c \;)) (do (.append sb ";\n") (recur (inc i) sq dq depth))
            :else (do (.append sb c) (recur (inc i) sq dq depth))))))))


(defn- fence
  "Wrap `s` in a code fence, or nil when blank."
  ([s] (fence s nil))
  ([s lang]
   (when-let [s (present-str s)]
     (str "```" (or lang "") "\n" s "\n```"))))

(defn- shell-section
  "One REPL-style labeled shell body section."
  ([label s] (shell-section label s nil))
  ([label s lang]
   (when-let [f (fence s lang)]
     (str "**" label "**\n" f))))

(defn- kv-lines
  "Render non-nil `[label value]` pairs as `label: value` lines."
  [pairs]
  (->> pairs
       (keep (fn [[k v]]
               (when-let [v (present-str v)]
                 (str k ": " v))))
       (str/join "\n")
       not-empty))

(defn- shell-run-status
  "Status fields for a sync shell_run result. Non-zero exit is display data, not a
   tool error, but it still gets the failed visual treatment."
  [r]
  (let [exit (get r "exit")]
    (cond (get r "timed_out") {:icon "⏱"
                               :label (str "timed out"
                                           (when-let [s (get r "timeout_secs")]
                                             (str " after " s "s")))
                               :failed? true}
          (and exit (not (== 0 (long exit)))) {:icon "✗" :label (str "exit " exit) :failed? true}
          exit {:icon "✓" :label (str "exit " exit) :failed? false}
          :else {:icon "✓" :label "finished" :failed? false})))

(defn- render-shell-run-result
  "shell_run → REPL-style collapsed/expanded card.

   Collapsed: `$ npm test (success) · 1.2s` or
   `$ grep x missing (failure) · exit 2 · 34ms`.
   Expanded: labeled COMMAND / STATUS / STDOUT / STDERR sections. The body is
   always present so shell cards are collapsible even when the command produced no
   output; the full command and metadata stay available behind the disclosure."
  [r]
  (let
    [{:keys [label failed?]}
     (shell-run-status r)

     cmd
     (or (shell-one-line (get r "cmd")) "shell")

     duration
     (duration-label (get r "duration_ms"))

     summary
     (str "$ "
          (clip-chip cmd shell-chip-max)
          " ("
          (if failed? "failure" "success")
          ")"
          (when failed? (str " · " label))
          (when duration (str " · " duration)))

     status
     (kv-lines [["status" label] ["duration" duration] ["cwd" (get r "cwd")]
                ["timeout"
                 (when-let [s (get r "timeout_secs")]
                   (str s "s"))] ["stdout" (when (get r "stdout_truncated") "truncated")]
                ["stderr" (when (get r "stderr_truncated") "truncated")]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "cmd")) "bash")
           (shell-section "STATUS" status) (shell-section "STDOUT" (get r "stdout") "bash")
           (shell-section "STDERR" (get r "stderr"))]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-bg-result
  "shell_bg → lifecycle card with the command, pid, and human attach hint in the
   expandable body."
  [r]
  (let
    [id
     (get r "id")

     status
     (or (get r "status") "started")

     summary
     (str "⚙ bg `"
          id
          "` "
          status
          (when-let [pid (get r "pid")]
            (str " · pid " pid)))

     details
     (kv-lines [["id" id] ["status" status] ["pid" (get r "pid")] ["attach" (get r "attach")]
                ["socket" (get r "socket")]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "cmd")) "bash")
           (shell-section "STATUS" details)]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-logs-result
  "shell_logs → compact process/log status plus a terminal transcript body."
  [r]
  (let
    [lines
     (or (get r "lines") [])

     text
     (->> lines
          (map (fn [pair]
                 (if (sequential? pair) (second pair) pair)))
          (str/join "\n"))

     status
     (or (get r "status") "?")

     duration
     (duration-label (get r "uptime_ms"))

     exited?
     (= "exited" status)

     summary
     (str (if exited? "■" "◷")
          " `"
          (get r "id")
          "` "
          status
          (when-let [exit (get r "exit")]
            (str " · exit " exit))
          " · "
          (count lines)
          " lines"
          (when-let [total (get r "line_count")]
            (when (not= total (count lines)) (str " / " total " total")))
          (when-let [d (get r "dropped")]
            (str " · " d " dropped"))
          (when duration (str " · " duration)))

     details
     (kv-lines [["id" (get r "id")] ["status" status] ["exit" (get r "exit")]
                ["shown" (str (count lines) " lines")] ["total" (get r "line_count")]
                ["dropped" (get r "dropped")] ["uptime" duration]])

     body
     (->> [(shell-section "STATUS" details) (shell-section "LOGS" text "bash")]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-send-result
  "shell_send → send-keys lifecycle card."
  [r]
  (let
    [details (kv-lines [["id" (get r "id")]
                        ["sent"
                         (when-let [n (get r "sent")]
                           (str n " chars"))] ["status" (get r "status")]])]
    {:summary (str "↵ `" (get r "id") "` sent " (get r "sent") " chars")
     :body (shell-section "STATUS" details)}))

;; =============================================================================
;; Symbols + prompt + extension. Alias `shell` → `shell_run` / `shell_bg` /
;; `shell_logs` in the flat Python sandbox.
;; =============================================================================

(def shell-run-symbol
  (vis/symbol
    #'shell-run
    {:symbol 'run
     :native-tool? true
     :name "shell_run"
     :description
     (str "Run a bounded command that should exit. Use file/editing tools for file work and "
          "`shell_bg` for servers, watchers, interactive programs, or other long-running work; "
          "non-zero exit is result data to inspect.")
     ;; shell_run(cmd, {opts}) — cmd positional, the rest an options dict.
     :call {:pos ["cmd"] :rest :opt}
     :render render-shell-run-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"cmd" {:type "string"
                      :minLength 1
                      :description "Command line, run via bash -lc in the workspace root."}
               "timeout_secs" {:type "integer"
                               :minimum 1
                               :maximum 600
                               :description "Sync timeout seconds (default 120, max 600)."}
               "cwd" {:type "string"
                      :description "Relative directory inside the workspace to run in."}}
              :required ["cmd"]
              :additionalProperties false}
     :before-fn (shell-gate-before-fn :shell/run)
     :tag :mutation
     :on-error-fn (shell-on-error :shell/run)}))

(def shell-bg-symbol
  (vis/symbol
    #'shell-bg
    {:symbol 'bg
     :native-tool? true
     :name "shell_bg"
     :description (str "Start a long-running or interactive command as a session resource. Never "
                       "hide background work with shell operators; inspect it with `shell_logs`, "
                       "interact with `shell_send`, and stop it through `resource_stop`.")
     ;; shell_bg(id, cmd) — both positional.
     :call {:pos ["id" "cmd"]}
     :render render-shell-bg-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties {"id" {:type "string"
                                 :minLength 1
                                 :description
                                 "Unique resource id among RUNNING shells; read logs / stop by it."}
                           "cmd" {:type "string"
                                  :minLength 1
                                  :description
                                  "Background command (bash -lc, workspace root); no timeout."}}
              :required ["id" "cmd"]
              :additionalProperties false}
     :before-fn (shell-gate-before-fn :shell/bg)
     :tag :mutation
     :on-error-fn (shell-on-error :shell/bg)}))

(def shell-logs-symbol
  (vis/symbol
    #'shell-logs
    {:symbol 'logs
     :native-tool? true
     :name "shell_logs"
     :description
     "Read the retained output and lifecycle state of a `shell_bg` resource; use it before deciding whether background work succeeded, failed, or still runs."
     ;; shell_logs(id, n?) — id positional, optional trailing n.
     :call {:pos ["id"] :opt-pos ["n"]}
     :render render-shell-logs-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"id" {:type "string" :minLength 1 :description "The background shell's resource id."}
               "n" {:type "integer"
                    :minimum 1
                    :maximum 2000
                    :description "Tail the last n lines (default 200, max 2000)."}}
              :required ["id"]
              :additionalProperties false}
     :before-fn (shell-gate-before-fn :shell/logs)
     :tag :observation
     :on-error-fn (shell-on-error :shell/logs)}))

(def shell-send-symbol
  (vis/symbol
    #'shell-send
    {:symbol 'send
     :native-tool? true
     :name "shell_send"
     :description (str
                    "Send input to a running interactive `shell_bg` resource, then read its "
                    "response with `shell_logs`. Human-only browser or device authentication must "
                    "be handed off through the resource's attach command.")
     ;; shell_send(id, text, {opts}) — id + text positional, rest an options dict.
     :call {:pos ["id" "text"] :rest :opt}
     :render render-shell-send-result
     :color-role :tool-color/shell
     :schema {:type "object"
              :properties
              {"id" {:type "string" :minLength 1 :description "The background shell's resource id."}
               "text" {:type "string" :description "Text written to the shell's stdin."}
               "enter" {:type "boolean"
                        :description "Append a newline to SUBMIT the line (default true)."}}
              :required ["id" "text"]
              :additionalProperties false}
     :before-fn (shell-gate-before-fn :shell/send)
     :tag :mutation
     :on-error-fn (shell-on-error :shell/send)}))

(def shell-symbols [shell-run-symbol shell-bg-symbol shell-logs-symbol shell-send-symbol])

(defn shell-attach-command
  "`vis ext shell attach <id>` — the human-side passthrough: join a live
   `shell_bg` shell's PTY in your OWN terminal (finish a browser OAuth, answer a
   prompt only a person can), then Ctrl-] to detach with the child untouched.
   `--socket PATH` targets an explicit socket; otherwise the newest shell whose
   id matches. Returns the attach exit code."
  [_parsed residual]
  (let
    [args
     (vec residual)

     socket
     (loop [xs args]
       (cond (empty? xs) nil
             (= (first xs) "--socket") (second xs)
             :else (recur (rest xs))))

     id
     (first (remove #(str/starts-with? % "--") args))]

    (pty-bridge/attach! {:id id :socket socket})))

(def shell-cli
  "CLI surface mounted under `vis ext shell`. Only `attach` for now — the human
   passthrough onto a background PTY the agent spawned."
  [{:cmd/name "shell"
    :cmd/doc "Attach a real terminal to a live background shell (shell_bg)."
    :cmd/usage "vis ext shell attach <id>"
    :cmd/subcommands
    [{:cmd/name "attach"
      :cmd/doc
      "Join a background shell's PTY in your terminal; Ctrl-] detaches (child keeps running)."
      :cmd/usage "vis ext shell attach <id> [--socket PATH]"
      :cmd/owns-tty? true
      :cmd/examples ["vis ext shell attach slack-auth" "vis ext shell attach dev-server"]
      :cmd/run-fn #'shell-attach-command}]}])

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shell"
     :ext/description
     "Shell compatibility layer (toggle :shell/enabled, OFF by default): sync shell_run(cmd) via bash -lc; background shell_bg(id, cmd) registered as a session resource (stop via resource_stop, footer + resources visibility); shell_logs(id) output tail."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; The toggle IS the activation gate: when OFF the extension is
     ;; inactive, so `sync-active-extension-symbols!` (per-env install +
     ;; every turn start) REMOVES shell_run/shell_bg/shell_logs from the
     ;; sandbox globals — `apropos` doesn't list them, calling raises a
     ;; plain NameError, and the prompt fragment is gone. The before-fn
     ;; refusal below stays as defense-in-depth for the same-turn window
     ;; where the toggle flips after symbols were already bound.
     :ext/activation-fn (fn [_env]
                          (toggles/enabled? :shell/enabled))
     :ext/engine {:ext.engine/alias 'shell :ext.engine/symbols shell-symbols}
     :ext/cli shell-cli}))

(vis/register-extension! vis-extension)
