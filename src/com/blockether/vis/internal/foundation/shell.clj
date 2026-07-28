(ns com.blockether.vis.internal.foundation.shell
  "`shell/` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature). Bound only when the user-owned `shell` toggle is ON
   (default ON; flip it OFF in Settings or in `vis.yml` via `toggles: {shell: false}`
   to drop the tools). The OS process jail is the containment layer while active.

   ONE model-facing binding — `shell` — bound BARE in the flat Python sandbox
   next to `git` / `cat` / `grep`. There is no `shell_run` / `shell_bg` /
   `shell_logs` / `shell_send` quartet any more: four names for ONE subsystem
   meant four call shapes and four result shapes for what is a single process
   lifecycle. One tool, one `op` grammar, one TOTAL result map:

   1. RUN (default) `await shell(cmd)` / `await shell(cmd, opts)` — `bash -lc` in the
      workspace root, waits up to a timeout. Output is bounded at READ time to a
      head+tail budget per stream, so only the MIDDLE of a huge stream is
      dropped, never its start or end (a chatty-then-killed command cannot
      balloon the heap). A non-zero exit is DATA the model reads, not an error.

   2. BACKGROUND `await shell(cmd, {\"op\": \"background\", \"id\": \"dev\"})` — an `id`
      makes it background (the op may stay implicit): spawned under a REAL pty,
      its merged output pumped into a bounded ring buffer, registered as a session
      RESOURCE in `internal.resources` (footer count, F4 dialog, `resources` ctx
      block). Prefer this for long builds, test suites, servers, watchers, and
      interactive commands; reserve run for short bounded work.

   3. LOGS / SEND / STOP `await shell({\"op\": \"logs\", \"id\": \"dev\"})` — tail the ring
      buffer, type into the pty, or kill the tree and drop the resource. Shell stop
      and `await resource_stop(id)` land on the same `resources/stop!`. An EXITED process
      is not auto-pruned, so its output and exit code stay readable until it is
      stopped.

   Commands have ONE spelling in Python: the first positional argument for
   run/background. Resource IDs also have ONE spelling: `id` inside the options map
   for background/logs/send/stop. Native JSON still carries a `cmd` property; the symbol's
   `:call` shape converts that transport field to the Python positional before
   dispatch.

   Every op answers with the SAME key set (`shell-result-base`): keys a stage
   does not fill are nil / false / 0 instead of absent, so model Python indexes
   any field without a KeyError.

   The `shell` toggle is registered HERE, extension-owned under the vis namespace."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.pty :as pty]
            [com.blockether.vis.internal.foundation.pty-bridge :as pty-bridge]
            [com.blockether.vis.internal.strutil :as strutil])
  (:import (java.io File)
           (java.lang ProcessHandle)
           (java.util HashMap)
           (java.util.concurrent TimeUnit)))

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
   balloon the heap. Returns {:text :truncated :omitted} — `omitted` is the exact
   dropped-char count (0 when nothing was dropped). Never throws: a stream closed
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
     :truncated @trunc
     ;; Exact dropped-char count: the text now carries an inline marker, so a
     ;; caller can SEE both that it is no longer parseable and how much is gone.
     :omitted (if @trunc (- (long @total) cap) 0)}))

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

(defn- resolve-cwd
  "Resolve a command cwd against the primary workspace, then authorize the
   canonical result against every filesystem root in the immutable environment
   snapshot plus the live workspace overlay. A configured sibling such as
   `../svar` is therefore valid when that canonical sibling is an allowed root;
   arbitrary traversal remains denied. Absolute paths follow the same rule."
  ^File [opts]
  (let
    [env
     (::environment opts)

     root
     (.getCanonicalFile (workspace/cwd))

     env-roots
     (workspace/env-filesystem-roots env)

     roots
     (->> (concat [{:trunk (.getPath root) :clone (.getPath root)}]
                  env-roots
                  workspace/*filesystem-roots*)
          (mapcat (juxt :trunk :clone))
          (keep #(some-> %
                         io/file
                         .getCanonicalFile))
          distinct
          vec)

     cwd-value
     (get opts "cwd")

     requested
     (when-not (str/blank? (str (or cwd-value ""))) (str cwd-value))]

    (if-not requested
      root
      (let
        [requested-file
         (io/file (paths/expand-home requested))

         dir
         (.getCanonicalFile
           (if (.isAbsolute requested-file) requested-file (io/file root requested)))

         allowed?
         (some (fn [^File allowed-root]
                 ;; Path#startsWith is component-aware and handles a filesystem root
                 ;; correctly (`/` is not naively joined with another `/`).
                 (or (= dir allowed-root) (.startsWith (.toPath dir) (.toPath allowed-root))))
               roots)

         root-paths
         (mapv #(.getPath ^File %) roots)]

        (cond (not allowed?) (throw (ex-info (str
                                               "shell cwd '"
                                               requested
                                               "' resolves outside the allowed filesystem roots ("
                                               (str/join ", " root-paths)
                                               ").")
                                             {:type ::cwd-unresolved
                                              :cwd requested
                                              :resolved (.getPath dir)
                                              :exists (.exists dir)
                                              :roots root-paths}))
              (not (.exists dir))
              (throw (ex-info (str "shell cwd '" requested "' does not exist (" (.getPath dir) ").")
                              {:type ::cwd-unresolved
                               :cwd requested
                               :resolved (.getPath dir)
                               :exists false
                               :roots root-paths}))
              (not (.isDirectory dir))
              (throw
                (ex-info
                  (str "shell cwd '" requested "' is a file, not a directory (" (.getPath dir) ").")
                  {:type ::cwd-unresolved
                   :cwd requested
                   :resolved (.getPath dir)
                   :exists true
                   :roots root-paths}))
              :else dir)))))

(defn- lf
  "Normalize CRLF to LF so captured output is byte-identical on every OS."
  ^String [^String s]
  (when s (.replace s "\r\n" "\n")))

(defn- bash-command
  "Bash executable to run commands with — bash on EVERY platform, so the model
   writes one command dialect everywhere. Under WSL the JVM reports
   `os.name=Linux`, so this is WSL's own real `bash` (correct — we're a Linux
   process there)."
  []
  "bash")

(defn- jail-policy
  "Resolve the per-session jail policy carried by `env`.

   A present policy function is security-critical: failures propagate and deny the
   spawn instead of silently returning nil/unwrapped argv. `sandbox: false` is
   represented explicitly by `{:disabled? true}` and remains the sole escape hatch."
  [env]
  (when-let [f (:jail-policy-fn env)]
    (or (f)
        (throw (ex-info "Shell process denied: session jail policy is unavailable"
                        {:type ::jail-policy-missing :session-id (:session-id env)})))))

(defn- spawn!
  ^Process [cmd ^File dir merge-err? policy]
  (let
    [^java.util.List args
     (process-jail/wrap-argv [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)] policy)

     pb
     (ProcessBuilder. args)]

    (.directory pb dir)
    ;; Route the child's HTTP clients at the loopback egress proxy when the jail
    ;; policy walls it to proxy-only egress (net-off-except-loopback).
    (if-let [full (process-jail/jailed-child-env policy)]
      ;; Confined child: REPLACE the inherited env with the allowlisted set so the
      ;; operator's API keys/tokens are never handed to sandboxed code.
      (let [^java.util.Map e (.environment pb)]
        (.clear e)
        (.putAll e ^java.util.Map full))
      (let [pe (process-jail/proxy-env policy)]
        (when (seq pe) (.putAll (.environment pb) ^java.util.Map pe))))
    (when merge-err? (.redirectErrorStream pb true))
    (.start pb)))

(defn- pty-spawn!
  "Spawn `cmd` under a REAL pseudo-terminal (internal.foundation.pty — pure Java
   FFM, no JNA and no extracted native helper): isatty() is TRUE, $TERM is set,
   and stdin is writable (the send op) — so interactive CLIs that refuse a dumb
   pipe (browser-auth prompts, password `read`, REPLs) actually run. Returns the
   pty HANDLE MAP (`:pid :in :send :wait :alive? :destroy`) that the pump /
   kill-tree! / wait path below consume. stdout+stderr share the one PTY stream
   (a real terminal has no separate error channel), so no merge-err? knob.
   `policy` is the live per-session jail policy value (or nil) applied to the
   spawned argv, so the OS jail confines the interactive child too."
  [cmd ^File dir policy]
  (pty/spawn! {:command (process-jail/wrap-argv [(bash-command) "--noprofile" "--norc" "-lc"
                                                 (str cmd)]
                                                policy)
               :dir (.getPath dir)
               :env (if-let [full (process-jail/jailed-child-env policy)]
                      ;; Confined child: allowlisted env only (secrets dropped).
                      (doto (HashMap. ^java.util.Map full) (.put "TERM" "xterm-256color"))
                      (doto (HashMap. ^java.util.Map (System/getenv))
                        (.put "TERM" "xterm-256color")
                        (.putAll ^java.util.Map (process-jail/proxy-env policy))))
               :cols 120
               :rows 40}))

(defn- kill-tree!
  "Destroy a spawned process + every descendant reachable via `ProcessHandle.of
   pid`: polite SIGTERM first, then a forced SIGKILL after a 2s grace. Accepts
   EITHER a `java.lang.Process` (the sync run's ProcessBuilder path) or the
   pty HANDLE MAP (the bg path); both spawn genuine OS processes reachable
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
;; SYNC run — Python sandbox: `await shell(cmd)`
;; =============================================================================

(defn- clamp-timeout-secs
  "Effective sync timeout from the opts value: default 120, floor 1, cap 600."
  ^long [v]
  (-> (long (or (->pos-long v "timeout_secs") default-timeout-secs))
      (max 1)
      long
      (min (long max-timeout-secs))))

(def ^:private shell-result-base
  "TOTAL key set of EVERY `shell` result — one tool, ONE result shape. `stage`
   names the stage that produced it (run / background / logs / send / stop); the keys
   that stage does not fill keep these neutral values instead of vanishing, so ordinary
   model Python (`r[\"exit\"]`, `r[\"lines\"]`) can never KeyError on a shell result.

   NOT `\"op\"`: the extension boundary stamps `\"op\"` on EVERY tool result with the
   tool's own origin (always \"shell\" here) — tool-specific stage detail must use a
   different key, exactly as `stamp-public-result-op` requires."
  {"stage" nil
   "id" nil
   "cmd" nil
   "cwd" nil
   ;; run
   "stdout" nil
   "stderr" nil
   "exit" nil
   "duration_ms" nil
   "timed_out" false
   "timeout_secs" nil
   "stdout_truncated" false
   "stderr_truncated" false
   "stdout_omitted_chars" 0
   "stderr_omitted_chars" 0
   ;; background lifecycle
   "pid" nil
   "status" nil
   "uptime_ms" nil
   "attach" nil
   "socket" nil
   "already_running" false
   "note" nil
   "lines" []
   "line_count" 0
   "dropped" 0
   "sent" 0
   "text" nil
   "keys" nil
   "stopped" false})

(defn- shell-result
  "One stage's own fields merged onto the total base, with `stage` stamped last."
  [op m]
  (assoc (merge shell-result-base m) "stage" op))

(defn- shell-run-impl
  ([env cmd] (shell-run-impl env cmd nil))
  ([env cmd opts]
   (let [cmd (str cmd)]
     (when (str/blank? cmd)
       (throw (ex-info (str "shell needs a non-blank command — pass it as the lone positional"
                            " or as {\"cmd\": \"…\"} in the options map.")
                       {:type ::blank-command})))
     (let
       [timeout-secs (clamp-timeout-secs (get opts "timeout_secs"))
        dir (resolve-cwd (assoc (or opts {}) ::environment env))
        t0 (now-ms)
        p (spawn! cmd dir false (jail-policy env))
        empty-tail {:text "" :truncated false :omitted 0}
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
           ;; TOTAL result shape (`shell-result`). The old "lean" map dropped a key
           ;; whenever it carried no signal, so ordinary model Python (`r["stderr"]`,
           ;; `r["timed_out"]`) died with a bare `KeyError` — read as "the tool
           ;; broke", retried with cosmetic variations, and spun.
           {:result (shell-result "run"
                                  {"cmd" cmd
                                   ;; Relative cwd is `/`-separated on every OS.
                                   "cwd" (paths/unixify (.getPath dir))
                                   "stdout" (lf (:text out))
                                   "stderr" (lf (:text err))
                                   "exit" exit
                                   "duration_ms" (- t1 t0)
                                   "timed_out" (not finished?)
                                   "timeout_secs" timeout-secs
                                   ;; A truncated stream has an inline "…[N chars omitted]…"
                                   ;; marker spliced into its MIDDLE, so it is no longer valid
                                   ;; JSON/parseable — the count says exactly how much is gone.
                                   "stdout_truncated" (boolean (:truncated out))
                                   "stderr_truncated" (boolean (:truncated err))
                                   "stdout_omitted_chars" (long (or (:omitted out) 0))
                                   "stderr_omitted_chars" (long (or (:omitted err) 0))})
            :op :shell
            :metadata {:command cmd
                       :exit exit
                       :timed-out? (not finished?)
                       :started-at-ms t0
                       :finished-at-ms t1
                       :duration-ms (- t1 t0)}}))))))

;; =============================================================================
;; BACKGROUND — Python sandbox: `await shell(cmd, {"op": "background", "id": …})`
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
  ;; so a CRLF-emitted line reads identically to a POSIX one.
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

(defn- bg-core
  "Identity keys shared by EVERY background stage of `shell`, merged onto the
   TOTAL base. `op` names the stage that produced the result, so the card renderer
   — and model Python — reads ONE declared field instead of sniffing which keys
   happen to exist. `exit` nil while running, `attach`/`socket` nil when no attach
   bridge was opened, `cmd`/`cwd`/`pid` nil only once the entry itself is gone."
  [op id entry]
  (let
    [exit
     (some-> (:exit entry)
             deref)

     bridge
     (:bridge entry)]

    (shell-result op
                  {"id" id
                   "cmd" (:cmd entry)
                   "cwd" (:cwd entry)
                   "pid" (:pid (:proc entry))
                   "status" (if (some? exit) "exited" "running")
                   "exit" exit
                   "uptime_ms" (- (now-ms) (long (or (:started-at entry) (now-ms))))
                   "attach" (when bridge (str "vis extension shell attach " id))
                   "socket" (:path bridge)})))

(defn- shell-bg-spawn!
  "Spawn a NEW background PTY under `id`. Callers guarantee no LIVE entry holds
   the id (`shell-bg-impl` owns that check); an exited-but-unread entry under
   the same id is replaced, discarding its retained logs by intent."
  [env id cmd opts]
  (let
    [session
     (:session-id env)

     id
     (str id)

     cmd
     (str cmd)]

    (when (str/blank? id)
      (throw (ex-info "The shell background op needs a non-blank resource id ({\"id\": …})."
                      {:type ::blank-id})))
    (when (str/blank? cmd)
      (throw (ex-info "The shell background op needs a non-blank command as its first argument."
                      {:type ::blank-command})))
    (when (bg-entry session id) (resources/unregister! session id) (drop-bg-entry! session id))
    (let
      [dir
       (resolve-cwd (assoc (or opts {}) ::environment env))

       p
       (pty-spawn! cmd dir (jail-policy env))

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
       ;; so a HUMAN can `vis extension shell attach <id>` into the live terminal
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
                            ;; output (same lines as the `logs` op), not just stop it.
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
        ;; TOTAL result shape, shared with every other stage through `bg-core`:
        ;; run / background / logs / send / stop all answer with the same key set
        ;; (`op` says which stage ran), so model Python never KeyErrors on a field
        ;; another stage would have carried. `already_running` false on a fresh
        ;; spawn, `note` nil when there is nothing to say.
        {:result (assoc (bg-core "background" id (bg-entry session id))
                   "already_running" false
                   "note" nil)
         :op :shell
         :metadata
         {:command cmd :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}}))))

(defn- shell-bg-impl
  "`await shell(cmd, {\"op\": \"background\", \"id\": id})` — IDEMPOTENT on a live id.

   Re-using an id whose process is still running used to THROW. That reads as a
   plain tool failure: a model that already started the shell (or that lost the
   result) learns nothing actionable, invents a new id, and spins — the exact
   runaway seen on `companion-dev`. Instead, return the RUNNING shell flagged
   `already_running` with its pid/uptime and the `logs` handle, so \"start it\" is
   answered by \"it IS started, here is how to watch it\". No second process is
   spawned; a genuinely fresh one needs a `stop` op first."
  [env id cmd opts]
  (let
    [session
     (:session-id env)

     id
     (str id)

     live
     (when-let [existing (bg-entry session id)]
       (when ((:alive? (:proc existing))) existing))]

    (if live
      (extension/success
        {:result
         (assoc (bg-core "background" id live)
           "already_running" true
           "note"
           (str
             "Background shell '"
             id
             "' was ALREADY running — nothing was restarted. Read its output "
             "with await shell({\"op\": \"logs\", \"id\": \""
             id
             "\"}). To start a fresh process, first run await shell({\"op\": \"stop\", \"id\": \""
             id
             "\"})."))
         :op :shell
         :metadata {:command (:cmd live)
                    :pid (:pid (:proc live))
                    :started-at-ms (:started-at live)
                    :finished-at-ms (now-ms)
                    :duration-ms 0}})
      (shell-bg-spawn! env id cmd opts))))

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
                            "' in this session — start one with"
                            " await shell(cmd, {\"op\": \"background\", \"id\": id});"
                            " live ids are listed in resources.")
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

        t
        (now-ms)]

       (extension/success
         ;; Sharing `bg-core`'s identity keys with every other stage: `exit` None
         ;; while running, `dropped` 0 when the ring buffer evicted nothing —
         ;; model Python indexes them directly instead of dying on a KeyError.
         ;; `lines` is a vec of plain STRINGS and is the ONLY copy of the tail:
         ;; the seq numbers were an internal ring-buffer detail that only ever made
         ;; `"\n".join(r["lines"])` throw, and a pre-joined `text` twin would bill
         ;; the same bytes to the context twice. Join it yourself when you print.
         {:result (assoc (bg-core "logs" id entry)
                    "lines" (mapv second shown)
                    "line_count" total
                    "dropped" (long (or dropped 0)))
          :op :shell
          :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))))

(def ^:private control-key-names
  "Human names for the control characters `send` writes into a PTY, so a card can
   show WHAT was typed even when the payload prints nothing at all."
  {\newline "↵" \return "↵" \tab "⇥" (char 27) "Esc" (char 127) "Del"})

(defn- key-token
  "Named key for ONE control character, or nil when the character is printable."
  [c]
  (let [n (int c)]
    (or (control-key-names c)
        (cond (zero? n) "C-@"
              (< n 27) (str "C-" (char (+ 96 n)))
              (< n 32) (str "C-" (char (+ 64 n)))
              (<= 127 n 159) (format "\\u%04x" n)
              :else nil))))

(defn- keys-label
  "Human keystroke label for a `send` payload: printable runs stay literal (quoted),
   every control character becomes its key name — `\"y\" ↵`, `C-c`, `Esc`. A send is
   frequently ENTIRELY non-printing, where a char count says nothing about what the
   shell actually received, so this is what the card shows."
  [s]
  (when (some? s)
    (->> (str s)
         (partition-by #(some? (key-token %)))
         (mapcat
           (fn [run]
             (if (key-token (first run)) (map key-token run) [(str "\"" (str/join run) "\"")])))
         (str/join " ")
         not-empty)))

(defn- shell-send-impl
  "Write `text` to a background shell's STDIN (its PTY master). With enter (default
   true) a trailing newline SUBMITS the line — exactly what an interactive prompt
   (password, `read`, a REPL, a y/N confirm) waits for. The send-keys equivalent:
   the agent drives an interactive program whose output the pump captured. Read the
   response with `await shell({\"op\": \"logs\", \"id\": id})`. Returns the total shell result."
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
                            "' in this session — start one with"
                            " await shell(cmd, {\"op\": \"background\", \"id\": id});"
                            " live ids are listed in resources.")
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
         (extension/success {:result (assoc (bg-core "send" id entry)
                                       "sent" (count payload)
                                       "text" payload
                                       "keys" (keys-label payload))
                             :op :shell
                             :metadata
                             {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}}))))))

;; -----------------------------------------------------------------------------
;; ONE lifecycle entry point — Python: `await shell(cmd, opts)` / `await shell({"op": …, "id": …})`
;; -----------------------------------------------------------------------------

(defn- opts-arg?
  "Is `x` the trailing OPTIONS map rather than a positional string? A Python dict
   crosses as a java.util.Map; a Clojure caller passes a map."
  [x]
  (or (map? x) (instance? java.util.Map x)))

(defn- opt
  "Read `k` from an options map that may be string- or keyword-keyed."
  [opts k]
  (if (nil? opts)
    nil
    (let [v (get opts (name k))]
      (if (nil? v) (get opts (keyword k)) v))))

(defn- shell-stop-impl
  "`await shell({\"op\": \"stop\", \"id\": id})` — the TERMINAL lifecycle stage, and part of why
   the four shell tools became one: stopping was only reachable through
   `resource_stop`, a sandbox builtin absent from the native tool list, so the end
   of a background shell's life was undiscoverable from the schema. Routes through
   `resources/stop!` — the single stop path the footer and `resource_stop` share —
   so the process tree dies, the retained logs are dropped, and the registry
   entry disappears exactly once."
  [env id]
  (let
    [session
     (:session-id env)

     id
     (str id)

     t
     (now-ms)

     entry
     (bg-entry session id)

     r
     (resources/stop! session id)]

    (when (= :unknown (:result r))
      (throw (ex-info (str "No background shell '" id
                           "' in this session — nothing to stop;"
                           " live ids are listed in resources.")
                      {:type ::unknown-bg-id :id id})))
    (when (= :error (:result r))
      (throw (ex-info (str "Background shell '" id "' failed to stop: " (:message r))
                      {:type ::stop-failed :id id})))
    (extension/success {:result (assoc (bg-core "stop" id entry)
                                  "status" "stopped"
                                  "stopped" true)
                        ;; Reported under the one `shell` op: `stop` is one stage of
                        ;; that tool's lifecycle, and only registered symbol ops
                        ;; carry a tag.
                        :op :shell
                        :metadata
                        {:id id :started-at-ms t :finished-at-ms (now-ms) :duration-ms 0}})))

(defn- shell-dispatch
  "One shell lifecycle grammar with no field aliases.

   `cmd` is positional only and is used only by run/background. `id` lives only in
   the options map and is used by background/logs/send/stop. Native JSON calls still
   carry a `cmd` property, but the symbol's `:call` shape converts it to the positional
   argument before this dispatcher runs. Unknown ops and missing fields fail
   loudly rather than silently choosing another spelling."
  ([env] (shell-dispatch env nil nil))
  ([env a] (if (opts-arg? a) (shell-dispatch env nil a) (shell-dispatch env a nil)))
  ([env command opts]
   (let
     [opts
      ;; A Python dict crosses as a java.util.Map; normalize so later `assoc`
      ;; (cwd resolution) and key reads work on ONE kind of map.
      (cond (nil? opts) {}
            (map? opts) opts
            :else (into {} opts))

      mapped-command?
      (or (contains? opts "cmd") (contains? opts :cmd))

      _
      (when mapped-command?
        (throw
          (ex-info
            "shell command must be positional. In python_execution use await shell(cmd, opts), not a cmd key in the options map."
            {:type ::mapped-command})))

      id
      (some-> (opt opts :id)
              str
              str/trim
              not-empty)

      op
      (or (some-> (opt opts :op)
                  str
                  str/trim
                  str/lower-case
                  not-empty)
          (if id "background" "run"))

      command
      (some-> command
              str
              not-empty)

      need-command
      (fn []
        (or
          command
          (throw
            (ex-info
              (str
                "shell op \""
                op
                "\" needs the command as its first argument. In python_execution use await shell(cmd, opts).")
              {:type ::missing-command :op op}))))

      need-id
      (fn []
        (or id
            (throw
              (ex-info
                (str
                  "shell op \""
                  op
                  "\" needs {\"id\": \"…\"} in the options map; live ids are listed in resources.")
                {:type ::missing-id :op op}))))

      reject-command
      (fn []
        (when command
          (throw (ex-info (str "shell op \""
                               op
                               "\" does not take a positional id — put {\"id\": \""
                               command
                               "\"} in the options map.")
                          {:type ::positional-id :op op :id command}))))]

     (case op
       "run"
       (shell-run-impl env (need-command) opts)

       "background"
       (shell-bg-impl env (need-id) (need-command) opts)

       "logs"
       (do (reject-command) (shell-logs-impl env (need-id) (opt opts :n)))

       "send"
       (do (reject-command) (shell-send-impl env (need-id) (opt opts :text) opts))

       "stop"
       (do (reject-command) (shell-stop-impl env (need-id)))

       (throw (ex-info
                (str "Unknown shell op "
                     (pr-str op)
                     " — use \"run\" (default), \"background\", \"logs\", \"send\" or \"stop\".")
                {:type ::unknown-op :op op}))))))

;; =============================================================================
;; Env injection — the before-fn hands the impl its env as first arg
;; =============================================================================

(defn- op-label
  "Human call name from an op keyword: :shell -> \"shell\"."
  [op]
  (if (namespace op) (str (namespace op) "_" (name op)) (name op)))

(defn- shell-gate-before-fn
  "Inject `env` as the impl's first arg (the model never sees it). Availability is
   gated by the `shell` toggle at the extension boundary; once bound, the OS process
   jail is the containment layer."
  [_op]
  (fn [env f args]
    {:env env :fn f :args (into [env] args)}))

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
                  :error (cond interrupted? {:message (str
                                                        (op-label op)
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
    "In `python_execution`, await every call:
await shell(\"git status\")
await shell(\"npm run build\", {\"op\": \"background\", \"id\": \"build\", \"cwd\": \"web\"})
await shell(\"npm run dev\", {\"op\": \"background\", \"id\": \"dev-server\"})
await shell({\"op\": \"logs\", \"id\": \"dev-server\", \"n\": 500})
await shell({\"op\": \"send\", \"id\": \"dev-server\", \"text\": \"y\"})
await shell({\"op\": \"stop\", \"id\": \"dev-server\"})

THE one shell tool. Commands have one Python spelling: the first positional argument to run/background. Resource ids also have one spelling: `\"id\"` in the options map for background/logs/send/stop. op defaults to \"run\", or to \"background\" when an id is present.
Stages:
  run — bash -lc in the workspace root; blocks until the command exits. Reserve it for short bounded commands. opts: timeout_secs (default 120, max 600), cwd. A non-zero exit is DATA to read, not a tool failure.
  background — returns immediately with no timeout and makes the shell an OWNED session resource under id. PREFER it for commands that may take a while: builds, test suites, daemons, watchers, and interactive work. Poll with logs instead of blocking a run call. Re-starting a LIVE id does NOT spawn a second process and is not an error: you get that shell back with \"already_running\": true. Reusing an EXITED id discards its retained logs.
  logs — last 200 lines, or n up to 2000.
  send — write text to the pty stdin; enter (default true) appends the newline that SUBMITS the line.
  stop — kill the process tree, discard retained logs, and drop the session resource. Uses the same stop path as resource_stop.
EVERY op returns the SAME keys — {\"stage\", \"id\", \"cmd\", \"cwd\", \"stdout\", \"stderr\", \"exit\", \"duration_ms\", \"timed_out\", \"timeout_secs\", \"stdout_truncated\", \"stderr_truncated\", \"stdout_omitted_chars\", \"stderr_omitted_chars\", \"pid\", \"status\", \"uptime_ms\", \"attach\", \"socket\", \"already_running\", \"note\", \"lines\", \"line_count\", \"dropped\", \"sent\", \"stopped\"} — always present, None/false/0 rather than missing, with \"stage\" naming the op that ran (r[\"op\"] is the tool origin every native result carries, always \"shell\").
Gotcha: oversized run output is truncated in the MIDDLE with an inline \"…[N chars omitted]…\" marker, so a truncated stream is NOT parseable — check \"stdout_truncated\" before json.loads(r[\"stdout\"]) and re-run with a narrower or aggregated command.
Gotcha: logs returns \"lines\" as plain STRINGS and NOTHING else carries that tail — print it with `print(\"\\n\".join(r[\"lines\"]))`, filter it with `any(\"ERROR\" in l for l in r[\"lines\"])`; shown count is len(lines), \"line_count\" is total-ever. (\"text\" belongs to send — the payload typed — never to logs.) Only a RUNNING shell accepts send. A step only a HUMAN can finish (browser OAuth, a device-code prompt) can't be typed by the agent — tell the user to run `vis extension shell attach <id>` in their own terminal, then detach with Ctrl-] (the child keeps running); the result carries the exact `attach` command."
    :arglists '([cmd] [cmd opts] [opts])}
  shell
  shell-dispatch)

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

(def ^:private terminal-escape-re
  #"(?s)(?:\u001B\].*?(?:\u0007|\u001B\\)|(?:\u001B\[|\u009B)[0-?]*[ -/]*[@-~]|\u001B[ -/]*[@-~])")

(def ^:private non-printing-control-re #"[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F-\u009F]")

(defn- normalize-terminal-output
  "Make captured terminal text safe and stable for Markdown renderers. Removes
   ANSI/VT escape sequences and non-printing controls, while preserving tabs and
   line feeds. Bare carriage returns become line feeds instead of leaking terminal
   overwrite behavior into the TUI or companion app."
  [s]
  (when (some? s)
    (-> (str s)
        (str/replace terminal-escape-re "")
        (str/replace "\r\n" "\n")
        (str/replace \return \newline)
        (str/replace non-printing-control-re ""))))

(defn- fence
  "Wrap normalized terminal text `s` in a code fence, or nil when blank."
  ([s] (fence s nil))
  ([s lang]
   (when-let [s (present-str (normalize-terminal-output s))]
     (strutil/fenced s lang))))

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
  "Status fields for a sync `run` result. Non-zero exit is display data, not a
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
  "shell op `run` → REPL-style collapsed/expanded card.

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
                ;; The timeout budget is TOTAL in the result but only worth a row
                ;; when it was actually hit.
                ["timeout" (when (get r "timed_out") (str (get r "timeout_secs") "s"))]
                ;; Truncation is REAL data loss: name the stream and the exact
                ;; number of characters the middle-excision dropped.
                ["stdout"
                 (when (get r "stdout_truncated")
                   (str "truncated · " (get r "stdout_omitted_chars") " chars omitted"))]
                ["stderr"
                 (when (get r "stderr_truncated")
                   (str "truncated · " (get r "stderr_omitted_chars") " chars omitted"))]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "cmd")) "bash")
           (shell-section "STATUS" status) (shell-section "STDOUT" (get r "stdout") "bash")
           (shell-section "STDERR" (get r "stderr"))]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-bg-result
  "shell op `background` → lifecycle card with the command, pid, and human attach
   hint in the expandable body."
  [r]
  (let
    [id
     (get r "id")

     status
     (or (get r "status") "started")

     summary
     ;; Text-presentation glyph, never an emoji: U+2699 COG is emoji-presented by
     ;; iOS/WebKit, so the companion painted a colour pictogram in a monochrome
     ;; card. `▸` matches the stop card's `✕` and the app's own chevrons.
     (str "▸ background `"
          id
          "` "
          status
          (when-let [pid (get r "pid")]
            (str " · pid " pid)))

     details
     (kv-lines [["id" id] ["status" status] ["pid" (get r "pid")] ["cwd" (get r "cwd")]
                ["attach" (get r "attach")] ["socket" (get r "socket")]
                ["uptime" (duration-label (get r "uptime_ms"))]
                ["already_running" (when (get r "already_running") "true")]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "cmd")) "bash")
           (shell-section "STATUS" details) (shell-section "NOTE" (get r "note"))]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-logs-result
  "shell op `logs` → compact process/log status plus a terminal transcript body."
  [r]
  (let
    [lines
     (or (get r "lines") [])

     text
     ;; `lines` is plain strings since the pre-joined `text` twin was dropped, but
     ;; PERSISTED events from older sessions still hold `[seq text]` pairs — replay
     ;; must render those, so unwrap defensively here (never on the model payload).
     (->> lines
          (map (fn [line]
                 (if (sequential? line) (second line) line)))
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
          ;; `dropped` is TOTAL (0 when nothing was evicted) — only SAY it when
          ;; something actually fell out of the ring buffer.
          (let [d (get r "dropped")]
            (when (and d (pos? (long d))) (str " · " d " dropped")))
          (when duration (str " · " duration)))

     details
     (kv-lines [["id" (get r "id")] ["status" status] ["exit" (get r "exit")]
                ["shown" (str (count lines) " lines")] ["total" (get r "line_count")]
                ["dropped" (get r "dropped")] ["uptime" duration] ["pid" (get r "pid")]])

     body
     (->> [(shell-section "STATUS" details) (shell-section "LOGS" text "bash")]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))

(defn- render-shell-send-result
  "shell op `send` → send-keys lifecycle card that SHOWS the keystrokes: printable
   text verbatim, control characters by name (`↵`, `C-c`, `Esc`). A bare char count
   never told the reader what the shell was actually driven with."
  [r]
  (let
    [keys-lbl
     (or (get r "keys") (keys-label (get r "text")))

     details
     (kv-lines [["id" (get r "id")] ["keys" keys-lbl]
                ["sent"
                 (when-let [n (get r "sent")]
                   (str n " chars"))] ["status" (get r "status")] ["pid" (get r "pid")]])

     body
     (->> [(shell-section "KEYS" keys-lbl) (shell-section "STATUS" details)]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary (str "↵ `" (get r "id")
                   "` sent " (if keys-lbl
                               (clip-chip (shell-one-line keys-lbl) shell-chip-max)
                               (str (get r "sent") " chars")))
     :body (when (seq body) body)}))

(defn- render-shell-stop-result
  "shell op `stop` → terminal lifecycle card."
  [r]
  {:summary (str "✕ background `" (get r "id") "` stopped")
   :body (shell-section "STATUS"
                        (kv-lines [["id" (get r "id")] ["status" "stopped"] ["pid" (get r "pid")]
                                   ["cmd" (shell-one-line (get r "cmd"))]
                                   ["uptime" (duration-label (get r "uptime_ms"))]]))})

(defn- render-shell-result
  "`shell` → the card of whichever op ran, read off the result's OWN `stage` field
   (run / background / logs / send / stop). Every stage returns the same TOTAL key
   set, so key-presence sniffing (`lines` → logs, `sent` → send) is not a valid
   dispatch: the stage is declared, not guessed. (`op` is NOT it — the boundary
   stamps that with the tool origin \"shell\" on every result.)"
  [r]
  (case (str (get r "stage"))
    "run"
    (render-shell-run-result r)

    "logs"
    (render-shell-logs-result r)

    "send"
    (render-shell-send-result r)

    "stop"
    (render-shell-stop-result r)

    (render-shell-bg-result r)))

;; =============================================================================
;; Symbols + prompt + extension. ONE builtin symbol — `shell` — bound bare in the
;; flat Python sandbox next to `git` / `cat` / `grep`.
;; =============================================================================

(def shell-symbol
  (vis/symbol
    #'shell
    {:symbol 'shell
     :native-tool? true
     :result
     (str
       "Fixed-key object with `stage`, `op`, `cmd`, `cwd`, `id`, `pid`, `status`, `exit`, `stdout`, "
       "`stderr`, `stdout_truncated`, `stderr_truncated`, `stdout_omitted_chars`, "
       "`stderr_omitted_chars`, `timed_out`, `timeout_secs`, `duration_ms`, `uptime_ms`, "
       "`line_count`, `lines`, `sent`, `text`, `keys`, `stopped`, `already_running`, `attach`, "
       "`socket`, "
       "`dropped`, and `note`; keys remain present with null/empty values when inapplicable.")
     :name "shell"
     :description
     (str
       "THE one shell tool — quick bounded run, background, logs, send, stop. `op` defaults to \"run\" "
       "(a bounded bash -lc that BLOCKS until exit; non-zero exit is data) and to \"background\" when "
       "an `id` is passed. PREFER background for commands that may take a while — builds, test suites, "
       "servers, watchers, or interactive work. It returns immediately as an OWNED session resource; "
       "poll it with \"logs\" instead of blocking a run call. Reserve run for short commands. \"send\" "
       "types into its pty; \"stop\" kills it — stop what you started. In `python_execution`, await "
       "`shell`; `cmd` is positional for run/background and `id` stays in the options map for "
       "background/logs/send/stop. Re-starting a LIVE id returns that shell (`already_running`), never "
       "a second process; live ids are in `session[\"resources\"]`. EVERY op returns the SAME total key "
       "set with `stage` naming the op; huge run output is truncated MID-stream — check "
       "`stdout_truncated` before parsing it. `logs` hands back the tail ONCE, as `lines` (plain "
       "strings) — join it yourself; `text` is send-only and is null on a logs result.")
     ;; Python's shell call shape keeps cmd positional, while native JSON necessarily carries `cmd` as
     ;; a property. The call shape converts that transport field to the positional;
     ;; every remaining property, including `id`, stays in the options map.
     :call {:opt-pos ["cmd"] :rest :opt}
     :render render-shell-result
     :color-role :tool-color/shell
     :schema
     {:type "object"
      :properties
      {"cmd" {:type "string"
              :minLength 1
              :description
              (str
                "run/background only: command line (bash -lc, workspace root). In Python this is "
                "the first positional argument; resource ids never use `cmd`.")}
       "op" {:type "string"
             :enum ["run" "background" "logs" "send" "stop"]
             :description "Operation (default \"run\", or \"background\" when an `id` is given)."}
       "id" {:type "string"
             :minLength 1
             :description
             "Background shell resource id — the SAME id for background / logs / send / stop."}
       "timeout_secs" {:type "integer"
                       :minimum 1
                       :maximum 600
                       :description "run only: timeout seconds (default 120, max 600)."}
       "cwd"
       {:type "string"
        :description
        "Directory under any allowed filesystem root; relative paths resolve from the workspace."}
       "n" {:type "integer"
            :minimum 1
            :maximum 2000
            :description "logs only: tail the last n lines (default 200, max 2000)."}
       "text" {:type "string" :description "send only: text written to the shell's stdin."}
       "enter" {:type "boolean"
                :description "send only: append a newline to SUBMIT the line (default true)."}}
      :additionalProperties false}
     :before-fn (shell-gate-before-fn :shell)
     :tag :mutation
     :on-error-fn (shell-on-error :shell)}))

(def shell-symbols [shell-symbol])

(defn shell-attach-command
  "`vis extension shell attach <id>` — the human-side passthrough: join a live
   background shell's PTY in your OWN terminal (finish a browser OAuth, answer a
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
  "CLI surface mounted under `vis extension shell`. Only `attach` for now — the human
   passthrough onto a background PTY the agent spawned."
  [{:cmd/name "shell"
    :cmd/doc "Attach a real terminal to a live background shell (shell op \"background\")."
    :cmd/usage "vis extension shell attach <id>"
    :cmd/subcommands
    [{:cmd/name "attach"
      :cmd/doc
      "Join a background shell's PTY in your terminal; Ctrl-] detaches (child keeps running)."
      :cmd/usage "vis extension shell attach <id> [--socket PATH]"
      :cmd/owns-tty? true
      :cmd/examples ["vis extension shell attach slack-auth"
                     "vis extension shell attach dev-server"]
      :cmd/run-fn #'shell-attach-command}]}])

(vis/register-toggle!
  {:id "shell"
   :label "Shell commands"
   :description (str "Expose the ONE `shell` tool: bounded runs plus the background lifecycle "
                     "(background / logs / send / stop). When OFF the shell compatibility layer "
                     "is not bound. Contained by the OS process jail whenever it is ON.")
   :default true
   :owner :vis
   :persist? true
   :group :sandbox})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shell"
     :ext/description
     "Shell compatibility layer: ONE `shell` tool for bounded commands and background PTY resources. Its op (run / background / logs / send / stop) covers the full process lifecycle; stop is also reachable through resource_stop. Bound when the `shell` toggle is ON (default); contained by the OS process jail."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; Gated by the user-owned `shell` toggle (default ON). The OS process jail is
     ;; the containment layer while shell is active; flipping the toggle OFF unbinds
     ;; the `shell` tool on the next env build / reload.
     :ext/activation-fn (fn [_env]
                          (vis/toggle-enabled? "shell"))
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols shell-symbols}
     :ext/cli shell-cli}))

(vis/register-extension! vis-extension)
