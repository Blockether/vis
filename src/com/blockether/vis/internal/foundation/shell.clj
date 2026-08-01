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

   1. RUN (default) `await shell({\"commands\": [\"ls\"]})` — `bash -lc` in the
      workspace root, waits up to a timeout. Output is bounded at READ time to a
      head+tail budget per stream, so only the MIDDLE of a huge stream is
      dropped, never its start or end (a chatty-then-killed command cannot
      balloon the heap). A non-zero exit is DATA the model reads, not an error.

   2. BACKGROUND `await shell({\"commands\": [\"npm run dev\"], \"op\": \"background\", \"id\": \"dev\"})`
      — an `id` makes it background (the op may stay implicit): spawned under a
      REAL pty, its merged output pumped into a bounded ring buffer, registered
      as a session RESOURCE. Prefer this for long builds, test suites, servers,
      watchers, and interactive commands; reserve run for short bounded work.

   3. LOGS / SEND / STOP `await shell({\"op\": \"logs\", \"id\": \"dev\"})` — tail
      the ring buffer, type `{\"text\": \"…\"}` into the pty, or kill the tree.

   EVERY public call takes exactly one map. Process commands are its non-empty
   `commands` string array; command strings and command arrays are never
   positional. `text` is the map field only for `send` keystrokes. Resource IDs
   live in that same map for background/logs/send/stop.

   Every op answers with the SAME key set (`shell-result-base`): keys a stage does
   not fill are nil / false / 0 / [] instead of absent, so model Python indexes any
   field without a KeyError. A run always has one result entry under `commands`,
   containing each command line and its bytes.


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
            [com.blockether.vis.internal.foundation.serial-batch :as batch]
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

(defn- resolve-dir
  "Resolve a command's `cwd` against the primary workspace, then authorize the
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

     dir-value
     ;; `cwd` is THE name for a working directory across the tool surface
     ;; (repl, repl_eval, run_tests, the language packs, and Python's own
     ;; `os.getcwd`). There is no other spelling: `dir` is gone, not aliased.
     (get opts "cwd")

     requested
     (when-not (str/blank? (str (or dir-value ""))) (str dir-value))]

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
                                             {:type ::dir-unresolved
                                              :dir requested
                                              :resolved (.getPath dir)
                                              :exists (.exists dir)
                                              :roots root-paths}))
              (not (.exists dir))
              (throw (ex-info (str "shell cwd '" requested "' does not exist (" (.getPath dir) ").")
                              {:type ::dir-unresolved
                               :dir requested
                               :resolved (.getPath dir)
                               :exists false
                               :roots root-paths}))
              (not (.isDirectory dir))
              (throw
                (ex-info
                  (str "shell cwd '" requested "' is a file, not a directory (" (.getPath dir) ").")
                  {:type ::dir-unresolved
                   :dir requested
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
     ;; A STRING is ONE `bash -lc` line. A SEQUENTIAL is a literal argv run with no
     ;; shell at all — nothing to quote, nothing to interpret — which is how `git`
     ;; rides this same spawn/jail/capture machinery.
     (process-jail/wrap-argv
       (if (sequential? cmd) (mapv str cmd) [(bash-command) "--noprofile" "--norc" "-lc" (str cmd)])
       policy)

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
;; SYNC run — Python sandbox: `await shell({"commands": ["ls"]})`
;; =============================================================================

(defn- clamp-timeout-secs
  "Effective sync timeout from the opts value: default 120, floor 1, cap 600."
  ^long [v]
  (-> (long (or (->pos-long v "timeout_secs") default-timeout-secs))
      (max 1)
      long
      (min (long max-timeout-secs))))

(def ^:private shell-result-base
  "TOTAL key set of EVERY `shell` result — one tool, ONE result shape, and no
   second envelope anywhere: a serial `commands` batch answers THIS SAME map,
   filling `commands` with one full entry per command. `stage` names the stage
   that produced it (run / background / logs / send / stop); the keys that stage
   does not fill keep these neutral values instead of vanishing, so ordinary
   model Python (`r[\"exit\"]`, `r[\"lines\"]`, `r[\"commands\"]`) can never KeyError
   on a shell result — and never has to branch on which shape came back.

   There is deliberately NO `cmd` and NO `stdout`/`stderr` key here. A command
   line, and the bytes it emitted, belong to the COMMAND: they live on its
   [[command-result]] entry under `commands`, the one place either is ever found.
   The top level only summarises the group, which is why `started`, `exit`,
   `timed_out` and `duration_ms` are aggregates and never a second copy.

   NOT `\"op\"`: the extension boundary stamps `\"op\"` on EVERY tool result with the
   tool's own origin (always \"shell\" here) — tool-specific stage detail must use a
   different key, exactly as `stamp-public-result-op` requires."
  {"stage" nil
   "id" nil
   "cwd" nil
   ;; The commands THIS result is about, in input order, one full entry each: a
   ;; run's own per-command results (a lone command is a batch of ONE), or the
   ;; lines a background group was started with. Never bare strings, and empty
   ;; only when the stage genuinely has no command left (a stopped shell whose
   ;; registry entry is already gone).
   batch/commands-key []
   ;; run — GROUP summary of those entries, never a copy of their output.
   ;; `started` is true only once EVERY child was spawned, so a batch can tell a
   ;; command that never started apart from one that ran and failed/timed out.
   "started" false
   "exit" nil
   "duration_ms" nil
   "timed_out" false
   "timeout_secs" nil
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

(def ^:private command-result-base
  "TOTAL key set of ONE entry under `commands` — the only place a command line
   and its output ever live. Every command answers this same map whether it ran
   alone, as one line of a ten-line batch, or as a line of a backgrounded script,
   so `r[\"commands\"][i][\"stdout\"]` is the ONE read for a command's output and
   there is no lone-command variant to tell apart."
  {"cmd" nil
   "cwd" nil
   ;; True only after the child process was spawned, so a command that never
   ;; started stays distinguishable from one that ran and failed/timed out.
   "started" false
   "stdout" nil
   "stderr" nil
   "exit" nil
   "duration_ms" nil
   "timed_out" false
   "timeout_secs" nil
   ;; A truncated stream has an inline \"…[N chars omitted]…\" marker spliced into
   ;; its MIDDLE, so it is no longer valid JSON/parseable — the count says exactly
   ;; how much is gone.
   "stdout_truncated" false
   "stderr_truncated" false
   "stdout_omitted_chars" 0
   "stderr_omitted_chars" 0
   ;; Why a command produced no process at all (its dir refused, unspawnable).
   "status" nil
   "note" nil})

(defn- command-result
  "ONE command's own total entry: its fields merged onto [[command-result-base]]."
  [m]
  (merge command-result-base m))

(defn- shell-run-impl
  "Run ONE command and answer its own [[command-result]] entry — never a tool
   result: what the tool answers with is always the batch that OWNS this entry
   (`shell-batch-impl`), so a command's line and output have exactly one home.

   `cmd` is either one bash line (a string) or a literal argv (a sequential,
   used by `git`). The echoed `cmd` is always the display string."
  ([env cmd] (shell-run-impl env cmd nil))
  ([env cmd opts]
   (let
     [argv
      (when (sequential? cmd) (mapv str cmd))

      cmd
      (if argv (str/join " " argv) (str cmd))]

     (when (str/blank? cmd)
       (throw (ex-info (str "shell needs a non-blank command — pass it as `commands`,"
                            " the first argument.")
                       {:type ::blank-command})))
     (let
       [timeout-secs
        (clamp-timeout-secs (get opts "timeout_secs"))

        dir
        (resolve-dir (assoc (or opts {}) ::environment env))

        t0
        (now-ms)

        p
        (spawn! (or argv cmd) dir false (jail-policy env))

        empty-tail
        {:text "" :truncated false :omitted 0}

        ;; Separate reader futures per stream — avoids the classic full-pipe
        ;; deadlock on chatty commands. `read-capped` bounds memory to the
        ;; head+tail budget per stream at READ time (dropping only the MIDDLE
        ;; of a huge stream, not its start), so a megabyte-then-killed command
        ;; can't balloon the heap yet the opening context survives.
        out-f
        (future
          (read-capped (io/reader (.getInputStream p)) max-sync-head-chars max-sync-tail-chars))

        err-f
        (future
          (read-capped (io/reader (.getErrorStream p)) max-sync-head-chars max-sync-tail-chars))

        finished?
        (try (.waitFor p timeout-secs TimeUnit/SECONDS)
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
         [out
          (deref out-f 5000 empty-tail)

          err
          (deref err-f 5000 empty-tail)

          exit
          (when finished? (.exitValue p))

          t1
          (now-ms)]

         (command-result
           ;; TOTAL entry shape (`command-result-base`). The old "lean" map dropped a
           ;; key whenever it carried no signal, so ordinary model Python
           ;; (`c["stderr"]`, `c["timed_out"]`) died with a bare `KeyError` — read as
           ;; "the tool broke", retried with cosmetic variations, and spun.
           {"cmd" cmd
            ;; The child exists: this is intentionally distinct from a batch entry
            ;; whose launch failed before it could run.
            "started" true
            ;; A relative `cwd` is `/`-separated on every OS.
            "cwd" (paths/unixify (.getPath dir))
            "stdout" (lf (:text out))
            "stderr" (lf (:text err))
            "exit" exit
            "duration_ms" (- t1 t0)
            "timed_out" (not finished?)
            "timeout_secs" timeout-secs
            "stdout_truncated" (boolean (:truncated out))
            "stderr_truncated" (boolean (:truncated err))
            "stdout_omitted_chars" (long (or (:omitted out) 0))
            "stderr_omitted_chars" (long (or (:omitted err) 0))}))))))

(defn- batch-exit
  "ONE exit code for a whole batch: the FIRST non-zero exit — the command an `&&`
   chain would have stopped at — else the last command's own exit. nil only when
   no command produced an exit at all (nothing started, or the last one was
   killed on its timeout)."
  [results]
  (or (some (fn [r]
              (let [exit (get r "exit")]
                (when (and exit (not (zero? (long exit)))) exit)))
            results)
      (get (last results) "exit")))

(defn- batch-note
  "The group's `note`: how many commands ran, and where their output is — always
   `commands`, whether the caller sent one line or ten."
  [^long n]
  (str n
       (if (= 1 n) " command ran" " commands ran in order")
       "; each command's own cmd, stdout, stderr and exit is its entry in"
       " \"commands\"."))

(defn- shell-batch-impl
  "Run `commands` strictly in input order: each bounded foreground shell call
   finishes before the next begins. Same ordered-batch machinery
   (`serial-batch`) the `git` tool runs its own `commands` through.

   EVERY bounded run lands here — a lone command is a batch of ONE, exactly as
   `git` has no single-command shape either. The answer is ONE ordinary shell
   result, never a second envelope: the total `shell-result` map, whose
   `commands` carries each command's own full entry in input order while the
   top-level `cwd`/`started`/`exit`/`timed_out`/`duration_ms` SUMMARISE the group.
   Nothing is echoed twice: the result has no top-level `cmd` (the commands are
   `commands`) and no top-level `stdout`/`stderr` (the bytes stay with the command
   that emitted them), and `note` says exactly that in the data."
  [env commands opts]
  (let
    [commands
     (batch/ordered "shell" commands)

     _
     (when-not (every? string? commands)
       (throw (ex-info "shell commands must be strings \u2014 one bash -lc command line each."
                       {:type ::bad-commands})))

     _
     (when (some str/blank? commands)
       (throw (ex-info "shell commands must not contain blank commands." {:type ::blank-command})))

     results
     (batch/run-serial commands
                       #(shell-run-impl env % opts)
                       ;; An infrastructure failure (for example ProcessBuilder refusing
                       ;; its dir) must not erase the completed entries nor make later
                       ;; commands ambiguous. Keep the input-position result and continue.
                       (fn [command ^Exception e]
                         (command-result {"cmd" command
                                          "status" "not started"
                                          "note" (or (.getMessage e) (.getName (class e)))})))]

    (extension/success {:result
                        (shell-result
                          "run"
                          (merge {"cwd" (some #(get % "cwd") results)
                                  ;; Started only when EVERY child was spawned, so a
                                  ;; launch failure anywhere stays visible at the top.
                                  "started" (every? #(get % "started") results)
                                  "exit" (batch-exit results)
                                  "duration_ms" (reduce + 0 (keep #(get % "duration_ms") results))
                                  "timed_out" (boolean (some #(get % "timed_out") results))
                                  "timeout_secs" (clamp-timeout-secs (get opts "timeout_secs"))
                                  "note" (batch-note (count results))}
                                 (batch/result results)))
                        :op :shell})))

;; =============================================================================
;; BACKGROUND — Python sandbox: `await shell({"commands": ["npm run dev"], "op": "background", "id": …})`
;; =============================================================================

(defonce ^:private bg-procs
  ;; { session-key -> { id -> {:proc :buffer :exit :pump :stopped? :cmd :dir
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
   bridge was opened, `commands`/`cwd`/`pid` empty or nil only once the entry
   itself is gone.

   The shell's own command lines ride the SAME `commands` key a run answers with,
   one [[command-result]] entry each: a background group IS its script, so
   `commands` is where its lines are and the result never echoes them under a
   second name."
  [op id entry]
  (let
    [exit
     (some-> (:exit entry)
             deref)

     bridge
     (:bridge entry)]

    (shell-result op
                  {"id" id
                   batch/commands-key (mapv #(command-result {"cmd" % "started" true})
                                            (:commands entry))
                   "cwd" (:dir entry)
                   "pid" (:pid (:proc entry))
                   "started" true
                   "status" (if (some? exit) "exited" "running")
                   "exit" exit
                   "uptime_ms" (- (now-ms) (long (or (:started-at entry) (now-ms))))
                   "attach" (when bridge (str "vis extension shell attach " id))
                   "socket" (:path bridge)})))

(defn- shell-bg-spawn!
  "Spawn a NEW background PTY under `id`. Callers guarantee no LIVE entry holds
   the id (`shell-bg-impl` owns that check); an exited-but-unread entry under
   the same id is replaced, discarding its retained logs by intent."
  [env id commands opts]
  (let
    [session
     (:session-id env)

     id
     (str id)

     commands
     (vec commands)

     ;; ONE background shell runs the WHOLE ordered group, so the lines become one
     ;; script for the pty. The lines themselves stay in `commands` on the entry —
     ;; this joined form is only what the process is fed and what a card displays.
     script
     (str/join "\n" commands)]

    (when (str/blank? id)
      (throw (ex-info "The shell background op needs a non-blank resource id ({\"id\": …})."
                      {:type ::blank-id})))
    (when (str/blank? script)
      (throw (ex-info "The shell background op needs its `commands` as the first argument."
                      {:type ::blank-command})))
    (when (bg-entry session id) (resources/unregister! session id) (drop-bg-entry! session id))
    (let
      [dir
       (resolve-dir (assoc (or opts {}) ::environment env))

       p
       (pty-spawn! script dir (jail-policy env))

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
         :commands commands
         :script script
         :dir (.getPath dir)
         :started-at t0})
      (resources/register! session
                           {:id id
                            :kind :shell
                            :label (one-line script 48)
                            :detail script
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
         {:command script :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}}))))

(defn- shell-bg-impl
  "`await shell({\"commands\": [\"npm run dev\"], \"op\": \"background\", \"id\": id})` — IDEMPOTENT on a live id.

   Re-using an id whose process is still running used to THROW. That reads as a
   plain tool failure: a model that already started the shell (or that lost the
   result) learns nothing actionable, invents a new id, and spins — the exact
   runaway seen on `companion-dev`. Instead, return the RUNNING shell flagged
   `already_running` with its pid/uptime and the `logs` handle, so \"start it\" is
   answered by \"it IS started, here is how to watch it\". No second process is
   spawned; a genuinely fresh one needs a `stop` op first."
  [env id commands opts]
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
         :metadata {:command (:script live)
                    :pid (:pid (:proc live))
                    :started-at-ms (:started-at live)
                    :finished-at-ms (now-ms)
                    :duration-ms 0}})
      (if-let
        [commands (some->> commands
                           (map str)
                           (remove str/blank?)
                           seq
                           vec)]
        (shell-bg-spawn! env id commands opts)
        (throw (ex-info (str "No background shell '"
                             id
                             "' is running, so it must be STARTED:"
                             " pass {\"commands\": [\"…\"], \"op\": \"background\", \"id\": \""
                             id
                             "\"} as one shell map.")
                        {:type ::missing-command :op "background" :id id}))))))

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
       (throw (ex-info
                (str "No background shell '"
                     id
                     "' in this session — start one with"
                     " await shell({\"commands\": [\"…\"], \"op\": \"background\", \"id\": id});"
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
      (let [e (first (remove nil? (map #(get opts %) ["is_enter" :is_enter "enter" :enter])))]
        (if (nil? e) true (boolean e)))]

     (when-not entry
       (throw (ex-info
                (str "No background shell '"
                     id
                     "' in this session — start one with"
                     " await shell({\"commands\": [\"…\"], \"op\": \"background\", \"id\": id});"
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
;; ONE lifecycle entry point — Python: `await shell({\"commands\": [\"ls\"]})`
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

(defn shell-dispatch
  "One shell lifecycle grammar. Every public call takes exactly one options map;
   process commands are the `commands` string array in that map."
  [env opts]
  (when-not (opts-arg? opts)
    (throw (ex-info "shell takes one options map, e.g. await shell({\"commands\": [\"ls\"]})."
                    {:type ::bad-options})))
  (let
    [opts
     (into {}
           (remove (fn [[_ v]]
                     (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v)))))
           opts)

     commands
     (opt opts :commands)

     text
     (opt opts :text)

     _
     (when (some? (opt opts :cmd))
       (throw (ex-info "shell has no `cmd` option — put bash lines in {\"commands\": [...]}."
                       {:type ::legacy-command-carrier})))

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

     checked-commands
     (fn []
       (let [lines (batch/ordered "shell" commands)]
         (when-not (every? string? lines)
           (throw (ex-info "shell commands must be strings — one bash -lc command line each."
                           {:type ::bad-commands})))
         (when (some str/blank? lines)
           (throw (ex-info "shell commands must not contain blank commands."
                           {:type ::blank-command})))
         lines))

     valid-commands
     (when (some? commands) (checked-commands))

     need-commands
     (fn []
       (or valid-commands
           (throw (ex-info
                    (str "shell op \"" op "\" needs {\"commands\": [\"…\"]} in its options map.")
                    {:type ::missing-command :op op}))))

     need-id
     (fn []
       (or id
           (throw (ex-info (str "shell op \""
                                op
                                "\" needs {\"id\": \"…\"}; live ids are listed in resources.")
                           {:type ::missing-id :op op}))))

     reject-commands
     (fn []
       (when (some? commands)
         (throw (ex-info (str "shell op \"" op "\" takes no commands — it acts on {\"id\": \"…\"}.")
                         {:type ::unexpected-commands :op op}))))

     reject-text
     (fn []
       (when (some? text)
         (throw (ex-info (str "shell op \"" op "\" takes no text payload.")
                         {:type ::unexpected-text :op op}))))

     need-text
     (fn []
       (cond (nil? text) ""
             (string? text) text
             :else (throw (ex-info "shell `text` must be a string." {:type ::bad-text}))))]

    (case op
      "run"
      (do (reject-text) (shell-batch-impl env (need-commands) opts))

      "background"
      (do (reject-text) (shell-bg-impl env (need-id) valid-commands opts))

      "logs"
      (do (reject-commands) (reject-text) (shell-logs-impl env (need-id) (opt opts :n)))

      "send"
      (do (reject-commands) (shell-send-impl env (need-id) (need-text) opts))

      "stop"
      (do (reject-commands) (reject-text) (shell-stop-impl env (need-id)))

      (throw (ex-info (str
                        "Unknown shell op "
                        (pr-str op)
                        " — use \"run\" (default), \"background\", \"logs\", \"send\" or \"stop\".")
                      {:type ::unknown-op :op op})))))

(defn run-argv
  "Run ONE literal argv through the SAME bounded machinery `shell` runs its own
   commands with: cwd authorization, process-jail policy, head+tail capped
   capture, timeout and kill-tree. Returns that command's own total entry — the
   SAME `command-result` map (`cmd`, `stdout`, `stderr`, `exit`, `duration_ms`,
   `timed_out`, truncation flags) `shell` puts under `commands`, so there is one
   command shape for both tools and no envelope to unwrap.

   No shell is involved — each element reaches the process verbatim, so nothing
   needs quoting. The `git` tool is a USER of this: every git command is a
   bounded shell command, so both tools share one runner, one jail and one
   capture policy."
  ([env argv] (run-argv env argv nil))
  ([env argv opts] (shell-run-impl env (vec argv) opts)))

(defn jailed-shell
  "Run a Python extension's shell request through the invoking session's jail.
   The only public form is `vis.shell({\"commands\": [\"ls\"]})`; it shares
   the same one-map grammar as the native shell tool."
  [env opts]
  (when-not (:session-id env)
    (throw (ex-info "jailed_shell is available only while handling a session"
                    {:type ::no-session})))
  (shell-dispatch env opts))


;; =============================================================================
;; Env injection — the before-fn hands the impl its env as first arg
;; =============================================================================

(defn- op-label
  "Human call name from an op keyword: :shell -> \"shell\"."
  [op]
  (if (namespace op) (str (namespace op) "_" (name op)) (name op)))

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
    "In `python_execution`, await every call with ONE map:
await shell({\"commands\": [\"git status\"]})
await shell({\"commands\": [\"npm run build\"], \"cwd\": \"web\"})
await shell({\"commands\": [\"npm ci\", \"npm test\"], \"op\": \"background\", \"id\": \"ci\"})
await shell({\"op\": \"logs\", \"id\": \"ci\", \"n\": 500})
await shell({\"op\": \"send\", \"id\": \"ci\", \"text\": \"y\"})
await shell({\"op\": \"stop\", \"id\": \"ci\"})

THE one shell tool. EVERY call takes exactly one map: process commands are the non-empty `commands` string array; never pass a command string or array positionally. `text` is only the keystrokes for `send`. op defaults to \"run\", or to \"background\" when an id is present.

Stages:
  run — bash -lc in the workspace root; blocks until commands exit. Reserve it for short bounded work. opts: timeout_secs (default 120, max 600), cwd. A non-zero exit is DATA to read, not a tool failure.
  background — returns immediately with no timeout and makes the shell an OWNED session resource under id. PREFER it for commands that may take a while: builds, test suites, daemons, watchers, and interactive work. Poll with logs instead of blocking a run call.
  logs — last 200 lines, or n up to 2000.
  send — types `text` into the pty stdin; it is never trimmed, so control characters and a bare newline arrive intact. is_enter (default true) appends the newline that SUBMITS the line.
  stop — kill the process tree, discard retained logs, and drop the session resource. Uses the same stop path as resource_stop.

EVERY call returns the SAME keys — {\"stage\", \"id\", \"cwd\", \"commands\", \"started\", \"exit\", \"duration_ms\", \"timed_out\", \"timeout_secs\", \"pid\", \"status\", \"uptime_ms\", \"attach\", \"socket\", \"already_running\", \"note\", \"lines\", \"line_count\", \"dropped\", \"sent\", \"text\", \"keys\", \"stopped\"}. A run has one full entry under result `commands`; lifecycle stages leave it empty."
    :arglists '([opts])}
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

(defn- result-script
  "The command lines a background/lifecycle result was started with, joined as one
   display string. They live under `commands` (one entry per line, never under a
   top-level `cmd`), so a card renders them from the ONE place they exist."
  [r]
  (some->> (get r batch/commands-key)
           seq
           (map #(get % "cmd"))
           (str/join "\n")
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
    (cond (and (contains? r "started") (not (get r "started")))
          {:icon "⊘" :label "not started" :failed? true}
          (get r "timed_out") {:icon "⏱"
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
     (kv-lines [["started" (when (contains? r "started") (if (get r "started") "yes" "no"))]
                ["status" label] ["duration" duration] ["cwd" (get r "cwd")]
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
     (->> [(shell-section "COMMAND" (format-shell-command (result-script r)) "bash")
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
                                   ["cmd" (shell-one-line (result-script r))]
                                   ["uptime" (duration-label (get r "uptime_ms"))]]))})

(defn- shell-batch-tally
  "Outcome tail of a shell batch headline. A shell command can fail BEFORE it
   starts (its dir refused, the process unspawnable), which git's simpler
   succeeded/failed split cannot express — hence a shell-specific tally."
  [results]
  (let
    [started
     (count (filter #(get % "started") results))

     failures
     (count (filter #(and (get % "started") (batch/failed? %)) results))]

    (str started
         " ran, "
         (- (count results) started)
         " not started, "
         (- started failures)
         " succeeded, "
         failures
         " failed")))

(defn- render-shell-batch-result
  "Render the commands a run executed, preserving each result. ONE command — the
   overwhelmingly common run — renders as its own REPL-style card: the batch is
   the SHAPE, not a rendering, and a single command deserves no card numbering.
   Several commands share `serial-batch/card` with `git`."
  [r]
  (let [results (vec (get r batch/commands-key))]
    (if (= 1 (count results))
      (render-shell-run-result (first results))
      (batch/card {:icon "$"
                   :noun "shell"
                   :results results
                   :render-one render-shell-run-result
                   :tally-fn shell-batch-tally}))))

(defn- render-shell-result
  "Render one shell result. `stage` is ALWAYS stamped, so dispatch on it first: a
   `run` is its commands (a batch), every lifecycle stage is its own card. Neither
   branch sniffs keys, and the legacy fallback only renders results old enough to
   pre-date `stage` (persisted events from a prior version)."
  [r]
  (case (str (get r "stage"))
    "run"
    (render-shell-batch-result r)

    "logs"
    (render-shell-logs-result r)

    "send"
    (render-shell-send-result r)

    "stop"
    (render-shell-stop-result r)

    "background"
    (render-shell-bg-result r)

    ;; Legacy: a result without `stage` (pre-canonicalisation) — only ever a run or
    ;; a background, distinguished by whether it actually carries command entries.
    (if (batch/batch? r) (render-shell-batch-result r) (render-shell-bg-result r))))

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
     "ONE shape for every call. A run has one command entry under `commands`; the top level summarises it."
     :name "shell"
     :description
     (str
       "THE one shell tool — bounded run, background, logs, send, stop. EVERY call takes one options map: "
       "await shell({\"commands\": [\"ls\"]}). Never pass a command string or an array as a positional argument. "
       "`op` defaults to \"run\", or \"background\" with an `id`. PREFER background for builds, tests, servers, watchers, or interactive work; poll with \"logs\". "
       "`send` types map `text` into a live shell's pty; `stop` kills it. `background` on a LIVE id returns that same shell (`already_running`) and needs no commands. "
       "Live ids are in `session[\"resources\"]`. A run's output is in `r[\"commands\"][0]`; huge output is truncated MID-stream — check `stdout_truncated` before parsing it. "
       "`logs` hands back the tail ONCE as `lines` (plain strings). In `python_execution` await `shell`.")
     :render render-shell-result
     :color-role :tool-color/shell
     :schema
     {:type "object"
      :properties
      {"commands"
       (batch/commands-property
         {:items {:type "string"}
          :description
          "Ordered bash -lc command lines. Required for run and a new background shell; one command is [\"ls\"]. Every shell command call is this one options map — never a positional string or array."})
       "op" {:type "string"
             :enum ["run" "background" "logs" "send" "stop"]
             :description "Operation (default \"run\", or \"background\" when an `id` is given)."}
       "id" {:type "string"
             :minLength 1
             :description
             "Background shell resource id — the same id for background / logs / send / stop."}
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
       "text"
       {:type "string"
        :description
        "send only: exact keystrokes to write to the live pty; this remains in the one options map."}
       "is_enter" {:type "boolean"
                   :description "send only: append a newline to SUBMIT the line (default true)."}}
      :additionalProperties false}
     :inject-env? true
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
