(ns com.blockether.vis.internal.foundation.shell
  "`shell/` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature). Bound only when the user-owned `shell` toggle is ON
   (default ON; flip it OFF in Settings or in `vis.yml` via `toggles: {shell: false}`
   to drop the tools). The OS process jail is the containment layer while active.

   FIVE model-facing bindings — `shell_run`, `shell_background`, `shell_logs`,
   `shell_type`, `shell_stop` — bound BARE in the flat Python sandbox next to
   `git` / `cat` / `grep`. One tool is one verb with one satisfiable schema and
   one result shape: a single `op`-discriminated tool made every precondition
   conditional prose, and the model discovered the contract by failing a call.

   1. `await shell_run([\"ls\"])` — `bash -lc` in the workspace root, bounded by an
      internal deadline. Output is bounded at READ time to a head+tail budget per
      stream, so only the MIDDLE of a huge stream is dropped, never its start or
      end (a chatty-then-killed command cannot balloon the heap). A non-zero exit
      is DATA the model reads, not an error.

   2. `await shell_background([\"npm run dev\"], id=\"dev\")` — spawned under a REAL
      pty, its merged output streamed verbatim to a log FILE, registered as a
      session RESOURCE. Prefer this for long builds, test suites, servers,
      watchers, and interactive commands; reserve `shell_run` for short bounded
      work.

   3. `await shell_logs(\"dev\")` reads that log from a byte OFFSET and returns
      NOW, `await shell_type(\"dev\", \"y\")` types into the pty,
      `await shell_stop(\"dev\")` kills the tree. NOTHING blocks on the model's
      behalf: waiting is control flow, and control flow belongs in a bounded
      `python_execution` loop that can break on what it actually read.

   `shell-dispatch` survives as the INTERNAL grammar the Python-extension entry
   points use, since those hand-author an options map and genuinely need an `op`.

   Every stage answers a stage-SCOPED total key set ([[result-core]] plus that one
   stage's own keys): a key the stage owns is nil / false / 0 / [] instead of
   absent, so model Python indexes it without a KeyError, while another stage's
   keys are simply not there to carry nothing. A run always has one result entry
   per command under `commands`, holding that command line and its bytes.


   The `shell` toggle is registered HERE, extension-owned under the vis namespace."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.egress-proxy :as egress]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway-sandbox :as gateway-sandbox]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.security-policy :as security-policy]
            [com.blockether.vis.internal.shell-log :as shell-log]
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

(def ^:private default-timeout-secs
  ;; ONE source of truth, shared with the Python eval watchdog: the watchdog must
  ;; sit a grace ABOVE this budget, or a shell call that names no timeout races a
  ;; watchdog that started first and can never return its own timeout envelope.
  rt/DEFAULT_SHELL_TIMEOUT_SECS)

(def ^:private max-timeout-secs
  ;; Ten minutes, shared with the Python eval watchdog: the watchdog floors itself
  ;; above THIS, so no legal `run`/`wait` budget can be preempted from outside.
  rt/MAX_SHELL_TIMEOUT_SECS)

(def ^:private max-sync-head-chars
  "Prefix of a SYNC stream always CAPTURED: the command's OPENING context —
   compile errors, the first failing assertion, the banner that says WHAT ran."
  100000)

(def ^:private max-sync-tail-chars
  "Suffix of a SYNC stream always CAPTURED: build / test failures and the final
   summary live at the END. Only the MIDDLE is dropped when a stream is huge —
   never the head, never the tail — so nothing important silently disappears.

   head+tail is the CAPTURE budget, NOT the display budget. The old 4k+12k pair
   mangled every ordinary machine-readable payload above 16k chars: a plain
   `gh issue list --json …` (21k chars) came back with the omitted-marker spliced
   at char 4000, so `json.loads(r[\"commands\"][0][\"stdout\"])` died on
   \"Invalid control character\" EVERY time. Cards clip separately
   ([[clip-stream]]) and the loop clips the wire, so capturing a parseable
   stream costs context nothing."
  300000)

(def ^:private max-bg-lines
  "Ring-buffer cap per background shell; older lines are dropped (counted)."
  2000)

(def ^:private max-line-chars
  "Per-line char cap in the background pump. A newline-free stream (e.g.
   `cat big.bin`) would otherwise let a line builder grow one unbounded line
   in memory; we force a break at this width instead."
  16000)


(defn- now-ms ^long [] (System/currentTimeMillis))

;; =============================================================================
;; Small helpers
;; =============================================================================

(defn- capped-capture
  "A capture of ONE stream that can be READ WHILE IT IS STILL FILLING.

   `:drain!` pumps a Reader to EOF keeping the HEAD and the TAIL of the stream and
   dropping only the MIDDLE when it exceeds `head-limit`+`tail-limit` — so neither
   the opening context nor the closing failure/summary is ever silently lost (the
   old tail-only cap swallowed everything before the last N chars). Memory stays at
   ~cap: the middle is collapsed at read time, so a megabyte-then-killed command
   cannot balloon the heap. It never throws — a stream closed mid-read just ends
   the drain.

   `:snapshot` answers `{:text :truncated :omitted}` for what has arrived SO FAR,
   with the exact dropped-char count (0 when nothing was dropped) and a visible
   omitted-count marker spliced in at the boundary. Snapshotting mid-stream is the
   whole point: a run that outstays its wait is no longer killed, so the bytes it
   had already printed must be answerable before its stream ends."
  [^long head-limit ^long tail-limit]
  (let
    [sb
     (StringBuilder.)

     cap
     (+ head-limit tail-limit)

     total
     (atom 0)

     trunc
     (atom false)]

    {:drain! (fn [^java.io.Reader r]
               (let [buf (char-array 8192)]
                 (try (loop []

                        (let [n (.read r buf 0 (alength buf))]
                          (when (pos? n)
                            (locking sb
                              (swap! total (fn [t]
                                             (+ (long t) n)))
                              (.append sb buf 0 n)
                              (when (> (.length sb) cap)
                                (reset! trunc true)
                                ;; keep the first `head-limit` chars + the last `tail-limit`;
                                ;; excise the run between them so memory stays at ~cap.
                                (.delete sb (int head-limit) (int (- (.length sb) tail-limit)))))
                            (recur))))
                      (catch Throwable _ nil))))
     :snapshot (fn []
                 (locking sb
                   (let
                     [s
                      (.toString sb)

                      omitted
                      (if @trunc (- (long @total) cap) 0)]

                     {:text (if @trunc
                              (str (subs s 0 head-limit)
                                   "\n\n…[" omitted
                                   " chars omitted]…\n\n" (subs s head-limit))
                              s)
                      :truncated @trunc
                      ;; Exact dropped-char count: the text now carries an inline marker, so a
                      ;; caller can SEE both that it is no longer parseable and how much is gone.
                      :omitted omitted})))}))

(defn- truncation-note
  "Note for a command whose capture lost a middle. Truncation splices a marker
   into the text, so the stream is no longer valid JSON/CSV — say so, and say
   what to do, instead of leaving a caller to decode a parser's
   \"Invalid control character\" on its own. nil when nothing was dropped."
  [out err]
  (let
    [dropped
     (fn [label m]
       (let [n (long (or (:omitted m) 0))]
         (when (pos? n) (str label " truncated · " n " chars dropped from the middle"))))

     parts
     (keep identity [(dropped "stdout" out) (dropped "stderr" err)])]

    (when (seq parts)
      (str (str/join " · " parts)
           " — the text carries an inline marker and no longer parses; narrow the output"
           " (`--jq`, `--limit`, `head -c`) or redirect it to a file and read that."))))

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
          ;; A root the draft policy withholds is not a working directory:
          ;; `shell` must not offer what `safe-path` refuses.
          (remove :denied?)
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

(defn- command-note
  "The single advisory line a command result carries. Truncation wins — it changes
   whether the text parses at all; otherwise a denied macOS Keychain lookup is
   named, since a confined `gh`/`git`/`security` otherwise fails with an opaque
   Security-framework message and no mention of the jail."
  [env out err]
  (or (truncation-note out err)
      (when (process-jail/keychain-denial? (:text out) (:text err))
        ;; No policy fn means no jail: an opaque Security failure is then a real
        ;; Keychain miss and blaming confinement would send the caller the wrong way.
        (when-let [policy (try (jail-policy env) (catch Throwable _ nil))]
          (process-jail/keychain-denial-hint policy (:text out) (:text err))))))

(defn- fd-exhaustion?
  "True when `t` (or any cause under it) is the OS refusing a spawn because THIS
   process ran out of file descriptors (EMFILE).

   The JDK reports EMFILE as a plain `IOException` whose text blames the spawn
   helper — \"Bad file descriptor\", \"Spawn helper ran into JDK version mismatch\",
   \"Re-install JDK\" — so the raw message sends the reader after a toolchain bug
   that does not exist. `error=24` / `error: 24` and the plain phrase are the
   reliable markers."
  [^Throwable t]
  (boolean (loop [^Throwable e t]
             (when e
               (let [m (str (.getMessage e))]
                 (if (or (str/includes? m "Too many open files")
                         (str/includes? m "error=24")
                         (str/includes? m "error: 24"))
                   true
                   (recur (.getCause e))))))))

(def ^:private fd-exhausted-message
  "What to SAY when a spawn is out of descriptors — the JDK's own text names the
   wrong culprit, so replace it with the real cause and the way out."
  (str "Out of file descriptors: this process hit its open-file limit, so no child "
       "process can be spawned (the JDK blames its spawn helper / a JDK mismatch; "
       "it is neither). Already retried once after asking the JVM to reclaim "
       "descriptors held by unreachable objects. Usual cause: sandbox Python that "
       "opened files without closing them — a dropped file object is NOT closed "
       "there, so always `with open(...) as f:` — or many live background shells "
       "(`shell_stop`). Free them and retry."))

(defn- spawn-retrying-fds
  "Run `spawn` (a thunk that starts an OS process) and, ONLY when it failed
   because this process is out of file descriptors, reclaim and try once more.

   A leaked descriptor is held by an unreachable object, so a GC + finalization
   pass genuinely returns it; without this, one leaky sandbox block wedges EVERY
   later `shell`/`git` call for the rest of the session. A persistent failure is
   rethrown as a typed `ex-info` carrying the real diagnosis, cause attached."
  [spawn]
  (try (spawn)
       (catch Throwable t
         (if-not (fd-exhaustion? t)
           (throw t)
           (do (System/gc)
               (System/runFinalization)
               (Thread/sleep 150)
               (try (spawn)
                    (catch Throwable t2
                      (throw (ex-info fd-exhausted-message {:type ::fd-exhausted} t2)))))))))
(defn- cleanup-jail-policy!
  [policy]
  (when-let [cleanup (::cleanup policy)]
    (try (cleanup) (catch Throwable _ nil))))

(defn- resolve-dir-for-policy
  ^File [opts env policy]
  (try (resolve-dir (assoc (or opts {}) ::environment (or (::environment policy) env)))
       (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

(defn- spawn!
  ^Process [cmd ^File dir merge-err? policy]
  (try
    (let
      [^java.util.List args
       ;; A STRING is ONE `bash -lc` line. A SEQUENTIAL is a literal argv run with no
       ;; shell at all — nothing to quote, nothing to interpret — which is how `git`
       ;; rides this same spawn/jail/capture machinery.
       ;; The detach prefix (when the platform has one) goes OUTSIDE the jail wrapper:
       ;; it only setpgid()s and execs, so everything that actually RUNS is still jailed.
       (process-jail/detached-argv (process-jail/wrap-argv (if (sequential? cmd)
                                                             (mapv str cmd)
                                                             [(bash-command) "--noprofile" "--norc"
                                                              "-lc" (str cmd)])
                                                           policy))

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
      (let [^Process p (spawn-retrying-fds #(.start pb))]
        (when (::cleanup policy)
          (let [^java.util.concurrent.CompletableFuture done (.onExit p)]
            (.thenRun done
                      ^Runnable
                      (reify
                        Runnable
                          (run [_] (cleanup-jail-policy! policy))))))
        p))
    (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

(defn- pty-spawn!
  "Spawn `cmd` under a REAL pseudo-terminal (internal.foundation.pty — pure Java
   FFM, no JNA and no extracted native helper): isatty() is TRUE, $TERM is set,
   and stdin is writable (the send op) — so interactive CLIs that refuse a dumb
   pipe (browser-auth prompts, password `read`, REPLs) actually run. Returns the
   pty HANDLE MAP (`:pid :in :send :wait :alive? :destroy`) that the pump /
   kill-tree! / wait path below consume. stdout+stderr share the one PTY stream
   (a real terminal has no separate error channel), so no merge-err? knob.
   `policy` is the jail policy value applied to this spawn; a fresh-config policy
   carries an idempotent cleanup callback retained for the process lifetime."
  [cmd ^File dir policy]
  (try (assoc (spawn-retrying-fds
                (fn []
                  (pty/spawn! {:command (process-jail/wrap-argv [(bash-command) "--noprofile"
                                                                 "--norc" "-lc" (str cmd)]
                                                                policy)
                               :dir (.getPath ^File dir)
                               :env (if-let [full (process-jail/jailed-child-env policy)]
                                      ;; Confined child: allowlisted env only (secrets dropped).
                                      (doto (HashMap. ^java.util.Map full)
                                        (.put "TERM" "xterm-256color"))
                                      (doto (HashMap. ^java.util.Map (System/getenv))
                                        (.put "TERM" "xterm-256color")
                                        (.putAll ^java.util.Map (process-jail/proxy-env policy))))
                               :cols 120
                               :rows 40})))
         ::cleanup (::cleanup policy))
       (catch Throwable t (cleanup-jail-policy! policy) (throw t))))

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
;; SYNC run — Python sandbox: `await shell_run(["ls"])`
;; =============================================================================

(defn- clamp-timeout-secs
  "Effective sync timeout from the opts value: default 120, floor 1, cap 600."
  ^long [v]
  (-> (long (or (->pos-long v "timeout_secs") default-timeout-secs))
      (max 1)
      long
      (min (long max-timeout-secs))))

(def ^:private result-core
  "Keys EVERY `shell` result carries, whatever stage produced it: who was asked,
   and the GROUP summary. `stage` names the stage that produced it (run /
   background / logs / wait / send / stop) and these keys stay present-but-neutral
   rather than vanishing, so ordinary model Python (`r[\"exit\"]`,
   `r[\"commands\"]`) can never KeyError on a field every stage owns.

   There is deliberately NO `command` and NO `stdout`/`stderr` here. A command
   line, and the bytes it emitted, belong to the COMMAND: they live on its
   [[command-result]] entry under `commands`, the one place either is ever found.
   The top level only summarises, which is why `started`, `exit`, `timed_out`
   and `duration_ms` are aggregates and never a second copy.

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
   ;; `started` is true only once EVERY child was spawned, so a batch can tell a
   ;; command that never started apart from one that ran and failed/timed out.
   "started" false
   "exit" nil
   "duration_ms" nil
   "timed_out" false
   "note" nil})

(def ^:private stage-keys
  "What ONE stage adds to [[result-core]] — and the only extra keys it may carry.

   Totality is owed WITHIN the stage the caller selected: lifecycle stages expose
   only their own fields while the shared identity/summary fields stay in
   [[result-core]]."
  {"run" {"timeout_secs" nil}
   "background"
   {"pid" nil "status" nil "uptime_ms" nil "attach" nil "socket" nil "already_running" false}
   "logs" {"pid" nil
           "status" nil
           "uptime_ms" nil
           "text" ""
           "offset" 0
           "next_offset" 0
           "is_eof" true
           "is_truncated" false}
   "send" {"pid" nil "status" nil "sent" 0 "text" nil "keys" nil}
   "stop" {"pid" nil "status" nil "uptime_ms" nil "stopped" false}})

(defn- shell-result
  "One stage's own fields merged onto THAT stage's total base, with `stage`
   stamped last. Fields outside the stage's own set are dropped, so no stage can
   leak another's keys back into the payload."
  [op m]
  (let [base (merge result-core (get stage-keys op))]
    (assoc (merge base (select-keys m (keys base))) "stage" op)))

(def ^:private command-result-base
  "TOTAL key set of ONE entry under `commands` — the only place a command line
   and its output ever live. Every command answers this same map whether it ran
   alone, as one line of a ten-line batch, or as a line of a backgrounded script,
   so `r[\"commands\"][i][\"stdout\"]` is the ONE read for a command's output and
   there is no lone-command variant to tell apart.

   Request-scope facts (`cwd`, `timeout_secs`) are NOT repeated per entry: they
   are identical for every command of a batch and already summarised at the top
   level. Nor are truncation booleans — `*_omitted_chars` is 0 exactly when
   nothing was dropped, so a flag beside it only said the same thing twice."
  {"command" nil
   ;; True only after the child process was spawned, so a command that never
   ;; started stays distinguishable from one that ran and failed/timed out.
   "started" false
   "stdout" nil
   "stderr" nil
   "exit" nil
   "duration_ms" nil
   "timed_out" false
   ;; A truncated stream has an inline \"…[N chars omitted]…\" marker spliced into
   ;; its MIDDLE, so it is no longer valid JSON/parseable — the count says exactly
   ;; how much is gone, and 0 means nothing was.
   "stdout_omitted_chars" 0
   "stderr_omitted_chars" 0
   ;; Why a command produced no process at all (its dir refused, unspawnable), or
   ;; that a captured stream lost its middle and no longer parses.
   "status" nil
   "note" nil})

(defn- command-result
  "ONE command's own total entry: its fields merged onto [[command-result-base]],
   with anything outside that set dropped."
  [m]
  (select-keys (merge command-result-base m) (keys command-result-base)))

(defn- shell-run-impl
  "Run ONE command and answer its own [[command-result]] entry — never a tool
   result: what the tool answers with is always the batch that OWNS this entry
   (`shell-batch-impl`), so a command's line and output have exactly one home.

   `cmd` is either one bash line (a string) or a literal argv (a sequential,
   used by `git`). The echoed `cmd` is always the display string.

   `handle` is what makes the run a HANDLE: `:sink` is the log file every captured
   byte is teed into, and `:on-spawn` is handed the live process the moment it
   exists, so the registry knows what is running before the wait even begins. A
   handled run that outstays its wait is NOT killed — the wait expired, the process
   did not, and the caller comes back to it by id — while a handle-less run (`git`)
   still dies on its deadline, because nothing could ever read it again."
  ([env cmd] (shell-run-impl env cmd nil nil))
  ([env cmd opts] (shell-run-impl env cmd opts nil))
  ([env cmd opts {:keys [sink on-spawn]}]
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

        policy
        (jail-policy env)

        dir
        (resolve-dir-for-policy opts env policy)

        t0
        (now-ms)

        p
        (spawn! (or argv cmd) dir false policy)

        _
        (when on-spawn (on-spawn p))

        ;; Separate reader futures per stream — avoids the classic full-pipe
        ;; deadlock on chatty commands. `capped-capture` bounds memory to the
        ;; head+tail budget per stream at READ time (dropping only the MIDDLE
        ;; of a huge stream, not its start), so a megabyte-then-killed command
        ;; can't balloon the heap yet the opening context survives — and it can
        ;; be snapshotted while the process is still printing.
        out-cap
        (capped-capture max-sync-head-chars max-sync-tail-chars)

        err-cap
        (capped-capture max-sync-head-chars max-sync-tail-chars)

        ;; Every captured byte is written through to the log file, so the handle's
        ;; log holds what the call returned AND everything printed after it.
        reader-of
        (fn [^java.io.InputStream s]
          (io/reader (if sink (shell-log/tee s sink) s)))

        out-f
        (future ((:drain! out-cap) (reader-of (.getInputStream p))))

        err-f
        (future ((:drain! err-cap) (reader-of (.getErrorStream p))))

        finished?
        (try (.waitFor p timeout-secs TimeUnit/SECONDS)
             (catch InterruptedException ie
               ;; Turn cancellation: kill the spawned tree before
               ;; the interrupt propagates to the loop.
               (kill-tree! p)
               (throw ie)))]

       (when (and (not finished?) (nil? sink))
         (kill-tree! p)
         ;; Closing the streams unblocks the reader futures on a wedged child
         ;; so their threads don't linger past our 5s deref ceiling.
         (doseq [^java.io.InputStream s [(.getInputStream p) (.getErrorStream p)]]
           (try (.close s) (catch Throwable _ nil))))
       ;; A handled run that timed out is STILL PRINTING: waiting on its drains
       ;; would be waiting on the very command whose wait already expired.
       (when (or finished? (nil? sink)) (deref out-f 5000 nil) (deref err-f 5000 nil))
       (let
         [out
          ((:snapshot out-cap))

          err
          ((:snapshot err-cap))

          exit
          (when finished? (.exitValue p))

          t1
          (now-ms)]

         (with-meta (command-result
                      ;; TOTAL entry shape (`command-result-base`). The old "lean" map dropped a
                      ;; key whenever it carried no signal, so ordinary model Python
                      ;; (`c[\"stderr\"]`, `c[\"timed_out\"]`) died with a bare `KeyError` — read as
                      ;; "the tool broke", retried with cosmetic variations, and spun.
                      {"command" cmd
                       ;; The child exists: this is intentionally distinct from a batch entry
                       ;; whose launch failed before it could run.
                       "started" true
                       "stdout" (lf (:text out))
                       "stderr" (lf (:text err))
                       "exit" exit
                       "duration_ms" (- t1 t0)
                       "timed_out" (not finished?)
                       ;; 0 exactly when nothing was dropped, so no truncation flag is owed
                       ;; beside it.
                       "stdout_omitted_chars" (long (or (:omitted out) 0))
                       "stderr_omitted_chars" (long (or (:omitted err) 0))
                       ;; A dropped middle makes the stream unparseable: name it here rather
                       ;; than let a caller's parser fail with an opaque message.
                       "note" (command-note env out err)})
           ;; Request scope, IDENTICAL for every entry of a batch: carried as metadata
           ;; so the group summarises one `cwd`/`timeout_secs` instead of every entry
           ;; repeating them, and nothing extra crosses to Python. A relative dir is
           ;; `/`-separated on every OS. A run left alive past its wait carries the
           ;; process and its still-running drains, which is what adoption needs.
           {:dir (paths/unixify (.getPath dir))
            :timeout-secs timeout-secs
            :process (when-not finished? p)
            :drains [out-f err-f]}))))))

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
       "; each command's own command, stdout, stderr and exit is its entry in"
       " \"commands\"."))

(defn- shell-quote
  "One argv token as a literal `bash -lc` word: plain words stay bare, anything
   else is single-quoted (with embedded quotes escaped), so a coerced argv keeps
   exactly the arguments it named."
  [^String token]
  (if (re-matches #"[A-Za-z0-9_@%+=:,./-]+" token)
    token
    (str "'" (str/replace token "'" "'\\''") "'")))

(defn- command-line
  "ONE bash line from a caller's entry. A string IS the line. An array of tokens —
   the argv spelling `git` takes, and the shape a caller reaches for out of habit
   — is coerced by quoting each token and joining, instead of failing the call;
   a `java.util.List` from the Python surface is that same array.

   A MAP is the one wrong shape that is not a typo but a MISPLACED CALL: this
   op's own arguments written where its process lines belong. It is refused by
   NAME, so the caller is told where `op`/`id`/`n` actually go instead of being
   told again that commands are strings."
  [command]
  (cond (string? command) command
        (or (map? command) (instance? java.util.Map command))
        (throw (ex-info
                 (str
                   "shell `commands` holds bash lines, not an options map: `op`, `id`, `n`, `until`"
                   " and `text` are TOP-LEVEL arguments — {\"op\": \"logs\", \"id\": \"build\","
                   " \"n\": 30} — and `commands` carries only run's or background's own lines.")
                 {:type ::bad-commands}))
        (or (sequential? command) (instance? java.util.List command))
        (str/join " " (map (comp shell-quote str) command))
        :else (throw (ex-info "shell commands must be strings — one bash -lc command line each."
                              {:type ::bad-commands}))))

(defn- ordered-lines
  "`commands` as the ordered batch of bash lines: `serial-batch/ordered` (so a
   bare string is the batch of ONE) plus [[command-line]] per entry. Only a blank
   line is refused — there is nothing to run."
  [commands]
  (let [lines (mapv command-line (batch/ordered "shell" commands))]
    (when (some str/blank? lines)
      (throw (ex-info "shell commands must not contain blank commands." {:type ::blank-command})))
    lines))


;; =============================================================================
;; BACKGROUND — Python sandbox: `await shell_background(["npm run dev"], id="dev")`
;; =============================================================================

(defonce ^:private bg-procs
  ;; { session-key -> { id -> {:proc :buffer :exit :pump :stopped? :cmd :dir
  ;; :started-at} } }. defonce so a dev `:reload` never orphans live processes.
  (atom {}))

(defonce ^:private bg-lifecycle-locks
  ;; A fixed stripe set keeps monitor identity stable across reloads without leaking
  ;; one lock per model-chosen id. Same session/id always lands on one stripe;
  ;; unrelated shells remain concurrent except for harmless hash collisions.
  (vec (repeatedly 64 #(Object.))))

(defn- bg-lifecycle-lock
  [session id]
  (nth bg-lifecycle-locks (mod (hash [(str session) (str id)]) (count bg-lifecycle-locks))))

(defonce ^:private _bridge-sweep
  ;; One-time GC at extension load: a prior vis crash/kill never ran serve!'s
  ;; :stop (the JVM held the AF_UNIX server), so stale attach sockets pile up in
  ;; bridge-dir. sweep-orphans! connect-probes each and unlinks the dead ones.
  (do (try (pty-bridge/sweep-orphans!) (catch Throwable _ nil)) true))

(defn- bg-entry [session id] (get-in @bg-procs [(str session) (str id)]))

(defn- bg-live?
  "True only while `id` still holds a RUNNING process in this session. An entry
   registered before its first process exists (a run claims its handle at the top
   of the batch) is not live yet."
  [session id]
  (boolean (when-let [entry (bg-entry session id)]
             (when-let [alive? (:alive? (:proc entry))]
               (alive?)))))

(defn- command->bg-id-slug
  "Name a background shell after the program it runs: `npm run dev` -> `npm`.
   Setup prefixes (`cd …`, `export …`, `VAR=1 …`) are skipped so the id names what
   is actually being watched, and a pathed binary keeps only its basename."
  [command]
  (let
    [program
     (->> (str/split (str command) #"&&|\|\||;|\n")
          (map str/trim)
          (remove str/blank?)
          (keep (fn [segment]
                  (->> (str/split segment #"\s+")
                       (remove str/blank?)
                       (remove #(str/includes? % "="))
                       first)))
          (remove #{"cd" "export" "source" "." "env" "sudo" "exec" "set" "nohup"})
          first)

     slug
     (-> (str (or program "shell"))
         (str/replace #"^.*/" "")
         str/lower-case
         (str/replace #"[^a-z0-9]+" "-")
         (str/replace #"^-+" "")
         (str/replace #"-+$" ""))]

    (if (str/blank? slug) "shell" (subs slug 0 (min 24 (count slug))))))

(defn- auto-bg-id
  "Derive the handle for a background START that carried none.

   Starting a long process is the ONE background call that does not act on an
   existing handle, so rejecting `{op: \"background\", commands: […]}` for a missing
   `id` failed a well-formed call over a name the caller had to invent — the most
   frequent shell dead end there is.

   Re-issuing the SAME script while it runs returns that shell's own id, so the
   duplicate start resolves to `already_running` instead of a second dev server;
   otherwise the program name is suffixed until no LIVE shell holds it, so an auto
   id never hijacks an unrelated running process."
  [session commands]
  (let
    [wanted
     (mapv str commands)

     running-same
     (->> (get @bg-procs (str session))
          (filter (fn [[id entry]]
                    (and (= wanted (vec (:commands entry))) (bg-live? session id))))
          ffirst)

     base
     (command->bg-id-slug (first wanted))]

    (or running-same
        (loop [n 1]
          (let [candidate (if (= 1 n) base (str base "-" n))]
            (cond (not (bg-live? session candidate)) candidate
                  (< n 100) (recur (inc n))
                  :else (str base "-" (System/nanoTime))))))))

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
  "Daemon thread: drain the process's merged output into its log FILE and the ring
   buffer, then record WHEN it ended and its exit code, and flip the registered
   resource to :exited.
   The resource stays listed (logs + exit readable) until resource_stop.

   The FILE is the log: every byte read is written through `sink`, so a reader can
   come back to any offset of it for as long as the session lives. The ring buffer
   is only the LINE view the attach bridge replays and the resource card shows, and
   it stays free to forget.

   `stopped?` is the cooperative-shutdown flag the stop-fn or an exited-entry
   replacement sets before retiring this generation. Final bridge/registry work
   is serialized with same-id start/stop and guarded by process identity, so an
   old pump can never update or unlink its successor. Returns the started Thread."
  ^Thread [session id p buffer exit-atom exited-at stopped? bridge-atom sink index-fn]
  (doto
    (Thread.
      (fn []
        ;; Char-level drain (not `line-seq`) so a newline-free stream
        ;; (`cat big.bin`) can't grow one unbounded line in memory: a line
        ;; is force-flushed at `max-line-chars`. The tee is UNDER that splitting,
        ;; so the file holds the stream exactly as the shell printed it.
        (try (with-open [r (io/reader (shell-log/tee ^java.io.InputStream (:in p) sink))]
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
        ;; The stream is finished, so the file is complete: flush and close it
        ;; BEFORE the exit code is published, or a reader that already saw
        ;; `exited` could still be missing the last buffered bytes.
        (shell-log/close! sink)
        (let [code (try ((:wait p)) (catch Throwable _ nil))]
          ;; Stamp the ENDING before publishing the code: `uptime_ms` is read off
          ;; these two atoms, and a reader that saw `exit` already set with no end
          ;; stamp would fall back to the clock and report the age of the READ.
          (compare-and-set! exited-at nil (now-ms))
          (reset! exit-atom code)
          (index-fn {:ended-at @exited-at :exit code})
          ;; Avoid contending with a manual stop in the normal case: it sets the
          ;; flag before closing the reader and then joins this thread.
          (when-not @stopped?
            #_{:clj-kondo/ignore [:locking-suspicious-lock]}
            (locking (bg-lifecycle-lock session id)
              ;; Re-check under the same monitor a replacement uses. If it won,
              ;; its entry and socket path belong to the successor and are sacred.
              (when (and (not @stopped?) (identical? p (:proc (bg-entry session id))))
                (when-let [b @bridge-atom]
                  (try ((:stop b)) (catch Throwable _ nil)))
                (try (resources/update! session
                                        id
                                        {:status :exited
                                         :detail
                                         (str "exit " code " — logs retained until resource_stop")})
                     (catch Throwable _ nil))))))
        (cleanup-jail-policy! {::cleanup (::cleanup p)})))
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
                   batch/commands-key (mapv #(command-result {"command" % "started" true})
                                            (:commands entry))
                   "cwd" (:dir entry)
                   "pid" (:pid (:proc entry))
                   "started" true
                   "status" (if (some? exit) "exited" "running")
                   "exit" exit
                   ;; LIFETIME, not the age of this read: it stops at the ending an
                   ;; earlier stage stamped, so a job that ran 3s never reports the ten
                   ;; minutes that passed before someone came back to read its logs.
                   "uptime_ms" (let
                                 [t0
                                  (long (or (:started-at entry) (now-ms)))

                                  t1
                                  (long (or (some-> (:exited-at entry)
                                                    deref)
                                            (now-ms)))]

                                 (max 0 (- t1 t0)))
                   "attach" (when bridge (str "vis-agent extension shell attach " id))
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
    (when-let [stale (bg-entry session id)]
      ;; The caller established that this generation has exited. Retire its
      ;; async pump/bridge under the lifecycle lock before reusing the socket path.
      ;; The pump re-checks this flag + process identity under the same lock, so it
      ;; cannot publish the old exit status onto the replacement.
      (reset! (:stopped? stale) true)
      (try (.close ^java.io.InputStream (:in (:proc stale))) (catch Throwable _ nil))
      (when-let [bridge (:bridge stale)]
        (try ((:stop bridge)) (catch Throwable _ nil)))
      (resources/unregister! session id)
      (drop-bg-entry! session id))
    (let
      [policy
       (jail-policy env)

       dir
       (resolve-dir-for-policy opts env policy)

       p
       (pty-spawn! script dir policy)

       ;; The log is a FILE, opened BEFORE the pump so not one byte of a fast
       ;; command's output can be printed before there is somewhere to keep it.
       sink
       (shell-log/open! session id)

       buffer
       (atom {:lines [] :next-seq 1 :dropped 0})

       exit-atom
       (atom nil)

       ;; Stamped ONCE, by whichever stage first observes the child is gone: an
       ;; uptime is a lifetime, so it must stop growing the moment the process ends
       ;; instead of measuring how long ago it was started.
       exited-at
       (atom nil)

       stopped?
       (atom false)

       bridge-atom
       (atom nil)

       t0
       (now-ms)

       ;; One sidecar row per log, so "what did that build print" is answerable a
       ;; turn later with no handle in hand. Written at spawn, and again by the
       ;; pump once the child is gone.
       index-fn
       (fn [m]
         (shell-log/index! (:db-info env)
                           session
                           id
                           (merge {:commands commands
                                   :script script
                                   :dir (.getPath dir)
                                   :log-path (:path sink)
                                   :started-at t0
                                   :ended-at nil
                                   :exit nil}
                                  m)))

       pump
       (start-pump! session id p buffer exit-atom exited-at stopped? bridge-atom sink index-fn)

       ;; Passthrough bridge: expose this PTY over a per-shell AF_UNIX socket
       ;; so a HUMAN can `vis-agent extension shell attach <id>` into the live terminal
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
      (index-fn nil)
      (swap! bg-procs assoc-in
        [(str session) id]
        {:proc p
         :buffer buffer
         :exit exit-atom
         :exited-at exited-at
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
                           {:stop-fn
                            (fn []
                              ;; Serialize teardown with replacement of this id. The
                              ;; registry claims the old resource before calling us, so a
                              ;; concurrent fresh start is valid; identity-check the final
                              ;; drop to prevent this old callback erasing its successor.
                              #_{:clj-kondo/ignore [:locking-suspicious-lock]}
                              (locking (bg-lifecycle-lock session id)
                                (reset! stopped? true)
                                (kill-tree! p)
                                ;; Close the read end so the pump's blocking `.read`
                                ;; returns even if a detached grandchild still holds the
                                ;; write end — the pump thread can't outlive the stop.
                                (try (.close ^java.io.InputStream (:in p)) (catch Throwable _ nil))
                                (try (.join pump 3000) (catch InterruptedException _ nil))
                                ;; Tear down the attach socket last: no more human attachers
                                ;; once the child is gone.
                                (when bridge (try ((:stop bridge)) (catch Throwable _ nil)))
                                (when (identical? p (:proc (bg-entry session id)))
                                  (drop-bg-entry! session id))))
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
         :op :shell-background
         :metadata
         {:command script :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}}))))

(defn- shell-bg-impl
  "`await shell_background([\"npm run dev\"], id=id)` — IDEMPOTENT on a live id.

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
     (str id)]

    ;; Starting a PTY and registering its lifecycle are side effects, so atomically
    ;; updating `bg-procs` alone cannot make the preceding live check safe. Keep the
    ;; entire check/spawn/register transition under the id's stable lifecycle lock:
    ;; concurrent starts must observe the first process, never each spawn a child
    ;; and overwrite its only stop handle. Waits/logs/sends and other ids stay free.
    #_{:clj-kondo/ignore [:locking-suspicious-lock]}
    (locking (bg-lifecycle-lock session id)
      (let
        [live (when-let [existing (bg-entry session id)]
                (when ((:alive? (:proc existing))) existing))]
        (if live
          (extension/success
            {:result (assoc (bg-core "background" id live)
                       "already_running" true
                       "note" (str "Background shell '"
                                   id
                                   "' was ALREADY running — nothing was restarted. Read its output "
                                   "with await shell_logs(\""
                                   id
                                   "\"). To start a fresh process, first run await shell_stop(\""
                                   id
                                   "\")."))
             :op :shell-background
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
                            {:type ::missing-command :op "background" :id id}))))))))

;; =============================================================================
;; RUN as a handle — a timeout is a WAIT that expired, not a lost process
;; =============================================================================

(defn- process-handle
  "A spawned `java.lang.Process` in the PTY handle SHAPE (`:pid :in :alive? :wait
   :destroy`) that `bg-core`, `kill-tree!` and the background registry already
   speak. A foreground run that outstays its wait is then an ORDINARY shell handle
   rather than a second kind of live process with its own lifecycle."
  [^Process p]
  {:pid (.pid p)
   :in (.getInputStream p)
   :alive? (fn []
             (.isAlive p))
   :wait (fn []
           (.waitFor p))
   :destroy (fn [force?]
              (if force? (.destroyForcibly p) (.destroy p)))})

(defn- adopt-run!
  "Promote a run that outstayed its wait into an ordinary background handle: the
   child is NOT killed, its log keeps filling, and `logs`/`stop` reach it by the id
   the run already answered with. The watcher thread finishes what the background
   pump finishes — wait out the drains, close the log, stamp the ending, publish the
   exit code — so a caller that comes back later reads a complete file and a real
   exit instead of a process nobody is accounting for."
  [{:keys [session id proc sink script cwd drains exit-atom exited-at stopped? index-fn]}]
  (swap! bg-procs assoc-in [(str session) id :dir] cwd)
  (index-fn {:dir cwd})
  (try (resources/register!
         session
         {:id id
          :kind :shell
          :label (one-line script 48)
          :detail script
          :pid (:pid proc)
          :owner "foundation-shell"
          :status :running}
         {:stop-fn (fn []
                     (reset! stopped? true)
                     (kill-tree! proc)
                     (try (.close ^java.io.InputStream (:in proc)) (catch Throwable _ nil))
                     (shell-log/close! sink)
                     (drop-bg-entry! session id))
          :alive-fn (fn []
                      (some? (bg-entry session id)))
          ;; A run keeps no line ring: the log FILE is the view, so the
          ;; registry card reads its tail the same way `logs` does.
          :logs-fn (fn []
                     (-> (shell-log/read-chunk id (shell-log/log-file session id))
                         :text
                         str/split-lines))
          :health-fn (fn []
                       (cond (nil? (bg-entry session id)) :down
                             (nil? @exit-atom) :running
                             (zero? (long @exit-atom)) :exited
                             :else :failed))})
       (catch Throwable _ nil))
  (doto (Thread.
          (fn []
            (doseq [f drains]
              (try (deref f) (catch Throwable _ nil)))
            ;; The stream is finished, so the file is complete: flush and close it
            ;; BEFORE the exit code is published, or a reader that already saw the
            ;; exit could still be missing the last buffered bytes.
            (shell-log/close! sink)
            (let [code (try ((:wait proc)) (catch Throwable _ nil))]
              (compare-and-set! exited-at nil (now-ms))
              (reset! exit-atom code)
              (index-fn {:dir cwd :ended-at @exited-at :exit code})
              (when-not @stopped?
                (try (resources/update! session
                                        id
                                        {:status :exited
                                         :detail
                                         (str "exit " code " — logs retained until resource_stop")})
                     (catch Throwable _ nil))))))
    (.setName (str "vis-shell-run-" id))
    (.setDaemon true)
    (.start))
  nil)

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
   that emitted them), and `note` says exactly that in the data.

   EVERY run IS a handle. It claims its `id` and its log file BEFORE it waits, so
   the result carries that id whether the batch finished or not, and a wait that
   expires is no longer a lost process: the command keeps running under its id,
   `shell_logs(id, offset=0)` reads everything the timed-out call never saw, and
   nothing after it is started, because the group is ordered and its first command
   has not finished. A batch that finished inside its wait drops the registry
   entry — there is no live process to account for — while its log file stays
   readable by id for as long as the session does."
  [env commands opts]
  (let
    [session
     (:session-id env)

     commands
     (ordered-lines commands)

     ;; The handle is named after the program, exactly as a background start is,
     ;; and never hijacks an id a live shell already holds.
     id
     (auto-bg-id session commands)

     script
     (str/join "\n" commands)

     ;; The log is a FILE, opened BEFORE the first spawn so not one byte of a fast
     ;; command's output can be printed before there is somewhere to keep it.
     sink
     (shell-log/open! session id)

     t0
     (now-ms)

     exit-atom
     (atom nil)

     exited-at
     (atom nil)

     stopped?
     (atom false)

     ;; The first command whose wait expired: everything after it stays unstarted,
     ;; and its process is what gets adopted.
     overdue
     (atom nil)

     index-fn
     (fn [m]
       (shell-log/index! (:db-info env)
                         session
                         id
                         (merge {:commands commands
                                 :script script
                                 :dir nil
                                 :log-path (:path sink)
                                 :started-at t0
                                 :ended-at nil
                                 :exit nil}
                                m)))

     on-spawn
     (fn [p]
       (swap! bg-procs assoc-in [(str session) id :proc] (process-handle p)))

     _
     (swap! bg-procs assoc-in
       [(str session) id]
       {:proc nil
        :buffer (atom {:lines [] :next-seq 1 :dropped 0})
        :exit exit-atom
        :exited-at exited-at
        :stopped? stopped?
        :commands commands
        :script script
        :dir nil
        :started-at t0})

     results
     (batch/run-serial
       commands
       (fn [command]
         (if @overdue
           (command-result {"command" command
                            "status" "not started"
                            "note" (str "The wait expired on an earlier command, which is still"
                                        " running as shell '" id
                                        "'; nothing after it was" " started.")})
           (let [r (shell-run-impl env command opts {:sink sink :on-spawn on-spawn})]
             (when (get r "timed_out") (reset! overdue (meta r)))
             r)))
       ;; An infrastructure failure (for example ProcessBuilder refusing
       ;; its dir) must not erase the completed entries nor make later
       ;; commands ambiguous. Keep the input-position result and continue.
       (fn [command ^Exception e]
         (command-result {"command" command
                          "status" "not started"
                          "note" (or (.getMessage e) (.getName (class e)))})))

     ;; Request scope lives on the GROUP, read back off the entries' metadata
     ;; rather than duplicated into each.
     cwd
     (some #(:dir (meta %)) results)

     timed-out
     @overdue]

    (if timed-out
      (adopt-run! {:session session
                   :id id
                   :proc (:proc (bg-entry session id))
                   :sink sink
                   :script script
                   :cwd cwd
                   :drains (:drains timed-out)
                   :exit-atom exit-atom
                   :exited-at exited-at
                   :stopped? stopped?
                   :index-fn index-fn})
      (do (shell-log/close! sink)
          (reset! exited-at (now-ms))
          (reset! exit-atom (batch-exit results))
          (index-fn {:dir cwd :ended-at @exited-at :exit @exit-atom})
          (drop-bg-entry! session id)))
    (extension/success
      {:result (shell-result "run"
                             (merge {;; ALWAYS a handle, finished or not — that is the whole point:
                                     ;; no rule to remember about when an id is present.
                                     "id" id
                                     "cwd" cwd
                                     ;; Started only when EVERY child was spawned, so a
                                     ;; launch failure anywhere stays visible at the top.
                                     "started" (every? #(get % "started") results)
                                     "exit" (batch-exit results)
                                     "duration_ms"
                                     (reduce + 0 (keep #(get % "duration_ms") results))
                                     "timed_out" (boolean timed-out)
                                     "timeout_secs" (clamp-timeout-secs (get opts "timeout_secs"))
                                     "note" (if timed-out
                                              (str (batch-note (count results))
                                                   " The WAIT expired, not the process: it is still"
                                                   " running as shell '"
                                                   id
                                                   "' — read everything it"
                                                   " printed with await shell_logs(\""
                                                   id
                                                   "\", offset=0), and stop it with"
                                                   " await shell_stop(\""
                                                   id
                                                   "\").")
                                              (batch-note (count results)))}
                                    (batch/result results)))
       :op :shell-run})))

(defn- retired-log-core
  "The `logs` identity for an id whose registry entry is GONE but whose log file is
   not — a run that finished inside its wait keeps no live process, yet its bytes
   stay readable by id for as long as the session does. The sidecar index row is
   where the command lines, the cwd, the lifetime and the exit come from, so \"what
   did that build print\" is answerable a turn later with nothing but the id."
  [env session id]
  (let
    [row
     (->> (shell-log/session-logs (:db-info env) session)
          (filter #(= id (get % "id")))
          first)

     started
     (get row "started_at")

     ended
     (get row "ended_at")]

    (assoc (shell-result
             "logs"
             {"id" id
              batch/commands-key (mapv #(command-result {"command" (str %) "started" true})
                                       (get row "commands"))
              "cwd" (get row "dir")
              "started" true
              "exit" (get row "exit")
              "uptime_ms" (when (and started ended) (max 0 (- (long ended) (long started))))})
      ;; The process is gone by construction: this stage reads a FILE, never a child.
      "status" "exited")))

(defn- shell-logs-impl
  "ONE read of a shell's log, from a byte OFFSET.

   The log is a file, so a read is a WINDOW on it and the caller owns the cursor:
   feed `next_offset` back and the loop walks the whole stream, however long it
   grew. There is no `n`, no eviction and no `dropped` — nothing a shell printed
   is ever gone while the session lives.

   With no `:offset` the window is the TAIL, which is what someone watching a live
   command wants; `{:offset 0}` is the head, and a loop from there is the whole
   log. `is_eof` false means read again NOW rather than sleep.

   The id may name a LIVE shell or a finished RUN: every run claims a handle before
   it waits, so a batch that ended inside its wait still has a log to read, and the
   file is what answers once the registry entry is gone."
  ([env id] (shell-logs-impl env id nil))
  ([env id {:keys [offset limit]}]
   (let
     [session
      (:session-id env)

      id
      (str id)

      entry
      (bg-entry session id)

      ^java.io.File file
      (shell-log/log-file session id)]

     (when-not (or entry (.isFile file))
       (throw (ex-info (str "No shell '" id
                            "' in this session — every run and every background start"
                            " answers with its own id; live ids are listed in resources.")
                       {:type ::unknown-bg-id :id id})))
     (let
       [chunk
        (shell-log/read-chunk id file {:offset offset :limit limit})

        t
        (now-ms)]

       (extension/success
         ;; Sharing `bg-core`'s identity keys with every other stage: `exit` None
         ;; while running. `text` is the window this read returned, already joined
         ;; — a file has no line numbers to carry and no seq pairs to unwrap.
         {:result (assoc (if entry (bg-core "logs" id entry) (retired-log-core env session id))
                    "text" (:text chunk)
                    "offset" (:offset chunk)
                    "next_offset" (:next-offset chunk)
                    "is_eof" (:is-eof chunk)
                    "is_truncated" (:is-truncated chunk))
          :op :shell-logs
          :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))))

(def ^:private terminal-escape-re
  ;; ANSI/VT sequences a PTY-attached tool emits. Stripped both when MATCHING a
  ;; `wait` predicate and when rendering captured output, so the two agree.
  #"(?s)(?:\u001B\].*?(?:\u0007|\u001B\\)|(?:\u001B\[|\u009B)[0-?]*[ -/]*[@-~]|\u001B[ -/]*[@-~])")



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
   response with `await shell_logs(id)`. Returns the total shell result."
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
       (throw (ex-info (str "No background shell '"
                            id
                            "' in this session — start one with"
                            " await shell_background([\"…\"], id=id);"
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
                             :op :shell-type
                             :metadata
                             {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}}))))))

;; -----------------------------------------------------------------------------
;; Internal lifecycle grammar — the model calls `shell_run` / `shell_background` /
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
  "`await shell_stop(id)` — the TERMINAL lifecycle stage. Stopping used to be
   reachable only through `resource_stop`, a sandbox builtin absent from the native
   tool list, so the end of a background shell's life was undiscoverable from the
   schema; it is now a tool of its own. Routes through
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

     ;; Make a native stop linearizable against native starts of the same id.
     ;; The stop callback re-enters this monitor (JVM monitors are reentrant).
     [entry r]
     #_{:clj-kondo/ignore [:locking-suspicious-lock]}
     (locking (bg-lifecycle-lock session id) [(bg-entry session id) (resources/stop! session id)])]

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
                        ;; Tagged with its OWN tool: each of the five verbs is a
                        ;; registered symbol op carrying its own observation/mutation tag.
                        :op :shell-stop
                        :metadata
                        {:id id :started-at-ms t :finished-at-ms (now-ms) :duration-ms 0}})))

(defn shell-dispatch
  "INTERNAL shell lifecycle grammar, kept for the Python-extension entry points
   (`trusted-extension-shell`, `jailed-shell`, `session-jailed-shell`) whose
   caller authors an options map by hand and therefore genuinely needs an `op`
   discriminator. The MODEL never reaches this: it calls `shell_run`,
   `shell_background`, `shell_logs`, `shell_type` or `shell_stop`, each with its
   own satisfiable schema, so one schema with five mutually-exclusive shapes is
   never presented as a contract to disambiguate."
  [env opts]
  (when-not (opts-arg? opts)
    (throw (ex-info "shell takes one options map, e.g. shell({\"commands\": [\"ls\"]})."
                    {:type ::bad-options})))
  (let
    [raw-opts
     (into {}
           (remove (fn [[_ v]]
                     (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v)))))
           opts)

     text
     (opt raw-opts :text)

     _
     (when (some? (opt raw-opts :cmd))
       (throw (ex-info "shell has no `cmd` option — put bash lines in {\"commands\": [...]}."
                       {:type ::legacy-command-carrier})))

     id
     (some-> (opt raw-opts :id)
             str
             str/trim
             not-empty)

     op
     (or (some-> (opt raw-opts :op)
                 str
                 str/trim
                 str/lower-case
                 not-empty)
         (if id "background" "run"))

     ;; Lifecycle stages operate on an existing background shell.  A caller that
     ;; mechanically reuses the start shape may still carry `commands`; it is not
     ;; relevant to these stages and must not make an otherwise valid call fail.
     opts
     (if (#{"logs" "send" "stop"} op) (dissoc raw-opts "commands" :commands) raw-opts)

     commands
     (opt opts :commands)

     checked-commands
     (fn []
       (ordered-lines commands))

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
      ;; A start is the one background stage with no prior handle: derive the id from
      ;; the command rather than fail a well-formed start over a missing name. Every
      ;; other stage (no commands) still names the shell it acts on.
      (do (reject-text)
          (shell-bg-impl env
                         (if (and (nil? id) valid-commands)
                           (auto-bg-id (:session-id env) valid-commands)
                           (need-id))
                         valid-commands
                         opts))

      "logs"
      (do (reject-commands)
          (reject-text)
          (shell-logs-impl env
                           (need-id)
                           {:offset (->pos-long (opt opts :offset) "offset")
                            :limit (->pos-long (opt opts :limit) "limit")}))

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
   SAME `command-result` map (`command`, `stdout`, `stderr`, `exit`, `duration_ms`,
   `timed_out`, `*_omitted_chars`) `shell` puts under `commands`, carrying the
   request's `:dir`/`:timeout-secs` as metadata, so there is one command shape for
   both tools and no envelope to unwrap.

   No shell is involved — each element reaches the process verbatim, so nothing
   needs quoting. The `git` tool is a USER of this: every git command is a
   bounded shell command, so both tools share one runner, one jail and one
   capture policy."
  ([env argv] (run-argv env argv nil))
  ([env argv opts] (shell-run-impl env (vec argv) opts)))

(defn trusted-extension-shell
  "Run a trusted Python extension's ordinary `vis.shell` request without applying
   a process jail. The extension context already has direct, unrestricted
   subprocess access; this is the result-shaped convenience API for the same trust
   boundary. Foreground calls therefore work outside a session too."
  [env opts]
  (shell-dispatch (-> (or env {})
                      (assoc :jail-policy-fn (constantly {:disabled? true}))
                      (assoc-in [:security-policy :sandbox] false))
                  opts))

(defn- latest-jail-policy
  "Strictly build one process policy from the currently merged config. This is
   called at the spawn boundary: invalid current config throws, and no session or
   last-good snapshot is consulted."
  [env]
  (let
    [base-dir
     (or (workspace/workspace-root env) (str (workspace/cwd)))

     security
     (security-policy/snapshot (or (config/load-config-raw) {}) {:base-dir base-dir})

     configured-roots
     (security-policy/read-write-roots security)

     active-workspace
     (or (:workspace env) {:root base-dir})

     latest-env
     (assoc (or env {})
       :security-policy security
       :workspace active-workspace
       :security/filesystem-roots configured-roots)

     jail-config
     (:process-jail security)]

    (if (or (false? (:sandbox security)) (:disabled? jail-config))
      {:disabled? true ::environment latest-env}
      (let
        [entries
         (workspace/env-filesystem-roots latest-env)

         clones
         (into []
               (comp (filter #(and (:clone %) (not= (:clone %) (:trunk %)))) (map :clone))
               entries)

         withheld
         (into #{}
               (comp (filter #(or (:denied? %) (and (:clone %) (not= (:clone %) (:trunk %)))))
                     (map :trunk))
               entries)

         roots-fn
         (fn []
           (vec (distinct (concat (when-let [root (:root active-workspace)]
                                    [(str root)])
                                  clones
                                  (remove #(contains? withheld (workspace/normalize-root %))
                                    configured-roots)))))

         network
         (:network security)

         compiled-network-policy
         (some-> (egress/compile-policy network)
                 (assoc :mitm? (boolean (seq (:rules network)))))

         token
         (str (java.util.UUID/randomUUID))

         cleaned?
         (atom false)

         cleanup
         (fn []
           (when (compare-and-set! cleaned? false true)
             (gateway-sandbox/unregister-session! token)))]

        (gateway-sandbox/register-session! token (constantly compiled-network-policy))
        (try (let
               [proxy-port
                (gateway-sandbox/ensure-proxy!)

                ca-file
                (gateway-sandbox/ensure-ca!)]

               (merge jail-config
                      {:roots-fn roots-fn
                       :net-enabled? true
                       :proxy-port proxy-port
                       :proxy-token token
                       :ca-file ca-file
                       ::environment latest-env
                       ::cleanup cleanup}))
             (catch Throwable t (cleanup) (throw t)))))))

(defn jailed-shell
  "Run `vis.jailed_shell` through a strict policy read from the latest merged
   on-disk configuration at every process spawn. Works with or without a session;
   invalid current config refuses that spawn instead of using a snapshot."
  [env opts]
  (shell-dispatch (assoc (or env {}) :jail-policy-fn #(latest-jail-policy env)) opts))

(defn session-jailed-shell
  "Run `vis.jailed_shell_session` through the invoking session's immutable jail
   snapshot. Requires a live session and never re-reads configuration."
  [env opts]
  (when-not (:session-id env)
    (throw (ex-info "jailed_shell_session is available only while handling a session"
                    {:type ::no-session})))
  (shell-dispatch env opts))


;; =============================================================================
;; Env injection — the before-fn hands the impl its env as first arg
;; =============================================================================

(defn- op-label
  "Human call name from a registered op keyword: :shell-run -> \"shell_run\"."
  [op]
  (str/replace (if (namespace op) (str (namespace op) "_" (name op)) (name op)) "-" "_"))

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

(defn- shell-call-opts
  "ONE options map from a call's arguments. Each shell tool takes named positional
   values (`commands`, `id`, `text`) followed by an optional keyword dict, and an
   all-kwargs call collapses into a single map — normalize all three shapes here so
   no tool has to."
  [ks args]
  (let
    [[pos more]
     (split-with (complement opts-arg?) args)

     named
     (into {}
           (remove (fn [[_ v]]
                     (nil? v)))
           (zipmap ks pos))]

    (reduce (fn [m o]
              (merge m (into {} o)))
            named
            more)))

(defn shell-run
  "`await shell_run([\"git status\"])` — run bash lines in order and return their
   output. Bounded by an internal deadline so a foreground call cannot hang the
   turn; long or interactive work is `shell_background`."
  {:arglists '([commands] [commands opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["commands"] args) "op" "run")))

(defn shell-background
  "`await shell_background([\"npm run dev\"], id=\"dev\")` — spawn a PTY under `id`
   and return immediately. Re-issuing the same script returns the LIVE shell instead
   of spawning a second one. Watch it with `shell_logs`."
  {:arglists '([commands] [commands opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["commands"] args) "op" "background")))

(defn shell-logs
  "`await shell_logs(\"dev\")` — read a background shell's log from a byte offset
   and return NOW. Nothing blocks on your behalf: a wait is a bounded loop you
   write in `python_execution` and break on what you actually read."
  {:arglists '([id] [id opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id"] args) "op" "logs")))

(defn shell-type
  "`await shell_type(\"dev\", \"y\")` — type keystrokes at a background shell's stdin.
   `is_enter` (default true) submits the line, which is what an interactive prompt
   waits for; read the response with `shell_logs`."
  {:arglists '([id text] [id text opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id" "text"] args) "op" "send")))

(defn shell-stop
  "`await shell_stop(\"dev\")` — kill the process tree and drop the retained logs and
   session resource."
  {:arglists '([id])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id"] args) "op" "stop")))

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
   top-level `command`), so a card renders them from the ONE place they exist."
  [r]
  (some->> (get r batch/commands-key)
           seq
           (map #(get % "command"))
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

(def ^:private card-head-chars
  "Head of a stream kept in a CARD. Display-only: capture stays whole."
  4000)

(def ^:private card-tail-chars
  "Tail of a stream kept in a CARD — failures and summaries live at the END."
  12000)

(defn- clip-stream
  "DISPLAY clip for a captured stream: keep the head and the tail, splice a
   visible omitted-count marker into the middle. The RESULT keeps the whole
   capture (up to [[max-sync-head-chars]]+[[max-sync-tail-chars]]) so
   `json.loads(r[\"commands\"][0][\"stdout\"])` works; only the card is clipped."
  [s]
  (let
    [head
     (long card-head-chars)

     tail
     (long card-tail-chars)

     cap
     (+ head tail)

     n
     (long (count (str s)))]

    (if-not (and (string? s) (> n cap))
      s
      (str (subs s 0 head) "\n\n…[" (- n cap) " chars omitted]…\n\n" (subs s (- n tail))))))

(defn- duration-label
  "Human duration for shell card chips/status sections."
  [ms]
  (when (number? ms)
    (cond (< (long ms) 1000) (str (long ms) "ms")
          (< (long ms) 60000) (str/replace (format "%.1fs" (/ (double ms) 1000.0)) "," ".")
          :else (str/replace (format "%.1fm" (/ (double ms) 60000.0)) "," "."))))

(defn- squeeze-blank-lines
  "Display-tidy the lines of a rendered command: trailing whitespace off every
   line, no blank head or tail, and an interior run of blanks collapsed to ONE.
   A blank line the CALLER wrote is authored structure — the paragraph break in a
   multi-line script — so it survives instead of being welded shut."
  [lines]
  (->> lines
       (map str/trimr)
       (partition-by str/blank?)
       (mapcat (fn [g]
                 (if (str/blank? (first g)) [""] g)))
       (drop-while str/blank?)
       reverse
       (drop-while str/blank?)
       reverse))

(defn- skip-inline-space
  "First index at or after `i` that is neither a space nor a tab. An operator
   break ends its own line, so the continuation must not start indented by the
   split — that is the ONLY whitespace this pretty-printer may drop."
  ^long [^String s ^long i ^long n]
  (loop [i i]
    (if (and (< i n)
             (let [c (.charAt s i)]
               (or (= c \space) (= c \tab))))
      (recur (inc i))
      i)))

(defn- format-shell-command
  "Pretty-print a shell command for the COMMAND card so a compound one-liner
   reads as separated statements instead of one crammed blob. Break onto its
   own line at TOP-LEVEL `;`, `&&`, `||` operators, keeping the operator at the
   end of its line. Quote- AND paren-aware: separators inside `'…'` / `\"…\"`
   or nested `$(…)` / `(…)` stay put (so `$(f || g)` and `2>&1 &` are never
   split), and a simple command comes back unchanged.

   The command's OWN line structure is display: a multi-line script keeps its
   indentation, and a blank line between two of its paragraphs survives as one
   blank row (see [[squeeze-blank-lines]])."
  [s]
  (let
    [s
     (str s)

     n
     (long (count s))

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
        (let [out (str/join "\n" (squeeze-blank-lines (str/split-lines (str sb))))]
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
            (and (zero? depth) (= c \&) (= nxt \&))
            (do (.append sb "&&\n") (recur (skip-inline-space s (+ i 2) n) sq dq depth))
            (and (zero? depth) (= c \|) (= nxt \|))
            (do (.append sb "||\n") (recur (skip-inline-space s (+ i 2) n) sq dq depth))
            (and (zero? depth) (= c \;)) (do (.append sb ";\n")
                                             (recur (skip-inline-space s (inc i) n) sq dq depth))
            :else (do (.append sb c) (recur (inc i) sq dq depth))))))))

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

     command
     (or (shell-one-line (get r "command")) "shell")

     duration
     (duration-label (get r "duration_ms"))

     summary
     (str "$ "
          (clip-chip command shell-chip-max)
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
                ;; Truncation is REAL data loss, but `*_omitted_chars` is 0 unless the
                ;; middle-excision actually dropped something — the count IS the flag,
                ;; so name the stream and the exact number of characters lost.
                ["stdout"
                 (let [n (get r "stdout_omitted_chars")]
                   (when (and n (pos? (long n))) (str "truncated · " n " chars omitted")))]
                ["stderr"
                 (let [n (get r "stderr_omitted_chars")]
                   (when (and n (pos? (long n))) (str "truncated · " n " chars omitted")))]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "command")) "bash")
           (shell-section "STATUS" status)
           (shell-section "STDOUT" (clip-stream (get r "stdout")) "bash")
           (shell-section "STDERR" (clip-stream (get r "stderr")))]
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
     (str "▸ `"
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
  "shell op `logs` → compact process/log status plus the window this read returned."
  [r]
  (let
    [text
     (or (get r "text") "")

     status
     (or (get r "status") "?")

     uptime
     (duration-label (get r "uptime_ms"))

     exited?
     (= "exited" status)

     off
     (long (or (get r "offset") 0))

     next-off
     (long (or (get r "next_offset") off))

     ;; The card says WHERE the reader now is, because that is the number the
     ;; next call feeds back. "more" means the file already holds bytes past this
     ;; window — read again rather than sleep.
     summary
     (str (if exited? "■" "◷")
          " `"
          (get r "id")
          "` "
          status
          (when-let [exit (get r "exit")]
            (str " · exit " exit))
          " · "
          (- next-off off)
          " B at "
          off
          (when-not (true? (get r "is_eof")) " · more")
          (when uptime (str " · " uptime)))

     details
     (kv-lines [["id" (get r "id")] ["status" status] ["exit" (get r "exit")] ["offset" off]
                ["next_offset" next-off] ["more" (when-not (true? (get r "is_eof")) "true")]
                ["uptime" uptime] ["pid" (get r "pid")]])

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
                                   ["command" (shell-one-line (result-script r))]
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
  (let
    [;; `cwd` and `timeout_secs` are GROUP scope — one copy at the top level
     ;; instead of the same pair on every entry — so hand each card the request
     ;; context its own `cwd` / `timeout` rows need.
     group
     (select-keys r ["cwd" "timeout_secs"])

     results
     (mapv #(merge group %) (get r batch/commands-key))]

    (if (= 1 (count results))
      (render-shell-run-result (first results))
      (batch/card {:icon "$"
                   :noun "shell"
                   :results results
                   :render-one render-shell-run-result
                   :tally-fn shell-batch-tally}))))


;; =============================================================================
;; Symbols + prompt + extension. ONE builtin symbol — `shell` — bound bare in the
;; flat Python sandbox next to `git` / `cat` / `grep`.
;; =============================================================================

(defn- live-bg-script
  "The bash the LIVE background shell `id` is already running. A `logs`/`send`/
   `stop` call runs no command of its OWN, but the command it acts on is right
   there in the registry — the same lines the finished card renders out of its
   result — so its pending card can show that real command.

   nil when no live shell answers to that id, or when two sessions both do and the
   answer would be a guess."
  [id]
  (when id
    (let [hits (into [] (keep #(get % id)) (vals @bg-procs))]
      (when (= 1 (count hits)) (present-str (:script (first hits)))))))

(defn- render-shell-call
  "PENDING-call display for one shell tool invocation: the SAME op-card the finished
   call wears, assembled by the SAME section builders, out of what is known BEFORE
   the run instead of out of a result. `op` comes from the TOOL, never from the
   arguments — each of the five is its own verb now.

   nil when the arguments name neither a command nor a target — the raw invocation
   stays the honest fallback."
  [op input]
  (let
    [id
     (some-> (opt input :id)
             str
             str/trim
             not-empty)

     commands
     (opt input :commands)

     cmds
     (when (and (#{"run" "background"} op) (some? commands))
       ;; A malformed real batch is the CALL's error to report, never this preview's.
       (try (vec (ordered-lines commands)) (catch Throwable _ nil)))

     keys-lbl
     (some-> (opt input :text)
             keys-label)

     script
     (if (seq cmds) (str/join "\n" cmds) (live-bg-script id))

     summary
     (cond
       ;; A background START is its own lifecycle card even though it carries
       ;; commands — mirror the finished `▸ `id` started · pid N`.
       (= "background" op) (str "▸ " (when id (str "`" id "` ")) "starting")
       (seq cmds) (str "$ "
                       (clip-chip (shell-one-line (first cmds)) shell-chip-max)
                       (when (next cmds) (str " · +" (dec (count cmds)) " more"))
                       " (running)")
       (nil? id) nil
       (= "logs" op) (str "◷ `" id "` reading logs")
       (= "send" op) (str "↵ `" id
                          "` sending" (when keys-lbl
                                        (str " "
                                             (clip-chip (shell-one-line keys-lbl) shell-chip-max))))
       (= "stop" op) (str "✕ background `" id "` stopping")
       :else (str "◷ `" id "` " op))

     body
     (->> [(shell-section "COMMAND"
                          (some-> script
                                  format-shell-command)
                          "bash")
           (shell-section "STATUS"
                          (kv-lines [["id" id] ["cwd" (opt input :cwd)] ["keys" keys-lbl]]))]
          (remove nil?)
          (str/join "\n\n"))]

    (when (or summary (seq body))
      (cond-> {}
        summary
        (assoc :summary summary)

        (seq body)
        (assoc :render body)))))

(defn- shell-start-renderer
  "Pending-card renderer for ONE shell tool: the verb is fixed by the tool, so the
   arguments never have to carry it."
  [op]
  (fn [input]
    (render-shell-call op input)))

(def shell-run-symbol
  (vis/symbol
    #'shell-run
    {:symbol 'shell-run
     :native-tool? true
     :name "shell_run"
     :result
     (str "`{id, cwd, commands, exit, duration_ms, timed_out}`; each entry under `commands` "
          "carries its own `command`, `stdout`, `stderr`, `exit`, `duration_ms`, `timed_out` and "
          "`note` (`*_omitted_chars` says what a huge stream lost). Nonzero exit is data. `id` is "
          "ALWAYS a handle: `timed_out` means the WAIT expired, not the process.")
     :description
     (str
       "Run `bash -lc` lines in order and return their output; drive it from `python_execution`: "
       "batch the `commands`, read `r[\"commands\"][i][\"stdout\"]`, print only what you need. "
       "The result is always a handle: on `timed_out` the command is STILL RUNNING under `id`, so "
       "resume with `shell_logs(id, offset=0)` and `shell_stop(id)` instead of running it again. "
       "Long or interactive work goes to `shell_background`.")
     :render-finish-call-fn render-shell-batch-result
     :render-start-call-fn (shell-start-renderer "run")
     :schema {:type "object"
              :properties {"commands" (batch/commands-property
                                        {:items {:type "string"}
                                         :description "`bash -lc` lines, run strictly in order."})
                           "cwd" {:type "string"
                                  :description "Dir under allowed root; relative uses workspace."}}
              :required ["commands"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation
     :on-error-fn (shell-on-error :shell-run)}))

(def shell-background-symbol
  (vis/symbol
    #'shell-background
    {:symbol 'shell-background
     :native-tool? true
     :name "shell_background"
     :result
     (str "`{id, cwd, commands, pid, status, note}`, plus `already_running` and `uptime_ms` when "
          "an idempotent re-issue found the shell already live.")
     :description
     (str "Spawn a PTY under `id` and return immediately; re-issuing the same script returns the "
          "LIVE shell instead of a second one. Then poll `shell_logs` in `python_execution`.")
     :render-finish-call-fn render-shell-bg-result
     :render-start-call-fn (shell-start-renderer "background")
     :schema {:type "object"
              :properties {"commands" (batch/commands-property
                                        {:items {:type "string"}
                                         :description "`bash -lc` lines, run strictly in order."})
                           "id" {:type "string"
                                 :minLength 1
                                 :description "Handle; derived from the program name when omitted."}
                           "cwd" {:type "string"
                                  :description "Dir under allowed root; relative uses workspace."}}
              :required ["commands"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation
     :on-error-fn (shell-on-error :shell-background)}))

(def shell-logs-symbol
  (vis/symbol
    #'shell-logs
    {:symbol 'shell-logs
     :native-tool? true
     :name "shell_logs"
     :result
     (str "`{id, text, offset, next_offset, is_eof, is_truncated, status, exit}` — always the same "
          "keys. `text` is the window this read returned; feed `next_offset` back to continue, and "
          "`is_eof` false means read again now.")
     :description
     (str "Read a background shell's log from a byte offset and return NOW; nothing blocks on your "
          "behalf — a wait is a bounded loop you write in `python_execution` and break on what you "
          "read. No offset reads the TAIL; `offset=0` starts at the beginning. Live ids: "
          "`session[\"resources\"]`.")
     :render-finish-call-fn render-shell-logs-result
     :render-start-call-fn (shell-start-renderer "logs")
     :schema {:type "object"
              :properties
              {"id" {:type "string" :minLength 1 :description "Background handle."}
               "offset" {:type "integer"
                         :minimum 0
                         :description "Byte to read from; omit for the tail, 0 for the whole log."}
               "limit"
               {:type "integer" :minimum 1 :description "Bytes this read returns; default 16384."}}
              :required ["id"]
              :additionalProperties false}
     :inject-env? true
     :tag :observation
     :on-error-fn (shell-on-error :shell-logs)}))

(def shell-type-symbol
  (vis/symbol
    #'shell-type
    {:symbol 'shell-type
     :native-tool? true
     :name "shell_type"
     :result "`{id, text, is_enter, status}`."
     :description
     "Type keystrokes at a background shell's stdin; `is_enter` (default true) submits the line."
     :render-finish-call-fn render-shell-send-result
     :render-start-call-fn (shell-start-renderer "send")
     :schema {:type "object"
              :properties
              {"id" {:type "string" :minLength 1 :description "Background handle."}
               "text" {:type "string" :description "Keystrokes written verbatim to stdin."}
               "is_enter" {:type "boolean" :description "Append a newline; default true."}}
              :required ["id" "text"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation
     :on-error-fn (shell-on-error :shell-type)}))

(def shell-stop-symbol
  (vis/symbol
    #'shell-stop
    {:symbol 'shell-stop
     :native-tool? true
     :name "shell_stop"
     :result "`{id, status, exit, note}`."
     :description "Kill a background shell's process tree and drop its retained logs and resource."
     :render-finish-call-fn render-shell-stop-result
     :render-start-call-fn (shell-start-renderer "stop")
     :schema {:type "object"
              :properties {"id" {:type "string" :minLength 1 :description "Background handle."}}
              :required ["id"]
              :additionalProperties false}
     :inject-env? true
     :tag :mutation
     :on-error-fn (shell-on-error :shell-stop)}))

(def shell-symbols
  [shell-run-symbol shell-background-symbol shell-logs-symbol shell-type-symbol shell-stop-symbol])

(defn shell-attach-command
  "`vis-agent extension shell attach <id>` — the human-side passthrough: join a live
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
  "CLI surface mounted under `vis-agent extension shell`. Only `attach` for now — the human
   passthrough onto a background PTY the agent spawned."
  [{:cmd/name "shell"
    :cmd/doc "Attach a real terminal to a live background shell (shell op \"background\")."
    :cmd/usage "vis-agent extension shell attach <id>"
    :cmd/subcommands
    [{:cmd/name "attach"
      :cmd/doc
      "Join a background shell's PTY in your terminal; Ctrl-] detaches (child keeps running)."
      :cmd/usage "vis-agent extension shell attach <id> [--socket PATH]"
      :cmd/owns-tty? true
      :cmd/examples ["vis-agent extension shell attach slack-auth"
                     "vis-agent extension shell attach dev-server"]
      :cmd/run-fn #'shell-attach-command}]}])

(vis/register-toggle! {:id "shell"
                       :label "Shell commands"
                       :description
                       (str "Expose the five shell tools: `shell_run` plus the background "
                            "lifecycle (`shell_background` / `shell_logs` / `shell_type` / "
                            "`shell_stop`). When OFF none of them is bound. Contained by the "
                            "OS process jail whenever it is ON.")
                       :default true
                       :owner :vis
                       :persist? true
                       :group :sandbox})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shell"
     :ext/description
     "Five shell tools: `shell_run` for bounded commands, and `shell_background` / `shell_logs` / `shell_type` / `shell_stop` for the background PTY lifecycle; `resource_stop` also stops PTYs. Default-on behind the `shell` toggle and OS process jail."
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
