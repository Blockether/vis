(ns com.blockether.vis.internal.foundation.shell
  "`shell/` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature). Bound only when the user-owned `shell` toggle is ON
   (default ON; flip it OFF in Settings or in `vis.yml` via `toggles: {shell: false}`
   to drop the tools). The OS process jail is the containment layer while active.

   ONE model-facing entry point — the `shell` PYTHON verb, bound BARE in the flat
   sandbox next to `ls` / `grep`, and NO native tool: a process is started from
   Python and nowhere else, because every verb after the spawn is a method on the
   handle the call returns. EVERY run is a background run: the call spawns
   under a real pty and returns the HANDLE now, so there is no `wait` on the
   request and no number that can select a second mode. ONE call runs ONE command:
   an ordered batch was a second budget, a second result shape and a second failure
   mode for what `&&` already says.

   1. `sh = await shell(\"ls\")` — `bash -lc` in the workspace root, spawned under
      a REAL pty, its merged output streamed verbatim to a log FILE and registered
      as a session RESOURCE. `sh.wait(30)` is what fills `exit`/`stdout`. Output is
      bounded at READ time to a head+tail budget per stream, so only the MIDDLE of a
      huge stream is dropped, never its start or end. A non-zero exit is DATA the
      model reads, not an error.

   2. A server, watcher or long build is the SAME call — you simply do not wait for
      it, or you wait for less than it takes. A wait that expires is never a lost
      process: it keeps running under its id and its log keeps filling.

   3. The log OUTLIVES the run. Every shell keeps its log file and its index row by
      id for as long as the session does, so \"what did that build print\" is
      answerable a turn later from the id alone. That retention is the feature, not
      a leak to reap.

   The result IS the HANDLE: every shell answer is a dict-with-methods in the
   sandbox, so the process is driven on the object the call already returned —
   `sh.logs(-50)` reads the last 50 LINES (or a byte OFFSET) and returns NOW,
   `sh.wait(30)` is the bounded poll loop written once in the engine, `sh.type(\"y\")`
   types into the pty and `sh.stop()` kills the tree. There are no id-taking verbs to
   re-type an id into; re-issuing a LIVE id gives the same handle back.

   STATUS is not a stage of its own: EVERY answer of EVERY stage already says what the
   shell is doing — `status`/`exit`, `started_at`/`finished_at`/`uptime_ms`, `log_path`,
   and the live `cpu_ms`/`cpu_percent`/`rss_bytes` of its process tree — so \"is it done
   yet\" is read off the result already in hand and never costs a second call.

   `shell-dispatch` survives as the INTERNAL grammar the Python-extension entry
   points use, since those hand-author an options map and genuinely need an `op`.

   EVERY result of EVERY stage — including an argv run, which uses the same runner
   — is the one [[shell-result-base]] key set: `stage` names the producer and is
   the only thing that varies. A key a stage has nothing to say about is nil /
   false / 0 instead of absent, so model Python indexes any of them without a
   KeyError, and a run answers with its `command` and that command's own bytes at
   the TOP level — there is no entry to unwrap and no second shape to learn.


   The `shell` toggle is registered HERE, extension-owned under the vis
   namespace. It closes the MODEL's door only: an installed extension keeps its
   own trusted process boundary (`vis.shell`, `subprocess`), which the toggle
   does not gate."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.cancellation :as cancellation]
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
            [com.blockether.vis.internal.foundation.pty-bridge :as pty-bridge]
            [com.blockether.vis.internal.strutil :as strutil])
  (:import (java.io File)
           (java.lang ProcessHandle)
           (java.util HashMap)
           (java.util.concurrent TimeUnit)))

;; Limits

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
   at char 4000, so `json.loads(r[\"stdout\"])` died on
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

;; Small helpers

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
     (atom false)

     append!
     ;; The ONE place text enters this capture — a pumped Reader and a `wait`
     ;; accumulating log windows are the same problem, so they share the same
     ;; bounded buffer instead of one of them growing without a cap.
     (fn [^String s]
       (when (pos? (.length s))
         (locking sb
           (swap! total (fn [t]
                          (+ (long t) (.length s))))
           (.append sb s)
           (when (> (.length sb) cap)
             (reset! trunc true)
             ;; keep the first `head-limit` chars + the last `tail-limit`;
             ;; excise the run between them so memory stays at ~cap.
             (.delete sb (int head-limit) (int (- (.length sb) tail-limit)))))))]

    {:append! append!
     :drain! (fn [^java.io.Reader r]
               (let [buf (char-array 8192)]
                 (try (loop []

                        (let [n (.read r buf 0 (alength buf))]
                          (when (pos? n) (append! (String. buf 0 n)) (recur))))
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
  [out]
  (let [n (long (or (:omitted out) 0))]
    (when (pos? n)
      (str "stdout truncated · "
           n
           " chars dropped from the middle"
           " — the text carries an inline marker and no longer parses; narrow the output"
           " (`--jq`, `--limit`, `head -c`) or redirect it to a file and read that."))))

(defn- ->whole-long
  "Coerce a GraalPy-crossed numeric option to a WHOLE long, or throw a typed
   error. nil passes through (caller supplies the default).

   Rounding a fraction is a silent MODE CHANGE, not a convenience: an
   `offset: 0.4` rounds to a different byte. A number the caller cannot have
   meant is refused at the boundary, where the message can still name the option."
  [x what]
  (cond (nil? x) nil
        (number? x) (let [d (double x)]
                      (cond (or (Double/isNaN d) (Double/isInfinite d))
                            (throw (ex-info
                                     (str what " must be a finite number, got " (pr-str x) ".")
                                     {:type ::bad-option :option what :value x}))
                            (not= d (Math/rint d))
                            (throw (ex-info (str what
                                                 " must be a whole number, got "
                                                 (pr-str x)
                                                 " — a fraction rounds into a different value.")
                                            {:type ::bad-option :option what :value x}))
                            :else (long d)))
        :else (throw (ex-info (str what " must be a number, got " (pr-str x) ".")
                              {:type ::bad-option :option what :value x}))))

(defn- ->pos-long
  "[[->whole-long]] that also refuses a NEGATIVE, for an option where a negative
   has no reading: clamping a `limit: -5` into a real read is the same silent
   mode change as rounding a fraction.

   `logs` takes its `offset` through [[->whole-long]] instead, because there a
   negative IS a reading — the last n LINES, as in `cat(path, -50)`."
  [x what]
  (let [n (->whole-long x what)]
    (when (and n (neg? (long n)))
      (throw (ex-info (str what " must not be negative, got " (pr-str x) ".")
                      {:type ::bad-option :option what :value x})))
    n))

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

(defn- last-redraw
  "The one frame of `line` a terminal would still be showing. A bare CR sends the
   cursor home and the text after it REDRAWS the line, so only the final non-empty
   segment was ever on screen; the empty segments a leading or trailing CR leaves
   behind are cursor moves, not blank frames."
  ^String [^String line]
  (or (last (remove empty? (str/split line #"\r" -1))) ""))

(defn- lf
  "Plain text from a PTY stream: CRLF to LF so captured output is byte-identical on
   every OS, and every bare CR resolved to the frame it left standing. A progress
   writer redraws ONE line — git sends 28 `Counting objects: N%` frames separated
   by CR — and a stream that keeps them all is 28 lines of noise that pushes the
   result the caller asked for out of the capped window."
  ^String [^String s]
  (when s
    (let [s (.replace s "\r\n" "\n")]
      (if (neg? (.indexOf s "\r")) s (str/join "\n" (map last-redraw (str/split s #"\n" -1)))))))

(def ^:private terminal-escape-re
  ;; ANSI/VT sequences a PTY-attached tool emits. Stripped both when MATCHING a
  ;; `wait` predicate and on every capture a caller reads, so the two agree.
  ;; The bare-ESC alternative ends at `[0-~]`, not `[@-~]`: the two-byte private
  ;; escapes `ESC =` / `ESC >` (keypad mode, written by `less` and every other
  ;; full-screen tool) end in 0x3D/0x3E, so a `@-~` final left their last byte
  ;; behind as a literal `=` or `>` in otherwise clean output. `ESC 7` / `ESC 8`
  ;; (save/restore cursor) were escaping the same way.
  #"(?s)(?:\u001B\].*?(?:\u0007|\u001B\\)|(?:\u001B\[|\u009B)[0-?]*[ -/]*[@-~]|\u001B[ -/]*[0-~])")

(def ^:private non-printing-control-re #"[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F-\u009F]")

(defn- normalize-terminal-output
  "The ONE reading of a PTY capture, shared by every consumer: ANSI/VT escapes and
   non-printing controls removed, tabs and line feeds kept, and a redrawn line
   resolved to the frame the terminal was left showing. What a human sitting at
   that terminal SAW is what `stdout` carries and what the card prints —
   a colour reset git wrote ONLY because our pty made isatty() true is invisible
   to a human and a literal `[m` to everyone reading the string, and half-cleaning
   it left the two readings disagreeing. The raw stream stays whole on disk at
   `log_path` for anyone who wants the bytes."
  [s]
  (when (some? s)
    (-> (str s)
        (str/replace terminal-escape-re "")
        ;; Controls go before the redraws collapse: an erase or a backspace is not a
        ;; frame, and leaving it in would make an empty segment look like one.
        (str/replace non-printing-control-re "")
        (lf))))

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
   spawn instead of silently returning nil/unwrapped argv. `jail.enabled: false` is
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
  [env out]
  (or (truncation-note out)
      (when (process-jail/keychain-denial? (:text out))
        ;; No policy fn means no jail: an opaque Security failure is then a real
        ;; Keychain miss and blaming confinement would send the caller the wrong way.
        (when-let [policy (try (jail-policy env) (catch Throwable _ nil))]
          (process-jail/keychain-denial-hint policy (:text out))))))

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
       "(`sh.stop()`). Free them and retry."))

(defn- spawn-retrying-fds
  "Run `spawn` (a thunk that starts an OS process) and, ONLY when it failed
   because this process is out of file descriptors, reclaim and try once more.

   A leaked descriptor is held by an unreachable object, so a GC + finalization
   pass genuinely returns it; without this, one leaky sandbox block wedges EVERY
   later `shell` call for the rest of the session. A persistent failure is
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

(def ^:private child-env
  "What EVERY shell child is handed, pty or not, confined or not.

   `GIT_OPTIONAL_LOCKS=0` is this extension's half of the index-lock contract
   `internal.git/git-argv` keeps for Vis' own calls. A plain `git status` or
   `git diff` REFRESHES the index, and refreshing WRITES it through
   `.git/index.lock` — so an agent-run status competed for the one lock the
   repo has against the footer poll, the environment snapshot and the rewind
   baseline, and one that was killed mid-refresh left the lock behind for
   every later git to trip over. `0` drops only the OPTIONAL locks: `add`,
   `commit`, `stash` and every other writer still take the lock they need."
  {"GIT_OPTIONAL_LOCKS" "0"})

(defn- spawn!
  ^Process [cmd ^File dir policy]
  (try
    (let
      [^java.util.List args
       ;; A STRING is ONE `bash -lc` line. A SEQUENTIAL is a literal argv run with no
       ;; shell at all — nothing to quote, nothing to interpret — which is how an argv run
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
        ;; Confined child: REPLACE the inherited env with the allowlisted set plus the
        ;; declared `environment:` values, so the operator's API keys/tokens are never
        ;; handed to sandboxed code while a DECLARED variable still arrives.
        (let [^java.util.Map e (.environment pb)]
          (.clear e)
          (.putAll e ^java.util.Map full))
        (let [pe (process-jail/child-env-additions policy)]
          (when (seq pe) (.putAll (.environment pb) ^java.util.Map pe))))
      ;; AFTER the policy branch on purpose: the confined one REPLACED the map.
      (.putAll (.environment pb) ^java.util.Map child-env)
      ;; ONE stream, exactly like the pty path every model-facing run takes: a
      ;; terminal has no separate error channel, so a result that offered a second
      ;; one could only ever answer nil and be read as "nothing went wrong".
      (.redirectErrorStream pb true)
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

(def ^:private pty-child-env
  "What every PTY child gets ON TOP of the ambient (or allowlisted) environment.

   `TERM` is the whole point of the pty: `isatty()` is true and terminfo names a
   real terminal, so a CLI that refuses a dumb pipe runs.

   The PAGERS are the bill for that honesty, and it is paid here. `git`, `gh`,
   `man`, `psql` and friends check `isatty(stdout)`, conclude a human is
   watching and fork `less` THEMSELVES — the tool spawns it, not bash and not
   this extension. `less` then writes SCREEN CONTROL into the same byte stream
   the log file keeps: `ESC =` / `ESC >` (keypad application/numeric mode) on
   entry and exit, erase-to-end-of-line, and soft wrapping that breaks a long
   line mid-token with `space BS`. A live terminal eats those bytes; a log
   READER sees a stray `=` above the output, a `>` glued to the next command's
   first line and paths split in half — and reasonably reads it as corruption.
   `cat` as the pager makes the captured bytes the bytes the command printed.
   `GIT_PAGER` is set as well as `PAGER` because it also outranks a `core.pager`
   the operator configured; a caller who genuinely wants paging still prefixes
   its own assignment (`PAGER=less …`), which wins over an inherited value.

   [[child-env]] rides along: a pty child is a shell child like any other."
  (assoc child-env
    "TERM" "xterm-256color"
    "PAGER" "cat"
    "GIT_PAGER" "cat"))

(defn- pty-spawn!
  "Spawn `cmd` under a REAL pseudo-terminal (internal.foundation.pty — pure Java
   FFM, no JNA and no extracted native helper): isatty() is TRUE, $TERM is set,
   and stdin is writable (the send op) — so interactive CLIs that refuse a dumb
   pipe (browser-auth prompts, password `read`, REPLs) actually run. Returns the
   pty HANDLE MAP (`:pid :in :send :wait :alive? :destroy`) that the pump /
   kill-tree! / wait path below consume. stdout+stderr share the one PTY stream
   — a real terminal has no separate error channel, which is why a shell result
   has no `stderr` field at all.
   `policy` is the jail policy value applied to this spawn; a fresh-config policy
   carries an idempotent cleanup callback retained for the process lifetime."
  [cmd ^File dir policy]
  (try (assoc (spawn-retrying-fds
                (fn []
                  (pty/spawn! {:command (process-jail/wrap-argv [(bash-command) "--noprofile"
                                                                 "--norc" "-lc" (str cmd)]
                                                                policy)
                               :dir (.getPath ^File dir)
                               :env (doto ^HashMap
                                          (if-let [full (process-jail/jailed-child-env policy)]
                                            ;; Confined child: allowlisted env + declared
                                            ;; `environment:` values (secrets dropped).
                                            (HashMap. ^java.util.Map full)
                                            (doto (HashMap. ^java.util.Map (System/getenv))
                                              (.putAll ^java.util.Map
                                                       (process-jail/child-env-additions policy))))
                                      (.putAll ^java.util.Map pty-child-env))
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
  (try (let
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
       ;; A cancel landing mid-kill still has a session to unwind: keep the
       ;; interrupt, never let the tree teardown swallow it.
       (catch Throwable t (cancellation/preserve-interrupt! t) nil))
  nil)

;; BLOCKING runner — INTERNAL only (`run-blocking`, `run-argv`, the bang path)

(defn- clamp-timeout-secs
  "Effective wait in seconds from the opts value: default 120, floor 1, cap 600.
   `label` is the option the CALLER spelled, so a refusal names the key that was
   actually passed (`seconds` on a handle wait) instead of an internal one."
  (^long [v] (clamp-timeout-secs v "timeout_secs"))
  (^long [v label]
   (-> (long (or (->pos-long v label) default-timeout-secs))
       (max 1)
       long
       (min (long max-timeout-secs)))))

(def ^:private shell-result-base
  "The ONE result shape of the whole shell family — every stage, every tool.

   `shell` itself, each handle op (`sh.logs`, `sh.type`, `sh.stop`, `sh.wait`) and
   an argv run all answer THIS key set, so a caller reads `r[\"exit\"]`, `r[\"stdout\"]`,
   `r[\"command\"]` and `r[\"status\"]` the same way whatever produced the map and no
   key can KeyError. `stage` NAMES the producer (`run` / `background` / `logs` /
   `send` / `stop`) — it is the only thing that varies, and it varies as DATA, not
   as a different shape. A key a stage has nothing to say about stays
    present-but-neutral rather than vanishing.

    But a key DERIVABLE from another key is not carried at all — one fact, one
    field: `pid` already says the child was spawned, `status` \"stopped\" already
    says a stop landed, `is_eof` false already says the read has more to come,
    `keys` already shows what was typed, and `attach` is the whole bridge command
    a caller needs, so its socket path stays the bridge's own business.

   ONE call runs ONE command, so there is no `commands` array and no entry to index
   into, and a lifecycle stage carries the same `command` as the shell it acts on.

   NOT `\"op\"`: the extension boundary stamps `\"op\"` on EVERY tool result with the
   tool's own origin — stage detail must use a different key, exactly as
   `stamp-public-result-op` requires."
  {"stage" nil
   "id" nil
   "cwd" nil
   ;; The ONE command THIS result is about: the bash line the call ran, or the
   ;; line the shell it acts on is running. nil only when the stage genuinely has
   ;; no command left (a stopped shell whose registry entry is already gone).
   "command" nil
   "status" nil
   "pid" nil
   "exit" nil
   "duration_ms" nil
   "uptime_ms" nil
   ;; WHEN, in epoch milliseconds, so a caller can say how long ago a shell started
   ;; and whether it has finished at all. `finished_at` nil IS "still running" —
   ;; the same fact `exit` nil carries, spelled on the clock.
   "started_at" nil
   "finished_at" nil
   ;; WHERE the bytes are ON THIS MACHINE. The log is an ordinary file, so `cat`,
   ;; `grep` and a human's editor reach it without going through a handle at all.
   "log_path" nil
   ;; LIVE cost of the process TREE, sampled at the moment this result was built:
   ;; nil once the child is gone, because usage is a measurement and there is
   ;; nothing left to measure.
   "cpu_ms" nil
   "cpu_percent" nil
   "rss_bytes" nil
   "timed_out" false
   "timeout_secs" nil
   ;; OUTPUT, under ONE name everywhere: what a run captured, and what a log read
   ;; returned. There is no second spelling of \"the bytes\" to learn — and no
   ;; `stderr`: every command runs under a real pty, where stdout and stderr ARE
   ;; ONE stream, so a second field could only ever answer nil and be misread as
   ;; \"nothing went wrong\".
   "stdout" nil
   ;; A truncated stream has an inline \"…[N chars omitted]…\" marker spliced into
   ;; its MIDDLE, so it no longer parses — the count says exactly how much is gone,
   ;; and 0 means nothing was.
   "stdout_omitted_chars" 0
   ;; The log CURSOR: a read is a window on a file and the caller owns the cursor.
   "offset" 0
   "next_offset" 0
   "is_eof" true
   ;; Attach bridge for an interactive shell, and what `sh.type` wrote.
   "attach" nil
   "already_running" false
   "keys" nil
   "note" nil})

(defn- shell-result
  "One stage's fields merged onto [[shell-result-base]], with `stage` stamped last
   and anything outside the base dropped — so every result of every shell stage
   has exactly the same keys."
  [stage m]
  (assoc (merge shell-result-base (select-keys m (keys shell-result-base))) "stage" stage))

(defn- shell-quote
  "One argv token as a literal `bash -lc` word: plain words stay bare, anything
   else is single-quoted (with embedded quotes escaped), so a coerced argv keeps
   exactly the arguments it named."
  [^String token]
  (if (re-matches #"[A-Za-z0-9_@%+=:,./-]+" token)
    token
    (str "'" (str/replace token "'" "'\\''") "'")))

(defn- shell-run-impl
  "Run ONE command and answer the `run`-stage [[shell-result-base]] map: the
   command, its bytes and its outcome. `shell-run-call` merges the handle's
   identity onto it, so a command's line and output have exactly one home.

   `cmd` is either one bash line (a string) or a literal argv (a sequential,
   used by an argv run). The echoed `cmd` is always the display string.

   `handle` is what makes the run a HANDLE: `:sink` is the log file every captured
   byte is teed into, and `:on-spawn` is handed the live process the moment it
   exists, so the registry knows what is running before the wait even begins. A
   handled run that outstays its wait is NOT killed — the wait expired, the process
   did not, and the caller comes back to it by id — while a handle-less run
   still dies on its deadline, because nothing could ever read it again."
  ([env cmd] (shell-run-impl env cmd nil nil))
  ([env cmd opts] (shell-run-impl env cmd opts nil))
  ([env cmd opts {:keys [sink on-spawn]}]
   (let
     [argv
      (when (sequential? cmd) (mapv str cmd))

      ;; An argv is echoed as the bash LINE that would run it — quoted token by
      ;; token, so the `command` a caller reads back is copy-pasteable and can be
      ;; split into exactly the tokens it named.
      cmd
      (if argv (str/join " " (map shell-quote argv)) (str cmd))]

     (when (str/blank? cmd)
       (throw (ex-info (str "shell needs a non-blank command — pass it as `command`,"
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
        (spawn! (or argv cmd) dir policy)

        _
        (when on-spawn (on-spawn p))

        ;; ONE reader future on the ONE merged stream — avoids the classic
        ;; full-pipe deadlock on chatty commands. `capped-capture` bounds memory
        ;; to the head+tail budget at READ time (dropping only the MIDDLE of a
        ;; huge stream, not its start), so a megabyte-then-killed command can't
        ;; balloon the heap yet the opening context survives — and it can be
        ;; snapshotted while the process is still printing.
        out-cap
        (capped-capture max-sync-head-chars max-sync-tail-chars)

        ;; Every captured byte is written through to the log file, so the handle's
        ;; log holds what the call returned AND everything printed after it.
        reader-of
        (fn [^java.io.InputStream s]
          (io/reader (if sink (shell-log/tee s sink) s)))

        out-f
        (future ((:drain! out-cap) (reader-of (.getInputStream p))))

        finished?
        (try (.waitFor p timeout-secs TimeUnit/SECONDS)
             (catch InterruptedException ie
               ;; Turn cancellation: kill the spawned tree before
               ;; the interrupt propagates to the loop.
               (kill-tree! p)
               (throw ie)))]

       (when (and (not finished?) (nil? sink))
         (kill-tree! p)
         ;; Closing the stream unblocks the reader future on a wedged child
         ;; so its thread doesn't linger past our 5s deref ceiling.
         (try (.close (.getInputStream p)) (catch Throwable _ nil)))
       ;; A handled run that timed out is STILL PRINTING: waiting on its drains
       ;; would be waiting on the very command whose wait already expired.
       (when (or finished? (nil? sink)) (deref out-f 5000 nil))
       (let
         [out
          ((:snapshot out-cap))

          exit
          (when finished? (.exitValue p))

          t1
          (now-ms)]

         (with-meta (shell-result "run"
                                  ;; TOTAL shape ([[shell-result-base]]). The old "lean" map dropped a
                                  ;; key whenever it carried no signal, so ordinary model Python
                                  ;; (`c[\"stdout\"]`, `c[\"timed_out\"]`) died with a bare `KeyError` — read as
                                  ;; "the tool broke", retried with cosmetic variations, and spun.
                                  {"command" cmd
                                   ;; The OS pid of the spawned child — `(:pid p)` here read a
                                   ;; keyword off a `Process` and answered nil on every run, so the
                                   ;; one stage that spawns was the one stage with no pid.
                                   "pid" (.pid p)
                                   ;; The SAME vocabulary every other stage answers with: a run that
                                   ;; finished is "exited", one whose wait expired is still "running"
                                   ;; — never nil, or "did it work" has no answer on the one stage
                                   ;; that actually knows.
                                   "status" (if finished? "exited" "running")
                                   ;; What the terminal SHOWED, not the control stream that
                                   ;; painted it: `log_path` still holds every byte.
                                   "stdout" (normalize-terminal-output (:text out))
                                   "exit" exit
                                   "duration_ms" (- t1 t0)
                                   "started_at" t0
                                   "finished_at" (when finished? t1)
                                   "timed_out" (not finished?)
                                   ;; 0 exactly when nothing was dropped, so no truncation flag is owed
                                   ;; beside it.
                                   "stdout_omitted_chars" (long (or (:omitted out) 0))
                                   ;; A dropped middle makes the stream unparseable: name it here rather
                                   ;; than let a caller's parser fail with an opaque message.
                                   "note" (command-note env out)})
           ;; Request scope, IDENTICAL for every entry of a batch: carried as metadata
           ;; so the group summarises one `cwd`/`timeout_secs` instead of every entry
           ;; repeating them, and nothing extra crosses to Python. A relative dir is
           ;; `/`-separated on every OS. A run left alive past its wait carries the
           ;; process and its still-running drains, which is what adoption needs.
           {:dir (paths/unixify (.getPath dir))
            :timeout-secs timeout-secs
            :process (when-not finished? p)
            :drains [out-f]}))))))

(defn- command-line
  "ONE bash line from the caller's `command`. A string IS the line. An array of tokens —
   the argv spelling a caller may take, and the shape a caller reaches for out of habit
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
                 (str "shell `command` is a bash line, not an options map: `op`, `id`, `n`, `until`"
                      " and `text` are TOP-LEVEL arguments — {\"op\": \"logs\", \"id\": \"build\","
                      " \"n\": 30} — and `command` carries only the line to run.")
                 {:type ::bad-commands}))
        (or (sequential? command) (instance? java.util.List command))
        (str/join " " (map (comp shell-quote str) command))
        :else (throw (ex-info "shell `command` must be a string — one bash -lc command line."
                              {:type ::bad-commands}))))

(defn- one-command
  "The caller's `command` as ONE bash line, via [[command-line]]. A blank line is
   refused — there is nothing to run. ONE call runs ONE command: an ordered batch
   was a second budget, a second result shape and a second failure mode for a
   thing `&&` already says, so it is gone."
  [command]
  (let [line (command-line command)]
    (when (str/blank? line)
      (throw (ex-info "shell needs a non-blank command." {:type ::blank-command})))
    line))


;; BACKGROUND — Python sandbox: `await shell({"command": "npm run dev", "id": "dev"})`

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

(defn- prune-bg-session
  "Drop a session's map once its last shell is gone, so an idle session leaves no
   empty shell registry behind."
  [m sk]
  (if (empty? (get m sk)) (dissoc m sk) m))

(defn- live-entry?
  "True while this registry `entry` holds a RUNNING process. An entry reserved
   before its first process exists (a start claims its handle at the top of the
   batch) is not live yet."
  [entry]
  (boolean (when-let [alive? (:alive? (:proc entry))]
             (alive?))))

(defn- env-origin
  "The TRUST ORIGIN of the caller reaching the shell family.

   A handle crosses a trust boundary the moment it outlives the spawn: the jail is
   consulted only when a process is created, while `logs`/`type`/`stop` and a
   re-issue reach an existing process by id alone. Without an origin a JAILED
   extension in the same session could read a trusted shell's output, type at its
   PTY or kill it simply by naming the id. Every entry is stamped with the origin
   that created it and every later op must present the same one."
  [env]
  (str (or (:shell-origin env) "tool")))

(defn- authorize-origin!
  "Refuse to act on a handle created by a DIFFERENT trust origin. An unknown id is
   not this function's business — the op's own \"no such shell\" error says that
   better."
  [env session id]
  (when-let [e (bg-entry session (str id))]
    (let
      [want (env-origin env)
       have (str (or (:origin e) want))]

      (when-not (= want have)
        (throw (ex-info (str "Shell '" id
                             "' was started by a different trust origin (" have
                             ", not " want
                             ") and cannot be read, typed at, stopped or re-issued from"
                             " here. Start your own shell under your own id.")
                        {:type ::foreign-shell :id id :origin have :caller want}))))))

(defn- live-bg-entry
  "The registry entry for `id` when its process is STILL RUNNING, else nil. One
   place decides \"is this handle alive\", so a start, a re-issue and a stop all
   agree about it."
  [session id]
  (when-let [e (bg-entry session id)]
    (when (live-entry? e) e)))

(defn- canonical-dir
  [d]
  (when-let
    [d (some-> d
               str
               not-empty)]
    (try (.getCanonicalPath (io/file d)) (catch Throwable _ d))))

(defn- reissue-live-entry
  "The live entry `id` may be RE-ATTACHED to, or nil when the id is free.

   A re-issue is how a caller picks a shell it already started back up, so it is a
   success — but ONLY when it names the same work. Identity is (id, command, cwd):
   handing a live id a DIFFERENT command, or the same command with a different
   `cwd`, used to answer success for a process that never ran what was asked, in a
   directory nobody requested. That is a silent no-op, so it is refused."
  [env session id command cwd]
  (when-let [e (live-bg-entry session id)]
    (authorize-origin! env session id)
    (let
      [want-cmd (some-> command
                        str
                        not-empty)
       want-dir (canonical-dir cwd)
       have-dir (canonical-dir (:dir e))
       mismatch (cond (and want-cmd (not= want-cmd (str (:command e))))
                      (str "it is running a different command (" (one-line (:command e) 60) ")")
                      (and want-dir have-dir (not= want-dir have-dir))
                      (str "it is running in a different directory (" have-dir ")"))]

      (when mismatch
        (throw (ex-info (str "Shell '" id
                             "' is already running and " mismatch
                             " — nothing was started. Stop it first with sh.stop(),"
                             " or start this command under a different id.")
                        {:type ::id-in-use :id id})))
      e)))

(defn- bg-id-taken?
  "True while a registry `entry` SPEAKS FOR its id: a running process, or a start
   that has claimed the id and not spawned yet.

   Liveness alone cannot answer this. A start reserves its entry, opens the log
   file and only then spawns, so in between the entry carries `:proc nil` and a
   live check reads the id as FREE — the window two concurrent auto-id starts of
   the same program both derive their id in. Only a KNOWN exit frees the id again,
   so a finished shell's short name stays reusable while its log keeps its bytes."
  [entry]
  (boolean (when entry
             (if (:proc entry)
               (live-entry? entry)
               (nil? (some-> (:exit entry)
                             deref))))))

(defn- drop-bg-entry!
  [session id]
  (let
    [sk
     (str session)

     id
     (str id)]

    (swap! bg-procs (fn [m]
                      (prune-bg-session (update m sk dissoc id) sk)))
    nil))

(defn- release-bg-claim!
  "Give a CLAIMED id back when its start never reached a process.

   Only an unspawned claim is dropped: registering a process replaces the entry,
   which then carries no `:claim`, so a failing start can neither erase the shell
   that took the id over nor push every later auto id one suffix along for the
   rest of the session."
  [session id]
  (let
    [sk
     (str session)

     id
     (str id)]

    (swap! bg-procs (fn [m]
                      (if (:claim (get-in m [sk id]))
                        (prune-bg-session (update m sk dissoc id) sk)
                        m)))
    nil))

(defn- releasing-claim
  "Run `f` on the id a start just claimed, releasing the claim if `f` throws
   before any process was registered under it."
  [session id f]
  (try (f id) (catch Throwable t (release-bg-claim! session id) (throw t))))

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
  "Derive AND CLAIM the handle for a background START that carried none.

   Starting a long process is the ONE background call that does not act on an
   existing handle, so rejecting `{op: \"background\", command: \"…\"}` for a missing
   `id` failed a well-formed call over a name the caller had to invent — the most
   frequent shell dead end there is.

   Re-issuing the SAME script while it runs returns that shell's own id, so the
   duplicate start resolves to `already_running` instead of a second dev server;
   otherwise the program name is suffixed until no shell HOLDS it, so an auto id
   never hijacks an unrelated process.

   The id is committed by the same `bg-procs` update that finds it. Searching
   through a plain read and registering afterwards is a check-then-act: two
   concurrent starts of one program both saw `echo` free, both took it, and then
   shared a single registry entry, log file and attach socket — one handle read
   the other child's output, which is how a `gather` of shells over the same
   program crossed its results. A loser of the CAS searches again against a map
   that already holds the winner's claim, and the committed id is read back by the
   claim token because an earlier attempt may have picked a different candidate.
   `release-bg-claim!` hands the id back if the start never spawns."
  [env session command]
  (let
    [origin
     (env-origin env)

     wanted
     (str command)

     sk
     (str session)

     ;; Only a shell THIS origin started can be resolved to; a live shell of
     ;; another origin is skipped by the suffix loop like any other taken id.
     running-same
     (->> (get @bg-procs sk)
          (filter (fn [[_ entry]]
                    (and (= wanted (:command entry))
                         (= origin (str (or (:origin entry) origin)))
                         (live-entry? entry))))
          ffirst)

     base
     (command->bg-id-slug wanted)

     token
     (str (java.util.UUID/randomUUID))]

    (or running-same
        (let
          [[_ committed] (swap-vals!
                           bg-procs
                           (fn [m]
                             (let
                               [ids (get m sk)
                                id (loop [n 1]
                                     (let [candidate (if (= 1 n) base (str base "-" n))]
                                       (cond (not (bg-id-taken? (get ids candidate))) candidate
                                             (< n 100) (recur (inc n))
                                             :else (str base "-" (System/nanoTime)))))]

                               (assoc-in m
                                 [sk id]
                                 {:claim token :command wanted :script wanted :origin origin}))))]
          (some (fn [[id entry]]
                  (when (= token (:claim entry)) id))
                (get committed sk))))))


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

(defn- tree-handles
  "This process and every descendant of it that is still alive. A shell's cost is
   the cost of the TREE it started — the `bash -lc` line is nearly free, the
   compiler it launched is not — so nothing here reports the wrapper alone."
  [pid]
  (when pid
    (when-let [^ProcessHandle h (.orElse (ProcessHandle/of (long pid)) nil)]
      (into [h] (.toList (.descendants h))))))

(defn- tree-cpu-ms
  "Total CPU time the tree has burned, in milliseconds, or nil when the platform
   will not say. This is CPU CONSUMED, not wall time: a shell asleep for an hour
   reports single-digit milliseconds, which is exactly how \"is it working or is it
   stuck\" is answered."
  [handles]
  (let
    [ms (keep (fn [^ProcessHandle h]
                (some-> ^java.time.Duration (.orElse (.totalCpuDuration (.info h)) nil)
                        (.toMillis)))
              handles)]
    (when (seq ms) (reduce + ms))))

(defn- cpu-time->ms
  "One `ps` CPU-time cell as milliseconds: `[[DD-]HH:]MM:SS[.ss]`. `ps` is the only
   portable source of a CHILD's CPU time — the JDK's `totalCpuDuration` is empty
   for a process this JVM did not measure — so the parse lives here rather than
   the number being dropped."
  [^String cell]
  (try (let
         [[days rest]
          (if (str/includes? cell "-") (str/split cell #"-" 2) [nil cell])

          parts
          (mapv #(Double/parseDouble %) (str/split rest #":"))

          secs
          (reduce (fn [acc p]
                    (+ (* 60.0 (double acc)) (double p)))
                  0.0
                  parts)]

         (long (* 1000.0 (+ (double secs) (* 86400.0 (double (if days (Long/parseLong days) 0)))))))
       (catch Exception _ nil)))

(defn- tree-ps-usage
  "Resident memory, CPU share and CPU time of the tree, read from `ps`. The JDK
   exposes neither RSS nor a child's CPU time, and RSS is the number a human means
   by \"how much RAM is this using\". Best effort by construction: a pid that exited
   between the listing and the read simply contributes nothing, and a platform
   without `ps` answers nil rather than failing the stage that asked."
  [pids]
  (when (seq pids)
    (try (let
           [p
            (.start (doto (ProcessBuilder.
                            ^java.util.List
                            (vector "ps" "-o" "rss=,pcpu=,time=" "-p" (str/join "," pids)))
                      (.redirectErrorStream true)))

            out
            (with-open [r (io/reader (.getInputStream p))]
              (slurp r))

            _
            (.waitFor p 2 TimeUnit/SECONDS)

            rows
            (keep (fn [line]
                    (let [cells (str/split (str/trim line) #"\s+")]
                      (when (= 3 (count cells))
                        (try [(Long/parseLong (first cells)) (Double/parseDouble (second cells))
                              (cpu-time->ms (nth cells 2))]
                             (catch Exception _ nil)))))
                  (str/split-lines out))]

           (when (seq rows)
             {"rss_bytes" (* 1024 (long (reduce + (map first rows))))
              ;; One decimal: `ps` reports hundredths of a percent that mean nothing and
              ;; make two consecutive samples look different when they are not.
              "cpu_percent" (/ (Math/round (* 10.0 (double (reduce + (map second rows))))) 10.0)
              "cpu_ms" (reduce + (keep #(nth % 2) rows))}))
         ;; CANCELLATION IS NOT A MEASUREMENT FAILURE. `.waitFor` throws
         ;; InterruptedException and CLEARS the flag, so folding it into the
         ;; best-effort `catch Throwable` below silently ate the interrupt: a
         ;; cancelled turn whose wait happened to be sampling here kept polling
         ;; until its own deadline instead of unwinding. Restore the flag and
         ;; answer nil — the sample is worthless, the cancellation is not.
         (catch InterruptedException _ (.interrupt (Thread/currentThread)) nil)
         (catch Throwable _ nil))))

(defn- process-usage
  "`cpu_ms` / `cpu_percent` / `rss_bytes` for a LIVE process tree, sampled now. An
   exited shell answers nil for all three — usage is a measurement, and a result
   that kept reporting the last sample would be claiming a process that no longer
   exists is still costing something."
  [pid]
  (when-let [handles (seq (tree-handles pid))]
    (merge {"cpu_ms" (tree-cpu-ms handles)}
           (into {}
                 (remove (comp nil? val))
                 (tree-ps-usage (mapv (fn [^ProcessHandle h]
                                        (.pid h))
                                      handles))))))

(defn- bg-core
  "Identity keys shared by EVERY background stage of `shell`, merged onto the
   TOTAL base. `op` names the stage that produced the result, so the card renderer
   — and model Python — reads ONE declared field instead of sniffing which keys
    happen to exist. `exit` nil while running, `attach` nil when no attach
   bridge was opened, `command`/`cwd`/`pid` nil only once the entry itself is
   gone.

   The shell's own command line rides the SAME `command` key a run answers with,
   so a lifecycle stage says what the shell it acts on is running and the result
   never echoes it under a second name."
  [op id entry]
  (let
    [exit
     (some-> (:exit entry)
             deref)

     bridge
     (:bridge entry)]

    (shell-result
      op
      (merge
        ;; Live cost of the tree, sampled HERE so every stage reports the same
        ;; three numbers the same way. Skipped once the child is gone: there is
        ;; nothing to sample, and a stale sample would be a lie.
        (when (nil? exit) (process-usage (:pid (:proc entry))))
        {"id" id
         "command" (:command entry)
         "cwd" (:dir entry)
         "pid" (:pid (:proc entry))
         "status" (if (some? exit) "exited" "running")
         "exit" exit
         ;; A LIVE shell's log is never complete: `is_eof` says "there is
         ;; nothing more to read", and while the child runs there always may
         ;; be. A read stage overrides this with what its own chunk saw.
         "is_eof" (some? exit)
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
         ;; The clock, in epoch ms: when it started, and when it ended — nil
         ;; while it has not.
         "started_at" (:started-at entry)
         "finished_at" (some-> (:exited-at entry)
                               deref)
         ;; The log FILE, by absolute path: readable with `cat`/`grep` by a
         ;; human, with no handle and no session in hand.
         "log_path" (:log-path entry)
         "attach" (when bridge (str "vis-agent extension shell attach " id))}))))

(defn- shell-bg-spawn!
  "Spawn a NEW background PTY under `id`. Callers guarantee no LIVE entry holds
   the id (`shell-bg-impl` owns that check); an exited-but-unread entry under
   the same id is replaced, discarding its retained logs by intent."
  [env id command opts]
  (let
    [session
     (:session-id env)

     id
     (str id)

     ;; ONE command, so the script the pty is fed and the line the entry keeps are
     ;; the same string — there is no group to join.
     script
     (str command)]

    (when (str/blank? id)
      (throw (ex-info "The shell background op needs a non-blank resource id ({\"id\": …})."
                      {:type ::blank-id})))
    (when (str/blank? script)
      (throw (ex-info "The shell background op needs its `command` as the first argument."
                      {:type ::blank-command})))
    ;; A CLAIM is this start's own reservation of the id and has no process to
    ;; retire; only a previous generation's entry does.
    (when-let
      [stale (let [e (bg-entry session id)]
               (when (:proc e) e))]
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
                           (merge {:command script
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
         :command script
         :script script
         :dir (.getPath dir)
         :log-path (:path sink)
         :origin (env-origin env)
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
         :op :shell
         :metadata
         {:command script :pid (:pid p) :started-at-ms t0 :finished-at-ms t0 :duration-ms 0}}))))

(defn- shell-bg-impl
  "`await shell(\"npm run dev\", {\"id\": id})` — IDEMPOTENT on a live id.

   Re-using an id whose process is still running used to THROW. That reads as a
   plain tool failure: a model that already started the shell (or that lost the
   result) learns nothing actionable, invents a new id, and spins — the exact
   runaway seen on `companion-dev`. Instead, return the RUNNING shell flagged
   `already_running` with its pid/uptime and the `logs` handle, so \"start it\" is
   answered by \"it IS started, here is how to watch it\". No second process is
   spawned; a genuinely fresh one needs a `stop` op first."
  [env id command opts]
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
      (let [live (reissue-live-entry env session id command (get opts "cwd"))]
        (if live
          (extension/success
            {:result (assoc (bg-core "background" id live)
                       "already_running" true
                       "note" (str "Background shell '" id
                                   "' was ALREADY running — nothing was restarted. Read its output "
                                   "with sh.logs(). To start a fresh process, first sh.stop()."))
             :op :shell
             :metadata {:command (:script live)
                        :pid (:pid (:proc live))
                        :started-at-ms (:started-at live)
                        :finished-at-ms (now-ms)
                        :duration-ms 0}})
          (if-let
            [command (some-> command
                             str
                             not-empty)]
            (shell-bg-spawn! env id command opts)
            (throw (ex-info (str "No background shell '"
                                 id
                                 "' is running, so it must be STARTED:"
                                 " pass {\"command\": \"…\", \"op\": \"background\", \"id\": \""
                                 id
                                 "\"} as one shell map.")
                            {:type ::missing-command :op "background" :id id}))))))))

;; RUN as a handle — a timeout is a WAIT that expired, not a lost process

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
              (try (deref f) (catch Throwable t (cancellation/preserve-interrupt! t) nil)))
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

(defn- live-run-result
  "A named run whose shell is ALREADY live: the same run shape, answering with the
   process that exists instead of spawning a second copy of it. Re-issuing an id is
   how a caller re-attaches to work it already started, so this is a success and
   never a refusal — and `timed_out` is FALSE, because no wait was made and
   `timed_out` only ever means one that was made expired."
  [id entry]
  (let [t (now-ms)]
    (extension/success {:result (assoc (bg-core "run" id entry)
                                  "duration_ms" 0
                                  ;; NOTHING was waited for, so nothing expired. `timed_out` means one
                                  ;; thing only — a wait ran out — and re-attaching to a live shell
                                  ;; never made a wait.
                                  "timed_out" false
                                  "already_running" true
                                  "note" (str
                                           "Shell '" id
                                           "' was ALREADY running — nothing was restarted. Read it"
                                           " with sh.logs(offset=0) and stop it with sh.stop()."))
                        :op :shell
                        :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))

(defn- run-of-background
  "A background start answered in the RUN shape. Waiting is the ONLY difference
   between a run and a background shell, so the two must not answer with two
   different maps: `id`, `command` and `status` are read the same way on every
   stage. `timed_out` is FALSE — no wait was made, so none expired — and `exit` is
   nil until `sh.wait(secs)` fills it."
  [r]
  (let [m (:result r)]
    (assoc r
      :result (assoc m
                ;; The background start already answered the TOTAL shape through
                ;; `bg-core` — `pid`, `status`, `uptime_ms`, `already_running`. Rebuilding
                ;; the map from scratch dropped exactly those, so a spawn could not say
                ;; what it had started or that it was running. Only the stage-specific
                ;; keys are overridden here.
                "stage" "run"
                "duration_ms" 0
                ;; Nothing WAITED, so nothing timed out: `timed_out` only ever
                ;; means "the wait expired", and a run has no wait.
                "timed_out" false
                ;; `bg-core` writes an explicit nil note, so this is `or`, never
                ;; `get`'s default: a present-but-nil key would swallow the spawn note.
                "note" (or (get m "note")
                           (str "Spawned: the shell runs under id '"
                                (get m "id")
                                "' — read it with sh.logs(offset=0), type at it with"
                                " sh.type(text), wait for it with sh.wait(secs), and"
                                " stop it with sh.stop().")))
      :op :shell)))

(defn run-blocking
  "INTERNAL blocking runner — NOT the tool. Run ONE bounded foreground command and answer the tool's own total result: the
   command's own `run`-stage map with the handle's identity merged on, so `r[\"exit\"]`,
   `r[\"stdout\"]` and `r[\"command\"]` are read at the top level and there is no
   entry to index into. One call is one command — an ordered batch is what `&&`
   and a second call are for — so there is no shared budget to divide and no
   \"never started\" entry to explain.

   EVERY run IS a handle. It claims its `id` and its log file BEFORE it waits, so
   the result carries that id whether the command finished or not, and a wait that
   expires is no longer a lost process: it keeps running under its id,
   `sh.logs(offset=0)` reads everything the timed-out call never saw, and
   `sh.stop()` ends it. A command that finished inside its wait drops the
   registry entry — there is no live process to account for — while its log file
   stays readable by id for as long as the session does."
  [env command opts]
  (let
    [session
     (:session-id env)

     command
     (one-command command)

     ;; The handle is named after the program, exactly as a `wait=0` start is,
     ;; and never hijacks an id a live shell already holds.
     id
     (or (some-> (or (get opts "id") (get opts :id))
                 str
                 not-empty)
         (auto-bg-id env session command))

     ;; The log is a FILE, opened BEFORE the spawn so not one byte of a fast
     ;; command's output can be printed before there is somewhere to keep it.
     sink
     (shell-log/open! session id)

     t0
     (now-ms)

     ;; The wait is how long the CALLER waits, and the only knob there is.
     wait-secs
     (clamp-timeout-secs (get opts "timeout_secs"))

     exit-atom
     (atom nil)

     exited-at
     (atom nil)

     stopped?
     (atom false)

     index-fn
     (fn [m]
       (shell-log/index! (:db-info env)
                         session
                         id
                         (merge {:command command
                                 :script command
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
        :command command
        :script command
        :dir nil
        :origin (env-origin env)
        :started-at t0})

     r
     ;; A start that dies before it spawns must not keep the id. The entry above is
     ;; what holds the handle against a concurrent auto id, so leaving it behind
     ;; would push every later run of this program one suffix along for good.
     (try (shell-run-impl env
                          command
                          (assoc opts "timeout_secs" wait-secs)
                          {:sink sink :on-spawn on-spawn})
          (catch Throwable t (drop-bg-entry! session id) (throw t)))

     cwd
     (:dir (meta r))

     timed-out
     (when (get r "timed_out") (meta r))]

    (if timed-out
      (adopt-run! {:session session
                   :id id
                   :proc (:proc (bg-entry session id))
                   :sink sink
                   :script command
                   :cwd cwd
                   :drains (:drains timed-out)
                   :exit-atom exit-atom
                   :exited-at exited-at
                   :stopped? stopped?
                   :index-fn index-fn})
      (do (shell-log/close! sink)
          (reset! exited-at (now-ms))
          (reset! exit-atom (get r "exit"))
          (index-fn {:dir cwd :ended-at @exited-at :exit @exit-atom})
          (drop-bg-entry! session id)))
    (extension/success
      {:result (shell-result
                 "run"
                 (merge r
                        {;; ALWAYS a handle, finished or not — that is the whole point:
                         ;; no rule to remember about when an id is present.
                         "id" id
                         "cwd" cwd
                         "timeout_secs" wait-secs
                         ;; The STATUS block every other stage answers with, stamped on the
                         ;; run itself: WHERE the bytes are and HOW LONG the command lived.
                         ;; A run that reported neither made "what is this shell doing" a
                         ;; second call for a question this result already knew.
                         "log_path" (:path sink)
                         "uptime_ms" (max 0 (- (long (or @exited-at (now-ms))) (long t0)))
                         "note" (if timed-out
                                  (str "The WAIT expired, not the process: it is still running as"
                                       " shell '" id
                                       "\" — read everything it printed with sh.logs(offset=0),"
                                       " and stop it with sh.stop().")
                                  (get r "note"))}))
       :op :shell})))

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
              "command" (get row "command")
              "cwd" (get row "dir")
              "exit" (get row "exit")
              "started_at" started
              "finished_at" ended
              "log_path" (or (get row "log_path") (.getPath (shell-log/log-file session id)))
              "uptime_ms" (when (and started ended) (max 0 (- (long ended) (long started))))})
      ;; The process is gone by construction: this stage reads a FILE, never a child.
      "status" "exited")))

(defn- shell-logs-impl
  "ONE read of a shell's log, by byte OFFSET or by LINES from the end.

   The log is a file, so a read is a WINDOW on it and the caller owns the cursor:
   feed `next_offset` back and the loop walks the whole stream, however long it
   grew. There is no `n`, no eviction and no `dropped` — nothing a shell printed
   is ever gone while the session lives.

   With no `:offset` the window is the TAIL, which is what someone watching a live
   command wants; `{:offset 0}` is the head, and a loop from there is the whole
   log. A NEGATIVE `:offset` is the last n LINES — `{:offset -50}` reads back 50
   lines, the same reading `cat(path, -50)` has, and clamps to the head when the
   log holds fewer. `is_eof` false means read again NOW rather than sleep.

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

     (authorize-origin! env session id)
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
         ;; while running. `stdout` is the window this read returned, already joined
         ;; — the SAME key a foreground run puts its bytes under, so "what did it
         ;; print" is one field whether the call waited or came back for it later.
         {:result (assoc (if entry (bg-core "logs" id entry) (retired-log-core env session id))
                    ;; Every shell is a PTY, so a tool writes for a SCREEN: CRLF line ends,
                    ;; progress redrawn with a bare CR, colour and keypad escapes it only
                    ;; sent because isatty() was true. The model reads TEXT, so this window
                    ;; is the terminal's own reading of those bytes — the raw stream stays
                    ;; whole on disk at `log_path` for anyone who wants it.
                    "stdout" (normalize-terminal-output (:text chunk))
                    "offset" (:offset chunk)
                    "next_offset" (:next-offset chunk)
                    "is_eof" (:is-eof chunk))
          :op :_shell-logs
          :metadata {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}})))))

(defn- wait-idle-poll-ms
  "Idle sleep before the next read while the child is ALIVE and its log is at EOF,
   as a function of how many idle reads came before it. It is pure LATENCY on a
   command that is about to finish — a flat 50 ms cadence made a 5 ms `echo` cost
   130 ms and looked, correctly, like a wait waiting for nothing — so the first
   reads are near-instant and only a command that keeps running settles on the
   cheap cadence a `npm run dev` deserves."
  [^long idle]
  (cond (< idle 4) 2
        (< idle 10) 10
        :else 50))

(def ^:private wait-drain-poll-ms
  "Sleep between the reads that confirm an EXITED child's log has gone quiet. It
   only has to outlast the pump's handoff of bytes written just before the exit,
   so it is short: every millisecond here is charged to a command that is already
   finished."
  5)

(def ^:private wait-chunk-limit
  "Bytes one wait iteration reads. Large enough that a build's output is drained in
   a few reads, small enough that a runaway producer cannot allocate without bound
   before the deadline is checked again."
  262144)

(defn- shell-wait-impl
  "`sh.wait(secs)` — the ONE bounded wait in the whole product, written HERE and
   reused by every caller: the sandbox handle, the extension handle and the tests
   all call this instead of writing the poll loop a fourth time.

   Read from the cursor, keep reading while bytes are still available, sleep only
   when the log is at EOF and the child is alive, and stop at the deadline — which
   bounds EVERY iteration, not only the idle one, because a command that never
   stops printing always has more bytes and a clock checked only at EOF is never
   reached. A child that has exited is drained to quiet before the wait reports,
   so the last line the pump wrote after the exit is never lost.

   `stdout` is everything printed since this wait's own cursor, under the same key
   a log read answers with, and `timed_out` says which of the two ended it: the
   deadline (true, the process runs on under its id) or the process (false).

   Every iteration polls [[rt/guest-safepoint!]], so a cancelled turn unwinds
   THIS loop instead of waiting the deadline out: an eval whose block sits in a
   long `sh.wait` is the one host wait a turn is most likely to be cancelled
   inside of."
  [env id {:keys [seconds offset]}]
  (let
    [t0
     (now-ms)

     secs
     (clamp-timeout-secs seconds "seconds")

     deadline
     (+ t0 (* 1000 secs))

     start
     (long (or (->pos-long offset "offset") 0))

     ;; A wait accumulates as much as a foreground run captured, and no more: a
     ;; command that never stops printing (`yes`) produced a megabyte a second on
     ;; this machine, so an unbounded accumulator turns a 600 s wait into a heap
     ;; problem. Same head+tail capture, same inline omitted-count marker.
     acc
     (capped-capture max-sync-head-chars max-sync-tail-chars)

     finish
     (fn [res nxt]
       (let [snap ((:snapshot acc))]
         (extension/success
           {:result (assoc res
                      "stage" "wait"
                      "offset" start
                      "next_offset" nxt
                      "stdout" (:text snap)
                      "stdout_omitted_chars" (long (or (:omitted snap) 0))
                      ;; The WAIT expired, not the process: a still-running child means
                      ;; the deadline ended this call and the shell keeps its id.
                      "timed_out" (= "running" (get res "status"))
                      "timeout_secs" secs
                      "duration_ms" (- (now-ms) t0))
            :op :_shell-wait
            :metadata
            {:id id :started-at-ms t0 :finished-at-ms (now-ms) :duration-ms (- (now-ms) t0)}})))]

    (loop
      [off
       start

       ;; Consecutive reads that brought no bytes — the poll cadence backs off
       ;; along this counter, so a fast command is noticed in single-digit ms.
       idle
       0

       quiet
       0]

      (let
        [res
         (:result (shell-logs-impl env id {:offset off :limit wait-chunk-limit}))

         text
         (or (get res "stdout") "")

         nxt
         (long (or (get res "next_offset") off))

         idle
         (if (= "" text) idle 0)]

        ;; Cancellable in HOST code: an interrupt lands HERE, at guest-code
        ;; speed, instead of at this wait's own deadline.
        (rt/guest-safepoint!)
        ((:append! acc) text)
        (cond (>= (now-ms) deadline) (finish res nxt)
              (not (get res "is_eof")) (recur nxt 0 0)
              (= "running" (get res "status")) (do (Thread/sleep (long (wait-idle-poll-ms idle)))
                                                   (recur nxt (inc idle) 0))
              ;; Exited: one confirming quiet read before reporting, so bytes the pump
              ;; flushed between the exit and this read still reach the caller.
              (and (= "" text) (pos? quiet)) (finish res nxt)
              :else (do (Thread/sleep (long wait-drain-poll-ms))
                        (recur nxt (inc idle) (if (= "" text) (inc quiet) 0))))))))


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
   response with `sh.logs()`. Returns the total shell result."
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

     (authorize-origin! env session id)
     (when-not entry
       (throw (ex-info (str "No background shell '"
                            id
                            "' in this session — start one with"
                            " await shell({\"command\": \"…\", \"wait\": 0, \"id\": id});"
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
         (extension/success {:result (assoc (bg-core "send" id entry) "keys" (keys-label payload))
                             :op :_shell-type
                             :metadata
                             {:id id :started-at-ms t :finished-at-ms t :duration-ms 0}}))))))

;; Internal lifecycle grammar — the model calls `shell` /

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
  "`sh.stop()` — the TERMINAL lifecycle stage. Stopping used to be
   reachable only through `resource_stop`, so the end of a background shell's life
   was undiscoverable from the thing that started it; it is now a method on the
   handle. Routes through
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
     (locking (bg-lifecycle-lock session id)
       (authorize-origin! env session id)
       [(bg-entry session id) (resources/stop! session id)])]

    (when (= :unknown (:result r))
      (throw (ex-info (str "No background shell '" id
                           "' in this session — nothing to stop;"
                           " live ids are listed in resources.")
                      {:type ::unknown-bg-id :id id})))
    (when (= :error (:result r))
      (throw (ex-info (str "Background shell '" id "' failed to stop: " (:message r))
                      {:type ::stop-failed :id id})))
    (extension/success {:result (assoc (bg-core "stop" id entry) "status" "stopped")
                        ;; Tagged with its OWN tool: each of the five verbs is a
                        ;; registered symbol op carrying its own observation/mutation tag.
                        :op :_shell-stop
                        :metadata
                        {:id id :started-at-ms t :finished-at-ms (now-ms) :duration-ms 0}})))

(defn shell-dispatch
  "INTERNAL shell lifecycle grammar, kept for the Python-extension entry points
   (`trusted-extension-shell`, `jailed-shell`, `session-jailed-shell`) whose
   caller authors an options map by hand and therefore genuinely needs an `op`
   discriminator. The MODEL never reaches this: it calls the `shell` PYTHON verb —
   one call that spawns one command — and drives what came back through the
   HANDLE's own methods (`sh.logs()`, `sh.wait()`, `sh.type(\"y\")`, `sh.stop()`), so one
   disambiguate."
  [env opts]
  (when-not (opts-arg? opts)
    (throw (ex-info "shell takes one options map, e.g. shell({\"command\": \"ls\"})."
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
       (throw (ex-info "shell has no `cmd` option — put the bash line in {\"command\": \"…\"}."
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
         ;; `run` is the only default there is now: a caller that names an `id` is
         ;; naming its handle, not selecting a second lifecycle.
         "run")

     ;; Lifecycle stages operate on an existing background shell.  A caller that
     ;; mechanically reuses the start shape may still carry `command`; it is not
     ;; relevant to these stages and must not make an otherwise valid call fail.
     opts
     (if (#{"logs" "wait" "send" "stop"} op) (dissoc raw-opts "command" :command) raw-opts)

     command
     (opt opts :command)

     valid-command
     (when (some? command) (one-command command))

     need-command
     (fn []
       (or valid-command
           (throw (ex-info
                    (str "shell op \"" op "\" needs {\"command\": \"…\"} in its options map.")
                    {:type ::missing-command :op op}))))

     need-id
     (fn []
       (or id
           (throw (ex-info (str "shell op \""
                                op
                                "\" needs {\"id\": \"…\"}; live ids are listed in resources.")
                           {:type ::missing-id :op op}))))

     reject-command
     (fn []
       (when (some? command)
         (throw (ex-info (str "shell op \"" op "\" takes no command — it acts on {\"id\": \"…\"}.")
                         {:type ::unexpected-command :op op}))))

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
      ;; There is no `wait` on the REQUEST. A run ALWAYS spawns under a real PTY and
      ;; returns NOW with its handle, so one call has one meaning and a number can
      ;; never select a second mode. Waiting is `sh.wait(secs)` on the handle — the
      ;; one place that can also read, type and stop — and the log file every run
      ;; leaves behind is the FEATURE: "what did that build print" stays answerable
      ;; by id for as long as the session lives.
      (do (reject-text)
          (let
            [cmd
             (need-command)

             session
             (:session-id env)

             run-id
             (or id (auto-bg-id env session cmd))]

            (releasing-claim
              session
              run-id
              (fn [run-id]
                (let [opts (assoc opts "id" run-id)]
                  (if-let [live (reissue-live-entry env session run-id cmd (opt opts :cwd))]
                    (live-run-result run-id live)
                    (run-of-background (shell-bg-impl env run-id cmd opts))))))))

      "background"
      ;; A start is the one background stage with no prior handle: derive the id from
      ;; the command rather than fail a well-formed start over a missing name. Every
      ;; other stage (no command) still names the shell it acts on.
      (do (reject-text)
          (let
            [bg-id (if (and (nil? id) valid-command)
                     (auto-bg-id env (:session-id env) valid-command)
                     (need-id))]
            (releasing-claim (:session-id env)
                             bg-id
                             (fn [bg-id]
                               (shell-bg-impl env bg-id valid-command opts)))))

      "logs"
      (do (reject-command)
          (reject-text)
          (shell-logs-impl env
                           (need-id)
                           {:offset (->whole-long (opt opts :offset) "offset")
                            :limit (->pos-long (opt opts :limit) "limit")}))

      "wait"
      ;; The ONE wait, on the handle and in the host: no caller writes a poll loop.
      (do (reject-command)
          (reject-text)
          (shell-wait-impl env (need-id) {:seconds (opt opts :seconds) :offset (opt opts :offset)}))

      "send"
      (do (reject-command) (shell-send-impl env (need-id) (need-text) opts))

      "stop"
      (do (reject-command) (reject-text) (shell-stop-impl env (need-id)))

      (throw (ex-info (str "Unknown shell op " (pr-str op)
                           " — use \"run\" (default), \"background\", \"logs\", \"wait\","
                           " \"send\" or \"stop\".")
                      {:type ::unknown-op :op op})))))

(defn run-argv
  "Run ONE literal argv through the SAME bounded machinery `shell` runs its own
   command with: cwd authorization, process-jail policy, head+tail capped
   capture, timeout and kill-tree. Returns that command's own total entry — the
   SAME [[shell-result-base]] map `shell` itself answers a foreground call with,
   carrying the request's `:dir`/`:timeout-secs` as metadata, so an argv run and `shell`
   have ONE result shape and there is no envelope to unwrap.

   No shell is involved — each element reaches the process verbatim, so nothing
   needs quoting. An argv caller is a USER of this: every argv command is a
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
                      (assoc :shell-origin "trusted-extension")
                      ;; Unconfined, but the workspace's `.env` and the operator's
                      ;; `environment:` declarations still apply: they say where a variable
                      ;; comes from, not who may see it.
                      (assoc :jail-policy-fn (constantly {:disabled? true
                                                          :env-values
                                                          (config/child-environment-values)}))
                      (assoc-in [:security-policy :jail-enabled] false))
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

    (if (or (false? (:jail-enabled security)) (:disabled? jail-config))
      {:disabled? true :env-values (config/child-environment-values) ::environment latest-env}
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
                       ;; Resolved per spawn: a `.env` edit lands on the next child.
                       :env-values (config/child-environment-values)
                       ::environment latest-env
                       ::cleanup cleanup}))
             (catch Throwable t (cleanup) (throw t)))))))

(defn jailed-shell
  "Run `vis.jailed_shell` through a strict policy read from the latest merged
   on-disk configuration at every process spawn. Works with or without a session;
   invalid current config refuses that spawn instead of using a snapshot."
  [env opts]
  (shell-dispatch (-> (or env {})
                      (assoc :shell-origin "jailed-extension")
                      (assoc :jail-policy-fn #(latest-jail-policy env)))
                  opts))

(defn session-jailed-shell
  "Run `vis.jailed_shell_session` through the invoking session's immutable jail
   snapshot. Requires a live session and never re-reads configuration."
  [env opts]
  (when-not (:session-id env)
    (throw (ex-info "jailed_shell_session is available only while handling a session"
                    {:type ::no-session})))
  (shell-dispatch (assoc env :shell-origin "jailed-session") opts))


;; Env injection — the before-fn hands the impl its env as first arg

(defn- op-label
  "Human call name from a registered op keyword: :_shell-logs -> \"_shell_logs\"."
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

;; Public, doc-bearing vars retain developer examples and fallback docs. Native
;; symbols below provide compact model-facing semantics; their schemas provide
;; exact inputs. The injected `env` first arg is hidden from both.

(defn- shell-call-opts
  "ONE options map from a call's arguments. Each shell tool takes named positional
   values (`command`, `id`, `text`) followed by an optional keyword dict, and an
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

(defn shell
  "`sh = await shell(\"npm test\")` — spawn ONE bash line under a pty and return
   the HANDLE now. Every run is a background run; waiting is `sh.wait(secs)`, the
   only wait there is. The handle reads
   (`sh.logs(offset=0)`), types
   (`sh.type(text)`) and kills (`sh.stop()`), and its log file outlives the call,
   so nothing is ever lost to a deadline or to a call that already returned."
  {:arglists '([command] [command opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["command"] args) "op" "run")))


(defn shell-logs
  "`sh.logs()` — read a background shell's log from a byte offset, or the last
   n LINES with a negative one (`sh.logs(-50)`), and return NOW. Nothing blocks
   on your behalf: a wait is a bounded loop you write in `python_execution` and
   break on what you actually read."
  {:arglists '([id] [id opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id"] args) "op" "logs")))

(defn shell-wait
  "`sh.wait(secs)` — the ONLY wait there is. Block until the shell exits or the
   deadline passes, and answer the accumulated `stdout` plus the final `exit`.
   `timed_out` true means the WAIT expired; the process runs on under its id."
  {:arglists '([id] [id seconds] [id seconds opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id" "seconds"] args) "op" "wait")))

(defn shell-type
  "`sh.type(\"y\")` — type keystrokes at a background shell's stdin.
   `is_enter` (default true) submits the line, which is what an interactive prompt
   waits for; read the response with `sh.logs()`."
  {:arglists '([id text] [id text opts])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id" "text"] args) "op" "send")))

(defn shell-stop
  "`sh.stop()` — kill the process tree and drop the retained logs and
   session resource."
  {:arglists '([id])}
  [env & args]
  (shell-dispatch env (assoc (shell-call-opts ["id"] args) "op" "stop")))

;; The `!cmd` bang card — `:result` → `{:summary :body}`, built by
;; `render-shell-run-result` below and called directly by the loop's bang path.
;; The result arrives string-keyed snake_case (strings-only boundary); the card
;; is the keyword `{:summary :body}` IR (that part is internal).

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
   `json.loads(r[\"stdout\"])` works; only the card is clipped."
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

(defn render-shell-run-result
  "shell op `run` → REPL-style collapsed/expanded card. The ONE surviving op-card
   renderer, and it is called DIRECTLY: the `!cmd` bang path in the loop PRINTS a
   command's output, so this card IS its whole answer. No registry, no symbol key
   — every other result is painted from the result's own data.

   Collapsed: `$ npm test (success) · 1.2s` or
   `$ grep x missing (failure) · exit 2 · 34ms`.
   Expanded: labeled COMMAND / STATUS / STDOUT sections. The body is
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
     (kv-lines [["status" label] ["duration" duration] ["cwd" (get r "cwd")]
                ;; The timeout budget is TOTAL in the result but only worth a row
                ;; when it was actually hit.
                ["timeout" (when (get r "timed_out") (str (get r "timeout_secs") "s"))]
                ;; Truncation is REAL data loss, but `*_omitted_chars` is 0 unless the
                ;; middle-excision actually dropped something — the count IS the flag,
                ;; so name the stream and the exact number of characters lost.
                ["stdout"
                 (let [n (get r "stdout_omitted_chars")]
                   (when (and n (pos? (long n))) (str "truncated · " n " chars omitted")))]])

     body
     (->> [(shell-section "COMMAND" (format-shell-command (get r "command")) "bash")
           (shell-section "STATUS" status)
           (shell-section "STDOUT" (clip-stream (get r "stdout")) "bash")]
          (remove nil?)
          (str/join "\n\n"))]

    {:summary summary :body (when (seq body) body)}))


;; Symbols + prompt + extension. ONE builtin symbol — `shell` — bound bare in the
;; flat Python sandbox next to `ls` / `grep`.

(defn- live-bg-script
  "The bash the LIVE background shell `id` is already running. A `logs`/`send`/
   `stop` call runs no command of its OWN, but the command it acts on is right
   there in the registry — the same line the finished card renders out of its
   result — so the live ticker can name that real command.

   nil when no live shell answers to that id, or when two sessions both do and the
   answer would be a guess."
  [id]
  (when id
    (let [hits (into [] (keep #(get % id)) (vals @bg-procs))]
      (when (= 1 (count hits)) (present-str (:script (first hits)))))))


(defn- shell-ticker
  "LIVE-TICKER phrase for one shell call — what the bubble says while the call is
   still in flight, completing `Vis is …`. This is the ONLY live presentation a
   process has: nothing is advertised as a native tool any more, so the spawn and
   every method on its handle say here what they are doing.

   A transport's own name answers nothing: `_shell-wait tt` names neither the
   command nor the budget, which is exactly how a wait that is doing its job reads
   as a wait stuck on nothing. Naming the HANDLE answers no better — `tt` is
   bookkeeping the caller invented and nobody else can resolve. So the sentence
   names the COMMAND and the budget — `running: npm test`, `waiting up to 60s for:
   npm test` — taken from the spawn's OWN arguments, and otherwise read live from
   the registry so `wait`/`logs`/`stop`, which run no command of their own, still
   say which bash they act on. `the shell` when neither answers: a generic noun
   tells the reader as much as a private token and misleads less."
  [op]
  (fn [_env args]
    (let
      [input
       (shell-call-opts (case op
                          "run"
                          ["command"]

                          "wait"
                          ["id" "seconds"]

                          "send"
                          ["id" "text"]

                          ["id"])
                        args)

       id
       (some-> (opt input :id)
               str
               str/trim
               not-empty)

       secs
       (or (some-> (opt input :seconds)
                   str
                   str/trim
                   not-empty)
           "120")

       ;; The spawn KNOWS its command — it is right there in the call — while a
       ;; lifecycle stage has to look up the shell it acts on.
       script
       (or (some-> (try (command-line (opt input :command)) (catch Throwable _ nil))
                   shell-one-line
                   not-empty)
           (some-> (live-bg-script id)
                   shell-one-line))

       ;; Every verb ends on the preposition, so the nameless case is the same
       ;; sentence with the generic noun: `stopping the shell`.
       verb
       (case op
         "run"
         "running"

         "wait"
         (str "waiting up to " secs "s for")

         "logs"
         "reading the log of"

         "send"
         "typing into"

         "stop"
         "stopping"

         op)]

      (if (seq script) (str verb ": " (clip-chip script shell-chip-max)) (str verb " the shell")))))


(def shell-symbol
  (vis/symbol
    #'shell
    {:symbol 'shell
     ;; NOT a native tool: a process is started from PYTHON and nowhere else.
     ;; A `tool_use` schema can only ever spawn ONE command and then hand back an
     ;; object the wire cannot hold — every verb after the spawn (`wait`, `logs`,
     ;; `type`, `stop`) is a METHOD on the handle, so the whole family belongs
     ;; where the handle already lives. `apropos('shell')` / `doc('shell')` are
     ;; how it is found, exactly like every other sandbox capability.
     :name "shell"
     :result
     (str "A HANDLE: ONE result shape for every shell answer — `{stage, id, cwd, command, "
          "status, exit, stdout, duration_ms, timed_out, offset, next_offset, is_eof, "
          "started_at, finished_at, log_path, cpu_ms, cpu_percent, rss_bytes, note, …}` plus the "
          "methods below. A fresh run has no `exit`; nonzero exit is data.")
     :description
     (str "`shell(command, {\"id\": …, \"cwd\": …})` — spawn ONE `bash -lc` `command` under a "
          "real pty and return its HANDLE NOW; every run is a background run. Chain with `&&`; "
          "run independent work as separate calls. Drive "
          "it through the handle: `sh.wait(secs)` (the ONLY wait; `timed_out` means the WAIT "
          "expired, not the process), `sh.logs(-50)` (the last 50 LINES; `offset=…` bytes), "
          "`sh.type(text)`, `sh.stop()` — never "
          "a rerun; re-issuing a live `id` returns THAT shell. Every answer already carries the "
          "STATUS — `status`/`exit`, the clock, `log_path` and live cpu/rss — so nothing asks "
          "twice. NEVER trim inside the command: `| head`, "
          "`| tail`, `| grep`, `2>/dev/null`, `> file` discard bytes the handle keeps whole, and "
          "a pipeline's exit is its LAST stage's, so a failed build looks green — run it plain, "
          "then slice on the handle or filter `log_path` in Python. "
          "`print((await shell(\"npm test\")).wait(300)[\"stdout\"])`.")
     :params [{:name "id" :note "reuse to re-attach a live shell"}
              {:name "cwd"}
              {:name "timeout_secs"}]
     :ticker-fn (shell-ticker "run")
     :inject-env? true
     :tag :mutation
     :on-error-fn (shell-on-error :shell)}))


(def shell-logs-symbol
  (vis/symbol
    #'shell-logs
    {:symbol '_shell-logs
     :name "_shell_logs"
     :result
     (str "The same shell result shape as every other stage (`stage` is \"logs\"): `stdout` is the "
          "window this read returned; feed `next_offset` back to continue, and `is_eof` false "
          "means read again now.")
     :description
     (str "TRANSPORT for `sh.logs(offset=…, limit=…)` — call the HANDLE the shell result already "
          "is, not this. Reads a background shell's log and returns NOW. No offset reads the TAIL; "
          "`offset=0` starts at the beginning; a NEGATIVE offset is the last n LINES "
          "(`sh.logs(-50)`), the same reading `cat(path, -50)` has.")
     :inject-env? true
     :tag :observation
     :ticker-fn (shell-ticker "logs")
     :on-error-fn (shell-on-error :_shell-logs)}))

(def shell-wait-symbol
  (vis/symbol
    #'shell-wait
    {:symbol '_shell-wait
     :name "_shell_wait"
     :result
     (str "The same shell result shape (`stage` \"wait\"): `stdout` is everything printed since "
          "this wait's cursor, `exit`/`status` are final unless `timed_out` is true, which means "
          "the WAIT expired and the shell keeps running under its id.")
     :description
     (str "TRANSPORT for `sh.wait(seconds)` — call the HANDLE the shell result already is, not "
          "this. Blocks until the command exits or the deadline passes; the bounded poll loop "
          "lives here so no caller writes one.")
     :inject-env? true
     :tag :observation
     :ticker-fn (shell-ticker "wait")
     :on-error-fn (shell-on-error :_shell-wait)}))

(def shell-type-symbol
  (vis/symbol
    #'shell-type
    {:symbol '_shell-type
     :name "_shell_type"
     :result "The same shell result shape (`stage` \"send\"): `sent` chars, `keys` label."
     :description
     "TRANSPORT for `sh.type(text, is_enter=True)` — call the handle. Writes keystrokes to a background shell's stdin."
     :inject-env? true
     :tag :mutation
     :ticker-fn (shell-ticker "send")
     :on-error-fn (shell-on-error :_shell-type)}))

(def shell-stop-symbol
  (vis/symbol
    #'shell-stop
    {:symbol '_shell-stop
     :name "_shell_stop"
     :result "The same shell result shape (`stage` \"stop\"): `status` \"stopped\", `exit`."
     :description
     "TRANSPORT for `sh.stop()` — call the handle. Kills a background shell's process tree and drops its retained logs and resource."
     :inject-env? true
     :tag :mutation
     :ticker-fn (shell-ticker "stop")
     :on-error-fn (shell-on-error :_shell-stop)}))

(def shell-symbols
  ;; NO native tool at all, and ONE object to drive what a Python call starts.
  ;; `shell` is an engine-bound sandbox verb; `_shell_logs` / `_shell_wait` /
  ;; `_shell_type` / `_shell_stop` are PRIVATE transport (underscore-prefixed so
  ;; `apropos` never lists them): the model calls the handle's own `sh.logs()` /
  ;; `sh.wait()` / `sh.type(\"y\")` / `sh.stop()`, because operations on an object the
  ;; caller already holds are control flow, not more schemas to disambiguate before
  ;; running a command. There is no status transport at all: every stage's answer
  ;; already carries the status block.
  [shell-symbol shell-logs-symbol shell-wait-symbol shell-type-symbol shell-stop-symbol])

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

(vis/register-toggle!
  {:id "shell"
   :label "Shell commands"
   ;; One line: the row is a control, not documentation. The full contract —
   ;; pty handle ops, "never a native tool", the jail, and the MODEL's-door
   ;; limit that leaves an installed extension's own `vis.shell` / `subprocess`
   ;; boundary ungated — lives in this namespace's docstring.
   :description "Expose the Python `shell` verb; the model's commands run inside the OS jail."
   :default true
   :owner :vis
   :persist? true
   :group :sandbox})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shell"
     :ext/description
     "No native shell tool: the `shell` PYTHON verb spawns ONE command under a pty and answers with a live handle (`sh.logs()` / `sh.wait()` / `sh.type(\"y\")` / `sh.stop()`) whose every answer carries the shell's status; `resource_stop` also stops PTYs. Default-on behind the `shell` toggle and OS process jail."
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
