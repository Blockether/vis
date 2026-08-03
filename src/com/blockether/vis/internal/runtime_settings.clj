(ns com.blockether.vis.internal.runtime-settings
  "Per-eval / per-call runtime knobs for the loop: Python-sandbox eval timeouts
   (with clamping and a shell-timeout-aware widener), the `svar/ask-code!` stream
   watchdog defaults, and the dynamic vars the loop binds per call.

   A LEAF — depends on nothing else in the engine, so the loop and its tests read
   these settings from one place instead of carrying them in the loop namespace.")

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in the Python sandbox."
  120000)

(def MIN_EVAL_TIMEOUT_MS "Floor for :eval-timeout-ms." 3000)

(def MAX_EVAL_TIMEOUT_MS "Hard ceiling for :eval-timeout-ms." (* 30 60 1000))

(def NATIVE_TOOL_TIMEOUT_MS
  "Wall-clock fallback for Clojure-native tool handlers when the caller does not
   request a `timeout_ms` — 30 seconds. Calls expected to take longer must
   explicitly request a timeout or use a background workflow. Slow synchronous
   SETUP (e.g. a cold project-REPL boot) runs OUTSIDE this wall via the loop's
   `:vis/outside-tool-wall` hook, so it never bills against this budget."
  30000)

(def ^:private native-tool-timeout-grace-ms
  "Room for a tool's own timeout to produce its structured result first."
  1000)

(defn native-tool-timeout-ms
  "Return the outer wall-clock deadline for a native handler. Always sits a short
   grace period ABOVE the tool's own budget — an explicit `timeout_ms`, otherwise
   the 30-second fallback — so a tool whose internal timeout equals the fallback
   (e.g. repl_eval's 30s default) still gets to produce its STRUCTURED timeout
   result before the wall fires. Capped at `MAX_EVAL_TIMEOUT_MS`."
  [input]
  (let
    [requested
     (when (map? input)
       (or (get input "timeout_ms") (get input :timeout_ms) (get input :timeout-ms)))

     requested
     (when (and (number? requested) (pos? (long requested))) (long requested))

     base
     ;; ALWAYS add the grace: the wall is a BACKSTOP, never a co-deadline. If it
     ;; equalled the tool's own timeout the two would race and the wall's raw
     ;; :vis/native-tool-timeout error would clobber the tool's nice structured
     ;; timeout result (repl_eval's `⧖ timed out after Nms` card degraded to a
     ;; bare message string exactly because default == default here).
     (+ (long (if requested (long requested) NATIVE_TOOL_TIMEOUT_MS))
        (long native-tool-timeout-grace-ms))]

    (long (min (long MAX_EVAL_TIMEOUT_MS) base))))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for Python code evaluation."
  DEFAULT_EVAL_TIMEOUT_MS)

(def ASK_CODE_TTFT_TIMEOUT_MS
  "Default time-to-first-token timeout for Vis `svar/ask-code!` calls (ms).

   300s = CODEX PARITY. The Codex CLI has NO separate first-token budget:
   its single `stream_idle_timeout_ms` (default
   `DEFAULT_STREAM_IDLE_TIMEOUT_MS = 300_000`) governs the wait for the
   FIRST event exactly like every later transport gap. Matching that 300s
   budget avoids hanging up first on a queued or cold-starting provider; a
   genuinely dead connection still fails on its own transport error. Model-
   progress silence is a separate, opt-in semantic watchdog below."
  300000)

(def ASK_CODE_IDLE_TIMEOUT_MS
  "Default inter-chunk idle timeout for Vis `svar/ask-code!` calls (ms).

   Fires when the transport itself goes silent — not one byte, not even an
   SSE keepalive comment. 300s = CODEX PARITY with
   `DEFAULT_STREAM_IDLE_TIMEOUT_MS`. Model-progress silence while keepalives
   continue is different and does not time out by default; see
   `ASK_CODE_SEMANTIC_TIMEOUT_MS`."
  300000)

(def ASK_CODE_SEMANTIC_TIMEOUT_MS
  "Default model/progress timeout for Vis `svar/ask-code!` streams (ms).

   Disabled by default. A provider can keep its transport healthy with SSE
   keepalives while legitimately emitting no model-visible event during a long
   encrypted-reasoning phase. Treating that silence as failure loses healthy
   turns, as the transport's 300s idle watchdog already catches truly silent
   or wedged connections.

   Callers that require a bounded model-progress gap can opt in with
   `:semantic-timeout-ms`; explicit nil also disables it per call."
  nil)

(defn with-default-ask-code-idle-timeout
  [opts]
  (cond-> opts
    (not (contains? opts :ttft-timeout-ms))
    (assoc :ttft-timeout-ms ASK_CODE_TTFT_TIMEOUT_MS)

    (not (contains? opts :idle-timeout-ms))
    (assoc :idle-timeout-ms ASK_CODE_IDLE_TIMEOUT_MS)

    (and (some? ASK_CODE_SEMANTIC_TIMEOUT_MS) (not (contains? opts :semantic-timeout-ms)))
    (assoc :semantic-timeout-ms ASK_CODE_SEMANTIC_TIMEOUT_MS)))

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  ^long [candidate]
  (let [candidate (long candidate)]
    (min (long MAX_EVAL_TIMEOUT_MS) (max (long MIN_EVAL_TIMEOUT_MS) candidate))))

(def ^:private shell-timeout-eval-grace-ms
  "Extra room around a bounded call's OWN timeout so that call can kill, drain,
   and return its structured timeout envelope before the outer Python eval
   watchdog fires. Mirrors `native-tool-timeout-grace-ms`: the watchdog is a
   BACKSTOP, never a co-deadline."
  10000)

(def DEFAULT_SHELL_TIMEOUT_SECS
  "Default per-command / per-wait budget of the `shell` tool — declared here, and
   read by `foundation.shell`, so ONE number governs both the tool and the
   watchdog above it.

   It matters here because it used to EQUAL the 120s eval watchdog. A `shell`
   call that named no timeout therefore raced a watchdog that had already
   started, and the watchdog always won: the turn got a bare `Timeout (120s)`
   with NO output instead of shell's own envelope (partial stdout, `timed_out`
   true, a killed process tree). The widener below now floors the watchdog above
   this budget whenever an eval calls the shell at all."
  120)

(def MAX_SHELL_TIMEOUT_SECS
  "Hard ceiling `foundation.shell` clamps EVERY `run` / `wait` budget to: ten
   minutes. Declared here so the widener below can floor the eval watchdog above
   the LONGEST budget a shell call may legally own, not merely above the default.

   A budget that is a variable or an expression is invisible to a text scan, so
   flooring at the 120s default let the watchdog preempt a legal ten-minute wait
   with a bare `Timeout (120s)` instead of shell's own envelope — the same defect
   `DEFAULT_SHELL_TIMEOUT_SECS` describes, one spelling further out."
  600)

(def RUN_TESTS_FLOOR_SECS
  "Floor for an eval that calls `run_tests`. A test run carries its OWN multi-
   minute budget (the Clojure pack's 290s nREPL deadline) and answers a timeout
   with a STRUCTURED test result; a direct tool call parks the native wall for
   exactly that reason. Called from `python_execution` it must not die earlier at
   the generic 120s watchdog and lose the run's result."
  300)

(def ^:private timeout-secs-re
  #"[\"']?(?:timeout_secs|timeout)[\"']?\s*(?::|=)\s*([0-9]+(?:\.[0-9]+)?)")

(def ^:private timeout-ms-re #"[\"']?timeout_ms[\"']?\s*(?::|=)\s*([0-9]+(?:\.[0-9]+)?)")

(def ^:private shell-call-re #"\bshell\s*\(|\bsubprocess\.")

(def ^:private run-tests-call-re #"\brun_tests\s*\(")

(defn- max-of
  [xs]
  (when-let [xs (seq (remove nil? xs))]
    (apply max xs)))

(defn explicit-shell-timeout-secs
  "Best-effort scan for an EXPLICIT timeout override in Python code, in seconds.
   Reads `timeout_secs` / `timeout` (seconds) and `timeout_ms` (milliseconds,
   rounded up) — `repl_eval` and MCP calls spell their budget in ms, and leaving
   that spelling out let a deliberately long call die at the default watchdog.
   The real tool still owns validation/clamping; this only prevents the outer
   watchdog from preempting a longer requested budget."
  [code]
  (let
    [code
     (str code)

     secs
     (for [[_ n] (re-seq timeout-secs-re code)]
       (long (Math/round (Double/parseDouble n))))

     ms
     (for [[_ n] (re-seq timeout-ms-re code)]
       (long (Math/ceil (/ (Double/parseDouble n) 1000.0))))]

    (max-of (concat secs ms))))

(defn implicit-call-budget-secs
  "Seconds an eval is entitled to purely from WHICH bounded calls it makes, when
   none of them names a number. A timeout that is a variable, an expression, or
   simply the tool's default is invisible to a text scan, so without this floor
   the watchdog silently preempts a call that owns a longer budget and answers
   timeouts itself.

   A `shell` call the scan cannot read may legally own the FULL cap — a ten-minute
   `wait` — so that is its floor. When the block DOES spell a second budget out the
   scan already reads it, and the floor drops back to shell's default so a second,
   unannotated call in the same block still cannot race the watchdog."
  [code]
  (let [code (str code)]
    (max-of
      [(when (re-find run-tests-call-re code) RUN_TESTS_FLOOR_SECS)
       (when (re-find shell-call-re code)
         (if (re-find timeout-secs-re code) DEFAULT_SHELL_TIMEOUT_SECS MAX_SHELL_TIMEOUT_SECS))])))

(defn eval-timeout-ms-for-code
  "Eval watchdog for ONE Python block: the configured base, raised so it sits a
   grace period ABOVE the longest bounded call the block makes."
  [base-timeout-ms code]
  (let
    [base
     (clamp-eval-timeout-ms base-timeout-ms)

     secs
     (max-of [(explicit-shell-timeout-secs code) (implicit-call-budget-secs code)])]

    (if secs
      (clamp-eval-timeout-ms (max base (+ (* 1000 (long secs)) (long shell-timeout-eval-grace-ms))))
      base)))

(def ^:dynamic *blocking-wall-park*
  "Park hook installed by the innermost enclosing timeout wall: `(fn [thunk])`.

   Every wall in this engine counts WALL-CLOCK time, so a legitimate block —
   above all a `human-input` pause waiting on the operator — otherwise dies at
   the wall with a bare `Timeout (120s)` while the dialog is still up. Code
   about to park on a human answer calls [[park-blocking-wall]]; every
   enclosing wall then stops its clock until the thunk returns."
  nil)

(defn park-blocking-wall
  "Run `thunk` with every enclosing timeout wall parked. With no wall installed
   (plain JVM call, tests) this is just `(thunk)`."
  [thunk]
  (if-let [park *blocking-wall-park*]
    (park thunk)
    (thunk)))

(defn parkable-wall
  "One MOVABLE wall clock for a bounded execution that began at `start` with
   `timeout-ms` of budget.

   Returns `{:deadline <atom epoch-ms> :park (fn [thunk])}`.

   `park` is RE-ENTRANT: nested parks each push the deadline out to
   [[MAX_EVAL_TIMEOUT_MS]] and only the OUTERMOST exit restores the base
   budget, so an inner park returning cannot collapse the clock while an outer
   park is still live. It also COMPOSES with the park inherited from
   [[*blocking-wall-park*]], so parking an inner wall parks every enclosing one
   too — a native tool that asks the operator a question must not be killed by
   the Python eval watchdog wrapped around it."
  [start timeout-ms]
  (let
    [timeout-ms
     (long timeout-ms)

     deadline
     (atom (+ (long start) timeout-ms))

     depth
     (atom 0)

     inherited
     *blocking-wall-park*

     park
     (fn [thunk]
       (swap! depth inc)
       (reset! deadline (+ (System/currentTimeMillis) (long MAX_EVAL_TIMEOUT_MS)))
       (try (thunk)
            (finally (reset! deadline (+ (System/currentTimeMillis)
                                         (if (pos? (long (swap! depth dec)))
                                           (long MAX_EVAL_TIMEOUT_MS)
                                           timeout-ms))))))]

    {:deadline deadline
     :park (if inherited
             (fn [thunk]
               (park #(inherited thunk)))
             park)}))

(defn await-wall
  "Wait for `fut` until the CURRENT value of `deadline` passes, re-reading the
   atom on every wake so a park that MOVED the wall extends the wait instead of
   expiring. Returns `timeout-value` once the wall is really reached."
  [fut deadline timeout-value]
  (loop []

    (let [remaining (- (long @deadline) (System/currentTimeMillis))]
      (if (pos? remaining)
        (let [r (deref fut remaining timeout-value)]
          (if (identical? timeout-value r) (recur) r))
        timeout-value))))

(def ^:dynamic *rlm-context* "Dynamic context for RLM debug logging." nil)
