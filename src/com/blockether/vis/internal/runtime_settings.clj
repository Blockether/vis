(ns com.blockether.vis.internal.runtime-settings
  "Per-eval / per-call runtime knobs for the loop: Python-sandbox eval timeouts
   (with clamping and a shell-timeout-aware widener), the `svar/ask-code!` stream
   watchdog defaults, and the dynamic vars the loop binds per call.

   Also the HOST half of the cancel contract: [[park-blocking-wall]] stops a
   wall's clock while host code legitimately blocks, and [[guest-safepoint!]]
   keeps that block cancellable.

   A LEAF — depends on nothing else in the engine, so the loop and its tests read
   these settings from one place instead of carrying them in the loop namespace."
  (:require [com.blockether.vis.internal.util :as util])
  (:import [org.graalvm.polyglot Context]))

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default wall-clock backstop around ONE `python_execution` block — five minutes.

   A BACKSTOP for guest code that would never finish on its own (`while True:`, a
   wedged frame), never a co-deadline for work that is progressing. Every BOUNDED
   call a block makes — shell, `run_tests`, HTTP — is already floored ABOVE this
   number by the widener below, so what this number really bounds is the in-sandbox
   compute no scan can see: a large parse, an image pass, an analytic loop over
   thousands of files. At two minutes that work was killed exactly where it got
   expensive, and the model re-ran the whole block from zero.

   The price is deliberate: a genuinely wedged block now costs five minutes of a
   turn instead of two. It stays far under [[MAX_EVAL_TIMEOUT_MS]] and under every
   bounded-call floor, so a block that makes such a call still gets that call's own
   budget plus the widener's grace."
  300000)

(def MIN_EVAL_TIMEOUT_MS "Floor for :eval-timeout-ms." 3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms — 35 minutes.

   It must sit ABOVE the longest bounded call a block may own plus the widener's
   grace: [[MAX_SHELL_TIMEOUT_SECS]] (30 min) + `shell-timeout-eval-grace-ms`. A
   ceiling equal to the shell cap would clamp the widened watchdog 10s BELOW the
   shell envelope, so a legal 30-minute wait would die at the watchdog with a bare
   `Timeout` and no output instead of shell's own structured envelope — the exact
   defect [[DEFAULT_SHELL_TIMEOUT_SECS]] describes. Lift the two together."
  (* 35 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for Python code evaluation."
  DEFAULT_EVAL_TIMEOUT_MS)

(def ASK_CODE_TTFT_TIMEOUT_MS
  "Default time-to-first-token timeout for Vis `svar/ask-code!` calls (ms).

   200s. The wait for the FIRST response header, and nothing else: no bytes
   arrived, no tool ran, no output was streamed, so the request can simply be
   made again.

   Deliberately WIDER than svar's own `router/DEFAULT_TTFT_TIMEOUT_MS` (two
   minutes). That number is tuned for a router holding a second candidate to
   cross to; Vis pins the route to one provider+model, so the header a cold
   start or a deep provider queue would have produced at 130s has nowhere
   else to come from and must not be called dead at 120s. It still sits under
   svar's `router/DEFAULT_TIMEOUT_MS` (the 300s whole-request cap Vis leaves
   at its default), so this watchdog — not the HTTP client — is what names the
   failure and the abort keeps its type (`:svar.core/stream-ttft-timeout`)
   for the retry below to recognize. A project that lowers `network.timeout_ms`
   under 200s reaches the client's own timeout first.

   Measured: a pinned zai-coding-plan turn spent its last 120s waiting for
   headers that never came and died with ten iterations of finished work
   behind it, because svar's router had no second candidate to cross to. The
   re-issue is what fixed that; this number only decides how long one try
   waits. Because the abort is raised while no output exists,
   `loop/pre-output-stream-retryable?` re-issues it seconds apart up to
   `MAX_PRE_OUTPUT_STREAM_RETRIES` times, so a wedged endpoint is named by
   three visible retries — about ten minutes at 200s, where 60s bought three —
   instead of by one silent gap. Never shorten this without that retry in
   place: alone it would only kill healthy slow-queue turns faster.

   Model-progress silence while keepalives continue is a separate, opt-in
   semantic watchdog below."
  200000)

(def ASK_CODE_IDLE_TIMEOUT_MS
  "Default inter-chunk idle timeout for Vis `svar/ask-code!` calls (ms).

   Fires when the transport itself goes silent — not one byte, not even an
   SSE keepalive comment. 300s = CODEX PARITY with
   `DEFAULT_STREAM_IDLE_TIMEOUT_MS`. Model-progress silence while keepalives
   continue is different and does not time out by default; see
   `ASK_CODE_SEMANTIC_TIMEOUT_MS`.

   The FIRST byte has its own svar watchdog. Provider-scoped policy may widen
   that prefill window independently; this 300s default governs gaps after the
   first byte."
  300000)

(def ASK_CODE_SEMANTIC_TIMEOUT_MS
  "Default model/progress timeout for Vis `svar/ask-code!` streams (ms).

   240 seconds without text, reasoning, tool-input progress or a terminal event
   means the model is stalled even if SSE keepalives still prove transport
   liveness. This exceeds Anthropic's documented ~185-second worst observed
   extended-thinking silence while bounding a live-but-unproductive stream.

   Explicit nil disables it per call. OpenAI Responses sessions intentionally
   ignore this setting and retain their transport-specific idle policy."
  240000)

(def ^:private ask-code-runtime-network-defaults
  {:ttft-timeout-ms ASK_CODE_TTFT_TIMEOUT_MS
   :idle-timeout-ms ASK_CODE_IDLE_TIMEOUT_MS
   :semantic-timeout-ms ASK_CODE_SEMANTIC_TIMEOUT_MS})

(defn with-default-ask-code-idle-timeout
  "Merge streaming limits with precedence: explicit call opts, provider policy,
   Vis runtime defaults, then svar defaults for keys Vis leaves absent."
  ([opts] (with-default-ask-code-idle-timeout opts nil))
  ([opts provider-network] (merge ask-code-runtime-network-defaults (or provider-network {}) opts)))

(def AGENT_INITIATOR_HEADERS
  "`X-Initiator: agent` — the header GitHub Copilot bills by.

   Copilot charges a FULL premium interaction for every request it believes a
   HUMAN initiated, and a MISSING `X-Initiator` counts as `user`. svar infers
   the header from message roles, so any freshly built system+user prompt looks
   exactly like something a person typed. Vis' background traffic — auto-titles,
   extension helpers, one-shot `ask-code!`/`llm-text!` calls — is agent
   activity, and without this header a cosmetic session title costs the same
   premium interaction as the user's own turn. Only the foreground turn's FIRST
   iteration is genuinely user initiated (`loop/copilot-initiator-for-iteration`)."
  {"X-Initiator" "agent"})

(defn with-agent-initiator
  "Mark `opts` as agent-initiated Copilot traffic. Caller headers win, so a call
   site that really is answering a human can still pin `\"user\"` itself."
  [opts]
  (assoc opts :llm-headers (merge AGENT_INITIATOR_HEADERS (:llm-headers opts))))

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  ^long [candidate]
  (let [candidate (long candidate)]
    (min (long MAX_EVAL_TIMEOUT_MS) (max (long MIN_EVAL_TIMEOUT_MS) candidate))))

(def ^:private shell-timeout-eval-grace-ms
  "Extra room around a bounded call's OWN timeout so that call can kill, drain,
   and return its structured timeout envelope before the outer Python eval
   watchdog fires. The watchdog is a
   BACKSTOP, never a co-deadline."
  10000)

(def DEFAULT_SHELL_TIMEOUT_SECS
  "Default per-command / per-wait budget of the `shell` tool — declared here, and
   read by `foundation.shell`, so ONE number governs both the tool and the
   watchdog above it.

   Thirty minutes, the same as [[MAX_SHELL_TIMEOUT_SECS]]. The budget is a
   CEILING on the WAIT, never a delay: a fast command returns the instant it
   exits, so the only calls this number touches are the long ones — a native
   image, a cold Gradle / npm build, a full suite — and at two minutes those were
   killed precisely where their output started to matter, with the work already
   paid for.

   It matters here because it used to EQUAL the eval watchdog's own default: both
   were two minutes. A `shell` call that named no timeout therefore raced a
   watchdog that had already started, and the watchdog always won — the turn got a
   bare `Timeout` with NO output instead of shell's own envelope (partial stdout,
   `timed_out` true, a killed process tree). The widener below now floors the
   watchdog above this budget whenever an eval calls the shell at all."
  1800)

(def MAX_SHELL_TIMEOUT_SECS
  "Hard ceiling `foundation.shell` clamps EVERY `run` / `wait` budget to: thirty
   minutes. Declared here so the widener below can floor the eval watchdog above
   the LONGEST budget a shell call may legally own, and so
   [[MAX_EVAL_TIMEOUT_MS]] can stay a grace period above THIS.

   A budget that is a variable or an expression is invisible to a text scan, so
   flooring at anything under the cap let the watchdog preempt a legal wait with a
   bare `Timeout` instead of shell's own envelope — the same defect
   `DEFAULT_SHELL_TIMEOUT_SECS` describes, one spelling further out."
  1800)

(def RUN_TESTS_TIMEOUT_MS
  "The budget for ONE `run_tests` run, in every pack — ten minutes.

   Declared here because three places must agree on it: the Clojure pack hands it
   to the nREPL eval that runs the suite, the Python pack waits exactly this long
   on the project interpreter's pytest, and [[RUN_TESTS_FLOOR_SECS]] floors the
   eval watchdog above it. Two packs each holding their own literal is the drift
   this namespace exists to prevent.

   Overrunning it is reported as a STRUCTURED result — a wedged nREPL, a killed
   pytest process, the output that did arrive — so the number says when we stop
   believing the suite will finish, never how much work a suite may legitimately
   do. Five minutes did not cover a cold full-suite run (JVM start, namespace
   loading, compilation), and such a run died with nothing to show for it.

   Stays under [[MAX_EVAL_TIMEOUT_MS]] minus the widener's grace."
  (* 10 60 1000))

(def RUN_TESTS_FLOOR_SECS
  "Floor for an eval that calls `run_tests`: the run's OWN budget, in seconds.

   A test run answers its own timeout with a structured test result, and nothing
   preempts a direct tool call. Called from `python_execution` it must not die
   earlier at the generic eval watchdog and lose that result, so the widener
   floors the block's wall here and adds `shell-timeout-eval-grace-ms` on top."
  (quot (long RUN_TESTS_TIMEOUT_MS) 1000))

(def HTTP_CALL_FLOOR_SECS
  "Floor for an eval that reaches the NETWORK through the HTTP shims (`requests`,
   `httpx`, `urllib3`, `urlopen`).

   A request's budget is invisible to a text scan in the way that matters: the
   shim's own per-call default is 30s, the block usually loops over N hosts, and
   the helper doing the fetching is typically defined in an EARLIER block. So a
   perfectly ordinary crawl raced the watchdog — two minutes, then — and the
   watchdog won: a bare `Timeout` for work that was progressing normally, which is
   the defect `DEFAULT_SHELL_TIMEOUT_SECS` describes, spelled in sockets.

   Unlike shell's, this floor does NOT drop when the block spells a literal
   `timeout=`: that number bounds ONE request, never the loop around it."
  300)

(def ^:private timeout-secs-re
  #"[\"']?(?:timeout_secs|timeout)[\"']?\s*(?::|=)\s*([0-9]+(?:\.[0-9]+)?)")

(def ^:private timeout-ms-re #"[\"']?timeout_ms[\"']?\s*(?::|=)\s*([0-9]+(?:\.[0-9]+)?)")

;; `subprocess` is NOT here: it never spawns (the POSIX refusal shim raises at
;; once), so a block that mentions it buys no time. Only the `shell` tool waits.
(def ^:private shell-call-re #"\bshell\s*\(")

(def ^:private run-tests-call-re #"\brun_tests\s*\(")

(def ^:private http-call-re
  #"\b(?:requests|httpx|urllib3)\s*\.|\b(?:import|from)\s+(?:requests|httpx|urllib3)\b|\burlopen\s*\(")

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
  (let [code
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

   A `shell` call may legally own the FULL cap, so that is its floor even when the
   block DOES spell a literal budget somewhere: that literal bounds ONE call, and a
   second, unannotated call in the same block still owns shell's default — which is
   the cap.

   An HTTP call keeps its floor for the same reason: a literal `timeout=` bounds
   ONE request, and the block is almost always a LOOP of them."
  [code]
  (let [code (str code)]
    (max-of [(when (re-find run-tests-call-re code) RUN_TESTS_FLOOR_SECS)
             (when (re-find http-call-re code) HTTP_CALL_FLOOR_SECS)
             (when (re-find shell-call-re code) MAX_SHELL_TIMEOUT_SECS)])))

(defn eval-timeout-ms-for-code
  "Eval watchdog for ONE Python block: the configured base, raised so it sits a
   grace period ABOVE the longest bounded call the block makes."
  [base-timeout-ms code]
  (let [base
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
   the wall with a bare `Timeout` while the dialog is still up. Code
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

(def ^:dynamic *blocking-wall-hold*
  "Hold hook installed by the innermost enclosing timeout wall: `(fn [] release)`.

   [[*blocking-wall-park*]] for work that ENTERS and LEAVES on separate calls.
   A live view opens on one crossing of the Python bridge and closes on another
   — there is no thunk to wrap around the watching — so a run SHOWING its work
   takes a release token at `open` and spends it at `close`."
  nil)

(defn hold-blocking-wall!
  "LIFT every enclosing wall until the returned thunk is called, and answer that
   thunk.

   Not a bigger budget — no budget. The wall is the backstop for code that fell
   SILENT, and a run painting a picture a human is watching is the opposite of
   silent: it is billed nothing at all, for as long as it shows its work. The
   base budget comes back, measured from the release, when the last hold is
   spent.

   Releasing twice is a no-op, so a `finally` releasing what a close already
   released cannot collapse the clock under work that is still running. With no
   wall installed (a host-side call, tests) both halves are no-ops."
  []
  (if-let [hold *blocking-wall-hold*]
    (hold)
    (fn [])))
(defn guest-safepoint!
  "Poll the polyglot safepoint of the context this host call is running inside,
   and let whatever it raises propagate.

   The other half of [[park-blocking-wall]]: a wall stops the CLOCK, this makes
   the wait CANCELLABLE. Every bounded host loop a sandbox block can enter — the
   `sh.wait` poll above all — calls this once per iteration, exactly as
   `Context.safepoint`'s javadoc prescribes: \"Polyglot embeddings that rely on
   cancellation should call this method whenever a potentially long-running host
   operation is executed.\"

   Without it a cancel cannot reach a block parked in host code: `Context.interrupt`
   is documented to fail on a thread that \"uses non-interruptible waiting or
   executes non-interruptible host code\", so the interrupt times out
   and only the WAITER unwinds while the guest thread stays inside GraalPy. That
   abandoned thread then takes the GIL UNINTERRUPTIBLY on its way out
   (`PythonContext.ensureGilAfterFailure`) and can die owning it; a
   `ReentrantLock` whose owner is dead is never released, and every later turn of
   that session parks in `PythonContext.acquireGil` forever. Polling here keeps
   the interrupt NON-DESTRUCTIVE instead: the guest unwinds through its own
   frames, the GIL is released on the way out, and the SAME context serves the
   next turn.

   A no-op off a guest thread (a host-side call, tests): nothing is entered
   there, so there is no safepoint to poll."
  []
  (try (.safepoint (Context/getCurrent)) (catch IllegalStateException _ nil)))

(defn parkable-wall
  "One MOVABLE wall clock for a bounded execution that began at `start` with
   `timeout-ms` of budget.

   Returns `{:deadline <atom epoch-ms or nil> :park (fn [thunk]) :hold (fn [] release)}`,
   where a NIL deadline is NO wall at all (see [[await-wall]]).

   `park` is RE-ENTRANT: nested parks each LIFT the wall and only the OUTERMOST
   exit restores the base budget, so an inner park returning cannot collapse the
   clock while an outer park is still live. It also COMPOSES with the park
   inherited from [[*blocking-wall-park*]], so parking an inner wall parks every
   enclosing one too — a `human-input` call that asks the operator a question
   must not be killed by the Python eval watchdog wrapped around its block.

   `hold` is the same clock, taken and released on separate calls (see
   [[*blocking-wall-hold*]]), sharing park's depth so the two nest in either
   order."
  [start timeout-ms]
  (let [timeout-ms
        (long timeout-ms)

        deadline
        (atom (+ (long start) timeout-ms))

        depth
        (atom 0)

        inherited
        *blocking-wall-park*

        inherited-hold
        *blocking-wall-hold*

        enter!
        (fn []
          (swap! depth inc)
          (reset! deadline nil))

        leave!
        (fn []
          (reset! deadline (when-not (pos? (long (swap! depth dec))) (+ (util/now-ms) timeout-ms))))

        park
        (fn [thunk]
          (enter!)
          (try (thunk) (finally (leave!))))

        hold
        (fn []
          (enter!)
          (let [spent (atom false)]
            (fn []
              (when (compare-and-set! spent false true) (leave!)))))]

    {:deadline deadline
     :park (if inherited
             (fn [thunk]
               (park #(inherited thunk)))
             park)
     :hold (if inherited-hold
             (fn []
               (let [outer
                     (inherited-hold)

                     mine
                     (hold)]

                 (fn []
                   (mine)
                   (outer))))
             hold)}))

(def ^:private LIFTED_WALL_POLL_MS
  "How often [[await-wall]] re-reads a LIFTED wall's deadline atom. Nothing is
   billed while that atom is nil; this is only how fast the base budget lands
   again once the last park or hold is spent."
  250)

(defn await-wall
  "Wait for `fut` until the CURRENT value of `deadline` passes, re-reading the
   atom on every wake so a park that MOVED the wall extends the wait instead of
   expiring. Returns `timeout-value` once the wall is really reached.

   A NIL deadline is no wall: a human is being asked, or is watching a live
   view, and the run is billed nothing — the wait just continues, polling only
   so the restored budget lands the moment the last hold is released."
  [fut deadline timeout-value]
  (loop []

    (let [at
          (deref deadline)

          remaining
          (long (if at (- (long at) (util/now-ms)) LIFTED_WALL_POLL_MS))]

      (if (pos? remaining)
        (let [r (deref fut remaining timeout-value)]
          (if (identical? timeout-value r) (recur) r))
        timeout-value))))

(def ^:dynamic *rlm-context* "Dynamic context for RLM debug logging." nil)
