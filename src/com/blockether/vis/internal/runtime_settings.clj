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
  (let [requested
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
  "Default time-to-first-token timeout for Vis `svar/ask-code!` calls.
   60s avoids false positives from Codex queue/cold-start spikes while
   still bounding truly stuck pre-header connections. A separate retry
   handles the one-off watchdog interrupt case."
  (* 60 1000))

(def ASK_CODE_IDLE_TIMEOUT_MS
  "Default inter-chunk idle timeout for Vis `svar/ask-code!` calls.
   3min gives slow reasoning streams room while avoiding long hangs when
   provider stops sending bytes entirely."
  (* 3 60 1000))

(def ASK_CODE_SEMANTIC_TIMEOUT_MS
  "Default model/progress timeout for Vis `svar/ask-code!` streams (ms).

   Catches the failure mode `idle-timeout-ms` cannot: the transport
   keeps emitting bytes (SSE `: ping` comments, blank separators, or
   any framing-layer keepalive that returns from `.readLine`) which
   resets the idle watchdog forever, yet zero `response.*.delta` /
   `message.*` events ever arrive. Without this watchdog the iteration
   loop blocks on `.readLine` until the model finally streams output.

   185s (185000 ms) matches Anthropic's documented worst case for
   legitimate extended thinking on Opus 4.5
   (anthropics/claude-agent-sdk-typescript#44): a genuinely deep
   pre-token reasoning phase stays under the budget, while a
   stuck/keepalive-only provider still surfaces a real error in ~3min
   instead of stalling the turn indefinitely.

   Disable per call with `:semantic-timeout-ms nil`."
  185000)


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
  "Extra room around shell_run's own timeout so the shell tool can kill, drain,
   and return its timeout envelope before the outer Python eval watchdog fires."
  10000)

(defn explicit-shell-timeout-secs
  "Best-effort scan for explicit shell/subprocess timeout overrides in Python
   code. The real shell tool still owns validation/clamping; this only prevents
   the outer eval watchdog (default 120s) from preempting a longer requested
   shell timeout."
  [code]
  (let [nums (for [[_ n] (re-seq
                           #"[\"']?(?:timeout_secs|timeout)[\"']?\s*(?::|=)\s*([0-9]+(?:\.[0-9]+)?)"
                           (str code))]
               (long (Math/round (Double/parseDouble n))))]
    (when (seq nums) (apply max nums))))

(defn eval-timeout-ms-for-code
  [base-timeout-ms code]
  (let [base (long (clamp-eval-timeout-ms base-timeout-ms))]
    (if-let [shell-secs (explicit-shell-timeout-secs code)]
      (clamp-eval-timeout-ms
        (max base (+ (* 1000 (long shell-secs)) (long shell-timeout-eval-grace-ms))))
      base)))

(def ^:dynamic *rlm-context* "Dynamic context for RLM debug logging." nil)
