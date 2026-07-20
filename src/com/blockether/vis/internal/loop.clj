(ns com.blockether.vis.internal.loop
  (:refer-clojure)
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.anomaly.core :as anomaly]
            [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.svar.internal.util :as util]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.ctx-engine :as ctx-engine]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.attachment-storage :as attachment-storage]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.python-extensions :as python-extensions]
            [com.blockether.vis.internal.render :as render]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.session-model :as session-model]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.provider-error :as perr]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.strutil :refer [truncate]]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent CancellationException ExecutionException Executors ExecutorService
            Future]
           [org.graalvm.polyglot Context Value]))

(defonce ^:private ^ExecutorService gather-executor
  ;; Virtual-thread-per-task pool backing the sandbox `gather` builtin. GraalPy
  ;; allows concurrent access to a context AND releases its lock on blocking I/O,
  ;; so I/O-bound tool calls dispatched here genuinely overlap (maki-style async
  ;; in ONE run_python program) — without enabling sockets/asyncio.
  (Executors/newVirtualThreadPerTaskExecutor))

(defn- settle-gather-futures!
  "Settle every submitted gather future — `{:ok v}` or `{:err e}` per slot,
   in order. On success paths ALL thunks run to completion (a failing slot
   never aborts its siblings). But the moment the SETTLING thread itself is
   interrupted (turn `cancel!` / eval-timeout `.cancel(true)` on the worker
   future), every still-running CHILD future is hard-cancelled
   (`.cancel(true)`) and the `InterruptedException` propagates. Without the
   propagation+cancel, an interrupt during settle was swallowed as that
   slot's `:err` and the loop blocked on the NEXT `.get` — so a cancelled
   `gather(rg(...), rg(...))` left orphaned virtual threads grinding at
   100% CPU each until process exit."
  [futs]
  (let [cancel-all! (fn []
                      (doseq [^Future f futs]
                        (try (.cancel f true) (catch Throwable _ nil))))]
    (try (mapv (fn [^Future f]
                 (try {:ok (.get f)}
                      (catch ExecutionException e {:err (or (.getCause e) e)})
                      (catch InterruptedException e (throw e))
                      (catch Throwable e {:err e})))
               futs)
         (catch InterruptedException e (cancel-all!) (throw e)))))

;; =============================================================================
;; Single-iteration runner
;; =============================================================================

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Per-iteration `(def ...)` discovery / dependency tracking was retired
;; together with the `definition_*` sidecar tables and `restore-sandbox!`.
;; Python defs are intra-turn scratch; cross-turn evidence is read from
;; persisted `session_turn_iteration.forms` rows.
;; ---------------------------------------------------------------------------

(def ^:private MINI_STACK_DEPTH 12)

(defn- throwable-chain
  [^Throwable t]
  (vec (take-while some?
                   (iterate (fn [^Throwable x]
                              (.getCause x))
                            t))))

(defn- throwable-cause-summary
  [^Throwable t]
  (mapv (fn [^Throwable x]
          (cond-> {:class (.getName (class x)) :message (or (ex-message x) (str x))}
            (:type (ex-data x))
            (assoc :type (:type (ex-data x)))))
        (throwable-chain t)))

(defn- mini-stack-trace
  [^Throwable t]
  (when t
    (let [frames (take MINI_STACK_DEPTH (.getStackTrace t))]
      (str/join "\n"
                (map (fn [^StackTraceElement frame]
                       (str "  at " frame))
                     frames)))))

(defn- format-exception-short
  [^Throwable t]
  (let [ed (ex-data t)]
    (cond-> {:class (.getName (class t))
             :message (or (ex-message t) (str t))
             :causes (throwable-cause-summary t)
             :mini-trace (mini-stack-trace t)}
      (:type ed)
      (assoc :type (:type ed))

      (:status ed)
      (assoc :status (:status ed))

      (:cause-class ed)
      (assoc :cause-class (:cause-class ed)))))

(defn- format-exception
  [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t) {:data (ex-data t) :context context}))

(defn- interrupted-cause?
  [^Throwable t]
  (boolean (some (fn [^Throwable x]
                   (or (instance? InterruptedException x)
                       (instance? CancellationException x)
                       (= "java.lang.InterruptedException" (ex-message x))))
                 (throwable-chain t))))

(def ^:private PROVIDER_INTERRUPT_RETRIES 1)

(def ^:private PROVIDER_STREAM_REWIND_RETRIES 3)

(def ^:private CONSECUTIVE_PROVIDER_ERROR_LIMIT
  "Circuit breaker for the iteration loop: after this many CONSECUTIVE
   provider-generate failures (e.g. :svar.llm/empty-content that survived
   svar's in-call same-model re-sends) the turn fails fast as a provider error
   instead of burning the whole iteration budget re-sending the same request
   (session burned 15/15 iterations on identical empty-content failures).
   Any successful iteration or non-provider error resets the streak."
  3)

(def ^:private CONSECUTIVE_EMPTY_REPLY_LIMIT
  "Circuit breaker for EMPTY replies — a turn where the model returns a clean
   stop (Anthropic `end_turn` / OpenAI `stop`) with no text and no tool call.
   svar treats a clean-stop empty as a LEGITIMATE completion, not an error
   (`empty-reply-anomaly-type`), so these no longer surface as provider errors
   nor feed `CONSECUTIVE_PROVIDER_ERROR_LIMIT`. We still auto-continue (re-invoke)
   so a mid-task thinking-only blip recovers, but cap consecutive empties here so
   a model that keeps \"finishing\" with nothing finalizes on its best sticky
   answer instead of spinning the whole iteration budget. Any non-empty iteration
   resets the streak."
  3)

(defn- next-provider-error-streak
  "Next consecutive provider-generate failure count. Increments only when the
   iteration error is :llm-provider/generate (model produced no usable
   content); any other error kind resets to 0 - those are RLM-correctable."
  [prev-streak llm-provider-error]
  (if (= :llm-provider/generate (:phase llm-provider-error)) (inc (long (or prev-streak 0))) 0))

(defn- provider-error-breaker-tripped?
  "True when the consecutive provider-generate failure streak has reached
   CONSECUTIVE_PROVIDER_ERROR_LIMIT - the iteration loop fails the turn fast
   instead of re-sending the same doomed request."
  [streak]
  (>= (long streak) (long CONSECUTIVE_PROVIDER_ERROR_LIMIT)))

(def ^:private MAX_PROVIDER_UNAVAILABLE_RETRIES
  "Transparent retries for svar's terminal `:svar.llm/provider-unavailable`
   — the single (pinned) provider's upstream call failed before any usable
   response came back (a transient 5xx / dropped connection svar already
   gave up on). A momentary blip usually clears on a re-send, so re-send the
   SAME request a few times (widening backoff) before surfacing the error.
   3 retries = 4 total attempts; if it STILL fails the turn ends with the
   provider-error card (the user picks where to go next) rather than silently
   hopping providers or feeding an unactionable outage back to the model."
  3)

(def ^:private PROVIDER_UNAVAILABLE_RETRY_DELAYS_MS
  "Widening backoff (ms) before each provider-unavailable retry, indexed by
   attempt — same procedure as `PROVIDER_STREAM_REWIND_DELAYS_MS`. A momentary
   upstream outage (5xx / dropped connection) realistically takes a few seconds
   to clear, so the delay grows 1s → 2s → 4s across the retries rather than
   re-hitting the same blip on a fixed cadence."
  [1000 2000 4000])

(defn- provider-unavailable-error?
  "True when an exception is svar's terminal `:svar.llm/provider-unavailable`
   (a single-provider turn whose upstream call failed before any usable
   response). Retry-able ONCE, then terminal."
  [^Throwable e]
  (= :svar.llm/provider-unavailable (:type (ex-data e))))

(defn- provider-unavailable-retry?
  "True while a provider-unavailable exception still has PU retry budget left
   (`pu-attempt` below the cap). Gate for the transparent re-send."
  [^Throwable e pu-attempt]
  (and (provider-unavailable-error? e)
       (< (long pu-attempt) (long MAX_PROVIDER_UNAVAILABLE_RETRIES))))

(defn- provider-unavailable-retry-delay-ms
  "Widening backoff (ms) before the `pu-attempt`-th (0-based) provider-unavailable
   retry; clamps past the vector's end to its final value (4000)."
  [pu-attempt]
  (long (nth PROVIDER_UNAVAILABLE_RETRY_DELAYS_MS pu-attempt 4000)))

(defn- next-retry-counters
  "Pure counter-threading for the iteration retry loop's recur arms. Given the
   retry decision `result` and the current `:attempt`/`:max-tokens-attempt`/
   `:pu-attempt`, returns the next `[attempt max-tokens-attempt pu-attempt]` with
   ONLY the budget owned by that retry class advanced — the others stay flat so
   the policies never starve each other (provider-unavailable's 1s→2s→4s backoff
   always starts at pu-attempt=0 no matter how many stream/auth retries preceded
   it). Returns nil when `result` is not a retry signal (a real iteration result)."
  [result
   {:keys [attempt max-tokens-attempt pu-attempt]
    :or {attempt 0 max-tokens-attempt 0 pu-attempt 0}}]
  (let [attempt
        (long attempt)

        max-tokens-attempt
        (long max-tokens-attempt)

        pu-attempt
        (long pu-attempt)]

    (cond (= result ::retry-stream) [(inc attempt) max-tokens-attempt pu-attempt]
          (= result ::retry-provider-unavailable) [attempt max-tokens-attempt (inc pu-attempt)]
          (and (map? result) (contains? result ::retry-max-tokens))
          [attempt (inc max-tokens-attempt) pu-attempt]
          (= result ::retry-auth-refresh) [(inc attempt) max-tokens-attempt pu-attempt]
          (= result ::retry-auth-backoff) [(inc attempt) max-tokens-attempt pu-attempt])))

(def ^:private PROVIDER_STREAM_REWIND_DELAYS_MS [1000 2000 4000])

(defn- provider-call-cancelled?
  "True when the in-flight provider call was cancelled by the user.

   Reads TWO signals because on Esc they race:
     1. `:cancel-atom` - the cooperative flag `vis/cancel!` flips.
     2. the worker thread's own interrupt status - set synchronously when
        the SSE read is aborted / the worker future is interrupted.

   The interrupt (2) can land BEFORE the atom write (1) becomes visible, so
   reading the atom alone yields a false negative and a genuine user Esc gets
   misclassified as a retryable spurious blip (issue #13). A real cancel
   leaves the thread interrupted; a spurious TTFT-watchdog blip surfaces as a
   NAKED InterruptedException (interrupt flag clear), so consulting the thread
   status closes the race without suppressing the legitimate retry."
  [environment]
  (or (boolean (some-> environment
                       :cancel-atom
                       deref))
      (.isInterrupted (Thread/currentThread))))

(def ^:private INTERRUPT_RETRY_MAX_ELAPSED_MS
  "Ceiling on how long a provider call may have run and still have its interrupt
   treated as a retryable spurious blip. svar's TTFT watchdog (~60s) can surface
   a naked `InterruptedException` on a cold-start / queue spike — worth ONE retry.
   But an interrupt that lands well AFTER the TTFT budget is svar's idle/semantic
   watchdog firing on a genuinely wedged stream: the provider already got its full
   budget, so retrying just RESETS every stall clock and doubles the wall-clock
   hang (2×semantic ≈ 8min) before the failure finally surfaces — and it surfaces
   as a bare interrupt that reads downstream like an Esc cancel. Past this ceiling
   we do NOT retry; let the timeout propagate as a real error fast."
  (+ (long rt/ASK_CODE_TTFT_TIMEOUT_MS) 30000))

(defn- retryable-provider-interrupt?
  "True for provider-thread interrupts that were not caused by Vis user cancel
   AND arrived early enough to be a spurious blip rather than a stream-watchdog
   timeout. svar's TTFT watchdog currently can surface as a naked
   InterruptedException; retry once instead of letting router treat it like Esc
   cancellation — but only when the call had not already burned past the TTFT
   budget (see [[INTERRUPT_RETRY_MAX_ELAPSED_MS]]), so a genuinely stalled stream
   fails fast instead of being re-sent into another multi-minute wedge."
  [^Throwable t environment elapsed-ms]
  (and (interrupted-cause? t)
       (not (provider-call-cancelled? environment))
       (< (long elapsed-ms) (long INTERRUPT_RETRY_MAX_ELAPSED_MS))))

(defn- call-provider-with-interrupt-retry!
  [environment iteration-position f]
  (loop [attempt 0]
    (let [start-ns (System/nanoTime)
          outcome (try {:ok? true :value (f)} (catch Throwable t {:ok? false :throwable t}))]

      (if (:ok? outcome)
        (:value outcome)
        (let [t (:throwable outcome)
              elapsed-ms (long (/ (- (System/nanoTime) start-ns) 1000000))]

          (if (and (< (long attempt) (long PROVIDER_INTERRUPT_RETRIES))
                   (retryable-provider-interrupt? t environment elapsed-ms))
            (do
              ;; Router restores interrupt status before rethrowing. Clear it
              ;; before retry or next HTTP call can fail immediately.
              (Thread/interrupted)
              (tel/log! {:level :warn
                         :data (assoc (format-exception-short t)
                                 :iteration iteration-position
                                 :attempt (inc attempt)
                                 :elapsed-ms elapsed-ms
                                 :max-retries PROVIDER_INTERRUPT_RETRIES
                                 :cancelled? (provider-call-cancelled? environment))}
                        "Provider call interrupted without user cancel; retrying")
              (recur (inc attempt)))
            (throw t)))))))


(defn- stream-transport-error?
  [^Throwable t]
  (let [data
        (ex-data t)

        msg-lower
        (str/lower-case (or (ex-message t) ""))

        cause
        (ex-cause t)

        cause-lower
        (str/lower-case (or (some-> cause
                                    ex-message)
                            ""))]

    (and (= :svar.core/http-error (:type data))
         (:stream? data)
         (not (interrupted-cause? t))
         (or (str/includes? msg-lower "stream connection error")
             (str/includes? msg-lower "connection reset")
             (str/includes? msg-lower "connection closed")
             (str/includes? msg-lower "closed")
             (str/includes? msg-lower "eof")
             (str/includes? msg-lower "timed out")
             ;; the wrapper socket died before any response bytes arrived (e.g.
             ;; "HTTP/1.1 header parser received no bytes") — a transient blip, retry.
             (str/includes? msg-lower "received no bytes")
             (str/includes? msg-lower "header parser")
             (str/includes? msg-lower "no bytes")
             ;; transient TLS blip — the server tore down the connection mid-handshake
             ;; (javax.net.ssl.SSLHandshakeException "Remote host terminated the
             ;; handshake"); retry / fall back instead of failing the turn.
             (str/includes? msg-lower "handshake")
             (str/includes? cause-lower "connection reset")
             (str/includes? cause-lower "connection closed")
             (str/includes? cause-lower "closed")
             (str/includes? cause-lower "eof")
             (str/includes? cause-lower "timed out")
             (str/includes? cause-lower "received no bytes")
             (str/includes? cause-lower "header parser")
             (str/includes? cause-lower "no bytes")
             (str/includes? cause-lower "handshake")))))
(defn- provider-retry-event
  [{:keys [provider model attempt delay-ms error]}]
  (cond-> {:event/type :llm.routing/provider-retry
           :reason :stream-connection-error
           :provider provider
           :model model
           :attempt attempt
           :delay-ms delay-ms
           :error error}
    provider
    (assoc :from-provider provider)

    model
    (assoc :from-model model)))

(defn- prepend-routing-trace
  [result retry-events]
  (if (seq retry-events)
    (update result :routed/trace #(vec (concat retry-events (or % []))))
    result))

(defn- add-routing-trace-to-ex
  [^Throwable t retry-events]
  (if (seq retry-events)
    (ex-info (ex-message t)
             (update (or (ex-data t) {}) :routed/trace #(vec (concat retry-events (or % []))))
             t)
    t))

(defn- call-provider-with-stream-rewind-retry!
  "Retry provider-stream transport failures at Vis level. This wrapper owns
   UI rewind because svar cannot retract chunks it already delivered through
   on-chunk. Safe boundary: wraps only the provider call, before response parse
   and before any Python/tool eval."
  [environment {:keys [iteration-position provider model on-chunk reset-stream-state!]} f]
  (loop [attempt
         0

         retry-events
         []]

    (let [outcome (try {:ok? true :value (f)} (catch Throwable t {:ok? false :throwable t}))]
      (if (:ok? outcome)
        (prepend-routing-trace (:value outcome) retry-events)
        (let [t (:throwable outcome)
              can-retry? (and (< (long attempt) (long PROVIDER_STREAM_REWIND_RETRIES))
                              (not (provider-call-cancelled? environment))
                              ;; `stream-transport-error?` covers MID-stream drops (chunks
                              ;; delivered, needs the UI rewind); `perr/transport-throwable?`
                              ;; covers PRE-response connection failures (no bytes / reset /
                              ;; refused / DNS / TLS) that never started a stream — the safest,
                              ;; most idempotent retry, and the case the old `:stream?` gate
                              ;; silently dropped even while telling the user "just retry".
                              ;; `:svar.llm/empty-content` is deliberately NOT here: svar
                              ;; already re-sends an empty reply to the same model (bounded
                              ;; backoff) inside the call, so retrying it again here would
                              ;; stack ladders into a 16-send worst case.
                              (or (stream-transport-error? t) (perr/transport-throwable? t)))]

          (if can-retry?
            (let [delay-ms (long (nth PROVIDER_STREAM_REWIND_DELAYS_MS attempt 2000))
                  event (provider-retry-event {:provider provider
                                               :model model
                                               :attempt (inc attempt)
                                               :delay-ms delay-ms
                                               :error (ex-message t)})]

              (reset-stream-state!)
              (when on-chunk
                (on-chunk {:phase :provider-retry-reset
                           :iteration iteration-position
                           :attempt (inc attempt)
                           :max-retries PROVIDER_STREAM_REWIND_RETRIES
                           :delay-ms delay-ms
                           :error (format-exception-short t)
                           :event event}))
              (tel/log! {:level :warn
                         :data (assoc (format-exception-short t)
                                 :iteration iteration-position
                                 :attempt (inc attempt)
                                 :max-retries PROVIDER_STREAM_REWIND_RETRIES
                                 :delay-ms delay-ms
                                 :provider provider
                                 :model model
                                 :rewind? true)}
                        "Provider stream failed after live output; rewinding progress and retrying")
              (when (pos? delay-ms) (Thread/sleep delay-ms))
              (recur (inc attempt) (conj retry-events event)))
            (throw (add-routing-trace-to-ex t retry-events))))))))

;; ---------------------------------------------------------------------------

(defn- log-stage-level
  "Severity for loop-stage telemetry.

   Routine stage breadcrumbs are debug-only to keep ~/.vis/vis.log cheap. Actual
   failed turns and tool timeouts must survive the default :info file handler,
   otherwise the first post-mortem clue disappears exactly when the user needs it.
   User cancellation is an intentional stop, not an error."
  [stage data]
  (cond (and (= stage :error) (= :cancelled (:reason data))) :info
        (= stage :error) :error
        (and (= stage :code-result) (:timeout? data)) :error
        (and (= stage :turn/complete) (= :error (:status data))) :error
        (and (= stage :turn/complete) (= :cancelled (:status data))) :info
        :else :debug))

(defn log-stage!
  [stage iteration data]
  (tel/log! {:level (log-stage-level stage data)
             :id ::loop-stage
             :data (merge {:stage stage :iteration iteration} data)}))

(defn- elapsed-ms [started-ns] (/ (double (- (System/nanoTime) (long started-ns))) 1000000.0))

(defn normalize-reasoning-level [v] (svar/normalize-reasoning-level v))

(defn- github-copilot-claude-model?
  [resolved-model]
  (and (contains? #{:github-copilot-individual :github-copilot-business} (:provider resolved-model))
       (boolean (re-find #"(?i)claude" (str (:name resolved-model))))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋)[\s!.?,]*\s*$")

(defn- casual-user-request?
  [s]
  (let [text (some-> s
                     str
                     str/trim)]
    (boolean (and text (<= (count text) 80) (re-find casual-request-pattern text)))))

(defn- copilot-claude-safe-reasoning-level
  "Return the reasoning level Vis is willing to send to GitHub Copilot Claude.

   Copilot bills by interaction class, not just visible response text. Deep
   reasoning on Claude can burn multiple premium interactions for a trivial
   prompt. Default policy:
   - casual chat gets no reasoning parameter;
   - :deep is capped to :balanced unless the caller opts in with
     :allow-copilot-claude-deep? true;
   - non-Copilot/non-Claude models are untouched."
  [resolved-model user-request reasoning-level {:keys [allow-copilot-claude-deep?]}]
  (cond (not (github-copilot-claude-model? resolved-model)) reasoning-level
        (casual-user-request? user-request) nil
        (and (= :deep reasoning-level) (not allow-copilot-claude-deep?)) :balanced
        :else reasoning-level))

(defn- copilot-provider?
  [provider-id]
  (contains? #{:github-copilot :github-copilot-individual :github-copilot-business
               :github-copilot-enterprise}
             provider-id))

(defn- copilot-llm-headers
  [resolved-model initiator]
  (when (and (copilot-provider? (:provider resolved-model)) (#{"user" "agent"} initiator))
    {"X-Initiator" initiator}))

(defn- copilot-initiator-for-iteration
  [iteration]
  (if (zero? (long (or iteration 0))) "user" "agent"))

(defn needs-input-answer?
  "True for explicit clarification/needs-input answer payloads.

   Foundation exposes this through `(needs-input ...)`; the loop
   keeps the predicate data-shaped instead of depending on foundation
   namespaces so the core runtime has no extension cycle."
  [v]
  (and (map? v)
       (= :needs-input (:vis/answer-mode v))
       (string? (:answer/text v))
       (not (str/blank? (:answer/text v)))))

(defn markdown-answer?
  "True for the canonical final-answer VALUE: `{:answer string}`.
   The answer is the plain prose the model replies with; `answer-fn` wraps that
   string into this `{:answer string}` shape. The only other accepted value is
   the `needs-input-answer?` map."
  [v]
  (and (map? v) (string? (:answer v))))

(defn answer-markdown
  "Disposable text projection of a final answer. Canonical typed content remains
   structured; this projection exists only for legacy text-only model context and
   will not be transported as a second answer shape."
  [answer]
  (let [v (:result answer answer)]
    (cond (needs-input-answer? v) (:answer/text v)
          (markdown-answer? v) (:answer v)
          (and (vector? v) (every? content/block-valid? v))
          (not-empty (str/trim (content/text-projection v)))
          :else nil)))

(defn- turn-error-data
  "First canonical error block from a final answer, or nil."
  [answer]
  (let [v (:result answer answer)]
    (when (vector? v) (some #(when (= "error" (get % "type")) %) v))))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")
(def ^:private MARKDOWN_FENCE_RE #"^\s*`{3,}[A-Za-z0-9_-]*\s*$")

(defn- bare-string-code-block? [expr] (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- markdown-fence-line? [line] (boolean (re-matches MARKDOWN_FENCE_RE (str line))))

(defn- markdown-fence-block?
  [expr]
  (let [lines (->> (str/split-lines (str expr))
                   (map str/trim)
                   (remove str/blank?))]
    (boolean (and (seq lines) (every? markdown-fence-line? lines)))))

(defn- comment-only-block?
  [python-context ^String expr]
  (try (zero? (long (env/count-top-level-forms python-context (str/trim expr))))
       (catch Throwable _ false)))

(defn- literal-code-block-error
  [python-context expr]
  (cond
    (bare-string-code-block? expr)
    "Your run_python code is just a bare string literal. To ANSWER, reply with plain text and DON'T call run_python — never pass a quoted string as the program."
    (markdown-fence-block? expr)
    "A Markdown fence (` ```… `) leaked into your run_python code. Pass ONLY executable Python statements — no fence markers."
    (comment-only-block? python-context expr)
    "Your run_python code is only `#` comments with no executable statement. Add a statement to run, or reply with plain text instead of calling run_python."))

;; The engine is full-Python: a block's source is the program verbatim and
;; passes through to eval untouched — no parsing, unwrapping, or reformatting.

(defn- python-op-error
  "Map a throwable from the Python eval path to the op-error shape: a GraalPy
   PolyglotException goes through env/map-polyglot-error (proper
   :python/syntax|runtime|host phase + line/column); anything else falls back to
   extension/ex->op-error. Class checked by NAME so this ns never imports the
   GraalPy classes directly."
  [python-context e code]
  (try (if (= "org.graalvm.polyglot.PolyglotException" (.getName (class e)))
         (env/map-polyglot-error python-context e code)
         (extension/ex->op-error e {:form-source code}))
       (catch Throwable _ {:message (or (ex-message e) (.getName (class e)))})))

;; =============================================================================
;; ONE persistent interpreter per session. The GraalPy sandbox is created ONCE
;; (`create-environment`) and reused across every turn, so the model's globals
;; (defs, imports, variables) carry across calls and turns NATURALLY, REPL-style.
;; (Resuming a session in a FRESH process starts with an empty sandbox; durable
;; file edits and conversation history persist, so the model recomputes what it
;; needs.)
;; =============================================================================

(defn- run-python-code
  "Run an agent code block through the embedded GraalPy sandbox. Wraps the
   worker-future + cancellation + tool-event/render sinks + `*1`/`*e` recovery
   stack around `env/run-python-block` (whole-block; tools fire in order through
   their ProxyExecutable wrappers, which read the SAME dynamic sinks)."
  [python-context code & {:keys [tool-event-fn env]}]
  (let [thrown
        (atom nil)

        tool-counts
        (atom {})

        cancel-token
        (:cancel-token env)

        attachment-reader
        (let [d
              (:db-info env)

              sid
              (:session-id env)]

          (when (and d sid)
            {:list (fn []
                     (try (->> (persistance/db-list-session-attachments d sid)
                               (mapv (fn [a]
                                       (cond-> {:id (:id a)
                                                :source (:source a)
                                                :filename (:filename a)
                                                :media-type (:media-type a)
                                                :kind (:kind a)
                                                :size (:size a)
                                                :position (:position a)
                                                :tool-call-id (:tool-call-id a)}
                                         (= :tool (:source a))
                                         (assoc :iteration-id (:iteration-id a))

                                         (= :user (:source a))
                                         (assoc :turn-id (:turn-soul-id a))))))
                          (catch Throwable _ [])))
             :read (fn [id]
                     (attachment-storage/hydrate (persistance/db-read-attachment d id)))}))

        record-tool-event
        (fn [event]
          (let [op
                (:op event)

                n
                (get (swap! tool-counts update op (fnil inc 0)) op)

                event*
                (cond-> event
                  (not= n 1)
                  (assoc :id (str (name (or op :tool)) "-" n)))]

            (when tool-event-fn (tool-event-fn event*))))

        exec-future
        (cancellation/worker-future
          "vis-python-eval"
          (fn []
            (try
              (binding [extension/*tool-event-sink*
                        record-tool-event

                        mpl-capture/*attachment-reader*
                        attachment-reader]

                ;; One persistent interpreter per session: globals (defs,
                ;; imports, vars) carry across calls/turns NATURALLY.
                (assoc (env/run-python-block python-context code {:form-cap (:form-cap env)})
                  :lru {}))
              (catch Throwable e
                (reset! thrown e)
                {:result nil :lru {} :forms [] :error (python-op-error python-context e code)}))))

        dispose-cancel-hook
        (when cancel-token
          (cancellation/on-cancel! cancel-token
                                   (fn []
                                     (try (.cancel ^java.util.concurrent.Future exec-future true)
                                          (catch Throwable _ nil)))))

        timeout-ms
        (long (rt/eval-timeout-ms-for-code rt/*eval-timeout-ms* code))

        execution-result
        (try (deref exec-future timeout-ms nil)
             (catch Throwable e
               (reset! thrown e)
               (try (.cancel ^java.util.concurrent.Future exec-future true) (catch Throwable _ nil))
               {:result nil :lru {} :error (python-op-error python-context e code)})
             (finally (when dispose-cancel-hook
                        (try (dispose-cancel-hook) (catch Throwable _ nil)))))]

    (when env
      (cond (nil? execution-result) (env/push-eval-error! env
                                                          (or @thrown (ex-info "Eval timeout" {})))
            (nil? (:error execution-result)) (env/push-eval-result! env (:result execution-result))
            :else (env/push-eval-error!
                    env
                    (or @thrown
                        (ex-info (or (:message (:error execution-result)) "eval error") {})))))
    (if (nil? execution-result)
      (do (.cancel ^java.util.concurrent.Future exec-future true)
          {:result nil
           :lru {}
           :error {:message (str "Timeout (" (/ timeout-ms 1000) "s)")}
           :timeout? true})
      execution-result)))

(defn- run-with-timing
  [python-context code _sandbox-ns timeout-ms start-time tool-event-fn env]
  (let [run!
        (fn []
          (run-python-code python-context code :tool-event-fn tool-event-fn :env env))

        execution-result
        (if timeout-ms
          (binding [rt/*eval-timeout-ms* (rt/clamp-eval-timeout-ms timeout-ms)]
            (run!))
          (run!))

        finished-time
        (System/currentTimeMillis)

        execution-time
        (- (long finished-time) (long start-time))]

    (cond-> execution-result
      true
      (assoc :execution-started-at-ms
        start-time :execution-finished-at-ms
        finished-time :duration-ms
        execution-time)

      (:timeout? execution-result)
      (assoc :timeout? true)

      (not (:timeout? execution-result))
      (assoc :timeout? false))))

(defn- execute-code
  "Run a single :code block through the Python sandbox.

   Optional kwargs:
     :timeout-ms - hard-cap eval time, clamped at the
                   rt/*eval-timeout-ms* bounds.

   Every call performs a real Python eval. There is no result cache:
   forms with side effects MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [{:keys [python-context sandbox-ns] :as environment} code & {:keys [timeout-ms tool-event-fn]}]
  (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :execute-code})]
    ;; Per-block-eval contract: feed original block source to `run-python-code`;
    ;; it parses, repairs delimiter slips when safe, then evaluates parsed
    ;; forms. Guard validators run against the repaired source when one exists
    ;; so a stray close paren does not block repair before eval.
    ;; Re-bind the live Python `context` snapshot BEFORE every eval. Sandbox
    ;; bindings are installed once at session start, so a static value would go
    ;; stale by iter 2; refreshing here keeps `context` aligned with the visible
    ;; `<context>` block and reflects intra-iter changes across blocks.
    ;; The snapshot is immutable/read-only — see ctx-loop/session-snapshot for
    ;; the guarantee. Re-binding also erases any model-created shadow binding.
    (when-let [snap (ctx-loop/session-snapshot environment)]
      ;; the agent gets real dict ergonomics (.get / comprehensions / [k]).
      (env/bind-ctx! python-context (ctx-renderer/project-ctx snap)))
    (let [start-time (System/currentTimeMillis)
          exec (try
                 ;; The Python sandbox surfaces its own syntax/empty-block
                 ;; errors via env/run-python-block.
                 (run-with-timing python-context
                                  code
                                  sandbox-ns
                                  timeout-ms
                                  start-time
                                  tool-event-fn
                                  environment)
                 (catch Throwable e
                   (env/push-eval-error! environment e)
                   {:result nil
                    :lru {}
                    :error (try (extension/ex->op-error e {:form-source code})
                                (catch Throwable _
                                  {:message (or (ex-message e) (.getName (class e)))
                                   :type (-> e
                                             ex-data
                                             :type)}))
                    :execution-started-at-ms start-time
                    :execution-finished-at-ms (System/currentTimeMillis)
                    :duration-ms (- (System/currentTimeMillis) start-time)
                    :timeout? false}))]

      exec)))

;; ---------------------------------------------------------------------------
;; All-observation concurrent batch
;;
;; When ONE iteration emits ≥2 native tool calls that are ALL read-only
;; OBSERVATIONS (cat / rg / find_files / ls / struct_index / struct_occurrences / struct_node /
;; file_exists — anything the extension declares `:tag :observation`), and NONE
;; is python_execution, a native handler, or carries a preflight error, we run
;; the whole batch CONCURRENTLY through the isolated virtual-thread pool
;; (`__vis_par_isolated__`) instead of serially. I/O-bound reads/greps overlap;
;; the ORDERED result list is re-split so result[i] still pairs to the i-th
;; tool_use_id, exactly as the serial path does. A single mutation
;; (patch/write/…) or python_execution in the iteration → the WHOLE iteration
;; stays serial + ordered (mutations are NEVER reordered or parallelized).
;; ---------------------------------------------------------------------------

(defn- observation-entry?
  "True when a preflighted code-entry is a synthesized-Python OBSERVATION safe to
   run concurrently: it answers a tool_use (`:svar/tool-call-id`), it is NOT a
   native handler, it has NO preflight error, and its tool name resolves to
   `:tag :observation` in `tags-by-name` (the AUTHORITATIVE per-symbol
   classification — never a hardcoded name list). `python_execution` has no
   symbol/tag, so it never matches (it is excluded by construction)."
  [entry tags-by-name]
  (and (:svar/tool-call-id entry)
       (not (:vis/native-handler entry))
       (not (:vis/preflight-error entry))
       (= :observation (get tags-by-name (:vis/tool-name entry)))))

(defn- observation-batch?
  "True when EVERY entry in `code-entries` is a concurrent-safe observation and
   there are at least two of them. Any doubt (a mutation, python_execution, a
   native handler, a preflight error, a missing/blank source) → false → the
   iteration runs serially. Conservative by design."
  [code-entries tags-by-name]
  (let [entries (vec code-entries)]
    (and (>= (count entries) 2)
         (every? (fn [e]
                   (and (not (str/blank? (str (:expr e)))) (observation-entry? e tags-by-name)))
                 entries))))

(defn- observation-batch-program
  "Synthesize the ONE Python program that runs `exprs` (each a bare observation
   call like `cat(\"x\", {…})` — a DEFERRED `__vis_Call__`) concurrently and
   returns an ORDERED list of per-call sentinels. `__vis_par_isolated__` settles
   each deferred call on the pool and isolates failures per slot. The synthesized
   names are `__vis_*` (baseline/protected), so they never collide with model
   globals; the block is a host-owned batch, never model-authored code."
  [exprs]
  (str
    "__vis_obs_batch__ = [" (str/join ", " exprs)
    "]\n"
    "__vis_par_isolated__([(lambda __x__=__x__: __vis_settle__(__x__)) for __x__ in __vis_obs_batch__])"))

(defn- sentinel-get
  "Read a `__vis_*` sentinel key from a batch slot. The per-call sentinels are
   built in `par-isolated-fn` as Clojure maps `{\"__vis_ok__\" true \"__vis_val__\" …}`
   and the boundary is STRINGS-ONLY, so a round-tripped slot is always a
   string-keyed map — one plain string lookup, no keyword fallback."
  [slot k]
  (get slot (name k)))

(defn- execute-observation-batch
  "Run an all-observation iteration's calls CONCURRENTLY via one `execute-code`
   of a `__vis_par_isolated__` batch, then RE-SPLIT the ordered sentinel list so
   each entry gets its OWN result envelope — the SAME shape `execute-code` yields
   for a single call, so everything downstream (render cards, tool_result pairing
   by id, DB persist, streaming) is unchanged.

   Returns a vector aligned 1:1 with `entries`: for slot i,
     {:result <tool-value>  :duration-ms d :timeout? false …}   on success, OR
     {:result nil :error <op-error> :duration-ms d :timeout? false …} on failure.
   The batch's wall time is stamped on every slot (they ran together); a per-slot
   duration is not separable and not needed for correctness.

   If the batch itself fails to run (a syntax/host error before any slot settles,
   or the sentinel list is malformed), returns nil so the caller falls back to
   the proven SERIAL path — the feature never makes a batchable iteration worse
   than serial."
  [environment entries]
  (let [exprs
        (mapv #(str (:expr %)) entries)

        program
        (observation-batch-program exprs)

        start
        (System/currentTimeMillis)

        exec
        (execute-code environment program)

        finish
        (System/currentTimeMillis)

        dur
        (- finish start)

        slots
        (:result exec)]

    ;; Guard: the batch must have run and returned exactly one sentinel per entry.
    ;; Any deviation (batch-level :error, wrong arity, non-map slot) → nil → the
    ;; caller runs the iteration serially instead. Never silently drop a call.
    (if (and (nil? (:error exec))
             (sequential? slots)
             (= (count slots) (count entries))
             (every? map? slots))
      (mapv (fn [slot expr]
              (let [base {:execution-started-at-ms start
                          :execution-finished-at-ms finish
                          :duration-ms dur
                          :timeout? false
                          :lru {}}]
                (if (sentinel-get slot :__vis_ok__)
                  (assoc base :result (sentinel-get slot :__vis_val__))
                  (let [exc (sentinel-get slot :__vis_exc__)]
                    (assoc base
                      :result nil
                      :error (if (instance? Throwable exc)
                               (python-op-error (:python-context environment) exc expr)
                               {:message (str (or exc "observation failed"))}))))))
            slots
            exprs)
      nil)))

(defn- run-native-handler
  "Execute a native-tool `:handler` directly in Clojure under a cancellable
   wall-clock deadline. Native handlers bypass the Python eval watchdog, so this
   boundary prevents a wedged REPL/socket/process from holding a turn forever.

   Slow SYNCHRONOUS setup runs OUTSIDE the wall: the handler's env carries
   `:vis/outside-tool-wall`, a `(fn [thunk])` that PARKS the deadline while the
   thunk runs (wedge-guarded by MAX_EVAL_TIMEOUT_MS) and RESTARTS the clock when
   it returns — so a cold project-REPL boot never bills against the eval's
   `timeout_ms` (see extension/run-outside-tool-wall)."
  [handler environment input display-src]
  (let [start
        (System/currentTimeMillis)

        timeout-ms
        (long (rt/native-tool-timeout-ms input))

        deadline
        (atom (+ start timeout-ms))

        park-depth
        (atom 0)

        outside-wall
        (fn [thunk]
          ;; RE-ENTRANT park. Native handlers nest this: run_tests wraps the
          ;; WHOLE run (language-surface) AND clj-test-fn separately wraps its
          ;; ensure-repl-for-dir! boot. A non-reentrant park let the INNER exit
          ;; collapse the clock back to the base `timeout-ms` (30s) while the
          ;; OUTER park was still live, so the actual test eval that ran after
          ;; the boot got only 30s — a slow first-load / wedged eval then died
          ;; at exactly 30000ms even though the pack budget is 290s. Fix: only
          ;; the OUTERMOST park restores the base wall; a nested exit re-parks to
          ;; MAX so the enclosing park (and its own budget) survives.
          (swap! park-depth inc)
          (reset! deadline (+ (System/currentTimeMillis) (long rt/MAX_EVAL_TIMEOUT_MS)))
          (try (thunk)
               (finally (reset! deadline (+ (System/currentTimeMillis)
                                            (if (pos? (long (swap! park-depth dec)))
                                              (long rt/MAX_EVAL_TIMEOUT_MS)
                                              timeout-ms))))))

        worker
        (cancellation/worker-future
          "vis-native-tool"
          #(handler (assoc environment :vis/outside-tool-wall outside-wall) input))

        dispose-cancel-hook
        (when-let [cancel-token (:cancel-token environment)]
          (cancellation/on-cancel! cancel-token
                                   #(try (.cancel ^java.util.concurrent.Future worker true)
                                         (catch Throwable _ nil))))

        done
        (fn [m]
          (merge {:lru {}
                  :timeout? false
                  :execution-started-at-ms start
                  :execution-finished-at-ms (System/currentTimeMillis)
                  :duration-ms (- (System/currentTimeMillis) start)}
                 m))]

    (try (let [timeout-sentinel
               (Object.)

               ;; The deadline is a MOVABLE atom (outside-wall parks/restarts it),
               ;; so wait in a loop: a sentinel wake re-checks the CURRENT deadline
               ;; and keeps waiting when the wall moved while we slept.
               ret
               (loop []

                 (let [remaining (- (long @deadline) (System/currentTimeMillis))]
                   (if (pos? remaining)
                     (let [r (deref worker remaining timeout-sentinel)]
                       (if (identical? timeout-sentinel r) (recur) r))
                     timeout-sentinel)))]

           (if (identical? timeout-sentinel ret)
             (do (.cancel ^java.util.concurrent.Future worker true)
                 (done {:result nil
                        :timeout? true
                        :error
                        {:message
                         (str "Native tool "
                              display-src
                              " timed out after "
                              timeout-ms
                              "ms; retry with an explicit timeout_ms or use a background workflow")
                         :type :vis/native-tool-timeout
                         :data {:tool display-src :timeout-ms timeout-ms}}}))
             (done (if (and (map? ret) (or (contains? ret :result) (contains? ret :error)))
                     ret
                     {:result ret}))))
         (catch Throwable e
           (env/push-eval-error! environment e)
           (done {:result nil
                  :error (try (extension/ex->op-error e {:form-source display-src})
                              (catch Throwable _
                                {:message (or (ex-message e) (.getName (class e)))
                                 :type (-> e
                                           ex-data
                                           :type)}))}))
         (finally (when dispose-cancel-hook (try (dispose-cancel-hook) (catch Throwable _ nil)))))))

;; Print-cap defaults for `fmt/bounded-value-str` - chosen so a wide flat
;; collection or a deep nested map still pr-strs without materializing
;; an unbounded JVM string before truncation. Override per call site
;; when a tighter or looser bound is required.

;; ---------------------------------------------------------------------------
;; Error normalization
;; ---------------------------------------------------------------------------

(defn- op-error
  "Coerce engine/model error values into the canonical structured :error map.

   Iteration blocks require `:error` to be nil or a map. Preflight gates and
   answer validators naturally produce strings; wrap them before persistence so
   a useful model-facing error does not become `:vis/invalid-iteration-block`."
  ([err] (op-error err nil))
  ([err {:keys [code phase]}]
   (cond (nil? err) nil
         (map? err) err
         (instance? Throwable err) (try (extension/ex->op-error err
                                                                (cond-> {}
                                                                  code
                                                                  (assoc :form-source code)))
                                        (catch Throwable _
                                          {:message (or (ex-message err) (.getName (class err)))}))
         :else (cond-> {:message (str err)}
                 code
                 (assoc :block {:source code :phase (or phase :preflight)})))))

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  ;; These are provider/runtime failures, not model strategy failures.
  ;; svar already performs its own transport retry/fallback policy before
  ;; surfacing them to Vis, so feeding them back into the RLM only burns
  ;; visible iterations and cannot help the model self-correct.
  #{:svar.core/http-error :svar.llm/all-providers-exhausted :svar.llm/circuit-open
    :svar.llm/provider-exhausted :svar.llm/provider-unavailable})

(defn- infrastructure-error?
  [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(def ^:private CONTEXT_OVERFLOW_HOPELESS_FACTOR
  "A preflight `:svar.tokens/context-overflow` whose measured input exceeds
   the call's max-input budget by this factor is unrecoverable INSIDE the
   turn: the overflow fires before the provider call, so the fed-back error
   never reaches the model (the next call dies in the same preflight), and
   appending the error only GROWS the input. Feeding it anyway produces a
   runaway iteration loop — observed live as VIS-9: claude-fable-5 on a
   stale svar catalog resolved an 8192 fallback limit, an ~81k base prompt
   (10x over) re-failed every ~1s for 376+ iterations until cancelled.

   BELOW the factor a marginal overflow stays on the feed path on purpose:
   trailer folding / summarize can legitimately shrink the next iteration,
   and that recovery path must keep working."
  1.5)

(defn- hopeless-context-overflow?
  "True when ex-data is a preflight context overflow too large for any
   realistic same-model compaction pass to rescue."
  [ex-data-map]
  (let [input
        (:input-tokens ex-data-map)

        max-input
        (:max-input-tokens ex-data-map)]

    (and (number? input)
         (number? max-input)
         (pos? (long max-input))
         (>= (double input) (* (double CONTEXT_OVERFLOW_HOPELESS_FACTOR) (double max-input))))))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview
  [messages]
  (when-let [c (some (fn [m]
                       (when (= (:role m) "user") (:content m)))
                     (reverse messages))]
    (let [s (if (sequential? c)
              ;; Multimodal content: preview the text blocks only — stringifying
              ;; the vector would dump base64 image payloads into error logs.
              (str/join " " (keep #(when (= "text" (:type %)) (:text %)) c))
              (str c))
          n (long (count s))]

      (if (> n (long LAST_USER_PREVIEW_CHARS))
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
             " ...<+"
             (- n (long LAST_USER_PREVIEW_CHARS))
             " chars>")
        s))))

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the turn row.
   Delegates to the unified `format-exception` and adds iteration context."
  [^Throwable e ctx]
  (format-exception e
                    {:context {:iteration (:iteration ctx)
                               :messages-count (count (:messages ctx))
                               :routing (:routing ctx)
                               :reasoning-level (:reasoning-level ctx)
                               :last-user-preview (last-user-message-preview (:messages ctx))}}))

(defn handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.
   Infrastructure failures are terminal for the turn; model/format/code
   failures still return `{::iteration-error ...}` for RLM self-correction."
  [^Throwable e ctx]
  (let [ex-data-map
        (ex-data e)

        iteration
        (:iteration ctx)

        hopeless-overflow?
        (hopeless-context-overflow? ex-data-map)

        fatal?
        (or (infrastructure-error? ex-data-map) hopeless-overflow?)

        iteration-error-data
        (exception->iteration-error-data e ctx)]

    (tel/log!
      {:level (if fatal? :error :warn)
       :data (let [base
                   (assoc (format-exception-short e) :iteration iteration)

                   ed
                   (ex-data e)

                   body
                   (some-> (:body ed)
                           str)]

               (cond-> base
                 (:status ed)
                 (assoc :status (:status ed))

                 (:request-id ed)
                 (assoc :request-id (:request-id ed))

                 (:request_id ed)
                 (assoc :request-id (:request_id ed))

                 (and body (not (str/blank? body)))
                 (assoc :body-snippet (truncate body 1000))))}
      (cond
        hopeless-overflow?
        "Hopeless preflight context overflow - failing turn (feeding it back can never reach the model and only grows the input; VIS-9)"
        fatal? "Provider infrastructure error - failing turn without RLM restarts"
        :else "RLM iteration failed, feeding error to LLM"))
    (cond-> {::iteration-error iteration-error-data}
      fatal?
      (assoc ::fatal-iteration-error true))))

;; ---------------------------------------------------------------------------
;; get-locals (read sandbox vars)
;; ---------------------------------------------------------------------------

(defn get-locals
  "User-defined sandbox vars surface. Live-vars introspection is cosmetic-off
   for the Python engine (the agent uses its own Python scope + stdlib), so this
   returns an empty map. Kept as a stable seam for the trailer/renderer callers."
  [_environment]
  {})

(defn- def-display-result
  "Pass-through seam for future display tweaks. Silent system-call
   elision now happens explicitly on progress chunks (`:silent?`) and
   via the `:vis/silent` return sentinel for quiet host effects; normal value-bearing forms remain visible."
  [_environment _code result]
  result)

;; ---------------------------------------------------------------------------
;; Parsed form helpers
;; ---------------------------------------------------------------------------

;; Replay-dedup keys hash via `extension/sha256-hex` — the ONE
;; string-digest helper.

(defn- ask-code-block-observation
  "Block count for logs/chunks — only the count is informative."
  [ask-result]
  {:form-count (count (or (:blocks ask-result) []))})

;; `normalized-code-source` removed: `code-entries-preflight` now computes
;; the same join inline on the surviving block sources (was only ever called
;; from the splitter's old preflight path).

;; `bare-symbol-entry?` removed with `plain-prose-code-error` — the
;; per-block-eval cut routes prose into the Python engine as a parse /
;; name error instead of detecting "every entry is a bare symbol" upfront.

(defn- code-entries-preflight
  "Per-block-eval preflight. One code block becomes one code-entry; the
   block's `:source` is the entry's `:expr` verbatim. The Python engine runs
   each entry as one whole-block coroutine during execution.

   Gate retained:
     - Duplicate-block dedup. Some providers stutter and emit the same
       block twice; we keep the first copy and drop the rest."
  [_iteration-position blocks]
  (let
    [blocks
     (vec (or blocks []))

     ;; Dedupe duplicate (stuttered) blocks. A native handler-tool block is
     ;; CODE-LESS (no :source) — keep it (don't blank-filter) and dedup it by
     ;; tool-call-id, since several distinct handler calls share a blank source.
     ;; Normal Python blocks still dedup by source.
     block-key
     (fn [b]
       (if (:vis/native-handler b) [::native (:svar/tool-call-id b)] (:source b)))

     unique-blocks
     (->> blocks
          (remove #(and (str/blank? (:source %)) (not (:vis/native-handler %))))
          (reduce (fn [{:keys [seen acc]} b]
                    (let [k (block-key b)]
                      (if (contains? seen k)
                        {:seen seen :acc acc}
                        {:seen (conj seen k) :acc (conj acc b)})))
                  {:seen #{} :acc []})
          :acc)

     duplicate-blocks-normalized?
     (< (count unique-blocks) (count blocks))

     ;; Each block becomes one code-entry. The entry carries:
     ;;   :expr             — verbatim block source (fed to the engine as-is)
     ;;   :block-lang       — svar's stamped engine lang ("python")
     ;;   :render-segments  — structural split for channel rendering (see
     ;;                       `render/parse-block-display`)
     raw-entries
     (mapv (fn [b]
             (let [src
                   (:source b)

                   segments
                   (when src (render/parse-block-display src))]

               (cond-> {:expr src :block-lang (:lang b) :render-segments segments}
                 ;; Carry the originating tool-call identity onto the
                 ;; entry so it survives into the executed form / envelope
                 ;; and `iteration-results-message` can pair EACH tool_use
                 ;; with its OWN tool_result.
                 (:svar/tool-call-id b)
                 (assoc :svar/tool-call-id (:svar/tool-call-id b))

                 (:vis/tool-name b)
                 (assoc :vis/tool-name (:vis/tool-name b))

                 ;; native handler-tool dispatch carries through to execution
                 (:vis/native-handler b)
                 (assoc :vis/native-handler (:vis/native-handler b))

                 (contains? b :vis/native-input)
                 (assoc :vis/native-input (:vis/native-input b)))))
           unique-blocks)

     raw-fence-error
     (some :vis/preflight-error raw-entries)

     parsed-total-blocks
     (count raw-entries)

     empty-code-error
     (when (zero? parsed-total-blocks)
       "Your reply was empty — no tool call and no answer. To ACT, make a tool call (e.g. python_execution); to FINISH, reply with plain prose (your answer).")

     ;; Normalized concat of all surviving block sources — also the
     ;; identity used for iteration-hash dedup in the trailer.
     normalized-code
     (->> raw-entries
          (remove :vis/preflight-error)
          (keep :expr) ;; code-less handler entries don't contribute
          (map str/trim)
          (remove str/blank?)
          (str/join "\n\n"))

     code-hash
     (when-not (str/blank? normalized-code) (extension/sha256-hex normalized-code))

     any-entry-error?
     (boolean (some :vis/preflight-error raw-entries))

     ;; NATIVE model: each tool_use → one block → one entry carrying its
     ;; `:svar/tool-call-id`. Merging would conflate distinct tool-calls into
     ;; one entry, so one tool_use would silently lose its result. Only the
     ;; no-tool-call path (a provider splitting ONE program into several
     ;; blocks) merges the survivors into a SINGLE code-entry =
     ;; `normalized-code` — gate the merge on every entry lacking a call id.
     merged-entries
     (if (and (> (count raw-entries) 1)
              (not any-entry-error?)
              (not (str/blank? normalized-code))
              (every? (complement :svar/tool-call-id) raw-entries))
       (let [segs (render/parse-block-display normalized-code)]
         [{:expr normalized-code
           :block-lang (:block-lang (first raw-entries))
           :render-segments segs}])
       raw-entries)]

    {:code-entries
     (if empty-code-error [{:expr "" :vis/preflight-error empty-code-error}] merged-entries)
     :empty-code-preflight-error empty-code-error
     :raw-fence-preflight-error raw-fence-error
     :duplicate-blocks-normalized? duplicate-blocks-normalized?
     :normalized-code normalized-code
     :code-hash code-hash
     :original-total-blocks parsed-total-blocks}))

(defn- answer-validation-rejection-message
  [{:keys [id]} hit]
  (let [message
        (some-> (:message hit)
                str
                str/trim
                not-empty)

        hint
        (some-> (:hint hit)
                str
                str/trim
                not-empty)]

    (str "Answer validation hook "
         id
         " rejected the final answer."
         (when message (str " " message))
         (when hint (str " Recovery: " hint)))))

(defn- answer-validation-hook-error-message
  [ext id ^Throwable t]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-threw
             :data
             {:ext (:ext/name ext) :hook id :phase :turn.answer/validate :error (ex-message t)}})
  nil)

(defn- answer-validation-invalid-return-message
  [ext id hit]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-invalid-return
             :data {:ext (:ext/name ext)
                    :hook id
                    :phase :turn.answer/validate
                    :returned hit
                    :explain (s/explain-data ::extension/answer-validation-reject hit)}})
  nil)

(defn- answer-validation-extensions
  [environment active-extensions]
  (or (seq active-extensions)
      (some-> (:extensions environment)
              deref
              seq)))

(defn final-answer-gate-error
  "Dispatch `:turn.answer/validate` extension hooks against the
   candidate answer. Returns nil when every hook accepts,
   otherwise a single string surfaced as the rejected answer's
   validation error.

   A final answer is plain prose with no tool calls, so it inherently
   never shares an iteration with extension/tool calls: the model uses
   one iteration to observe tool output, then a later iteration replies
   with the answer. Extensions that need an additional veto (e.g.
   user-facing safety / format gates) still get their
   `:turn.answer/validate` hook fired here.

   `active-extensions` is passed by the turn loop so activation is
   computed once per turn; direct callers may omit it and provide
   `:extensions` on the environment."
  ([environment iteration blocks] (final-answer-gate-error environment iteration blocks nil nil))
  ([environment iteration blocks answer-value]
   (final-answer-gate-error environment iteration blocks answer-value nil))
  ([environment iteration blocks answer-value active-extensions]
   (final-answer-gate-error environment iteration blocks answer-value active-extensions nil))
  ([environment iteration blocks answer-value active-extensions extra-ctx]
   (let [ctx (merge {:environment environment
                     :phase :turn.answer/validate
                     :iteration iteration
                     :blocks blocks
                     :answer answer-value}
                    extra-ctx)]
     ;; Extension `:turn.answer/validate` vetoes only. An answer reply is plain
     ;; prose with no tool calls, so it carries no tool ops to gate.
     (some (fn [ext]
             (some (fn [{:keys [id phase] hook-fn :fn :as hook}]
                     (when (= :turn.answer/validate phase)
                       (binding [extension/*current-extension* ext
                                 extension/*current-symbol* nil]

                         (try (let [hit (hook-fn ctx)]
                                (cond (s/valid? ::extension/answer-validation-reject hit)
                                      (answer-validation-rejection-message hook hit)
                                      (and (map? hit) (:reject hit))
                                      (answer-validation-invalid-return-message ext id hit)))
                              (catch Throwable t
                                (answer-validation-hook-error-message ext id t))))))
                   (or (:ext/hooks ext) [])))
           (answer-validation-extensions environment active-extensions)))))

(defn- finalize-answer!
  "Finalize the turn from a prose ANSWER reply (`s` = the markdown). Classifies
   the value, runs `ctx-loop/finalize-turn!` (the real turn/context finalization),
   and sets turn-state `:answer` so run-iteration's FINAL path stores + renders it.
   Reads the per-turn atoms off `environment` — the answer is the answer; we
   just record it and finalize."
  [environment s]
  (let [turn-state-atom
        (:turn-state-atom environment)

        value
        (cond (needs-input-answer? s) s
              (markdown-answer? s) s
              (string? s) {:answer s}
              (nil? s) {:answer ""}
              :else {:answer (pr-str s) :vis/coerced? true})

        answer-text
        (cond (and (map? value) (string? (:answer value))) (:answer value)
              (and (map? value) (string? (:answer/text value))) (:answer/text value)
              (string? value) value
              :else nil)

        turn-summary
        (when (map? value) (:turn-summary value))

        user-request
        (some-> turn-state-atom
                deref
                :user-request)

        current-title
        (some-> (:session-title-atom environment)
                deref
                str
                str/trim
                not-empty)]

    (ctx-loop/finalize-turn! {:ctx-atom (:ctx-atom environment) :turn-state-atom turn-state-atom}
                             {:answer answer-text
                              :turn-summary turn-summary
                              :user-request user-request
                              :session-title current-title})
    ;; :position nil — an answer reply has no python form to attach to.
    (swap! turn-state-atom assoc :answer {:value value :position nil})
    (when-not (str/blank? (str answer-text))
      (swap! turn-state-atom assoc :best-answer {:value value :answer-markdown answer-text}))
    value))

(defn- iteration-start-hook-hit
  "Normalize the value returned by a `:turn.iteration/start` hook.

   Iteration-start hooks are currently advisory only; this compatibility path
   still validates/normalizes legacy hook maps for extensions that return them,
   but no model-facing context tasks are emitted."
  [ext id lifetime hit]
  (cond
    (nil? hit) nil
    (not (map? hit))
    (do
      (tel/log!
        {:level :warn
         :id ::iteration-start-hook-invalid-return
         :data {:ext (:ext/name ext) :hook id :returned hit}}
        "Extension :turn.iteration/start hook returned non-map value; expected nil or hook-task map")
      nil)
    :else (let [title
                (:title hit)

                emit
                (when (map? (:emit hit)) (:emit hit))

                hook-task?
                (and (string? title) (not (str/blank? title)))]

            (cond
              ;; Pure-emit hook: no hook-task body, only :emit payload.
              (and emit (not hook-task?)) {:id id :task nil :emit emit}
              (not hook-task?)
              (do (tel/log!
                    {:level :warn
                     :id ::iteration-start-hook-missing-title
                     :data {:ext (:ext/name ext) :hook id :returned hit}}
                    "Hook returned map without non-blank :title (and no :emit payload); dropping")
                  nil)
              :else (cond-> {:id id
                             :task (cond-> {:title title :status :todo :source :hook :hook-id id}
                                     (:importance hit)
                                     (assoc :importance (:importance hit))

                                     lifetime
                                     (assoc :lifetime lifetime))}
                      emit
                      (assoc :emit emit))))))

(defn- iteration-start-hook-error-hit
  [ext id t]
  (tel/log! {:level :warn
             :id ::iteration-start-hook-threw
             :data {:ext (:ext/name ext) :hook id :error (ex-message t)}}
            "Extension :turn.iteration/start hook threw")
  nil)

(defn- collect-iteration-start-hints
  "Run active `:turn.iteration/start` hooks. Legacy hook-task output is ignored;
   this currently returns an empty vector after preserving hook validation/logging."
  [_environment active-extensions ctx]
  (vec (mapcat (fn [ext]
                 (keep (fn [{:keys [id phase lifetime] hook-fn :fn}]
                         (when (= :turn.iteration/start phase)
                           (binding [extension/*current-extension*
                                     ext

                                     extension/*current-symbol*
                                     nil]

                             (try (iteration-start-hook-hit ext id lifetime (hook-fn ctx))
                                  (catch Throwable t (iteration-start-hook-error-hit ext id t))))))
                       (or (:ext/hooks ext) [])))
               active-extensions)))

(defn- session-turn-position
  [environment session-turn-id]
  (or (try (when-let [session-id (:session-id environment)]
             (some (fn [turn]
                     (when (= (str (:id turn)) (str session-turn-id)) (:position turn)))
                   (persistance/db-list-session-turns (:db-info environment) session-id)))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::session-turn-position-failed
                        :data {:session-id (:session-id environment)
                               :session-turn-id session-turn-id
                               :error (ex-message t)}}
                       "Could not resolve session turn position for iteration hooks")
             nil))
      1))

;; `iter-of-scope` (form-scope → iteration-scope) is loop-local and forward-
;; declared so the resume-context path (above it in the file) can normalize form
;; scopes. The selector RESOLVER (`scope-key` / `turn-key` / `expand-through` /
;; `supersede-summaries`) lives in `ctx-engine` so the wire (`apply-summaries`)
;; and the render-time ledger (`ctx-engine/folds-view`) share ONE resolver.
(declare iter-of-scope)
(declare form-wire-chars)

(defn- prior-turn-scope-index
  "Lean per-form scope index for ONE prior turn's `forms`, reshaped by the model's
   fold/drop `summaries` — the cross-process RESUME view. Folds are recorded at
   ITERATION granularity (`tN/iN`) but forms carry FORM scopes (`tN/iN/fN`), so
   each form's scope is normalized via `iter-of-scope` before matching. (This is
   the fix for the latent bug where a raw form-scope lookup against the
   iteration-keyed drop/gist sets NEVER hit — so folds silently failed to apply
   in resume context.) Both a fold and a drop collapse their iteration to ONE
   deduped breadcrumb (not repeated per form): a fold → `{:scope tN/iN :gist g}`,
   a drop → `{:scope tN/iN :dropped? true :note why}` (the reason is kept so
   introspection never loses what went or why); every other live form keeps its
   `{:scope tN/iN/fN :src …}` line. `:drop?` — not gist presence — picks the
   label. A `:through` range cursor is resolved against this turn's own iteration
   scopes. Pure."
  [forms summaries]
  (let [universe
        (distinct (keep #(iter-of-scope (:scope %)) forms))

        sums
        (ctx-engine/supersede-summaries (ctx-engine/expand-through (or summaries []) universe))

        ;; Summary intents are STRING-KEYED ({"scopes" "gist" "drop" "through"})
        ;; — they persist inside the ctx nippy blob, and the DB is strings-only.
        drop-of
        (into {}
              (mapcat (fn [s]
                        (when (get s "drop")
                          (map (fn [sc]
                                 [sc (get s "gist")])
                               (get s "scopes"))))
                      sums))

        gist-of
        (into {}
              (mapcat (fn [s]
                        (when (and (not (get s "drop")) (get s "gist"))
                          (map (fn [sc]
                                 [sc (get s "gist")])
                               (get s "scopes"))))
                      sums))]

    (first
      (reduce (fn [[acc seen] f]
                (let [sc
                      (:scope f)

                      isc
                      (iter-of-scope sc)]

                  (cond (and isc (contains? drop-of isc)) ; dropped → ONE audit line
                        (if (contains? seen isc)
                          [acc seen]
                          [(conj acc
                                 (cond-> {:scope isc :dropped? true}
                                   (get drop-of isc)
                                   (assoc :note (get drop-of isc)))) (conj seen isc)])
                        (and isc (contains? gist-of isc))                 ; folded → ONE gist line
                        (if (contains? seen isc)
                          [acc seen]
                          [(conj acc {:scope isc :gist (get gist-of isc)}) (conj seen isc)])
                        (and sc
                             (or (some? (:result f)) (some? (:stdout f))) ; live, worth listing
                             (not= "vis_silent" (:result f)))
                        [(conj acc {:scope sc :src (ctx-engine/compact-src (:src f))}) seen]
                        :else [acc seen])))
              [[] #{}]
              forms))))

(defn- previous-turn-context
  "ALL prior ANSWERED turns as cross-process RESUME context — the conversation a
   fresh process must reconstruct to continue. Each turn carries its user
   request, its prose answer, and a LEAN index of the scopes it produced (scope +
   the call that made it), so the model KNOWS what it asked and answered.
   Oldest→newest; current turn excluded; nil when none."
  [environment current-turn-id]
  (try
    (when-let [session-id (:session-id environment)]
      (let [d (:db-info environment)
            ;; Summary-awareness: the model's session_fold/session_drop intents
            ;; (persisted on the ctx blob) reshape the scope index UNIFORMLY via
            ;; `prior-turn-scope-index`, so a prior turn renders the same here as
            ;; it did live — dropped scopes vanish, folded scopes carry their gist.
            summaries (some-> (:ctx-atom environment)
                              deref
                              (get "session_summaries"))
            ;; Include every prior turn the model must reconstruct to continue:
            ;; ANSWERED turns (Q/A carry) AND INTERRUPTED ones — a turn the
            ;; process was killed mid-flight (e.g. a gateway restart) still
            ;; carries the user's request, and a follow-up "continue" is
            ;; meaningless without it. Skip only the current turn and a still-
            ;; running one.
            include? (fn [turn]
                       (and (not= (str (:id turn)) (str current-turn-id))
                            (not= :running (:status turn))
                            (or (seq (some-> (:content turn)
                                             answer-markdown
                                             str
                                             str/trim))
                                (contains? #{:interrupted :error} (:status turn)))))
            turns (filter include? (persistance/db-list-session-turns d session-id))]

        (not-empty
          (mapv (fn [turn]
                  (let [forms (->> (try (persistance/db-list-session-turn-iterations d (:id turn))
                                        (catch Throwable _ []))
                                   (filter #(= :done (:status %)))
                                   (mapcat :forms))
                        ;; A form is worth listing if it produced EITHER a value
                        ;; (:result) OR printed output (:stdout); fold/drop intents
                        ;; reshape it (see prior-turn-scope-index). Print-only forms
                        ;; carry only :stdout (de-conflated).
                        scopes (vec (take 40 (prior-turn-scope-index forms summaries)))]

                    {:user-request (:user-request turn)
                     ;; An interrupted/error turn's only "answer" is the orphan-sweep
                     ;; sentinel / provider fallback, or nil — never a normal
                     ;; success answer. Drop it so the turn renders as
                     ;; UNFINISHED work to continue, not as "you answered with
                     ;; a warning/error".
                     :answer (when-not (contains? #{:interrupted :error} (:status turn))
                               (answer-markdown (:content turn)))
                     :interrupted? (contains? #{:interrupted :error} (:status turn))
                     :results scopes}))
                turns))))
    (catch Throwable t
      (tel/log! {:level :warn
                 :id ::previous-turn-context-failed
                 :data {:session-id (:session-id environment)
                        :session-turn-id current-turn-id
                        :error (ex-message t)}}
                "Could not load previous turn context; continuing without Q/A carry")
      nil)))

(defn- previous-request-usage
  "Return latest persisted provider request before `current-turn-id`.

   `:session/utilization` is rendered before the next provider call, so iter 1
   of a new turn cannot use current-turn API usage yet. Seed it from the prior
   persisted iteration instead; once this turn completes one iteration, live
   `usage-atom` readings take over."
  [environment current-turn-id]
  (try (when-let [session-id (:session-id environment)]
         (let [db (:db-info environment)
               turns (or (persistance/db-list-session-turns db session-id) [])
               current-id (str current-turn-id)]

           (some (fn [turn]
                   (let [iters
                         (try (persistance/db-list-session-turn-iterations db (:id turn))
                              (catch Throwable t
                                (tel/log!
                                  {:level :warn
                                   :id ::previous-request-iterations-failed
                                   :data {:session-id session-id
                                          :session-turn-id (:id turn)
                                          :error (ex-message t)}}
                                  "Could not load prior turn iterations while seeding utilization")
                                []))]
                     (when-let [it (last (filter #(pos? (long (or (:input-tokens %) 0))) iters))]
                       {:last-request-tokens (long (:input-tokens it))
                        :last-request-turn-id (:id turn)
                        :last-request-turn-position (:position turn)
                        :last-request-iteration (:position it)})))
                 (reverse (remove #(= (str (:id %)) current-id) turns)))))
       (catch Throwable t
         (tel/log! {:level :warn
                    :id ::previous-request-usage-failed
                    :data {:session-id (:session-id environment)
                           :session-turn-id current-turn-id
                           :error (ex-message t)}}
                   "Could not load previous request usage; first iteration will omit utilization")
         nil)))

(defn- stamp-utilization!
  "Monotonic update of `\"engine_utilization\"` on the ctx-atom. UPGRADES when a
   real measurement (`util`) exists; NEVER removes an existing value. A
   transient nil — iter-1 seed miss, or an errored iteration that returned no
   usage — must not BLANK an already-shown utilization; that flicker is the
   `sometimes works / sometimes doesn't` bug. The last value carries on the
   per-session live atom (`:engine/*` is stripped only at persist time) until
   a fresh request refreshes it; a brand-new session starts blank because
   nothing was ever stamped."
  [ctx-atom util]
  (when (and ctx-atom util) (swap! ctx-atom assoc "engine_utilization" util)))

(defn- stamp-iter-universe!
  "Record the CURRENT wire's iteration universe — the `tN/iN` scopes present in
   `trailer-iters` — on the ctx-atom as `engine_iter_universe`, so the render-time
   `ctx-engine/folds-view` resolves fold selectors + computes the still-live
   ledger against the SAME scopes the wire folds against. ALSO stamps
   `engine_iter_weights`: a `{scope → ~tokens}` map priced with the SAME estimator
   the `session_fold` card prices by (clipped wire chars / 4), so it can report how
   much wire a fold reclaims. A scope created THIS iteration (not yet sent) has no
   weight until the next send — the card just omits its token clause. Engine-ephemeral (an `engine_*`
   key, stripped at persist by `strip-ephemeral`); overwritten every send so it
   always reflects the live trailer."
  [ctx-atom trailer-iters]
  (when ctx-atom
    (let [scope-of
          (fn [rec]
            (some iter-of-scope (keep :scope (:forms-vec rec))))

          uni
          (into []
                (comp (keep (fn [[_ rec]]
                              (scope-of rec)))
                      (distinct))
                trailer-iters)

          weights
          (persistent! (reduce (fn [m [_ rec]]
                                 (if-let [sc (scope-of rec)]
                                   (let [chars (reduce +
                                                       0
                                                       (map form-wire-chars
                                                            (remove :summary? (:forms-vec rec))))]
                                     (assoc! m sc (+ (long (get m sc 0)) (quot (long chars) 4))))
                                   m))
                               (transient {})
                               trailer-iters))]

      (swap! ctx-atom assoc "engine_iter_universe" uni "engine_iter_weights" weights))))

(defn- runtime-turn-prefix
  [environment]
  (let [id-s
        (str (or (:session-turn-id (ctx-loop/read-turn-state environment))
                 (:environment-id environment)
                 "00000000"))

        prefix
        (subs id-s 0 (min 8 (count id-s)))]

    (if (re-matches #"(?i)[0-9a-f]{8}" prefix) prefix "00000000")))

(defn- eval-block-role
  "Block role for the outer lifecycle event — one of the four values
   in the iteration-block role enum:
     :answer    the model's final answer to the user
     :tool      any Python evaluation (tool call OR raw user code)
     :nudge     system-emitted reminders / diagnostics
     :thinking  model reasoning blocks
   The previous `:vis/error` role is gone — errors are derived from
   `:success?` on the envelope (or block-level `:error` slot for
   non-tool evals). Replaces the prior `eval-rendering-kind` fn."
  [result]
  (cond (= :answer (:role result)) :answer
        (= :tool (:role result)) :tool
        (= :nudge (:role result)) :nudge
        (= :thinking (:role result)) :thinking
        (keyword? (:role result)) (:role result)
        :else :tool))

(defn- eval-envelope
  "Generic canonical envelope for every executed block that passes
   through the Vis eval pipeline. Tool calls can add nested metadata
   in their returned envelope; this records the outer block
   evaluation so plain calls and tool calls share a common block-level
   trace."
  [turn-prefix iteration form-idx form-count result rendering-kind]
  (let [finished
        (long (or (:execution-finished-at-ms result) (System/currentTimeMillis)))

        duration
        (long (or (:duration-ms result) 0))

        started
        (long (or (:execution-started-at-ms result) (max 0 (- finished duration))))

        form-position
        (inc (long form-idx))]

    {:op (or (:op result)
             (case rendering-kind
               :nudge
               :vis/system

               :answer
               :vis/answer

               :python/eval))
     :started-at-ms started
     :finished-at-ms finished
     :status (cond (:timeout? result) :timeout
                   (:error result) :error
                   :else :done)
     :iteration iteration
     :form-position form-position
     :form-count form-count
     :ref (str "turn/" turn-prefix "/iteration/" iteration "/block/" form-position)
     :timeout? (boolean (:timeout? result))
     :repaired? (boolean (:repaired? result))}))

(defn- envelope-timestamps-ordered?
  [envelope]
  (<= (long (:started-at-ms envelope)) (long (:finished-at-ms envelope))))

(defn- envelope-form-position-valid?
  [envelope]
  (<= (long (:form-position envelope)) (long (:form-count envelope))))

(defn- envelope-ref-consistent?
  [envelope]
  (let [[_ iteration block] (re-matches
                              #"(?i)^turn/[0-9a-f]{8}/iteration/([1-9][0-9]*)/block/([1-9][0-9]*)$"
                              (:ref envelope))]
    (and iteration
         block
         (= (Long/parseLong iteration) (long (:iteration envelope)))
         (= (Long/parseLong block) (long (:form-position envelope))))))

(defn- envelope-has-no-derived-duration? [envelope] (not (contains? envelope :duration-ms)))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
             (nat-int? (:started-at-ms envelope))
             (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))

(defn- block-duration-ms [block] (or (envelope-duration-ms (:envelope block)) 0))

(s/def ::id nat-int?)
(s/def ::code string?)
(s/def ::error (s/nilable map?))                       ; structured :error map
(s/def ::timeout? (s/nilable boolean?))
(s/def ::repaired? (s/nilable boolean?))
(s/def ::comment string?)
(s/def ::op #{:python/eval :vis/guard :vis/system :vis/answer})
(s/def ::status #{:done :error :timeout})
(s/def ::iteration pos-int?)
(s/def ::form-position pos-int?)
(s/def ::form-count pos-int?)
(s/def ::started-at-ms nat-int?)
(s/def ::finished-at-ms nat-int?)
(s/def ::ref
  (s/and string? #(re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/[1-9][0-9]*/block/[1-9][0-9]*$" %)))
(s/def ::block-envelope
  (s/and (s/keys :req-un [::op ::status ::iteration ::form-position ::form-count ::started-at-ms
                          ::finished-at-ms ::ref]
                 :opt-un [::timeout? ::repaired?])
         envelope-timestamps-ordered?
         envelope-form-position-valid?
         envelope-ref-consistent?
         envelope-has-no-derived-duration?))
(s/def ::envelope ::block-envelope)
(s/def ::iteration-block
  (s/keys :req-un [::id ::code ::error ::envelope]
          :opt-un [::result ::timeout? ::repaired? ::comment]))

(defn validate-iteration-blocks!
  "Fail fast if a stored/evaluated block lost mandatory envelope.
   Tool-result envelopes enforce their nested info separately;
   this spec enforces the outer block-level eval envelope for every
   executed block."
  [blocks]
  (let [blocks (mapv (fn [block]
                       (cond-> block
                         (contains? block :error)
                         (update :error
                                 op-error
                                 {:code (:code block) :phase (get-in block [:envelope :op])})))
                     (or blocks []))]
    (doseq [block blocks]
      (when-not (s/valid? ::iteration-block block)
        (throw (ex-info "Invalid iteration block"
                        {:type :vis/invalid-iteration-block
                         :block block
                         :explain (s/explain-data ::iteration-block block)}))))
    blocks))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn- token-number
  [tokens ks]
  (some (fn [k]
          (let [v (get tokens k)]
            (when (number? v) v)))
        ks))

(defn- ask-result->api-usage
  "Return svar's canonical usage map, falling back to its flat public `:tokens`
   projection for older/custom providers. svar 0.7 uses keyword token keys
   (`:cache-created`, not the wire-only `\"cache_created\"`), so normalizing the
   flat fallback here prevents a silent all-zero turn when `:api-usage` is
   absent."
  [{:keys [api-usage tokens]}]
  (or api-usage
      (when (map? tokens)
        (let [input
              (long (or (token-number tokens [:input "input"]) 0))

              output
              (long (or (token-number tokens [:output "output"]) 0))

              cached
              (long (or (token-number tokens [:cached "cached"]) 0))

              cache-created
              (long (or (token-number tokens [:cache-created :cache_created "cache_created"]) 0))

              input-regular
              (long (or (token-number tokens [:input-regular :input_regular "input_regular"])
                        (max 0 (- input cached cache-created))))

              reasoning
              (token-number tokens [:reasoning "reasoning"])]

          (cond-> {:input-tokens input
                   :output-tokens output
                   :input-tokens-details
                   {:regular input-regular :cache-write cache-created :cache-read cached}
                   :total-tokens (long (+ input output))}
            (some? reasoning)
            (assoc :output-tokens-details {:reasoning (long reasoning)}))))))

(defn- reasoning-effort-configurable?
  "True when a model accepts a caller-selected reasoning effort.

   `:reasoning?` means the model can produce reasoning/thinking text.
   It does NOT imply that Vis may tune that thinking depth. Z.ai GLM
   binary thinking is preserved-thinking only, so keep the stream visible
   but do not send abstract `:reasoning` levels to svar. Provider-native
   `:reasoning-effort` is validated separately against catalog metadata."
  [resolved-model]
  (and (boolean (:reasoning? resolved-model))
       (not= false (:reasoning-effort? resolved-model))
       (not= :zai-thinking (:reasoning-style resolved-model))))

(defn- ^:private replay-reasoning-chars
  "Total `:thinking-signature` (or `:thinking` fallback) char count for
   the canonical thinking blocks on `assistant-message`. 0 when nil.
   The signature field is what svar's wire serializer hoists into
   `reasoning_content` — that is what counts against the budget."
  [assistant-message]
  (->> (get assistant-message :content)
       (filter (fn [b]
                 (= "thinking" (:type b))))
       (map (fn [b]
              (count (or (:thinking-signature b) (:thinking b) ""))))
       (reduce + 0)))

(defn- preserved-thinking-replay-messages
  "Provider-agnostic preserved-thinking replay. Returns every compatible
   `:assistant-message` from `trailer-iters` in arrival order.

   Why every message, not just the last:
     - Z.ai / GLM-5.x preserved thinking (`clear_thinking: false`) keeps
       reasoning_content across assistant turns only when each prior
       assistant message echoes the model's full reasoning back. Drop a
       step and GLM either re-derives the same scratch state at every
       iteration (re-reading the same file with `cached_tokens` pinned
       across many iterations) or starts to
       hallucinate that an earlier conclusion is still live.
     - Anthropic extended thinking signs each block with an HMAC and
       refuses replay if the chain is broken; sending only the last
       block fails signature validation as soon as the model produced
       more than one block since the user message.
     - OpenAI Responses encrypted reasoning items must replay in order
       — the next call rejects a single isolated item with
       'reasoning without following item'.

   The earlier conservative 'last-only' policy was tuned for
   pre-`clear_thinking`
   GLM-4.6 where any replay contaminated the next step. The modern
   GLM-5.1 + Anthropic 4.x + OpenAI Responses contract all want full
   chains; pi-ai's `transform-messages.js` follows the same approach
   (every prior assistant `thinking` block preserved when same model).

   `compatible-preserved-thinking-trailer-iters` upstream has already
   filtered iterations to (a) same provider+model as the target call,
   (b) opted in via `:preserved-thinking/replay?` (live-turn freshly
   produced iterations), (c) carrying a valid `:assistant-message`,
   (d) signature-compatible with the replay target. Anything that
   reaches this fn is safe to replay verbatim.

   The wire serializer for the active model translates each canonical
   message to its native shape; iteration-loop never branches on
   provider."
  [trailer-iters]
  (let [msgs (vec (keep #(some-> %
                                 second
                                 :assistant-message)
                        trailer-iters))]
    (when (seq msgs)
      ;; Keep this call so oversized reasoning chains are observable to
      ;; future budget instrumentation. Sum across the full chain instead
      ;; of just the latest step — budget watchers care about cumulative
      ;; replay size, not single-step size.
      (doseq [m msgs]
        (replay-reasoning-chars m)))
    msgs))

(defn- replay-context
  "Small identity map for deciding whether preserved-thinking can be
   replayed into the next provider call. Provider-native thinking
   signatures are not portable: z.ai stores reasoning text under
   `:thinking-signature`, Anthropic expects an HMAC signature, and
   OpenAI Responses stores a JSON reasoning item. Replaying across a
   provider/model switch corrupts the next request (Anthropic 400:
   invalid signature in thinking block)."
  [resolved-model]
  {:provider (:provider resolved-model)
   :model (some-> (:name resolved-model)
                  str)})

(defn- anthropic-replay-context?
  [{:keys [provider model]}]
  (or (boolean (re-find #"(?i)anthropic" (str provider)))
      (boolean (re-find #"(?i)^claude" (str model)))))

(defn- thinking-blocks
  [assistant-message]
  (filterv #(= "thinking" (:type %)) (:content assistant-message)))

(defn- anthropic-invalid-thinking-replay-block?
  "True for poisoned Anthropic replay state. In bad historical rows,
   Vis recorded a fallback z.ai response as Anthropic; z.ai stores raw
   reasoning text as `:thinking-signature`, so signature == thinking.
   Anthropic signatures are opaque HMACs and must not equal prose."
  [block]
  (let [thinking
        (:thinking block)

        signature
        (:thinking-signature block)]

    (and (string? thinking)
         (not (str/blank? thinking))
         (string? signature)
         (= thinking signature))))

(defn- assistant-message-compatible-with-replay-target?
  [target assistant-message]
  (not (and (anthropic-replay-context? target)
            (some anthropic-invalid-thinking-replay-block? (thinking-blocks assistant-message)))))

(defn- actual-llm-provider
  "Provider that actually served an ask-result. svar may route/fallback
   inside ask-code!, so prefer routed metadata over Vis' pre-call guess."
  [resolved-model ask-result]
  (or (:routed/provider-id ask-result) (:provider resolved-model)))

(defn- actual-llm-model
  "Model that actually served an ask-result. See `actual-llm-provider`."
  [resolved-model ask-result]
  (or (:routed/model ask-result)
      (some-> (:name resolved-model)
              str)))

(defn- llm-id
  [provider model]
  (cond-> {}
    provider
    (assoc :provider (name (keyword provider)))

    model
    (assoc :model (str model))))

(defn- llm-routing-summary
  [selected-model iteration-result]
  (let [routing-trace
        (vec (or (:llm-routing-trace iteration-result) []))

        fallback-ev
        (first (filter #(contains? #{:llm.routing/provider-fallback :llm.routing/model-fallback
                                     :llm.routing/format-fallback}
                                   (:event/type %))
                       routing-trace))

        ;; The authoritative anchors are the fallback event's from/to when a
        ;; real fallback was traced: the router may pre-resolve so the iteration
        ;; result's provider/model already reflect the FALLBACK, which would
        ;; otherwise collapse selected==actual and drop the '↳ from …' note.
        selected
        (llm-id (or (:from-provider fallback-ev) (:provider selected-model))
                (or (:from-model fallback-ev)
                    (some-> (:name selected-model)
                            str)))

        actual
        (llm-id (or (:to-provider fallback-ev)
                    (:llm-provider iteration-result)
                    (:provider selected-model))
                (or (:to-model fallback-ev)
                    (:llm-model iteration-result)
                    (some-> (:name selected-model)
                            str)))]

    (cond-> {:selected selected
             :actual actual
             :fallback? (boolean (or (not= selected actual)
                                     (some #(not= :llm.routing/provider-retry (:event/type %))
                                           routing-trace)))}
      (seq routing-trace)
      (assoc :trace routing-trace))))

(defn- attach-llm-routing-summary
  [result selected-model iteration-result]
  (let [routing
        (llm-routing-summary selected-model iteration-result)

        actual
        (:actual routing)

        selected
        (:selected routing)]

    (cond-> (assoc result
              :provider (:provider actual)
              :model (:model actual)
              :llm-selected selected
              :llm-actual actual
              :llm-fallback? (:fallback? routing))
      (seq (:trace routing))
      (assoc :llm-routing-trace (:trace routing))

      (:cost result)
      (update :cost merge (select-keys actual [:provider :model])))))

(defn- reasoning-effort-iteration-evidence
  [iteration requested selected-model iteration-result]
  (let [routing
        (llm-routing-summary selected-model iteration-result)

        resolution
        (:reasoning-effort-resolution iteration-result)

        actual
        (:actual routing)]

    {:iteration (inc (long iteration))
     :provider (:provider actual)
     :model (:model actual)
     :effective (:effective resolution)
     :wire-style (:wire-style resolution)
     :wire-fragment (:extra-body resolution)
     :fallback? (:fallback? routing)
     :selected (:selected routing)
     :requested requested}))

(defn- turn-eval-evidence
  [requested trace]
  (when requested
    (let [iterations
          (vec (keep :reasoning-effort trace))

          missing-count
          (- (count trace) (count iterations))

          fallback-reasons
          (for [{:keys [iteration fallback? selected provider model]}
                iterations

                :when fallback?]

            {:type :provider-model-fallback
             :iteration iteration
             :selected selected
             :actual {:provider provider :model model}})

          mismatch-reasons
          (for [{:keys [iteration effective provider model]}
                iterations

                :when (not= requested effective)]

            {:type :reasoning-effort-mismatch
             :iteration iteration
             :requested requested
             :effective effective
             :provider provider
             :model model})

          reasons
          (vec (concat (when (or (empty? trace) (pos? missing-count))
                         [{:type :missing-reasoning-effort-evidence :iterations missing-count}])
                       fallback-reasons
                       mismatch-reasons))]

      {:valid? (boolean (and (seq iterations) (empty? reasons)))
       :invalid-reasons reasons
       :reasoning-effort {:requested requested :iterations iterations}})))

(defn- compatible-preserved-thinking-trailer-iters
  "Keep only iterations whose provider-native thinking may be replayed into
   the next provider call.

   Cross-turn trailer seeds explicitly carry
   `:preserved-thinking/replay? false`; those iterations remain visible in
   persisted iterations as durable evidence, but their opaque provider-native thinking
   state is not replayed into a different user turn. Within a live turn,
   freshly-produced iterations opt in by setting the flag to true. Historical
   in-memory test fixtures that omit the flag are treated as replayable for
   backward compatibility."
  [trailer-iters target]
  (let [{target-provider :provider target-model :model} target]
    (filterv (fn [[_
                   {:keys [assistant-message llm-provider llm-model]
                    replay? :preserved-thinking/replay?}]]
               (and (not= false replay?)
                    assistant-message
                    (= target-provider llm-provider)
                    (= target-model llm-model)
                    (assistant-message-compatible-with-replay-target? target assistant-message)))
      (or trailer-iters []))))

;; -----------------------------------------------------------------------------
;; Frozen result messages — prefix-cache-friendly form-result history.
;;
;; Form results used to render inside the regenerated `<context>` user message
;; at the end of every provider call. Because the prefix cache ends at the first
;; changed byte, the accumulated results were re-billed uncached on every
;; iteration. The fix: each result renders ONCE into a permanent `<results>` user
;; message, interleaved chronologically with assistant replays, so the
;; conversation grows APPEND-ONLY:
;; assistant replays, so the conversation grows APPEND-ONLY:
;;
;;   [system, user_initial,
;;    <pre-turn pins>,
;;    asst_iter1, <results t/i1>,
;;    asst_iter2, <results t/i2>,
;;    ...,
;;    <mutable context tail>]
;;
;; Compaction (`session_fold`) REWRITES pins → the frozen
;; messages change → one deliberate cache bust, paid only under window
;; pressure instead of on every call.
;; -----------------------------------------------------------------------------

(defn- iter-of-scope
  "Form scope `\"t1/i2/f3\"` → its iteration scope `\"t1/i2\"` (drops the `/fN`).
   nil for non-form scopes (e.g. the synthetic `:summary` keyword)."
  [scope]
  (when (string? scope)
    (let [parts (str/split scope #"/")]
      (when (>= (count parts) 2) (str (nth parts 0) "/" (nth parts 1))))))

(defn- compaction-verbs
  "Build the model-facing compaction verb bound into the sandbox as
   `session_fold`, closing over `ctx-atom`. It records a `:session/summaries`
   intent the wire applies via `apply-summaries`, and RETURNS a visible
   confirmation string (NOT the `\"vis_silent\"` row-suppression sentinel) so the
   action shows in the Python result instead of vanishing.

   First positional arg selects the target: a list/string of explicit scopes, or
   a `{\"through\" \"tN/iN\"}` options dict (Python kwargs don't cross `wrap-ifn`;
   `->clj` keeps dict keys as VERBATIM STRINGS, so `\"through\"` is the accessor).
   The second arg — the gist — is OPTIONAL: pass it to KEEP a one-line takeaway,
   OMIT it to simply DISCARD the step (this replaces the old `session_drop`; a
   gist-less fold collapses the step with no summary line). Intents are
   STRING-KEYED (they persist inside the ctx nippy blob — strings-only DB):
     {\"scopes\" #{…}|\"through\" \"tN/iN\", \"gist\" <takeaway>?}
   `apply-summaries` still renders any legacy persisted `{\"drop\" true}` intents."
  [ctx-atom]
  (let [->set
        (fn [scopes]
          (into #{}
                (comp (map str) (map str/trim) (remove str/blank?))
                (cond (sequential? scopes) scopes
                      (string? scopes) [scopes]
                      :else nil)))

        freeze
        (fn [intent]
          ;; Unbounded-above selectors (`since`, or `from` without `to`) would
          ;; otherwise re-resolve against the GROWING universe on every send and
          ;; silently swallow iterations created AFTER the fold — a standing
          ;; subscription to future work the model never chose to fold. Freeze
          ;; the ceiling NOW: resolve to concrete scopes against the current
          ;; universe so the fold captures only what existed at fold time.
          ;; Bounded selectors (`through`, `from`+`to`) are already safe and
          ;; pass through untouched (still re-resolved raw, but their upper
          ;; bound blocks any new scope).
          (let [unbounded?
                (or (contains? intent "since")
                    (and (contains? intent "from") (not (contains? intent "to"))))

                universe
                (some-> ctx-atom
                        deref
                        (get "engine_iter_universe"))]

            (if (and unbounded? (seq universe))
              (first (ctx-engine/expand-through [intent] universe))
              intent)))

        target
        (fn [scopes]
          (if (map? scopes)
            (let [pick
                  (fn [k]
                    (some-> (get scopes k)
                            str
                            str/trim
                            not-empty))

                  thr
                  (pick "through")

                  snc
                  (pick "since")

                  frm
                  (pick "from")

                  to
                  (pick "to")]

              (cond thr [{"through" thr} (str "through " thr)]
                    snc [(freeze {"since" snc}) (str "since " snc)]
                    (or frm to) [(freeze (cond-> {}
                                           frm
                                           (assoc "from" frm)

                                           to
                                           (assoc "to" to)))
                                 (str "window " (or frm "start") ".." (or to "end"))]
                    :else nil))
            (let [ss (->set scopes)]
              (when (seq ss) [{"scopes" ss} (str/join ", " (sort ss))]))))

        current-turn
        (fn []
          (let [v (some-> ctx-atom deref (get "session_turn"))]
            (cond (integer? v) (long v)
                  (string? v) (parse-long (str/trim v))
                  :else nil)))

        scope-turn
        (fn [scope]
          (or (some-> (ctx-engine/scope-key scope) first)
              (ctx-engine/turn-key scope)))

        selected-turns
        (fn [intent]
          (let [ctx (some-> ctx-atom deref)
                universe (get ctx "engine_iter_universe")
                resolved (first (ctx-engine/expand-through [intent] (or universe [])))
                refs (concat (get resolved "scopes")
                             (get intent "scopes")
                             (keep #(get intent %) ["through" "since" "from" "to"]))]
            (into #{} (keep scope-turn) refs)))

        record!
        (fn [intent]
          (when ctx-atom (swap! ctx-atom update "session_summaries" (fnil conj []) intent)))

        fmt-tok
        (fn [t]
          (if (>= (long t) 1000)
            (str (long (Math/round (/ (double t) 1000.0))) "k")
            (str (long t))))

        ;; Human-facing enrichment for the fold card: how much wire THIS fold
        ;; reclaims — in ~tokens (summed from `engine_iter_weights`) AND as a
        ;; fraction of the model's per-call limit (`~P% of window`). That figure
        ;; is deliberately the fold's OWN contribution (a REDUCTION), never a
        ;; derived "how full am I" level: `last_request_tokens` grows with every
        ;; new tool result, so a PROJECTED level would RISE across iterations
        ;; even when the fold helped (issue #27's scary regression: a
        ;; moving-for-the-wrong-reason figure beside `saved ~Nk` reads as the
        ;; fold's result). Alongside it we ALSO surface the live window fullness
        ;; as `context <U>%` — but taken straight from the provider's
        ;; authoritative `saturation` (last-request / model-input-limit), the
        ;; SAME number `session["utilization"]["now"]` shows and clearly a
        ;; separate, absolute reading — so it can't be misread as the fold's own
        ;; reduction. Best-effort — any hiccup degrades to no suffix rather than
        ;; breaking the card.
        priced
        (fn [base]
          (try
            (let [ctx
                  (some-> ctx-atom
                          deref)

                  universe
                  (get ctx "engine_iter_universe")

                  weights
                  (get ctx "engine_iter_weights")

                  util
                  (get ctx "engine_utilization")

                  scopes
                  (into #{}
                        (mapcat #(get % "scopes"))
                        (ctx-engine/expand-through [base] (or universe [])))

                  toks
                  (reduce + 0 (keep #(get weights %) scopes))

                  lim
                  (get util "model_input_limit")

                  pct
                  (when (and (pos? (long toks)) lim (pos? (long lim)))
                    (long (Math/round (/ (* 100.0 (double toks)) (double lim)))))

                  sat
                  (get util "saturation")]

              (when (pos? (long toks))
                (str " · saved ~"
                     (fmt-tok toks)
                     " tokens"
                     (when pct (str " · ~" pct "% of window"))
                     (when (and sat (pos? (long sat))) (str " · context " sat "%")))))
            (catch Throwable _ "")))]

    {'session-fold
     (fn session-fold [scopes & [gist]]
       (if-let [[base label] (target scopes)]
         (let [turn (current-turn)
               blocked-turns (when turn
                               (into (sorted-set)
                                     (filter #(>= (long %) turn))
                                     (selected-turns base)))]
           (when-not turn
             (throw (ex-info "session_fold cannot prove the current turn; folding is blocked."
                             {:type :vis/session-fold-turn-unknown})))
           (when (seq blocked-turns)
             (throw
               (ex-info
                 (str "session_fold accepts only completed prior-turn scopes. "
                      "Current/future turn scopes must stay live through verification; "
                      "retry at the start of the next turn.")
                 {:type :vis/session-fold-active-turn
                  :current-turn turn
                  :blocked-turns blocked-turns})))
           (let [g (some-> gist
                           str
                           str/trim
                           not-empty)
                 note (priced base)
                 intent (cond-> base
                          g
                          (assoc "gist" g)

                          (not (str/blank? note))
                          (assoc "note" note))]

             (record! intent)
             (tel/log! {:level :info :id ::session-fold :data {:intent intent}}
                       "model folded scopes")
             (str "folded " label note (when g (str " → " g)))))
         (str "session_fold: nothing to fold — pass [\"t1/i2\", …] (a bare \"t1\" folds "
              "the whole turn), or a selector {\"through\"|\"since\": \"t1/i2\"} / "
              "{\"from\": \"t1/i2\", \"to\": \"t1/i5\"}")))}))

(defn- apply-summaries
  "Wire-only rewrite of `trailer-iters` applying the model's `session_fold`/
   `session_drop` intents at ITERATION granularity. Each summary is
   `{\"scopes\" #{\"tN/iN\" …} \"gist\" <string|nil>}` (drop = nil gist), or a range
   `{\"through\" \"tN/iN\" …}` which `expand-through` resolves to the trailer's own
   iteration scopes ≤ the cursor. Every iteration whose `tN/iN` scope is
   summarized COLLAPSES: its output is removed and it's tagged `:collapsed? true`
   so `conversation-suffix` drops its assistant + tool_result pair entirely; at
   the EARLIEST iteration of each group one gist form is injected, rendered as
   `# -- tN/iN … -- summarized: <gist>` (or `-- dropped`). Real compaction: the
   whole iteration leaves the wire, replaced by one line.

   Pure and deterministic (same summaries → same output → prefix-cacheable).
   Operates on a COPY; persisted iter-records are untouched."
  [trailer-iters summaries]
  (if (empty? summaries)
    (vec trailer-iters)
    (let [iter-scope-of
          (fn [rec]
            (some iter-of-scope (keep :scope (:forms-vec rec))))

          ;; Resolve any `:through` range cursor against THIS trailer's live
          ;; iteration scopes before matching, so a range fold collapses every
          ;; step at or before the cursor; then supersede covered summaries so a
          ;; broader re-fold replaces the finer one (one breadcrumb, not two).
          summaries
          (ctx-engine/supersede-summaries
            (ctx-engine/expand-through summaries (keep iter-scope-of (map second trailer-iters))))

          summarized
          (into #{} (mapcat #(get % "scopes")) summaries)

          ; set of "tN/iN"
          ;; summary → earliest trailer index whose iteration scope it names
          anchors
          (reduce
            (fn [m s]
              (if-let [idx (some (fn [[i [_ rec]]]
                                   (when (contains? (set (get s "scopes")) (iter-scope-of rec)) i))
                                 (map-indexed vector trailer-iters))]
                (update m
                        idx
                        (fnil conj [])
                        {:gist (get s "gist")
                         :drop? (get s "drop")
                         :summary-iters (vec (sort (get s "scopes")))
                         :note (get s "note")})
                m))
            {}
            summaries)]

      (vec
        (map-indexed (fn [i [pos rec]]
                       (let [collapsed?
                             (contains? summarized (iter-scope-of rec))

                             gists
                             (get anchors i)

                             gist-forms
                             (when gists
                               (mapv (fn [g]
                                       {:scope :summary
                                        :summary? true
                                        :summary-gist (:gist g)
                                        :summary-drop? (:drop? g)
                                        :summary-iters (:summary-iters g)
                                        :summary-note (:note g)})
                                     gists))]

                         [pos
                          (cond-> rec
                            collapsed?
                            (assoc :collapsed?
                              true :forms-vec
                              [])

                            gist-forms
                            (assoc :forms-vec (vec gist-forms)))]))
                     trailer-iters)))))

(defn- error->display
  "LLM-legible rendering of a form `:error` for the model wire. The human
   `:message` (which may already carry a multi-line babashka-style source
   excerpt with a caret) is shown with REAL newlines — NEVER an escaped
   one-line Python/JSON literal, which turns a caret excerpt into an
   unreadable `\n`-wall the model can't parse. The failure phase rides in the
   header (`✗ runtime error:` / `syntax` / `host`); the precise line/col are
   already visible under the caret, so no redundant `:data` blob is emitted. A
   `:hint` not already folded into the message is appended on its own line.
   Falls back to the plain value for a non-map error."
  [error]
  (if-not (map? error)
    (str "✗ error: " error)
    (let [msg
          (or (:message error)
              (some-> (:type error)
                      name)
              "error")

          phase
          (some-> (get-in error [:data :phase])
                  (#(if (keyword? %) (name %) (str %))))

          hint
          (:hint error)]

      (cond-> (str "✗ " (when phase (str phase " ")) "error: " msg)
        (and hint (not (str/includes? (str msg) (str hint))))
        (str "\nhint: " hint)))))

(defn- patch-file-summary?
  "True when `m` is a patch/write/struct_patch PER-FILE summary — the
   `{\"path\" \"op\" \"changed\" …}` shape `patch-result-file-summary` emits. Used to
   recognise an edit result at the model-wire crossing without touching any other
   tool's `:result`."
  [m]
  (and (map? m) (string? (get m "path")) (string? (get m "op")) (contains? m "changed")))

(defn- strip-echo-diff
  "Drop the `\"diff\"` from ONE edit summary when the applied bytes are exactly what
   the model authored — a byte-exact edit (no fuzzy `\"passes\"`, no `\"indent_delta\"`
   auto-shift). In that case the unified diff merely re-describes the model's own
   `replace` text and carries zero new signal, so it's pure echo-bloat on the wire.
   Keep it whenever the harness may have changed the edit under the model (a fuzzy
   pass fired or the indent auto-shifted) — that's the one time the diff is real
   signal the model can't otherwise know. The HUMAN card keeps the diff regardless
   (it renders off the un-stripped summary)."
  [m]
  (if (and (contains? m "diff") (not (seq (get m "passes"))) (nil? (get m "indent_delta")))
    (dissoc m "diff")
    m))

(defn- strip-echo-diffs
  "Model-wire compaction for a patch/write/struct_patch `:result`: strip each
   byte-exact file summary's redundant `\"diff\"` (see `strip-echo-diff`). A no-op
   for any non-edit `:result` (only touched when EVERY element is a file summary)."
  [result]
  (cond (and (sequential? result) (seq result) (every? patch-file-summary? result))
        (mapv strip-echo-diff result)
        (patch-file-summary? result) (strip-echo-diff result)
        :else result))


(def ^:private MAX_FORM_WIRE_CHARS
  "Per-block printed-output ceiling. A block's stdout is head-clipped to this
   many chars in the tool result — a universal backstop for a runaway print()
   that tool-level caps don't catch (the model can `print(open-ended
   composition)`). The block's values still live in the sandbox (persistent REPL
   vars the model can re-slice and print less of). ~64KB ≈ 16k tokens: generous
   for an intentional full-file read, tight enough that one runaway print can't
   blow the request."
  65536)

(defn- tool-result-display
  "The human-channel DISPLAY for one executed TOOL CALL as `{:summary :body}` —
   the ONE surface both the TUI and web render, so they're unified:
     - native tool WITH a `:render` fn → its `{:summary :body}` card. `:summary`
       is the op-card HEADLINE (e.g. \"5 hits in 1 file\"); `:body` is the detail
       beneath it. A renderer that returns a bare string is treated as a body.
     - native tool WITHOUT one → its `:result` pretty-printed (Python-literal,
       fenced) as the body, no summary;
     - `python_execution` → its `:stdout` verbatim as the body, no summary.
   The body is head-clipped to `MAX_FORM_WIRE_CHARS`. Returns nil when there's
   nothing to show; the summary is the proper op-card title (NOT a first-line
   slice of the body)."
  [result* tool-name renderers]
  (let [clip
        (fn [^String s]
          (let [s
                (str/trimr (str s))

                n
                (long (count s))]

            (when (pos? n)
              (if (> n (long MAX_FORM_WIRE_CHARS))
                (str (subs s 0 MAX_FORM_WIRE_CHARS)
                     "\n# ⋯ output clipped at "
                     MAX_FORM_WIRE_CHARS
                     "/"
                     n
                     " chars")
                s))))

        ;; per-tool custom renderer (declared on the symbol's :native-tool :render)
        custom
        (when-let [rf (and tool-name (some? (:result result*)) (get renderers tool-name))]
          (try (rf (:result result*))
               (catch Throwable e
                 ;; never swallow silently — a broken renderer must be visible
                 ;; in the logs; the result still shows via the pretty-print path.
                 (tel/log! {:level :warn
                            :id ::native-tool-render-failed
                            :data {:tool tool-name :error (ex-message e)}}
                           (str "native-tool :render for "
                                tool-name
                                " threw — falling back to pretty-print"))
                 nil)))

        ->card
        (fn [m]
          (let [summary
                (some-> (:summary m)
                        str
                        str/trim
                        not-empty)

                body
                (some-> (:body m)
                        str
                        clip)]

            (when (or summary body) {:summary summary :body body})))]

    (cond
      ;; The outer wall-clock BACKSTOP fired (the tool's own timeout did not return
      ;; its structured result first): there is no :result to render, but the card
      ;; must still read as a TIMEOUT — a distinct ⧖ headline, not the raw
      ;; :vis/native-tool-timeout error string. Sits FIRST so a timeout always wins
      ;; its own display regardless of any partial stdout/error also present.
      (:timeout? result*) (let [err
                                (:error result*)

                                ms
                                (some-> err
                                        :data
                                        :timeout-ms)]

                            {:summary (str "⧖ timed out" (when ms (str " after " ms "ms")))
                             :body (some-> (:message err)
                                           str
                                           not-empty
                                           clip
                                           (->> (str "\n")))})
      ;; renderer returned a canonical {:summary :body} card …
      (map? custom) (->card custom)
      ;; … or a legacy bare string (body only, no summary)
      (string? custom) (->card {:body custom})
      ;; native tool value, no custom renderer → monospaced Python-literal body, so
      ;; a dict/list reads as structured data rather than reflowed prose.
      (some? (:result result*)) (when-let [s (clip (env/ctx->python-str (:result result*)))]
                                  {:body (str "```python\n" s "\n```")})
      ;; A `vis-image` fence (matplotlib `plt.show()` → inline PNG, ASCII plot
      ;; carried as its fallback body) rides stdout as MARKDOWN so the channel
      ;; paints it inline; wrapping it in a ``` block would escape the 4-backtick
      ;; fence, so pass the stdout through verbatim (unclipped — the fence is small
      ;; and self-bounded) whenever one is present.
      (str/includes? (str (:stdout result*)) "````vis-image") {:body (str (:stdout result*))}
      ;; python_execution printed output → fenced so newlines are preserved verbatim
      ;; (plain stdout is NOT markdown; bare \n collapses to a space through the
      ;; CommonMark SoftLineBreak → :space path if left unwrapped).
      (not (str/blank? (str (:stdout result*)))) {:body
                                                  (str "```\n" (clip (:stdout result*)) "\n```")}
      :else nil)))

(defn- iteration-results-message
  "Render ONE prior tool-call iteration as the `tool_result` user message that
   answers its `tool_use`(s) — maki model: the content is what the program
   PRINTED (raw stdout), plus errors and any `summarize`/`drop` fold lines.
   One `tool_result` block per `tool_use`, each carrying ITS
   OWN forms' output (forms are grouped by `:svar/tool-call-id`) — the maki
   model, where one call may be python_execution and others direct file tools.
   Falls back to a plain text user message when no tool calls are recorded."
  [iter-record]
  (let [;; ONE scope source: the `forms-vec` (each `{:scope :result …}`).
        ;; Falls back to scoped `:blocks` forms.
        forms
        (or (:forms-vec iter-record)
            (mapcat (fn [b]
                      (or (seq (:forms b)) [b]))
                    (:blocks iter-record)))

        ;; `session_fold(...)` / `session_drop(...)` folds (synthetic forms
        ;; apply-summaries injected) render FIRST as one Python comment naming the
        ;; iteration scopes they replaced. `:summary-drop?` picks the label; the
        ;; gist carries the takeaway (fold) or the reason (drop):
        ;;   # ⋯ folded t1/i1-i2 · <gist>
        ;;   # ⋯ dropped t1/i3 · <why>
        summary-lines
        (keep (fn [f]
                (when (:summary? f)
                  (let [at
                        (or (ctx-engine/pretty-scopes (:summary-iters f) nil)
                            (str/join "," (:summary-iters f)))

                        note
                        (:summary-note f)

                        g
                        (:summary-gist f)]

                    (str "# ⋯ "
                         (if (:summary-drop? f) "dropped " "folded ")
                         at
                         note
                         (when g (str " · " g))))))
              forms)

        ;; What becomes context:
        ;;   - python_execution: ONLY what the program PRINTED, shown RAW. A bare
        ;;     expression's value is NOT auto-echoed — print() to see it.
        ;;   - NATIVE file tools (cat/rg/…): the call's RETURN value, because the
        ;;     model never print()s a native call — its result IS the output.
        ;; Errors always surface (the model must see a failure even if nothing
        ;; printed). A clipped value still lives in the sandbox to re-slice.
        clip-wire
        (fn [s]
          (let [s
                (str/trimr (str s))

                n
                (long (count s))]

            (if (> n (long MAX_FORM_WIRE_CHARS))
              (str (subs s 0 MAX_FORM_WIRE_CHARS)
                   "\n# ⋯ output clipped at "
                   MAX_FORM_WIRE_CHARS
                   "/"
                   n
                   " chars — narrow next time (slice/filter before reading).")
              s)))

        ;; Each form carries exactly ONE success channel (the engine emits one or
        ;; the other, never both): `:result` = a native tool call's returned value
        ;; (rendered), `:stdout` = what python_execution print()ed. An `:error`
        ;; replaces the return. So no tool-family branch is needed — surface
        ;; whichever is present. The ERROR envelope is the one internal
        ;; keyword-keyed shape rendered here — `error->display` renders it as
        ;; clean, LLM-legible text with REAL newlines (source excerpt + caret
        ;; kept readable, never an escaped one-line literal).
        form-output
        (fn [f]
          (cond (:summary? f) nil
                (:error f) (error->display (:error f))
                (some? (:result f)) (clip-wire (env/ctx->python-str (strip-echo-diffs (:result f))))
                (not (str/blank? (str (:stdout f)))) (clip-wire (:stdout f))
                :else nil))

        ;; ctx structural delta (executable `ctx["a"]["b"] = …` / `del ctx[…]`),
        ;; emitted only when ctx changed — rides the SAME message, append-only.
        ctx-diff
        (not-empty (some-> (:ctx-diff iter-record)
                           str
                           str/trim))

        tool-calls
        (seq (:tool-calls iter-record))

        ;; Forms grouped by the tool_use they answer. A form with no id (a
        ;; summarize/drop fold form, or a legacy unpaired form) folds onto the
        ;; FIRST call so nothing is lost.
        forms-by-id
        (group-by :svar/tool-call-id forms)

        orphan-forms
        (get forms-by-id nil)

        ;; Build the wire body for ONE tool-call from ITS OWN forms, plus the
        ;; iteration-level lines (folds / form-budget / ctx delta) carried on the
        ;; first call only (they describe the whole reply, not a single call).
        ;; RESULT HANDLE: a NATIVE tool call stores its return value keyed by THIS
        ;; tool_use id, so the model can re-read it later via `ntr[<id>]` without
        ;; re-running the tool. Surface the EXACT key inline (the id the model saw
        ;; on the wire can differ from the one vis stored — e.g. OpenAI Responses'
        ;; composite `call_id|item_id` — so we must hand it the literal key, not
        ;; let it guess). Emitted only when the call actually produced a stored
        ;; `:result` (python_execution prints, it doesn't store a return, so it
        ;; gets no handle).
        result-handle
        (fn [tc own]
          (when (and (:id tc) (some #(some? (:result %)) own))
            (str "# saved: ntr[" (pr-str (str (:id tc))) "] — re-read without re-running")))

        call-content
        (fn [idx tc]
          (let [own
                (cond-> (vec (get forms-by-id (:id tc)))
                  (zero? (long idx))
                  (into (or orphan-forms [])))

                lines
                (keep form-output own)

                iscope
                (some #(iter-of-scope (:scope %)) own)

                header
                (when (and iscope (seq lines)) (str "# " iscope))

                handle
                (when (seq lines) (result-handle tc own))

                body-ls
                (concat (when (zero? (long idx)) summary-lines)
                        (when header [header])
                        (when handle [handle])
                        lines)

                body
                (when (seq body-ls) (str/join "\n" body-ls))]

            (str/join "\n\n" (remove str/blank? [body (when (zero? (long idx)) ctx-diff)]))))

        ;; Legacy text fallback (a record with NO tool calls): all forms joined.
        fallback-content
        (let [lines
              (keep form-output forms)

              iscope
              (some #(iter-of-scope (:scope %)) forms)

              header
              (when (and iscope (seq lines)) (str "# " iscope))

              body-ls
              (concat summary-lines (when header [header]) lines)

              body
              (when (seq body-ls) (str/join "\n" body-ls))]

          (str/join "\n\n" (remove str/blank? [body ctx-diff])))]

    (cond
      ;; Collapsed by summarize/drop: the whole iteration is gone — emit ONLY the
      ;; gist line as plain text (conversation-suffix drops its assistant +
      ;; tool_result pair, so there is no tool_use to answer here).
      (:collapsed? iter-record) (when-let [body (not-empty (str/join "\n" summary-lines))]
                                  {:role "user" :content body})
      ;; Native/tool-call iteration: emit ONE `tool_result` per `tool_use` (the
      ;; API requires every call be answered), each carrying ITS OWN forms'
      ;; output. One of the calls may be python_execution, the rest direct
      ;; file tools, and each owns its result.
      tool-calls
      {:role "user"
       :content
       (vec
         (map-indexed
           (fn [idx tc]
             (let [own
                   (cond-> (vec (get forms-by-id (:id tc)))
                     (zero? (long idx))
                     (into (or orphan-forms [])))

                   ;; A tool call FAILED when any of its forms errored.
                   ;; Flag the tool_result `:is_error true` so the model
                   ;; treats it as a failure, not an empty success.
                   ;; svar passes it to Anthropic as `is_error: true`;
                   ;; on OpenAI/Gemini (no structured flag) the error TEXT
                   ;; in :content carries the signal.
                   errored?
                   (boolean (some :error own))

                   c
                   (call-content idx tc)]

               (cond->
                 {:type "tool_result"
                  :tool_use_id (:id tc)
                  :content
                  (if (str/blank? c)
                    "(no return — python_execution returns what it print()s; this call printed nothing. print() what you want to see.)"
                    c)}
                 errored?
                 (assoc :is_error true))))
           tool-calls))}
      ;; Legacy text fallback (no tool calls on this record).
      (not (str/blank? fallback-content)) {:role "user" :content fallback-content})))

(defn- strip-assistant-thinking
  "Cross-provider/model-SAFE version of a canonical assistant replay: drop
   the `thinking` / `redacted_thinking` blocks (opaque provider-native state
   — z.ai raw text, Anthropic HMAC, Responses encrypted items — none of
   which survive a provider/model switch) but KEEP the text and `tool_use`
   blocks, so the paired `<results>` tool_result message still answers a
   tool_use on the wire. Returns nil when nothing but thinking remains (an
   empty assistant message is a 400 on every wire)."
  [assistant-message]
  (when assistant-message
    (let [content (vec (remove #(contains? #{"thinking" "redacted_thinking"} (:type %))
                         (:content assistant-message)))]
      (when (seq content) (assoc assistant-message :content content)))))

(defn- attachment->image-block
  "Canonical multimodal image block for one stored iteration attachment. The
   `image_url` data-URI shape is svar's cross-wire canonical form — it
   translates to Anthropic `image` / OpenAI `image_url` / Gemini inline data
   per provider, and svar auto-flags the Copilot vision header when present."
  [{:keys [media-type base64]}]
  {:type "image_url"
   :image_url {:url (str "data:" (or (not-empty (str media-type)) "image/png") ";base64," base64)}})

(defn- target-supports-vision?
  "True when the replay `target` model advertises `:vision` in svar's per-model
   capability registry. Gates generated-figure replay so a text-only model is
   never handed image blocks (Copilot without vision, glm-5-turbo, deepseek …)."
  [target]
  (contains? (:capabilities (svar-router/infer-model-metadata {:name (str (:model target))}))
             :vision))

(defn- image-attachment?
  "True when a stored iteration attachment is an IMAGE — the only kind a vision
   model can consume as an `image_url` block. A generic `vis_attach` artifact
   (csv/json/pdf/wav/…) is DB- and display-only: it must never be handed to the
   provider as an image (a broken `data:text/csv;…` image block), so replay
   filters on the `image/` media-type (falling back to the coarse `:kind`)."
  [{:keys [media-type kind]}]
  (or (str/starts-with? (str media-type) "image/")
      (and (str/blank? (str media-type)) (= "image" (str kind)))))

(defn- iteration-image-message
  "A `{:role \"user\"}` message carrying every IMAGE a prior iteration's tool
   calls produced (matplotlib figures, `vis_attach`ed images), as canonical
   `image_url` blocks — the vision replay of generated artifacts, sourced from
   the iteration's persisted `:attachments` (non-image artifacts are skipped —
   see `image-attachment?`). nil when the iteration produced no images. Emitted
   as its OWN message right AFTER the iteration's `<results>` so an image never
   sits between an assistant `tool_use` and its answering `tool_result` (which
   would break tool-call adjacency on the OpenAI chat wire)."
  [iter-rec]
  (when-let [imgs (seq (filter image-attachment? (:attachments iter-rec)))]
    {:role "user" :content (mapv attachment->image-block imgs)}))


(defn- conversation-suffix
  "Append-only conversation suffix for the current turn: each prior iteration
   as an `[assistant-replay, <results> user message]` PAIR, in iteration
   order — the tool-call/tool-result shape (see the wire shape documented
   above). The assistant replay carries provider-native thinking payloads
   (signed Anthropic thinking / z.ai reasoning / Responses items) so the model
   keeps its reasoning session; the results message carries what running that
   iteration's code actually returned.

   A provider/model MISMATCH (mid-turn fallback, health-gate demotion making
   selected≠actual, model-name aliasing) must NOT blind the model: the old
   behaviour dropped the whole pair, so the model never saw its own tool
   results and re-issued the identical call every iteration. Now only the
   opaque THINKING is dropped (`strip-assistant-thinking`) — the tool_use +
   results still replay. When an entry has no assistant message at all
   (or nothing but thinking), its results degrade to a PLAIN TEXT user
   message (a tool_result with no answering tool_use is a wire error).

   Cross-turn seeds (`:preserved-thinking/replay? false`) stay fully
   excluded — their content already rides the frozen prior-turn rendering.

   Compatible entries route through `preserved-thinking-replay-messages`
   so the oversized-chain telemetry stays."
  [trailer-iters target]
  (let [iters
        (vec (or trailer-iters []))

        compatible
        (into #{} (map first) (compatible-preserved-thinking-trailer-iters iters target))

        ;; Generated figures replay only to a vision-capable target; a
        ;; text-only model gets the fence's summary/ASCII already carried in
        ;; the results text, never image blocks it can't consume.
        vision?
        (target-supports-vision? target)]

    (vec
      (mapcat
        (fn [[pos iter-rec :as entry]]
          (let [results
                (iteration-results-message iter-rec)

                ;; Image artifacts this iteration produced, as their OWN user
                ;; message appended AFTER the results (keeps tool_use/tool_result
                ;; adjacency intact). nil for text targets or image-less iters.
                img
                (when vision? (iteration-image-message iter-rec))

                +img
                (fn [msgs]
                  (cond-> (vec msgs)
                    img
                    (conj img)))]

            (cond
              ;; Collapse WINS over provenance: a `session_fold`/`session_drop`
              ;; that covered this iteration removes its whole assistant +
              ;; tool_result pair AND its generated image. The figure's vision
              ;; visibility TRACKS its iteration's textual visibility (one
              ;; invariant), so a folded step keeps only its one-line gist
              ;; (plain text) — real compaction, bytes and all. Checked BEFORE
              ;; the cross-turn seed branch so a folded seed also drops its
              ;; image; otherwise a prior-turn figure would be byte-immune to
              ;; compaction and re-billed to the vision model every turn.
              (:collapsed? iter-rec) (if results [results] [])
              ;; Cross-turn seed (NOT collapsed): opted out of THINKING/results
              ;; replay (its evidence lives in the frozen prior-turn context, so
              ;; emitting text here would double-render it). Produced IMAGE
              ;; artifacts are the exception: their bytes were NEVER wired to any
              ;; prior turn, so a vision model can only see a prior figure if we
              ;; emit it now — the standalone `img` message (no thinking, no
              ;; results).
              (false? (:preserved-thinking/replay? iter-rec)) (if img [img] [])
              ;; Same provider+model, valid signature → verbatim replay
              ;; with the full thinking chain.
              (contains? compatible pos)
              (+img (let [replay (first (preserved-thinking-replay-messages [entry]))]
                      (cond-> [replay]
                        results
                        (conj results))))
              ;; Mismatched provider/model or poisoned signature: replay
              ;; SANS thinking so the tool_use ids stay answerable, then
              ;; the results.
              :else (if-let [stripped (strip-assistant-thinking (:assistant-message iter-rec))]
                      (+img (cond-> [stripped]
                              results
                              (conj results)))
                      ;; No assistant message (errored before one landed) or
                      ;; nothing but thinking: no tool_use to answer — degrade
                      ;; the results to plain text.
                      (if-let [textual (iteration-results-message (dissoc iter-rec :tool-calls))]
                        (+img [textual])
                        [])))))
        iters))))

(defn- form-wire-chars
  "Approximate the wire SIZE (chars) one form contributes — error / native result
   / stdout — capped at MAX_FORM_WIRE_CHARS exactly as the real renderer clips it,
   so a giant read can't over-rank itself. Strings count directly; a non-string
   result is serialized the way the wire renders it. Pure."
  [f]
  (let [n (long (cond (:summary? f) 0
                      (some? (:error f)) (count (str (:error f)))
                      (some? (:result f))
                      (let [r (:result f)]
                        (if (string? r) (count r) (count (env/ctx->python-str r))))
                      :else (count (str (:stdout f)))))]
    (long (min n (long MAX_FORM_WIRE_CHARS)))))

;; ── Native tool surface (maki-style HYBRID) ──────────────────────────────────
;; Prefer `python_execution` for batched / filtered / chained workflows; a single
;; simple operation may use its native tool directly. Each native schema mirrors
;; the SAME function bound into GraalPy, and native dispatch synthesizes a call to
;; that function, so both surfaces share confinement, anchors, rendering, and
;; docs. Explicitly native-only handler tools are the exception. Replying with
;; plain text and NO tool call ends the turn.

(defn- python-execution-capability-line
  "The ONE line that tells the model what the sandbox can actually reach THIS
   session — built from real capabilities (`caps` = `:sandbox-caps` on the env), so
   the description never claims filesystem/network the sandbox doesn't have. `caps`
   nil (no env wired) ⇒ no line (don't assert capabilities we can't confirm)."
  [caps]
  (when caps
    (let
      [net
       (:network caps)

       net-on?
       (boolean (:enabled? net))

       allowed
       (seq (remove #(= "*" (str %)) (:allowed-domains net)))

       star?
       (some #(= "*" (str %)) (:allowed-domains net))

       fs-part
       (if (:fs? caps)
         "Filesystem: REAL access (open/read/write, os.walk, glob), CONFINED to the filesystem roots (outside ⇒ PermissionError) — still prefer cat/rg/patch for plain reads/edits."
         "Filesystem: none (no workspace) — read/edit via the cat/rg/patch tools.")

       net-part
       (cond (not net-on?) "Network: off."
             allowed (str "Network: on, reachable hosts: " (str/join ", " allowed) ".")
             star? "Network: on (any host except blocked defaults)."
             :else "Network: on.")]

      (str fs-part " " net-part))))

(defn- python-execution-tool
  "The engine-level `python_execution` tool schema. Preferred for batched,
   transformed, filtered, chained, and structural workflows so intermediate data
   never lands in context. Active engine-bound native tools share this sandbox;
   explicitly native-only handlers are the exception. The capability line is
   built from `caps` so fs/network claims match what the sandbox can actually do."
  [caps]
  {:name "python_execution"
   :description (str
                  "Execute Python in the session's persistent sandbox to TRANSFORM / FILTER / "
                  "CHAIN tool results in one shot — its RETURN is the text it print()s (the "
                  "last-expression value is NOT returned, so print() what you want back). State "
                  "(vars, imports, defs) persists across calls AND turns. Active engine-bound "
                  "native tools are bare snake_case functions here; explicitly native-only tools "
                  "and python_execution itself are exceptions. Action tools are async: `await` "
                  "them, and use `await gather(a, b)` for independent calls. `apropos` and `doc` "
                  "are synchronous. Prefer this surface for multi-tool or structural work; use a "
                  "direct native call for one simple operation."
                  (when-let [cap (python-execution-capability-line caps)]
                    (str "\n" cap)))
   :schema {:type "object"
            :properties {"code" {:type "string"
                                 :description "Python source to execute in the sandbox."}}
            :required ["code"]}})

(defn- session-fold-tool
  "Engine-level `session_fold` native-tool schema — the context-compaction verb
   advertised as a first-class tool_use. It is ALSO callable bare / inside
   `python_execution` (the SAME bound sandbox verb from `compaction-verbs`);
   native dispatch just synthesizes `session_fold(target, gist)` into that verb
   (see the injected call-shape), so ONE definition drives both surfaces and the
   ctx-atom closure is reused — no separate Clojure handler."
  []
  {:name "session_fold"
   :description (str "Compact COMPLETED PRIOR-TURN steps out of the conversation: fold "
                     "the named steps (each tagged `# tN/iN` in its result) off the wire, "
                     "replaced by ONE optional gist line. `target` selects what to fold; "
                     "`gist` is the single takeaway kept in their place — its RATIONALE: what "
                     "the steps ESTABLISHED and why they are safe to drop — OMIT it to drop the "
                     "steps outright. Folds are idempotent and superseding: a broader re-fold "
                     "replaces a finer one (one breadcrumb, never a stack). Folding is WIRE-ONLY: "
                     "it NEVER deletes from the DB, so it is always reversible — a folded native "
                     "tool result stays fetchable by its id via `ntr[...]` (this turn or a past "
                     "turn). Current and future turn scopes are rejected so live reproduction, "
                     "anchors, edits, and verification cannot disappear. Same verb is callable inside python_execution as "
                     "`session_fold(target, gist)`.")
   :schema
   {:type "object"
    :properties
    {"target" {:description
               (str "What to fold. Either a LIST of step ids like [\"t2/i3\", \"t2/i4\"] "
                    "(a bare \"t2\" in the list folds that WHOLE turn), OR a selector "
                    "object: {\"through\": \"tN/iN\"} folds every step up to and INCLUDING "
                    "that one; {\"from\": \"tA/iA\", \"to\": \"tB/iB\"} an inclusive window "
                    "(either bound optional); {\"since\": \"tN/iN\"} that step through the "
                    "newest.")}
     "gist" {:type "string"
             :description
             (str "Optional one-line takeaway kept in place of the folded steps — the "
                  "RATIONALE for the fold: what the steps established and why they are done, "
                  "anchored (e.g. \"http timeout @ http.py:52\"). OMIT to drop the steps "
                  "with no summary line.")}}
    :required ["target"]}})

(defn- apropos-tool
  "Engine-level native schema for the sandbox's existing `apropos(query)`
   discovery function. Native dispatch synthesizes the same Python call, so the
   direct and `python_execution` surfaces always list the same live bindings."
  []
  {:name "apropos"
   :description (str "List available Python sandbox tools as a compact "
                     "markdown `| tool | gist |` table. Omit `query` to list all tools. "
                     "The same tools are also callable inside python_execution via "
                     "`apropos(query)`, which returns a `{name: gist}` dict for filtering; "
                     "use `doc` for one tool's full contract.")
   :schema {:type "object"
            :properties {"query" {:type "string"
                                  :description "Optional substring used to filter tool names."}}}})

(defn- doc-tool
  "Engine-level native schema for the sandbox's existing `doc(name)` function.
   Native dispatch synthesizes the same Python call; documentation therefore
   stays sourced from the live sandbox registry rather than a copied table."
  []
  {:name "doc"
   :description (str "Show one Python sandbox tool's exact callable contract, "
                     "including arguments, result shape, and mechanics. This is "
                     "the same `doc(name)` function available inside python_execution.")
   :schema {:type "object"
            :properties {"name" {:type "string" :description "Exact tool name from apropos."}}
            :required ["name"]}})

(def ^:private engine-native-tool-call-shapes
  {"apropos" {:py-name "__vis_apropos_table__" :opt-pos ["query"]}
   "doc" {:pos ["name"]}
   "session_fold" {:pos ["target"] :opt-pos ["gist"]}})

(defn- native-tools
  "The native tool surface advertised to the model: the file tools declared via
   each extension's `vis/symbol` `:native-tool` opt (single source of truth —
   schema lives WITH the symbol), plus native `apropos` / `doc`, context folding,
   and `python_execution`. Order: extension tools, discovery tools, session_fold,
   python_execution. `caps` (`:sandbox-caps` from the env) tailors the latter's
   fs/network line."
  [active-extensions caps env]
  (conj (extension/native-tool-schemas active-extensions env)
        (apropos-tool)
        (doc-tool)
        (session-fold-tool)
        (python-execution-tool caps)))

(defn- py-literal
  "Render a JSON-ish value as a PYTHON literal string (`True`/`False`/`None`,
   quoted strings, lists, dicts) so a native tool-call's structured arguments can
   be synthesized into a call to the bound Python tool fn. JSON's lowercase
   true/false/null are NOT valid Python, hence this rather than raw JSON.
   STRINGS-ONLY: a keyword/symbol here means a producer leaked one past
   `normalize-tool-input` / a `:call` shape fn — throw, never render `:kw`
   into Python source."
  [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (or (keyword? v) (symbol? v)) (env/boundary-violation! :keyword-value v ["py-literal"])
        (string? v) (str \"
                         (-> ^String v
                             (str/replace "\\" "\\\\")
                             (str/replace "\"" "\\\"")
                             (str/replace "\n" "\\n")
                             (str/replace "\r" "\\r")
                             (str/replace "\t" "\\t"))
                         \")
        (map? v) (str "{"
                      (str/join ", "
                                (map (fn [[k val]]
                                       (str (py-literal (str k)) ": " (py-literal val)))
                                     v))
                      "}")
        (sequential? v) (str "[" (str/join ", " (map py-literal v)) "]")
        (integer? v) (str v)
        (number? v) (str v)
        :else (py-literal (str v))))

(defn- normalize-tool-input
  "EXTERNAL-EDGE ADAPTER (svar wire): svar's JSON parse may hand tool args with
   keyword OR string keys depending on the provider adapter. This is the ONE
   sanctioned conversion point — everything downstream (synth-call, py-literal,
   the sandbox) is strings-only. `normalize-dict-key` additionally strips a
   model-drift leading colon (`\":path\"`) so positional extraction still finds
   the key and the call just works.

   DEEP: keys are normalized at EVERY depth, not just the top level. Tools like
   `patch` carry NESTED dicts (`edits [{:from_anchor …}]`); a shallow pass left
   the model-drift colon on those nested keys, so the synthesized Python call
   leaked `patch([{\":from_anchor\": …}])`. Only KEYS are touched — values (edit
   `replace` text, anchors, paths) pass through verbatim."
  [input]
  (letfn [(nk [k] (env/normalize-dict-key (if (keyword? k) (name k) (str k))))
          (walk [x]
            (cond (map? x) (into {}
                                 (map (fn [[k v]]
                                        [(nk k) (walk v)]))
                                 x)
                  (or (vector? x) (seq? x) (set? x)) (mapv walk x)
                  :else x))]
    (walk (or input {}))))

(defn- synth-call
  "Synthesize `py-name(args…)` from a native tool's `:call` SHAPE map + the tool
   input (positional-args contract). Shape keys (all optional):
     :py-name   bound python name override (default: wire name `nm`)
     :lead-opt  one optional leading positional key — emitted only when present
     :pos       required positional keys, in order
     :opt-pos   trailing optional positional keys — each emitted only when present
     :rest      :opt    → append remaining keys as a dict, OMITTED when empty
                :always → always append the remaining-keys dict (even when empty)
   Every input key NOT consumed by :lead-opt/:pos/:opt-pos falls into the :rest
   dict. `py-literal` escapes each argument, so any payload (Clojure source, quotes,
   newlines, unicode) round-trips as a valid Python literal."
  [nm shape input]
  (let [py-name
        (or (:py-name shape) nm)

        lead
        (:lead-opt shape)

        pos
        (:pos shape)

        opt-pos
        (:opt-pos shape)

        consumed
        (cond-> (set pos)
          lead
          (conj lead)

          (seq opt-pos)
          (into opt-pos))

        rest-map
        (apply dissoc input consumed)

        args
        (concat (when (and lead (contains? input lead)) [(py-literal (get input lead))])
                (map #(py-literal (get input %)) pos)
                (keep #(when (contains? input %) (py-literal (get input %))) opt-pos)
                (case (:rest shape)
                  :always
                  [(py-literal rest-map)]

                  :opt
                  (when (seq rest-map) [(py-literal rest-map)])

                  nil))]

    (str py-name "(" (str/join ", " args) ")")))

(defn- tool-call->python-source
  "Synthesize the Python a native tool-call runs. A native tool becomes a BARE call
   into its already-bound async fn (`cat(\"x\", {...})`) — it auto-runs and its
   result becomes the tool_result, reusing the whole GraalPy execution + confinement
   + render pipeline.

   The call SHAPE is DATA on the tool's symbol (`:ext.symbol/call`, projected into
   `shapes` as wire-name → shape-map-or-fn) — the engine holds NO per-tool list, so
   a new module's tool works by default or by declaring its own shape. A shape map
   is interpreted by `synth-call`; a `(fn [input] -> {:args [raw-vals] :py-name?})`
   is an escape hatch for the two irreducible ones (`patch` reshape, `ls` path
   default) — it returns RAW argument values (no `py-literal`, so tool namespaces
   need no engine dependency) which THIS fn renders. A tool with NO shape → the
   generic `name({…whole input…})` form (correct for struct_patch, struct_rename,
   rg, find_files, struct_occurrences, struct_index …).

   `python_execution` is the ONE engine tool (not a symbol): its `code` really IS a
   Python program, passed through verbatim. This is deliberately the ONLY `code`
   special-case — a native tool's `code` is a PAYLOAD (replacement source), NOT a
   program, so it must ride escaped inside the call, never be dumped as the program."
  [shapes tc]
  (let [nm
        (:name tc)

        input
        (normalize-tool-input (:input tc))]

    (if (= nm "python_execution")
      (or (get input "code") "")
      (let [shape (get shapes nm)]
        (cond (fn? shape) (let [{:keys [py-name args]} (shape input)]
                            (str (or py-name nm) "(" (str/join ", " (map py-literal args)) ")"))
              (map? shape) (synth-call nm shape input)
              :else (str nm "(" (py-literal input) ")"))))))

;; ---------------------------------------------------------------------------
;; Prompt-cache breakpoints (Anthropic `cache_control`; OpenAI-style strips the
;; marker and uses implicit prefix caching). TWO breakpoints, pi/maki-style:
;;   1. the last SYSTEM message — the FROZEN `session={…}` prefix (long-lived,
;;      stable across turns thanks to :standing-ctx-atom), and
;;   2. the LAST message overall — a MOVING recency breakpoint that caches the
;;      append-only transcript up to here (grows each iteration).
;; Manual placement makes svar skip its auto-cache-last-system-block (so we own
;; both slots); svar honours ≤4 breakpoints per call.
;; ---------------------------------------------------------------------------
(defn- tag-block-cached
  "Mark the LAST content block of message `m` with `:svar/cache true`. Coerces a
   bare-string `:content` into a text block first; leaves other shapes untouched."
  [m]
  (let [content (:content m)]
    (cond (string? content) (assoc m :content [{:type "text" :text content :svar/cache true}])
          (and (vector? content) (seq content))
          (let [i (dec (count content))
                blk (nth content i)
                blk (if (map? blk)
                      (assoc blk :svar/cache true)
                      {:type "text" :text (str blk) :svar/cache true})]

            (assoc m :content (assoc content i blk)))
          :else m)))

(defn- apply-cache-breakpoints
  "Place the two prompt-cache breakpoints on `messages`: the last `:role
   \"system\"` message (frozen prefix) and the last message overall (moving
   recency). No-op on empty; idempotent when both land on one message (first
   call, before any transcript exists)."
  [messages]
  (let [messages (vec messages)]
    (if (empty? messages)
      messages
      (let [last-sys (last (keep-indexed (fn [i m]
                                           (when (= "system" (:role m)) i))
                                         messages))
            messages (cond-> messages
                       last-sys
                       (update last-sys tag-block-cached))]

        (update messages (dec (count messages)) tag-block-cached)))))

(defn- live-code-from-tool-input
  "Best-effort decode of the `code` argument from a `run_python` tool call's
   streaming (and therefore possibly truncated) argument JSON, so the live
   bubble can paint the Python as the model writes it. Native tool calling puts
   ALL of the model's work in the tool-call arguments (the assistant text is
   empty), so without this the live stream shows only reasoning.

   Scans for the FIRST `\"code\": \"` key (the value precedes any occurrence
   inside the code itself), then JSON-unescapes the string value until the
   closing quote OR the end of the truncated buffer. Returns nil when the
   `code` value has not started streaming yet."
  ^String [^String tool-input]
  (when-not (str/blank? tool-input)
    (when-let [m (re-find #"\"code\"\s*:\s*\"" tool-input)]
      (let [start (+ (long (str/index-of tool-input m)) (count m))
            n (count tool-input)
            sb (StringBuilder.)]

        (loop [i start]
          (if (>= i n)
            (str sb)                  ;; truncated mid-value
            (let [c (.charAt tool-input i)]
              (cond (= c \") (str sb) ;; closing quote — value complete
                    (= c \\) (if (>= (inc i) n)
                               (str sb)
                               (let [e (.charAt tool-input (inc i))]
                                 (case e
                                   \n
                                   (do (.append sb \newline) (recur (+ i 2)))

                                   \t
                                   (do (.append sb \tab) (recur (+ i 2)))

                                   \r
                                   (do (.append sb \return) (recur (+ i 2)))

                                   \b
                                   (do (.append sb \backspace) (recur (+ i 2)))

                                   \f
                                   (do (.append sb \formfeed) (recur (+ i 2)))

                                   \"
                                   (do (.append sb \") (recur (+ i 2)))

                                   \\
                                   (do (.append sb \\) (recur (+ i 2)))

                                   \/
                                   (do (.append sb \/) (recur (+ i 2)))

                                   \u
                                   (if (>= (+ i 6) n)
                                     (str sb) ;; truncated escape
                                     (do (.append sb
                                                  (char (Integer/parseInt
                                                          (subs tool-input (+ i 2) (+ i 6))
                                                          16)))
                                         (recur (+ i 6))))

                                   (do (.append sb e) (recur (+ i 2))))))
                    :else (do (.append sb c) (recur (inc i)))))))))))

(defn- live-tool-code-markdown
  "Append-only Markdown projection for cumulative native-tool code. While the
   provider is still streaming, leave the fence open; append the closing fence
   only on the terminal frame. Closing and reopening it on every cumulative
   chunk moves the suffix and corrupts append-only gateway delta math."
  [code done?]
  (str "```python\n" code (when done? "\n```")))

(defn- prose-beyond-code
  "The assistant `prose` (a model `:content` string streamed ALONGSIDE a tool
   call) is worth showing ONLY when it carries commentary BEYOND the code it's
   about to run. Models frequently restate the exact `run_python` code in their
   message — as a ```fenced``` block or verbatim — which then renders as a dim
   DUPLICATE of the real code block. So strip any fenced code from the prose and
   compare what's left (and the whole prose, de-whitespaced) against the
   concatenated tool-call code; return the prose when it still says something,
   else nil. `tool-calls` are the native tool calls; their `:input` carries
   `code`."
  [prose tool-calls]
  (when-let [p (some-> prose
                       str
                       str/trim
                       not-empty)]
    (let [code (->> tool-calls
                    (map (fn [tc]
                           (or (:code (:input tc)) (get (:input tc) "code") "")))
                    (str/join "\n"))
          squash #(str/replace (str %) #"\s+" "")
          fenced-stripped (-> p
                              (str/replace #"(?s)```.*?```" "")
                              str/trim)]

      (when-not (or (str/blank? fenced-stripped)  ;; prose was ONLY fenced code
                    (= (squash p) (squash code))) ;; prose IS the code verbatim
        p))))

(defn run-iteration
  "Runs a single RLM iteration: ask! -> check final -> execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc."
  [environment messages &
   [{:keys [routing iteration reasoning-level reasoning-effort resolved-model on-chunk extra-body
            llm-headers active-extensions answer-validation-context]}]]
  (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :run-iteration})]
    (let [iteration-position (inc (long (or iteration 0)))
          turn-prefix (runtime-turn-prefix environment)
          turn-position (or (:turn-position (ctx-loop/read-turn-state environment)) 1)
          form-scope (fn [idx]
                       (str "t" turn-position "/i" iteration-position "/f" (inc (long idx))))
          effective-reasoning (when (and (nil? reasoning-effort)
                                         (some? reasoning-level)
                                         (reasoning-effort-configurable? resolved-model))
                                (or (normalize-reasoning-level reasoning-level)
                                    (throw (ex-info "Invalid :reasoning-level."
                                                    {:type :vis/invalid-reasoning-level
                                                     :got reasoning-level}))))
          turn-state-atom (or (:turn-state-atom environment)
                              (throw (ex-info "environment missing :turn-state-atom"
                                              {:type :vis/missing-turn-state-atom})))
          ;; Reset this iteration's answer + form-index pointer on the single
          ;; turn-state-atom. finalize-answer! sets :answer during eval (an
          ;; answer reply); the FINAL path reads it back after all forms run.
          _ (swap! turn-state-atom assoc :answer nil :form-idx nil)
          ;; Stream reasoning chunks to the TUI while the LLM is
          ;; thinking. Every chunk carries `:phase` - consumers
          ;; dispatch on it. Phases:
          ;;   :reasoning      - LLM streaming reasoning text
          ;;   :form-start     - the block started evaluating
          ;;   :form-result    - the block finished evaluating
          ;;   :iteration-final - iteration complete (final-result
          ;;                      or normal end-of-iteration marker)
          ;;
          ;; Reasoning DELTA contract: providers (via svar) emit `:reasoning`
          ;; as the FULL accumulated reasoning text on every SSE tick.
          ;; Forwarding that verbatim makes append-only consumers (CLI
          ;; trace, JSON/EDN trace streams) re-emit the entire growing
          ;; block on every tick — the screenshotted "sending and sending"
          ;; bug. We close that here at the producer: track per-iteration
          ;; accumulated length, compute `:delta` (just the new tail) and
          ;; ship it alongside `:thinking` (still the full accumulated text
          ;; for redraw-style consumers like the TUI timeline). Consumers
          ;; that want append-only streaming append `:delta`; the others
          ;; ignore it and read `:thinking` as before.
          reasoning-len-volatile (volatile! 0)
          content-len-volatile (volatile! 0)
          tool-code-len-volatile (volatile! 0)
          tool-code-volatile (volatile! nil)
          reset-stream-state! (fn []
                                (vreset! reasoning-len-volatile 0)
                                (vreset! content-len-volatile 0)
                                (vreset! tool-code-len-volatile 0)
                                (vreset! tool-code-volatile nil))
          streaming-fn
          (when on-chunk
            (fn [{:keys [reasoning content tool-input done?] :as chunk}]
              (cond (:event/type chunk) (on-chunk {:phase :provider-fallback
                                                   :iteration iteration-position
                                                   :event chunk})
                    :else (do (when (or (some? reasoning) done?)
                                (let [thinking (some-> reasoning
                                                       str)
                                      prev-len (long @reasoning-len-volatile)
                                      cur-len (long (count (or thinking "")))
                                      delta (cond (nil? thinking) nil
                                                  (< cur-len prev-len) thinking
                                                  (= cur-len prev-len) ""
                                                  :else (subs thinking prev-len))]

                                  (vreset! reasoning-len-volatile cur-len)
                                  (on-chunk {:phase :reasoning
                                             :iteration iteration-position
                                             :thinking thinking
                                             :delta delta
                                             :done? (boolean done?)})))
                              (when (some? content)
                                ;; Stream provider content (the answer
                                ;; markdown) so the bubble surfaces live
                                ;; progress between reasoning and parsed
                                ;; forms. Same delta math as
                                ;; reasoning; consumers redraw or append.
                                (let [content-s (some-> content
                                                        str)
                                      prev-len (long @content-len-volatile)
                                      cur-len (long (count (or content-s "")))
                                      delta (cond (nil? content-s) nil
                                                  (< cur-len prev-len) content-s
                                                  (= cur-len prev-len) ""
                                                  :else (subs content-s prev-len))]

                                  (vreset! content-len-volatile cur-len)
                                  (on-chunk {:phase :content
                                             :iteration iteration-position
                                             :content content-s
                                             :delta delta
                                             :done? (boolean done?)})))
                              ;; Native tool calling: the model's Python is the
                              ;; tool-call arguments, not text content. Decode the
                              ;; live `code` value and stream it as content (a
                              ;; python code block) so the live bubble paints the
                              ;; code being written. Skipped once real text content
                              ;; (a plain-text answer reply) is present.
                              (when (and (str/blank? (or content ""))
                                         (or (some? tool-input)
                                             (and done? (some? @tool-code-volatile))))
                                (when-let [code (or (some-> tool-input
                                                            live-code-from-tool-input)
                                                    @tool-code-volatile)]
                                  (when-not (str/blank? code)
                                    (let [prev-len (long @tool-code-len-volatile)
                                          cur-len (long (count code))
                                          delta (cond (< cur-len prev-len) code
                                                      (= cur-len prev-len) ""
                                                      :else (subs code prev-len))]

                                      (vreset! tool-code-len-volatile cur-len)
                                      (vreset! tool-code-volatile code)
                                      (on-chunk {:phase :content
                                                 :iteration iteration-position
                                                 :content (live-tool-code-markdown code done?)
                                                 :delta delta
                                                 :done? (boolean done?)})))))))))
          copilot-initiator (copilot-initiator-for-iteration iteration)
          effective-llm-headers
          (not-empty (merge (copilot-llm-headers resolved-model copilot-initiator) llm-headers))
          provider-started-at-ms (System/currentTimeMillis)
          _ (when on-chunk
              (on-chunk {:phase :provider-call
                         :iteration iteration-position
                         :started-at-ms provider-started-at-ms}))
          provider-start-ns (System/nanoTime)
          ;; Phase C: per-session cache-key for OpenAI / Codex / Z.ai
          ;; sticky routing. svar 0.6.x auto-generates a key from the
          ;; system-prompt SHA1 prefix when caller omits this, but
          ;; passing the session-soul-id explicitly is better:
          ;; (1) sticky AT SESSION GRANULARITY across system-prompt
          ;;     micro-changes (e.g. extension reload, AGENTS.md edit)
          ;;     so the inference engine stays warm even when the
          ;;     auto-hash would otherwise rotate.
          ;; (2) cross-turn cache reuse within the same session (auto-
          ;;     hash regenerates if any system byte changed; explicit
          ;;     session-id pins routing regardless).
          ;; (3) Anthropic wire strips the field at body-build time so
          ;;     it's a no-op there — cheap to set unconditionally.
          session-cache-key (some-> (:session-id environment)
                                    str)
          ;; Phase E: sticky routing default. Cross-provider fallback
          ;; (svar `:on-transient-error :hybrid`) poisons cache prefixes:
          ;; an Anthropic 15K cached prefix becomes worthless the moment
          ;; the next call lands on OpenAI/Z.ai, and the OpenAI cache is
          ;; empty so we pay full input rate to bootstrap. Keep fallback
          ;; within the active provider unless the caller explicitly
          ;; overrides.
          base-routing (or routing {})
          sticky-routing (cond-> base-routing
                           (not (contains? base-routing :on-transient-error))
                           (assoc :on-transient-error :fallback-model-in-the-same-provider))
          ;; svar's empty-reply resend ladder (same model, same request) is
          ;; invisible mid-call — collect each re-send here and surface it as
          ;; a typed routing-trace event so the UI shows what the heal cost
          ;; instead of silence.
          empty-reply-resend-events (atom [])
          ask-opts
          (rt/with-default-ask-code-idle-timeout
            (cond-> {;; Native tool calling (codex/maki model): the model
                     ;; takes every action by calling `run_python` with a
                     ;; Python program; a reply with NO tool call is the
                     ;; final answer (its text). svar returns
                     ;; {:stop-reason :tool-calls|:end :tool-calls :content
                     ;; :assistant-message}.
                     :tools (native-tools active-extensions (:sandbox-caps environment) environment)
                     :tool-choice :auto
                     ;; two prompt-cache breakpoints: frozen system prefix
                     ;; + moving recency (transcript). See apply-cache-breakpoints.
                     :messages (apply-cache-breakpoints messages)
                     :routing sticky-routing
                     :check-context? true
                     :preserved-thinking? true
                     :on-empty-reply-resend
                     (fn [{:keys [attempt max-resends delay-ms]}]
                       (swap! empty-reply-resend-events conj
                         (cond-> {:event/type :llm.routing/provider-retry
                                  :reason :empty-content
                                  :attempt attempt
                                  :max-resends max-resends
                                  :delay-ms delay-ms}
                           (:provider resolved-model)
                           (assoc :from-provider (name (:provider resolved-model)))

                           (:name resolved-model)
                           (assoc :from-model (str (:name resolved-model))))))}
              session-cache-key
              (assoc :cache-key session-cache-key)

              effective-reasoning
              (assoc :reasoning effective-reasoning)

              reasoning-effort
              (assoc :reasoning-effort reasoning-effort)

              streaming-fn
              (assoc :on-chunk streaming-fn)

              effective-llm-headers
              (assoc :llm-headers effective-llm-headers)

              extra-body
              (assoc :extra-body extra-body)

              ;; Caller-driven cancellation (svar 0.7.19+): a no-arg
              ;; predicate svar polls on a watchdog so a user Stop
              ;; aborts the in-flight SSE read in ~50ms (close the
              ;; body stream + interrupt) instead of waiting for the
              ;; whole response or a 30s/120s timeout. Reads the same
              ;; cancel-atom `vis/cancel!` flips.
              (:cancel-atom environment)
              (assoc :cancel-fn
                (let [ca (:cancel-atom environment)]
                  (fn []
                    (boolean (deref ca)))))))
          ask-result-raw (binding [svar-llm/*log-context* (assoc svar-llm/*log-context*
                                                            :session-turn-id (:environment-id
                                                                               environment)
                                                            :iteration iteration-position)]
                           (call-provider-with-stream-rewind-retry!
                             environment
                             {:iteration-position iteration-position
                              :provider (some-> (:provider resolved-model)
                                                name)
                              :model (some-> (:name resolved-model)
                                             str)
                              :on-chunk on-chunk
                              :reset-stream-state! reset-stream-state!}
                             #(call-provider-with-interrupt-retry!
                                environment
                                iteration-position
                                (fn []
                                  (svar/ask-code! (:router environment) ask-opts)))))
          ask-result (prepend-routing-trace ask-result-raw @empty-reply-resend-events)
          code-observation (ask-code-block-observation ask-result)
          provider-duration-ms (elapsed-ms provider-start-ns)
          _ (log-stage! :provider-call/stop
                        iteration
                        (merge {:duration-ms provider-duration-ms
                                :raw-length (count (or (:raw ask-result-raw) ""))
                                :tokens (:tokens ask-result-raw)
                                :fallback? (boolean (some #(not= :llm.routing/provider-retry
                                                                 (:event/type %))
                                                          (:routed/trace ask-result-raw)))}
                               code-observation))
          parse-started-at-ms (System/currentTimeMillis)
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :start
                         :iteration iteration-position
                         :started-at-ms parse-started-at-ms
                         :provider-duration-ms provider-duration-ms
                         :raw-length (count (or (:raw ask-result-raw) ""))
                         :form-count (:form-count code-observation)
                         :code-observation code-observation}))
          model-reasoning (:reasoning ask-result)
          thinking model-reasoning
          _ (log-stage! :llm-response
                        iteration
                        (merge {:has-reasoning (some? model-reasoning)
                                :raw-length (count (or (:raw ask-result) ""))
                                :duration-ms (:duration-ms ask-result)
                                :provider-duration-ms provider-duration-ms
                                :tokens (:tokens ask-result)
                                :thinking thinking}
                               code-observation))
          api-usage (ask-result->api-usage ask-result)
          reasoning-effort-resolution (:routed/reasoning-effort ask-result)
          ;; Native tool calling: the model either CALLS `run_python`
          ;; (`:stop-reason :tool-calls`) or, with NO tool call
          ;; (`:stop-reason :end`), returns its final answer as `:content`.
          ;; An answer reply finalizes the turn directly (finalize-answer!
          ;; records it; the FINAL path below stores/renders it) and runs no
          ;; code. A tool-call reply becomes the executable blocks: one
          ;; `run_python` call → one block, carrying the tool_use `:id` so the
          ;; driver can pair its result into a `tool_result` message.
          tool-calls (vec (:tool-calls ask-result))
          ;; The model can return PROSE (`:content`) ALONGSIDE a tool call — its
          ;; commentary while it acts. Capture it ALWAYS: with no tool calls it IS
          ;; the final answer; WITH tool calls it's assistant prose shown above the
          ;; code (previously dropped — only the code rendered, the markdown lost).
          prose-md (some-> (:content ask-result)
                           str
                           str/trim
                           not-empty)
          answer-md (when (and (empty? tool-calls) (= :end (:stop-reason ask-result))) prose-md)
          ;; Show the prose ONLY when it adds something the code doesn't already
          ;; say — otherwise it's a dim duplicate of the run_python block.
          assistant-prose (when (seq tool-calls) (prose-beyond-code prose-md tool-calls))
          _ (when answer-md (finalize-answer! environment answer-md))
          _ (when (and assistant-prose on-chunk)
              (on-chunk
                {:phase :assistant-prose :iteration iteration-position :text assistant-prose}))
          ;; Native tools whose symbol declares a `:native-tool :handler` run
          ;; DIRECTLY in Clojure (no synthesized Python). All others synthesize a
          ;; bare call into their bound fn and run through GraalPy as before.
          native-handlers (extension/native-tool-handlers active-extensions environment)
          ;; Per-tool call SHAPES (wire-name → shape-map-or-fn), projected from each
          ;; symbol's `:ext.symbol/call`. The synthesizer holds no per-tool list.
          call-shapes (merge (extension/native-tool-call-shapes active-extensions environment)
                             engine-native-tool-call-shapes)
          blocks (if answer-md
                   []
                   (mapv (fn [tc]
                           (if-let [h (get native-handlers (:name tc))]
                             ;; a `:handler` native tool: dispatched in Clojure,
                             ;; never runs as Python. `:source` is the synthesized
                             ;; call for DISPLAY/persist/validation only (blocks
                             ;; require a string `:code`; a nil here failed
                             ;; `validate-iteration-blocks!` and errored the whole
                             ;; iteration on every handler-tool call) — execution
                             ;; branches on `:vis/native-handler` before `:source`
                             ;; is ever evaluated.
                             {:lang "native"
                              :source (tool-call->python-source call-shapes tc)
                              :svar/tool-call-id (:id tc)
                              :vis/tool-name (:name tc)
                              :vis/native-handler h
                              :vis/native-input (normalize-tool-input (:input tc))}
                             {:lang "python"
                              ;; Native file tool → synthesized bare call into its
                              ;; bound fn; python_execution → the model's own code.
                              :source (tool-call->python-source call-shapes tc)
                              :svar/tool-call-id (:id tc)
                              ;; Carry the tool name so the render layer can paint a
                              ;; native tool nicely vs. python_execution stdout.
                              :vis/tool-name (:name tc)}))
                         tool-calls))
          preflight-start-ns (System/nanoTime)
          preflight-result (if answer-md
                             {:code-entries [] :normalized-code "" :raw-fence-preflight-error nil}
                             (code-entries-preflight iteration-position blocks))
          preflight-duration-ms (elapsed-ms preflight-start-ns)
          {:keys [code-entries normalized-code]} preflight-result
          _ (log-stage! :response-preflight/stop
                        iteration
                        (merge {:duration-ms preflight-duration-ms
                                :code-length (count normalized-code)
                                :forms (count code-entries)
                                :raw-fence-preflight? (boolean (:raw-fence-preflight-error
                                                                 preflight-result))}
                               code-observation))
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :done
                         :iteration iteration-position
                         :duration-ms preflight-duration-ms
                         :code-length (count normalized-code)
                         :forms (count code-entries)
                         :code-observation code-observation}))
          ;; No structural answer-gate: a tool-call reply is always real work to
          ;; run; the answer arrives as plain text (`:stop-reason :end`) and is
          ;; finalized before any forms are built, so this path only ever runs
          ;; executable tool code.
          suppress-form-start? (some :vis/preflight-error code-entries)
          total-blocks (count code-entries)
          ;; per-tool result renderers (symbol `:native-tool :render`), looked up
          ;; once for this iteration — form-result-display applies them so a native
          ;; tool's result shows as a clean card, unified across TUI + web.
          ;; `session_fold` is ENGINE-level (no extension symbol), so its card
          ;; renderer rides here: the verb returns "folded <label><note> → <gist>"
          ;; — split it so the receipt (label + reclaimed tokens + utilization) is
          ;; the op-card HEADLINE and the gist the expandable body, instead of the
          ;; whole string hiding as a body-only card the user must expand.
          native-renderers (assoc (extension/native-tool-renderers active-extensions)
                             "session_fold" (fn [result]
                                              (let [s (str result)]
                                                (if-let [i (str/index-of s " → ")]
                                                  {:summary (subs s 0 (long i))
                                                   ;; The gist is a terse breadcrumb the model
                                                   ;; wrote (backticks / file:line / :render spans),
                                                   ;; NOT authored markdown — fence it so the body
                                                   ;; shows VERBATIM instead of being re-styled.
                                                   :body
                                                   (str "```\n" (subs s (+ (long i) 3)) "\n```")}
                                                  {:summary s}))))
          ;; per-OP renderers for TOOL RESULTS the model print()ed in Python — keyed
          ;; by the result's `:op` (the only origin handle a printed value carries),
          ;; so `print(await rg(...))` paints rg's card just like a native call.
          printed-renderers (extension/native-tool-renderers-by-op active-extensions)
          ;; per-tool BADGE color (symbol `:native-tool :color-role`) — the
          ;; channels paint a native tool's result card in its role color
          ;; (read/search/edit/…), recreating the old colored op-card.
          native-color-roles (merge
                               ;; `python_execution` is the engine-level tool (not an
                               ;; extension symbol), so give its eval card a colour
                               ;; too — extensions still override by wire name.
                               {"python_execution" :tool-color/shell}
                               (extension/native-tool-color-roles active-extensions))
          ;; ALL-OBSERVATION CONCURRENCY: when every code-entry is a read-only
          ;; observation (≥2 of them, no mutation / python_execution / handler /
          ;; preflight error), run the whole batch CONCURRENTLY through
          ;; `__vis_par_isolated__` once, then re-split the ordered results per
          ;; entry below. A `delay` so the batch fires on the FIRST entry's turn
          ;; (its `:form-start` already emitted, preserving in-order streaming) and
          ;; only when the iteration actually reaches eval. If the batch can't run
          ;; cleanly (`nil`), every entry falls back to the SERIAL `execute-code`
          ;; path — the feature never degrades a batchable iteration below serial.
          obs-tags-by-name (extension/native-tool-tags active-extensions)
          batch-observations? (observation-batch? code-entries obs-tags-by-name)
          batch-results (when batch-observations?
                          (delay (execute-observation-batch environment (vec code-entries))))
          executed
          (mapv
            (fn [idx
                 {:keys [expr render-segments]
                  :vis/keys [preflight-error]
                  form-repaired? :repaired?
                  :as entry}]
              (log-stage! :code-exec
                          iteration
                          {:idx (inc (long idx)) :total total-blocks :code expr})
              (when (and on-chunk (not suppress-form-start?))
                (on-chunk {:phase :form-start
                           :iteration iteration-position
                           :position idx
                           :count total-blocks
                           ;; Carry the native-tool name so channels can hide the
                           ;; redundant invocation code WHILE the tool runs (not just
                           ;; after the result lands). nil for a non-tool form.
                           :vis/tool-name (:vis/tool-name entry)
                           :scope (form-scope idx)
                           :code expr
                           :render-segments render-segments
                           :started-at-ms (System/currentTimeMillis)}))
              ;; Stamp form-idx BEFORE eval so the
              ;; executing block's position is recorded
              ;; on the turn-state atom.
              (swap! turn-state-atom assoc :form-idx idx)
              (let [scope (form-scope idx)
                    raw-result
                    (cond
                      preflight-error {:result nil
                                       :error (op-error preflight-error
                                                        {:code expr :phase :vis/preflight})
                                       :duration-ms 0
                                       :op :vis/guard}
                      ;; native handler-tool → run in Clojure (run-native-handler)
                      (:vis/native-handler entry) (run-native-handler (:vis/native-handler entry)
                                                                      environment
                                                                      (:vis/native-input entry)
                                                                      (:vis/tool-name entry))
                      :else
                      (if-let [err (literal-code-block-error (:python-context environment) expr)]
                        {:result nil
                         :error (op-error err {:code expr :phase :vis/guard})
                         :duration-ms 0
                         :op :vis/guard}
                        (let [tool-event-fn (when (and on-chunk (not suppress-form-start?))
                                              (fn [tool-event]
                                                (on-chunk {:phase :tool-start
                                                           :iteration iteration-position
                                                           :position idx
                                                           :count total-blocks
                                                           :scope scope
                                                           :code expr
                                                           :render-segments render-segments
                                                           :tool-event tool-event})))
                              ;; ALL-OBSERVATION concurrent batch: the whole
                              ;; iteration ran together via `__vis_par_isolated__`
                              ;; (forced on the first entry's turn). This slot pulls
                              ;; ITS re-split result — same envelope `execute-code`
                              ;; yields, so downstream render/pairing/persist is
                              ;; unchanged. `nil` batch (couldn't run cleanly) →
                              ;; fall through to the serial path. Batched runs don't
                              ;; emit per-tool `:tool-start` sub-events (one eval,
                              ;; no per-call sink routing); `:form-start` /
                              ;; `:form-result` still fire per entry, in order.
                              batched (when batch-results (nth @batch-results idx nil))
                              r (or batched
                                    (if tool-event-fn
                                      (execute-code environment expr :tool-event-fn tool-event-fn)
                                      (execute-code environment expr)))]

                          (log-stage! :code-result
                                      iteration
                                      {:idx (inc (long idx))
                                       :total total-blocks
                                       :duration-ms (:duration-ms r)
                                       :error (:error r)
                                       :timeout? (:timeout? r)
                                       :result (:result r)})
                          r)))
                    ;; Carry parinfer's whole-source
                    ;; rebalance flag into the block
                    ;; result. `execute-code` may also
                    ;; set `:repaired?` (extension hook
                    ;; rescue); both paths converge on
                    ;; the same flag for the channel.
                    result (cond-> raw-result
                             form-repaired?
                             (assoc :repaired? true)

                             (:auto-repaired raw-result)
                             (assoc :repaired? true))
                    display-result (def-display-result environment expr result)
                    ;; def-display-result is now a pass-through; kept on the
                    ;; call path so future display-tweaks have a single seam.
                    block-role (eval-block-role display-result)
                    envelope (eval-envelope turn-prefix
                                            iteration-position
                                            idx
                                            total-blocks
                                            display-result
                                            block-role)
                    result* (assoc display-result
                              :envelope envelope
                              :role block-role)
                    ;; The rendered human display `{:summary :body}` (native-tool
                    ;; card, pretty result, or stdout) — computed ONCE and persisted
                    ;; so a DB-restored / post-turn trace shows the SAME card the live
                    ;; stream did, instead of pr-str'ing the raw result map. `:summary`
                    ;; is the op-card HEADLINE; `:body` (→ `:result-render`) the detail.
                    result-card
                    (tool-result-display result* (:vis/tool-name entry) native-renderers)
                    ;; TOOL RESULTS the model print()ed (each carrying :op) → one op-card
                    ;; each, rendered loop-side via the SAME symbol renderer a native call
                    ;; uses (channels paint pre-rendered strings; they have no renderer). Each
                    ;; card is a CANONICAL MINI-FORM — the exact display-field shape a single
                    ;; native-tool form carries — so the ONE projection (`form/result-card`,
                    ;; `form/->display`, `form/<-wire`) handles each card with ZERO new shape:
                    ;; the TUI/web loop `result-card` per card, the gateway round-trips them by
                    ;; recursing `<-wire` (so the nested `:tool-color-role` keyword survives the
                    ;; JSON hop the same way the singular one does), and nippy persists them.
                    ;;   {:vis/tool-name "cat", :result-summary "…", :result-render "…md…",
                    ;;    :tool-color-role :tool-color/read}
                    printed-cards (vec
                                    (keep (fn [pr]
                                            ;; `pr` crossed the boundary — STRING keys ("op").
                                            (when-let [t (get printed-renderers
                                                              (some-> (get pr "op")
                                                                      str))]
                                              (let [c ((:render t) pr)
                                                    c (if (map? c) c {:body (str c)})]

                                                {:vis/tool-name (some-> (get pr "op")
                                                                        str)
                                                 :result-summary (some-> (:summary c)
                                                                         str
                                                                         not-empty)
                                                 :result-render (some-> (:body c)
                                                                        str
                                                                        not-empty)
                                                 :tool-color-role (:color-role t)})))
                                          (:printed-results result*)))
                    ;; Cards REPLACE the raw stdout body for display ONLY when the block printed
                    ;; nothing but tool results — otherwise (mixed text + results, or any plain
                    ;; print) show the full stdout so NO printed text is ever lost.
                    only-results? (:only-printed-results? result*)
                    cards (when (and only-results? (seq printed-cards)) printed-cards)
                    result-render (if cards nil (:body result-card))
                    result-summary
                    (if cards
                      (str (count cards) " printed result" (when (> (count cards) 1) "s"))
                      (:summary result-card))]

                ;; Per-block streaming chunk (:phase
                ;; :form-result). Fires the moment a
                ;; block lands so the channel can render
                ;; results incrementally instead
                ;; of waiting for the whole batch. Same
                ;; envelope on success and error -
                ;; consumers branch on `:error nil?`,
                ;; not on shape.
                ;;
                ;; Preflight rejections are MODEL-FACING
                ;; only: they teach the model to correct
                ;; its next iteration, but the user does
                ;; not need to see the synthetic error
                ;; box. Suppress the live chunk when the
                ;; result came from a preflight gate
                ;; (mirrors `suppress-form-start?`).
                (when (and on-chunk (not preflight-error))
                  (on-chunk
                    {:phase :form-result
                     :iteration iteration-position
                     :position idx
                     :count total-blocks
                     :scope scope
                     :code expr
                     :render-segments render-segments
                     ;; Pre-rendered human-channel display: a
                     ;; STRING the TUI/web paint as markdown —
                     ;; native `:result` pretty-printed (not raw
                     ;; EDN), or python_execution's `:stdout`.
                     :result result-render
                     :result-render result-render
                     ;; The op-card HEADLINE — a real tool-authored summary
                     ;; ("5 hits in 1 file"), NOT a first-line slice of the body.
                     :result-summary result-summary
                     ;; SEPARATE colored collapsible cards (one per printed tool result).
                     :cards cards
                     ;; Native tool identity for the result BADGE (label + color), so the
                     ;; LIVE gateway stream paints the same op-card the DB-restored trace does.
                     :vis/tool-name (:vis/tool-name entry)
                     :tool-color-role (get native-color-roles (:vis/tool-name entry))
                     ;; Raw stdout kept for model-context consumers.
                     :stdout (:stdout result*)
                     :error (:error result*)
                     :envelope (:envelope result*)
                     :role (:role result*)
                     :timeout? (boolean (:timeout? result*))
                     :repaired? (boolean (:repaired? result*))
                     :auto-repaired? (boolean (:auto-repaired result*))}))
                {:block expr
                 :result result*
                 :result-render result-render
                 :result-summary result-summary
                 :cards cards
                 :render-segments render-segments
                 :svar/tool-call-id (:svar/tool-call-id entry)
                 :vis/tool-name (:vis/tool-name entry)
                 :tool-color-role (get native-color-roles (:vis/tool-name entry))}))
            (range)
            code-entries)
          form-sources (mapv :block executed)
          form-results (mapv :result executed)
          form-segments (mapv :render-segments executed)
          form-tool-ids (mapv :svar/tool-call-id executed)
          form-tool-names (mapv :vis/tool-name executed)
          form-color-roles (mapv :tool-color-role executed)
          form-result-renders (mapv :result-render executed)
          form-result-summaries (mapv :result-summary executed)
          form-cards (mapv :cards executed)
          ;; Preflight gate → synthetic block carries `:vis/preflight? true`
          ;; so channels can suppress the model-facing-only error box. Keep
          ;; the block in the persisted/trailer stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range)
                                   (map (fn [{:vis/keys [preflight-error]}]
                                          (boolean preflight-error))
                                        code-entries))
          blocks
          (validate-iteration-blocks!
            (mapv (fn [idx code result segments tool-call-id tool-name tool-color-role result-render
                       result-summary cards]
                    (cond-> {:id idx
                             :code code
                             :result (:result result)
                             ;; What the block PRINTED — the python_execution
                             ;; result (a native call's result is :result).
                             ;; One block = one tool call, so this is the
                             ;; call's whole stdout (no per-form split).
                             :stdout (:stdout result)
                             ;; Artifacts the block PRODUCED (matplotlib
                             ;; show/savefig, vis_attach, $VIS_OUTBOX write),
                             ;; captured at the SOURCE into the sandbox sink —
                             ;; carried down so the DB attachment OWNS the bytes.
                             :attachments (:attachments result)
                             :error (op-error (:error result)
                                              {:code code :phase (get-in result [:envelope :op])})
                             :envelope (:envelope result)
                             :role (:role result)
                             :timeout? (:timeout? result)
                             :repaired? (:repaired? result)
                             ;; Per-block resolve-symbol* LRU stamps:
                             ;; symbol-name -> current-turn-pos for every
                             ;; symbol the engine hook saw resolve during
                             ;; this block's eval. Iteration writer
                             ;; merges into the long-lived per-env LRU.
                             :lru (or (:lru result) {})
                             ;; If the engine auto-repaired delimiter
                             ;; mistakes (parinferish) before eval, the
                             ;; repaired source flows here so the trailer
                             ;; can disclose the diff and the model can
                             ;; correct itself if the repair was wrong.
                             :repaired-source (:repaired-source result)}
                      ;; Per-block render breakdown for channel display.
                      ;; Legacy channels that only read :code fall
                      ;; back to the full block source.
                      (seq segments)
                      (assoc :render-segments segments)

                      (:vis/silent result)
                      (assoc :vis/silent true)

                      ;; Tool-call identity rides onto the block so
                      ;; `blocks->forms` stamps each form envelope with the
                      ;; tool_use it answers (per-call result pairing).
                      tool-call-id
                      (assoc :svar/tool-call-id tool-call-id)

                      tool-name
                      (assoc :vis/tool-name tool-name)

                      tool-color-role
                      (assoc :tool-color-role tool-color-role)

                      result-render
                      (assoc :result-render result-render)

                      result-summary
                      (assoc :result-summary result-summary)

                      (seq cards)
                      (assoc :cards cards)

                      (get preflight-by-idx idx)
                      (assoc :vis/preflight? true)))
                  (range)
                  form-sources
                  form-results
                  form-segments
                  form-tool-ids
                  form-tool-names
                  form-color-roles
                  form-result-renders
                  form-result-summaries
                  form-cards))]

      (if-let [{value :value} (:answer @turn-state-atom)]
        ;; FINAL path: a plain-text answer reply (svar `:stop-reason :end`),
        ;; already finalized above by `finalize-answer!`. An answer is plain
        ;; prose with no tool calls, so there is no form to gate, elide, or
        ;; attach a post-hoc error to (`:position` is always nil now). The only
        ;; veto is an extension `:turn.answer/validate` hook via
        ;; `final-answer-gate-error`.
        ;;
        ;; `value` is already canonical `[:ast & nodes]` (or a needs-input map):
        ;; the engine boundary ran `render/->ast`. Persist the IR as-is; channels
        ;; render at their boundary via `:channel/messages-renderer-fn`.
        ;; `resolved-model` is a MAP `{:name :provider :reasoning?}` — surface
        ;; `:name`/`:provider` separately so the `iteration.llm_model` column
        ;; stays clean (a stringified map would leak in otherwise).
        (let [validation-error (final-answer-gate-error environment
                                                        iteration-position
                                                        blocks
                                                        value
                                                        active-extensions
                                                        (assoc answer-validation-context
                                                          :code-entries code-entries))
              model-name (actual-llm-model resolved-model ask-result)
              provider (actual-llm-provider resolved-model ask-result)]

          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks)
                         [{:id 0
                           :code "(final-answer-validation)"
                           :result nil
                           :error (op-error validation-error
                                            {:code "(final-answer-validation)"
                                             :phase :vis/final-answer-validation})}])
             :final-result nil
             :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages
             :llm-provider provider
             :llm-model model-name
             :llm-selected-provider (:provider resolved-model)
             :llm-selected-model (some-> (:name resolved-model)
                                         str)
             :llm-actual-provider provider
             :llm-actual-model model-name
             :llm-routing-trace (:routed/trace ask-result)
             :reasoning-effort-resolution reasoning-effort-resolution
             :llm-returned-empty-code? (empty? blocks)
             :assistant-message (:assistant-message ask-result)}
            {:thinking thinking
             :blocks blocks
             :final-result {:final? true :answer value}
             :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages
             :llm-provider provider
             :llm-model model-name
             :llm-selected-provider (:provider resolved-model)
             :llm-selected-model (some-> (:name resolved-model)
                                         str)
             :llm-actual-provider provider
             :llm-actual-model model-name
             :llm-routing-trace (:routed/trace ask-result)
             :reasoning-effort-resolution reasoning-effort-resolution
             :llm-returned-empty-code? (empty? blocks)
             :assistant-message (:assistant-message ask-result)}))
        ;; Normal path (tool-call iteration)
        {:thinking thinking
         :assistant-prose assistant-prose
         :blocks blocks
         :tool-calls tool-calls
         :final-result nil
         :api-usage api-usage
         :duration-ms (or (:duration-ms ask-result) 0)
         :llm-messages messages
         :llm-provider (actual-llm-provider resolved-model ask-result)
         :llm-model (actual-llm-model resolved-model ask-result)
         :llm-selected-provider (:provider resolved-model)
         :llm-selected-model (some-> (:name resolved-model)
                                     str)
         :llm-actual-provider (actual-llm-provider resolved-model ask-result)
         :llm-actual-model (actual-llm-model resolved-model ask-result)
         :llm-routing-trace (:routed/trace ask-result)
         :reasoning-effort-resolution reasoning-effort-resolution
         :llm-returned-empty-code? (empty? blocks)
         :assistant-message (:assistant-message ask-result)}))))

;; =============================================================================
;; Multi-iteration turn engine
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Core helpers
;; -----------------------------------------------------------------------------

(defn- stream-output-overflow?
  [err]
  (let [data (:data err)]
    (and (= :svar.core/stream-incomplete (:type data))
         (= "max_output_tokens" (str (:reason data))))))

(def ^:private stream-truncated-types
  "Error types that indicate the provider dropped the SSE stream before
   emitting content or the terminal `[DONE]` marker. These are transient
   server-side failures — the model never saw the request fail, so
   retrying the identical call is safe and cheap."
  #{:svar.core/stream-truncated})

(def ^:private MAX_STREAM_TRUNCATED_RETRIES
  "Maximum transparent retries for stream-truncated errors per iteration.
   Each retry re-sends the identical messages; reasoning starts fresh on
   the provider side. 2 retries = 3 total attempts."
  2)

(def ^:private MAX_AUTH_REFRESH_RETRIES
  "Max transparent auth-401 retries per iteration. Attempt 0 forces ONE OAuth
   refresh-token exchange (the stored access token was invalidated server-side,
   e.g. refresh-token rotation) + router rebuild and re-sends. If that fresh
   token 401s AGAIN it is almost always PROPAGATION LAG at the provider edge,
   not a dead credential — so the remaining attempts back off and retry the
   SAME token (no re-mint) to let it settle, per [[auth-propagation-backoff-ms]]."
  4)

(defn- stream-truncated-error?
  "True when an exception represents a provider stream that was cut
   before any content arrived. Safe to retry transparently."
  [^Throwable e]
  (let [data
        (ex-data e)

        content-acc-len
        (long (or (:content-acc-len data) 0))]

    (and (contains? stream-truncated-types (:type data)) (zero? content-acc-len))))

(def ^:private MAX_MAX_TOKENS_EXCEEDED_RETRIES
  "Max transparent retries for `:svar.llm/max-tokens-exceeded` per
   iteration. Each retry bumps `:extra-body {:max_tokens N}` by
   `MAX_TOKENS_RETRY_BUMP_FACTOR` so a reasoning-heavy iteration that
   burnt the auto-budget on hidden thinking gets another shot with
   headroom. 1 retry = 2 total attempts; subsequent bumps would either
   exceed the provider's output ceiling or pay 2-4× for the same
   reasoning content, so we cap retries here and let the next iteration
   redistribute the work instead."
  1)

(def ^:private MAX_TOKENS_RETRY_BUMP_FACTOR
  "Multiplier applied to the previous `max_tokens` on a max-tokens
   retry. 2.0 doubles the budget, which empirically covers the
   reasoning-heavy iterations (observed with Copilot
   Claude burning the full 2048 auto-budget on hidden reasoning before
   ever emitting a tool call) without overshooting the provider's
   output-cap on subsequent calls."
  2.0)

(defn- max-tokens-exceeded-error?
  "True when an exception represents `:svar.llm/max-tokens-exceeded`
   from svar's `ask-code!*` blank-content guard. The model produced
   reasoning but the visible content slot was empty because the
   provider's `finish_reason: \"length\"` truncated the response.
   Retry-able via `:extra-body {:max_tokens N}` bump."
  [^Throwable e]
  (= :svar.llm/max-tokens-exceeded (:type (ex-data e))))

(defn- bumped-max-tokens-extra-body
  "Build an `:extra-body` override that doubles the previous `max_tokens`.
   `prev-max` comes from the error's `:output-tokens` (svar reports
   exactly how many tokens the truncated call produced — that number
   equals the cap the provider enforced). Falls back to 8192 × factor
   for callers that lost the count along the way."
  [prev-extra-body prev-max]
  (let [base
        (long (or prev-max 8192))

        bumped
        (long (Math/ceil (* (double base) (double MAX_TOKENS_RETRY_BUMP_FACTOR))))]

    (assoc (or prev-extra-body {}) :max_tokens bumped)))

(defn- max-tokens-exhausted?
  "True for `:svar.llm/max-tokens-exceeded` errors that survived all
   per-iteration retries. See svar's `ask-code!*` blank-content guard
   for the underlying detection."
  [iteration-error-data]
  (= :svar.llm/max-tokens-exceeded (:type iteration-error-data)))

(defn- llm-provider-error-context
  [iteration iteration-error-data]
  (let
    [output-overflow?
     (stream-output-overflow? iteration-error-data)

     max-tokens-exhaust?
     (max-tokens-exhausted? iteration-error-data)

     data
     (:data iteration-error-data)

     reasoning-length
     (some-> data
             :reasoning-length
             long)

     output-tokens
     (some-> data
             :output-tokens
             long)

     message
     (cond
       output-overflow?
       "Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens)."
       max-tokens-exhaust? (str "Provider truncated the response at max_tokens ("
                                (or output-tokens "?")
                                " tokens consumed, "
                                (or reasoning-length "?")
                                " went to hidden reasoning, 0 to visible content). "
                                "Vis already retried once with a doubled budget; this iteration"
                                " still hit the cap.")
       :else (str "LLM call failed: " (:message iteration-error-data)))

     hint
     (cond
       output-overflow?
       "Do not continue the broad strategy. Use a compact path now: one small probe if essential, otherwise stop, report the exact impediment, and ask for confirmation before more changes. Avoid dumping large maps, file contents, diffs, or repeated diagnostics."
       max-tokens-exhaust?
       "Shorten next iteration. Keep tool procedure canonical and compact. Drop unrelated defs and FINISH with a plain-prose answer early if the previous iteration already has enough evidence. Heavy reasoning models on Copilot/Codex cap output independently of context size."
       :else
       "Adjust your approach or finish with a plain-prose answer using only observed evidence.")]

    (cond-> {:phase :llm-provider/generate
             :type (cond output-overflow? :llm-provider/output-budget-exhausted
                         max-tokens-exhaust? :llm-provider/max-tokens-exhausted
                         :else :llm-provider/call-failed)
             :iteration (inc (long iteration))
             :message message
             :hint hint}
      max-tokens-exhaust?
      (assoc :reasoning-length
        reasoning-length :output-tokens
        output-tokens)

      (and (not output-overflow?) (:type iteration-error-data))
      (assoc :source-type (:type iteration-error-data)))))

(defn- iteration-error-feedback
  [iteration iteration-error-data user-request]
  (let [llm-provider-error (llm-provider-error-context iteration iteration-error-data)]
    (str "[Iteration "
         (:iteration llm-provider-error)
         "]\n"
         ";; llm-provider-error =\n" (pr-str llm-provider-error)
         "\n" (when (stream-output-overflow? iteration-error-data)
                (str "Original request: " user-request)))))

;; Provider-error presentation moved to
;; `com.blockether.vis.internal.provider-error` (shared with the TUI trace
;; renderer so a failure reads identically on every surface).

;; -----------------------------------------------------------------------------
;; Router lifecycle + model helpers (turn single-file API)
;; -----------------------------------------------------------------------------

(defonce ^:private router-atom (atom nil))

(defn- enrich-provider-models
  "Apply a provider's optional `:provider/enrich-models-fn` hook to a
   svar-shaped provider at router-build time. Providers whose backend can
   report a model's real context window (LM Studio via its native endpoint)
   register this hook to resolve `:context`/`:tool-call?`; the host stays
   provider-agnostic — no per-provider branching here.

   Runs only at router build (`get-router` / `rebuild-router!`, both memoized
   via `router-atom`), so any network the hook does is once-per-build, never
   per turn. Failure-safe: a throwing or empty hook leaves models untouched and
   svar falls back to its conservative DEFAULT_CONTEXT_LIMIT."
  [svar-provider router-opts]
  (if-let [f (:provider/enrich-models-fn (registry/provider-by-id (:id svar-provider)))]
    (try (let [models (f svar-provider router-opts)]
           (cond-> svar-provider
             (seq models)
             (assoc :models (vec models))))
         (catch Throwable _ svar-provider))
    svar-provider))

(declare auth-refresh-allowed?)

(defn- boot-refresh-provider-token!
  "Build-time sibling of `try-refresh-provider-token!`. When a provider's
   router build fails with an AUTH-shaped error (401/403 or an auth-worded
   message) AND the provider exposes `:provider/refresh-token-fn`, force ONE
   refresh-token exchange (persisting rotated creds) so the caller can retry
   the build. Strictly gated to auth errors — transport failures (TLS/DNS/
   timeout) are never auth-shaped and fall straight through to the skip path.

   Unlike the mid-turn `try-refresh-provider-token!` it must NOT rebuild the
   router: it runs DURING the build itself, and a rebuild would recurse.
   Returns true when a refresh actually happened (caller retries the build)."
  [pid ^Throwable t]
  (let [d
        (ex-data t)

        provider-message
        (perr/provider-body-message (some-> (:body d)
                                            str))

        provider
        (registry/provider-by-id pid)

        f
        (:provider/refresh-token-fn provider)]

    (boolean
      (when (and f
                 (perr/auth-provider-error? (:status d) provider-message (ex-message t))
                 (auth-refresh-allowed? pid))
        (let [rejected (config/baked-token pid)]
          (try
            ;; force refresh-token exchange + persist; pass the rejected token
            ;; so reuse can't hand it straight back. Fall back to 0-arity for
            ;; older/third-party hooks that don't accept it.
            (try (f rejected) (catch clojure.lang.ArityException _ (f)))
            (tel/log! {:level :warn :id ::boot-auth-token-refreshed :data {:provider pid}}
                      (str "Provider build hit auth error — force-refreshed OAuth token for "
                           pid
                           "; retrying build"))
            true
            (catch Throwable rt
              (tel/log! {:level :warn
                         :id ::boot-auth-token-refresh-failed
                         :data {:provider pid :error (ex-message rt)}}
                        (str "Provider build auth refresh FAILED for " pid "; skipping"))
              false)))))))


(defn- runtime-router-providers
  "Resolve durable provider config into the svar runtime shape.

   On-disk config intentionally omits ephemeral credentials for OAuth-backed
   providers such as OpenAI Codex. Resolve those fields immediately before
   constructing a router so each provider can refresh tokens and attach any
   provider-specific headers.

   Each provider may also enrich its own models via `:provider/enrich-models-fn`
   (e.g. LM Studio resolving real context windows)."
  [config]
  (let [ropts (config/router-opts config)]
    ;; RESILIENT build: `->svar-provider` may eagerly fetch an OAuth token
    ;; (Copilot/Codex), and that can fail (expired token, GitHub 403
    ;; "not accessible by integration", network). A single failing provider
    ;; must NOT abort the whole router build and crash startup — skip it with a
    ;; warning and keep every provider that DID resolve. Falling through with
    ;; the others (or none) lets the app start and surface a fixable message.
    (->> (:providers config)
         (keep
           (fn [p]
             (letfn [(build [] (enrich-provider-models (config/->svar-provider p) ropts))]
               (try (build)
                    (catch Throwable t
                      ;; Before dropping an auth-failed provider, try to HEAL it:
                      ;; a server-rotated OAuth token is auth-shaped and force-
                      ;; refreshable in place — refresh once, retry the build once.
                      ;; Anything else (or a failed retry) falls through to skip.
                      (or (try (when (boot-refresh-provider-token! (:id p) t) (build))
                               (catch Throwable _ nil))
                          (do (tel/log! {:level :warn
                                         :id ::provider-unavailable-skipped
                                         :data {:provider (:id p)
                                                :status (:status (ex-data t))
                                                :error (ex-message t)}
                                         :msg (str "Provider "
                                                   (some-> (:id p)
                                                           name)
                                                   " unavailable — skipping ("
                                                   (ex-message t)
                                                   ")")})
                              nil)))))))
         vec)))

(defn- honor-config-primary!
  "svar's `make-router` PREPENDS each known provider's catalog `:default-models`
   to the front of its model list (`:prepend-default-models?`, read from svar's
   template — the user's provider map can't override it). So `(first :models)` —
   the effective/root model `resolve-effective-model` returns — is svar's default,
   NOT the model the user put first in their config. The user reorders a model to
   primary and it silently has no effect.

   This re-seats each provider: float the user's FIRST-CONFIGURED model back to
   the front (and set `:root`), leaving svar's other models behind it as
   fallbacks. No-op for a provider the user didn't configure, or whose first
   model svar dropped (unknown/excluded) — so it can never empty a provider."
  [router config]
  (let [primary (into {}
                      (keep (fn [p]
                              (when-let [m (first (:models p))]
                                [(:id p) (config/model-name m)])))
                      (:providers config))]
    (update router
            :providers
            (fn [provs]
              (mapv (fn [p]
                      (let [want (get primary (:id p))
                            hit (and want (some #(when (= want (:name %)) %) (:models p)))]

                        (if hit
                          (assoc p
                            :models (into [hit] (remove #(= want (:name %)) (:models p)))
                            :root want)
                          p)))
                    provs)))))

(defn get-router
  "Get or create the shared LLM router.

   Honors `:router` opts from `~/.vis/config.edn` (`:rate-limit`,
   `:network`, `:budget`, ...). Without that block svar's built-in
   defaults apply. See `config/router-opts` for the supported keys."
  []
  (or @router-atom
      (let [cfg
            (config/resolve-config)

            r
            (-> (svar/make-router (runtime-router-providers cfg) (config/router-opts cfg))
                (honor-config-primary! cfg))]

        (reset! router-atom r)
        r)))

(defn router-initialized?
  "True once the shared router has been built (via `get-router`/`rebuild-router!`).
   Lets a frontend defer the FIRST build to lazy first-use instead of forcing it
   at startup — so OAuth token fetches (Copilot/Codex) never run at TUI boot."
  []
  (some? @router-atom))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change.

   Forwards `:router` opts so live config edits (e.g. tuning
   `:same-provider-delays-ms`) take effect on the next `set-provider!`
   without restarting the JVM."
  [config]
  (let [r (-> (svar/make-router (runtime-router-providers config) (config/router-opts config))
              (honor-config-primary! config))]
    (reset! router-atom r)
    r))

;; ── OAuth 401 recovery ──────────────────────────────────────────────────
;;
;; A provider's access token can be locally-unexpired yet rejected by the
;; server (401/403) — classic refresh-token rotation: another client (or a
;; second vis process sharing the same on-disk creds) refreshed and rotated
;; the token, silently invalidating the one this router baked in at build
;; time. `:provider/get-token-fn` only refreshes on LOCAL expiry, so a plain
;; router rebuild would re-bake the same dead token. The fix: on an auth
;; error from a provider that exposes `:provider/refresh-token-fn`, FORCE a
;; refresh-token exchange (persists rotated creds), rebuild the router so the
;; fresh token is resolved in, reseat cached envs, and retry the turn once.

(declare refresh-cached-routers!)

;; ── auth-refresh circuit breaker ─────────────────────────────────────────
;; The convergence fix (feed the real baked token) collapses the normal
;; 401→refresh→401 storm to a single exchange. This breaker is the HARD
;; backstop: if a future regression (a new provider, a change in a provider's
;; rotation semantics) makes refreshes flap anyway, STOP hammering the token
;; endpoint — trip after N forced refreshes inside a rolling window and surface
;; the provider's own auth error (which already says "re-authenticate") instead
;; of the daemon flapping forever and starving every other gateway call.

(def ^:private AUTH_REFRESH_WINDOW_MS
  "Rolling window (ms) for the forced-OAuth-refresh circuit breaker."
  60000)

(def ^:private AUTH_REFRESH_WINDOW_MAX
  "Max forced OAuth refreshes for one provider inside `AUTH_REFRESH_WINDOW_MS`
   before the breaker trips. Legitimate rotation refreshes at most a handful of
   times a minute; more than this is a flap, not real rotation."
  6)

(def ^:private AUTH_PROPAGATION_BACKOFF_MS
  "Base backoff (ms) before retrying the SAME just-refreshed token after a
   post-refresh auth 401. A freshly-minted OAuth token is briefly not-yet-valid
   at the provider edge; a short wait lets propagation settle instead of
   re-minting — which only spawns another not-yet-valid token (the 401 storm)."
  1200)

(def ^:private AUTH_PROPAGATION_WINDOW_MS
  "How long (ms) after a FORCED OAuth refresh a subsequent auth 401 reads as
   PROPAGATION LAG (retry the same freshly-minted token with backoff) rather
   than a dead credential (re-mint). Comfortably exceeds the full post-refresh
   backoff sequence (`MAX_AUTH_REFRESH_RETRIES` retries of
   `auth-propagation-backoff-ms`, ~11s) so the whole settling burst stays
   classified as lag; the marker is cleared on the first accepted request so it
   never lingers into a later genuine rotation."
  30000)

(defn- auth-propagation-backoff-ms
  "Backoff (ms) for the Nth (0-based) post-refresh propagation retry, capped 5s."
  [attempt]
  (long (min 5000 (* (long AUTH_PROPAGATION_BACKOFF_MS) (inc (long attempt))))))

(defonce ^:private auth-refresh-events
  ;; provider-id -> vector of epoch-ms timestamps of recent forced refreshes.
  (atom {}))

(defonce ^:private auth-last-refreshed
  ;; provider-id -> {:at <epoch-ms of the last FORCED refresh>}. A recency
  ;; marker: a fresh auth 401 within AUTH_PROPAGATION_WINDOW_MS of it reads as
  ;; PROPAGATION LAG (back off, retry the SAME token), not a dead credential.
  ;; Cleared on the first accepted request by `note-provider-request-ok!`.
  (atom {}))

(defn- auth-refresh-allowed?
  "Circuit breaker for forced OAuth refreshes. Atomically records this attempt
   for `pid`, prunes timestamps older than the rolling window, and returns true
   while the provider is still under the per-window budget. When it returns
   false the breaker is OPEN: the caller must NOT refresh and should surface the
   provider's auth error, so the user re-authenticates once instead of the
   daemon flapping the token endpoint."
  [pid]
  (let [now
        (System/currentTimeMillis)

        cutoff
        (- now (long AUTH_REFRESH_WINDOW_MS))

        recent
        (-> (swap! auth-refresh-events
              update
              pid
              (fn [ts]
                (conj (filterv #(> (long %) cutoff) (or ts [])) now)))
            (get pid))]

    (<= (long (count recent)) (long AUTH_REFRESH_WINDOW_MAX))))

(defn auth-refresh-metrics
  "Observability snapshot of the OAuth-refresh circuit breaker. Returns the
   rolling window, the trip threshold, the per-provider count of forced
   refreshes still inside the window, and the set of providers currently OVER
   budget (breaker OPEN). Surfaced by the gateway `/metrics` endpoint so an
   auth-refresh flap is visible at a glance instead of needing a `vis.log`
   grep."
  []
  (let [cutoff
        (- (System/currentTimeMillis) (long AUTH_REFRESH_WINDOW_MS))

        in-window
        (into {}
              (for [[pid ts]
                    @auth-refresh-events

                    :let [n
                          (long (count (filter #(> (long %) cutoff) ts)))]
                    :when (pos? n)]

                [pid n]))]

    {:window-ms AUTH_REFRESH_WINDOW_MS
     :max-per-window AUTH_REFRESH_WINDOW_MAX
     :refreshes-in-window in-window
     :breaker-open (into #{}
                         (keep (fn [[pid n]]
                                 (when (> (long n) (long AUTH_REFRESH_WINDOW_MAX)) pid)))
                         in-window)}))

(defn- auth-error-shaped?
  "True when `e` looks like a provider auth rejection (401/403 or an auth-shaped
   message), regardless of whether the provider can refresh."
  [^Throwable e]
  (let [d (ex-data e)]
    (perr/auth-provider-error? (:status d)
                               (perr/provider-body-message (some-> (:body d)
                                                                   str))
                               (ex-message e))))

(defn- refresh-just-failed?
  "True when we FORCED an OAuth refresh for this provider very recently (within
   [[AUTH_PROPAGATION_WINDOW_MS]]) and the token is STILL auth-failing — i.e.
   re-minting did NOT clear the 401. Signals PROPAGATION LAG (back off and retry
   the SAME token) rather than a genuinely dead credential.

   Keyed on refresh RECENCY, not token VALUE. The earlier value-equality check
   (`minted == baked-token`) only held for providers that keep the same access
   token across a router rebuild; providers like GitHub Copilot mint a FRESH
   token on every exchange (`ensure-router-has-current-token!` re-exchanges on
   rebuild), so the minted token never equalled the current baked one, the guard
   fell open, and the loop re-minted on every post-refresh 401 → the 401 storm.
   A recency marker matches EVERY provider. It is cleared on the first accepted
   request ([[note-provider-request-ok!]]) so a genuine rotation minutes later is
   read as a fresh 401 (re-mint), not misread as lag."
  [^Throwable e resolved-model]
  (let [pid (:provider resolved-model)]
    (and (auth-error-shaped? e)
         (boolean (when-let [{:keys [at]} (get @auth-last-refreshed pid)]
                    (< (- (System/currentTimeMillis) (long at))
                       (long AUTH_PROPAGATION_WINDOW_MS)))))))

(defn- note-provider-request-ok!
  "Clear the just-refreshed propagation marker for this turn's provider once a
   request has been ACCEPTED (auth succeeded). Keeps [[refresh-just-failed?]]'s
   recency window scoped to the post-refresh settling burst, so a real
   credential rotation later is treated as a fresh 401 (re-mint), never misread
   as propagation lag. No-op when the provider has no marker."
  [resolved-model]
  (when-let [pid (:provider resolved-model)]
    (when (contains? @auth-last-refreshed pid) (swap! auth-last-refreshed dissoc pid))))

(defn- auth-refreshable-error?
  "True when exception `e` is a provider auth rejection (401/403 or an
   auth-shaped message) AND the failing provider exposes a force-refresh
   hook we can use to recover."
  [^Throwable e resolved-model]
  (let [d
        (ex-data e)

        status
        (:status d)

        provider-message
        (perr/provider-body-message (some-> (:body d)
                                            str))

        pid
        (:provider resolved-model)]

    (boolean (and (perr/auth-provider-error? status provider-message (ex-message e))
                  (some-> (registry/provider-by-id pid)
                          :provider/refresh-token-fn)))))

(defn- ensure-router-has-current-token!
  "Rebuild the global router + reseat cached envs so provider `pid`'s freshly
   rotated/reused token goes live — but COALESCE a 401 storm: if the live router
   already bakes the current on-file token (a peer tab just rebuilt after the
   same refresh), skip the process-global rebuild+reseat entirely. Only the
   first tab through pays for the global work; the rest ride its result (the
   retry path reseats this turn's env from the now-fresh global router). This is
   what turns N concurrent 401s into ONE rebuild instead of N."
  [pid]
  (let [current (try (:token ((:provider/get-token-fn (registry/provider-by-id pid))))
                     (catch Throwable _ nil))]
    (when (or (nil? current) (not= current (config/baked-token pid)))
      (let [r (rebuild-router! (config/resolve-config))]
        (refresh-cached-routers! r)))))

(defn- try-refresh-provider-token!
  "Force an OAuth refresh for the failing provider, then rebuild + reseat
   routers so the fresh token is live. Returns true when a refresh actually
   happened (caller may retry), false otherwise (caller surfaces the error).

   Threads the REJECTED access token — the one THIS router baked in at build
   time and the server just 401'd (`config/baked-token`), NOT the current
   on-file token — into the force-refresh hook. The distinction matters under
   concurrency: a peer tab/process may already have rotated the on-file token to
   a fresh one, so re-reading it would hand the single-flight reuse a token that
   is NOT the one that failed — it would then refuse a perfectly good peer token
   and force yet another rotation, and the 401 storm never converges. Feeding the
   real rejected token lets reuse hand back the peer's fresh token instead,
   collapsing the storm to a single exchange."
  [resolved-model]
  (let [pid
        (:provider resolved-model)

        provider
        (registry/provider-by-id pid)

        f
        (:provider/refresh-token-fn provider)

        rejected
        (config/baked-token pid)]

    (boolean
      (cond (not f) false
            (not (auth-refresh-allowed? pid))
            (do (tel/log! {:level :error
                           :id ::auth-refresh-circuit-open
                           :data {:provider pid
                                  :window-ms AUTH_REFRESH_WINDOW_MS
                                  :max AUTH_REFRESH_WINDOW_MAX}}
                          (str "Auth 401 — OAuth refresh circuit OPEN for " pid
                               " (> " AUTH_REFRESH_WINDOW_MAX
                               " refreshes in " (quot (long AUTH_REFRESH_WINDOW_MS) 1000)
                               "s); NOT refreshing — surfacing provider error,"
                               " re-authenticate this provider"))
                false)
            :else
            ;; force refresh-token exchange + persist; pass the rejected token so
            ;; reuse can't return it. Fall back to 0-arity for older/third-party
            ;; hooks that don't accept it.
            (try (try (f rejected) (catch clojure.lang.ArityException _ (f)))
                 (ensure-router-has-current-token! pid)
                 ;; Stamp the refresh time. If the very next re-send ALSO
                 ;; auth-fails within AUTH_PROPAGATION_WINDOW_MS,
                 ;; `refresh-just-failed?` reads this recency marker and treats
                 ;; it as PROPAGATION LAG — backing off and retrying the SAME
                 ;; token instead of re-minting (the storm). Cleared on the
                 ;; first accepted request by `note-provider-request-ok!`.
                 (swap! auth-last-refreshed assoc pid {:at (System/currentTimeMillis)})
                 (tel/log! {:level :warn :id ::auth-token-refreshed :data {:provider pid}}
                           (str "Auth 401 — force-refreshed OAuth token for "
                                pid
                                " and rebuilt router; retrying turn"))
                 true
                 (catch Throwable t
                   (tel/log! {:level :error
                              :id ::auth-token-refresh-failed
                              :data {:provider pid :error (ex-message t)}}
                             (str "Auth 401 — OAuth token refresh FAILED for "
                                  pid
                                  "; surfacing provider error"))
                   false))))))

(defn ask-code!
  "One-shot routed `svar/ask-code!` against the global router.
   Plain-text completion + Markdown-code-block extraction — returns the
   svar map `{:blocks :raw :reasoning :tokens :cost :duration-ms
   :assistant-message :provider-state}`. `:blocks` is a vec of
   `{:lang :source}` (one entry per Markdown code block); concatenate
   yourself with `svar.internal.codes/concat-sources` if you need a
   single string. `ask!` (JSON-spec) is gone; every Vis caller uses
   `ask-code!`."
  [opts]
  (svar/ask-code! (get-router) (rt/with-default-ask-code-idle-timeout opts)))

(defn llm-text!
  "Fast helper LLM call for extensions.

   Uses svar routing (`:routing {:optimize :cost}`) instead of Vis-side model
   name heuristics. The call still goes through `svar/ask-code!` because Vis no
   longer uses the retired `ask!` structured-output path; `:lang \"text\"`,
   `:reasoning :off`, and `:code-tail-pointer? true` make the return a plain
   text string under :text. Callers may pass either :messages or :system +
   :prompt."
  [{:keys [messages system prompt reasoning temperature routing] :as opts}]
  (let [messages
        (or messages
            (cond-> []
              (seq system)
              (conj {:role "system" :content system})

              (seq prompt)
              (conj {:role "user" :content prompt})))

        resp
        (svar/ask-code! (get-router)
                        (rt/with-default-ask-code-idle-timeout
                          (merge (dissoc opts :system :prompt :temperature)
                                 {:messages messages
                                  :lang "text"
                                  :reasoning (or reasoning :off)
                                  :routing (or routing {:optimize :cost})
                                  :code-tail-pointer? true}
                                 (when (some? temperature) {:temperature temperature}))))

        text
        (or (some-> resp
                    :result
                    str/trim
                    not-empty)
            (some-> resp
                    :raw
                    str/trim
                    not-empty)
            "")]

    (assoc resp :text text)))

(defn resolve-effective-model
  "Best-effort root model descriptor from router config.

   The returned map carries `:name` (model id, e.g. \"gpt-4o\") AND
   `:provider` (provider id keyword, e.g. `:openai`) so every caller
   can persist BOTH alongside the model. Earlier versions returned
   just the model map and the provider id was silently dropped on
   the way to the DB - leaving the meta layer with no way to render
   `provider/model`."
  ([router]
   (let [provider
         (first (:providers router))

         model
         (first (:models provider))]

     (when model
       (cond-> (if (map? model) model {:name (str model)})
         (:id provider)
         (assoc :provider (:id provider))))))
  ([router _routing-overrides] (resolve-effective-model router)))

(defn router-for-model
  "Return a router variant whose provider/model ORDER reflects a model PREFERENCE,
   so svar's router picks + falls back accordingly — WE don't pick one model, we
   express the preference and let the inner router decide (no svar change: it
   already routes by the router's order). `prefs` is a model name OR an ORDERED
   coll of names; matching models are hoisted to the front in preference order
   (within each provider AND across providers), and the rest of the router follows
   UNCHANGED as fallback. Blank/unknown prefs → the router as-is (child inherits the
   parent's order). Coordinator: `sub_loop(prompt, subctx, {\"models\": [\"haiku\",
   \"sonnet\"]})` (or a single `\"model\"`) — try the cheap one first, fall back."
  [router prefs]
  (let [names (->> (if (coll? prefs) prefs [prefs])
                   (keep #(some-> %
                                  str
                                  not-empty))
                   vec)]
    (if (empty? names)
      router
      (let [m-name (fn [m]
                     (:name (if (map? m) m {:name (str m)})))
            rank (zipmap names (range))
            ;; lower = more preferred; unlisted = +inf (keeps relative order, stable sort)
            m-rank (fn [m]
                     (get rank (m-name m) Long/MAX_VALUE))
            p-rank (fn [p]
                     (reduce min Long/MAX_VALUE (map m-rank (:models p))))
            reorder (fn [p]
                      (update p :models #(vec (sort-by m-rank %))))]

        (assoc router
          :providers (->> (:providers router)
                          (map reorder)
                          (sort-by p-rank)
                          vec))))))

(defn- provider-root-model
  "Root model NAME for a provider id in `router`, or nil. Prefers the provider's
   declared `:root`, else its first model."
  [router pid]
  (when-let [p (first (filter #(= (:id %) pid) (:providers router)))]
    (or (some-> (:root p)
                str
                not-empty)
        (let [m (first (:models p))]
          (some-> (if (map? m) (:name m) m)
                  str)))))

(defn model-routing-status
  "Live routing health for the model a channel is DISPLAYING (`displayed-provider`
   + `displayed-model` — the per-session pick or the config default the picker
   shows).

   svar opens a circuit breaker on a provider after repeated transient failures
   (5xx / 'Overloaded' 529 / dropped streams) and routes turns to the next
   AVAILABLE provider so work keeps flowing. The displayed model is computed
   from config ORDER and is NOT breaker-aware, so during an outage the picker
   says `opus` while turns actually land on `zai`. This reconciles the two: when
   the displayed provider's breaker is open/half-open, it reports what svar is
   actually serving so the channel can surface
   `⚠ <displayed> overloaded — routing to <serving>`.

   Returns nil when the displayed provider is healthy, else
   `{:overloaded-provider <kw> :overloaded-model <str>
     :serving-provider <kw> :serving-model <str>}`. `serving-*` is nil if every
   provider is down."
  ([displayed-provider displayed-model]
   (model-routing-status (get-router) displayed-provider displayed-model))
  ([router displayed-provider displayed-model]
   (when (and router displayed-provider)
     (let [pid
           (keyword displayed-provider)

           stats
           (try (svar/router-stats router) (catch Throwable _ nil))

           cb-of
           (fn [p]
             (get-in stats [:providers p :circuit-breaker] :closed))

           open?
           (fn [p]
             (contains? #{:open :half-open} (cb-of p)))]

       (when (open? pid)
         (let [serving
               (first (remove #(open? (:id %)) (:providers router)))

               sp
               (:id serving)]

           {:overloaded-provider pid
            :overloaded-model (some-> displayed-model
                                      str)
            :serving-provider sp
            :serving-model (when sp (provider-root-model router sp))}))))))

(defn subctx->seed-ctx
  "Seed ctx for a sub_loop child's ctx-atom from the model-supplied `subctx`.
   Child contexts start from an empty engine ctx. PURE. Kept as the named seed site."
  [_subctx]
  {})

;; -----------------------------------------------------------------------------
;; System var helpers
;;
;; There is no cross-turn var snapshotting: the engine does not parse the
;; iteration's block source for `(def NAME …)` shapes to materialize and
;; persist sandbox locals. Sandbox state is intra-turn scratch only.
;; ----------------------------------------------------------------------------
;; Auto-archive was retired together with the `definition_*` sidecar
;; tables: there is no cross-turn var registry to drive eviction off,
;; and the Python sandbox is fresh every turn anyway. `auto-archive-hot-
;; symbols!` is a no-op stub kept so call sites compile while we sweep
;; them out.
;; ----------------------------------------------------------------------------

(defn auto-archive-hot-symbols!
  "Deprecated NOOP. Cross-turn def survival was removed when the
   `definition_*` sidecar tables were dropped; the Python sandbox starts
   fresh each turn, so there is nothing to archive."
  [_environment]
  nil)

;; -----------------------------------------------------------------------------
;; Iteration loop + run-turn! (inlined from former base)
;; -----------------------------------------------------------------------------

;; Forward reference: defined in the environment lifecycle section
;; ~1500 lines below. Removing this declare requires extracting
;; `sync-active-extension-symbols!` + its 3 helpers (`extension-
;; aliases`, `extension-namespace-bindings`, `require-extension-
;; alias!`) into a separate ns (e.g. `internal/extension_environment.clj`).
;; Tracked as the proper file-split task (sister of
;; `extension-info` declare in extension.clj).
(declare sync-active-extension-symbols!)

(def ^:private FRESH_ITER_CARRY
  ;; `:trailer-iters` is a vec of `[iteration-position {:thinking :blocks}]`
  ;; pairs (oldest-first). The prompt renderer trims the rendered trailer
  ;; by token budget (50% of model context), not fixed iteration count.
  {:trailer-iters []})

(def ^:private balanced-reasoning :balanced)

(do (defn- status->id [status] (when status (keyword "rlm.status" (name status))))
    (def ^:private cost-map-keys
      ["input_cost" "input_uncached_cost" "input_cached_cost" "input_cache_write_cost"
       "cache_read_cost" "cache_write_cost" "output_cost" "total_cost"])
    (defn- estimate-token-cost
      "Estimate cost from provider usage while preserving cached/non-cached input split."
      ([model input-tokens output-tokens] (estimate-token-cost model input-tokens output-tokens {}))
      ([model input-tokens output-tokens opts]
       (try (wire/canonical (svar-router/estimate-cost model
                                                       input-tokens
                                                       output-tokens
                                                       svar-router/MODEL_PRICING
                                                       (or opts {})))
            (catch Throwable _ nil))))
    (defn- merge-cost-maps
      [acc extra-cost]
      (merge-with + (select-keys acc cost-map-keys) (select-keys extra-cost cost-map-keys))))

(defn model-pricing
  "Per-model price table entry (USD per MILLION tokens) for `model`, looked up
   by exact model name in svar's `MODEL_PRICING` — `{:input :output :cache-read
   :cached-input …}` — or nil when the model isn't priced. Read-only view over
   the same table `estimate-token-cost` bills against, so channel pickers show
   the price that actually gets charged."
  [model]
  (when model (get svar-router/MODEL_PRICING (str model))))

(def ^:private loop-give-up-text
  "Markdown surfaced when a turn is force-finalized after the model kept
   repeating without ever landing — and produced no usable answer to fall
   back on."
  (str "I kept repeating the same steps without converging on a confident "
       "answer, so I stopped to avoid spinning. See the iteration trace above "
       "for what I gathered."))

(defn- repetition-loop-state
  "Pure repetition-only loop detector. Given this iteration's executed `blocks`
   and the prior `:stuck` carry, returns the next `:stuck` fields plus `:stuck?`.

   One signal, no iteration/budget counting: identical action code repeated
   across iterations (the model reran the same search / rebuilt the same parser)
   ⇒ stuck. (There is no done() any more; a plain-text answer always finalizes,
   so there is no non-finalizing-done loop to detect.)"
  [blocks prev-stuck]
  (let [action-code
        (mapv :code blocks)

        action-sig
        (when (seq action-code) (hash action-code))

        sig-repeat?
        (boolean (and action-sig (= action-sig (:last-sig prev-stuck))))]

    {:stuck? sig-repeat? :action-sig action-sig}))

(defn- loop-checkpoint-message
  "The repetition decision-checkpoint, injected as a user turn the moment the
   model loops (identical action code repeated across iterations). Confronts the
   one-shot urge: shows the best answer so far and forces a finish / one-tool /
   blocked decision instead of another open-ended probe. `sticky-md` is the best
   answer so far (Markdown) or nil."
  [sticky-md]
  (str "⚠️ STOP — you are repeating yourself. You wanted to one-shot this, but "
       "you have now looped without finalizing.\n\n"
       (if (str/blank? (str sticky-md))
         "You have NOT produced any answer yet.\n\n"
         (str "Your best answer so far:\n\n---\n" sticky-md "\n---\n\n"))
       "DECIDE NOW:\n"
       "1. FINISH — reply with the best answer as plain prose; no tool call.\n"
       "2. ONE TOOL — only if one named missing fact blocks the answer, call exactly "
       "one NEW tool that can obtain it. Never repeat a prior call.\n"
       "3. BLOCKED — reply in plain prose stating exactly what blocks you.\n"
       "No other investigation."))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt session-turn-id
           ;; `max-context-tokens` feeds advisory context-pressure hooks;
           ;; trailer assembly itself still owns no token trimming.
           max-context-tokens hooks cancel-atom cancel-token reasoning-default routing extra-body
           reasoning-effort turn-features allow-copilot-claude-deep? workspace-overrides]}]
  (let [environment
        (cond-> environment
          (seq turn-features)
          (assoc :turn/features turn-features)

          (seq workspace-overrides)
          (merge workspace-overrides)

          ;; Surface the cancellation token on the environment
          ;; so `run-python-code` can call
          ;; `cancellation/on-cancel!` to register a hard
          ;; `.cancel(true)` on the Python worker future.
          ;; Without this the UI cancel flag (already flipped
          ;; by `vis/cancel!`) only reaches the outer turn
          ;; future; the inner Python worker keeps spinning,
          ;; pins a thread and starves the input loop until
          ;; the eval timeout fires.
          cancel-token
          (assoc :cancel-token cancel-token)

          cancel-atom
          (assoc :cancel-atom cancel-atom)

          ;; Per-turn context surfaced to engine hooks and
          ;; render-time diagnostics.
          true
          (assoc :turn/user-request
            user-request :turn/system-prompt
            system-prompt))

        resolved-model
        (resolve-effective-model (:router environment))

        effective-model
        (:name resolved-model)

        _
        (assert effective-model "Router must resolve a root model")

        ;; Clear any sticky best-answer from a PRIOR turn (the atom lives on
        ;; the per-session env) so this turn's cancel-fallback only ever
        ;; surfaces an answer THIS turn actually produced.
        _
        (some-> (:turn-state-atom environment)
                (swap! assoc :best-answer nil))

        has-reasoning?
        (and (nil? reasoning-effort) (reasoning-effort-configurable? resolved-model))

        base-reasoning-level
        (or (normalize-reasoning-level reasoning-default) balanced-reasoning)

        ;; Activate extensions ONCE per session turn. Threaded through both
        ;; the prompt message assembler (core, environment, extension messages)
        ;; and the per-iteration ext hint collector - activation-fn never
        ;; re-fires inside the loop.
        active-exts
        (prompt/active-extensions environment)

        _extensions-snapshot
        (prompt/extensions-snapshot active-exts)

        _
        (sync-active-extension-symbols! environment active-exts)

        session-snapshot
        (fn []
          {:id (:session-id environment)
           :title (some-> (:session-title-atom environment)
                          deref
                          str
                          str/trim
                          not-empty)
           :turn-id session-turn-id
           :user-request user-request})

        _session-base
        (session-snapshot)

        turn-position
        (session-turn-position environment session-turn-id)

        previous-usage
        (previous-request-usage environment session-turn-id)

        ;; Standing session context (workspace/env/routing/tools) baked into the
        ;; cached system prefix ONCE PER PROCESS and FROZEN (`:standing-ctx-atom`).
        ;; Re-rendering it per turn would change the cached prefix on any state
        ;; change and bust the prompt cache; instead the block is frozen and every
        ;; change rides as an appended `session[...] = …` delta. First turn seeds
        ;; it; later turns reuse the frozen block. (A fresh process renders fresh —
        ;; cold cache anyway.)
        standing-ctx-atom
        (:standing-ctx-atom environment)

        _
        (when (and standing-ctx-atom (nil? @standing-ctx-atom))
          (reset! standing-ctx-atom
            {:block (ctx-loop/render-block! environment ctx-renderer/render-ctx-static)
             :baseline (ctx-loop/render-block! environment ctx-renderer/ctx-static-map)}))

        static-context-str
        (or (:block (some-> standing-ctx-atom
                            deref))
            (ctx-loop/render-block! environment ctx-renderer/render-ctx-static))

        ;; Delta baseline = the LAST-EMITTED map, carried ACROSS turns via
        ;; standing-ctx-atom (NOT re-seeded per turn). Each iter diffs the current
        ;; util-inclusive map against it and appends `session[...] = …` on change.
        last-context-atom
        (atom (or (:baseline (some-> standing-ctx-atom
                                     deref))
                  (ctx-loop/render-block! environment ctx-renderer/ctx-static-map)))

        stable-prompt-messages
        (prompt/assemble-stable-prompt-messages environment
                                                {:system-prompt system-prompt
                                                 :active-extensions active-exts
                                                 :session-context static-context-str})

        ;; Image attachments: paths of readable image files mentioned in the
        ;; user message (terminal drop pastes the path) become multimodal
        ;; blocks on the initial user message. Engine-side so every channel
        ;; gets the same behavior; never throws.
        user-attachments
        (let [disk
              (try (attachments/collect-user-images user-request
                                                    {:workspace-root (:workspace/root environment)})
                   (catch Throwable t
                     (tel/log!
                       {:level :warn :id ::user-image-scan-failed :data {:error (ex-message t)}})
                     {:attached [] :skipped []}))]
          ;; INLINE (web/API) uploads — validated in prepare-turn-context and
          ;; carried on the env — ride AHEAD of disk-scanned images; both feed
          ;; the same multimodal assemble seam.
          {:attached (into (vec (:user/attachments environment)) (:attached disk))
           :skipped (into (vec (:user/skipped-attachments environment)) (:skipped disk))})

        _
        (when (seq (:attached user-attachments))
          (tel/log! {:level :info
                     :id ::user-images-attached
                     :data {:count (count (:attached user-attachments))
                            :paths (mapv :path (:attached user-attachments))
                            :skipped (mapv :path (:skipped user-attachments))}
                     :msg "attached user-message images"}))

        initial-messages
        (prompt/assemble-initial-messages {:stable-prompt-messages stable-prompt-messages
                                           :initial-user-content user-request
                                           :user-images (:attached user-attachments)
                                           :skipped-images (:skipped user-attachments)
                                           :previous-turn-context
                                           (previous-turn-context environment session-turn-id)})

        ;; The cumulative `:input-tokens` field sums canonical input tokens
        ;; from every iteration in this turn — useful for billing /
        ;; budget accounting but MUST NOT be passed to the
        ;; context-pressure hint, which compares against the model's
        ;; per-call context window. Cumulative input can cross the 50%
        ;; threshold after many iterations even when each individual
        ;; request stays small, producing fake context-pressure warnings.
        ;;
        ;; `:last-iter-input` carries the most recent SINGLE-CALL
        ;; request input tokens, which is the right proxy for \"what the next
        ;; request will look like\". Reasoning tokens from a preserved-
        ;; thinking-enabled provider already flow into the next iter's
        ;; input-token count server-side, so a single last-iter snapshot
        ;; already captures that growth without us re-computing it.
        ;;
        ;; Iter 1 of a new user turn has no live provider usage yet. Keep
        ;; billing fields zeroed, but seed the utilization/hint proxy from
        ;; latest persisted request in the session so the model still sees
        ;; `:session/utilization` immediately.
        usage-atom
        (atom {:input-tokens 0
               :output-tokens 0
               :reasoning-tokens 0
               :reasoning-reported? false
               :cached-tokens 0
               :cache-creation-tokens 0
               :last-iter-input 0
               :last-iter-reasoning 0
               :previous-request-input (long (or (:last-request-tokens previous-usage) 0))
               :iter-count 0})

        ;; Running SUM of per-iteration cost maps, each priced by the model
        ;; that ACTUALLY served that iteration (svar may fall back mid-turn;
        ;; the health gate can make selected≠actual). nil until the first
        ;; priced iteration; a turn served entirely by an unpriced local
        ;; model stays nil and finalize-cost falls back to the root-model
        ;; estimate (which prices to nothing for the same reason). Without
        ;; this, a turn served by a free local model was billed at the
        ;; SELECTED model's pricing (e.g. gemma-on-lmstudio at Opus rates).
        accrued-cost-atom
        (atom nil)

        accumulate-usage!
        (fn [api-usage]
          (when api-usage
            (swap! usage-atom
              (fn [acc]
                (let [iter-in
                      (long (or (:input-tokens api-usage) 0))

                      iter-reason
                      (get-in api-usage [:output-tokens-details :reasoning])]

                  (cond-> (-> acc
                              (update :input-tokens + iter-in)
                              (update :output-tokens + (or (:output-tokens api-usage) 0))
                              (update :cached-tokens
                                      +
                                      (or (get-in api-usage [:input-tokens-details :cache-read]) 0))
                              (update :cache-creation-tokens
                                      +
                                      (or (get-in api-usage [:input-tokens-details :cache-write])
                                          0))
                              ;; Per-iter snapshots: overwrite, not accumulate.
                              (assoc :last-iter-input iter-in)
                              (assoc :last-iter-reasoning iter-reason)
                              (update :iter-count inc))
                    (some? iter-reason)
                    (-> (update :reasoning-tokens + (long iter-reason))
                        (assoc :reasoning-reported? true))))))))

        ;; Per-iteration token + cost projection. The schema's
        ;; `iteration.llm_*_tokens` / `iteration.llm_cost_usd` columns
        ;; carry one row per iteration so a future `vis report`
        ;; caller can sum or break down cost without re-walking
        ;; provider envelopes. Returns nil when the call surfaced no
        ;; usage (e.g. iteration-level error before a response
        ;; landed), in which case the persistance layer leaves the
        ;; columns NULL.
        iteration-token-cost
        (fn iteration-token-cost ([api-usage] (iteration-token-cost api-usage nil))
          ([api-usage actual-model] (when api-usage (let [in
                                                          (long (or (:input-tokens api-usage) 0))

                                                          out
                                                          (long (or (:output-tokens api-usage) 0))

                                                          reas
                                                          (get-in api-usage
                                                                  [:output-tokens-details
                                                                   :reasoning])

                                                          cach
                                                          (long (or (get-in api-usage
                                                                            [:input-tokens-details
                                                                             :cache-read])
                                                                    0))

                                                          cache-created
                                                          (long (or (get-in api-usage
                                                                            [:input-tokens-details
                                                                             :cache-write])
                                                                    0))

                                                          ;; svar's `estimate-cost` returns a MAP
                                                          ;; keyed map; `wire/canonical` re-keys it to
                                                          ;; canonical snake strings at this boundary.
                                                          ;; Pull `"total_cost"` out; nil pricing
                                                          ;; (e.g. unknown model) leaves the
                                                          ;; column NULL on disk, which the read
                                                          ;; side defaults to 0.0.
                                                          ;; Price by the model that ACTUALLY served
                                                          ;; the call (svar mid-turn fallback / the
                                                          ;; health gate make selected≠actual); the
                                                          ;; pre-resolved root model is the fallback
                                                          ;; pricing key only when routing metadata
                                                          ;; is absent (e.g. error before a response).
                                                          cost-map
                                                          (estimate-token-cost
                                                            (or (some-> actual-model
                                                                        str
                                                                        not-empty)
                                                                effective-model)
                                                            in
                                                            out
                                                            {:api-usage api-usage})

                                                          total
                                                          (when (map? cost-map)
                                                            (get cost-map "total_cost"))]

                                                      (when (map? cost-map)
                                                        (swap! accrued-cost-atom #(merge-cost-maps
                                                                                    (or % {})
                                                                                    cost-map)))
                                                      {:tokens (cond-> {"input" in
                                                                        "output" out
                                                                        "cached" cach
                                                                        "cache_created"
                                                                        cache-created}
                                                                 (some? reas)
                                                                 (assoc "reasoning" (long reas)))
                                                       :cost-usd (when (number? total)
                                                                   (double total))}))))

        finalize-cost
        (fn []
          (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens
                        cache-creation-tokens reasoning-reported?]}
                @usage-atom

                total-tokens
                (+ (long input-tokens) (long output-tokens))

                ;; Prefer the SUM of per-iteration costs (each priced
                ;; by its actual serving model) over re-estimating the
                ;; whole turn at the root model's rates — a turn that
                ;; fell back mid-way (or was served entirely by a free
                ;; local model while a paid model was selected) must
                ;; not bill at the selected model's pricing.
                cost
                (or @accrued-cost-atom
                    (estimate-token-cost effective-model
                                         input-tokens
                                         output-tokens
                                         {:cached-tokens cached-tokens
                                          :cache-creation-tokens cache-creation-tokens}))]

            {:tokens (cond-> {"input" input-tokens
                              "output" output-tokens
                              "cached" cached-tokens
                              "cache_created" cache-creation-tokens
                              "total" total-tokens}
                       reasoning-reported?
                       (assoc "reasoning" reasoning-tokens))
             :cost cost}))

        ;; `:on-chunk` is a per-reasoning-chunk streaming hook fired
        ;; from svar's stream callback. It fires dozens of times per
        ;; iteration, not at lifecycle boundaries. Lifecycle callbacks
        ;; now use namespaced `:ext/hooks` phases; on-chunk stays the
        ;; high-frequency streaming-only surface.
        on-chunk
        (:on-chunk hooks)

        emit-hook!
        (fn [hook-fn payload log-message]
          ;; Single-fn caller-hook helper, used by
          ;; on-chunk only.
          (when hook-fn
            (try (hook-fn payload)
                 (catch Exception e
                   (tel/log! {:level :warn :data (format-exception-short e)} log-message)))))

        iteration-cache-created-tokens
        (fn [token-cost]
          (let [cache-created (long (or (get-in token-cost [:tokens "cache_created"]) 0))]
            (when (pos? cache-created) cache-created)))]

    ;; -----------------------------------------------------------------
    ;; Turn-start state.
    ;;
    ;; The Python `context` dict is bound separately from tool bindings. The
    ;; visible `<context>` block and live dict share the same projection; see
    ;; ctx-loop/session-snapshot for the read-only guarantee.
    ;; Seed turn-scoped fields on the single turn-state-atom in one swap.
    (ctx-loop/set-turn-state! environment
                              :iteration-id nil
                              :session-turn-id session-turn-id
                              :user-request user-request
                              :turn-position (or turn-position 1)
                              :iteration nil
                              :form-idx nil
                              ;; FORCING plan-gate: distinct files mutated THIS turn (reset each turn).
                              ;; The 2nd distinct file without an approved plan arms the gate.
                              :files-mutated #{})
    ;; Sync context `:session/turn` to the persisted turn-position via
    ;; `eng/enter-turn`. `enter-turn` is idempotent and also clears transient
    ;; engine blockers so the new turn starts clean.
    (when-let [ctx-atom (:ctx-atom environment)]
      (swap! ctx-atom (fn [c]
                        (ctx-engine/enter-turn c (or turn-position 1)))))
    ;; REPL-style recovery slots (`*1` `*2` `*3` `*e`) are per-turn. A
    ;; follow-up turn opens with all four nil so leftover values from
    ;; the previous turn never bleed into the new OODA loop.
    (env/reset-eval-bindings! environment)
    ;; Hot symbol archival runs only after a final successful answer.
    ;; Failed/cancelled turns keep their live scratch symbols for
    ;; recovery. This is sandbox namespace pruning — unrelated to CTX
    ;; trailer state (which only summarises, never compacts).
    ;; Cross-turn carry: seed `trailer-iters` with persisted iterations
    ;; of the current session (across every prior turn) so a
    ;; follow-up turn opens with prior context. Rendering trims by token
    ;; budget, so carry is not capped by iteration count. Each entry is
    ;; `[iter-position {:thinking :blocks}]` matching the in-memory shape
    ;; the renderer expects. Failures degrade silently to an empty seed.
    ;;
    ;; IMPORTANT: cross-turn entries feed the TRAILER ONLY. Do not replay
    ;; their provider-native preserved-thinking assistant messages into the
    ;; new user turn — replaying prior-turn preserved thinking makes some
    ;; providers treat the answer as already accepted and burn input tokens.
    ;; Durable cross-turn memory must flow through persisted iterations,
    ;; not hidden reasoning state.
    (let [seeded-trailer-iters
          (try
            (when-let [session-id (:session-id environment)]
              (let [d (:db-info environment)
                    queries (persistance/db-list-session-turns d session-id)
                    current-turn-id-str (str session-turn-id)
                    ;; Drop CURRENT turn rows (defensive: they should not
                    ;; exist yet at seed time, but a restart/recover path
                    ;; could leave partial rows) and PRIOR-turn iterations
                    ;; whose status is NOT :done. Erroring / running /
                    ;; interrupted iterations are exploration noise that
                    ;; poisons follow-up turns when replayed verbatim into a
                    ;; later turn's trailer. Carry only the iterations that
                    ;; landed a clean result; defs from earlier exploration
                    ;; survive independently via the def restore path.
                    iters (->> queries
                               (remove #(= (str (:id %)) current-turn-id-str))
                               (mapcat (fn [q]
                                         (try (persistance/db-list-session-turn-iterations d
                                                                                           (:id q))
                                              (catch Throwable _ []))))
                               (filter #(= :done (:status %)))
                               (sort-by :created-at)
                               vec)
                    iters-atts
                    ;; Batch-load OUTBOUND artifacts (figures/files) once for the
                    ;; whole carry so a later-turn vision model can SEE prior
                    ;; generated images — the bytes were persisted, never wired.
                    (try (persistance/db-list-iterations-attachments d (keep :id iters))
                         (catch Throwable _ {}))]

                (mapv (fn [it]
                        [(or (:position it) 1)
                         {:thinking (:thinking it)
                          ;; Cross-turn rows render scopes from the SAME forms-vec
                          ;; the live path uses, so scopes stay consistent.
                          :forms-vec (:forms it)
                          :blocks [(cond-> {:position 0 :code (or (:code it) "")}
                                     (contains? it :result)
                                     (assoc :result (:result it))

                                     (contains? it :error)
                                     (assoc :error (:error it)))]
                          :llm-provider (:provider it)
                          :llm-model (some-> (:model it)
                                             str)
                          ;; Persisted assistant messages are intentionally NOT
                          ;; replayed across user turns. Keep row diagnostics,
                          ;; but `compatible-preserved-thinking-trailer-iters`
                          ;; rejects this entry before replay.
                          :assistant-message (force (:llm-assistant-message it))
                          ;; Its produced artifacts still ride to a vision model
                          ;; (see the `replay? false` branch of `conversation-suffix`),
                          ;; even though the assistant/thinking chain is dropped.
                          :attachments (attachment-storage/hydrate-all (get iters-atts
                                                                            (str (:id it))))
                          :preserved-thinking/replay? false}])
                      iters)))
            (catch Throwable t
              (tel/log! {:level :warn
                         :id ::cross-turn-trailer-seed-failed
                         :data {:error (ex-message t)}
                         :msg
                         "Cross-turn carry seed failed; first iteration starts with an empty tape"})
              nil))]
      (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages :trace []}
                                 FRESH_ITER_CARRY
                                 (when (seq seeded-trailer-iters)
                                   {:trailer-iters seeded-trailer-iters}))]
          (let [{:keys [iteration messages trace trailer-iters llm-provider]} loop-state]
            (ctx-loop/set-turn-state! environment :iteration (inc (long iteration)))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error iteration {:reason :cancelled})
                  ;; Sticky best-answer: surface the latest non-blank answer
                  ;; this turn produced instead of a blank answer.
                  (let [sticky (some-> (:turn-state-atom environment)
                                       deref
                                       :best-answer
                                       :value)
                        result (merge {:answer sticky
                                       :status :cancelled
                                       :status-id (status->id :cancelled)
                                       :trace trace
                                       :iteration-count iteration}
                                      (finalize-cost))]

                    result))
              :else
              (let [raw-reasoning-level (when has-reasoning? base-reasoning-level)
                    reasoning-level (copilot-claude-safe-reasoning-level
                                      resolved-model
                                      user-request
                                      raw-reasoning-level
                                      {:allow-copilot-claude-deep? allow-copilot-claude-deep?})
                    _ (log-stage! :iteration/start
                                  iteration
                                  {:message-count (count messages)
                                   :reasoning reasoning-level
                                   :reasoning-effort reasoning-effort
                                   :requested-reasoning raw-reasoning-level})
                    pre-resolved-model (resolve-effective-model (:router environment)
                                                                (or routing {}))
                    ;; `:context-limit` for the `context-pressure-hint`
                    ;; threshold. Walk three sources in priority order:
                    ;;   1. caller-supplied `:max-context-tokens` (turn-
                    ;;      level override; rarely set today — TUI
                    ;;      `vis/send!` does not pass it).
                    ;;   2. The resolved model's `:input-limit` (models.dev
                    ;;      input cap, e.g. Copilot Claude-sonnet-4.6 = 128K).
                    ;;   3. The resolved model's `:context` (input+output
                    ;;      budget, used when models.dev exposes no
                    ;;      separate input cap).
                    ;;   4. 200_000 fallback for unknown models, matching
                    ;;      the historical advisory ceiling.
                    ;; Without this the hint fired off a uniform 200K
                    ;; baseline and either pestered the model too early
                    ;; on a 1M-context Anthropic native call or, worse,
                    ;; under-warned on a 128K Copilot call where the
                    ;; 50% trigger now lands at ~64K instead of 100K.
                    effective-context-limit (or max-context-tokens
                                                (:input-limit pre-resolved-model)
                                                (:context pre-resolved-model)
                                                200000)
                    _llm-provider-context (cond-> {:selected (llm-id (:provider pre-resolved-model)
                                                                     (some-> (:name
                                                                               pre-resolved-model)
                                                                             str))
                                                   :routing (cond-> {:fallback? false}
                                                              (seq routing)
                                                              (assoc :request routing))}
                                            (:error llm-provider)
                                            (assoc :error (:error llm-provider)))
                    iteration-position (inc (long iteration))
                    current-session (session-snapshot)
                    _iteration-hints (collect-iteration-start-hints
                                       environment
                                       active-exts
                                       {:environment environment
                                        :phase :turn.iteration/start
                                        :session current-session
                                        :iteration iteration-position
                                        :session-title (:title current-session)
                                        ;; Title setup is host-owned by `maybe-auto-title!`.
                                        ;; Keep the model-facing title hook quiet.
                                        :title-refresh? false
                                        :turn-position turn-position
                                        ;; Use `:last-iter-input` so the hint reflects the SIZE OF
                                        ;; THE NEXT REQUEST instead of the cumulative-turn total.
                                        ;; `:input-tokens` (cumulative) is kept on the snapshot for
                                        ;; budget-aware extensions that want to surface turn-level
                                        ;; spend separately. Falls back to the previous persisted
                                        ;; request on iter 0 (before this turn has provider usage)
                                        ;; so first-iter hints are not gated on a missing sample.
                                        :input-tokens (let [u @usage-atom]
                                                        (if (pos? (long (:iter-count u)))
                                                          (long (:last-iter-input u))
                                                          (long (:previous-request-input u))))
                                        :cumulative-input-tokens (:input-tokens @usage-atom)
                                        :cumulative-reasoning-tokens (:reasoning-tokens @usage-atom)
                                        :iter-count (:iter-count @usage-atom)
                                        :context-limit effective-context-limit})
                    ;; Stamp :engine/utilization onto the ctx so the next
                    ;; render surfaces :session/utilization (how much of
                    ;; the window the LAST request used). :engine/* is
                    ;; stripped before persist, so the transient count
                    ;; never enters the durable snapshot.
                    ;; Stamp :engine/utilization (rendered as :session/utilization
                    ;; next iter). Monotonic — see `stamp-utilization!`: a
                    ;; transient req=0 keeps the last value instead of blanking.
                    _util-stamp (when-let [ca (:ctx-atom environment)]
                                  (let [u @usage-atom
                                        req (if (pos? (long (:iter-count u)))
                                              (long (:last-iter-input u))
                                              (long (:previous-request-input u)))]

                                    (stamp-utilization!
                                      ca
                                      (ctx-engine/utilization
                                        req
                                        effective-context-limit
                                        (:input-tokens u)
                                        ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))))
                    ;; Standing context render + budget guard.
                    ;;
                    ;; The engine never silently drops data; the model owns
                    ;; deliberate prior-turn compaction through `session_fold`.
                    ;; This turn's append-only suffix: [assistant-replay,
                    ;; <results>] pairs per prior iteration, so the model sees
                    ;; both its reasoning AND what its code returned.
                    ;; Standing session context lives ONCE in the cached system
                    ;; prompt; any mid-turn change rides INSIDE the causing
                    ;; iteration's <results> message (see iteration-results-message
                    ;; / the iter-ctx-diff capture). So the wire is strictly
                    ;; append-only with no trailing context churn:
                    ;;
                    ;;   [system (+ <context>), user_initial,
                    ;;    asst_1, <results 1>,
                    ;;    asst_2, <results 2 (+ <context> diff if iter-2 changed it)>,
                    ;;    ...
                    ;;    asst_(n-1), <results n-1>]
                    ;;
                    ;; The original user_initial stays as the ONE user-role anchor
                    ;; near the start (placed by `assemble-initial-messages`); we
                    ;; never repeat it. Matches z.ai's canonical preserved-thinking
                    ;; shape (user → asst → user → asst → user).
                    _ (stamp-iter-universe! (:ctx-atom environment) trailer-iters)
                    conversation-suffix-msgs (conversation-suffix
                                               (apply-summaries trailer-iters
                                                                (some-> (:ctx-atom environment)
                                                                        deref
                                                                        (get "session_summaries")))
                                               (replay-context pre-resolved-model))
                    provider-messages (into (vec messages) conversation-suffix-msgs)
                    effective-messages provider-messages
                    resolved-model pre-resolved-model
                    effective-routing (or routing {})
                    iteration-result
                    ;; Per-iteration retry state.
                    ;;   `:attempt`              — generic counter shared by every retry policy.
                    ;;   `:max-tokens-attempt`   — separate counter so a stream-truncated retry
                    ;;                              earlier in the call doesn't burn the max-tokens
                    ;;                              quota (and vice versa).
                    ;;   `:current-extra-body`   — carries any caller-supplied extra-body PLUS the
                    ;;                              max_tokens bumps applied so far. Re-merged with
                    ;;                              svar's auto-params downstream; explicit override
                    ;;                              wins per `preserve-auto-params` merge order.
                    (loop [attempt 0
                           max-tokens-attempt 0
                           ;; PU (provider-unavailable) gets its OWN retry budget,
                           ;; independent of the stream-rewind / auth retries that
                           ;; also thread through `attempt` — else its "1s->2s->4s,
                           ;; 3 retries" story only holds when PU is the FIRST
                           ;; failure in the turn.
                           pu-attempt 0
                           current-extra-body extra-body
                           ;; `env` is threaded so the auth-refresh retry can
                           ;; reseat its `:router` to the rebuilt one (the
                           ;; in-flight env captured the pre-refresh router).
                           env environment]

                      (let [result
                            (try
                              (run-iteration env
                                             effective-messages
                                             {:iteration iteration
                                              :reasoning-level reasoning-level
                                              :reasoning-effort reasoning-effort
                                              :routing effective-routing
                                              :resolved-model resolved-model
                                              :on-chunk on-chunk
                                              :active-extensions active-exts
                                              :answer-validation-context
                                              {:user-request user-request
                                               :previous-iterations trailer-iters
                                               :previous-blocks (vec (mapcat (comp :blocks second)
                                                                             trailer-iters))}
                                              :extra-body current-extra-body})
                              (catch Exception e
                                (cond
                                  (and (stream-truncated-error? e)
                                       (< (long attempt) (long MAX_STREAM_TRUNCATED_RETRIES)))
                                  (do (tel/log! {:level :warn
                                                 :id ::stream-truncated-retry
                                                 :data {:iteration iteration
                                                        :attempt (inc (long attempt))
                                                        :max-retries MAX_STREAM_TRUNCATED_RETRIES
                                                        :type (:type (ex-data e))}}
                                                (str "Stream truncated, transparent retry "
                                                     (inc (long attempt))
                                                     "/" MAX_STREAM_TRUNCATED_RETRIES))
                                      ::retry-stream)
                                  ;; Max-tokens cap: model burnt the entire output
                                  ;; budget on hidden reasoning before emitting a
                                  ;; tool call. Double the budget and try once more so the
                                  ;; turn doesn't fail when the same call would have
                                  ;; succeeded with a slightly larger ceiling. Reasoning-
                                  ;; heavy iterations hit this when the provider's
                                  ;; finish_reason: \"length\" leaves content-acc empty.
                                  (and (max-tokens-exceeded-error? e)
                                       (< (long max-tokens-attempt)
                                          (long MAX_MAX_TOKENS_EXCEEDED_RETRIES)))
                                  (let [data (ex-data e)
                                        prev-max (or (:output-tokens data)
                                                     (:max_tokens current-extra-body)
                                                     8192)
                                        bumped (bumped-max-tokens-extra-body current-extra-body
                                                                             prev-max)]

                                    (tel/log! {:level :warn
                                               :id ::max-tokens-exceeded-retry
                                               :data {:iteration iteration
                                                      :attempt (inc (long max-tokens-attempt))
                                                      :max-retries MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                                      :prev-max prev-max
                                                      :new-max (:max_tokens bumped)
                                                      :reasoning-length (:reasoning-length data)}}
                                              (str "max_tokens exhausted on reasoning (~"
                                                   (or (:reasoning-length data) "?")
                                                   " reasoning tokens); retry "
                                                   (inc (long max-tokens-attempt))
                                                   "/" MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                                   " with max_tokens=" (:max_tokens bumped)))
                                    ;; Bump max-tokens-attempt so a second cap-hit
                                    ;; doesn't loop forever; keep `attempt` flat so a
                                    ;; subsequent stream-truncated still has its own
                                    ;; quota.
                                    {::retry-max-tokens bumped})
                                  ;; Post-refresh auth 401: the token we
                                  ;; JUST force-refreshed 401'd AGAIN. Almost
                                  ;; always OAuth PROPAGATION LAG at the
                                  ;; provider edge (a freshly-minted token is
                                  ;; briefly not-yet-valid), NOT a dead
                                  ;; credential — the same token succeeds
                                  ;; seconds later. Re-minting is what CAUSES
                                  ;; the storm, so DON'T refresh: back off and
                                  ;; retry the SAME token until it settles.
                                  (and (< (long attempt) (long MAX_AUTH_REFRESH_RETRIES))
                                       (refresh-just-failed? e resolved-model))
                                  ::retry-auth-backoff
                                  ;; Auth 401/403 from a refreshable
                                  ;; provider: force an OAuth refresh +
                                  ;; router rebuild, then re-send once.
                                  ;; `try-refresh-provider-token!` does
                                  ;; the work and returns true only when
                                  ;; a refresh actually happened.
                                  (and (< (long attempt) (long MAX_AUTH_REFRESH_RETRIES))
                                       (auth-refreshable-error? e resolved-model)
                                       (try-refresh-provider-token! resolved-model))
                                  ::retry-auth-refresh
                                  ;; svar gave up on the single pinned
                                  ;; provider (transient 5xx / dropped
                                  ;; connection) and threw its terminal
                                  ;; `provider-unavailable`. On the MAIN
                                  ;; turn we don't hop providers, but a
                                  ;; momentary blip usually clears: back
                                  ;; off (widening delay) and re-send the
                                  ;; SAME request a few times before the
                                  ;; turn fails with the card.
                                  (provider-unavailable-retry? e pu-attempt)
                                  (let [delay-ms (provider-unavailable-retry-delay-ms pu-attempt)]
                                    (tel/log! {:level :warn
                                               :id ::provider-unavailable-retry
                                               :data {:iteration iteration
                                                      :attempt (inc (long pu-attempt))
                                                      :max-retries MAX_PROVIDER_UNAVAILABLE_RETRIES
                                                      :delay-ms delay-ms
                                                      :status (:status (ex-data e))}}
                                              (str "Provider unavailable, transparent retry "
                                                   (inc (long pu-attempt))
                                                   "/" MAX_PROVIDER_UNAVAILABLE_RETRIES))
                                    (Thread/sleep (long delay-ms))
                                    ::retry-provider-unavailable)
                                  :else (handle-iteration-exception! e
                                                                     {:iteration iteration
                                                                      :messages effective-messages
                                                                      :routing effective-routing
                                                                      :reasoning-level
                                                                      reasoning-level}))))]
                        (if-let [[attempt* max-tokens-attempt* pu-attempt*]
                                 (next-retry-counters result
                                                      {:attempt attempt
                                                       :max-tokens-attempt max-tokens-attempt
                                                       :pu-attempt pu-attempt})]
                          (let [attempt* (long attempt*)
                                max-tokens-attempt* (long max-tokens-attempt*)
                                pu-attempt* (long pu-attempt*)]

                            (cond (and (map? result) (contains? result ::retry-max-tokens))
                                  (recur attempt*
                                         max-tokens-attempt*
                                         pu-attempt*
                                         (::retry-max-tokens result)
                                         env)
                                  (= result ::retry-auth-refresh)
                                  ;; Token was force-refreshed and the router rebuilt;
                                  ;; reseat THIS turn's env onto the fresh router before
                                  ;; re-sending (run-iteration uses (:router env)).
                                  (recur attempt*
                                         max-tokens-attempt*
                                         pu-attempt*
                                         current-extra-body
                                         (assoc env :router (get-router)))
                                  (= result ::retry-auth-backoff)
                                  ;; Same just-refreshed token 401'd (propagation lag):
                                  ;; wait, then re-send the SAME token — no re-mint, no
                                  ;; router rebuild. `attempt` grows so backoff widens and
                                  ;; the retry budget still bounds it.
                                  (do (Thread/sleep (long (auth-propagation-backoff-ms attempt)))
                                      (recur attempt*
                                             max-tokens-attempt*
                                             pu-attempt*
                                             current-extra-body
                                             env))
                                  ;; ::retry-stream / ::retry-provider-unavailable: same
                                  ;; request, same env; only the owning counter advanced.
                                  :else (recur attempt*
                                               max-tokens-attempt*
                                               pu-attempt*
                                               current-extra-body
                                               env)))
                          result)))]

                (if-let [iteration-error-data (::iteration-error iteration-result)]
                  ;; Cancellation short-circuit. When the user pressed Esc
                  ;; mid-call, `cancel!` flipped the flag BEFORE
                  ;; future-cancel, so by the time we land here the flag is
                  ;; already true. Treat the resulting interrupt-shaped
                  ;; \"iteration-error-data\" as cancellation, not a real failure: skip
                  ;; the trace entry, skip the DB write, skip the on-chunk
                  ;; error chunk (otherwise the bubble paints a phantom
                  ;; ITERATION N ERROR block right next to FINAL ANSWER:
                  ;; \"_Cancelled by user._\"). Bail straight to the cancel
                  ;; result that the top-of-loop branch would have produced.
                  (if (and cancel-atom @cancel-atom)
                    (do (log-stage! :error iteration {:reason :cancelled})
                        (let [sticky (some-> (:turn-state-atom environment)
                                             deref
                                             :best-answer
                                             :value)
                              result (merge {:answer sticky
                                             :status :cancelled
                                             :status-id (status->id :cancelled)
                                             :trace trace
                                             :iteration-count iteration}
                                            (finalize-cost))]

                          result))
                    (let [llm-provider-error (llm-provider-error-context iteration
                                                                         iteration-error-data)
                          ;; Consecutive provider-generate failure streak.
                          ;; Counts only :llm-provider/generate errors (the
                          ;; model never produced usable content); any other
                          ;; error kind resets - those are RLM-correctable.
                          provider-error-streak (next-provider-error-streak (:provider-error-streak
                                                                              loop-state)
                                                                            llm-provider-error)
                          provider-breaker-tripped? (provider-error-breaker-tripped?
                                                      provider-error-streak)
                          error-feedback
                          (iteration-error-feedback iteration iteration-error-data user-request)
                          trace-entry
                          {:iteration iteration :error iteration-error-data :final? false}
                          ;; Preserve forensic evidence on every error
                          ;; path, not just `:empty-content`. Pre-fix
                          ;; only empty-content carried `:reasoning`
                          ;; into the DB row; `:max-tokens-exceeded`
                          ;; (svar.llm) and any other generate-time
                          ;; failure had their reasoning silently
                          ;; dropped: the model can emit reasoning
                          ;; tokens before a cap-truncation, but the
                          ;; persisted row had `:thinking nil` so the
                          ;; transcript could not show what the model
                          ;; was actually thinking about.
                          ;;
                          ;; INTEGRITY NOTE: `err-data` is the raw
                          ;; `ex-data` of svar's thrown exception (see
                          ;; `exception->iteration-error-data` →
                          ;; `format-exception` which just attaches
                          ;; `(ex-data t)` verbatim). `:reasoning` /
                          ;; `:content` / `:partial-content` /
                          ;; `:api-usage` are produced by svar's
                          ;; `envelope-data` (`internal/llm.clj`) which
                          ;; only `assoc`s the SSE-accumulator values
                          ;; — NO transformation, no synthesis. The
                          ;; same `reasoning` variable feeds the
                          ;; success path's `:thinking` column. We
                          ;; never invent reasoning text here.
                          err-data (:data iteration-error-data)
                          err-reasoning (:reasoning err-data)
                          err-partial-content (or (:content err-data) (:partial-content err-data))
                          err-api-usage (or (:api-usage iteration-result) (:api-usage err-data))
                          err-iteration-id
                          (persistance/db-store-iteration!
                            (:db-info environment)
                            (let [tc (iteration-token-cost err-api-usage)]
                              (cond-> {:session-turn-id session-turn-id
                                       :vars []
                                       :code (or err-partial-content "")
                                       :thinking err-reasoning
                                       :duration-ms 0
                                       :llm-full-duration-ms 0
                                       :error iteration-error-data
                                       :llm-messages effective-messages
                                       :llm-provider (:provider resolved-model)
                                       :llm-model (str (:name resolved-model))
                                       :llm-routing
                                       (cond-> {:selected (llm-id (:provider resolved-model)
                                                                  (some-> (:name resolved-model)
                                                                          str))
                                                :actual (llm-id (:provider resolved-model)
                                                                (some-> (:name resolved-model)
                                                                        str))
                                                :fallback? false}
                                         (seq (get-in iteration-error-data [:data :routed/trace]))
                                         (assoc :fallback?
                                           true :trace
                                           (vec (get-in iteration-error-data
                                                        [:data :routed/trace]))))
                                       :cache-created-tokens (iteration-cache-created-tokens tc)}
                                tc
                                (assoc :tokens
                                  (:tokens tc) :cost-usd
                                  (:cost-usd tc)))))]

                      (ctx-loop/set-turn-state! environment :iteration-id err-iteration-id)
                      ;; Live error chunk - `:phase :iteration-error`
                      ;; signals the iteration aborted before any
                      ;; block could run. No block chunks fired
                      ;; this iteration, so the channel sees a clean
                      ;; reasoning -> error transition.
                      (emit-hook! on-chunk
                                  {:phase :iteration-error
                                   :iteration (inc (long iteration))
                                   :thinking err-reasoning
                                   :error iteration-error-data
                                   :done? true}
                                  "on-chunk (iteration error)")
                      (if (or (::fatal-iteration-error iteration-result)
                              ;; Circuit breaker: N consecutive provider-generate
                              ;; failures - feeding the same request back cannot
                              ;; help; fail the turn as a provider error.
                              provider-breaker-tripped?)
                        (let
                          [trace' (conj trace trace-entry)
                           fallback
                           (or (some-> (:error trace-entry)
                                       perr/provider-error-content)
                               [(content/error
                                  "provider_unavailable"
                                  "The model provider failed before Vis received a usable response."
                                  true)])
                           result (merge {:answer fallback
                                          :status :error
                                          :status-id (status->id :error)
                                          :trace trace'
                                          :iteration-count (inc (long iteration))}
                                         (finalize-cost))]

                          result)
                        (recur (assoc loop-state
                                 :iteration (inc (long iteration))
                                 :provider-error-streak provider-error-streak
                                 :empty-iteration-streak 0
                                 :messages (conj messages {:role "user" :content error-feedback})
                                 :llm-provider {:error llm-provider-error}
                                 :trace (conj trace trace-entry))))))
                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        _ (note-provider-request-ok! resolved-model)
                        {:keys [thinking assistant-prose blocks final-result]} iteration-result
                        block (first blocks)
                        ;; Phase 7: merge per-iteration `:lru` stamps
                        ;; (collected by the patched resolve-symbol*)
                        ;; into the long-lived per-env LRU map. The trailer's
                        ;; live-vars view reads this to age user vars out of
                        ;; the discovery line after
                        ;; `JOURNAL_LRU_TURN_WINDOW` quiet turns.
                        ;; Phase 7 LRU merge. Flat: read both ends, then
                        ;; a single guarded swap!. No nested when-let.
                        lru-atom (:def-resolve-lru-atom environment)
                        iteration-lru (not-empty (:lru block))
                        _ (when (and lru-atom iteration-lru) (swap! lru-atom merge iteration-lru))
                        ;; Each executed block becomes one envelope on the
                        ;; :forms column. `:code` is the concatenated block
                        ;; bodies for forensics.
                        ;; Cursor for envelope keying. `iteration` here
                        ;; is the 0-based loop counter; the loop normalizes it to
                        ;; 1-based via `ctx-loop/set-turn-state!` at the top of
                        ;; each iter. The renderer + cursor-snapshot consume that
                        ;; atom, so persisted form scopes and rendered context
                        ;; agree.
                        cursor {:turn (or (:turn-position (ctx-loop/read-turn-state environment)) 1)
                                :iter (or (:iteration (ctx-loop/read-turn-state environment))
                                          (inc (long (or iteration 0))))}
                        ;; One block = one tool call = one form: each block maps
                        ;; 1:1 to a form envelope. The block already carries its
                        ;; whole :result / :stdout / :error and its `:channel`
                        ;; slice (the pre-rendered tool sink IR persisted on
                        ;; `session_turn_iteration.forms`), so `blocks->forms`
                        ;; projects it directly with no per-statement explosion.
                        ;; Tag resolver: lift extension-declared
                        ;; observation/mutation tag into `classify-form-tag`
                        ;; so extension tools (`patch`, `git_commit`,
                        ;; `git_push`, …) classify correctly without the
                        ;; engine hard-coding their head symbol.
                        ;;
                        ;; The HEAD `classify-form-tag` reads off the model's
                        ;; source is the snake_case Python CALL name
                        ;; (`git_push`), but `extension/op-tag` is keyed by
                        ;; canonical op keywords (`:git/push!`). The old
                        ;; `(keyword "git_push")` lookup never hit `:git/push!`,
                        ;; so EVERY extension mutation fell through to
                        ;; `:observation`. `ctx-renderer/fold-op-index` is
                        ;; the ONE memoized fold to sandbox call names (the
                        ;; same fold the globals bind under) — shared with the
                        ;; trailer's model-render lookup, and no longer rebuilt
                        ;; on every iteration. Unregistered heads miss the
                        ;; map and fall through to the engine's core mutation
                        ;; set inside `classify-form-tag`.
                        py-name->tag (ctx-renderer/fold-op-index (extension/op-tag-index))
                        head-tag-resolver (fn [head-sym]
                                            (when head-sym (get py-name->tag (str head-sym))))
                        ;; Native tool calling: an iteration is EITHER tool
                        ;; calls (→ executable blocks → forms) OR a plain-text
                        ;; answer (`:stop-reason :end`, finalized above) which
                        ;; carries NO forms. A no-block iteration therefore has
                        ;; an empty form vector — never a synthetic
                        ;; `{:error "empty iteration"}` artifact. With
                        ;; tools+answer there is no such "empty" reply to flag:
                        ;; the answer is the answer, and it renders as the answer
                        ;; (not as a failed form / "empty iteration" card).
                        forms-vec (if (seq blocks)
                                    (ctx-engine/blocks->forms blocks cursor head-tag-resolver)
                                    [])
                        block-code (str/join "\n" (keep :code blocks))
                        first-block (or (first blocks) {})
                        ;; Outbound artifacts a tool call PRODUCED this
                        ;; iteration: every artifact a block PRODUCED (matplotlib
                        ;; `plt.show()`/`savefig`), captured at the SOURCE into the
                        ;; sandbox sink and stamped with the block's tool-call-id, so
                        ;; the figure PNG is OWNED by the DB and survives a
                        ;; restart / replay (V1 only kept the temp-file path).
                        iteration-attachments
                        (into []
                              (mapcat (fn [b]
                                        (map #(assoc % :tool-call-id (:svar/tool-call-id b))
                                             (:attachments b))))
                              blocks)
                        iteration-id
                        (persistance/db-store-iteration!
                          (:db-info environment)
                          ;; Price by the ACTUAL serving model (`:llm-model` =
                          ;; routed metadata), not the pre-resolved root — a
                          ;; fallback iteration must not bill at the selected
                          ;; model's rates.
                          (let [tc (iteration-token-cost (:api-usage iteration-result)
                                                         (:llm-model iteration-result))]
                            (cond-> {:session-turn-id session-turn-id
                                     :code (or block-code "")
                                     :forms forms-vec
                                     :attachments (attachment-storage/offload-attachments
                                                    iteration-attachments)
                                     :duration-ms
                                     (long (or (envelope-duration-ms (:envelope first-block)) 0))
                                     :llm-full-duration-ms (long (or (:duration-ms iteration-result)
                                                                     0))
                                     :thinking thinking
                                     :assistant-prose assistant-prose
                                     :answer (when final-result
                                               (answer-markdown (:answer final-result)))
                                     :llm-provider (or (:llm-provider iteration-result)
                                                       (:provider resolved-model))
                                     :llm-model (:llm-model iteration-result)
                                     :llm-returned-empty-code? (:llm-returned-empty-code?
                                                                 iteration-result)
                                     :llm-assistant-message (:assistant-message iteration-result)
                                     :llm-routing (llm-routing-summary pre-resolved-model
                                                                       iteration-result)
                                     :cache-created-tokens (iteration-cache-created-tokens tc)}
                              tc
                              (assoc :tokens
                                (:tokens tc) :cost-usd
                                (:cost-usd tc)))))
                        _ (ctx-loop/set-turn-state! environment :iteration-id iteration-id)
                        ;; =====================================================
                        ;; Context end-of-iter bookkeeping.
                        ;; =====================================================
                        ctx-atom-ref (:ctx-atom environment)
                        _ (when ctx-atom-ref
                            (swap! ctx-atom-ref (fn [c]
                                                  ;; `cursor` is the loop-internal keyword shape
                                                  ;; ({:turn :iter} — blocks->forms destructures
                                                  ;; it); the ctx is STRING-KEYED, so project to
                                                  ;; the "session_scope" shape at the seam.
                                                  (ctx-engine/advance-iter (assoc c
                                                                             "session_scope"
                                                                             {"turn" (:turn cursor)
                                                                              "iter" (:iter cursor)
                                                                              "next_form" 1})
                                                                           forms-vec))))
                        _ (when ctx-atom-ref
                            (tel/log! {:level :info
                                       :id ::iter-end-ctx
                                       :data {:iteration iteration
                                              :cursor cursor
                                              :pinned-forms (count forms-vec)}}
                                      "CTX iter-end: cursor advanced"))
                        trace-entry {:iteration iteration
                                     :thinking thinking
                                     :assistant-prose assistant-prose
                                     :blocks blocks
                                     :reasoning-effort (when reasoning-effort
                                                         (reasoning-effort-iteration-evidence
                                                           iteration
                                                           reasoning-effort
                                                           pre-resolved-model
                                                           iteration-result))
                                     :final? (boolean final-result)}]

                    (cond
                      final-result
                      (do (log-stage! :final
                                      iteration
                                      {:answer (answer-markdown (:answer final-result))
                                       :iteration-count (inc (long iteration))})
                          (log-stage! :iteration/stop
                                      iteration
                                      {:blocks (count blocks)
                                       :errors (count (filter :error blocks))
                                       :times (mapv block-duration-ms blocks)})
                          ;; Iteration-final chunk (`:phase :iteration-final`).
                          ;; Per-block chunks already streamed every block
                          ;; result; this is the trim \"iteration is
                          ;; complete, here is the terminal answer\"
                          ;; signal. Consumers attach `:final` to
                          ;; whatever's already on screen. An answer is plain
                          ;; prose with no form slot, so `:answer-position`
                          ;; is nil.
                          (when on-chunk
                            (on-chunk {:phase :iteration-final
                                       :iteration (inc (long iteration))
                                       :thinking thinking
                                       :assistant-prose assistant-prose
                                       :iteration-id iteration-id
                                       :attachment-count (count iteration-attachments)
                                       :final {:answer (:answer final-result)
                                               :iteration-count (inc (long iteration))
                                               :status :success}
                                       :answer-position (:answer-position final-result)
                                       ;; Live working-memory snapshot so the F2
                                       ;; context dialog updates DURING the turn,
                                       ;; not only after it ends.
                                       :done? true}))
                          (let [result (-> (merge {:answer (:answer final-result)
                                                   :trace (conj trace trace-entry)
                                                   :iteration-count (inc (long iteration))
                                                   :utilization
                                                   (let [u @usage-atom
                                                         req (if (pos? (long (:iter-count u)))
                                                               (long (:last-iter-input u))
                                                               (long (:previous-request-input u)))]

                                                     (ctx-engine/utilization
                                                       req
                                                       effective-context-limit
                                                       (:input-tokens u)
                                                       ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                                  (finalize-cost))
                                           (attach-llm-routing-summary pre-resolved-model
                                                                       iteration-result))]
                            (auto-archive-hot-symbols! environment)
                            result))
                      :else
                      (if (empty? blocks)
                        (let [empty-streak (inc (long (or (:empty-iteration-streak loop-state) 0)))]
                          (log-stage! :empty iteration {:empty-streak empty-streak})
                          (log-stage! :iteration/stop iteration {:blocks 0 :errors 0 :times []})
                          (if (>= empty-streak (long CONSECUTIVE_EMPTY_REPLY_LIMIT))
                            ;; Too many consecutive empty replies — finalize on the
                            ;; best sticky answer (give-up text if none) instead of
                            ;; re-invoking forever. Mirrors the forced-finalize shape.
                            (let [answer (or (some-> (:turn-state-atom environment)
                                                     deref
                                                     :best-answer
                                                     :value)
                                             {:answer loop-give-up-text})]
                              (log-stage! :final
                                          iteration
                                          {:reason :empty-replies
                                           :iteration-count (inc (long iteration))})
                              (when on-chunk
                                (on-chunk {:phase :iteration-final
                                           :iteration (inc (long iteration))
                                           :thinking thinking
                                           :assistant-prose assistant-prose
                                           :iteration-id iteration-id
                                           :attachment-count (count iteration-attachments)
                                           :final {:answer answer
                                                   :iteration-count (inc (long iteration))
                                                   :status :success}
                                           :done? true}))
                              (-> (merge {:answer answer
                                          :trace (conj trace trace-entry)
                                          :iteration-count (inc (long iteration))
                                          :utilization
                                          (let [u @usage-atom
                                                req (if (pos? (long (:iter-count u)))
                                                      (long (:last-iter-input u))
                                                      (long (:previous-request-input u)))]

                                            (ctx-engine/utilization
                                              req
                                              effective-context-limit
                                              (:input-tokens u)
                                              ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                         (finalize-cost))
                                  (attach-llm-routing-summary pre-resolved-model iteration-result)))
                            ;; Transparent auto-continue: re-invoke so a mid-task
                            ;; thinking-only blip turns into real output next round.
                            (recur (merge loop-state
                                          {:iteration (inc (long iteration))
                                           :provider-error-streak 0
                                           :empty-iteration-streak empty-streak
                                           :trace (conj trace trace-entry)}))))
                        (do
                          (log-stage! :iteration/stop
                                      iteration
                                      {:blocks (count blocks)
                                       :errors (count (filter :error blocks))
                                       :times (mapv block-duration-ms blocks)})
                          (let [_ blocks
                                ;; Repetition-only loop detection (no iteration or
                                ;; budget counting): identical action code repeated
                                ;; across iterations ⇒ stuck.
                                {:keys [stuck? action-sig]}
                                (repetition-loop-state blocks (:stuck loop-state))
                                nudged? (boolean (:nudged? (:stuck loop-state)))
                                sticky (some-> (:turn-state-atom environment)
                                               deref
                                               :best-answer
                                               :value)
                                ;; Checkpoint already shown AND still stuck ⇒ force-finalize.
                                forced? (and nudged? stuck?)
                                forced-answer (or sticky {:answer loop-give-up-text})
                                ;; ctx-diff for THIS iteration: the standing context
                                ;; AFTER its code ran, captured ONLY if it changed since
                                ;; the model last saw it (this iter started an nREPL,
                                ;; switched model, added a dir, …). It rides INSIDE this
                                ;; iteration's <results> message (see
                                ;; `iteration-results-message`) and advances the running
                                ;; baseline, so the change is attributed to the code that
                                ;; caused it — append-only, no stray context messages.
                                iter-ctx-diff (let [;; util-inclusive: live token usage rides as a cheap
                                                    ;; appended `session["utilization"] = …` delta (the
                                                    ;; frozen block stays util-free for cache stability).
                                                    cur (ctx-loop/render-block!
                                                          environment
                                                          ctx-renderer/ctx-delta-map)
                                                    prev @last-context-atom]

                                                (when (and cur (not= cur prev))
                                                  (reset! last-context-atom cur)
                                                  ;; carry the baseline ACROSS turns so the next turn
                                                  ;; diffs against the last-emitted state, not a re-render.
                                                  (some-> standing-ctx-atom
                                                          (swap! assoc :baseline cur))
                                                  ;; structural Python delta (session[…] = … / del),
                                                  ;; not the whole <context> block — append-only.
                                                  (ctx-renderer/render-ctx-delta prev cur)))
                                next-recent (conj (vec (or trailer-iters []))
                                                  [(inc (long iteration))
                                                   {:thinking thinking
                                                    :blocks blocks
                                                    ;; The `forms-vec` (each `{:scope :result …}`) is the
                                                    ;; ONE scope source: persistence and the context
                                                    ;; wire both read it, so scopes stay consistent.
                                                    :forms-vec forms-vec
                                                    ;; Outbound image artifacts this iteration's
                                                    ;; tool calls produced (matplotlib figures),
                                                    ;; each `{:tool-call-id :media-type :base64 …}`.
                                                    ;; The conversation-suffix replays them as a
                                                    ;; vision user message so the model SEES its
                                                    ;; own plots within the turn.
                                                    :attachments iteration-attachments
                                                    :ctx-diff iter-ctx-diff
                                                    :llm-provider (:llm-provider iteration-result)
                                                    :llm-model (:llm-model iteration-result)
                                                    ;; svar's canonical replay handle for this
                                                    ;; iteration. Re-emitted only within this
                                                    ;; live user turn via
                                                    ;; `append-preserved-thinking-replay`; cross-turn
                                                    ;; seeds opt out with
                                                    ;; `:preserved-thinking/replay? false`.
                                                    :assistant-message (:assistant-message
                                                                         iteration-result)
                                                    ;; Native tool calls for this iteration — iteration-results-message
                                                    ;; pairs one `tool_result` block per call's :id (the API requires
                                                    ;; every tool_use be answered).
                                                    :tool-calls (:tool-calls iteration-result)
                                                    :preserved-thinking/replay? true}])]

                            ;; ONE iteration-final chunk, AFTER the decision. Terminal
                            ;; (:done? true + :final answer) when force-finalizing so
                            ;; live channels render the forced answer; a non-terminal
                            ;; chunk before the force-finalize would never show it.
                            (when on-chunk
                              (on-chunk
                                (if forced?
                                  {:phase :iteration-final
                                   :iteration (inc (long iteration))
                                   :thinking thinking
                                   :assistant-prose assistant-prose
                                   :iteration-id iteration-id
                                   :attachment-count (count iteration-attachments)
                                   :final {:answer forced-answer
                                           :iteration-count (inc (long iteration))
                                           :status :success}
                                   :done? true}
                                  {:phase :iteration-final
                                   :iteration (inc (long iteration))
                                   :thinking thinking
                                   :assistant-prose assistant-prose
                                   :iteration-id iteration-id
                                   :attachment-count (count iteration-attachments)
                                   :final nil
                                   :done? false})))
                            (if forced?
                              (do (log-stage! :final
                                              iteration
                                              {:reason :loop-forced
                                               :iteration-count (inc (long iteration))})
                                  ;; NB: no `:status` key — mirrors the normal
                                  ;; success path so prior_outcome derives to
                                  ;; `complete` (a bare `:status :success` violates
                                  ;; the session_turn_state.prior_outcome CHECK).
                                  (-> (merge {:answer forced-answer
                                              :trace (conj trace trace-entry)
                                              :iteration-count (inc (long iteration))
                                              :utilization
                                              (let [u @usage-atom
                                                    req (if (pos? (long (:iter-count u)))
                                                          (long (:last-iter-input u))
                                                          (long (:previous-request-input u)))]

                                                (ctx-engine/utilization
                                                  req
                                                  effective-context-limit
                                                  (:input-tokens u)
                                                  ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                             (finalize-cost))
                                      (attach-llm-routing-summary pre-resolved-model
                                                                  iteration-result)))
                              (recur
                                (merge
                                  (dissoc loop-state :llm-provider)
                                  {:iteration (inc (long iteration))
                                   :provider-error-streak 0
                                   :empty-iteration-streak 0
                                   ;; Inject ONE guidance turn when repetition
                                   ;; is detected → stern decision-checkpoint.
                                   :messages (cond-> messages
                                               stuck?
                                               (conj {:role "user"
                                                      :content (loop-checkpoint-message
                                                                 (when sticky
                                                                   (answer-markdown sticky)))}))
                                   :trace (conj trace trace-entry)
                                   :trailer-iters next-recent
                                   :stuck {:last-sig action-sig :nudged? stuck?}})))))))))))))))))

(defn- slash-ctx-for-env
  "Build the slash dispatch ctx from a turn env. Pure data; carries
   the channel/session/workspace coordinates the slash handlers read."
  [env user-request]
  (let [db-info
        (:db-info env)

        state-id
        (or (:session/state-id env)
            (when db-info
              (some-> (:session-id env)
                      (persistance/db-latest-session-state-id db-info))))]

    (cond-> {:channel/id (or (:channel env) :tui)
             :session/id (:session-id env)
             :db-info db-info
             :command/raw user-request}
      state-id
      (assoc :session/state-id state-id)

      (:session-title-atom env)
      (assoc :session-title-atom (:session-title-atom env))

      (:workspace/id env)
      (assoc :workspace/id (:workspace/id env))

      (:workspace-atom env)
      (assoc :workspace-atom (:workspace-atom env)))))

(defn- slash-body->markdown
  "Project a slash body to Markdown without constructing renderer IR."
  [body]
  (cond (nil? body) nil
        (string? body) body
        (and (vector? body) (every? content/block-valid? body)) (content/text-projection body)
        :else (pr-str body)))


(defn- slash-result->answer-markdown
  "Build the prose Markdown carried by a slash result's canonical prose block."
  [{:keys [result error reason]}]
  (cond result (let [title
                     (or (:slash/title result) "Slash handled")

                     body
                     (some-> (:slash/body result)
                             slash-body->markdown
                             str/trim
                             not-empty)]

                 (cond-> (str "**" title "**")
                   body
                   (str "\n\n" body)))
        error (str "**Slash failed**\n\n" error)
        reason (str "**Slash unavailable**\n\n" reason)
        :else "**Slash handled**"))

(defn- slash-result->wire
  "STRINGS-ONLY view of a slash result for the form envelope `:result`. That
   envelope is stored and later crosses the Clojure->Python boundary via `->py`,
   which is strings-only (no silent keyword->string — a keyword there throws).
   The keyword `:slash/*` map + IR body/data stay Clojure-side (answer-markdown
   and the live/restored answer bubble render from those); the model only needs
   the outcome, so keys are strings and enum values are stringified here AT THE
   SOURCE."
  [{:keys [result error reason]}]
  (if result
    (cond-> {"slash/status" (name (or (:slash/status result) :ok))}
      (:slash/title result)
      (assoc "slash/title" (str (:slash/title result))))
    (cond-> {"slash/status" "error" "slash/title" (str (or error "slash error"))}
      reason
      (assoc "slash/reason" (name reason)))))

(defn- apply-slash-mutations!
  "Compatibility hook for slash results that used to mutate context state.
   No slash-driven context mutations are currently supported."
  [_env _slash-result]
  ;; No-op: slash results no longer mutate context.
  nil)

(defn- run-slash-turn!
  "Persist a slash-only turn: one `session_turn_soul` + state + ONE
   synthetic `session_turn_iteration` whose forms vec carries the slash
   envelope at `:tag :user-slash`. The turn is marked :success without
   any LLM round-trip. Returns the same shape `iteration-loop` would
   have produced (so callers don't special-case slash turns).

   Slash context mutations are no longer applied; the synthetic iter row is
   persisted for audit/history only."
  [env user-request slash-result]
  (let [db-info
        (:db-info env)

        turn-id
        (persistance/db-store-session-turn!
          db-info
          {:parent-session-id (:session-id env) :user-request user-request :status :running})

        turn-pos
        (or (session-turn-position env turn-id) 1)]

    ;; Stamp turn-state so synthesize-scope returns the canonical
    ;; `t<N>/i1/f1` scope for any CTX mutations the slash emits.
    (ctx-loop/set-turn-state! env
                              :iteration-id nil
                              :session-turn-id turn-id
                              :user-request user-request
                              :turn-position turn-pos
                              :iteration 1
                              :form-idx 0)
    (apply-slash-mutations! env slash-result)
    (let [scope
          (str "t" turn-pos "/i1/f1")

          envelope
          {:scope scope
           :tag :user-slash
           :src user-request
           ;; STRINGS-ONLY: this crosses the Python boundary via ->py.
           :result (slash-result->wire slash-result)}

          answer-md
          (slash-result->answer-markdown slash-result)

          ;; Snapshot the CTX as it stands AFTER the slash mutations
          ;; so resume picks up the spec/task/fact writes. Mirrors
          ;; run-normal-turn!'s ctx-snapshot path: gc-pass + strip
          ;; cursor + drop ephemerals before Nippy-encoding.
          ctx-snapshot
          (when-let [ca (:ctx-atom env)]
            (let [stamped (ctx-loop/stamp-cursor env @ca)
                  gced (ctx-engine/gc-pass stamped)
                  clean (-> gced
                            (dissoc "session_scope")
                            ctx-engine/strip-ephemeral)]

              (reset! ca clean)
              clean))]

      (try (persistance/db-store-iteration! db-info
                                            {:session-turn-id turn-id
                                             :code user-request
                                             :forms [envelope]
                                             :duration-ms 0
                                             :llm-full-duration-ms 0
                                             :thinking ""
                                             :answer answer-md
                                             :llm-messages []
                                             :llm-returned-empty-code? false})
           (catch Throwable t
             (tel/log!
               {:level :warn :id ::slash-iter-persist-failed :data {:error (ex-message t)}})))
      (persistance/db-update-session-turn! db-info
                                           turn-id
                                           {:content [(content/prose answer-md)]
                                            :iteration-count 1
                                            :duration-ms 0
                                            :status :success
                                            :prior-outcome :complete
                                            :ctx ctx-snapshot})
      {:session-turn-id turn-id
       :answer answer-md
       :iteration-count 1
       :duration-ms 0
       :status :success
       :slash slash-result
       :prior-outcome :complete})))
(defn- run-normal-turn!
  "LLM round-trip path: store turn, run iteration-loop, persist
   the end-of-turn CTX snapshot, update the turn row with answer +
   tokens. Called by `run-turn!` when slash dispatch said the user
   message was NOT a slash."
  [env user-request loop-opts]
  (let [;; Persist EVERY image the user attached to this turn as durable
        ;; `session_turn_attachment` BLOB bytes: INLINE uploads (web/API base64,
        ;; carried on `:user/attachments`) AND terminal-drop images (paths pasted
        ;; into the message, sniffed + loaded here via the same magic-byte scan
        ;; the assemble seam uses). Storing the bytes - not just the on-disk path
        ;; - lets resume + history re-render survive the source file moving or
        ;; being deleted. Best-effort: a scan failure never blocks the turn.
        disk-attachments
        (try (:attached (attachments/collect-user-images user-request
                                                         {:workspace-root (:workspace/root env)}))
             (catch Throwable t
               (tel/log!
                 {:level :warn :id ::turn-image-persist-scan-failed :data {:error (ex-message t)}})
               nil))

        turn-attachments
        (into (vec (:user/attachments env)) disk-attachments)

        session-turn-id
        (persistance/db-store-session-turn!
          (:db-info env)
          (cond-> {:parent-session-id (:session-id env) :user-request user-request :status :running}
            (seq turn-attachments)
            (assoc :attachments (attachment-storage/offload-attachments turn-attachments))))

        turn-position
        (session-turn-position env session-turn-id)

        _
        (ctx-loop/set-turn-state! env
                                  :session-turn-id session-turn-id
                                  :user-request user-request
                                  :turn-position (or turn-position 1)
                                  :iteration nil
                                  :form-idx nil
                                  :iteration-id nil)

        _
        (titling/maybe-auto-title! env user-request)

        result
        (iteration-loop env user-request (assoc loop-opts :session-turn-id session-turn-id))

        prior-outcome
        (:status result)

        ;; Snapshot the CTX as it stands at end-of-turn. Run gc-pass first
        ;; so terminal-status entries past their TTL drop out of the live
        ;; tree before persistence; historical snapshots in earlier
        ;; session_turn_state rows still carry them (the archive store + the
        ;; persisted forms rows). The renderer stamps the cursor in fresh each
        ;; call; we drop the cursor before persisting because the next-turn
        ;; loader will derive a new cursor from the loop counters (cursor
        ;; is iter-local, not turn-local). Persisted Nippy-encoded to
        ;; session_turn_state.ctx in the same transaction that flips the
        ;; turn status, so live CTX = ctx on the latest turn-state for the
        ;; latest turn-soul of the session_state.
        ctx-snapshot
        (when-let [ca (:ctx-atom env)]
          (let [stamped (ctx-loop/stamp-cursor env @ca)
                gced (ctx-engine/gc-pass stamped)
                ;; Strip cursor + every `"engine_*"` ephemeral
                ;; key (warnings, pending-satisfies) before
                ;; persisting. The next resume rebuilds the
                ;; cursor from loop counters and starts each
                ;; turn with empty ephemerals via empty-ctx.
                clean (-> gced
                          (dissoc "session_scope")
                          ctx-engine/strip-ephemeral)]

            (reset! ca clean)
            clean))

        _
        (persistance/db-update-session-turn! (:db-info env)
                                             session-turn-id
                                             {:content (content/answer-content (:answer result))
                                              :iteration-count (:iteration-count result)
                                              :duration-ms (:duration-ms result)
                                              :status (or (:status result) :success)
                                              :tokens (:tokens result)
                                              :cost (:cost result)
                                              :prior-outcome prior-outcome
                                              :ctx ctx-snapshot})]

    (assoc result
      :session-turn-id session-turn-id
      :prior-outcome prior-outcome)))

(defn- health-gated-router
  "ONE health gate for every routing entry point (turn start AND
   sub_loop child): demote unreachable LOCAL providers to the router's
   end (`providers/demote-unreachable-providers` — never throws) and
   log the demotion once. Returns `{:router r :demoted [ids]}`."
  [router where]
  (let [{:keys [demoted] :as gated} (providers/demote-unreachable-providers router)]
    (when (seq demoted)
      (tel/log! {:level :warn
                 :id ::unreachable-providers-demoted
                 :data {:demoted demoted :where where}
                 :msg "router health gate: unreachable local providers demoted to last resort"}))
    gated))

(defn- parse-bang
  "Parse a `!`/`!&` shell-sugar user message into `{:kind :run|:bg :cmd :id?}`,
   or nil when `text` is NOT a bang. `!<cmd>` desugars to `shell_run(cmd)`
   (synchronous); `!&<cmd>` desugars to `shell_bg(id, cmd)` in the background
   under an auto-generated resource id. A blank command (a bare `!`) is ordinary
   prose, so it returns nil and the message runs as a normal turn."
  [text]
  (when (string? text)
    (let [t (str/triml text)]
      (cond (str/starts-with? t "!&")
            (let [cmd (str/trim (subs t 2))]
              (when (seq cmd)
                {:kind :bg :cmd cmd :id (str "bg-" (subs (str (java.util.UUID/randomUUID)) 0 8))}))
            (str/starts-with? t "!") (let [cmd (str/trim (subs t 1))]
                                       (when (seq cmd) {:kind :run :cmd cmd}))))))

(defn- bang-card->markdown
  "Combine a native shell op-card `{:summary :body}` into the answer Markdown a
   `!`/`!&` turn shows as its answer bubble."
  [{:keys [summary body]}]
  (let [summary
        (some-> summary
                str
                str/trim
                not-empty)

        body
        (some-> body
                str
                str/trimr
                not-empty)]

    (cond (and summary body) (str summary "\n\n" body)
          summary summary
          body body
          :else "_shell command produced no output_")))

(defn- run-bang-turn!
  "LLM-free `!`/`!&` shell-sugar turn: run the shell tool DIRECTLY (honoring the
   user-owned `:shell/enabled` toggle), then persist ONE synthetic iteration —
   the SAME shape `run-slash-turn!` writes — whose form carries the shell RESULT
   map, native-tool identity, and `:tag :user-shell`. The op-card renders as the
   answer bubble (channels suppress the redundant trace by that tag), and the
   persisted `:result` rides later prompts' prior-turn context exactly as a
   model-issued `shell_run` / `shell_bg` does across turns. Returns the same
   shape `iteration-loop` would (so callers don't special-case bang turns)."
  [env user-request {:keys [kind cmd id]} loop-opts]
  (let [db-info
        (:db-info env)

        turn-id
        (persistance/db-store-session-turn!
          db-info
          {:parent-session-id (:session-id env) :user-request user-request :status :running})

        turn-pos
        (or (session-turn-position env turn-id) 1)

        _
        (ctx-loop/set-turn-state! env
                                  :iteration-id nil
                                  :session-turn-id turn-id
                                  :user-request user-request
                                  :turn-position turn-pos
                                  :iteration 1
                                  :form-idx 0)

        enabled?
        (toggles/enabled? :shell/enabled)

        tool-name
        (if (= kind :bg) "shell_bg" "shell_run")

        t0
        (System/currentTimeMillis)

        ;; Run the shell tool. `requiring-resolve` keeps foundation-shell a
        ;; DROPPABLE plug-in (nil when the jar is absent) and avoids a compile-time
        ;; cycle; the `:shell/enabled` gate is applied HERE (the symbol's own
        ;; before-fn gate is bypassed by the direct var call).
        on-chunk
        (or (:on-chunk loop-opts) (get-in loop-opts [:hooks :on-chunk]))

        ;; A bang turn never enters `iteration-loop` (the only path that streams
        ;; live `:progress` activity), so while the shell tool blocks the live
        ;; bubble shows the zero-iterations placeholder and claims Vis is
        ;; "calling the provider". Emit ONE shell-phase chunk BEFORE the
        ;; blocking call so the tracker renders `Vis is running: <cmd>` instead.
        _
        (when (and enabled? (fn? on-chunk))
          (try (on-chunk {:phase (if (= kind :bg) :shell-bg :shell-run) :iteration 1 :cmd cmd})
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::bang-progress-emit-failed
                            :data {:cmd cmd :error (ex-message t)}}))))

        envelope
        (when enabled?
          (try
            (if (= kind :bg)
              ((requiring-resolve 'com.blockether.vis.internal.foundation.shell/shell-bg)
                env
                id
                cmd)
              ((requiring-resolve 'com.blockether.vis.internal.foundation.shell/shell-run) env cmd))
            (catch Throwable t
              (tel/log! {:level :warn :id ::bang-run-threw :data {:cmd cmd :error (ex-message t)}})
              {:result nil :error {:message (or (ex-message t) (str t))}})))

        t1
        (System/currentTimeMillis)

        result-map
        (:result envelope)

        err
        (:error envelope)

        active-exts
        ;; `registered-extensions` (NOT activation-filtered) so the shell op-card
        ;; renderers resolve even when the shell layer's activation-fn is false —
        ;; the `:shell/enabled` gate is applied above, renderer lookup is display-only.
        (try (extension/registered-extensions) (catch Throwable _ []))

        renderers
        (try (extension/native-tool-renderers active-exts) (catch Throwable _ {}))

        color-roles
        (try (extension/native-tool-color-roles active-exts) (catch Throwable _ {}))

        display
        (when (some? result-map)
          (try (tool-result-display {:result result-map} tool-name renderers)
               (catch Throwable _ nil)))

        answer-md
        (cond (not enabled?) (str "**Shell layer is OFF.** Only you can enable it: settings dialog"
                                  " → 'Shell commands (compatibility layer)'. Then `"
                                  cmd
                                  "` will run.")
              (some? err) (str "**shell error**\n\n```\n" (or (:message err) (pr-str err)) "\n```")
              display (bang-card->markdown display)
              :else (str "_ran `" cmd "`_"))

        block
        (cond-> {:code (str tool-name "(" (pr-str cmd) ")")
                 :svar/tool-call-id (str "bang-" (subs (str (java.util.UUID/randomUUID)) 0 8))
                 :vis/tool-name tool-name
                 :tool-color-role (get color-roles tool-name)
                 :envelope {:started-at-ms t0 :finished-at-ms t1}}
          (some? result-map)
          (assoc :result result-map)

          (some? err)
          (assoc :error err)

          display
          (assoc :result-render
            (:body display) :result-summary
            (:summary display)))

        ;; One block = one form. Stamp `:tag :user-shell` so the channels suppress
        ;; the redundant trace card (the answer bubble already shows it), the same
        ;; way `:user-slash` is suppressed.
        forms
        (mapv #(assoc % :tag :user-shell)
              (ctx-engine/blocks->forms [block] {:turn turn-pos :iter 1} nil))

        ;; Snapshot CTX like run-slash-turn! / run-normal-turn! so resume is stable.
        ctx-snapshot
        (when-let [ca (:ctx-atom env)]
          (let [stamped (ctx-loop/stamp-cursor env @ca)
                gced (ctx-engine/gc-pass stamped)
                clean (-> gced
                          (dissoc "session_scope")
                          ctx-engine/strip-ephemeral)]

            (reset! ca clean)
            clean))]

    (try (persistance/db-store-iteration! db-info
                                          {:session-turn-id turn-id
                                           :code user-request
                                           :forms forms
                                           :duration-ms (- t1 t0)
                                           :llm-full-duration-ms 0
                                           :thinking ""
                                           :answer answer-md
                                           :llm-messages []
                                           :llm-returned-empty-code? false})
         (catch Throwable t
           (tel/log! {:level :warn :id ::bang-iter-persist-failed :data {:error (ex-message t)}})))
    (persistance/db-update-session-turn! db-info
                                         turn-id
                                         {:content [(content/prose answer-md)]
                                          :iteration-count 1
                                          :duration-ms (- t1 t0)
                                          :status :success
                                          :prior-outcome :complete
                                          :ctx ctx-snapshot})
    {:session-turn-id turn-id
     :answer answer-md
     :iteration-count 1
     :duration-ms (- t1 t0)
     :status :success
     :prior-outcome :complete}))


(defn run-turn!
  "Store turn -> iteration-loop -> update turn -> return result.

   Derives `:prior-outcome` (one of `:complete`, `:cancelled`, `:error`)
   from the loop result and
   persists it on the `session_turn_state` row. The next turn's
   `<system_state>` digest reads it.

   BEFORE the LLM round-trip, every turn is passed through
   `slash/dispatch`. When the user-message resolves
   to a registered slash, the turn is fully handled by a synthetic
   iteration (`tag :user-slash`) and the LLM is never called. The
   transcript still shows the user message + the slash envelope.

   A slash NO extension claims (`:reason :unknown`) gets one more
   chance as a PROMPT TEMPLATE (`.vis/prompts/*.md`, `~/.vis/prompts`,
   provider-contributed templates like `/skill:<name>`): when a
   template matches, the expanded text runs as a NORMAL LLM turn.
   Registered slashes always win over templates."
  [env user-request loop-opts]
  (when-not (map? env) (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let [;; Re-resolve the active workspace from the session's CURRENT pin so a
        ;; mid-session `/draft new | apply | abandon` takes effect THIS turn.
        ;; The cached env was built at session start (on trunk); without this
        ;; the agent keeps editing trunk after entering a draft.
        env
        (or (when-let [db (:db-info env)]
              (when-let [sid (or (:session/state-id env)
                                 (some->> (:session-id env)
                                          (persistance/db-latest-session-state-id db)))]
                (when-let [ws (persistance/db-workspace-for-session db sid)]
                  ;; Keep the sandbox confinement's live pointer in step —
                  ;; sandbox-roots-fn derefs this on every real-fs access.
                  (some-> (:workspace-atom env)
                          (reset! ws))
                  (assoc env
                    :workspace ws
                    :workspace/id (:id ws)
                    :workspace/root (:root ws)
                    ;; Carry the extra filesystem roots across the mid-session
                    ;; env rebuild too — create-environment seeds them, but
                    ;; a /draft refresh re-derived the env from the row and
                    ;; dropped them, silently reverting to primary-root-only
                    ;; confinement for the rest of the session.
                    :workspace/filesystem-roots (vec (:filesystem-roots ws))))))
            env)

        ;; Turn-start health gate: probe LOCAL providers (Ollama/LM Studio)
        ;; and sink unreachable ones to the END of this turn's router, so a
        ;; dead local endpoint can't catch the turn or an svar fallback.
        ;; Dead providers are HIDDEN from `routing.available`
        ;; for this turn — the model can't route a child to what it
        ;; can't see (the per-turn env binding is local, so a provider
        ;; that comes back reappears next turn) — and the demotion
        ;; raises an engine warning so the user knows. Remote providers
        ;; are not network-checked here.
        env
        (let [{:keys [router demoted]}
              (health-gated-router (:router env) :turn)

              ;; Digest entries are string-keyed/string-valued (they cross
              ;; the boundary); demoted ids are internal keywords — compare
              ;; on the stringified id.
              dset
              (into #{} (map name) demoted)

              env'
              (cond-> (assoc env :router router)
                (and (seq dset) (seq (get (:routing env) "available")))
                (update-in [:routing "available"]
                           (fn [avail]
                             (vec (remove #(contains? dset (get % "provider")) avail)))))]

          (when (seq demoted)
            (when-let [ca (:ctx-atom env)]
              (swap! ca update
                "engine_warnings"
                (fnil conj [])
                {:code :provider-unreachable
                 :anchor ["session_routing"]
                 :message (str "Local provider(s) " (str/join ", " (map name demoted))
                               " unreachable — demoted to last-resort and hidden"
                               " from routing for this turn.")})))
          env')

        slash-result
        (try (slash/dispatch env (slash-ctx-for-env env user-request) user-request)
             (catch Throwable t
               (tel/log! {:level :warn
                          :id ::slash-dispatch-threw
                          :data {:user-request user-request :error (ex-message t)}})
               {:handled? false}))]

    (if-let [bang (parse-bang user-request)]
      (run-bang-turn! env user-request bang loop-opts)
      (if (:handled? slash-result)
        (if-let [expansion (when (= :unknown (:reason slash-result))
                             (try (prompt-templates/expand env user-request)
                                  (catch Throwable t
                                    (tel/log! {:level :warn
                                               :id ::template-expand-threw
                                               :data {:user-request user-request
                                                      :error (ex-message t)}})
                                    nil)))]
          (run-normal-turn! env (:text expansion) loop-opts)
          (run-slash-turn! env user-request slash-result))
        (run-normal-turn! env user-request loop-opts)))))

(defn custom-bindings
  "Current custom sandbox bindings {sym -> value}."
  [env]
  (some-> (:state-atom env)
          deref
          :custom-bindings))

;; -----------------------------------------------------------------------------
;; Prepare turn context
;; -----------------------------------------------------------------------------

(defn- forced-routing-for-pref
  "svar routing that FORCES a per-session provider+model preference.

   Why this exists: `router-for-model` reorders the router's `:providers`
   VECTOR, but svar's default `:strategy :root` selection sorts candidates by
   each provider's `:priority` field (NOT vector order — see
   `svar…router/candidate-sort-key`). So a config where anthropic is
   `:priority 0` and zai is `:priority 1` ALWAYS routes to anthropic's root
   (opus) no matter how Vis reorders the vector — the per-session pick was
   silently ignored. The fix is to hand svar the EXACT model (force-model) /
   provider (force-provider), which it honors regardless of priority.

   Returns routing additions, validated against `router` so a stale pref
   degrades instead of throwing (resolve-routing throws on an unknown
   provider):
     - provider+model both present & valid -> {:provider <kw> :model <str>}
     - model present & owned by some provider -> {:model <str>}
       (force-model restricts candidates to providers that expose it)
     - otherwise -> {} (no override; default `:strategy :root` runs)

   `provider` accepts a string id (`\"zai-coding-plan\"`, as stored in the DB
   pref) or keyword; `model` is the model name string."
  [router provider model]
  (let [model
        (some-> model
                str
                str/trim
                not-empty)

        prov-kw
        (some-> provider
                name
                keyword)

        prov
        (when prov-kw (first (filter #(= (:id %) prov-kw) (:providers router))))

        owns?
        (fn [p]
          (and model (some #(= (:name %) model) (:models p))))]

    (cond (and model prov (owns? prov)) {:provider prov-kw :model model}
          (and model (some owns? (:providers router))) {:model model}
          :else {})))

(defn- prepare-turn-context
  "Validates inputs, resolves sandbox bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model max-context-tokens system-prompt debug? hooks cancel-token
                eval-timeout-ms reasoning-default reasoning-effort routing extra-body]
         :or {debug? false}}
        opts]
    (when-not (:db-info env)
      (anomaly/incorrect! "Invalid RLM environment" {:type :vis/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect!
        "messages must be a non-empty vector of message maps, e.g. [(svar/user \"...\")]"
        {:type :vis/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect!
        ":eval-timeout-ms must be an integer (milliseconds)"
        {:type :vis/invalid-eval-timeout :got eval-timeout-ms :got-type (type eval-timeout-ms)}))
    (let [;; Per-session model preference: when the caller passes no explicit
          ;; `:model`, fall back to the persisted per-session choice (set by
          ;; ANY channel — web picker or TUI — via `session-model/set-model!`).
          ;; This is what unifies routing across channels: the engine, not the
          ;; channel, applies the session's pick.
          ;; The preference is {:provider :model}: the MODEL drives display/cost
          ;; (router-for-model + resolve-effective-model below) AND, crucially,
          ;; gets forced into svar's `:routing` (forced-routing-for-pref) so the
          ;; pick actually binds — reordering the router vector alone does NOT
          ;; (svar selects by provider :priority, not vector order).
          session-pref (when (:session-id env)
                         (session-model/model-of (:db-info env) (:session-id env)))
          model (or model (:model session-pref))
          pref-provider (:provider session-pref)
          ;; Cancellation TOKEN carries the cooperative flag AND the
          ;; on-cancel! callback registry that hard-cancels Python /
          ;; provider futures. Callers create one via
          ;; `cancellation/cancellation-token` and pass it as
          ;; `:cancel-token`. The derived atom is the lower-level
          ;; primitive every poll site checks.
          cancel-token (or cancel-token (cancellation/cancellation-token))
          cancel-atom (cancellation/cancellation-atom cancel-token)
          ;; INLINE image uploads (web/API base64, no durable disk path):
          ;; validate here (magic-byte sniff + size/count caps) so BOTH the
          ;; assemble seam and turn persistence see the canonical
          ;; `{:attached :skipped}` shape.
          prepared-attachments (attachments/prepare-inline-attachments (:user/attachments opts))
          ;; `user-request` = ONLY the current turn's user message.
          ;; Prior dialog transcript is dropped here — one ask, one value.
          ;; Durable context flows through ctx and persisted iterations, not
          ;; by joining every message's content into one growing blob.
          extract-text (fn [c]
                         (cond (string? c) c
                               (sequential? c)
                               (str/join " " (keep #(when (= "text" (:type %)) (:text %)) c))
                               :else nil))
          ;; Locate the LAST user message once. It is the only human text
          ;; sent into this turn. Prior dialog transcript is intentionally
          ;; NOT replayed to the model; durable context flows through
          ;; persisted iterations, defs, SYSTEM vars, and DB-backed tools.
          last-user-idx (->> (map-indexed vector messages)
                             reverse
                             (some (fn [[i m]]
                                     (when (contains? #{"user" :user} (:role m)) i))))
          last-user-message (when last-user-idx (nth messages last-user-idx))
          user-request (or (some-> last-user-message
                                   :content
                                   extract-text)
                           ;; Fallback: no :user role found (malformed caller) -
                           ;; use the last message's text. Better than an empty user request.
                           (some-> messages
                                   last
                                   :content
                                   extract-text)
                           "")
          ;; A `:model` preference HOISTS that model to the router root for
          ;; DISPLAY + COST: `resolve-effective-model` reads the vector head, so
          ;; root-model/root-provider (and the persisted cost label) reflect the
          ;; pick. Blank/unknown names degrade to the config order.
          env-router (cond-> (:router env)
                       (and model (not (str/blank? (str model))))
                       (router-for-model model))
          root-resolved-model (when env-router (resolve-effective-model env-router))
          root-model (or (:name root-resolved-model) model)
          root-provider (:provider root-resolved-model)
          root-provider-map (some #(when (= root-provider (:id %)) %) (:providers env-router))
          reasoning-effort-resolution (when (some? reasoning-effort)
                                        (svar/resolve-reasoning-effort
                                          (or (:api-style root-resolved-model)
                                              (:api-style root-provider-map))
                                          root-resolved-model
                                          reasoning-effort))
          _ (when (and (some? reasoning-effort) (nil? (:effective reasoning-effort-resolution)))
              (throw (ex-info (str "Reasoning effort " (pr-str reasoning-effort)
                                   " is unsupported for " (some-> root-provider
                                                                  name)
                                   "/" root-model
                                   "; accepted values: "
                                   (if (seq (:supported reasoning-effort-resolution))
                                     (str/join ", " (:supported reasoning-effort-resolution))
                                     "none"))
                              {:type :vis/unsupported-reasoning-effort
                               :vis/user-error true
                               :requested reasoning-effort
                               :provider root-provider
                               :model root-model
                               :supported (:supported reasoning-effort-resolution)
                               :resolution reasoning-effort-resolution})))
          ;; …but vector order does NOT bind svar's actual selection (it sorts
          ;; by provider :priority). FORCE the pick into `:routing` so the call
          ;; truly lands on the chosen provider+model. A caller-supplied
          ;; `:routing` (e.g. sub_loop's own pin) wins on merge.
          routing (let [merged (merge (forced-routing-for-pref (:router env) pref-provider model)
                                      (or routing {}))]
                    ;; MAIN turn (depth 0): pin the ACTIVE provider+model so a provider
                    ;; failure surfaces as an error the USER acts on (retry / switch
                    ;; provider — TUI Ctrl+K) instead of svar silently hopping across the
                    ;; whole configured fleet (`with-provider-fallback` → the confusing
                    ;; "tried every provider" card). A session pick already pins this way;
                    ;; this makes the DEFAULT (no-pick) turn behave identically. sub_loop
                    ;; CHILDREN (depth > 0) are EXEMPT — dispatched agents legitimately
                    ;; optimize / fall back across the `models` list they were given.
                    (cond-> merged
                      (and root-provider
                           root-model
                           (not (contains? merged :provider))
                           (not (contains? merged :model))
                           (zero? (long (or (some-> (:depth-atom env)
                                                    deref)
                                            0))))
                      (merge (forced-routing-for-pref (:router env) root-provider root-model))))
          db-info (:db-info env)
          custom-bindings (custom-bindings env)
          python-context (:python-context env)
          _ (doseq [[sym val] (or custom-bindings {})]
              (when val (env/set-python-binding! python-context sym val)))
          ;; Workspace pin lives on the env itself (set in create-environment).
          ;; Opts may carry namespaced `:workspace/*` overrides for unusual
          ;; per-turn cases; the bare `:workspace` key is not accepted
          ;; (only :workspace/* namespaced keys flow through).
          ;; turn-state-atom already lives on env (one atom for all
          ;; per-turn cursor + id fields); no re-assoc needed.
          workspace-overrides (select-keys opts
                                           [:workspace/root :workspace/id :workspace/sandbox?
                                            :vcs/kind :vcs/ref :vcs/mainline])
          ;; Reseat :router to the preference-hoisted one — run-iteration-phase
          ;; routes off THIS environment's router, not the ctx :router below.
          environment (cond-> (assoc env
                                :router env-router
                                :user/attachments (:attached prepared-attachments)
                                :user/skipped-attachments (:skipped prepared-attachments))
                        (seq workspace-overrides)
                        (merge workspace-overrides)

                        ;; Refresh the routing digest HEAD
                        ;; (:model/:provider) to the per-turn pick so
                        ;; `context["routing"]` + the TUI footer reflect the
                        ;; session's chosen provider/model. The digest
                        ;; is built ONCE at env creation from the GLOBAL
                        ;; router head (the config default), so without
                        ;; this every turn's `:session/routing` showed
                        ;; the default provider (e.g. zai) even after
                        ;; the user switched models — the forced pref
                        ;; bound the actual call but never the displayed
                        ;; routing. `:available` is preserved.
                        (and (seq (:routing env)) (or root-model root-provider))
                        (update :routing
                                (fn [r]
                                  (cond-> r
                                    root-model
                                    (assoc "model" (str root-model))

                                    root-provider
                                    (assoc "provider" (name root-provider))))))
          environment-id (:environment-id env)]

      {:cancel-token cancel-token
       :cancel-atom cancel-atom
       :user-request user-request
       :router env-router
       :root-resolved-model root-resolved-model
       :root-model root-model
       :root-provider root-provider
       :db-info db-info
       :environment environment
       :environment-id environment-id
       :spec spec
       :max-context-tokens max-context-tokens
       :system-prompt system-prompt
       :debug? debug?
       :hooks hooks
       :eval-timeout-ms eval-timeout-ms
       :reasoning-default reasoning-default
       :reasoning-effort (:effective reasoning-effort-resolution)
       :reasoning-effort-resolution reasoning-effort-resolution
       :routing routing
       :extra-body extra-body
       :turn-features (get opts :turn/features)
       :workspace-overrides workspace-overrides
       :messages messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, session-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec max-context-tokens system-prompt hooks cancel-atom
           cancel-token reasoning-default reasoning-effort routing extra-body turn-features
           workspace-overrides]}]
  (let [iteration-result
        (run-turn! environment
                   user-request
                   (cond-> {:output-spec spec
                            :max-context-tokens max-context-tokens
                            :system-prompt system-prompt
                            :reasoning-default reasoning-default
                            :reasoning-effort reasoning-effort
                            :hooks hooks
                            :cancel-atom cancel-atom
                            :cancel-token cancel-token}
                     routing
                     (assoc :routing routing)

                     extra-body
                     (assoc :extra-body extra-body)

                     turn-features
                     (assoc :turn-features turn-features)

                     (seq workspace-overrides)
                     (assoc :workspace-overrides workspace-overrides)))

        session-turn-id
        (:session-turn-id iteration-result)

        {iteration-tokens :tokens iteration-cost :cost}
        iteration-result

        total-tokens-atom
        (atom (or iteration-tokens {}))

        total-cost-atom
        (atom (or iteration-cost {}))

        merge-cost!
        (fn [extra-tokens extra-cost]
          (when extra-tokens
            (swap! total-tokens-atom (fn [acc]
                                       (merge-with +
                                                   acc
                                                   (select-keys extra-tokens
                                                                ["input" "output" "reasoning"
                                                                 "cached" "total"])))))
          (when extra-cost
            (swap! total-cost-atom (fn [acc]
                                     (merge-cost-maps acc extra-cost)))))]

    {:iteration-result iteration-result
     :session-turn-id session-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom total-cost-atom
     :merge-cost! merge-cost!}))

;; -----------------------------------------------------------------------------
;; Finalize turn result
;; -----------------------------------------------------------------------------

(defn- finalize-turn-result
  "Updates DB turn record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model / N
   iteration / duration / tokens / $total` after a restart."
  [{:keys [db-info root-model root-provider reasoning-effort]}
   {:keys [session-turn-id start-time iteration-count status status-id trace locals answer
           confidence reasoning utilization total-tokens-atom total-cost-atom]}]
  (let [duration-ms
        (util/elapsed-since start-time)

        eval-evidence
        (turn-eval-evidence reasoning-effort trace)

        cost-with-model
        (cond-> @total-cost-atom
          (and root-model (not (get @total-cost-atom "model")))
          (assoc "model" (str root-model))

          (and root-provider (not (get @total-cost-atom "provider")))
          (assoc "provider"
            (if (keyword? root-provider) (name root-provider) (str root-provider))))]

    (if status
      ;; failure path - surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do (log-stage! :turn/complete
                      0
                      {:duration-ms duration-ms :iteration-count iteration-count :status status})
          (let [fallback-answer (:result answer answer)]
            (try (persistance/db-update-session-turn! db-info
                                                      session-turn-id
                                                      {:content (content/answer-content
                                                                  fallback-answer)
                                                       ;; First-class structured error for a failed turn.
                                                       :error (turn-error-data fallback-answer)
                                                       :iteration-count iteration-count
                                                       :duration-ms duration-ms
                                                       :status status
                                                       :tokens @total-tokens-atom
                                                       :cost cost-with-model})
                 (catch Exception e
                   (tel/log! {:level :warn
                              :data (format-exception-short e)
                              :msg "Failed to update turn (max iterations)"})))
            (cond-> {:answer fallback-answer
                     :status status
                     :status-id status-id
                     :trace trace
                     :iteration-count iteration-count
                     :duration-ms duration-ms
                     :tokens @total-tokens-atom
                     :cost cost-with-model}
              eval-evidence
              (assoc :eval eval-evidence)

              (some? locals)
              (assoc :locals locals))))
      ;; success path
      (do (log-stage! :turn/complete
                      0
                      {:duration-ms duration-ms
                       :iteration-count iteration-count
                       :cost (str (get cost-with-model "total_cost"))})
          (try (persistance/db-update-session-turn! db-info
                                                    session-turn-id
                                                    {:content (content/answer-content answer)
                                                     :iteration-count iteration-count
                                                     :duration-ms duration-ms
                                                     :status :success
                                                     :tokens @total-tokens-atom
                                                     :cost cost-with-model})
               (catch Exception e
                 (tel/log! {:level :warn
                            :data (format-exception-short e)
                            :msg "Failed to update turn (success)"})))
          (cond-> {:answer answer
                   :trace trace
                   :iteration-count iteration-count
                   :duration-ms duration-ms
                   :tokens @total-tokens-atom
                   :cost cost-with-model
                   :utilization utilization}
            eval-evidence
            (assoc :eval eval-evidence)

            (some? confidence)
            (assoc :confidence confidence)

            (some? reasoning)
            (assoc :reasoning reasoning))))))

;; -----------------------------------------------------------------------------
;; Public entry point
;; -----------------------------------------------------------------------------

(defn turn!
  "Runs one session turn on an RLM environment using iterative LLM code evaluation.

    Params:
    `environment` - RLM environment from create-environment.
    `messages` - Vector of message maps. Always a vector, e.g.:
                 [(svar/user <prompt-text>)]
                 [(svar/user <prompt-text> (svar/image <b64> <mime-type>))]
   `opts` - Map, optional:
     - :spec - Output spec for structured answers.
     - :model - Override config's default model.
      - :max-context-tokens - Token budget for context.
      - :debug? - Enable verbose debug logging (default: false). Logs iteration details,
        code evaluation, LLM responses at :debug level with :rlm-phase context.
      - :reasoning-default - Optional base reasoning effort for reasoning-capable models.
        Accepts :low/:medium/:high or low/medium/high strings. Adaptive escalation still applies.
      - :reasoning-effort - Exact provider-native effort string, `high` or `max`.
        Catalog-gated and threaded unchanged through every iteration.
      - :extra-body - Optional provider-specific request-body params merged into the
        upstream LLM call after auto max_tokens + reasoning translation.

    Returns:
   Map with:
      - :trace - Vector of iteration trace entries, each containing:
          {:iteration N
           :response <llm-response-text>
           :blocks [{:id 0 :code <code-str> :result <value> :error nil
                     :envelope {:started-at-ms 10 :finished-at-ms 15 ...}}
                       ...]}
     - :iteration-count - Number of iterations used.
     - :duration-ms - Turn duration in milliseconds.
     - :tokens - Token usage map {\"input\" N \"output\" N \"total\" N} (canonical string keys).
     - :cost - Cost map {\"input_cost\" N \"output_cost\" N \"total_cost\" N} (canonical string keys).
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages] (turn! environment messages {}))
  ([environment messages opts]
   (let [ctx
         (prepare-turn-context environment messages opts)

         {:keys [eval-timeout-ms debug? user-request root-model db-info environment-id]}
         ctx]

     (binding [rt/*rlm-context*
               {:rlm-environment-id environment-id
                :rlm-type :main
                :rlm-debug? debug?
                :rlm-phase :turn
                :db-info db-info
                :session-soul-id (:session-id environment)}

               rt/*eval-timeout-ms*
               (rt/clamp-eval-timeout-ms (or eval-timeout-ms rt/*eval-timeout-ms*))]

       (tel/with-ctx+
         {:db-info db-info :session-soul-id (:session-id environment)}
         (log-stage! :turn/open
                     0
                     {:model root-model
                      :reasoning? (boolean (:reasoning? (first (mapcat :models
                                                                       (:providers
                                                                         (:router environment))))))
                      :user-request user-request})
         (let [start-time
               (System/nanoTime)

               phase2
               (run-iteration-phase ctx)

               {:keys [iteration-result session-turn-id total-tokens-atom total-cost-atom]}
               phase2

               {iteration-answer :answer
                trace :trace
                iteration-count :iteration-count
                status :status
                status-id :status-id
                locals :locals
                confidence :confidence
                reasoning :reasoning}
               iteration-result

               result
               (if status
                 (finalize-turn-result ctx
                                       {:session-turn-id session-turn-id
                                        :start-time start-time
                                        :iteration-count iteration-count
                                        :status status
                                        :status-id status-id
                                        :trace trace
                                        :locals locals
                                        :answer iteration-answer
                                        :total-tokens-atom total-tokens-atom
                                        :total-cost-atom total-cost-atom})
                 (finalize-turn-result ctx
                                       {:session-turn-id session-turn-id
                                        :start-time start-time
                                        :iteration-count iteration-count
                                        :trace trace
                                        :answer iteration-answer
                                        :confidence confidence
                                        :reasoning reasoning
                                        :utilization (:utilization iteration-result)
                                        :total-tokens-atom total-tokens-atom
                                        :total-cost-atom total-cost-atom}))]

           result))))))

;; =============================================================================
;; Environment lifecycle + system prompt
;; =============================================================================

;; =============================================================================
;; Helpers
;; =============================================================================

;; =============================================================================
;; Public env accessors
;; =============================================================================

;; `db-info` (the env accessor) was a thin wrapper over `(:db-info env)`
;; that no caller actually invoked - every consumer either destructured
;; `:db-info` directly or used the no-arg `(db-info)` defined further
;; down (which returns the process-wide shared connection). The defn was
;; deleted to keep ONE canonical `db-info` symbol on this namespace.

(defn sync-active-extension-symbols!
  "Make the Python sandbox's callable globals match active extension state.

   `install-extension!` keeps every extension row in `:extensions`, but only
   active extensions contribute callable symbols. Called after per-env
   installation and again at turn start so `:ext/activation-fn` changes become
   real tool availability, not just prompt visibility.

   The Python sandbox is FLAT globals (no namespaces/aliases/macros): active
   extensions putMember their symbols straight into the top scope; deactivated
   extensions have theirs removed (putMember nil). Symbol names are snake-ified
   by env/set-python-binding!."
  ([environment]
   (sync-active-extension-symbols! environment (prompt/active-extensions environment)))
  ([environment active-extensions]
   (when-let [active-atom (:active-extensions environment)]
     (reset! active-atom (vec (or active-extensions []))))
   (when-let [python-context (:python-context environment)]
     (let [installed (vec (or (some-> (:extensions environment)
                                      deref)
                              []))
           active-set (set (map :ext/name active-extensions))]

       (doseq [ext installed
               :let [alias (extension/ext-alias-symbol ext)
                     by-sym
                     (into {} (map (juxt :ext.symbol/symbol identity) (extension/ext-symbols ext)))]
               [sym f] (try (extension/wrap-extension ext environment) (catch Throwable _ nil))]

         ;; Aliased extensions (`:ext.engine/alias 'clj`) bind into the FLAT
         ;; Python sandbox as `<alias>_<name>` — the snake form of the
         ;; `alias/name` shape (clj_eval, git_status, search_web, br_open). Without
         ;; folding the alias in, the tool leaked as its BARE suffix (`eval`,
         ;; `status`, `web`), so the model's prompt-promised `clj_eval(...)`
         ;; call hit a NameError and `apropos`/`dir` showed the wrong names.
         ;; Builtins carry no alias → bound bare, like the engine verbs.
         ;;
         ;; Deactivated extensions get their members REMOVED, not nil'd:
         ;; `putMember nil` parks a None under the name, which `apropos`
         ;; kept listing and which called as 'NoneType is not callable' —
         ;; a disabled tool must not exist in the sandbox at all.
         (let [target (if alias (clojure.core/symbol (str alias "/" (name sym))) sym)]
           ;; bound only when the EXTENSION is active AND the symbol's :active-fn
           ;; holds for env — one gate, native tools and Python verbs alike.
           (if (and (contains? active-set (:ext/name ext))
                    (extension/symbol-active? (get by-sym sym) environment))
             (do (env/set-python-binding! python-context target f)
                 ;; Seed this symbol's doc into `__vis_docs__` keyed by its bound
                 ;; py-name, so `doc(git_status)` / `doc(mcp_servers)` /
                 ;; `apropos("mcp")` carry real descriptions. ALIASED extensions
                 ;; bind here (per turn), NOT at context creation, so the eager
                 ;; `build-agent-context` seed never saw them.
                 (env/set-python-binding-doc! python-context
                                              target
                                              (extension/symbol-doc-text (get by-sym sym))))
             (env/remove-python-binding! python-context target))))))
   environment))

(defn install-extension!
  "Register a validated extension into `environment` (per-env registration,
   distinct from the global-registry `register-extension!` defined earlier
   in this file).

   Checks `:ext/requires` - if the extension declares dependencies, all
   listed extension namespaces must already be registered. Throws on
   missing dependencies.

   If an extension with the same `:ext/name` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!` (removed for GraalVM native-image compatibility).

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment - missing :extensions atom"
                        {:type :vis/invalid-env}))
  (when-let [requires (seq (:ext/requires ext))]
    (let [registered (into #{} (map :ext/name) @(:extensions environment))
          missing (vec (remove registered requires))]

      (when (seq missing)
        (anomaly/incorrect! (str "Extension '" (:ext/name ext)
                                 "' requires " missing
                                 " but they are not registered. " "Register dependencies first.")
                            {:type :extension/missing-dependencies
                             :extension (:ext/name ext)
                             :requires (vec requires)
                             :missing missing
                             :registered (vec registered)}))))
  (swap! (:extensions environment) (fn [exts]
                                     (let [ns-sym
                                           (:ext/name ext)

                                           without
                                           (vec (remove #(= (:ext/name %) ns-sym) exts))]

                                       (conj without ext))))
  ;; Extension rows stay installed even when inactive, but callable symbol
  ;; bindings are activation-aware (sync-active-extension-symbols!). The Python
  ;; sandbox has no passive Java class/import config — the agent writes Python +
  ;; uses its own stdlib; the Clojure tools do any Java work.
  (sync-active-extension-symbols! environment)
  environment)

;; =============================================================================
;; Environment Lifecycle
;; =============================================================================

;; The sub_loop RUNTIME (dispose-environment! + helpers + sub-loop! + the
;; composite runners) is defined BELOW, before create-environment, so verbs
;; resolve in order. sub-loop! is the ONE back-edge — it calls create-environment
;; to build the child env — so create-environment is the only forward declare.
(declare create-environment)

(defn dispose-environment!
  "Disposes a vis environment and releases resources. For persistent DBs
   (created with `:path`), data is preserved. For disposable DBs, all
   data is deleted.

   A sub_loop CHILD env BORROWS the parent's DB connection (`:owns-db?` false) —
   disposing the child must NOT close it, or the parent loses its DB mid-turn."
  [environment]
  (when-let [python-context (:python-context environment)]
    (try (.close ^Context python-context true) (catch Throwable _ nil)))
  (when (and (:db-info environment) (not (false? (:owns-db? environment))))
    (persistance/db-dispose-connection! (:db-info environment))))

;; =============================================================================
;; sub_loop runtime — a child agentic loop (slice C). The model writes Python:
;; it slices `context` into a focused `subctx` and calls
;; `sub_loop(prompt, subctx, {"model": …})`. The child is a CHILD session reusing
;; create-environment (own done/bindings/ctx + forked Context on the shared Engine),
;; on its OWN workspace (rift clone where supported, else shared root), optionally
;; on a cheaper proposed model. On close its workspace diff merges back and the
;; result (status + evidence + produced facts + what-changed) returns to the parent.
;; =============================================================================

(def ^:private MAX-SUBLOOP-DEPTH
  "Recursion cap: a coordinator → child → grandchild … chain may nest at most this
   deep before `sub-loop!` refuses, so an agent-tree can't explode unbounded."
  5)

(def ^:private MAX-PARALLEL-SUBLOOPS
  "Concurrency cap for `parallel` — at most this many child turns run at once.
   LLM calls (and the shared single SQLite writer) are the bottleneck, so a small
   cap keeps provider rate-limits + write contention sane; extra specs queue."
  4)

(defn- status->str
  "Coerce a status to its python-facing STRING name (keyword → name, else str).
   sub_loop results cross to the model as Python, so statuses are STRINGS, never
   keywords — matching `plan_step`'s string surface and the rendered ctx."
  [s]
  (when (some? s) (if (keyword? s) (name s) (str s))))

(def ^:private ^Object workspace-mutation-lock
  ;; Serializes the FAST rift steps (clone + merge-back) across concurrent
  ;; `parallel` children: `cow-clone!` does `rift/init` on the SHARED parent
  ;; (concurrent inits would race) and `apply!` writes into the ONE parent root
  ;; (concurrent applies could interleave). Only these ~ms ops serialize — the
  ;; expensive child LLM turn (`run-turn!`) still runs fully concurrently.
  (Object.))

(defn child-workspace!
  "Spawn the child's workspace. An isolation backend available for the parent's
   root → a CoW clone of the parent's workspace (`workspace/create! {:from
   parent-ws}`): isolated writes, and `workspace/apply!` later lands the
   since-fork diff back into the parent root. Else (Windows / non-POSIX, or no
   backend) → a trunk row at the parent's root (`create-trunk-at!`): SHARED
   files, no clone (safety = disjoint `:files`). Returns the workspace row."
  [db-info parent-ws]
  (if (workspace/isolated-workspaces-supported? (:root parent-ws))
    (workspace/create! db-info {:from parent-ws :label "subloop"})
    (workspace/create-trunk-at! db-info (:root parent-ws))))

(defn- log-subloop-warn!
  "Surface a sub_loop lifecycle failure (merge-back / teardown) — NEVER swallowed
   silently: a failed merge is lost work, a failed clone-trash is a disk leak.
   The step still best-efforts on, but the warning keeps the failure visible."
  [step ^Throwable t ws-id]
  (tel/log! {:level :warn
             :id ::subloop-lifecycle
             :data {:step step :workspace-id ws-id :error (ex-message t)}}
            (str "sub_loop " (name step) " failed for child workspace " ws-id)))

(defn- guard
  "Functional resource bracket: run `(use resource)` and ALWAYS `(release
   resource)` afterward, returning `use`'s value. A release failure is LOGGED
   (tagged `step`/`ws-id`), never swallowed — teardown problems stay visible
   while the original result (or exception) still propagates."
  [resource release step ws-id use]
  (try (use resource)
       (finally (try (release resource) (catch Throwable t (log-subloop-warn! step t ws-id))))))

(defn- merge-child-edits!
  "Land the child's since-fork diff back into the parent root, serialized so
   concurrent `parallel` applies don't interleave. Returns the `apply!` result,
   or — on failure — nil plus a LOGGED warning (the merge is lost, so it must be
   visible, not silently dropped)."
  [db-info child-ws]
  (locking workspace-mutation-lock
    (try (workspace/apply! db-info {:workspace-id (:id child-ws)})
         (catch Throwable t (log-subloop-warn! :merge t (:id child-ws)) nil))))

(def child-forced-toggles
  "Toggles a `sub_loop` child has ON by DEFAULT, whatever the global state — so
   a dispatched agent can always run shell commands and reach the harness
   skills/agents (the user's rule: sub-agents get both compat layers enabled).
   Soft keyword coupling only (no load dependency on those extensions), the same
   convention as the posix shim's tool-name coupling. Bound around the child
   turn; binding-conveyance carries it into `parallel` futures."
  #{:shell/enabled :vis/harness-skills :vis/harness-agents})

(defn- project-child-result
  "Run the child turn, merge its edits back (rift path), and project the result
   the coordinator merges by `task_id`: the model-supplied focus id, status (a
   STRING — python-facing, never a keyword), answer, and what changed."
  [child-env {:keys [db-info child-ws rift? subctx prompt system-prompt]}]
  (let [;; A harness AGENT dispatch rides its markdown body in as the child's
        ;; system-prompt addendum (build-system-prompt appends it to CORE);
        ;; ordinary sub_loops pass none.
        turn-opts
        (if (seq (str system-prompt)) {:system-prompt (str system-prompt)} {})

        ;; Child runs with shell + harness forced ON (see child-forced-toggles).
        ;; sync-active-extension-symbols! (turn start, inside run-turn!) reads
        ;; toggles/enabled? under this binding → the child's sandbox gets the
        ;; shell + skill/agent verbs bound even when they're OFF for the parent.
        result
        (binding [toggles/*forced-on* (into toggles/*forced-on* child-forced-toggles)]
          (run-turn! child-env (str prompt) turn-opts))

        merged
        (when rift? (merge-child-edits! db-info child-ws))

        ;; `subctx` is a Python dict — STRING keys. The projected result crosses
        ;; BACK into Python, so its keys/values are strings too.
        focus
        (some-> (get subctx "focus")
                str
                not-empty)]

    {"task_id" focus
     "status" (status->str (:status result))
     "answer" (:answer result)
     "changed_files" (vec (:changed merged))}))

(defn sub-loop!
  "Run a CHILD agentic loop for `prompt` over `subctx` (the model-supplied focused
   slice; see `subctx->seed-ctx`). Forks a child session env (own ctx-atom seeded
   from subctx, own forked Context on the shared Engine, own workspace per
   the parent root's isolation backend, reusing the parent's SINGLE DB
   connection + depth-cap),
   optionally on a cheaper PROPOSED model preference list `models`
   (`router-for-model` — always a vector, svar falls back). Runs `run-turn!`,
   merges the child's workspace diff back (rift path), then ALWAYS tears the child
   down — env disposed, rift clone trashed (both via `guard`, failures logged) so
   nothing leaks across `parallel`/`retry`. Returns:
     {:task_id <focus> :status <string> :answer :changed_files}
   Throws `:vis/subloop-depth-exceeded` past `MAX-SUBLOOP-DEPTH`."
  [parent-env {:keys [prompt subctx models system-prompt]}]
  (let [depth (inc (long (or (some-> parent-env
                                     :depth-atom
                                     deref)
                             0)))]
    (when (> (long depth) (long MAX-SUBLOOP-DEPTH))
      (throw (ex-info (str "sub_loop depth cap (" MAX-SUBLOOP-DEPTH ") exceeded")
                      {:type :vis/subloop-depth-exceeded :depth depth})))
    (let [db-info (:db-info parent-env)
          parent-ws (:workspace parent-env)
          ;; clone serialized (rift/init on the shared parent races otherwise)
          ;; Health gate FIRST — before the child workspace clone exists.
          ;; `preferred` is the reorder the coordinator asked for;
          ;; demotion sinks unreachable LOCAL providers to the end, so
          ;; the child AUTO-ROUTES to the next healthy provider instead
          ;; of burning minutes against a dead endpoint. When the
          ;; coordinator's EXPLICIT preference was the demoted provider,
          ;; the reroute is ANNOTATED on the child result (`:rerouted`)
          ;; so the parent knows its routing was overridden and why.
          preferred (router-for-model (:router parent-env) models)
          {:keys [router demoted]} (health-gated-router preferred :sub-loop)
          rerouted
          (when (and (seq demoted)
                     (seq (if (coll? models) models (when models [models])))
                     ;; router-for-model hoisted the preferred
                     ;; model's provider to the FRONT pre-demotion;
                     ;; if that very provider got demoted, the
                     ;; explicit preference was dead.
                     (contains? (set demoted) (:id (first (:providers preferred)))))
            ;; Crosses into Python on the child result — strings.
            {"from" (vec (if (coll? models) models [models]))
             "unreachable" (mapv name demoted)
             "used" (:name (resolve-effective-model router))
             "reason"
             "preferred model's provider unreachable; auto-routed to the next healthy provider"})
          child-ws (locking workspace-mutation-lock (child-workspace! db-info parent-ws))
          ;; rift path = the child got its OWN clone (root differs from parent);
          ;; the shared-root fallback writes in place (nothing to merge or trash).
          rift? (boolean (and (:root child-ws) parent-ws (not= (:root child-ws) (:root parent-ws))))
          ws-id (:id child-ws)]

      ;; Nested brackets — the CLONE is released LAST (after the env), so the
      ;; order is: merge diff → dispose env → trash clone. `guard` logs any
      ;; teardown failure instead of leaking it.
      (guard child-ws
             (fn [ws]
               (when rift?
                 (locking workspace-mutation-lock
                   (workspace/abandon! db-info
                                       {:workspace-id (:id ws) :reason "subloop complete"}))))
             :abandon
             ws-id
             (fn [ws]
               (guard (create-environment router
                                          {:workspace-id (:id ws)
                                           :child {:parent-db-info db-info
                                                   :depth depth
                                                   ;; link the child soul to THIS parent's session_state
                                                   ;; (cross-soul) → queryable sub-tree, hidden from the
                                                   ;; top-level session list, cascades on parent delete.
                                                   :parent-state-id (:session/state-id parent-env)
                                                   :seed-ctx (subctx->seed-ctx subctx)}})
                      dispose-environment!
                      :dispose
                      ws-id
                      (fn [child-env]
                        (cond-> (project-child-result child-env
                                                      {:db-info db-info
                                                       :child-ws ws
                                                       :rift? rift?
                                                       :subctx subctx
                                                       :prompt prompt
                                                       :system-prompt system-prompt})
                          ;; surface the health override to the coordinator
                          rerouted
                          (assoc "rerouted" rerouted)))))))))

(defn- failed-subloop-result
  "The uniform `sub_loop`-result shape for a child that errored (so `parallel`
   slots and `retry` attempts read like a normal result, just with `:error`).
   The throw is surfaced TWO ways — as this `:status \"failed\"` result the
   coordinator sees, AND a logged warning — so the failure is never silent."
  [spec ^Throwable t]
  (let [focus (some-> (get spec "subctx")
                      (get "focus")
                      str
                      not-empty)]
    (log-subloop-warn! :run t focus)
    ;; Crosses into Python — string keys, matching `project-child-result`.
    {"task_id" focus "status" "failed" "error" (ex-message t) "answer" nil "changed_files" []}))

(def ^:private subloop-failure-statuses
  "A child whose focus task landed in one of these (or threw → `:error`) is a
   FAILURE for `retry` — re-run; everything else is success enough to keep.
   STRINGS only — sub_loop result statuses are strings (`status->str`)."
  #{"failed" "rejected" "error"})

(defn- subloop-failed?
  "True when a `sub_loop` result represents a failed child (threw, or its focus
   task ended in a failure status) — the signal `retry`/`sequence`/`selector`
   branch on. A `:skipped`-mapped status (cancelled/rejected/deferred) is NOT a
   failure here — those are neutral; only `:failed`/`:error` count."
  [r]
  (or (some? (get r "error")) (contains? subloop-failure-statuses (status->str (get r "status")))))

(defn- run-spec!
  "Run ONE child for `spec` (a Python dict `{\"prompt\" \"subctx\" \"models\"}` —
   STRING keys), folding a throw into the uniform `failed-subloop-result`. The
   shared per-child step under every composite runner
   (parallel/sequence/selector) + retry."
  [parent-env spec]
  (try (sub-loop!
         parent-env
         {:prompt (get spec "prompt") :subctx (get spec "subctx") :models (get spec "models")})
       (catch Throwable t (failed-subloop-result spec t))))

(defn retry-sub-loop!
  "DECORATOR (not a composite): re-run the SAME `spec` until its child SUCCEEDS,
   up to `n` total attempts (default 2). Returns the first successful result,
   else the last failure — stamped with the `:attempts` made. (Contrast
   `selector-sub-loops!`, which tries DIFFERENT alternatives.)"
  [parent-env spec n]
  (let [attempts (max 1 (long (or n 2)))]
    (loop [i 1]
      (let [r (assoc (run-spec! parent-env spec) "attempts" i)]
        (if (or (not (subloop-failed? r)) (>= i attempts)) r (recur (inc i)))))))

(defn sequence-sub-loops!
  "`:sequence` composite — run `specs` IN ORDER, each only after the prior
   SUCCEEDS, SHORT-CIRCUITING on the first failure. Serial by nature (each child
   may depend on the last). Returns the vector of results ACTUALLY RUN, in order:
   all of them when every child succeeded, or up to and INCLUDING the first
   failure when it stopped early (that last result carries the failure). Mirrors
   the BT sequence: all-succeed, fail-fast."
  [parent-env specs]
  (reduce (fn [acc spec]
            (let [r
                  (run-spec! parent-env spec)

                  acc
                  (conj acc r)]

              (if (subloop-failed? r) (reduced acc) acc)))
          []
          (vec specs)))

(defn selector-sub-loops!
  "`:selector` composite (a.k.a. fallback) — try `specs` IN ORDER until one
   child SUCCEEDS, then STOP. Serial. Returns the vector of results tried, in
   order: the failed alternatives followed by the first success (the last
   result), or — if every alternative failed — all of them (all failures).
   Mirrors the BT selector: any-succeed. (Unlike `retry`, the alternatives are
   DIFFERENT specs.)"
  [parent-env specs]
  (reduce (fn [acc spec]
            (let [r
                  (run-spec! parent-env spec)

                  acc
                  (conj acc r)]

              (if (subloop-failed? r) acc (reduced acc))))
          []
          (vec specs)))

(defn parallel-sub-loops!
  "Run several `sub-loop!`s CONCURRENTLY on Clojure futures, bounded by
   `MAX-PARALLEL-SUBLOOPS` (a Semaphore), and return their results as a vector in
   INPUT ORDER. `specs` is a seq of `{:prompt :subctx :models}` maps (the model
   passes a list of dicts; keys arrive keyword-snake at the GraalPy boundary).

   All children share the parent's ONE db-info + depth-cap; the fast rift clone /
   merge-back steps serialize on `workspace-mutation-lock` while the expensive
   child LLM turns overlap. A child that throws does NOT sink the batch — its slot
   becomes a `{:status \"failed\" :error …}` result so the coordinator can see the
   failure and merge the rest. The sandbox denies Python threads, so concurrency
   lives Clojure-side on the shared GraalVM Engine (forks are safe mid-eval)."
  [parent-env specs]
  (let [specs
        (vec specs)

        sem
        (java.util.concurrent.Semaphore. MAX-PARALLEL-SUBLOOPS)

        futs
        (mapv (fn [spec]
                (future (.acquire sem) (try (run-spec! parent-env spec) (finally (.release sem)))))
              specs)]

    ;; Settle in input order — but when the COORDINATING thread is interrupted
    ;; (turn cancel / eval timeout), hard-cancel every child sub-loop before
    ;; propagating. Otherwise cancelled parallel sub-loops kept running as
    ;; orphaned full LLM turns (same leak the gather settle loop had).
    (try (mapv deref futs)
         (catch InterruptedException e
           (doseq [f futs]
             (try (future-cancel f) (catch Throwable _ nil)))
           (throw e)))))

(defn create-environment
  "Creates a vis environment (component) for session lifecycle and
   querying.

   The environment holds:
     - Python sandbox context with custom bindings + bindings cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` - Required. Result of `llm/make-router`.
     `opts`   - Map with `:db` and optional `:session`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               - no DB (sandbox-only execution)
       :memory           - ephemeral in-process SQLite DB
       path string       - persistent SQLite DB at path
       {:path p}         - persistent SQLite DB at path
       {:datasource ds}  - caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db session channel external-id title workspace-id child prewarm?]}]
  (when-not router (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  ;; `child` (a sub_loop child env) carries:
  ;;   :parent-db-info  reuse the parent's DB connection (don't open/close one)
  ;;   :depth           starting recursion depth (parent depth + 1)
  ;;   :seed-ctx        initial ctx-atom value (the model-supplied subctx) — used
  ;;                    instead of a DB restore
  ;; A different `router` (model) can be passed for the child to optimize cost
  ;; (cheap/fast model for an easy subtask) — first-class, nothing special needed.
  (let [depth-atom
        (atom (or (:depth child) 0))

        owns-db?
        (nil? (:parent-db-info child))

        db-info
        (or (:parent-db-info child) (persistance/db-create-connection! db))

        state-atom
        (atom {:custom-bindings {} :environment nil :session-id nil})

        environment-atom
        (atom nil)

        environment-id
        (str (util/uuid))

        ;; SINGLE turn-state atom holds all per-turn cursor fields
        ;; (current-{turn-position,iteration,form-idx,iteration-id,
        ;;  session-turn-id,user-request}-atom). All six fields live
        ;; under map keys with the same names minus `current-` /
        ;; `-atom`. Reads via `ctx-loop/read-turn-state`; writes via
        ;; `ctx-loop/set-turn-state!` / `swap-turn-state!`. Extension
        ;; symbol wrappers close over THIS atom; the loop swap!s it
        ;; between turns and forms.
        turn-state-atom
        (ctx-loop/make-turn-state-atom)

        ;; Seed iteration to 1 so early hooks reading the atom before
        ;; the loop's per-turn reset see a sensible value.
        _
        (swap! turn-state-atom assoc :iteration 1)

        ;; Title atom: in-memory cache for the session title.
        ;; The DB column on `session_state` is the persisted
        ;; truth; this atom is the fast read path for  and
        ;; the source for the title hint / channel chrome at iteration
        ;; boundaries. `set-title!` writes both, in that order, then
        ;; broadcasts to every registered listener.
        ;; On RESUME (no caller-supplied title) seed the atom from the PERSISTED
        ;; session title. Without this a fresh process starts the atom empty, so
        ;; `maybe-auto-title!`'s guard sees "untitled" and RE-titles the session
        ;; from the next message (e.g. a "continue") — overwriting a good title
        ;; cross-process. Placeholder titles ("Untitled") still fall through to
        ;; auto-title via `usable-existing-title`.
        resolved-title
        (or (not-empty (str title))
            (when (and db-info session)
              (when-let [rid (persistance/db-resolve-session-id db-info session)]
                (not-empty (str (:title (persistance/db-get-session db-info rid)))))))

        session-title-atom
        (atom (or resolved-title ""))

        root-resolved-model
        (resolve-effective-model router)

        root-model
        (or (:name root-resolved-model) "unknown")

        root-provider
        (:provider root-resolved-model)

        ;; Routing digest surfaced in the model-facing ctx (`routing`) so
        ;; the agent can SEE its current model + what's available, and pick a
        ;; cheaper/faster one for an easy `sub_loop` (the `model` arg). Read-only;
        ;; the agent never reconfigures routing, it just routes children by cost.
        ;; STRING-KEYED: this digest lands in ctx as `session_routing` and
        ;; crosses the Python boundary. Provider ids stringify at the source.
        routing-digest
        (cond-> {"model" root-model}
          root-provider
          (assoc "provider" (name root-provider))

          (seq (:providers router))
          (assoc "available"
            (mapv (fn [p]
                    {"provider" (name (:id p)) "models" (mapv :name (:models p))})
                  (:providers router))))

        ;; Snapshot a base system prompt for the session row so the
        ;; sidebar / DB inspectors have something stable to display.
        ;; Real per-turn assembly goes through `prompt/assemble-stable-prompt-messages`
        ;; with `:active-extensions`, so this snapshot is just metadata.
        system-prompt
        (prompt/build-system-prompt {})

        resolved-session-id
        (persistance/db-resolve-session-id db-info session)

        ;; Workspace pin (1:1 with session_state):
        ;;   - resuming a session       → derive workspace from its latest state
        ;;   - brand-new session        → mint a trunk workspace, pass its id
        ;;                                into db-store-session! below
        ;; db-info nil (sandbox-only mode) → skip; iteration loop never asserts
        ;;                                workspace pin when there's no DB
        active-workspace
        (when db-info
          (cond
            ;; Resume path: the existing session already pins a
            ;; workspace; honour it.
            resolved-session-id
            (some->> (persistance/db-latest-session-state-id db-info resolved-session-id)
                     (persistance/db-workspace-for-session db-info))
            ;; New session, caller pre-spawned a workspace
            ;; (e.g. /workspace slash spawn-branch path).
            workspace-id (persistance/db-workspace-get db-info workspace-id)
            ;; New session, no pre-spawn: clone cwd.
            :else (workspace/ensure-workspace! db-info {})))

        session-id
        (or resolved-session-id
            (persistance/db-store-session! db-info
                                           (cond-> {:channel (or channel :tui)
                                                    :external-id external-id
                                                    :model root-model
                                                    :title title
                                                    :system-prompt system-prompt
                                                    :workspace-id (:id active-workspace)
                                                    ;; sub_loop child → link this whole soul
                                                    ;; to the parent's session_state (cross-
                                                    ;; soul), keeping it out of the top-level
                                                    ;; list; nil for a normal session.
                                                    :parent-state-id (:parent-state-id child)
                                                    ;; Unadopted TUI warm-pool sessions are
                                                    ;; created UNCLAIMED (:claimed? false) so
                                                    ;; they stay out of the cross-channel list
                                                    ;; until a tab uses them (first turn claims).
                                                    :claimed? (not prewarm?)}
                                             root-provider
                                             (assoc :provider root-provider))))

        ;; Resolve the session_state row id ONCE here (reliable at env build)
        ;; and stamp it on the env, so slashes/turns don't re-query it — the
        ;; per-call re-query intermittently returns nil for fresh sessions,
        ;; which broke `/draft new`'s pin ("session not ready").
        session-state-id
        (when (and db-info session-id) (persistance/db-latest-session-state-id db-info session-id))

        ;; Context wiring (see ctx-loop). `ctx-atom` carries stable session
        ;; context, while `turn-state-atom` tracks live counters. Seeded fresh;
        ;; reloaded from session_turn_state.ctx (Nippy BLOB) on session resume.
        ctx-atom
        (ctx-loop/make-ctx-atom session-id)

        ;; ONE model-driven context-compaction verb, recording a
        ;; `:session/summaries` intent the wire applies via `apply-summaries`:
        ;;
        ;;   session_fold(["tN/iN", …], "what this step established")  — KEEP the
        ;;     conclusion. Collapses those scopes into a single summary line; the
        ;;     summary is the distilled takeaway you still need.
        ;;   session_fold({"through": "tN/iN"}, "…")  — RANGE: fold every step at
        ;;     or before tN/iN in one shot (a positional options dict, NOT a kwarg:
        ;;     Python kwargs don't cross into a Clojure verb — see wrap-ifn).
        ;;   session_fold(["tN/iN", …])  — the gist is OPTIONAL: OMIT it to just
        ;;     DISCARD the step outright (an approach you abandoned, a read you
        ;;     misread) where keeping even a summary would mislead. Replaces the
        ;;     old `session_drop`.
        ;;
        ;; It records a `:session/summaries` intent the wire applies via
        ;; apply-summaries, and RETURNS a visible confirmation (not the silent
        ;; sentinel) so the fold shows in the Python result. See
        ;; `compaction-verbs` for the intent shape + range handling.
        compaction
        (compaction-verbs ctx-atom)

        ;; maki-style in-program concurrency: run each thunk (a Python callable,
        ;; e.g. `lambda: rg({...})`) on a VIRTUAL THREAD and return results in
        ;; order. GraalPy releases its lock on blocking I/O, so I/O-bound tool
        ;; calls genuinely overlap inside ONE run_python call. Dynamic sink
        ;; bindings (tool-event/render) are conveyed via `bound-fn*` so tools
        ;; called concurrently still render. ALL thunks run; if several FAIL,
        ;; every future is still settled (we don't abort at the first throw) and
        ;; ONE aggregated error names EVERY failure by slot index — so the model
        ;; fixes them all in one pass instead of one-per-iteration. No failures →
        ;; results in order, exactly as before.
        gather-fn
        (fn gather [& thunks]
          (let [thunks
                (if (and (= 1 (count thunks)) (sequential? (first thunks)))
                  (vec (first thunks)) ; gather([f1 f2]) too
                  (vec thunks))

                call
                (fn [t]
                  (cond (instance? Value t) (.execute ^Value t (object-array 0))
                        (ifn? t) (t)
                        :else t))

                futs
                (mapv (fn [t]
                        (.submit gather-executor
                                 ^Callable
                                 (bound-fn* (fn []
                                              (call t)))))
                      thunks)

                ;; settle EVERY future — value OR error per slot; child
                ;; futures are hard-cancelled when WE get interrupted
                outcomes
                (settle-gather-futures! futs)

                failures
                (keep-indexed (fn [i o]
                                (when (contains? o :err) [i (:err o)]))
                              outcomes)]

            (if (empty? failures)
              (mapv :ok outcomes)
              ;; aggregate ALL failures into ONE error (slot index +
              ;; message); chain the first as cause for the traceback.
              (throw (ex-info (str "gather: " (count failures)
                                   "/" (count outcomes)
                                   " awaitables failed — "
                                   (str/join "; "
                                             (map (fn [[i e]]
                                                    (str "[" i "] " (or (ex-message e) (str e))))
                                                  failures)))
                              {:vis/gather-failures (mapv (fn [[i e]]
                                                            {:index i
                                                             :message (or (ex-message e) (str e))})
                                                          failures)
                               :vis/gather-total (count outcomes)}
                              (second (first failures)))))))

        ;; ISOLATED sibling of `gather-fn`, backing `__vis_par_isolated__`. Runs
        ;; every thunk on the SAME virtual-thread pool with the SAME real overlap,
        ;; but NEVER throws an aggregate on failure: each slot returns a per-call
        ;; SENTINEL — `{"__vis_ok__" true "__vis_val__" v}` on success, or
        ;; `{"__vis_ok__" false "__vis_exc__" <Throwable>}` on failure. The raw
        ;; Throwable crosses back as a host object (env/->clj `asHostObject`), so
        ;; the loop maps it through the SAME `python-op-error` path a serial call
        ;; uses — byte-identical error fidelity, but ISOLATED (one failing
        ;; observation never poisons its siblings). This backs the
        ;; all-observations concurrent batch (`execute-observation-batch`); the
        ;; model never calls it (it is synthesized by the host, not advertised).
        par-isolated-fn
        (fn par-isolated [& thunks]
          (let [thunks
                (if (and (= 1 (count thunks)) (sequential? (first thunks)))
                  (vec (first thunks))
                  (vec thunks))

                call
                (fn [t]
                  (cond (instance? Value t) (.execute ^Value t (object-array 0))
                        (ifn? t) (t)
                        :else t))

                futs
                (mapv (fn [t]
                        (.submit gather-executor
                                 ^Callable
                                 (bound-fn* (fn []
                                              (call t)))))
                      thunks)]

            (mapv (fn [o]
                    (if (contains? o :err)
                      {"__vis_ok__" false "__vis_exc__" (:err o)}
                      {"__vis_ok__" true "__vis_val__" (:ok o)}))
                  (settle-gather-futures! futs))))

        ;; Build the ctx-loop env subset used by the engine bindings + helpers.
        ;; Just the cursor counters + the single ctx-atom. Warnings
        ;; live as `:engine/warnings` on the ctx itself, no side atoms.
        ;; (D12 retired `:engine/pending-satisfies` along with
        ;; satisfy-hint!; hook-task satisfaction is plain `plan_step`.)
        _ctx-loop-env
        {:ctx-atom ctx-atom
         :turn-state-atom turn-state-atom
         ;; DB + session id ride on the same env
         ;; map so `build-introspect-bindings`
         ;; can hit `session_turn_iteration.forms`
         ;; for the per-form / per-iter / per-turn
         ;; introspection verbs without an extra
         ;; closure capture.
         :db-info db-info
         :session-id session-id}

        ;; The current human turn text and engine context flow through ctx.
        ;; Introspect verbs reach archived entries + any past turn snapshot
        ;; via the soul/state chain. History loader is a thunk so the
        ;; per-call DB read only happens when the model actually invokes
        ;; one of the verbs.
        ;;
        ;; The cross-turn snapshot history loader is gone — rewind/lens/
        ;; grep read the LIVE ctx-atom + per-form DB rows directly, not a
        ;; {turn → ctx} history map. The loader arg is kept nil for
        ;; call-site compatibility.
        env-bindings
        (merge
          ;; BUILT-IN extension kernel (`foundation`):
          ;; cat/ls/rg/patch/… interned BARE into the
          ;; sandbox ns next to the engine verbs — no
          ;; `v/` alias. env resolved lazily (atom not
          ;; built yet). Listed FIRST so engine verbs
          ;; below win any accidental name collision.
          (extension/builtin-sandbox-bindings (fn []
                                                @environment-atom))
          ;; Engine verbs (no `done` — a plain-text reply
          ;; finalizes the turn): the compaction verbs +
          ;; `__vis_par__`, the host virtual-thread pool
          ;; that backs the async runtime's `gather`
          ;; (Python-side `gather`/`await` live in the
          ;; env_python async-runtime preamble; this is
          ;; the dispatcher they call to overlap awaitables
          ;; on real virtual threads).
          compaction
          {(symbol "__vis_par__") gather-fn (symbol "__vis_par_isolated__") par-isolated-fn}
          ;; ntr[tool_id] host callbacks:
          ;; retrieve a PRIOR native tool's persisted result by
          ;; its provider tool_use id (`:svar/tool-call-id`) —
          ;; NO re-fetch. `prime` is the batched pre-scan load
          ;; (list of ids → {id → result}); `fetch` is the lazy
          ;; single-id fallback for a dynamic key. Both close
          ;; over db-info + the live session id and delegate to
          ;; the ONE batched persistence query.
          {(symbol "__vis_native_result_prime__") (fn native-result-prime [ids]
                                                    (persistance/db-native-results-for-tool-ids
                                                      db-info
                                                      session-id
                                                      (into #{} (filter some?) (or ids []))))
           (symbol "__vis_native_result_fetch__")
           (fn native-result-fetch [id]
             (get (persistance/db-native-results-for-tool-ids db-info session-id #{id}) id))
           ;; `ids` is the discovery callback: EVERY native tool_use id in the
           ;; session branch (newest first) so the sandbox can iterate the
           ;; store (keys/items/values/len) instead of needing ids up front.
           (symbol "__vis_native_result_ids__")
           (fn native-result-ids []
             (vec (persistance/db-native-result-ids-for-session db-info session-id)))}
          ;; DELEGATION DISABLED FOR NOW — `#_` discards the whole
          ;; binding map so none of the child-dispatch verbs are
          ;; bound (sub_loop + parallel/sequence/selector/retry).
          ;; The runtime (sub-loop! / parallel-sub-loops! / …) stays
          ;; intact; re-enable by deleting the `#_`. Also unadvertised
          ;; in the system prompt (prompt.clj delegation section).
          #_{'sub-loop (fn sub-loop [prompt subctx & more]
                         ;; "models" is ALWAYS a list (ordered preference,
                         ;; even for one: ["haiku"]) — ONE consistent surface,
                         ;; never a scalar. svar routes + falls back the order.
                         ;; The opts dict crosses the GraalPy boundary via
                         ;; `env-python/->clj`, which keeps dict keys as
                         ;; VERBATIM STRINGS — the accessor is "models".
                         (sub-loop!
                           @environment-atom
                           {:prompt prompt :subctx subctx :models (get (first more) "models")}))
             ;; parallel([{prompt, subctx, models}, …]) — dispatch
             ;; SEVERAL children concurrently (bounded), results in
             ;; input order. Each spec dict crosses the boundary
             ;; keyword-snake (see sub_loop). Same single db-info +
             ;; depth-cap; failures surface per-slot, not as a throw.
             'parallel (fn parallel [specs]
                         (parallel-sub-loops! @environment-atom specs))
             ;; :sequence composite — children IN ORDER,
             ;; gated on success, fail-fast.
             'sequence (fn sequence [specs]
                         (sequence-sub-loops! @environment-atom specs))
             ;; :selector composite — try alternatives IN
             ;; ORDER until one succeeds.
             'selector (fn selector [specs]
                         (selector-sub-loops! @environment-atom specs))
             ;; retry({prompt, subctx, models}, n) — re-run ONE child
             ;; until its focus task succeeds, up to n attempts (default
             ;; 2; selector semantics). Result is stamped with :attempts.
             'retry (fn retry [spec & more]
                      (retry-sub-loop! @environment-atom spec (first more)))}
          ;; Canonical stateful-resource lifecycle:
          ;; `resource_stop(id)` / `resource_restart(id)`
          ;; (B-dispatch — act by id; ctx advertises
          ;; can_stop/can_restart). Session-scoped so the
          ;; agent only touches THIS session's resources.
          ;; No context mutator or introspect
          ;; bindings are installed here.
          (resources/sandbox-bindings session-id))

        ;; Engine substrate: embedded GraalPy (env/create-python-context builds a
        ;; deny-by-default polyglot Context, wires the Clojure tools as Python
        ;; callables, and installs doc/apropos introspection).
        ;; Confine the Python sandbox's REAL filesystem to the workspace root +
        ;; filesystem-root working copies — the SAME set the file tools confine to.
        ;; nil (no workspace) ⇒ the sandbox stays IO-NONE.
        ;; The fn derefs `workspace-atom` (reset by run-turn!'s per-turn
        ;; workspace re-resolve) instead of closing over the env-creation-time
        ;; row, so a mid-session `/draft new|apply|abandon` or `/root <path>`
        ;; retargets the sandbox confinement too — not just the file tools.
        workspace-atom
        (atom active-workspace)

        sandbox-roots-fn
        (when active-workspace
          (fn []
            (when-let [ws @workspace-atom]
              (into [(str (:root ws))] (keep :clone (workspace/filesystem-roots ws))))))

        ;; Network capability: OFF unless the `:network/enabled` toggle is on. When
        ;; on, config.edn `:network` tunes the host policy:
        ;;   :allowed-domains [...]  confine to these (empty or ["*"] ⇒ allow all)
        ;;   :denied-domains  [...]  always-blocked, ON TOP of the secure defaults
        ;;                            (cloud-metadata SSRF endpoints) — wins over allow
        ;; Children inherit it via the bound sandbox.
        net-cfg
        (get (config/load-config-raw) :network)

        network-opts
        {:enabled? (toggles/enabled? :network/enabled)
         :allowed-domains (:allowed-domains net-cfg)
         :denied-domains (:denied-domains net-cfg)}

        {:keys [python-context sandbox-ns initial-ns-keys]}
        (env/create-python-context (merge env-bindings (:custom-bindings @state-atom))
                                   sandbox-roots-fn
                                   network-opts)

        env
        (cond-> {:environment-id environment-id
                 :session-id session-id
                 :session/state-id session-state-id
                 :channel (or channel :tui)
                 ;; What the Python sandbox can ACTUALLY reach this session —
                 ;; `python-execution-tool` builds its fs/network description
                 ;; from this so the prompt never claims a capability the
                 ;; sandbox lacks (no workspace ⇒ no fs; toggle off ⇒ no net).
                 :sandbox-caps {:fs? (boolean sandbox-roots-fn) :network network-opts}
                 ;; Live workspace pointer for the sandbox confinement —
                 ;; run-turn! resets it after its per-turn workspace
                 ;; re-resolve so `sandbox-roots-fn` tracks /draft + /root.
                 :workspace-atom workspace-atom
                 :depth-atom depth-atom
                 ;; false for a sub_loop child reusing the parent's connection
                 ;; — dispose-environment! must NOT close a borrowed DB.
                 :owns-db? owns-db?
                 ;; routing digest → rendered into ctx as `routing`
                 ;; (current model + available, for sub_loop model choice).
                 :routing routing-digest
                 :db-info db-info}
          ;; Workspace info attached at env-build time so the extension
          ;; wrapper's `(workspace/workspace-root env)` finds a non-blank
          ;; root the very first time it fires.
          active-workspace
          (assoc :workspace
            active-workspace :workspace/id
            (:id active-workspace) :workspace/root
            (:root active-workspace) :workspace/filesystem-roots
            (vec (:filesystem-roots active-workspace))
            ;; Every workspace is a rift CoW clone — always a sandbox.
            ;; Reported on :workspace/sandbox?, NOT as a VCS. The
            ;; model-facing :vcs/kind is the real repo VCS, computed in
            ;; foundation.workspace-ctx/render-block.
            :workspace/sandbox?
            true))

        env
        (assoc env
          ;; Context atoms — visible to the rest of the loop so renderer /
          ;; per-iter capture / done snapshot can read or stamp them.
          :ctx-atom ctx-atom
          :turn-state-atom turn-state-atom
          ;; PROMPT-CACHE STABILITY: the standing `session = {…}` block rides
          ;; in the cached system prefix, so it is FROZEN once per process and
          ;; reused across turns — every state change rides as an appended
          ;; `session[...] = …` delta instead of re-rendering the prefix (which
          ;; would bust the cache on any change). Holds
          ;; `{:block <frozen text> :baseline <last-emitted static map>}`;
          ;; nil until the first turn seeds it. A fresh process (resume/restart)
          ;; starts nil → renders fresh from current state (cold cache anyway).
          :standing-ctx-atom (atom nil)
          :state-atom state-atom
          :python-context python-context
          :sandbox-ns sandbox-ns
          :initial-ns-keys initial-ns-keys
          ;; Long-lived per-env LRU map: `{var-name-string →
          ;; last-used-turn-pos}`. Merged from each iteration's
          ;; `:lru` after eval.
          :def-resolve-lru-atom (atom {})
          :router router
          :session-title-atom session-title-atom
          :extensions (atom [])
          :active-extensions (atom []))]

    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :session-id session-id)
    ;; A sub_loop CHILD seeds its in-memory ctx straight from the model-supplied
    ;; subctx (its focused bigger-picture slice) — no DB restore.
    (when-let [seed (:seed-ctx child)]
      (reset! ctx-atom (assoc seed
                         "session_id" session-id
                         "engine_warnings" []
                         "engine_pending_satisfies" [])))
    ;; Restore the context state when resuming. Sandbox defs do NOT persist
    ;; across turns (the `definition_*` sidecar tables were dropped).
    (when (and resolved-session-id (nil? (:seed-ctx child)))
      ;; The latest session_turn_state.ctx (Nippy BLOB) carries the persisted
      ;; context snapshot. Cursor is iter-local so we don't restore it; the
      ;; renderer stamps a fresh one from the loop counters.
      (try (when-let [persisted-ctx (persistance/db-load-latest-ctx db-info session-id)]
             ;; The Nippy blob IS the whole ctx now (no separate task/fact/archive
             ;; tables). It has no `"engine_*"` ephemeral keys (stripped before
             ;; Nippy), so re-seed those empty here so swap! callers don't need
             ;; nil-guards. Read once, on resume; the live render stays in-memory.
             (reset! ctx-atom (assoc persisted-ctx
                                "session_id" session-id
                                "engine_warnings" []
                                "engine_pending_satisfies" [])))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::restore-ctx-failed
                        :data {:error (ex-message t) :session-id session-id}
                        :msg "Failed to restore context state from DB - starting empty"}))))
    ;; Auto-discover everything from `META-INF/vis-extension/vis.edn` on the
    ;; classpath, then install extensions in dependency order. The
    ;; same loader populates channel/command/provider/persistance
    ;; registries as a side effect; we just care about the extension
    ;; rows here.
    (extension/discover-extensions!)
    ;; Project-local Python extensions (`.vis/extensions/*.py`) load after
    ;; classpath discovery so they land in the same registry walk below.
    ;; Idempotent by content fingerprint — a no-op when nothing changed.
    (python-extensions/load-python-extensions!)
    (extension/register-extensions! env install-extension!)
    env))

;; =============================================================================
;; Session env cache
;; =============================================================================

;; ---------------------------------------------------------------------------
;; In-process session cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce
  ^{:doc
    "In-process env cache.

   Keyed by `java.util.UUID` session-soul-id. Under the 1:1 session ↔
   workspace invariant this key is isomorphic to `(:workspace/id env)`
   — one cache entry = one session = one workspace = one Python sandbox
   lineage. Lookups normalize incoming strings to UUID via `cache-key`
   so string-id callers keep working alongside the UUID key."}
  cache
  (atom {}))

(defn- cache-key
  "Normalize an id-shaped value (UUID or string-UUID) to a UUID
   suitable for keying `cache`. Nil → nil so wrapped lookups stay
   honest."
  [id]
  (persistance/->uuid id))

;; ---------------------------------------------------------------------------
;; Idle-env reaper — authoritative backstop against unbounded GraalPy Context
;; growth. Every cached session env pins a GraalPy `Context` (see
;; `dispose-environment!`); the cache itself is never bounded and the tab-close
;; release path (TUI → gateway `/release`) is best-effort and skips busy / still-
;; open / stale-registry sessions, so Contexts leaked whenever that path missed.
;; A background daemon thread sweeps on an interval and disposes envs that have
;; gone idle past a TTL — guarded by each entry's `ReentrantLock` (a running
;; turn holds it, so `tryLock` failing means "busy, skip") so an eval is never
;; killed mid-flight. Evicting a resident env is SAFE: the transcript lives in
;; the DB and `ensure-env!` transparently rebuilds the Context on the next touch.
;; ---------------------------------------------------------------------------

(def ^:private env-idle-ttl-ms
  "Idle window before a cached session env's GraalPy Context is disposed by the
   background reaper. Override with `VIS_ENV_IDLE_TTL_MS`; <= 0 disables the TTL
   sweep. Default 15 min."
  (or (some-> (System/getenv "VIS_ENV_IDLE_TTL_MS")
              str/trim
              parse-long)
      (* 15 60 1000)))

(def ^:private env-cache-max
  "Soft cap on resident session envs. After the TTL sweep, if the cache still
   exceeds this the reaper force-evicts the least-recently-active idle entries
   (still lock-guarded) until back under the cap — a second guard for burst
   churn (e.g. prewarm). Override with `VIS_ENV_CACHE_MAX`; <= 0 disables it.
   Default 32."
  (or (some-> (System/getenv "VIS_ENV_CACHE_MAX")
              str/trim
              parse-long)
      32))

(def ^:private env-reaper-interval-ms
  "How often the idle-env reaper wakes to sweep. Override with
   `VIS_ENV_REAPER_INTERVAL_MS`. Default 60 s."
  (or (some-> (System/getenv "VIS_ENV_REAPER_INTERVAL_MS")
              str/trim
              parse-long)
      (* 60 1000)))

(def ^:private env-max-turns-per-ctx
  "Turns a single session's GraalPy Context serves before the reaper recycles it
   between turns (dispose + rebuild in place). Bounds a long-lived session the
   idle reaper never touches because it never goes idle. Override with
   `VIS_ENV_MAX_TURNS_PER_CTX`; <= 0 disables. Default 50."
  (or (some-> (System/getenv "VIS_ENV_MAX_TURNS_PER_CTX")
              str/trim
              parse-long)
      50))

(def ^:private env-heap-watermark-pct
  "JVM heap-usage percent (used/max) at or above which the reaper treats the
   process as under memory pressure and force-evicts EVERY idle (unlocked)
   session env this sweep — ignoring the idle TTL — to shed GraalPy Contexts
   fast. A running turn holds its entry's lock so it is never evicted; the
   transcript reloads from the DB on the next touch. Override with
   `VIS_ENV_HEAP_WATERMARK_PCT`; <= 0 disables the watermark. Default 85."
  (or (some-> (System/getenv "VIS_ENV_HEAP_WATERMARK_PCT")
              str/trim
              parse-long)
      85))

(defn- heap-used-pct
  "Current JVM heap utilization as an integer percent of the max heap
   (used = total - free). 0 when the max heap is unknown."
  []
  (let [rt
        (Runtime/getRuntime)

        mx
        (.maxMemory rt)]

    (if (pos? mx) (long (/ (* 100 (- (.totalMemory rt) (.freeMemory rt))) mx)) 0)))

(defn- heap-pressure?
  "True when the watermark is enabled and current heap use is at/above it."
  []
  (and (pos? (long env-heap-watermark-pct))
       (>= (long (heap-used-pct)) (long env-heap-watermark-pct))))

(defn- new-cache-entry
  "Build a cache entry wrapping `env`: the environment, its per-session
   `ReentrantLock` (one turn at a time), and an `AtomicLong` `:last-active`
   epoch-ms stamp the reaper reads to decide idleness."
  [env]
  {:environment env
   :lock (java.util.concurrent.locks.ReentrantLock.)
   :last-active (java.util.concurrent.atomic.AtomicLong. (System/currentTimeMillis))
   :turns (java.util.concurrent.atomic.AtomicLong. 0)})

(defn- touch-entry!
  "Bump `entry`'s `:last-active` stamp to now so the reaper treats it as warm.
   Returns `entry` for threading."
  [entry]
  (when-let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
    (.set la (System/currentTimeMillis)))
  entry)

(defn- bump-turns!
  "Increment `entry`'s per-context turn counter and return the new count (0 when
   the entry carries no counter). Read by `send!` to decide a Layer-2 recycle."
  [entry]
  (if-let [^java.util.concurrent.atomic.AtomicLong t (:turns entry)]
    (.incrementAndGet t)
    0))

(defn- evict-if-idle!
  "Dispose + `dissoc` cache entry `k` when its lock is free (no turn running)
   AND it has been idle at least `min-idle-ms` (0 = force). Lock-guarded and
   re-checked under the lock, so it never races a live turn or a concurrent
   `close!`. Returns true iff it evicted."
  [k min-idle-ms]
  (let [entry
        (get @cache k)

        ^java.util.concurrent.locks.ReentrantLock lock
        (:lock entry)]

    (boolean (when (and entry lock (.tryLock lock))
               (try (let [cur
                          (get @cache k)

                          ^java.util.concurrent.atomic.AtomicLong la
                          (:last-active cur)

                          idle
                          (if la (- (System/currentTimeMillis) (.get la)) 0)]

                      (when (and cur (>= (long idle) (long min-idle-ms)))
                        (try (dispose-environment! (:environment cur)) (catch Throwable _ nil))
                        (swap! cache dissoc k)
                        true))
                    (finally (.unlock lock)))))))

(defn reap-idle-envs!
  "One reaper sweep: dispose + evict cached session envs idle past
   `env-idle-ttl-ms` (or, under heap pressure past `env-heap-watermark-pct`,
   EVERY idle env this sweep — TTL ignored), then — if the cache still exceeds
   `env-cache-max` — force-evict the least-recently-active idle entries until
   back under the cap. Every eviction is lock-guarded (a running turn is
   skipped). Returns the number of entries evicted. Safe to call directly
   (tests / manual sweeps)."
  []
  (let [now
        (System/currentTimeMillis)

        age
        (fn [entry]
          (if-let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
            (- now (.get la))
            0))

        pressure?
        (heap-pressure?)

        effective-ttl
        (if pressure? 0 (long env-idle-ttl-ms))

        ttl-evicted
        (if (or pressure? (pos? (long env-idle-ttl-ms)))
          (->> @cache
               (filter (fn [[_ entry]]
                         (>= (long (age entry)) (long effective-ttl))))
               (reduce (fn [n [k _]]
                         (if (evict-if-idle! k effective-ttl) (inc (long n)) n))
                       0))
          0)

        lru-evicted
        (if (pos? (long env-cache-max))
          (let [snapshot
                @cache

                over
                (- (long (count snapshot)) (long env-cache-max))]

            (if (pos? (long over))
              (->> snapshot
                   (sort-by (fn [[_ e]]
                              (age e))
                            >)
                   (take over)
                   (reduce (fn [n [k _]]
                             (if (evict-if-idle! k 0) (inc (long n)) n))
                           0))
              0))
          0)]

    (+ (long ttl-evicted) (long lru-evicted))))

(defn- reaper-loop
  "Background sweep loop: sleep the interval, sweep, repeat. Exits on interrupt;
   any sweep error is logged and swallowed so a single bad sweep never kills the
   reaper."
  []
  (loop []

    (let [continue? (try (Thread/sleep (long env-reaper-interval-ms))
                         (reap-idle-envs!)
                         true
                         (catch InterruptedException _ false)
                         (catch Throwable t
                           (tel/log! {:level :warn :data {:error (ex-message t)}}
                                     "env-reaper sweep failed")
                           true))]
      (when continue? (recur)))))

(defonce ^:private env-reaper-thread (atom nil))

(defn- ensure-env-reaper!
  "Start the idle-env reaper daemon thread once, lazily, on the first cache
   insert. Started here (not at namespace load) so a native-image build-time
   init never spawns a thread, and only when reaping is actually enabled."
  []
  (when (and (pos? (long env-reaper-interval-ms))
             (or (pos? (long env-idle-ttl-ms))
                 (pos? (long env-cache-max))
                 (pos? (long env-heap-watermark-pct)))
             (nil? @env-reaper-thread))
    (locking cache
      (when (nil? @env-reaper-thread)
        (let [t (doto (Thread. ^Runnable reaper-loop "vis-env-reaper") (.setDaemon true))]
          (reset! env-reaper-thread t)
          (.start t))))))

(defn cache-env!
  "Insert `env` into the cache under `session-id` (UUID, or string
   normalized via `cache-key`). Returns `{:id <UUID> :environment env}`."
  [session-id env]
  (let [k (cache-key session-id)]
    (swap! cache assoc k (new-cache-entry env))
    (ensure-env-reaper!)
    {:id k :environment env}))

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

  `create-environment` snapshots the router into
  `(:router env)` at construction time, and the iteration loop calls
  `(svar/ask-code! (:router environment) ...)` - not the global
  `router-atom`. So when a frontend changes provider
  config and rebuilds the global router, every long-lived env in the
  cache (TUI keeps one for the whole session) keeps talking to the
  *previous* model until disposed.

  Call this immediately after `rebuild-router!` so the
  next `send!` on any cached session picks up the new router."
  [router]
  (when router
    (swap! cache (fn [m]
                   (reduce-kv (fn [acc id {:keys [environment] :as entry}]
                                (assoc acc
                                  id (assoc entry :environment (assoc environment :router router))))
                              {}
                              m))))
  nil)

;; Keep live session envs in sync with Python-extension (re)loads. Each env
;; caches its own `:extensions` rows — slash dispatch (`active-slashes env`)
;; and sandbox bindings read those, NOT the global registry — so a `/reload`
;; that swaps the registry must also reseat every cached env. Otherwise a
;; newly added extension stays invisible to running sessions and stale rows
;; keep calling into the closed GraalPy context ("Context execution was
;; cancelled"). Same propagation pattern as `refresh-cached-routers!`.

(defn set-provider!
  "Set the single active provider config. Persists to disk, updates
   in-memory state, rebuilds the global router, and reseats cached
   session envs. `provider` is a svar-native provider map
   `{:id :base-url :api-key :models [...]}`. Replaces an existing
   provider with the same `:id` or appends a new entry."
  [provider]
  (let [cfg
        (or (config/current-config) {:providers []})

        pid
        (:id provider)

        provs
        (vec (:providers cfg))

        idx
        (some (fn [[i p]]
                (when (= (:id p) pid) i))
              (map-indexed vector provs))

        updated
        (if idx (assoc provs idx provider) (conj provs provider))

        prioritized
        (vec (cons provider (remove #(= (:id %) pid) updated)))

        new-cfg
        {:providers prioritized}]

    (config/save-config! new-cfg :set-provider!)
    (reset! @#'config/active-config new-cfg)
    (try (let [r (rebuild-router! new-cfg)]
           (refresh-cached-routers! r))
         (catch Exception e
           (tel/log! {:level :warn :data {:error (ex-message e)}}
                     "Failed to rebuild router after provider change")))
    new-cfg))

(defn- open-env!
  ;; App session entry (create! + resume). The vis engine is the embedded
  ;; GraalPy Python sandbox — there is no other substrate.
  [id {:keys [channel external-id title workspace-id prewarm?]}]
  (let [router
        (get-router)

        env
        (create-environment router
                            (cond-> {:db (config/resolve-db-spec)}
                              id
                              (assoc :session id)

                              channel
                              (assoc :channel channel)

                              external-id
                              (assoc :external-id external-id)

                              title
                              (assoc :title title)

                              workspace-id
                              (assoc :workspace-id workspace-id)

                              prewarm?
                              (assoc :prewarm? prewarm?)))]

    env))

(defn- ensure-env!
  [id]
  (let [k (cache-key id)]
    (if-let [entry (get @cache k)]
      (touch-entry! entry)
      (let [env (open-env! k {})]
        (swap! cache (fn [m]
                       (if (contains? m k) m (assoc m k (new-cache-entry env)))))
        (ensure-env-reaper!)
        (get @cache k)))))

(defn- recycle-env!
  "Between-turns context recycle (Layer 2): rebuild a FRESH env for session `k`
   and swap it into the existing cache entry IN PLACE — REUSING the same
   `ReentrantLock` so a caller queued on the lock re-reads the fresh env — then
   dispose the OLD GraalPy Context (and its own per-env DB connection). MUST be
   called while holding the entry lock, so no turn races the swap and `old` is
   stable. The transcript lives in the DB; `open-env!` resumes it, so the model
   loses only its ephemeral Python globals — the point of the recycle."
  [k]
  (when-let [old (get @cache k)]
    (let [fresh-env (open-env! k {})]
      (swap! cache assoc
        k
        (assoc old
          :environment fresh-env
          :last-active (java.util.concurrent.atomic.AtomicLong. (System/currentTimeMillis))
          :turns (java.util.concurrent.atomic.AtomicLong. 0)))
      (try (dispose-environment! (:environment old)) (catch Throwable _ nil)))))

(defn db-info
  "Return the process-wide shared DB connection bound to
   `(config/resolve-db-spec)`. Thin wrapper over
   `persistance.core/db-shared-connection!` that fills in the default db-spec
   so frontend callers stay clear of config resolution."
  []
  (persistance/db-shared-connection! (config/resolve-db-spec)))

(defn create!
  "Create a brand-new session.

   Opts (all optional):
     :title         display title
     :external-id   channel-specific external id
     :workspace-id  pre-spawned workspace to pin the new session to.
                    When omitted, a trunk workspace is auto-minted in
                    create-environment."
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id workspace-id prewarm?]}]
   (let [env
         (open-env! nil
                    (cond-> {:channel channel
                             :external-id (some-> external-id
                                                  str)
                             :title title}
                      workspace-id
                      (assoc :workspace-id workspace-id)

                      prewarm?
                      (assoc :prewarm? prewarm?)))

         id
         (:session-id env)

         _
         (cache-env! id env)]

     {:id id ; UUID
      :channel channel
      :external-id (some-> external-id
                           str)
      :title title
      :workspace-id (:workspace/id env)})))

(defn by-id
  "Return the session record (UUID `:id`) or nil."
  [id]
  (when-let [session (persistance/db-get-session (db-info) id)]
    {:id (:id session) ; UUID
     :channel (:channel session)
     :external-id (:external-id session)
     :system-prompt (:system-prompt session)
     :model (:model session)
     :title (:title session)
     :created-at (:created-at session)
     :owner-id (:owner-id session)
     :project-id (:project-id session)
     :project-name (:project-name session)
     :project-position (:project-position session)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id (:id c) ; UUID
           :channel (:channel c)
           :external-id (:external-id c)
           :title (:title c)
           :created-at (:created-at c)
           :owner-id (:owner-id c)
           :project-id (:project-id c)
           :project-name (:project-name c)
           :project-position (:project-position c)})
        (persistance/db-list-sessions (db-info) channel)))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn projects
  "List projects (cross-channel). `opts`: :owner-id (default \"local\"),
   :include-archived?. Each carries a live :session-count."
  ([] (projects {}))
  ([opts] (persistance/db-list-projects (db-info) opts)))

(defn get-project [project-id] (persistance/db-get-project (db-info) project-id))

(defn create-project! [opts] (persistance/db-create-project! (db-info) opts))

(defn get-project-by-root
  "Project bound to canonical workspace `root` for `owner-id` (default
   \"local\"), or nil."
  ([root] (get-project-by-root "local" root))
  ([owner-id root] (persistance/db-get-project-by-root (db-info) owner-id root)))

(defn ensure-project-for-root!
  "Get-or-create the project bound to canonical workspace `root` (a project IS a
   tab set). Race-safe: on a UNIQUE(owner_id, workspace_root) collision from a
   creator the insert throws and we re-read. `name` seeds a freshly created
   project (falls back to the root path)."
  ([root] (ensure-project-for-root! "local" root nil))
  ([owner-id root name]
   (or (get-project-by-root owner-id root)
       (try (create-project! {:name (or (not-empty (str name)) (str root))
                              :owner-id (or owner-id "local")
                              :workspace-root root})
            ;; ONLY a lost get-or-create race is expected here (the partial
            ;; UNIQUE index rejects the duplicate) -> re-read the winner. Any
            ;; OTHER failure (disk full, real constraint break) must NOT be
            ;; swallowed as nil: re-read, and if there's still no project the
            ;; original error was the true cause, so rethrow it.
            (catch Throwable e (or (get-project-by-root owner-id root) (throw e)))))))

(defn update-project! [project-id opts] (persistance/db-update-project! (db-info) project-id opts))

(defn delete-project! [project-id] (persistance/db-delete-project! (db-info) project-id))

(defn assign-project!
  "Assign the session soul to `project-id` (nil clears / removes from project)."
  [session-id project-id]
  (persistance/db-set-session-project! (db-info) session-id project-id))

(defn reorder-project-sessions!
  "Persist a manual order for the sessions inside `project-id`."
  [project-id session-ids]
  (persistance/db-reorder-project-sessions! (db-info) project-id session-ids))

;; =============================================================================
;; Host title setter + public env accessor
;; =============================================================================

(defn env-for [id] (:environment (ensure-env! id)))

(defn set-title!
  "Host-driven title change. Resolves the live env (if any) so the
   in-memory atom + listener fan-out stay in sync; falls back to a
   plain DB write when no env is live for this session (e.g.
   `vis sessions` rename ops)."
  [id title]
  (let [env (env-for id)]
    (titling/set-title-with-broadcast! (or (:db-info env) (db-info))
                                       id
                                       (:session-title-atom env)
                                       title))
  nil)

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [k
         (cache-key id)

         {:keys [^java.util.concurrent.locks.ReentrantLock lock] :as entry}
         (ensure-env! id)

         message-vec
         (if (string? messages) [(svar/user messages)] messages)]

     ;; ReentrantLock keeps one turn per session. Extension reload marks
     ;; envs dirty; actual sandbox reset happens here, after prior IR/render is
     ;; finished and before the next user code executes.
     (.lock lock)
     (try
       ;; Re-read :environment UNDER the lock: a between-turns turn-cap recycle
       ;; or a router/extension reseat may have swapped it since we captured
       ;; `entry`, so the queued turn runs against the CURRENT context.
       (turn! (:environment (or (get @cache k) entry)) message-vec opts)
       (finally (let [cur (or (get @cache k) entry)]
                  (touch-entry! cur)
                  (let [n (bump-turns! cur)]
                    (if (and (pos? (long env-max-turns-per-ctx))
                             (>= (long n) (long env-max-turns-per-ctx)))
                      ;; Layer 2: recycle this session's Context between turns so a
                      ;; single never-idle session can't grow it unbounded.
                      (try (recycle-env! k) (catch Throwable _ nil))
                      ;; Layer 1: best-effort guest gc.collect() between turns.
                      (env/collect-garbage! (:environment cur)))))
                (.unlock lock))))))

(defn close!
  [id]
  (let [k (cache-key id)]
    (when-let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
               (clojure.core/get @cache k)]
      ;; BOUNDED: a running turn holds the lock for the whole turn (minutes),
      ;; and a wedged one holds it forever. Wait briefly for a clean handoff,
      ;; then force-dispose anyway — an in-flight turn fails fast against a
      ;; disposed env, which beats blocking close/delete forever behind a
      ;; hung provider stream.
      (if (.tryLock lock 5 java.util.concurrent.TimeUnit/SECONDS)
        (try (try (dispose-environment! environment) (catch Exception _ nil))
             (finally (.unlock lock)))
        (try (dispose-environment! environment) (catch Exception _ nil))))
    (swap! cache dissoc k)))

(defn delete!
  [id]
  (close! id)
  (let [d (db-info)]
    (try (persistance/db-delete-session-tree! d id) (catch Exception _ nil))))

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted - the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn db-sweep-orphaned-running-turns!
  "Mark every `:running` turn as `:interrupted`. Run at process start
   to clean up turns that crashed or were killed mid-write so the next
   turn's handover digest renders the right outcome instead of guessing.
   Returns the number of turns swept."
  ([] (db-sweep-orphaned-running-turns! (db-info)))
  ([db]
   (let [orphans (try (persistance/db-list-session-turns-by-status db :running)
                      (catch Exception _ []))]
     (doseq [{:keys [id iteration-count duration-ms]} orphans]
       (try (persistance/db-update-session-turn!
              db
              id
              {:content [(content/error "turn_interrupted" ORPHAN_INTERRUPTED_ANSWER true)]
               :iteration-count (or iteration-count 0)
               :duration-ms (or duration-ms 0)
               :status :interrupted
               :prior-outcome :cancelled})
            (catch Exception _ nil)))
     (count orphans))))

(defn close-all!
  []
  ;; Process-shutdown path: never let one wedged turn hang the whole
  ;; shutdown. Bounded 2s wait per session, then force-dispose.
  (doseq [[_ {:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}] @cache]
    (if (.tryLock lock 2 java.util.concurrent.TimeUnit/SECONDS)
      (try (try (dispose-environment! environment) (catch Exception _ nil))
           (finally (.unlock lock)))
      (try (dispose-environment! environment) (catch Exception _ nil))))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
