(ns com.blockether.vis.internal.loop
  (:refer-clojure)
  (:require
    [charred.api :as json]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [com.blockether.anomaly.core :as anomaly]
    [com.blockether.svar.core :as svar]
    [com.blockether.svar.internal.llm :as svar-llm]
    [com.blockether.svar.internal.router :as svar-router]
    [com.blockether.svar.internal.util :as util]
    [com.blockether.vis.internal.attachments :as attachments]
    [com.blockether.vis.internal.config :as config]
    [com.blockether.vis.internal.security-policy :as security-policy]
    [com.blockether.vis.internal.cancellation :as cancellation]
    [com.blockether.vis.internal.content :as content]
    [com.blockether.vis.internal.ctx-engine :as ctx-engine]
    [com.blockether.vis.internal.ctx-loop :as ctx-loop]
    [com.blockether.vis.internal.gateway.wire :as wire]
    [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
    [com.blockether.vis.internal.env-python :as env]
    [com.blockether.vis.internal.egress-proxy :as egress]
    [com.blockether.vis.internal.form :as form]
    [com.blockether.vis.internal.gateway-sandbox :as gateway-sandbox]
    [com.blockether.vis.internal.process-jail :as process-jail]
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
    [com.blockether.vis.internal.shell-log :as shell-log]
    [com.blockether.vis.internal.slash :as slash]
    [com.blockether.vis.internal.strutil :as strutil :refer [truncate]]
    [com.blockether.vis.internal.titling :as titling]
    [com.blockether.vis.internal.toggles :as toggles]
    [com.blockether.vis.internal.workspace :as workspace]
    [taoensso.telemere :as tel])
  (:import [java.util.concurrent ExecutionException ExecutorService Future SynchronousQueue
            ThreadFactory ThreadPoolExecutor TimeUnit]
           [java.util.concurrent.atomic AtomicLong]
           [org.graalvm.polyglot Context Value]))

(def ^:private gather-max-threads
  "Hard ceiling on concurrent `gather` worker threads. A GraalPy `Value.execute`
   (the deferred tool-call a `gather` thunk runs) PINS its carrier thread for the
   whole blocking call — GraalPy does NOT unmount a virtual thread across the
   polyglot boundary — so an unbounded virtual-thread pool let heavy concurrent
   overlap ratchet the JDK virtual-thread ForkJoinPool toward its 256-carrier
   ceiling, which never fully reclaimed under sustained turns (the observed
   process-thread growth). A BOUNDED platform pool caps that deterministically
   while still overlapping up to this many real tool calls. Override with
   `VIS_GATHER_MAX_THREADS`; floored at 4. Default 32.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (max 4
              (long (or (some-> (System/getenv "VIS_GATHER_MAX_THREADS")
                                str/trim
                                parse-long)
                        32)))))

(defonce ^:private gather-executor
  ;; Pool backing the sandbox `gather` builtin. Each thunk runs a GraalPy
  ;; `Value.execute` that PINS its carrier for the blocking tool call (the
  ;; polyglot boundary does not unmount virtual threads), so virtual threads gave
  ;; no overlap benefit here and only let carrier spawning grow unbounded. This is
  ;; a bounded, self-reclaiming PLATFORM pool: up to `gather-max-threads` daemon
  ;; workers of genuine overlap, 30 s idle keep-alive, and a SynchronousQueue with
  ;; no retained backlog. Saturated nested work may run inline only from a platform
  ;; thread; a virtual submitter instead blocks on the queue's virtual-thread-safe
  ;; handoff until a platform worker is free, so GraalPy never pins its carrier.
  ;;
  ;; A `delay`, never an eager build: `native-image` initializes this namespace at
  ;; BUILD time, so constructing the pool here forced `@gather-max-threads` on the
  ;; BUILDER — shipping its `VIS_GATHER_MAX_THREADS` answer and putting a live
  ;; ThreadPoolExecutor in the image heap. Built on first `gather`, in the process
  ;; that will actually run the thunks.
  (delay
    (let
      [seq
       (AtomicLong. 0)

       tf
       (reify
         ThreadFactory
           (newThread [_ r]
             (doto (Thread. ^Runnable r (str "vis-gather-" (.getAndIncrement seq)))
               (.setDaemon true))))

       rejection-handler
       (reify
         java.util.concurrent.RejectedExecutionHandler
           (rejectedExecution [_ task executor]
             (cond (.isShutdown ^ThreadPoolExecutor executor)
                   (throw (java.util.concurrent.RejectedExecutionException.
                            "Gather executor is shut down"))
                   (.isVirtual (Thread/currentThread))
                   (try (.put (.getQueue ^ThreadPoolExecutor executor) ^Runnable task)
                        (catch InterruptedException e
                          (.interrupt (Thread/currentThread))
                          (throw (java.util.concurrent.RejectedExecutionException.
                                   "Interrupted while applying gather backpressure"
                                   e))))
                   :else (.run ^Runnable task))))]

      (doto (ThreadPoolExecutor. 0
                                 (int @gather-max-threads)
                                 30
                                 TimeUnit/SECONDS
                                 (SynchronousQueue.)
                                 tf
                                 rejection-handler)
        (.allowCoreThreadTimeOut true)))))

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
  (let
    [cancel-all! (fn []
                   (doseq [^Future f futs]
                     (try (.cancel f true) (catch Throwable _ nil))))]
    (try (mapv (fn [^Future f]
                 (try {:ok (.get f)}
                      (catch ExecutionException e {:err (or (.getCause e) e)})
                      (catch InterruptedException e (throw e))
                      (catch Throwable e {:err e})))
               futs)
         (catch InterruptedException e (cancel-all!) (throw e)))))

;; Single-iteration runner

;; Core helpers

;; Per-iteration `(def ...)` discovery / dependency tracking was retired
;; together with the `definition_*` sidecar tables. Python defs are NOT scratch:
;; they persist for the whole session and, via `env/persist-session-defs!`, into
;; the next PROCESS; cross-turn evidence is read from persisted
;; `session_turn_iteration.forms` rows.

(def ^:private MINI_STACK_DEPTH 12)

(defn- throwable-chain
  [^Throwable t]
  (vec (take-while some?
                   (iterate (fn [^Throwable x]
                              (.getCause x))
                            t))))

(def ^:private CAUSE_CHAIN_LIMIT
  "How many links of a cause chain the error path inspects. Wrapped provider
   failures sit one or two causes deep; a bound keeps a pathological chain from
   turning error formatting into work."
  8)

(defn- bounded-cause-chain [^Throwable e] (take CAUSE_CHAIN_LIMIT (throwable-chain e)))

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

(def ^:private STREAM_FINALIZATION_LOG_KEYS
  "The bounded part of svar's `:stream-finalization` summary: every value is a
   scalar, so the whole selection is safe to log verbatim. It answers what a
   truncated or stalled stream always raises — which SSE event arrived last,
   whether a finish reason was seen, how much had already accumulated — and none
   of it used to survive into the log, so `:svar.core/stream-truncated` read as a
   bare message with no evidence at all.

   The sibling `:partial-content` / `:reasoning` keys of the same ex-data are the
   whole assistant turn and are deliberately never copied here."
  [:terminal? :terminal-kind :terminal-event-type :last-event-type :finish-reason :incomplete?
   :incomplete-reason :content-acc-len :reasoning-acc-len :http-status])

(defn- format-exception-short
  [^Throwable t]
  (let
    [ed
     (ex-data t)

     finalization
     (let [sf (:stream-finalization ed)]
       (when (map? sf)
         (not-empty
           (into {} (filter (comp some? val)) (select-keys sf STREAM_FINALIZATION_LOG_KEYS)))))]

    (cond->
      {:class (.getName (class t))
       :message (or (ex-message t) (str t))
       :causes (throwable-cause-summary t)
       :mini-trace (mini-stack-trace t)}
      (:type ed)
      (assoc :type (:type ed))

      (:status ed)
      (assoc :status (:status ed))

      (:cause-class ed)
      (assoc :cause-class (:cause-class ed))

      finalization
      (assoc :stream-finalization finalization)

      (some? (:content-acc-len ed))
      (assoc :content-acc-len (:content-acc-len ed))

      (some? (:reasoning-acc-len ed))
      (assoc :reasoning-acc-len (:reasoning-acc-len ed)))))

(defn- format-exception
  [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t) {:data (ex-data t) :context context}))
(def ^:private CONSECUTIVE_EMPTY_REPLY_LIMIT
  "Maximum consecutive clean-stop replies with no text or tool call. Svar treats
   those as legitimate completions, so Vis may continue a thinking-only blip, but
   caps the sequence to avoid consuming the full iteration budget without output."
  3)
(def ^:private MAX_PRE_OUTPUT_STREAM_RETRIES
  "How many times Vis re-issues a request a stream watchdog aborted BEFORE any
   output arrived. Two: one for the ordinary blip, one for the blip that repeats,
   and then the turn fails with the stall named. An unbounded ladder would hide a
   wedged endpoint behind minutes of silence, which is the defect this policy
   exists to end, not to move."
  2)

(def ^:private PRE_OUTPUT_STREAM_RETRY_DELAYS_MS
  "Backoff before each pre-output re-issue, indexed by attempt. Short on purpose:
   nothing was generated, so trying again costs one connection, and a provider
   that never answered is queueing rather than throttling — a throttle arrives as
   a 429 and takes svar's rate-limit path instead."
  [1000 3000])

(defn- pre-output-stream-retryable?
  "True when `e` is a stream-watchdog abort Vis may re-issue itself.

   Three conditions, all required: the failure is one of svar's typed watchdog
   aborts anywhere in its cause chain (HTTP clients wrap the typed ex-info), NO
   output has streamed for this attempt, and the attempt budget is not spent.
   With output already painted a resend would duplicate visible text — exactly
   why svar refuses it — so that case stays terminal.

   Measured cause: a provider accepted the POST and sent no response header for
   the whole TTFT budget; svar declined the retry (`:no-retry-path`), its router
   had no second candidate under Vis' pinned sticky routing, and a turn carrying
   ten iterations of finished work died asking the human to type 'Continue'."
  [^Throwable e {:keys [attempt output-started?]}]
  (and (not output-started?)
       (< (long (or attempt 0)) (long MAX_PRE_OUTPUT_STREAM_RETRIES))
       (boolean (some perr/pre-output-stream-abort? (bounded-cause-chain e)))))

(defn- pre-output-stream-backoff-ms
  "Backoff in ms before pre-output re-issue number `attempt` (0-based), clamped to
   the last step of `PRE_OUTPUT_STREAM_RETRY_DELAYS_MS`."
  ^long [attempt]
  (long (nth PRE_OUTPUT_STREAM_RETRY_DELAYS_MS
             (min (long attempt) (dec (count PRE_OUTPUT_STREAM_RETRY_DELAYS_MS))))))

(defn- next-retry-counters
  "Pure counter-threading for retry policies Vis still owns: context overflow,
   max-token recovery, auth refresh/fallback, and the pre-output stream-watchdog
   re-issue. Provider transport and availability are otherwise svar's: it owns
   those retries and returns one terminal result to Vis. The single exception is
   a watchdog abort with NO output — svar declines that one on purpose and its
   router cannot re-route it under a pinned route, so Vis threads it here
   (`pre-output-stream-retryable?`). Returns nil for a real iteration result."
  [result {:keys [attempt max-tokens-attempt] :or {attempt 0 max-tokens-attempt 0}}]
  (let
    [attempt
     (long attempt)

     max-tokens-attempt
     (long max-tokens-attempt)]

    (cond (= result ::retry-context-overflow) [attempt max-tokens-attempt]
          (and (map? result) (contains? result ::retry-max-tokens)) [attempt
                                                                     (inc max-tokens-attempt)]
          (and (map? result) (contains? result ::retry-auth-fallback)) [attempt max-tokens-attempt]
          (= result ::retry-auth-refresh) [(inc attempt) max-tokens-attempt]
          (= result ::retry-pre-output-stream) [(inc attempt) max-tokens-attempt]
          (= result ::retry-auth-backoff) [(inc attempt) max-tokens-attempt])))
(defn- provider-retry-event
  [{:keys [provider model reason attempt delay-ms error status]}]
  (cond->
    {:event/type :llm.routing/provider-retry
     :reason (or reason :stream-connection-error)
     :provider provider
     :model model
     :attempt attempt
     :delay-ms delay-ms
     :error error}
    provider
    (assoc :from-provider provider)

    model
    (assoc :from-model model)

    (some? status)
    (assoc :status status)))

(defn- provider-retry-progress-chunk
  "Canonical live-progress chunk for one transparent provider retry. Keeps the
   concise error identity plus retry/backoff metadata needed by every channel;
   the full throwable remains in telemetry only."
  [iteration-position ^Throwable t {:keys [provider model reason attempt max-retries delay-ms]}]
  (let
    [delay-ms
     (long (or delay-ms 0))

     event
     (provider-retry-event {:provider provider
                            :model model
                            :reason reason
                            :attempt attempt
                            :delay-ms delay-ms
                            :status (:status (ex-data t))
                            :error (ex-message t)})

     error
     (cond-> (select-keys (format-exception-short t) [:type :message :status :cause-class])
       (some? attempt)
       (assoc :attempt attempt)

       (some? max-retries)
       (assoc :max-retries max-retries)

       (pos? delay-ms)
       (assoc :delay-ms delay-ms))]

    {:phase :provider-retry-reset
     :iteration iteration-position
     :attempt attempt
     :max-retries max-retries
     :delay-ms delay-ms
     :error error
     :event event}))

(defn- empty-reply-resend-chunk
  "Live-progress chunk for ONE of svar's same-model empty-reply re-sends.

   The ladder runs INSIDE a single `ask-code!` call, so collecting it and only
   prepending it to the routing trace afterwards leaves the UI with nothing to
   paint for the whole heal — minutes of a frozen bubble that reads as a hang,
   and nothing at all when the human gives up and cancels. Same shape as a
   transport rewind (`:provider-retry-reset`), so every channel already knows how
   to draw it and the gateway persists it the moment it happens."
  [iteration-position resolved-model {:keys [attempt max-resends delay-ms]}]
  (let
    [event
     (cond->
       {:event/type :llm.routing/provider-retry
        :reason :empty-content
        :attempt attempt
        :max-resends max-resends
        :delay-ms delay-ms}
       (:provider resolved-model)
       (assoc :from-provider (name (:provider resolved-model)))

       (:name resolved-model)
       (assoc :from-model (str (:name resolved-model))))

     error
     (cond->
       {:type :svar.llm/empty-content
        :message "Empty reply (no text, no tool call) — re-sending the same request"}
       (some? attempt)
       (assoc :attempt attempt)

       (some? max-resends)
       (assoc :max-retries max-resends)

       (some? delay-ms)
       (assoc :delay-ms delay-ms))]

    {:phase :provider-retry-reset
     :iteration iteration-position
     :attempt attempt
     :max-retries max-resends
     :delay-ms delay-ms
     :error error
     :event event}))

(defn- refusal-fallback-chunk
  "Live-progress chunk for ONE automatic refusal fallback: the current model's
   Anthropic safety classifier DECLINED, so svar switched to a fallback model
   (e.g. Opus 5 → Opus 4.8). Same shape as a transport rewind
   (`:provider-retry-reset`) so every channel already knows how to draw it and
   the gateway persists it the instant it happens — the UI shows the switch
   instead of a silent multi-second gap."
  [iteration-position {:keys [from-model to-model category explanation attempt]}]
  (let
    [event
     (cond-> {:event/type :llm.routing/provider-retry :reason :refusal-fallback :attempt attempt}
       from-model
       (assoc :from-model (str from-model))

       to-model
       (assoc :to-model (str to-model))

       category
       (assoc :category (str category)))

     error
     (cond->
       {:type :svar.llm/refusal
        :message (str "Model declined this request"
                      (when category (str " (" category ")"))
                      (when to-model (str " — switching to " to-model)))}
       explanation
       (assoc :explanation (str explanation))

       attempt
       (assoc :attempt attempt))]

    {:phase :provider-retry-reset
     :iteration iteration-position
     :attempt attempt
     :error error
     :event event}))

(defn- prepend-routing-trace
  [result retry-events]
  (if (seq retry-events)
    (update result :routed/trace #(vec (concat retry-events (or % []))))
    result))

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

(defn- copilot-provider?
  [provider-id]
  (contains? #{:github-copilot :github-copilot-individual :github-copilot-business
               :github-copilot-enterprise}
             provider-id))

(defn- github-copilot-claude-model?
  ;; Every Copilot plan bills Claude the same way, so the premium-interaction
  ;; policy below must recognise all of them - naming only the individual and
  ;; business ids let Copilot Enterprise send :deep reasoning uncapped.
  [resolved-model]
  (and (copilot-provider? (:provider resolved-model))
       (boolean (re-find #"(?i)claude" (str (:name resolved-model))))))

(def ^:private refusal-fallback-models
  "Ordered models Vis retries when the CURRENT model's Anthropic safety
   classifier DECLINES the request (`stop_reason: refusal`). Anthropic
   recommends serving a refused Fable 5 / Opus 5 request on another Claude
   model; Opus 4.8 is Vis's standing fallback. Passed to svar as
   `:refusal-fallbacks`, which owns the actual client-side model switch."
  ["claude-opus-4-8"])

(defn- refusal-fallbacks-for
  "The refusal-fallback chain for `resolved-model`, or nil. Only Anthropic's
   Fable/Opus/Sonnet 5 models emit `stop_reason: refusal`, so this targets those
   by name and drops the current model — so we never attach a pointless fallback
   elsewhere, and never retry into the very model that just refused."
  [resolved-model]
  (let [nm (str (:name resolved-model))]
    (when (re-find #"(?i)claude-(opus|fable|sonnet)-5" nm)
      (not-empty (vec (remove #{nm} refusal-fallback-models))))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋)[\s!.?,]*\s*$")

(defn- casual-user-request?
  [s]
  (let
    [text (some-> s
                  str
                  str/trim)]
    (boolean (and text (<= (count text) 80) (re-find casual-request-pattern text)))))

(defn- copilot-claude-reasoning-level
  "Return the reasoning level Vis sends to GitHub Copilot Claude.

   Only casual chat is special-cased: a bare greeting names no depth, and
   Claude's adaptive thinking then decides for itself whether the turn is
   worth thinking about. Non-Copilot / non-Claude models are untouched.

   There is no longer a `:deep` cap. It existed because Copilot once served
   Claude over the OPENAI-compatible chat wire, where svar pushed
   `reasoning_effort`; the proxy could not read an OpenAI knob for an
   Anthropic model, chose its own depth and spiralled into autonomous
   reasoning loops. Copilot Claude has ridden the native `/v1/messages` wire
   since svar v0.7.111, where depth is `output_config.effort` — the field the
   backend actually reads. Capping there bought nothing but thinking
   SHALLOWER than Anthropic's own default, which is exactly how a `:deep`
   turn ended up rendering two-word thinking summaries."
  [resolved-model user-request reasoning-level]
  (cond (not (github-copilot-claude-model? resolved-model)) reasoning-level
        (casual-user-request? user-request) nil
        :else reasoning-level))


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

(defn- failed-turn-content
  "Content blocks for a turn that ended in FAILURE - never a throw.

   `content/answer-content` VALIDATES, and the fallback answer a failed turn
   carries is frequently not answer-shaped: a provider-exhaustion turn hands back
   a raw error value. Unguarded, that validation throw escapes `send!` BEFORE
   [[persist-turn-outcome!]] runs, so the turn keeps no status, no error and no
   counters, the UI shows an empty turn, and \"Final answer must be canonical
   content or Markdown prose\" replaces the provider failure that actually killed
   it in the log. Measured: an upstream stream timeout turned into a turn with no
   answer and no error card at all, and the human had to type 'Continue'.

   On a throw the content is rebuilt from the last iteration error in `trace` -
   the same provider failure the gateway card names."
  [answer trace]
  (try (content/answer-content answer)
       (catch Throwable _
         (or (try (some-> (some :error (reverse trace))
                          perr/provider-error-content
                          seq
                          vec)
                  (catch Throwable _ nil))
             [(content/error "turn_failed" "Turn failed" false)]))))

(defn- persist-turn-outcome!
  "THE terminal write for one turn: the row that says HOW the turn ended.

   Never let the write that RECORDS an outcome be the write that LOSES it. A
   turn's own payload is the least trustworthy value in the process -- a runtime
   error can quote the whole document that broke it, and SQLite refuses any
   bound value over `SQLITE_MAX_LENGTH` with `[SQLITE_TOOBIG]`. An unguarded
   throw here leaves the turn `:running` for good, with no status, no error and
   no iteration count, inside a session that has long since finished it.

   On a throw the outcome is re-written from a MINIMAL payload: the same status
   and counters, plus a bounded error naming the persistence failure, carrying
   neither `:ctx` nor the answer content that could not be stored. Returns true
   when either write landed, false when the outcome could not be recorded at
   all."
  [db-info session-turn-id opts]
  (try (persistance/db-update-session-turn! db-info session-turn-id opts)
       true
       (catch Throwable t
         (tel/log! {:level :warn
                    :id ::turn-outcome-persist-failed
                    :data {:session-turn-id (str session-turn-id)
                           :status (:status opts)
                           :error (ex-message t)}})
         (let
           [block (content/error
                    "turn_outcome_persist_failed"
                    (str
                      "Warning: this turn finished, but its answer could not be stored ("
                      (ex-message t)
                      "). The outcome is recorded; the answer and working memory for it are lost.")
                    true)]
           (try (persistance/db-update-session-turn!
                  db-info
                  session-turn-id
                  (assoc (select-keys opts
                                      [:iteration-count :duration-ms :tokens :cost :prior-outcome])
                    :status (or (:status opts) :error)
                    :content [block]
                    :error block))
                true
                (catch Throwable t2
                  (tel/log! {:level :error
                             :id ::turn-outcome-lost
                             :data {:session-turn-id (str session-turn-id) :error (ex-message t2)}})
                  false))))))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")

(def ^:private MARKDOWN_FENCE_RE #"^\s*`{3,}[A-Za-z0-9_-]*\s*$")

(defn- bare-string-code-block? [expr] (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- markdown-fence-line? [line] (boolean (re-matches MARKDOWN_FENCE_RE (str line))))

(defn- markdown-fence-block?
  [expr]
  (let
    [lines (->> (str/split-lines (str expr))
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
    "Your python_execution code is just a bare string literal. To ANSWER, reply with plain text and DON'T call python_execution — never pass a quoted string as the program."
    (markdown-fence-block? expr)
    "A Markdown fence (` ```… `) leaked into your python_execution code. Pass ONLY executable Python statements — no fence markers."
    (comment-only-block? python-context expr)
    "Your python_execution code is only `#` comments with no executable statement. Add a statement to run, or reply with plain text instead of calling python_execution."))

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

;; ONE persistent interpreter per session. The GraalPy sandbox is created ONCE
;; (`create-environment`) and reused across every turn, so the model's globals
;; (defs, imports, variables) carry across calls and turns NATURALLY, REPL-style.
;; (Resuming a session in a FRESH process starts with an empty sandbox; durable
;; file edits and conversation history persist, so the model recomputes what it
;; needs.)

(def ^:private GUEST_INTERRUPT_GRACE_MS
  "How long [[interrupt-guest!]] waits for the GraalPy safepoint to unwind a
   runaway guest before giving up."
  5000)

(defn- interrupt-guest!
  "Cancel whatever is EXECUTING in `python-context` right now, at a Truffle
   safepoint.

   `Future.cancel(true)` only interrupts the JAVA worker thread, and GraalPy does
   NOT observe `Thread.interrupt` inside guest code: a model block that spins
   (`while True: ...`) survived every eval timeout / Esc cancel and kept burning a
   whole core FOREVER — measured at 1.01 busy cores with BOTH worker futures
   already cancelled — while its virtual thread stayed pinned to a
   ForkJoinPool carrier and its frames stayed reachable. Only
   `Context.interrupt` (or a cancelling close) unwinds guest frames. Safe from
   any thread that is not itself executing this context (the loop/eval-timeout
   thread and the canceller are not), never throws, and leaves the context
   REUSABLE for the next turn — the interrupt is NON-DESTRUCTIVE, so nothing here
   ever needs a fresh interpreter.

   Returns TRUE when it landed, i.e. every thread executing the context left it
   inside the grace. FALSE is the javadoc's own failure mode — \"a context thread
   may not be interruptible if it uses non-interruptible waiting or executes
   non-interruptible host code\", reported as a `TimeoutException` — and it is
   the ONLY reason to fall back on `Thread.interrupt`, which lands inside
   GraalPy's GIL re-acquire (`PythonContext.ensureGilAfterFailure` takes the lock
   uninterruptibly) and leaves a worker abandoned there dead OWNING the GIL. Host
   waits that poll [[rt/guest-safepoint!]] keep this returning true."
  [python-context]
  (boolean (when python-context
             (try (.interrupt ^org.graalvm.polyglot.Context python-context
                              (java.time.Duration/ofMillis (long GUEST_INTERRUPT_GRACE_MS)))
                  true
                  (catch java.util.concurrent.TimeoutException _ false)
                  (catch Throwable _ false)))))

(defn attachment-descriptor
  "One `session_attachment` row as the compact DESCRIPTOR `list_attachments()` and
   `get_attachment(id)` hand the model: identity, provenance and shape, and never
   any bytes.

   PROVENANCE STARTS AT THE TURN. Every row carries `session_turn_soul_id`, so
   every descriptor carries `:turn-id` — a user image and a tool artifact are
   placed the same way and a rail can be grouped by turn without a second
   lookup. `:iteration-id` / `:tool-call-id` are the FINER grain only a tool
   artifact has, so a user image omits both instead of carrying nils that say
   nothing.

   `:is-pending` is false here by construction: a stored row is stored. The
   sandbox reader answers the same key `true` for an artifact the RUNNING block
   just attached, which is not in the database yet."
  [a]
  (cond->
    {:id (:id a)
     :source (:source a)
     :filename (:filename a)
     ;; VERSION: same filename in this session = one artifact
     ;; iterated. The rail is a set of version CHAINS, not loose files.
     :version (:version a)
     :media-type (:media-type a)
     :kind (:kind a)
     :size (:size a)
     :position (:position a)
     :turn-id (:turn-soul-id a)
     :is-pending false
     :audience (attachments/attachment-audience a)}
    (= :tool (:source a))
    (assoc :iteration-id
      (:iteration-id a) :tool-call-id
      (:tool-call-id a))))


(defn- run-python-code
  "Run an agent code block through the embedded GraalPy sandbox. Wraps the
   worker-future + cancellation + tool-event/render sinks + `*1`/`*e` recovery
   stack around `env/run-python-block` (whole-block; tools fire in order through
   their ProxyExecutable wrappers, which read the SAME dynamic sinks)."
  [python-context code & {:keys [tool-event-fn env]}]
  (let
    [thrown
     (atom nil)

     tool-counts
     (atom {})

     cancel-token
     (:cancel-token env)

     attachment-reader
     (let
       [d
        (:db-info env)

        sid
        (:session-id env)]

       (when (and d sid)
         {:list (fn []
                  (try (mapv attachment-descriptor
                             (persistance/db-list-session-attachments-meta d sid))
                       (catch Throwable _ [])))
          :read (fn [id]
                  ;; Never turn a UUID into cross-session read authority: prove it
                  ;; belongs to this active session before the indexed row lookup.
                  (when (some #(= (str id) (str (:id %)))
                              (persistance/db-list-session-attachments-meta d sid))
                    (attachment-storage/hydrate (persistance/db-read-attachment d id))))
          :reinspect (fn [id]
                       (when-let
                         [a (when (some #(= (str id) (str (:id %)))
                                        (persistance/db-list-session-attachments-meta d sid))
                              (attachment-storage/hydrate (persistance/db-read-attachment d id)))]
                         (when (and (str/starts-with? (str (:media-type a)) "image/")
                                    ;; An externally stored image whose backend is unavailable
                                    ;; has metadata but no bytes. Do not acknowledge it then emit
                                    ;; an invalid `data:image/...;base64,` block next request.
                                    (not (str/blank? (str (:base64 a)))))
                           (mpl-capture/queue-reinspection! a)
                           a)))}))

     record-tool-event
     (fn [event]
       (let
         [op
          (:op event)

          n
          (get (swap! tool-counts update op (fnil inc 0)) op)

          event*
          (cond-> event
            (not= n 1)
            (assoc :id (str (name (or op :tool)) "-" n)))]

         (when tool-event-fn (tool-event-fn event*))))

     reinspection-sink
     (atom [])

     timeout-ms
     (long (rt/eval-timeout-ms-for-code rt/*eval-timeout-ms* code))

     ;; MOVABLE wall: a `human-input` pause inside the block parks this clock
     ;; instead of dying at it (see rt/parkable-wall).
     {eval-deadline :deadline eval-park :park}
     (rt/parkable-wall (System/currentTimeMillis) timeout-ms)

     exec-future
     (cancellation/worker-future
       "vis-python-eval"
       (fn []
         (try
           ;; THE session context, installed on the thread the guest actually runs
           ;; on. A sandbox SHIM bridge (`ls`, `attach`, …) reads the AMBIENT
           ;; context — only an extension SYMBOL installs its own around every
           ;; call — and this worker future starts bare, so without this the
           ;; block's shims ran session-less: `workspace/*filesystem-roots*` empty
           ;; (`ls` refusing a bound extra filesystem root that `cat`/`grep` on the
           ;; same path accept) and a nil environment reaching the `:fs/access`
           ;; gate that is supposed to hide a tree from the listing too.
           (extension/with-context
             {:env env}
             (binding
               [rt/*blocking-wall-park*
                eval-park

                extension/*tool-event-sink*
                record-tool-event

                mpl-capture/*attachment-reader*
                attachment-reader

                mpl-capture/*attachment-reinspection-sink*
                reinspection-sink]

               ;; One persistent interpreter per session: globals (defs,
               ;; imports, vars) carry across calls/turns NATURALLY.
               (assoc (env/run-python-block python-context code {:form-cap (:form-cap env)})
                 :lru {}
                 :reinspect-attachments (mpl-capture/drain-reinspections reinspection-sink))))
           (catch Throwable e
             (reset! thrown e)
             {:result nil :lru {} :forms [] :error (python-op-error python-context e code)}))))

     dispose-cancel-hook
     (when cancel-token
       (cancellation/on-cancel! cancel-token
                                (fn []
                                  ;; The DOCUMENTED cancel first: it unwinds guest
                                  ;; frames and every host wait that polls
                                  ;; `rt/guest-safepoint!`, and leaves the context
                                  ;; reusable. Fall back on the Java interrupt only
                                  ;; when that did not land — it hits GraalPy's
                                  ;; uninterruptible GIL re-acquire, where an
                                  ;; abandoned worker dies owning the GIL.
                                  (when-not (interrupt-guest! python-context)
                                    (try (.cancel ^java.util.concurrent.Future exec-future true)
                                         (catch Throwable _ nil))))))

     timeout-sentinel
     (Object.)

     execution-result
     (try (rt/await-wall exec-future eval-deadline timeout-sentinel)
          (catch Throwable e
            (reset! thrown e)
            (when-not (interrupt-guest! python-context)
              (try (.cancel ^java.util.concurrent.Future exec-future true) (catch Throwable _ nil)))
            {:result nil :lru {} :error (python-op-error python-context e code)})
          (finally (when dispose-cancel-hook (try (dispose-cancel-hook) (catch Throwable _ nil)))))]

    (if (identical? timeout-sentinel execution-result)
      ;; Eval timeout: the guest frame is unwound by a Truffle safepoint interrupt,
      ;; and only a guest that refuses to unwind is worth the Java interrupt on top
      ;; of it.
      (do (when-not (interrupt-guest! python-context)
            (.cancel ^java.util.concurrent.Future exec-future true))
          ;; What the block PRINTED before the wall is real work — progress lines
          ;; of a fetch loop, results already computed. The guest never reaches
          ;; its own `{:stdout}` outcome here, so drain the capture buffer onto
          ;; the envelope instead of answering with a bare `Timeout` and
          ;; nothing else: that is unactionable, and the model re-runs the whole
          ;; block blind.
          (let
            [envelope
             {:result nil
              :lru {}
              :error {:message (str "Timeout (" (/ timeout-ms 1000) "s)")}
              :timeout? true}

             out
             (env/partial-stdout python-context)]

            (cond-> envelope
              out
              (assoc :stdout out))))
      execution-result)))

(defn- run-with-timing
  [python-context code _sandbox-ns timeout-ms start-time tool-event-fn env]
  (let
    [run!
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
    (let
      [start-time (System/currentTimeMillis)
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

      ;; Helper definitions outlive the PROCESS. The sandbox dies with the
      ;; gateway, so this session's own `def`s are snapshotted after every block
      ;; and re-created by `restore-session-defs!` in the next process's fresh
      ;; sandbox. Best effort, after the outcome is in hand — never in its way.
      (env/persist-session-defs! python-context (:session-id environment))
      exec)))

;; Print-cap defaults for `fmt/bounded-value-str` - chosen so a wide flat
;; collection or a deep nested map still pr-strs without materializing
;; an unbounded JVM string before truncation. Override per call site
;; when a tighter or looser bound is required.

;; Error normalization

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
  ;; Provider/runtime failures cannot be repaired by feeding them to the model.
  (into #{:svar.core/http-error :svar.core/stream-cancelled :svar.core/stream-idle-timeout
          :svar.core/stream-semantic-timeout :svar.llm/all-providers-exhausted
          :svar.llm/circuit-open :svar.llm/provider-exhausted :svar.llm/provider-unavailable}
        perr/CONTEXT_OVERFLOW_TYPES))

(defn- infrastructure-error?
  [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(defn- provider-failure-cause
  "The throwable carrying a provider failure that escaped svar, or nil. Svar has
   already classified it and exhausted every retry/fallback policy it owns, so
   feeding it back to the model would issue a second provider request from Vis.
   HTTP clients wrap the typed exception, so inspect bounded causes without
   reclassifying the error here; the throwable returned is the one whose
   `perr/provider-error-kind` names the failure on the card AND in the log."
  [^Throwable e]
  (some (fn [^Throwable t]
          (when (perr/provider-failure? t) t))
        (bounded-cause-chain e)))

(defn- non-correctable-log-message
  "The fatal log line for a provider failure svar already gave up on. It names
   the CLASSIFIED kind — the same one the card shows — because the previous fixed
   text called every one of them a rate limit / auth / spend cap failure, which
   sent readers of a truncated stream hunting for a billing problem."
  [^Throwable provider-failure]
  (str "Non-correctable provider error ("
       (name (perr/provider-error-kind provider-failure))
       ") - failing turn instead of re-asking the same provider"))

(defn- user-error-data?
  "True when an ex-data / iteration-error `:data` map marks a user-fixable failure."
  [d]
  (or (true? (:vis/user-error d)) (= :svar/no-providers (:type d))))

(defn- user-configuration-error?
  "True for a failure the USER must fix outside the conversation.

   `:vis/user-error` marks exactly that class: an unset `${API_KEY}` env var,
   a router with no usable provider, or a bad CLI/config value. The model cannot
   repair any of those inside the conversation, so fail once with the actionable
   message intact."
  [^Throwable e ex-data-map]
  (boolean (or (user-error-data? ex-data-map)
               (some user-error-data? (map ex-data (bounded-cause-chain e))))))

(defn- user-error-content
  "Terminal content for a turn killed by a user-fixable configuration error.

   The generic provider card (`provider_unavailable`) would swallow the ONE
   thing the user needs — the name of the unset env var — so render the
   actionable message itself. Returns nil for every other failure, leaving the
   provider card path untouched."
  [iteration-error-data]
  (let
    [d
     (:data iteration-error-data)

     msg
     (some-> (:message iteration-error-data)
             str
             str/trim
             not-empty)]

    (when (and msg (or (user-error-data? d) (user-error-data? iteration-error-data)))
      [(content/error "config_error" msg false)])))

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
  (let
    [input
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
  (when-let
    [c (some (fn [m]
               (when (= (:role m) "user") (:content m)))
             (reverse messages))]
    (let
      [s (if (sequential? c)
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
  (let
    [ex-data-map
     (ex-data e)

     iteration
     (:iteration ctx)

     hopeless-overflow?
     (hopeless-context-overflow? ex-data-map)

     provider-failure
     (provider-failure-cause e)

     non-correctable?
     (some? provider-failure)

     user-error?
     (user-configuration-error? e ex-data-map)

     fatal?
     (or (infrastructure-error? ex-data-map) hopeless-overflow? non-correctable? user-error?)

     iteration-error-data
     (exception->iteration-error-data e ctx)]

    (tel/log!
      {:level (if fatal? :error :warn)
       :data (let
               [base
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
        non-correctable? (non-correctable-log-message provider-failure)
        user-error?
        "User configuration error (unset env var / no usable provider) - failing turn once with the actionable message"
        fatal? "Provider infrastructure error - failing turn without RLM restarts"
        :else "RLM iteration failed, feeding error to LLM"))
    (cond-> {::iteration-error iteration-error-data}
      fatal?
      (assoc ::fatal-iteration-error true))))

;; get-locals (read sandbox vars)

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

;; Parsed form helpers

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

     ;; Dedupe duplicate (stuttered) blocks by source: the same program arriving
     ;; twice is the provider repeating itself, so keep the first copy.
     block-key
     (fn [b]
       (:source b))

     unique-blocks
     (->> blocks
          (remove #(str/blank? (:source %)))
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
             (let
               [src
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
                 (assoc :vis/tool-name (:vis/tool-name b)))))
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
  (let
    [message
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
   (let
     [ctx (merge {:environment environment
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
                       (extension/with-context
                         {:ext ext :env environment}
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
  (let
    [turn-state-atom
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
    :else (let
            [title
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
              :else (cond->
                      {:id id
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
  [environment active-extensions ctx]
  (vec (mapcat (fn [ext]
                 (keep (fn [{:keys [id phase lifetime] hook-fn :fn}]
                         (when (= :turn.iteration/start phase)
                           (extension/with-context
                             {:ext ext :env environment}
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
   in resume context.)

   Both a fold and a drop collapse to ONE breadcrumb per distinct GIST/reason —
   NOT one per form and NOT one per covered iteration. One `fold_session` over 40
   iterations carries one gist, so repeating that gist 40 times is 40x the tokens
   for zero information; every later request (and every message queued behind a
   running turn) paid it. Dedup therefore keys on the breadcrumb TEXT: a fold →
   `{:scope tN/iN :gist g}`, a drop → `{:scope tN/iN :dropped? true :note why}`
   (the reason is kept so introspection never loses what went or why), `:scope`
   being the FIRST iteration the summary covers. Every other live form keeps its
   `{:scope tN/iN/fN :src …}` line. `:drop?` — not gist presence — picks the
   label. An open-start (`-tN/iK`) range cursor is resolved against this turn's
   own iteration scopes. Pure."
  [forms summaries]
  (let
    [universe
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
                (let
                  [sc
                   (:scope f)

                   isc
                   (iter-of-scope sc)]

                  (cond (and isc (contains? drop-of isc)) ; dropped → ONE audit line per reason
                        (let
                          [note
                           (get drop-of isc)

                           k
                           [:dropped note]]

                          (if (contains? seen k)
                            [acc seen]
                            [(conj acc
                                   (cond-> {:scope isc :dropped? true}
                                     note
                                     (assoc :note note))) (conj seen k)]))
                        (and isc (contains? gist-of isc)) ; folded → ONE line per distinct gist
                        (let
                          [gist
                           (get gist-of isc)

                           k
                           [:gist gist]]

                          (if (contains? seen k)
                            [acc seen]
                            [(conj acc {:scope isc :gist gist}) (conj seen k)]))
                        (and sc
                             (or (some? (:result f)) (some? (:stdout f))) ; live, worth listing
                             (not= "vis_silent" (:result f)))
                        [(conj acc {:scope sc :src (ctx-engine/compact-src (:src f))}) seen]
                        :else [acc seen])))
              [[] #{}]
              forms))))

(defn- user-slash-iteration?
  "True for a synthetic slash-command iteration. These rows stay in local
   transcript/audit history but must never enter a later provider request."
  [iteration]
  (boolean (some #(= "user-slash"
                     (some-> (:tag %)
                             name))
                 (:forms iteration))))

(def ^:private terminal-incomplete-turn-statuses #{:interrupted :error :cancelled})

(def ^:private interrupted-turn-statuses #{:interrupted :error})

(defn- terminal-incomplete-turn-status?
  [status]
  (contains? terminal-incomplete-turn-statuses status))

(defn- interrupted-turn-status? [status] (contains? interrupted-turn-statuses status))

(defn- previous-turn-context
  "Prior provider-visible turns as an append-only RESUME sequence, compacted by
   the persisted fold ledger. Q/A removal keys off EXPLICIT whole-turn intent
   only (`\"turns\"` stamped by expand-through: a bare `tN` or a range selector
   spanning the turn) — an enumerated iteration fold that happens to name every
   iteration keeps the turn's Q/A recap with folded result lines. A turn covered
   with explicit intent loses its complete Q/A + result recap here; the trailer
   (`apply-summaries`) owns the ONE durable checkpoint anchored at its folded
   iterations. A covered turn with NO done iterations has no trailer anchor, so
   it materializes a minimal `:checkpoint?` entry here instead of vanishing —
   nothing leaves the wire without a visible tombstone. Broader/newer summaries
   are resolved first (supersede merges whole-turn intent), so a fold-of-fold
   cannot leave older Q/A or breadcrumbs beside the checkpoint.

   Synthetic slash turns remain local-only. Oldest→newest; current/running turns
   are excluded. Cancelled/error/interrupted turns remain even without an answer
   so settled work and the unfinished boundary survive; nil when no
   provider-visible representation remains."
  [environment current-turn-id]
  (try
    (when-let [session-id (:session-id environment)]
      (let
        [d (:db-info environment)
         summaries (some-> (:ctx-atom environment)
                           deref
                           (get "session_summaries"))
         include? (fn [turn]
                    (and (not= (str (:id turn)) (str current-turn-id))
                         (not= :running (:status turn))
                         (or (seq (some-> (:content turn)
                                          answer-markdown
                                          str
                                          str/trim))
                             (terminal-incomplete-turn-status? (:status turn)))))
         turns (filter include? (persistance/db-list-session-turns d session-id))
         turn-data
         (into []
               (keep
                 (fn [turn]
                   (let
                     [iterations (->> (try (persistance/db-list-session-turn-iterations d
                                                                                        (:id turn))
                                           (catch Throwable _ []))
                                      (filter #(= :done (:status %)))
                                      vec)]
                     (when-not (some user-slash-iteration? iterations)
                       (let [forms (vec (mapcat :forms iterations))]
                         {:turn (long (or (:position turn) 0))
                          :user-request (:user-request turn)
                          :answer (when-not (terminal-incomplete-turn-status? (:status turn))
                                    (answer-markdown (:content turn)))
                          :interrupted? (interrupted-turn-status? (:status turn))
                          :cancelled? (= :cancelled (:status turn))
                          :forms forms
                          :iter-scopes (into #{} (keep #(iter-of-scope (:scope %))) forms)})))))
               turns)
         ;; Q/A recap weight per turn (~tokens, chars/4 — the SAME estimator as
         ;; `engine_iter_weights`): stamped on the ctx so `fold_session`'s ack and
         ;; the `now` budget can price the recap a whole-turn fold removes. Built
         ;; from the DB turn rows (not the fold ledger), so an already-folded
         ;; turn keeps a stable weight instead of dropping to zero.
         _ (when-let [ca (:ctx-atom environment)]
             (try (swap! ca assoc
                    "engine_turn_weights"
                    (into {}
                          (map (fn [{:keys [turn user-request answer]}]
                                 [turn
                                  (quot (+ (count (str user-request)) (count (str answer))) 4)]))
                          turn-data))
                  (catch Throwable _ nil)))
         universe (into [] (comp (mapcat :iter-scopes) (distinct)) turn-data)
         resolved (ctx-engine/supersede-summaries (ctx-engine/expand-through (or summaries [])
                                                                             universe))
         ;; A whole-turn fold removes turn T's Q/A recap ONLY when it was issued
         ;; in a LATER turn — one that actually SAW T's answer. A whole-turn
         ;; selector issued DURING turn T (`issued_turn` = T) resolves to cover
         ;; T against next request's now-complete universe, but must NOT erase the
         ;; answer T produced AFTER the fold was recorded; it degrades to the
         ;; enumerated path (Q/A recap kept, only the settled result lines fold on
         ;; the trailer). A later broader re-fold supersedes it with its own
         ;; higher `issued_turn`, legitimately re-enabling removal. Legacy
         ;; summaries with no `issued_turn` keep the prior unconditional behavior.
         covering-summary (fn [{:keys [turn]}]
                            (last (filter (fn [summary]
                                            (and (contains? (set (get summary "turns")) turn)
                                                 (let [it (get summary "issued_turn")]
                                                   (or (nil? it) (> (long it) (long turn))))))
                                          resolved)))]

        (some->>
          (reduce
            (fn
              [out
               {:keys [turn user-request answer interrupted? cancelled? forms iter-scopes] :as td}]
              (if-let [summary (covering-summary td)]
                (if (seq iter-scopes)
                  ;; The trailer's apply-summaries path owns the ONE durable
                  ;; breadcrumb (anchored at this turn's folded iterations).
                  ;; Removing the complete Q/A representation here avoids
                  ;; echoing that checkpoint in a second wire location.
                  out
                  ;; No done iterations → no trailer anchor exists anywhere.
                  ;; Materialize the checkpoint HERE so the fold never
                  ;; erases a turn without a visible tombstone. Consecutive
                  ;; turns covered by the SAME summary share one entry.
                  (let [prev (peek out)]
                    (if (and (:checkpoint? prev) (identical? (:summary prev) summary))
                      (conj (pop out) (update prev :turns conj turn))
                      (conj out
                            {:checkpoint? true
                             :summary summary
                             :turns [turn]
                             :gist (or (some-> (get summary "gist")
                                               str
                                               str/trim
                                               not-empty)
                                       (str "(dropped — raw turn data remains in session storage"
                                            (when (toggles/enabled? "introspection")
                                              "; recover via `await read_session()`")
                                            ")"))}))))
                (conj out
                      (cond->
                        {:turn turn
                         :user-request user-request
                         :answer answer
                         :interrupted? interrupted?
                         :results (vec (take 40 (prior-turn-scope-index forms resolved)))}
                        cancelled?
                        (assoc :cancelled? true)))))
            []
            turn-data)
          not-empty
          (mapv #(dissoc % :summary)))))
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
  (try
    (when-let [session-id (:session-id environment)]
      (let
        [db (:db-info environment)
         turns (or (persistance/db-list-session-turns db session-id) [])
         current-id (str current-turn-id)]

        (some (fn [turn]
                (let
                  [iters (try (persistance/db-list-session-turn-iterations db (:id turn))
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
  (when (and ctx-atom util)
    (swap! ctx-atom (fn [ctx]
                      ;; Arm at 75% of the operating budget. Pressure guidance escalates
                      ;; before overflow and remains armed until a measured request falls
                      ;; below that threshold; ignored warnings never silently expire.
                      (let
                        [turn
                         (long (or (get ctx "session_turn") 1))

                         req
                         (long (or (get util "last_request_tokens") 0))

                         cap
                         (long (or (get util "auto_compress_above") 0))

                         pressured?
                         (and (pos? cap) (>= (* req 4) (* cap 3)))

                         since
                         (get ctx "engine_overbudget_hint_turn")]

                        (cond-> (assoc ctx "engine_utilization" util)
                          (and pressured? (nil? since))
                          (assoc "engine_overbudget_hint_turn" turn)

                          (not pressured?)
                          (dissoc "engine_overbudget_hint_turn")))))))

(defn- stamp-iter-universe!
  "Record the raw iteration universe while pricing only `wire-iters` — the
   CURRENT provider-visible projection. `wire-iters` defaults to
   `trailer-iters`."
  [ctx-atom trailer-iters & [wire-iters]]
  (when ctx-atom
    (let
      [scope-of
       (fn [rec]
         (some iter-of-scope (keep :scope (:forms-vec rec))))

       uni
       (into []
             (comp (keep (fn [[_ rec]]
                           (scope-of rec)))
                   (distinct))
             trailer-iters)

       ;; Keep the raw scope identity, but price the corresponding visible record.
       ;; `apply-summaries` preserves trailer order while replacing collapsed forms
       ;; with a zero-weight breadcrumb, so an already-folded scope cannot reclaim
       ;; its historical raw payload again on a later, broader fold.
       ;;
       ;; A cross-turn seed carried from a turn that COMPLETED normally is worth
       ;; ZERO for the same reason: `conversation-suffix`'s
       ;; `:preserved-thinking/replay? false` branch emits no assistant message and
       ;; no results for it (the outcome already rides in the prior-turn recap), so
       ;; its payload does not reside on the wire at all. Pricing it anyway let any
       ;; selector reaching back over a turn boundary (a `-tN/iK` fold early
       ;; in a new turn) bill the FULL historical payload of every prior-turn
       ;; iteration that was never explicitly folded — cards claiming to reclaim
       ;; more than the entire request they folded, and phantom tokens accumulating
       ;; toward the session-rebase threshold. Seeds from terminal INCOMPLETE turns
       ;; do replay their settled results as plain text, so they keep their weight.
       off-wire-seed?
       (fn [rec]
         (and (false? (:preserved-thinking/replay? rec))
              (not (terminal-incomplete-turn-status? (:cross-turn/turn-status rec)))))

       weights
       (persistent! (reduce (fn [m [[_ raw-rec] [_ wire-rec]]]
                              (if-let [sc (scope-of raw-rec)]
                                (let
                                  [chars (if (or (off-wire-seed? raw-rec) (off-wire-seed? wire-rec))
                                           0
                                           (reduce +
                                                   0
                                                   (map form-wire-chars
                                                        (remove :summary? (:forms-vec wire-rec)))))]
                                  (assoc! m sc (+ (long (get m sc 0)) (quot (long chars) 4))))
                                m))
                            (transient {})
                            (map vector trailer-iters (or wire-iters trailer-iters))))]

      (swap! ctx-atom assoc "engine_iter_universe" uni "engine_iter_weights" weights))))

(defn- runtime-turn-prefix
  [environment]
  (let
    [id-s
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
  (let
    [finished
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
  (let
    [[_ iteration block] (re-matches
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
  (let
    [blocks (mapv (fn [block]
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

;; run-iteration

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
        (let
          [input
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

          (cond->
            {:input-tokens input
             :output-tokens output
             :input-tokens-details
             {:regular input-regular :cache-write cache-created :cache-read cached}
             :total-tokens (long (+ input output))}
            (some? reasoning)
            (assoc :output-tokens-details {:reasoning (long reasoning)}))))))

(defn reasoning-effort-configurable?
  "True when a model accepts a CALLER-selected reasoning effort.

   svar decides this, not Vis: `:reasoning-effort?` is stamped on every model
   the router normalizes, from the WIRE that model rides. `:reasoning?` only
   says the model thinks — GitHub Copilot's Gemini/Grok tiers think but are
   `:server-managed` on the OpenAI-compatible wire, and Z.ai GLM thinking is
   binary, so neither accepts a depth and neither may show a depth control.
   Copilot's Claude tier rides the native Anthropic wire and DOES take one."
  [resolved-model]
  (boolean (:reasoning-effort? resolved-model)))

(defn verbosity-configurable?
  "True when a model accepts a caller-selected answer verbosity.

   Also svar's call: `:verbosity-style` is stamped from the wire, so every
   provider on the OpenAI Responses endpoint (Codex AND GitHub Copilot's GPT
   tier) gets the knob and nothing else does. Never test a provider id here."
  [resolved-model]
  (some? (:verbosity-style resolved-model)))

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
  (let
    [msgs (vec (keep #(some-> %
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
  (let
    [thinking
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
  (let
    [routing-trace
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
     (llm-id
       (or (:to-provider fallback-ev) (:llm-provider iteration-result) (:provider selected-model))
       (or (:to-model fallback-ev)
           (:llm-model iteration-result)
           (some-> (:name selected-model)
                   str)))]

    (cond->
      {:selected selected
       :actual actual
       :fallback? (boolean (or (not= selected actual)
                               (some #(not= :llm.routing/provider-retry (:event/type %))
                                     routing-trace)))}
      (seq routing-trace)
      (assoc :trace routing-trace))))

(defn- attach-llm-routing-summary
  [result selected-model iteration-result]
  (let
    [routing
     (llm-routing-summary selected-model iteration-result)

     actual
     (:actual routing)

     selected
     (:selected routing)]

    (cond->
      (assoc result
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
  (let
    [routing
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
    (let
      [iterations
       (vec (keep :reasoning-effort trace))

       missing-count
       (- (count trace) (count iterations))

       fallback-reasons
       (for
         [{:keys [iteration fallback? selected provider model]}
          iterations

          :when fallback?]

         {:type :provider-model-fallback
          :iteration iteration
          :selected selected
          :actual {:provider provider :model model}})

       mismatch-reasons
       (for
         [{:keys [iteration effective provider model]}
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
    (filterv (fn
               [[_
                 {:keys [assistant-message llm-provider llm-model]
                  replay? :preserved-thinking/replay?}]]
               (and (not= false replay?)
                    assistant-message
                    (= target-provider llm-provider)
                    (= target-model llm-model)
                    (assistant-message-compatible-with-replay-target? target assistant-message)))
      (or trailer-iters []))))

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
;; Compaction (`fold_session`) REWRITES pins → the frozen
;; messages change → one deliberate cache bust, paid only under window
;; pressure instead of on every call.

(defn- iter-of-scope
  "Form scope `\"t1/i2/f3\"` → its iteration scope `\"t1/i2\"` (drops the `/fN`).
   nil for non-form scopes (e.g. the synthetic `:summary` keyword)."
  [scope]
  (when (string? scope)
    (let [parts (str/split scope #"/")]
      (when (>= (count parts) 2) (str (nth parts 0) "/" (nth parts 1))))))

(def ^:private SESSION_REBASE_RECLAIMED_TOKENS 200000)

(defn- rebase-session-context!
  "Materialize `cur` as both the same-turn full delta and next-turn standing
   snapshot when a fold crossed the rebase threshold. Returns nil otherwise."
  [standing-ctx-atom session-rebase-atom cur]
  (when (and cur
             (true? (:pending? (some-> session-rebase-atom
                                       deref))))
    (reset! standing-ctx-atom {:block (ctx-renderer/render-ctx-map cur) :baseline cur})
    (reset! session-rebase-atom {:reclaimed-tokens 0 :pending? false})
    (ctx-renderer/render-ctx-delta {} cur)))

(defn- compaction-verbs
  "Build the model-facing compaction verb bound into the sandbox as
   `fold_session`, closing over `ctx-atom`. It records a `:session/summaries`
   intent the wire applies via `apply-summaries`, and RETURNS a visible
   confirmation string (NOT the `\"vis_silent\"` row-suppression sentinel) so the
   action shows in the Python result instead of vanishing.

    The verb takes exactly TWO arguments: a KEY and an optional GIST. The key is
    a STRING in the `ctx-engine/fold-key` grammar — \"t2/i5\" one step, \"t2\" a
    whole turn, \"t2/i1-i56\" a range, \"-t2/i56\"/\"t2/i5-\" an open one, commas
    to union several — disjoint RANGES included (a list of key strings works
    too). Anything that is not a step key, or that resolves to no settled step,
    is refused BY NAME with the
    grammar. The gist is OPTIONAL: pass it to KEEP a one-line takeaway, OMIT it
    to simply DISCARD the step (this replaces the old `session_drop`; a
    gist-less fold collapses the step with no summary line). What it RECORDS is
    string-keyed and persists inside the ctx nippy blob (strings-only DB);
    `ctx-engine/expand-through` owns that recorded shape, and `apply-summaries`
    still renders legacy persisted drops."
  [ctx-atom & [session-rebase-atom]]
  (let
    [normalize-key
     (fn [value]
       ;; Some Python call shapes hand a LIST of keys across as one JSON string;
       ;; decode it so "[\"t1/i2\", \"t1/i3\"]" binds like the list itself.
       (if (and (string? value) (re-matches #"\s*\[.*" value))
         (try (let [parsed (json/read-json value)]
                (if (sequential? parsed) parsed value))
              (catch Throwable _ value))
         value))

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
       (let
         [unbounded?
          (boolean (some (fn [r]
                           (or (contains? r "since")
                               (and (contains? r "from") (not (contains? r "to")))))
                         (ctx-engine/intent-ranges intent)))

          universe
          (some-> ctx-atom
                  deref
                  (get "engine_iter_universe"))]

         (if (and unbounded? (seq universe))
           (first (ctx-engine/expand-through [intent] universe))
           intent)))

     parse-key
     (fn [k]
       (when-let [parsed (ctx-engine/fold-key (normalize-key k))]
         (when-let [error (:error parsed)]
           (throw (ex-info error {:type :vis/fold-session-key :key k})))
         [(freeze (:intent parsed)) (:label parsed)]))

     current-turn
     (fn []
       (let
         [v (some-> ctx-atom
                    deref
                    (get "session_turn"))]
         (cond (integer? v) (long v)
               (string? v) (parse-long (str/trim v))
               :else nil)))

     record!
     (fn [intent]
       (when ctx-atom
         (swap! ctx-atom
           (fn [ctx]
             (let
               [candidates
                ;; Stamp the RECORDING turn onto the intent: a range
                ;; cursor is re-resolved against every LATER turn's live
                ;; universe, so without this stamp a stale/foreign-numbered
                ;; cursor (`{"through" "t113"}` in a session now at t103)
                ;; collapses the whole live turn and the model goes blind.
                ;; `apply-summaries` lets a summary touch LIVE-turn scopes
                ;; only when `at_turn` IS that turn.
                (conj (vec (get ctx "session_summaries"))
                      (cond-> intent
                        (current-turn)
                        (assoc "at_turn" (current-turn))))

                ;; The supersede universe is the live wire PLUS every
                ;; concrete scope the candidates themselves name — so a
                ;; bare `tN` re-fold covers earlier enumerated folds of
                ;; that turn even before (or after) those iterations are
                ;; stamped into `engine_iter_universe`.
                universe
                (into (vec (or (get ctx "engine_iter_universe") []))
                      (comp (mapcat #(get % "scopes")) (filter ctx-engine/scope-key))
                      candidates)

                tagged
                (mapv (fn [idx summary]
                        (assoc summary "__record_idx" idx))
                      (range)
                      candidates)

                winners
                (-> tagged
                    (ctx-engine/expand-through universe)
                    ctx-engine/supersede-summaries)

                kept
                (into #{} (map #(get % "__record_idx")) winners)]

               ;; Persist the original selector shape for stable receipts/tests,
               ;; but discard superseded intents NOW. Rendering no longer has to
               ;; refine an ever-growing fold-of-fold chain on every request.
               (assoc ctx
                 "session_summaries" (into []
                                           (keep-indexed (fn [idx summary]
                                                           (when (contains? kept idx) summary)))
                                           candidates)))))))

     fmt-tok
     (fn [t]
       (let [t (long t)]
         (cond (>= t 1000000) (str (/ (Math/round (/ (double t) 100000.0)) 10.0) "M")
               (>= t 1000) (str (long (Math/round (/ (double t) 1000.0))) "k")
               :else (str t))))

     ;; Human-facing enrichment for the fold card: how much wire THIS fold
     ;; reclaims — in ~tokens (summed from `engine_iter_weights`) AND as a
     ;; fraction of the OPERATING ceiling (`~P% of budget`): `auto_compress_above`
     ;; (the 200k soft compaction guardrail), or the live handled context when a
     ;; bigger task has already floated above it. That figure
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
         (let
           [ctx
            (some-> ctx-atom
                    deref)

            universe
            (get ctx "engine_iter_universe")

            weights
            (get ctx "engine_iter_weights")

            util
            (get ctx "engine_utilization")

            ;; Price the DELTA this intent still removes, not its entire selector.
            ;; A model can fold repeatedly before the next provider projection has
            ;; re-stamped visible weights; the earlier summary already hid its raw
            ;; payload even though its old weight is still present in this ctx.
            expanded
            (ctx-engine/expand-through [base] (or universe []))

            existing
            (ctx-engine/expand-through (get ctx "session_summaries") (or universe []))

            already-scopes
            (into #{} (mapcat #(get % "scopes")) existing)

            scopes
            (set/difference (into #{} (mapcat #(get % "scopes")) expanded) already-scopes)

            ;; Whole-turn intent also removes the turn's Q/A recap from the
            ;; prior-turn context. Apply the same delta rule: a wider re-fold
            ;; must not recharge a recap an earlier whole-turn fold removed.
            already-turns
            (into #{} (mapcat #(get % "turns")) existing)

            new-turns
            (set/difference (into #{} (mapcat #(get % "turns")) expanded) already-turns)

            qa-toks
            (let [tw (get ctx "engine_turn_weights")]
              (reduce + 0 (keep #(get tw %) new-turns)))

            toks
            (+ (long (reduce + 0 (keep #(get weights %) scopes))) (long qa-toks))

            lim
            (get util "model_input_limit")

            ;; Denominator for `% of budget` is the OPERATING ceiling, NOT the
            ;; hard per-call max. `auto_compress_above` (the 200k soft
            ;; guardrail = the budget we actually work within) is what a fold's
            ;; reclaim is relevant against; dividing by the 1M ceiling read every
            ;; fold ~7x smaller than its real weight, so compaction looked like
            ;; noise exactly when it worked. On a BIGGER task the handled context
            ;; (`last_request_tokens`) floats ABOVE that soft guardrail before
            ;; auto-compress fires, so `max` lets the ceiling grow with what
            ;; truly resides — the fraction stays honest instead of pinning to a
            ;; budget already breached. `toks` is residence-bounded (expand-through
            ;; resolves only against the live universe), so it can't exceed the
            ;; grown ceiling: no >100% category error survives, no cap needed.
            ceiling
            (max (long (or (get util "auto_compress_above") 0))
                 (long (or (get util "last_request_tokens") 0)))

            pct
            (when (and (pos? (long toks)) (pos? (long ceiling)))
              (long (Math/round (/ (* 100.0 (double toks)) (double ceiling)))))

            sat
            (get util "saturation")

            ;; A fold can legitimately cover scopes that already left the wire:
            ;; iterations of a turn that COMPLETED normally replay no results at
            ;; all (`off-wire-seed?` above), so only that turn's Q/A recap still
            ;; resides — and ONLY a whole-turn token (`tN`) charges and removes a
            ;; recap. Folding those `tN/iM` ids is an honest no-op, but a card
            ;; listing 44 folded scopes beside a small `saved ~` reads as broken
            ;; accounting (issue #88). So name the weightless share and point at
            ;; the shape that WOULD reclaim it. A scope MISSING from `weights`
            ;; is merely unsent (never priced yet), not off-wire.
            off-wire
            (if (map? weights)
              (filterv #(and (contains? scopes %)
                             (contains? weights %)
                             (zero? (long (get weights %))))
                (or universe []))
              [])

            recap-turns
            (let
              [tw
               (get ctx "engine_turn_weights")

               folded?
               (into already-turns new-turns)]

              (into []
                    (comp (keep #(some-> (second (re-matches #"t(\d+)/i\d+" %))
                                         parse-long))
                          (distinct)
                          (remove folded?)
                          (filter #(pos? (long (get tw % 0)))))
                    off-wire))

            ;; Only advise when a whole-turn fold really would reclaim something:
            ;; recap-less or already-whole-turn-folded scopes need no nudge.
            off-wire-note
            (when (seq recap-turns)
              (str " · "
                   (count off-wire)
                   "/"
                   (count scopes)
                   " scopes already off-wire — fold "
                   (str/join ", " (map #(str "t" %) recap-turns))
                   " to drop their recaps"))

            saved
            (cond (pos? (long toks)) (str " · saved ~" (fmt-tok toks)
                                          " tokens" (when pct (str " · ~" pct "% of budget")))
                  ;; Utilization IS stamped but this fold reclaims no NEW wire — a re-fold
                  ;; of scopes already collapsed on a prior turn, or a fresh scope not yet
                  ;; sent. Either way it freed 0 tokens; stay explicit rather than silently
                  ;; dropping the clause, so the human sees a no-op, not a display bug.
                  (some? util) " · saved ~0 tokens"
                  ;; NO stamped utilization at all: nothing to price, so the card degrades
                  ;; to the bare confirmation and the recorded intent carries no `note`.
                  :else "")

            ;; Even a fold that reclaims no NEW wire (every covered scope
            ;; already collapsed on a prior turn — a whole-session
            ;; `{"through" "tN"}` re-fold) should still tell the human where
            ;; the window stands, so the card is never a bare `folded …`.
            ;; `context <U>%` is the provider's authoritative saturation — but
            ;; it is stamped by the LAST request, so on a card printed the
            ;; instant a fold lands it is strictly the PRE-fold reading:
            ;; nothing has been sent since. Alone next to `saved ~188k` that
            ;; reads as "the fold changed nothing" (the number even looks
            ;; frozen across consecutive folds), which is exactly the display
            ;; bug humans report. So when this fold DOES reclaim wire we render
            ;; the transition `44%→~31%`: the projection is anchored to the
            ;; SAME `last_request_tokens` and only SUBTRACTS the residence this
            ;; fold removes, so unlike issue #27's estimate — which baselined
            ;; on a growing request and therefore climbed after a successful
            ;; fold — it is monotonically ≤ the current reading and can only
            ;; move for the fold's own reason. With nothing reclaimed the
            ;; absolute reading stands unchanged. The arrow is deliberately
            ;; UNSPACED: a receipt's gist is split off at the FIRST " → ", so a
            ;; spaced arrow here would swallow the whole tail into the gist.
            ctx-pct
            (when (and sat (pos? (long sat)))
              (let
                [lrt
                 (long (or (get util "last_request_tokens") 0))

                 lim
                 (long (or lim 0))

                 measurable?
                 (and (pos? lrt) (pos? lim))

                 left
                 (max 0 (- lrt (long toks)))

                 left-pct
                 (when measurable? (long (Math/round (/ (* 100.0 (double left)) (double lim)))))]

                (cond (and measurable? (pos? (long toks))) (str " · context "
                                                                sat
                                                                "%→~"
                                                                left-pct
                                                                "% ("
                                                                (fmt-tok lrt)
                                                                "→"
                                                                (fmt-tok left)
                                                                " tokens)")
                      measurable?
                      (str " · context " sat "% (" (fmt-tok lrt) "/" (fmt-tok lim) " tokens)")
                      :else (str " · context " sat "%"))))]

           {:note (str saved ctx-pct off-wire-note) :reclaimed-tokens toks})
         (catch Throwable _ {:note "" :reclaimed-tokens 0})))]

    {'fold-session
     (fn fold-session [fold-key & [gist]]
       ;; Python kwargs cross as ONE trailing dict (`__vis_direct_kwargs__`):
       ;; `fold_session(k, gist="…")` arrives as (k {"gist" "…"}) and a fully
       ;; keyword call as ({"key" … "gist" …}). Unwrap both so keyword and
       ;; positional calls bind identically; anything else that spreads at the
       ;; top level is not a key, so it travels on to `ctx-engine/fold-key` and
       ;; is refused by name with the grammar instead of folding nothing.
       (let
         [kwargs?
          (fn [m]
            (and (map? m) (or (contains? m "key") (contains? m "gist"))))

          [fold-key gist]
          (cond (kwargs? gist) [(if (contains? gist "key") (get gist "key") fold-key)
                                (get gist "gist")]
                (and (nil? gist) (kwargs? fold-key)) [(or (get fold-key "key")
                                                          (not-empty (dissoc fold-key "gist")))
                                                      (get fold-key "gist")]
                :else [fold-key gist])]

         (if-let [[base label] (parse-key fold-key)]
           (let
             [turn (current-turn)
              uni (some-> ctx-atom
                          deref
                          (get "engine_iter_universe"))
              universe (set uni)
              ;; Resolve the selector against the SETTLED wire. `universe` is every
              ;; iteration already on THIS request's trailer: all prior turns PLUS
              ;; the current turn's COMPLETED iterations. Bare-turn / range / cursor
              ;; selectors are universe-bounded, so they can only ever name settled
              ;; steps; an EXPLICIT `tN/iN` literal is the one shape that survives
              ;; resolution verbatim, so it is the only way to point at the live
              ;; iteration still being emitted (present on no trailer, absent here).
              resolved (first (ctx-engine/expand-through [base] (or uni [])))
              ;; The live iteration is any CURRENT-turn (or future) scope not yet
              ;; settled. Prior turns are always foldable, AND so is every finished
              ;; iteration of the current turn — only the in-flight iteration is
              ;; off-limits, because folding it would collapse steps this very turn
              ;; is still producing.
              live-scopes (when turn
                            (into (sorted-set)
                                  (filter (fn [sc]
                                            (when-let [k (ctx-engine/scope-key sc)]
                                              (and (>= (long (first k)) (long turn))
                                                   (not (contains? universe sc))))))
                                  (get resolved "scopes")))]

             (when-not turn
               (throw (ex-info "fold_session cannot prove the current turn; folding is blocked."
                               {:type :vis/fold-session-turn-unknown})))
             (when (seq live-scopes)
               (throw (ex-info
                        (str "fold_session blocked: " (str/join ", " live-scopes)
                             " name the live iteration you are emitting right now — not yet a "
                             "settled wire step. Fold only COMPLETED steps: every prior turn AND "
                             "the current turn's finished iterations (e.g. \"-tN/iK\" up to the "
                             "last settled iteration). Do not retry THESE scopes this turn.")
                        {:type :vis/fold-session-active-turn
                         :current-turn turn
                         :blocked-scopes live-scopes})))
             ;; A target that resolves to NO settled wire step folds nothing: the ack
             ;; would read `saved ~0 tokens` and the gist would anchor to an id the
             ;; wire never held (a mistyped range, a turn that does not exist yet).
             ;; Refuse it by name instead of recording a silent no-op fold. Whole-turn
             ;; intent (`turns`) at or before the live turn stays legal even with no
             ;; iteration on this trailer — it also removes that turn's Q/A recap,
             ;; which resides outside the iteration universe.
             (let
               [named (get resolved "scopes")
                settled-turns (filter (fn [tn]
                                        (<= (long tn) (long turn)))
                                      (get resolved "turns"))
                ordered (sort-by ctx-engine/scope-key (filter ctx-engine/scope-key uni))]

               (when (and (seq ordered) (empty? settled-turns) (not-any? universe named))
                 (throw (ex-info (str "fold_session: " label
                                      " matches no settled step — this session's wire holds "
                                      (first ordered)
                                      " … " (last ordered)
                                      ". " ctx-engine/fold-key-grammar)
                                 {:type :vis/fold-session-unknown-key
                                  :key label
                                  :scopes (into (sorted-set) named)}))))
             (let
               [g (some-> gist
                          str
                          str/trim
                          not-empty)
                {:keys [note reclaimed-tokens]} (priced base)
                ;; Stamp the ISSUING turn so `previous-turn-context` never lets
                ;; a whole-turn fold recorded DURING turn N erase turn N's own
                ;; Q/A recap next request (the answer is produced after the fold;
                ;; the gist can't summarize it). `turn` is always non-nil here —
                ;; the guard above throws when it can't prove the current turn.
                intent (cond-> base
                         turn
                         (assoc "issued_turn" turn)

                         g
                         (assoc "gist" g)

                         (not (str/blank? note))
                         (assoc "note" note))]

               (record! intent)
               (when (and session-rebase-atom (pos? (long reclaimed-tokens)))
                 (swap! session-rebase-atom
                   (fn [{accumulated :reclaimed-tokens :as state}]
                     (let [total (+ (long (or accumulated 0)) (long reclaimed-tokens))]
                       (assoc state
                         :reclaimed-tokens total
                         :pending? (>= (long total) (long SESSION_REBASE_RECLAIMED_TOKENS)))))))
               (tel/log! {:level :info :id ::fold-session :data {:intent intent}}
                         "model folded scopes")
               (str "folded " label note (when g (str " → " g)))))
           (str "fold_session: nothing to fold — " ctx-engine/fold-key-grammar))))}))


(defn- apply-summaries
  "Wire-only rewrite of `trailer-iters` applying the model's `fold_session`/
   `session_drop` intents at ITERATION granularity. Each summary is
   `{\"scopes\" #{\"tN/iN\" …} \"gist\" <string|nil>}` (drop = nil gist), or a range
   `{\"through\" \"tN/iN\" …}` (several windows ride in `\"ranges\"`) which
   `expand-through` resolves to the trailer's own iteration scopes ≤ the cursor.
   Every iteration whose `tN/iN` scope is
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
    (let
      [iter-scope-of
       (fn [rec]
         (some iter-of-scope (keep :scope (:forms-vec rec))))

       ;; Turn this trailer belongs to. A fold intent is a POINT-IN-TIME
       ;; statement, but its range cursor is RE-RESOLVED on every later request
       ;; — so a cursor that outlives its own turn numbering (a session now at
       ;; t103 still carrying `{"through" "t113"}` from an earlier numbering)
       ;; expands over EVERY live iteration, collapses the whole turn, and the
       ;; model goes blind: it re-issues the identical call every iteration
       ;; because its own results never reach the wire. A summary may collapse
       ;; LIVE-turn iterations only when it belongs to this turn: `"at_turn"`
       ;; (stamped when recorded) decides it; for legacy intents with no stamp,
       ;; a cursor pointing at a turn NEWER than the live one is stale by
       ;; construction. Prior-turn scopes are never affected.
       live-turn
       (some->> (map second trailer-iters)
                (keep iter-scope-of)
                (keep (fn [s]
                        (first (ctx-engine/scope-key s))))
                seq
                (apply max))

       cursor-turn-of
       (fn [s]
         (let
           [turns (into []
                        (comp (mapcat (fn [r]
                                        (vals (select-keys r ["through" "to" "from" "since"]))))
                              (keep (fn [c]
                                      (or (first (ctx-engine/scope-key c))
                                          (ctx-engine/turn-key c)))))
                        (ctx-engine/intent-ranges s))]
           (when-let [turns (seq turns)]
             (apply max turns))))

       stale-for-live-turn?
       (fn [s]
         (when live-turn
           (let
             [at
              (let [v (get s "at_turn")]
                (cond (integer? v) (long v)
                      (string? v) (parse-long (str/trim v))
                      :else nil))

              cursor
              (cursor-turn-of s)]

             (if at
               (< (long at) (long live-turn))
               (boolean (and cursor (> (long cursor) (long live-turn))))))))

       ;; Resolve any `:through` range cursor against THIS trailer's live
       ;; iteration scopes before matching, so a range fold collapses every
       ;; step at or before the cursor; then supersede covered summaries so a
       ;; broader re-fold replaces the finer one (one breadcrumb, not two).
       summaries
       (->> summaries
            (mapv (fn [s]
                    (assoc s "__stale_live" (boolean (stale-for-live-turn? s)))))
            (#(ctx-engine/expand-through % (keep iter-scope-of (map second trailer-iters))))
            (keep (fn [summary]
                    (let
                      [scopes (into #{}
                                    (remove (fn [s]
                                              (and (get summary "__stale_live")
                                                   (= live-turn (first (ctx-engine/scope-key s))))))
                                    (get summary "scopes"))]
                      (when (seq scopes)
                        (-> summary
                            (dissoc "__stale_live")
                            (assoc "scopes" scopes))))))
            (ctx-engine/supersede-summaries))

       summarized
       (into #{} (mapcat #(get % "scopes")) summaries)

       ; set of "tN/iN"
       ;; summary → earliest trailer index whose iteration scope it names
       anchors
       (reduce (fn [m s]
                 (if-let
                   [idx (some (fn [[i [_ rec]]]
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
                       (let
                         [collapsed?
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
    (let
      [msg
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
  "True when `m` is a struct_patch PER-FILE summary — the
   `{\"path\" \"op\" \"changed\" …}` shape `patch-result-file-summary` emits. Used to
   recognise an edit result at the model-wire crossing without touching any other
   tool's `:result`."
  [m]
  (and (map? m) (string? (get m "path")) (string? (get m "op")) (contains? m "changed")))

(defn- strip-echo-diff
  "Drop the `\"diff\"` from one model-wire edit summary. Anchor and structural
   edits apply the exact replacement the model supplied, so the diff is echo-bloat;
   the human card still renders it from the unstripped result."
  [m]
  (dissoc m "diff"))

(defn- strip-echo-diffs
  "Model-wire compaction for a struct_patch `:result`: strip each
   byte-exact file summary's redundant `\"diff\"` (see `strip-echo-diff`). A no-op
   for any non-edit `:result` (only touched when EVERY element is a file summary)."
  [result]
  (cond (and (sequential? result) (seq result) (every? patch-file-summary? result))
        (mapv strip-echo-diff result)
        (patch-file-summary? result) (strip-echo-diff result)
        :else result))


(defn- elide-table-fences
  "Drop the ROWS out of every ````vis-table` fence in model-facing output.

   A CSV/TSV `attach` is DATA for the HUMAN: the fence rides the transcript
   verbatim (see `form/result-display`) and both surfaces paint it as a live
   grid — sortable, pageable, openable as a full-screen sheet. The model needs
   none of that. Sending the payload would re-upload the whole sheet on EVERY
   later request, because tool results replay: one 500-row export then costs
   more context than the rest of the turn, forever, and teaches nothing the
   `[Table: …]` headline does not already say.

   So the wire keeps the headline (name, rows × cols, size, caption) and loses
   the rows; the bytes stay in the DB as a durable attachment, one
   `read_attachment` away. Everything outside a table fence — including a
   `vis-image` fence, which carries only a path — passes through untouched."
  [s]
  (let
    [text
     (str s)

     fence
     "````"

     marker
     (str fence "vis-table")]

    (if-not (str/includes? text marker)
      text
      (str/join "\n"
                (loop
                  [lines
                   (str/split-lines text)

                   out
                   []]

                  (if (empty? lines)
                    out
                    (let [[line & more] lines]
                      (if (= (str/trim line) marker)
                        (let
                          [summary (str/trim (str (first more)))
                           after (drop-while #(not= (str/trim %) fence) (rest more))]

                          (recur (rest after)
                                 (conj out
                                       (str (if (str/blank? summary) "[Table]" summary)
                                            " — rows are NOT in this context: the grid is rendered"
                                            " in the transcript and the bytes are a stored"
                                            " attachment (list_attachments() lists it,"
                                            " read_attachment(id) opens it)."))))
                        (recur more (conj out line))))))))))

(defn- iteration-results-message
  "Render ONE prior tool-call iteration as the `tool_result` user message that
   answers its `tool_use`(s): the content is what the program PRINTED (raw
   stdout), plus errors and any `summarize`/`drop` fold lines. One `tool_result`
   block per `tool_use`, each carrying ITS OWN forms' output (forms are grouped
   by `:svar/tool-call-id`), because one reply may carry several
   `python_execution` calls.
   Falls back to a plain text user message when no tool calls are recorded."
  [iter-record]
  (let
    [;; ONE scope source: the `forms-vec` (each `{:scope :result …}`).
     ;; Falls back to scoped `:blocks` forms.
     forms
     (or (:forms-vec iter-record)
         (mapcat (fn [b]
                   (or (seq (:forms b)) [b]))
                 (:blocks iter-record)))

     ;; `fold_session(...)` / `session_drop(...)` folds (synthetic forms
     ;; apply-summaries injected) render FIRST as one Python comment naming the
     ;; iteration scopes they replaced. `:summary-drop?` picks the label; the
     ;; gist carries the takeaway (fold) or the reason (drop):
     ;;   # ⋯ folded t1/i1-i2 · <gist>
     ;;   # ⋯ dropped t1/i3 · <why>
     summary-lines
     (keep (fn [f]
             (when (:summary? f)
               (let
                 [at
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

     ;; What becomes context: ONLY what the program PRINTED, shown RAW. A bare
     ;; expression's value is NOT auto-echoed — print() to see it.
     ;; Errors always surface (the model must see a failure even if nothing
     ;; printed). A clipped value still lives in the sandbox to re-slice.
     clip-wire
     (fn [s]
       (let
         [s
          (str/trimr (str s))

          n
          (long (count s))]

         (if (> n (long form/MAX_FORM_WIRE_CHARS))
           (str (subs s 0 form/MAX_FORM_WIRE_CHARS)
                "\n# ⋯ output clipped at "
                form/MAX_FORM_WIRE_CHARS
                "/"
                n
                " chars — narrow next time (slice/filter before reading).")
           s)))

     ;; Each form carries exactly ONE success channel (the engine emits one or
     ;; the other, never both): `:result` = the block's returned value when it
     ;; printed nothing (rendered), `:stdout` = what it print()ed. An `:error`
     ;; replaces the return. So surface whichever is present. The ERROR
     ;; envelope is the one internal keyword-keyed shape rendered here —
     ;; `error->display` renders it as clean, LLM-legible text with REAL
     ;; newlines (source excerpt + caret kept readable, never an escaped
     ;; one-line literal).
     form-output
     (fn [f]
       (cond (:summary? f) nil
             (:error f) (error->display (:error f))
             (some? (:result f)) (clip-wire (env/ctx->python-str (strip-echo-diffs (:result f))))
             (not (str/blank? (str (:stdout f)))) (clip-wire (elide-table-fences (:stdout f)))
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
     call-content
     (fn [idx tc]
       (let
         [own
          (cond-> (vec (get forms-by-id (:id tc)))
            (zero? (long idx))
            (into (or orphan-forms [])))

          lines
          (keep form-output own)

          iscope
          (some #(iter-of-scope (:scope %)) own)

          header
          (when (and iscope (seq lines)) (str "# " iscope))

          body-ls
          (concat (when (zero? (long idx)) summary-lines) (when header [header]) lines)

          body
          (when (seq body-ls) (str/join "\n" body-ls))]

         (str/join "\n\n" (remove str/blank? [body (when (zero? (long idx)) ctx-diff)]))))

     ;; Legacy text fallback (a record with NO tool calls): all forms joined.
     fallback-content
     (let
       [lines
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
             (let
               [own
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
                    ;; Empty body: `python_execution` is the only call there is, and its
                    ;; result is what the block PRINTED — so an empty body means it
                    ;; printed nothing.
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
    (let
      [content (vec (remove #(contains? #{"thinking" "redacted_thinking"} (:type %))
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

(defn- wire-image-attachment
  "One stored iteration attachment as the wire will carry it, or nil.

   The whole verdict lives in `attachments/wire-image` (the ONE send-time image
   gate): a generic `attach` artifact (csv/json/pdf/wav/…) is DB- and
   display-only, an `image/svg+xml` figure or a BMP is re-containered to PNG,
   and a payload the decoder cannot turn into pixels — a corrupt raster whose
   header sniffs perfectly — is DROPPED. Dropping matters more here than
   anywhere: handing such a row over as an `image_url` block is a hard 400 that
   repeats on EVERY later turn, because attachments replay, so one bad row
   otherwise kills the whole session for good. Judged on the way out, it costs
   that one figure and the session lives.

   A row whose audience is the HUMAN alone never even reaches the decoder: it
   was recorded for the human, and its bytes are not this model's business."
  [attachment]
  (when-not (attachments/hidden-from-model? attachment)
    (let [wired (attachments/wire-image attachment)]
      (when (:base64 wired) wired))))

(defn- iteration-wired-images
  "Every IMAGE a prior iteration's tool calls produced (matplotlib figures,
   `attach`ed images, plus anything `show_attachment` re-queued),
   each already across the send-time gate (see `wire-image-attachment`) and so
   carrying verified pixels in a container the wire accepts."
  [iter-rec]
  (vec (keep wire-image-attachment
             (concat (:attachments iter-rec) (:reinspect-attachments iter-rec)))))

(def ^:private max-replay-image-bytes
  "Base64 budget for ALL produced images replayed in ONE request.

   Multimodal history is not a reference: every prior figure is re-uploaded, in
   full, on every single request for the rest of the session. Unbudgeted that
   grows without bound until the provider rejects the request outright — and
   then EVERY later turn fails too, including plain text ones, because the same
   oversized history is rebuilt each time. A session must not be able to brick
   itself by plotting one figure too many, so the newest images ride and the
   older ones step off (and are NAMED, see `dropped-images-note`)."
  (* 8 1024 1024))

(def ^:private max-replay-images
  "Hard count ceiling for replayed images, independent of bytes: many small
   figures still cost real vision tokens on every request."
  16)

(defn- replay-image-plan
  "Decide, for ONE request, which produced images still ride and which step off.

   Newest-first greedy fill of [[max-replay-image-bytes]] / [[max-replay-images]]:
   the freshest figure is the one the model is actually reasoning about, and the
   oldest are the ones a summary already covers. The single newest image is
   ALWAYS kept, even alone over budget — a request that shows the model nothing
   is worse than a large one. `:collapsed?` iterations are skipped outright:
   `fold_session` already removed their whole pair.

   Pure. Returns `{pos {:images [...] :dropped [...]}}` keyed by trailer
   position, so each iteration's verdict lands in ITS place in the transcript."
  [entries]
  (first
    (reduce (fn [[plan used-bytes used-count] [pos iter-rec]]
              (let [imgs (when-not (:collapsed? iter-rec) (iteration-wired-images iter-rec))]
                (if (empty? imgs)
                  [plan used-bytes used-count]
                  (let
                    [[kept dropped b c]
                     (reduce (fn [[kept dropped b c] img]
                               (let [sz (long (count (str (:base64 img))))]
                                 (if (or (zero? (long c))
                                         (and (<= (+ (long b) sz) (long max-replay-image-bytes))
                                              (< (long c) (long max-replay-images))))
                                   [(conj kept img) dropped (+ (long b) sz) (inc (long c))]
                                   [kept (conj dropped img) b c])))
                             [[] [] used-bytes used-count]
                             imgs)]
                    [(assoc plan pos {:images kept :dropped dropped}) b c]))))
            [{} 0 0]
            ;; newest first: recency wins the budget, distance pays for it
            (reverse (vec entries)))))

(defn- attachment-recovery-label
  "How the model names a dropped image when asking for it back — its stored
   attachment id when there is one, otherwise the filename it was given."
  [attachment]
  (or (not-empty (str (:id attachment))) (not-empty (str (:filename attachment))) "image"))

(defn- dropped-images-note
  "Plain-text stand-in for images this request could not afford, or nil.

   An image that silently disappears from the history is a model hallucinating
   about pixels it can no longer see. Naming the rows — with the id that brings
   one BACK — turns a byte-budget decision into an ordinary tool call."
  [dropped]
  (when (seq dropped)
    {:role "user"
     :content (str "["
                   (count dropped)
                   " image(s) from this step are stored but NOT in this request"
                   " (image replay budget): " (str/join ", "
                                                        (map attachment-recovery-label dropped))
                   ". Call show_attachment(\"<id>\") to put one back on the next request,"
                   " or read_attachment(\"<id>\") to open its bytes in Python.]")}))

(defn- iteration-image-messages
  "The messages one prior iteration contributes AFTER its `<results>`: a
   `{:role \"user\"}` message of canonical `image_url` blocks for the images that
   fit this request's budget, then a note naming any that did not. Possibly
   empty; always a VECTOR, so callers splice rather than branch.

   Emitted as its OWN message right AFTER the iteration's `<results>` so an
   image never sits between an assistant `tool_use` and its answering
   `tool_result` (which would break tool-call adjacency on the OpenAI chat
   wire)."
  [{:keys [images dropped]}]
  (cond-> []
    (seq images)
    (conj {:role "user" :content (mapv attachment->image-block images)})

    (seq dropped)
    (conj (dropped-images-note dropped))))

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

   Cross-turn seeds (`:preserved-thinking/replay? false`) from completed turns
   stay fully excluded because their answer/recap already carries the outcome.
   Seeds from terminal incomplete turns replay only their settled results as
   plain text (never opaque thinking or orphaned tool_result blocks), preserving
   cancellation/error continuity without duplicating successful-turn evidence.

   Compatible entries route through `preserved-thinking-replay-messages`
   so the oversized-chain telemetry stays.

   Produced IMAGES replay under a per-request budget (`replay-image-plan`):
   images are re-uploaded in full on every request, so an unbounded history
   eventually exceeds the provider's request limit and breaks every later
   turn. Newest wins; older ones are named instead of sent."
  [trailer-iters target]
  (let
    [iters
     (vec (or trailer-iters []))

     compatible
     (into #{} (map first) (compatible-preserved-thinking-trailer-iters iters target))

     ;; Generated figures replay only to a vision-capable target; a
     ;; text-only model gets the fence's summary/ASCII already carried in
     ;; the results text, never image blocks it can't consume.
     vision?
     (target-supports-vision? target)

     ;; ONE newest-first pass over the whole trailer, so the byte budget is
     ;; decided for the request as a whole rather than per iteration.
     image-plan
     (when vision? (replay-image-plan iters))]

    (vec
      (mapcat
        (fn [[pos iter-rec :as entry]]
          (let
            [results
             (iteration-results-message iter-rec)

             ;; Image artifacts this iteration produced, as their OWN
             ;; message(s) appended AFTER the results (keeps tool_use/tool_result
             ;; adjacency intact). Empty for text targets, image-less iters, and
             ;; iterations the budget pushed out (which contribute a note).
             img
             (when vision? (iteration-image-messages (get image-plan pos)))

             +img
             (fn [msgs]
               (into (vec msgs) img))]

            (cond
              ;; Collapse WINS over provenance: a `fold_session`/`session_drop`
              ;; that covered this iteration removes its whole assistant +
              ;; tool_result pair AND its generated image. The figure's vision
              ;; visibility TRACKS its iteration's textual visibility (one
              ;; invariant), so a folded step keeps only its one-line gist
              ;; (plain text) — real compaction, bytes and all. Checked BEFORE
              ;; the cross-turn seed branch so a folded seed also drops its
              ;; image; otherwise a prior-turn figure would be byte-immune to
              ;; compaction and re-billed to the vision model every turn.
              (:collapsed? iter-rec) (if results [results] [])
              ;; Cross-turn seed (NOT collapsed): never replay opaque thinking.
              ;; A terminal incomplete turn has no reliable answer summary, so
              ;; preserve its settled outputs as ordinary text; removing
              ;; :tool-calls prevents orphaned tool_result blocks. Successful
              ;; turns already carry their outcome in the prior-turn recap and
              ;; continue to emit only any previously-unwired image artifacts.
              (false? (:preserved-thinking/replay? iter-rec))
              (if (terminal-incomplete-turn-status? (:cross-turn/turn-status iter-rec))
                (if-let [textual (iteration-results-message (dissoc iter-rec :tool-calls))]
                  (+img [textual])
                  (vec img))
                (vec img))
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
   / stdout — capped at `form/MAX_FORM_WIRE_CHARS` exactly as the real renderer
   clips it, so a giant read can't over-rank itself. Strings count directly; a
   non-string result is serialized the way the wire renders it. Pure."
  [f]
  (let
    [n (long (cond (:summary? f) 0
                   (some? (:error f)) (count (str (:error f)))
                   (some? (:result f)) (let [r (:result f)]
                                         (if (string? r) (count r) (count (env/ctx->python-str r))))
                   :else (count (str (:stdout f)))))]
    (long (min n (long form/MAX_FORM_WIRE_CHARS)))))

;; ── The model-facing surface: ONE tool ───────────────────────────────────────
;; `python_execution` is the only call the provider ever sees. Every capability is
;; a plain Python name bound into GraalPy inside it, so confinement, rendering and
;; docs have exactly one home and the model never routes between surfaces.
;; Replying with plain text and NO tool call ends the turn.

(defn- python-execution-capability-line
  "Describe confirmed sandbox capabilities without duplicating the detailed,
   immutable policy exposed in `session[\"access\"]`."
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
         "FS: see `session[\"access\"][\"filesystem\"]` for effective roots and modes; prefer `ls`/`grep` over shell."
         "FS: unavailable.")

       net-part
       (cond (not net-on?) "Network: off."
             allowed (str "Network: on, reachable hosts: " (str/join ", " allowed) ".")
             star? "Network: on (any host except blocked defaults)."
             :else "Network: on; see `session[\"access\"][\"network\"]`.")]

      (str fs-part " " net-part))))


(defn- python-execution-tool
  "The engine-level `python_execution` tool schema — the ONLY tool the provider
   is given. Batched, transformed, filtered, chained and structural workflows all
   run here, so intermediate data never lands in context. The capability line is
   built from `caps` so fs/network claims match what the sandbox can actually do."
  [caps]
  {:name "python_execution"
   :description
   (str
     "Run Python in the session sandbox — the only call. Batch, filter and chain work here, then print "
     "only what the answer needs: `await gather(...)` runs independent calls together. State persists; "
     "project packages need a project REPL. "
     "Only `print` returns; bare expressions drop and errors surface. Every capability is a plain Python "
     "name here, so a result is an ordinary value you keep in a variable — but a value you never printed "
     "is gone from the transcript once the block ends. A shell is WATCHED here: `sh = await shell(...)`, then a BOUNDED "
     "loop that calls `sh.logs()` on the handle it got back and breaks on what it read (an error line, "
     "a parsed port); `sh.wait(secs)` is that loop already written — no tool "
     "waits for you. Close what you open (`with open(...)`): a dropped file handle is "
     "NOT auto-closed here, so the sandbox reclaims leaked descriptors and refuses more than 512 held at "
     "once (`VIS_PY_MAX_OPEN_FILES`) — leaked descriptors stop the process spawning `shell` children."
     (when-let [cap (python-execution-capability-line caps)]
       (str " " cap)))
   :result
   "Exactly captured `print(...)` output (empty string when none); evaluation failures are failed tool results, not result objects."
   :schema {:type "object"
            :properties {"code" {:type "string" :description "Python source."}}
            :required ["code"]
            :additionalProperties false}})


(defn- model-facing-tools
  "The ONE provider-visible tool. `python_execution` IS the model-facing surface:
   every other capability is already a bare Python name inside that sandbox, so a
   second JSON schema advertises a door the model can open anyway — and charges
   for it on every request. Discovery of the rest is pulled, not pushed:
   `apropos(text)` searches and `doc(name)` retrieves, both from inside a block.

   The raw-result contract is folded into the description here, so the one tool
   cannot reach a provider without saying what it hands back. Nothing is
   advertised `strict`: a per-wire grammar opt-in has no place on a surface that
   must reach every provider."
  [caps]
  (let [{:keys [description result] :as tool} (python-execution-tool caps)]
    [(-> tool
         (assoc :description (str description "\n\nRaw result: " result))
         (dissoc :result))]))


(def ^:private tool-protocol-leak-re
  "A LONE closing tool-call tag. `invoke`/`parameter` are the provider's own
   tool-call encoding, never data, and a mangled close tag (`</antmlutparameter>`
   instead of the real one) makes the API hand the tag itself over as the
   parameter's VALUE. The tag name is matched loosely on both sides so the
   mangling — the whole reason this arrives at all — is still recognized."
  #"\s*</[A-Za-z0-9_:.-]*(?:invoke|parameter|function_calls|function_results)>\s*")

(defn- tool-protocol-leak?
  "True when `v` is a string that is NOTHING BUT a tool-call closing tag, so it
   is transport wreckage rather than an argument. A value that merely MENTIONS
   the tag (a grep query, a paragraph about the protocol) is left alone."
  [v]
  (and (string? v) (some? (re-matches tool-protocol-leak-re v))))

(defn- report-tool-protocol-leak!
  "Record the wreckage `normalize-tool-input` is about to drop; always nil.

   Dropping it is the right repair, but the drop is also what ERASES the fault:
   the second instance of it was found solely because the mangled tag had been
   persisted in `session_turn_iteration.tool_calls`. Nothing corrupt reaches
   engine data any more, so this warning is the only trace left of a provider
   that mangled its own tool-call encoding."
  [id data]
  (tel/log! {:level :warn :id id :data data} "Dropped provider tool-call transport wreckage")
  nil)

(defn- normalize-tool-input
  "MODEL-DRIFT + EXTENSION-EDN adapter for ONE tool call's arguments.

   NOT a svar workaround: svar decodes tool arguments strings-only at the wire
   edge — it parses a provider body with the tool-argument subtrees left
   UNINTERNED (`RAW_TOOL_ARG_KEYS` / `keywordize-response`) — so nothing that
   arrives from a provider is ever a keyword. What this pass owns is the three
   things svar rightly refuses to touch:

     1. MODEL DRIFT — a model that literally writes `\":path\"` as a JSON key.
        `env/normalize-dict-key` strips that leading colon so positional
        extraction still finds the key and the call just works.
     2. EXTENSION-AUTHORED EDN — `:call` shapes are Clojure data written by
        humans, so they legitimately carry keywords.
     3. TRANSPORT WRECKAGE — the model's tool-call close tag arrives mangled
        and the provider hands the TAG over as an argument value
        (`apropos(\"</antmlutparameter>\\n\")`, `grep` with `\"\\n</invoke>\\n\"`
        under an entity-escaped key). Such an entry is DROPPED
        (`tool-protocol-leak?`), which is what the model meant: an optional
        argument disappears and `apropos()` runs, a required one is missing and
        the tool says so, instead of the call silently answering a question
        nobody asked. The same wreckage can miss the object shape entirely:
        svar's decode is strict and FAITHFUL, so an `arguments` payload that
        parsed to a JSON string/array/number comes back as a String, a vector,
        a number. Engine data is a string-keyed MAP, so a non-map lands as `{}`
        rather than travelling on to call synthesis, receipts and persistence.

   DEEP: keys are normalized at EVERY depth, not just the top level. Tools like
   `struct_patch` carry NESTED dicts (`edits [{\":target\" …}]`); a shallow pass
   left the drift colon on those nested keys, so the synthesized Python call
   leaked `struct_patch({\"edits\": [{\":target\": …}]})`.

   VALUES TOO, at every depth: a keyword/symbol VALUE (`{\"op\" :delete}` out of
   extension EDN) is stringified HERE — `:delete` -> `\"delete\"`, `:a/b` ->
   `\"a/b\"` — because the sandbox boundary refuses a keyword value and
   throws `boundary-violation!`, which killed the whole tool call instead of
   running it. Everything else (edit `code` text, paths, numbers, booleans)
   passes through verbatim."
  [input]
  (letfn [(nk [k] (env/normalize-dict-key (if (keyword? k) (subs (str k) 1) (str k))))
          (nv [x] (if (keyword? x) (subs (str x) 1) (str x)))
          (walk [x]
            (cond (map? x) (into {}
                                 (keep (fn [[k v]]
                                         (if (tool-protocol-leak? v)
                                           (report-tool-protocol-leak! ::tool-protocol-leak
                                                                       {:argument (str k) :value v})
                                           [(nk k) (walk v)])))
                                 x)
                  (or (vector? x) (seq? x) (set? x)) (mapv walk x)
                  (or (keyword? x) (symbol? x)) (nv x)
                  :else x))]
    (let [normalized (walk (or input {}))]
      (if (map? normalized)
        normalized
        (do (report-tool-protocol-leak! ::tool-input-not-an-object
                                        {:type (str (type normalized)) :value (pr-str normalized)})
            {})))))

(defn- normalize-tool-calls
  "THE DOOR: every tool call svar returns enters the engine HERE.

   svar hands model-authored arguments over strings-only — its response parse
   leaves tool-argument subtrees UNINTERNED, so a provider can no longer deliver
   `:path` — which makes this pass exactly one thing: the single place MODEL
   DRIFT is repaired (a literal `\":path\"` key, a stray keyword value out of an
   extension's EDN), at the point where `ask-code!`'s result becomes engine
   data.

   Vis is strings-only end to end — the sandbox, the synthesized Python,
   persistence and the wire all speak snake_case strings — so the whole
   tool-call vector is normalized ONCE. Everything downstream — call synthesis,
   replay elision, receipts and iteration records — reads plain
   string keys and must NOT re-check a keyword variant."
  [tool-calls]
  (mapv #(update % :input normalize-tool-input) (vec tool-calls)))


;; Prompt-cache breakpoints (Anthropic `cache_control`; OpenAI-style strips the
;; marker and uses implicit prefix caching). TWO breakpoints, pi/maki-style:
;;   1. the last SYSTEM message — the FROZEN `session={…}` prefix (long-lived,
;;      stable across turns thanks to :standing-ctx-atom), and
;;   2. the LAST message overall — a MOVING recency breakpoint that caches the
;;      append-only transcript up to here (grows each iteration).
;; Manual placement makes svar skip its auto-cache-last-system-block (so we own
;; both slots); svar honours ≤4 breakpoints per call.
(defn- tag-block-cached
  "Mark the LAST content block of message `m` with `:svar/cache true`. Coerces a
   bare-string `:content` into a text block first; leaves other shapes untouched."
  [m]
  (let [content (:content m)]
    (cond (string? content) (assoc m :content [{:type "text" :text content :svar/cache true}])
          (and (vector? content) (seq content))
          (let
            [i (dec (count content))
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
      (let
        [last-sys (last (keep-indexed (fn [i m]
                                        (when (= "system" (:role m)) i))
                                      messages))
         messages (cond-> messages
                    last-sys
                    (update last-sys tag-block-cached))]

        (update messages (dec (count messages)) tag-block-cached)))))

(defn- prose-beyond-code
  "The assistant `prose` (a model `:content` string streamed ALONGSIDE a tool
   call) is worth showing ONLY when it carries commentary BEYOND the code it's
   about to run. Models frequently restate the exact `python_execution` code in their
   message — as a ```fenced``` block or verbatim — which then renders as a dim
   DUPLICATE of the real code block. So strip any fenced code from the prose and
   compare what's left (and the whole prose, de-whitespaced) against the
   concatenated tool-call code; return the prose when it still says something,
   else nil. `tool-calls` are the model's `python_execution` calls; their `:input` carries
   `code`."
  [prose tool-calls]
  (when-let
    [p (some-> prose
               str
               str/trim
               not-empty)]
    (let
      [code (->> tool-calls
                 (map (fn [tc]
                        (get (:input tc) "code" "")))
                 (str/join "\n"))
       squash #(str/replace (str %) #"\s+" "")
       fenced-stripped (-> p
                           (str/replace #"(?s)```.*?```" "")
                           str/trim)]

      (when-not (or (str/blank? fenced-stripped)  ;; prose was ONLY fenced code
                    (= (squash p) (squash code))) ;; prose IS the code verbatim
        p))))


(defn- provider-call-reason
  "WHY this provider request exists. The FIRST call of a turn answers the human's
   own submit; every later one is the agent loop continuing by itself on the tool
   results it just produced. A retry is never a `:provider-call` — it carries its
   own `:provider-retry-reset` marker — so those are the only two reasons."
  [^long iteration-position]
  (if (<= iteration-position 1) :user-submit :tool-result))

(defn- provider-call-chunk
  "The lifecycle marker that opens ONE provider call.

   It names the provider and the model the request is dispatched to: when the
   stream then goes silent, that marker is the only thing the gateway watchdog
   has left to attribute the stall to, and a failure card that cannot say WHICH
   provider went quiet tells the human nothing.

   It also names WHY the request is being made (`:reason`): a tool-result
   continuation that the loop decided on its own must not look like the human
   pressing enter again."
  [iteration-position resolved-model started-at-ms]
  {:phase :provider-call
   :iteration iteration-position
   :reason (provider-call-reason iteration-position)
   :started-at-ms started-at-ms
   :provider (some-> (:provider resolved-model)
                     name)
   :model (some-> (:name resolved-model)
                  str)})

(def ^:private FD_RECLAIM_THRESHOLD
  "Reclaim leaked file descriptors once open FDs cross this fraction of the
   process limit. Below it, the per-iteration check is just a cheap OS-bean read;
   at/above it, a GC pass closes file objects the sandbox dropped WITHOUT
   `with open(...)` — GraalPy does not refcount-close a dropped file object, so
   an os.walk that reads a large tree can pile up thousands of descriptors before
   GC catches up, starving the JVM's SHARED descriptor table (auth sockets, git,
   cancel) and wedging the session (vis session 7d3f9026). Proactive reclaim
   keeps a leaky sandbox block from ever reaching EMFILE."
  0.6)

(defn- fd-usage
  "[open-count max-count] file descriptors for THIS process via the JVM OS bean,
   or nil when the platform bean is not a `UnixOperatingSystemMXBean` (e.g.
   Windows) or reports no limit. Cheap — a couple of native counter reads."
  []
  (let [bean (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (when (instance? com.sun.management.UnixOperatingSystemMXBean bean)
      (let
        [b ^com.sun.management.UnixOperatingSystemMXBean bean
         m (.getMaxFileDescriptorCount b)]

        (when (pos? m) [(.getOpenFileDescriptorCount b) m])))))

(defn reclaim-fds-under-pressure!
  "When open FDs exceed `FD_RECLAIM_THRESHOLD` of the process limit, force a GC +
   finalization pass so descriptors held by unreachable (leaked) file objects are
   CLOSED — the sandbox's cheapest safety net against open-without-close Python
   (mirrors the on-demand reclaim `shell`'s spawn path already does, but PROACTIVE
   at the iteration boundary so the control plane never starves). No-op — one bean
   read — when there is headroom. Returns the number of descriptors freed when it
   acted, else nil."
  []
  (when-let [[open lim] (fd-usage)]
    (when (> (/ (double open) (double lim)) (double FD_RECLAIM_THRESHOLD))
      (System/gc)
      (System/runFinalization)
      ;; The Cleaner/finalizer threads need a beat to actually release the FDs
      ;; after GC marks the file objects unreachable (same 100-150ms `shell`
      ;; uses). Only paid under pressure, never on the common path.
      (Thread/sleep 120)
      (let
        [after (or (first (fd-usage)) open)
         freed (max 0 (- (long open) (long after)))]

        (tel/log! {:level :warn
                   :id ::fd-reclaim
                   :data {:before open :after after :limit lim :freed freed}
                   :msg "file-descriptor pressure — GC reclaimed leaked handles"})
        freed))))

(defn run-iteration
  "Runs a single RLM iteration: ask! -> check final -> execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc."
  [environment messages &
   [{:keys [routing iteration reasoning-level reasoning-effort resolved-model on-chunk extra-body
            llm-headers active-extensions answer-validation-context]}]]
  (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :run-iteration})]
    (let
      [iteration-position (inc (long (or iteration 0)))
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
       reset-stream-state! (fn []
                             (vreset! reasoning-len-volatile 0)
                             (vreset! content-len-volatile 0))
       streaming-fn
       (when on-chunk
         (fn [{:keys [reasoning content done?] :as chunk}]
           (cond (:event/type chunk)
                 (on-chunk {:phase :provider-fallback :iteration iteration-position :event chunk})
                 :else (do (when (or (some? reasoning) done?)
                             (let
                               [;; The provider's trailing `…` is the summary-elision
                                ;; MARKER, not text the model wrote; strip it at the
                                ;; producer so the live stream, the CLI trace rail and
                                ;; every gateway consumer agree on one clean string.
                                thinking (some-> reasoning
                                                 str
                                                 strutil/strip-elision-marker)
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
                             (let
                               [content-s (some-> content
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
                           ;; Native tool input previews are intentionally not
                           ;; surfaced: no teal "native_call" preview card.
                           nil))))
       copilot-initiator (copilot-initiator-for-iteration iteration)
       effective-llm-headers
       (not-empty (merge (copilot-llm-headers resolved-model copilot-initiator) llm-headers))
       provider-started-at-ms (System/currentTimeMillis)
       _ (when on-chunk
           (on-chunk
             (provider-call-chunk iteration-position resolved-model provider-started-at-ms)))
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
       ;; Automatic refusal fallback: when the model's Anthropic safety
       ;; classifier declines (stop_reason refusal), svar switches to the
       ;; fallback model (Opus 5 → Opus 4.8). Each switch is collected here and
       ;; surfaced on the routing trace, same as an empty-reply resend.
       refusal-fallback-events (atom [])
       refusal-fallbacks (refusal-fallbacks-for resolved-model)
       provider-tools (model-facing-tools (:sandbox-caps environment))
       ask-opts
       (rt/with-default-ask-code-idle-timeout
         (cond->
           {;; ONE tool on the wire: the model takes every action
            ;; by calling `python_execution` with a Python
            ;; program; a reply with NO tool call is the
            ;; final answer (its text). svar returns
            ;; {:stop-reason :tool-calls|:end :tool-calls :content
            ;; :assistant-message}.
            :tools provider-tools
            :tool-choice :auto
            ;; two prompt-cache breakpoints: frozen system prefix
            ;; + moving recency (transcript). See apply-cache-breakpoints.
            :messages (apply-cache-breakpoints messages)
            :routing sticky-routing
            :check-context? true
            :preserved-thinking? true
            :on-empty-reply-resend (fn [{:keys [attempt max-resends delay-ms]}]
                                     ;; LIVE, not post-hoc: emit the retry chunk the
                                     ;; instant svar re-sends, so the channel paints
                                     ;; "resend 1/3" instead of silence and the gateway
                                     ;; persists the recap even if the human cancels
                                     ;; mid-ladder. The same event still rides the
                                     ;; routing trace the finished call returns.
                                     (let
                                       [chunk (empty-reply-resend-chunk iteration-position
                                                                        resolved-model
                                                                        {:attempt attempt
                                                                         :max-resends max-resends
                                                                         :delay-ms delay-ms})]
                                       (swap! empty-reply-resend-events conj (:event chunk))
                                       (reset-stream-state!)
                                       (when on-chunk (on-chunk chunk))))}
           session-cache-key
           (assoc :cache-key session-cache-key)

           refusal-fallbacks
           (assoc :refusal-fallbacks
             refusal-fallbacks :on-refusal-fallback
             (fn [ev]
               ;; LIVE: the instant svar switches models, paint the switch
               ;; and persist the recap — otherwise the UI shows a silent
               ;; gap while the fallback model streams its answer.
               (let [chunk (refusal-fallback-chunk iteration-position ev)]
                 (swap! refusal-fallback-events conj (:event chunk))
                 (reset-stream-state!)
                 (when on-chunk (on-chunk chunk)))))

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
       ask-result-raw (binding
                        [svar-llm/*log-context* (assoc svar-llm/*log-context*
                                                  :session-turn-id (:environment-id environment)
                                                  :iteration iteration-position)]
                        ;; Svar is the single owner of provider classification and retries.
                        ;; Once this call returns or throws, Vis must not issue it again.
                        (svar/ask-code! (:router environment) ask-opts))
       ask-result (prepend-routing-trace ask-result-raw
                                         (into (vec @empty-reply-resend-events)
                                               @refusal-fallback-events))
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
       ;; The model either CALLS `python_execution`
       ;; (`:stop-reason :tool-calls`) or, with NO tool call
       ;; (`:stop-reason :end`), returns its final answer as `:content`.
       ;; An answer reply finalizes the turn directly (finalize-answer!
       ;; records it; the FINAL path below stores/renders it) and runs no
       ;; code. A tool-call reply becomes the executable blocks: one
       ;; `python_execution` call → one block, carrying the tool_use `:id` so the
       ;; driver can pair its result into a `tool_result` message.
       tool-calls (normalize-tool-calls (:tool-calls ask-result))
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
       ;; say — otherwise it's a dim duplicate of the python_execution block.
       assistant-prose (when (seq tool-calls) (prose-beyond-code prose-md tool-calls))
       _ (when answer-md (finalize-answer! environment answer-md))
       _ (when (and assistant-prose on-chunk)
           (on-chunk {:phase :assistant-prose :iteration iteration-position :text assistant-prose}))
       ;; The ONE advertised tool is `python_execution`, so a tool call IS a
       ;; Python block: its `code` argument is the program, verbatim.
       blocks (if answer-md
                []
                (mapv (fn [tc]
                        {:lang "python"
                         :source (or (get (:input tc) "code") "")
                         :svar/tool-call-id (:id tc)
                         :vis/tool-name (:name tc)})
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
       executed
       (mapv
         (fn
           [idx
            {:keys [expr render-segments]
             :vis/keys [preflight-error]
             form-repaired? :repaired?
             :as entry}]
           (log-stage! :code-exec iteration {:idx (inc (long idx)) :total total-blocks :code expr})
           (when (and on-chunk (not suppress-form-start?))
             (on-chunk {:phase :form-start
                        :iteration iteration-position
                        :position idx
                        :count total-blocks
                        :vis/tool-name (:vis/tool-name entry)
                        :scope (form-scope idx)
                        :code expr
                        :render-segments render-segments
                        :started-at-ms (System/currentTimeMillis)}))
           ;; Stamp form-idx BEFORE eval so the
           ;; executing block's position is recorded
           ;; on the turn-state atom.
           (swap! turn-state-atom assoc :form-idx idx)
           (let
             [scope (form-scope idx)
              raw-result (cond preflight-error {:result nil
                                                :error (op-error preflight-error
                                                                 {:code expr :phase :vis/preflight})
                                                :duration-ms 0
                                                :op :vis/guard}
                               :else
                               (if-let
                                 [err (literal-code-block-error (:python-context environment) expr)]
                                 {:result nil
                                  :error (op-error err {:code expr :phase :vis/guard})
                                  :duration-ms 0
                                  :op :vis/guard}
                                 (let
                                   [tool-event-fn (when (and on-chunk (not suppress-form-start?))
                                                    (fn [tool-event]
                                                      (on-chunk {:phase :tool-start
                                                                 :iteration iteration-position
                                                                 :position idx
                                                                 :count total-blocks
                                                                 :scope scope
                                                                 :code expr
                                                                 :render-segments render-segments
                                                                 :tool-event tool-event})))
                                    r (if tool-event-fn
                                        (execute-code environment expr :tool-event-fn tool-event-fn)
                                        (execute-code environment expr))]

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
              ;; card, pretty result, or stdout) — a PURE projection of the
              ;; form's own fields, so it rides the LIVE stream but is never
              ;; persisted: a restored trace re-derives the same card through
              ;; `form/result-render` instead of keeping a second copy of what
              ;; `:result`/`:stdout` already say. `:summary` is the op-card
              ;; HEADLINE; `:body` (→ `:result-render`) the detail.
              displayable (assoc result* :src expr)
              result-card (form/result-display displayable)
              result-render (form/result-render displayable)
              result-summary (:summary result-card)]

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
                  ;; Native tool identity for the result badge.
                  :vis/tool-name (:vis/tool-name entry)
                  ;; Raw stdout kept for model-context consumers.
                  :stdout (:stdout result*)
                  :error (:error result*)
                  :envelope (:envelope result*)
                  :role (:role result*)
                  :timeout? (boolean (:timeout? result*))
                  :repaired? (boolean (:repaired? result*))}))
             {:block expr
              :result result*
              :result-summary result-summary
              :render-segments render-segments
              :svar/tool-call-id (:svar/tool-call-id entry)
              :vis/tool-name (:vis/tool-name entry)}))
         (range)
         code-entries)
       form-sources (mapv :block executed)
       form-results (mapv :result executed)
       form-segments (mapv :render-segments executed)
       form-tool-ids (mapv :svar/tool-call-id executed)
       form-tool-names (mapv :vis/tool-name executed)
       form-result-summaries (mapv :result-summary executed)
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
         (mapv (fn [idx code result segments tool-call-id tool-name result-summary]
                 (cond->
                   {:id idx
                    :code code
                    :result (:result result)
                    ;; What the block PRINTED — the python_execution
                    ;; result (a native call's result is :result).
                    ;; One block = one tool call, so this is the
                    ;; call's whole stdout (no per-form split).
                    :stdout (:stdout result)
                    ;; Artifacts the block PRODUCED (matplotlib
                    ;; show/savefig or an `attach` call),
                    ;; captured at the SOURCE into the sandbox sink —
                    ;; carried down so the DB attachment OWNS the bytes.
                    :attachments (:attachments result)
                    ;; Reinspection is ephemeral: it reaches the next request but
                    ;; is never written as a duplicate iteration artifact.
                    :reinspect-attachments (:reinspect-attachments result)
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

                   result-summary
                   (assoc :result-summary result-summary)

                   (get preflight-by-idx idx)
                   (assoc :vis/preflight? true)))
               (range)
               form-sources
               form-results
               form-segments
               form-tool-ids
               form-tool-names
               form-result-summaries))]

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
        (let
          [validation-error (final-answer-gate-error environment
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

;; Multi-iteration turn engine

;; Core helpers

(defn- stream-output-overflow?
  [err]
  (let [data (:data err)]
    (and (= :svar.core/stream-incomplete (:type data))
         (= "max_output_tokens" (str (:reason data))))))
(def ^:private MAX_AUTH_REFRESH_RETRIES
  "Max transparent auth-401 retries per iteration. Attempt 0 forces ONE OAuth
   refresh-token exchange (the stored access token was invalidated server-side,
   e.g. refresh-token rotation) + router rebuild and re-sends. If that fresh
   token 401s AGAIN it is almost always PROPAGATION LAG at the provider edge,
   not a dead credential — so the remaining attempts back off and retry the
   SAME token (no re-mint) to let it settle, per [[auth-propagation-backoff-ms]]."
  4)
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
  (let
    [base
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

    (cond->
      {:phase :llm-provider/generate
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

;; Router lifecycle + model helpers (turn single-file API)

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
  "Build-time sibling of `try-refresh-provider-token!`.

   When Svar classifies a router-build failure as authentication and the
   provider exposes `:provider/refresh-token-fn`, force one credential mutation
   so the caller can make a new build request. Svar remains the sole failure
   classifier; this function only refreshes a credential Svar cannot mint.

   Unlike the mid-turn path, it must not rebuild the router recursively."
  [pid ^Throwable t]
  (let
    [provider
     (registry/provider-by-id pid)

     f
     (:provider/refresh-token-fn provider)]

    (boolean
      (when (and f (= :auth (:category (perr/svar-classification t))) (auth-refresh-allowed? pid))
        (let [rejected (config/baked-token pid)]
          (try (try (f rejected) (catch clojure.lang.ArityException _ (f)))
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

(defn- boot-refresh-credential-command!
  "Command-backed sibling of `boot-refresh-provider-token!`.

   Svar decides whether the failure is authentication. This function only
   invalidates a short-lived command credential so a subsequent build request
   can carry a new token; `auth-refresh-allowed?` bounds that mutation."
  [p ^Throwable t]
  (let [pid (:id p)]
    (boolean (when (and (:api-key-command p)
                        (= :auth (:category (perr/svar-classification t)))
                        (auth-refresh-allowed? pid))
               (config/invalidate-credential-command! pid)
               (tel/log!
                 {:level :warn :id ::boot-credential-command-refreshed :data {:provider pid}}
                 (str "Provider build hit auth error — re-running credential command for "
                      pid
                      "; retrying build"))
               true))))

(defn- runtime-router-providers
  "Resolve durable provider config into the svar runtime shape.

   On-disk config intentionally omits ephemeral credentials for OAuth-backed
   providers such as OpenAI Codex. Resolve those fields immediately before
   constructing a router so each provider can refresh tokens and attach any
   provider-specific headers.

   Each provider may also enrich its own models via `:provider/enrich-models-fn`
   (e.g. LM Studio resolving real context windows)."
  [config]
  (let
    [ropts
     (config/router-opts config)

     ;; Route authenticated-but-unconfigured OAuth providers too, so a
     ;; provider chosen in a channel model picker (`picker-fleet`) ACTUALLY
     ;; routes without first being persisted into `:providers`. `->svar-provider`
     ;; resolves their token from the registry `:provider/get-token-fn`, so they
     ;; need no on-disk api-key. Config entries win on id; the rest are appended.
     configured
     (:providers config)

     configured-ids
     (into #{} (map :id) configured)

     provider-fleet
     (into (vec configured)
           (remove #(contains? configured-ids (:id %)))
           (try (providers/authenticated-preset-providers) (catch Throwable _ nil)))]

    ;; RESILIENT build: `->svar-provider` may eagerly fetch an OAuth token
    ;; (Copilot/Codex), and that can fail (expired token, GitHub 403
    ;; "not accessible by integration", network). A single failing provider
    ;; must NOT abort the whole router build and crash startup — skip it with a
    ;; warning and keep every provider that DID resolve. Falling through with
    ;; the others (or none) lets the app start and surface a fixable message.
    (->> provider-fleet
         ;; A provider whose `${NAME}` never resolved CANNOT authenticate: its
         ;; `:api-key` is still the literal reference. Drop it here rather than
         ;; letting it 401 on a real turn, and — crucially — keep every other
         ;; provider. One unset var must never cost you a session running on a
         ;; healthy provider. If it was the ONLY provider, svar raises
         ;; `:svar/no-providers`, which already routes to the provider manager
         ;; (see `config/no-provider-ex`), and that dialog now names
         ;; the exact variable.
         (remove (fn [p]
                   (when-let [{:keys [reason env-vars]} (config/provider-credential-gap p)]
                     (tel/log! {:level :warn
                                :id ::provider-env-unresolved-skipped
                                :data {:provider (:id p) :env-vars env-vars}
                                :msg reason})
                     true)))
         (keep
           (fn [p]
             (letfn [(build [] (enrich-provider-models (config/->svar-provider p) ropts))]
               (try (build)
                    (catch Throwable t
                      ;; Before dropping an auth-failed provider, try to HEAL it:
                      ;; a server-rotated OAuth token is auth-shaped and force-
                      ;; refreshable in place — refresh once, retry the build once.
                      ;; Anything else (or a failed retry) falls through to skip.
                      (or (try (when (or (boot-refresh-provider-token! (:id p) t)
                                         (boot-refresh-credential-command! p t))
                                 (build))
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

(defn- config-root-pair
  "Resolve ONE `<role>` provider/model tag to `[provider-keyword wanted-model]`,
   or nil when the role names no provider.

   The model key accepts the same `provider/model` form as `--model`; its
   provider part wins over the sibling provider key, but ONLY when the fleet
   really has that provider — model ids CONTAIN slashes (openrouter serves
   `z-ai/glm-4.6v`), and splitting those seated the wrong root, so a default the
   user picked never took effect. Resolution mirrors
   `providers/default-selection` / `providers/fallback-selection` so the picker
   and the router can never disagree: once the provider resolves it is promoted
   even when the model name does not match its catalog, in which case its first
   model becomes that root."
  [config {:keys [provider-key model-key implicit-provider]}]
  (let
    [requested-model
     (some-> (get config model-key)
             str
             str/trim
             not-empty)

     provider-by-id
     (fn [id]
       (some #(when (= id (:id %)) %) (:providers config)))

     tagged-value
     (or (get config provider-key) implicit-provider)

     tagged
     (cond (keyword? tagged-value) tagged-value
           (string? tagged-value) (keyword tagged-value))

     whole-model?
     (boolean (some #(= requested-model (config/model-name %)) (:models (provider-by-id tagged))))

     slash
     (when (and requested-model (not whole-model?))
       (when-let [idx (str/index-of requested-model "/")]
         (let
           [idx (long idx)
            prefix (keyword (subs requested-model 0 (long idx)))]

           (when (provider-by-id prefix) [prefix (not-empty (subs requested-model (inc idx)))]))))

     provider
     (or (first slash) tagged)

     wanted-model
     (or (if slash (second slash) requested-model)
         ;; No explicit model tag: the configured provider's FIRST model is
         ;; the selection, exactly as when no default was ever picked.
         (some-> (provider-by-id provider)
                 :models
                 first
                 config/model-name))]

    (when provider [provider wanted-model])))

(defn- seat-root
  "Return `provider-entries` with `provider-id` moved to the FRONT and
   `wanted-model` promoted to its `:root`. nil when that provider — or any model
   for it — is absent, so the caller can leave the fleet untouched."
  [provider-entries provider-id wanted-model]
  (when-let [selected (some #(when (= provider-id (:id %)) %) provider-entries)]
    (when-let
      [hit (or (some #(when (= wanted-model (:name %)) %) (:models selected))
               (first (:models selected)))]
      (into [(assoc selected
               :models (into [hit] (remove #(= (:name hit) (:name %))) (:models selected))
               :root (:name hit))]
            (remove #(= provider-id (:id %)))
            provider-entries))))


(defn- honor-config-roots!
  "Make the explicit provider/model tags the router's effective roots: the
   PRIMARY pair first, the FALLBACK pair — always a DIFFERENT provider — second,
   every other provider left in its configured order behind them.

   Provider/model vector order is otherwise left alone and has no configuration
   meaning. A config that tags nothing keeps its first provider/first model
   selection, and an untagged, unknown or
   primary-colliding fallback leaves the tail exactly as it was."
  [router config]
  (let
    [primary
     (config-root-pair config
                       {:provider-key :default-provider
                        :model-key :default-model
                        :implicit-provider (:id (first (:providers config)))})

     fallback
     (config-root-pair config {:provider-key :fallback-provider :model-key :fallback-model})

     fallback
     (when (and fallback (not= (first fallback) (first primary))) fallback)]

    (if (or primary fallback)
      (update router
              :providers
              (fn [provider-entries]
                (let
                  [seated (cond-> provider-entries
                            fallback
                            (as-> entries (or (apply seat-root entries fallback) entries))

                            primary
                            (as-> entries (or (apply seat-root entries primary) entries)))]
                  (if (identical? seated provider-entries)
                    provider-entries
                    (providers/reprioritize-providers seated)))))
      router)))

(defn- env-gap-router-error
  "Restate a bare `:svar/no-providers` when the REASON is an unset `${NAME}`:
   `runtime-router-providers` drops every provider whose reference never
   resolved, so an empty fleet is the LAST place that knowledge still exists.
   Without this a headless/CLI user sees svar's generic \"requires at least one
   provider\" and has to guess WHICH variable is missing — exactly the debug
   session the `${NAME}` feature exists to prevent.

   Keeps `:type :svar/no-providers` AND the original as the cause, so
   `config/no-provider-ex` still routes the TUI to the provider manager.
   Returns `t` untouched when the failure has nothing to do with env gaps."
  [config ^Throwable t]
  (let [gaps (config/provider-env-gaps config)]
    (if (or (empty? gaps) (not (config/no-provider-ex t)))
      t
      (ex-info (str "No usable provider — "
                    (str/join "; "
                              (map (fn [[provider-id env-vars]]
                                     (config/provider-env-message provider-id env-vars))
                                   gaps))
                    ". Set "
                    (str/join ", " (distinct (mapcat val gaps)))
                    " in your shell (export NAME=value) and start vis again.")
               {:type :svar/no-providers :vis/user-error true :env-gaps gaps}
               t))))

(defn- build-router
  "`svar/make-router` for a resolved config, with the env-gap diagnosis attached."
  [config]
  (try (svar/make-router (runtime-router-providers config) (config/router-opts config))
       (catch Throwable t (throw (env-gap-router-error config t)))))

(defn get-router
  "Get or create the shared LLM router.

   Honors `:router` opts from `~/.vis/config.edn` (`:rate-limit`,
   `:network`, `:budget`, ...). Without that block svar's built-in
   defaults apply. See `config/router-opts` for the supported keys."
  []
  (or @router-atom
      (let
        [cfg
         (config/resolve-config)

         r
         (-> (build-router cfg)
             (honor-config-roots! cfg))]

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
  (let
    [r (-> (build-router config)
           (honor-config-roots! config))]
    (reset! router-atom r)
    r))

;; ── OAuth credential hydration + 401 recovery ────────────────────────────
;;
;; svar routers intentionally retain provider health/budget state, but their
;; provider maps are immutable snapshots. OAuth credentials must not share that
;; lifetime: another tab/process can rotate a token at any moment. Therefore
;; every provider attempt gets a shallow router copy whose dynamic credential
;; fields are resolved immediately before network I/O. The shared router keeps
;; all of its state; only the attempt's provider vector is credential-hydrated.
;; A 401 then refreshes storage only. The retry boundary reads the new credential
;; itself, so recovery never depends on rebuilding global or cached routers.

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

(def ^:private AUTH_COOLDOWN_MS
  "How long (ms) a provider stays EXCLUDED from routing after its credentials were
   rejected and the turn had to rescue itself on another provider.

   The rescue route itself is per-ITERATION state, so without a process-wide
   cooldown the very next iteration re-probes the dead credential: every single
   iteration then pays a 401 round-trip, a fallback log line and a visible
   progress chunk until the user re-authenticates."
  300000)

(defonce ^:private provider-auth-cooldown
  ;; provider-id -> {:until <epoch-ms>, :since <epoch-ms>, :hits <long>}. Opened by
  ;; `note-provider-auth-cooldown!` when auth recovery is exhausted and the turn
  ;; falls back to another provider; closed by `note-provider-request-ok!` as soon
  ;; as the provider accepts a request again (fresh login / rotated key).
  (atom {}))

(defn- note-provider-auth-cooldown!
  "Open (or extend) the auth cooldown for `pid` after a fallback. Returns true only
   for the FIRST trip of a cooldown window so the caller can log the escape once at
   :warn and keep the repeats at :debug."
  [pid]
  (boolean
    (when pid
      (let
        [now
         (System/currentTimeMillis)

         after
         (swap! provider-auth-cooldown (fn [m]
                                         (let
                                           [prev
                                            (get m pid)

                                            live?
                                            (and prev (> (long (:until prev)) now))]

                                           (assoc m
                                             pid {:until (+ now (long AUTH_COOLDOWN_MS))
                                                  :since (if live? (:since prev) now)
                                                  :hits (if live? (inc (long (:hits prev))) 1)}))))]

        (= 1 (long (:hits (get after pid))))))))

(defn- clear-provider-auth-cooldown!
  "Close the auth cooldown for `pid`; called once the provider accepts a request.
   Returns true when a cooldown was actually cleared."
  [pid]
  (boolean (when (and pid (contains? @provider-auth-cooldown pid))
             (swap! provider-auth-cooldown dissoc pid)
             true)))

(defn- auth-cooled-providers
  "Set of providers whose credentials are still inside their auth cooldown. Prunes
   expired entries on the way so the map cannot grow without bound."
  []
  (let [now (System/currentTimeMillis)]
    (set (keys (swap! provider-auth-cooldown (fn [m]
                                               (into {}
                                                     (filter (fn [[_ v]]
                                                               (> (long (:until v)) now)))
                                                     m)))))))

(defn auth-cooldown-metrics
  "Observability snapshot of the per-provider auth cooldown: the window length and
   the providers still excluded, with the epoch-ms the exclusion lifts and how many
   fallbacks landed inside the window."
  []
  (let [cooled (auth-cooled-providers)]
    {:cooldown-ms AUTH_COOLDOWN_MS
     :cooled-providers cooled
     :cooldowns (select-keys @provider-auth-cooldown cooled)}))

(defn- apply-auth-cooldown-routing
  "Seed an iteration's routing with the providers still serving an auth cooldown so
   the dead credential is skipped BEFORE the request instead of being rediscovered
   with another 401.

   A PIN does not outrank the cooldown. EVERY main turn is pinned — `prepare-turn-context`
   forces the active provider+model into `:routing` so a provider failure surfaces as
   an error the user acts on — so exempting a pinned provider exempted every real
   turn: vis logged a five-minute cooldown and then re-probed, re-minted and
   re-fell-back on the very next iteration, ~12-16s later (issue #114). A COOLED pin
   is released exactly the way [[auth-fallback-routing]] releases it, which is the
   route the previous fallback already took. A pin on a HEALTHY provider is left
   alone, and the provider's own accepted request re-admits it immediately."
  [routing]
  (let
    [current
     (or routing {})

     cooled
     (auth-cooled-providers)

     pinned
     (or (:provider current) (:force-provider current))]

    (if (empty? cooled)
      current
      (cond->
        (-> current
            (cond->
              (contains? cooled pinned)
              (dissoc :provider :model :force-provider :force-model))
            (assoc :on-auth-error :fallback-provider)
            (update :exclude-providers (fnil into #{}) cooled))
        (or (nil? (:on-transient-error current))
            (= :fallback-model-in-the-same-provider (:on-transient-error current)))
        (assoc :on-transient-error :hybrid)))))

(defn- auth-refresh-allowed?
  "Circuit breaker for forced OAuth refreshes. Atomically prunes timestamps older
   than the rolling window for `pid`, records this attempt ONLY when it is
   GRANTED, and returns true while the provider is still under the per-window
   budget. When it returns false the breaker is OPEN: the caller must NOT refresh
   and must recover without touching the token endpoint, so the user
   re-authenticates once instead of the daemon flapping it.

   Recording only GRANTED refreshes is what lets the breaker CLOSE again. An
   earlier version stamped every call, denials included, so a fleet of tabs still
   retrying inside the window kept re-arming the breaker they had just tripped:
   the window never drained and the process stayed in permanent auth fallback
   until restart, even once the on-file token was healthy again."
  [pid]
  (let
    [now
     (System/currentTimeMillis)

     cutoff
     (- now (long AUTH_REFRESH_WINDOW_MS))

     live
     (fn [ts]
       (filterv #(> (long %) cutoff) (or ts [])))

     [before after]
     (swap-vals! auth-refresh-events
                 update
                 pid
                 (fn [ts]
                   (let [kept (live ts)]
                     (cond-> kept
                       (< (long (count kept)) (long AUTH_REFRESH_WINDOW_MAX))
                       (conj now)))))]

    (> (long (count (get after pid))) (long (count (live (get before pid)))))))

(defn auth-refresh-metrics
  "Observability snapshot of the OAuth-refresh circuit breaker. Returns the
   rolling window, the trip threshold, the per-provider count of forced
   refreshes still inside the window, and the set of providers at the refresh
   limit (breaker OPEN). Surfaced by the gateway `/metrics` endpoint so an
   auth-refresh flap is visible at a glance instead of needing a `vis.log`
   grep."
  []
  (let
    [cutoff
     (- (System/currentTimeMillis) (long AUTH_REFRESH_WINDOW_MS))

     in-window
     (into {}
           (for
             [[pid ts]
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
                                 (when (>= (long n) (long AUTH_REFRESH_WINDOW_MAX)) pid)))
                         in-window)}))

(defn- auth-error-shaped?
  "True exactly when Svar's canonical failure verdict is authentication.

   Vis uses the verdict only to cool down or mutate credentials; it never
   reclassifies provider status codes, prose, or routing attempts."
  [^Throwable e]
  (= :auth (:category (perr/svar-classification e))))

(defn- auth-fallback-routing
  "Build one cross-provider rescue route after OAuth refresh/backoff is exhausted.
   Returns nil after visible output, without a provider id, or once enabled."
  [^Throwable e routing resolved-model]
  (let
    [data
     (ex-data e)

     provider
     (:provider resolved-model)

     output-started?
     (or (pos? (long (or (:content-acc-len data) 0)))
         (pos? (long (or (:reasoning-acc-len data) 0)))
         (some? (:partial-content data))
         (some? (:reasoning data)))

     current
     (or routing {})]

    (when (and provider
               (auth-error-shaped? e)
               (not output-started?)
               (not= :fallback-provider (:on-auth-error current)))
      (cond->
        (-> current
            (dissoc :provider :model :force-provider :force-model)
            (assoc :on-auth-error :fallback-provider)
            (update :exclude-providers (fnil conj #{}) provider))
        (or (nil? (:on-transient-error current))
            (= :fallback-model-in-the-same-provider (:on-transient-error current)))
        (assoc :on-transient-error :hybrid)))))

(defn- refresh-just-failed?
  "True when we FORCED an OAuth refresh for this provider very recently (within
   [[AUTH_PROPAGATION_WINDOW_MS]]) and the credential is STILL auth-failing.
   Signals propagation lag (back off and retry the request-bound hydrated token)
   rather than a genuinely dead credential. The recency marker is provider-wide
   and is cleared by [[note-provider-request-ok!]] after accepted I/O."
  [^Throwable e resolved-model]
  (let [pid (:provider resolved-model)]
    (and (auth-error-shaped? e)
         (boolean (when-let [{:keys [at]} (get @auth-last-refreshed pid)]
                    (< (- (System/currentTimeMillis) (long at))
                       (long AUTH_PROPAGATION_WINDOW_MS)))))))

(defn- note-provider-request-ok!
  "Clear the just-refreshed propagation marker AND any auth cooldown for the provider
   that ACCEPTED this iteration's request. Keeps [[refresh-just-failed?]]'s recency
   window scoped to the post-refresh settling burst, so a real credential rotation
   later is treated as a fresh 401 (re-mint), never misread as propagation lag, and
   lets a re-authenticated provider re-enter routing immediately instead of waiting
   out [[AUTH_COOLDOWN_MS]].

   `iteration-result`'s `:llm-provider` is the provider that actually SERVED the
   request; `resolved-model` is only Vis' pre-call guess — `resolve-effective-model`
   reads the router HEAD, which the turn's pin hoists — so noting the guess let a
   turn RESCUED on a peer re-admit the dead credential, and the next iteration
   re-probed it (issue #114). No-op when the provider has neither marker."
  [resolved-model iteration-result]
  (when-let
    [pid (let [served (:llm-provider iteration-result)]
           (cond (keyword? served) served
                 (string? served) (keyword served)
                 :else (:provider resolved-model)))]
    (when (contains? @auth-last-refreshed pid) (swap! auth-last-refreshed dissoc pid))
    (clear-provider-auth-cooldown! pid)))

(defn- auth-refreshable-error?
  "True when Svar classified `e` as authentication and Vis can produce a new
   credential for the failing provider.

   Refreshing OAuth or `api_key_command` output mutates the next request; it is
   not a second provider failure classifier or transport retry policy."
  [^Throwable e resolved-model]
  (let [pid (:provider resolved-model)]
    (boolean (and (= :auth (:category (perr/svar-classification e)))
                  (or (some-> (registry/provider-by-id pid)
                              :provider/refresh-token-fn)
                      (config/command-backed? pid))))))

(defn- hydrate-router-credentials
  "Return an attempt-local copy of `router` with every provider's current
   credential fields resolved immediately before request dispatch.

   Two credential sources are hydrated here: a registry-backed
   `:provider/get-token-fn` (OAuth and friends), and a command-backed
   `api_key_command`, whose token is re-read from the credential cache so an
   `invalidate-credential-command!` on a 401 actually reaches the wire instead of
   waiting for the next router build.

   Router health, budget and retry state are preserved by sharing the original
   map and replacing only `:providers`. A provider token lookup failure is
   deliberately failure-safe: that provider retains its previous snapshot so
   normal request/error handling remains authoritative."
  [router]
  (update router
          :providers
          (fn [provider-entries]
            (mapv (fn [{:keys [id] :as provider-entry}]
                    (if-let
                      [get-token-fn (some-> (registry/provider-by-id id)
                                            :provider/get-token-fn)]
                      (try (let [{:keys [token api-url llm-headers responses-path]} (get-token-fn)]
                             (cond-> provider-entry
                               (some? token)
                               (assoc :api-key token)

                               (some? api-url)
                               (assoc :base-url api-url)

                               (some? llm-headers)
                               (assoc :llm-headers llm-headers)

                               (some? responses-path)
                               (assoc :responses-path responses-path)))
                           (catch Throwable t
                             (tel/log! {:level :warn
                                        :id ::provider-credential-hydration-failed
                                        :data {:provider id :error (ex-message t)}}
                                       (str "Could not hydrate current credential for "
                                            id
                                            "; retaining the previous request snapshot"))
                             provider-entry))
                      ;; Command-backed: the cache serves the same token in the
                      ;; steady state (no fork per request) and re-execs the
                      ;; helper exactly once after a 401 invalidated it. A helper
                      ;; that is failing right now yields nil and keeps the
                      ;; snapshot, so the provider error stays authoritative.
                      (if-let [token (config/command-token id)]
                        (assoc provider-entry :api-key token)
                        provider-entry)))
                  provider-entries))))

(defn- hydrate-environment-router
  "Hydrate only the router snapshot used by this provider attempt."
  [environment]
  (update environment :router hydrate-router-credentials))

(defn- router-provider-token
  "Token actually carried by provider `pid` in this exact router snapshot."
  [router pid]
  (some #(when (= pid (:id %)) (:api-key %)) (:providers router)))

(defn- try-refresh-provider-token!
  "Recover a refreshable auth rejection without mutating any router.

   `attempt-router` is the exact request snapshot that received the 401, making
   its provider `:api-key` the exact rejected token. Before spending refresh
   budget, resolve current storage once: if a peer already installed a different
   token, simply retry and let request-bound hydration adopt it. Otherwise force
   one persisted refresh. The next attempt hydrates from storage; no global
   rebuild or cached-environment reseat is involved.

   A command-backed provider has no OAuth hook at all: its refresh is dropping
   the memoized `api_key_command` token so the next request boundary re-runs the
   helper. Same budget, same one-retry contract."
  [attempt-router resolved-model]
  (let
    [pid
     (:provider resolved-model)

     provider
     (registry/provider-by-id pid)

     f
     (:provider/refresh-token-fn provider)

     get-token-fn
     (:provider/get-token-fn provider)

     rejected
     (router-provider-token attempt-router pid)

     current
     (try (some-> get-token-fn
                  (apply [])
                  :token)
          (catch Throwable _ nil))]

    (boolean
      (cond (and (not f) (config/command-backed? pid))
            (if (auth-refresh-allowed? pid)
              (do (config/invalidate-credential-command! pid)
                  ;; Mark the attempt like an OAuth refresh so a SECOND 401 takes
                  ;; the propagation backoff instead of re-forking the helper.
                  (swap! auth-last-refreshed assoc pid {:at (System/currentTimeMillis)})
                  (tel/log! {:level :warn :id ::credential-command-refreshed :data {:provider pid}}
                            (str "Auth 401 for " pid
                                 " — re-running its credential command; retrying with"
                                 " request-bound credential hydration"))
                  true)
              (do (tel/log! {:level :error
                             :id ::auth-refresh-circuit-open
                             :data {:provider pid
                                    :window-ms AUTH_REFRESH_WINDOW_MS
                                    :max AUTH_REFRESH_WINDOW_MAX}}
                            (str "Auth 401 — credential-command refresh circuit OPEN for "
                                 pid
                                 "; NOT re-running the helper — surfacing provider error"))
                  false))
            (not f) false
            ;; A concurrent request/process already won the rotation. Do not
            ;; touch either the breaker or token endpoint; retry hydration will
            ;; pick this value up at the request boundary.
            (and (some? current) (not= current rejected))
            (do (tel/log! {:level :warn :id ::auth-peer-token-adopted :data {:provider pid}}
                          (str "Auth 401 for "
                               pid
                               " used a stale request credential; adopting the peer token"))
                true)
            (not (auth-refresh-allowed? pid))
            (do (tel/log! {:level :error
                           :id ::auth-refresh-circuit-open
                           :data {:provider pid
                                  :window-ms AUTH_REFRESH_WINDOW_MS
                                  :max AUTH_REFRESH_WINDOW_MAX}}
                          (str "Auth 401 — OAuth refresh circuit OPEN for " pid
                               " (" AUTH_REFRESH_WINDOW_MAX
                               " refreshes in " (quot (long AUTH_REFRESH_WINDOW_MS) 1000)
                               "s); NOT refreshing — surfacing provider error,"
                               " re-authenticate this provider"))
                false)
            :else (try
                    ;; Pass exactly what this attempt sent. Older/third-party hooks
                    ;; may still expose only a zero-arity implementation.
                    (try (f rejected) (catch clojure.lang.ArityException _ (f)))
                    (swap! auth-last-refreshed assoc pid {:at (System/currentTimeMillis)})
                    (tel/log! {:level :warn :id ::auth-token-refreshed :data {:provider pid}}
                              (str "Auth 401 — force-refreshed OAuth token for "
                                   pid
                                   "; retrying with request-bound credential hydration"))
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
  (svar/ask-code! (get-router)
                  (rt/with-agent-initiator (rt/with-default-ask-code-idle-timeout opts))))

(defn llm-text!
  "Fast helper LLM call for extensions.

   Uses svar routing (`:routing {:optimize :cost}`) instead of Vis-side model
   name heuristics. The call still goes through `svar/ask-code!` because Vis no
   longer uses the retired `ask!` structured-output path; `:lang \"text\"`,
   `:reasoning :off`, and `:code-tail-pointer? true` make the return a plain
   text string under :text. Callers may pass either :messages or :system +
   :prompt."
  [{:keys [messages system prompt reasoning temperature routing] :as opts}]
  (let
    [opts
     ;; Helper traffic is agent activity, not a human prompt: Copilot bills an
     ;; unmarked request as a full premium interaction.
     (rt/with-agent-initiator opts)

     messages
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
   (let
     [provider
      (first (:providers router))

      model
      (first (:models provider))]

     (when model
       (cond-> (if (map? model) model {:name (str model)})
         (:id provider)
         (assoc :provider (:id provider))))))
  ([router _routing-overrides] (resolve-effective-model router)))

(defn resolve-model-info
  "Resolved model map for the model a SESSION actually routes to.

   `resolve-effective-model` answers a different question — the router's GLOBAL
   root — and a channel that asks it about a session's capabilities describes
   the wrong model whenever the session picked something else (which is the
   normal case: Ctrl+T and the web picker both write a per-session preference).
   `provider-id`/`model-name` come from that preference; either may be nil, and
   the first provider/model that matches what IS given wins. Falls back to the
   root model so a session with no preference still gets an answer."
  [router provider-id model-name]
  (let
    [provider-id
     (some-> provider-id
             name
             not-empty
             keyword)

     hit
     (first (for
              [provider
               (:providers router)

               :when (or (nil? provider-id) (= provider-id (:id provider)))
               model
               (:models provider)

               :let [model
                     (if (map? model) model {:name (str model)})]
               :when (or (nil? model-name) (= (str model-name) (str (:name model))))]

              (cond-> model
                (:id provider)
                (assoc :provider (:id provider)))))]

    (or hit (resolve-effective-model router))))

(defn router-for-model
  "Return a router variant whose provider/model ORDER reflects a model PREFERENCE,
   so svar's router picks + falls back accordingly — WE don't pick one model, we
   express the preference and let the inner router decide (no svar change: it
   already routes by the router's order). `prefs` is a model name OR an ORDERED
   coll of names; matching models are hoisted to the front in preference order
   (within each provider AND across providers), and the rest of the router follows
   UNCHANGED as fallback. Blank/unknown prefs → the router as-is (child inherits the
   parent's order). Coordinator: `sub_loop(prompt, subctx, {\"models\": [\"haiku\",
   \"sonnet\"]})` (or a single `\"model\"`) — try the cheap one first, fall back.

   Vector order alone is DECORATION to svar, which selects by provider `:priority`
   and then by the provider's `:root` model name. So the hoist is also written into
   both fields: a matched provider's `:root` becomes its preferred model and the
   whole fleet is renumbered from its new position. Without that, a coordinator's
   `models` list changed the turn card and the cost row while every child turn
   still ran the default provider's root model."
  [router prefs]
  (let
    [names (->> (if (coll? prefs) prefs [prefs])
                (keep #(some-> %
                               str
                               not-empty))
                vec)]
    (if (empty? names)
      router
      (let
        [m-name (fn [m]
                  (:name (if (map? m) m {:name (str m)})))
         rank (zipmap names (range))
         ;; lower = more preferred; unlisted = +inf (keeps relative order, stable sort)
         m-rank (fn [m]
                  (get rank (m-name m) Long/MAX_VALUE))
         p-rank (fn [p]
                  (reduce min Long/MAX_VALUE (map m-rank (:models p))))
         reorder (fn [p]
                   (let
                     [models (vec (sort-by m-rank (:models p)))
                      head (first models)]

                     (cond-> (assoc p :models models)
                       ;; Only a provider that actually offers a PREFERRED model
                       ;; gets a new root; the rest keep their configured one so
                       ;; fallback still lands on what the config chose.
                       (contains? rank (m-name head))
                       (assoc :root (m-name head)))))]

        (assoc router
          :providers (->> (:providers router)
                          (map reorder)
                          (sort-by p-rank)
                          providers/reprioritize-providers))))))

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
     (let
       [pid
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
         (let
           [serving
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

;; System var helpers
;;
;; There is no cross-turn var snapshotting: the engine does not parse the
;; iteration's block source for `(def NAME …)` shapes to materialize and
;; persist sandbox locals. Sandbox state is intra-turn scratch only.
;; Auto-archive was retired together with the `definition_*` sidecar
;; tables: there is no cross-turn var registry to drive eviction off,
;; and the Python sandbox is fresh every turn anyway. `auto-archive-hot-
;; symbols!` is a no-op stub kept so call sites compile while we sweep
;; them out.

(defn auto-archive-hot-symbols!
  "Deprecated NOOP. Cross-turn def survival was removed when the
   `definition_*` sidecar tables were dropped; the Python sandbox starts
   fresh each turn, so there is nothing to archive."
  [_environment]
  nil)

;; Iteration loop + run-turn! (inlined from former base)

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
  ;; pairs (oldest-first). NOTHING trims this by token budget: neither the
  ;; seed (cross-turn carry) nor the renderer. `max-context-tokens` only
  ;; feeds the advisory context-pressure hint. The sole token-driven
  ;; reduction is reactive — `context-overflow-recovery` after the provider
  ;; refuses the request.
  {:trailer-iters []})

(def ^:private balanced-reasoning :balanced)

(do
  (defn- status->id [status] (when status (keyword "rlm.status" (name status))))
  (def ^:private cost-map-keys
    ["input_cost" "input_uncached_cost" "input_cached_cost" "input_cache_write_cost"
     "cache_read_cost" "cache_write_cost" "output_cost" "total_cost"])
  (def ^:private codex-fast-price-multiplier 2.0)
  (defn- codex-fast-cost-multiplier
    "OpenAI Codex Fast mode uses Priority processing, currently billed at 2x
       Standard for input, cached input, cache writes, and output. Keep the
       multiplier provider-gated: `service_tier` is an open wire extension and
       must not change another provider's accounting if a caller sends it there."
    [extra-body provider]
    (let
      [tier
       (or (:service_tier extra-body)
           (get extra-body "service_tier")
           (:service-tier extra-body)
           (get extra-body "service-tier"))

       provider-id
       (cond (keyword? provider) (name provider)
             (some? provider) (str provider))]

      (if (and (= "priority"
                  (some-> tier
                          str
                          str/lower-case))
               (= "openai-codex" provider-id))
        codex-fast-price-multiplier
        1.0)))
  (defn- estimate-token-cost
    "Estimate cost from provider usage while preserving cached/non-cached input split.
       `:cost-multiplier` scales every monetary component after svar prices the
       canonical usage; token counts remain untouched."
    ([model input-tokens output-tokens] (estimate-token-cost model input-tokens output-tokens {}))
    ([model input-tokens output-tokens opts]
     (try (let
            [opts
             (or opts {})

             multiplier
             (double (or (:cost-multiplier opts) 1.0))

             cost-map
             (wire/canonical (svar-router/estimate-cost model
                                                        input-tokens
                                                        output-tokens
                                                        svar-router/MODEL_PRICING
                                                        (dissoc opts :cost-multiplier)))]

            (if (and (map? cost-map) (not= 1.0 multiplier))
              (reduce (fn [m k]
                        (update m k #(if (number? %) (* multiplier (double %)) %)))
                      cost-map
                      cost-map-keys)
              cost-map))
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

(def ^:private empty-replies-give-up-text
  "Fallback shown only when the provider repeatedly returns an empty reply."
  "The model returned empty replies repeatedly, so the turn was stopped.")

(defn- provider-output-chunk?
  "True after visible assistant output or a tool execution entered the stream.
   Engine lifecycle/progress chunks are safe to replay after a pre-output failure."
  [chunk]
  (contains? #{:reasoning :content :assistant-prose :form-start :tool-start :form-result}
             (:phase chunk)))

(defn- emergency-fold-activity
  "Mechanical activity count for the omitted iterations, derived only from the
   tool calls they recorded. `python_execution` is the only call there is, so the
   COUNT is the whole shape — no tool family is left to name."
  [trailer-iters scopes]
  (let
    [calls
     (into []
           (comp (filter (fn [[_ rec]]
                           (contains? scopes (some iter-of-scope (keep :scope (:forms-vec rec))))))
                 (mapcat (fn [[_ rec]]
                           (keep :name (:tool-calls rec)))))
           trailer-iters)

     n
     (long (count calls))]

    (when (pos? n) (str n " tool call" (when (not= n 1) "s")))))

(def ^:private CONTEXT_OVERFLOW_MARGINS
  "Headroom each successive rescue leaves under the provider's input limit AFTER the
   estimator's undercount has been measured (`estimator-undercount`).

   The undercount itself is never guessed: the overflow carries `:input-tokens`, the
   provider's own count of the exact message set our estimator priced, so a rescue
   divides the limit by a measurement taken from the very request that was refused.
   These margins cover only what that measurement cannot know — the projection swaps
   dense tool JSON for prose gists, whose own ratio differs from the refused set's,
   and the reply still needs room. A retry that overflows again re-measures against
   its own numbers and tightens the margin."
  [0.9 0.7 0.5])

(def ^:private CONTEXT_OVERFLOW_BLIND_CUTS
  "Fallback targets for an overflow that carries no usable `:input-tokens` or
   `:max-input-tokens` (some providers refuse without measuring).

   With nothing to calibrate against, a rescue can only bisect its OWN estimate, so
   each attempt targets this fraction of the refused request's local size. Still no
   invented factor: the target is purely relative and escalation does the searching."
  [0.5 0.25 0.1])

(defn- estimator-undercount
  "How far the local estimator undercounts the provider ON THIS MESSAGE SET.

   `provider-tokens` (the overflow's `:input-tokens`) and `local-tokens`
   (`svar-router/count-messages`) price the SAME messages, so their ratio is a
   measurement of the provider's tokenizer against ours rather than a constant:
   dense tool JSON ran ~1.49x for session `cd24926e` (1,437,952 provider vs 963,503
   local), prose runs far closer to 1.0, and another model runs somewhere else.

   nil when either side is missing. Never below 1.0 — an estimator that reads
   GENEROUS earns no licence to send more than the limit."
  [provider-tokens local-tokens]
  (when (and (number? provider-tokens)
             (number? local-tokens)
             (pos? (long provider-tokens))
             (pos? (long local-tokens)))
    (max 1.0 (/ (double provider-tokens) (double local-tokens)))))

(defn- overflow-fold-budget
  "Local-estimator budget for the next rescue of a refused request.

   Measured path: `provider-limit` / measured undercount * `margin`, i.e. the budget
   is expressed in the same currency the projection can actually count. Blind path
   (no provider numbers to measure): `cut` of the request's own local size. nil only
   when there is nothing at all to aim at, which leaves the projection with its
   weakest contract — the result must merely be smaller."
  [{:keys [provider-tokens provider-limit margin cut]} local-tokens]
  (let [factor (estimator-undercount provider-tokens local-tokens)]
    (cond (and factor (number? provider-limit) (pos? (long provider-limit)))
          (long (* (/ (double provider-limit) (double factor)) (double (or margin 1.0))))
          (and (number? cut) (number? local-tokens) (pos? (long local-tokens)))
          (long (* (double local-tokens) (double cut)))
          :else nil)))

(defn- emergency-fold-projection
  "Build a provider projection with settled trailer iterations collapsed through the
   same `apply-summaries` path as `fold_session`.

   Folding is GRADUATED: the foldable universe is walked OLDEST first and the search
   keeps the SMALLEST prefix whose projection fits the budget `budget-fn` derives from
   the local size of the refused request (see `overflow-fold-budget`), so a rescue
   spends the least recent context it can and recent settled work survives verbatim.
   Only when no prefix fits does everything foldable collapse. Canonical trailer
   history and persisted summaries are never mutated. Existing semantic folds remain
   authoritative and are not replaced by the mechanical gist. The gate uses Svar's
   message-token estimator, not serialized characters; the retry itself repeats Svar's
   provider-aware exact preflight before any send."
  [base-messages trailer-iters summaries replay-target model budget-fn]
  (let
    [universe
     (into []
           (keep (fn [[_ rec]]
                   (some iter-of-scope (keep :scope (:forms-vec rec)))))
           trailer-iters)

     already-folded
     (into #{} (mapcat #(get % "scopes")) (ctx-engine/expand-through summaries universe))

     ;; Chronological (the trailer is ordered), so a prefix is always the OLDEST work.
     foldable
     (into [] (comp (distinct) (remove already-folded)) universe)

     live-turn
     (some->> universe
              (keep (comp first ctx-engine/scope-key))
              seq
              (apply max))]

    (when (seq foldable)
      (let
        [before-messages
         (into (vec base-messages) (conversation-suffix trailer-iters replay-target))

         before-tokens
         (svar-router/count-messages model before-messages)

         ;; Budget is derived from what the provider actually charged for THIS set,
         ;; so the gate below compares local estimate against local estimate.
         budget
         (budget-fn before-tokens)

         project
         (fn [n]
           (let
             [scopes
              (into #{} (take n) foldable)

              activity
              (emergency-fold-activity trailer-iters scopes)

              intent
              (cond->
                {"scopes" scopes
                 "gist" (str "Emergency transport fold omitted "
                             (count scopes)
                             " settled iteration(s)"
                             (when (seq activity) (str " (" activity ")"))
                             " after context overflow; canonical session history remains intact.")}
                live-turn
                (assoc "at_turn" live-turn))

              folded-trailer
              (apply-summaries trailer-iters (conj (vec summaries) intent))

              messages
              (into (vec base-messages) (conversation-suffix folded-trailer replay-target))]

             {:messages messages
              :scopes scopes
              :after-tokens (svar-router/count-messages model messages)}))

         fits?
         (fn [{:keys [after-tokens]}]
           (and (< (long after-tokens) (long before-tokens))
                (or (nil? budget) (<= (long after-tokens) (long budget)))))

         ;; Folding more can only shrink the projection, so the smallest fitting
         ;; prefix is a binary search — O(log n) estimator passes, not O(n).
         chosen
         (loop
           [lo
            1

            hi
            (count foldable)

            best
            nil]

           (if (> (long lo) (long hi))
             (or best (project (count foldable)))
             (let
               [mid
                (quot (+ (long lo) (long hi)) 2)

                candidate
                (project mid)]

               (if (fits? candidate) (recur lo (dec mid) candidate) (recur (inc mid) hi best)))))]

        (when (fits? chosen)
          {:messages (:messages chosen)
           :before-tokens before-tokens
           :after-tokens (:after-tokens chosen)
           :saved-tokens (- (long before-tokens) (long (:after-tokens chosen)))
           :budget-tokens budget
           :folded-scopes (count (:scopes chosen))
           :foldable-scopes (count foldable)
           :scopes (:scopes chosen)})))))

(defn- context-overflow-recovery!
  "Claim the next overflow rescue of THIS iteration, publish the failed request's
   measured utilization, and return a smaller provider projection. nil means terminal.

   The budget is MEASURED, not guessed. The refused request carries the provider's own
   `:input-tokens` for exactly the message set our estimator prices, so the rescue
   divides `:max-input-tokens` by that observed undercount (`estimator-undercount`)
   before spending it; only the safety margin is a constant, and only because the
   post-fold message mix is not the mix that was measured.

   Rescues ESCALATE: `CONTEXT_OVERFLOW_MARGINS` tightens the margin on every attempt,
   so a cheap fold of the oldest settled work is tried before history collapses
   wholesale — and a retry that overflows AGAIN re-measures against its own numbers
   instead of ending the turn. Each rescue must be strictly smaller than the previous
   one; without progress the retry would resend a request the provider already refused,
   so the iteration goes terminal instead."
  [{:keys [error output-started? recovery-state ctx-atom turn-input-tokens base-messages
           trailer-iters summaries replay-target model]}]
  (let [overflow (ex-data error)]
    (when (and (contains? perr/CONTEXT_OVERFLOW_TYPES (:type overflow)) (not @output-started?))
      (let
        [{:keys [attempts last-after-tokens]} (swap! recovery-state update :attempts (fnil inc 0))
         attempt (long attempts)]

        (when (<= attempt (count CONTEXT_OVERFLOW_MARGINS))
          (stamp-utilization! ctx-atom
                              (ctx-engine/utilization (:input-tokens overflow)
                                                      (:max-input-tokens overflow)
                                                      turn-input-tokens
                                                      ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))
          (when-let
            [projection (emergency-fold-projection
                          base-messages
                          trailer-iters
                          summaries
                          replay-target
                          model
                          (fn [local-tokens]
                            (overflow-fold-budget
                              {:provider-tokens (:input-tokens overflow)
                               :provider-limit (:max-input-tokens overflow)
                               :margin (nth CONTEXT_OVERFLOW_MARGINS (dec attempt))
                               :cut (nth CONTEXT_OVERFLOW_BLIND_CUTS (dec attempt))}
                              local-tokens)))]
            (when (or (nil? last-after-tokens)
                      (< (long (:after-tokens projection)) (long last-after-tokens)))
              (swap! recovery-state assoc :last-after-tokens (:after-tokens projection))
              (assoc projection
                :attempt attempt
                :provider-tokens (:input-tokens overflow)
                :provider-limit (:max-input-tokens overflow)
                :estimator-undercount (estimator-undercount (:input-tokens overflow)
                                                            (:before-tokens projection))))))))))

(def ^:private voice-projection-prompt
  "This is a text-only turn whose client may optionally read a separate projection aloud. Write the complete normal answer exactly as you would for any text client; do not mention voice mode, listening, playback, or ask the user to confirm that audio played. Then finish with exactly one fenced `vis-speech` block. Inside that block write only concise, natural plain text suitable for text-to-speech: no Markdown, code, tables, raw URLs, citation syntax, or tool narration. State the outcome, an important caveat, and any question that needs an answer. Do not repeat secrets. Aim for 15 to 45 seconds.")

(defn- voice-system-prompt
  [system-prompt turn-features]
  (if (true? (get turn-features "voice_projection"))
    (str (when-not (str/blank? (str system-prompt)) (str system-prompt "\n\n"))
         voice-projection-prompt)
    system-prompt))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt session-turn-id
           ;; `max-context-tokens` feeds advisory context-pressure hooks;
           ;; trailer assembly itself still owns no token trimming.
           max-context-tokens hooks cancel-atom cancel-token reasoning-default routing extra-body
           reasoning-effort turn-features workspace-overrides]}]
  (let
    [system-prompt
     (voice-system-prompt system-prompt turn-features)

     environment
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

     root-cost-multiplier
     (codex-fast-cost-multiplier extra-body (:provider resolved-model))

     _
     (assert effective-model "Router must resolve a root model")

     ;; Clear any sticky best-answer from a PRIOR turn (the atom lives on the
     ;; per-session env) so this turn's cancel-fallback and its answer only ever
     ;; surface what THIS turn actually produced.
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

     ;; Turn identity must be current before the frozen context and the first
     ;; user message are assembled. This makes every turn boundary explicit on
     ;; iteration 1 rather than waiting for the first tool-result delta.
     _turn-sync
     (when-let [ctx-atom (:ctx-atom environment)]
       (swap! ctx-atom ctx-engine/enter-turn (or turn-position 1)))

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
       (reset! standing-ctx-atom {:block (ctx-loop/render-block! environment
                                                                 ctx-renderer/render-ctx-static)
                                  :baseline (ctx-loop/render-block! environment
                                                                    ctx-renderer/ctx-static-map)}))

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
     (let
       [disk (try (attachments/collect-user-images user-request
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

     ;; Resolve once for both image capability and the first-turn utilization
     ;; ceiling. The previous persisted request is the only real measurement
     ;; available before iteration 1.
     initial-resolved-model
     (resolve-effective-model (:router environment) (or routing {}))

     initial-target-vision?
     (or (empty? (:attached user-attachments))
         (target-supports-vision? (replay-context initial-resolved-model)))

     initial-context-limit
     (or max-context-tokens
         (:input-limit initial-resolved-model)
         (:context initial-resolved-model)
         200000)

     _initial-utilization
     (when-let [ctx-atom (:ctx-atom environment)]
       (if-let
         [measured (ctx-engine/utilization (:last-request-tokens previous-usage)
                                           initial-context-limit
                                           0
                                           ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS)]
         (stamp-utilization! ctx-atom measured)
         (swap! ctx-atom (fn [ctx]
                           (if (get ctx "engine_utilization")
                             ctx
                             (assoc ctx
                               "engine_utilization"
                               {"last_request_tokens" 0
                                "turn_total_tokens" 0
                                "auto_compress_above" ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS
                                "model_input_limit" (long initial-context-limit)
                                "saturation" 0
                                "headroom_tokens" (long initial-context-limit)
                                "measured" false}))))))

     turn-context
     (ctx-loop/render-block! environment ctx-renderer/render-turn-boundary)

     initial-messages
     (prompt/assemble-initial-messages {:stable-prompt-messages stable-prompt-messages
                                        :initial-user-content user-request
                                        :turn-context turn-context
                                        :user-images (:attached user-attachments)
                                        :skipped-images (:skipped user-attachments)
                                        :vision? initial-target-vision?
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
             (let
               [iter-in
                (long (or (:input-tokens api-usage) 0))

                iter-reason
                (get-in api-usage [:output-tokens-details :reasoning])]

               (cond->
                 (-> acc
                     (update :input-tokens + iter-in)
                     (update :output-tokens + (or (:output-tokens api-usage) 0))
                     (update :cached-tokens
                             +
                             (or (get-in api-usage [:input-tokens-details :cache-read]) 0))
                     (update :cache-creation-tokens
                             +
                             (or (get-in api-usage [:input-tokens-details :cache-write]) 0))
                     ;; Per-iter snapshots: overwrite, not accumulate.
                     (assoc :last-iter-input iter-in)
                     (assoc :last-iter-reasoning iter-reason)
                     (update :iter-count inc))
                 (some? iter-reason)
                 (-> (update :reasoning-tokens + (long iter-reason))
                     (assoc :reasoning-reported? true))))))))

     ;; Per-iteration token + cost projection. The schema's
     ;; `iteration.llm_*_tokens` / `iteration.llm_cost_usd` columns
     ;; carry one row per iteration so a future `vis-agent report`
     ;; caller can sum or break down cost without re-walking
     ;; provider envelopes. Returns nil when the call surfaced no
     ;; usage (e.g. iteration-level error before a response
     ;; landed), in which case the persistance layer leaves the
     ;; columns NULL.
     iteration-token-cost
     (fn iteration-token-cost ([api-usage] (iteration-token-cost api-usage nil nil))
       ([api-usage actual-model actual-provider]
        (when api-usage
          (let
            [in
             (long (or (:input-tokens api-usage) 0))

             out
             (long (or (:output-tokens api-usage) 0))

             reas
             (get-in api-usage [:output-tokens-details :reasoning])

             cach
             (long (or (get-in api-usage [:input-tokens-details :cache-read]) 0))

             cache-created
             (long (or (get-in api-usage [:input-tokens-details :cache-write]) 0))

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
             (estimate-token-cost (or (some-> actual-model
                                              str
                                              not-empty)
                                      effective-model)
                                  in
                                  out
                                  {:api-usage api-usage
                                   :cost-multiplier (codex-fast-cost-multiplier
                                                      extra-body
                                                      (or actual-provider
                                                          (:llm-provider api-usage)
                                                          (:provider api-usage)
                                                          (:provider resolved-model)))})

             total
             (when (map? cost-map) (get cost-map "total_cost"))]

            (when (map? cost-map) (swap! accrued-cost-atom #(merge-cost-maps (or % {}) cost-map)))
            {:tokens (cond-> {"input" in "output" out "cached" cach "cache_created" cache-created}
                       (some? reas)
                       (assoc "reasoning" (long reas)))
             :cost-usd (when (number? total) (double total))}))))

     finalize-cost
     (fn []
       (let
         [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens cache-creation-tokens
                  reasoning-reported?]}
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
                                    :cache-creation-tokens cache-creation-tokens
                                    :cost-multiplier root-cost-multiplier}))]

         {:tokens (cond->
                    {"input" input-tokens
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
    (let
      [seeded-trailer-iters
       (try
         (when-let [session-id (:session-id environment)]
           (let
             [d (:db-info environment)
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
                                   (map #(assoc % :cross-turn/turn-status (:status q))
                                        (try (persistance/db-list-session-turn-iterations d (:id q))
                                             (catch Throwable _ [])))))
                         (filter #(= :done (:status %)))
                         ;; Slash commands are local control-plane events. Keep
                         ;; their rows for transcript/audit, never provider replay.
                         (remove user-slash-iteration?)
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
                       ;; The provider's assistant envelope is NOT persisted: the
                       ;; signature inside it replays only to the same provider,
                       ;; inside the session that earned it, and
                       ;; `compatible-preserved-thinking-trailer-iters` rejects a
                       ;; reseeded iteration before replay regardless.
                       ;; Its produced artifacts still ride to a vision model
                       ;; (see the `replay? false` branch of `conversation-suffix`),
                       ;; even though the assistant/thinking chain is dropped.
                       :attachments (attachment-storage/hydrate-all (get iters-atts (str (:id it))))
                       ;; The owning turn's terminal status decides whether settled
                       ;; outputs need continuity replay on later requests.
                       :cross-turn/turn-status (:cross-turn/turn-status it)
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
        (loop
          [loop-state (merge {:iteration 0 :messages initial-messages :trace []}
                             FRESH_ITER_CARRY
                             (when (seq seeded-trailer-iters)
                               {:trailer-iters seeded-trailer-iters}))]
          (let [{:keys [iteration messages trace trailer-iters llm-provider]} loop-state]
            (ctx-loop/set-turn-state! environment :iteration (inc (long iteration)))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error
                              iteration
                              {:reason :cancelled
                               :cancel-source (cancellation/cancel-reason (:cancel-token
                                                                            environment))})
                  ;; Sticky best-answer: surface the latest non-blank answer
                  ;; this turn produced instead of a blank answer.
                  (let
                    [sticky (some-> (:turn-state-atom environment)
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
              (let
                [raw-reasoning-level (when has-reasoning? base-reasoning-level)
                 reasoning-level
                 (copilot-claude-reasoning-level resolved-model user-request raw-reasoning-level)
                 _ (log-stage! :iteration/start
                               iteration
                               {:message-count (count messages)
                                :reasoning reasoning-level
                                :reasoning-effort reasoning-effort
                                :requested-reasoning raw-reasoning-level})
                 pre-resolved-model (resolve-effective-model (:router environment) (or routing {}))
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
                 _llm-provider-context (cond->
                                         {:selected (llm-id (:provider pre-resolved-model)
                                                            (some-> (:name pre-resolved-model)
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
                               (let
                                 [u @usage-atom
                                  req (if (pos? (long (:iter-count u)))
                                        (long (:last-iter-input u))
                                        (long (:previous-request-input u)))]

                                 (stamp-utilization! ca
                                                     (ctx-engine/utilization
                                                       req
                                                       effective-context-limit
                                                       (:input-tokens u)
                                                       ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))))
                 ;; Standing context render + budget guard.
                 ;;
                 ;; Canonical history stays intact. The model owns semantic folds;
                 ;; emergency rescue may compact only the provider-facing projection.
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
                 ;; First stamp the universe from the canonical raw trailer; then
                 ;; apply folds and re-stamp token weights from exactly the
                 ;; provider-visible projection. Raw universe/NTR recovery stays
                 ;; intact, while an already-collapsed payload is never priced twice.
                 _raw-iter-state (stamp-iter-universe! (:ctx-atom environment) trailer-iters)
                 replay-target (replay-context pre-resolved-model)
                 summarized-trailer-iters (apply-summaries trailer-iters
                                                           (some-> (:ctx-atom environment)
                                                                   deref
                                                                   (get "session_summaries")))
                 _visible-iter-state (stamp-iter-universe! (:ctx-atom environment)
                                                           trailer-iters
                                                           summarized-trailer-iters)
                 conversation-suffix-msgs (conversation-suffix summarized-trailer-iters
                                                               replay-target)
                 provider-messages (into (vec messages) conversation-suffix-msgs)
                 effective-messages-atom (atom provider-messages)
                 ;; Per-ITERATION rescue counter: escalating context-overflow folds.
                 context-recovery-state (atom {:attempts 0})
                 provider-output-started? (atom false)
                 effective-messages provider-messages
                 resolved-model pre-resolved-model
                 ;; Providers still serving an auth cooldown are excluded up front:
                 ;; the per-iteration rescue route below dies with the iteration, so
                 ;; only this seeding keeps a dead credential from being re-probed.
                 effective-routing (apply-auth-cooldown-routing routing)
                 ;; Mutates once only when exhausted auth recovery releases a dead provider.
                 iteration-routing (atom effective-routing)
                 iteration-result
                 ;; Per-iteration retry state. `:max-tokens-attempt` is separate
                 ;; from auth/context recovery so those policies do not consume
                 ;; the max-token budget; `:current-extra-body` carries its bump.
                 (loop
                   [attempt 0
                    max-tokens-attempt 0
                    current-extra-body extra-body
                    ;; `env` is threaded so the auth-refresh retry can
                    ;; reseat its `:router` to the rebuilt one (the
                    ;; in-flight env captured the pre-refresh router).
                    env environment]

                   (let
                     [attempt-env (hydrate-environment-router env)
                      result
                      (try
                        ;; Cheap FD-pressure guard BEFORE the provider call: a
                        ;; prior iteration's sandbox Python may have leaked
                        ;; descriptors (open without `with`), and this call is
                        ;; about to open an auth/refresh socket. Reclaim under
                        ;; pressure so a leak never starves the control plane.
                        (reclaim-fds-under-pressure!)
                        (reset! provider-output-started? false)
                        (run-iteration
                          attempt-env
                          @effective-messages-atom
                          {:iteration iteration
                           :reasoning-level reasoning-level
                           :reasoning-effort reasoning-effort
                           :routing @iteration-routing
                           :resolved-model resolved-model
                           :on-chunk (fn [chunk]
                                       (when (provider-output-chunk? chunk)
                                         (reset! provider-output-started? true))
                                       (emit-hook! on-chunk chunk "Provider chunk hook failed"))
                           :active-extensions active-exts
                           :answer-validation-context
                           {:user-request user-request
                            :previous-blocks (vec (mapcat (comp :blocks second) trailer-iters))}
                           :extra-body current-extra-body})
                        (catch Exception e
                          (cond
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
                            (let
                              [data (ex-data e)
                               prev-max
                               (or (:output-tokens data) (:max_tokens current-extra-body) 8192)
                               bumped (bumped-max-tokens-extra-body current-extra-body prev-max)]

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
                              ;; cannot loop forever.
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
                            ;; Auth 401/403 from a refreshable provider: adopt a
                            ;; peer credential or persist one forced refresh, then
                            ;; re-send. The exact attempt router supplies the
                            ;; rejected token; the next request boundary hydrates
                            ;; the new value without rebuilding shared routers.
                            (and (< (long attempt) (long MAX_AUTH_REFRESH_RETRIES))
                                 (auth-refreshable-error? e resolved-model)
                                 (try-refresh-provider-token! (:router attempt-env) resolved-model))
                            ::retry-auth-refresh
                            ;; Refresh/backoff failed or credentials were revoked.
                            ;; Release the dead provider, then let svar walk the fleet.
                            (auth-fallback-routing e @iteration-routing resolved-model)
                            (let
                              [fallback-routing
                               (auth-fallback-routing e @iteration-routing resolved-model)
                               ;; Persist the release ACROSS iterations. Without the
                               ;; cooldown the next iteration rebuilds routing from
                               ;; scratch and re-sends to the dead provider.
                               first-trip? (note-provider-auth-cooldown! (:provider resolved-model))
                               chunk (provider-retry-progress-chunk
                                       (inc (long iteration))
                                       e
                                       {:provider (:provider resolved-model)
                                        :model (or (:name resolved-model) (:model resolved-model))
                                        :reason :authentication-fallback
                                        :attempt 1
                                        :max-retries 1
                                        :delay-ms 0})]

                              (when first-trip?
                                (emit-hook! on-chunk chunk "Auth fallback progress hook failed"))
                              (tel/log! {:level (if first-trip? :warn :debug)
                                         :id ::auth-provider-fallback
                                         :data {:iteration iteration
                                                :provider (:provider resolved-model)
                                                :cooldown-ms AUTH_COOLDOWN_MS
                                                :status (:status (ex-data e))}}
                                        "Provider auth recovery exhausted; falling back")
                              {::retry-auth-fallback fallback-routing})
                            ;; Stream watchdog BEFORE any output: the provider
                            ;; took the request and never answered, so nothing
                            ;; was generated, billed or painted and the
                            ;; identical request may simply be made again. svar
                            ;; declines this retry at its HTTP layer (one retry
                            ;; there costs a whole timeout) and hands it to
                            ;; router-owned provider fallback, which has no
                            ;; second candidate under Vis' pinned sticky
                            ;; routing — so without this branch a provider that
                            ;; stayed silent kills a turn whose finished
                            ;; iterations are all still sitting there.
                            (pre-output-stream-retryable?
                              e
                              {:attempt attempt :output-started? @provider-output-started?})
                            (let
                              [delay-ms (pre-output-stream-backoff-ms attempt)
                               chunk (provider-retry-progress-chunk
                                       (inc (long iteration))
                                       e
                                       {:provider (:provider resolved-model)
                                        :model (or (:name resolved-model) (:model resolved-model))
                                        :reason :stream-watchdog-pre-output
                                        :attempt (inc (long attempt))
                                        :max-retries MAX_PRE_OUTPUT_STREAM_RETRIES
                                        :delay-ms delay-ms})]

                              (emit-hook! on-chunk chunk "Pre-output stream retry hook failed")
                              (tel/log! {:level :warn
                                         :id ::pre-output-stream-retry
                                         :data {:iteration iteration
                                                :provider (:provider resolved-model)
                                                :attempt (inc (long attempt))
                                                :max-retries MAX_PRE_OUTPUT_STREAM_RETRIES
                                                :delay-ms delay-ms
                                                :type (:type (ex-data e))}}
                                        (str "Stream watchdog fired before any output; "
                                             "re-issuing the same request"))
                              ::retry-pre-output-stream)
                            :else
                            (if-let
                              [recovery (context-overflow-recovery!
                                          {:error e
                                           :output-started? provider-output-started?
                                           :recovery-state context-recovery-state
                                           :ctx-atom (:ctx-atom environment)
                                           :turn-input-tokens (:input-tokens @usage-atom)
                                           :base-messages messages
                                           :trailer-iters trailer-iters
                                           :summaries (some-> (:ctx-atom environment)
                                                              deref
                                                              (get "session_summaries"))
                                           :replay-target replay-target
                                           :model (or (:name resolved-model)
                                                      (:model resolved-model))})]
                              (do (reset! effective-messages-atom (:messages recovery))
                                  (tel/log!
                                    {:level :warn
                                     :id ::context-overflow-emergency-fold
                                     :data (dissoc recovery :messages)
                                     :msg
                                     "Context overflow: retrying once with folded settled history"})
                                  ::retry-context-overflow)
                              (handle-iteration-exception! e
                                                           {:iteration iteration
                                                            :messages @effective-messages-atom
                                                            :routing @iteration-routing
                                                            :reasoning-level reasoning-level})))))]

                     (if-let
                       [[attempt* max-tokens-attempt*] (next-retry-counters result
                                                                            {:attempt attempt
                                                                             :max-tokens-attempt
                                                                             max-tokens-attempt})]
                       (let
                         [attempt* (long attempt*)
                          max-tokens-attempt* (long max-tokens-attempt*)]

                         (cond (and (map? result) (contains? result ::retry-max-tokens))
                               (recur attempt* max-tokens-attempt* (::retry-max-tokens result) env)
                               (and (map? result) (contains? result ::retry-auth-fallback))
                               (do (reset! iteration-routing (::retry-auth-fallback result))
                                   (recur attempt* max-tokens-attempt* current-extra-body env))
                               (= result ::retry-auth-refresh)
                               ;; Storage changed (or a peer already changed it). The
                               ;; next loop pass hydrates this same persistent router
                               ;; immediately before dispatch.
                               (recur attempt* max-tokens-attempt* current-extra-body env)
                               (= result ::retry-auth-backoff)
                               ;; Retry the same fresh token; propagation may still be settling.
                               (do (Thread/sleep (long (auth-propagation-backoff-ms attempt)))
                                   (recur attempt* max-tokens-attempt* current-extra-body env))
                               (= result ::retry-pre-output-stream)
                               ;; The provider never answered, so nothing about
                               ;; the route or the request needs changing.
                               (do (Thread/sleep (long (pre-output-stream-backoff-ms attempt)))
                                   (recur attempt* max-tokens-attempt* current-extra-body env))
                               ;; Stream retry: same route and env.
                               :else (recur attempt* max-tokens-attempt* current-extra-body env)))
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
                    (do (log-stage! :error
                                    iteration
                                    {:reason :cancelled
                                     :cancel-source (cancellation/cancel-reason (:cancel-token
                                                                                  environment))})
                        (let
                          [sticky (some-> (:turn-state-atom environment)
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
                    (let
                      [llm-provider-error (llm-provider-error-context iteration
                                                                      iteration-error-data)
                       error-feedback
                       (iteration-error-feedback iteration iteration-error-data user-request)
                       trace-entry {:iteration iteration :error iteration-error-data :final? false}
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
                         (let
                           [tc (iteration-token-cost err-api-usage
                                                     (:name resolved-model)
                                                     (:provider resolved-model))]
                           (cond->
                             {:session-turn-id session-turn-id
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
                              (cond->
                                {:selected (llm-id (:provider resolved-model)
                                                   (some-> (:name resolved-model)
                                                           str))
                                 :actual (llm-id (:provider resolved-model)
                                                 (some-> (:name resolved-model)
                                                         str))
                                 :fallback? false}
                                (seq (get-in iteration-error-data [:data :routed/trace]))
                                (assoc :fallback?
                                  true :trace
                                  (vec (get-in iteration-error-data [:data :routed/trace]))))
                              :cache-created-tokens (iteration-cache-created-tokens tc)}
                             tc
                             (assoc :tokens
                               (:tokens tc) :cost-usd
                               (:cost-usd tc)))))]

                      (ctx-loop/set-turn-state! environment :iteration-id err-iteration-id)
                      ;; A recoverable iteration error remains useful live feedback.
                      ;; Terminal failures instead produce exactly one canonical provider
                      ;; card below; emitting this raw chunk first made the TUI show an
                      ;; unformatted error followed by the formatted terminal card.
                      (when-not (::fatal-iteration-error iteration-result)
                        (emit-hook! on-chunk
                                    {:phase :iteration-error
                                     :iteration (inc (long iteration))
                                     :thinking err-reasoning
                                     :error iteration-error-data
                                     :done? true}
                                    "on-chunk (iteration error)"))
                      (if (::fatal-iteration-error iteration-result)
                        (let
                          [trace' (conj trace trace-entry)
                           fallback
                           (or (some-> (:error trace-entry)
                                       user-error-content)
                               (some-> (:error trace-entry)
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
                                 :empty-iteration-streak 0
                                 :messages (conj messages {:role "user" :content error-feedback})
                                 :llm-provider {:error llm-provider-error}
                                 :trace (conj trace trace-entry))))))
                  (let
                    [_ (accumulate-usage! (:api-usage iteration-result))
                     ;; The provider that ACCEPTED the request re-enters routing, never
                     ;; the pre-call guess: a turn rescued on a peer used to re-admit the
                     ;; dead credential and the next iteration re-probed it (issue #114).
                     _ (note-provider-request-ok! resolved-model iteration-result)
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
                     ;; so extension tools (`db_commit`,
                     ;; `db_push`, …) classify correctly without the
                     ;; engine hard-coding their head symbol.
                     ;;
                     ;; The HEAD `classify-form-tag` reads off the model's
                     ;; source is the snake_case Python CALL name
                     ;; (`db_push`), but `extension/op-tag` is keyed by
                     ;; canonical op keywords (`:db/push!`). The old
                     ;; `(keyword "db_push")` lookup never hit `:db/push!`,
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
                     forms-vec
                     (if (seq blocks) (ctx-engine/blocks->forms blocks cursor head-tag-resolver) [])
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
                     reinspection-attachments (into [] (mapcat :reinspect-attachments) blocks)
                     iteration-id
                     (persistance/db-store-iteration!
                       (:db-info environment)
                       ;; Price by the ACTUAL serving model (`:llm-model` =
                       ;; routed metadata), not the pre-resolved root — a
                       ;; fallback iteration must not bill at the selected
                       ;; model's rates.
                       (let
                         [tc (iteration-token-cost (:api-usage iteration-result)
                                                   (:llm-model iteration-result)
                                                   (:llm-provider iteration-result))]
                         (cond->
                           {:session-turn-id session-turn-id
                            :code (or block-code "")
                            :forms forms-vec
                            :attachments (attachment-storage/offload-attachments
                                           iteration-attachments)
                            :duration-ms (long (or (envelope-duration-ms (:envelope first-block))
                                                   0))
                            :llm-full-duration-ms (long (or (:duration-ms iteration-result) 0))
                            :thinking thinking
                            :assistant-prose assistant-prose
                            :answer (when final-result (answer-markdown (:answer final-result)))
                            :llm-provider (or (:llm-provider iteration-result)
                                              (:provider resolved-model))
                            :llm-model (:llm-model iteration-result)
                            :llm-returned-empty-code? (:llm-returned-empty-code? iteration-result)
                            :llm-routing (llm-routing-summary pre-resolved-model iteration-result)
                            :cache-created-tokens (iteration-cache-created-tokens tc)}
                           tc
                           (assoc :tokens
                             (:tokens tc) :cost-usd
                             (:cost-usd tc)))))
                     _ (ctx-loop/set-turn-state! environment :iteration-id iteration-id)
                     ;; Context end-of-iter bookkeeping.
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
                          (let
                            [result
                             (-> (merge {:answer (:answer final-result)
                                         :trace (conj trace trace-entry)
                                         :iteration-count (inc (long iteration))
                                         :utilization (let
                                                        [u @usage-atom
                                                         req (if (pos? (long (:iter-count u)))
                                                               (long (:last-iter-input u))
                                                               (long (:previous-request-input u)))]

                                                        (ctx-engine/utilization
                                                          req
                                                          effective-context-limit
                                                          (:input-tokens u)
                                                          ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                        (finalize-cost))
                                 (attach-llm-routing-summary pre-resolved-model iteration-result))]
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
                            (let
                              [answer (or (some-> (:turn-state-atom environment)
                                                  deref
                                                  :best-answer
                                                  :value)
                                          {:answer empty-replies-give-up-text})]
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
                                          (let
                                            [u @usage-atom
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
                                           :empty-iteration-streak empty-streak
                                           :trace (conj trace trace-entry)}))))
                        (do
                          (log-stage! :iteration/stop
                                      iteration
                                      {:blocks (count blocks)
                                       :errors (count (filter :error blocks))
                                       :times (mapv block-duration-ms blocks)})
                          (let
                            [_ blocks
                             ;; ctx-diff for THIS iteration: the standing context
                             ;; AFTER its code ran, captured ONLY if it changed since
                             ;; the model last saw it (this iter started an nREPL,
                             ;; switched model, added a dir, …). It rides INSIDE this
                             ;; iteration's <results> message (see
                             ;; `iteration-results-message`) and advances the running
                             ;; baseline, so the change is attributed to the code that
                             ;; caused it — append-only, no stray context messages.
                             iter-ctx-diff
                             (let
                               [;; util-inclusive: live token usage rides as a cheap
                                ;; appended `session["utilization"] = …` delta (the
                                ;; frozen block stays util-free for cache stability).
                                cur (ctx-loop/render-block! environment ctx-renderer/ctx-delta-map)
                                prev @last-context-atom
                                rebase? (true? (:pending? (some-> (:session-rebase-atom environment)
                                                                  deref)))]

                               (when (and cur (or rebase? (not= cur prev)))
                                 (reset! last-context-atom cur)
                                 (if rebase?
                                   (rebase-session-context! standing-ctx-atom
                                                            (:session-rebase-atom environment)
                                                            cur)
                                   (do
                                     ;; carry the baseline ACROSS turns so the next turn
                                     ;; diffs against the last-emitted state, not a re-render.
                                     (some-> standing-ctx-atom
                                             (swap! assoc :baseline cur))
                                     ;; structural Python delta (session[…] = … / del),
                                     ;; not the whole <context> block — append-only.
                                     (ctx-renderer/render-ctx-delta prev cur)))))
                             ;; The immediately preceding provider call consumed any
                             ;; reinspection image. Clear old queues before carrying
                             ;; history forward: reinspection is exactly one request.
                             next-recent (conj
                                           (mapv (fn [[pos rec]]
                                                   [pos (dissoc rec :reinspect-attachments)])
                                                 (or trailer-iters []))
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
                                             :reinspect-attachments reinspection-attachments
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
                                             ;; Tool calls for this iteration — iteration-results-message
                                             ;; pairs one `tool_result` block per call's :id (the API requires
                                             ;; every tool_use be answered).
                                             :tool-calls (:tool-calls iteration-result)
                                             :preserved-thinking/replay? true}])]

                            ;; The model controls when the turn is complete. Repeated tool
                            ;; calls remain ordinary non-terminal iterations.
                            (when on-chunk
                              (on-chunk {:phase :iteration-final
                                         :iteration (inc (long iteration))
                                         :thinking thinking
                                         :assistant-prose assistant-prose
                                         :iteration-id iteration-id
                                         :attachment-count (count iteration-attachments)
                                         :final nil
                                         :done? false}))
                            (recur (merge (dissoc loop-state :llm-provider)
                                          {:iteration (inc (long iteration))
                                           :empty-iteration-streak 0
                                           :messages messages
                                           :trace (conj trace trace-entry)
                                           :trailer-iters next-recent}))))))))))))))))

(defn- slash-ctx-for-env
  "Build the slash dispatch ctx from a turn env. Pure data; carries
   the channel/session/workspace coordinates the slash handlers read."
  [env user-request]
  (let
    [db-info
     (:db-info env)

     state-id
     (or (:session/state-id env)
         (when db-info
           (some-> (:session-id env)
                   (persistance/db-latest-session-state-id db-info))))]

    (cond->
      {:channel/id (or (:channel env) :tui)
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
  (cond result (let
                 [title
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
  [env user-request slash-result loop-opts]
  (let
    [db-info
     (:db-info env)

     ;; A slash turn never enters `iteration-loop` (the only path that streams
     ;; live `:progress` activity), so the zero-iterations live bubble otherwise
     ;; claims Vis is "calling the provider". Emit ONE `:slash` phase chunk BEFORE
     ;; the (possibly slow) local dispatch so the tracker renders
     ;; `Vis is running: /<name>` instead — a PURE command never touches a provider.
     on-chunk
     (or (:on-chunk loop-opts) (get-in loop-opts [:hooks :on-chunk]))

     slash-label
     (or (re-find #"^/\S+" (str/trim (str user-request))) (str/trim (str user-request)))

     _
     (when (fn? on-chunk)
       (try (on-chunk {:phase :slash :iteration 1 :slash slash-label})
            (catch Throwable t
              (tel/log! {:level :warn
                         :id ::slash-progress-emit-failed
                         :data {:slash slash-label :error (ex-message t)}}))))

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
    (let
      [scope
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
         (let
           [stamped (ctx-loop/stamp-cursor env @ca)
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
      (persist-turn-outcome! db-info
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
  (let
    [;; Persist EVERY image the user attached to this turn as durable
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

     ;; Deferred auto-title: the LLM naming call happens HERE, after the
     ;; foreground turn is done, so it can never take a rate-limited
     ;; gateway's slot away from the user's own request (Blockether/vis#71).
     ;; A no-op unless `titling.mode` is `llm`; the deterministic title is
     ;; already on the session either way.
     _
     (titling/after-turn-auto-title! env user-request)

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
       (let
         [stamped (ctx-loop/stamp-cursor env @ca)
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

     turn-content
     ;; A FAILED turn's fallback answer may not be answer-shaped (a
     ;; provider-exhaustion turn hands back a raw error value, not prose). NEVER
     ;; let `answer-content` throw here: an unguarded throw propagates out of
     ;; `send!` and MASKS the real provider failure — the 529/overload or
     ;; rate-limit that actually killed the turn, preserved in `:trace` — with
     ;; the misleading "Final answer must be canonical content or Markdown
     ;; prose". On a throw, persist no content; the gateway rebuilds honest
     ;; content from the trace error (see gateway/state).
     (try (content/answer-content (:answer result)) (catch Throwable _ []))

     _
     (persist-turn-outcome! (:db-info env)
                            session-turn-id
                            {:content turn-content
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
   or nil when `text` is NOT a bang. `!<cmd>` invokes the shell tool's synchronous
   run op; `!&<cmd>` invokes its background op under an auto-generated resource id.
   A blank command (a bare `!`) is ordinary
   prose, so it returns nil and the message runs as a normal turn."
  [text]
  (when (string? text)
    (let [t (str/triml text)]
      (cond (str/starts-with? t "!&") (let [cmd (str/trim (subs t 2))]
                                        (when (seq cmd)
                                          {:kind :bg
                                           :cmd cmd
                                           :id (str "background-"
                                                    (subs (str (java.util.UUID/randomUUID)) 0 8))}))
            (str/starts-with? t "!") (let [cmd (str/trim (subs t 1))]
                                       (when (seq cmd) {:kind :run :cmd cmd}))))))

(defn- bang-card->markdown
  "Combine a native shell op-card `{:summary :body}` into the answer Markdown a
   `!`/`!&` turn shows as its answer bubble."
  [{:keys [summary body]}]
  (let
    [summary
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
   user-owned `shell` toggle), then persist ONE synthetic iteration —
   the SAME shape `run-slash-turn!` writes — whose form carries the shell RESULT
   map, native-tool identity, and `:tag :user-shell`. The op-card renders as the
   answer bubble (channels suppress the redundant trace by that tag), and the
   persisted `:result` rides later prompts' prior-turn context exactly as a
   model-issued `shell` call does across turns. Returns the same
   shape `iteration-loop` would (so callers don't special-case bang turns)."
  [env user-request {:keys [kind cmd id]} loop-opts]
  (let
    [db-info
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
     (toggles/enabled? "shell")

     ;; The renderer is looked up by the tool NAME, and a bang picks the same
     ;; tool the model would — there is only one: a background bang is a run
     ;; whose caller waits for nothing.
     tool-name
     "shell"

     t0
     (System/currentTimeMillis)

     ;; Run the shell tool. `requiring-resolve` keeps foundation-shell a
     ;; DROPPABLE plug-in (nil when the jar is absent) and avoids a compile-time
     ;; cycle; the "shell" toggle gate is applied HERE (the symbol's own
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
       (try (let
              [shell-fn (requiring-resolve
                          (if (= kind :bg)
                            'com.blockether.vis.internal.foundation.shell/shell
                            ;; A `!cmd` bang PRINTS the command's output, so it is the one
                            ;; caller that genuinely blocks. The tool no longer takes a wait
                            ;; knob — waiting is a handle method — so the bang path calls the
                            ;; INTERNAL blocking runner directly instead of a request flag.
                            'com.blockether.vis.internal.foundation.shell/run-blocking))]
              ;; Calling the shell var directly skips the symbol-call seam, so the
              ;; workspace view stays unbound and `resolve-dir` falls back to the
              ;; PROCESS cwd — a bang inside a draft would then run on trunk.
              (extension/with-context
                {:env env}
                (if (= kind :bg) (shell-fn env cmd {"id" id}) (shell-fn env cmd {}))))
            (catch Throwable t
              (tel/log! {:level :warn :id ::bang-run-threw :data {:cmd cmd :error (ex-message t)}})
              {:result nil :error {:message (or (ex-message t) (str t))}})))

     t1
     (System/currentTimeMillis)

     result-map
     (:result envelope)

     err
     (:error envelope)

     display
     ;; A bang PRINTS its command's output, so the shell card — exit-code headline
     ;; plus COMMAND / STATUS / STDOUT — IS the answer here. It is built by
     ;; calling shell's renderer DIRECTLY: one function, no symbol table, no
     ;; registry lookup. Every other result is painted from its own data.
     (when (some? result-map)
       (try (let
              [render
               (requiring-resolve
                 'com.blockether.vis.internal.foundation.shell/render-shell-run-result)

               card
               (render result-map)]

              (when (map? card)
                (let
                  [summary
                   (some-> (:summary card)
                           str
                           str/trim
                           not-empty)

                   body
                   (some-> (:body card)
                           str
                           form/clip-to-wire)]

                  (when (or summary body) {:summary summary :body body}))))
            (catch Throwable _ nil)))

     answer-md
     (cond (not enabled?) (str "**Shell layer is OFF.** Only you can enable it: settings dialog"
                               " → 'Shell commands'. Then `"
                               cmd
                               "` will run.")
           (some? err) (str "**shell error**\n\n" (strutil/fenced (or (:message err) (pr-str err))))
           display (bang-card->markdown display)
           :else (str "_ran `" cmd "`_"))

     block
     (cond->
       {:code
        (if (= kind :bg)
          (str "await shell({\"command\": " (pr-str cmd) ", \"wait\": 0, \"id\": " (pr-str id) "})")
          (str "await shell({\"command\": " (pr-str cmd) "})"))
        :svar/tool-call-id (str "bang-" (subs (str (java.util.UUID/randomUUID)) 0 8))
        :vis/tool-name tool-name
        ;; The card is titled by the op that produced it, exactly like a printed
        ;; result: a `!cmd` bubble reads SHELL, not RESULT.
        :op tool-name
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
       (let
         [stamped (ctx-loop/stamp-cursor env @ca)
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
    (persist-turn-outcome! db-info
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
   provider-contributed templates like `/<name>`): when a
   template matches, the expanded text runs as a NORMAL LLM turn.
   Registered slashes always win over templates."
  [env user-request loop-opts]
  (when-not (map? env) (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let
    [;; Re-resolve the active workspace from the session's CURRENT pin so a
     ;; mid-session `/draft new | apply | abandon` takes effect THIS turn.
     ;; The cached env was built at session start (on trunk); without this
     ;; the agent keeps editing trunk after entering a draft.
     env
     (or (when-let [db (:db-info env)]
           (when-let
             [sid (or (:session/state-id env)
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
                 :workspace/root (:root ws)))))
         env)

     ;; Turn-start health gate: probe LOCAL providers (Ollama/LM Studio)
     ;; and sink unreachable ones to the END of this turn's router, so a
     ;; dead local endpoint can't catch the turn or an svar fallback.
     ;; The demotion is per-turn (the env binding is local, so a provider
     ;; that comes back reappears next turn) and raises an engine warning
     ;; so the user knows. Remote providers are not network-checked here.
     env
     (let
       [{:keys [router demoted]}
        (health-gated-router (:router env) :turn)

        env'
        (assoc env :router router)]

       (when (seq demoted)
         (when-let [ca (:ctx-atom env)]
           (swap! ca update
             "engine_warnings"
             (fnil conj [])
             {:code :provider-unreachable
              :anchor ["session_routing"]
              :message (str "Local provider(s) "
                            (str/join ", " (map name demoted))
                            " unreachable — demoted to last-resort for this turn.")})))
       env')

     slash-result
     (extension/with-context
       {:env env}
       (try (slash/dispatch env (slash-ctx-for-env env user-request) user-request)
            (catch Throwable t
              (tel/log! {:level :warn
                         :id ::slash-dispatch-threw
                         :data {:user-request user-request :error (ex-message t)}})
              {:handled? false})))]

    (if-let [bang (parse-bang user-request)]
      (run-bang-turn! env user-request bang loop-opts)
      (if (:handled? slash-result)
        (if-let
          [expansion (when (= :unknown (:reason slash-result))
                       (extension/with-context {:env env}
                                               (try (prompt-templates/expand env user-request)
                                                    (catch Throwable t
                                                      (tel/log! {:level :warn
                                                                 :id ::template-expand-threw
                                                                 :data {:user-request user-request
                                                                        :error (ex-message t)}})
                                                      nil))))]
          (let
            [turn-env (if-let [root (:project-root expansion)]
                        (assoc env
                          :workspace/root root
                          :workspace (assoc (:workspace env) :root root))
                        env)]
            (extension/with-context {:env turn-env}
                                    (run-normal-turn! turn-env (:text expansion) loop-opts)))
          (run-slash-turn! env user-request slash-result loop-opts))
        (run-normal-turn! env user-request loop-opts)))))

(defn custom-bindings
  "Current custom sandbox bindings {sym -> value}."
  [env]
  (some-> (:state-atom env)
          deref
          :custom-bindings))

;; Prepare turn context

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
  (let
    [model
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

(defn- router-with-pinned-model
  "Teach `router` about a session-pinned model its CONFIG does not list.

   Provider `:models` in config is a curated subset; the gateway deliberately
   accepts any model the provider's LIVE catalog exposes, and the model picker
   offers exactly those. `forced-routing-for-pref` however validates the pin
   against `:models`, so a live-catalog pick degraded to `{}` and the turn silently
   ran the DEFAULT model — the pick looked applied in the UI and never bound. svar
   `resolve-routing` throws on a model it does not know, so the pin has to be
   MATERIALISED instead: synthesize a minimal `{:name model}` entry on the pinned
   provider (the same shape catalog hydration produces) and let provider-level
   settings inherit as usual.

   Returns `router` unchanged when there is no pin, the provider is unknown, or it
   already lists the model."
  [router provider model]
  (let
    [model
     (some-> model
             str
             str/trim
             not-empty)

     pid
     (some-> provider
             name
             keyword)

     ps
     (vec (:providers router))

     idx
     (when (and model pid)
       (first (keep-indexed (fn [i p]
                              (when (= (:id p) pid) i))
                            ps)))

     p
     (when idx (nth ps idx))]

    (if (and p (not (some #(= (:name %) model) (:models p))))
      (assoc router :providers (assoc ps idx (update p :models (fnil conj []) {:name model})))
      router)))

(defn- router-for-pinned-provider
  "Hoist `provider-id`'s entry to the router HEAD and renumber the fleet.

   `router-for-model` alone cannot do this: when two providers expose the SAME
   model name they tie on rank and the stable sort keeps config order. A session
   pinned to `github-copilot-individual/gpt-5.4` therefore CALLED copilot (the
   forced `:routing` binds that) while `resolve-effective-model` read the head —
   openai-codex — so the turn card, the cost row and every provider-error card
   named (and PRICED) the wrong provider. Hoisting the pinned provider makes
   display/cost attribution agree with the call, and puts the pinned provider
   first in the fallback order — which needs the `:priority` renumbering too,
   since svar drops `:force-provider` on an auth fallback and re-sorts by
   priority alone."
  [router provider-id]
  (let
    [pid
     (some-> provider-id
             name
             keyword)

     ps
     (:providers router)]

    (if-let [p (and pid (first (filter #(= (:id %) pid) ps)))]
      (assoc router
        :providers (providers/reprioritize-providers (into [p] (remove #(= (:id %) pid)) ps)))
      router)))

(defn- prepare-turn-context
  "Validates inputs, resolves sandbox bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let
    [{:keys [spec model max-context-tokens system-prompt debug? hooks cancel-token eval-timeout-ms
             reasoning-default reasoning-effort routing extra-body]
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
    (let
      [;; Per-session model preference: when the caller passes no explicit
       ;; `:model`, fall back to the persisted per-session choice (set by
       ;; ANY channel — web picker or TUI — via `session-model/set-model!`).
       ;; This is what unifies routing across channels: the engine, not the
       ;; channel, applies the session's pick.
       ;; The preference is {:provider :model}: the MODEL drives display/cost
       ;; (router-for-model + resolve-effective-model below) AND, crucially,
       ;; gets forced into svar's `:routing` (forced-routing-for-pref) so the
       ;; pick actually binds — reordering the router vector alone does NOT
       ;; (svar selects by provider :priority, not vector order).
       session-pref (when (and (nil? model) (:session-id env))
                      (session-model/model-of (:db-info env) (:session-id env)))
       ;; ONE canonical spelling of the pin from here on. `forced-routing-for-pref`
       ;; and `router-with-pinned-model` trim; `router-for-model` does NOT — so a
       ;; pref carrying stray whitespace (a hand-edited DB row, a client that pads
       ;; the field) BOUND the right model while the display/cost root fell back to
       ;; the pinned provider's first model: the turn card named a model the turn
       ;; never ran.
       model (some-> (or model (:model session-pref))
                     str
                     str/trim
                     not-empty)
       ;; A persisted provider belongs only to the persisted model it was saved
       ;; with. Never combine it with an explicit caller model: that creates a
       ;; synthetic provider/model pair and can silently degrade to config order.
       ;; Same canonical spelling for the provider half of the pin: padded here
       ;; and `forced-routing-for-pref` matched NO provider, dropping `:provider`
       ;; from the forced routing while the display root still named it — the two
       ;; halves of the pin disagreeing is exactly what this binding prevents.
       ;; Keywords survive as their bare name (`:lmstudio` -> "lmstudio"), never
       ;; as `":lmstudio"`.
       pref-provider (let [p (:provider session-pref)]
                       (some-> (if (keyword? p) (name p) p)
                               str
                               str/trim
                               not-empty))
       ;; The pin the session actually BINDS (provider+model, validated against
       ;; the router). Computed once: it drives BOTH the display/cost root
       ;; (env-router below) and svar's forced `:routing`, so the two can never
       ;; name different providers again.
       ;; A pick may name a model only the provider's LIVE catalog lists (the
       ;; picker offers those); materialise it on the pinned provider or the pin
       ;; validates away and the turn silently runs the default model.
       pref-router (router-with-pinned-model (:router env) pref-provider model)
       pref-forced (forced-routing-for-pref pref-router pref-provider model)
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
       env-router (cond-> pref-router
                    (and model (not (str/blank? (str model))))
                    (router-for-model model)

                    ;; …and a pinned PROVIDER hoists that provider, so a model
                    ;; name two providers share attributes to the one being called.
                    (:provider pref-forced)
                    (router-for-pinned-provider (:provider pref-forced)))
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
       routing (let [merged (merge pref-forced (or routing {}))]
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
                                        [:workspace/root :workspace/id :workspace/sandbox? :vcs/kind
                                         :vcs/ref :vcs/mainline])
       ;; Reseat :router to the preference-hoisted one — run-iteration-phase
       ;; routes off THIS environment's router, not the ctx :router below.
       environment (cond->
                     (assoc env
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
                     ;; routing.
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

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, session-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec max-context-tokens system-prompt hooks cancel-atom
           cancel-token reasoning-default reasoning-effort routing extra-body turn-features
           workspace-overrides]}]
  (let
    [iteration-result
     (run-turn! environment
                user-request
                (cond->
                  {:output-spec spec
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
                                                             ["input" "output" "reasoning" "cached"
                                                              "total"])))))
       (when extra-cost
         (swap! total-cost-atom (fn [acc]
                                  (merge-cost-maps acc extra-cost)))))]

    {:iteration-result iteration-result
     :session-turn-id session-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom total-cost-atom
     :merge-cost! merge-cost!}))

(defn- finalize-turn-result
  "Updates DB turn record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model / N
   iteration / duration / tokens / $total` after a restart."
  [{:keys [db-info root-model root-provider reasoning-effort]}
   {:keys [session-turn-id start-time iteration-count status status-id trace locals answer
           confidence reasoning utilization total-tokens-atom total-cost-atom]}]
  (let
    [duration-ms
     (util/elapsed-since start-time)

     eval-evidence
     (turn-eval-evidence reasoning-effort trace)

     cost-with-model
     (cond-> @total-cost-atom
       (and root-model (not (get @total-cost-atom "model")))
       (assoc "model" (str root-model))

       (and root-provider (not (get @total-cost-atom "provider")))
       (assoc "provider" (if (keyword? root-provider) (name root-provider) (str root-provider))))]

    (if status
      ;; failure path - surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do
        (log-stage! :turn/complete
                    0
                    {:duration-ms duration-ms :iteration-count iteration-count :status status})
        (let
          [fallback-answer
           (:result answer answer)

           failure-content
           (failed-turn-content fallback-answer trace)]

          (persist-turn-outcome! db-info
                                 session-turn-id
                                 {:content failure-content
                                  ;; First-class structured error for a failed turn.
                                  :error (or (turn-error-data fallback-answer)
                                             (turn-error-data failure-content))
                                  :iteration-count iteration-count
                                  :duration-ms duration-ms
                                  :status status
                                  :tokens @total-tokens-atom
                                  :cost cost-with-model})
          (cond->
            {:answer fallback-answer
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
          (persist-turn-outcome! db-info
                                 session-turn-id
                                 {:content (content/answer-content answer)
                                  :iteration-count iteration-count
                                  :duration-ms duration-ms
                                  :status :success
                                  :tokens @total-tokens-atom
                                  :cost cost-with-model})
          (cond->
            {:answer answer
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

;; Public entry point

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
   (let
     [ctx
      (prepare-turn-context environment messages opts)

      {:keys [eval-timeout-ms debug? user-request root-model db-info environment-id]}
      ctx]

     (binding
       [rt/*rlm-context*
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
         (let
           [start-time
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

;; Environment lifecycle + system prompt

;; Helpers

;; Public env accessors

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
     (let
       [installed (vec (or (some-> (:extensions environment)
                                   deref)
                           []))
        active-set (set (map :ext/name active-extensions))]

       (doseq
         [ext installed
          :let [alias (extension/ext-alias-symbol ext)
                by-sym (into {}
                             (map (juxt :ext.symbol/symbol identity) (extension/ext-symbols ext)))]
          [sym f] (try (extension/wrap-extension ext environment) (catch Throwable _ nil))]

         ;; Aliased extensions (`:ext.engine/alias 'clj`) bind into the FLAT
         ;; Python sandbox as `<alias>_<name>` — the snake form of the
         ;; `alias/name` shape (clj_eval, db_status, search_web, br_open). Without
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
                 ;; py-name, so `doc(db_status)` / `doc(mcp_servers)` /
                 ;; `apropos("mcp")` carry real descriptions. ALIASED extensions
                 ;; bind here (per turn), NOT at context creation, so the eager
                 ;; `build-agent-context` seed never saw them.
                 (env/set-python-binding-doc! python-context
                                              target
                                              (extension/symbol-doc-text (get by-sym sym)))
                 ;; ...and its declared parameter list, so `inspect.signature` /
                 ;; `help` on an aliased tool answer with real parameters
                 ;; instead of the async trampoline's own `(*a, **k)`.
                 (env/set-python-binding-signature! python-context
                                                    target
                                                    (extension/symbol-signature (get by-sym sym)))
                 ;; ...and the keys its options dict must carry, so `doc(name)`
                 ;; states requiredness for an aliased tool too.
                 (env/set-python-binding-keys! python-context
                                               target
                                               (extension/symbol-keys-line (get by-sym sym))))
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
    (let
      [registered (into #{} (map :ext/name) @(:extensions environment))
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
                                     (let
                                       [ns-sym
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

;; Environment Lifecycle

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
  ;; Drop this session from the SHARED gateway egress proxy's registry. The shared
  ;; proxy + CA are daemon-lifetime (internal.gateway-sandbox/shutdown!), not
  ;; per-session, so nothing is stopped here — only this session's policy is removed.
  (when-let [tok (:sandbox-token environment)]
    (gateway-sandbox/unregister-session! tok))
  (when-let [tok (:repl-sandbox-token environment)]
    (gateway-sandbox/unregister-session! tok))
  (process-jail/unregister-session-jail! (:session-id environment))
  (when-let [python-context (:python-context environment)]
    (try (.close ^Context python-context true) (catch Throwable _ nil)))
  (when (and (:db-info environment) (not (false? (:owns-db? environment))))
    (persistance/db-dispose-connection! (:db-info environment))))

;; sub_loop runtime — a child agentic loop (slice C). The model writes Python:
;; it slices `context` into a focused `subctx` and calls
;; `sub_loop(prompt, subctx, {"model": …})`. The child is a CHILD session reusing
;; create-environment (own done/bindings/ctx + forked Context on the shared Engine),
;; on its OWN workspace (rift clone where supported, else shared root), optionally
;; on a cheaper proposed model. On close its workspace diff merges back and the
;; result (status + evidence + produced facts + what-changed) returns to the parent.

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


(defn- project-child-result
  "Run the child turn, merge its edits back (rift path), and project the result
   the coordinator merges by `task_id`: the model-supplied focus id, status (a
   STRING — python-facing, never a keyword), answer, and what changed."
  [child-env {:keys [db-info child-ws rift? subctx prompt system-prompt]}]
  (let
    [;; A harness AGENT dispatch rides its markdown body in as the child's
     ;; system-prompt addendum (build-system-prompt appends it to CORE);
     ;; ordinary sub_loops pass none.
     turn-opts
     (if (seq (str system-prompt)) {:system-prompt (str system-prompt)} {})

     result
     (run-turn! child-env (str prompt) turn-opts)

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
  (let
    [depth (inc (long (or (some-> parent-env
                                  :depth-atom
                                  deref)
                          0)))]
    (when (> (long depth) (long MAX-SUBLOOP-DEPTH))
      (throw (ex-info (str "sub_loop depth cap (" MAX-SUBLOOP-DEPTH ") exceeded")
                      {:type :vis/subloop-depth-exceeded :depth depth})))
    (let
      [db-info (:db-info parent-env)
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
                                                   ;; Security is inherited by VALUE. A child may use a
                                                   ;; different workspace/model, never a newer vis.yml.
                                                   :security-policy (:security-policy parent-env)
                                                   :seed-ctx (subctx->seed-ctx subctx)}})
                      dispose-environment!
                      :dispose
                      ws-id
                      (fn [child-env]
                        (cond->
                          (project-child-result child-env
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
  (let
    [focus (some-> (get spec "subctx")
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
            (let
              [r
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
            (let
              [r
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
  (let
    [specs
     (vec specs)

     sem
     (java.util.concurrent.Semaphore. MAX-PARALLEL-SUBLOOPS)

     futs
     (mapv (fn [spec]
             (future (.acquire sem)
                     ;; DEFENCE IN DEPTH — `run-spec!` already folds a throw into a failed
                     ;; result, but ANYTHING escaping it (an Error, or a failure while
                     ;; BUILDING that result) used to come out of `deref` below as an
                     ;; ExecutionException: it sank the whole batch AND skipped the sibling
                     ;; cancel, so every other child kept running as an orphaned LLM turn
                     ;; whose result nobody ever read.
                     (try (run-spec! parent-env spec)
                          (catch Throwable t (failed-subloop-result spec t))
                          (finally (.release sem)))))
           specs)]

    ;; Settle in input order — but when the COORDINATING thread is interrupted
    ;; (turn cancel / eval timeout), hard-cancel every child sub-loop before
    ;; propagating. Otherwise cancelled parallel sub-loops kept running as
    ;; orphaned full LLM turns (same leak the gather settle loop had). ANY escape
    ;; must take the batch with it, not just an interrupt.
    (try (mapv deref futs)
         (catch Throwable t
           (doseq [f futs]
             (try (future-cancel f) (catch Throwable _ nil)))
           (throw t)))))

(defonce ^:private last-good-security-snapshot
  ;; Retains the most recent VALID security snapshot so an invalid live config
  ;; edit never tears down running sessions — see `security-config-snapshot`.
  (atom nil))

(defn- security-config-snapshot
  "Read, validate, resolve, and hash security configuration once for a ROOT
   environment. Child environments inherit this exact immutable value.

   RESILIENT: a wrong config must never kill a running session. `load-config-raw`
   already loads leniently; if BUILDING the snapshot still fails for ANY reason
   — a `:vis/invalid-config` contract violation, an internal derived-policy
   assertion, or an unexpected error while resolving paths — we log ONE warning
   and reuse the last-good snapshot (or a minimal deny-safe `{}` snapshot on first
   load) instead of throwing. Nothing here ever re-throws. The next `save!`/`/reload`
   with a valid config replaces it."
  []
  (try (let [snap (security-policy/snapshot (or (config/load-config-raw) {}))]
         (reset! last-good-security-snapshot snap)
         snap)
       (catch Throwable e
         (let
           [invalid? (and (instance? clojure.lang.ExceptionInfo e)
                          (= :vis/invalid-config (:type (ex-data e))))]
           (tel/log! {:level :warn
                      :id ::security-config-invalid
                      :data
                      (if invalid? {:problems (:problems (ex-data e))} {:error (ex-message e)})
                      :msg (str "security config could not be applied; "
                                (if @last-good-security-snapshot
                                  "keeping the last-good policy"
                                  "falling back to a deny-safe policy")
                                " so the session survives")})
           (let
             [problems (try (config/config-problems) (catch Throwable _ nil))
              base (or @last-good-security-snapshot
                       (try (security-policy/snapshot {}) (catch Throwable _ {})))]

             (assoc base
               :config-error {"source" (or (:source (ex-data e)) "vis.yml / ~/.vis/state.yml")
                              "message" (str "The live config on disk could not be applied; "
                                             (if @last-good-security-snapshot
                                               "the last-good policy is in effect."
                                               "a deny-safe policy is in effect."))
                              "problems" (if (seq problems) (vec problems) [(ex-message e)])
                              "hint"
                              (str "Fix the keys above in vis.yml or ~/.vis/state.yml, then run "
                                   "/reload. Keys are snake_case strings; the config is closed, "
                                   "so unknown or renamed keys are rejected.")}))))))

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
  (when (and (:parent-db-info child) (nil? (:security-policy child)))
    (throw (ex-info "Child environment requires the parent's security-policy snapshot"
                    {:type :vis/missing-child-security-policy})))
  ;; `child` (a sub_loop child env) carries:
  ;;   :parent-db-info  reuse the parent's DB connection (don't open/close one)
  ;;   :security-policy inherit the parent's immutable policy (required)
  ;;   :depth           starting recursion depth (parent depth + 1)
  ;;   :seed-ctx        initial ctx-atom value (the model-supplied subctx) — used
  ;;                    instead of a DB restore
  ;; A different `router` (model) can be passed for the child to optimize cost
  ;; (cheap/fast model for an easy subtask) — first-class, nothing special needed.
  (let
    [depth-atom
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

     ;; Routing digest surfaced in the model-facing ctx (`routing`): the CURRENT
     ;; model + provider, nothing more. The provider/model CATALOG is deliberately
     ;; NOT shipped — child dispatch (`sub_loop`) is unadvertised, so nothing could
     ;; act on it, and it cost ~445 tokens on EVERY request. STRING-KEYED: this
     ;; digest lands in ctx as `session_routing` and crosses the Python boundary.
     routing-digest
     (cond-> {"model" root-model}
       root-provider
       (assoc "provider" (name root-provider)))

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
         resolved-session-id (some->> (persistance/db-latest-session-state-id db-info
                                                                              resolved-session-id)
                                      (persistance/db-workspace-for-session db-info))
         ;; New session, caller pre-spawned a workspace
         ;; (e.g. /workspace slash spawn-branch path).
         workspace-id (persistance/db-workspace-get db-info workspace-id)
         ;; New session, no pre-spawn: clone cwd.
         :else (workspace/ensure-workspace! db-info {})))

     session-id
     (or resolved-session-id
         (persistance/db-store-session! db-info
                                        (cond->
                                          {:channel (or channel :tui)
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

     ;; Large folds already invalidate the provider cache. Once their cumulative
     ;; newly reclaimed wire crosses the threshold, rebase the standing session
     ;; snapshot too instead of retaining an unbounded chain of historical deltas.
     session-rebase-atom
     (atom {:reclaimed-tokens 0 :pending? false})

     ;; ONE model-driven context-compaction verb, recording a
     ;; `:session/summaries` intent the wire applies via `apply-summaries`:
     ;;
     ;;   fold_session("tN/iM", "what this step established")  — the KEY names
     ;;     the step, the GIST keeps its conclusion: the step collapses into
     ;;     that one distilled line.
     ;;   fold_session("tN/i1-i56", "…")  — ONE key string folds a whole window
     ;;     (`-tN/iM` everything through it, `tN/iM-` everything since it, `tN`
     ;;     a whole turn, commas union several). See `ctx-engine/fold-key` for
     ;;     the grammar.
     ;;   fold_session("tN/iM")  — the gist is OPTIONAL: OMIT it to just
     ;;     DISCARD the step outright (an approach you abandoned, a read you
     ;;     misread) where keeping even a summary would mislead. Replaces the
     ;;     old `session_drop`.
     ;;
     ;; It records a `:session/summaries` intent the wire applies via
     ;; apply-summaries, and RETURNS a visible confirmation (not the silent
     ;; sentinel) so the fold shows in the Python result. See
     ;; `compaction-verbs` for the intent shape + range handling.
     compaction
     (compaction-verbs ctx-atom session-rebase-atom)

     ;; maki-style in-program concurrency: run each thunk (a Python callable,
     ;; e.g. `lambda: rg({...})`) on a VIRTUAL THREAD and return results in
     ;; order. GraalPy releases its lock on blocking I/O, so I/O-bound tool
     ;; calls genuinely overlap inside ONE python_execution call. Dynamic sink
     ;; bindings (tool-event/render) are conveyed via `bound-fn*` so tools
     ;; called concurrently still render. ALL thunks run; if several FAIL,
     ;; every future is still settled (we don't abort at the first throw) and
     ;; ONE aggregated error names EVERY failure by slot index — so the model
     ;; fixes them all in one pass instead of one-per-iteration. No failures →
     ;; results in order, exactly as before.
     gather-fn
     (fn gather [& thunks]
       (let
         [thunks
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
                  (.submit ^ExecutorService @gather-executor
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
     ;; every thunk on the SAME bounded platform pool with the SAME real overlap,
     ;; but NEVER throws an aggregate on failure: each slot returns a per-call
     ;; SENTINEL — `{"__vis_ok__" true "__vis_val__" v}` on success, or
     ;; `{"__vis_ok__" false "__vis_exc__" <Throwable>}` on failure. The raw
     ;; Throwable crosses back as a host object (env/->clj `asHostObject`), so
     ;; the loop maps it through the SAME `python-op-error` path a serial call
     ;; uses — byte-identical error fidelity, but ISOLATED (one failing
     ;; observation never poisons its siblings). Exposed to the sandbox as
     ;; `__vis_par_isolated__` so python code can fan out inside ONE block.
     par-isolated-fn
     (fn par-isolated [& thunks]
       (let
         [thunks
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
                  (.submit ^ExecutorService @gather-executor
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
       ;; `__vis_par__`, the bounded host platform pool
       ;; that backs the async runtime's `gather`
       ;; (Python-side `gather`/`await` live in the
       ;; env_python async-runtime preamble; this is
       ;; the dispatcher they call to overlap awaitables
       ;; on bounded platform workers).
       compaction
       {(symbol "__vis_par__") gather-fn (symbol "__vis_par_isolated__") par-isolated-fn}
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
       ;; `resource_stop(id)` (B-dispatch — act by id;
       ;; ctx advertises can_stop). Session-scoped so the
       ;; agent only touches THIS session's resources.
       ;; No context mutator or introspect
       ;; bindings are installed here.
       (resources/sandbox-bindings session-id))

     ;; Security configuration is resolved exactly once for a root environment.
     ;; A sub_loop child receives the parent's value through `:child`; it never
     ;; re-reads model-writable vis.yml. `/reload` bumps `policy-reload-epoch`,
     ;; so each live env recycles at its next turn and rebuilds this snapshot.
     security-config
     (or (:security-policy child) (security-config-snapshot))

     configured-rw-roots
     (security-policy/read-write-roots security-config)

     ;; Engine substrate: embedded GraalPy (env/create-python-context builds a
     ;; deny-by-default polyglot Context, wires the Clojure tools as Python
     ;; callables, and installs doc/apropos introspection). Its live roots are the
     ;; workspace overlay plus immutable configured read/write roots. Native file
     ;; tools consume the same configured roots through the environment below.
     workspace-atom
     (atom active-workspace)

     sandbox-roots-fn
     (when (or active-workspace (seq configured-rw-roots))
       (fn []
         (let
           [ws
            @workspace-atom

            ;; The SAME per-root draft resolution the native tools use, so the
            ;; Python sandbox cannot reach a root the draft policy withholds
            ;; (`:denied?`) or write straight into a root this draft only owns
            ;; a private copy of — the clone is granted in its place.
            entries
            (workspace/env-filesystem-roots {:security-policy security-config
                                             :workspace ws
                                             :security/filesystem-roots configured-rw-roots})

            clones
            (into []
                  (comp (filter #(and (:clone %) (not= (:clone %) (:trunk %)))) (map :clone))
                  entries)

            withheld
            (into #{}
                  (comp (filter #(or (:denied? %) (and (:clone %) (not= (:clone %) (:trunk %)))))
                        (map :trunk))
                  entries)]

           (vec (distinct (concat (when ws [(str (:root ws))])
                                  clones
                                  (remove #(contains? withheld (workspace/normalize-root %))
                                    configured-rw-roots)))))))

     access-view-fn
     (fn []
       (let
         [ws
          @workspace-atom

          live-roots
          (when ws [(:root ws)])]

         (security-policy/access-view security-config live-roots)))

     jail-config
     (:process-jail security-config)

     jail-enabled?
     (not (:disabled? jail-config))

     net-cfg
     (:network security-config)

     ;; Host sockets stay available to the interpreter; the jail is the ONE
     ;; network switch. With the jail OFF there is no egress proxy AND no
     ;; in-interpreter domain guard — the sandbox network is unconfined, the
     ;; same all-or-nothing containment the OS process jail gives subprocesses.
     net-on?
     true

     network-opts
     {:enabled? net-on?
      :jail-enabled? jail-enabled?
      :allowed-domains (:allowed-domains net-cfg)
      :denied-domains (:denied-domains net-cfg)
      :exclude-domains (:exclude-domains net-cfg)
      :allow-private (:allow-private net-cfg)
      :rules (:rules net-cfg)}

     ;; One shared gateway proxy serves every environment. Unguessable tokens
     ;; attribute requests to this environment's immutable policy snapshot.
     sandbox-token
     (str (java.util.UUID/randomUUID))

     repl-sandbox-token
     (str (java.util.UUID/randomUUID))

     compiled-network-policy
     (some-> (egress/compile-policy net-cfg)
             (assoc :mitm? (boolean (seq (:rules net-cfg)))))

     _register-sandbox
     (when (and sandbox-roots-fn jail-enabled?)
       (gateway-sandbox/register-session! sandbox-token (constantly compiled-network-policy)))

     _register-repl-sandbox
     (when (and sandbox-roots-fn jail-enabled?)
       (gateway-sandbox/register-session! repl-sandbox-token (constantly compiled-network-policy)))

     ;; The user-controlled keys come only from config-spec/process-jail-config.
     ;; Per-spawn evaluation retains live workspace roots, lazy proxy startup and
     ;; the resolved `environment:` declarations; nothing else re-reads config.
     jail-policy-fn
     (when sandbox-roots-fn
       (fn []
         (let
           [proxy?
            (and jail-enabled? net-on?)

            proxy-port
            (when proxy? (gateway-sandbox/ensure-proxy!))

            ca-file
            (when proxy? (gateway-sandbox/ensure-ca!))

            java-trust
            (when proxy? (gateway-sandbox/ensure-java-trust!))

            repl-proxy-port
            (when proxy? (gateway-sandbox/ensure-session-proxy! repl-sandbox-token))]

           (merge jail-config
                  {:roots-fn sandbox-roots-fn
                   :net-enabled? net-on?
                   ;; Resolved per spawn (never baked into the session snapshot), so a
                   ;; `.env` edit or a refreshed keychain item reaches the next child.
                   :env-values (config/child-environment-values)
                   :proxy-port proxy-port
                   :proxy-token (when proxy? sandbox-token)
                   :repl-proxy-port repl-proxy-port
                   :repl-ca-file ca-file
                   :java-trust-store (:java-trust-store java-trust)
                   :java-trust-store-password (:java-trust-store-password java-trust)
                   :ca-file ca-file}))))

     ;; Register one live policy function for the standard language-process launch
     ;; contract. Managed REPLs and project test runners share the same Seatbelt +
     ;; gateway-proxy boundary as `shell` / subprocess, keyed per session.
     _register-repl-jail
     (when session-id (process-jail/register-session-jail! session-id jail-policy-fn))

     ;; The `:fs/access` gate, pushed down into the sandbox filesystem: a path an
     ;; extension's gate hook refuses is refused for `open(..., "w")`,
     ;; `shutil.move` and `Path.unlink` exactly as it is for `struct_patch`.
     sandbox-gate-fn
     (when sandbox-roots-fn
       (extension/fs-access-gate (fn []
                                   @environment-atom)))

     {:keys [python-context sandbox-ns initial-ns-keys]}
     (env/create-python-context (merge env-bindings (:custom-bindings @state-atom))
                                sandbox-roots-fn
                                network-opts
                                nil
                                nil
                                sandbox-gate-fn)

     ;; A gateway restart or a `/resume` in a new process builds a FRESH sandbox
     ;; while the transcript still shows the helpers this session refined, so the
     ;; next call would be a NameError against code the model can read. Re-create
     ;; them from the snapshot `execute-code` wrote after every block.
     _restored-defs
     (env/restore-session-defs! python-context session-id)

     env
     (cond->
       {:environment-id environment-id
        :session-id session-id
        :session/state-id session-state-id
        :channel (or channel :tui)
        ;; Immutable canonical security policy plus its live workspace overlay.
        ;; Context, GraalPy, native file tools, shell, managed language processes,
        ;; and egress all derive from this same environment-owned value.
        :security-policy security-config
        :security/filesystem-roots configured-rw-roots
        :security/no-search-roots (security-policy/no-search-roots security-config)
        :access-view-fn access-view-fn
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
        ;; (current model + provider only).
        :routing routing-digest
        :db-info db-info
        ;; Per-session OS-jail policy fn — the shell jail is ALWAYS ON; nil only when
        ;; no sandbox roots exist. Shell/subprocess executors consult it per spawn; see process-jail.
        :jail-policy-fn jail-policy-fn
        ;; This session's unguessable token for the SHARED gateway egress proxy /
        ;; MITM CA (internal.gateway-sandbox). Registered at env build; dropped from
        ;; the proxy's session registry in dispose-environment!.
        :sandbox-token sandbox-token
        :repl-sandbox-token repl-sandbox-token}
       ;; Workspace info attached at env-build time so the extension
       ;; wrapper's `(workspace/workspace-root env)` finds a non-blank
       ;; root the very first time it fires.
       active-workspace
       (assoc :workspace
         active-workspace :workspace/id
         (:id active-workspace) :workspace/root
         (:root active-workspace)
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
       :session-rebase-atom session-rebase-atom
       ;; PROMPT-CACHE STABILITY: the standing `session = {…}` block rides
       ;; in the cached system prefix and is normally frozen across turns.
       ;; State changes ride as appended `session[...] = …` deltas. A large
       ;; fold deliberately rebases this block to the current materialized
       ;; session, bounding the delta chain while spending a cache miss that
       ;; compaction already made useful. Holds
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
    ;; Load-once, never adopt: this runs on every env cache miss, every recycle
    ;; and every `sub_loop` child env, and none of those is a human act. Only
    ;; this process's own start and `/reload` may pick an edit up.
    (python-extensions/ensure-python-extensions-loaded!)
    (extension/register-extensions! env install-extension!)
    env))

;; Session env cache

;; In-process session cache + channel utilities

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

(defonce
  ^{:doc
    "Monotonic `/reload` epoch. Every `/reload` bumps it (via a reload hook).
   A cache entry stamped with an older epoch is recycled at its NEXT turn
   boundary so its immutable security-policy snapshot rebuilds from the
   freshly-reloaded vis.yml. This is the sanctioned way `/reload` replaces the
   frozen network-domain / filesystem-root policy: the snapshot drives the
   GraalPy context, egress proxy, and process jail at env-creation time, so it
   can only change by rebuilding the env — never by an in-place reseat."}
  policy-reload-epoch
  (atom 0))

(defn mark-policy-reload!
  "Invalidate every cached env's frozen security-policy snapshot. Registered as
   a `/reload` hook: live sessions recycle at their next turn so vis.yml edits
   to network domains / filesystem roots actually take effect. Returns nil."
  []
  (swap! policy-reload-epoch inc)
  nil)

;; Wire `mark-policy-reload!` into the `/reload` slash. `run-reload-hooks!`
;; (invoked by `reload-slash` after `config/reload-config!`) bumps the epoch, so
;; the next turn of each live session rebuilds its security-policy snapshot.
;; `defonce` keeps the registration idempotent across `(require ... :reload)`.
(defonce ^:private _policy-reload-hook
  (extension/register-reload-hook! ::security-policy-reload mark-policy-reload!))

(defn- cache-key
  "Normalize an id-shaped value (UUID or string-UUID) to a UUID
   suitable for keying `cache`. Nil → nil so wrapped lookups stay
   honest."
  [id]
  (persistance/->uuid id))

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

(def ^:private env-idle-ttl-ms
  "Idle window before a cached session env's GraalPy Context is disposed by the
   background reaper. Override with `VIS_ENV_IDLE_TTL_MS`; <= 0 disables the TTL
   sweep. Default 15 min.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_IDLE_TTL_MS")
                     str/trim
                     parse-long)
             (* 15 60 1000))))

(def ^:private env-cache-max
  "Soft cap on resident session envs. After the TTL sweep, if the cache still
   exceeds this the reaper force-evicts the least-recently-active idle entries
   (still lock-guarded) until back under the cap. Override with
   `VIS_ENV_CACHE_MAX`; <= 0 disables it. Default 8.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_CACHE_MAX")
                     str/trim
                     parse-long)
             8)))

(def ^:private env-reaper-interval-ms
  "How often the idle-env reaper wakes to sweep. Override with
   `VIS_ENV_REAPER_INTERVAL_MS`. Default 60 s.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_REAPER_INTERVAL_MS")
                     str/trim
                     parse-long)
             (* 60 1000))))

(def ^:private env-max-turns-per-ctx
  "Turns a single session's GraalPy Context serves before the reaper recycles it
   between turns. Override with `VIS_ENV_MAX_TURNS_PER_CTX`; <= 0 disables.
   Default 25.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_MAX_TURNS_PER_CTX")
                     str/trim
                     parse-long)
             25)))

(def ^:private env-heap-watermark-pct
  "JVM heap-usage percent (used/max) at or above which the reaper treats the
   process as under memory pressure and force-evicts EVERY idle (unlocked)
   session env this sweep — ignoring the idle TTL — to shed GraalPy Contexts
   fast. A running turn holds its entry's lock so it is never evicted; the
   transcript reloads from the DB on the next touch. Override with
   `VIS_ENV_HEAP_WATERMARK_PCT`; <= 0 disables the watermark. Default 85.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_HEAP_WATERMARK_PCT")
                     str/trim
                     parse-long)
             85)))

(def ^:private env-heap-budget-mb
  "Absolute JVM heap-used ceiling in MB. At or above it, the reaper force-evicts
   every idle session env. Override with `VIS_ENV_HEAP_BUDGET_MB`; <= 0 disables.
   Default 2048 (2 GB), low enough to react before allocation bursts reach the
   multi-gigabyte resident-set spikes seen under concurrent GraalPy turns.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_HEAP_BUDGET_MB")
                     str/trim
                     parse-long)
             2048)))

(def ^:private env-rss-budget-mb
  "Resident-set ceiling in MB. JVM heap alone misses GraalPy/native allocations,
   so this gate also forces idle-env eviction when process RSS is high. Override
   with `VIS_ENV_RSS_BUDGET_MB`; <= 0 disables. Default 3072 (3 GB).

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_RSS_BUDGET_MB")
                     str/trim
                     parse-long)
             3072)))

(defn- process-rss-bytes
  "Best-effort process resident set in bytes. Reads procfs on Linux and `ps` on
   macOS/other Unix hosts. Returns 0 when unavailable; never throws."
  []
  (try (let [status-path (java.nio.file.Path/of "/proc/self/status" (make-array String 0))]
         (if (java.nio.file.Files/isRegularFile status-path (make-array java.nio.file.LinkOption 0))
           (let
             [status (java.nio.file.Files/readString status-path)
              kb (some-> (re-find #"(?m)^VmRSS:\s+(\d+)\s+kB" status)
                         second
                         parse-long)]

             (* (long (or kb 0)) 1024))
           (let
             [pid (.pid (java.lang.ProcessHandle/current))
              process (.exec (Runtime/getRuntime)
                             ^"[Ljava.lang.String;"
                             (into-array String ["ps" "-o" "rss=" "-p" (str pid)]))]

             (try (if (and (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS)
                           (zero? (.exitValue process)))
                    (* (long (or (some-> (slurp (.getInputStream process))
                                         str/trim
                                         parse-long)
                                 0))
                       1024)
                    0)
                  (finally (.destroy process))))))
       ;; Same shape as the shell's usage sampler: `ps` is best-effort, the
       ;; cancel that interrupted it is not.
       (catch Throwable t (cancellation/preserve-interrupt! t) 0)))

(defn- heap-used-pct
  "Current JVM heap utilization as an integer percent of the max heap
   (used = total - free). 0 when the max heap is unknown."
  []
  (let
    [rt
     (Runtime/getRuntime)

     mx
     (.maxMemory rt)]

    (if (pos? mx) (long (/ (* 100 (- (.totalMemory rt) (.freeMemory rt))) mx)) 0)))

(defn- memory-pressure-for-rss?
  [rss-bytes]
  (or (and (pos? (long @env-heap-watermark-pct))
           (>= (long (heap-used-pct)) (long @env-heap-watermark-pct)))
      (and (pos? (long @env-heap-budget-mb))
           (let [rt (Runtime/getRuntime)]
             (>= (- (.totalMemory rt) (.freeMemory rt)) (* (long @env-heap-budget-mb) 1024 1024))))
      (and (pos? (long @env-rss-budget-mb))
           (>= (long rss-bytes) (* (long @env-rss-budget-mb) 1024 1024)))))

(defn- heap-pressure?
  "True when JVM heap percentage, absolute heap, or process RSS crosses its
   configured gate. The RSS gate catches GraalPy/native memory invisible to the
   Java heap counters. Accepts a sampled RSS value to avoid duplicate process
   calls during metrics and reaper sweeps."
  ([] (heap-pressure? (process-rss-bytes)))
  ([rss-bytes] (memory-pressure-for-rss? rss-bytes)))

(defn- cpu-load-pct
  "Whole-process CPU load as a percent (0–100; -1 when the JVM can't sample the
   interval yet). Uses com.sun's OperatingSystemMXBean when present; never throws."
  ^long []
  (let [os (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? com.sun.management.OperatingSystemMXBean os)
      (let [v (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean os)]
        (if (>= v 0.0) (Math/round (* v 100.0)) -1))
      -1)))

(defn gateway-runtime-metrics
  "Bounded process/runtime gauges for the gateway metrics endpoint. Values are
   sampled on demand; no profiler or background allocation is required."
  []
  (let
    [rt
     (Runtime/getRuntime)

     heap-used
     (- (.totalMemory rt) (.freeMemory rt))

     rss
     (process-rss-bytes)

     gc-beans
     (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)

     thread-bean
     (java.lang.management.ManagementFactory/getThreadMXBean)]

    {:jvm-heap-used-bytes heap-used
     :jvm-heap-committed-bytes (.totalMemory rt)
     :jvm-heap-max-bytes (.maxMemory rt)
     :process-rss-bytes rss
     :jvm-gc-count-total
     (reduce (fn [^long n bean]
               (let [v (.getCollectionCount ^java.lang.management.GarbageCollectorMXBean bean)]
                 (+ n (long (max 0 v)))))
             (long 0)
             gc-beans)
     :jvm-gc-time-ms-total
     (reduce (fn [^long n bean]
               (let [v (.getCollectionTime ^java.lang.management.GarbageCollectorMXBean bean)]
                 (+ n (long (max 0 v)))))
             (long 0)
             gc-beans)
     :jvm-thread-count (.getThreadCount thread-bean)
     :env-cache-size (count @cache)
     :env-heap-pressure (heap-pressure? rss)}))

(defn- mem-log-enabled?
  "Master switch for memory-observability logging, shared conceptually with the
   per-block heap sample in `internal.env-python`. Enabled unless VIS_MEM_LOG is a
   falsey token (0/false/off/no) — one flag silences the reaper sweep summary."
  []
  (let
    [raw (some-> (System/getenv "VIS_MEM_LOG")
                 str/trim
                 str/lower-case)]
    (not (contains? #{"0" "false" "off" "no"} raw))))

(defn- new-cache-entry
  "Build a cache entry wrapping `env`: the environment, its per-session
   `ReentrantLock` (one turn at a time), an `AtomicLong` `:last-active`
   epoch-ms stamp the reaper reads to decide idleness, and the `:condemned`
   flag `condemn-env!` raises when this entry's turn was declared over by a
   thread that never came back."
  [env]
  {:environment env
   :lock (java.util.concurrent.locks.ReentrantLock.)
   :condemned (java.util.concurrent.atomic.AtomicBoolean. false)
   :last-active (java.util.concurrent.atomic.AtomicLong. (System/currentTimeMillis))
   :turns (java.util.concurrent.atomic.AtomicLong. 0)
   ;; The `/reload` epoch this env was built under. `send!` recycles the entry
   ;; when a later `/reload` has bumped `policy-reload-epoch` past this stamp.
   :policy-epoch (java.util.concurrent.atomic.AtomicLong. (long @policy-reload-epoch))})

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
  (let
    [entry
     (get @cache k)

     ^java.util.concurrent.locks.ReentrantLock lock
     (:lock entry)]

    (boolean (when (and entry lock (.tryLock lock))
               (try (let
                      [cur
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
  (let
    [now
     (System/currentTimeMillis)

     age
     (fn [entry]
       (if-let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
         (- now (.get la))
         0))

     rss-bytes
     (process-rss-bytes)

     pressure?
     (heap-pressure? rss-bytes)

     effective-ttl
     (if pressure? 0 (long @env-idle-ttl-ms))

     ttl-evicted
     (if (or pressure? (pos? (long @env-idle-ttl-ms)))
       (->> @cache
            (filter (fn [[_ entry]]
                      (>= (long (age entry)) (long effective-ttl))))
            (reduce (fn [n [k _]]
                      (if (evict-if-idle! k effective-ttl) (inc (long n)) n))
                    0))
       0)

     lru-evicted
     (if (pos? (long @env-cache-max))
       (let
         [snapshot
          @cache

          over
          (- (long (count snapshot)) (long @env-cache-max))]

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
       0)

     total
     (+ (long ttl-evicted) (long lru-evicted))

     cpu
     (cpu-load-pct)]

    (when (mem-log-enabled?)
      (tel/log!
        {:level :info
         :id ::env-reaper-sweep
         :data {:evicted total
                :ttl-evicted ttl-evicted
                :lru-evicted lru-evicted
                :heap-used-pct (heap-used-pct)
                :process-rss-bytes rss-bytes
                :cpu-proc-pct cpu
                :memory-pressure? pressure?
                :cache-size (count @cache)}}
        (format
          "env-reaper evicted=%d (ttl=%d lru=%d) heap=%d%% rss=%dMB cpu=%d%% pressure=%s cache=%d"
          (long total)
          (long ttl-evicted)
          (long lru-evicted)
          (long (heap-used-pct))
          (quot (long rss-bytes) 1048576)
          cpu
          pressure?
          (count @cache))))
    total))

(defn- reaper-loop
  "Background sweep loop: sleep the interval, sweep, repeat. Exits on interrupt;
   any sweep error is logged and swallowed so a single bad sweep never kills the
   reaper."
  []
  (loop []

    (let
      [continue? (try (Thread/sleep (long @env-reaper-interval-ms))
                      (reap-idle-envs!)
                      true
                      (catch InterruptedException _ false)
                      (catch Throwable t
                        (tel/log! {:level :warn :data {:error (ex-message t)}}
                                  "env-reaper sweep failed")
                        true))]
      (when continue? (recur)))))

(defonce ^:private env-reaper-thread (atom nil))

(defn- env-reaper-enabled?
  "True when the sweep interval and at least one eviction policy are enabled."
  []
  (and (pos? (long @env-reaper-interval-ms))
       (or (pos? (long @env-idle-ttl-ms))
           (pos? (long @env-cache-max))
           (pos? (long @env-heap-watermark-pct))
           (pos? (long @env-heap-budget-mb))
           (pos? (long @env-rss-budget-mb)))))

(defn- ensure-env-reaper!
  "Start the idle-env reaper daemon thread once, lazily, on the first cache
   insert. Started here (not at namespace load) so a native-image build-time
   init never spawns a thread, and only when reaping is actually enabled."
  []
  (when (and (env-reaper-enabled?) (nil? @env-reaper-thread))
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

(defn sync-cached-extension-symbols!
  "Synchronize extension bindings in every idle cached session immediately.

   A Settings change is process-wide while each session owns a persistent
   Python context. Busy contexts retain their started tool surface and are
   synchronized at the next turn boundary. Returns the refreshed count."
  []
  (reduce-kv (fn [refreshed _ {:keys [environment lock]}]
               (if (and lock (.tryLock ^java.util.concurrent.locks.ReentrantLock lock))
                 (try (sync-active-extension-symbols! environment)
                      (unchecked-inc (long refreshed))
                      (finally (.unlock ^java.util.concurrent.locks.ReentrantLock lock)))
                 refreshed))
             0
             @cache))

;; A Settings flip must reach the TOOLS, whatever channel made it. The fan-out
;; used to sit inline in the gateway's HTTP settings handler, so a flip from the
;; TUI dialog (which calls `toggles/set-enabled!` straight) or from an extension
;; persisted to state.yml and refreshed nothing — every other cached session kept
;; its stale tool surface until a restart. The toggle registry is the ONE seam
;; every channel goes through, so the fan-out belongs on its listener.
;; `notify!` swallows listener throws, and `defonce` keeps the registration
;; idempotent across `(require ... :reload)`.
(defonce ^:private _toggle-extension-sync-listener
  (toggles/add-listener! (fn [_event]
                           (sync-cached-extension-symbols!))))

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

(defn reload-router!
  "Rebuild the shared LLM router from the freshly reloaded config and reseat it
   on every cached env. Registered as a `/reload` hook.

   `reload-slash` re-reads vis.yml through `config/reload-config!`, but the
   router is an immutable SNAPSHOT: built once by `get-router` and captured
   again inside every long-lived session env (`(:router environment)`). Without
   this hook a `default_model` / provider edit only took effect after a full
   restart — the engine kept routing turns through the previous router, and
   every frontend that names the router default (the TUI footer model chip via
   `resolve-effective-model`) kept showing the OLD model.

   No-ops when the router was never built, so lazy first use is preserved: a
   `/reload` must not force OAuth token fetches at TUI boot. Returns nil."
  []
  (when (router-initialized?) (refresh-cached-routers! (rebuild-router! (config/current-config))))
  nil)

;; Wire `reload-router!` into the `/reload` slash. `run-reload-hooks!` runs
;; AFTER `config/reload-config!`, so the rebuild always sees the new config.
;; `defonce` keeps the registration idempotent across `(require ... :reload)`.
(defonce ^:private _router-reload-hook
  (extension/register-reload-hook! ::router-reload reload-router!))

;; Provider/selection mutations live in `providers`, which this namespace
;; requires — so the router cannot be rebuilt from there. Register `reload-router!`
;; as the hook they fire, so a default/fallback pick (or an add/remove/reorder
;; provider, or a new API key) rebuilds the shared router and reseeds every cached
;; session env — not only on `/reload`. Before this, a default-model change via the
;; picker persisted and displayed but left the shared router on the OLD root, so a
;; new session's first turn ran the previous model until the user re-pinned it on
;; the session.
;; The VAR, not its value: `defonce` skips its body on a `(require … :reload)`, so
;; registering the FUNCTION left the hook pointing at the definition from the FIRST
;; load while every caller resolved the new one.
(defonce ^:private _router-rebuild-hook (providers/set-router-rebuild-hook! #'reload-router!))

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
  (let
    [cfg
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
  (let
    [router
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
      ;; NB: a cache HIT does NOT touch `:last-active`. Idleness must reflect
      ;; real turn activity (marked in `send!`'s finally), not passive reads:
      ;; hot render/status paths (`gateway.state/live-env`, `context-snapshot`)
      ;; resolve the env via `env-for` on every poll, and touching here reset
      ;; the idle clock each time — so a rendered-but-idle session was NEVER
      ;; reaped (its Context stayed resident indefinitely).
      entry
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
          :turns (java.util.concurrent.atomic.AtomicLong. 0)
          ;; Restamp to the current epoch: the fresh env carries the latest
          ;; security-policy snapshot, so it is no longer reload-stale.
          :policy-epoch (java.util.concurrent.atomic.AtomicLong. (long @policy-reload-epoch))))
      (try (dispose-environment! (:environment old)) (catch Throwable _ nil)))))

(def ^:private ENGINE_LOCK_POLL_MS
  "How long one attempt at a session's turn lock waits before `send!` re-reads
   the cache entry it is queueing on.

   Not a deadline: a turn legitimately owns that lock for its whole run, and a
   queued turn is supposed to wait. It is the beat at which the waiter notices
   its entry was CONDEMNED (see [[condemn-env!]]) and stops waiting for a thread
   that is never coming back."
  250)

(defn- detach-entry!
  "Drop `entry` from the cache, but only while it is still `k`'s entry.

   Nothing is disposed and the lock is not touched: the thread parked inside
   that Context still owns both, and closing a Context out from under a live
   guest thread is not safe. So the Context, its actions-pool thread and its
   native heap leak until the process exits — which beats a session that can
   never take another turn. The next [[ensure-env!]] builds a fresh env under a
   FRESH lock, and the ghost is left holding an object nobody references."
  [k entry]
  (loop []

    (let [m @cache]
      (cond (not (identical? entry (get m k))) false
            (compare-and-set! cache m (dissoc m k)) true
            :else (recur)))))

(defn condemn-env!
  "Mark session `id`'s engine entry CONDEMNED: the daemon has already declared
   this session's turn over, but the thread that ran it never came back, so it
   may be holding the entry's `ReentrantLock` forever.

   The mark is a fact the NEXT turn reads: [[acquire-turn-lock!]] abandons a
   condemned entry instead of queueing behind a dead lock. A worker that does
   thaw clears the mark simply by taking the lock normally, so a backstop that
   fired early costs nothing. Returns true when an entry was marked."
  [id]
  (boolean (when-let
             [^java.util.concurrent.atomic.AtomicBoolean flag (:condemned (get @cache
                                                                               (cache-key id)))]
             (.set flag true)
             true)))

(defn- acquire-turn-lock!
  "Take session `id`'s one-turn-at-a-time lock and return the entry that owns
   it, WITHOUT ever parking on it forever.

   This used to be a bare `.lock`. A turn wedged inside the engine — parked on
   GraalPy's GIL, where `Thread.interrupt` cannot reach it — never unlocks, so
   every later turn for that session parked in `Unsafe.park`: `turn.started` on
   the wire, not one event after it, and deaf to its own cancel, for the life of
   the daemon. Meanwhile the cancel backstop had already synthesized
   `turn.cancelled` and the daemon reported the session idle.

   So the wait is a POLL. Each round re-reads the session's CURRENT entry, and a
   CONDEMNED one is detached and rebuilt rather than waited on; `tryLock` is
   interruptible, so a queued turn's own cancel finally reaches it."
  [id]
  (let [k (cache-key id)]
    (loop []

      (let
        [{:keys [^java.util.concurrent.locks.ReentrantLock lock
                 ^java.util.concurrent.atomic.AtomicBoolean condemned]
          :as entry}
         (ensure-env! id)]
        (if (.tryLock lock (long ENGINE_LOCK_POLL_MS) java.util.concurrent.TimeUnit/MILLISECONDS)
          (do (when condemned (.set condemned false)) entry)
          (do
            (when (and condemned (.get condemned) (detach-entry! k entry))
              (tel/log!
                {:level :warn :id ::engine-abandoned :data {:session k}}
                "Session engine abandoned: its turn was declared over but its thread never returned - starting a fresh context"))
            (recur)))))))

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
   (let
     [env
      (open-env! nil
                 (cond->
                   {:channel channel
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
     :model (:model session) ; the state's ROOT model, not the user's pin
     :model-pref (:model-pref session) ; {:provider :model} pin, or nil for router default
     :title (:title session)
     :created-at (:created-at session)
     :owner-id (:owner-id session)
     :project-id (:project-id session)
     :project-name (:project-name session)
     :project-position (:project-position session)
     :favorite-rank (:favorite-rank session)}))

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
           :project-position (:project-position c)
           :favorite-rank (:favorite-rank c)})
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

(defn project-session-ids
  "Ids of every session soul belonging to `project-id`, across channels.

   This is MEMBERSHIP, not a client's visible list: an untitled or empty
   conversation is a member too, and a caller that fans out over what it can see
   would delete the visible rows and silently keep the rest."
  [project-id]
  (let [pid (str project-id)]
    (->> (by-channel :all)
         (filter (fn [s]
                   (= pid (str (:project-id s)))))
         (mapv :id))))

(defn assign-project!
  "Assign the session soul to `project-id` (nil clears / removes from project)."
  [session-id project-id]
  (persistance/db-set-session-project! (db-info) session-id project-id))

(defn set-favorite!
  "Star (`true`) or unstar (`false`) the session soul. Returns the rank it now
   holds, or nil once it is unstarred."
  [session-id is-favorite]
  (persistance/db-set-session-favorite! (db-info) session-id is-favorite))

(defn reorder-project-sessions!
  "Atomically adopt any loose named session into `project-id`, then persist the
   manual order. Guests owned by another project are never stolen."
  [project-id session-ids]
  (persistance/db-adopt-and-reorder-project-sessions! (db-info) project-id session-ids))

;; Host title setter + public env accessor

(defn env-for [id] (:environment (ensure-env! id)))

(defn set-title!
  "Host-driven title change. Resolves the live env (if any) so the
   in-memory atom + listener fan-out stay in sync; falls back to a
   plain DB write when no env is live for this session (e.g.
   `vis-agent sessions` rename ops)."
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
   (let
     [k
      (cache-key id)

      message-vec
      (if (string? messages) [(svar/user messages)] messages)

      ;; ONE turn per session, and the lock is TAKEN right here — through
      ;; `acquire-turn-lock!`, which never parks on a lock a wedged turn is
      ;; never going to release. Extension reload marks envs dirty; the actual
      ;; sandbox reset happens under the lock below, after prior IR/render is
      ;; finished and before the next user code executes.
      {:keys [^java.util.concurrent.locks.ReentrantLock lock] :as entry}
      (acquire-turn-lock! id)]

     (try
       ;; Apply a pending `/reload` FIRST: if this entry was built under an
       ;; older policy epoch, recycle it now so the turn runs against a
       ;; security-policy snapshot rebuilt from the freshly-reloaded vis.yml
       ;; (new network domains / filesystem roots take effect here). Done under
       ;; the lock, before the turn, so no eval races the swap.
       (let
         [cur
          (or (get @cache k) entry)

          ^java.util.concurrent.atomic.AtomicLong pe
          (:policy-epoch cur)]

         (when (and pe (< (.get pe) (long @policy-reload-epoch)))
           (try (recycle-env! k) (catch Throwable _ nil))))
       ;; Re-read :environment UNDER the lock: a between-turns turn-cap recycle
       ;; or a router/extension reseat may have swapped it since we captured
       ;; `entry`, so the queued turn runs against the CURRENT context.
       (turn! (:environment (or (get @cache k) entry)) message-vec opts)
       (finally
         ;; Housekeeping must NEVER strand the lock. A throw from `touch-entry!`,
         ;; `bump-turns!` or the guest-side `collect-garbage!` used to skip the
         ;; `.unlock` below, pinning this session's entry as permanently
         ;; "busy": `evict-if-idle!` tryLocks, fails forever, and the Context
         ;; (plus its actions-pool thread and native heap) leaks for the life of
         ;; the gateway. Unlock in an inner `finally` so it is unconditional.
         (try (let [cur (or (get @cache k) entry)]
                (touch-entry! cur)
                (let [n (bump-turns! cur)]
                  (if (and (pos? (long @env-max-turns-per-ctx))
                           (>= (long n) (long @env-max-turns-per-ctx)))
                    ;; Layer 2: recycle this session's Context between turns so a
                    ;; single never-idle session can't grow it unbounded.
                    (try (recycle-env! k) (catch Throwable _ nil))
                    ;; Layer 1: best-effort guest gc.collect() between turns.
                    (env/collect-garbage! (:environment cur)))))
              (catch Throwable _ nil)
              (finally (.unlock lock))))))))

(defn close!
  [id]
  (let [k (cache-key id)]
    (when-let
      [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]} (clojure.core/get @cache
                                                                                              k)]
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
  ;; A shell log dies with the session that produced it, and with nothing else:
  ;; the bytes on disk outlive the process, so the delete has to name them.
  (shell-log/delete-session-logs! id)
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
   (let
     [orphans (try (persistance/db-list-session-turns-by-status db :running)
                   (catch Exception _ []))]
     (doseq [{:keys [id iteration-count duration-ms]} orphans]
       (persist-turn-outcome! db
                              id
                              {:content
                               [(content/error "turn_interrupted" ORPHAN_INTERRUPTED_ANSWER true)]
                               :iteration-count (or iteration-count 0)
                               :duration-ms (or duration-ms 0)
                               :status :interrupted
                               :prior-outcome :cancelled}))
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
