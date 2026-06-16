(ns com.blockether.vis.internal.loop
  (:refer-clojure)
  (:require
   [charred.api :as json]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.internal.safe-guards :as safe-guards]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.render :as render]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.session-model :as session-model]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.providers :as providers]
   [com.blockether.vis.internal.registry :as registry]
   [com.blockether.vis.internal.resources :as resources]
   [com.blockether.vis.internal.slash :as slash]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.internal.tokens :as tokens]
   [com.blockether.vis.internal.workspace :as workspace]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent CancellationException]))

;; =============================================================================
;; Query runtime settings
;; =============================================================================

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in the Python sandbox."
  120000)

(def MIN_EVAL_TIMEOUT_MS
  "Floor for :eval-timeout-ms."
  3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms."
  (* 30 60 1000))

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
   loop blocks on `.readLine` until the model finally streams output
   (observed: codex/responses gpt-5.5 iter 0 silent for 11min 9s on
   2026-05-20, session da9f0b47, no timeout ever fired).

   4 minutes (240000 ms) is the considered ceiling: > Anthropic's
   documented 185s worst case for legitimate extended thinking on
   Opus 4.5 (anthropics/claude-agent-sdk-typescript#44), high enough
   that a deep reasoning model with a real long pre-token phase still
   succeeds, low enough that a stuck provider surfaces a real error
   in under 5 minutes instead of holding the whole turn hostage.

   Disable per call with `:semantic-timeout-ms nil`."
  (* 4 60 1000))

(defn- with-default-ask-code-idle-timeout
  [opts]
  (cond-> opts
    (not (contains? opts :ttft-timeout-ms))
    (assoc :ttft-timeout-ms ASK_CODE_TTFT_TIMEOUT_MS)

    (not (contains? opts :idle-timeout-ms))
    (assoc :idle-timeout-ms ASK_CODE_IDLE_TIMEOUT_MS)

    (and (some? ASK_CODE_SEMANTIC_TIMEOUT_MS)
      (not (contains? opts :semantic-timeout-ms)))
    (assoc :semantic-timeout-ms ASK_CODE_SEMANTIC_TIMEOUT_MS)))

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

(def ^:dynamic *rlm-context*
  "Dynamic context for RLM debug logging."
  nil)

;; =============================================================================
;; Single-iteration runner
;; =============================================================================

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

;; ---------------------------------------------------------------------------
;; Per-iteration `(def ...)` discovery / dependency tracking was retired
;; together with the `definition_*` sidecar tables and `restore-sandbox!`.
;; Python defs are intra-turn scratch; cross-turn references go through
;; `:session/facts` and `introspect-form` / `introspect-iter` /
;; `introspect-turn` (DB reads against `session_turn_iteration.forms`).
;; ---------------------------------------------------------------------------

(def ^:private MINI_STACK_DEPTH 12)

(defn- throwable-chain
  [^Throwable t]
  (vec (take-while some? (iterate (fn [^Throwable x] (.getCause x)) t))))

(defn- throwable-cause-summary
  [^Throwable t]
  (mapv (fn [^Throwable x]
          (cond-> {:class (.getName (class x))
                   :message (or (ex-message x) (str x))}
            (:type (ex-data x)) (assoc :type (:type (ex-data x)))))
    (throwable-chain t)))

(defn- mini-stack-trace
  [^Throwable t]
  (when t
    (let [frames (take MINI_STACK_DEPTH (.getStackTrace t))]
      (str/join "\n"
        (map (fn [^StackTraceElement frame]
               (str "  at " frame))
          frames)))))

(defn- format-exception-short [^Throwable t]
  (let [ed (ex-data t)]
    (cond-> {:class (.getName (class t))
             :message (or (ex-message t) (str t))
             :causes (throwable-cause-summary t)
             :mini-trace (mini-stack-trace t)}
      (:type ed) (assoc :type (:type ed))
      (:status ed) (assoc :status (:status ed))
      (:cause-class ed) (assoc :cause-class (:cause-class ed)))))

(defn- format-exception [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t)
    {:data (ex-data t) :context context}))

(defn- interrupted-cause?
  [^Throwable t]
  (boolean
    (some (fn [^Throwable x]
            (or (instance? InterruptedException x)
              (instance? CancellationException x)
              (= "java.lang.InterruptedException" (ex-message x))))
      (throwable-chain t))))

(def ^:private PROVIDER_INTERRUPT_RETRIES 1)

(def ^:private PROVIDER_STREAM_REWIND_RETRIES 2)

(def ^:private CONSECUTIVE_PROVIDER_ERROR_LIMIT
  "Circuit breaker for the iteration loop: after this many CONSECUTIVE
   provider-generate failures (e.g. :svar.llm/empty-content that survived the
   per-call stream-rewind retries) the turn fails fast as a provider error
   instead of burning the whole iteration budget re-sending the same request
   (session burned 15/15 iterations on identical empty-content failures).
   Any successful iteration or non-provider error resets the streak."
  3)

(defn- next-provider-error-streak
  "Next consecutive provider-generate failure count. Increments only when the
   iteration error is :llm-provider/generate (model produced no usable
   content); any other error kind resets to 0 - those are RLM-correctable."
  [prev-streak llm-provider-error]
  (if (= :llm-provider/generate (:phase llm-provider-error))
    (inc (long (or prev-streak 0)))
    0))

(defn- provider-error-breaker-tripped?
  "True when the consecutive provider-generate failure streak has reached
   CONSECUTIVE_PROVIDER_ERROR_LIMIT - the iteration loop fails the turn fast
   instead of re-sending the same doomed request."
  [streak]
  (>= (long streak) CONSECUTIVE_PROVIDER_ERROR_LIMIT))

(def ^:private PROVIDER_STREAM_REWIND_DELAYS_MS [1000 2000])

(defn- provider-call-cancelled?
  [environment]
  (boolean (some-> environment :cancel-atom deref)))

(defn- retryable-provider-interrupt?
  "True for provider-thread interrupts that were not caused by Vis user cancel.
   svar's TTFT watchdog currently can surface as a naked InterruptedException;
   retry once instead of letting router treat it like Esc cancellation."
  [^Throwable t environment]
  (and (interrupted-cause? t)
    (not (provider-call-cancelled? environment))))

(defn- call-provider-with-interrupt-retry!
  [environment iteration-position f]
  (loop [attempt 0]
    (let [outcome (try
                    {:ok? true :value (f)}
                    (catch Throwable t
                      {:ok? false :throwable t}))]
      (if (:ok? outcome)
        (:value outcome)
        (let [t (:throwable outcome)]
          (if (and (< attempt PROVIDER_INTERRUPT_RETRIES)
                (retryable-provider-interrupt? t environment))
            (do
              ;; Router restores interrupt status before rethrowing. Clear it
              ;; before retry or next HTTP call can fail immediately.
              (Thread/interrupted)
              (tel/log! {:level :warn
                         :data (assoc (format-exception-short t)
                                 :iteration iteration-position
                                 :attempt (inc attempt)
                                 :max-retries PROVIDER_INTERRUPT_RETRIES
                                 :cancelled? (provider-call-cancelled? environment))}
                "Provider call interrupted without user cancel; retrying")
              (recur (inc attempt)))
            (throw t)))))))

(defn- stream-transport-error?
  [^Throwable t]
  (let [data (ex-data t)
        msg-lower (str/lower-case (or (ex-message t) ""))
        cause (ex-cause t)
        cause-lower (str/lower-case (or (some-> cause ex-message) ""))]
    (and (= :svar.core/http-error (:type data))
      (:stream? data)
      (not (interrupted-cause? t))
      (or (str/includes? msg-lower "stream connection error")
        (str/includes? msg-lower "connection reset")
        (str/includes? msg-lower "connection closed")
        (str/includes? msg-lower "closed")
        (str/includes? msg-lower "eof")
        (str/includes? msg-lower "timed out")
        (str/includes? cause-lower "connection reset")
        (str/includes? cause-lower "connection closed")
        (str/includes? cause-lower "closed")
        (str/includes? cause-lower "eof")
        (str/includes? cause-lower "timed out")))))

(defn- empty-content-error?
  "True for :svar.llm/empty-content anywhere in the cause chain - the provider
   streamed reasoning (or nothing) but produced no textual content. This is a
   transient model hiccup, not a user/program error; retry the provider call
   instead of surfacing it."
  [^Throwable t]
  (boolean
    (some #(= :svar.llm/empty-content (:type (ex-data %)))
      (take-while some? (iterate ex-cause t)))))

(defn- provider-retry-event
  [{:keys [provider model attempt delay-ms error]}]
  (cond-> {:event/type :llm.routing/provider-retry
           :reason :stream-connection-error
           :provider provider
           :model model
           :attempt attempt
           :delay-ms delay-ms
           :error error}
    provider (assoc :from-provider provider)
    model (assoc :from-model model)))

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
  (loop [attempt 0
         retry-events []]
    (let [outcome (try
                    {:ok? true :value (f)}
                    (catch Throwable t
                      {:ok? false :throwable t}))]
      (if (:ok? outcome)
        (prepend-routing-trace (:value outcome) retry-events)
        (let [t (:throwable outcome)
              can-retry? (and (< attempt PROVIDER_STREAM_REWIND_RETRIES)
                           (not (provider-call-cancelled? environment))
                           (or (stream-transport-error? t)
                             (empty-content-error? t)))]
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
                           :iteration-count iteration-position
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
              (when (pos? delay-ms)
                (Thread/sleep delay-ms))
              (recur (inc attempt) (conj retry-events event)))
            (throw (add-routing-trace-to-ex t retry-events))))))))

;; ---------------------------------------------------------------------------

(defn log-stage!
  [stage iteration data]
  (tel/log! {:level :info :data (merge {:stage stage :iteration iteration} data)}))

(defn- elapsed-ms
  [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn normalize-reasoning-level [v]
  (svar/normalize-reasoning-level v))

(defn- github-copilot-claude-model?
  [resolved-model]
  (and (contains? #{:github-copilot-individual :github-copilot-business}
         (:provider resolved-model))
    (boolean (re-find #"(?i)claude" (str (:name resolved-model))))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋)[\s!.?,]*\s*$")

(defn- casual-user-request?
  [s]
  (let [text (some-> s str str/trim)]
    (boolean
      (and text
        (<= (count text) 80)
        (re-find casual-request-pattern text)))))

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
  (cond
    (not (github-copilot-claude-model? resolved-model)) reasoning-level
    (casual-user-request? user-request) nil
    (and (= :deep reasoning-level) (not allow-copilot-claude-deep?)) :balanced
    :else reasoning-level))

(defn- copilot-provider?
  [provider-id]
  (contains? #{:github-copilot :github-copilot-individual :github-copilot-business}
    provider-id))

(defn- copilot-llm-headers
  [resolved-model initiator]
  (when (and (copilot-provider? (:provider resolved-model))
          (#{"user" "agent"} initiator))
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
   `done(\"\"\"…\"\"\")` takes a positional string which `answer-fn` wraps into
   this `{:answer string}` shape. The only other accepted value is the
   `needs-input-answer?` map."
  [v]
  (and (map? v)
    (string? (:answer v))))

(defn answer-markdown
  "Extract the raw Markdown source from a final-answer value.

   Canonical shapes:
   - `{:answer string}`           -> the string (from `done(\"\"\"...\"\"\")`)
   - `{:vis/answer-mode :needs-input :answer/text string}` -> `:answer/text`
   - `[:ir {…} …]` canonical IR AST -> rendered to flat Markdown. loop.clj
     hands back an IR AST (NOT a Markdown map) for the provider-error /
     fatal-iteration fallbacks. The LIVE wire carries it as `:answer_ir`,
     but `session_turn_state.answer_markdown` is the only persisted answer
     channel — so without rendering it here a failed turn persisted a NULL
     answer and reopened as a bare \"(error)\" with the real message lost.

   Returns nil for anything else."
  [answer]
  (let [v (:result answer answer)]
    (cond
      (needs-input-answer? v) (:answer/text v)
      (markdown-answer? v)    (:answer v)
      (and (vector? v) (= :ir (first v)))
      (some-> (render/render v :markdown) str/trim not-empty)
      :else                   nil)))

(defn- unverified-done-tasks
  "Task titles the model closed as :done with a stated :acceptance but
   :verified? not true — work presented as done that was never confirmed.
   Pure; returns a vec of title strings (empty when none)."
  [ctx]
  (->> (vals (:session/tasks ctx))
    (filter (fn [t] (and (= :done (:status t))
                      (some? (:acceptance t))
                      (not (true? (:verified? t))))))
    (mapv (fn [t] (or (:title t) "task")))))

(defn- with-answer-markdown
  "Return `answer` with its Markdown `:answer` string replaced by `new-md`,
   handling both the bare `{:answer s}` and wrapped `{:result {:answer s}}`
   shapes. Any other value is returned unchanged."
  [answer new-md]
  (cond
    (and (map? (:result answer)) (string? (:answer (:result answer))))
    (assoc-in answer [:result :answer] new-md)
    (string? (:answer answer))
    (assoc answer :answer new-md)
    :else answer))

(defn append-runtime-appendices
  "Truthful close-of-turn backstop on the Markdown-answer pipeline.

   When the model closed tasks as :done with a stated :acceptance but never
   set :verified? true, append an `⚠ Unverified` note listing them to the
   answer Markdown — so vis NEVER presents unconfirmed work as done. Soft and
   honest BY DESIGN: it does NOT block the turn (verification may be
   impossible — vis has no test runner), it just tells the truth. Needs-input
   maps and non-Markdown values pass through untouched.

   Needs-input maps stay data-shaped so the prompt-flow gate can still read
   `:answer/text` without a render hop; Markdown-answer maps stay map-shaped so
   the persistence layer can read `:answer` verbatim."
  [environment answer _answer-value]
  (let [ctx (some-> (:ctx-atom environment) deref)
        unv (when ctx (unverified-done-tasks ctx))
        v   (:result answer answer)]
    (if (and (seq unv) (markdown-answer? v))
      (with-answer-markdown answer
        (str (:answer v)
          "\n\n---\n⚠ **Unverified** — closed without a verification check"
          (when (> (count unv) 1) (str " (" (count unv) ")"))
          ":\n"
          (str/join "\n" (map #(str "- " %) unv))))
      answer)))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")
(def ^:private MARKDOWN_FENCE_RE #"^\s*`{3,}[A-Za-z0-9_-]*\s*$")

(defn- bare-string-code-block? [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- markdown-fence-line? [line]
  (boolean (re-matches MARKDOWN_FENCE_RE (str line))))

(defn- markdown-fence-block? [expr]
  (let [lines (->> (str/split-lines (str expr))
                (map str/trim)
                (remove str/blank?))]
    (boolean (and (seq lines)
               (every? markdown-fence-line? lines)))))

(defn- comment-only-block? [^String expr]
  (try
    (zero? (env/count-top-level-forms (str/trim expr)))
    (catch Throwable _ false)))

;; multi-fence-hint / attach-multi-fence-hint removed: lenient mode (the
;; loop's only ask-code! path) never yields >1 block, so a multi-fence
;; merge can't happen and the reminder was unreachable.

(defn- literal-code-block-error [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer (the loop auto-detects plain text), not in :code."

    (markdown-fence-block? expr)
    "Raw Markdown fence leaked into :code (` ```... `). Remove the fence marker and keep only executable Clojure forms in the code block."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

;; The engine is full-Python: a block's source is the program verbatim and
;; passes through to eval untouched — no parsing, unwrapping, or reformatting.

(defn- python-op-error
  "Map a throwable from the Python eval path to the op-error shape: a GraalPy
   PolyglotException goes through env/map-polyglot-error (proper
   :python/syntax|runtime|host phase + line/column); anything else falls back to
   extension/ex->op-error. Class checked by NAME so this ns never imports the
   GraalPy classes directly."
  [e code]
  (try
    (if (= "org.graalvm.polyglot.PolyglotException" (.getName (class e)))
      (env/map-polyglot-error e code)
      (extension/ex->op-error e {:form-source code}))
    (catch Throwable _
      {:message (or (ex-message e) (.getName (class e)))})))

(defn- run-python-code
  "Run an agent code block through the embedded GraalPy sandbox. Wraps the
   worker-future + cancellation + tool-event/render sinks + `*1`/`*e` recovery
   stack around `env/run-python-block` (whole-block; tools fire in order through
   their ProxyExecutable wrappers, which read the SAME dynamic sinks)."
  [python-context code & {:keys [tool-event-fn env]}]
  (let [thrown        (atom nil)
        tool-counts   (atom {})
        cancel-token  (:cancel-token env)
        channel-sink  (atom [])
        sink-pos      (atom -1)
        record-tool-event (fn [event]
                            (let [op (:op event)
                                  n  (get (swap! tool-counts update op (fnil inc 0)) op)
                                  event* (cond-> event
                                           (not= n 1) (assoc :id (str (name (or op :tool)) "-" n)))]
                              (when tool-event-fn (tool-event-fn event*))))
        exec-future (cancellation/worker-future "vis-python-eval"
                      (fn []
                        (try
                          (binding [extension/*tool-event-sink* record-tool-event
                                    extension/*render-sink*      channel-sink
                                    extension/*sink-position*    sink-pos]
                            (assoc (env/run-python-block python-context code) :channel @channel-sink :lru {}))
                          (catch Throwable e
                            (reset! thrown e)
                            {:result nil :channel @channel-sink :lru {} :forms []
                             :error (python-op-error e code)}))))
        dispose-cancel-hook (when cancel-token
                              (cancellation/on-cancel! cancel-token
                                (fn [] (try (.cancel ^java.util.concurrent.Future exec-future true)
                                         (catch Throwable _ nil)))))
        timeout-ms  (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             (reset! thrown e)
                             (try (.cancel ^java.util.concurrent.Future exec-future true)
                               (catch Throwable _ nil))
                             {:result nil :channel @channel-sink :lru {}
                              :error (python-op-error e code)})
                           (finally
                             (when dispose-cancel-hook
                               (try (dispose-cancel-hook) (catch Throwable _ nil)))))]
    (when env
      (cond
        (nil? execution-result)          (env/push-eval-error! env (or @thrown (ex-info "Eval timeout" {})))
        (nil? (:error execution-result)) (env/push-eval-result! env (:result execution-result))
        :else (env/push-eval-error! env (or @thrown (ex-info (or (:message (:error execution-result))
                                                               "eval error") {})))))
    (if (nil? execution-result)
      (do (.cancel ^java.util.concurrent.Future exec-future true)
        {:result nil :channel @channel-sink :lru {}
         :error {:message (str "Timeout (" (/ timeout-ms 1000) "s)")} :timeout? true})
      execution-result)))

(defn- run-with-timing [python-context code _sandbox-ns timeout-ms start-time tool-event-fn env]
  (let [run! (fn [] (run-python-code python-context code :tool-event-fn tool-event-fn :env env))
        execution-result (if timeout-ms
                           (binding [*eval-timeout-ms* (clamp-eval-timeout-ms timeout-ms)] (run!))
                           (run!))
        finished-time    (System/currentTimeMillis)
        execution-time   (- finished-time start-time)]
    (cond-> execution-result
      true (assoc :execution-started-at-ms start-time
             :execution-finished-at-ms finished-time
             :duration-ms execution-time)
      (:timeout? execution-result) (assoc :timeout? true)
      (not (:timeout? execution-result)) (assoc :timeout? false))))

(defn- execute-code
  "Run a single :code block through the Python sandbox.

   Optional kwargs:
     :timeout-ms - hard-cap eval time, clamped at the
                   *eval-timeout-ms* bounds.

   Every call performs a real Python eval. There is no result cache:
   forms with side effects (e.g. host primitives `done(...)` and
   `set_session_title(...)`) MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [{:keys [python-context sandbox-ns] :as environment} code
   & {:keys [timeout-ms tool-event-fn]}]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :execute-code})]
    ;; Per-block-eval contract: feed original block source to `run-python-code`;
    ;; it parses, repairs delimiter slips when safe, then evaluates parsed
    ;; forms. Guard validators run against the repaired source when one exists
    ;; so a stray close paren does not block repair before eval.
    ;; Re-intern the bare `ctx` snapshot BEFORE every eval. The engine interns
    ;; sandbox bindings once at session start, so a static value would go
    ;; stale by iter 2; refreshing here keeps `ctx` == the rendered EDN the
    ;; model just read (and reflects intra-iter mutations across blocks).
    ;; The snapshot is an immutable read-only map — see
    ;; ctx-loop/session-snapshot for the read-only guarantee. Re-interning
    ;; also erases any `(def ctx …)` the model wrote in a prior block.
    (when python-context
      (try
        (when-let [snap (ctx-loop/session-snapshot environment)]
          ;; Bind `ctx` as a NATIVE Python dict built from the SAME canonical
          ;; projection (`ctx-renderer/project-ctx`) the `# ctx` text is printed
          ;; from — so the live `ctx` value and the rendered snapshot agree, and
          ;; the agent gets real dict ergonomics (.get / comprehensions / [k]).
          (env/bind-ctx! python-context (ctx-renderer/project-ctx snap)))
        (catch Throwable _ nil)))
    (let [start-time (System/currentTimeMillis)
          exec       (try
                       ;; The Python sandbox surfaces its own syntax/empty-block
                       ;; errors via env/run-python-block.
                       (run-with-timing python-context code sandbox-ns timeout-ms
                         start-time tool-event-fn environment)
                       (catch Throwable e
                         (env/push-eval-error! environment e)
                         {:result nil
                          :channel []
                          :lru {}
                          :error (try (extension/ex->op-error e {:form-source code})
                                   (catch Throwable _
                                     {:message (or (ex-message e)
                                                 (.getName (class e)))
                                      :type (-> e ex-data :type)}))
                          :execution-started-at-ms start-time
                          :execution-finished-at-ms (System/currentTimeMillis)
                          :duration-ms (- (System/currentTimeMillis) start-time)
                          :timeout? false}))]
      exec)))

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
   (cond
     (nil? err) nil
     (map? err) err
     (instance? Throwable err)
     (try (extension/ex->op-error err
            (cond-> {}
              code (assoc :form-source code)))
       (catch Throwable _
         {:message (or (ex-message err) (.getName (class err)))}))
     :else
     (cond-> {:message (str err)}
       code (assoc :block {:source code
                           :phase  (or phase :preflight)})))))

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  ;; These are provider/runtime failures, not model strategy failures.
  ;; svar already performs its own transport retry/fallback policy before
  ;; surfacing them to Vis, so feeding them back into the RLM only burns
  ;; visible iterations and cannot help the model self-correct.
  #{:svar.core/http-error
    :svar.llm/all-providers-exhausted
    :svar.llm/circuit-open
    :svar.llm/provider-exhausted})

(defn- infrastructure-error? [ex-data-map]
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
   within-turn compaction to recover (see CONTEXT_OVERFLOW_HOPELESS_FACTOR)."
  [ex-data-map]
  (and (= :svar.tokens/context-overflow (:type ex-data-map))
    (let [input (:input-tokens ex-data-map)
          max-input (:max-input-tokens ex-data-map)]
      (and (number? input) (number? max-input) (pos? max-input)
        (>= (double input)
          (* CONTEXT_OVERFLOW_HOPELESS_FACTOR (double max-input)))))))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview [messages]
  (when-let [c (some (fn [m] (when (= (:role m) "user") (:content m)))
                 (reverse messages))]
    (let [s (str c)]
      (if (> (count s) LAST_USER_PREVIEW_CHARS)
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
          " ...<+" (- (count s) LAST_USER_PREVIEW_CHARS) " chars>")
        s))))

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the turn row.
   Delegates to the unified `format-exception` and adds iteration context."
  [^Throwable e ctx]
  (format-exception e
    {:context {:iteration         (:iteration ctx)
               :messages-count    (count (:messages ctx))
               :routing           (:routing ctx)
               :reasoning-level   (:reasoning-level ctx)
               :last-user-preview (last-user-message-preview (:messages ctx))}}))

(defn handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.
   Infrastructure failures are terminal for the turn; model/format/code
   failures still return `{::iteration-error ...}` for RLM self-correction."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        iteration (:iteration ctx)
        hopeless-overflow? (hopeless-context-overflow? ex-data-map)
        fatal? (or (infrastructure-error? ex-data-map) hopeless-overflow?)
        iteration-error-data (exception->iteration-error-data e ctx)]
    (tel/log! {:level (if fatal? :error :warn)
               :data  (let [base (assoc (format-exception-short e) :iteration iteration)
                            ed   (ex-data e)
                            body (some-> (:body ed) str)]
                        (cond-> base
                          (:status ed)            (assoc :status (:status ed))
                          (:request-id ed)        (assoc :request-id (:request-id ed))
                          (:request_id ed)        (assoc :request-id (:request_id ed))
                          (and body (not (str/blank? body)))
                          (assoc :body-snippet (truncate body 1000))))}
      (cond
        hopeless-overflow?
        "Hopeless preflight context overflow - failing turn (feeding it back can never reach the model and only grows the input; VIS-9)"
        fatal?
        "Provider infrastructure error - failing turn without RLM restarts"
        :else
        "RLM iteration failed, feeding error to LLM"))
    (cond-> {::iteration-error iteration-error-data}
      fatal? (assoc ::fatal-iteration-error true))))

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
   via the `:vis/silent` return sentinel for host primitives such as
   `session-title`; normal value-bearing forms remain visible."
  [_environment _code result]
  result)

;; ---------------------------------------------------------------------------
;; Answer-scoping helper (Option C)
;;
;; The iteration loop discards a `done(...)` call iff the form
;; that ITSELF invoked it errored. Sibling errors (a typo in some
;; OTHER form, a bad v/edit elsewhere) do NOT gate termination -
;; the model's request to finalize is honored as long as the answer-
;; bearing form ran cleanly. Pre-Option C the loop discarded on ANY
;; sibling error, which is how a turn could rack up 148 retries with
;; the model repeatedly emitting `done(...)` next to a single
;; broken `(def ...)`.
;;
;; Returns the error from the form at `form-idx` in `form-results`
;; or nil when that form's evaluation succeeded. `form-idx` may be
;; nil (older answer-atom payloads) or out-of-bounds (defensive
;; against shape drift) - both yield nil (no discard).
;; ---------------------------------------------------------------------------

(defn answer-form-error
  "Return the `:error` produced by the form at `form-idx` in
   `form-results`, or nil when the form succeeded or `form-idx`
   is missing/out-of-bounds. Pure; no side effects. Public so the
   loop and tests can both reach it without re-implementing the
   bounds check."
  [form-results form-idx]
  (when (and form-idx
          (integer? form-idx)
          (not (neg? form-idx))
          (< form-idx (count form-results)))
    (:error (nth form-results form-idx))))

;; ---------------------------------------------------------------------------
;; Parsed form helpers
;; ---------------------------------------------------------------------------

(defn- entry-source
  "The verbatim Python source string for a code entry / source string.
   Entries built by `code-entries-preflight` carry the block source on
   `:expr`; a bare string is its own source."
  [entry-or-source]
  (cond
    (map? entry-or-source)    (str (or (:expr entry-or-source) (:source entry-or-source) ""))
    (string? entry-or-source) entry-or-source
    :else                     (str entry-or-source)))

(defn- session-title-meta-form?
  [entry-or-source]
  (= "set_session_title" (ctx-engine/form-head-name (entry-source entry-or-source))))

;; No raw-fence gate: the model legitimately writes ``` fences inside
;; `done("""…""")` strings; a truly stray fence surfaces as a Python
;; SyntaxError the model self-corrects.

;; Replay-dedup keys hash via `extension/sha256-hex` — the ONE
;; string-digest helper (this ns previously re-rolled MessageDigest + a
;; second, slower hex fold).

(defn- ask-code-block-observation
  "Block count for logs/chunks. Lenient mode (the loop's only ask-code!
   path) makes svar return ≤1 block and hardcode `:saw-fence?`/`:malformed?`
   to false, so only the count is informative — the fenced-era fence/dropped
   diagnostics (`empty-code-error-with-observation`) were removed."
  [ask-result]
  {:form-count (count (or (:blocks ask-result) []))})

;; `normalized-code-source` removed: `code-entries-preflight` now computes
;; the same join inline on the surviving block sources (was only ever called
;; from the splitter's old preflight path).

(def ^:private NEEDS_INPUT_CALL_RE #"\bneeds_input\s*\(")
(defn- form-contains-needs-input-call?
  [entry-or-source]
  (boolean (re-find NEEDS_INPUT_CALL_RE (entry-source entry-or-source))))

(defn- direct-answer-entry?
  [entry-or-source]
  (= "done" (ctx-engine/form-head-name (entry-source entry-or-source))))

;; `bare-symbol-entry?` removed with `plain-prose-code-error` — the
;; per-block-eval cut routes prose into the Python engine as a parse /
;; name error instead of detecting "every entry is a bare symbol" upfront.

;; Cut from this layer:
;;   - `plain-prose-code-error` + `prose->comment` — the splitter no longer
;;     produces multi-symbol entry vectors that a prose response could
;;     accidentally satisfy. With one block = one Python eval, prose lands in
;;     the engine as a parse error or name error and the model
;;     self-corrects from the structured error.
;;   - `duplicate-fenced-blocks?` + `dedupe-fenced-block-code` +
;;     `executable-form-source`/-`sources` — dedup now happens inline
;;     in `code-entries-preflight` on the block vector directly.

(defn- code-entries-preflight
  "Per-block-eval preflight. One svar Markdown code block becomes one
   code-entry; the block's `:source` is the entry's `:expr` verbatim.
   The Python engine parses + evals each entry as a single chunk during
   execution — there is no top-level form splitting at this layer.

   Gates retained:
     - `raw-markdown-fence-leak-error` per block. A nested ``` in the
       extracted source means svar's normalizer fell into a recursive shape;
       structured rejection beats a JVM crash.
     - Duplicate-block dedup. Some providers stutter and emit the same
       block twice; we keep the first copy and drop the rest.

  Top-level `(do ...)` wrappers are unwrapped before eval/display.
  Direct sibling top-level forms are canonical; nested host bookkeeping is
  not a supported display contract."
  [_iteration-position blocks]
  (let [blocks                       (vec (or blocks []))
        ;; Dedupe by source. Same as the old `dedupe-fenced-block-code`
        ;; but operates on the block vector directly.
        unique-blocks                (->> blocks
                                       (remove #(str/blank? (:source %)))
                                       (reduce (fn [{:keys [seen acc]} b]
                                                 (if (contains? seen (:source b))
                                                   {:seen seen :acc acc}
                                                   {:seen (conj seen (:source b))
                                                    :acc  (conj acc b)}))
                                         {:seen #{} :acc []})
                                       :acc)
        duplicate-blocks-normalized? (< (count unique-blocks) (count blocks))
        ;; Build entries: one block → one entry. Per-block raw-fence-leak
        ;; guard — if a single block's source still carries a literal
        ;; ```, reject just that block; sibling blocks keep their chance
        ;; to run.
        ;; Each block becomes one code-entry. The entry carries:
        ;;   :expr             — verbatim block source (fed to the engine as-is)
        ;;   :block-lang       — svar's stamped engine lang ("python")
        ;;   :render-segments  — per-form structural split for channel
        ;;                       rendering (P1.1; see
        ;;                       `render/parse-block-display`)
        ;;   :vis/structurally-silent?
        ;;                     — true iff the block contains ONLY structural
        ;;                       forms (`done(...)` / `(set-
        ;;                       session-title! ...)`); channels that
        ;;                       don't read segments can drop the whole entry.
        raw-entries                  (mapv (fn [b]
                                             (let [src (:source b)
                                                   ;; NO raw-markdown-fence-leak gate: the engine is
                                                   ;; full-Python and the model LEGITIMATELY writes
                                                   ;; ``` fences inside `done("""…""")` strings
                                                   ;; (markdown answers). The old guard used the
                                                   ;; Clojure reader (`code-string-reads-clean?`) to
                                                   ;; tell a stray fence from one inside a string —
                                                   ;; but it can't parse Python `f"""…"""`, so it
                                                   ;; FALSE-rejected valid code → empty `:code` →
                                                   ;; wasted "no executable code" retries (the model
                                                   ;; then fell back to dumping `str(tree)`). A truly
                                                   ;; stray fence now surfaces as a Python
                                                   ;; SyntaxError the model sees and self-corrects.
                                                   segments        (render/parse-block-display src)
                                                   ;; Structurally silent = parsed segments carry no
                                                   ;; `:code` at all (answer-only recap). The channel
                                                   ;; hides `:code` rows unless `:vis/show-raw-code`
                                                   ;; is ON; the engine only cares whether the block
                                                   ;; has any code so persistence agrees with paint.
                                                   structurally-silent?
                                                   (and (seq segments)
                                                     (not-any? #(= :code (:kind %)) segments))]
                                               {:expr       src
                                                :block-lang (:lang b)
                                                :render-segments segments
                                                :vis/structurally-silent? structurally-silent?}))
                                       unique-blocks)
        raw-fence-error              (some :vis/preflight-error raw-entries)
        parsed-total-blocks          (count raw-entries)
        empty-code-error             (when (zero? parsed-total-blocks)
                                       "LLM returned no executable code. Reply with a Python program (the whole reply is the program); put prose in done(\"\"\"...\"\"\").")
        ;; Normalized concat of all surviving block sources — also the
        ;; identity used for iteration-hash dedup in the trailer.
        normalized-code              (->> raw-entries
                                       (remove :vis/preflight-error)
                                       (map (comp str/trim :expr))
                                       (remove str/blank?)
                                       (str/join "\n\n"))
        code-hash                    (when-not (str/blank? normalized-code)
                                       (extension/sha256-hex normalized-code))]
    {:code-entries                  (if empty-code-error
                                      [{:expr ""
                                        :vis/preflight-error empty-code-error}]
                                      raw-entries)
     :empty-code-preflight-error    empty-code-error
     :raw-fence-preflight-error     raw-fence-error
     :duplicate-blocks-normalized?  duplicate-blocks-normalized?
     :normalized-code               normalized-code
     :code-hash                     code-hash
     :original-total-blocks         parsed-total-blocks}))

(defn- answer-validation-rejection-message
  [{:keys [id]} hit]
  (let [message (some-> (:message hit) str str/trim not-empty)
        hint    (some-> (:hint hit) str str/trim not-empty)]
    (str "Answer validation hook " id " rejected the final answer."
      (when message (str " " message))
      (when hint (str " Recovery: " hint)))))

(defn- answer-validation-hook-error-message
  [ext id ^Throwable t]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-threw
             :data {:ext (:ext/name ext)
                    :hook id
                    :phase :turn.answer/validate
                    :error (ex-message t)}})
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
    (some-> (:extensions environment) deref seq)))

(defn- mutation-block?
  "True when `block` ran a tool/op tagged `:mutation` (e.g. `v/patch`, a file
   write) — as opposed to a read-only `:observation` (`rg`/`cat`/`ls`). Drives
   the `done(…)`-as-proposal gate: a `done()` in the same fence as a MUTATION
   is decided before the mutation's outcome is observed, so it's surfaced for
   confirm/refine; a `done()` after pure reads finalizes directly. The engine
   stamps every op envelope with `:tag :observation | :mutation`."
  [block]
  (let [mut? (fn [env] (and (extension/tool-result? env) (= :mutation (:tag env))))]
    (boolean
      (or (mut? (:result block))
        (some #(mut? (:result %)) (:forms block))))))

(defn open-plan-steps-block
  "FORCING done-gate: a refusal STRING when the model tries to finalize while a
   `:plan? true` step is UNRESOLVED. A step is unresolved when ANY of:
     - it is an ACCEPTED-but-open step — node-outcome `:pending`/`:running`
       (todo/doing). A `:candidate` step is a PROPOSAL (chat-approve, stop-and-
       wait), NOT open work: propose-a-plan-then-done() is the designed flow, so
       candidates must NEVER block — else an all-candidate plan loops done()
       forever (the model retries, the gate keeps refusing). OR
     - it is `:done` with a stated `:acceptance` but BLANK/absent `:evidence`
       (EVIDENCE-not-status: a self-asserted `:done` without proof doesn't count —
       closes the 'mark everything done to escape' silencing loop), OR
     - it is a non-success terminal (`:cancelled`/`:deferred`/`:rejected`/`:failed`)
       with BLANK/absent `:reason` (blocks SILENT abandonment — an acknowledged
       reason is the honest escape, a quiet status flip is not).
   nil = clear to finalize. Only plan steps block; hook/non-plan tasks never do.
   Cleared per step via done(+evidence) / deferred(+reason) / cancelled(+reason).
   The harness pushing back — \"you still have these on your plate\". See
   dev/TASK_GATES_PROPOSAL.md (done-gate)."
  [_tasks]
  ;; Tasks are gone — there are no plan steps to block finalize. Always clear.
  nil)

(defn final-answer-gate-error
  "Dispatch `:turn.answer/validate` extension hooks against the
   candidate `done(…)` answer. Returns nil when every hook accepts,
   otherwise a single string surfaced as the rejected answer form's
   validation error.

   Runs the hard structural floor first: a final answer must not
   share an iteration with extension/tool calls. The model needs one
   iteration to observe tool output, then a later iteration may call
   `done(...)` using that evidence. Own-form errors are enforced
   upstream by `answer-form-error`. Extensions that need an additional
   veto (e.g. user-facing safety / format gates) still get their
   `:turn.answer/validate` hook fired here.

   `active-extensions` is passed by the turn loop so activation is
   computed once per turn; direct callers may omit it and provide
   `:extensions` on the environment."
  ([environment iteration blocks]
   (final-answer-gate-error environment iteration blocks nil nil))
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
     ;; Extension `:turn.answer/validate` vetoes only. The structural
     ;; "called a tool this iteration" case is NO LONGER a hard reject —
     ;; run-iteration treats it as a PROPOSAL (see `:answer-proposed?`):
     ;; the answer is surfaced back next turn with the now-visible tool
     ;; results for the model to confirm or refine.
     (some (fn [ext]
             (some (fn [{:keys [id phase] hook-fn :fn :as hook}]
                     (when (= :turn.answer/validate phase)
                       (binding [extension/*current-extension* ext
                                 extension/*current-symbol* nil]
                         (try
                           (let [hit (hook-fn ctx)]
                             (cond
                               (s/valid? ::extension/answer-validation-reject hit)
                               (answer-validation-rejection-message hook hit)

                               (and (map? hit) (:reject hit))
                               (answer-validation-invalid-return-message ext id hit)))
                           (catch Throwable t
                             (answer-validation-hook-error-message ext id t))))))
               (or (:ext/hooks ext) [])))
       (answer-validation-extensions environment active-extensions)))))

(defn- iteration-start-hook-hit
  "Normalise the value returned by a `:turn.iteration/start` hook into a
   hook-task shape (D12). Hooks emit `{:title :importance?}` maps. The
   loop wraps this into a slimmed task map keyed by the hook id, suitable
   for direct fold onto `:session/tasks`. The model satisfies a hook task
   by self-asserting `plan_step(\"hook-id\", {\"status\": \"done\"})` — there is no
   validator-fn and no proof.

   Returns `{:id <kw> :task <task-map> :emit {:tasks :facts}}`
   or nil. The `:emit` key carries optional secondary CTX writes:
   each entry under `:emit/tasks` / `:emit/facts` flows through
   `ctx-loop/apply-and-record!` exactly like a model-emitted mutator.
   Hooks may also return ONLY `{:emit ...}` to seed tasks/facts without
   registering a hook-task themselves -- in that case `:title` may be
   absent and `:task` returns nil.

   `lifetime` (`:turn` | `:session`) flows from the hook registration
   spec into the task. `:turn` tasks are dropped at advance-turn so a
   transient signal (e.g. context-pressure) doesn't linger in CTX;
   `:session` tasks (default) follow the standard TTL.

   Hooks that emit a hook-task but omit a non-blank `:title` (and carry
   no `:emit` payload) are rejected with a log warn."
  [ext id lifetime hit]
  (cond
    (nil? hit) nil

    (not (map? hit))
    (do (tel/log! {:level :warn
                   :id ::iteration-start-hook-invalid-return
                   :data {:ext (:ext/name ext) :hook id :returned hit}}
          "Extension :turn.iteration/start hook returned non-map value; expected nil or hook-task map")
      nil)

    :else
    (let [title         (:title hit)
          emit          (when (map? (:emit hit)) (:emit hit))
          hook-task?    (and (string? title) (not (str/blank? title)))]
      (cond
        ;; Pure-emit hook: no hook-task body, only :emit payload.
        (and emit (not hook-task?))
        {:id id :task nil :emit emit}

        (not hook-task?)
        (do (tel/log! {:level :warn
                       :id ::iteration-start-hook-missing-title
                       :data {:ext (:ext/name ext) :hook id :returned hit}}
              "Hook returned map without non-blank :title (and no :emit payload); dropping")
          nil)

        :else
        (cond-> {:id id
                 :task (cond-> {:title   title
                                :status  :todo
                                :source  :hook
                                :hook-id id}
                         (:importance hit) (assoc :importance (:importance hit))
                         lifetime          (assoc :lifetime lifetime))}
          emit (assoc :emit emit))))))

(defn- iteration-start-hook-error-hit
  [ext id t]
  (tel/log! {:level :warn
             :id ::iteration-start-hook-threw
             :data {:ext (:ext/name ext)
                    :hook id
                    :error (ex-message t)}}
    "Extension :turn.iteration/start hook threw")
  nil)

(defn- collect-iteration-start-hints
  "Run active `:turn.iteration/start` hooks and return model-facing
   hook-task descriptors (D12). Each descriptor is
   `{:id <kw> :task <task-map>}` ready for fold onto `:session/tasks`
   via `apply-mutator :task-set!`. Hook re-emission is idempotent at the
   engine layer: existing hook-tasks with the same id are noop'd. Hooks
   re-evaluate their condition every iter so once the condition lifts
   the hook stops emitting, and the existing terminal-status task drops
   from live ctx after gc-pass TTL."
  [_environment active-extensions ctx]
  (vec
    (mapcat (fn [ext]
              (keep (fn [{:keys [id phase lifetime] hook-fn :fn}]
                      (when (= :turn.iteration/start phase)
                        (binding [extension/*current-extension* ext
                                  extension/*current-symbol* nil]
                          (try
                            (iteration-start-hook-hit ext id lifetime (hook-fn ctx))
                            (catch Throwable t
                              (iteration-start-hook-error-hit ext id t))))))
                (or (:ext/hooks ext) [])))
      active-extensions)))

(defn- session-turn-position
  [environment session-turn-id]
  (or
    (try
      (when-let [session-id (:session-id environment)]
        (some (fn [turn]
                (when (= (str (:id turn)) (str session-turn-id))
                  (:position turn)))
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

(defn- previous-turn-context
  "Return latest completed prior turn as follow-up context.

   Cross-turn trailer carry preserves code/eval evidence, but final prose
   answers are otherwise absent from the next provider call. Short follow-ups
   (`yes`, `do it`, `the second one`) need the immediate prior Q/A as their
   referent, so seed exactly one latest prior answer from persistence."
  [environment current-turn-id]
  (try
    (when-let [session-id (:session-id environment)]
      (let [turns (persistance/db-list-session-turns (:db-info environment) session-id)
            answered? (fn [turn]
                        (let [answer (some-> (:answer-markdown turn) str str/trim)]
                          (and (seq answer)
                            (not= (str (:id turn)) (str current-turn-id))
                            (not= :running (:status turn)))))]
        (when-let [turn (last (filter answered? turns))]
          {:user-request (:user-request turn)
           :answer       (:answer-markdown turn)})))
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
      (let [db (:db-info environment)
            turns (or (persistance/db-list-session-turns db session-id) [])
            current-id (str current-turn-id)]
        (some (fn [turn]
                (let [iters (try
                              (persistance/db-list-session-turn-iterations db (:id turn))
                              (catch Throwable t
                                (tel/log! {:level :warn
                                           :id ::previous-request-iterations-failed
                                           :data {:session-id session-id
                                                  :session-turn-id (:id turn)
                                                  :error (ex-message t)}}
                                  "Could not load prior turn iterations while seeding utilization")
                                []))]
                  (when-let [it (last (filter #(pos? (long (or (:input-tokens %) 0))) iters))]
                    {:last-request-tokens        (long (:input-tokens it))
                     :last-request-turn-id       (:id turn)
                     :last-request-turn-position (:position turn)
                     :last-request-iteration     (:position it)})))
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
  "Monotonic update of `:engine/utilization` on the ctx-atom. UPGRADES when a
   real measurement (`util`) exists; NEVER removes an existing value. A
   transient nil — iter-1 seed miss, or an errored iteration that returned no
   usage — must not BLANK an already-shown utilization; that flicker is the
   `sometimes works / sometimes doesn't` bug. The last value carries on the
   per-session live atom (`:engine/*` is stripped only at persist time) until
   a fresh request refreshes it; a brand-new session starts blank because
   nothing was ever stamped."
  [ctx-atom util]
  (when (and ctx-atom util)
    (swap! ctx-atom assoc :engine/utilization util)))

(defn- runtime-turn-prefix
  [environment]
  (let [id-s (str (or (:session-turn-id (ctx-loop/read-turn-state environment))
                    (:environment-id environment)
                    "00000000"))
        prefix (subs id-s 0 (min 8 (count id-s)))]
    (if (re-matches #"(?i)[0-9a-f]{8}" prefix)
      prefix
      "00000000")))

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
  (cond
    (= :answer    (:role result)) :answer
    (= :tool      (:role result)) :tool
    (= :nudge     (:role result)) :nudge
    (= :thinking  (:role result)) :thinking
    (keyword? (:role result))     (:role result)
    ;; `done` returns the answer sentinel; a tool's keyword crosses `->py` into
    ;; Python and back as its snake STRING, so the canonical form-result sentinel
    ;; is "vis_answer" (NOT the keyword `:vis/answer`). See env-python/->py.
    (= "vis_answer" (:result result))   :answer
    :else :tool))

(defn- eval-envelope
  "Generic canonical envelope for every top-level form that passes
   through the Vis eval pipeline. Tool calls can add nested metadata
   in their returned envelope; this records the outer regular form
   evaluation so plain calls and tool calls share a common block-level
   trace."
  [turn-prefix iteration form-idx form-count result rendering-kind]
  (let [finished      (long (or (:execution-finished-at-ms result)
                              (System/currentTimeMillis)))
        duration      (long (or (:duration-ms result) 0))
        started       (long (or (:execution-started-at-ms result)
                              (max 0 (- finished duration))))
        form-position (inc (long form-idx))]
    {:op             (or (:op result)
                       (case rendering-kind
                         :nudge  :vis/system
                         :answer :vis/answer
                         :python/eval))
     :started-at-ms  started
     :finished-at-ms finished
     :status         (cond
                       (:timeout? result) :timeout
                       (:error result) :error
                       :else :done)
     :iteration      iteration
     :form-position  form-position
     :form-count     form-count
     :ref            (str "turn/" turn-prefix "/iteration/" iteration "/block/" form-position)
     :timeout?       (boolean (:timeout? result))
     :repaired?      (boolean (:repaired? result))}))

(defn- envelope-timestamps-ordered?
  [envelope]
  (<= (long (:started-at-ms envelope))
    (long (:finished-at-ms envelope))))

(defn- envelope-form-position-valid?
  [envelope]
  (<= (long (:form-position envelope))
    (long (:form-count envelope))))

(defn- envelope-ref-consistent?
  [envelope]
  (let [[_ iteration block] (re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/([1-9][0-9]*)/block/([1-9][0-9]*)$"
                              (:ref envelope))]
    (and iteration
      block
      (= (Long/parseLong iteration) (long (:iteration envelope)))
      (= (Long/parseLong block) (long (:form-position envelope))))))

(defn- envelope-has-no-derived-duration?
  [envelope]
  (not (contains? envelope :duration-ms)))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
          (nat-int? (:started-at-ms envelope))
          (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope))
             (long (:started-at-ms envelope))))))

(defn- block-duration-ms
  [block]
  (or (envelope-duration-ms (:envelope block)) 0))

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
  (s/and string?
    #(re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/[1-9][0-9]*/block/[1-9][0-9]*$" %)))
(s/def ::block-envelope
  (s/and
    (s/keys :req-un [::op ::status ::iteration ::form-position ::form-count
                     ::started-at-ms ::finished-at-ms ::ref]
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
   regular top-level form."
  [blocks]
  (let [blocks (mapv (fn [block]
                       (cond-> block
                         (contains? block :error)
                         (update :error op-error
                           {:code (:code block)
                            :phase (get-in block [:envelope :op])})))
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
  [{:keys [tokens]}]
  {:prompt_tokens (long (or (token-number tokens [:input]) 0))
   :completion_tokens (long (or (token-number tokens [:output]) 0))
   :completion_tokens_details {:reasoning_tokens (long (or (token-number tokens [:reasoning]) 0))}
   :prompt_tokens_details {:cached_tokens (long (or (token-number tokens [:cached :cached-input :input-cached]) 0))
                           :cache_creation_tokens (long (or (token-number tokens [:cache-created
                                                                                  :cache-created-input
                                                                                  :cache-creation
                                                                                  :cache-write
                                                                                  :cache_creation])
                                                          0))}})

(defn- reasoning-effort-configurable?
  "True when a model accepts a caller-selected reasoning effort.

   `:reasoning?` means the model can produce reasoning/thinking text.
   It does NOT imply that Vis may tune that thinking depth. Z.ai GLM
   thinking is binary/preserved-thinking only, so keep the stream visible
   but do not send `:reasoning` levels to svar. Providers can opt out
   explicitly with `:reasoning-effort? false`."
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
    (filter (fn [b] (= "thinking" (:type b))))
    (map (fn [b] (count (or (:thinking-signature b) (:thinking b) ""))))
    (reduce + 0)))

(defn- preserved-thinking-replay-messages
  "Provider-agnostic preserved-thinking replay. Returns every compatible
   `:assistant-message` from `trailer-iters` in arrival order.

   Why every message, not just the last:
     - Z.ai / GLM-5.x preserved thinking (`clear_thinking: false`) keeps
       reasoning_content across assistant turns only when each prior
       assistant message echoes the model's full reasoning back. Drop a
       step and GLM either re-derives the same scratch state at every
       iteration (observed: session 3102ad16, 26 iterations re-reading
       the same file with `cached_tokens` pinned at 2368) or starts to
       hallucinate that an earlier conclusion is still live.
     - Anthropic extended thinking signs each block with an HMAC and
       refuses replay if the chain is broken; sending only the last
       block fails signature validation as soon as the model produced
       more than one block since the user message.
     - OpenAI Responses encrypted reasoning items must replay in order
       — the next call rejects a single isolated item with
       'reasoning without following item'.

   The earlier conservative 'last-only' policy (rationale comment
   referenced session a9389e1d) was tuned for pre-`clear_thinking`
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
  (let [msgs (vec (keep #(some-> % second :assistant-message) trailer-iters))]
    (when (seq msgs)
      ;; Keep this call so oversized reasoning chains are observable to
      ;; future budget instrumentation. Sum across the full chain instead
      ;; of just the latest step — budget watchers care about cumulative
      ;; replay size, not single-step size.
      (doseq [m msgs] (replay-reasoning-chars m)))
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
   :model    (some-> (:name resolved-model) str)})

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
  (let [thinking  (:thinking block)
        signature (:thinking-signature block)]
    (and (string? thinking)
      (not (str/blank? thinking))
      (string? signature)
      (= thinking signature))))

(defn- assistant-message-compatible-with-replay-target?
  [target assistant-message]
  (not (and (anthropic-replay-context? target)
         (some anthropic-invalid-thinking-replay-block?
           (thinking-blocks assistant-message)))))

(defn- actual-llm-provider
  "Provider that actually served an ask-result. svar may route/fallback
   inside ask-code!, so prefer routed metadata over Vis' pre-call guess."
  [resolved-model ask-result]
  (or (:routed/provider-id ask-result)
    (:provider resolved-model)))

(defn- actual-llm-model
  "Model that actually served an ask-result. See `actual-llm-provider`."
  [resolved-model ask-result]
  (or (:routed/model ask-result)
    (some-> (:name resolved-model) str)))

(defn- llm-id
  [provider model]
  (cond-> {}
    provider (assoc :provider (name (keyword provider)))
    model    (assoc :model (str model))))

(defn- llm-routing-summary
  [selected-model iteration-result]
  (let [routing-trace (vec (or (:llm-routing-trace iteration-result) []))
        fallback-ev   (first (filter #(contains? #{:llm.routing/provider-fallback
                                                   :llm.routing/format-fallback}
                                        (:event/type %))
                               routing-trace))
        ;; The authoritative anchors are the fallback event's from/to when a
        ;; real fallback was traced: the router may pre-resolve so the iteration
        ;; result's provider/model already reflect the FALLBACK, which would
        ;; otherwise collapse selected==actual and drop the '↳ from …' note.
        selected (llm-id (or (:from-provider fallback-ev) (:provider selected-model))
                   (or (:from-model fallback-ev) (some-> (:name selected-model) str)))
        actual   (llm-id (or (:to-provider fallback-ev) (:llm-provider iteration-result) (:provider selected-model))
                   (or (:to-model fallback-ev) (:llm-model iteration-result) (some-> (:name selected-model) str)))]
    (cond-> {:selected selected
             :actual   actual
             :fallback? (boolean
                          (or (not= selected actual)
                            (some #(not= :llm.routing/provider-retry (:event/type %)) routing-trace)))}
      (seq routing-trace) (assoc :trace routing-trace))))

(defn- attach-llm-routing-summary
  [result selected-model iteration-result]
  (let [routing  (llm-routing-summary selected-model iteration-result)
        actual   (:actual routing)
        selected (:selected routing)]
    (cond-> (assoc result
              :provider (:provider actual)
              :model    (:model actual)
              :llm-selected selected
              :llm-actual actual
              :llm-fallback? (:fallback? routing))
      (seq (:trace routing))
      (assoc :llm-routing-trace (:trace routing))
      (:cost result)
      (update :cost merge (select-keys actual [:provider :model])))))

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
    (filterv (fn [[_ {:keys [assistant-message llm-provider llm-model]
                      replay? :preserved-thinking/replay?}]]
               (and (not= false replay?)
                 assistant-message
                 (= target-provider llm-provider)
                 (= target-model llm-model)
                 (assistant-message-compatible-with-replay-target?
                   target assistant-message)))
      (or trailer-iters []))))

;; -----------------------------------------------------------------------------
;; Frozen trailer messages — prefix-cache-friendly form-result history.
;;
;; The trailer used to render INSIDE the single regenerated `<context>`
;; user message at the end of every provider call. Because the prefix
;; cache ends at the first changed byte, the WHOLE accumulated trailer
;; was re-billed uncached on every iteration (measured: cached tokens
;; pinned at ~8k while prompts grew past 22k; ~91% of the uncached
;; spend). The fix: each trailer pin renders ONCE into a permanent
;; `<results>` user message, interleaved chronologically with the
;; assistant replays, so the conversation grows APPEND-ONLY:
;;
;;   [system, user_initial,
;;    <pre-turn pins>,
;;    asst_iter1, <results t/i1>,
;;    asst_iter2, <results t/i2>,
;;    ...,
;;    <mutable context tail>]
;;
;; Compaction (`summarize` / auto-fold) REWRITES pins → the frozen
;; messages change → one deliberate cache bust, paid only under window
;; pressure instead of on every call.
;; -----------------------------------------------------------------------------

(defn- parse-pin-position
  "`{:turn N :iter M}` from a pin's `:scope` (form pins) or `:scope-end`
   (summary pins — placed at the END of the range they replace). nil
   when the scope doesn't parse."
  [pin]
  (when-let [s (or (:scope pin) (:scope-end pin))]
    (when-let [[_ t i] (re-matches #"t(\d+)/i(\d+).*" (str s))]
      {:turn (parse-long t) :iter (parse-long i)})))

(defn- frozen-trailer-messages
  "Build the append-only conversation suffix from the CURRENT trailer +
   this turn's preserved-thinking replays.

   Returns:
     {:pins   [user-msg ...]   ; <results> messages ONLY — what the
                               ; budget guard measures (replays were
                               ; never budgeted; z.ai reasoning echoes
                               ; are billed-free)
      :suffix [msg ...]}       ; the full wire suffix: pre-turn pins,
                               ; then per-iteration [asst replay,
                               ; <results>] pairs in position order.

   Append-only invariant: within a turn, iteration K's suffix is a
   WIRE-prefix of iteration K+1's as long as no pin was rewritten —
   positions only grow, old pins and old replays are immutable, and
   `render-trailer-pin` is deterministic. (The moving `:svar/cache`
   breakpoint reshapes the LAST pin's vis-level message, but string
   content and a single text block serialize to identical wire bytes —
   only the cache_control placement moves, which never invalidates the
   cached prefix.) Unparseable scopes group with the pre-turn pins
   (stable position at the front)."
  [env trailer-iters target turn-position]
  (let [pins          (vec (or (some-> (:ctx-atom env) deref :session/trailer) []))
        compatible    (compatible-preserved-thinking-trailer-iters trailer-iters target)
        ;; Through `preserved-thinking-replay-messages` (not a bare
        ;; :assistant-message pull) so the oversized-chain telemetry
        ;; stays. The upstream filter guarantees every compatible entry
        ;; carries an assistant-message → 1:1 zip with positions.
        replays       (vec (or (preserved-thinking-replay-messages compatible) []))
        replay-by-pos (into (sorted-map)
                        (zipmap (map (comp long first) compatible) replays))
        pin-info      (mapv (fn [p] [(parse-pin-position p) p]) pins)
        this-turn?    (fn [[info _]]
                        (and info (= (long (or turn-position 0)) (long (:turn info)))))
        pre-pins      (mapv second (remove this-turn? pin-info))
        pins-by-pos   (reduce (fn [m [{:keys [iter]} p]]
                                (update m (long iter) (fnil conj []) p))
                        (sorted-map)
                        (filter this-turn? pin-info))
        positions     (sort (distinct (concat (keys replay-by-pos) (keys pins-by-pos))))
        pin-msg       (fn [p] {:role "user" :content (ctx-renderer/render-trailer-pin p)})
        ;; PRE-TURN pins render WITH their (compacted) form sources:
        ;; assistant replays never cross the turn boundary, so without
        ;; src these are orphaned results — output with no visible call.
        pre-pin-msg   (fn [p] {:role "user"
                               :content (ctx-renderer/render-trailer-pin p {:include-src? true})})
        pre-msgs      (mapv pre-pin-msg pre-pins)
        cur-pin-msgs  (vec (mapcat (fn [pos] (map pin-msg (get pins-by-pos pos []))) positions))
        ;; Anthropic prompt caching is BREAKPOINT-based: svar auto-tags
        ;; only the system prompt, so without a marker here nothing of
        ;; the conversation caches there. Tag the LAST <results> message
        ;; — the breakpoint moves forward as pins append, so each call
        ;; hits the previously cached prefix incrementally. Providers
        ;; with token-prefix auto-caching strip `:svar/*` keys (no-op).
        ;; Only PIN messages are tagged (replay messages carry
        ;; provider-native thinking payloads; don't reshape those).
        mark-last-pin (fn [msgs]
                        (if-let [idx (last (keep-indexed
                                             (fn [i m]
                                               (when (and (= "user" (:role m))
                                                       (string? (:content m)))
                                                 i))
                                             msgs))]
                          (update msgs idx
                            (fn [m] (assoc m :content
                                      [{:type "text" :text (:content m)
                                        :svar/cache true}])))
                          msgs))]
    {:pins   (into pre-msgs cur-pin-msgs)
     :suffix (mark-last-pin
               (vec (concat
                      pre-msgs
                      (mapcat (fn [pos]
                                (concat
                                  (when-let [m (get replay-by-pos pos)] [m])
                                  (map pin-msg (get pins-by-pos pos []))))
                        positions))))}))

(declare rejection-fact-entries)

(defn run-iteration
  "Runs a single RLM iteration: ask! -> check final -> execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc."
  [environment messages & [{:keys [routing iteration reasoning-level resolved-model on-chunk extra-body llm-headers active-extensions answer-validation-context]}]]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :run-iteration})]
    (let [iteration-position (inc (long (or iteration 0)))
          turn-prefix (runtime-turn-prefix environment)
          turn-position (or (:turn-position (ctx-loop/read-turn-state environment)) 1)
          form-scope (fn [idx]
                       (str "t" turn-position "/i" iteration-position "/f" (inc idx)))
          effective-reasoning (when (and (some? reasoning-level)
                                      (reasoning-effort-configurable? resolved-model))
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Reasoning fallback: when the turn asked for reasoning but the model
          ;; has no native thinking channel (`:reasoning?` absent — e.g. a local
          ;; LM Studio model), give it a scratchpad in the code itself via
          ;; `;;` comments. Effort-configurable models reason natively and skip it.
          reason-via-comments? (and (some? reasoning-level)
                                 (not (:reasoning? resolved-model)))
          messages (cond-> messages
                     reason-via-comments? prompt/with-reasoning-comments-nudge)
          ;; Reset the per-environment answer-atom before this iteration.
          ;; The Python sandbox's `done("""...""")` fn `reset!`s it during
          ;; code evaluation; we read it back after all forms run.
          answer-atom (or (:answer-atom environment)
                        (throw (ex-info "environment missing :answer-atom"
                                 {:type :vis/missing-answer-atom})))
          _ (reset! answer-atom nil)
          ;; Form-index pointer the executed-mapv reset!s before each
          ;; expression's eval so `answer-fn` can stamp `:form-idx` on
          ;; the answer-atom payload. Pairs with the discard check
          ;; below: an answer is gated only by the form that emitted
          ;; it, not by sibling forms. Form-idx now lives on the
          ;; single turn-state-atom (see ctx-loop/make-turn-state-atom).
          turn-state-atom (or (:turn-state-atom environment)
                            (throw (ex-info "environment missing :turn-state-atom"
                                     {:type :vis/missing-turn-state-atom})))
          _ (swap! turn-state-atom assoc :form-idx nil)
          ;; Stream reasoning chunks to the TUI while the LLM is
          ;; thinking. Every chunk carries `:phase` - consumers
          ;; dispatch on it. Phases:
          ;;   :reasoning      - LLM streaming reasoning text
          ;;   :form-start     - one form started evaluating (per-form)
          ;;   :form-result    - one form finished evaluating (per-form)
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
          content-len-volatile   (volatile! 0)
          reset-stream-state!     (fn []
                                    (vreset! reasoning-len-volatile 0)
                                    (vreset! content-len-volatile 0))
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning content done?] :as chunk}]
                           (cond
                             (:event/type chunk)
                             (on-chunk {:phase           :provider-fallback
                                        :iteration-count iteration-position
                                        :event           chunk})

                             :else
                             (do
                               (when (or (some? reasoning) done?)
                                 (let [thinking (some-> reasoning str)
                                       prev-len (long @reasoning-len-volatile)
                                       cur-len  (long (count (or thinking "")))
                                       delta (cond
                                               (nil? thinking)        nil
                                               (< cur-len prev-len)   thinking
                                               (= cur-len prev-len)   ""
                                               :else                  (subs thinking prev-len))]
                                   (vreset! reasoning-len-volatile cur-len)
                                   (on-chunk {:phase           :reasoning
                                              :iteration-count iteration-position
                                              :thinking  thinking
                                              :delta     delta
                                              :done?     (boolean done?)})))
                               (when (some? content)
                                 ;; Stream provider content (the answer
                                 ;; markdown + code fence) so the bubble
                                 ;; surfaces live progress between reasoning
                                 ;; and parsed forms. Same delta math as
                                 ;; reasoning; consumers redraw or append.
                                 (let [content-s (some-> content str)
                                       prev-len  (long @content-len-volatile)
                                       cur-len   (long (count (or content-s "")))
                                       delta (cond
                                               (nil? content-s)       nil
                                               (< cur-len prev-len)   content-s
                                               (= cur-len prev-len)   ""
                                               :else                  (subs content-s prev-len))]
                                   (vreset! content-len-volatile cur-len)
                                   (on-chunk {:phase           :content
                                              :iteration-count iteration-position
                                              :content   content-s
                                              :delta     delta
                                              :done?     (boolean done?)})))))))
          copilot-initiator (copilot-initiator-for-iteration iteration)
          effective-llm-headers (not-empty
                                  (merge (copilot-llm-headers resolved-model copilot-initiator)
                                    llm-headers))
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
          session-cache-key (some-> (:session-id environment) str)
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
          ask-opts (with-default-ask-code-idle-timeout
                     (cond-> {:lang     "python"
                              ;; FENCED mode (NOT lenient): the model wraps its
                              ;; Python in a ```python … ``` block and svar
                              ;; extracts ONLY the fenced interior — any prose
                              ;; OUTSIDE the fence is IGNORED, not run. Structural
                              ;; prose-immunity: a stray sentence before/after the
                              ;; code no longer turns the whole reply into a syntax
                              ;; error (the GPT/Copilot prose failure mode). Keep
                              ;; `:code-tail-pointer? true`: svar then appends its
                              ;; FENCED reminder ("reply with exactly one ```python
                              ;; fence") as the recency nudge the system prompt
                              ;; alone loses on long transcripts.
                              :lenient  false
                              :code-tail-pointer? true
                              :messages messages
                              :routing  sticky-routing
                              :check-context? true
                              :preserved-thinking? true}
                       session-cache-key (assoc :cache-key session-cache-key)
                       effective-reasoning  (assoc :reasoning effective-reasoning)
                       streaming-fn         (assoc :on-chunk streaming-fn)
                       effective-llm-headers (assoc :llm-headers effective-llm-headers)
                       extra-body           (assoc :extra-body extra-body)
                       ;; Caller-driven cancellation (svar 0.7.19+): a no-arg
                       ;; predicate svar polls on a watchdog so a user Stop
                       ;; aborts the in-flight SSE read in ~50ms (close the
                       ;; body stream + interrupt) instead of waiting for the
                       ;; whole response or a 30s/120s timeout. Reads the same
                       ;; cancel-atom `vis/cancel!` flips.
                       (:cancel-atom environment)
                       (assoc :cancel-fn
                         (let [ca (:cancel-atom environment)]
                           (fn [] (boolean (deref ca)))))))
          ask-result-raw (binding [svar-llm/*log-context* (assoc svar-llm/*log-context*
                                                            :session-turn-id (:environment-id environment)
                                                            :iteration iteration-position)]
                           (call-provider-with-stream-rewind-retry!
                             environment
                             {:iteration-position iteration-position
                              :provider (some-> (:provider resolved-model) name)
                              :model (some-> (:name resolved-model) str)
                              :on-chunk on-chunk
                              :reset-stream-state! reset-stream-state!}
                             #(call-provider-with-interrupt-retry! environment iteration-position
                                (fn []
                                  (svar/ask-code! (:router environment) ask-opts)))))
          ask-result ask-result-raw
          code-observation (ask-code-block-observation ask-result)
          provider-duration-ms (elapsed-ms provider-start-ns)
          _ (log-stage! :provider-call/stop iteration
              (merge {:duration-ms provider-duration-ms
                      :raw-length (count (or (:raw ask-result-raw) ""))
                      :tokens (:tokens ask-result-raw)
                      :fallback? (boolean (some #(not= :llm.routing/provider-retry (:event/type %)) (:routed/trace ask-result-raw)))}
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
          _ (log-stage! :llm-response iteration
              (merge {:has-reasoning (some? model-reasoning)
                      :raw-length    (count (or (:raw ask-result) ""))
                      :duration-ms   (:duration-ms ask-result)
                      :provider-duration-ms provider-duration-ms
                      :tokens        (:tokens ask-result)
                      :thinking      thinking}
                code-observation))
          api-usage (ask-result->api-usage ask-result)
          ;; svar/ask-code! returns the per-block vector in `:blocks`
          ;; (single source of truth; the `:result` concatenated
          ;; string was removed in svar v0.5.3). One block → one
          ;; code-entry; the engine evaluates each entry as a single chunk.
          blocks (vec (:blocks ask-result))
          preflight-start-ns (System/nanoTime)
          preflight-result (code-entries-preflight iteration-position blocks)
          preflight-duration-ms (elapsed-ms preflight-start-ns)
          {:keys [code-entries normalized-code]} preflight-result
          _ (log-stage! :response-preflight/stop iteration
              (merge {:duration-ms preflight-duration-ms
                      :code-length (count normalized-code)
                      :forms (count code-entries)
                      :raw-fence-preflight? (boolean (:raw-fence-preflight-error preflight-result))}
                code-observation))
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :done
                         :iteration iteration-position
                         :duration-ms preflight-duration-ms
                         :code-length (count normalized-code)
                         :forms (count code-entries)
                         :code-observation code-observation}))
          direct-answer-entry (when (= 1 (count code-entries))
                                (first code-entries))
          final-answer-preflight-error
          (when (and direct-answer-entry
                  (not (:vis/preflight-error direct-answer-entry))
                  (direct-answer-entry? direct-answer-entry)
                  (not (form-contains-needs-input-call? direct-answer-entry)))
            ;; Pass prior-iteration context so the structural gate sees
            ;; evidence-prior? correctly. Without this, a clean answer-only
            ;; iteration after several probe iterations is falsely rejected
            ;; as "no evidence for this turn yet" because the gate's
            ;; `previous-iterations` defaults to nil. That bug burns 5-7+
            ;; iterations on simple tasks (see autoresearch fw-005 trace).
            (final-answer-gate-error environment iteration-position [] nil
              active-extensions
              (assoc answer-validation-context
                :position 0
                :code-entries code-entries)))
          code-entries (if final-answer-preflight-error
                         [{:expr "(vis/preflight-error :final-answer-gate)"
                           :vis/preflight-error final-answer-preflight-error}]
                         code-entries)
          suppress-form-start? (or (some :vis/preflight-error code-entries)
                                 final-answer-preflight-error)
          total-blocks (count code-entries)
          executed (mapv (fn [idx {:keys [expr render-segments]
                                   :vis/keys [preflight-error structurally-silent?]
                                   form-repaired? :repaired?
                                   :as entry}]
                           (log-stage! :code-exec iteration
                             {:idx (inc idx) :total total-blocks :code expr})
                           (when (and on-chunk
                                   (not suppress-form-start?)
                                   (not (session-title-meta-form? entry))
                                   (not structurally-silent?))
                             (on-chunk {:phase           :form-start
                                        :iteration-count iteration-position
                                        :position        idx
                                        :count           total-blocks
                                        :scope           (form-scope idx)
                                        :code            expr
                                        :render-segments render-segments
                                        :vis/structurally-silent? (boolean structurally-silent?)
                                        :started-at-ms   (System/currentTimeMillis)}))
                           ;; Stamp form-idx BEFORE eval so any
                           ;; `done(...)` call inside this form
                           ;; captures the right index on the
                           ;; answer-atom payload.
                           (swap! turn-state-atom assoc :form-idx idx)
                           (let [scope (form-scope idx)
                                 raw-result (cond
                                              preflight-error
                                              {:result nil
                                               :error (op-error preflight-error
                                                        {:code expr :phase :vis/preflight})
                                               :duration-ms 0
                                               :op :vis/guard}
                                              :else
                                              (if-let [err (literal-code-block-error expr)]
                                                {:result nil
                                                 :error (op-error err {:code expr :phase :vis/guard})
                                                 :duration-ms 0
                                                 :op :vis/guard}
                                                (let [tool-event-fn (when (and on-chunk
                                                                            (not suppress-form-start?)
                                                                            (not structurally-silent?))
                                                                      (fn [tool-event]
                                                                        (on-chunk {:phase           :tool-start
                                                                                   :iteration-count iteration-position
                                                                                   :position        idx
                                                                                   :count           total-blocks
                                                                                   :scope           scope
                                                                                   :code            expr
                                                                                   :render-segments render-segments
                                                                                   :vis/structurally-silent? (boolean structurally-silent?)
                                                                                   :tool-event     tool-event})))
                                                      r (if tool-event-fn
                                                          (execute-code environment expr :tool-event-fn tool-event-fn)
                                                          (execute-code environment expr))]
                                                  (log-stage! :code-result iteration
                                                    {:idx (inc idx) :total total-blocks
                                                     :duration-ms (:duration-ms r)
                                                     :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                                  r)))
                                 ;; Carry parinfer's whole-source
                                 ;; rebalance flag into the per-form
                                 ;; result. `execute-code` may also
                                 ;; set `:repaired?` (extension hook
                                 ;; rescue); both paths converge on
                                 ;; the same flag for the channel.
                                 result (cond-> raw-result
                                          form-repaired? (assoc :repaired? true)
                                          (:auto-repaired raw-result) (assoc :repaired? true))
                                 display-result (def-display-result environment expr result)
                                 ;; def-display-result is now a pass-through; kept on the
                                 ;; call path so future display-tweaks have a single seam.

                                 block-role (eval-block-role display-result)
                                 envelope (eval-envelope turn-prefix iteration-position idx total-blocks display-result block-role)
                                 result* (assoc display-result
                                           :envelope envelope
                                           :role block-role)]
                             ;; Per-form streaming chunk (:phase
                             ;; :form-result). Fires the moment a
                             ;; form lands so the channel can render
                             ;; per-form results incrementally instead
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
                               (on-chunk {:phase             :form-result
                                          :iteration-count   iteration-position
                                          :position          idx
                                          :count             total-blocks
                                          :scope             scope
                                          :code              expr
                                          :render-segments   render-segments
                                          :vis/structurally-silent? (boolean structurally-silent?)
                                          :result            (:result result*)
                                          :channel           (:channel result*)
                                          :error             (:error result*)
                                          :envelope          (:envelope result*)
                                          :role              (:role result*)
                                          ;; :silent? is the channel-facing hide flag. A block is
                                          ;; hidden ONLY when it is structurally code-free —
                                          ;; `structurally-silent?` (segments carry only
                                          ;; answer/title recaps, no `:code`). The `:vis/silent`
                                          ;; RESULT sentinel no longer hides the block: it only
                                          ;; suppresses the RESULT echo (channels drop a nil
                                          ;; result-render). This is what lets `plan_step` /
                                          ;; `fact_set` — which return `:vis/silent` but now
                                          ;; classify as `:code` — show their CALL while their
                                          ;; effect lives in the context dialog.
                                          :silent?     (boolean structurally-silent?)
                                          :timeout?          (boolean (:timeout? result*))
                                          :repaired?         (boolean (:repaired? result*))
                                          :auto-repaired?    (boolean (:auto-repaired result*))}))
                             {:block expr
                              :result result*
                              :render-segments render-segments
                              :vis/structurally-silent? (boolean structurally-silent?)}))
                     (range) code-entries)
          form-sources    (mapv :block executed)
          form-results  (mapv :result executed)
          form-segments (mapv :render-segments executed)
          form-silents  (mapv :vis/structurally-silent? executed)
          ;; Preflight gate → synthetic block carries `:vis/preflight? true`
          ;; so channels can suppress the model-facing-only error box. Keep
          ;; the block in the persisted/trailer stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range) (map (fn [{:vis/keys [preflight-error]}]
                                                  (boolean preflight-error))
                                             code-entries))
          _ (when (:ctx-atom environment)
              (doseq [[fk fpartial] (rejection-fact-entries turn-position iteration-position
                                      code-entries form-results)]
                (ctx-loop/apply-and-record! environment :fact-set! [fk fpartial])))
          blocks (validate-iteration-blocks!
                   (mapv (fn [idx code result segments structurally-silent?]
                           (cond-> {:id idx
                                    :code code
                                    :result (:result result)
                                    :channel (:channel result)
                                    :error (op-error (:error result) {:code code :phase (get-in result [:envelope :op])})
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
                                    ;; Per-form outcomes: one entry per parsed
                                    ;; top-level form with {:source :result :error}.
                                    ;; The REPL trailer renders these in order so
                                    ;; the model sees every form's value, not
                                    ;; just the last one. Empty when the parser
                                    ;; rejected the source (fell back to whole-
                                    ;; block eval).
                                    :forms (vec (:forms result))
                                    ;; If the engine auto-repaired delimiter
                                    ;; mistakes (parinferish) before eval, the
                                    ;; repaired source flows here so the trailer
                                    ;; can disclose the diff and the model can
                                    ;; correct itself if the repair was wrong.
                                    :repaired-source (:repaired-source result)}
                             ;; Per-form render breakdown for channel display.
                             ;; Channels that read :render-segments hide
                             ;; done(…) / set_session_title(…)
                             ;; forms while keeping the prelude visible.
                             ;; Legacy channels that only read :code fall
                             ;; back to the full block source.
                             (seq segments) (assoc :render-segments segments)
                             structurally-silent? (assoc :vis/structurally-silent? true)
                             (:vis/silent result) (assoc :vis/silent true)
                             (get preflight-by-idx idx) (assoc :vis/preflight? true)))
                     (range) form-sources form-results form-segments form-silents))
          silent-form-idxs (into #{}
                             (keep-indexed (fn [idx block]
                                             ;; Only structurally code-free blocks hide. A
                                             ;; `:vis/silent` RESULT alone no longer elides a
                                             ;; code-bearing block (plan_step/fact_set now show
                                             ;; their call; their result echo is suppressed
                                             ;; downstream instead).
                                             (when (:vis/structurally-silent? block)
                                               idx)))
                             blocks)]
      (if-let [{value :value form-idx :position} @answer-atom]
          ;; FINAL path: model called `done("""...""")` during this
          ;; iteration. Atom payload is `{:value :form-idx}`. The
          ;; form-scoped error gate fires if the answer-bearing form's
          ;; own evaluation errored anyway
          ;;      (e.g. `(do (v/edit ...throws...) done("""x"""))` -
          ;;      the form had inner work that crashed), the answer
          ;; answer is discarded with the form's own error. Sibling
          ;; forms before the answer-form may error freely; that
          ;; doesn't gate termination.
          ;;
          ;; `resolved-model` is a MAP - `{:name str :provider kw
          ;; :reasoning? bool}` - not a string. Persisting `(str
          ;; resolved-model)` would land a stringified map in
          ;; `iteration.llm_model`; surface `:name` and `:provider`
          ;; separately so both columns get clean values.
        ;; `value` is already canonical `[:ir & nodes]` (or a
        ;; needs-input map) - `answer-fn` ran `render/->ast` at the
        ;; engine boundary. Persist the IR as-is; channels render at
        ;; their boundary via `:channel/messages-renderer-fn`.
        (let [final-answer    value
              total-forms     (count code-entries)
              own-form-error  (answer-form-error form-results form-idx)
              gate-error      (when (nil? own-form-error)
                                (final-answer-gate-error environment iteration-position blocks value active-extensions
                                  (assoc answer-validation-context
                                    :position form-idx
                                    :code-entries code-entries)))
              validation-error (cond
                                 own-form-error
                                 (error/final-answer-code-error-message own-form-error)
                                 gate-error
                                 gate-error)
              ;; PROPOSAL: a `done(…)` emitted in the same fence as tool /
              ;; extension calls was decided BEFORE those results were
              ;; observed. Don't finalize and DON'T error — flag it so the
              ;; turn loop shows the proposed answer back next iteration (with
              ;; the now-visible results) and asks the model to confirm or
              ;; refine. The proposed text is already captured as the sticky
              ;; best-answer by `answer-fn`.
              answer-proposed? (and (nil? validation-error)
                                 (boolean (some mutation-block? blocks)))
              ;; Surface the validation error on the answer-bearing
              ;; form's row so the model sees \"my done(...) was
              ;; rejected because...\" right next to its own code.
              blocks*     (cond-> blocks
                            (and validation-error form-idx
                              (< form-idx (count blocks))
                              (nil? (get-in blocks [form-idx :error])))
                            (assoc-in [form-idx :error]
                              (op-error validation-error
                                {:code (get form-sources form-idx)
                                 :phase :vis/final-answer-validation})))
              ;; Re-emit a `:phase :form-result` chunk for the
              ;; answer-bearing form when the validator attached an
              ;; error post-hoc. The original `:form-result` chunk
              ;; fired the moment the form returned `:vis/answer`
              ;; with `:error nil`; without this re-emit the TUI
              ;; tracker renders the rejected answer as a succeeded
              ;; form, hiding the validation error from the user.
              _ (when (and validation-error form-idx on-chunk
                        (< form-idx (count blocks*)))
                  (let [b (get blocks* form-idx)]
                    (on-chunk {:phase             :form-result
                               :iteration-count   iteration-position
                               :position          form-idx
                               :count             total-forms
                               :scope             (form-scope form-idx)
                               :code              (:code b)
                               :render-segments   (:render-segments b)
                               :vis/structurally-silent? (boolean (:vis/structurally-silent? b))
                               :result            (:result b)
                               :error             (:error b)
                               :envelope          (:envelope b)
                               :role              (:role b)
                               :silent?     (boolean (or (:vis/silent b)
                                                       (= "vis_silent" (:result b))
                                                       (:vis/structurally-silent? b)))
                               :timeout?          (boolean (:timeout? b))
                               :repaired?         (boolean (:repaired? b))})))
              model-name       (actual-llm-model resolved-model ask-result)
              provider         (actual-llm-provider resolved-model ask-result)]
          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks*)
                       [{:id 0 :code "(final-answer-validation)"
                         :result nil
                         :error (op-error validation-error
                                  {:code "(final-answer-validation)"
                                   :phase :vis/final-answer-validation})}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :silent-form-idxs silent-form-idxs
             :llm-messages messages :llm-provider provider :llm-model model-name
             :llm-selected-provider (:provider resolved-model)
             :llm-selected-model (some-> (:name resolved-model) str)
             :llm-actual-provider provider
             :llm-actual-model model-name
             :llm-routing-trace (:routed/trace ask-result)
             :llm-raw-response (:raw ask-result)
             :llm-executable-blocks (:blocks ask-result)
             :llm-returned-empty-code? (empty? blocks)
             :assistant-message (:assistant-message ask-result)}
            (let [final-answer* (append-runtime-appendices environment final-answer value)]
              (cond->
                {:thinking thinking
                 :blocks blocks
                 ;; nil when the answer is only a PROPOSAL (tools ran in the
                 ;; same fence) — the turn does not finalize; the loop asks the
                 ;; model to confirm/refine next iteration. Otherwise finalize.
                 :final-result (when-not answer-proposed?
                                 {:final?           true
                                  :answer           final-answer*
                                  ;; Index of the form that called
                                  ;; `done(...)`. Channels use this to
                                  ;; ELIDE the answer-bearing form from the
                                  ;; per-iteration code trace (the channel
                                  ;; renders the answer text below; showing
                                  ;; `done("""...""")` above it is
                                  ;; redundant prose-as-code).
                                  :answer-position  form-idx})
                 :api-usage api-usage
                 :duration-ms (or (:duration-ms ask-result) 0)
                 :silent-form-idxs silent-form-idxs
                 :llm-messages messages :llm-provider provider :llm-model model-name
                 :llm-selected-provider (:provider resolved-model)
                 :llm-selected-model (some-> (:name resolved-model) str)
                 :llm-actual-provider provider
                 :llm-actual-model model-name
                 :llm-routing-trace (:routed/trace ask-result)
                 :llm-raw-response (:raw ask-result)
                 :llm-executable-blocks (:blocks ask-result)
                 :llm-returned-empty-code? (empty? blocks)
                 :assistant-message (:assistant-message ask-result)}
                answer-proposed? (assoc :answer-proposed? true)))))
          ;; Normal path
        {:thinking thinking
         :blocks blocks
         :final-result nil :api-usage api-usage
         :duration-ms (or (:duration-ms ask-result) 0)
         :silent-form-idxs silent-form-idxs
         :llm-messages messages
         :llm-provider (actual-llm-provider resolved-model ask-result)
         :llm-model    (actual-llm-model resolved-model ask-result)
         :llm-selected-provider (:provider resolved-model)
         :llm-selected-model (some-> (:name resolved-model) str)
         :llm-actual-provider (actual-llm-provider resolved-model ask-result)
         :llm-actual-model (actual-llm-model resolved-model ask-result)
         :llm-routing-trace (:routed/trace ask-result)
         :llm-raw-response (:raw ask-result)
         :llm-executable-blocks (:blocks ask-result)
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
  "Max transparent retries for a provider auth 401/403 per iteration. One
   retry: on the first auth rejection we force an OAuth refresh-token
   exchange (the stored access token was locally-unexpired but invalidated
   server-side, e.g. refresh-token rotation by another client), rebuild
   the router with the fresh token, and re-send once. A second 401 means
   the refresh itself is bad — surface the provider error."
  1)

(defn- stream-truncated-error?
  "True when an exception represents a provider stream that was cut
   before any content arrived. Safe to retry transparently."
  [^Throwable e]
  (let [data (ex-data e)]
    (and (contains? stream-truncated-types (:type data))
      (zero? (or (:content-acc-len data) 0)))))

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
   reasoning-heavy iterations observed on session 52983a42 (Copilot
   Claude burning the full 2048 auto-budget on hidden reasoning before
   ever opening a code fence) without overshooting the provider's
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
  (let [base   (long (or prev-max 8192))
        bumped (long (Math/ceil (* (double base) MAX_TOKENS_RETRY_BUMP_FACTOR)))]
    (assoc (or prev-extra-body {}) :max_tokens bumped)))

(defn- max-tokens-exhausted?
  "True for `:svar.llm/max-tokens-exceeded` errors that survived all
   per-iteration retries. See svar's `ask-code!*` blank-content guard
   for the underlying detection."
  [iteration-error-data]
  (= :svar.llm/max-tokens-exceeded (:type iteration-error-data)))

(defn- llm-provider-error-context
  [iteration iteration-error-data]
  (let [output-overflow?    (stream-output-overflow? iteration-error-data)
        max-tokens-exhaust? (max-tokens-exhausted? iteration-error-data)
        data                (:data iteration-error-data)
        reasoning-length    (some-> data :reasoning-length long)
        output-tokens       (some-> data :output-tokens long)
        message (cond
                  output-overflow?
                  "Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens)."
                  max-tokens-exhaust?
                  (str "Provider truncated the response at max_tokens ("
                    (or output-tokens "?")
                    " tokens consumed, "
                    (or reasoning-length "?")
                    " went to hidden reasoning, 0 to visible content). "
                    "Vis already retried once with a doubled budget; this iteration"
                    " still hit the cap.")
                  :else
                  (str "LLM call failed: " (:message iteration-error-data)))
        hint (cond
               output-overflow?
               "Do not continue the broad strategy. Use a compact path now: one small probe if essential, otherwise stop, report the exact impediment, and ask for confirmation before more changes. Avoid dumping large maps, file contents, diffs, or repeated diagnostics."
               max-tokens-exhaust?
               "Shorten next iteration. Follow current :session/tasks and heed :session/hints; keep tool procedure canonical and compact. Drop unrelated defs and emit `done(...)` early if previous iteration already has enough evidence. Heavy reasoning models on Copilot/Codex cap output independently of context size."
               :else
               "Adjust your approach or finish with `done(...)` using only observed evidence.")]
    (cond-> {:phase     :llm-provider/generate
             :type      (cond
                          output-overflow?    :llm-provider/output-budget-exhausted
                          max-tokens-exhaust? :llm-provider/max-tokens-exhausted
                          :else               :llm-provider/call-failed)
             :iteration (inc (long iteration))
             :message   message
             :hint      hint}
      max-tokens-exhaust?
      (assoc :reasoning-length reasoning-length
        :output-tokens output-tokens)
      (and (not output-overflow?) (:type iteration-error-data))
      (assoc :source-type (:type iteration-error-data)))))

(defn- iteration-error-feedback
  [iteration iteration-error-data user-request]
  (let [llm-provider-error (llm-provider-error-context iteration iteration-error-data)]
    (str "[Iteration " (:iteration llm-provider-error) "]\n"
      ";; llm-provider-error =\n"
      (pr-str llm-provider-error)
      "\n"
      (when (stream-output-overflow? iteration-error-data)
        (str "Original request: " user-request)))))

(def ^:private CHAT_ERROR_BODY_RENDER_CHARS
  "Cap on raw upstream HTTP body chars surfaced in the chat error
   bubble. Long enough that Anthropic / OpenAI / z.ai full JSON error
   envelopes (`{\"type\":\"error\",\"error\":{...},\"request_id\":...}`)
   round-trip whole — their structured `error.message` is what the
   model and the user actually need to act on. Short enough that a
   pathological provider 5xx HTML page or full streamed partial body
   doesn't take over the chat transcript. svar already caps at
   `MAX_HTTP_ERROR_BODY_CHARS` (8 KiB) on the way in; this is the
   render-side ceiling and stays well under the TUI bubble “collapse
   long body” threshold."
  4000)

(defn- parse-provider-body
  [body]
  (when (and (string? body) (not (str/blank? body)))
    (try
      (json/read-json body :key-fn keyword)
      (catch Throwable _ nil))))

(defn- provider-body-message
  [body]
  (let [parsed (parse-provider-body body)]
    (or (get-in parsed [:error :message])
      (:message parsed)
      (some-> body str/trim not-empty))))

(defn- invalid-thinking-signature-message?
  [message]
  (boolean (and (string? message)
             (re-find #"(?i)invalid.*signature.*thinking.*block" message))))

(defn- auth-provider-error?
  [status message wrapper-message]
  (let [text (str (or message "") "\n" (or wrapper-message ""))]
    (boolean
      (or (contains? #{401 403} status)
        (re-find #"(?i)(authentication|unauthorized|forbidden|credential|api[ -]?key|access[ -]?token|expired token|invalid token)"
          text)))))

(defn- auth-provider-next-step
  [data]
  (let [provider-id (or (:provider-id data) (:provider data) (:provider/id data))]
    (str "NEXT STEP: re-authenticate this provider or update its API key, then retry. "
      "TUI: Ctrl+K -> Model / Providers -> re-authenticate provider. "
      "CLI: run `vis providers auth"
      (when provider-id (str " " provider-id))
      "` for OAuth providers; for API-key providers, fix the configured key/env var and restart Vis.")))

(defn- provider-error-explanation
  [err]
  (let [message          (or (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        provider-message (provider-body-message body-raw)]
    (cond
      (invalid-thinking-signature-message? provider-message)
      (str "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key. "
        "Fix: do not replay preserved-thinking unless provider AND model match; retry with only normal transcript/trailer context.")

      (auth-provider-error? status provider-message message)
      (str "WHAT HAPPENED: provider rejected credentials before the model ran."
        (when (seq provider-message)
          (str " Provider message: " provider-message))
        " "
        (auth-provider-next-step data))

      (seq provider-message)
      (str "WHAT HAPPENED: provider rejected the request before the model ran. Provider message: " provider-message)

      :else
      "WHAT HAPPENED: provider rejected the request before the model ran.")))

(defn- provider-error-ir
  [err]
  (let [message          (or (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        request-id       (or (:request-id data) (:request_id data))
        provider-message (provider-body-message body-raw)
        provider-id      (or (:provider-id data) (:provider data) (:provider/id data))
        provider-body    (when (and body-raw (not (str/blank? body-raw)))
                           (truncate body-raw CHAT_ERROR_BODY_RENDER_CHARS))
        ;; Categorical kind, surfaced to channels via the IR attrs below so
        ;; a chat surface (Telegram) can pick concise copy + a glyph instead
        ;; of re-deriving it from the rendered prose.
        kind             (cond
                           (invalid-thinking-signature-message? provider-message) :invalid-thinking-signature
                           (auth-provider-error? status provider-message message)  :auth
                           :else                                                    :generic)
        ;; Structured echo of the same facts the rich IR renders, so a
        ;; channel can render a compact view without parsing IR back out.
        info             {:kind             kind
                          :status           status
                          :request-id       (some-> request-id str)
                          :provider-message (not-empty provider-message)
                          :wrapper-message  (not-empty message)
                          :provider-id      provider-id
                          :body             provider-body}
        facts            (cond-> [[:li {} [:p {} [:span {} "Wrapper: "] [:c {} message]]]]
                           status (conj [:li {} [:p {} [:span {} "HTTP: "] [:c {} (str status)]]])
                           request-id (conj [:li {} [:p {} [:span {} "Request id: "] [:c {} (str request-id)]]])
                           provider-message (conj [:li {} [:p {} [:span {} "Provider message: "] [:c {} provider-message]]]))
        ir               (render/->ast
                           (cond-> [:ir {}
                                    [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                                    [:p {} [:strong {} [:span {} "Provider call failed before the model could run."]]]
                                    [:p {} [:strong {} [:span {} (provider-error-explanation err)]]]
                                    (into [:ul {}] facts)]
                             provider-body (conj [:p {} [:span {} "Provider response:"]]
                                             [:code {:lang "json"} provider-body])))]
    (assoc ir 1 (assoc (second ir) :vis/provider-error true :vis/provider-error-data info))))

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
    (try
      (let [models (f svar-provider router-opts)]
        (cond-> svar-provider
          (seq models) (assoc :models (vec models))))
      (catch Throwable _ svar-provider))
    svar-provider))

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
    (mapv #(enrich-provider-models (config/->svar-provider %) ropts)
      (:providers config))))

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
    (update router :providers
      (fn [provs]
        (mapv (fn [p]
                (let [want (get primary (:id p))
                      hit  (and want (some #(when (= want (:name %)) %) (:models p)))]
                  (if hit
                    (assoc p
                      :models (into [hit] (remove #(= want (:name %)) (:models p)))
                      :root   want)
                    p)))
          provs)))))

(defn get-router
  "Get or create the shared LLM router.

   Honors `:router` opts from `~/.vis/config.edn` (`:rate-limit`,
   `:network`, `:budget`, ...). Without that block svar's built-in
   defaults apply. See `config/router-opts` for the supported keys."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (-> (svar/make-router (runtime-router-providers cfg)
                    (config/router-opts cfg))
                (honor-config-primary! cfg))]
      (reset! router-atom r)
      r)))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change.

   Forwards `:router` opts so live config edits (e.g. tuning
   `:same-provider-delays-ms`) take effect on the next `set-provider!`
   without restarting the JVM."
  [config]
  (let [r (-> (svar/make-router (runtime-router-providers config)
                (config/router-opts config))
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

(defn- auth-refreshable-error?
  "True when exception `e` is a provider auth rejection (401/403 or an
   auth-shaped message) AND the failing provider exposes a force-refresh
   hook we can use to recover."
  [^Throwable e resolved-model]
  (let [d                (ex-data e)
        status           (:status d)
        provider-message (provider-body-message (some-> (:body d) str))
        pid              (:provider resolved-model)]
    (boolean
      (and (auth-provider-error? status provider-message (ex-message e))
        (some-> (registry/provider-by-id pid) :provider/refresh-token-fn)))))

(defn- try-refresh-provider-token!
  "Force an OAuth refresh for the failing provider, then rebuild + reseat
   routers so the fresh token is live. Returns true when a refresh actually
   happened (caller may retry), false otherwise (caller surfaces the error)."
  [resolved-model]
  (let [pid (:provider resolved-model)
        f   (some-> (registry/provider-by-id pid) :provider/refresh-token-fn)]
    (boolean
      (when f
        (try
          (f)                                   ;; force refresh-token exchange + persist
          (let [r (rebuild-router! (config/resolve-config))]
            (refresh-cached-routers! r))
          (tel/log! {:level :warn :id ::auth-token-refreshed :data {:provider pid}}
            (str "Auth 401 — force-refreshed OAuth token for " pid
              " and rebuilt router; retrying turn"))
          true
          (catch Throwable t
            (tel/log! {:level :error :id ::auth-token-refresh-failed
                       :data {:provider pid :error (ex-message t)}}
              (str "Auth 401 — OAuth token refresh FAILED for " pid
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
  (svar/ask-code! (get-router) (with-default-ask-code-idle-timeout opts)))

(defn llm-text!
  "Fast helper LLM call for extensions.

   Uses svar routing (`:routing {:optimize :cost}`) instead of Vis-side model
   name heuristics. The call still goes through `svar/ask-code!` because Vis no
   longer uses the retired `ask!` structured-output path; `:lang \"text\"`,
   `:reasoning :off`, and `:code-tail-pointer? true` make the return a plain
   text string under :text. Callers may pass either :messages or :system +
   :prompt."
  [{:keys [messages system prompt reasoning temperature routing] :as opts}]
  (let [messages (or messages
                   (cond-> []
                     (seq system) (conj {:role "system" :content system})
                     (seq prompt) (conj {:role "user" :content prompt})))
        resp     (svar/ask-code! (get-router)
                   (with-default-ask-code-idle-timeout
                     (merge (dissoc opts :system :prompt :temperature)
                       {:messages           messages
                        :lang               "text"
                        :reasoning          (or reasoning :off)
                        :routing            (or routing {:optimize :cost})
                        :code-tail-pointer? true}
                       (when (some? temperature)
                         {:temperature temperature}))))
        text     (or (some-> resp :result str/trim not-empty)
                   (some-> resp :raw str/trim not-empty)
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
   (let [provider (first (:providers router))
         model    (first (:models provider))]
     (when model
       (cond-> (if (map? model) model {:name (str model)})
         (:id provider) (assoc :provider (:id provider))))))
  ([router _routing-overrides]
   (resolve-effective-model router)))

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
                (keep #(some-> % str not-empty))
                vec)]
    (if (empty? names)
      router
      (let [m-name   (fn [m] (:name (if (map? m) m {:name (str m)})))
            rank     (zipmap names (range))
            ;; lower = more preferred; unlisted = +inf (keeps relative order, stable sort)
            m-rank   (fn [m] (get rank (m-name m) Long/MAX_VALUE))
            p-rank   (fn [p] (reduce min Long/MAX_VALUE (map m-rank (:models p))))
            reorder  (fn [p] (update p :models #(vec (sort-by m-rank %))))]
        (assoc router :providers
          (->> (:providers router) (map reorder) (sort-by p-rank) vec))))))

(defn- provider-root-model
  "Root model NAME for a provider id in `router`, or nil. Prefers the provider's
   declared `:root`, else its first model."
  [router pid]
  (when-let [p (first (filter #(= (:id %) pid) (:providers router)))]
    (or (some-> (:root p) str not-empty)
      (let [m (first (:models p))]
        (some-> (if (map? m) (:name m) m) str)))))

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
     (let [pid    (keyword displayed-provider)
           stats  (try (svar/router-stats router) (catch Throwable _ nil))
           cb-of  (fn [p] (get-in stats [:providers p :circuit-breaker] :closed))
           open?  (fn [p] (contains? #{:open :half-open} (cb-of p)))]
       (when (open? pid)
         (let [serving (first (remove #(open? (:id %)) (:providers router)))
               sp      (:id serving)]
           {:overloaded-provider pid
            :overloaded-model    (some-> displayed-model str)
            :serving-provider    sp
            :serving-model       (when sp (provider-root-model router sp))}))))))

(defn subctx->seed-ctx
  "Convert the model-supplied `subctx` — a Python dict that arrives KEYWORD-SNAKE
   (`{:tasks {:oauth {:status \"doing\" :title \"x\"}} :facts {…}
   :focus \"oauth\"}`) — into an engine ctx for the child's ctx-atom:
     - `:tasks` → `:session/tasks`, `:facts` → `:session/facts`
     - entity MAP KEYS → STRINGS (`:oauth` → `\"oauth\"`; entity keys/ids are
       strings only now)
     - task `:status` string VALUES → status keywords (`\"doing\"` → `:doing`) via
       `normalize-plan-status`, so the child's render + rollup read them right.
   Other fields pass through untouched. PURE. The model OWNS the slice (focus +
   bigger picture); this only re-shapes the boundary types."
  [subctx]
  (let [strk (fn [k] (if (keyword? k) (name k) (str k)))
        tasks (fn [m] (when (map? m)
                        (into {} (map (fn [[k t]]
                                        [(strk k)
                                         (cond-> t
                                           (and (map? t) (contains? t :status))
                                           (update :status ctx-engine/normalize-plan-status))]))
                          m)))
        facts (fn [m] (when (map? m)
                        (into {} (map (fn [[k f]] [(strk k) f])) m)))]
    (cond-> {}
      (:tasks subctx) (assoc :session/tasks (tasks (:tasks subctx)))
      (:facts subctx) (assoc :session/facts (facts (:facts subctx))))))

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
;; `extension-info` declare in extension.clj). See AGENTS.md S2.
(declare sync-active-extension-symbols!)

(def ^:private FRESH_ITER_CARRY
  ;; `:trailer-iters` is a vec of `[iteration-position {:thinking :blocks}]`
  ;; pairs (oldest-first). The prompt renderer trims the rendered trailer
  ;; by token budget (50% of model context), not fixed iteration count.
  {:trailer-iters []})

(def ^:private balanced-reasoning :balanced)

(do
  (defn- status->id [status]
    (when status (keyword "rlm.status" (name status))))

  (def ^:private cost-map-keys
    [:input-cost
     :input-uncached-cost
     :input-cached-cost
     :input-cache-write-cost
     :cache-read-cost
     :cache-write-cost
     :output-cost
     :total-cost])

  (defn- estimate-token-cost
    "Estimate cost from provider usage while preserving cached/non-cached input split."
    ([model input-tokens output-tokens]
     (estimate-token-cost model input-tokens output-tokens {}))
    ([model input-tokens output-tokens opts]
     (try
       (svar-router/estimate-cost model input-tokens output-tokens
         svar-router/MODEL_PRICING
         (or opts {}))
       (catch Throwable _ nil))))

  (defn- merge-cost-maps
    [acc extra-cost]
    (merge-with +
      (select-keys acc cost-map-keys)
      (select-keys extra-cost cost-map-keys))))

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

   Two signals, no iteration/budget counting:
     - a `done(…)` that reached this point did NOT finalize the turn
       (gated / discarded / retracted); two in a row ⇒ stuck.
     - identical non-`done()` action code repeated across iterations (the model
       reran the same search / rebuilt the same parser) ⇒ stuck."
  [blocks prev-stuck]
  (let [had-done?   (boolean (some #(re-find #"\(done\b" (str (:code %))) blocks))
        action-code (mapv :code (remove #(re-find #"\(done\b" (str (:code %))) blocks))
        action-sig  (when (seq action-code) (hash action-code))
        done-streak (if had-done? (inc (long (or (:done-streak prev-stuck) 0))) 0)
        sig-repeat? (boolean (and action-sig (= action-sig (:last-sig prev-stuck))))]
    {:stuck?      (or (>= done-streak 2) sig-repeat?)
     :done-streak done-streak
     :action-sig  action-sig}))

(defn- loop-checkpoint-message
  "The repetition decision-checkpoint, injected as a user turn the moment the
   model loops (a `done(…)` that didn't finalize, repeated; or identical
   non-`done()` action code repeated). Confronts the one-shot urge: shows the
   best answer so far and forces a commit / justified-continue / blocked
   decision instead of yet another open-ended probe. `sticky-md` is the best
   answer so far (Markdown) or nil."
  [sticky-md]
  (str "⚠️ STOP — you are repeating yourself. You wanted to one-shot this, but "
    "you have now looped without finalizing.\n\n"
    (if (str/blank? (str sticky-md))
      "You have NOT produced any answer yet.\n\n"
      (str "Your best answer so far:\n\n---\n" sticky-md "\n---\n\n"))
    "DECIDE NOW — run NO tools/searches this iteration:\n"
    "1. COMMIT — if the answer above is good enough, call `done(\"\"\"…\"\"\")` with it "
    "(refine the wording if you must).\n"
    "2. CONTINUE — name the ONE specific missing fact AND why it is worth "
    "another iteration, then fetch ONLY that. Repeating a prior search/parse "
    "is not allowed.\n"
    "3. BLOCKED — call `done(\"\"\"…\"\"\")` stating exactly what blocks you.\n"
    "Pick one. Do not investigate further."))

(defn- proposal-confirm-message
  "Injected when the model called `done(…)` in the SAME fence as tool/extension
   calls — so that answer was a proposal decided before the results (now visible
   in the trace above) came back. Asks it to confirm or refine, rather than the
   old hard rejection. `proposed-md` is the proposed answer (Markdown) or nil."
  [proposed-md]
  (str "You called `done(…)` in the same step as a MUTATION (a file change / "
    "patch) — so that answer was a PROPOSAL, decided before the mutation's "
    "outcome was observed. The result is now in the trace above.\n\n"
    (if (str/blank? (str proposed-md))
      ""
      (str "Your proposed answer:\n\n---\n" proposed-md "\n---\n\n"))
    "Now that you can SEE the actual results:\n"
    "- If the answer still holds, re-call `done(\"\"\"…\"\"\")` on its OWN (no other "
    "tool calls this iteration) to finalize — refine the wording with the "
    "observed evidence if useful.\n"
    "- If the results change your conclusion, fix it, then `done(\"\"\"…\"\"\")`.\n"
    "Do not re-run the same tools just to double-check."))

(defn- rejection-fact-entries
  "Collect the durable rejection / auto-repair events of one iteration as
   [fact-key fact-partial] pairs for `ctx-loop/apply-and-record! :fact-set!`.
   Captures the failures the channel SUPPRESSES (preflight gates) or silently
   FIXES (glued-forms auto-repair) plus raw syntax rejections, so each leaves a
   searchable `turn_<T>_i<I>_*` fact instead of vanishing when the trailer folds."
  [turn-position iteration-position code-entries form-results]
  (let [clip (fn [s] (let [s (str s)]
                       (if (> (count s) 240) (str (subs s 0 240) " ...") s)))
        mk   (fn [tag k] (str "turn_" turn-position "_i" iteration-position "_" tag "_" k))
        preflight (keep-indexed
                    (fn [k entry]
                      (when-let [pe (:vis/preflight-error entry)]
                        [(mk "preflight" k)
                         {:status :active
                          :content (str "## Preflight rejection (t" turn-position "/i" iteration-position ")\n\n"
                                     "**kind:** " (clip (pr-str pe)) "\n\n"
                                     "An engine GATE rejected this block before eval - model-facing only "
                                     "(no user error box). Offending source:\n\n```\n"
                                     (clip (:expr entry)) "\n```")}]))
                    code-entries)
        repairs (keep-indexed
                  (fn [k result]
                    (let [ar (:auto-repaired result)]
                      (when (= :glued-forms (:kind ar))
                        [(mk "repair" k)
                         {:status :active
                          :content (str "## Glued-forms auto-repair (t" turn-position "/i" iteration-position ")\n\n"
                                     "Top-level forms were smashed onto one line (the OpenAI/Codex "
                                     "missing-newline failure); the engine AUTO-REPAIRED by inserting "
                                     "newlines at each boundary and re-ran. Original:\n\n```\n"
                                     (clip (:original ar)) "\n```\n\nRepaired:\n\n```\n"
                                     (clip (:repaired ar)) "\n```")}])))
                  form-results)
        syntax (keep-indexed
                 (fn [k result]
                   (let [err  (:error result)
                         data (:data err)]
                     (when (and err (or (:glued-forms? data)
                                      (= :python/syntax (:phase data))))
                       [(mk "syntax" k)
                        {:status :active
                         :content (str "## Syntax rejection (t" turn-position "/i" iteration-position ")\n\n"
                                    "**phase:** " (pr-str (:phase data)) "\n\n"
                                    (or (:message err) "(no message)") "\n\nOffending source:\n\n```\n"
                                    (clip (or (:code data)
                                            (:expr (nth code-entries k nil)))) "\n```")}])))
                 form-results)]
    (vec (concat preflight repairs syntax))))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt
           session-turn-id
           ;; `max-context-tokens` feeds advisory context-pressure hooks;
           ;; trailer assembly itself still owns no token trimming.
           max-context-tokens
           hooks cancel-atom cancel-token
           reasoning-default routing extra-body turn-features allow-copilot-claude-deep?
           workspace-overrides]}]
  (let [environment (cond-> environment
                      (seq turn-features) (assoc :turn/features turn-features)
                      (seq workspace-overrides) (merge workspace-overrides)
                      ;; Surface the cancellation token on the environment
                      ;; so `run-python-code` can call
                      ;; `cancellation/on-cancel!` to register a hard
                      ;; `.cancel(true)` on the Python worker future.
                      ;; Without this the UI cancel flag (already flipped
                      ;; by `vis/cancel!`) only reaches the outer turn
                      ;; future; the inner Python worker keeps spinning,
                      ;; pins a thread and starves the input loop until
                      ;; the eval timeout fires — the exact "TUI
                      ;; unresponsive" symptom the user hit in session
                      ;; 11d4f817.
                      cancel-token (assoc :cancel-token cancel-token)
                      cancel-atom  (assoc :cancel-atom  cancel-atom)
                      ;; Per-turn context surfaced to engine hooks and
                      ;; render-time diagnostics.
                      true (assoc :turn/user-request user-request
                             :turn/system-prompt system-prompt))
        resolved-model (resolve-effective-model (:router environment))
        effective-model (:name resolved-model)
        _ (assert effective-model "Router must resolve a root model")
        ;; Clear any sticky best-answer from a PRIOR turn (the atom lives on
        ;; the per-session env) so this turn's cancel-fallback only ever
        ;; surfaces an answer THIS turn actually produced.
        _ (some-> (:best-answer-atom environment) (reset! nil))
        has-reasoning? (reasoning-effort-configurable? resolved-model)
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per session turn. Threaded through both
        ;; the prompt message assembler (core, environment, extension messages)
        ;; and the per-iteration ext hint collector - activation-fn never
        ;; re-fires inside the loop.
        active-exts   (prompt/active-extensions environment)
        _extensions-snapshot (prompt/extensions-snapshot active-exts)
        _             (sync-active-extension-symbols! environment active-exts)
        session-snapshot (fn []
                           {:id           (:session-id environment)
                            :title        (some-> (:session-title-atom environment) deref str str/trim not-empty)
                            :turn-id      session-turn-id
                            :user-request user-request})
        _session-base (session-snapshot)
        turn-position (session-turn-position environment session-turn-id)
        previous-usage (previous-request-usage environment session-turn-id)
        stable-prompt-messages (prompt/assemble-stable-prompt-messages environment
                                 {:system-prompt     system-prompt
                                  :active-extensions active-exts})
        initial-messages (prompt/assemble-initial-messages
                           {:stable-prompt-messages stable-prompt-messages
                            :initial-user-content   user-request
                            :previous-turn-context  (previous-turn-context environment session-turn-id)})
        ;; The cumulative `:input-tokens` field sums `prompt_tokens`
        ;; from every iteration in this turn — useful for billing /
        ;; budget accounting but MUST NOT be passed to the
        ;; context-pressure hint, which compares against the model's
        ;; per-call context window. Session 3102ad16 (2026-05-20)
        ;; surfaced the bug: after 13 iterations the cumulative input
        ;; crossed the 50% threshold even though each individual
        ;; request stayed at ~10K tokens. The model received fake
        ;; \"Context pressure: ~115K / 200K (58%)\" warnings and started
        ;; defensively flipping the context-pressure hook-task to
        ;; :done (pre-D12 it was a defensive
        ;; `(satisfy-hint! :vis.foundation/context-pressure)`) while
        ;; still operating on a tiny window.
        ;;
        ;; `:last-iter-input` carries the most recent SINGLE-CALL
        ;; `prompt_tokens`, which is the right proxy for \"what the next
        ;; request will look like\". Reasoning tokens from a preserved-
        ;; thinking-enabled provider already flow into the next iter's
        ;; `prompt_tokens` server-side, so a single last-iter snapshot
        ;; already captures that growth without us re-computing it.
        ;;
        ;; Iter 1 of a new user turn has no live provider usage yet. Keep
        ;; billing fields zeroed, but seed the utilization/hint proxy from
        ;; latest persisted request in the session so the model still sees
        ;; `:session/utilization` immediately.
        usage-atom (atom {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0 :cached-tokens 0
                          :cache-creation-tokens 0
                          :last-iter-input 0 :last-iter-reasoning 0
                          :previous-request-input (long (or (:last-request-tokens previous-usage) 0))
                          :iter-count 0})
        accumulate-usage! (fn [api-usage]
                            (when api-usage
                              (swap! usage-atom
                                (fn [acc]
                                  (let [iter-in     (long (or (:prompt_tokens api-usage) 0))
                                        iter-reason (long (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))]
                                    (-> acc
                                      (update :input-tokens + iter-in)
                                      (update :output-tokens + (or (:completion_tokens api-usage) 0))
                                      (update :reasoning-tokens + iter-reason)
                                      (update :cached-tokens + (or (get-in api-usage [:prompt_tokens_details :cached_tokens])
                                                                 (get-in api-usage [:prompt_tokens_details :input_cached_tokens])
                                                                 0))
                                      (update :cache-creation-tokens + (or (get-in api-usage [:prompt_tokens_details :cache_creation_tokens])
                                                                         (get-in api-usage [:prompt_tokens_details :cache_write_tokens])
                                                                         0))
                                      ;; Per-iter snapshots: overwrite, not accumulate.
                                      (assoc :last-iter-input iter-in)
                                      (assoc :last-iter-reasoning iter-reason)
                                      (update :iter-count inc)))))))
        ;; Per-iteration token + cost projection. The schema's
        ;; `iteration.llm_*_tokens` / `iteration.llm_cost_usd` columns
        ;; carry one row per iteration so a future `vis report`
        ;; caller can sum or break down cost without re-walking
        ;; provider envelopes. Returns nil when the call surfaced no
        ;; usage (e.g. iteration-level error before a response
        ;; landed), in which case the persistance layer leaves the
        ;; columns NULL.
        iteration-token-cost (fn [api-usage]
                               (when api-usage
                                 (let [in   (long (or (:prompt_tokens api-usage) 0))
                                       out  (long (or (:completion_tokens api-usage) 0))
                                       reas (long (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                       cach (long (or (get-in api-usage [:prompt_tokens_details :cached_tokens])
                                                    (get-in api-usage [:prompt_tokens_details :input_cached_tokens])
                                                    0))
                                       cache-created (long (or (get-in api-usage [:prompt_tokens_details :cache_creation_tokens])
                                                             (get-in api-usage [:prompt_tokens_details :cache_write_tokens])
                                                             0))
                                       ;; svar's `estimate-cost` returns a MAP
                                       ;; `{:input-cost :output-cost :total-cost
                                       ;; :model :pricing}`, NOT a bare number.
                                       ;; Pull `:total-cost` out; nil pricing
                                       ;; (e.g. unknown model) leaves the
                                       ;; column NULL on disk, which the read
                                       ;; side defaults to 0.0.
                                       cost-map (estimate-token-cost effective-model in out {:api-usage api-usage})
                                       total    (when (map? cost-map) (:total-cost cost-map))]
                                   {:tokens   {:input in :output out :reasoning reas :cached cach
                                               :cache-created cache-created}
                                    :cost-usd (when (number? total) (double total))})))
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens
                                      cached-tokens cache-creation-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (estimate-token-cost effective-model input-tokens output-tokens
                                     {:cached-tokens cached-tokens
                                      :cache-creation-tokens cache-creation-tokens})]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :cache-created cache-creation-tokens
                                    :total total-tokens}
                           :cost cost}))
        ;; `:on-chunk` is a per-reasoning-chunk streaming hook fired
        ;; from svar's stream callback. It fires dozens of times per
        ;; iteration, not at lifecycle boundaries. Lifecycle callbacks
        ;; now use namespaced `:ext/hooks` phases; on-chunk stays the
        ;; high-frequency streaming-only surface.
        on-chunk             (:on-chunk hooks)
        emit-hook! (fn [hook-fn payload log-message]
                     ;; Single-fn caller-hook helper, used by
                     ;; on-chunk only.
                     (when hook-fn
                       (try (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data (format-exception-short e)} log-message)))))
        iteration-cache-created-tokens
        (fn [token-cost]
          (let [cache-created (long (or (get-in token-cost [:tokens :cache-created]) 0))]
            (when (pos? cache-created) cache-created)))]
    ;; -----------------------------------------------------------------
    ;; Turn-start state.
    ;;
    ;; The `ctx` symbol (vctx/build) is not a sandbox binding. Engine state is
    ;; rendered into every user message as bare-EDN under `;; ctx`,
    ;; not bound as an engine value. See ctx_loop/build-engine-bindings for
    ;; the model-facing mutator + introspect surface.
    ;; Seed turn-scoped fields on the single turn-state-atom in one swap.
    (ctx-loop/set-turn-state! environment
      :iteration-id    nil
      :session-turn-id session-turn-id
      :user-request    user-request
      :turn-position   (or turn-position 1)
      :iteration       nil
      :form-idx        nil
      ;; FORCING plan-gate: distinct files mutated THIS turn (reset each turn).
      ;; The 2nd distinct file without an approved plan arms the gate.
      :files-mutated   #{})
    ;; Phase G fix: sync engine ctx `:session/turn` to the persisted
    ;; turn-position via `eng/enter-turn`. Without this call the engine
    ;; ctx `:session/turn` stayed at the `empty-ctx` default of 1
    ;; forever (`eng/advance-turn` was never wired in), so
    ;; the title-gate (`(= 1 :session/turn)`) fired at every turn after
    ;; the first, every iter would-be-done refused, model retry-loops
    ;; until it gives up or the user cancels. Concrete forensics:
    ;; session c4eb7bab t2 i20-i25 — 7 wasted iterations / ~265s /
    ;; ~17K output tokens / ~1.7M input-token rotation, all because
    ;; ctx `:session/turn` was stuck at 1.
    ;;
    ;; `enter-turn` is idempotent: setting :session/turn to its current
    ;; value (turn 1 first call) is a no-op. It also clears
    ;; `:engine/blockers` + `:engine/turn-events` so the new turn starts
    ;; clean.
    (when-let [ctx-atom (:ctx-atom environment)]
      (swap! ctx-atom
        (fn [c] (ctx-engine/enter-turn c (or turn-position 1)))))
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
    ;; new user turn. In session a9389e1d, Z.ai/GLM received prior-turn
    ;; assistant replay and opened the next request with "answer already
    ;; accepted", then burned >100k input tokens. Durable cross-turn memory
    ;; must flow through persisted iterations, not hidden reasoning state.
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
                    ;; poisoned follow-up turns in session 2ccde943: 7 handle
                    ;; mistakes from turn 1 were replayed verbatim into
                    ;; turn 3's trailer, teaching the model that probing
                    ;; is unreliable. Carry only the iterations that landed
                    ;; a clean result; defs from earlier exploration
                    ;; survive independently via the def restore path.
                    iters (->> queries
                            (remove #(= (str (:id %)) current-turn-id-str))
                            (mapcat (fn [q]
                                      (try (persistance/db-list-session-turn-iterations d (:id q))
                                        (catch Throwable _ []))))
                            (filter #(= :done (:status %)))
                            (sort-by :created-at)
                            vec)]
                (mapv (fn [it]
                        [(or (:position it) 1)
                         {:thinking (:thinking it)
                          :blocks   [(cond-> {:position 0
                                              :code (or (:code it) "")}
                                       (contains? it :result) (assoc :result (:result it))
                                       (contains? it :error) (assoc :error (:error it)))]
                          :llm-provider (:provider it)
                          :llm-model    (some-> (:model it) str)
                          ;; Persisted assistant messages are intentionally NOT
                          ;; replayed across user turns. Keep row diagnostics,
                          ;; but `compatible-preserved-thinking-trailer-iters`
                          ;; rejects this entry before replay.
                          :assistant-message (:llm-assistant-message it)
                          :preserved-thinking/replay? false}])
                  iters)))
            (catch Throwable t
              (tel/log! {:level :warn :id ::cross-turn-trailer-seed-failed
                         :data  {:error (ex-message t)}
                         :msg   "Cross-turn carry seed failed; first iteration starts with an empty tape"})
              nil))]
      (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                  :trace []}
                            FRESH_ITER_CARRY
                            (when (seq seeded-trailer-iters)
                              {:trailer-iters seeded-trailer-iters}))]
          (let [{:keys [iteration messages trace trailer-iters llm-provider]} loop-state]
            (ctx-loop/set-turn-state! environment :iteration (inc (long iteration)))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error iteration {:reason :cancelled})
                ;; Sticky best-answer: surface the latest non-blank `done(…)`
                ;; candidate this turn produced instead of a blank answer.
                (let [sticky (some-> (:best-answer-atom environment) deref :value)
                      result (merge {:answer sticky :status :cancelled :status-id (status->id :cancelled)
                                     :trace trace :iteration-count iteration} (finalize-cost))]
                  result))

              :else
              (let [raw-reasoning-level (when has-reasoning?
                                          base-reasoning-level)
                    reasoning-level (copilot-claude-safe-reasoning-level
                                      resolved-model user-request raw-reasoning-level
                                      {:allow-copilot-claude-deep? allow-copilot-claude-deep?})
                    _ (log-stage! :iteration/start iteration {:message-count (count messages)
                                                              :reasoning reasoning-level
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
                    _llm-provider-context (cond-> {:selected (llm-id (:provider pre-resolved-model)
                                                               (some-> (:name pre-resolved-model) str))
                                                   :routing  (cond-> {:fallback? false}
                                                               (seq routing) (assoc :request routing))}
                                            (:error llm-provider)
                                            (assoc :error (:error llm-provider)))
                    iteration-position (inc (long iteration))
                    current-session (session-snapshot)
                    iteration-hints (collect-iteration-start-hints environment active-exts
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
                    _util-stamp
                    (when-let [ca (:ctx-atom environment)]
                      (let [u   @usage-atom
                            req (if (pos? (long (:iter-count u)))
                                  (long (:last-iter-input u))
                                  (long (:previous-request-input u)))]
                        (stamp-utilization! ca
                          (ctx-engine/utilization req effective-context-limit
                            (:input-tokens u)
                            safe-guards/DEFAULT_PROMPT_BUDGET_TOKENS))))
                    ;; D12: foundation hooks emit hook-task shapes
                    ;; `{:id <kw> :task <task-map>}`. Route each through
                    ;; `apply-mutator :task-set!` so the standard write
                    ;; path (cycle-check, hook-repeat dedup, :born
                    ;; stamping) applies. Hooks already satisfied this
                    ;; turn keep :done via the hook-repeat dedup.
                    fold-start-ms (System/currentTimeMillis)
                    folded-hits (when (:ctx-atom environment)
                                  (filterv (fn [{:keys [id task]}] (and id task))
                                    iteration-hints))
                    _ (doseq [{:keys [id task]} (or folded-hits [])]
                        (ctx-loop/apply-and-record! environment :task-set! [id task]))
                    ;; Hook :emit payload: route secondary `:tasks /
                    ;; :facts` writes from every hit through
                    ;; `apply-and-record!` so a
                    ;; hook can seed CTX in the same way a slash's
                    ;; `:slash/tasks` / `:slash/facts` envelope does.
                    ;; Hooks that returned ONLY an :emit map (no
                    ;; hook-task body) still flow through this path —
                    ;; they were filtered out of `folded-hits` above
                    ;; but kept in `iteration-hints`.
                    _ (when (:ctx-atom environment)
                        (doseq [{emit :emit} iteration-hints
                                :when (map? emit)]
                          (doseq [[k partial] (:tasks emit)]
                            (ctx-loop/apply-and-record! environment :task-set! [k partial]))
                          (doseq [[k partial] (:facts emit)]
                            (ctx-loop/apply-and-record! environment :fact-set! [k partial]))))
                    fold-duration-ms (- (System/currentTimeMillis) fold-start-ms)
                    _ (when (seq folded-hits)
                        (tel/log! {:level :info :id ::hook-task-fold
                                   :data {:iteration iteration
                                          :emitted-ids (mapv :id folded-hits)
                                          :duration-ms fold-duration-ms}}
                          "Folded foundation hook-tasks into :session/tasks"))
                    iteration-context ""
                    ;; CTX engine render + budget guard.
                    ;;
                    ;; `render-block!` builds the bare-EDN `;; ctx`
                    ;; block the model reads/writes against. Before
                    ;; handing it to the provider we wrap the call in
                    ;; `safe-guards/ensure-prompt-under-budget!`: it
                    ;; renders once, measures total prompt tokens
                    ;; (jtokkit cl100k_base — approximation, ~10-30%
                    ;; margin), and if the prompt would cross the
                    ;; configured budget (default 144k / 72% of a
                    ;; 200k-token window) it auto-summarises the oldest
                    ;; trailer pins into a summary stub (companion-LLM
                    ;; semantic summary OR engine dummy if companion
                    ;; unconfigured / timed out), then re-renders.
                    ;;
                    ;; Up to 3 round-trips; each round-trip is one
                    ;; sync companion call (15 s hard wall) plus a
                    ;; re-render. The mutation lands on `:ctx-atom`
                    ;; so subsequent iters see the compacted trailer
                    ;; too. Engine never throws — if the summarization
                    ;; cascade exhausts itself the prompt ships
                    ;; over-budget and the provider's own error is
                    ;; the next signal.
                    ;; Frozen form-result history (prefix-cache): pins
                    ;; render as permanent <results> messages; the
                    ;; regenerated tail below carries ONLY the mutable
                    ;; ctx. The fn re-derives from ctx-atom so each
                    ;; budget-guard fold round measures the post-fold
                    ;; pin set.
                    frozen-msgs-fn (fn [env*]
                                     (frozen-trailer-messages env* trailer-iters
                                       (replay-context pre-resolved-model)
                                       turn-position))
                    summary-render (safe-guards/ensure-prompt-under-budget!
                                     environment
                                     {:render-fn     (fn [env]
                                                       (ctx-loop/render-block!
                                                         env ctx-renderer/render-ctx-mutable))
                                      :stable-msgs   initial-messages
                                      :extra-msgs-fn (fn [env*] (:pins (frozen-msgs-fn env*)))
                                      :main-provider (:provider pre-resolved-model)
                                      :born-scope    (str "t" turn-position
                                                       "/i" (inc iteration) "/f0")})
                    ctx-rendered   (:rendered summary-render)
                    _ (when (seq (:rounds summary-render))
                        (log-stage! :ctx/auto-summarized iteration
                          {:rounds       (count (:rounds summary-render))
                           :final-tokens (:total-tokens summary-render)
                           :over-budget? (:over-budget? summary-render)
                           :batches      (mapv (fn [r] (select-keys r [:source :batch-size
                                                                       :tokens-freed
                                                                       :scope-range]))
                                           (:rounds summary-render))}))
                    iteration-context (->> [iteration-context ctx-rendered]
                                        (remove str/blank?)
                                        (str/join "\n\n"))
                      ;; Single canonical preserved-thinking replay path —
                      ;; svar's per-provider wire serializer turns the
                      ;; canonical assistant messages into native
                      ;; Anthropic / z.ai / Responses shapes.
                      ;;
                      ;; R3 hybrid message shape (per ADR/session
                      ;; 1db62d10): preserved-thinking replays + the
                      ;; iteration-context trailer both APPEND to
                      ;; the end. The original user_initial stays as the
                      ;; ONE user-role anchor near the start (placed there
                      ;; by `assemble-initial-messages`); we never repeat
                      ;; it. Final wire shape:
                      ;;
                      ;;   [system, user_initial,
                      ;;    asst_iter1, user_trailer_after_iter1,
                      ;;    asst_iter2, user_trailer_after_iter2,
                      ;;    ...
                      ;;    asst_iter(n-1), user_trailer_after_iter(n-1)]
                      ;;
                      ;; This matches z.ai's canonical preserved-thinking
                      ;; example (user → asst → user → asst → user) and
                      ;; stops GLM-5.1 from re-reading the same initial
                      ;; goal every iter and restarting its plan.
                    ;; Wire shape (append-only; see frozen-trailer-messages):
                    ;;   [system, user_initial,
                    ;;    <pre-turn pins>, asst_1, <results 1>, asst_2, <results 2>, …,
                    ;;    <mutable context tail>]
                    ;; Recomputed AFTER the budget guard so a fold round's
                    ;; rewritten pins are what actually ship.
                    provider-messages (into (vec messages)
                                        (:suffix (frozen-msgs-fn environment)))
                    effective-messages (cond-> provider-messages
                                         (not (str/blank? (or iteration-context "")))
                                         (conj {:role "user" :content iteration-context}))
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
                           current-extra-body extra-body
                           ;; `env` is threaded so the auth-refresh retry can
                           ;; reseat its `:router` to the rebuilt one (the
                           ;; in-flight env captured the pre-refresh router).
                           env environment]
                      (let [result (try
                                     (run-iteration env effective-messages
                                       {:iteration iteration :reasoning-level reasoning-level
                                        :routing effective-routing
                                        :resolved-model resolved-model
                                        :on-chunk on-chunk
                                        :active-extensions active-exts
                                        :answer-validation-context
                                        {:user-request user-request
                                         :previous-iterations trailer-iters
                                         :previous-blocks (vec (mapcat (comp :blocks second) trailer-iters))}
                                        :extra-body current-extra-body})
                                     (catch Exception e
                                       (cond
                                         (and (stream-truncated-error? e)
                                           (< attempt MAX_STREAM_TRUNCATED_RETRIES))
                                         (do
                                           (tel/log! {:level :warn
                                                      :id ::stream-truncated-retry
                                                      :data {:iteration iteration
                                                             :attempt (inc attempt)
                                                             :max-retries MAX_STREAM_TRUNCATED_RETRIES
                                                             :type (:type (ex-data e))}}
                                             (str "Stream truncated, transparent retry "
                                               (inc attempt) "/" MAX_STREAM_TRUNCATED_RETRIES))
                                           ::retry-stream)

                                         ;; Max-tokens cap: model burnt the entire output
                                         ;; budget on hidden reasoning before opening a code
                                         ;; fence. Double the budget and try once more so the
                                         ;; turn doesn't fail when the same call would have
                                         ;; succeeded with a slightly larger ceiling. Reasoning-
                                         ;; heavy iterations on Copilot Claude (session
                                         ;; 52983a42) hit this when the provider's
                                         ;; finish_reason: \"length\" left content-acc empty.
                                         (and (max-tokens-exceeded-error? e)
                                           (< max-tokens-attempt MAX_MAX_TOKENS_EXCEEDED_RETRIES))
                                         (let [data    (ex-data e)
                                               prev-max (or (:output-tokens data)
                                                          (:max_tokens current-extra-body)
                                                          8192)
                                               bumped   (bumped-max-tokens-extra-body
                                                          current-extra-body prev-max)]
                                           (tel/log! {:level :warn
                                                      :id ::max-tokens-exceeded-retry
                                                      :data {:iteration iteration
                                                             :attempt (inc max-tokens-attempt)
                                                             :max-retries MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                                             :prev-max prev-max
                                                             :new-max (:max_tokens bumped)
                                                             :reasoning-length (:reasoning-length data)}}
                                             (str "max_tokens exhausted on reasoning (~"
                                               (or (:reasoning-length data) "?")
                                               " reasoning tokens); retry "
                                               (inc max-tokens-attempt) "/"
                                               MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                               " with max_tokens=" (:max_tokens bumped)))
                                           ;; Bump max-tokens-attempt so a second cap-hit
                                           ;; doesn't loop forever; keep `attempt` flat so a
                                           ;; subsequent stream-truncated still has its own
                                           ;; quota.
                                           {::retry-max-tokens bumped})

                                         ;; Auth 401/403 from a refreshable
                                         ;; provider: force an OAuth refresh +
                                         ;; router rebuild, then re-send once.
                                         ;; `try-refresh-provider-token!` does
                                         ;; the work and returns true only when
                                         ;; a refresh actually happened.
                                         (and (< attempt MAX_AUTH_REFRESH_RETRIES)
                                           (auth-refreshable-error? e resolved-model)
                                           (try-refresh-provider-token! resolved-model))
                                         ::retry-auth-refresh

                                         :else
                                         (handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level}))))]
                        (cond
                          (= result ::retry-stream)
                          (recur (inc attempt) max-tokens-attempt current-extra-body env)
                          (and (map? result) (contains? result ::retry-max-tokens))
                          (recur attempt (inc max-tokens-attempt) (::retry-max-tokens result) env)
                          (= result ::retry-auth-refresh)
                          ;; Token was force-refreshed and the router rebuilt;
                          ;; reseat THIS turn's env onto the fresh router before
                          ;; re-sending (run-iteration uses (:router env)).
                          (recur (inc attempt) max-tokens-attempt current-extra-body
                            (assoc env :router (get-router)))
                          :else result)))]
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
                      (let [sticky (some-> (:best-answer-atom environment) deref :value)
                            result (merge {:answer sticky :status :cancelled
                                           :status-id (status->id :cancelled)
                                           :trace trace :iteration-count iteration}
                                     (finalize-cost))]
                        result))
                    (let [llm-provider-error (llm-provider-error-context iteration iteration-error-data)
                          ;; Consecutive provider-generate failure streak.
                          ;; Counts only :llm-provider/generate errors (the
                          ;; model never produced usable content); any other
                          ;; error kind resets - those are RLM-correctable.
                          provider-error-streak (next-provider-error-streak
                                                  (:provider-error-streak loop-state) llm-provider-error)
                          provider-breaker-tripped? (provider-error-breaker-tripped? provider-error-streak)
                          error-feedback (iteration-error-feedback iteration iteration-error-data user-request)
                          trace-entry {:iteration iteration :error iteration-error-data :final? false}
                          ;; Preserve forensic evidence on every error
                          ;; path, not just `:empty-content`. Pre-fix
                          ;; only empty-content carried `:reasoning`
                          ;; into the DB row; `:max-tokens-exceeded`
                          ;; (svar.llm) and any other generate-time
                          ;; failure had their reasoning silently
                          ;; dropped. Session 52983a42 iter 14 surfaced
                          ;; the bug — model emitted ~2.2K reasoning
                          ;; tokens before the cap-truncation, but the
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
                          err-data            (:data iteration-error-data)
                          err-reasoning       (:reasoning err-data)
                          err-partial-content (or (:content err-data)
                                                (:partial-content err-data))
                          err-api-usage       (or (:api-usage iteration-result)
                                                (:api-usage err-data))
                          err-iteration-id (persistance/db-store-iteration! (:db-info environment)
                                             (let [tc (iteration-token-cost err-api-usage)]
                                               (cond-> {:session-turn-id session-turn-id :vars [] :code (or err-partial-content "")
                                                        :thinking err-reasoning :duration-ms 0 :llm-full-duration-ms 0 :error iteration-error-data
                                                        :llm-messages effective-messages
                                                        :llm-provider (:provider resolved-model)
                                                        :llm-model (str (:name resolved-model))
                                                        :llm-routing (cond-> {:selected (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                              :actual   (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                              :fallback? false}
                                                                       (seq (get-in iteration-error-data [:data :routed/trace]))
                                                                       (assoc :fallback? true
                                                                         :trace (vec (get-in iteration-error-data [:data :routed/trace]))))
                                                        :cache-created-tokens (iteration-cache-created-tokens tc)}
                                                 tc (assoc :tokens (:tokens tc)
                                                      :cost-usd (:cost-usd tc)))))]
                      (ctx-loop/set-turn-state! environment :iteration-id err-iteration-id)
                      ;; Live error chunk - `:phase :iteration-error`
                      ;; signals the iteration aborted before any
                      ;; forms could run. No per-form chunks fired
                      ;; this iteration, so the channel sees a clean
                      ;; reasoning -> error transition.
                      (emit-hook! on-chunk
                        {:phase     :iteration-error
                         :iteration (inc (long iteration))
                         :thinking  err-reasoning
                         :error     iteration-error-data
                         :done?     true}
                        "on-chunk (iteration error)")
                      (if (or (::fatal-iteration-error iteration-result)
                            ;; Circuit breaker: N consecutive provider-generate
                            ;; failures - feeding the same request back cannot
                            ;; help; fail the turn as a provider error.
                            provider-breaker-tripped?)
                        (let [trace' (conj trace trace-entry)
                              fallback (or (some-> (:error trace-entry) provider-error-ir)
                                         (render/->ast [:ir {}
                                                        [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                                                        [:p {} [:span {} "Provider call failed before the model could run."]]]))
                              result (merge {:answer fallback
                                             :status :error
                                             :status-id (status->id :error)
                                             :trace trace'
                                             :iteration-count (inc iteration)}
                                       (finalize-cost))]
                          result)
                        (recur (assoc loop-state
                                 :iteration (inc iteration)
                                 :provider-error-streak provider-error-streak
                                 :messages (conj messages {:role "user" :content error-feedback})
                                 :llm-provider {:error llm-provider-error}
                                 :trace (conj trace trace-entry))))))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking blocks final-result]} iteration-result
                        block (first blocks)
                        ;; Phase 7: merge per-iteration `:lru` stamps
                        ;; (collected by the patched resolve-symbol*)
                        ;; into the long-lived per-env LRU map. The trailer's
                        ;; live-vars view reads this to age user vars out of
                        ;; the discovery line after
                        ;; `JOURNAL_LRU_TURN_WINDOW` quiet turns.
                        ;; Phase 7 LRU merge. Flat: read both ends, then
                        ;; a single guarded swap!. No nested when-let.
                        lru-atom      (:def-resolve-lru-atom environment)
                        iteration-lru (not-empty (:lru block))
                        _ (when (and lru-atom iteration-lru)
                            (swap! lru-atom merge iteration-lru))
                        ;; Multi-form capture: every executed top-level form
                        ;; in this iter's fence becomes one envelope on the
                        ;; new :forms column. `:code` is the whole fence body
                        ;; concatenated for forensics. There is NO
                        ;; single-form result/error column.
                        ;; Cursor for trailer pin + per-form envelope keying.
                        ;; `iteration` here is the 0-based loop counter; the
                        ;; loop normalises it to 1-based via
                        ;; `(ctx-loop/set-turn-state! env :iteration (inc iteration))`
                        ;; at the top of each iter. The renderer +
                        ;; cursor-snapshot consume that atom, so they see
                        ;; 1-based iters. Trailer pin + form envelopes
                        ;; MUST agree with the renderer (model
                        ;; references scopes `tN/iM/fK` against the
                        ;; rendered cursor), so we read the same 1-based
                        ;; source here. Using raw `iteration` (0) at this
                        ;; point caused a turn-1 off-by-one: trailer
                        ;; pinned `t1/i0/fK` while the rendered ctx showed
                        ;; the model `t1/i1` — scope references in the
                        ;; form-results map didn't line up with the
                        ;; cursor the model saw.
                        cursor      {:turn (or (:turn-position (ctx-loop/read-turn-state environment)) 1)
                                     :iter (or (:iteration (ctx-loop/read-turn-state environment))
                                             (inc (long (or iteration 0))))}
                        ;; Expand multi-form blocks into per-form mini-blocks
                        ;; before projecting into engine envelopes. The eval
                        ;; pipeline returns ONE outer block per iter; that
                        ;; outer block carries a `:forms` vec with
                        ;; per-top-level-form `{:source :result :error}`
                        ;; entries (D3 \"multi-form capture\"). Without this
                        ;; expansion `blocks->forms` would treat the whole
                        ;; fence as a single envelope at f1, dropping every
                        ;; form after the first from the trailer pin and
                        ;; from the form-results map the model references
                        ;; by scope.
                        ;; Per-form expansion. Each entry inherits a
                        ;; `:channel` slice from the parent fence so the
                        ;; per-form envelope carries the pre-rendered tool
                        ;; sink IR persisted on `session_turn_iteration.forms`.
                        ;; Without this, restored bubbles for any iteration
                        ;; whose model wrote `(def r (patch …))` lost the
                        ;; PATCH preview pane (session
                        ;; 311fd734-3640-4288-ba3d-cff83fb7260f turn 8): the
                        ;; channel sink lives on the FENCE atom but the
                        ;; expansion dropped it when projecting per-form,
                        ;; so `block->envelope` couldn't attach `:channel`
                        ;; and the rebuild path had no IR to replay.
                        ;;
                        ;; Distribution rules:
                        ;;   * single-form fence → the whole fence sink
                        ;;     belongs to that one form, copy it over;
                        ;;   * multi-form fence → partition sink entries
                        ;;     by their recorded `:form-idx` (engine
                        ;;     stamps it on every entry); entries
                        ;;     without a form-idx (shape drift)
                        ;;     ride on the FIRST form so they aren't lost.
                        expanded-blocks
                        (mapcat
                          (fn [b]
                            (if-let [fs (seq (:forms b))]
                              (let [fence-channel (vec (:channel b))
                                    by-form-idx (group-by #(or (:form-idx %) ::no-form-idx)
                                                  fence-channel)
                                    orphan      (vec (get by-form-idx ::no-form-idx))
                                    single?     (= 1 (count fs))]
                                (map-indexed
                                  (fn [idx f]
                                    (let [direct (vec (get by-form-idx idx))
                                          channel (cond
                                                    single?              fence-channel
                                                    (and (zero? idx)
                                                      (seq orphan))      (into direct orphan)
                                                    :else                 direct)]
                                      (cond-> {:code   (or (:source f) (:src f) (:code f) "")
                                               :result (:result f)
                                               :error  (:error f)}
                                        (seq channel) (assoc :channel channel))))
                                  fs))
                              [b]))
                          blocks)
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
                        py-name->tag
                        (ctx-renderer/fold-op-index (extension/op-tag-index))
                        head-tag-resolver
                        (fn [head-sym]
                          (when head-sym (get py-name->tag (str head-sym))))
                        forms-vec   (if (seq expanded-blocks)
                                      (ctx-engine/blocks->forms expanded-blocks cursor head-tag-resolver)
                                      [(ctx-engine/block->envelope
                                         {:code "" :error {:message "empty iteration"}}
                                         1 cursor head-tag-resolver)])
                        fence-code  (str/join "\n" (keep :code blocks))
                        first-block (or (first blocks) {})
                        iteration-id (persistance/db-store-iteration! (:db-info environment)
                                       (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                         (cond-> {:session-turn-id session-turn-id
                                                  :code (or fence-code "")
                                                  :forms forms-vec
                                                  :duration-ms (long (or (envelope-duration-ms (:envelope first-block)) 0))
                                                  :llm-full-duration-ms (long (or (:duration-ms iteration-result) 0))
                                                  :thinking thinking
                                                  :answer (when final-result (answer-markdown (:answer final-result)))
                                                  :llm-messages (:llm-messages iteration-result)
                                                  :llm-provider (or (:llm-provider iteration-result) (:provider resolved-model))
                                                  :llm-model (:llm-model iteration-result)
                                                  :llm-raw-response (:llm-raw-response iteration-result)
                                                  :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                  :llm-returned-empty-code? (:llm-returned-empty-code? iteration-result)
                                                  :llm-assistant-message (:assistant-message iteration-result)
                                                  :llm-routing (llm-routing-summary pre-resolved-model iteration-result)
                                                  :cache-created-tokens (iteration-cache-created-tokens tc)}
                                           tc (assoc :tokens (:tokens tc)
                                                :cost-usd (:cost-usd tc)))))
                        _ (ctx-loop/set-turn-state! environment :iteration-id iteration-id)
                        ;; =====================================================
                        ;; CTX engine end-of-iter pipeline (D11/D12).
                        ;; Advance the trailer and log enough state for replay
                        ;; from ~/.vis/vis.log alone.
                        ;; =====================================================
                        ctx-atom-ref (:ctx-atom environment)
                        advance-iter-start-ms (System/currentTimeMillis)
                        hook-tasks-pre    (when ctx-atom-ref
                                            (into {}
                                              (for [[k v] (:session/tasks @ctx-atom-ref)
                                                    :when (= :hook (:source v))]
                                                [k (select-keys v [:status :done-born])])))
                        _ (when ctx-atom-ref
                            (swap! ctx-atom-ref
                              (fn [c]
                                (ctx-engine/advance-iter
                                  (assoc c :session/scope cursor)
                                  forms-vec))))
                        advance-iter-duration-ms (- (System/currentTimeMillis) advance-iter-start-ms)
                        trailer-after-pin (when ctx-atom-ref (:session/trailer @ctx-atom-ref))
                        ;; Per-pin token telemetry — the measurement loop
                        ;; behind every trailer diet: what does THIS pin
                        ;; cost as the frozen <results> message it will
                        ;; ride every subsequent prompt as?
                        _ (when-let [pin (and (seq trailer-after-pin)
                                           (let [p (peek trailer-after-pin)]
                                             (when (= (str "t" (:turn cursor) "/i" (:iter cursor))
                                                     (:scope p))
                                               p)))]
                            (try
                              (tel/log! {:level :info :id ::trailer-pin-tokens
                                         :data {:scope  (:scope pin)
                                                :forms  (count (:forms pin))
                                                :tokens (tokens/count-tokens
                                                          (ctx-renderer/render-trailer-pin pin))}})
                              (catch Throwable _ nil)))
                        form-results-map  (when ctx-atom-ref (ctx-loop/trailer->form-results (or trailer-after-pin [])))
                        hook-tasks-post   (when ctx-atom-ref
                                            (into {}
                                              (for [[k v] (:session/tasks @ctx-atom-ref)
                                                    :when (= :hook (:source v))]
                                                [k (select-keys v [:status :done-born])])))
                        warnings-post     (when ctx-atom-ref (:engine/warnings @ctx-atom-ref))
                        _ (when ctx-atom-ref
                            (tel/log! {:level :info :id ::iter-end-ctx
                                       :data {:iteration iteration
                                              :cursor cursor
                                              :pinned-forms (count forms-vec)
                                              :trailer-entries (count (or trailer-after-pin []))
                                              :form-result-scopes (vec (sort (keys (or form-results-map {}))))
                                              :hook-tasks-pre hook-tasks-pre
                                              :hook-tasks-post hook-tasks-post
                                              :hook-tasks-changed? (not= hook-tasks-pre hook-tasks-post)
                                              :warnings warnings-post
                                              :advance-iter-ms advance-iter-duration-ms}}
                              "CTX iter-end: trailer pinned"))
                        trace-entry {:iteration iteration :thinking thinking
                                     :blocks blocks :final? (boolean final-result)}]
                    (cond
                      final-result
                      (do (log-stage! :final iteration
                            {:answer (answer-markdown (:answer final-result))
                             :iteration-count (inc iteration)})
                        (log-stage! :iteration/stop iteration
                          {:blocks (count blocks) :errors (count (filter :error blocks))
                           :times (mapv block-duration-ms blocks)})
                        ;; Iteration-final chunk (`:phase :iteration-final`).
                        ;; Per-form chunks already streamed every form
                        ;; result; this is the trim \"iteration is
                        ;; complete, here is the terminal answer\"
                        ;; signal. Consumers attach `:final` to
                        ;; whatever's already on screen.
                        ;;
                        ;; `:answer-position` tells the channel which
                        ;; per-block slot was the `done(...)` call;
                        ;; the progress tracker elides that slot so
                        ;; the renderer doesn't paint the answer
                        ;; call's code above the answer text.
                        (when on-chunk
                          (on-chunk {:phase            :iteration-final
                                     :iteration-count  (inc (long iteration))
                                     :thinking         thinking
                                     :final            {:answer          (:answer final-result)
                                                        :iteration-count (inc iteration)
                                                        :status          :success}
                                     :answer-position  (:answer-position final-result)
                                     :silent-form-idxs (:silent-form-idxs iteration-result)
                                     ;; Live working-memory snapshot so the F2
                                     ;; context dialog updates DURING the turn,
                                     ;; not only after it ends.
                                     :tasks            (when ctx-atom-ref (ctx-engine/nest-tasks (:session/tasks @ctx-atom-ref)))
                                     :facts            (when ctx-atom-ref (:session/facts @ctx-atom-ref))
                                     :done?            true}))
                        (let [result (-> (merge {:answer (:answer final-result) :trace (conj trace trace-entry)
                                                 :iteration-count (inc iteration)
                                                 :utilization (let [u @usage-atom
                                                                    req (if (pos? (long (:iter-count u)))
                                                                          (long (:last-iter-input u))
                                                                          (long (:previous-request-input u)))]
                                                                (ctx-engine/utilization req effective-context-limit
                                                                  (:input-tokens u)
                                                                  safe-guards/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                           (finalize-cost))
                                       (attach-llm-routing-summary pre-resolved-model iteration-result))]
                          (auto-archive-hot-symbols! environment)
                          result))

                      :else
                      (if (empty? blocks)
                        (do (log-stage! :empty iteration {})
                          (log-stage! :iteration/stop iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :provider-error-streak 0
                                    :trace (conj trace trace-entry)})))

                        (do (log-stage! :iteration/stop iteration
                              {:blocks (count blocks) :errors (count (filter :error blocks))
                               :times (mapv block-duration-ms blocks)})
                          (let [_ blocks
                                ;; Repetition-only loop detection (no iteration or
                                ;; budget counting): a `done(…)` that reached here
                                ;; did NOT finalize, 2 in a row ⇒ stuck; or identical
                                ;; non-`done()` action code repeats ⇒ stuck.
                                {:keys [stuck? done-streak action-sig]} (repetition-loop-state blocks (:stuck loop-state))
                                nudged?       (boolean (:nudged? (:stuck loop-state)))
                                sticky        (some-> (:best-answer-atom environment) deref :value)
                                ;; Checkpoint already shown AND still stuck ⇒ force-finalize.
                                forced?       (and nudged? stuck?)
                                forced-answer (or sticky {:answer loop-give-up-text})
                                next-recent (conj (vec (or trailer-iters []))
                                              [(inc (long iteration))
                                               {:thinking thinking
                                                :blocks   blocks
                                                :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                :llm-provider (:llm-provider iteration-result)
                                                :llm-model    (:llm-model iteration-result)
                                                  ;; svar's canonical replay handle for this
                                                  ;; iteration. Re-emitted only within this
                                                  ;; live user turn via
                                                  ;; `append-preserved-thinking-replay`; cross-turn
                                                  ;; seeds opt out with
                                                  ;; `:preserved-thinking/replay? false`.
                                                :assistant-message (:assistant-message iteration-result)
                                                :preserved-thinking/replay? true}])]
                            ;; ONE iteration-final chunk, AFTER the decision. Terminal
                            ;; (:done? true + :final answer) when force-finalizing so
                            ;; live channels render the forced answer — the bug fix:
                            ;; previously a non-terminal chunk fired BEFORE the
                            ;; force-finalize, so the forced answer never showed.
                            (when on-chunk
                              (on-chunk (if forced?
                                          {:phase            :iteration-final
                                           :iteration-count  (inc (long iteration))
                                           :thinking         thinking
                                           :final            {:answer          forced-answer
                                                              :iteration-count (inc iteration)
                                                              :status          :success}
                                           :silent-form-idxs (:silent-form-idxs iteration-result)
                                           :tasks            (when ctx-atom-ref (ctx-engine/nest-tasks (:session/tasks @ctx-atom-ref)))
                                           :facts            (when ctx-atom-ref (:session/facts @ctx-atom-ref))
                                           :done?            true}
                                          {:phase            :iteration-final
                                           :iteration        (inc (long iteration))
                                           :thinking         thinking
                                           :final            nil
                                           :silent-form-idxs (:silent-form-idxs iteration-result)
                                           :tasks            (when ctx-atom-ref (ctx-engine/nest-tasks (:session/tasks @ctx-atom-ref)))
                                           :facts            (when ctx-atom-ref (:session/facts @ctx-atom-ref))
                                           :done?            false})))
                            (if forced?
                              (do (log-stage! :final iteration {:reason :loop-forced
                                                                :iteration-count (inc iteration)})
                                ;; NB: no `:status` key — mirrors the normal
                                ;; success path so prior_outcome derives to
                                ;; `complete` (a bare `:status :success` violates
                                ;; the session_turn_state.prior_outcome CHECK).
                                (-> (merge {:answer forced-answer
                                            :trace (conj trace trace-entry)
                                            :iteration-count (inc iteration)
                                            :utilization (let [u @usage-atom
                                                               req (if (pos? (long (:iter-count u)))
                                                                     (long (:last-iter-input u))
                                                                     (long (:previous-request-input u)))]
                                                           (ctx-engine/utilization req effective-context-limit
                                                             (:input-tokens u)
                                                             safe-guards/DEFAULT_PROMPT_BUDGET_TOKENS))}
                                      (finalize-cost))
                                  (attach-llm-routing-summary pre-resolved-model iteration-result)))
                              (recur (merge (dissoc loop-state :llm-provider)
                                       {:iteration          (inc iteration)
                                        :provider-error-streak 0
                                        ;; Inject ONE guidance turn:
                                        ;;  - repetition detected → stern
                                        ;;    decision-checkpoint;
                                        ;;  - else a mutation `done()` proposal →
                                        ;;    gentle confirm/refine.
                                        :messages           (cond-> messages
                                                              stuck?
                                                              (conj {:role "user"
                                                                     :content (loop-checkpoint-message
                                                                                (when sticky (answer-markdown sticky)))})

                                                              (and (not stuck?)
                                                                (:answer-proposed? iteration-result))
                                                              (conj {:role "user"
                                                                     :content (proposal-confirm-message
                                                                                (when sticky (answer-markdown sticky)))}))
                                        :trace              (conj trace trace-entry)
                                        :trailer-iters      next-recent
                                        :stuck              {:done-streak done-streak
                                                             :last-sig    action-sig
                                                             :nudged?     stuck?}})))))))))))))))))

(defn- slash-ctx-for-env
  "Build the slash dispatch ctx from a turn env. Pure data; carries
   the channel/session/workspace coordinates the slash handlers read."
  [env user-request]
  (let [db-info  (:db-info env)
        state-id (or (:session/state-id env)
                   (when db-info
                     (some-> (:session-id env)
                       (persistance/db-latest-session-state-id db-info))))]
    (cond-> {:channel/id   (or (:channel env) :tui)
             :session/id   (:session-id env)
             :db-info      db-info
             :command/raw  user-request}
      state-id              (assoc :session/state-id state-id)
      (:workspace/id env)   (assoc :workspace/id (:workspace/id env)))))

(defn- slash-body->ir
  "Coerce a slash `:slash/body` value to canonical IR.
   - nil          -> nil (no body)
   - IR vector    -> normalized through render/->ast (identity-fast-path)
   - Hiccup vec   -> rebuilt as IR via render/->ast
   - String       -> parsed as Markdown via render/markdown->ir
   - anything else -> rendered as the printable form (defensive)"
  [body]
  (cond
    (nil? body)                       nil
    (render/ir? body)                 (render/->ast body)
    (and (vector? body) (keyword? (first body)))
    (render/->ast body)
    (string? body)                    (render/markdown->ir body)
    :else                             (render/markdown->ir (pr-str body))))

(defn- ir->markdown
  "Render IR back to a flat Markdown string for the persisted
   `answer_markdown` column. nil round-trips as nil."
  [ir]
  (when ir (render/render ir :markdown)))

(defn- slash-result->answer-markdown
  "Build the persisted `answer_markdown` for a slash turn from the
   dispatch envelope. The body in `:slash/result` is IR-or-string;
   we coerce to IR, render to Markdown, and prefix the title.
   Channels display the IR directly; persistence stays plain Markdown
   for transcript export / re-render."
  [{:keys [result error reason]}]
  (cond
    result
    (let [title (or (:slash/title result) "Slash handled")
          ir    (slash-body->ir (:slash/body result))
          body  (some-> ir ir->markdown str/trim not-empty)]
      (cond-> (str "**" title "**")
        body (str "\n\n" body)))
    error
    (str "**Slash error** (" (name (or reason :error)) ")\n\n" error)
    :else
    "_slash handled_"))

(defn- apply-slash-mutations!
  "Route a slash result's `:slash/tasks / :slash/facts` entries through
   `ctx-loop/apply-and-record!` so the slash leaves the same kind of CTX
   trace a model-emitted `plan_step(...)` / `fact_set(...)` would.
   Engine FSM checks and dedup run identically; warnings land on
   `:engine/warnings` for the next render pass."
  [env slash-result]
  (let [result (:result slash-result)]
    (when (map? result)
      (doseq [[k partial] (:slash/tasks result)]
        (ctx-loop/apply-and-record! env :task-set! [k partial]))
      (doseq [[k partial] (:slash/facts result)]
        (ctx-loop/apply-and-record! env :fact-set! [k partial])))))

(defn- run-slash-turn!
  "Persist a slash-only turn: one `session_turn_soul` + state + ONE
   synthetic `session_turn_iteration` whose forms vec carries the slash
   envelope at `:tag :user-slash`. The turn is marked :success without
   any LLM round-trip. Returns the same shape `iteration-loop` would
   have produced (so callers don't special-case slash turns).

   When the slash result carries `:slash/tasks / :facts`,
   those mutations are applied to the env's `ctx-atom` via
   `apply-and-record!` BEFORE the synthetic iter row lands, so the
   end-of-turn ctx snapshot (persisted to `session_turn_state.ctx`)
   includes them. Engine warnings collected here flow into
   `:engine/warnings` for the next render pass."
  [env user-request slash-result]
  (let [db-info     (:db-info env)
        turn-id     (persistance/db-store-session-turn! db-info
                      {:parent-session-id (:session-id env)
                       :user-request      user-request
                       :status            :running})
        turn-pos    (or (session-turn-position env turn-id) 1)]
    ;; Stamp turn-state so synthesize-scope returns the canonical
    ;; `t<N>/i1/f1` scope for any CTX mutations the slash emits.
    (ctx-loop/set-turn-state! env
      :iteration-id    nil
      :session-turn-id turn-id
      :user-request    user-request
      :turn-position   turn-pos
      :iteration       1
      :form-idx        0)
    (apply-slash-mutations! env slash-result)
    (let [scope     (str "t" turn-pos "/i1/f1")
          envelope  {:scope  scope
                     :tag    :user-slash
                     :src    user-request
                     :result (or (:result slash-result)
                               {:slash/status :error
                                :slash/title  (or (:error slash-result) "slash error")
                                :slash/reason (:reason slash-result)})}
          answer-md (slash-result->answer-markdown slash-result)
          ;; Snapshot the CTX as it stands AFTER the slash mutations
          ;; so resume picks up the spec/task/fact writes. Mirrors
          ;; run-normal-turn!'s ctx-snapshot path: gc-pass + strip
          ;; cursor + drop ephemerals before Nippy-encoding.
          ctx-snapshot (when-let [ca (:ctx-atom env)]
                         (let [stamped (ctx-loop/stamp-cursor env @ca)
                               gced    (ctx-engine/gc-pass stamped)
                               clean   (-> gced
                                         (dissoc :session/scope)
                                         ctx-engine/strip-ephemeral)]
                           (reset! ca clean)
                           clean))]
      (try (persistance/db-store-iteration! db-info
             {:session-turn-id          turn-id
              :code                     user-request
              :forms                    [envelope]
              :duration-ms              0
              :llm-full-duration-ms     0
              :thinking                 ""
              :answer                   answer-md
              :llm-messages             []
              :llm-returned-empty-code? false})
        (catch Throwable t
          (tel/log! {:level :warn :id ::slash-iter-persist-failed
                     :data  {:error (ex-message t)}})))
      (persistance/db-update-session-turn! db-info turn-id
        {:answer-markdown answer-md
         :iteration-count 1
         :duration-ms     0
         :status          :success
         :prior-outcome   :complete
         :ctx             ctx-snapshot})
      {:session-turn-id turn-id
       :answer          answer-md
       :iteration-count 1
       :duration-ms     0
       :status          :success
       :slash           slash-result
       :prior-outcome   :complete})))

;; =============================================================================
;; Title listeners + set-title! broadcast
;;
;; Channels (TUI, Telegram, ...) that want to react to a session
;; title change - typically because the model emitted `set_session_title("...")`
;; mid-turn - register a listener via `add-title-listener!`. The
;; listener fn receives the new title; it MUST be cheap (typically a
;; `state/dispatch` into the channel's app-db). Listeners are stored
;; per session-id so a TUI watching session A doesn't get
;; woken by a Telegram bot updating session B.
;;
;; Both `set-title!` (host-driven, e.g. CLI rename) and the sandbox
;; `set_session_title("...")` fn (model-driven) funnel through
;; `set-title-with-broadcast!`, which is the single mutation point.
;; That keeps the in-memory env atom + DB column + listener fan-out
;; in lockstep - no path can update one without the others.
;; =============================================================================

(defonce ^:private title-listeners
  ;; {session-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-listener!
  "Register `listener-fn` for `session-id`. The fn is invoked with
   the new title (a string) every time the title changes. Multiple
   listeners are supported; they fire in unspecified order.

   Returns the listener fn so callers can pass it to
   `remove-title-listener!` later."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-listener!
  "Deregister a previously added listener. Idempotent."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defonce ^:private global-title-listeners
  ;; #{listener-fn ...} - fns of [session-id-uuid title], fired on EVERY
  ;; session's title change (the per-session listeners above stay scoped).
  (atom #{}))

(defn add-global-title-listener!
  "Register `listener-fn` to observe title changes across ALL sessions.
   The fn is invoked with the session id (a UUID) and the new title
   every time ANY session's title changes - host rename, model
   `set_session_title(...)` and auto-title generation alike, since they
   all funnel through `set-title-with-broadcast!`.

   Returns the listener fn so callers can pass it to
   `remove-global-title-listener!` later."
  [listener-fn]
  (swap! global-title-listeners conj listener-fn)
  listener-fn)

(defn remove-global-title-listener!
  "Deregister a previously added global title listener. Idempotent."
  [listener-fn]
  (swap! global-title-listeners disj listener-fn)
  nil)

(defn- broadcast-title-change!
  "Fire every registered listener for `session-id` with `title`, then
   every GLOBAL listener with `(session-id title)`. Listeners that throw
   are swallowed and logged - a misbehaving channel must NOT block the
   iteration loop."
  [session-id title]
  (let [cid (persistance/->uuid session-id)]
    (doseq [f (get @title-listeners cid)]
      (try (f title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Title listener threw: " (ex-message t))}))))
    (doseq [f @global-title-listeners]
      (try (f cid title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::global-title-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Global title listener threw: " (ex-message t))}))))))

;; ── Title-PENDING listeners ────────────────────────────────────────────────
;; A separate channel from the title VALUE listeners above: this one fires
;; `true` when host auto-title generation STARTS and `false` when it ends,
;; so a channel can show a "generating title…" spinner. Kept distinct so the
;; pending signal never gets confused with a real title string.

(defonce ^:private title-pending-listeners
  ;; {session-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-pending-listener!
  "Register `listener-fn` for `session-id`; invoked with a boolean
   (true when title generation starts, false when it ends). Returns the
   listener fn for later `remove-title-pending-listener!`."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-pending-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-pending-listener!
  "Deregister a previously added pending listener. Idempotent."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-pending-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defn- broadcast-title-pending!
  "Fire every pending listener for `session-id` with `pending?`.
   Listeners that throw are swallowed and logged."
  [session-id pending?]
  (let [cid (persistance/->uuid session-id)]
    (doseq [f (get @title-pending-listeners cid)]
      (try (f (boolean pending?))
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-pending-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Title-pending listener threw: " (ex-message t))}))))))

(defn set-title-with-broadcast!
  "Single mutation point for session titles.

   1. Writes the title to the persisted `session_state` row.
   2. Updates the env's in-memory `:session-title-atom` so the next iteration's
      `:session-title-atom` mirror sees the new value AND so a
      read from the Python sandbox returns the fresh string immediately,
      without a DB round-trip.
   3. Broadcasts to every registered listener.

   `session-title-atom` may be nil (host-driven path with no live env)."
  [db-info session-id session-title-atom title]
  (let [t (str title)]
    (persistance/db-update-session-title! db-info session-id t)
    (when session-title-atom (reset! session-title-atom t))
    (broadcast-title-change! session-id t)
    nil))

(def ^:private AUTO_TITLE_MAX_CHARS 80)
(def ^:private AUTO_TITLE_TTFT_MS 15000)
(def ^:private AUTO_TITLE_IDLE_MS 10000)
(def ^:private AUTO_TITLE_SEMANTIC_MS 30000)

(def ^:private auto-title-placeholder-labels
  #{"untitled" "untitled session"})

(defn- auto-title-placeholder?
  [s]
  (contains? auto-title-placeholder-labels
    (str/lower-case (str/trim (str s)))))

(defn- usable-existing-title
  [s]
  (let [t (some-> s str str/trim not-empty)]
    (when-not (auto-title-placeholder? t)
      t)))

(defn- strip-code-fence
  "Drop a surrounding Markdown code fence so a model that answers with
   ```text\n<title>\n``` doesn't leak the fence info-string (`text`) as the
   title. Returns the inner body when fenced, else the input unchanged."
  [s]
  (let [t (str/trim (or s ""))]
    (if (str/starts-with? t "```")
      (-> t
        (str/replace #"(?s)\A```[^\n]*\n?" "")
        (str/replace #"\n?```\s*\z" "")
        str/trim)
      t)))

(defn- sanitize-auto-title
  [s]
  (let [line (->> (-> (or s "") str strip-code-fence str/split-lines)
               (map str/trim)
               (remove str/blank?)
               ;; skip any stray fence markers left mid-string
               (remove #(re-matches #"`{3,}.*" %))
               first
               (#(or % ""))
               (#(-> %
                   (str/replace #"(?i)^\s*(title|new title)\s*[:\-–—]\s*" "")
                   (str/replace #"^[\s\"'`*_#>\-–—]+" "")
                   (str/replace #"[\s\"'`*_#>\-–—.]+$" "")
                   str/trim)))
        clipped (truncate line AUTO_TITLE_MAX_CHARS)]
    (when-not (or (str/blank? clipped)
                (auto-title-placeholder? clipped))
      clipped)))

(def ^:private uuid-text-pattern
  #"(?i)\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b")

(defn- fallback-auto-title
  "Cheap deterministic title when the auto-title LLM route fails. Keeps TUI
   sessions from staying `Untitled` just because the cheapest routed model was
   unavailable/unsupported."
  [user-request]
  (let [words (->> (str/replace (str user-request) uuid-text-pattern " ")
                (re-seq #"[\p{L}\p{N}][\p{L}\p{N}'-]*")
                (take 7)
                (str/join " "))]
    (sanitize-auto-title words)))

(def ^:private auto-title-spec
  "Structured-output spec for the title side-channel. Using `ask!` + spec (SAP
   JSON parsing) instead of `ask-code!` fence extraction removes a whole class
   of bugs: a model that wraps its answer in a text code fence no longer leaks
   the fence info-string as the title — SAP reads the JSON title field
   regardless of surrounding markdown."
  (svar/spec
    (svar/field svar/NAME :title
      svar/TYPE svar/TYPE_STRING
      svar/CARDINALITY svar/CARDINALITY_ONE
      svar/DESCRIPTION "Short session title: 3-7 words, a stable noun phrase, no surrounding quotes or markdown.")))

(defn- auto-title-prompt
  [previous-title user-request]
  [{:role "system"
    :content (str "You generate short chat/session titles. 3-7 words, a stable noun phrase. "
               "Use the user's latest request and the previous title. If the previous title "
               "still fits, keep it unchanged; otherwise update it to reflect the new focus.")}
   {:role "user"
    :content (str "Previous title: " (or (not-empty previous-title) "<none>")
               "\nLatest user request:\n" user-request)}])

(def ^:private AUTO_TITLE_PROVIDER_ORDER
  "Preferred provider order for the auto-title side-channel. These are all
   flat-fee coding-plan subscriptions, so per-token `:cost` is the WRONG lens
   (it would dodge the metered-but-actually-free plans toward $0 Copilot).
   Instead we pin each plan in this deliberate order and pick its SMALLEST
   model (`{:provider p :optimize [:cost :speed]}` selects the cheapest +
   fastest model WITHIN the pinned provider: glm on zai, gpt-5.3-codex on
   codex, haiku on anthropic, a mini on copilot). Any configured provider not
   listed here is appended afterwards so the chain still covers the whole
   fleet. First provider that returns a usable title wins; on failure
   (model unavailable / endpoint rejects) we fall through to the next."
  [:zai-coding-plan
   :openai-codex
   :anthropic-coding-plan
   :github-copilot-individual
   :github-copilot-business
   :github-copilot-enterprise])

(defn- model-auto-title!
  "Generate a session title before the main provider call, off the model's
   visible surface. A single `ask!` declares the preferred plan order via
   `:prefer-providers`; svar walks it natively (cheapest+fastest model per
   plan, falling through on model-unsupported / transient failure) so there is
   no host-side provider loop. Returns nil if the whole chain fails (the caller
   then keeps the previous title or uses the deterministic fallback)."
  [{:keys [router]} previous-title user-request]
  (let [resp (try
               (svar/ask! router
                 (with-default-ask-code-idle-timeout
                   {:messages            (auto-title-prompt previous-title user-request)
                    :spec                auto-title-spec
                    :reasoning           :off
                    :routing             {:prefer-providers AUTO_TITLE_PROVIDER_ORDER
                                          :optimize         [:cost :speed]}
                    :ttft-timeout-ms     AUTO_TITLE_TTFT_MS
                    :idle-timeout-ms     AUTO_TITLE_IDLE_MS
                    :semantic-timeout-ms AUTO_TITLE_SEMANTIC_MS}))
               (catch Throwable t
                 (tel/log! {:level :debug
                            :id ::auto-title-call-failed
                            :data {:error (ex-message t)}}
                   "Auto-title call failed across all preferred providers")
                 nil))]
    (sanitize-auto-title (some-> resp :result :title))))

(defn- maybe-auto-title!
  "Generate the session title ONCE, asynchronously, the first time a normal
   LLM turn runs without a usable title. After a title exists it is never
   regenerated - re-titling every turn only churns the channel chrome. Manual
   `set_session_title(...)` remains available as an override. Returns a
   future or nil; callers intentionally do not wait."
  [{:keys [db-info session-id session-title-atom] :as env} user-request]
  (when (and db-info session-id (:router env)
          (not (usable-existing-title (some-> session-title-atom deref))))
    (future
      ;; Announce "generating title" so a channel (TUI) can spinner the
      ;; tab; always clear it in the finally.
      (broadcast-title-pending! session-id true)
      (try
        (let [title (or (try
                          (model-auto-title! env nil user-request)
                          (catch Throwable t
                            (tel/log! {:level :warn
                                       :id ::auto-title-failed
                                       :data {:session-id session-id
                                              :error (ex-message t)}}
                              "Auto-title LLM call failed; using deterministic fallback")
                            nil))
                      (fallback-auto-title user-request))]
          (when (seq title)
            (set-title-with-broadcast! db-info session-id session-title-atom title)))
        (catch Throwable t
          (tel/log! {:level :warn
                     :id ::auto-title-update-failed
                     :data {:session-id session-id
                            :error (ex-message t)}}
            "Auto-title update failed; keeping existing title"))
        (finally
          (broadcast-title-pending! session-id false))))))

(defn- run-normal-turn!
  "LLM round-trip path: store turn, run iteration-loop, persist
   the end-of-turn CTX snapshot, update the turn row with answer +
   tokens. Called by `run-turn!` when slash dispatch said the user
   message was NOT a slash."
  [env user-request loop-opts]
  (let [session-turn-id (persistance/db-store-session-turn! (:db-info env)
                          {:parent-session-id (:session-id env)
                           :user-request user-request
                           :status :running})
        turn-position (session-turn-position env session-turn-id)
        _ (ctx-loop/set-turn-state! env
            :session-turn-id session-turn-id
            :user-request user-request
            :turn-position (or turn-position 1)
            :iteration nil
            :form-idx nil
            :iteration-id nil)
        _ (maybe-auto-title! env user-request)
        result (iteration-loop env user-request
                 (assoc loop-opts :session-turn-id session-turn-id))
        prior-outcome (:status result)
        ;; Snapshot the CTX as it stands at end-of-turn. Run gc-pass first
        ;; so terminal-status entries past their TTL drop out of the live
        ;; tree before persistence; historical snapshots in earlier
        ;; session_turn_state rows still carry them, and the archive store +
        ;; recall({"match"/"ids" …}) can reach them. The renderer stamps the cursor in fresh each
        ;; call; we drop the cursor before persisting because the next-turn
        ;; loader will derive a new cursor from the loop counters (cursor
        ;; is iter-local, not turn-local). Persisted Nippy-encoded to
        ;; session_turn_state.ctx in the same transaction that flips the
        ;; turn status, so live CTX = ctx on the latest turn-state for the
        ;; latest turn-soul of the session_state.
        ctx-snapshot (when-let [ca (:ctx-atom env)]
                       (let [stamped (ctx-loop/stamp-cursor env @ca)
                             gced    (ctx-engine/gc-pass stamped)
                             ;; Strip cursor + every `:engine/*` ephemeral
                             ;; key (warnings, pending-satisfies) before
                             ;; persisting. The next resume rebuilds the
                             ;; cursor from loop counters and starts each
                             ;; turn with empty ephemerals via empty-ctx.
                             clean   (-> gced
                                       (dissoc :session/scope)
                                       ctx-engine/strip-ephemeral)]
                         (reset! ca clean)
                         clean))
        _ (persistance/db-update-session-turn! (:db-info env) session-turn-id
            {;; The persisted answer is the raw Markdown source the
             ;; model wrote in `done("""...""")`. Channels parse
             ;; the Markdown into IR at render time via
             ;; `render/markdown->ir`; the database stays human-
             ;; readable and round-trips byte-for-byte through copy /
             ;; export / transcript.
             :answer-markdown (when-let [a (:answer result)] (answer-markdown a))
             :iteration-count (:iteration-count result)
             :duration-ms     (:duration-ms result)
             :status          (or (:status result) :success)
             :tokens          (:tokens result)
             :cost            (:cost result)
             :prior-outcome   prior-outcome
             :ctx             ctx-snapshot})]
    (assoc result :session-turn-id session-turn-id :prior-outcome prior-outcome)))

(defn- health-gated-router
  "ONE health gate for every routing entry point (turn start AND
   sub_loop child): demote unreachable LOCAL providers to the router's
   end (`providers/demote-unreachable-providers` — never throws) and
   log the demotion once. Returns `{:router r :demoted [ids]}`."
  [router where]
  (let [{:keys [demoted] :as gated} (providers/demote-unreachable-providers router)]
    (when (seq demoted)
      (tel/log! {:level :warn :id ::unreachable-providers-demoted
                 :data {:demoted demoted :where where}
                 :msg "router health gate: unreachable local providers demoted to last resort"}))
    gated))

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
   transcript still shows the user message + the slash envelope."
  [env user-request loop-opts]
  (when-not (map? env)
    (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let [;; Re-resolve the active workspace from the session's CURRENT pin so a
        ;; mid-session `/draft new | apply | abandon` takes effect THIS turn.
        ;; The cached env was built at session start (on trunk); without this
        ;; the agent keeps editing trunk after entering a draft.
        env (or (when-let [db (:db-info env)]
                  (when-let [sid (or (:session/state-id env)
                                   (some->> (:session-id env)
                                     (persistance/db-latest-session-state-id db)))]
                    (when-let [ws (persistance/db-workspace-for-session db sid)]
                      (assoc env
                        :workspace      ws
                        :workspace/id   (:id ws)
                        :workspace/root (:root ws)
                        ;; Carry the extra context roots across the mid-session
                        ;; env rebuild too — create-environment seeds them, but
                        ;; a /draft refresh re-derived the env from the row and
                        ;; dropped them, silently reverting to primary-root-only
                        ;; confinement for the rest of the session.
                        :workspace/context-roots (vec (:context-roots ws))))))
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
        env (let [{:keys [router demoted]} (health-gated-router (:router env) :turn)
                  dset (set demoted)
                  env' (cond-> (assoc env :router router)
                         (and (seq dset) (seq (:available (:routing env))))
                         (update-in [:routing :available]
                           (fn [avail]
                             (vec (remove #(contains? dset (:provider %)) avail)))))]
              (when (seq demoted)
                (when-let [ca (:ctx-atom env)]
                  (swap! ca update :engine/warnings (fnil conj [])
                    {:code    :provider-unreachable
                     :anchor  [:session/routing]
                     :message (str "Local provider(s) "
                                (str/join ", " (map name demoted))
                                " unreachable — demoted to last-resort and hidden"
                                " from routing for this turn.")})))
              env')
        slash-result (try (slash/dispatch env (slash-ctx-for-env env user-request) user-request)
                       (catch Throwable t
                         (tel/log! {:level :warn :id ::slash-dispatch-threw
                                    :data  {:user-request user-request
                                            :error        (ex-message t)}})
                         {:handled? false}))]
    (if (:handled? slash-result)
      (run-slash-turn! env user-request slash-result)
      (run-normal-turn! env user-request loop-opts))))

(defn custom-bindings
  "Current custom sandbox bindings {sym -> value}."
  [env]
  (some-> (:state-atom env) deref :custom-bindings))

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
  (let [model   (some-> model str str/trim not-empty)
        prov-kw (some-> provider name keyword)
        prov    (when prov-kw (first (filter #(= (:id %) prov-kw) (:providers router))))
        owns?   (fn [p] (and model (some #(= (:name %) model) (:models p))))]
    (cond
      (and model prov (owns? prov))            {:provider prov-kw :model model}
      (and model (some owns? (:providers router))) {:model model}
      :else                                    {})))

(defn- prepare-turn-context
  "Validates inputs, resolves sandbox bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model
                max-context-tokens
                system-prompt debug? hooks cancel-token eval-timeout-ms
                reasoning-default routing extra-body]
         :or   {debug? false}} opts]
    (when-not (:db-info env)
      (anomaly/incorrect! "Invalid RLM environment" {:type :vis/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect! "messages must be a non-empty vector of message maps, e.g. [(svar/user \"...\")]"
        {:type :vis/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect! ":eval-timeout-ms must be an integer (milliseconds)"
        {:type     :vis/invalid-eval-timeout
         :got      eval-timeout-ms
         :got-type (type eval-timeout-ms)}))
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
          session-pref  (when (:session-id env)
                          (session-model/model-of (:db-info env) (:session-id env)))
          model         (or model (:model session-pref))
          pref-provider (:provider session-pref)
          ;; Cancellation TOKEN carries the cooperative flag AND the
          ;; on-cancel! callback registry that hard-cancels Python /
          ;; provider futures. Callers create one via
          ;; `cancellation/cancellation-token` and pass it as
          ;; `:cancel-token`. The derived atom is the lower-level
          ;; primitive every poll site checks.
          cancel-token           (or cancel-token
                                   (cancellation/cancellation-token))
          cancel-atom            (cancellation/cancellation-atom cancel-token)
          ;; `user-request` = ONLY the current turn's user message.
          ;;
          ;; Prior behavior joined every message's :content (including
          ;; previous turns' user messages + assistant answers + system!) into
          ;; one growing blob. That corrupted three things at once:
          ;;   1. the persisted user request stored the entire transcript
          ;;      for every turn - the sidebar showed "Siema\nSiema!\n...".
          ;;   2. Any model-facing context derived from that blob grew with
          ;;      each turn instead of reflecting the current ask. Surface now
          ;;      flows through ctx.
          ;;   3. The synthetic `{:requirement ...}` frame the LLM sees
          ;;      restated the whole session as the "requirement".
          ;;
          ;; Prior dialog transcript is dropped here. `user-request` is
          ;; ONLY the current turn - one ask, one value.
          extract-text           (fn [c]
                                   (cond
                                     (string? c)     c
                                     (sequential? c) (str/join " "
                                                       (keep #(when (= "text" (:type %)) (:text %)) c))
                                     :else           nil))
          ;; Locate the LAST user message once. It is the only human text
          ;; sent into this turn. Prior dialog transcript is intentionally
          ;; NOT replayed to the model; durable context flows through
          ;; persisted iterations, defs, SYSTEM vars, and DB-backed tools.
          last-user-idx          (->> (map-indexed vector messages)
                                   reverse
                                   (some (fn [[i m]]
                                           (when (contains? #{"user" :user} (:role m))
                                             i))))
          last-user-message      (when last-user-idx (nth messages last-user-idx))
          user-request           (or (some-> last-user-message :content extract-text)
                                   ;; Fallback: no :user role found (malformed caller) -
                                   ;; use the last message's text. Better than an empty user request.
                                   (some-> messages last :content extract-text)
                                   "")
          ;; A `:model` preference HOISTS that model to the router root for
          ;; DISPLAY + COST: `resolve-effective-model` reads the vector head, so
          ;; root-model/root-provider (and the persisted cost label) reflect the
          ;; pick. Blank/unknown names degrade to the config order.
          env-router             (cond-> (:router env)
                                   (and model (not (str/blank? (str model))))
                                   (router-for-model model))
          ;; …but vector order does NOT bind svar's actual selection (it sorts
          ;; by provider :priority). FORCE the pick into `:routing` so the call
          ;; truly lands on the chosen provider+model. A caller-supplied
          ;; `:routing` (e.g. sub_loop's own pin) wins on merge.
          routing                (merge (forced-routing-for-pref (:router env) pref-provider model)
                                   (or routing {}))
          root-resolved-model    (when env-router (resolve-effective-model env-router))
          root-model             (or (:name root-resolved-model) model)
          root-provider          (:provider root-resolved-model)
          db-info                (:db-info env)
          custom-bindings        (custom-bindings env)
          python-context                (:python-context env)
          _                      (doseq [[sym val] (or custom-bindings {})]
                                   (when val (env/set-python-binding! python-context sym val)))
          ;; Workspace pin lives on the env itself (set in create-environment).
          ;; Opts may carry namespaced `:workspace/*` overrides for unusual
          ;; per-turn cases; the bare `:workspace` key is not accepted
          ;; (only :workspace/* namespaced keys flow through).
          ;; turn-state-atom already lives on env (one atom for all
          ;; per-turn cursor + id fields); no re-assoc needed.
          workspace-overrides    (select-keys opts [:workspace/root :workspace/id
                                                    :workspace/sandbox? :vcs/kind
                                                    :vcs/ref :vcs/mainline])
          ;; Reseat :router to the preference-hoisted one — run-iteration-phase
          ;; routes off THIS environment's router, not the ctx :router below.
          environment            (cond-> (assoc env :router env-router)
                                   (seq workspace-overrides) (merge workspace-overrides)
                                   ;; Refresh the routing digest HEAD
                                   ;; (:model/:provider) to the per-turn pick so
                                   ;; ctx `routing` + the TUI footer reflect the
                                   ;; session's chosen provider/model. The digest
                                   ;; is built ONCE at env creation from the GLOBAL
                                   ;; router head (the config default), so without
                                   ;; this every turn's `:session/routing` showed
                                   ;; the default provider (e.g. zai) even after
                                   ;; the user switched models — the forced pref
                                   ;; bound the actual call but never the displayed
                                   ;; routing. `:available` is preserved.
                                   (and (seq (:routing env))
                                     (or root-model root-provider))
                                   (update :routing
                                     (fn [r]
                                       (cond-> r
                                         root-model    (assoc :model (str root-model))
                                         root-provider (assoc :provider root-provider)))))
          environment-id         (:environment-id env)]
      {:cancel-token           cancel-token
       :cancel-atom            cancel-atom
       :user-request           user-request
       :router                 env-router
       :root-resolved-model    root-resolved-model
       :root-model             root-model
       :root-provider          root-provider
       :db-info                db-info
       :environment            environment
       :environment-id         environment-id
       :spec                   spec
       :max-context-tokens     max-context-tokens
       :system-prompt          system-prompt
       :debug?                 debug?
       :hooks                  hooks
       :eval-timeout-ms        eval-timeout-ms
       :reasoning-default      reasoning-default
       :routing                routing
       :extra-body             extra-body
       :turn-features          (get opts :turn/features)
       :workspace-overrides    workspace-overrides
       :messages               messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, session-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec
           max-context-tokens system-prompt
           hooks cancel-atom cancel-token
           reasoning-default routing extra-body turn-features workspace-overrides]}]
  (let [iteration-result (run-turn! environment user-request
                           (cond-> {:output-spec            spec
                                    :max-context-tokens     max-context-tokens
                                    :system-prompt          system-prompt
                                    :reasoning-default      reasoning-default
                                    :hooks                  hooks
                                    :cancel-atom            cancel-atom
                                    :cancel-token           cancel-token}
                             routing       (assoc :routing routing)
                             extra-body    (assoc :extra-body extra-body)
                             turn-features (assoc :turn-features turn-features)
                             (seq workspace-overrides) (assoc :workspace-overrides workspace-overrides)))
        session-turn-id         (:session-turn-id iteration-result)
        {iteration-tokens :tokens
         iteration-cost   :cost} iteration-result
        total-tokens-atom (atom (or iteration-tokens {}))
        total-cost-atom   (atom (or iteration-cost {}))
        merge-cost!       (fn [extra-tokens extra-cost]
                            (when extra-tokens
                              (swap! total-tokens-atom
                                (fn [acc]
                                  (merge-with + acc
                                    (select-keys extra-tokens [:input :output :reasoning :cached :total])))))
                            (when extra-cost
                              (swap! total-cost-atom
                                (fn [acc]
                                  (merge-cost-maps acc extra-cost)))))]
    {:iteration-result  iteration-result
     :session-turn-id         session-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom   total-cost-atom
     :merge-cost!       merge-cost!}))

;; -----------------------------------------------------------------------------
;; Finalize turn result
;; -----------------------------------------------------------------------------

(defn- finalize-turn-result
  "Updates DB turn record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model / N
   iteration / duration / tokens / $total` after a restart."
  [{:keys [db-info root-model root-provider]}
   {:keys [session-turn-id start-time iteration-count status status-id trace locals
           answer confidence reasoning utilization total-tokens-atom total-cost-atom]}]
  (let [duration-ms (util/elapsed-since start-time)
        cost-with-model (cond-> @total-cost-atom
                          (and root-model (not (:model @total-cost-atom)))
                          (assoc :model (str root-model))
                          (and root-provider (not (:provider @total-cost-atom)))
                          (assoc :provider root-provider))]
    (if status
      ;; failure path - surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do
        (log-stage! :turn/complete 0
          {:duration-ms duration-ms :iteration-count iteration-count :status status})
        (let [fallback-answer (:result answer answer)]
          (try
            (persistance/db-update-session-turn! db-info session-turn-id
              {:answer          fallback-answer
               :iteration-count iteration-count
               :duration-ms     duration-ms
               :status          status
               :tokens          @total-tokens-atom
               :cost            cost-with-model})
            (catch Exception e
              (tel/log! {:level :warn :data (format-exception-short e)
                         :msg   "Failed to update turn (max iterations)"})))
          (cond-> {:answer          fallback-answer
                   :status          status
                   :status-id       status-id
                   :trace           trace
                   :iteration-count iteration-count
                   :duration-ms     duration-ms
                   :tokens          @total-tokens-atom
                   :cost            cost-with-model}
            (some? locals) (assoc :locals locals))))
      ;; success path
      (do
        (log-stage! :turn/complete 0
          {:duration-ms duration-ms :iteration-count iteration-count
           :cost (str (:total-cost cost-with-model))})
        (try
          (persistance/db-update-session-turn! db-info session-turn-id
            {:answer          answer
             :iteration-count iteration-count
             :duration-ms     duration-ms
             :status          :success
             :tokens          @total-tokens-atom
             :cost            cost-with-model})
          (catch Exception e
            (tel/log! {:level :warn :data (format-exception-short e)
                       :msg   "Failed to update turn (success)"})))
        (cond-> {:answer          answer
                 :trace           trace
                 :iteration-count iteration-count
                 :duration-ms     duration-ms
                 :tokens          @total-tokens-atom
                 :cost            cost-with-model
                 :utilization     utilization}
          (some? confidence) (assoc :confidence confidence)
          (some? reasoning)  (assoc :reasoning reasoning))))))

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
        code evaluation, LLM responses at :info level with :rlm-phase context.
      - :reasoning-default - Optional base reasoning effort for reasoning-capable models.
        Accepts :low/:medium/:high or low/medium/high strings. Adaptive escalation still applies.
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
     - :tokens - Token usage map {:input N :output N :total N}.
     - :cost - Cost map {:input-cost N :output-cost N :total-cost N}.
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages]
   (turn! environment messages {}))
  ([environment messages opts]
   (let [ctx (prepare-turn-context environment messages opts)
         {:keys [eval-timeout-ms
                 debug? user-request root-model
                 db-info
                 environment-id]} ctx]
     (binding [*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                    :rlm-debug? debug? :rlm-phase :turn
                                    :db-info db-info
                                    :session-soul-id (:session-id environment)}
               *eval-timeout-ms*  (clamp-eval-timeout-ms
                                    (or eval-timeout-ms *eval-timeout-ms*))]
       (tel/with-ctx+ {:db-info db-info
                       :session-soul-id (:session-id environment)}
         (log-stage! :turn/open 0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :user-request user-request})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result session-turn-id
                       total-tokens-atom total-cost-atom]} phase2
               {iteration-answer :answer
                trace            :trace
                iteration-count  :iteration-count
                status           :status
                status-id        :status-id
                locals           :locals
                confidence       :confidence
                reasoning        :reasoning} iteration-result
               result
               (if status
                 (finalize-turn-result
                   ctx
                   {:session-turn-id          session-turn-id
                    :start-time        start-time
                    :iteration-count   iteration-count
                    :status            status
                    :status-id         status-id
                    :trace             trace
                    :locals            locals
                    :answer            iteration-answer
                    :total-tokens-atom total-tokens-atom
                    :total-cost-atom   total-cost-atom})
                 (finalize-turn-result
                   ctx
                   {:session-turn-id          session-turn-id
                    :start-time        start-time
                    :iteration-count   iteration-count
                    :trace             trace
                    :answer            iteration-answer
                    :confidence        confidence
                    :reasoning         reasoning
                    :utilization       (:utilization iteration-result)
                    :total-tokens-atom total-tokens-atom
                    :total-cost-atom   total-cost-atom}))]
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
     (let [installed  (vec (or (some-> (:extensions environment) deref) []))
           active-set (set (map :ext/name active-extensions))]
       (doseq [ext installed
               :let [alias (extension/ext-alias-symbol ext)]
               [sym f] (try (extension/wrap-extension ext environment)
                         (catch Throwable _ nil))]
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
           (if (contains? active-set (:ext/name ext))
             (env/set-python-binding! python-context target f)
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
          missing    (vec (remove registered requires))]
      (when (seq missing)
        (anomaly/incorrect!
          (str "Extension '" (:ext/name ext)
            "' requires " missing " but they are not registered. "
            "Register dependencies first.")
          {:type       :extension/missing-dependencies
           :extension  (:ext/name ext)
           :requires   (vec requires)
           :missing    missing
           :registered (vec registered)}))))
  (swap! (:extensions environment)
    (fn [exts]
      (let [ns-sym  (:ext/name ext)
            without (vec (remove #(= (:ext/name %) ns-sym) exts))]
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
  (tel/log! {:level :warn :id ::subloop-lifecycle
             :data {:step step :workspace-id ws-id :error (ex-message t)}}
    (str "sub_loop " (name step) " failed for child workspace " ws-id)))

(defn- guard
  "Functional resource bracket: run `(use resource)` and ALWAYS `(release
   resource)` afterward, returning `use`'s value. A release failure is LOGGED
   (tagged `step`/`ws-id`), never swallowed — teardown problems stay visible
   while the original result (or exception) still propagates."
  [resource release step ws-id use]
  (try
    (use resource)
    (finally
      (try (release resource)
        (catch Throwable t (log-subloop-warn! step t ws-id))))))

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
  "Run the child turn, merge its edits back (rift path), and project the focus
   result the coordinator merges by `task_id`: status (a STRING — python-facing,
   never a keyword), evidence, produced facts, answer, and what changed."
  [child-env {:keys [db-info child-ws rift? subctx prompt system-prompt]}]
  (let [;; A harness AGENT dispatch rides its markdown body in as the child's
        ;; system-prompt addendum (build-system-prompt appends it to CORE);
        ;; ordinary sub_loops pass none.
        turn-opts  (if (seq (str system-prompt))
                     {:system-prompt (str system-prompt)} {})
        ;; Child runs with shell + harness forced ON (see child-forced-toggles).
        ;; sync-active-extension-symbols! (turn start, inside run-turn!) reads
        ;; toggles/enabled? under this binding → the child's sandbox gets the
        ;; shell + skill/agent verbs bound even when they're OFF for the parent.
        result     (binding [toggles/*forced-on*
                             (into toggles/*forced-on* child-forced-toggles)]
                     (run-turn! child-env (str prompt) turn-opts))
        merged     (when rift? (merge-child-edits! db-info child-ws))
        child-ctx  @(:ctx-atom child-env)
        focus      (some-> (:focus subctx) str not-empty)
        focus-task (when focus (get-in child-ctx [:session/tasks focus]))]
    {:task_id       focus
     :status        (status->str (or (:status focus-task) (:status result)))
     :evidence      (:evidence focus-task)
     :facts         (or (:session/facts child-ctx) {})
     :answer        (:answer result)
     :changed_files (vec (:changed merged))}))

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
     {:task_id <focus> :status <string> :evidence :facts :answer :changed_files}
   Throws `:vis/subloop-depth-exceeded` past `MAX-SUBLOOP-DEPTH`."
  [parent-env {:keys [prompt subctx models system-prompt]}]
  (let [depth (inc (or (some-> parent-env :depth-atom deref) 0))]
    (when (> depth MAX-SUBLOOP-DEPTH)
      (throw (ex-info (str "sub_loop depth cap (" MAX-SUBLOOP-DEPTH ") exceeded")
               {:type :vis/subloop-depth-exceeded :depth depth})))
    (let [db-info   (:db-info parent-env)
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
          rerouted  (when (and (seq demoted)
                            (seq (if (coll? models) models (when models [models])))
                            ;; router-for-model hoisted the preferred
                            ;; model's provider to the FRONT pre-demotion;
                            ;; if that very provider got demoted, the
                            ;; explicit preference was dead.
                            (contains? (set demoted) (:id (first (:providers preferred)))))
                      {:from        (vec (if (coll? models) models [models]))
                       :unreachable (mapv name demoted)
                       :used        (:name (resolve-effective-model router))
                       :reason      "preferred model's provider unreachable; auto-routed to the next healthy provider"})
          child-ws  (locking workspace-mutation-lock
                      (child-workspace! db-info parent-ws))
          ;; rift path = the child got its OWN clone (root differs from parent);
          ;; the shared-root fallback writes in place (nothing to merge or trash).
          rift?     (boolean (and (:root child-ws) parent-ws
                               (not= (:root child-ws) (:root parent-ws))))
          ws-id     (:id child-ws)]
      ;; Nested brackets — the CLONE is released LAST (after the env), so the
      ;; order is: merge diff → dispose env → trash clone. `guard` logs any
      ;; teardown failure instead of leaking it.
      (guard child-ws
        (fn [ws] (when rift?
                   (locking workspace-mutation-lock
                     (workspace/abandon! db-info {:workspace-id (:id ws)
                                                  :reason       "subloop complete"}))))
        :abandon ws-id
        (fn [ws]
          (guard (create-environment router
                   {:workspace-id (:id ws)
                    :child {:parent-db-info  db-info
                            :depth           depth
                            ;; link the child soul to THIS parent's session_state
                            ;; (cross-soul) → queryable sub-tree, hidden from the
                            ;; top-level session list, cascades on parent delete.
                            :parent-state-id (:session/state-id parent-env)
                            :seed-ctx        (subctx->seed-ctx subctx)}})
            dispose-environment! :dispose ws-id
            (fn [child-env]
              (cond-> (project-child-result child-env
                        {:db-info db-info :child-ws ws :rift? rift?
                         :subctx subctx :prompt prompt :system-prompt system-prompt})
                ;; surface the health override to the coordinator
                rerouted (assoc :rerouted rerouted)))))))))

(defn- failed-subloop-result
  "The uniform `sub_loop`-result shape for a child that errored (so `parallel`
   slots and `retry` attempts read like a normal result, just with `:error`).
   The throw is surfaced TWO ways — as this `:status \"failed\"` result the
   coordinator sees, AND a logged warning — so the failure is never silent."
  [spec ^Throwable t]
  (let [focus (some-> (:subctx spec) :focus str not-empty)]
    (log-subloop-warn! :run t focus)
    {:task_id       focus
     :status        "failed"
     :error         (ex-message t)
     :facts         {}
     :answer        nil
     :changed_files []}))

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
  (or (some? (:error r))
    (contains? subloop-failure-statuses (status->str (:status r)))))

(defn- run-spec!
  "Run ONE child for `spec` (`{:prompt :subctx :models}`), folding a throw into
   the uniform `failed-subloop-result`. The shared per-child step under every
   composite runner (parallel/sequence/selector) + retry."
  [parent-env spec]
  (try (sub-loop! parent-env {:prompt (:prompt spec)
                              :subctx (:subctx spec)
                              :models (:models spec)})
    (catch Throwable t (failed-subloop-result spec t))))

(defn retry-sub-loop!
  "DECORATOR (not a composite): re-run the SAME `spec` until its child SUCCEEDS,
   up to `n` total attempts (default 2). Returns the first successful result,
   else the last failure — stamped with the `:attempts` made. (Contrast
   `selector-sub-loops!`, which tries DIFFERENT alternatives.)"
  [parent-env spec n]
  (let [attempts (max 1 (long (or n 2)))]
    (loop [i 1]
      (let [r (assoc (run-spec! parent-env spec) :attempts i)]
        (if (or (not (subloop-failed? r)) (>= i attempts))
          r
          (recur (inc i)))))))

(defn sequence-sub-loops!
  "`:sequence` composite — run `specs` IN ORDER, each only after the prior
   SUCCEEDS, SHORT-CIRCUITING on the first failure. Serial by nature (each child
   may depend on the last). Returns the vector of results ACTUALLY RUN, in order:
   all of them when every child succeeded, or up to and INCLUDING the first
   failure when it stopped early (that last result carries the failure). Mirrors
   the BT sequence: all-succeed, fail-fast."
  [parent-env specs]
  (reduce (fn [acc spec]
            (let [r   (run-spec! parent-env spec)
                  acc (conj acc r)]
              (if (subloop-failed? r) (reduced acc) acc)))
    [] (vec specs)))

(defn selector-sub-loops!
  "`:selector` composite (a.k.a. fallback) — try `specs` IN ORDER until one
   child SUCCEEDS, then STOP. Serial. Returns the vector of results tried, in
   order: the failed alternatives followed by the first success (the last
   result), or — if every alternative failed — all of them (all failures).
   Mirrors the BT selector: any-succeed. (Unlike `retry`, the alternatives are
   DIFFERENT specs.)"
  [parent-env specs]
  (reduce (fn [acc spec]
            (let [r   (run-spec! parent-env spec)
                  acc (conj acc r)]
              (if (subloop-failed? r) acc (reduced acc))))
    [] (vec specs)))

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
  (let [specs (vec specs)
        sem   (java.util.concurrent.Semaphore. MAX-PARALLEL-SUBLOOPS)
        futs  (mapv
                (fn [spec]
                  (future
                    (.acquire sem)
                    (try (run-spec! parent-env spec)
                      (finally (.release sem)))))
                specs)]
    (mapv deref futs)))

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
  [router {:keys [db session channel external-id title workspace-id child]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  ;; `child` (a sub_loop child env) carries:
  ;;   :parent-db-info  reuse the parent's DB connection (don't open/close one)
  ;;   :depth           starting recursion depth (parent depth + 1)
  ;;   :seed-ctx        initial ctx-atom value (the model-supplied subctx) — used
  ;;                    instead of a DB restore
  ;; A different `router` (model) can be passed for the child to optimize cost
  ;; (cheap/fast model for an easy subtask) — first-class, nothing special needed.
  (let [depth-atom               (atom (or (:depth child) 0))
        owns-db?                 (nil? (:parent-db-info child))
        db-info                  (or (:parent-db-info child)
                                   (persistance/db-create-connection! db))
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :session-id nil})
        environment-atom         (atom nil)
        environment-id           (str (util/uuid))
        ;; Iteration-final-answer signal. The Python sandbox's `done(
        ;; """...""")` fn `reset!`s this atom with `{:value :form-idx}`;
        ;; the iteration loop reads it back after evaluating each
        ;; iteration's forms and discards iff the form at `:form-idx`
        ;; itself errored (Option C scoping - sibling errors do NOT
        ;; gate the answer). Reset to nil before every iteration runs.
        answer-atom              (atom nil)
        ;; Sticky best-answer: the LATEST non-blank answer any `done(…)` call
        ;; produced this turn, retained ACROSS iterations (NOT reset per-iter,
        ;; unlike answer-atom). When a turn ends without a clean terminal
        ;; answer — user cancel, or a model that kept investigating/retracting
        ;; and never landed an accepted `done()` (GPT one-shot / over-
        ;; investigation pattern) — the turn surfaces this instead of a blank
        ;; answer. Best-effort: a tentative answer beats nothing.
        best-answer-atom         (atom nil)
        ;; SINGLE turn-state atom holds all per-turn cursor fields
        ;; (current-{turn-position,iteration,form-idx,iteration-id,
        ;;  session-turn-id,user-request}-atom). All six fields live
        ;; under map keys with the same names minus `current-` /
        ;; `-atom`. Reads via `ctx-loop/read-turn-state`; writes via
        ;; `ctx-loop/set-turn-state!` / `swap-turn-state!`. Extension
        ;; symbol wrappers close over THIS atom; the loop swap!s it
        ;; between turns and forms.
        turn-state-atom          (ctx-loop/make-turn-state-atom)
        ;; Seed iteration to 1 so early hooks reading the atom before
        ;; the loop's per-turn reset see a sensible value.
        _                        (swap! turn-state-atom assoc :iteration 1)
        ;; Title atom: in-memory cache for the session title.
        ;; The DB column on `session_state` is the persisted
        ;; truth; this atom is the fast read path for  and
        ;; the source for the title hint / channel chrome at iteration
        ;; boundaries. `set-title!` writes both, in that order, then
        ;; broadcasts to every registered listener.
        session-title-atom               (atom (or title ""))
        root-resolved-model      (resolve-effective-model router)
        root-model               (or (:name root-resolved-model) "unknown")
        root-provider            (:provider root-resolved-model)
        ;; Routing digest surfaced in the model-facing ctx (`routing`) so
        ;; the agent can SEE its current model + what's available, and pick a
        ;; cheaper/faster one for an easy `sub_loop` (the `model` arg). Read-only;
        ;; the agent never reconfigures routing, it just routes children by cost.
        routing-digest           (cond-> {:model root-model}
                                   root-provider (assoc :provider root-provider)
                                   (seq (:providers router))
                                   (assoc :available
                                     (mapv (fn [p] {:provider (:id p)
                                                    :models   (mapv :name (:models p))})
                                       (:providers router))))
        ;; Snapshot a base system prompt for the session row so the
        ;; sidebar / DB inspectors have something stable to display.
        ;; Real per-turn assembly goes through `prompt/assemble-stable-prompt-messages`
        ;; with `:active-extensions`, so this snapshot is just metadata.
        system-prompt            (prompt/build-system-prompt {})
        resolved-session-id (persistance/db-resolve-session-id db-info session)
        ;; Workspace pin (1:1 with session_state):
        ;;   - resuming a session       → derive workspace from its latest state
        ;;   - brand-new session        → mint a trunk workspace, pass its id
        ;;                                into db-store-session! below
        ;; db-info nil (sandbox-only mode) → skip; iteration loop never asserts
        ;;                                workspace pin when there's no DB
        active-workspace    (when db-info
                              (cond
                                ;; Resume path: the existing session already pins a
                                ;; workspace; honour it.
                                resolved-session-id
                                (some->> (persistance/db-latest-session-state-id db-info resolved-session-id)
                                  (persistance/db-workspace-for-session db-info))
                                ;; New session, caller pre-spawned a workspace
                                ;; (e.g. /workspace slash spawn-branch path).
                                workspace-id
                                (persistance/db-workspace-get db-info workspace-id)
                                ;; New session, no pre-spawn: clone cwd.
                                :else
                                (workspace/ensure-workspace! db-info {})))
        session-id          (or resolved-session-id
                              (persistance/db-store-session! db-info
                                (cond-> {:channel       (or channel :tui)
                                         :external-id   external-id
                                         :model         root-model
                                         :title         title
                                         :system-prompt system-prompt
                                         :workspace-id  (:id active-workspace)
                                         ;; sub_loop child → link this whole soul
                                         ;; to the parent's session_state (cross-
                                         ;; soul), keeping it out of the top-level
                                         ;; list; nil for a normal session.
                                         :parent-state-id (:parent-state-id child)}
                                  root-provider (assoc :provider root-provider))))
        ;; Resolve the session_state row id ONCE here (reliable at env build)
        ;; and stamp it on the env, so slashes/turns don't re-query it — the
        ;; per-call re-query intermittently returns nil for fresh sessions,
        ;; which broke `/draft new`'s pin ("session not ready").
        session-state-id    (when (and db-info session-id)
                              (persistance/db-latest-session-state-id db-info session-id))
        ;; CTX engine wiring (see ctx-loop). ONE atom carries the entire
        ;; engine state for the session: specs/tasks/facts/trailer +
        ;; ephemeral `:engine/warnings` + `:engine/pending-satisfies`.
        ;; Seeded fresh; reloaded from session_turn_state.ctx (Nippy BLOB)
        ;; on session resume so the model picks up where the last
        ;; done(…) left off. Defined BEFORE answer-fn because answer-fn
        ;; closes over it to apply the engine swap at turn close.
        ctx-atom                 (ctx-loop/make-ctx-atom session-id)
        ;; Sandbox binding for `done("""...""")` - the canonical turn-
        ;; termination call. Closes over `answer-atom` AND
        ;; turn-state-atom's :form-idx so the iteration loop can scope
        ;; the discard check to the form that actually called this.
        ;; Returns the marker keyword so the per-form result row makes
        ;; request visible.
        answer-fn                (fn done [s]
                                   ;; Canonical final-answer shape:
                                   ;;   done("""markdown string""")
                                   ;;
                                   ;; ONE positional Markdown string — this is
                                   ;; what the prompt advertises and what GPT-
                                   ;; class models reliably emit. The Markdown
                                   ;; string IS the answer source of truth;
                                   ;; channels derive IR via
                                   ;; `render/markdown->ir` when they need
                                   ;; layout.
                                   ;;
                                   ;; The map form `done("""…""")`
                                   ;; is also accepted (the needs-input map +
                                   ;; optional `:turn-summary` metadata), but is
                                   ;; not advertised.
                                   ;;
                                   ;; Needs-input maps stay data-shaped so
                                   ;; the prompt-flow gate reads them as
                                   ;; maps via `needs-input-answer?` /
                                   ;; `:answer/text`.
                                   ;;
                                   ;; Everything else is a programmer/model
                                   ;; error: we wrap it in a synthetic
                                   ;; Markdown answer so the loop can still
                                   ;; surface a user-visible message, and
                                   ;; the answer-validation gate may reject
                                   ;; it downstream.
                                   ;; Flat: classify value — extract trailer
                                   ;; directives — hand off to ctx-loop —
                                   ;; stamp answer. No nested when/let chains
                                   ;; juggling ctx-atom; that lives behind
                                   ;; `ctx-loop/apply-done!`.
                                   ;;
                                   ;; FORCING done-gate — checked HERE because
                                   ;; `apply-done!` (below) finalizes DURING eval, so
                                   ;; a run-iteration post-processing gate is too late.
                                   ;; While the model's plan still has OPEN steps:
                                   ;; skip finalize, leave `answer-atom` UNSET (the
                                   ;; turn does not finalize, the loop continues), and
                                   ;; RETURN the reason as done()'s result so the model
                                   ;; reads it and retries after resolving the steps.
                                   (if-let [done-block-msg (open-plan-steps-block
                                                             (some-> ctx-atom deref :session/tasks))]
                                     done-block-msg
                                     (let [value             (cond
                                                               (needs-input-answer? s) s
                                                               (markdown-answer? s)    s
                                                               (string? s)             {:answer s}
                                                               (nil? s)                {:answer ""}
                                                               :else                   {:answer (pr-str s)
                                                                                        :vis/coerced? true})
                                         ;; Phase F (redesigned): extract :answer + free-form
                                         ;; :turn-summary so the engine can write the
                                         ;; `:turn-N-answer` fact under :session/facts. The fact
                                         ;; carries the question + the FULL answer markdown verbatim
                                         ;; under :content (head+tail-clipped in-prompt, recallable in
                                         ;; full) + entity ids born/done this turn. Next turn's ;; ctx
                                         ;; EDN block surfaces this fact inside the cached prefix —
                                         ;; carries previous-turn context inside the cached
                                         ;; prefix instead of a separate user-message rebuild.
                                           answer-text       (cond
                                                               (and (map? value) (string? (:answer value))) (:answer value)
                                                               (and (map? value) (string? (:answer/text value))) (:answer/text value)
                                                               (string? value) value
                                                               :else nil)
                                           turn-summary      (when (map? value) (:turn-summary value))
                                         ;; Question = the user request that opened this turn. Pulled
                                         ;; from the live turn-state so the fact carries the prompt
                                         ;; verbatim for cross-turn lookup.
                                           user-request      (some-> turn-state-atom deref :user-request)
                                           done-env          {:ctx-atom        ctx-atom
                                                              :turn-state-atom turn-state-atom}
                                         ;; Phase D: title-gate. Read live title from the closed-over
                                         ;; session-title-atom (defined in open-env!) so the gate sees
                                         ;; the current value even mid-iter (e.g. model emitted
                                         ;; set_session_title earlier in the same fence).
                                           current-title   (some-> session-title-atom deref str str/trim not-empty)
                                           done-ret          (ctx-loop/apply-done! done-env
                                                               {:answer         answer-text
                                                                :turn-summary   turn-summary
                                                                :user-request   user-request
                                                                :session-title  current-title})]
                                       (reset! answer-atom
                                         {:value    value
                                          :position (:form-idx @turn-state-atom)})
                                     ;; Sticky best-answer: retain the latest
                                     ;; non-blank candidate across iterations so
                                     ;; a cancelled/never-finalized turn can
                                     ;; surface it instead of a blank answer.
                                     ;; Survives downstream gating/retraction —
                                     ;; captured at the call site.
                                       (when-not (str/blank? (str answer-text))
                                         (reset! best-answer-atom
                                           {:value           value
                                            :answer-markdown answer-text}))
                                     ;; Canonical answer sentinel is the Python-native
                                     ;; string: `done` is reached only as a Python
                                     ;; callable, and a keyword return would snake to
                                     ;; this same string crossing `->py` anyway.
                                       "vis_answer")))
        ;; The session title is fully HOST-OWNED (loop/maybe-auto-title!
        ;; generates it in the background and writes it via
        ;; `set-title-with-broadcast!`). There is NO model-facing
        ;; `set_session_title` tool — the agent neither sets nor reads the
        ;; title; it just appears in channel chrome.
        ;; Build the ctx-loop env subset used by the engine bindings + helpers.
        ;; Just the cursor counters + the single ctx-atom. Warnings
        ;; live as `:engine/warnings` on the ctx itself, no side atoms.
        ;; (D12 retired `:engine/pending-satisfies` along with
        ;; satisfy-hint!; hook-task satisfaction is plain `plan_step`.)
        ctx-loop-env             {:ctx-atom        ctx-atom
                                  :turn-state-atom turn-state-atom
                                  ;; DB + session id ride on the same env
                                  ;; map so `build-introspect-bindings`
                                  ;; can hit `session_turn_iteration.forms`
                                  ;; for the per-form / per-iter / per-turn
                                  ;; introspection verbs without an extra
                                  ;; closure capture.
                                  :db-info         db-info
                                  :session-id      session-id}
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
        env-bindings             (merge
                                   ;; BUILT-IN extension kernel (`foundation`):
                                   ;; cat/ls/rg/patch/… interned BARE into the
                                   ;; sandbox ns next to the engine verbs — no
                                   ;; `v/` alias. env resolved lazily (atom not
                                   ;; built yet). Listed FIRST so engine verbs
                                   ;; below win any accidental name collision.
                                   (extension/builtin-sandbox-bindings
                                     (fn [] @environment-atom))
                                   {'done            answer-fn}
                                   ;; sub_loop(prompt, subctx, {"model": …}) — dispatch a
                                   ;; CHILD agent on a focused subctx, optionally on a
                                   ;; cheaper proposed model. Resolves the PARENT env at
                                   ;; call time (environment-atom) so the child's depth /
                                   ;; db / router / workspace come from THIS session.
                                   {'sub-loop (fn sub-loop [prompt subctx & more]
                                                ;; "models" is ALWAYS a list (ordered preference,
                                                ;; even for one: ["haiku"]) — ONE consistent surface,
                                                ;; never a scalar. svar routes + falls back the order.
                                                ;; The opts dict crosses the GraalPy boundary via
                                                ;; `env-python/->clj`, which KEYWORDIZES every dict key
                                                ;; (snake verbatim) — so the key is `:models`, NOT the
                                                ;; string "models" (reading the string silently yields
                                                ;; nil → child runs on the DEFAULT model, not the
                                                ;; proposed one).
                                                (sub-loop! @environment-atom
                                                  {:prompt prompt
                                                   :subctx subctx
                                                   :models (:models (first more))}))
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
                                             (retry-sub-loop! @environment-atom spec
                                               (first more)))}
                                   ;; Canonical stateful-resource lifecycle:
                                   ;; `resource_stop(id)` / `resource_restart(id)`
                                   ;; (B-dispatch — act by id; ctx advertises
                                   ;; can_stop/can_restart). Session-scoped so the
                                   ;; agent only touches THIS session's resources.
                                   (resources/sandbox-bindings session-id)
                                   ;; build-engine-bindings contributes every
                                   ;; engine mutator (task/fact + contradicts).
                                   (ctx-loop/build-engine-bindings ctx-loop-env)
                                   (ctx-loop/build-introspect-bindings
                                     ctx-loop-env nil))
        ;; Engine substrate: embedded GraalPy (env/create-python-context builds a
        ;; deny-by-default polyglot Context, wires the Clojure tools as Python
        ;; callables, and installs doc/apropos introspection).
        {:keys [python-context sandbox-ns initial-ns-keys]}
        (env/create-python-context (merge env-bindings (:custom-bindings @state-atom)))
        env (cond-> {:environment-id                    environment-id
                     :session-id                   session-id
                     :session/state-id                  session-state-id
                     :channel                           (or channel :tui)
                     :depth-atom                        depth-atom
                     ;; false for a sub_loop child reusing the parent's connection
                     ;; — dispose-environment! must NOT close a borrowed DB.
                     :owns-db?                          owns-db?
                     ;; routing digest → rendered into ctx as `routing`
                     ;; (current model + available, for sub_loop model choice).
                     :routing                           routing-digest
                     :db-info                           db-info}
              ;; Workspace info attached at env-build time so the extension
              ;; wrapper's `(workspace/workspace-root env)` finds a non-blank
              ;; root the very first time it fires.
              active-workspace
              (assoc :workspace          active-workspace
                :workspace/id       (:id active-workspace)
                :workspace/root     (:root active-workspace)
                :workspace/context-roots (vec (:context-roots active-workspace))

                ;; Every workspace is a rift CoW clone — always a sandbox.
                ;; Reported on :workspace/sandbox?, NOT as a VCS. The
                ;; model-facing :vcs/kind is the real repo VCS, computed in
                ;; foundation.workspace-ctx/render-block.
                :workspace/sandbox? true))
        env (assoc env
              ;; CTX engine atoms — visible to the rest of the loop so the
              ;; renderer / per-iter capture / done snapshot can read and
              ;; mutate them. Mutator-time writes happen via sandbox bindings
              ;; built above; render-time reads via ctx-loop/current-ctx.
              :ctx-atom                          ctx-atom
              :turn-state-atom                   turn-state-atom
              ;; FORCING plan-gate (proposal Decision 2 / G1): a POLICY CALLBACK
              ;; the foundation editing layer consults before a content mutation
              ;; (write/patch). Keeps that layer engine-agnostic — the ctx-engine
              ;; decision + audit-fact recording live here. Returns a refusal
              ;; STRING to block, or nil to allow (recording intent + the
              ;; `atomic`-override audit fact on the allow path).
              :mutation-gate
              (fn mutation-gate [{:keys [op paths atomic?]}]
                (let [tasks     (some-> ctx-atom deref :session/tasks)
                      already   (or (:files-mutated @turn-state-atom) #{})
                      approved? (ctx-engine/approved-plan? tasks)
                      refusal   (ctx-engine/plan-gate-block already paths approved? atomic?)]
                  (if refusal
                    refusal
                    (do
                      ;; record intent-to-mutate so the NEXT distinct file arms the gate
                      (swap! turn-state-atom update :files-mutated
                        (fnil into #{}) (remove nil? paths))
                      ;; audited escape: leave a fact trail ONLY when atomic actually
                      ;; bypassed a block (2nd file, no plan) — visible to user + parent.
                      (when (and atomic?
                              (ctx-engine/plan-gate-block already paths approved? false))
                        (ctx-loop/apply-and-record! @environment-atom :fact-set!
                          [(str "atomic_override_"
                             (str/replace (str (first (sort (set (remove nil? paths)))))
                               #"[^a-zA-Z0-9]+" "_"))
                           {:content (str (name op) " atomic-override: edited "
                                       (str/join ", " (sort (set (remove nil? paths))))
                                       " as one indivisible change with no plan")
                            :status :active}]))
                      nil))))
              :state-atom                        state-atom
              :python-context                           python-context
              :sandbox-ns                        sandbox-ns
              :initial-ns-keys                   initial-ns-keys
             ;; Long-lived per-env LRU map: `{var-name-string →
             ;; last-used-turn-pos}`. Merged from each iteration's
             ;; `:lru` after eval. Drives the trailer's live-vars
             ;; surface, which ages user vars out of the discovery
             ;; line after quiet turns.
              :def-resolve-lru-atom              (atom {})
              :router                            router
              :answer-atom                       answer-atom
              :best-answer-atom                  best-answer-atom
              :session-title-atom           session-title-atom
              :extensions                        (atom [])
              :active-extensions                 (atom []))]
    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :session-id session-id)
    ;; A sub_loop CHILD seeds its in-memory ctx straight from the model-supplied
    ;; subctx (its focused bigger-picture slice) — no DB restore.
    (when-let [seed (:seed-ctx child)]
      (reset! ctx-atom (assoc seed
                         :session/id session-id
                         :engine/warnings          []
                         :engine/pending-satisfies [])))
    ;; Restore the CTX engine state when resuming. Sandbox defs do NOT
    ;; persist across turns (the `definition_*` sidecar
    ;; tables were dropped); cross-turn memory rides on the
    ;; per-turn CTX snapshot.
    (when (and resolved-session-id (nil? (:seed-ctx child)))
      ;; The latest
      ;; session_turn_state.ctx (Nippy BLOB) carries specs/tasks/facts/trailer
      ;; from the last done(…). Cursor is iter-local so we don't restore it;
      ;; the renderer stamps a fresh one from the loop counters.
      (try
        (when-let [persisted-ctx (persistance/db-load-latest-ctx db-info session-id)]
          ;; Persisted snapshot has no `:engine/*` ephemeral keys (stripped
          ;; before Nippy). Re-seed them empty so swap! callers don't need
          ;; nil-guards.
          ;;
          ;; Tasks/facts/archived are AUTHORITATIVE in the dedicated tables
          ;; (write-through, atomic with the blob, keyed by session_state).
          ;; Rehydrate the in-memory snapshot FROM THE TABLES; empty stores
          ;; keep the blob's copy — by construction identical (same tx), so
          ;; this is a no-op equivalence, not a legacy fallback. The blob
          ;; still carries trailer/scope; the live render stays in-memory
          ;; (zero per-iter DB) — the DB is read only here, once, on resume.
          (let [ss-id (persistance/db-latest-session-state-id db-info session-id)
                t-rows (when ss-id (persistance/db-list-tasks   db-info ss-id))
                f-rows (when ss-id (persistance/db-list-facts   db-info ss-id))
                a-rows (when ss-id (persistance/db-list-archive db-info ss-id))]
            (reset! ctx-atom
              (cond-> (assoc persisted-ctx
                        :session/id session-id
                        :engine/warnings          []
                        :engine/pending-satisfies [])
                (seq t-rows) (assoc :session/tasks t-rows)
                (seq f-rows) (assoc :session/facts f-rows)
                (seq a-rows) (assoc :session/archived a-rows)))))
        (catch Throwable t
          (tel/log! {:level :warn :id ::restore-ctx-failed
                     :data {:error (ex-message t) :session-id session-id}
                     :msg "Failed to restore CTX engine state from DB - starting empty"}))))
    ;; Auto-discover everything from `META-INF/vis-extension/vis.edn` on the
    ;; classpath, then install extensions in dependency order. The
    ;; same loader populates channel/command/provider/persistance
    ;; registries as a side effect; we just care about the extension
    ;; rows here.
    (extension/discover-extensions!)
    (extension/register-extensions! env install-extension!)
    env))

;; =============================================================================
;; Session env cache
;; =============================================================================

;; ---------------------------------------------------------------------------
;; In-process session cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce
  ^{:doc "In-process env cache.

   Keyed by `java.util.UUID` session-soul-id. Under the 1:1 session ↔
   workspace invariant this key is isomorphic to `(:workspace/id env)`
   — one cache entry = one session = one workspace = one Python sandbox
   lineage. Lookups normalize incoming strings to UUID via `cache-key`
   so string-id callers keep working alongside the UUID key."}
  cache (atom {}))

(defn- cache-key
  "Normalize an id-shaped value (UUID or string-UUID) to a UUID
   suitable for keying `cache`. Nil → nil so wrapped lookups stay
   honest."
  [id]
  (persistance/->uuid id))

(defn cache-env!
  "Insert `env` into the cache under `session-id` (UUID, or string
   normalized via `cache-key`). Returns `{:id <UUID> :environment env}`."
  [session-id env]
  (let [k (cache-key session-id)]
    (swap! cache assoc k {:environment env
                          :lock (java.util.concurrent.locks.ReentrantLock.)})
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
    (swap! cache
      (fn [m]
        (reduce-kv
          (fn [acc id {:keys [environment] :as entry}]
            (assoc acc id
              (assoc entry :environment (assoc environment :router router))))
          {} m))))
  nil)

(defn set-provider!
  "Set the single active provider config. Persists to disk, updates
   in-memory state, rebuilds the global router, and reseats cached
   session envs. `provider` is a svar-native provider map
   `{:id :base-url :api-key :models [...]}`. Replaces an existing
   provider with the same `:id` or appends a new entry."
  [provider]
  (let [cfg     (or (config/current-config) {:providers []})
        pid     (:id provider)
        provs   (vec (:providers cfg))
        idx     (some (fn [[i p]] (when (= (:id p) pid) i))
                  (map-indexed vector provs))
        updated (if idx (assoc provs idx provider) (conj provs provider))
        prioritized (vec (cons provider (remove #(= (:id %) pid) updated)))
        new-cfg {:providers prioritized}]
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
  [id {:keys [channel external-id title workspace-id]}]
  (let [router (get-router)
        env    (create-environment router
                 (cond-> {:db (config/resolve-db-spec)}
                   id          (assoc :session id)
                   channel     (assoc :channel channel)
                   external-id (assoc :external-id external-id)
                   title       (assoc :title title)
                   workspace-id (assoc :workspace-id workspace-id)))]
    env))

(defn- ensure-env!
  [id]
  (let [k (cache-key id)]
    (if-let [entry (get @cache k)]
      entry
      (let [env (open-env! k {})]
        (swap! cache
          (fn [m]
            (if (contains? m k)
              m
              (assoc m k {:environment env
                          :lock (java.util.concurrent.locks.ReentrantLock.)}))))
        (get @cache k)))))

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
  ([channel {:keys [title external-id workspace-id]}]
   (let [env  (open-env! nil (cond-> {:channel     channel
                                      :external-id (some-> external-id str)
                                      :title       title}
                               workspace-id (assoc :workspace-id workspace-id)))
         id   (:session-id env)
         _    (cache-env! id env)]
     {:id           id                ; UUID
      :channel      channel
      :external-id  (some-> external-id str)
      :title        title
      :workspace-id (:workspace/id env)})))

(defn by-id
  "Return the session record (UUID `:id`) or nil."
  [id]
  (when-let [session (persistance/db-get-session (db-info) id)]
    {:id            (:id session)       ; UUID
     :channel       (:channel session)
     :external-id   (:external-id session)
     :system-prompt (:system-prompt session)
     :model         (:model session)
     :title         (:title session)
     :created-at    (:created-at session)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id          (:id c)         ; UUID
           :channel     (:channel c)
           :external-id (:external-id c)
           :title       (:title c)
           :created-at  (:created-at c)})
    (persistance/db-list-sessions (db-info) channel)))

(defn for-telegram-chat!
  [chat-id]
  (let [ext (str chat-id)]
    (or (when-let [id (persistance/db-find-session-by-external (db-info) :telegram ext)]
          (by-id id))
      (create! :telegram {:external-id ext}))))

;; =============================================================================
;; Host title setter + public env accessor
;; =============================================================================

(defn env-for
  [id]
  (:environment (ensure-env! id)))

(defn set-title!
  "Host-driven title change. Resolves the live env (if any) so the
   in-memory atom + listener fan-out stay in sync; falls back to a
   plain DB write when no env is live for this session (e.g.
   `vis sessions` rename ops)."
  [id title]
  (let [env (env-for id)]
    (set-title-with-broadcast! (or (:db-info env) (db-info))
      id
      (:session-title-atom env)
      title))
  nil)

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [^java.util.concurrent.locks.ReentrantLock lock] :as entry}
         (ensure-env! id)
         message-vec (if (string? messages) [(svar/user messages)] messages)]
     ;; ReentrantLock keeps one turn per session. Extension reload marks
     ;; envs dirty; actual sandbox reset happens here, after prior IR/render is
     ;; finished and before the next user code executes.
     (.lock lock)
     (try
       (turn! (:environment entry) message-vec opts)
       (finally (.unlock lock))))))

(defn close!
  [id]
  (let [k (cache-key id)]
    (when-let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
               (clojure.core/get @cache k)]
      (.lock lock)
      (try
        (try (dispose-environment! environment) (catch Exception _ nil))
        (finally (.unlock lock))))
    (swap! cache dissoc k)))

(defn delete!
  [id]
  (close! id)
  (let [d (db-info)]
    (try (persistance/db-delete-session-tree! d id)
      (catch Exception _ nil))))

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
       (try
         (persistance/db-update-session-turn! db id
           {:answer          ORPHAN_INTERRUPTED_ANSWER
            :iteration-count (or iteration-count 0)
            :duration-ms     (or duration-ms 0)
            :status          :interrupted
            :prior-outcome   :cancelled})
         (catch Exception _ nil)))
     (count orphans))))

(defn close-all!
  []
  (doseq [[_ {:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}] @cache]
    (.lock lock)
    (try
      (try (dispose-environment! environment) (catch Exception _ nil))
      (finally (.unlock lock))))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
