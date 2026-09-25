(ns com.blockether.vis.internal.loop.iteration
  "The iteration engine of a turn.

   `run-iteration` sends one provider request and runs the code blocks it
   returns. `iteration-loop` repeats that until the model answers, recovering
   from empty replies, stream failures, exhausted output budgets, rejected
   credentials and context overflow within their retry budgets."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.internal.attachment.audio-transcribe :as audio-transcribe]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.attachment.linked-reports :as linked-reports]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.attachment.vision-describe :as vision-describe]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.channel.render :as render]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.context.renderer :as ctx-renderer]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop.compaction :as compaction]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.errors :as loop-errors]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.internal.session.model :as session-model]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.view.core :as view]
            [taoensso.nippy :as nippy]
            [taoensso.telemere :as tel]))

(def ^:private CONSECUTIVE_EMPTY_REPLY_LIMIT
  "Maximum consecutive clean-stop replies with no text or tool call. Svar treats
   those as legitimate completions, so Vis may continue a thinking-only blip, but
   caps the sequence to avoid consuming the full iteration budget without output."
  3)

(def ^:private MAX_STREAM_RECOVERY_RETRIES
  "Bound same-iteration recovery for pre-output watchdogs and reasoning-only EOF.
   Reasoning retries may incur provider usage; announce every retry and never replay tools."
  2)

(def ^:private STREAM_RECOVERY_RETRY_DELAYS_MS
  "Brief backoff before each same-iteration stream recovery attempt."
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
       (< (long (or attempt 0)) (long MAX_STREAM_RECOVERY_RETRIES))
       (boolean (some perr/pre-output-stream-abort? (loop-errors/bounded-cause-chain e)))))

(defn- reasoning-only-stream-retryable?
  "Only the provider-call boundary can verify reasoning-only EOF before code eval.
   Do not infer replay safety from missing content or from a stream error thrown later."
  [^Throwable e attempt]
  (and (perr/stream-truncated-error? e)
       (= :reasoning (:stream-output (ex-data e)))
       (< (long attempt) (long MAX_STREAM_RECOVERY_RETRIES))))

(defn- stream-recovery-backoff-ms
  "Backoff in ms before recovery number `attempt` (0-based), clamped to the last step."
  ^long [attempt]
  (long (nth STREAM_RECOVERY_RETRY_DELAYS_MS
             (min (long attempt) (dec (count STREAM_RECOVERY_RETRY_DELAYS_MS))))))

(def ^:private RETRY_BUDGET_KINDS
  "The per-iteration budget each Vis-owned retry sentinel spends. Auth, stream and
   max-token recovery count separately, so one kind of retry never spends another
   kind's budget. Auth fallback and context-overflow recovery own their bounds."
  {::retry-auth-refresh :auth
   ::retry-auth-backoff :auth
   ::retry-stream-recovery :stream
   ::retry-max-tokens :max-tokens
   ::retry-auth-fallback nil
   ::retry-context-overflow nil})

(defn- retry-sentinel
  "The retry sentinel `result` carries, or nil for a real result. Map sentinels
   also carry the retry's input: the bumped extra body or the fallback routing."
  [result]
  (cond (keyword? result) (when (contains? RETRY_BUDGET_KINDS result) result)
        (map? result) (some #(when (contains? result %) %)
                            [::retry-max-tokens ::retry-auth-fallback])))

(defn- next-retry-counters
  "Pure counter-threading for Vis-owned context, max-token, auth and stream recovery.
   Svar owns other transport retries. Vis additionally recovers pre-output watchdogs
   and verified reasoning-only EOF before code eval. `counters` holds one count per
   budget kind in [[RETRY_BUDGET_KINDS]]. Returns nil for a real result."
  [result counters]
  (when-let [sentinel (retry-sentinel result)]
    (if-let [kind (get RETRY_BUDGET_KINDS sentinel)]
      (update counters kind (fnil inc 0))
      counters)))

(defn- provider-retry-event
  [{:keys [provider model reason attempt delay-ms error status]}]
  (cond-> {:event/type :llm.routing/provider-retry
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
  (let [delay-ms
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
        (cond-> (select-keys (loop-errors/format-exception-short t)
                             [:type :message :status :cause-class])
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
  (let [event
        (cond-> {:event/type :llm.routing/provider-retry
                 :reason :empty-content
                 :attempt attempt
                 :max-resends max-resends
                 :delay-ms delay-ms}
          (:provider resolved-model)
          (assoc :from-provider (name (:provider resolved-model)))

          (:name resolved-model)
          (assoc :from-model (str (:name resolved-model))))

        error
        (cond-> {:type :svar.llm/empty-content
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
   Anthropic safety classifier DECLINED, so svar switched to a SIBLING MODEL of the
   same provider (e.g. Opus 5 -> Opus 4.8). Same chunk shape as a transport rewind
   (`:provider-retry-reset`) so every channel already knows how to draw it and the
   gateway persists it the instant it happens — the UI shows the switch instead of a
   silent multi-second gap.

   The TRACE event is a `:llm.routing/model-fallback`, never a provider retry: the
   credential, the provider and the wire all worked, and counting a content decision
   as a retry reports a healthy provider as flaky. The prompt cache is gone all the
   same — an Anthropic cache belongs to ONE model — which is what the model-scoped
   type tells the turn note."
  [iteration-position {:keys [from-model to-model category explanation attempt]}]
  (let [event
        (cond-> {:event/type :llm.routing/model-fallback :reason :refusal :attempt attempt}
          from-model
          (assoc :from-model (str from-model))

          to-model
          (assoc :to-model (str to-model))

          category
          (assoc :category (str category)))

        error
        (cond-> {:type :svar.llm/refusal
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

   Routine stage breadcrumbs are debug-only to keep the process log cheap. Actual
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

;; Parsed form helpers

;; Replay-dedup keys hash via `util/sha256-hex` — the ONE
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

   Every provider call becomes one entry, including blank calls (preflight errors).
   Identical programs under distinct calls are distinct calls; none are merged."
  [_iteration-position blocks]
  (let
    [blocks
     (vec (or blocks []))

     source-blocks
     (vec (remove #(and (str/blank? (:source %)) (not (:svar/tool-call-id %))) blocks))

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
                 (str/blank? src)
                 (assoc :vis/preflight-error "python_execution requires non-blank code.")

                 ;; Carry the originating tool-call identity onto the
                 ;; entry so it survives into the executed form / envelope
                 ;; and `iteration-results-message` can pair EACH tool_use
                 ;; with its OWN tool_result.
                 (:svar/tool-call-id b)
                 (assoc :svar/tool-call-id (:svar/tool-call-id b))

                 (:vis/tool-name b)
                 (assoc :vis/tool-name (:vis/tool-name b)))))
           source-blocks)

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
     (when-not (str/blank? normalized-code) (util/sha256-hex normalized-code))

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
             :data {:ext (:ext/name ext) :hook id :phase :turn.answer/validate :returned hit}})
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
     (or (council/reply-error environment)
         (some (fn [ext]
                 (some (fn [{:keys [id phase] hook-fn :fn :as hook}]
                         (when (= :turn.answer/validate phase)
                           (extension/with-context
                             {:ext ext :env environment}
                             (try (let [hit (hook-fn ctx)]
                                    (cond (extension/answer-validation-reject? hit)
                                          (answer-validation-rejection-message hook hit)
                                          (and (map? hit) (:reject hit))
                                          (answer-validation-invalid-return-message ext id hit)))
                                  (catch Throwable t
                                    (answer-validation-hook-error-message ext id t))))))
                       (or (:ext/hooks ext) [])))
               (answer-validation-extensions environment active-extensions))))))

(defn- finalize-answer!
  "Finalize the turn from an accepted terminal ANSWER reply (`s` = the markdown). Classifies
   the value, runs `ctx-loop/finalize-turn!` (the real turn/context finalization),
   and sets turn-state `:answer` so run-iteration's FINAL path stores + renders it.
   Reads the per-turn atoms off `environment` — the answer is the answer; we
   just record it and finalize."
  [environment s]
  (let [turn-state-atom
        (:turn-state-atom environment)

        value
        (cond (transcript/needs-input-answer? s) s
              (transcript/markdown-answer? s) s
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

(defn run-iteration
  "Runs a single RLM iteration: ask! -> check final -> execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc."
  [environment messages &
   [{:keys [routing iteration reasoning-level reasoning-effort resolved-model on-chunk extra-body
            llm-headers active-extensions answer-validation-context request-context on-response
            message-token-counter input-token-estimator]}]]
  (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :run-iteration})]
    (let [iteration-position (inc (long (or iteration 0)))
          turn-prefix (transcript/runtime-turn-prefix environment)
          turn-position (or (:turn-position (ctx-loop/read-turn-state environment)) 1)
          request-context (transcript/request-log-context environment iteration request-context)
          form-scope (fn [idx]
                       (str "t" turn-position "/i" iteration-position "/f" (inc (long idx))))
          effective-reasoning (when (and (nil? reasoning-effort)
                                         (some? reasoning-level)
                                         (transcript/reasoning-effort-configurable? resolved-model))
                                (or (loop-router/normalize-reasoning-level reasoning-level)
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
          ;; Stream chunks by phase. Reasoning carries both the corrected cumulative text
          ;; and an append-only delta; if a provider rewrites its cumulative text, emit no delta.
          reasoning-prev-volatile (volatile! "")
          content-prev-volatile (volatile! "")
          reset-stream-state! (fn []
                                (vreset! reasoning-prev-volatile "")
                                (vreset! content-prev-volatile ""))
          cumulative-delta! (fn [prev-volatile s]
                              (let [prev (str @prev-volatile)]
                                (vreset! prev-volatile (or s ""))
                                (cond (nil? s) nil
                                      (= prev s) ""
                                      (str/starts-with? s prev) (subs s (count prev))
                                      :else "")))
          streaming-fn
          (when on-chunk
            (fn [{:keys [reasoning content done?] :as chunk}]
              ;; svar speaks two kinds of notice on this stream. Routing events ARE
              ;; the provider swap `:provider-fallback` names. Session events are the
              ;; Codex socket's own lifecycle: a restart replays the turn on a fresh
              ;; socket, so text already painted arrives again - rewind the live
              ;; attempt exactly like a provider stream retry and restart the delta
              ;; bookkeeping, or an append-only consumer prints the replayed stream
              ;; twice. A rate-limit snapshot is telemetry no phase draws; forwarding
              ;; it as a fallback wrote swaps that never happened into the recap.
              (cond (= :llm.session/stream-restarted (:event/type chunk))
                    (do (reset-stream-state!)
                        (on-chunk {:phase :provider-retry-reset
                                   :iteration iteration-position
                                   :attempt (:attempt chunk)
                                   :max-retries (:max-retries chunk)
                                   :error {:type :llm.session/stream-restarted
                                           :message (str "Session stream restarted"
                                                         (when-let [reason (:reason chunk)]
                                                           (str " (" (name reason) ")")))}
                                   :event chunk}))
                    (some-> (:event/type chunk)
                            namespace
                            (= "llm.session"))
                    nil
                    (= :llm.routing/provider-retry (:event/type chunk))
                    (do (reset-stream-state!)
                        (on-chunk {:phase :provider-retry-reset
                                   :iteration iteration-position
                                   :attempt (:attempt chunk)
                                   :delay-ms (:delay-ms chunk)
                                   :error {:type :llm.routing/provider-retry
                                           :message (:error chunk)}
                                   :event chunk}))
                    (:event/type chunk) (do (reset-stream-state!)
                                            (on-chunk {:phase :provider-fallback
                                                       :iteration iteration-position
                                                       :event chunk}))
                    :else (do (when (or (some? reasoning) done?)
                                (let [;; The provider's trailing `…` is the summary-elision
                                      ;; MARKER, not text the model wrote; strip it at the
                                      ;; producer so the live stream, the CLI trace rail and
                                      ;; every gateway consumer agree on one clean string.
                                      thinking (some-> reasoning
                                                       str
                                                       util/strip-elision-marker)
                                      delta (cumulative-delta! reasoning-prev-volatile thinking)]

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
                                      delta (cumulative-delta! content-prev-volatile content-s)]

                                  (on-chunk {:phase :content
                                             :iteration iteration-position
                                             :content content-s
                                             :delta delta
                                             :done? (boolean done?)})))
                              ;; Provider transport bookkeeping has no channel projection.
                              nil))))
          copilot-initiator (loop-router/copilot-initiator-for-iteration iteration)
          effective-llm-headers
          (not-empty (merge (loop-router/copilot-llm-headers resolved-model copilot-initiator)
                            llm-headers))
          provider-network (loop-router/provider-network-policy (:router environment)
                                                                resolved-model)
          provider-deadlines (loop-router/provider-watchdog-timeouts provider-network)
          first-output-timeout-ms (long (or (:first-output-timeout-ms provider-deadlines)
                                            rt/ASK_CODE_FIRST_OUTPUT_TIMEOUT_MS))
          ;; Let Svar abort this attempt and the retry callback run before the
          ;; gateway's last-resort watchdog can cancel the entire turn.
          provider-watchdog-timeouts (assoc provider-deadlines
                                       :first-output-timeout-ms (+ first-output-timeout-ms 10000))
          goal-at-request-start (let [goal (goals/check-goal environment)]
                                  (when (= "active" (get goal "status")) goal))
          provider-started-at-ms (util/now-ms)
          _ (when on-chunk
              (on-chunk (transcript/provider-call-chunk iteration-position
                                                        resolved-model
                                                        provider-started-at-ms
                                                        provider-watchdog-timeouts)))
          provider-start-ns (System/nanoTime)
          ;; An explicit session cache key preserves sticky routing across prompt changes.
          ;; Anthropic ignores the field, so setting it unconditionally is harmless.
          session-cache-key (some-> (:session-id environment)
                                    str)
          ;; Prepared turns opt into the cross-provider fleet. Direct callers that
          ;; omit a policy stay on a warm provider; disabling `provider_fallback`
          ;; pins the resolved model below.
          pinned-routing (loop-router/pin-routing-to-model routing resolved-model)
          ;; Anthropic's safety classifier declines with HTTP 200 (`stop_reason: refusal`):
          ;; the credential, the provider and the wire are all healthy, so the recovery is a
          ;; SIBLING MODEL of the same provider — pin it, or the router resolves the fallback
          ;; name on whichever provider is cheapest.
          refusal-fallbacks (loop-router/refusal-fallbacks-for (:router environment) resolved-model)
          refusal-routing (cond-> pinned-routing
                            refusal-fallbacks
                            (loop-router/pin-routing-to-provider resolved-model))
          sticky-routing (cond-> refusal-routing
                           (not (contains? refusal-routing :on-transient-error))
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
          provider-tools (transcript/model-facing-tools (:sandbox-caps environment))
          ask-opts
          (rt/with-default-ask-code-idle-timeout
            (cond-> {;; ONE tool on the wire: the model takes every action
                     ;; by calling `python_execution` with a Python
                     ;; program; a reply with NO tool call is the
                     ;; final answer (its text). svar returns
                     ;; {:stop-reason :tool-calls|:end :tool-calls :content
                     ;; :assistant-message}.
                     :tools provider-tools
                     :tool-choice :auto
                     ;; four prompt-cache breakpoints: the frozen system prefix
                     ;; plus the trailing transcript. See apply-cache-breakpoints.
                     :messages (vec messages)
                     :prompt-cache-policy (transcript/prompt-cache-policy (:provider
                                                                            resolved-model))
                     :routing sticky-routing
                     :check-context? true
                     :input-token-estimator input-token-estimator
                     :preserved-thinking? true
                     :on-empty-reply-resend
                     (fn [{:keys [attempt max-resends delay-ms]}]
                       ;; LIVE, not post-hoc: emit the retry chunk the
                       ;; instant svar re-sends, so the channel paints
                       ;; "resend 1/3" instead of silence and the gateway
                       ;; persists the recap even if the human cancels
                       ;; mid-ladder. The same event still rides the
                       ;; routing trace the finished call returns.
                       (let [chunk (empty-reply-resend-chunk iteration-position
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
                    (boolean (deref ca))))))
            provider-network)
          ask-result-raw
          ;; Svar forwards query-id; its request-id belongs to the upstream HTTP request.
          (svar/with-log-context
            {:query-id (:request-id request-context) :iteration iteration-position}
            ;; Svar remains the owner of provider classification and retries.
            (try (transcript/ask-code-with-first-output-timeout! environment
                                                                 resolved-model
                                                                 ask-opts
                                                                 first-output-timeout-ms)
                 (catch Exception e
                   (when (perr/context-overflow-error? e)
                     (let [model (or (get-in (ex-data e) [:request-accounting :model])
                                     (:model (ex-data e))
                                     (:name resolved-model)
                                     (:model resolved-model))]
                       (transcript/log-context-token-counts!
                         messages
                         (prompt/request-health environment
                                                messages
                                                provider-tools
                                                model
                                                (:request-accounting (ex-data e))
                                                message-token-counter)
                         (:provider resolved-model)
                         model
                         request-context
                         (merge {:outcome :context-overflow :route-source :resolved}
                                (transcript/context-overflow-token-data (ex-data e))))))
                   (throw e))))
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
          parse-started-at-ms (util/now-ms)
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
          api-usage (:api-usage ask-result)
          _ (goals/account! environment goal-at-request-start api-usage)
          actual-provider (transcript/actual-llm-provider resolved-model ask-result)
          actual-model (transcript/actual-llm-model resolved-model ask-result)
          ;; Blockether/vis#174: publish measured input before Python can fold this request.
          _ (when on-response
              (on-response
                {:api-usage api-usage :llm-provider actual-provider :llm-model actual-model}))
          fold-measurement (get (some-> (:ctx-atom environment)
                                        deref)
                                "engine_fold_measurement")
          request-health (cond-> (prompt/request-health environment
                                                        messages
                                                        provider-tools
                                                        actual-model
                                                        (:request-accounting ask-result)
                                                        message-token-counter)
                           fold-measurement
                           (assoc :fold-measurement fold-measurement))
          _ (transcript/log-context-token-counts! messages
                                                  request-health
                                                  actual-provider
                                                  actual-model
                                                  request-context
                                                  {:outcome :succeeded
                                                   :route-source :response
                                                   :provider-input-tokens (:input-tokens
                                                                            api-usage)})
          _agent-checkpoint (agents/checkpoint! environment messages)
          prompt-cache-sample (transcript/note-prompt-cache-request!
                                (:prompt-cache-history-atom environment)
                                actual-provider
                                actual-model
                                (:prompt-cache-context ask-result)
                                messages
                                (:input-tokens api-usage)
                                (get-in api-usage [:input-tokens-details :cache-read])
                                provider-started-at-ms)
          prompt-cache-reusable-tokens (:reusable-tokens prompt-cache-sample)
          prompt-cache-continuity (:continuity prompt-cache-sample)
          prompt-cache-reuse-kind (:reuse-kind prompt-cache-sample)
          reasoning-effort-resolution (:routed/reasoning-effort ask-result)
          ;; The model either CALLS `python_execution`
          ;; (`:stop-reason :tool-calls`) or, with NO tool call
          ;; (`:stop-reason :end`), returns its final answer as `:content`.
          ;; An answer reply finalizes the turn directly (finalize-answer!
          ;; records it; the FINAL path below stores/renders it) and runs no
          ;; code. A tool-call reply becomes the executable blocks: one
          ;; `python_execution` call → one block, carrying the tool_use `:id` so the
          ;; driver can pair its result into a `tool_result` message.
          tool-calls (transcript/normalize-tool-calls (:tool-calls ask-result))
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
          ;; #216: persist and stream why a reasoning-only/empty response did no work.
          ;; This is engine feedback, not fabricated provider prose or a Python error.
          assistant-prose
          (if (seq tool-calls)
            (transcript/prose-beyond-code prose-md tool-calls)
            (when-not prose-md
              "Provider returned no executable tool call or answer text; nothing was executed."))
          ;; Keep useful prose for a later stop without finalizing the turn before validation
          ;; or while an explicit goal still has work to do.
          _ (when answer-md
              (swap! turn-state-atom assoc
                :best-answer
                {:value {:answer answer-md} :answer-markdown answer-md}))
          _ (when (and assistant-prose on-chunk)
              (on-chunk
                {:phase :assistant-prose :iteration iteration-position :text assistant-prose}))
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
          ;; No tool call is not malformed Python: let the bounded empty-reply path handle it.
          preflight-result (if (empty? blocks)
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
                           :vis/tool-name (:vis/tool-name entry)
                           :scope (form-scope idx)
                           :code expr
                           :render-segments render-segments
                           :started-at-ms (util/now-ms)}))
              ;; Stamp form-idx BEFORE eval so the
              ;; executing block's position is recorded
              ;; on the turn-state atom.
              (swap! turn-state-atom assoc :form-idx idx)
              (let [scope (form-scope idx)
                    emit-activity! (when (and on-chunk (not suppress-form-start?))
                                     (fn [snapshot settled?]
                                       (on-chunk (cond-> {:phase :form-activity
                                                          :iteration iteration-position
                                                          :position idx
                                                          :count total-blocks
                                                          :scope scope
                                                          :activity snapshot}
                                                   settled?
                                                   (assoc :settled? true)))))
                    goal-halt (goals/halt-result environment goal-at-request-start)
                    raw-execution
                    (try
                      (cond goal-halt {:error (loop-errors/op-error (:answer goal-halt)
                                                                    {:code expr :phase :vis/goal})
                                       :duration-ms 0
                                       :op :vis/guard}
                            preflight-error {:error (loop-errors/op-error preflight-error
                                                                          {:code expr
                                                                           :phase :vis/preflight})
                                             :duration-ms 0
                                             :op :vis/guard}
                            :else
                            (if-let [err (python-exec/literal-code-block-error (env/python-context
                                                                                 environment)
                                                                               expr)]
                              {:error (loop-errors/op-error err {:code expr :phase :vis/guard})
                               :duration-ms 0
                               :op :vis/guard}
                              (let [tool-event-fn (when (and on-chunk (not suppress-form-start?))
                                                    (fn [tool-event]
                                                      ;; Turn progress consumes starts only. Terminal truth
                                                      ;; feeds Activity without changing that stream.
                                                      (when (and (= :start (:phase tool-event))
                                                                 (activity-event/visible-event?
                                                                   tool-event))
                                                        (on-chunk {:phase :tool-start
                                                                   :iteration iteration-position
                                                                   :position idx
                                                                   :count total-blocks
                                                                   :scope scope
                                                                   :code expr
                                                                   :render-segments render-segments
                                                                   :tool-event tool-event}))))
                                    r (let [activity-env (assoc environment
                                                           ;; Activity belongs to the form and routes on the
                                                           ;; position shared by all of that form's frames.
                                                           :activity/on-snapshot
                                                           (when emit-activity!
                                                             (fn [snapshot]
                                                               (emit-activity! snapshot false))))]
                                        (if tool-event-fn
                                          (python-exec/execute-code activity-env
                                                                    expr
                                                                    :tool-event-fn
                                                                    tool-event-fn)
                                          (python-exec/execute-code activity-env expr)))]

                                (log-stage! :code-result
                                            iteration
                                            {:idx (inc (long idx))
                                             :total total-blocks
                                             :duration-ms (:duration-ms r)
                                             :error (:error r)
                                             :timeout? (:timeout? r)})
                                r)))
                      (catch clojure.lang.ExceptionInfo e
                        ;; Issue #180: keep the attempted source and error in the
                        ;; normal form/persistence path even when entry is refused.
                        (if (= ::env/context-retired (:type (ex-data e)))
                          {:error (assoc (ex-data e) :message (ex-message e)) :duration-ms 0}
                          (throw e))))
                    ;; Carry parinfer's whole-source rebalance flag into the execution
                    ;; record. `execute-code` may also set `:repaired?` through the
                    ;; extension rescue hook; both paths converge on the same channel flag.
                    execution (cond-> (council/record-failure!
                                        environment
                                        entry
                                        (update raw-execution :error loop-errors/op-error))
                                form-repaired?
                                (assoc :repaired? true)

                                (:auto-repaired raw-execution)
                                (assoc :repaired? true))
                    block-role (transcript/eval-block-role execution)
                    envelope (transcript/eval-envelope turn-prefix
                                                       iteration-position
                                                       idx
                                                       total-blocks
                                                       execution
                                                       block-role)
                    execution* (assoc execution
                                 :envelope envelope
                                 :role block-role)
                    _ (when (and emit-activity! (map? (:activity execution*)))
                        ;; The last revision has the same event type as the running
                        ;; replacements, but is durable. Output remains output.
                        (emit-activity! (:activity execution*) true))]

                ;; Stream each block result immediately, except model-facing preflight rejections.
                (when (and on-chunk (not preflight-error))
                  (on-chunk
                    {:phase :form-result
                     :iteration iteration-position
                     :position idx
                     :count total-blocks
                     :scope scope
                     :code expr
                     :render-segments render-segments
                     ;; The live card and replay use the same serving model and call.
                     :vis/tool-name (:vis/tool-name entry)
                     :svar/tool-call-id (:svar/tool-call-id entry)
                     :llm-model (transcript/actual-llm-model resolved-model ask-result)
                     :stdout (:stdout execution*)
                     :error (:error execution*)
                     :envelope (:envelope execution*)
                     :role (:role execution*)
                     :timeout? (boolean (:timeout? execution*))
                     :repaired? (boolean (:repaired? execution*))}))
                {:block expr
                 :execution execution*
                 :render-segments render-segments
                 :svar/tool-call-id (:svar/tool-call-id entry)
                 :vis/tool-name (:vis/tool-name entry)}))
            (range)
            code-entries)
          form-sources (mapv :block executed)
          form-executions (mapv :execution executed)
          form-segments (mapv :render-segments executed)
          form-tool-ids (mapv :svar/tool-call-id executed)
          form-tool-names (mapv :vis/tool-name executed)
          ;; Preflight gate → synthetic block carries `:vis/preflight? true`
          ;; so channels can suppress the model-facing-only error box. Keep
          ;; the block in the persisted/trailer stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range)
                                   (map (fn [{:vis/keys [preflight-error]}]
                                          (boolean preflight-error))
                                        code-entries))
          blocks
          (transcript/validate-iteration-blocks!
            (mapv (fn [idx code execution segments tool-call-id tool-name]
                    (cond-> {:id idx
                             :code code
                             ;; What the block PRINTED — Python's one success channel.
                             ;; One block = one tool call, so this is the call's whole
                             ;; stdout (no per-form split).
                             :stdout (:stdout execution)
                             :llm-model (transcript/actual-llm-model resolved-model ask-result)
                             ;; Artifacts the block PRODUCED (an `attach` call),
                             ;; captured at the SOURCE into the sandbox sink —
                             ;; carried down so the DB attachment OWNS the bytes.
                             :attachments (:attachments execution)
                             ;; Reinspection is ephemeral: it reaches the next request but
                             ;; is never written as a duplicate iteration artifact.
                             :reinspect-attachments (:reinspect-attachments execution)
                             :error (loop-errors/op-error
                                      (:error execution)
                                      {:code code :phase (get-in execution [:envelope :op])})
                             :envelope (:envelope execution)
                             :role (:role execution)
                             :timeout? (:timeout? execution)
                             :repaired? (:repaired? execution)
                             ;; Per-block resolve-symbol* LRU stamps:
                             ;; symbol-name -> current-turn-pos for every
                             ;; symbol the engine hook saw resolve during
                             ;; this block's eval. Iteration writer
                             ;; merges into the long-lived per-env LRU.
                             :lru (or (:lru execution) {})
                             ;; If the engine auto-repaired delimiter
                             ;; mistakes (delimiter repair) before eval, the
                             ;; repaired source flows here so the trailer
                             ;; can disclose the diff and the model can
                             ;; correct itself if the repair was wrong.
                             :repaired-source (:repaired-source execution)}
                      ;; Render metadata is optional; `:code` remains the canonical source.
                      (seq segments)
                      (assoc :render-segments segments)

                      ;; Persist the settler's one frozen snapshot on the block;
                      ;; its own durable wire event carried the same value above.
                      (some? (:activity execution))
                      (assoc :activity (:activity execution))

                      (some? (:vis/fold-count execution))
                      (assoc :vis/fold-count (:vis/fold-count execution))

                      ;; Provider-call identity rides onto the block so
                      ;; `blocks->forms` stamps each form envelope with the
                      ;; tool_use call its Python execution answers.
                      tool-call-id
                      (assoc :svar/tool-call-id tool-call-id)

                      tool-name
                      (assoc :vis/tool-name tool-name)

                      (get preflight-by-idx idx)
                      (assoc :vis/preflight? true)))
                  (range)
                  form-sources
                  form-executions
                  form-segments
                  form-tool-ids
                  form-tool-names))]

      (if answer-md
        ;; Answer validation and goal continuation are separate: accepted progress is not
        ;; an error and must not finalize the turn/context while the goal remains active.
        (let [value {:answer answer-md}
              validation-error (final-answer-gate-error environment
                                                        iteration-position
                                                        blocks
                                                        value
                                                        active-extensions
                                                        (assoc answer-validation-context
                                                          :code-entries code-entries))
              goal-continuation (when-not validation-error (goals/continuation-prompt environment))
              model-name (transcript/actual-llm-model resolved-model ask-result)
              provider (transcript/actual-llm-provider resolved-model ask-result)]

          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks)
                         [{:id 0
                           :code "(final-answer-validation)"
                           :error (loop-errors/op-error validation-error
                                                        {:code "(final-answer-validation)"
                                                         :phase :vis/final-answer-validation})}])
             :final-result nil
             :request-health request-health
             :api-usage api-usage
             :prompt-cache (:prompt-cache ask-result)
             :prompt-cache-reusable-tokens prompt-cache-reusable-tokens
             :prompt-cache-continuity prompt-cache-continuity
             :prompt-cache-reuse-kind prompt-cache-reuse-kind
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
             :assistant-prose (when goal-continuation answer-md)
             :goal-continuation goal-continuation
             :blocks blocks
             :final-result (when-not goal-continuation
                             {:final? true :answer (finalize-answer! environment value)})
             :request-health request-health
             :api-usage api-usage
             :prompt-cache (:prompt-cache ask-result)
             :prompt-cache-reusable-tokens prompt-cache-reusable-tokens
             :prompt-cache-continuity prompt-cache-continuity
             :prompt-cache-reuse-kind prompt-cache-reuse-kind
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
             :assistant-message (or (:assistant-message ask-result)
                                    (when goal-continuation
                                      {:role "assistant"
                                       :content [{:type "text" :text answer-md}]}))}))
        ;; Normal path (tool-call iteration)
        {:thinking thinking
         :assistant-prose assistant-prose
         :blocks blocks
         :tool-calls tool-calls
         :final-result nil
         :request-health request-health
         :api-usage api-usage
         :prompt-cache (:prompt-cache ask-result)
         :prompt-cache-reusable-tokens prompt-cache-reusable-tokens
         :prompt-cache-continuity prompt-cache-continuity
         :prompt-cache-reuse-kind prompt-cache-reuse-kind
         :duration-ms (or (:duration-ms ask-result) 0)
         :llm-messages messages
         :llm-provider (transcript/actual-llm-provider resolved-model ask-result)
         :llm-model (transcript/actual-llm-model resolved-model ask-result)
         :llm-selected-provider (:provider resolved-model)
         :llm-selected-model (some-> (:name resolved-model)
                                     str)
         :llm-actual-provider (transcript/actual-llm-provider resolved-model ask-result)
         :llm-actual-model (transcript/actual-llm-model resolved-model ask-result)
         :llm-routing-trace (:routed/trace ask-result)
         :reasoning-effort-resolution reasoning-effort-resolution
         :llm-returned-empty-code? (empty? blocks)
         :assistant-message (:assistant-message ask-result)}))))

;; Multi-iteration turn engine helpers

(defn- stream-output-overflow? [err] (loop-errors/output-budget-exhausted-data? (:data err)))

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

;; Iteration loop

(def ^:private FRESH_ITER_CARRY
  ;; `:trailer-iters` is a vec of `[iteration-position {:thinking :blocks}]`
  ;; pairs (oldest-first). NOTHING trims this by token budget: neither the
  ;; seed (cross-turn carry) nor the renderer. `max-context-tokens` only
  ;; feeds the advisory context-pressure hint. The sole token-driven
  ;; reduction is reactive — `context-overflow-recovery` after the provider
  ;; refuses the request.
  {:trailer-iters []})

(def ^:private balanced-reasoning :balanced)

(def ^:private empty-replies-give-up-text
  "Fallback shown only when the provider repeatedly returns an empty reply."
  "The model returned empty replies repeatedly, so the turn was stopped.")

(defn- provider-output-chunk?
  "True after visible assistant output or a tool execution entered the stream.
   Engine lifecycle/progress chunks are safe to replay after a pre-output failure."
  [chunk]
  (case (:phase chunk)
    (:reasoning :content :assistant-prose)
    (boolean (some seq ((juxt :delta :thinking :content :text) chunk)))

    (:form-start :tool-start :form-result)
    true

    false))

(defn- emergency-fold-activity
  "Mechanical activity count for the omitted iterations, derived only from the
   tool calls they recorded. `python_execution` is the only call there is, so the
   COUNT is the whole shape — no tool family is left to name."
  [trailer-iters scopes]
  (let [calls
        (into []
              (comp (filter (fn [[_ rec]]
                              (contains? scopes (transcript/iteration-record-scope rec))))
                    (mapcat (fn [[_ rec]]
                              (keep :name (:tool-calls rec)))))
              trailer-iters)

        n
        (long (count calls))]

    (when (pos? n) (str n " tool call" (when (not= n 1) "s")))))

(def ^:private CONTEXT_OVERFLOW_MARGINS
  "Headroom each rescue leaves under the reported input limit after translating
   rejection counts into local estimator units (`estimator-undercount`). Preflight
   counts are not necessarily provider measurements. The folded message mix differs
   from the original projection; successive margins leave progressively more room."
  [0.9 0.7 0.5])

(def ^:private CONTEXT_OVERFLOW_BLIND_CUTS
  "Fallback targets for an overflow that carries no usable `:input-tokens` or
   `:max-input-tokens` (some providers refuse without measuring).

   With nothing to calibrate against, a rescue can only bisect its OWN estimate, so
   each attempt targets this fraction of the refused request's local size. Still no
   invented factor: the target is purely relative and escalation does the searching."
  [0.5 0.25 0.1])

(defn- overflow-fold-budget
  "Local-estimator budget for the next rescue of a refused request.

   Reported path: `reported-limit` / budget undercount * `margin`, expressed in the
   same units the projection counts. Blind path (no usable rejection counts): `cut`
   of the request's local size. nil means only strict shrinkage can be required."
  [{:keys [reported-tokens reported-limit margin cut]} local-tokens]
  (let [factor (transcript/estimator-undercount reported-tokens local-tokens)]
    (cond (and factor (number? reported-limit) (pos? (long reported-limit)))
          (long (* (/ (double reported-limit) (double factor)) (double (or margin 1.0))))
          (and (number? cut) (number? local-tokens) (pos? (long local-tokens)))
          (long (* (double local-tokens) (double cut)))
          :else nil)))

(defn- emergency-fold-projection
  "Build a provider projection with settled trailer iterations collapsed through the
   same `apply-summaries` path as `fold_session`.

   Folding is GRADUATED: the foldable universe is walked OLDEST first and the search
   keeps the SMALLEST prefix whose projection fits the budget `budget-fn` derives from
   the pending request. A fold spends the least recent context it can; recent
   settled work survives verbatim. Canonical history and persisted semantic gists
   remain authoritative. Every actual send still repeats Svar's provider-aware
   preflight; a local or usage-anchored estimate is not an exact provider count."
  ([base-messages trailer-iters summaries replay-target model budget-fn]
   (emergency-fold-projection base-messages
                              trailer-iters
                              summaries
                              replay-target
                              model
                              budget-fn
                              {}))
  ([base-messages trailer-iters summaries replay-target model budget-fn
    {:keys [count-messages-fn reason]}]
   (let [count-messages-fn
         (or count-messages-fn #(svar/count-messages model %))

         universe
         (into []
               (keep (fn [[_ rec]]
                       (transcript/iteration-record-scope rec)))
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
       (let [before-messages
             (into (vec base-messages) (transcript/conversation-suffix trailer-iters replay-target))

             before-tokens
             (count-messages-fn before-messages)

             ;; Budget is derived from what the provider actually charged for THIS set,
             ;; so the gate below compares local estimate against local estimate.
             budget
             (budget-fn before-tokens)

             project
             (fn [n]
               (let [scopes
                     (into #{} (take n) foldable)

                     activity
                     (emergency-fold-activity trailer-iters scopes)

                     intent
                     (cond-> {"scopes" scopes
                              "gist" (str (if (= :budget reason)
                                            "Proactive transport fold omitted "
                                            "Emergency transport fold omitted ")
                                          (count scopes)
                                          " settled iteration(s)"
                                          (when (seq activity) (str " (" activity ")"))
                                          (if (= :budget reason)
                                            " at the operating context budget"
                                            " after context overflow")
                                          "; canonical session history remains intact.")}
                       live-turn
                       (assoc "at_turn" live-turn))

                     folded-trailer
                     (transcript/apply-summaries trailer-iters (conj (vec summaries) intent))

                     messages
                     (into (vec base-messages)
                           (transcript/conversation-suffix folded-trailer replay-target))]

                 {:messages messages
                  :scopes scopes
                  :summary intent
                  :after-tokens (count-messages-fn messages)}))

             fits?
             (fn [{:keys [after-tokens]}]
               (and (< (long after-tokens) (long before-tokens))
                    (or (nil? budget) (<= (long after-tokens) (long budget)))))

             ;; Folding more can only shrink the projection, so the smallest fitting
             ;; prefix is a binary search — O(log n) estimator passes, not O(n).
             chosen
             (loop [lo
                    1

                    hi
                    (count foldable)

                    best
                    nil]

               (if (> (long lo) (long hi))
                 (or best (project (count foldable)))
                 (let [mid
                       (quot (+ (long lo) (long hi)) 2)

                       candidate
                       (project mid)]

                   (if (fits? candidate)
                     (recur lo (dec mid) candidate)
                     (recur (inc mid) hi best)))))]

         (when (fits? chosen)
           {:messages (:messages chosen)
            :before-tokens before-tokens
            :after-tokens (:after-tokens chosen)
            :saved-tokens (- (long before-tokens) (long (:after-tokens chosen)))
            :budget-tokens budget
            :folded-scopes (count (:scopes chosen))
            :foldable-scopes (count foldable)
            :scopes (:scopes chosen)
            :summary (:summary chosen)}))))))

(defn- measured-request-estimator
  "Return a measured-prefix estimate, or nil when this route/prefix has no valid anchor."
  [history provider model prompt-cache-context count-messages]
  (let [entry
        (get history [provider (str model)])

        prior
        (:fingerprints entry)

        input
        (:input-tokens entry)

        anchored?
        (and (seq prior)
             (integer? input)
             (pos? (long input))
             (transcript/same-prompt-cache-context? (:prompt-cache-context entry)
                                                    prompt-cache-context))

        priming
        (long (count-messages model []))]

    (fn [messages]
      (when-some [tail (when (and anchored?
                                  (<= (count prior) (count messages))
                                  (= prior
                                     (:fingerprints (transcript/message-cache-data
                                                      entry
                                                      (subvec (vec messages) 0 (count prior))))))
                         (subvec (vec messages) (count prior)))]
        (+ (long input) (- (long (count-messages model tail)) priming))))))

(defn- request-context-estimator
  "Estimate pending input from an exact accepted prefix plus its new tail. Cached
   input is included once. Rewrites and changed tools/account/model invalidate usage."
  [history provider model prompt-cache-context & [message-token-counter]]
  (let [count-messages
        (or message-token-counter svar/count-messages)

        measured
        (measured-request-estimator history provider model prompt-cache-context count-messages)]

    (fn [messages]
      (or (measured messages) (long (count-messages model messages))))))

(defn- request-input-token-estimator
  "Validate calibration against the actual Svar route. Nil keeps Svar's prepared-wire
   fallback, rather than replacing it with a canonical-message estimate."
  [history-atom]
  (fn [{:keys [provider-id model messages prompt-cache-context tokenizer]}]
    (let [counter
          (prompt/request-token-counter {:tokenizer tokenizer})

          estimate
          (measured-request-estimator (when history-atom @history-atom)
                                      provider-id
                                      model
                                      prompt-cache-context
                                      counter)]

      (estimate messages))))

(defn- history-fold-projection
  "Return a strictly smaller, fitting projection; never mutate canonical history.
   A carried base is first rebuilt so inherited iterations become foldable. Both
   proactive compaction and overflow recovery use the same graduated fold."
  [{:keys [request-messages base-messages trailer-iters summaries replay-target model
           canonical-base-messages-fn canonical-trailer-iters budget-tokens last-after-tokens
           count-messages-fn reason]}]
  (let [count-messages-fn
        (or count-messages-fn #(svar/count-messages model %))

        before-tokens
        (count-messages-fn (or request-messages
                               (into (vec base-messages)
                                     (transcript/conversation-suffix trailer-iters replay-target))))

        fits?
        (fn [after]
          (and (< (long after) (long before-tokens))
               (or (nil? budget-tokens) (<= (long after) (long budget-tokens)))
               (or (nil? last-after-tokens) (< (long after) (long last-after-tokens)))))

        canonical-base
        (when canonical-base-messages-fn (vec (canonical-base-messages-fn)))

        base
        (or canonical-base base-messages)

        trailer
        (if canonical-base canonical-trailer-iters trailer-iters)

        canonical
        (when canonical-base
          (into canonical-base (transcript/conversation-suffix trailer replay-target)))

        canonical-tokens
        (when canonical (count-messages-fn canonical))

        projection
        (or (when (and canonical (fits? canonical-tokens))
              {:messages canonical
               :after-tokens canonical-tokens
               :folded-scopes 0
               :foldable-scopes 0
               :scopes #{}
               :projection-kind :canonical-rebuild})
            (some->
              (emergency-fold-projection base
                                         trailer
                                         summaries
                                         replay-target
                                         model
                                         (constantly budget-tokens)
                                         {:count-messages-fn count-messages-fn :reason reason})
              (assoc :projection-kind (if (= :budget reason) :proactive-fold :emergency-fold))))]

    (when (and projection (fits? (:after-tokens projection)))
      (cond-> (assoc projection
                :before-tokens before-tokens
                :saved-tokens (- (long before-tokens) (long (:after-tokens projection)))
                :budget-tokens budget-tokens)
        canonical-base
        (assoc :canonical-base-messages
          canonical-base :retry-prompt-base
          :canonical)))))

(defn- pre-request-context-projection
  "Force a fitting history projection at the operating budget, before any send.
   Price the actual pending user/tool input, even on the first resumed iteration.
   This is not an overflow retry and does not consume its bounded rescue budget.
   If immutable input cannot fit, leave the request for normal preflight to reject."
  [{:keys [request-messages model budget-tokens count-messages-fn] :as opts}]
  (let [count-messages-fn (or count-messages-fn #(svar/count-messages model %))]
    (when (and (number? budget-tokens)
               (pos? (long budget-tokens))
               (>= (long (count-messages-fn request-messages)) (long budget-tokens)))
      (some-> (history-fold-projection (assoc opts
                                         :count-messages-fn count-messages-fn
                                         :reason :budget))
              (dissoc :retry-prompt-base)
              (assoc :projection-count-source :request-context-estimate)))))

(defn- context-overflow-recovery!
  "Claim a pre-output overflow rescue and return a strictly smaller request.
   Price the exact refused request and tighten the rejection-derived budget.
   The caller retains a rebuilt base and any transport-only fold intent; persisted
   history and semantic summaries are never changed here."
  [{:keys [error output-started? recovery-state ctx-atom turn-input-tokens request-messages
           base-messages trailer-iters replay-target model]
    :as opts}]
  (let [overflow (ex-data error)]
    (when (and (contains? perr/CONTEXT_OVERFLOW_TYPES (:type overflow)) (not @output-started?))
      (let [{:keys [attempts last-after-tokens]}
            (swap! recovery-state update :attempts (fnil inc 0))
            attempt (long attempts)]

        (when (<= attempt (count CONTEXT_OVERFLOW_MARGINS))
          (transcript/stamp-utilization! ctx-atom
                                         (ctx-engine/utilization (:input-tokens overflow)
                                                                 (:max-input-tokens overflow)
                                                                 turn-input-tokens
                                                                 (loop-router/context-fold-budget
                                                                   (:max-input-tokens overflow))))
          (let [before-tokens (svar/count-messages model
                                                   (or request-messages
                                                       (into (vec base-messages)
                                                             (transcript/conversation-suffix
                                                               trailer-iters
                                                               replay-target))))
                budget (overflow-fold-budget {:reported-tokens (:input-tokens overflow)
                                              :reported-limit (:max-input-tokens overflow)
                                              :margin (nth CONTEXT_OVERFLOW_MARGINS (dec attempt))
                                              :cut (nth CONTEXT_OVERFLOW_BLIND_CUTS (dec attempt))}
                                             before-tokens)]

            (when-let [projection (history-fold-projection (assoc opts
                                                             :budget-tokens budget
                                                             :last-after-tokens last-after-tokens))]
              (swap! recovery-state assoc :last-after-tokens (:after-tokens projection))
              (merge projection
                     (transcript/context-overflow-token-data overflow)
                     {:attempt attempt
                      :projection-count-source :svar-message-estimate
                      :projection-counted-from :request-messages
                      :estimator-undercount (transcript/estimator-undercount (:input-tokens
                                                                               overflow)
                                                                             before-tokens)}))))))))

(def ^:private voice-projection-prompt
  "This is a text-only turn whose client may optionally read a separate projection aloud. Write the complete normal answer exactly as you would for any text client; do not mention voice mode, listening, playback, or ask the user to confirm that audio played. Then finish with exactly one fenced `vis-speech` block. Inside that block write only concise, natural plain text suitable for text-to-speech: no Markdown, code, tables, raw URLs, citation syntax, or tool narration. State the outcome, an important caveat, and any question that needs an answer. Do not repeat secrets. Aim for 15 to 45 seconds.")

(defn- voice-system-prompt
  [system-prompt turn-features]
  (if (true? (get turn-features "voice_projection"))
    (str (when-not (str/blank? (str system-prompt)) (str system-prompt "\n\n"))
         voice-projection-prompt)
    system-prompt))

(defn- with-council-execution
  "Keep publication identity and references in execution state across provider
   retries. Detach them before persistence, and clear them on every exit without
   clearing a newer execution installed by another worker."
  [environment active iteration-key f]
  (if-not active
    (f)
    (let [identity
          {:activation-id (:activation-id active) :iteration-key iteration-key}

          current?
          (fn [state]
            (= identity (select-keys (:council state) (keys identity))))

          detach
          (fn [state]
            (if (current? state) (dissoc state :council) state))]

      (ctx-loop/set-turn-state! environment :council (assoc identity :publications []))
      (try (let [result
                 (f)

                 _
                 (when-not (::loop-errors/iteration-error result)
                   (council/acknowledge-input! (:db-info environment)
                                               (str (:session-id environment))
                                               active
                                               iteration-key))

                 state
                 (first (swap-vals! (:turn-state-atom environment) detach))]

             (when-not (current? state)
               (tel/log! {:level :warn
                          :id ::council-execution-superseded
                          :data {:session-id (str (:session-id environment))}}))
             (assoc result
               :council-publications (when (current? state)
                                       (get-in state [:council :publications]))))
           (finally (ctx-loop/swap-turn-state! environment detach))))))

(defn- seed-trailer-iters
  "Select scopes before reading prior-turn bodies; keep artifacts as disk metadata."
  [environment session-turn-id summaries]
  (when-let [session-id (:session-id environment)]
    (let [db (:db-info environment)
          turns (remove #(= (str (:id %)) (str session-turn-id))
                  (persistance/db-list-session-turns-meta db session-id))
          history (transcript/provider-history-metadata db (map :id turns))
          metadata (:iterations history)
          turns (remove #(contains? (:local-turn-ids history) (str (:id %))) turns)
          entries (into []
                        (mapcat (fn [turn]
                                  (for [it (get metadata (str (:id turn)))
                                        :when (= :done (:status it))]

                                    [(str (:id it))
                                     {:iteration-id (:id it)
                                      :iteration-scope (str "t" (:position turn)
                                                            "/i" (:position it))
                                      :llm-provider (:provider it)
                                      :llm-model (:model it)
                                      :cross-turn/turn-status (:status turn)
                                      :preserved-thinking/replay? false}])))
                        turns)
          compacted (transcript/apply-summaries entries summaries)
          visible (remove (comp :collapsed? second) compacted)
          incomplete-ids (keep (fn [[_ rec]]
                                 (when (transcript/terminal-incomplete-turn-status?
                                         (:cross-turn/turn-status rec))
                                   (:iteration-id rec)))
                               visible)
          bodies (persistance/db-list-iterations db incomplete-ids)
          artifacts (persistance/db-list-iterations-attachments-meta
                      db
                      (map (comp :iteration-id second) visible))]

      (mapv (fn [[pos rec]]
              (if (:collapsed? rec)
                [pos rec]
                (let [body (get bodies (str (:iteration-id rec)))
                      slash? (transcript/user-slash-iteration? body)]

                  [pos
                   (cond-> (assoc rec
                             :attachments (mapv #(assoc % ::transcript/attachment-db db)
                                                (get artifacts (str (:iteration-id rec)))))
                     (and body (not slash?))
                     (assoc :forms-vec (:forms body)))])))
            compacted))))

(defn- store-trace!
  "Append one exact trace entry to the turn-local disk journal; retain only its offset."
  [^java.io.RandomAccessFile journal entry]
  (let [offset (.length journal)]
    (.seek journal offset)
    (nippy/freeze-to-out! journal entry)
    offset))

(defn- read-trace
  [^java.io.RandomAccessFile journal entry]
  ;; The terminal entry never becomes carry: preserve it directly, including native
  ;; terminal diagnostics that can contain live callbacks and cannot be serialized.
  (if (integer? entry) (do (.seek journal (long entry)) (nippy/thaw-from-in! journal)) entry))

(defn- with-trace-store
  "Keep trace bodies on disk until the terminal response needs its transient snapshot."
  [f]
  (let [path (java.nio.file.Files/createTempFile "vis-turn-trace-"
                                                 ".bin"
                                                 (make-array java.nio.file.attribute.FileAttribute
                                                             0))]
    (try (with-open [journal (java.io.RandomAccessFile. (.toFile path) "rw")]
           (update (f journal) :trace #(mapv (partial read-trace journal) %)))
         (finally (java.nio.file.Files/deleteIfExists path)))))

(def ^:private RECORDING_WAIT_MS "Maximum foreground wait for all recordings in one turn." 300000)

(defn- transcribe-turn-attachments
  "Bound recording joins without charging local speech time to the provider watchdog."
  [rows {:keys [hooks cancel-atom cancel-token]}]
  (when (some #(and (attachments/audio-media-type? (:media-type %)) (str/blank? (:transcription %)))
              rows)
    (when-let [on-chunk (:on-chunk hooks)]
      (on-chunk {:phase :attachment-transcription :iteration 1})))
  (let [transcribed (audio-transcribe/transcribe-attachments
                      rows
                      {:timeout-ms RECORDING_WAIT_MS
                       :cancelled? #(or (some-> cancel-atom
                                                deref)
                                        (cancellation/cancelled? cancel-token))})]
    (when (some #(and (attachments/audio-media-type? (:media-type %))
                      (= audio-transcribe/PENDING (:transcription-status %)))
                transcribed)
      (throw (ex-info (str "Recording transcription timed out after 5 minutes. "
                           "It is still running in the background; the transcript will be saved "
                           "when ready. Try again once it appears.")
                      {:type ::recording-transcription-timeout :timeout-ms RECORDING_WAIT_MS})))
    transcribed))

(defn- iteration-loop*
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt session-turn-id
           ;; The limit feeds pressure hints and the pre-request budget gate;
           ;; canonical history itself is never trimmed.
           max-context-tokens hooks cancel-atom cancel-token reasoning-default routing extra-body
           reasoning-effort turn-features workspace-overrides]
    trace-store ::trace-store}]
  (let [system-prompt
        (str (voice-system-prompt system-prompt turn-features)
             (when (goals/check-goal environment) (str "\n\n" goals/prompt)))

        environment
        (cond-> environment
          (seq turn-features)
          (assoc :turn/features turn-features)

          (seq workspace-overrides)
          (merge workspace-overrides)

          true
          (update :router loop-router/codex-fast-router extra-body turn-features)

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

        goal-at-turn-start
        (goals/check-goal environment)

        resolved-model
        (loop-router/resolve-effective-model (:router environment))

        effective-model
        (:name resolved-model)

        _
        (assert effective-model "Router must resolve a root model")

        ;; Clear any sticky best-answer from a PRIOR turn (the atom lives on the
        ;; per-session env) so this turn's cancel-fallback and its answer only ever
        ;; surface what THIS turn actually produced.
        _
        (some-> (:turn-state-atom environment)
                (swap! assoc :best-answer nil))

        has-reasoning?
        (and (nil? reasoning-effort) (transcript/reasoning-effort-configurable? resolved-model))

        base-reasoning-level
        (or (loop-router/normalize-reasoning-level reasoning-default) balanced-reasoning)

        ;; Activate extensions ONCE per session turn. Threaded through both
        ;; the prompt message assembler (core, environment, extension messages)
        ;; and the per-iteration ext hint collector - activation-fn never
        ;; re-fires inside the loop.
        active-exts
        (prompt/active-extensions environment)

        _extensions-snapshot
        (prompt/extensions-snapshot active-exts)

        _
        (loop-env/sync-active-extension-symbols! environment active-exts)

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
        (transcript/session-turn-position environment session-turn-id)

        previous-usage
        (transcript/previous-request-usage environment session-turn-id)

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
          ;; A RECORDING carries no pixels and no wire takes it, so the local
          ;; speech engine turns it into its own words — once, content-keyed. This
          ;; is the ONE place that waits for them, because this is where they are
          ;; read: the manifest quotes them where the audio cannot go.
          {:attached (transcribe-turn-attachments
                       (into (vec (:user/attachments environment)) (:attached disk))
                       {:hooks hooks :cancel-atom cancel-atom :cancel-token cancel-token})
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
        (loop-router/resolve-effective-model (:router environment) (or routing {}))

        initial-extra-body
        (loop-router/provider-extra-body extra-body)

        root-cost-multiplier
        (loop-router/codex-fast-cost-multiplier extra-body
                                                turn-features
                                                (:provider initial-resolved-model))

        initial-prompt-cache-context
        (transcript/resolved-prompt-cache-context environment
                                                  initial-resolved-model
                                                  routing
                                                  initial-extra-body)

        initial-target-vision?
        (or (empty? (:attached user-attachments))
            (transcript/target-supports-vision? (transcript/replay-context initial-resolved-model)))

        initial-context-limit
        (or max-context-tokens
            (:input-limit initial-resolved-model)
            (:context initial-resolved-model)
            200000)

        initial-fold-budget
        (loop-router/context-fold-budget initial-context-limit)

        _initial-utilization
        (when-let [ctx-atom (:ctx-atom environment)]
          ;; Restore the same whole-session operation count used by usage stats.
          ;; Live fold execution increments it before rendering the next delta;
          ;; neither a printed receipt nor a provider measurement is required.
          (when-let [sid (:session-id environment)]
            (swap! ctx-atom assoc
              "engine_fold_count"
              (long (or (:fold-count (persistance/db-session-usage-stats (:db-info environment)
                                                                         sid))
                        0))))
          (if-let [measured (ctx-engine/utilization (:last-request-tokens previous-usage)
                                                    initial-context-limit
                                                    0
                                                    initial-fold-budget)]
            (transcript/stamp-utilization! ctx-atom measured)
            (swap! ctx-atom (fn [ctx]
                              (if (get ctx "engine_utilization")
                                ctx
                                (assoc ctx
                                  "engine_utilization"
                                  {"last_request_tokens" 0
                                   "turn_total_tokens" 0
                                   "auto_compress_above" initial-fold-budget
                                   "model_input_limit" (long initial-context-limit)
                                   "saturation" 0
                                   "headroom_tokens" (long initial-context-limit)
                                   "measured" false}))))))

        turn-context
        (ctx-loop/render-block! environment ctx-renderer/render-turn-boundary)

        ;; A blind target does not lose the user's screenshot: a sighted model on the
        ;; foreground provider gets first refusal before the rest of the fleet. It turns
        ;; each image into text once (the description is content-keyed), and the report
        ;; rides the manifest where the image blocks would have been.
        initial-image-descriptions
        (when-not initial-target-vision?
          (vision-describe/describe-attachments (:router environment)
                                                user-request
                                                (:attached user-attachments)
                                                (:provider initial-resolved-model)))

        ;; The current turn is assembled separately so an immediate same-route
        ;; follow-up can append it to the exact prior request. Canonical assembly
        ;; remains the fallback and is re-run after any semantic fold changes.
        current-turn-messages
        (prompt/assemble-initial-messages {:initial-user-content user-request
                                           :turn-context turn-context
                                           :user-images (:attached user-attachments)
                                           :skipped-images (:skipped user-attachments)
                                           :vision? initial-target-vision?
                                           :image-descriptions initial-image-descriptions})

        canonical-messages
        (fn canonical-messages ([] (canonical-messages environment))
          ([context-environment] (prompt/assemble-initial-messages
                                   {:stable-prompt-messages stable-prompt-messages
                                    :initial-user-content user-request
                                    :turn-context turn-context
                                    :user-images (:attached user-attachments)
                                    :skipped-images (:skipped user-attachments)
                                    :vision? initial-target-vision?
                                    :image-descriptions initial-image-descriptions
                                    :previous-turn-context (transcript/previous-turn-context
                                                             context-environment session-turn-id
                                                             (:name initial-resolved-model))})))

        summaries-at-turn-start
        (transcript/current-session-summaries environment)

        routing-pref-at-turn-start
        (session-model/model-of (:db-info environment) (:session-id environment))

        resumed-message-base
        (transcript/resumable-prompt-message-base (transcript/load-prompt-cache-state
                                                    (:db-info environment)
                                                    (:session/state-id environment))
                                                  (:provider initial-resolved-model)
                                                  (:name initial-resolved-model)
                                                  initial-prompt-cache-context
                                                  (or turn-position 1)
                                                  summaries-at-turn-start
                                                  stable-prompt-messages
                                                  current-turn-messages)

        message-base-atom
        (atom
          (or (agents/inherited-base environment
                                     (or turn-position 1)
                                     current-turn-messages
                                     summaries-at-turn-start)
              resumed-message-base
              {:messages (canonical-messages) :summaries summaries-at-turn-start :resumed? false}))

        ;; Transport folds survive subsequent iterations without changing the semantic ledger.
        emergency-summaries-atom
        (atom [])

        compact-trailer
        (fn [entries]
          (transcript/apply-summaries entries
                                      (into @emergency-summaries-atom
                                            (transcript/current-session-summaries environment))))

        initial-messages
        (:messages @message-base-atom)

        ;; Context pressure uses the latest single-call input, not cumulative turn billing. On
        ;; the first iteration, seed it from the session's latest persisted request.
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

        ;; Svar owns the metric, including route/scope isolation, the rolling window,
        ;; and freshness. Vis retains only the latest wire-ready snapshot for this turn.
        prompt-cache-status-atom
        (atom nil)

        note-prompt-cache-status!
        (fn [status]
          (when (map? status)
            (let [wire-status (wire/->wire status)]
              (reset! prompt-cache-status-atom wire-status)
              (transcript/stamp-prompt-cache-status! (:ctx-atom environment) status))))

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
        ;; carry one row per iteration so a future `vis-agent report`
        ;; caller can sum or break down cost without re-walking
        ;; provider envelopes. Returns nil when the call surfaced no
        ;; usage (e.g. iteration-level error before a response
        ;; landed), in which case the persistance layer leaves the
        ;; columns NULL.
        iteration-token-cost
        (fn iteration-token-cost ([api-usage] (iteration-token-cost api-usage nil nil))
          ([api-usage actual-model actual-provider] (when api-usage
                                                      (let
                                                        [in
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

                                                         ;; Canonicalize `estimate-cost`, persist `total_cost`, and price the model
                                                         ;; that actually served the call. Use the resolved model only when routing data is absent.
                                                         served-provider
                                                         (or actual-provider
                                                             (:llm-provider api-usage)
                                                             (:provider api-usage)
                                                             (:provider initial-resolved-model))

                                                         cost-map
                                                         (loop-router/estimate-token-cost
                                                           (or (some-> actual-model
                                                                       str
                                                                       not-empty)
                                                               effective-model)
                                                           in
                                                           out
                                                           {:api-usage api-usage
                                                            :cost-multiplier
                                                            (loop-router/codex-fast-cost-multiplier
                                                              extra-body
                                                              turn-features
                                                              served-provider)})

                                                         total
                                                         (when (map? cost-map)
                                                           (get cost-map "total_cost"))]

                                                        (when (map? cost-map)
                                                          (swap! accrued-cost-atom
                                                            #(loop-router/merge-cost-maps
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
                    (loop-router/estimate-token-cost effective-model
                                                     input-tokens
                                                     output-tokens
                                                     {:cached-tokens cached-tokens
                                                      :cache-creation-tokens cache-creation-tokens
                                                      :cost-multiplier root-cost-multiplier}))]

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
                   (tel/log! {:level :warn :data (loop-errors/format-exception-short e)}
                             log-message)))))

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
                              :session-turn-state-id
                              (:state-id (last (persistance/db-list-session-turn-states
                                                 (:db-info environment)
                                                 session-turn-id)))
                              :user-request user-request
                              :turn-position (or turn-position 1)
                              :iteration nil
                              :form-idx nil
                              ;; FORCING plan-gate: distinct files mutated THIS turn (reset each turn).
                              ;; The 2nd distinct file without an approved plan arms the gate.
                              :files-mutated #{})
    ;; Archive hot symbols only after a successful answer. Seed the trailer from prior turns,
    ;; but never replay their provider-native reasoning into a new user turn.
    (let [seeded-trailer-iters
          (try (seed-trailer-iters environment session-turn-id summaries-at-turn-start)
               (catch Throwable t
                 (tel/log!
                   {:level :warn
                    :id ::cross-turn-trailer-seed-failed
                    :data {:error (ex-message t)}
                    :msg "Cross-turn carry seed failed; first iteration starts with an empty tape"})
                 nil))]
      (binding [rt/*rlm-context* (merge rt/*rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages :trace []}
                                 FRESH_ITER_CARRY
                                 (when (seq seeded-trailer-iters)
                                   {:trailer-iters seeded-trailer-iters}))]
          (let [{:keys [iteration trace trailer-iters llm-provider]} loop-state
                goal-halt (goals/request-halt-result environment goal-at-turn-start)]

            (when-not goal-halt
              (ctx-loop/set-turn-state! environment :iteration (inc (long iteration))))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error
                              iteration
                              {:reason :cancelled
                               :cancel-source (cancellation/cancel-reason (:cancel-token
                                                                            environment))})
                  ;; Sticky best-answer: surface the latest non-blank answer
                  ;; this turn produced instead of a blank answer.
                  (let [sticky (some-> (:turn-state-atom environment)
                                       deref
                                       :best-answer
                                       :value)
                        result (merge {:answer sticky
                                       :status :cancelled
                                       :status-id (loop-router/status->id :cancelled)
                                       :trace trace
                                       :iteration-count iteration}
                                      (finalize-cost))]

                    result))
              goal-halt (merge goal-halt
                               {:status-id (loop-router/status->id (:status goal-halt))
                                :answer (if (= :success (:status goal-halt))
                                          (finalize-answer! environment (:answer goal-halt))
                                          (:answer goal-halt))
                                :trace trace
                                :iteration-count iteration}
                               (finalize-cost))
              (not (agents/claim-iteration! environment))
              (merge
                {:status :error
                 :status-id (loop-router/status->id :error)
                 :answer
                 "Subagent stopped: cancelled, iteration budget exhausted, or active-team capacity reached. Retry when capacity is available."
                 :trace trace
                 :iteration-count iteration}
                (finalize-cost))
              :else
              (let
                [route-change (agents/routing-change environment routing-pref-at-turn-start)
                 environment (if route-change
                               (assoc environment
                                 :router (agents/restrict-router environment
                                                                 (loop-router/get-router)))
                               environment)
                 environment (loop-router/hydrate-environment-router environment)
                 routing (if route-change
                           (merge (dissoc routing :provider :model)
                                  (some-> (:preference route-change)
                                          (update :provider keyword)))
                           routing)
                 pre-resolved-model (loop-router/resolve-model-info (:router environment)
                                                                    (:provider routing)
                                                                    (:model routing))
                 raw-reasoning-level (when has-reasoning? base-reasoning-level)
                 reasoning-level (loop-router/copilot-claude-reasoning-level pre-resolved-model
                                                                             user-request
                                                                             raw-reasoning-level)
                 iteration-extra-body (loop-router/provider-extra-body extra-body)
                 ;; The window the NEXT request is actually measured against —
                 ;; the rescued peer's when this turn moved, else the pin's.
                 ;; Priority and history live on `iteration-context-limit`.
                 served-model (when-not route-change (loop-router/turn-served-model environment))
                 request-budget-atom (atom (loop-router/resolved-context-budget
                                             environment
                                             (or served-model pre-resolved-model)
                                             routing
                                             iteration-extra-body))
                 effective-context-limit (loop-router/iteration-context-limit max-context-tokens
                                                                              served-model
                                                                              pre-resolved-model
                                                                              @request-budget-atom)
                 effective-fold-budget (loop-router/context-fold-budget effective-context-limit)
                 _llm-provider-context (cond-> {:selected (transcript/llm-id
                                                            (:provider pre-resolved-model)
                                                            (some-> (:name pre-resolved-model)
                                                                    str))
                                                :routing (cond-> {:fallback? false}
                                                           (seq routing)
                                                           (assoc :request routing))}
                                         (:error llm-provider)
                                         (assoc :error (:error llm-provider)))
                 ;; Canonical history stays intact; folds affect only the provider projection. Each
                 ;; iteration appends its assistant replay and result, including any context change. Stamp
                 ;; the raw universe before applying folds, then price the visible projection.
                 _raw-iter-state (transcript/stamp-iter-universe! (:ctx-atom environment)
                                                                  trailer-iters)
                 replay-target (transcript/replay-context pre-resolved-model)
                 summaries (transcript/current-session-summaries environment)
                 message-base
                 (transcript/prompt-message-base! message-base-atom summaries canonical-messages)
                 messages (:messages message-base)
                 _ (log-stage! :iteration/start
                               iteration
                               {:message-count (count messages)
                                :reasoning reasoning-level
                                :reasoning-effort reasoning-effort
                                :requested-reasoning raw-reasoning-level})
                 transport-summaries (into @emergency-summaries-atom summaries)
                 summarized-trailer-iters (transcript/apply-summaries trailer-iters
                                                                      transport-summaries)
                 _visible-iter-state (transcript/stamp-iter-universe!
                                       (:ctx-atom environment)
                                       trailer-iters
                                       summarized-trailer-iters
                                       {:model (:model replay-target) :replay-target replay-target})
                 ;; An exact carried request already contains every completed prior
                 ;; turn. Keep only live-turn growth until a fold changes the ledger;
                 ;; that one semantic rewrite switches the base to canonical recap.
                 visible-trailer-iters (transcript/conversation-trailer-for-base
                                         summarized-trailer-iters
                                         (:resumed? message-base))
                 conversation-options {:describe-images (transcript/replay-image-describer
                                                          environment
                                                          user-request
                                                          (:provider replay-target))}
                 provider-base (transcript/conversation-messages message-base
                                                                 summarized-trailer-iters
                                                                 replay-target
                                                                 conversation-options)
                 message-token-counter (prompt/request-token-counter (select-keys pre-resolved-model
                                                                                  [:tokenizer]))
                 council-active (when (council/enabled? environment)
                                  (get (council/runtime (:db-info environment)
                                                        (str (:session-id environment)))
                                       (str (:session-id environment))))
                 council-input (when council-active
                                 (council/prepare-input!
                                   (:db-info environment)
                                   (str (:session-id environment))
                                   (:activation-id council-active)
                                   (:group-id council-active)
                                   (:input-state council-active)
                                   [session-turn-id iteration]
                                   ;; Conservative: one UTF-8 byte per spare token, plus headroom.
                                   (max 0
                                        (- (long effective-fold-budget)
                                           (long (message-token-counter
                                                   (or (:name pre-resolved-model)
                                                       (:model pre-resolved-model))
                                                   provider-base))
                                           256))))
                 council-trailer (cond-> (vec trailer-iters)
                                   (council/input-message council-input)
                                   (conj [(inc (long iteration))
                                          {:iteration-scope (str "t" (or turn-position 1)
                                                                 "/i" (inc (long iteration)))
                                           :council-input council-input}]))
                 provider-messages (council/append-input provider-base council-input)
                 effective-messages-atom (atom provider-messages)
                 install-projection! (fn [projection]
                                       (when-let [base (:canonical-base-messages projection)]
                                         (reset! message-base-atom {:messages base
                                                                    :summaries summaries
                                                                    :resumed? false}))
                                       (when-let [summary (:summary projection)]
                                         (swap! emergency-summaries-atom conj summary))
                                       (reset! effective-messages-atom (council/append-input
                                                                         (:messages projection)
                                                                         council-input)))
                 context-estimator (request-context-estimator
                                     @(:prompt-cache-history-atom environment)
                                     (:provider pre-resolved-model)
                                     (or (:name pre-resolved-model) (:model pre-resolved-model))
                                     (transcript/resolved-prompt-cache-context environment
                                                                               pre-resolved-model
                                                                               routing
                                                                               iteration-extra-body)
                                     message-token-counter)
                 _pre-request-fold
                 (when-let [projection (pre-request-context-projection
                                         {:request-messages provider-messages
                                          :base-messages messages
                                          :trailer-iters visible-trailer-iters
                                          :summaries transport-summaries
                                          :replay-target replay-target
                                          :model (or (:name pre-resolved-model)
                                                     (:model pre-resolved-model))
                                          :budget-tokens effective-fold-budget
                                          :count-messages-fn context-estimator
                                          :canonical-base-messages-fn (when (:resumed? message-base)
                                                                        canonical-messages)
                                          :canonical-trailer-iters summarized-trailer-iters})]
                   (install-projection! projection)
                   (tel/log! {:level :info
                              :id ::context-proactive-fold
                              :data
                              (merge
                                (transcript/request-log-context
                                  environment
                                  iteration
                                  {:prompt-base (if (:resumed? message-base) :resumed :canonical)})
                                (dissoc projection :messages :canonical-base-messages :summary))
                              :msg "Context budget reached: compacted history before the request"}))
                 ;; Per-ITERATION rescue counter: escalating context-overflow folds.
                 context-recovery-state (atom {:attempts 0})
                 provider-output-started? (atom false)
                 provider-replay-unsafe? (atom false)
                 effective-messages @effective-messages-atom
                 resolved-model pre-resolved-model
                 ;; Providers still serving an auth cooldown are excluded up front:
                 ;; the per-iteration rescue route below dies with the iteration, so
                 ;; only this seeding keeps a dead credential from being re-probed.
                 effective-routing (loop-router/apply-auth-cooldown-routing routing)
                 ;; Mutates once only when exhausted auth recovery releases a dead provider.
                 iteration-routing (atom effective-routing)
                 applied-routing-preference
                 (atom (if route-change (:preference route-change) routing-pref-at-turn-start))
                 iteration-result
                 ;; Per-iteration retry state. `retries` counts each recovery kind
                 ;; on its own budget (see RETRY_BUDGET_KINDS), so no policy spends
                 ;; another's; `current-extra-body` carries the max-token bump.
                 (with-council-execution
                   environment
                   council-active
                   [session-turn-id iteration]
                   (fn []
                     (loop [retries {:auth 0 :stream 0 :max-tokens 0}
                            current-extra-body iteration-extra-body
                            ;; `env` is threaded so the auth-refresh retry can
                            ;; reseat its `:router` to the rebuilt one (the
                            ;; in-flight env captured the pre-refresh router).
                            env environment]

                       (let
                         [route-change (agents/routing-change env @applied-routing-preference)
                          attempt-routing (if route-change
                                            (merge (dissoc @iteration-routing :provider :model)
                                                   (some-> (:preference route-change)
                                                           (update :provider keyword)))
                                            @iteration-routing)
                          env (if route-change
                                (assoc env
                                  :router (agents/restrict-router env (loop-router/get-router)))
                                env)
                          resolved-model (if route-change
                                           (loop-router/resolve-model-info (:router env)
                                                                           (:provider
                                                                             attempt-routing)
                                                                           (:model attempt-routing))
                                           resolved-model)
                          attempt-env (update (loop-router/hydrate-environment-router
                                                env
                                                (:provider resolved-model))
                                              :router
                                              #(agents/restrict-router env %))
                          _ (when route-change
                              (reset! applied-routing-preference (:preference route-change)))
                          _ (reset! iteration-routing attempt-routing)
                          attempt-base @message-base-atom
                          attempt-summaries (into @emergency-summaries-atom summaries)
                          attempt-trailer (transcript/apply-summaries trailer-iters
                                                                      attempt-summaries)
                          visible-attempt-trailer (transcript/conversation-trailer-for-base
                                                    attempt-trailer
                                                    (:resumed? attempt-base))
                          request-context
                          (transcript/request-log-context
                            attempt-env
                            iteration
                            {:context-recovery-attempt (:attempts @context-recovery-state)
                             :prompt-base (if (:resumed? attempt-base) :resumed :canonical)
                             :base-message-count (count (:messages attempt-base))
                             :trailer-iteration-count (count visible-attempt-trailer)})
                          _fold-estimator
                          (when-let [ca (:ctx-atom attempt-env)]
                            (swap! ca assoc
                              "engine_fold_estimator"
                              (transcript/request-fold-estimator
                                {:message-base-atom message-base-atom
                                 :canonical-messages-fn #(canonical-messages (assoc environment
                                                                               :ctx-atom (atom %)))
                                 :trailer-iters trailer-iters
                                 :emergency-summaries-atom emergency-summaries-atom
                                 :replay-target (transcript/replay-context resolved-model)
                                 :conversation-options conversation-options
                                 :count-messages-fn message-token-counter})))
                          result
                          (try
                            (when (and cancel-atom @cancel-atom)
                              (throw (ex-info "Provider request cancelled"
                                              {:type :svar.core/stream-cancelled})))
                            (reset! provider-output-started? false)
                            (reset! provider-replay-unsafe? false)
                            (run-iteration
                              attempt-env
                              @effective-messages-atom
                              {:iteration iteration
                               :request-context request-context
                               :message-token-counter message-token-counter
                               :input-token-estimator (request-input-token-estimator
                                                        (:prompt-cache-history-atom environment))
                               :reasoning-level reasoning-level
                               :reasoning-effort reasoning-effort
                               :routing @iteration-routing
                               :resolved-model resolved-model
                               :on-response
                               (fn [response]
                                 (transcript/stamp-served-route! environment response)
                                 (when-let [ca (:ctx-atom environment)]
                                   (swap! ca transcript/record-provider-input response))
                                 (let [served (loop-router/resolve-model-info
                                                (:router attempt-env)
                                                (:llm-provider response)
                                                (:llm-model response))]
                                   (reset! request-budget-atom (loop-router/resolved-context-budget
                                                                 attempt-env
                                                                 served
                                                                 @iteration-routing
                                                                 current-extra-body)))
                                 (when-let [input (get-in response [:api-usage :input-tokens])]
                                   (let [window (loop-router/iteration-context-limit
                                                  max-context-tokens
                                                  (loop-router/turn-served-model environment)
                                                  pre-resolved-model
                                                  @request-budget-atom)]
                                     (transcript/stamp-utilization!
                                       (:ctx-atom environment)
                                       (ctx-engine/utilization
                                         input
                                         window
                                         (+ (long (:input-tokens @usage-atom)) (long input))
                                         (loop-router/context-fold-budget window))))))
                               :on-chunk (fn [chunk]
                                           (when (provider-output-chunk? chunk)
                                             (reset! provider-output-started? true)
                                             (when (not= :reasoning (:phase chunk))
                                               (reset! provider-replay-unsafe? true)))
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
                                     (< (long (:max-tokens retries))
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
                                                    :attempt (inc (long (:max-tokens retries)))
                                                    :max-retries MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                                    :prev-max prev-max
                                                    :new-max (:max_tokens bumped)
                                                    :reasoning-length (:reasoning-length data)}}
                                            (str "max_tokens exhausted on reasoning (~"
                                                 (or (:reasoning-length data) "?")
                                                 " reasoning tokens); retry "
                                                 (inc (long (:max-tokens retries)))
                                                 "/" MAX_MAX_TOKENS_EXCEEDED_RETRIES
                                                 " with max_tokens=" (:max_tokens bumped)))
                                  ;; Spend the max-token budget so a second cap-hit
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
                                (and (< (long (:auth retries)) (long MAX_AUTH_REFRESH_RETRIES))
                                     (loop-router/refresh-just-failed? e resolved-model))
                                ::retry-auth-backoff
                                ;; Auth 401/403 from a refreshable provider: adopt a
                                ;; peer credential or persist one forced refresh, then
                                ;; re-send. The exact attempt router supplies the
                                ;; rejected token; the next request boundary hydrates
                                ;; the new value without rebuilding shared routers.
                                (and (< (long (:auth retries)) (long MAX_AUTH_REFRESH_RETRIES))
                                     (loop-router/auth-refreshable-error? e resolved-model)
                                     (loop-router/try-refresh-provider-token! (:router attempt-env)
                                                                              resolved-model))
                                ::retry-auth-refresh
                                ;; Refresh/backoff failed or credentials were revoked.
                                ;; Release the dead provider, then let svar walk the fleet.
                                (loop-router/auth-fallback-routing e
                                                                   @iteration-routing
                                                                   resolved-model)
                                (let [fallback-routing (loop-router/auth-fallback-routing
                                                         e
                                                         @iteration-routing
                                                         resolved-model)
                                      ;; Persist the release ACROSS iterations. Without the
                                      ;; cooldown the next iteration rebuilds routing from
                                      ;; scratch and re-sends to the dead provider.
                                      first-trip? (auth-health/note-failure! (:provider
                                                                               resolved-model))
                                      chunk (provider-retry-progress-chunk
                                              (inc (long iteration))
                                              e
                                              {:provider (:provider resolved-model)
                                               :model (or (:name resolved-model)
                                                          (:model resolved-model))
                                               :reason :authentication-fallback
                                               :attempt 1
                                               :max-retries 1
                                               :delay-ms 0})]

                                  (when first-trip?
                                    (emit-hook! on-chunk
                                                chunk
                                                "Auth fallback progress hook failed"))
                                  (tel/log! {:level (if first-trip? :warn :debug)
                                             :id ::auth-provider-fallback
                                             :data {:iteration iteration
                                                    :provider (:provider resolved-model)
                                                    :cooldown-ms auth-health/AUTH_COOLDOWN_MS
                                                    :status (:status (ex-data e))}}
                                            "Provider auth recovery exhausted; falling back")
                                  {::retry-auth-fallback fallback-routing})
                                ;; Re-issue only this provider call, before code eval. The
                                ;; reset supersedes provisional reasoning; prior tool results
                                ;; remain in the unchanged request. Stop always wins.
                                (and (not (and cancel-atom @cancel-atom))
                                     (or (pre-output-stream-retryable? e
                                                                       {:attempt (:stream retries)
                                                                        :output-started?
                                                                        @provider-output-started?})
                                         (and
                                           (not @provider-replay-unsafe?)
                                           (reasoning-only-stream-retryable? e (:stream retries)))))
                                (let [delay-ms (stream-recovery-backoff-ms (:stream retries))
                                      chunk (provider-retry-progress-chunk
                                              (inc (long iteration))
                                              e
                                              {:provider (:provider resolved-model)
                                               :model (or (:name resolved-model)
                                                          (:model resolved-model))
                                               :reason (if (perr/stream-truncated-error? e)
                                                         :stream-truncated-reasoning
                                                         :stream-watchdog-pre-output)
                                               :attempt (inc (long (:stream retries)))
                                               :max-retries MAX_STREAM_RECOVERY_RETRIES
                                               :delay-ms delay-ms})]

                                  (emit-hook! on-chunk chunk "Stream recovery progress hook failed")
                                  (tel/log! {:level :warn
                                             :id ::stream-recovery-retry
                                             :data {:iteration iteration
                                                    :provider (:provider resolved-model)
                                                    :attempt (inc (long (:stream retries)))
                                                    :max-retries MAX_STREAM_RECOVERY_RETRIES
                                                    :delay-ms delay-ms
                                                    :type (:type (ex-data e))}}
                                            "Retrying provider stream before code execution")
                                  ::retry-stream-recovery)
                                :else
                                (if-let [recovery (context-overflow-recovery!
                                                    {:error e
                                                     :output-started? provider-output-started?
                                                     :recovery-state context-recovery-state
                                                     :ctx-atom (:ctx-atom environment)
                                                     :turn-input-tokens (:input-tokens @usage-atom)
                                                     :request-messages @effective-messages-atom
                                                     :base-messages (:messages attempt-base)
                                                     :trailer-iters visible-attempt-trailer
                                                     :summaries attempt-summaries
                                                     :canonical-base-messages-fn
                                                     (when (:resumed? attempt-base)
                                                       canonical-messages)
                                                     :canonical-trailer-iters attempt-trailer
                                                     :replay-target replay-target
                                                     :model (or (:name resolved-model)
                                                                (:model resolved-model))})]
                                  (do
                                    (install-projection! recovery)
                                    (tel/log!
                                      {:level :warn
                                       :id ::context-overflow-emergency-fold
                                       :data (merge request-context
                                                    (dissoc recovery
                                                      :messages
                                                      :canonical-base-messages
                                                      :summary))
                                       :msg
                                       "Context overflow: retrying with a smaller history projection"})
                                    ::retry-context-overflow)
                                  (do (when (perr/context-overflow-error? e)
                                        (tel/log!
                                          {:level :warn
                                           :id ::context-overflow-terminal
                                           :data (merge request-context
                                                        (transcript/context-overflow-token-data
                                                          (ex-data e))
                                                        {:output-started? @provider-output-started?
                                                         :recovery-attempts
                                                         (:attempts @context-recovery-state)})
                                           :msg
                                           "Context overflow: no emergency-fold retry scheduled"}))
                                      (loop-errors/handle-iteration-exception!
                                        e
                                        {:iteration iteration
                                         :messages @effective-messages-atom
                                         :routing @iteration-routing
                                         :reasoning-level reasoning-level
                                         :stream-recovery
                                         (when (some #(or (perr/stream-truncated-error? %)
                                                          (perr/pre-output-stream-abort? %))
                                                     (loop-errors/bounded-cause-chain e))
                                           {:attempts (:stream retries)
                                            :declined (cond (or @provider-replay-unsafe?
                                                                (= :content
                                                                   (:stream-output (ex-data e))))
                                                            :output-started
                                                            (>= (long (:stream retries))
                                                                (long MAX_STREAM_RECOVERY_RETRIES))
                                                            :retry-budget-exhausted
                                                            :else :not-reasoning-only)})}))))))]

                         (if-let [retries* (next-retry-counters result retries)]
                           (cond (and (map? result) (contains? result ::retry-max-tokens))
                                 (recur retries* (::retry-max-tokens result) env)
                                 (and (map? result) (contains? result ::retry-auth-fallback))
                                 (do (reset! iteration-routing (::retry-auth-fallback result))
                                     (recur retries* current-extra-body env))
                                 (= result ::retry-auth-refresh)
                                 ;; Storage changed (or a peer already changed it). The
                                 ;; next loop pass hydrates this same persistent router
                                 ;; immediately before dispatch.
                                 (recur retries* current-extra-body env)
                                 (= result ::retry-auth-backoff)
                                 ;; Retry the same fresh token; propagation may still be settling.
                                 (do (Thread/sleep (long (loop-router/auth-propagation-backoff-ms
                                                           (:auth retries))))
                                     (recur retries* current-extra-body env))
                                 (= result ::retry-stream-recovery)
                                 ;; Keep the completed history, route and request unchanged.
                                 (do (Thread/sleep (long (stream-recovery-backoff-ms (:stream
                                                                                       retries))))
                                     (recur retries* current-extra-body env))
                                 ;; Context-overflow retry: the installed projection applies.
                                 :else (recur retries* current-extra-body env))
                           result)))))]

                (if-let [iteration-error-data (::loop-errors/iteration-error iteration-result)]
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
                        (let [sticky (some-> (:turn-state-atom environment)
                                             deref
                                             :best-answer
                                             :value)
                              result (merge {:answer sticky
                                             :status :cancelled
                                             :status-id (loop-router/status->id :cancelled)
                                             :trace trace
                                             :iteration-count iteration}
                                            (finalize-cost))]

                          result))
                    (let [llm-provider-error (llm-provider-error-context iteration
                                                                         iteration-error-data)
                          error-feedback
                          (iteration-error-feedback iteration iteration-error-data user-request)
                          trace-entry
                          {:iteration iteration :error iteration-error-data :final? false}
                          ;; Preserve the provider's raw reasoning, content and usage on every failure
                          ;; path. The same reasoning value populates `:thinking` after success.
                          err-data (:data iteration-error-data)
                          err-reasoning (:reasoning err-data)
                          err-partial-content (or (:content err-data) (:partial-content err-data))
                          err-api-usage (or (:api-usage iteration-result) (:api-usage err-data))
                          err-iteration-id
                          (persistance/db-store-iteration!
                            (:db-info environment)
                            (let [tc (iteration-token-cost err-api-usage
                                                           (:name resolved-model)
                                                           (:provider resolved-model))]
                              (cond-> {:session-turn-id session-turn-id
                                       :council-input council-input
                                       :council-publications (:council-publications
                                                               iteration-result)
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
                                       (cond-> {:selected (transcript/llm-id
                                                            (:provider resolved-model)
                                                            (some-> (:name resolved-model)
                                                                    str))
                                                :actual (transcript/llm-id
                                                          (:provider resolved-model)
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
                      ;; A recoverable iteration error remains useful live feedback.
                      ;; Terminal failures instead produce exactly one canonical provider
                      ;; card below; emitting this raw chunk first made the TUI show an
                      ;; unformatted error followed by the formatted terminal card.
                      (when-not (::loop-errors/fatal-iteration-error iteration-result)
                        (emit-hook! on-chunk
                                    {:phase :iteration-error
                                     :iteration (inc (long iteration))
                                     :thinking err-reasoning
                                     :error iteration-error-data
                                     :done? true}
                                    "on-chunk (iteration error)"))
                      (if (::loop-errors/fatal-iteration-error iteration-result)
                        (let
                          [trace' (conj trace trace-entry)
                           fallback
                           (or (some-> (:error trace-entry)
                                       loop-errors/python-error-content)
                               (some-> (:error trace-entry)
                                       loop-errors/user-error-content)
                               (some-> (:error trace-entry)
                                       perr/provider-error-content)
                               [(content/error
                                  "provider_unavailable"
                                  "The model provider failed before Vis received a usable response."
                                  true)])
                           result (merge {:answer fallback
                                          :status :error
                                          :status-id (loop-router/status->id :error)
                                          :trace trace'
                                          :iteration-count (inc (long iteration))}
                                         (finalize-cost))]

                          result)
                        (recur (assoc loop-state
                                 :iteration (inc (long iteration))
                                 :empty-iteration-streak 0
                                 :trailer-iters (compact-trailer council-trailer)
                                 :messages (conj messages {:role "user" :content error-feedback})
                                 :llm-provider {:error llm-provider-error}
                                 :trace (conj trace (store-trace! trace-store trace-entry)))))))
                  (let [_ (note-prompt-cache-status! (:prompt-cache iteration-result))
                        _ (accumulate-usage! (:api-usage iteration-result))
                        ;; The provider that ACCEPTED the request re-enters routing, never
                        ;; the pre-call guess: a turn rescued on a peer used to re-admit the
                        ;; dead credential and the next iteration re-probed it (issue #114).
                        _ (loop-router/note-provider-request-ok! resolved-model iteration-result)
                        ;; …and `session_routing` / `session_utilization` follow the
                        ;; provider that answered, so the model budgets against the
                        ;; window it is now talking to instead of the pin's.
                        _ (transcript/stamp-served-route! environment iteration-result)
                        effective-context-limit (loop-router/iteration-context-limit
                                                  max-context-tokens
                                                  (loop-router/turn-served-model environment)
                                                  pre-resolved-model
                                                  @request-budget-atom)
                        effective-fold-budget (loop-router/context-fold-budget
                                                effective-context-limit)
                        ;; Publish this response's measurement before rendering its
                        ;; context delta. Stamping at the next loop head made the
                        ;; next model request read usage from TWO requests ago.
                        _ (when-let [ca (:ctx-atom environment)]
                            (let [u @usage-atom
                                  window (loop-router/iteration-context-limit
                                           max-context-tokens
                                           (loop-router/turn-served-model environment)
                                           pre-resolved-model
                                           @request-budget-atom)]

                              (transcript/stamp-utilization!
                                ca
                                (ctx-engine/utilization (:last-iter-input u)
                                                        window
                                                        (:input-tokens u)
                                                        (loop-router/context-fold-budget window)))))
                        ;; …and when the pin is the credential that died, the SESSION
                        ;; follows the rescue: the picker chip stops naming a provider
                        ;; this session cannot reach, and the next turn no longer re-pins
                        ;; it just to pay another 401 (issue #154).
                        pick-move (loop-router/reseat-pick-after-auth-rescue! environment
                                                                              iteration-result)
                        _ (when pick-move
                            (emit-hook! on-chunk
                                        (loop-router/pick-moved-chunk (inc (long iteration))
                                                                      pick-move)
                                        "Auth rescue pick-move hook failed"))
                        ;; The move rides this turn's OWN routing trace, which every surface
                        ;; already carries, so the note under the answer explains the chip that
                        ;; changed by itself instead of leaving it a mystery.
                        iteration-result (cond-> iteration-result
                                           pick-move
                                           (update :llm-routing-trace
                                                   (fnil conj [])
                                                   (loop-router/pick-move-event pick-move)))
                        iteration-result (linked-reports/deliver-iteration environment
                                                                           iteration-result)
                        {:keys [thinking assistant-prose goal-continuation blocks final-result]}
                        iteration-result
                        python-error (env/retired-context-error environment)
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
                        ;; One block is one form with its own result and Activity snapshot. Resolve
                        ;; extension tags through the shared Python-name index; unknown heads fall back to
                        ;; the engine's core mutation classifier.
                        py-name->tag (ctx-renderer/fold-op-index (extension/op-tag-index))
                        head-tag-resolver (fn [head-sym]
                                            (when head-sym (get py-name->tag (str head-sym))))
                        ;; A provider reply is either one or more python_execution calls
                        ;; (executable blocks → forms) or a plain-text terminal answer. A
                        ;; no-block iteration therefore has an empty form vector, never a
                        ;; synthetic error artifact.
                        forms-vec (if (seq blocks)
                                    (ctx-engine/blocks->forms blocks cursor head-tag-resolver)
                                    [])
                        block-code (str/join "\n" (keep :code blocks))
                        first-block (or (first blocks) {})
                        ;; Outbound artifacts a tool call PRODUCED this
                        ;; iteration: every artifact a block PRODUCED with
                        ;; `attach`, captured at the SOURCE into the
                        ;; sandbox sink and stamped with the block's tool-call-id, so
                        ;; the image bytes are OWNED by the DB and survive a
                        ;; restart / replay (V1 only kept the temp-file path).
                        iteration-attachments
                        (into (vec (:linked-report-attachments iteration-result))
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
                          (let [tc (iteration-token-cost (:api-usage iteration-result)
                                                         (:llm-model iteration-result)
                                                         (:llm-provider iteration-result))
                                served (loop-router/resolve-model-info
                                         (:router environment)
                                         (:llm-provider iteration-result)
                                         (:llm-model iteration-result))
                                ;; Resolution falls back to the router root for missing entries;
                                ;; that is not evidence of the serving model's input window.
                                known-served? (and (:llm-provider iteration-result)
                                                   (:llm-model iteration-result)
                                                   (= (name (:llm-provider iteration-result))
                                                      (some-> (:provider served)
                                                              name))
                                                   (= (str (:llm-model iteration-result))
                                                      (str (:name served))))
                                ;; Persist the same request budget as live CTX, including
                                ;; output reserve and any tighter caller ceiling.
                                limit (when (or known-served?
                                                (loop-router/token-limit max-context-tokens))
                                        effective-context-limit)
                                budget (loop-router/context-fold-budget limit)]

                            (cond-> {:session-turn-id session-turn-id
                                     :council-input council-input
                                     :council-publications (:council-publications iteration-result)
                                     :request-health
                                     (cond-> (assoc (:request-health iteration-result)
                                               :budget-tokens budget
                                               :reminder-tokens (long (Math/ceil
                                                                        (* 0.75 (double budget)))))
                                       limit
                                       (assoc :model-input-limit limit))
                                     :code (or block-code "")
                                     :forms forms-vec
                                     :attachments (attachment-storage/offload-attachments
                                                    iteration-attachments)
                                     :duration-ms (long (or (form/envelope-duration-ms
                                                              (:envelope first-block))
                                                            0))
                                     :llm-full-duration-ms (long (or (:duration-ms iteration-result)
                                                                     0))
                                     :thinking thinking
                                     :assistant-prose assistant-prose
                                     :answer (when final-result
                                               (transcript/answer-markdown (:answer final-result)))
                                     :llm-provider (or (:llm-provider iteration-result)
                                                       (:provider resolved-model))
                                     :llm-model (:llm-model iteration-result)
                                     :llm-returned-empty-code? (:llm-returned-empty-code?
                                                                 iteration-result)
                                     :llm-routing (transcript/llm-routing-summary pre-resolved-model
                                                                                  iteration-result)
                                     :prompt-cache-reusable-tokens (:prompt-cache-reusable-tokens
                                                                     iteration-result)
                                     :prompt-cache-continuity (:prompt-cache-continuity
                                                                iteration-result)
                                     :cache-created-tokens (iteration-cache-created-tokens tc)}
                              tc
                              (assoc :tokens
                                (:tokens tc) :cost-usd
                                (:cost-usd tc)))))
                        _ (ctx-loop/set-turn-state! environment :iteration-id iteration-id)
                        ;; A view still open when the block ends outlives the collector that
                        ;; block was draining: from here on its record belongs to the
                        ;; ITERATION, which is only nameable now.
                        _ (view/adopt-open-views! iteration-id)
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
                                     :reasoning-effort
                                     (when reasoning-effort
                                       (transcript/reasoning-effort-iteration-evidence
                                         iteration
                                         reasoning-effort
                                         pre-resolved-model
                                         iteration-result))
                                     :final? (boolean final-result)}]

                    (cond
                      (and python-error (not (and cancel-atom @cancel-atom)))
                      (-> (merge {:answer (loop-errors/python-error-content python-error)
                                  :status :error
                                  :status-id (loop-router/status->id :error)
                                  :trace (conj trace (assoc trace-entry :error python-error))
                                  :iteration-count (inc (long iteration))}
                                 (finalize-cost))
                          (transcript/attach-llm-routing-summary pre-resolved-model
                                                                 iteration-result))
                      final-result
                      (do (log-stage! :final
                                      iteration
                                      {:answer (transcript/answer-markdown (:answer final-result))
                                       :iteration-count (inc (long iteration))})
                          (log-stage! :iteration/stop
                                      iteration
                                      {:blocks (count blocks)
                                       :errors (count (filter :error blocks))
                                       :times (mapv transcript/block-duration-ms blocks)})
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
                          (-> (merge {:answer (:answer final-result)
                                      :trace (conj trace trace-entry)
                                      :iteration-count (inc (long iteration))
                                      :utilization (let [u @usage-atom
                                                         req (if (pos? (long (:iter-count u)))
                                                               (long (:last-iter-input u))
                                                               (long (:previous-request-input u)))]

                                                     (ctx-engine/with-prompt-cache-status
                                                       (ctx-engine/utilization
                                                         req
                                                         effective-context-limit
                                                         (:input-tokens u)
                                                         effective-fold-budget)
                                                       @prompt-cache-status-atom))}
                                     (finalize-cost))
                              (transcript/attach-llm-routing-summary pre-resolved-model
                                                                     iteration-result)
                              (assoc :prompt-cache-completion
                                     {:provider (:llm-provider iteration-result)
                                      :model (:llm-model iteration-result)
                                      :messages (:llm-messages iteration-result)
                                      :turn-position (or turn-position 1)
                                      :summaries (transcript/current-session-summaries environment)
                                      :stable-message-count (count stable-prompt-messages)
                                      :assistant-message (:assistant-message iteration-result)})))
                      :else
                      (if (and (empty? blocks) (not goal-continuation))
                        (let [empty-streak (inc (long (or (:empty-iteration-streak loop-state) 0)))]
                          (log-stage! :empty iteration {:empty-streak empty-streak})
                          (log-stage! :iteration/stop iteration {:blocks 0 :errors 0 :times []})
                          (if (>= empty-streak (long CONSECUTIVE_EMPTY_REPLY_LIMIT))
                            ;; Too many consecutive empty replies — finalize on the
                            ;; best sticky answer (give-up text if none) instead of
                            ;; re-invoking forever. Mirrors the forced-finalize shape.
                            (let [goal-halt (goals/request-halt-result environment
                                                                       goal-at-turn-start)
                                  active-goal? (= "active"
                                                  (get (goals/check-goal environment) "status"))
                                  status (or (:status goal-halt) (if active-goal? :error :success))
                                  answer (or (when goal-halt {:answer (:answer goal-halt)})
                                             (when active-goal?
                                               {:answer empty-replies-give-up-text})
                                             (some-> (:turn-state-atom environment)
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
                                                   :status status}
                                           :done? true}))
                              (-> (merge {:answer answer
                                          :status status
                                          :status-id (loop-router/status->id status)
                                          :trace (conj trace trace-entry)
                                          :iteration-count (inc (long iteration))
                                          :utilization
                                          (let [u @usage-atom
                                                req (if (pos? (long (:iter-count u)))
                                                      (long (:last-iter-input u))
                                                      (long (:previous-request-input u)))]

                                            (ctx-engine/with-prompt-cache-status
                                              (ctx-engine/utilization req
                                                                      effective-context-limit
                                                                      (:input-tokens u)
                                                                      effective-fold-budget)
                                              @prompt-cache-status-atom))}
                                         (finalize-cost))
                                  (transcript/attach-llm-routing-summary pre-resolved-model
                                                                         iteration-result)))
                            ;; Transparent auto-continue: re-invoke so a mid-task
                            ;; thinking-only blip turns into real output next round.
                            (recur (merge loop-state
                                          {:iteration (inc (long iteration))
                                           :empty-iteration-streak empty-streak
                                           :trailer-iters (compact-trailer council-trailer)
                                           :trace (conj trace
                                                        (store-trace! trace-store trace-entry))}))))
                        (do
                          (log-stage! :iteration/stop
                                      iteration
                                      {:blocks (count blocks)
                                       :errors (count (filter :error blocks))
                                       :times (mapv transcript/block-duration-ms blocks)})
                          (let [_ blocks
                                ;; ctx-diff for THIS iteration: the standing context
                                ;; AFTER its code ran, captured ONLY if it changed since
                                ;; the model last saw it (this iter started an nREPL,
                                ;; switched model, added a dir, …). It rides INSIDE this
                                ;; iteration's <results> message (see
                                ;; `iteration-results-message`) and advances the running
                                ;; baseline, so the change is attributed to the code that
                                ;; caused it — append-only, no stray context messages.
                                iter-ctx-diff
                                (let [;; util-inclusive: live token usage rides as a cheap
                                      ;; appended `session["utilization"] = …` delta (the
                                      ;; frozen block stays util-free for cache stability).
                                      cur (ctx-loop/render-block! environment
                                                                  ctx-renderer/ctx-delta-map)
                                      prev @last-context-atom
                                      rebase? (true? (:pending? (some-> (:session-rebase-atom
                                                                          environment)
                                                                        deref)))]

                                  (when (and cur (or rebase? (not= cur prev)))
                                    (reset! last-context-atom cur)
                                    (if rebase?
                                      (compaction/rebase-session-context! standing-ctx-atom
                                                                          (:session-rebase-atom
                                                                            environment)
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
                                               {:council-input council-input
                                                :thinking thinking
                                                :goal-continuation goal-continuation
                                                :blocks blocks
                                                ;; `forms-vec` is the one scope source: persistence
                                                ;; and model context both read it.
                                                :forms-vec forms-vec
                                                ;; Outbound image artifacts this iteration's
                                                ;; tool calls produced with `attach`,
                                                ;; each `{:tool-call-id :media-type :base64 …}`.
                                                ;; The conversation-suffix replays them as a
                                                ;; vision user message so the model SEES its
                                                ;; own images within the turn.
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

                            ;; Tool calls and accepted goal progress remain non-terminal;
                            ;; only an accepted answer without active goal work ends the turn.
                            (when on-chunk
                              (on-chunk {:phase :iteration-final
                                         :iteration (inc (long iteration))
                                         :thinking thinking
                                         :assistant-prose assistant-prose
                                         :iteration-id iteration-id
                                         :attachment-count (count iteration-attachments)
                                         :final nil
                                         :done? false}))
                            (recur (merge
                                     (dissoc loop-state :llm-provider)
                                     {:iteration (inc (long iteration))
                                      :empty-iteration-streak 0
                                      :messages messages
                                      :trace (conj trace (store-trace! trace-store trace-entry))
                                      :trailer-iters (compact-trailer next-recent)}))))))))))))))))

(defn iteration-loop
  "Run the core loop with disk-backed trace history, released on every exit path."
  [environment user-request opts]
  (with-trace-store #(iteration-loop* environment user-request (assoc opts ::trace-store %))))
