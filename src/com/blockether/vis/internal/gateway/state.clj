(ns com.blockether.vis.internal.gateway.state
  "Gateway session manager (docs/GATEWAY.md §2).

   One process-global registry over the live session fleet: per-session
   ordered event log (monotonic `:seq`, ring-buffered), SSE subscriber
   fan-out, async turn submission with idempotency keys, cancellation,
   and turn/cost metrics.

   The engine is reached ONLY through the same internal surfaces the
   TUI/Telegram channels use: `loop/create!`-`send!`-`close!` for the
   lifecycle, `:hooks {:on-chunk ...}` phased chunks for the live
   stream, `ctx-loop/session-snapshot` for the context. No engine state
   lives here - this namespace owns wire bookkeeping (events, turn
   records, subscribers), nothing else."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
   [com.blockether.vis.internal.gateway.wire :as wire]
   [com.blockether.vis.internal.iteration :as iteration]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.plan-review :as plan-review]
   [com.blockether.vis.internal.render :as ir]
   [taoensso.telemere :as tel]))

(def ^:private EVENT_RING_MAX
  "Per-session event-log ring size. Older events stay durable in the
   session transcript; the ring only backs SSE cursor replay."
  10000)

(def ^:private RESULT_PR_LIMIT 4000)
(def ^:private ERROR_PR_LIMIT 2000)

;; sid (UUID) -> {:next-seq long
;;                :events [event ...]          ; ring, ascending :seq
;;                :subscribers {sub-id fn}     ; SSE sinks
;;                :turns {tid turn-record}     ; :cancel-token stripped on wire
;;                :turn-order [tid ...]
;;                :current-turn tid|nil
;;                :idempotency {key tid}
;;                :last-active epoch-ms}
(defonce ^:private registry (atom {}))

(defonce ^:private metrics
  (atom {:turns-total 0
         :turns-failed 0
         :tokens-input 0
         :tokens-output 0
         :cost-total 0.0
         :duration-ms-total 0
         :per-session {}}))

;; =============================================================================
;; Event log + fan-out
;; =============================================================================

(defn- trim-ring [events]
  (let [n (count events)]
    (if (> n EVENT_RING_MAX)
      (subvec events (- n EVENT_RING_MAX))
      events)))

(defn append-event!
  "Append one event for `sid` and fan it out to subscribers.

   Assigns the next monotonic `:seq` atomically. `:store? false` events
   (per-token reasoning deltas) are fanned out live but kept OUT of the
   replay ring so a cursor replay stays meaningful. A subscriber sink
   that throws is dropped - one dead SSE connection must never poison
   the appender or sibling subscribers."
  ([sid type payload] (append-event! sid type payload {:store? true}))
  ([sid type payload {:keys [store?]}]
   (let [captured (volatile! nil)]
     (swap! registry update sid
       (fn [entry]
         (let [entry (or entry {:next-seq 0})
               n     (inc (:next-seq entry 0))
               event (merge {:schema 1
                             :seq n
                             :ts (System/currentTimeMillis)
                             :session_id (str sid)
                             :type type}
                       payload)]
           (vreset! captured event)
           (cond-> (assoc entry :next-seq n :last-active (System/currentTimeMillis))
             store? (update :events #(trim-ring (conj (or % []) event)))))))
     (let [event @captured]
       (doseq [[sub-id sink] (get-in @registry [sid :subscribers])]
         (try
           (sink event)
           (catch Throwable t
             (swap! registry update-in [sid :subscribers] dissoc sub-id)
             (tel/log! :debug ["gateway: dropped dead subscriber" sub-id (ex-message t)]))))
       event))))

(defn subscribe!
  "Register an SSE sink and return the replay vector (events with
   `:seq` > `cursor`) ATOMICALLY with the registration, so no event can
   fall between replay and live fan-out. The caller serializes replay
   writes against live sink calls (see server.clj)."
  [sid sub-id sink cursor]
  (let [replay (volatile! [])]
    (swap! registry update sid
      (fn [entry]
        (let [entry (or entry {:next-seq 0})]
          (vreset! replay (filterv #(> (:seq %) (or cursor 0)) (:events entry)))
          (assoc-in entry [:subscribers sub-id] sink))))
    @replay))

(defn unsubscribe! [sid sub-id]
  (swap! registry update-in [sid :subscribers] dissoc sub-id)
  nil)

(defn current-seq
  "Highest event `:seq` assigned for `sid` so far. Subscribing with this
   as the cursor yields a live-only stream (empty replay)."
  [sid]
  (get-in @registry [sid :next-seq] 0))

;; =============================================================================
;; Per-session model preference
;; =============================================================================

(defn set-session-model!
  "Set (or clear, with nil/blank) the per-session model preference.
   Every turn submitted for `sid` rides it as the engine's `:model`
   routing preference — `router-for-model` hoists matching models, the
   router order stays as fallback, and an unknown name degrades to the
   default order. Channel-agnostic: any client of the gateway state
   (web, an embedded caller) gets per-session models through this."
  [sid model]
  (let [model (some-> model str str/trim not-empty)]
    (swap! registry update sid
      #(-> (or % {:next-seq 0})
         (assoc :model-pref model
           :last-active (System/currentTimeMillis))))
    model))

(defn session-model
  "The session's model preference, or nil for the router default."
  [sid]
  (get-in @registry [sid :model-pref]))

;; =============================================================================
;; Chunk -> event translation (§8)
;; =============================================================================

(defn- chunk->event
  "Translate one phased iteration chunk (progress.clj contract) into a
   `[type store? payload]` wire event triple."
  [{:keys [phase position code result error silent? done? iteration
           text thinking content] :as chunk}]
  (let [payload
        (case phase
          :reasoning       {:text (or text thinking content)}
          :form-start      {:block_id position :code code}
          :form-result     {:block_id position
                            :code code
                            ;; Result rides the wire the way the MODEL reads it
                            ;; — `render-form-value` (recall window, rg gutter,
                            ;; shell model-render, else the Python printer), NOT
                            ;; pr-str'd Clojure. A bare string stays verbatim.
                            :result (when (some? result)
                                      (wire/bounded-str
                                        (try (ctx-renderer/render-form-value code result)
                                          (catch Throwable _ (wire/bounded-pr result RESULT_PR_LIMIT)))
                                        RESULT_PR_LIMIT))
                            :error (when (some? error) (wire/bounded-pr error ERROR_PR_LIMIT))
                            :silent (boolean silent?)
                            :duration_ms (let [{:keys [started-at-ms finished-at-ms]} (:envelope chunk)]
                                           (when (and (nat-int? started-at-ms) (nat-int? finished-at-ms))
                                             (max 0 (- (long finished-at-ms) (long started-at-ms)))))
                            ;; Tool calls inside the form, as DISPLAY-state ops:
                            ;; the extension render-fn's `{:summary :display}`
                            ;; canonical IR (GATEWAY.md §4.1 ALWAYS IR) — the
                            ;; same rows the TUI paints as `▶ LABEL …`. Clients
                            ;; render these instead of the raw `:result` blob.
                            :ops (when (seq (:channel chunk))
                                   (->> (:channel chunk)
                                     (sort-by :position)
                                     (mapv (fn [entry]
                                             (let [op (iteration/sink-entry->op entry)
                                                   {:keys [started-at-ms finished-at-ms]} op]
                                               {:op      (when-let [o (:op op)]
                                                           (if (keyword? o) (subs (str o) 1) (str o)))
                                                :tag     (some-> (:tag op) name)
                                                :status  (name (:status op))
                                                :summary (:summary op)
                                                :display (:display op)
                                                :duration_ms (when (and (nat-int? started-at-ms)
                                                                     (nat-int? finished-at-ms))
                                                               (max 0 (- (long finished-at-ms)
                                                                        (long started-at-ms))))})))))}
          :iteration-final {:done (boolean done?)}
          :iteration-error {:error (when (some? error) (wire/bounded-pr error ERROR_PR_LIMIT))
                            :thinking thinking}
          {:detail (wire/bounded-pr (dissoc chunk :phase) ERROR_PR_LIMIT)})]
    [(case phase
       :reasoning            "reasoning.delta"
       :form-start           "block.started"
       :form-result          "block.output"
       :iteration-final      "iteration.completed"
       :iteration-error      "iteration.error"
       :provider-retry-reset "provider.retry"
       (str "chunk." (name phase)))
     (not= phase :reasoning)
     (cond-> payload
       (some? iteration) (assoc :iteration iteration))]))

;; =============================================================================
;; Context
;; =============================================================================

(defn context-snapshot
  "The read-only ctx mirror the model sees as its bound `ctx`
   (`ctx-loop/session-snapshot`), for an existing session, ENRICHED for
   the USER with `:session/archived` (the GC'd/summarized entities the
   model itself can only reach through `recall`). Resolving the env
   through `lp/env-for` rehydrates an evicted session on demand.
   nil when the session does not exist."
  [sid]
  (when (lp/by-id sid)
    (when-let [env (lp/env-for sid)]
      (when-let [snapshot (ctx-loop/session-snapshot env)]
        (let [archived (try (ctx-loop/session-archived env) (catch Throwable _ nil))]
          (cond-> snapshot
            (seq archived) (assoc :session/archived archived)))))))

(defn- emit-context-updated! [sid]
  (let [snapshot (try (context-snapshot sid) (catch Throwable _ nil))]
    (when-let [utilization (:session/utilization snapshot)]
      (append-event! sid "context.updated" {:utilization utilization}))))

;; =============================================================================
;; Turn records
;; =============================================================================

(defn- answer-md
  "Normalize a `send!` `:answer` value to a markdown string. Accepts the
   canonical `{:answer md}` from done(), the wrapped `{:result {:answer
   md}}`, a needs-input map, or a plain string (cancel/error surfaces)."
  [answer]
  (cond
    (string? answer) answer
    (and (map? answer) (string? (:answer answer))) (:answer answer)
    (and (map? answer) (string? (get-in answer [:result :answer]))) (get-in answer [:result :answer])
    (nil? answer) nil
    :else (wire/bounded-pr answer RESULT_PR_LIMIT)))

(defn- wire-turn [turn]
  (when turn (dissoc turn :cancel-token)))

(defn get-turn
  "Wire view of one turn record, or nil."
  [sid tid]
  (wire-turn (get-in @registry [sid :turns tid])))

(defn- persisted-turn->wire
  "Map one persisted turn row (`db-list-session-turns` / `row->turn`
   shape) onto the wire turn record, so a reopened session shows its
   full history after a daemon restart — the ENGINE persisted every
   turn; only the gateway's overlay is in-memory."
  [sid row]
  {:turn_id (str (:id row))
   :session_id (str sid)
   :status (let [s (some-> (:status row) name)]
             (if (contains? #{nil "" "running"} s) "completed" s))
   :request (:user-request row)
   :answer_md (:answer-markdown row)
   :iteration_count (:iteration-count row)
   :duration_ms (:duration-ms row)
   :tokens {:input (:input-tokens row) :output (:output-tokens row)}
   :cost (cond-> {:total-cost (:total-cost row)}
           (:model row) (assoc :model (:model row))
           (:provider row) (assoc :provider (:provider row)))
   :started_at (when-let [d (:created-at row)]
                 (when (instance? java.util.Date d) (.getTime ^java.util.Date d)))})

(defn list-turns
  "Wire views of every turn for `sid`, newest first: the PERSISTED turn
   history hydrated from the engine DB (survives daemon restarts), with
   this process's in-memory records as the live overlay (running turns,
   richer terminal payloads) winning by turn id. No `:answer_ir` here —
   fetch the single turn for the IR payload."
  [sid]
  (let [{:keys [turns turn-order]} (get @registry sid)
        live (->> (or turn-order [])
               (keep #(some-> (get turns %) wire-turn (dissoc :answer_ir)))
               vec)
        live-ids (set (map :turn_id live))
        persisted (try
                    (->> (persistance/db-list-session-turns (lp/db-info) sid)
                      (map #(persisted-turn->wire sid %))
                      (remove #(contains? live-ids (:turn_id %)))
                      vec)
                    (catch Throwable t
                      (tel/log! :warn ["gateway: turn-history hydration failed" (ex-message t)])
                      []))]
    ;; persisted rows arrive oldest-first; the wire contract is
    ;; newest-first (the page reverses for display).
    (vec (concat (reverse live) (reverse persisted)))))

(defn- finish-turn! [sid tid patch]
  (swap! registry update sid
    (fn [entry]
      (cond-> (update-in entry [:turns tid] merge patch)
        (= tid (:current-turn entry)) (assoc :current-turn nil)))))

(defn- record-metrics! [sid {:keys [tokens cost duration-ms status]}]
  (let [input (long (or (:input tokens) 0))
        output (long (or (:output tokens) 0))
        cost-total (double (or (:total-cost cost) 0.0))
        duration (long (or duration-ms 0))
        failed? (contains? #{:error :cancelled} status)]
    (swap! metrics
      (fn [m]
        (-> m
          (update :turns-total inc)
          (update :turns-failed (if failed? inc identity))
          (update :tokens-input + input)
          (update :tokens-output + output)
          (update :cost-total + cost-total)
          (update :duration-ms-total + duration)
          (update-in [:per-session (str sid)]
            (fnil (fn [s]
                    (-> s
                      (update :turns inc)
                      (update :tokens-input + input)
                      (update :tokens-output + output)
                      (update :cost-total + cost-total)))
              {:turns 0 :tokens-input 0 :tokens-output 0 :cost-total 0.0})))))))

;; =============================================================================
;; Turn execution
;; =============================================================================

(defn- run-turn!
  "Worker body for one submitted turn. Streams phased chunks into the
   event log, runs the blocking `lp/send!`, then lands the terminal turn
   record + events. Never throws - a worker failure becomes a `failed`
   turn record and a `turn.failed` event."
  [sid tid request {:keys [model reasoning-default cancel-token]}]
  (let [on-chunk (fn [chunk]
                   (try
                     (let [[type store? payload] (chunk->event chunk)]
                       (append-event! sid type (assoc payload :turn_id tid)
                         {:store? store?}))
                     (catch Throwable t
                       (tel/log! :warn ["gateway: chunk translation failed" (ex-message t)]))))]
    (try
      (let [opts   (cond-> {:hooks {:on-chunk on-chunk}
                            :cancel-token cancel-token}
                     model (assoc :model model)
                     reasoning-default (assoc :reasoning-default reasoning-default))
            result (lp/send! sid request opts)
            answer (:answer result)
            needs-input? (= :needs-input (:vis/answer-mode answer))
            md     (answer-md answer)
            answer-ir (when md (try (ir/markdown->ir md) (catch Throwable _ nil)))
            status (cond
                     (= :cancelled (:status result)) "cancelled"
                     (= :error (:status result))     "failed"
                     needs-input?                    "suspended"
                     :else                           "completed")
            patch  {:status status
                    :answer_md md
                    :answer_ir answer-ir
                    :needs_input needs-input?
                    :tokens (:tokens result)
                    :cost (:cost result)
                    :confidence (:confidence result)
                    :iteration_count (:iteration-count result)
                    :duration_ms (:duration-ms result)
                    :finished_at (System/currentTimeMillis)}]
        (finish-turn! sid tid patch)
        (record-metrics! sid result)
        (append-event! sid
          (if (= status "failed") "turn.failed" "turn.completed")
          (-> patch (dissoc :answer_ir) (assoc :turn_id tid)))
        (emit-context-updated! sid))
      (catch Throwable t
        (tel/log! :error ["gateway: turn worker failed" tid (ex-message t)])
        (finish-turn! sid tid {:status "failed"
                               :error (ex-message t)
                               :finished_at (System/currentTimeMillis)})
        (append-event! sid "turn.failed"
          {:turn_id tid :status "failed" :error (ex-message t)})))))

(defn submit-turn!
  "Submit one turn for `sid`. Async: enqueues a worker and returns the
   running turn record immediately.

   Returns `{:turn record}` (plus `:idempotent? true` on an idempotency
   replay) or `{:error :session-not-found | :turn-in-progress |
   :invalid-request, ...}`. One turn per session: the engine already
   serializes via the `send!` ReentrantLock; refusing here keeps the
   gateway contract explicit (409) instead of silently queueing."
  [sid {:keys [request idempotency-key model reasoning-default]}]
  (cond
    (or (not (string? request)) (str/blank? request))
    {:error :invalid-request :message "request must be a non-blank string"}

    (nil? (lp/by-id sid))
    {:error :session-not-found}

    :else
    (let [tid (str (java.util.UUID/randomUUID))
          token (cancellation/cancellation-token)
          model (or model (session-model sid))
          decision (volatile! nil)]
      (swap! registry update sid
        (fn [entry]
          (let [entry (or entry {:next-seq 0})]
            (cond
              (and idempotency-key (get-in entry [:idempotency idempotency-key]))
              (do (vreset! decision [:idempotent (get-in entry [:idempotency idempotency-key])])
                entry)

              (:current-turn entry)
              (do (vreset! decision [:busy (:current-turn entry)])
                entry)

              :else
              (do (vreset! decision [:accepted tid])
                (-> entry
                  (assoc :current-turn tid
                    :last-active (System/currentTimeMillis))
                  (assoc-in [:turns tid]
                    (cond-> {:turn_id tid
                             :session_id (str sid)
                             :status "running"
                             :request request
                             :cancel-token token
                             :started_at (System/currentTimeMillis)}
                      model (assoc :model model)))
                  (update :turn-order (fnil conj []) tid)
                  (cond-> idempotency-key
                    (assoc-in [:idempotency idempotency-key] tid))))))))
      (let [[kind v] @decision]
        (case kind
          :idempotent {:turn (get-turn sid v) :idempotent? true}
          :busy       {:error :turn-in-progress :turn-id v}
          :accepted
          (do
            (append-event! sid "turn.started" {:turn_id tid :request request})
            ;; Virtual-thread worker (cancellation/worker-future), NOT
            ;; clojure.core/future: a blocking LLM turn must never pin a
            ;; platform pool thread, and the worker stays cancellable.
            (cancellation/worker-future (str "gateway-turn-" tid)
              #(run-turn! sid tid request {:model model
                                           :reasoning-default reasoning-default
                                           :cancel-token token}))
            {:turn (get-turn sid tid)}))))))

(defn cancel-turn!
  "Fire the cancellation token of a running turn. Returns
   `{:status \"cancelling\"}` or `{:error ...}`."
  [sid tid]
  (let [turn (get-in @registry [sid :turns tid])]
    (cond
      (nil? turn) {:error :turn-not-found}
      (not= "running" (:status turn)) {:error :not-running :status (:status turn)}
      :else (do (some-> (:cancel-token turn) cancellation/cancel!)
              {:status "cancelling"}))))

(defn approve-turn!
  "Resolve a `suspended` (needs-input / candidate proposal-stop) turn.

   The engine's stop-and-wait model means approval IS the next user
   message: the suspended turn already completed with a needs-input
   answer, so the decision is submitted as a new turn that the engine
   reads against its persisted plan. `decision`: approve | reject |
   edit (`note` carries the rejection reason / revision text) |
   review (`steps` carries per-step verdicts
   `[{:key <step_key> :verdict approve|reject|comment :note <str>?} …]`,
   `note` the overall remark — compiled through the SAME
   `plan-review/review->message` grammar the TUI dialog and web card
   submit, so every surface speaks one review language)."
  [sid tid {:keys [decision note steps]}]
  (let [turn (get-turn sid tid)]
    (cond
      (nil? turn) {:error :turn-not-found}
      (not= "suspended" (:status turn)) {:error :not-suspended :status (:status turn)}
      :else
      (let [request (case decision
                      "approve" "Approved. Proceed with the proposed plan."
                      "reject"  (str "Rejected - do not proceed."
                                  (when-not (str/blank? (str note)) (str " " note)))
                      "edit"    (str "Revise the proposed plan as follows, then proceed: " note)
                      "review"  (plan-review/review->message steps note)
                      nil)]
        (if (nil? request)
          {:error :invalid-request
           :message (if (= "review" decision)
                      "review carried no verdicts and no note"
                      "decision must be approve | reject | edit | review")}
          (submit-turn! sid {:request request}))))))

;; =============================================================================
;; Session lifecycle + souls
;; =============================================================================

(defn create-session!
  "Create a fresh `:api`-channel session. Returns the wire soul."
  [{:keys [title external-id workspace-id]}]
  (let [created (lp/create! :api (cond-> {}
                                   title (assoc :title title)
                                   external-id (assoc :external-id external-id)
                                   workspace-id (assoc :workspace-id workspace-id)))]
    (swap! registry assoc (:id created)
      {:next-seq 0 :last-active (System/currentTimeMillis)})
    {:id (str (:id created))
     :channel "api"
     :title (:title created)
     :external_id (:external-id created)
     :workspace_id (:workspace-id created)}))

(defn soul
  "Wire soul for one session: persisted record + live gateway status."
  [sid]
  (when-let [session (lp/by-id sid)]
    (let [entry (get @registry sid)
          last-turn (some->> (:turn-order entry) peek (get (:turns entry)))]
      {:id (str (:id session))
       :channel (some-> (:channel session) name)
       :title (:title session)
       :model (:model session)
       :external_id (:external-id session)
       :created_at (:created-at session)
       :status (cond
                 (:current-turn entry) "running"
                 (= "suspended" (:status last-turn)) "suspended"
                 :else "idle")
       :current_turn_id (:current-turn entry)
       :last_active_at (:last-active entry)})))

(defn list-sessions
  "Wire souls for every persisted `:api` session."
  []
  (->> (lp/by-channel :api)
    (keep (comp soul :id))
    vec))

(defn close-session!
  "Dispose the live environment and delete the session. Idempotent."
  [sid]
  (try (lp/close! sid) (catch Throwable _ nil))
  (try (lp/delete! sid) (catch Throwable _ nil))
  (swap! registry dissoc sid)
  nil)

(defn set-title! [sid title]
  (when (lp/by-id sid)
    (lp/set-title! sid title)
    (soul sid)))

(defn metrics-snapshot
  "Global + per-session counters for /metrics."
  []
  (let [reg @registry]
    (assoc @metrics
      :sessions-tracked (count reg)
      :turns-running (count (keep :current-turn (vals reg))))))

(defn warm-db!
  "Force the persistence backend + shared connection on the CALLER's
   thread. The gateway runs this on its single-threaded boot path so
   the heavyweight backend namespace never lazy-loads under request
   concurrency (see require-backend-ns! in internal/persistance.clj)."
  []
  (try
    (lp/db-info)
    true
    (catch Throwable t
      (tel/log! :warn ["gateway: db warmup failed" (ex-message t)])
      false)))
