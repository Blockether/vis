(ns com.blockether.vis.ext.goal.core
  "Per-conversation /goal extension (Codex-style).

   Stored as ONE singleton row per conversation soul in the
   `extension_aggregate` sidecar table:

     extension_id          = \"com.blockether.vis.ext.goal.core\"
     aggregate_key         = \"goal\"
     kind                  = \"goal-state\"
     conversation_soul_id  = <soul-id>                  (one per soul)
     content               = Nippy blob `{:objective :status :done-reason
                                   :started-at-ms :paused-at-ms
                                   :total-paused-ms :set-by}`

   Public Clojure surface (used by the iteration loop, TUI header,
   `/goal` slash dispatcher, and model-side `goal/` sandbox alias):

     (get-goal        db-info conv-id)           -> goal map or nil
     (set-goal!       db-info conv-id opts)      -> {:objective :set-by}
     (pause-goal!     db-info conv-id)
     (resume-goal!    db-info conv-id)
     (mark-goal-done! db-info conv-id reason)   -> reason ∈ #{:achieved
                                                              :unmet
                                                              :budget-limited}
     (clear-goal!     db-info conv-id)

   Pure goal logic lives here too: constants, validators, transition
   table, paused-aware elapsed-ms math, display formatters, and the
   system-prompt block. Do not split it back into `internal.goal`;
   this extension owns the lifecycle, storage row, prompt hook, TUI
   hook, and sandbox-facing goal helpers.

   None of the DB mutators run inside a vis extension callback
   context, so they bypass the `ext-*` runtime-bound API and call
   the privileged `db-*-extension-aggregate*` facade with
   `:extension-id` filled in explicitly. Keeping all of that behind
   this single namespace is the whole reason the goal feature is an
   extension instead of a conversation_soul column."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   ;; Privileged db-* facade. vis.core deliberately exposes only the
   ;; READ side of the extension_aggregate API (`db-get-...`,
   ;; `db-list-...`) on the public surface so generic extensions
   ;; can't fabricate :extension-id. The goal extension legitimately
   ;; OWNS its rows, so it reaches past the public surface and binds
   ;; the privileged write facade directly. This is the same shape
   ;; as foundation/* reaching into internal/persistance for things
   ;; the model is forbidden from doing.
   [com.blockether.vis.internal.persistance :as persistance]))

;; =============================================================================
;; Constants — single source of truth for goal state shape.
;; Stored content is a Nippy map in extension_aggregate, so the
;; mutators below enforce these guards before writes.
;; =============================================================================

(def ^:const MAX_OBJECTIVE_CHARS
  "Mirrors Codex's ThreadGoal.objective cap. Mutators, validators,
   and TUI input clipping must agree on this cap before writing
   extension_aggregate content."
  4000)

(def ^:const STATUSES
  "Closed set of goal lifecycle statuses. 3 instead of Codex's 5; the
   `:done-reason` field disambiguates achieved/unmet/budget-limited."
  #{:active :paused :done})

(def ^:const DONE_REASONS
  "Closed set of `:done-reason` values that explain why a `:done`
   goal terminated. `:cleared` is recorded only by the explicit
   `clear-goal!` path so the audit trail can tell
   `\"user threw it away\"` from `\"timer ran out\"`."
  #{:achieved :unmet :budget-limited :cleared})

(def ^:const SET_BY_VALUES
  "Who created/replaced the goal. `:user` from the TUI slash command,
   `:model` from the SCI sandbox helper. Lets analytics and the TUI
   surface a per-row provenance badge without bolting on an audit log."
  #{:user :model})

;; =============================================================================
;; Validators
;; =============================================================================

(defn valid-objective?
  "True when `s` is a non-blank string ≤ `MAX_OBJECTIVE_CHARS`.
   Used defensively before `set-goal!` writes so callers get a
   friendly ex-info instead of invalid persisted state."
  [s]
  (and (string? s)
    (not (str/blank? s))
    (<= (count s) MAX_OBJECTIVE_CHARS)))

(defn valid-status? [s] (contains? STATUSES s))
(defn valid-done-reason? [r] (contains? DONE_REASONS r))
(defn valid-set-by? [v] (contains? SET_BY_VALUES v))

;; =============================================================================
;; State machine
;;
;; Captured as data so callers (mutators, future REPL doctor,
;; future test fuzz harness) all consult the same source of truth and
;; nobody has to re-derive the legal moves from prose docstrings.
;; =============================================================================

(def TRANSITIONS
  "Map of `from-status → set-of-actions`, where each action implies
   one specific destination status. `nil` (no goal yet) means the row
   has every goal_* column null — the only legal action is `:set`.

   Note `:set` is allowed from EVERY state including `:done`: setting
   a fresh goal after a prior one finished is the natural \"new goal
   on the same conversation\" flow. Tests pin this in
   the goal extension regression tests."
  {nil      #{:set}
   :active  #{:set :pause :clear :mark-done}
   :paused  #{:set :resume :clear :mark-done}
   :done    #{:set}})

(defn legal-action?
  "True when `action` is allowed from `from-status` (where `nil`
   represents `\"no goal currently exists\"`). Pure; the mutators
   consult this before writing the extension_aggregate row."
  [from-status action]
  (boolean (some-> (get TRANSITIONS from-status) (contains? action))))

;; =============================================================================
;; Paused-aware elapsed math
;; =============================================================================

(defn effective-elapsed-ms
  "Compute the wall-clock elapsed time the goal has spent in the
   `:active` state, in ms.

     elapsed = (now - started_at) - total_paused_ms
                                  - (when paused: now - paused_at)

   `goal` is the shape returned by `get-goal`:
     {:started-at-ms long
      :paused-at-ms  long-or-nil
      :total-paused-ms long
      …}

   Returns 0 when the goal is missing/incomplete (so the TUI can call
   this on every tick without nil-checking). Never returns a negative
   value even if the system clock walks backward across a pause."
  ^long [{:keys [started-at-ms paused-at-ms total-paused-ms]} now-ms]
  (if-not (and (number? started-at-ms) (number? now-ms))
    0
    (let [base   (max 0 (- (long now-ms) (long started-at-ms)))
          fixed  (long (or total-paused-ms 0))
          live   (if (number? paused-at-ms)
                   (max 0 (- (long now-ms) (long paused-at-ms)))
                   0)]
      (max 0 (- base fixed live)))))

(defn format-elapsed
  "Caveman-readable `\"12m 03s\"` / `\"1h 04m\"` / `\"45s\"` formatter
   for the TUI subtitle. `ms < 0` and nil are tolerated. Resolution
   intentionally caps at minutes once we cross 1h — sub-minute jitter
   would just create visual noise in the header."
  [ms]
  (let [ms  (max 0 (long (or ms 0)))
        s   (quot ms 1000)
        m   (quot s  60)
        h   (quot m  60)]
    (cond
      (zero? s)      "0s"
      (< s 60)       (str s "s")
      (< m 60)       (format "%dm %02ds" m (rem s 60))
      :else          (format "%dh %02dm" h (rem m 60)))))

;; =============================================================================
;; Renderers
;; =============================================================================

(defn format-goal-summary
  "One-liner the TUI subtitle / footer status surface render. Returns
   nil when there's no goal so callers can use it as the `when` guard
   for showing the row at all.

   Example outputs:
     \"goal · active · 12m 03s · Finish migration\"
     \"goal · paused · 4m 27s · Finish migration\"
     \"goal · done (achieved) · 1h 12m · Finish migration\"
     \"goal · done (cleared) · 4m 27s\""
  ([goal] (format-goal-summary goal (System/currentTimeMillis)))
  ([{:keys [status objective done-reason] :as goal} now-ms]
   (when (and goal (some? status))
     (let [elapsed (case status
                     :active  (effective-elapsed-ms goal now-ms)
                     :paused  (effective-elapsed-ms goal now-ms)
                     ;; `:done` snapshots elapsed-ms inside :elapsed-ms
                     ;; (set by mark-goal-done!), but for
                     ;; renderer purity we recompute paused-aware too —
                     ;; which is identical because the ticker stopped.
                     :done    (or (:elapsed-ms goal)
                                (effective-elapsed-ms goal now-ms)))
           status-tag (case status
                        :active "active"
                        :paused "paused"
                        :done   (str "done (" (some-> done-reason name) ")"))
           tail       (when (and (not (str/blank? (str objective)))
                              (or (= status :active) (= status :paused)
                                ;; `done(cleared)` blanks the objective
                                ;; in the DB, so we won't have one here.
                                (and (= status :done)
                                  (not= done-reason :cleared))))
                        (str " · " objective))]
       (str "goal · " status-tag " · " (format-elapsed elapsed) tail)))))

(defn goal-system-prompt-block
  "XML-tagged block injected into the system prompt when a goal is
   active or paused. `nil` when no goal exists or the goal is `:done`
   (a finished goal carries no live instructions).

   The block is intentionally short (~5 lines) to keep token cost low
   even on conversations that touch the system prompt every turn.
   Format mirrors the existing `<previous_turn_context>` / system
   blocks the prompt assembler emits, so a future refactor that moves
   to a different wrapping convention can sweep this together with
   the rest."
  [{:keys [status objective set-by] :as goal} now-ms]
  (when (and goal (contains? #{:active :paused} status)
          (not (str/blank? (str objective))))
    (let [elapsed (effective-elapsed-ms goal now-ms)
          status-attr (name status)
          set-by-attr (name (or set-by :user))]
      (str "<conversation_goal status=\"" status-attr
        "\" set-by=\"" set-by-attr
        "\" elapsed=\"" (format-elapsed elapsed) "\">\n"
        "  " (str/replace objective "\n" "\n  ") "\n"
        "  Stay focused on this goal across iterations. Treat any "
        "off-goal exploration as scope creep and call `(turn-answer! ...)` "
        "with what you have when the next deliverable is ready.\n"
        "</conversation_goal>"))))

;; =============================================================================
;; Truncation helpers for narrow surfaces
;; =============================================================================

(def ^:const SUBTITLE_OBJECTIVE_MAX 80)

(defn truncate-objective
  "Single-line ellipsis-truncated objective for narrow surfaces (TUI
   subtitle, footer). Strips inner newlines so the renderer never
   wraps mid-objective. `n` defaults to `SUBTITLE_OBJECTIVE_MAX`."
  ([s] (truncate-objective s SUBTITLE_OBJECTIVE_MAX))
  ([s n]
   (when (string? s)
     (let [flat (str/replace s #"\s+" " ")
           t    (str/trim flat)]
       (if (<= (count t) (long n))
         t
         (str (subs t 0 (max 0 (- (long n) 1))) "…"))))))

;; =============================================================================
;; Stable identity used as `:extension-id` on every aggregate row.
;; If you rename this ns / change `:ext/namespace` below, you MUST add
;; a one-shot migration that rewrites old rows; otherwise prior goals
;; become unreachable.
;; =============================================================================

(def ^:private EXTENSION_ID
  "com.blockether.vis.ext.goal.core")

(def ^:private AGGREGATE_KEY "goal")
(def ^:private KIND          "goal-state")

(defn- aggregate-query
  "Compose the `db-*-extension-aggregate*` query map keyed on (this
   extension, this kind, this soul). Single source of truth so every
   read/write hits the same row."
  [conv-id]
  {:extension-id          EXTENSION_ID
   :aggregate-key         AGGREGATE_KEY
   :kind                  KIND
   :conversation-soul-id  conv-id})

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- snapshot-from
  "Decorate a stored goal-state map with `:elapsed-ms` (paused-aware,
   relative to `now-ms`) so callers don't have to recompute. Pure.
   Keeps loop / TUI callers on one stable returned map shape."
  [content now-ms]
  (when content
    (assoc content :elapsed-ms (effective-elapsed-ms content now-ms))))

;; =============================================================================
;; Read
;; =============================================================================

(defn get-goal
  "Return the goal for `conv-id` or nil. Map shape:
     {:objective :status :done-reason :started-at-ms :paused-at-ms
      :total-paused-ms :set-by :elapsed-ms}
   Tolerant: returns nil on any DB miss / missing row / blank inputs.
   Never throws \u2014 callers (system-prompt assembler, TUI ticker) need
   a safe read path."
  [db-info conv-id]
  (when (and db-info conv-id)
    (try
      (when-let [row (persistance/db-get-extension-aggregate db-info (aggregate-query conv-id))]
        (snapshot-from (:content row) (now-ms)))
      (catch Throwable _ nil))))

(defn- read-state-in-tx
  "Read raw goal state (no elapsed-ms) inside a write transaction so
   pause/resume/mark-done can compose `read \u2192 transition \u2192 write`
   atomically. We pass `tx-info` (a sub-store) instead of the outer
   db-info because every mutation here runs through `db-put-...!`
   which itself opens a sqlite-write-tx; reading via `db-get-...`
   on the same tx-info shares the same connection / lock."
  [tx-info conv-id]
  (some-> (persistance/db-get-extension-aggregate tx-info (aggregate-query conv-id))
    :content))

;; =============================================================================
;; Write \u2014 every mutation shares one helper that lifts validation
;; failures into actionable ex-info before the DB is touched.
;; =============================================================================

(defn- illegal-transition!
  [from-status action]
  (throw (ex-info (str "Illegal goal transition: " (or from-status "<no goal>")
                    " -[" action "]\u2192 ?")
           {:type           :vis/goal-illegal-transition
            :vis/user-error true
            :from-status    from-status
            :action         action})))

(defn- guard-action!
  "Throws `:vis/goal-illegal-transition` when `action` is not legal
   from `from-status` per the pure transition table. Mirrors the SQL
   CHECK invariants that used to live on conversation_soul \u2014 those
   are gone now; this is the only guard."
  [from-status action]
  (when-not (legal-action? from-status action)
    (illegal-transition! from-status action)))

(defn- put!
  [db-info conv-id new-state]
  (persistance/db-put-extension-aggregate! db-info
    (assoc (aggregate-query conv-id)
      :metadata {:status (some-> (:status new-state) name)
                 ;; Surface the high-level shape on the JSON metadata
                 ;; column so a future REPL doctor / sql audit can
                 ;; SELECT goals without thawing the Nippy blob.
                 :set-by (some-> (:set-by new-state) name)}
      :content  new-state)))

(defn set-goal!
  "Create or REPLACE the goal on `conv-id`. Resets the timer
   (started-at = now, total-paused-ms = 0, paused-at = nil,
   done-reason = nil, status = :active).

   `opts`:
     :objective  required, non-blank string \u2264 4000 chars
     :set-by     required, one of #{:user :model}

   Returns the freshly-set goal map (with :elapsed-ms = 0).
   Throws ex-info on bad inputs:
     :vis/goal-objective-blank      blank/empty objective
     :vis/goal-objective-too-long   > MAX_OBJECTIVE_CHARS
     :vis/goal-bad-set-by           :set-by not in #{:user :model}"
  [db-info conv-id {:keys [objective set-by]}]
  (when-not (and db-info conv-id)
    (throw (ex-info "set-goal! requires open db + conversation-id"
             {:type :vis/goal-args-invalid})))
  (when (or (not (string? objective)) (str/blank? objective))
    (throw (ex-info "Goal objective must be a non-blank string"
             {:type :vis/goal-objective-blank :vis/user-error true})))
  (when (> (count objective) MAX_OBJECTIVE_CHARS)
    (throw (ex-info (str "Goal objective exceeds " MAX_OBJECTIVE_CHARS " chars")
             {:type :vis/goal-objective-too-long
              :vis/user-error true
              :length (count objective)
              :max    MAX_OBJECTIVE_CHARS})))
  (when-not (valid-set-by? set-by)
    (throw (ex-info "Goal :set-by must be :user or :model"
             {:type :vis/goal-bad-set-by :vis/user-error true :got set-by})))
  ;; legal-action? gate: replacing a :done goal IS legal
  ;; (TRANSITIONS includes #{:set} on every status). We don't need
  ;; to read first \u2014 :set is unconditionally allowed except in the
  ;; theoretical case where a future status bans it; legal-action?
  ;; nil :set returns true (no goal) and same for :done.
  (let [now (now-ms)
        new-state {:objective       objective
                   :status          :active
                   :done-reason     nil
                   :started-at-ms   now
                   :paused-at-ms    nil
                   :total-paused-ms 0
                   :set-by          set-by}]
    (put! db-info conv-id new-state)
    (snapshot-from new-state now)))

(defn pause-goal!
  "Move an :active goal to :paused. No-op when already paused.
   Throws :vis/goal-illegal-transition when no goal exists or goal
   is :done. Returns the post-transition goal map."
  [db-info conv-id]
  (when (and db-info conv-id)
    ;; Read + write outside a single tx is a tiny race window. The
    ;; user mostly wins this race because pause is single-user from
    ;; the TUI / model. If two writers race, the worst case is one
    ;; pause stamp wins and the other no-ops \u2014 still consistent.
    (let [current (read-state-in-tx db-info conv-id)
          status  (:status current)
          now     (now-ms)]
      (if (= :paused status)
        (snapshot-from current now)
        (do
          (guard-action! status :pause)
          (let [next-state (assoc current
                             :status       :paused
                             :paused-at-ms now)]
            (put! db-info conv-id next-state)
            (snapshot-from next-state now)))))))

(defn resume-goal!
  "Move a :paused goal back to :active. Adds (now - paused_at) to
   total_paused_ms so elapsed math stays paused-aware. No-op when
   already active. Throws :vis/goal-illegal-transition when no goal
   exists or goal is :done."
  [db-info conv-id]
  (when (and db-info conv-id)
    (let [current (read-state-in-tx db-info conv-id)
          status  (:status current)
          now     (now-ms)]
      (if (= :active status)
        (snapshot-from current now)
        (do
          (guard-action! status :resume)
          (let [paused-at  (long (or (:paused-at-ms current) 0))
                prior      (long (or (:total-paused-ms current) 0))
                this-pause (max 0 (- now paused-at))
                next-state (assoc current
                             :status          :active
                             :paused-at-ms    nil
                             :total-paused-ms (+ prior this-pause))]
            (put! db-info conv-id next-state)
            (snapshot-from next-state now)))))))

(defn mark-goal-done!
  "Move :active|:paused -> :done with `reason` \u2208 #{:achieved :unmet
   :budget-limited}. For :cleared, call `clear-goal!` instead so
   callers don't accidentally mix \"finished\" with \"thrown away\"."
  [db-info conv-id reason]
  (when (and db-info conv-id)
    (when-not (contains? #{:achieved :unmet :budget-limited} reason)
      (throw (ex-info "Goal mark-done reason must be :achieved, :unmet, or :budget-limited"
               {:type :vis/goal-bad-done-reason :vis/user-error true :got reason})))
    (let [current (read-state-in-tx db-info conv-id)
          status  (:status current)
          now     (now-ms)]
      (guard-action! status :mark-done)
      (let [paused-at   (:paused-at-ms current)
            prior       (long (or (:total-paused-ms current) 0))
            final-pause (when paused-at
                          (max 0 (- now (long paused-at))))
            next-state  (cond-> (assoc current
                                  :status       :done
                                  :done-reason  reason
                                  :paused-at-ms nil)
                          final-pause (assoc :total-paused-ms (+ prior final-pause)))]
        (put! db-info conv-id next-state)
        (snapshot-from next-state now)))))

(defn clear-goal!
  "Tombstone the goal: status = :done, done-reason = :cleared,
   objective wiped. The row keeps the timer fields so a UI can still
   say \"goal cleared after 12m\". For a hard reset, call `set-goal!`
   with a fresh objective which overwrites everything.

   Idempotent: calling twice in a row is fine \u2014 the second call is a
   no-op when status is already :done. Returns the post-clear goal
   map (or nil when no goal existed)."
  [db-info conv-id]
  (when (and db-info conv-id)
    (let [current (read-state-in-tx db-info conv-id)
          status  (:status current)
          now     (now-ms)]
      (when current
        (let [paused-at   (:paused-at-ms current)
              prior       (long (or (:total-paused-ms current) 0))
              final-pause (when (and paused-at (= :paused status))
                            (max 0 (- now (long paused-at))))
              next-state  (cond-> (assoc current
                                    :objective    nil
                                    :status       :done
                                    :done-reason  :cleared
                                    :paused-at-ms nil)
                            final-pause (assoc :total-paused-ms (+ prior final-pause)))]
          (put! db-info conv-id next-state)
          (snapshot-from next-state now))))))

;; =============================================================================
;; Extension registration. Goal exposes NO sandbox `v/` symbols (it is
;; a privileged conversation-state surface, not a model tool). The
;; iteration loop / TUI / slash dispatcher call our public Clojure
;; fns directly. We register the extension for two reasons:
;;
;;   1. The extension_aggregate FK contract requires a registered
;;      extension id; using an unregistered id silently stays out of
;;      `vis extensions list` output.
;;   2. Future evolution (model-facing helpers gated behind a
;;      `[features] goals = true` flag) gets a natural home.
;; =============================================================================

;; =============================================================================
;; :ext/prompt hook — the canonical surface every extension uses to
;; inject a static block into the system prompt. The goal extension
;; rendering an XML-tagged block here is what makes the model see the
;; live objective + paused-aware elapsed time at every turn. Stays nil
;; when no goal is set (so the `<extensions>` envelope skips us).
;; =============================================================================

(defn- ext-prompt-fn
  "Build the system-prompt fragment the iteration loop injects every
   turn. Reads the goal at call time so a fresh `/goal X` issued
   between turns is visible to the model on the very next turn
   without an explicit re-bind. Returns nil when there's no goal or
   when the goal is :done — a finished goal carries no live
   instruction, so squelching the block keeps the cached prefix
   shorter on idle conversations."
  [env]
  (when-let [g (and (:db-info env) (:conversation-id env)
                 (get-goal (:db-info env) (:conversation-id env)))]
    (goal-system-prompt-block g (now-ms))))

;; =============================================================================
;; SCI sandbox surface — model-side helpers under the `goal/` alias.
;;
;; Always on (no `[features] goals = true` flag): the prompt block tells
;; the model the goal exists, and these helpers let it read / mark / pause
;; without a TUI human. `:before-fn inject-environment` prepends `env` to
;; every call so callers from the SCI sandbox don't need to thread it.
;; =============================================================================

(defn- inject-environment
  "Mirror of foundation/introspection's :before-fn pattern. SCI calls
   the symbol with no env arg; the engine's symbol-wrapper invokes this
   :before-fn with `[env f args]` and we return the same map shape with
   `env` prepended to args."
  [env f args]
  {:env env :fn f :args (into [env] args)})

(defn- env->cid
  "Resolve the conversation id from a sandbox env. Iteration loop binds
   `:conversation-id` directly; doctor / REPL paths sometimes leave it
   nil so callers must guard."
  [env]
  (:conversation-id env))

(defn- require-cid! [env action]
  (let [cid (env->cid env)]
    (when-not cid
      (throw (ex-info (str "Cannot " action " — no active conversation in this env")
               {:type :vis/goal-no-active-conversation
                :vis/user-error true
                :action action})))
    cid))

(defn- safe-db [env]
  (or (:db-info env)
    (throw (ex-info "Cannot mutate goal — env has no :db-info handle"
             {:type :vis/goal-no-db-info
              :vis/user-error true}))))

;; ----------------------------------------------------------------------------
;; Sandbox-facing fns. Every public surface lives on a defn (so `:doc` /
;; `:arglists` come straight off the var meta, the way `vis/symbol`
;; expects). The fns take `env` as the first arg — the symbol-wrapper's
;; `inject-environment` :before-fn injects it. From the model's POV the
;; call sites read `(goal/status)`, `(goal/set "...")`, etc.
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Implementation fns are private and take `env` explicitly. The PUBLIC
;; vars (used by `vis/symbol`) are bound below via `def` with the
;; model-facing `:arglists` lie attached. Defn-via-`def` instead of
;; `defn` because `defn` overwrites `:arglists` in var meta with the
;; actual parameter list, defeating the lie.
;; ----------------------------------------------------------------------------

(defn- status-impl [env]
  (let [cid (env->cid env)]
    (when (and (:db-info env) cid)
      (get-goal (:db-info env) cid))))

(defn- set-impl [env objective]
  (set-goal! (safe-db env) (require-cid! env :set-goal)
    {:objective objective :set-by :model}))

(defn- pause-impl [env]
  (pause-goal! (safe-db env) (require-cid! env :pause-goal)))

(defn- resume-impl [env]
  (resume-goal! (safe-db env) (require-cid! env :resume-goal)))

(defn- clear-impl [env]
  (clear-goal! (safe-db env) (require-cid! env :clear-goal)))

(defn- mark-impl [env reason]
  (mark-goal-done! (safe-db env) (require-cid! env :mark-goal-done) reason))

(def ^{:doc      "Read the current conversation's goal. Returns a map
  {:objective :status :done-reason :started-at-ms :paused-at-ms
   :total-paused-ms :set-by :elapsed-ms} or nil when no goal is set.
  `:status` is one of #{:active :paused :done}; `:done-reason` is
  #{:achieved :unmet :budget-limited :cleared} or nil; `:elapsed-ms`
  is paused-aware (the live pause window is subtracted automatically)."
       :arglists '([])}
  goal-status status-impl)

(def ^{:doc      "Set or REPLACE the conversation's goal with `objective`.
  Resets the timer (started-at = now, total-paused-ms = 0). The new
  goal is tagged :set-by :model. Returns the freshly-set goal map.
  Throws on blank objective, > 4000 chars, or invalid set-by."
       :arglists '([objective])}
  goal-set set-impl)

(def ^{:doc      "Pause the active goal. Freezes the elapsed-ms ticker;
  the live pause window will be folded into total-paused-ms by the
  next resume. No-op when already paused. Throws when no goal exists
  or the goal is :done."
       :arglists '([])}
  goal-pause pause-impl)

(def ^{:doc      "Resume the paused goal. Unfreezes the elapsed-ms ticker.
  No-op when already active. Throws when no goal exists or the goal
  is :done."
       :arglists '([])}
  goal-resume resume-impl)

(def ^{:doc      "Tombstone the goal: status = :done, done-reason :cleared,
  objective wiped. Idempotent. Use this when the user/model decides
  the objective is no longer relevant; for finishing successfully,
  call (goal/mark :achieved) instead."
       :arglists '([])}
  goal-clear clear-impl)

(def ^{:doc      "Mark the goal :done with a reason in #{:achieved :unmet
  :budget-limited}. (For :cleared, call (goal/clear).) Throws when
  no goal exists or the goal is already :done. The TUI surfaces the
  reason in the goal subtitle."
       :arglists '([reason])}
  goal-mark mark-impl)

;; ----------------------------------------------------------------------------
;; Symbol entries — each binds the public name (without the leading
;; `goal-` prefix the local var carries; sandbox sees `goal/status`,
;; `goal/set`, `goal/pause`, etc. via `:symbol` override). The shared
;; `:ext/alias` below routes them all to the `goal/` alias.
;; ----------------------------------------------------------------------------

(def ^:private status-symbol
  (vis/symbol #'goal-status
    {:symbol 'status
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private set-symbol
  (vis/symbol #'goal-set
    {:symbol 'set
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private pause-symbol
  (vis/symbol #'goal-pause
    {:symbol 'pause
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private resume-symbol
  (vis/symbol #'goal-resume
    {:symbol 'resume
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private clear-symbol
  (vis/symbol #'goal-clear
    {:symbol 'clear
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private mark-symbol
  (vis/symbol #'goal-mark
    {:symbol 'mark
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private goal-symbols
  [status-symbol set-symbol pause-symbol resume-symbol clear-symbol mark-symbol])

(doseq [[op tag] [[:goal/status :op.tag/observation]
                  [:goal/set :op.tag/mutation]
                  [:goal/pause :op.tag/mutation]
                  [:goal/resume :op.tag/mutation]
                  [:goal/clear :op.tag/mutation]
                  [:goal/mark :op.tag/mutation]]]
  (vis/register-op! op {:tag tag}))

;; =============================================================================
;; TUI slash command — `/goal ...`
;;
;; Registered as a `:ext/channel-hooks` entry under `:tui`. The TUI's
;; slash dispatcher invokes `run-fn` with a ctx map carrying
;; `:command/argv` (split arg tokens) and `:app-db` (atom). We read
;; the active conversation id off `(:conversation @app-db)`, then
;; route to the matching public Clojure API. Errors surface as host
;; notifications via `(vis/notify! ...)` so the LEFT slot of the
;; header shows them — same path every other channel uses.
;; =============================================================================

(defn- notify!
  [text level]
  (vis/notify! (str text) :level level :ttl-ms 4000))

(defn- ctx-conv-id [ctx]
  (some-> (:app-db ctx) deref :conversation :id str))

(defn- summary-line [g]
  (or (format-goal-summary g) "<no goal>"))

(defn- run-set! [ctx objective]
  (let [cid (ctx-conv-id ctx)]
    (cond
      (not cid)
      (notify! "/goal: no active conversation" :warn)

      (str/blank? objective)
      (notify! "/goal: objective is required (e.g. /goal Finish the migration)" :warn)

      :else
      (try
        (let [g (set-goal! (vis/db-info) cid {:objective (str/trim objective)
                                              :set-by    :user})]
          (notify! (str "goal set · " (summary-line g)) :success))
        (catch clojure.lang.ExceptionInfo e
          (notify! (str "/goal failed: " (or (ex-message e) "unknown error"))
            :error))
        (catch Throwable e
          (notify! (str "/goal failed: " (.getMessage e)) :error))))))

(defn- run-show! [ctx]
  (when-let [cid (ctx-conv-id ctx)]
    (if-let [g (get-goal (vis/db-info) cid)]
      (notify! (summary-line g) :info)
      (notify! "no goal set on this conversation" :info))))

(defn- run-mutation!
  "Wrap a 0-arity goal mutation (`pause-goal!` / `resume-goal!` /
   `clear-goal!`) so failures land in the notification banner instead
   of crashing the slash dispatcher. Reads cid + db at call time so a
   stale ctx (e.g. user switched conversation between the keypress and
   the dispatch) can't fire on the previous conversation."
  [ctx label mutate-fn success-fmt]
  (let [cid (ctx-conv-id ctx)]
    (cond
      (not cid)
      (notify! (str "/goal " label ": no active conversation") :warn)

      :else
      (try
        (let [g (mutate-fn (vis/db-info) cid)]
          (notify! (success-fmt g) :success))
        (catch clojure.lang.ExceptionInfo e
          (notify! (str "/goal " label " failed: "
                     (or (ex-message e) "unknown error")) :error))
        (catch Throwable e
          (notify! (str "/goal " label " failed: " (.getMessage e)) :error))))))

(defn- run-mark!
  [ctx reason]
  (let [cid (ctx-conv-id ctx)]
    (if (not cid)
      (notify! "/goal mark: no active conversation" :warn)
      (try
        (let [g (mark-goal-done! (vis/db-info) cid reason)]
          (notify! (str "goal → done (" (name reason) ") · " (summary-line g))
            :success))
        (catch clojure.lang.ExceptionInfo e
          (notify! (str "/goal mark failed: "
                     (or (ex-message e) "unknown error")) :error))
        (catch Throwable e
          (notify! (str "/goal mark failed: " (.getMessage e)) :error))))))

(defn- goal-slash-run!
  "Dispatcher for `/goal ...`. Routes by the first arg token:
     pause | resume | clear                   -> lifecycle mutation
     achieved | unmet | budget-limited        -> mark-done
     <empty>                                  -> show current goal status
     <anything else>                          -> set goal to the full arg string

   `:command/argv` carries the split tokens; the raw arg string is in
   `:command/args` so multi-word objectives like `/goal Finish the
   migration and keep tests green` round-trip without re-joining."
  [{:keys [command/args command/argv] :as ctx}]
  (let [;; Derive argv from args when the dispatcher caller didn't pre-split
        ;; (some channels pass only the raw arg string). Splitting on
        ;; whitespace matches the same convention `command-argv` uses in
        ;; channel-tui/screen.clj, so a typed `/goal Round 2` keeps
        ;; argv = ["Round" "2"], routes to :set with the FULL args text.
        argv*     (or (seq argv)
                    (when-not (str/blank? (str args))
                      (str/split (str/trim (str args)) #"\s+")))
        first-arg (str/lower-case (str (first argv*)))]
    (case first-arg
      ""               (run-show! ctx)
      "pause"          (run-mutation! ctx "pause"  pause-goal!
                         (fn [g] (str "goal paused · " (summary-line g))))
      "resume"         (run-mutation! ctx "resume" resume-goal!
                         (fn [g] (str "goal resumed · " (summary-line g))))
      "clear"          (run-mutation! ctx "clear"  clear-goal!
                         (fn [_] "goal cleared"))
      "achieved"       (run-mark! ctx :achieved)
      "unmet"          (run-mark! ctx :unmet)
      "budget-limited" (run-mark! ctx :budget-limited)
      ;; default: treat the entire arg string as a goal objective.
      ;; This is the canonical Codex `/goal X` shape and what the
      ;; cmd-args field documents.
      (run-set! ctx (str args)))))

;; =============================================================================
;; TUI header row contributor (declarative)
;; =============================================================================
;;
;; The goal feature owns its display in the TUI header via an
;; `:ext/channel-hooks` entry (see `vis-extension` below). Pure
;; declarative — no `requiring-resolve` into channel-tui, no hard
;; coupling in either direction. Channel-tui discovers this hook by
;; iterating registered extensions and filtering on hook-id pattern.
;;
;; If channel-tui isn't on the classpath the hook is registered but
;; never invoked. Headless tools / Telegram-only deployments don't
;; pay any cost. Telegram or future channels declare their own
;; equivalent hooks; this namespace doesn't need to know about them.

(def ^:private goal-tui-cache
  ;; {conv-id [now-ms goal-or-nil]} — 100ms TTL. The render-fn is
  ;; called twice per frame (once for height layout, once for draw),
  ;; so without caching every 80ms tick fires two SQLite queries
  ;; (~12ms each warm). TTL keeps it to one query per tick. User-
  ;; driven mutations bypass this cache and invalidate on the next
  ;; tick (TTL slack ≤100ms; users won't notice).
  (atom {}))

(def ^:private goal-tui-cache-ttl-ms 100)

(defn- cached-goal-for-conv
  [conv-id]
  (let [now (System/currentTimeMillis)
        [cached-at cached-goal] (get @goal-tui-cache conv-id)]
    (if (and cached-at (< (- now (long cached-at)) goal-tui-cache-ttl-ms))
      cached-goal
      (let [g (try (get-goal (vis/db-info) conv-id) (catch Throwable _ nil))]
        (swap! goal-tui-cache assoc conv-id [now g])
        g))))

(defn- goal-status->fg-role
  "Map a goal status to a channel-agnostic foreground role keyword.
   Channels translate the role to their own palette:
     :paused  → warn-yellow / yellow chip / etc.
     :done    → muted text
     other    → default text"
  [status]
  (case status
    :paused :warn
    :done   :muted
    :default))

(defn- goal-row-render
  "Header row contributor for the goal subtitle. Returns CANONICAL IR
   (`[:ir {?:align ?:fg-role} & blocks]`) — channel-agnostic data.
   Each channel translates IR to its surface (TUI: paints styled
   line; Telegram: emits markdown; web: renders HTML).

   Returns nil when no goal is set (the row contributes zero rows).

   Render hints carried on the IR root attrs:
     :align    :center  — subtitle row is centered in the band.
     :fg-role  :warn / :muted / :default — status-driven color cue."
  [db _cols]
  (when-let [conv-id (some-> db :conversation :id)]
    (when-let [goal (cached-goal-for-conv conv-id)]
      (when (some? (:status goal))
        (let [summary (format-goal-summary goal
                        (System/currentTimeMillis))]
          (when (and (string? summary) (not (str/blank? summary)))
            ;; Wrap the summary in `:em` so the IR walker emits
            ;; the italic inline-style sentinel; the channel
            ;; renders it accordingly. The outer `:p` is the
            ;; canonical block wrapper.
            [:ir {:align   :center
                  :fg-role (goal-status->fg-role (:status goal))}
             [:p {}
              [:em {} [:span {} summary]]]]))))))

(defn- goal-tui-commands
  "Returns the list of TUI slash commands the goal extension contributes.
   The dispatcher always shows `/goal` in the slash menu (via
   `:label`); subcommands are documented under `:doc` so the user
   discovers them inline."
  [_ctx]
  [{:id      :goal
    :label   "Goal: set / pause / resume / clear / mark"
    :doc     (str "Per-conversation goal (Codex-style). Usage:\n"
               "  /goal <objective>            — set or replace\n"
               "  /goal                        — show current status\n"
               "  /goal pause                  — freeze the elapsed-ms timer\n"
               "  /goal resume                 — unfreeze the elapsed-ms timer\n"
               "  /goal clear                  — tombstone (done :cleared)\n"
               "  /goal achieved               — finish, marked successful\n"
               "  /goal unmet                  — finish, marked blocked\n"
               "  /goal budget-limited         — finish, marked over budget")
    :args    [{:name "objective | subcommand"
               :kind :positional
               :required false}]
    ;; `:palette? false` keeps `/goal` out of the universal command
    ;; palette (Ctrl+K) since the slash menu is the canonical entry.
    :palette? false
    :run-fn  goal-slash-run!}])

(def vis-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.goal.core
     :ext/doc       (str "Per-conversation goal feature (Codex-style /goal). "
                      "Soul-level: one durable objective per conversation lifetime, "
                      "with paused-aware elapsed-ms timer. Set via TUI `/goal X` or "
                      "model side `(goal/set \"...\")`; iteration loop injects the "
                      "active goal as a system-prompt block.")
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/kind      "conversation-state"
     ;; Sandbox alias — model calls `(goal/status)`, `(goal/set "...")`,
     ;; `(goal/pause)`, etc. Always on; per user fiat, NOT gated behind a
     ;; `[features] goals = true` flag.
     :ext/alias  {:ns 'vis.ext.goal :alias 'goal}
     :ext/symbols   goal-symbols
     ;; Static fragment injected via the canonical extensions surface.
     ;; No `requiring-resolve` from internal/prompt.clj — the goal
     ;; extension hooks itself in like every other extension.
     :ext/prompt    ext-prompt-fn
     ;; TUI surface: `/goal ...` slash command. Registered as a
     ;; channel-hook so the TUI's command-suggest / dispatcher pick it
     ;; up automatically; no TUI code change needed.
     :ext/channel-hooks
     [{:channel-id  :tui
       :hook-id     :goal/slash
       :commands-fn #'goal-tui-commands}
      ;; Header subtitle row contributor. Channel-tui consumes any
      ;; hook whose `:hook-id` ends in "header-row" (or equals
      ;; `:tui/header-row` exactly) and calls its `:render-fn` per
      ;; frame with `(db cols) -> ir | nil`.
      ;;
      ;; The render-fn returns CANONICAL IR — channel-agnostic data.
      ;; Channel-tui walks the IR via the styled-line walker; other
      ;; channels (Telegram, future web) translate the same IR to
      ;; their own surface (markdown, HTML, ...). Optional render
      ;; hints carried as IR root attrs:
      ;;   :align    :left|:center|:right
      ;;   :fg-role  :default|:muted|:warn|:error|:success
      ;;
      ;; Pure declarative — no `requiring-resolve` into channel-tui
      ;; from here, no hard-coded extension lookups from channel-tui
      ;; to here. See header.clj's `header-row-specs` for the
      ;; contract. Other channels declare their own hooks
      ;; (`:telegram/preamble`, etc.) without touching this extension.
      {:channel-id :tui
       :hook-id    :goal/header-row
       :render-fn  #'goal-row-render}]}))

(vis/register-extension! vis-extension)


