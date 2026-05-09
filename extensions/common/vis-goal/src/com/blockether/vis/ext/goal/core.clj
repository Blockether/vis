(ns com.blockether.vis.ext.goal.core
  "Per-conversation /goal extension (Codex-style).

   Stored as ONE singleton row per conversation soul in the
   `extension_aggregate` sidecar table:

     extension_id   = \"com.blockether.vis.ext.goal.core\"
     aggregate_key  = \"goal\"
     kind           = \"goal-state\"
     scope_key      = \"conversation-soul:<soul-id>\"   (one per soul)
     content        = Nippy blob `{:objective :status :done-reason
                                   :started-at-ms :paused-at-ms
                                   :total-paused-ms :set-by}`

   Public Clojure surface (used by the iteration loop, the TUI footer,
   and the `/goal` slash dispatcher):

     (get-goal       db-info conv-id)            -> goal map or nil
     (set-goal!      db-info conv-id opts)       -> {:objective :set-by}
     (pause-goal!    db-info conv-id)
     (resume-goal!   db-info conv-id)
     (mark-goal-done! db-info conv-id reason)    -> reason \u2208 #{:achieved
                                                              :unmet
                                                              :budget-limited}
     (clear-goal!    db-info conv-id)

   None of these run inside a vis extension callback context, so we
   bypass the `ext-*` runtime-bound API and call the privileged
   `db-*-extension-aggregate*` facade with `:extension-id` filled in
   explicitly. Keeping all of that behind this single namespace is
   the whole reason the goal feature is an extension instead of a
   conversation_soul column.

   Pure logic (state-machine table, paused-aware elapsed-ms math,
   formatters, system-prompt block) lives in
   `com.blockether.vis.internal.goal`. Don't duplicate validators
   here \u2014 import them."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.goal :as goal-pure]
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
   :conversation-soul-id  conv-id
   :scope-key             (str "conversation-soul:" conv-id)})

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- snapshot-from
  "Decorate a stored goal-state map with `:elapsed-ms` (paused-aware,
   relative to `now-ms`) so callers don't have to recompute. Pure.
   Mirrors the shape that pre-extension `db-get-conversation-goal`
   returned, so loop / TUI integration tests can switch backends
   without changing assertions."
  [content now-ms]
  (when content
    (assoc content :elapsed-ms (goal-pure/effective-elapsed-ms content now-ms))))

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
  (when-not (goal-pure/legal-action? from-status action)
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
  (when (> (count objective) goal-pure/MAX_OBJECTIVE_CHARS)
    (throw (ex-info (str "Goal objective exceeds " goal-pure/MAX_OBJECTIVE_CHARS " chars")
             {:type :vis/goal-objective-too-long
              :vis/user-error true
              :length (count objective)
              :max    goal-pure/MAX_OBJECTIVE_CHARS})))
  (when-not (goal-pure/valid-set-by? set-by)
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
;;   1. The extension_aggregate FK / scope_key contract requires a
;;      registered extension id; using an unregistered id silently
;;      stays out of `vis extensions list` output.
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
    (goal-pure/goal-system-prompt-block g (now-ms))))

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
;; `goal/set`, `goal/pause`, etc. via `:sym` override). The shared
;; `:ext/ns-alias` below routes them all to the `goal/` alias.
;; ----------------------------------------------------------------------------

(def ^:private status-symbol
  (vis/symbol #'goal-status
    {:sym 'status
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private set-symbol
  (vis/symbol #'goal-set
    {:sym 'set
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private pause-symbol
  (vis/symbol #'goal-pause
    {:sym 'pause
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private resume-symbol
  (vis/symbol #'goal-resume
    {:sym 'resume
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private clear-symbol
  (vis/symbol #'goal-clear
    {:sym 'clear
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private mark-symbol
  (vis/symbol #'goal-mark
    {:sym 'mark
     :before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def ^:private goal-symbols
  [status-symbol set-symbol pause-symbol resume-symbol clear-symbol mark-symbol])

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
     :ext/ns-alias  {:ns 'vis.ext.goal :alias 'goal}
     :ext/symbols   goal-symbols
     ;; Static fragment injected via the canonical extensions surface.
     ;; No `requiring-resolve` from internal/prompt.clj — the goal
     ;; extension hooks itself in like every other extension.
     :ext/prompt    ext-prompt-fn}))

(vis/register-extension! vis-extension)
