(ns com.blockether.vis.internal.goal
  "Pure (no-I/O) helpers for the per-conversation /goal feature.

   Side-effecting goal operations live in
   `com.blockether.vis.internal.persistance` (DB writes) and
   `com.blockether.vis.ext.channel-tui.*` (UI surface). This namespace
   carries:

     * the canonical goal data shape returned by the persistance layer
       (a Clojure map, NOT a defrecord, so pi-shaped sandbox `pr-str`
       previews and JSON serialization stay simple);
     * the legal status-transition table the API layer consults before
       writing;
     * paused-aware elapsed-ms math the TUI ticker re-runs every second
       without round-tripping the DB;
     * the system-prompt block the iteration loop injects so the model
       sees the active goal as live context.

   Why this lives in `internal/` and not `extensions/foundation`:
   foundation/* is sandbox-facing — it gets stitched into the SCI
   sandbox under the `v/` alias and would expose every fn here as a
   sandbox-callable surface. Goal lifecycle is privileged (writes a
   timestamp, transitions DB state); SCI helpers must call into the
   slimmer `com.blockether.vis.core/db-*-conversation-goal*` facade
   instead. Keep this ns out of the sandbox compose."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Constants — kept in lockstep with the SQL CHECK constraints in
;; V1__schema.sql and the API-layer guards in
;; extensions/persistance/.../core.clj. If you bump one, bump all three.
;; =============================================================================

(def ^:const MAX_OBJECTIVE_CHARS
  "Mirrors Codex's ThreadGoal.objective cap. Schema CHECK + API throw +
   this constant must agree. Used by validators here and by the TUI
   composer to clip pasted input before submission."
  4000)

(def ^:const STATUSES
  "Closed set of goal lifecycle statuses. 3 instead of Codex's 5; the
   `:done-reason` field disambiguates achieved/unmet/budget-limited."
  #{:active :paused :done})

(def ^:const DONE_REASONS
  "Closed set of `:done-reason` values that explain why a `:done`
   goal terminated. `:cleared` is recorded only by the explicit
   `db-clear-conversation-goal!` path so the audit trail can tell
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
  "True when `s` is a non-blank string ≤ `MAX_OBJECTIVE_CHARS`. The
   API-layer setter should call this defensively even though the SQL
   CHECK enforces the upper bound — a friendly ex-info beats a
   sqlite constraint error."
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
;; Captured as data so callers (persistance API, future REPL doctor,
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
   `conversation-goal-replace-test`."
  {nil      #{:set}
   :active  #{:set :pause :clear :mark-done}
   :paused  #{:set :resume :clear :mark-done}
   :done    #{:set}})

(defn legal-action?
  "True when `action` is allowed from `from-status` (where `nil`
   represents `\"no goal currently exists\"`). Pure; the persistance
   layer consults this before mutating, then the schema's CHECK
   constraints catch any race that slips past."
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

   `goal` is the shape returned by `db-get-conversation-goal`:
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
                     ;; (set by persistance/db-mark-…-done!), but for
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
        "off-goal exploration as scope creep and call `(answer ...)` "
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
