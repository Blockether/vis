(ns com.blockether.vis.ext.foundation-core.hints
  "Foundation-shipped :turn.iteration/start hook tasks (D12).

   What used to be `:session/hints` is now hook-sourced tasks under
   `:session/tasks`. Two hooks remain:
     - `title-hint`            — set/refresh the session title via
                                  `(set-session-title! \"...\")`
     - `context-pressure-hint` — warn when prompt size crosses ~50%
                                  of window

   Each hook returns a TASK-SHAPE map (the loop wraps it as a full
   `::cs/task` keyed by the hook id):

     {:title        \"<imperative title>\"
      :importance   :info | :warn | :critical
      :validator-fn \"(fn [{:keys [src result error]}] …)\"}

   `:validator-fn` is REQUIRED. The engine evaluates this SCI source
   against the form envelope at `:proof` when the model writes
   `(task-set! id {:status :done :proof \"tN/iM/fK\"})`. Pass → :done
   sticks. Fail → task reverts to :todo + warning."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Shared validator-fn source strings
;; =============================================================================
;;
;; SCI source strings the engine compiles + runs against a form envelope
;; `{:scope :tag :src :form :result :error}` when the model attaches a
;; proof scope to (satisfy-hint! :id [<scope>]). Validators should
;; pattern-match on `:form` (the parsed sexp) and only fall back to
;; `:src` string-matching when parsing failed — string matchers leak
;; false positives from comments / string literals / nested mentions.
;; Defined as top-level constants so the same SCI compile cache entry
;; is reused across iters.

(def ^:const TITLE_VALIDATOR_FN_SRC
  "Validator for `:vis.foundation/session-title`. Proof scope must be a
   form whose HEAD call is `set-session-title!` with a non-blank string
   title. Match is purely structural on `:form` (parsed sexp) — no
   `:src` string fallback. A proof scope whose source did not parse
   cleanly is a broken proof; we fail closed instead of pretending
   string-includes? is a substitute for an AST check.

   Acceptance:
     - no `:error` was raised when the form ran
     - `:form` is a list whose head symbol's name is
       \"set-session-title!\" (namespace ignored — SCI may resolve
       it as `set-session-title!` or `vis/set-session-title!`)
     - the form's first arg is a non-blank string title"
  "(fn [{:keys [form error]}]
     (and (nil? error)
          (seq? form)
          (let [head (first form)
                head-name (when (symbol? head) (name head))
                arg (second form)]
            (and (= \"set-session-title!\" head-name)
                 (string? arg)
                 (not (clojure.string/blank? arg))))))")

(def ^:const CONTEXT_PRESSURE_VALIDATOR_FN_SRC
  "Validator for `:vis.foundation/context-pressure`. The model satisfies
   this hint by calling `(done …)` (turn converges) — optionally with
   `:trailer-summarize` / `:trailer-drop` on the arg map to drop bulky
   prior iters from the trailer.

   Match is purely structural on `:form` (parsed sexp) so a comment
   like `;; will :trailer-drop later` or a string literal carrying
   `done` can never satisfy the hint. No `:src` fallback."
  "(fn [{:keys [form error]}]
     (and (nil? error)
          (seq? form)
          (let [head (first form)
                head-name (when (symbol? head) (name head))]
            (= \"done\" head-name))))")

(def ^:const TITLE_REFRESH_TURN_PERIOD
  "Turn cadence at which `title-hint` fires. BOTH branches —
   blank-title (`:critical`) and refresh-tick (`:info`) — are now
   gated by this cadence; the hint surfaces on iteration 0 of
   turn 1 (first turn of the session), then on iteration 0 of
   every Nth turn after that (10, 20, 30, ...).

   Why blank-title is also cadence-gated now:
   the earlier rule fired `:critical` every iteration whenever the
   title was blank. If the model failed the validator once (wrong
   proof scope) the task reverted to `:todo`, the hint re-fired
   every iter, and `progress.clj` emitted one `Title — …` recap
   row per `(set-session-title! …)` call. Cadence-gating keeps
   the recap to at most one nudge per cadence tick — quiet inside
   any single turn, quiet on turns 2–9, etc.

   Why turn-cadence instead of iteration-cadence:
   the previous iteration-mod-12 rule never fired across a string of
   short single-iteration turns (session 9a55ca1a reproduced
   this). A turn-cadence rule fires once per visible focus shift
   and stays quiet inside any single turn no matter how long it
   runs."
  10)

(def ^:const CONTEXT_PRESSURE_THRESHOLD
  "Fraction of the model's effective input-token budget at which
   `context-pressure-hint` engages. 0.50 lands the hint at ~100k
   under Vis' uniform 200k ceiling - matches z.ai's reported GLM
   sweet spot of 95k-100k input tokens, before accuracy/latency
   degrade. Plenty of headroom remains for the current iteration's
   thinking + tool calls + answer payload."
  0.50)

(defn- turn-cadence-tick?
  "True iff `turn-position` is the first turn or a multiple of
   `TITLE_REFRESH_TURN_PERIOD`. Defensive on non-positive / non-integer
   input so the surrounding hook never crashes the iteration."
  [turn-position]
  (let [tp (long (or turn-position 0))]
    (and (pos? tp)
      (or (= 1 tp)
        (zero? (mod tp TITLE_REFRESH_TURN_PERIOD))))))

(defn title-hint
  "Return a hint map when the session title needs attention,
   otherwise nil. The model's only write path is the host primitive
   `(set-session-title! \"...\")`; there is no in-sandbox read
   binding for the current title (retired as redundant) — this hint
   carries the current value in its text body when refresh-cadence
   fires.

   Inputs are pulled from the host-supplied hint ctx:

     `:session-title` - current trimmed title (nil/blank if unset)
     `:title-refresh?`     - host signalled a turn-boundary refresh check
                             (true on iteration 0 of every turn)
     `:turn-position`      - 1-based turn position inside the session
     `:iteration`          - 1-based iteration position inside the turn
                             (retained for callers that probe the fn,
                              no longer used for cadence)

   Two branches, both cadence-gated:
     blank-title  : `:critical` — fires when title is blank AND the
                    host flagged `:title-refresh?` AND `turn-position`
                    is the first turn or a multiple of
                    `TITLE_REFRESH_TURN_PERIOD`. Quiet on intermediate
                    iterations and intermediate turns.
     refresh-tick : `:info` — fires when title is set AND the host
                    flagged `:title-refresh?` AND the same cadence
                    predicate holds. Otherwise nil."

  [{:keys [session-title title-refresh? turn-position]}]
  (let [blank?  (or (nil? session-title) (str/blank? session-title))
        cadence (and title-refresh? (turn-cadence-tick? turn-position))]
    (cond
      ;; Blank title is a real gap, but we no longer scream every
      ;; iteration. Gate by cadence + iter-0 refresh flag so the
      ;; nudge lands once per cadence tick (turn 1, 10, 20, …) and
      ;; the model is free to skip it on noisy intermediate turns
      ;; without the renderer racking up duplicate `Title — …`
      ;; recap rows. `:critical` keeps the priority loud when the
      ;; nudge actually fires.
      (and blank? cadence)
      {:importance   :critical
       :validator-fn TITLE_VALIDATOR_FN_SRC
       :title (str "Set the session title via bare `(set-session-title! \"...\")` "
                "(3-7-word noun phrase, e.g. \"Refactor auth flow\" or "
                "\"Triage 148 path failures\"). The title is currently empty. "
                "Emit the call as its own top-level form before your first real probe; "
                "do not namespace-qualify it (engine-owned, not a `v/` tool). "
                "Then `(task-set! :vis.foundation/session-title {:status :done :proof \"<scope>\"})`.")}

      ;; Periodic refresh stays :info — the existing title already labels
      ;; the session; this branch only hints when focus may have
      ;; shifted, and is fine to skip.
      (and (not blank?) cadence)
      {:importance   :info
       :validator-fn TITLE_VALIDATOR_FN_SRC
       :title (str "Refresh title if focus has shifted. "
                "Current session title is \"" session-title "\". "
                "You are " turn-position " turn(s) into this session. "
                "Call bare `(set-session-title! \"...\")` (do not namespace-qualify it), "
                "then `(task-set! :vis.foundation/session-title {:status :done :proof \"<scope>\"})`.")})))

(defn context-pressure-hint
  "Return a `:high`-importance hint when the most recent provider
   request's `prompt_tokens` crossed
   `CONTEXT_PRESSURE_THRESHOLD * context-limit`, otherwise nil.

   Inputs from the host-supplied ctx:

     `:input-tokens`             - `prompt_tokens` of the LAST iteration
                                   (proxy for the NEXT request's size).
                                   On iter 0 the host falls back to the
                                   pre-call estimate so the very first
                                   request can still trigger if it is
                                   already over budget.
     `:cumulative-input-tokens`  - sum of `prompt_tokens` across every
                                   iteration of this turn (turn-level
                                   budget signal; rendered in the hint
                                   message but not compared against
                                   `context-limit`).
     `:cumulative-reasoning-tokens` - reasoning tokens this turn so far.
                                   Surfaced for the model's awareness
                                   on `:reasoning-style :server-managed`
                                   providers where the user otherwise has
                                   no visibility into hidden think budget.
     `:iter-count`               - completed-iteration count this turn,
                                   used to phrase the message and to
                                   suppress the hint on the very first
                                   call.
     `:context-limit`            - effective per-call input window for
                                   the resolved model (`:input-limit`
                                   from models.dev when present).

   Why `:input-tokens` is now last-iter and not cumulative:
     The earlier policy compared cumulative `:input-tokens` against
     `context-limit`, conflating two semantically different sizes
     (turn-spend vs per-call cap). Session 3102ad16 (2026-05-20)
     triggered the hint on iteration 13 at ~115K cumulative even
     though each provider request still fit inside ~10K tokens; the
     model started defensively flipping the hook-task
     `:vis.foundation/context-pressure` to :done (pre-D12 the same
     pattern surfaced as defensive `(satisfy-hint! …)` calls) and
     looping. Comparing the SAME-SHAPED number (per-call request size
     vs per-call cap) keeps the hint honest.

   Returns nil when either input is missing or non-positive - the
   hint is purely advisory and must never fail the iteration."
  [{:keys [input-tokens context-limit
           cumulative-input-tokens cumulative-reasoning-tokens iter-count]}]
  (let [used (long (or input-tokens 0))
        limit (long (or context-limit 0))]
    (when (and (pos? used)
            (pos? limit)
            (>= (/ (double used) (double limit))
              CONTEXT_PRESSURE_THRESHOLD))
      (let [pct (long (Math/round (* 100.0 (/ (double used) (double limit)))))
            cum-in (long (or cumulative-input-tokens 0))
            cum-rs (long (or cumulative-reasoning-tokens 0))
            iters  (long (or iter-count 0))
            cumul-clause (when (and (pos? iters) (or (pos? cum-in) (pos? cum-rs)))
                           (str " Turn so far: " iters " iteration(s), ~" cum-in
                             " cumulative input tokens"
                             (when (pos? cum-rs)
                               (str ", ~" cum-rs " reasoning tokens"))
                             " billed."))]
        {:importance   :warn
         :validator-fn CONTEXT_PRESSURE_VALIDATOR_FN_SRC
         :title (str "Converge now: next request is ~" used " / " limit
                  " input tokens (~" pct "%) of this model's effective window."
                  cumul-clause
                  " Finalise via `(done {:answer \"…\"})`, or drop/summarise older "
                  "trailer iters via `(done {:answer … :trailer-summarize [{:scope-start <…> "
                  ":scope-end <…> :summary …}]})`. Then `(task-set! "
                  ":vis.foundation/context-pressure {:status :done :proof \"<scope>\"})`. "
                  "Avoid dumping more file contents, diffs, or repeated diagnostics. "
                  "Models in this family degrade on long tails beyond ~50% of the window.")}))))

;; ----------------------------------------------------------------------------
;; Hooks (`:ext/hooks`) — :turn.iteration/start emits MODEL-FACING
;; `(get-in ctx [:session :hints])` entries. Answer-time enforcement now
;; lives in the harness gate (`final-answer-structural-criteria-errors`); no
;; foundation :turn.answer/validate hooks remain.
;; ----------------------------------------------------------------------------

(def hooks
  "`:ext/hooks` vector for vis-foundation. Each entry conforms to the
   `::hook` spec in `com.blockether.vis.internal.extension`."
  [{:id    :vis.foundation/session-title
    :doc   "Hint the model to set / refresh the session title when it's blank, refresh-flagged, or stale."
    :phase :turn.iteration/start
    :fn    title-hint}
   {:id    :vis.foundation/context-pressure
    :doc   "Warn when assembled input tokens cross ~50% of the model's context window."
    :phase :turn.iteration/start
    ;; Ephemeral signal — the hint reads the LAST iter's prompt_tokens,
    ;; which can swing wildly across turns (heavy refactor turn at 60%
    ;; followed by a one-word "ok" turn at 1%). Letting the resulting
    ;; hook-task survive across turns turned into a cargo-cult bug:
    ;; the model marked it `:done` with `:proof` pointing at the
    ;; `(task-set! …)` form itself, the validator (which expects a
    ;; `(done …)` / trailer-summarize source) said no, the task stayed
    ;; in `:session/tasks` as `:done :validated? false` for 6 turns,
    ;; and the renderer kept surfacing it so the model kept re-marking
    ;; it on every turn including trivial chat replies (Vis conv
    ;; 11d4f817 / t14–t16). `:lifetime :turn` drops the task at
    ;; `advance-turn`; if next turn's input is still over budget the
    ;; hint re-fires and the task is re-created cleanly.
    :lifetime :turn
    :fn    context-pressure-hint}])
