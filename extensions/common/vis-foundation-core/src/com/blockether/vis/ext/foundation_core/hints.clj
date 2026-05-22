(ns com.blockether.vis.ext.foundation-core.hints
  "Foundation-shipped :turn.iteration/start hints.

   Two soft hints remain:
     - `title-hint`            — set/refresh the session title via
                                  `(set-session-title! \"...\")`
     - `context-pressure-hint` — warn when prompt size crosses ~50% of window

   Each hint hook now returns a CTX-SHAPE map directly (so the loop can
   `assoc` it onto `:session/hints` without remapping):

     {:body         \"<message body>\"
      :importance   :info | :warn | :critical
      :validator-fn \"(fn [{:keys [src result]}] …)\"}

   `:validator-fn` is REQUIRED — no legacy zero-arity satisfy. The engine
   evaluates this SCI source against each proof scope the model attaches
   to `(satisfy-hint! :id [<scopes>])`; only when ALL scopes pass does
   the hint id drop from `:session/hints`. Hooks that omit `:validator-fn`
   are rejected by the loop with a log warn.

   Previously this namespace also shipped two evidence-related hints
   (`blind-answer-guard-check` and `action-request-needs-evidence-check`)
   that used verb-regex heuristics to fire on investigation/action
   requests with no observed tool work. A later cut retired the legacy
   structural floor inside `(done …)` along with these heuristics:
   single-form iterations either throw (and `(done …)` never runs) or
   succeed (and the answer is observed in the same eval frame), so
   the regex pre-filter no longer earned its complexity."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Shared validator-fn source strings
;; =============================================================================
;;
;; SCI source strings the engine compiles + runs against a form envelope
;; `{:scope :tag :src :result :error}` when the model attaches a proof
;; scope to (satisfy-hint! :id [<scope>]). Defined as top-level
;; constants so the same SCI compile cache entry is reused across iters.

(def ^:const TITLE_VALIDATOR_FN_SRC
  "Validator for `:vis.foundation/session-title`. Proof scope must be a
   form whose source calls `(set-session-title! \"…\")`. Source-string
   match is sufficient — the host fn already has its own argument
   validation; we just confirm the model actually invoked it."
  "(fn [{:keys [src error]}]
     (and (string? src)
       (clojure.string/includes? src \"set-session-title!\")
       (nil? error)))")

(def ^:const CONTEXT_PRESSURE_VALIDATOR_FN_SRC
  "Validator for `:vis.foundation/context-pressure`. The model satisfies
   this hint by either calling `(done …)` (turn converges) or by
   invoking a trailer-summarising form (drops bulky prior iters
   from the trailer). Either action visibly reduces the next request's
   token footprint."
  "(fn [{:keys [src error]}]
     (and (string? src)
       (nil? error)
       (or (clojure.string/starts-with? src \"(done\")
           (clojure.string/includes? src \":trailer-summarize\")
           (clojure.string/includes? src \":trailer-drop\"))))")

(def ^:const TITLE_REFRESH_TURN_PERIOD
  "Turn cadence at which `title-hint` re-asks the model to refresh
   the session title when it is already set. The hint fires on
   iteration 0 of turn 1 (first turn of the session), then on
   iteration 0 of every Nth turn after that (10, 20, 30, ...).

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

   Two branches now, no iteration-mod cadence:
     blank-title  : always hint (a missing title is its own case)
     refresh-tick : fires when the host flagged `:title-refresh?` AND
                    `turn-position` is the first turn or a multiple of
                    `TITLE_REFRESH_TURN_PERIOD`."
  [{:keys [session-title title-refresh? turn-position]}]
  (let [blank? (or (nil? session-title) (str/blank? session-title))]
    (cond
      ;; Blank title is a real gap, not a soft suggestion. The title
      ;; is the only label the sidebar / persisted session row
      ;; carries; without it the session is anonymous. :high
      ;; makes the model actually call `(set-session-title! ...)`
      ;; instead of skipping the hint as low-priority advisory noise.
      blank?
      {:importance   :critical
       :validator-fn TITLE_VALIDATOR_FN_SRC
       :body (str "The session title is currently empty. "
               "Set it via bare `(set-session-title! \"...\")` (3-7-word noun phrase, "
               "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
               "the session is discoverable in the sidebar. "
               "Emit that call as its own top-level form before your first real probe. "
               "Do not namespace-qualify it; it is engine-owned, not a foundation `v/` tool. "
               "Then `(satisfy-hint! :vis.foundation/session-title [<scope-of-that-call>])`. "
               "Keep host bookkeeping as direct sibling forms so traces stay clean.")}

      ;; Periodic refresh stays :info — the existing title already labels
      ;; the session; this branch only hints when focus may have
      ;; shifted, and is fine to skip.
      (and title-refresh? (turn-cadence-tick? turn-position))
      {:importance   :info
       :validator-fn TITLE_VALIDATOR_FN_SRC
       :body (str "Current session title is \"" session-title "\". "
               "You are " turn-position " turn(s) into this session. "
               "If the focus has shifted, refresh it via bare "
               "`(set-session-title! \"...\")`; do not namespace-qualify it. "
               "Then `(satisfy-hint! :vis.foundation/session-title [<scope-of-that-call>])`.")})))

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
     model started defensively `(satisfy-hint! :vis.foundation/
     context-pressure)` and looping. Comparing the SAME-SHAPED number
     (per-call request size vs per-call cap) keeps the hint honest.

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
         :body (str "Context pressure: next request is ~" used " / " limit
                 " input tokens (~" pct "%) of this model's effective window."
                 cumul-clause
                 " Converge now — finalise the answer via `(done {:answer \"…\"})`, "
                 "or drop/summarise older trailer iters via `(done {:answer … "
                 ":trailer-summarize [{:scope-start <…> :scope-end <…> :summary …}]})`. "
                 "Then `(satisfy-hint! :vis.foundation/context-pressure [<scope-of-that-call>])`. "
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
    :fn    context-pressure-hint}])
