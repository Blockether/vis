(ns com.blockether.vis.ext.foundation.hints
  "Foundation-shipped :turn.iteration/start hints.

   Two soft hints remain:
     - `title-hint`           — set/refresh the conversation title via
                                  `(set-conversation-title! \"...\")`
     - `context-pressure-hint` — warn when prompt size crosses ~50% of window

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

(def ^:const TITLE_REFRESH_TURN_PERIOD
  "Turn cadence at which `title-hint` re-asks the model to refresh
   the conversation title when it is already set. The hint fires on
   iteration 0 of turn 1 (first turn of the conversation), then on
   iteration 0 of every Nth turn after that (10, 20, 30, ...).

   Why turn-cadence instead of iteration-cadence:
   the previous iteration-mod-12 rule never fired across a string of
   short single-iteration turns (conversation 9a55ca1a reproduced
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
  "Return a hint map when the conversation title needs attention,
   otherwise nil. The model's only write path is the host primitive
   `(set-conversation-title! \"...\")`; there is no in-sandbox read
   binding for the current title (retired as redundant) — this hint
   carries the current value in its text body when refresh-cadence
   fires.

   Inputs are pulled from the host-supplied hint ctx:

     `:conversation-title` - current trimmed title (nil/blank if unset)
     `:title-refresh?`     - host signalled a turn-boundary refresh check
                             (true on iteration 0 of every turn)
     `:turn-position`      - 1-based turn position inside the conversation
     `:iteration`          - 1-based iteration position inside the turn
                             (retained for callers that probe the fn,
                              no longer used for cadence)

   Two branches now, no iteration-mod cadence:
     blank-title  : always hint (a missing title is its own case)
     refresh-tick : fires when the host flagged `:title-refresh?` AND
                    `turn-position` is the first turn or a multiple of
                    `TITLE_REFRESH_TURN_PERIOD`."
  [{:keys [conversation-title title-refresh? turn-position]}]
  (let [blank? (or (nil? conversation-title) (str/blank? conversation-title))]
    (cond
      ;; Blank title is a real gap, not a soft suggestion. The title
      ;; is the only label the sidebar / persisted conversation row
      ;; carries; without it the conversation is anonymous. :high
      ;; makes the model actually call `(set-conversation-title! ...)`
      ;; instead of skipping the hint as low-priority advisory noise.
      blank?
      {:importance :high
       :text (str "The conversation title is currently empty. "
               "Set it via bare `(set-conversation-title! \"...\")` (3-7-word noun phrase, "
               "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
               "the conversation is discoverable in the sidebar. "
               "Emit that call as its own top-level form before your first real probe. "
               "Do not namespace-qualify it; it is engine-owned, not a foundation `v/` tool. "
               "Keep host bookkeeping as direct sibling forms so traces stay clean.")}

      ;; Periodic refresh stays :low — the existing title already labels
      ;; the conversation; this branch only hints when focus may have
      ;; shifted, and is fine to skip.
      (and title-refresh? (turn-cadence-tick? turn-position))
      {:importance :low
       :text (str "Current conversation title is \"" conversation-title "\". "
               "You are " turn-position " turn(s) into this conversation. "
               "If the focus has shifted, refresh it via bare "
               "`(set-conversation-title! \"...\")`; do not namespace-qualify it.")})))

(defn context-pressure-hint
  "Return a `:high`-importance hint when the estimated input tokens
   for the assembled prompt-so-far cross
   `CONTEXT_PRESSURE_THRESHOLD * context-limit`, otherwise nil.

   Inputs from the host-supplied ctx:

     `:input-tokens`  - estimated tokens of the assembled prompt-so-far
     `:context-limit` - effective context window for the resolved model

   Returns nil when either input is missing or non-positive - the
   hint is purely advisory and must never fail the iteration."
  [{:keys [input-tokens context-limit]}]
  (let [used (long (or input-tokens 0))
        limit (long (or context-limit 0))]
    (when (and (pos? used)
            (pos? limit)
            (>= (/ (double used) (double limit))
              CONTEXT_PRESSURE_THRESHOLD))
      (let [pct (long (Math/round (* 100.0 (/ (double used) (double limit)))))]
        {:importance :high
         :text (str "Context pressure: ~" used " / " limit " input tokens (~"
                 pct "%) of this model's effective window. "
                 "Converge now - finalise the answer via `(done ...)`, "
                 "avoid dumping more file contents, diffs, or repeated diagnostics. "
                 "Models in this family degrade on long tails beyond ~50% of the window.")}))))

;; ----------------------------------------------------------------------------
;; Hooks (`:ext/hooks`) — :turn.iteration/start emits MODEL-FACING
;; `(:hints ctx)` entries. Answer-time enforcement now lives in the
;; harness gate (`final-answer-structural-criteria-errors`); no
;; foundation :turn.answer/validate hooks remain.
;; ----------------------------------------------------------------------------

(def hooks
  "`:ext/hooks` vector for vis-foundation. Each entry conforms to the
   `::hook` spec in `com.blockether.vis.internal.extension`."
  [{:id    :vis.foundation/conversation-title
    :doc   "Hint the model to set / refresh the conversation title when it's blank, refresh-flagged, or stale."
    :phase :turn.iteration/start
    :fn    title-hint}
   {:id    :vis.foundation/context-pressure
    :doc   "Warn when assembled input tokens cross ~50% of the model's context window."
    :phase :turn.iteration/start
    :fn    context-pressure-hint}])
