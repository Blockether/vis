(ns com.blockether.vis.ext.foundation.nudges
  "Foundation-shipped :turn.iteration/start hints.

   Two soft nudges remain:
     - `title-nudge`           — set/refresh the conversation title via
                                  `(set-conversation-title! \"...\")`
     - `context-pressure-nudge` — warn when prompt size crosses ~50% of window

   Previously this namespace also shipped two evidence-related hints
   (`blind-answer-guard-check` and `action-request-needs-evidence-check`)
   that used verb-regex heuristics to fire on investigation/action
   requests with no observed tool work. Both are now subsumed by the
   harness-level structural gate inside `(done ...)`
   (see `final-answer-structural-criteria-errors` in
   `com.blockether.vis.internal.loop`): if the latest iteration's
   <journal> carries no evidence, the answer is refused regardless of
   user-request shape. Removing the nudges keeps a single source of
   truth and drops the regex heuristic surface."
  (:require
   [clojure.string :as str]))

(def ^:const TITLE_REFRESH_TURN_PERIOD
  "Turn cadence at which `title-nudge` re-asks the model to refresh
   the conversation title when it is already set. The nudge fires on
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
   `context-pressure-nudge` engages. 0.50 lands the nudge at ~100k
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

(defn title-nudge
  "Return a nudge map when the conversation title needs attention,
   otherwise nil. The model's only write path is the host primitive
   `(set-conversation-title! \"...\")`; there is no in-sandbox read
   binding for the current title (retired as redundant) — this nudge
   carries the current value in its text body when refresh-cadence
   fires.

   Inputs are pulled from the host-supplied nudge ctx:

     `:conversation-title` - current trimmed title (nil/blank if unset)
     `:title-refresh?`     - host signalled a turn-boundary refresh check
                             (true on iteration 0 of every turn)
     `:turn-position`      - 1-based turn position inside the conversation
     `:iteration`          - 1-based iteration position inside the turn
                             (retained for callers that probe the fn,
                              no longer used for cadence)

   Two branches now, no iteration-mod cadence:
     blank-title  : always nudge (a missing title is its own case)
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
               "Set it via `(set-conversation-title! \"...\")` (3-7-word noun phrase, "
               "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
               "the conversation is discoverable in the sidebar. "
               ;; Bundle with the first real probe so we don't spend a
               ;; whole iteration on metadata. Title-setting is
               ;; structurally silent: no extension tool, no journal
               ;; evidence — by itself it's a wasted round-trip.
               "Bundle this call into the SAME iteration as your first "
               "real probe, e.g. "
               "`(do (set-conversation-title! \"...\") (def x (v/cat \"...\")))`, "
               "so the iteration also carries evidence.")}

      ;; Periodic refresh stays :low — the existing title already labels
      ;; the conversation; this branch only nudges when focus may have
      ;; shifted, and is fine to skip.
      (and title-refresh? (turn-cadence-tick? turn-position))
      {:importance :low
       :text (str "Current conversation title is \"" conversation-title "\". "
               "You are " turn-position " turn(s) into this conversation. "
               "If the focus has shifted, refresh it via "
               "`(set-conversation-title! \"...\")`.")})))

(defn context-pressure-nudge
  "Return a `:high`-importance nudge when the estimated input tokens
   for the assembled prompt-so-far cross
   `CONTEXT_PRESSURE_THRESHOLD * context-limit`, otherwise nil.

   Inputs from the host-supplied ctx:

     `:input-tokens`  - estimated tokens of the assembled prompt-so-far
     `:context-limit` - effective context window for the resolved model

   Returns nil when either input is missing or non-positive - the
   nudge is purely advisory and must never fail the iteration."
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
;; <iteration_hint> entries. Answer-time enforcement now lives in the
;; harness gate (`final-answer-structural-criteria-errors`); no
;; foundation :turn.answer/validate hooks remain.
;; ----------------------------------------------------------------------------

(defn- nudge->hook-hit
  [n]
  (when n {:hint (:text n) :importance (:importance n)}))

(def hooks
  "`:ext/hooks` vector for vis-foundation. Each entry conforms to the
   `::hook` spec in `com.blockether.vis.internal.extension`."
  [{:id    :vis.foundation/conversation-title
    :doc   "Nudge the model to set / refresh the conversation title when it's blank, refresh-flagged, or stale."
    :phase :turn.iteration/start
    :fn    (fn [ctx] (nudge->hook-hit (title-nudge ctx)))}
   {:id    :vis.foundation/context-pressure
    :doc   "Warn when assembled input tokens cross ~50% of the model's context window."
    :phase :turn.iteration/start
    :fn    (fn [ctx] (nudge->hook-hit (context-pressure-nudge ctx)))}])
