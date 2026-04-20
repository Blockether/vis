(ns com.blockether.vis.loop.nudges
  "Per-iteration SYSTEM_NUDGE composers.

   A nudge is a short, tagged directive appended to an iteration's
   user-message. It exists to push the agent in a specific direction
   (finalize, prune, slow down) WITHOUT changing the iteration-loop's
   control flow. Each composer is a pure fn returning a string or
   nil — nil = no nudge this turn.

   Keep nudges LOW-TONE and ACTIONABLE. Each one should:
   - name the trigger condition concretely,
   - offer one or two concrete exits, and
   - not scold.

   The iteration-loop stitches non-nil results together under the
   agent's normal iteration-context and lets svar's spec do the
   structural work. Nudges never replace the spec — they supplement."
  (:require
    [clojure.string :as str]))

(def ^:const VAR_INDEX_NUDGE_THRESHOLD
  "Number of user-defined vars in <var_index> beyond which we remind
   the agent to `:forget` stale scratch vars. Past ~30 the index
   stops being a glance-and-go view and starts costing real context."
  30)

(def ^:const BUDGET_WARNING_LAST_ITER_REMAINING
  "When `remaining <= this`, the loudest nudge fires (LAST ITERATION /
   one-turn-left framing). Literal so the trigger is visible here
   rather than scattered as a magic 1 in iteration-loop."
  1)

(def ^:const BUDGET_WARNING_CLOSE_TO_CAP_REMAINING
  "Softer nudge fires once remaining drops this low — tells the agent
   to consolidate rather than keep exploring."
  3)

(defn budget-warning
  "Remaining-iterations nudge. Fires in two stages:
   - `remaining = 1`  → LAST ITERATION prompt with two hard exits
                        (emit :final or request-more-iterations).
   - `remaining ≤ 3`  → consolidation reminder.
   Otherwise returns nil.

   `:current-max-iterations` is the LIVE cap that includes any
   extensions the model requested via (request-more-iterations N).
   Don't pass the initial static cap — the agent needs to reason
   against the extended budget, not the starting one."
  [{:keys [iteration current-max-iterations]}]
  (let [iter-num  (inc (long iteration))
        max-iter  (long current-max-iterations)
        remaining (- max-iter (long iteration))]
    (cond
      (<= remaining BUDGET_WARNING_LAST_ITER_REMAINING)
      (str "\n<budget warning=\"LAST ITERATION\">\n"
        "THIS IS ITERATION " iter-num " OF " max-iter
        " — THE HARD CAP. No iteration runs after this one.\n"
        "TWO WAYS OUT:\n"
        "  1. Emit :final NOW with your best partial answer from <var_index>/<journal>.\n"
        "     A partial answer beats an empty bubble.\n"
        "  2. Call (request-more-iterations N) THIS TURN to extend the budget —\n"
        "     returns {:granted n :new-budget M :cap 500}. Max 50 per request.\n"
        "     Only do this if you have a concrete plan for the extra turns;\n"
        "     don't extend to keep exploring aimlessly.\n"
        "</budget>")

      (<= remaining BUDGET_WARNING_CLOSE_TO_CAP_REMAINING)
      (str "\n<budget warning=\"CLOSE TO CAP\">\n"
        "Iteration " iter-num " of " max-iter
        ". You have " remaining " turn" (when (not= 1 remaining) "s") " left including this one.\n"
        "STOP exploring. Synthesize from existing <var_index> + <journal>.\n"
        "If you genuinely need more turns for a concrete plan, call\n"
        "(request-more-iterations N) NOW — don't wait for the last iteration.\n"
        "Otherwise commit to a confidence level and emit :final.\n"
        "</budget>"))))

(defn bump-and-detect-repetition
  "Increment the running call-counts by every `[code, result-preview]`
   pair AND every `[:error-only, error-message]` pair in a fresh batch
   of executions, and return the updated counts plus an LLM-facing
   warning string (or nil).

   Why dual keys: the code+result key catches a model re-running the
   IDENTICAL call with the identical result (classic loop). The
   error-only key catches a subtler failure mode — model keeps varying
   the inputs (different greps, different globs) but the tool keeps
   rejecting with the SAME `:error` string. In that case the
   code+result pair is new every time, so the classical key never
   triggers; but the agent clearly learned nothing between attempts.
   Flagging on repeated error message lets the loop teach 'the shape
   of your call is wrong, change your approach — not your inputs'.

   Threshold: 2+ occurrences. First occurrence is a genuine mistake;
   the second is a pattern worth interrupting.

   Pure — returns `[new-counts warning-or-nil]`. The iteration-loop
   owns the atom that holds `counts`; `repetition-warning` below is
   the side-effectful wrapper that swap!s and returns the warning."
  [counts executions]
  (let [truncate (fn [s n] (if (> (count s) n) (str (subs s 0 n) "...") s))
        pair-keys  (mapv (fn [e] [:pair (:code e) (truncate (str (:result e)) 200)]) executions)
        error-keys (->> executions
                     (keep (fn [e]
                             (when-let [err (:error e)]
                               [:error-only (truncate (str err) 200)]))))
        all-keys   (into pair-keys error-keys)
        counts' (reduce (fn [acc k] (update acc k (fnil inc 0))) counts all-keys)
        repeated-pairs (->> pair-keys
                         (filter #(>= (get counts' % 0) 2))
                         (map (fn [[_ code _]] (str "  - " (truncate (str code) 80)))))
        repeated-errors (->> error-keys
                          distinct
                          (filter #(>= (get counts' % 0) 2))
                          (map (fn [[_ msg]]
                                 (str "  - error repeated: " (truncate (str msg) 120)))))
        lines (into (vec repeated-pairs) repeated-errors)
        warning (when (seq lines)
                  (str "\n\n⚠ REPETITION DETECTED: These calls/errors have fired 2+ times this query:\n"
                    (str/join "\n" lines)
                    "\nRetrying the same action — or varying inputs while hitting the SAME error — will not produce different results. "
                    "Change your APPROACH (different tool, different shape of call), or emit :final with what you have."))]
    [counts' warning]))

(defn repetition-warning
  "Side-effectful composer: swap!s `call-counts-atom` with the bump
   produced by `prev-executions` and returns the warning string (or
   nil). Call from the iteration-context builder so the warning from
   the PREVIOUS iteration's executions lands at the TOP of the next
   iteration's user message — that's when the agent is about to act
   and can still change course.

   When either argument is missing, returns nil (no-op)."
  [call-counts-atom prev-executions]
  (when (and call-counts-atom (seq prev-executions))
    (let [[counts' warning] (bump-and-detect-repetition
                              @call-counts-atom prev-executions)]
      (reset! call-counts-atom counts')
      warning)))

(defn var-index-overflow
  "Var-count nudge. Fires when the live <var_index> carries more than
   `VAR_INDEX_NUDGE_THRESHOLD` user vars. Points at `:forget` as the
   cheap consolidation move — DB rows survive, `(restore-var 'sym)`
   reclaims later."
  [var-count]
  (when (and (integer? var-count) (> (long var-count) VAR_INDEX_NUDGE_THRESHOLD))
    (str "\n[SYSTEM_NUDGE] <var_index> currently holds " var-count
      " user vars (threshold " VAR_INDEX_NUDGE_THRESHOLD
      "). Many are likely scratch/stale. Drop the names you no longer "
      "need via `:forget [\"a\" \"b\" …]` in your response — DB rows "
      "survive the forget so `(restore-var 'a)` can reclaim them later "
      "if you change your mind. Keeping the index lean shrinks every "
      "future prompt.")))
