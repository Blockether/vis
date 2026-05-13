(ns com.blockether.vis.ext.foundation.nudges
  "Built-in `<current_engine_start_nudge>` policy for vis-foundation, expressed as
   `:ext/hooks` declarations. Active hooks ship here today:

     1. `:vis.foundation/conversation-title` (importance :low)
        Reminds the model to keep `CONVERSATION_TITLE` current. Fires
        when the title is blank, at every `TITLE_REFRESH_NUDGE_PERIOD`
        iterations inside a long turn, or on turn boundaries (the
        host passes `:title-refresh?`).

     2. `:vis.foundation/context-pressure` (importance :high)
        Fires when the estimated input tokens for the assembled
        prompt-so-far cross `CONTEXT_PRESSURE_THRESHOLD * context-limit`.
        Default 0.50 - with the uniform 200k Vis ceiling, that means
        the hook engages at ~100k input tokens, which matches z.ai's
        empirically reported GLM sweet spot of 95k-100k input tokens.

     3. `:vis.foundation/blind-answer` (importance :high)
        Fires on iteration 1 when the user request contains
        investigation verbs (why / fix / check / find / debug / ...)
        and no prior tool calls have run. Ignores explicit planning-only
        requests and symbol-name occurrences like `z/patch-check`.
        Warns the model that answering from memory on an investigation
        request is a hallucination.

     4. `:vis.foundation/action-request-needs-evidence`
        (hard `:turn.answer/validate` guard)
        Rejects completion claims for action requests that have no
        prior observed work in the current turn.

   Keeping policy in an extension (not core hardcoded built-ins)
   means these go through the same `:ext/hooks` protocol any
   third-party extension uses, can be swapped per channel/config,
   and the iteration assembly in core stays policy-free."
  (:require
   [clojure.string :as str]))

(def ^:const TITLE_REFRESH_NUDGE_PERIOD
  "Iteration cadence at which `title-nudge` re-asks the model to
   refresh `CONVERSATION_TITLE`. Independent of the always-on nudge
   fired when the title is blank. 12 sits in the middle of the
   user-requested 10-20 range - frequent enough that titles stay
   current as the conversation drifts, infrequent enough that a
   settled conversation isn't pestered every turn."
  12)

(def ^:const CONTEXT_PRESSURE_THRESHOLD
  "Fraction of the model's effective input-token budget at which
   `context-pressure-nudge` engages. 0.50 lands the nudge at ~100k
   under Vis' uniform 200k ceiling - matches z.ai's reported GLM
   sweet spot of 95k-100k input tokens, before accuracy/latency
   degrade. Plenty of headroom remains for the current iteration's
   thinking + tool calls + answer payload."
  0.50)

(defn title-nudge
  "Return a `:low`-importance nudge map when `CONVERSATION_TITLE`
   needs attention, otherwise nil. Inputs are pulled from the
   host-supplied nudge ctx:

     `:conversation-title` - current trimmed title (nil/blank if unset)
     `:title-refresh?`     - host signalled a turn-boundary refresh check
     `:iteration`          - 1-based iteration position inside the turn"
  [{:keys [conversation-title title-refresh? iteration]}]
  (let [blank? (or (nil? conversation-title) (str/blank? conversation-title))]
    (cond
      blank?
      {:importance :low
       :text (str "CONVERSATION_TITLE is currently empty. "
               "Set it via `(set-conversation-title! \"...\")` (3-7-word noun phrase, "
               "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
               "the conversation is discoverable in the sidebar.")}

      title-refresh?
      {:importance :low
       :text (str "Current CONVERSATION_TITLE is \"" conversation-title "\". "
               "If this turn changes the conversation focus, refresh the title via "
               "`(set-conversation-title! \"...\")`.")}

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      {:importance :low
       :text (str "You're " iteration " iterations into this turn. "
               "If the conversation's focus has shifted from \"" conversation-title "\", "
               "refresh the title via `(set-conversation-title! \"...\")`.")})))

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
                 "Converge now - finalise the answer via `(turn-answer! ...)`, "
                 "avoid dumping more file contents, diffs, or repeated diagnostics. "
                 "Models in this family degrade on long tails beyond ~50% of the window.")}))))

;; ----------------------------------------------------------------------------
;; Hooks (`:ext/hooks`) — structured lifecycle-phase callbacks. Pre-phase
;; hooks (e.g. :turn.iteration/start) emit MODEL-FACING <current_engine_start_nudge> entries;
;; post-phase hooks (e.g. :turn.iteration/stop, :turn/stop) are side-effect only.
;; ----------------------------------------------------------------------------

;; `title-nudge` and `context-pressure-nudge` are exposed as plain fns so
;; tests can probe them independently of the hook envelope. Their hook
;; wrappers below adapt the `{:importance :text}` map they return into the
;; `{:hint :importance}` shape pre-phase hooks return.

(defn- nudge->hook-hit
  [n]
  (when n {:hint (:text n) :importance (:importance n)}))

(def ^:private investigation-verb-regex
  ;; Stems of verbs that strongly imply "answer requires runtime/file
  ;; observation". Match whole-word, case-insensitive. Avoid treating
  ;; tool/symbol names (`z/patch-check`, `foo/check`) as user verbs.
  #"(?i)(?<![-/])\b(investigate|investigating|debug|debugging|why|fix|fixing|check|checking|find|finding|look(?:up|s)?|inspect|inspecting|reproduce|reproducing|diagnose|diagnosing|verify|verifying|trace|tracing|where|which|what does|how does|show me|search for|grep|locate|count)\b")

(def ^:private planning-only-regex
  #"(?i)\b(planning-only|plan-only|opinion-only|design-only|no tools?|do not inspect|do not investigate|do not modify files)\b")

(defn- looks-like-investigation?
  [user-request]
  (let [s (some-> user-request str str/trim)]
    (and s
      (>= (count s) 8)        ;; "hey" / "thx" / "siema" stay below
      (not (re-find planning-only-regex s))
      (boolean (re-find investigation-verb-regex s)))))

(defn blind-answer-guard-check
  "Fires on iteration 1 when the user request looks like an
   investigation but the model is about to answer without any tool
   observation. Returns nil (silent) or `{:hint :importance}`.

   Soft: the nudge only TELLS the model to investigate; preflight
   does NOT reject the answer. If the model still answers blindly,
   that's its call — the failure mode is at least surfaced."
  [{:keys [iteration user-request previous-blocks]}]
  (when (and (= 1 (long (or iteration 1)))
          (looks-like-investigation? user-request)
          (empty? previous-blocks))
    {:importance :high
     :hint (str "The user request looks like an investigation (verbs like "
             "'why', 'fix', 'check', 'find', 'debug', 'show me' …). "
             "You MUST call at least one tool (v/cat, v/rg, z/locators, "
             "v/ls …) to observe the actual state before composing "
             "`(turn-answer! …)`. Answering from memory on an investigation "
             "request is a hallucination. If the request is truly "
             "trivial chat (greeting, ack), ignore this nudge.")}))

(defn- previous-iteration-entries
  [{:keys [previous-iterations previous-blocks]}]
  (if (seq previous-iterations)
    (map (fn [entry]
           (if (vector? entry)
             {:iteration (first entry)
              :blocks (:blocks (second entry))}
             {:iteration (or (:iteration entry) (:position entry))
              :blocks (:blocks entry)}))
      previous-iterations)
    [{:iteration nil :blocks previous-blocks}]))

(def ^:private action-request-regex
  #"(?i)\b(fix|implement|patch|change|add|remove|delete|create|edit|update|verify|run|commit|push)\b")

(def ^:private blocked-answer-regex
  #"(?i)\b(blocked|cannot|can't|unable|need input|needs input|partial|not done|failed)\b")

(defn- action-request?
  [{:keys [user-request]}]
  (let [s (some-> user-request str str/trim)]
    (boolean
      (and (seq s)
        (not (re-find planning-only-regex s))
        (re-find action-request-regex s)))))

(defn- answer-blocked-or-partial?
  [answer]
  (boolean (when-some [s (some-> answer pr-str)]
             (re-find blocked-answer-regex s))))

(defn- answer-form?
  [form]
  (boolean (some-> form str str/trim (str/starts-with? "(turn-answer!"))))

(defn- successful-work-event?
  [block]
  (let [form (or (:code block) (:form block))]
    (and (seq (str form))
      (not (answer-form? form))
      (or (and (contains? block :error) (nil? (:error block)))
        (some true? (map :success? (:journal block)))))))

(defn- turn-has-work-evidence?
  [ctx]
  (boolean
    (some successful-work-event?
      (mapcat :blocks (previous-iteration-entries ctx)))))

(defn action-request-needs-evidence-check
  "Hard answer validator. Mutating/action requests must have at least one
   previous tool/code evidence event in this turn before a final answer claims
   completion. Blocked/partial failure reports are allowed so the model can
   truthfully stop when no useful recovery exists."
  [{:keys [answer] :as ctx}]
  (when (and (action-request? ctx)
          (not (turn-has-work-evidence? ctx))
          (not (answer-blocked-or-partial? answer)))
    {:reject true
     :message "User asked for action, but this turn contains no observed tool/code work before the final answer."
     :hint "Do the requested work or inspection first. If you are blocked, answer explicitly as blocked/partial instead of claiming completion."}))

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
    :fn    (fn [ctx] (nudge->hook-hit (context-pressure-nudge ctx)))}
   {:id    :vis.foundation/blind-answer
    :doc   "Warn when iteration 1 is about to answer an investigation-style request without any tool calls."
    :phase :turn.iteration/start
    :fn    blind-answer-guard-check}
   {:id    :vis.foundation/action-request-needs-evidence
    :doc   "Reject action-request completion claims with no prior observed tool/code evidence in the turn."
    :phase :turn.answer/validate
    :fn    action-request-needs-evidence-check}])
