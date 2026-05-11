(ns com.blockether.vis.ext.foundation.nudges
  "Built-in `<system_nudge>` policy for vis-foundation, expressed as
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
               "Set it via `(conversation-title \"...\")` (3-7-word noun phrase, "
               "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
               "the conversation is discoverable in the sidebar.")}

      title-refresh?
      {:importance :low
       :text (str "Current CONVERSATION_TITLE is \"" conversation-title "\". "
               "If this turn changes the conversation focus, refresh the title via "
               "`(conversation-title \"...\")`.")}

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      {:importance :low
       :text (str "You're " iteration " iterations into this turn. "
               "If the conversation's focus has shifted from \"" conversation-title "\", "
               "refresh the title via `(conversation-title \"...\")`.")})))

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
                 "Converge now - finalise the answer via `(answer ...)`, "
                 "avoid dumping more file contents, diffs, or repeated diagnostics. "
                 "Models in this family degrade on long tails beyond ~50% of the window.")}))))

;; ----------------------------------------------------------------------------
;; Hooks (`:ext/hooks`) — structured lifecycle-phase callbacks. Pre-phase
;; hooks (e.g. :turn.iteration/start) emit MODEL-FACING <system_nudge> entries;
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
             "v/bash …) to observe the actual state before composing "
             "`(answer …)`. Answering from memory on an investigation "
             "request is a hallucination. If the request is truly "
             "trivial chat (greeting, ack), ignore this nudge.")}))

(defn- error-message
  [err]
  (cond
    (nil? err) nil
    (string? err) err
    (map? err) (or (:message err) (:msg err) (pr-str err))
    :else (str err)))

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

(def ^:private call-head-regex
  #"\(\s*([A-Za-z0-9_.*+!?'<>:=/-]+)")

(defn- form-tool
  [form]
  (when-let [head (some->> form str (re-find call-head-regex) second)]
    (let [[ns-part name-part] (str/split head #"/" 2)]
      (if name-part
        (keyword ns-part name-part)
        (keyword ns-part)))))

(defn- short-form
  [form]
  (let [s (some-> form str str/trim)]
    (when (seq s)
      (if (< 180 (count s))
        (str (subs s 0 180) "...")
        s))))

(defn- form-key
  [form]
  (some-> form str str/trim not-empty))

(defn- verification-form?
  [form]
  (let [s (some-> form str str/lower-case)]
    (boolean
      (and s
        (or (str/includes? s "./verify.sh")
          (str/includes? s "clojure -m:test")
          (str/includes? s "clj -m:test")
          (str/includes? s "lazytest"))))))

(defn failure-obligations
  "Extract first-class failure obligations from previous iterations.
   Public for regression tests."
  [ctx]
  (->> (previous-iteration-entries ctx)
    (mapcat (fn [{:keys [iteration blocks]}]
              (mapcat
                (fn [block-idx block]
                  (let [block-num (inc block-idx)
                        block-form (or (:code block) (:form block))
                        block-form-key (form-key block-form)
                        block-tool (or (some-> block :result :symbol) (form-tool block-form))
                        block-msg (error-message (:error block))
                        block-failure (when block-msg
                                        [{:id (str "iter/" iteration "/block/" block-num)
                                          :kind :failure/block
                                          :iteration iteration
                                          :block block-num
                                          :tool block-tool
                                          :form (short-form block-form)
                                          :form-key block-form-key
                                          :message block-msg}])
                        journal-failures
                        (keep-indexed
                          (fn [journal-idx entry]
                            (when (false? (:success? entry))
                              (let [form (or (:form entry) block-form)]
                                {:id (str "iter/" iteration "/block/" block-num "/journal/" (inc journal-idx))
                                 :kind :failure/journal
                                 :iteration iteration
                                 :block block-num
                                 :journal (inc journal-idx)
                                 :tool (or (form-tool form) block-tool)
                                 :form (short-form form)
                                 :form-key (form-key form)
                                 :message (error-message (:error entry))})))
                          (:journal block))]
                    (concat block-failure journal-failures)))
                (range)
                (or blocks []))))
    (remove #(str/blank? (:message %)))
    vec))

(defn proof-events
  "Extract successful later proof events from previous iterations.
   Public for regression tests.

   Tool calls usually surface success through `:journal` sink entries.
   Bare forms like `(conversation-title ...)` or `(def x ...)` often do not,
   so a later clean block evaluation also counts as a proof event. Block-level
   proofs close only by exact form match; journal proofs keep the broader
   same-tool / verification closure semantics."
  [ctx]
  (->> (previous-iteration-entries ctx)
    (mapcat (fn [{:keys [iteration blocks]}]
              (mapcat
                (fn [block-idx block]
                  (let [block-num (inc block-idx)
                        block-form (or (:code block) (:form block))
                        block-form-key (form-key block-form)
                        block-tool (or (some-> block :result :symbol) (form-tool block-form))
                        block-proof (when (and (nil? (:error block)) block-form-key)
                                      [{:iteration iteration
                                        :block block-num
                                        :tool block-tool
                                        :form (short-form block-form)
                                        :form-key block-form-key
                                        :verification? (verification-form? block-form)
                                        :source :block}])
                        journal-proofs
                        (keep-indexed
                          (fn [journal-idx entry]
                            (when (true? (:success? entry))
                              (let [form (or (:form entry) block-form)
                                    tool (or (form-tool form) block-tool)]
                                {:iteration iteration
                                 :block block-num
                                 :journal (inc journal-idx)
                                 :tool tool
                                 :form (short-form form)
                                 :form-key (form-key form)
                                 :verification? (verification-form? form)
                                 :source :journal})))
                          (:journal block))]
                    (concat block-proof journal-proofs)))
                (range)
                (or blocks []))))
    vec))

(defn- later-proof-closes?
  [failure proof]
  (and (number? (:iteration failure))
    (number? (:iteration proof))
    (< (long (:iteration failure)) (long (:iteration proof)))
    (or (:verification? proof)
      (and (:form-key failure)
        (= (:form-key failure) (:form-key proof)))
      (and (= :journal (:source proof))
        (:tool failure)
        (= (:tool failure) (:tool proof))))))

(defn open-error-obligations
  "Return failure obligations not closed by a later proof event.
   A proof must occur in a later iteration, because the model cannot observe
   same-iteration failures before composing a final answer."
  [ctx]
  (let [failures (failure-obligations ctx)
        proofs (proof-events ctx)]
    (->> failures
      (remove (fn [failure]
                (some #(later-proof-closes? failure %) proofs)))
      vec)))

(defn- obligation-summary
  [{:keys [iteration block journal tool form message]}]
  (str "iteration " (or iteration "?")
    "/block " (or block "?")
    (when journal (str "/journal " journal))
    (when tool (str " " tool))
    (when form (str " `" form "`"))
    ": " message))

(defn unresolved-error-answer-guard-check
  "Hard answer validator. Reject a candidate answer while any previous failure
   obligation remains open. A later successful verification command closes all
   prior failures; a later successful same-tool journal call closes that tool's
   prior failure; a later successful bare-form re-evaluation (for example
   `(conversation-title ...)` or `(def x ...)`) closes the exact same earlier
   failed form. Same-iteration proof does not count because the model could not
   observe the failure before composing the answer."
  [ctx]
  (let [open (open-error-obligations ctx)]
    (when (seq open)
      (let [preview (str/join "; " (map obligation-summary (take 3 open)))]
        {:reject true
         :message (str "Open failure obligations remain in the journal: " preview)
         :hint "Do not answer yet. Read the failed journal entry. If answer-alone preflight rejected sibling forms, rerun those forms without `(answer ...)` and observe success. Otherwise fix or explicitly prove/acknowledge the failure, then run a later proof step (for example ./verify.sh --quick or a successful same-tool retry) before answering."}))))

(def ^:private action-request-regex
  #"(?i)\b(fix|implement|patch|change|add|remove|delete|create|edit|update|verify|run|commit|push)\b")

(def ^:private short-followup-action-regex
  #"(?i)^\s*(do it|do that|please do it|please do that|make it|go ahead|yes|yep|ok|okay|a)\s*[.!?]*\s*$")

(def ^:private blocked-answer-regex
  #"(?i)\b(blocked|cannot|can't|unable|need input|needs input|partial|not done|failed)\b")

(defn- action-request?
  [{:keys [user-request current-objective]}]
  (let [s (some-> user-request str str/trim)
        objective-source (:source current-objective)
        objective-text (some-> current-objective :text str str/trim not-empty)]
    (boolean
      (and (seq s)
        (not (re-find planning-only-regex s))
        (or (re-find action-request-regex s)
          (and (re-find short-followup-action-regex s)
            (= :previous-user-request objective-source)
            objective-text))))))

(defn- answer-blocked-or-partial?
  [answer]
  (boolean (when-some [s (some-> answer pr-str)]
             (re-find blocked-answer-regex s))))

(defn- answer-form?
  [form]
  (boolean (some-> form str str/trim (str/starts-with? "(answer"))))

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
    :fn    blind-answer-guard-check}])
