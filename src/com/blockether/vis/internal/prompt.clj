(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces:

     1. The system prompt — written once, cached per-conversation.
        `assemble-system-prompt` joins the minimal core prompt + the
        environment block + each active extension's prompt fragment.

     2. The trailing user message — rebuilt every iteration. Two slots:
          <journal>      last JOURNAL_KEEP_ITERS iterations, code + result,
                        addressable as iN.K. The model's working memory.
          <var_index>   user-defined `(def ...)` bindings in the SCI env.
        Extensions can append `[system_nudge]` lines via `:ext/nudge-fn`.

   The two slots above plus the SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` — `TURN_USER_REQUEST`, `TURN_CONVERSATION_TURN_ID`,
   `TURN_CONVERSATION_SOUL_ID`, `TURN_CONVERSATION_STATE_ID`,
   `TURN_SYSTEM_PROMPT`, `TURN_ACTIVE_EXTENSIONS`, `ITERATION_ID`,
   `ITERATION_PREVIOUS_REASONING`, `CONVERSATION_TITLE`,
   `CONVERSATION_PREVIOUS_ANSWER`) bound in SCI cover everything the
   model needs."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Hard cap on a single value's pr-str when shown to the model in <journal>."
  6000)

(def ^:const JOURNAL_KEEP_ITERS
  "Rolling-window size for <journal> entries. 12 carries the last
   dozen iterations of the conversation (cross-turn) so a follow-up
   turn sees the immediate context of the prior turn's work without
   re-fetching via `(v/inspect)`.

   When the iteration's prompt token count crosses
   `CONTEXT_PRESSURE_THRESHOLD` of the model's context window the
   loop fires a `[system_nudge]` instructing the model to curate
   `(def …)` summaries it cares about; older entries then drop off
   the rolling window verbatim. The runtime never auto-summarizes —
   the model owns its working memory, in line with the RLM
   principle this project is built on (see AGENTS.md ▸ 'No
   auto-compaction')."
  12)

(def ^:const CONTEXT_PRESSURE_THRESHOLD
  "Fraction of the model's effective input-token budget at which the
   loop fires a context-pressure nudge. 60% leaves headroom for the
   current iteration's own thinking + tool calls + answer payload
   without forcing the model to firefight an overflow."
  0.60)

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) " …") s)))

(defn- strip-sandbox-ns [s]
  (str/replace (str s) #"\bsandbox/" ""))

(defn- realize-value [v]
  (cond
    (nil? v) nil
    (map? v) v
    (vector? v) v
    (string? v) v
    :else v))

(defn safe-pr-str
  "Bounded pr-str. Used in <journal> rendering."
  ([v] (safe-pr-str v {}))
  ([v {:keys [max-chars print-length print-level]
       :or {max-chars MAX_RESULT_DISPLAY_CHARS
            print-length 64
            print-level 6}}]
   (try
     (binding [*print-length* print-length
               *print-level*  print-level]
       (let [s (strip-sandbox-ns (pr-str v))]
         (if (> (count s) max-chars)
           (str (subs s 0 max-chars) " …<+" (- (count s) max-chars) " chars>")
           s)))
     (catch Throwable t
       (str "<unprintable: " (.getMessage t) ">")))))

(defn truncated-pr-str
  "Wrapper used by <journal>. Returns [bounded-str truncated?]."
  [v]
  (let [bounded   (safe-pr-str v {:max-chars MAX_RESULT_DISPLAY_CHARS})
        truncated? (boolean (re-find #" …<\+\d+ chars>$" bounded))]
    [bounded truncated?]))

;; =============================================================================
;; <journal> — last JOURNAL_KEEP_ITERS iterations of code + results
;; =============================================================================

(defn- tool-result-journal-text
  [v]
  (let [presentation (extension/presentation v)]
    (cond
      (= :hide (:journal presentation)) "<tool result hidden>"
      (:markdown v) (:markdown v)
      :else (str "<tool result " (pr-str (select-keys v [:ok? :result-shape :error])) ">"))))

(defn- provenance-journal-suffix
  "Compact block-level provenance for the model-facing journal. Keep
   only the fields that help the next iteration reason about where the
   observation came from; full provenance remains stored on the block."
  [provenance]
  (when (map? provenance)
    (let [visible (cond-> (select-keys provenance [:ref :op :status :duration-ms])
                    (:timeout? provenance) (assoc :timeout? true)
                    (:repaired? provenance) (assoc :repaired? true))]
      (when (seq visible)
        (str " :provenance " (pr-str visible))))))

(defn- format-block-line
  "One iteration's single block as a `<journal>` line. Renders
   `<canonical-ref>  <code> → <value>` plus any non-blank stdout/stderr and a
   slow-suffix when execution exceeded 5s. Bare-symbol blocks read
   naturally because the `<code>` IS the symbol and `<value>` is its
   bound content. Tool-result envelopes render through shared display text
   instead of dumping the full map into the journal."
  [iteration-position k expr]
  (let [{:keys [code error result stdout stderr execution-time-ms provenance]} expr
        display-ref    (or (:ref provenance)
                         (str "i" iteration-position "." (inc k)))
        code-str      (str/trim (or code ""))
        stdout-suffix (when-not (str/blank? stdout)
                        (str " :stdout " (pr-str (truncate stdout 600))))
        stderr-suffix (when-not (str/blank? stderr)
                        (str " :stderr " (pr-str (truncate stderr 600))))
        time-ms       (or execution-time-ms 0)
        slow-suffix   (when (> time-ms 5000)
                        (str " (" time-ms "ms)"))
        value-part    (if error
                        (str "ERROR: " (truncate error 600))
                        (if (extension/tool-result? result)
                          (tool-result-journal-text result)
                          (let [v (realize-value result)
                                [value-str truncated?] (truncated-pr-str v)]
                            (str value-str
                              (when truncated? " :truncated? true")))))]
    (str "  " display-ref "  " code-str " → " value-part
      (or slow-suffix "")
      (or (provenance-journal-suffix provenance) "")
      (or stdout-suffix "")
      (or stderr-suffix ""))))

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: optional thinking line,
   then per-block canonical-ref lines that include the leading `:comment`
   (when present) right above the code→value line."
  [iteration-position iteration-data]
  (let [{:keys [thinking blocks]} iteration-data
        header-lines (when (and (string? thinking)
                             (not (str/blank? thinking)))
                       [(str "  i" iteration-position " thinking: "
                          (truncate (str/trim thinking) 800))])
        block-lines  (vec (mapcat (fn [[k blk]]
                                    (let [comment-text (some-> (:comment blk) str/trim)
                                          comment-line (when (and comment-text
                                                               (not (str/blank? comment-text)))
                                                         (str "  " (or (get-in blk [:provenance :ref])
                                                                     (str "i" iteration-position "." (inc k)))
                                                           "  ;; "
                                                           (truncate comment-text 400)))]
                                      (cond-> []
                                        comment-line (conj comment-line)
                                        :always      (conj (format-block-line iteration-position
                                                             k blk)))))
                            (map-indexed vector (or blocks []))))]
    (vec (concat header-lines block-lines))))

(defn- format-journal-block
  "Render the last JOURNAL_KEEP_ITERS iterations with canonical provenance
   refs when available. `iters` is a seq of `[iteration-position {:thinking :blocks}]`
   pairs, oldest-first. Each iteration's segment carries:
     - `iN thinking: …` once at the top (when the iteration emitted
       any reasoning text)
     - per-block `iN.K  ;; <comment>` line above the code line, when
       the model authored a leading `;; …` / `#_(...)` comment for
       that form
     - `<canonical-ref>  <code> → <value>` for every block in the iteration"
  [iters]
  (let [kept  (take-last JOURNAL_KEEP_ITERS (or iters []))
        lines (->> kept
                (mapcat (fn [[pos iteration-data]]
                          (format-journal-iteration-block pos iteration-data)))
                vec)]
    (when (seq lines)
      (str "<journal>\n" (str/join "\n" lines) "\n</journal>"))))

;; =============================================================================
;; <var_index> — read/cache the current SCI sandbox shape
;; =============================================================================

(defn read-var-index-str
  "Lazily build (and cache) the <var_index> body for the active env.
   Returns nil when the env has no SCI context (test fixtures)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [var-index-atom (or (:var-index-atom environment)
                           (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @var-index-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx) [:namespaces 'sandbox])
              idx         (env/build-var-index
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)]
          (swap! var-index-atom assoc :index idx :revision current-revision)
          idx)))))

;; =============================================================================
;; Iteration context — the trailing user message
;; =============================================================================

(def ^:const TITLE_REFRESH_NUDGE_PERIOD
  "Iteration cadence at which the loop re-nudges the model to refresh
   `CONVERSATION_TITLE`. Independent of the always-on nudge fired
   when the title is blank. 12 lands in the middle of the
   user-requested 10-20 range — frequent enough that the title stays
   current as the conversation drifts, infrequent enough that a
   settled conversation isn't pestered every turn."
  12)

(defn- ^:long count-prompt-tokens
  "Token count for `text` against `model`. Falls back to
   `(quot (count text) 4)` (rough English/code rule of thumb) when
   the encoder lookup fails for an unrecognized model id."
  [model text]
  (when (string? text)
    (or (try
          (when (string? model)
            (svar-router/count-tokens model text))
          (catch Throwable _ nil))
      (long (quot (count text) 4)))))

(defn- model-context-limit
  "Best-effort lookup of `model`'s context window. Falls back to a
   conservative 32k when the table doesn't know the model id, which
   is the smallest mainstream tier still in production use — better
   to nudge a bit early on a 200k model than to never nudge at all."
  [model]
  (or (try
        (when (string? model)
          (svar-router/context-limit model svar-router/MODEL_CONTEXT_LIMITS))
        (catch Throwable _ nil))
    32000))

(defn- context-pressure-nudge
  "Built-in `[system_nudge]` line that fires when the assembled prompt
   (system message + history + new iteration trailer) crosses
   `CONTEXT_PRESSURE_THRESHOLD` of the model's input-token budget.
   Returns nil when usage is below threshold or token info is
   unavailable.

   The nudge does NOT auto-summarize — RLM puts curation in the
   model's hands. Instead it (a) reports the live usage so the model
   sees the budget concretely, (b) gives a Chain-of-Density-style
   recipe for a `(def …)` summary the MODEL writes itself, and (c)
   reminds the model that older raw iters stay reachable through the
   foundation read API even after they roll off the journal."
  [_model used-tokens limit-tokens]
  (when (and (integer? used-tokens) (integer? limit-tokens) (pos? limit-tokens))
    (let [util (double (/ used-tokens limit-tokens))]
      (when (>= util CONTEXT_PRESSURE_THRESHOLD)
        (str "[system_nudge] Context window is at "
          (int (Math/round (* 100.0 util))) "% ("
          used-tokens " / " limit-tokens " tokens). Older <journal>\n"
          "  iterations will roll off the rolling window soon. Curate the\n"
          "  insight you've earned BEFORE that happens — emit a structured\n"
          "  `(def …)` so the value lands in <var_index> + persisted history and\n"
          "  survives the roll. Chain-of-Density-style recipe (use only\n"
          "  facts that already appeared in the journal; no new\n"
          "  characterizations / evaluative adjectives):\n"
          "\n"
          "    (def turn-summary\n"
          "      {:findings   [{:where \"src/auth.clj:42\" :what \"jwt-decode rejects nbf-skew\"}\n"
          "                    {:where \"iN.K\"           :what \"<concrete-fact>\"}]\n"
          "       :errors     [{:iteration N :class :patch-no-match :recovery \"…\"}]\n"
          "       :decisions  [{:choice \"validate-then-decode\" :rationale \"…\"}]\n"
          "       :next-step  \"extract verify-jwt to its own ns\"})\n"
          "\n"
          "  Keys above are illustrative — use whatever shape fits the\n"
          "  task. Atoms preferred (file paths, symbol names, error keys,\n"
          "  iN.K refs) over prose. Raw history stays reachable via\n"
          "  `(v/inspect)` after iterations roll off; use its :transcript\n"
          "  and :failures keys when you genuinely need precision your\n"
          "  `(def …)` didn't capture.")))))

(defn- title-nudge
  "Built-in `[system_nudge]` line that fires when:
     1. `CONVERSATION_TITLE` is currently empty, OR
     2. `iteration` is a positive multiple of
        `TITLE_REFRESH_NUDGE_PERIOD` (cadence reminder once a title
        has been set).
   Returns nil otherwise."
  [environment iteration]
  (let [title (some-> (:conversation-title-atom environment) deref str str/trim)
        blank? (or (nil? title) (str/blank? title))]
    (cond
      blank?
      (str "[system_nudge] CONVERSATION_TITLE is currently empty. "
        "Set it via `(conversation-title \"…\")` (3-7-word noun phrase, "
        "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
        "the conversation is discoverable in the sidebar.")

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      (str "[system_nudge] You're " iteration " iterations into this turn. "
        "If the conversation's focus has shifted from \"" title "\", "
        "refresh the title via `(conversation-title \"…\")`."))))

(defn build-iteration-context
  "Assemble the per-iteration trailing user message.

   Two slots:
     <journal>     — last JOURNAL_KEEP_ITERS iterations, thinking +
                     comments + code + result.
     <var_index>   — `(def ...)` bindings in the SCI env.

   Plus zero or more `[system_nudge]` lines. Built-ins:
     - title nudge (fires on blank title or every
       TITLE_REFRESH_NUDGE_PERIOD iterations).
   Active extensions can append more via `:ext/nudge-fn`.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`. Computed once
        per turn; threaded through every iteration. Each extension's
        :ext/nudge-fn is consulted (rare).

   Optional:
     `:blocks-by-iteration` — last few iterations of
        `[iteration-position {:thinking :blocks}]` pairs for the
        <journal> renderer.
     `:iteration` — current iteration position (1-based for rendered refs;
        callers that keep an internal counter convert before exposing it)."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       model system-prompt]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [recent-block (format-journal-block blocks-by-iteration)
        last-iteration-blocks (some-> blocks-by-iteration last second)
        var-index-str (read-var-index-str environment)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        title-line (title-nudge environment iteration)
        ;; Token-budget probe. Estimate the size of the assembled
        ;; prompt that would be sent to the LLM; fire the
        ;; context-pressure nudge when it crosses
        ;; `CONTEXT_PRESSURE_THRESHOLD`. The probe is a no-op when
        ;; `:model` isn't supplied (e.g. test fixtures).
        prompt-text (str/join "\n\n"
                      (keep identity
                        [system-prompt recent-block var-block]))
        used-tokens (count-prompt-tokens model prompt-text)
        ctx-limit   (model-context-limit model)
        pressure-line (when (and used-tokens ctx-limit)
                        (context-pressure-nudge model used-tokens ctx-limit))
        ext-nudges (when (seq active-extensions)
                     (let [iter-position (if (some? iteration)
                                           (inc (long iteration))
                                           1)
                           ctx {:environment environment
                                :iteration iter-position
                                :previous-blocks last-iteration-blocks}]
                       (into []
                         (keep (fn [ext]
                                 (when-let [nudge-fn (:ext/nudge-fn ext)]
                                   (try
                                     (let [result (nudge-fn ctx)]
                                       (when (and (string? result) (not (str/blank? result)))
                                         result))
                                     (catch Throwable t
                                       (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                                       nil)))))
                         active-extensions)))
        all-nudges (cond-> []
                     title-line       (conj title-line)
                     pressure-line    (conj pressure-line)
                     (seq ext-nudges) (into ext-nudges))
        nudges-block (when (seq all-nudges) (str/join "\n" all-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [recent-block var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn assemble-initial-messages
  [{:keys [system-prompt initial-user-content history-messages]}]
  (vec
    (concat
      (when system-prompt [{:role "system" :content system-prompt}])
      (or history-messages [])
      (when initial-user-content [{:role "user" :content initial-user-content}]))))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

;; =============================================================================
;; System prompt
;; =============================================================================

;; Citation: Michael Whitford, Nucleus, https://github.com/michaelwhitford/nucleus
;; The core prompt uses a Nucleus-style symbolic center and a VSM-shaped
;; operating stack, with attribution kept in source instead of prompt text.
(def CORE_SYSTEM_PROMPT
  "You are Vis, a recursive language model (RLM) running in a sandboxed Small Clojure Interpreter (SCI). You control a read/eval/observe loop:

  emit Clojure forms -> host evaluates them -> results/errors enter <journal>
  -> next iteration observes <journal> -> repeat until terminal answer.

λ engage(nucleus).
[phi fractal euler tao pi mu ∃ ∀] | [Δ λ Ω ∞/0 | ε/φ Σ/μ c/h signal/noise order/entropy truth/provability self/other] | OODA ⊗ RGR ⊗ REPL
Human ⊗ Vis ⊗ Workspace

ΩVisContract :=
  ConversationIntentᵛ
    → ConversationPlanᵛ
      → BlockingGateᵛ
        → CanonicalEvidenceRef⁺
          → TimelineEvent

Entities:
  ConversationIntentᵛ := conversation-scoped top-level gate: what Vis believes the user wants
  ConversationPlanᵛ := strategy for satisfying one ConversationIntentᵛ
  BlockingGateᵛ := concrete falsifiable check for one plan; required gates block by default
  CanonicalEvidenceRef⁺ := one-or-more canonical refs into observed provenance timeline
  TimelineEvent := eval/tool/system/error event, e.g. turn/3f2a91c0/iteration/4/block/2

Invariants:
  ∀ Intent: belongs to one conversation_soul; status ∈ active|fulfilled|abandoned
  ∀ Focus: belongs to one conversation_turn_state; focused intents decide current answer readiness
  ∀ Plan: belongs to one Intent; at most one active Plan per Intent; new active plan supersedes prior active plan
  ∀ Gate: belongs to one Plan; required open Gate blocks; required blocked Gate requires re-plan or abandon
  ∀ ProvenGate: proof summary + observed canonical provenance refs required
  ∀ FulfilledIntent: fulfillment summary + observed canonical provenance refs required
  ∀ AbandonedIntent: abandonment reason + observed canonical provenance refs required
  ∀ Ref: canonical, observed, same conversation, not invented; compact refs like i1.1 are display labels only

Runtime Nucleus:
  before final answer:
    provenance-checks pass
    focused conversation intents checked via `(v/intents)`
    every focused intent is fulfilled or abandoned
    focused active intent has one active plan while work is in progress
    active plan has gates
    required open gates block
    required blocked gates require re-plan or abandon
    proven gates cite observed proof refs
    fulfillment/abandonment cites observed refs

Nucleus palette:
  phi/fractal/euler/tao/pi/mu  self-reference, scalable structure, compounding learning, minimal essence, cycles, least fixed point
  ∃/∀                         seek existing solutions; preserve invariants across all cases
  Δ/λ/Ω/∞/0                    optimize with evidence; compose functions/data; terminate at fixed point; test boundaries
  ε/φ Σ/μ c/h                  balance good-enough/ideal, capability/minimal complexity, speed/atomic safety
  signal/noise                 attend to relevant evidence; discard distraction
  order/entropy                impose enough structure without killing exploration
  truth/provability            separate reality from what has been shown by tests/traces/data
  self/other                   distinguish model assumptions from user intent and workspace facts

VSM operating stack:
  S5 identity      λ identity(turn). higher_priority_rules > nucleus_notation | user_intent ∧ evidence ∧ safety -> success
  S4 intelligence  λ learn(unknown). observe -> orient -> hypothesize -> probe -> revise | errors = evidence
  S3 control       λ control(turn). focused_intents ∧ gates ∧ context ∧ resources ∧ verification ∧ completion_criteria
  S2 coordination  λ coordinate(work). journal ∧ vars ∧ extensions ∧ files ∧ tools ∧ user_feedback
  S1 operations    λ operate(iter). emit_clojure -> host_eval -> observe -> act_or_continue -> answer_when_done

Core lambda:
  λ vis(turn).
    understand(TURN_USER_REQUEST) -> plan(minimal_next_probe)
    -> explore(read/search/run narrow evidence)
    -> observe(<journal> ∧ <var_index>)
    -> act(only_when_supported_by_evidence)
    -> verify(targeted_checks ∨ exact_blocker)
    -> resolve_intents(fulfilled∨abandoned, canonical_refs)
    -> Ω(answer(markdown, evidence, changed_files?, verification?, risks?))
  | symbolic_frame < explicit_system_developer_project_user_instructions
  | no_guessing | inspect_before_edit | errors_are_evidence

Non-compressible runtime contract follows. These protocol rules override any compact symbolic reading.

Reply each iteration with one or more ```clojure … ``` fences. Their source concatenates into top-level forms; each form runs in order. The text INSIDE each fence must be executable Clojure forms only: no nested Markdown fences, no raw ``` markers, no prose outside Clojure comments. Think in Clojure data: build small values, transform maps/vectors/sets, surface state into the journal, then act or answer.

CRITICAL: `(answer ARG)` is a terminal COMMIT. ONE accepted answer ends the user turn. Call it only when the task is complete, verified when relevant, or concretely blocked with evidence. Never use `(answer …)` for progress messages like \"scanned\", \"done\", \"I will inspect next\", or \"found files\".

FIRST-ITERATION ANSWER BAN for code/debug/change work. If TURN_USER_REQUEST asks to inspect, debug, fix, edit, refactor, implement, test, verify, run, search, or explain repository/code state, iteration 1 MUST NOT call `(answer …)`. Iteration 1 for those tasks emits symbols only: understand state, intents, first probes, and values to observe in canonical refs. The host continues automatically when no answer is emitted. Only trivial chat whose full satisfaction needs no repo/code/tool/evidence may answer in iteration 1.

Final-answer format rule: final answers are Markdown by default. Unless TURN_USER_REQUEST explicitly asks for another format (plain text, JSON, EDN, CSV, etc.), build the answer as Markdown and pass that Markdown string to `(answer …)`. Prefer the `v/` Markdown helpers when they are active: compose blocks with `v/join`, paragraphs with `v/p`, lists with `v/ul` / `v/ol`, tables with `v/table`, code with `v/code` / `v/code-block`, and source citations with `v/file-link`. Do not hand-write raw Markdown fences in emitted Clojure source; use `v/code-block` when the answer needs a fenced code block.

RLM control loop:
  Not every iteration needs an answer; continue by omitting `(answer ...)`; the runtime will loop you.
  Many iterations only collect evidence for the next journal.
  UNDERSTAND  classify request; create/focus the conversation intent when required
  PLAN        choose the smallest evidence-gathering and action path
  INTENTS     use ConversationIntent -> ConversationPlan -> BlockingGate -> Provenance Reference
  EXPLORE     run narrow reads/searches/tools; surface observations into <journal>
  OBSERVE     read <journal>/<var_index> results before drawing conclusions
  ACT         edit/write only when evidence supports the change
  VERIFY      run targeted checks, or capture an exact blocker
  PROVE       prove/block Gates with observed canonical provenance refs
  RESOLVE     fulfill or abandon focused Intents with observed canonical provenance refs
  CHECK       run `(v/intents)` and `(v/provenance-guards)`
  ANSWER      final Markdown only after focused intents are fulfilled/abandoned; after iteration 1 it must be the only top-level form in its iteration

Intent-required classifier:
  REQUIRED for code/debug/change/refactor/test/verify/repo inspection, multi-step plans,
  proof/audit requests, and any claim that files/tools/runtime were inspected, changed, or verified.
  OPTIONAL for trivial chat and pure conceptual explanation with no workspace/evidence claim.
  If unsure, create the intent. If intent creation fails, do not claim gated completion.

Conversation intents are database-backed, not a local `turn-state` map:
  ConversationIntentᵛ  what the user wants in this conversation branch
  ConversationPlanᵛ   how this intent will be satisfied
  BlockingGateᵛ       falsifiable completion condition; required open gates block
  CanonicalEvidenceRef⁺  canonical refs from the observed provenance timeline

Do NOT maintain a parallel local proof map.
`(v/intents)` is the single read/check/report surface. Use the `v/issue-*` intent/plan/gate writers and `v/prove-gate!` / `v/block-gate!`; legacy short writer names and the old separate proof-object surface are removed. Fulfill or abandon focused intents before final answer.

Loop law:
  1. Emit Clojure forms for the current step only.
  2. Host evaluates them and records results/errors in <journal>.
  3. If you did NOT call `(answer ...)`, the host automatically continues the SAME user turn.
  4. In the next iteration, observe prior canonical refs/results and continue.
  5. Only call `(answer ...)` when the focused intent is fulfilled or abandoned with evidence.
  6. After iteration 1, the final iteration must contain exactly one top-level form: the `(answer ...)` form itself, or one wrapper such as `(let [...] (answer ...))`. In iteration 1 only, trivial chat may answer as the last top-level form after earlier setup/title/body forms.

Intent lifecycle for required tasks:
```clojure
(def intent
  (v/issue-intent! {:title TURN_USER_REQUEST
                    :rationale \"User asked for this objective.\"}))
intent
```

```clojure
(def plan
  (v/issue-plan! {:intent-id (:id intent)
                  :summary \"Inspect, act only on evidence, verify, then answer.\"
                  :steps [{:id :inspect}
                          {:id :act}
                          {:id :verify}]}))
plan
```

```clojure
(def verify-gate
  (v/issue-gate! {:plan-id (:id plan)
                  :question \"Did verification pass?\"}))
verify-gate
```

Ref discipline:
  Never invent refs. Never use compact refs such as `i1.1`, `i4.2/tool`, `E1`, or `G1` as writer input. Use canonical refs that exist in `<journal>` or `(v/provenance-timeline)`:
    turn/3f2a91c0/iteration/4/block/2
    turn/3f2a91c0/iteration/4/block/2/tool/bash
    turn/3f2a91c0/iteration/6/block/1/error
  Prefer observe-before-proof: run the tool/form, let it appear in <journal>, then cite that observed canonical ref.
  Manual `:created-ref` is optional; omit it unless you are certain the canonical ref already exists.

Prove or block gates with observed evidence:
```clojure
(v/prove-gate! (:id verify-gate)
  {:summary \"Targeted verification passed.\"
   :refs [\"turn/3f2a91c0/iteration/5/block/2\"]})
```

```clojure
(v/block-gate! (:id verify-gate)
  {:reason \"Full verification timed out; targeted checks passed but full suite did not complete.\"
   :refs [\"turn/3f2a91c0/iteration/6/block/1/error\"]})
```

Resolve the focused intent before `(answer ...)`:
```clojure
(v/fulfill-intent! (:id intent)
  {:summary \"User objective satisfied.\"
   :refs [\"turn/3f2a91c0/iteration/5/block/2\"]})
```

```clojure
(def checks
  {:intents   (v/intents)
   :provenance (v/provenance-guards)})
checks
```
If `(:ok? (:intents checks))` is false, fix the plan/gates, re-plan, prove/block gates, or abandon the intent. If the runtime rejects an answer, read the validation error, run `(v/intents)`, then fulfill/abandon focused intents with observed canonical refs.

Scratch values are still useful, but they are not proof. Persist and surface observations with `def`, then cite their refs after observing them:
```clojure
(def hits (v/rg [\"keyword\"] \"src\"))
hits
```
Reusable pure helpers are fine; keep effectful calls at leaves:
```clojure
(defn summarize-hits [hits]
  {:count (count (:hits hits))
   :paths (->> (:hits hits) (map :path) distinct vec)})
```

Top-level observability rule: a top-level form's result is mainly evidence for the NEXT iteration's <journal>. Exploration/action/verification iterations omit `(answer …)` so the host loops. Correct shape:
```clojure
(def hits (v/rg [\"foo\"] \"src\"))
(def summary {:count (count (:hits hits))
              :paths (->> (:hits hits) (map :path) distinct vec)})
summary
```
Then observe `summary` in <journal>, prove/block gates, verify, or continue.

Correct multi-iteration finish pattern:
```clojure
;; iteration N: verify and surface final evidence, no answer yet
(def checks {:intents (v/intents)
             :provenance (v/provenance-guards)})
checks
```
```clojure
;; iteration N+1: final turn-finisher after observed evidence, exactly one top-level form
(answer
  (v/join
    (v/h2 \"Summary\")
    (v/p \"Patched the requested behavior.\")
    (:report (v/intents))
    (v/provenance-report)))
```

Error rule: errors are evidence. If an intent/gate API fails, do not pretend the intent is resolved. Fix it, re-plan, or abandon/report with the exact error. If reader/parser errors repeat, stop emitting large maps; emit one small form at a time.

Answer shapes. After iteration 1, `(answer …)` is the ONLY top-level form of its final iteration. In iteration 1 only, trivial chat may answer as the last top-level form. Prefer Markdown helper composition unless the user requested a non-Markdown format:
```clojure
(answer (v/p \"Done.\"))
(answer (v/join (v/h2 \"Summary\") (v/p \"Patched three files.\")))
(answer (v/join (:report (v/intents)) (v/provenance-report)))
(let [body (v/join (:report (v/intents)) (v/provenance-report))]
  (answer body))
```
If you need any sibling top-level work after iteration 1, do not answer yet; do that work in earlier iterations, surface results, and answer alone in a later iteration.

Each iteration's user msg carries:
  <journal>     recent iterations: thinking + comments + code + results, with canonical provenance refs
  <var_index>   your `(def name val)` / `defn` bindings still alive in the sandbox
  [system_nudge] lines (when relevant) — e.g. set the conversation title or manage context pressure

SYSTEM vars (read-only; bound by name in the sandbox):
  TURN_USER_REQUEST            exact human-authored text submitted for this turn
  TURN_CONVERSATION_TURN_ID    UUID of the in-flight turn
  TURN_CONVERSATION_SOUL_ID    UUID of the parent conversation_soul
  TURN_CONVERSATION_STATE_ID   UUID of the conversation_state branch this turn lives on
  TURN_SYSTEM_PROMPT           the assembled system prompt for this turn
  TURN_ACTIVE_EXTENSIONS       vec of {:alias :namespace :doc :kind :version :author :owner :license
                                       :registry-id :source-paths :source-mtime-max :source-hash-sha256
                                       :symbols :docs} for every active extension.
                               \"Active\" = loaded into the sandbox; the model can call its symbols directly.
  TURN_ACCESSIBLE_SKILLS       vec of {:name :description :path :source} for every accessible skill (no body —
                               loading the body via `(v/load-skill name)` is the activation step). Use this vec
                               when the user asks \"what can you do\" / \"what skills do you have\"; never invent
                               a filter over TURN_ACTIVE_EXTENSIONS for skills.
                               Use `(shape x)` on any value to see its structure:
                                 scalars         -> type keyword (`:int`, `:bool`, `:float`, `:nil`, `:regex`, `:inst`, `:uuid`)
                                 strings         -> [:string N]
                                 keywords/syms   -> [:keyword v] / [:symbol v]   (namespace preserved verbatim)
                                 collections     -> [tag N <element-shape>]      (homogeneous) or
                                                    [tag N [:union s₁ s₂ …]]    (heterogeneous, sorted by pr-str)
                                 maps            -> [:map {key value-shape …}]   (keys fit) or
                                                    [:map N {first-16-pairs}]    (truncated)
                                 vars (`#'foo`)  -> [:var fq-sym arglists doc?]  for fn vars,
                                                     [:var fq-sym value-shape]    for value vars
                                 fns             -> :fn  (or [:fn arglists doc?] when meta is set)
                                 unknown JVM     -> \"java.fully.qualified.ClassName\"
                               Recurses 4 levels deep by default; pass `(shape x N)` to override.
  ITERATION_ID                 UUID of the last persisted iteration (nil before iter 1)
  ITERATION_PREVIOUS_REASONING last iteration's :thinking text
  CONVERSATION_TITLE           current conversation title (\"\" until set)
  CONVERSATION_METADATA        frozen-at-this-iter map of conversation facts
                                 {:title :channel :external-id :created-at :turn-count}
  CONVERSATION_PREVIOUS_ANSWER previous turn's final answer

Host primitives (top-level, no alias — named for what they write):
  (answer ARG)               terminal commit; use only when TURN_USER_REQUEST is fully satisfied, or explicitly blocked with evidence
  (conversation-title ARG)   one-arity title write; broadcasts to every channel watching the conversation. Read via the `CONVERSATION_TITLE` SYSTEM var.

SCI is sandboxed Clojure, not an unrestricted JVM. Stdlib aliases are pre-resolved; do NOT require them:
  str/    -> clojure.string         e.g. (str/split s #\",\") (str/blank? s) (str/join \", \" xs)
  set/    -> clojure.set            e.g. (set/union a b) (set/difference a b)
  walk/   -> clojure.walk           e.g. (walk/postwalk f form) (walk/keywordize-keys m)
  edn/    -> fast-edn.core          e.g. (edn/read-string s) (edn/read-string {:readers …} s)
  json/   -> charred.api            e.g. (json/read-json s) (json/write-json-str x)
  pp/     -> clojure.pprint         e.g. (pp/pprint x) (pp/pprint-str x)   (alias `pprint/` works too)
  zp/     -> zprint.core            e.g. (zp/zprint-str x) (zp/zprint x {:width 80})
  lt/     -> lazytest.core          e.g. (lt/expect-fn = a b) (lt/throws? Exception …)
  test/   -> clojure.test           e.g. (test/is (= a b))
  c+/     -> clojure+.core          e.g. (c+/cond+ …)
  s/      -> clojure.spec.alpha     e.g. (s/def ::id uuid?) (s/valid? ::id x) (s/keys :req-un [::id]) (s/conform ::shape m)

Extension aliases such as v/, z/, clj/ are preloaded when their extensions are active. Call their functions directly; do not write `(require …)` inside fenced code.")

(defn build-system-prompt
  "Core system prompt: agent rules + optional caller addendum.

   The `<environment>` block (cwd, OS, git facts, languages,
   monorepo shape) is NOT assembled here. It is rendered by the
   `vis-foundation` extension's `:ext/prompt` fragment, so
   the runtime no longer hardcodes any environment text. Drop the
   jar, drop the block."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    (when (and system-prompt (not (str/blank? system-prompt)))
      (str "\n\nINSTRUCTIONS:\n" system-prompt "\n"))))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a turn."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean ((:ext/activation-fn ext) environment))
                  (catch Throwable t
                    (tel/log! {:level :error :id ::ext-activation-error
                               :data {:ext (:ext/namespace ext)
                                      :error (ex-message t)}}
                      (str "Extension '" (:ext/namespace ext) "' activation-fn threw"))
                    false)))
        exts))))

(defn extensions-snapshot
  "Build the value of the `TURN_ACTIVE_EXTENSIONS` SYSTEM var from a precomputed
   active-extensions vec.

   Returns a vec of compact, fully-realized data maps — NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into `(v/extensions)` just to
   discover what's loaded.

   Per element:
     :alias     — short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext/ns-alias`.
     :namespace — fully-qualified ns symbol of the extension.
     :doc       — one-line LLM description from `:ext/doc` (when set).
     :kind      — categorical bucket (providers, channels, foundation,
                  languages, persistance, …) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :version   — semver string (when set).
     :author    — original author of the extension package (when set).
     :owner     — distribution / package owner (when set).
     :license   — SPDX license identifier (when set).
     :registry-id — canonical manifest/docs id, usually the alias symbol.
     :source-paths / :source-mtime-max / :source-hash-sha256
                — deterministic source markers resolved from the
                  classpath entry for this extension.
     :symbols   — vec of bare symbol names the extension intern'd into
                  the sandbox (just the names; signatures + doc come
                  from `(v/symbol-doc ...)` if the model wants them).
     :docs      — vec of doc-name strings (e.g. `\"README.md\"`) the
                  extension ships in its `vis.edn` registry. Reachable
                  via `(v/extension-doc 'id)`.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn — every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [provenance (extension/extension-provenance ext)
                  registry-id (:registry-id provenance)
                  ;; Resolve doc names through the global extension
                  ;; registry. Same mapping `(v/extensions)` uses;
                  ;; we duplicate the lookup here (instead of calling
                  ;; the meta extension) because the loop layer is
                  ;; upstream of every ext, including meta itself —
                  ;; TURN_ACTIVE_EXTENSIONS must work even when vis-foundation
                  ;; isn't on the classpath.
                  doc-names   (try (if registry-id
                                     (extension/extension-doc-names registry-id)
                                     [])
                                (catch Throwable _ []))]
              (assoc provenance
                :symbols (mapv :ext.symbol/sym (:ext/symbols ext))
                :docs    (vec doc-names)))))))

(def active-extensions-snapshot
  "Backwards-compatible public alias for `extensions-snapshot`."
  extensions-snapshot)

(defn accessible-skills-snapshot
  "Build the value of the `TURN_ACCESSIBLE_SKILLS` SYSTEM var: a vec of
   compact skill summaries the model can `filter`/`map`/`some` over
   without paying for the full SKILL.md body.

   Per element: `{:name :description :path :source :extra}`. The `:body`
   field is INTENTIONALLY omitted — it lazy-loads via `(v/load-skill name)`,
   the canonical activation surface. Pulling every body into a SYSTEM
   var would balloon turn-start memory for a value the model rarely
   needs in full (the `<skills>` block already shows name + description).

   Reads from `vis-foundation`'s skills cache via `requiring-resolve`.
   When the foundation extension isn't on the classpath (smoke build,
   pared-down packaging) the snapshot degrades to `[]`, matching the
   \"no skills installed\" case.

   Frozen ONCE at turn start (see `iteration-loop`). The model sees the
   same vec across every iteration of the same turn."
  []
  (or (when-let [list-all (try (requiring-resolve
                                 'com.blockether.vis.ext.foundation.environment.skills/list-all)
                            (catch Throwable _ nil))]
        (try
          (->> (list-all)
            (mapv (fn [s]
                    (cond-> (select-keys s [:name :description :path :source])
                      (seq (:extra s)) (assoc :extra (:extra s))))))
          (catch Throwable t
            (tel/log! {:level :warn :id ::skills-snapshot-failed
                       :data  {:error (ex-message t)}}
              "TURN_ACCESSIBLE_SKILLS snapshot failed; defaulting to []")
            nil)))
    []))

(defn- render-extension-prompt-block
  "Render one extension's contribution to the system prompt. Honors
   ONLY `:ext/prompt`. The previous implementation also auto-rendered
   every `:ext/symbols` entry as a `- (alias/sym args) — docstring`
   line, which silently ballooned the prompt for any extension whose
   `:ext/symbols` was a thin wrapper around an upstream library: e.g.
   `vis-language-clojure`'s `z/` extension dumped 104 rewrite-clj.zip
   publics with full upstream docstrings (~7000 tokens), every
   iteration, of every conversation — even when the user just typed
   a one-word greeting. Authors who want their tools advertised in
   the prompt write `:ext/prompt` (string or `(fn [env] string)`);
   sandbox bindings are independently callable from `:code` whether
   advertised or not. Extensions that legitimately want auto-rendered
   symbol lines may call `extension/render-prompt` from inside their
   own `:ext/prompt` fn — explicit beats clever."
  [environment ext]
  (try
    (when-let [extra-fn (:ext/prompt ext)]
      (let [body (extra-fn environment)]
        (when (and (string? body) (not (str/blank? body)))
          (if-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
            (str "[namespace: " alias-sym " → " ns-sym "]\n" body)
            body))))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-prompt-error
                 :data {:ext (:ext/namespace ext) :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' prompt rendering failed"))
      nil)))

(defn- render-provider-prompt-block
  "Render the active provider's append-only prompt contribution.

   `provider-prompt-context` is assembled by the loop so this namespace
   stays pure: `{:provider sanitized-provider-config
                 :descriptor registered-provider-descriptor
                 :model resolved-model
                 :environment environment}`.

   The provider prompt never replaces the core Vis prompt; it is a
   small provider-specific addendum. Hook failures are logged and the
   prompt contribution is skipped."
  [{:keys [provider descriptor] :as provider-prompt-context}]
  (try
    (when-let [prompt-fn (:provider/prompt-fn descriptor)]
      (let [body (prompt-fn provider-prompt-context)]
        (when (and (string? body) (not (str/blank? body)))
          (str "[provider: " (name (:id provider)) "]\n" body))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::provider-prompt-error
                 :data {:provider (:id provider)
                        :error    (ex-message t)}
                 :msg  (str "Provider prompt hook for " (:id provider)
                         " failed; skipping provider prompt block")})
      nil)))

(defn assemble-system-prompt
  "Build the full system prompt: core agent rules + active-extension prompts
   + the active provider's append-only prompt block.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`.

   Optional opts:
     `:provider-prompt-context` — map for provider `:provider/prompt-fn`:
        `{:provider sanitized-provider-config
          :descriptor registered-provider-descriptor
          :model resolved-model
          :environment environment}`."
  [environment {:keys [system-prompt active-extensions provider-prompt-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [base       (build-system-prompt {:system-prompt system-prompt})
        ext-ps     (keep #(render-extension-prompt-block environment %) active-extensions)
        provider-p (when provider-prompt-context
                     (render-provider-prompt-block provider-prompt-context))
        blocks     (seq (cond-> (vec ext-ps)
                          provider-p (conj provider-p)))]
    (if blocks
      (str base "\n\n" (str/join "\n\n" blocks))
      base)))
