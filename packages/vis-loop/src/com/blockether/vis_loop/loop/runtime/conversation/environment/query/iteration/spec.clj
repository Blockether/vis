(ns com.blockether.vis-loop.loop.runtime.conversation.environment.query.iteration.spec
  "svar specs for one LLM iteration response."
  (:require
   [com.blockether.svar.internal.spec :as spec]))

(def CODE_BLOCK_SPEC
  "Spec for a single code block with its expected execution time budget."
  (spec/spec :code-block
    (spec/field {::spec/name        :expr
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "One valid Clojure S-expression to execute in SCI (tool calls must be parenthesized)."})
    (spec/field {::spec/name        :time-ms
                 ::spec/type        :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Expected max execution time in ms. def 100, assert 500, heavy 2000. Max 5000."})
    (spec/field {::spec/name        :doc
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    false
                 ::spec/description "Optional docstring for the var defined by this :expr. Use this INSTEAD of Clojure's native `(def name \"docstring\" val)` / `(defn name \"docstring\" [args] body)` form — keep :expr clean: `(def name val)` and `(defn name [args] body)`. The loop attaches this string as :doc metadata after eval; <var_index> renders the first line. Ignored when :expr does not define a var."})))

(def NEXT_SPEC
  "Per-iteration steering hint. Both fields are optional — emit `:next` only
   when you want to change something for the next turn.

   `:model` picks a model class from the router's configured pool:
     cost         → cheapest available (rough drafts, simple lookups)
     speed        → fastest available (trivial follow-ups)
     intelligence → most capable available (hard reasoning, synthesis)

   `:reasoning` picks a thinking depth. Vis translates this to the right
   provider-specific parameter — `reasoning_effort` on OpenAI GPT-5/o-series,
   `thinking.budget_tokens` on Anthropic Claude 4.x. Models without native
   reasoning ignore it silently.
     quick    → short chain-of-thought, minimal tokens
     balanced → moderate reasoning (default for most turns)
     deep     → deep reasoning, large thinking budget

   The two dials are orthogonal: `:model :cost :reasoning :deep` is a valid
   combination that asks for the cheapest model at maximum depth."
  ;; Values-only enums — svar 0.3.2+ emits them as an inline type union
  ;; (`"cost" or "speed" or "intelligence"`) without a per-value comment
  ;; block. The STEERING section in `runtime.prompt` already explains
  ;; when to reach for each value, so re-iterating the descriptions in
  ;; the JSON schema would just burn tokens.
  (spec/spec :next-turn
    (spec/field {::spec/name        :model
                 ::spec/type        :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    false
                 ::spec/description "Model class to use on the next iteration"
                 ::spec/values      ["cost" "speed" "intelligence"]})
    (spec/field {::spec/name        :reasoning
                 ::spec/type        :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    false
                 ::spec/description "Thinking depth for the next iteration"
                 ::spec/values      ["quick" "balanced" "deep"]})))

(def PLAN_ITEM_SPEC
  "One TODO item in the agent's structured plan. Item :id is monotonic
   across iterations; once assigned, never reused for different content."
  (spec/spec :plan-item
    (spec/field {::spec/name        :id
                 ::spec/type        :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    true
                 ::spec/description "Stable item id, monotonic, unchanged across iterations."})
    (spec/field {::spec/name        :content
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    true
                 ::spec/description "Imperative one-liner describing the task."})
    (spec/field {::spec/name        :status
                 ::spec/type        :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    true
                 ::spec/values      ["pending" "in-progress" "done" "blocked"]
                 ::spec/description "Item status. Exactly ONE :in-progress at a time across the whole plan."})
    (spec/field {::spec/name        :evidence
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    false
                 ::spec/description "What proved completion / blockage (e.g. iN.K result id, var name)."})))

(def PLAN_STATE_SPEC
  "Full structured plan. Sticky across iterations — the loop carries it forward
   verbatim until the model re-emits :plan with deliberate changes."
  (spec/spec :plan-state
    {:refs [PLAN_ITEM_SPEC]}
    (spec/field {::spec/name        :goal
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required    true
                 ::spec/description "One-line task framing. Set on iteration 0; rarely changes."})
    (spec/field {::spec/name        :items
                 ::spec/type        :spec.type/ref
                 ::spec/target      :plan-item
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/required    true
                 ::spec/description "Ordered TODO list. ≤20 items. EXACTLY ONE :in-progress at a time (or zero if all done)."})
    (spec/field {::spec/name        :open
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/required    false
                 ::spec/description "Unanswered questions held for later."})
    (spec/field {::spec/name        :decided
                 ::spec/type        :spec.type/string
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/required    false
                 ::spec/description "Eliminated approaches with rationale."})))

(defn- make-iteration-spec
  "Builds an iteration response spec.

   Options:
     :include-thinking? — true for non-reasoning providers (chain-of-thought goes
       in JSON), false for reasoning providers (chain-of-thought is native, no
       duplication).

   ## Plan slot

   Iteration responses MAY include a structured `:plan` (see
   `PLAN_STATE_SPEC`) and `:breadcrumb` (≤120 chars past-tense one-liner).
   Both are sticky: the loop carries the most recent `:plan` verbatim
   across iterations until the model re-emits one. Breadcrumbs are
   appended to a bounded chain (last K=20) and presented in
   `<breadcrumbs>`. The model never has to call `(var-history)` to
   recover its own plan; the projection always shows the live plan +
   the last K breadcrumbs + the most recent `:thinking` text.

   When `:answer` is set with any plan item still :pending or
   :in-progress, `:abandon-reason` is required. The iteration handler
   enforces this cross-field rule (svar's spec engine doesn't model
   conditional required fields)."
  [{:keys [include-thinking?]}]
  (let [base-fields
        [(spec/field {::spec/name        :code
                      ::spec/type        :spec.type/ref
                      ::spec/target      :code-block
                      ::spec/cardinality :spec.cardinality/many
                      ::spec/required    true
                      ::spec/description "Required code blocks to execute this iteration. Emit AS MANY blocks as possible — pack all independent operations into one array. Empty array `[]` when no computation needed."})
         (spec/field {::spec/name        :next
                      ::spec/type        :spec.type/ref
                      ::spec/target      :next-turn
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "Optional steering for the next iteration."})
         (spec/field {::spec/name        :plan
                      ::spec/type        :spec.type/ref
                      ::spec/target      :plan-state
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "Structured plan. EMIT ON ITERATION 0. Carried verbatim across iterations until you re-emit. Re-emit only when reality forces a real change. Max 20 items. Exactly one :in-progress."})
         (spec/field {::spec/name        :breadcrumb
                      ::spec/type        :spec.type/string
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "≤120 char single line summarizing what THIS iteration accomplished, in the strategic frame of <plan>. Past tense. Mention which item id you advanced."})
         (spec/field {::spec/name        :answer
                      ::spec/type        :spec.type/string
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "Optional final answer for this iteration. Never use emoji/emoticons."})
         (spec/field {::spec/name        :abandon-reason
                      ::spec/type        :spec.type/string
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "Required when emitting :answer with open <plan> items. Concrete reason this turn cannot complete the plan (e.g. 'blocked: missing access to X')."})
         ;; There is intentionally NO `:answer-type` field. The iteration
         ;; handler renders every answer as a Mustache template against
         ;; sandbox vars. The model just emits `:answer`; one less ceremonial
         ;; field for the LLM to forget.
         (spec/field {::spec/name        :confidence
                      ::spec/type        :spec.type/keyword
                      ::spec/cardinality :spec.cardinality/one
                      ::spec/required    false
                      ::spec/description "Optional confidence level."
                      ::spec/values      ["high" "medium" "low"]})]

        fields
        (if include-thinking?
          (into [(spec/field {::spec/name        :thinking
                              ::spec/type        :spec.type/string
                              ::spec/cardinality :spec.cardinality/one
                              ::spec/description "Short reasoning for this iteration. Never use emoji/emoticons."})]
            base-fields)
          base-fields)]
    (apply spec/spec {:refs [CODE_BLOCK_SPEC NEXT_SPEC PLAN_STATE_SPEC]} fields)))

(defn iteration-spec
  "Compose the iteration response spec for the current environment state.
   `:has-reasoning?` selects the thinking/non-thinking variant."
  [{:keys [has-reasoning?]}]
  (make-iteration-spec {:include-thinking? (not has-reasoning?)}))

(def ITERATION_SPEC_BASE
  "Fixed variant — no :thinking (reasoning providers)."
  (make-iteration-spec {:include-thinking? false}))

(def ITERATION_SPEC_NON_REASONING
  "Fixed variant with :thinking (non-reasoning providers)."
  (make-iteration-spec {:include-thinking? true}))

(def ITERATION_SPEC_REASONING
  "Fixed reasoning variant — alias for ITERATION_SPEC_BASE."
  ITERATION_SPEC_BASE)
