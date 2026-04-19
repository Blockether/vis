(ns com.blockether.vis.loop.runtime.prompt
  "System prompt construction for the RLM iteration loop.

   The system prompt itself is FIXED: MINDSET / SYMBOLIC REASONING / CONTEXT
   MODEL / ARCH / STEERING / GROUNDING / PERF / CLJ / RESPONSE FORMAT.
   It contains no tool-specific copy.

   Tool documentation is data-driven via `render-active-tools`: every tool
   in the registry (plus AMBIENT_TOOL_DEFS) contributes its own `:prompt`
   block ONLY when its `:activation-fn` returns truthy for the live env.
   Tool authors own their prompt copy; this namespace just assembles."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.spec :as spec]
   [com.blockether.vis.loop.storage.schema
    :refer [iteration-spec
            ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING]]
   [com.blockether.vis.loop.knowledge.skills :as rlm-skills]))

(def ^:private CAVEMAN_ITERATION_OUTPUT
  "Drop: articles, filler, hedging, conjunctions. Fragments OK. → for causality. One word when enough. Tech terms exact. Code unchanged.
Pattern: [thing] [action] [reason]. [next step].")

(def ^:private FINAL_ANSWER_OUTPUT
  "Normal English. Clear, direct sentences. No AI filler (no \"As an AI\", \"I believe\",
\"In conclusion\"). No hedging. Factual. Technical terms exact. Concise but complete. Prefer tables and lists over prose.")

(defn- render-constant
  "Render a single `:type :def` constant registered via register-env-def!.
   Constants are literal bindings (no activation, no callable), so the
   block is minimal: symbol + one-line doc + example if provided."
  [{:keys [sym doc examples]}]
  (str "  <constant name=\"" sym "\">\n"
    (when doc (str "    " doc "\n"))
    (when (seq examples)
      (str "    Examples:\n"
        (str/join "\n" (map #(str "      " %) examples)) "\n"))
    "  </constant>"))

(defn render-constants
  "Render `:type :def` constants into a <constants> block.

   Tools (`:type :fn`) flow through `render-active-tools` + the `<tools>`
   block — they carry `:prompt` / activation. This block is reserved for
   constants registered via `register-env-def!` which don't have
   activation semantics and aren't callable."
  [custom-docs]
  (let [defs (filter #(= :def (:type %)) custom-docs)]
    (when (seq defs)
      (str "\n<constants>\n"
        (str/join "\n" (map render-constant defs))
        "\n</constants>\n"))))

;; =============================================================================
;; Ambient tool-defs — always active, bound per-query via sci-update-binding!
;; =============================================================================
;;
;; These don't live in `tool-registry-atom` because they're constructed per-query
;; (e.g. request-more-iterations closes over the per-query max-iterations-atom).
;; Their prompt-level docs belong in the <tools> block, though, so we carry a
;; static list of prompt-only tool-defs here and merge it in at render time.

(def ^:private AMBIENT_TOOL_DEFS
  [{:sym 'sub-rlm-query
    :group "meta"
    :arglists '([prompt] [prompt opts])
    :activation-fn (constantly true)
    :examples ["(sub-rlm-query \"Summarize src/foo.clj in one sentence\")"
               "(sub-rlm-query \"cheap lookup\" {:routing {:optimize :cost}})"
               "(sub-rlm-query \"hard derivation\" {:routing {:optimize :intelligence} :reasoning :deep})"
               "(sub-rlm-query \"multi-step\" {:max-iter 5})"]
    :prompt "Spawn a sub-query against a (possibly different) model. Returns a string
answer synchronously.

Args: prompt (string, required), opts (map, optional).
Opts:
  :routing   — `{:optimize :cost | :speed | :intelligence}`. Default: same
               model as the current turn.
  :reasoning — `:normal | :deep` when the target model supports reasoning.
  :max-iter  — multi-iteration budget for the sub-query (default 1 — single-shot).

Use for: cheap lookups inside a hard turn (`:cost`), isolated hard
derivations inside an easy turn (`:intelligence :deep`), or farming out
a sub-problem that doesn't need the full outer context. Do NOT use for
work that could be done inline with code."}

   {:sym 'sub-rlm-query-batch
    :group "meta"
    :arglists '([items])
    :activation-fn (constantly true)
    :examples ["(sub-rlm-query-batch [{:prompt \"q1\"} {:prompt \"q2\"}])"
               "(sub-rlm-query-batch [{:prompt \"cheap\" :routing {:optimize :cost}} {:prompt \"hard\" :reasoning :deep}])"]
    :prompt "Run many independent sub-queries in parallel. Takes a vector of
`{:prompt ... (opts)}` maps, returns a vector of answer strings in the
same order.

Use when you have independent sub-tasks you'd otherwise fire with
multiple sub-rlm-query calls. The batch variant parallelizes across
provider connections."}

   {:sym 'request-more-iterations
    :group "meta"
    :arglists '([n])
    :activation-fn (constantly true)
    :examples ["(request-more-iterations 10)"
               "(request-more-iterations 50)"]
    :prompt "Extend the current turn's iteration budget.

Args: n (positive int) — how many EXTRA iterations to grant. Max 50 per
request, hard ceiling 500 total. Returns `{:granted k :new-budget M :cap C}`.

Call this AS SOON as you realize the turn needs more steps — don't wait
for the last iteration; the cap terminates before you get another chance.
Only extend when you have a concrete plan for the extra turns; don't
request speculatively. Default budget (10) is plenty for most tasks."}])

;; =============================================================================
;; Activation-aware tool prompt rendering
;; =============================================================================
;;
;; Every tool can carry a `:prompt` (string or fn of env). When the tool's
;; activation-fn returns truthy for the current env, its prompt is rendered
;; into the <tools> block. This replaces the hardcoded per-feature blocks
;; (git, documents, history, concepts) with data-driven injection:
;; tool authors own their own prompt copy, and the system prompt just asks
;; "which tools are active right now?"

(defn- trim-prompt
  "Trim leading/trailing blanks and collapse >2 consecutive blank lines.
   Keeps per-tool prompts visually consistent even when authors double-space."
  [s]
  (when (string? s)
    (-> s
      str/trim
      (str/replace #"\n{3,}" "\n\n"))))

(defn- resolve-tool-prompt
  "Resolve (:prompt tool-def) to a trimmed string or nil.

   Accepts either shape so callers can pass raw prompt-only tool-defs
   (e.g. AMBIENT_TOOL_DEFS) without routing through `make-tool-def`:
   - string → used as-is.
   - fn     → invoked with `env`."
  [env tool-def]
  (let [p (:prompt tool-def)]
    (cond
      (string? p) (trim-prompt p)
      (fn? p)     (try (trim-prompt (p env)) (catch Throwable _ nil))
      :else       nil)))

(defn- active-tool?
  "True when the tool's activation-fn returns truthy for env. Missing
   activation-fn is treated as always-active (matches make-tool-def default)."
  [env tool-def]
  (let [af (:activation-fn tool-def)]
    (or (nil? af) (boolean (try (af env) (catch Throwable _ false))))))

(defn- format-tool-signature
  "Render a compact `(sym arg1 arg2)` hint from :arglists. Picks the LONGEST
   arity so the LLM sees every positional arg (incl. optional trailing ones).
   `[& args]`-only fns render as `(sym & args)`."
  [sym arglists]
  (let [al   (->> arglists (sort-by count >) first)
        args (when (seq al)
               (->> al
                 (map str)
                 (str/join " ")))]
    (str "(" sym (when-not (str/blank? args) (str " " args)) ")")))

(defn- render-one-tool
  "Render a single tool as a <tool> block. `:prompt` carries the rich
   guidance; `:examples` / signature fall through as auxiliary hints."
  [env {:keys [sym group examples arglists] :as tool-def}]
  (let [prompt   (resolve-tool-prompt env tool-def)
        sig-line (format-tool-signature sym arglists)]
    (when (or prompt (seq examples))
      (str "<tool name=\"" sym "\""
        (when group (str " group=\"" group "\""))
        ">\n"
        "  <signature>" sig-line "</signature>\n"
        (when prompt
          (str "  <prompt>\n"
            (->> (str/split-lines prompt)
              (map #(str "    " %))
              (str/join "\n"))
            "\n  </prompt>\n"))
        (when (seq examples)
          (str "  <examples>\n"
            (->> examples
              (map #(str "    " %))
              (str/join "\n"))
            "\n  </examples>\n"))
        "</tool>"))))

(defn- group-label
  "Group sort key: missing/blank groups sort to the end under `misc`."
  [group]
  (if (str/blank? group) "misc" group))

(defn render-active-tools
  "Render every tool in `tool-defs` whose activation-fn passes for `env`
   into a single <tools> block, grouped by :group. Returns nil when no
   tool is active — the caller should elide the block entirely.

   Preconditions:
   - `tool-defs` is a seq of canonical tool-defs (each with :sym, :activation-fn,
     optionally :prompt / :group / :examples / :arglists).
   - `env` is the live RLM env; :activation-fn and :prompt both receive it."
  [env tool-defs]
  (when (seq tool-defs)
    (let [active  (filter #(active-tool? env %) tool-defs)
          with-bodies (keep #(when-let [body (render-one-tool env %)]
                               [(group-label (:group %)) (:sym %) body])
                        active)]
      (when (seq with-bodies)
        (let [by-group (group-by first with-bodies)
              groups   (sort (keys by-group))]
          (str "\n<tools>\n"
            (str/join "\n"
              (for [g groups
                    :let [entries (->> (get by-group g)
                                    (sort-by second)
                                    (map #(nth % 2)))]]
                (str "<group name=\"" g "\">\n"
                  (str/join "\n" entries)
                  "\n</group>")))
            "\n</tools>\n"))))))

(defn build-system-prompt
  "Builds the system prompt — compact, token-efficient.

   Tool documentation is fully DATA-DRIVEN: every tool in `:tool-defs`
   whose `:activation-fn` returns truthy for `:env` contributes its
   `:prompt` into the rendered `<tools>` block. The system prompt itself
   carries no tool-specific copy — it's just agent-contract text
   (MINDSET / CONTEXT / ARCH / CLJ / RESPONSE FORMAT).

   Opts:
     :env              — live RLM env; required for tool rendering.
     :tool-defs        — canonical tool-defs from (:tool-registry-atom env).
     :has-reasoning?   — provider reasoning support (drives iteration-spec).
     :has-documents?   — drives iteration-spec :sources field.
     :output-spec      — optional caller-provided output schema.
     :system-prompt    — optional caller-provided INSTRUCTIONS block.
     :skill-registry   — skills manifest (still a hardcoded block).
     :custom-docs      — `:type :def` constants from register-env-def!."
  [{:keys [output-spec custom-docs has-reasoning? has-documents?
           system-prompt skill-registry env tool-defs]
    :as opts}]
  (str
    "Clojure SCI agent. Write, exec, iterate.
Current date/time (server local): " (.truncatedTo (java.time.LocalDateTime/now) java.time.temporal.ChronoUnit/SECONDS) "


MINDSET:
- ALL reasoning MUST happen in :code. The SCI sandbox is your brain. Think by computing, not by writing prose.
- NEVER mentally simulate, estimate, or speculate. Write code, run it, read <journal>.
- Even for simple math, dates, string ops — CODE IT. (+ 2 2) beats \"I think 4\".
- Reasoning text: 2-5 lines max to state intent. Then CODE.
- Text/Q&A tasks: fetch data with tools, then :final.
- Asserts: ALWAYS (assert expr \"message\"). Bare asserts = useless errors.

SYMBOLIC REASONING — prefer data and predicates over prose:
- Model the problem as DATA first: sets, maps, vectors, relations. Prose is a last resort.
- Def NAMED FACTS as you observe them: (def fact-1 {:file path :line 42 :claim \"grep uses String paths\"}).
- Grow a fact set incrementally: (def facts (conj (or facts #{}) fact-1 fact-2)).
- Write PREDICATES instead of narrating. (def valid? (fn [x] (and (map? x) (:path x)))).
- Verify claims with clojure.core: (every? pred xs), (some pred xs), (= expected actual), (filter pred xs).
- Use SET OPERATIONS for whole-collection reasoning: clojure.set/intersection, /difference, /union, /select.
- Use DESTRUCTURING to pattern-match shapes. A mismatch is a fact, not a failure — def it and move on.
- When stuck, derive the MINIMAL concrete example as data, solve it there, then generalize with (mapv f xs).
- When a hypothesis fails (assert), FORK: def a new hypothesis, don't re-run the disproved one.
- When the journal shows a large value, REFERENCE IT BY VAR NAME next iteration — don't re-fetch.

CONTEXT MODEL — the prompt is FIXED size; you PULL what you need:
- <journal>         — previous iteration's execution results (ONLY the previous one).
- <var_index>       — every def'd var, including SYSTEM vars (marked `(SYSTEM, …) …` in the doc column).
- SYSTEM vars are bound by the agent loop, usable like any other SCI var,
  and NEVER forgotten (:forget on them is silently refused):
    *query*         current user query (this turn).
    *reasoning*     YOUR thinking from the previous iteration.
    *answer*        final answer from the previous turn in this conversation.
- Everything older is ON-DEMAND via tools in <tools>. Nothing else accumulates.

ARCH:
- Single-shot iter. State = def'd vars. <var_index> = vars. <journal> = last iteration's results.
- Cross-query memory is ONLY def'd vars. Plain final answers do not persist.
- (doc fn) for tool docs. Aliases: str/ set/ walk/ edn/ json/ zp/ pp/ lt/ test/
- (def x \"docstring\" val) → docstring in <var_index>. Defs for reusable state only.
- VAR REUSE: ALWAYS redef existing vars instead of creating new names for the same concept.
  Check <var_index> first. (def file-list ...) again, NOT (def files ...). Vars show (vN) when updated.
- STORE RESULTS: Always (def answer-name result) to persist computed answers.
  Plain :final text does NOT persist across queries. Only def'd vars survive.
- BATCH WORK: :code is an array — emit MANY blocks per iteration when work is independent.
  Prefer one iteration with [(def a ...) (def b ...) (def c ...) (analyze a b c)] over three
  iterations. Each iteration costs a full round-trip; batching saves tokens and latency.
  Split into multiple iterations only when later blocks depend on results from earlier ones.
- READ LARGER: when reading files, pull big chunks, not tiny slices. Prefer (read-file path)
  or (read-file path {:offset 1 :limit 500}) over 30-line windows. Re-reading the same file
  to grow your window is wasteful — the file fits in context, just fetch enough the first time.
- ITERATION BUDGET: default cap is 10 iterations. When the prompt shows a <budget> warning,
  either finalize with a partial answer or extend via `request-more-iterations` (see <tools>).
  Don't silently run off the end — that yields an empty bubble.
- :code ALWAYS executes — even with :final. Code runs first, then :final is accepted.
- VAR RESOLVE: :answer single word matching a def → auto-resolved to var value.
  Example: :code [(def reply (str \"Answer: \" x))], :answer \"reply\" → user sees string.
- MUSTACHE: :answer-type mustache-text or mustache-markdown to render :answer as Mustache.
  Sandbox vars = context. {{var}}, {{#list}}..{{/list}}, {{^val}}..{{/val}}, {{.}}, {{list.size}}.
  NO pipe filters. NO {{#each}} → use {{#list}} directly.
  mustache-text = plain text. mustache-markdown = Markdown output.
  Example: :code [(def items [{:n \"A\"} {:n \"B\"}])],
           :answer \"{{items.size}} items:\\n{{#items}}• {{n}}\\n{{/items}}\", :answer-type mustache-text.
  Missing vars → rejected. Define all referenced vars in :code first.
- :forget evicts vars from sandbox. Emit :forget only when actually dropping vars this iteration.

STEERING (optional :next map for the NEXT turn — omit entirely for the default path):
- :next.model — switch model class. 'cost' for trivial lookups / formatting, 'speed' for
  fast follow-ups, 'intelligence' for hard reasoning / synthesis / debugging tricky code.
  Only set when the current model is clearly under- or over-powered for the next turn.
- :next.reasoning — thinking depth. 'quick' for simple assertions, var lookups, obvious
  code. 'balanced' is the everyday default — leave :reasoning unset or choose balanced.
  'deep' for ambiguous requirements, multi-step analysis, cross-referencing documents,
  or debugging after repeated failures. One level up after a recoverable error is fine;
  escalating to deep every turn wastes tokens.
- Both dials are orthogonal. {:model 'cost' :reasoning 'deep'} means 'think hard on the
  cheapest model'. Keep the pair empty unless you actually want to change something.

GROUNDING:
- Only tools listed in <tools> exist. Data in :final MUST come from <journal>, <var_index>, or
  values you explicitly pulled via a tool this turn. Never fabricate.
- :answer-type mustache-text|mustache-markdown → Mustache-rendered. All referenced vars MUST be def'd.

PERF:
- SCI is FAST. def=100ms, assert=500ms, heavy=2000ms max. No 5000+ budgets.
- COMPUTE, DON'T SCAN. Never drop-while millions. Compute start from n directly.
- Separate def from tests. One block = one concern. Vars persist across blocks.

CLJ:
- letfn for recursion. (let [f (fn [] (f))] ...) → BROKEN. Use letfn.
- iterate = ONE arg. Destructure: (fn [[a b]] ...) not (fn [a b] ...).
- Prefer (fn [x] ...) over #(). Nested #() = illegal.
- Eager > lazy: mapv filterv reduce into.
- Quote lists: '(1 2 3). Complete expr per block. No fragments.
"
    (rlm-skills/skills-manifest-block skill-registry)
    (when system-prompt
      (str "\nINSTRUCTIONS:\n" system-prompt "\n"))
    ;; Data-driven tool prompts: every active tool contributes its own
    ;; :prompt block. Tools whose activation-fn returns false are elided.
    ;; Ambient tools (`sub-rlm-query`, `request-more-iterations`, ...) are
    ;; always-on and contributed from AMBIENT_TOOL_DEFS, merged with the
    ;; registry entries. Dedup by :sym, registry wins (it has the live :fn).
    (when env
      (let [by-sym (into {}
                     (map (fn [t] [(:sym t) t]))
                     (concat AMBIENT_TOOL_DEFS tool-defs))]
        (render-active-tools env (vals by-sym))))
    (render-constants custom-docs)
    (when output-spec
      (str "\nOUTPUT SCHEMA:\n" (spec/spec->prompt output-spec) "\n"))
    "
RESPONSE FORMAT:
"
    ;; Build the iteration spec from the current env's activation flags so
    ;; `:sources` is only advertised when document-retrieval tools actually
    ;; exist this turn. Without docs, the LLM has no way to produce valid
    ;; source IDs — no point in nudging it to try.
    (spec/spec->prompt (iteration-spec {:has-reasoning? has-reasoning?
                                        :has-documents? has-documents?}))
    "
"
    (if has-reasoning?
      "JSON only. Native reasoning — omit 'thinking'."
      "JSON with 'thinking' + 'code'.")
    "
Set final fields when done: {\"answer\": \"...\", \"answer-type\": \"mustache-text\", \"confidence\": \"high|medium|low\"}

RULES:
- ALWAYS test. Untested = wrong. No repeat fail → different approach.
- <var_index>|<journal> answers query → finalize now.
- No prose in :code. Bare string literal = wrong. Prose → :answer with mustache-text or mustache-markdown.
- Simplest solution. No over-eng. No unused abstractions.

OUTPUT: "
    CAVEMAN_ITERATION_OUTPUT " (iterations). "
    FINAL_ANSWER_OUTPUT " (final answer).
Answer → top-level final fields when done. No boilerplate.
"))
