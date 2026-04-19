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
   [com.blockether.vis.loop.storage.schema :refer [iteration-spec]]
   [com.blockether.vis.loop.knowledge.skills :as rlm-skills]))

(def ^:private OUTPUT_STYLE_GUIDE
  "One merged voice guide — was split into CAVEMAN (iteration voice) +
   FINAL_ANSWER (final-voice) which dumped the same 'no hedging / no AI
   filler' advice into the prompt twice. Both contexts are covered in a
   single paragraph now; the iteration-vs-final distinction lives in a
   trailing parenthetical the caller already renders."
  "Factual, direct, concise. No AI filler (\"As an AI\", \"I believe\", \"In conclusion\"). No hedging. Tech terms exact, code unchanged. Tables/lists over prose. Iteration voice: fragments OK, [thing] [action] [reason] pattern, one word when enough. Final-answer voice: clear complete sentences.")

(defn environment-block
  "Build the `<environment>` section of the system prompt: CWD, home, user,
   platform, shell. Mirrors Claude Code's `# Environment` block.

   Rendered automatically for EVERY adapter (web, tui, telegram, cli) by
   `build-system-prompt` — individual adapters do NOT concatenate their
   own copy. The sentence about relative paths exists because file tools
   (read-file / write-file / edit-file / grep / list-dir) accept any
   `io/file` path, so without the CWD hint the model defaults to absolute
   paths that leak the host filesystem into the conversation DB.

   Pure read of `System/getProperty` + `System/getenv`; no secrets, no
   session state. Public so test code can assert expected shape."
  []
  (let [cwd    (System/getProperty "user.dir")
        os     (System/getProperty "os.name")
        arch   (System/getProperty "os.arch")
        shell  (or (System/getenv "SHELL") "unknown")
        user   (System/getProperty "user.name")
        home   (System/getProperty "user.home")]
    (str "\n<environment>\n"
      "  Working directory: " cwd "\n"
      "  Home directory: " home "\n"
      "  User: " user "\n"
      "  Platform: " os " (" arch ")\n"
      "  Shell: " shell "\n"
      "  File paths: the sandbox JVM's `user.dir` is the working directory above.\n"
      "    Prefer RELATIVE paths (e.g. `src/foo.clj`) over absolute paths for any\n"
      "    file tool — read-file, write-file, edit-file, grep, list-dir all resolve\n"
      "    relative paths against `user.dir`.\n"
      "</environment>\n")))

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
    :prompt "Sync sub-query. Opts: `:routing {:optimize :cost|:speed|:intelligence}`, `:reasoning :normal|:deep`, `:max-iter` (default 1). Use when sub-problem needs different model/depth; else inline code."}

   {:sym 'sub-rlm-query-batch
    :group "meta"
    :arglists '([items])
    :activation-fn (constantly true)
    :examples ["(sub-rlm-query-batch [{:prompt \"q1\"} {:prompt \"q2\"}])"
               "(sub-rlm-query-batch [{:prompt \"cheap\" :routing {:optimize :cost}} {:prompt \"hard\" :reasoning :deep}])"]
    :prompt "Parallel `sub-rlm-query`. Takes `[{:prompt \"...\" …opts} …]`, returns a vector of answer strings in order. Use when sub-tasks are independent."}

   {:sym 'request-more-iterations
    :group "meta"
    :arglists '([n])
    :activation-fn (constantly true)
    :examples ["(request-more-iterations 10)"
               "(request-more-iterations 50)"]
    :prompt "Extend iteration budget by `n` (≤50/call, 500 total). Returns `{:granted k :new-budget M :cap C}`. Call EARLY when you see you need more; only with a concrete plan. Default 10 is enough for most."}])

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

(defn- resolve-group-preamble
  "Resolve the first non-blank `:group-preamble` among the active tools of
   a group to a single string. Rendered once at the top of the group's
   `<group>` block, so tools in the group don't each have to paste the
   same context (repo summary, shared caveats, etc.) into their own
   `:prompt`. The preamble may be a string or a `(fn [env]) → string`."
  [env tools-in-group]
  (some (fn [t]
          (let [p (:group-preamble t)
                s (cond
                    (string? p) p
                    (fn? p)     (try (p env) (catch Throwable _ nil))
                    :else       nil)]
            (when (and s (not (str/blank? s))) (str/trim s))))
    tools-in-group))

(defn render-active-tools
  "Render every tool in `tool-defs` whose activation-fn passes for `env`
   into a single <tools> block, grouped by :group. Returns nil when no
   tool is active — the caller should elide the block entirely.

   A group may carry a one-shot `<group-preamble>` chunk: the first
   active tool in the group that declares `:group-preamble` (string or
   `(fn [env])`) contributes it. This keeps shared context (e.g. the
   git repo summary) out of every sibling `:prompt` — was pasted 7×
   across the git tools before. Sibling prompts should assume the
   preamble is present.

   Preconditions:
   - `tool-defs` is a seq of canonical tool-defs (each with :sym,
     :activation-fn, optionally :prompt / :group / :group-preamble /
     :examples / :arglists).
   - `env` is the live RLM env; :activation-fn, :prompt, and
     :group-preamble all receive it."
  [env tool-defs]
  (when (seq tool-defs)
    (let [active  (filter #(active-tool? env %) tool-defs)
          with-bodies (keep #(when-let [body (render-one-tool env %)]
                               [(group-label (:group %)) (:sym %) body %])
                        active)]
      (when (seq with-bodies)
        (let [by-group (group-by first with-bodies)
              groups   (sort (keys by-group))]
          (str "\n<tools>\n"
            (str/join "\n"
              (for [g groups
                    :let [group-entries (get by-group g)
                          preamble (resolve-group-preamble env (map #(nth % 3) group-entries))
                          entries (->> group-entries
                                    (sort-by second)
                                    (map #(nth % 2)))]]
                (str "<group name=\"" g "\">\n"
                  (when preamble
                    (str "<group-preamble>\n" preamble "\n</group-preamble>\n"))
                  (str/join "\n" entries)
                  "\n</group>")))
            "\n</tools>\n"))))))

(defn- multi-model?
  "True when the env's router has more than one callable model available.
   Used to gate the STEERING section — there's no point telling the LLM to
   pick between `:cost` / `:speed` / `:intelligence` when only one model
   can actually run."
  [env]
  (when-let [router (:router env)]
    (> (count (mapcat :models (:providers router))) 1)))

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
- DIRECT ANSWER: for greetings or plain text replies, leave :code empty and set :answer directly. The previous turn's final is already bound as SYSTEM var *answer*. Don't wrap prose in (def reply …).
- DEF ONLY FOR CROSS-ITERATION STATE: <journal> already shows every value your last iteration computed — use them directly (inline a count, copy a literal, compose a new pipeline). `(def x …)` earns its keep only when (1) you'll need x more than ONE iteration ahead (journal keeps only the previous iter), (2) x is the anchor for `:answer` (single-word resolve or `{{x}}` in Mustache), or (3) recomputing x is genuinely expensive (huge file parse, slow API). For ephemeral intermediates, skip def — the journal is your scratchpad.
- BATCH WORK: :code is an array — emit many independent blocks per iteration. Split into sequential iterations ONLY when later blocks depend on earlier results. Each iter = full round-trip.
- READ LARGER: prefer one (read-file path) or (read-file path {:offset 1 :limit 500}) over many 30-line windows — the file fits in context once.
- ITERATION BUDGET: default cap 10. On <budget> warning, finalize with a partial answer or call (request-more-iterations). Don't silently run off the end.
- :code ALWAYS executes — even with :final. Code runs first, then :final is accepted.
- VAR RESOLVE: :answer single word matching a def → auto-resolved to var value.
  Useful when the answer IS a computed value you already stored:
    :code [(def totals (map sum-row rows))]
    :answer \"totals\" → user sees the vector's string form.
  NOT useful for prose you could just inline. See DIRECT ANSWER above.
- MUSTACHE (:answer-type = mustache-text | mustache-markdown — the -markdown variant renders in the UI as Markdown, otherwise identical):
  Sandbox vars = context. Tags: {{var}} {{#list}}..{{/list}} {{^val}}..{{/val}} {{.}} {{list.size}}.
  No pipe filters. No {{#each}} — use {{#list}}. Missing vars rejected; define all referenced vars in :code first.
  Ex: :code [(def items [{:n \"A\"} {:n \"B\"}])], :answer \"{{items.size}} items:\\n{{#items}}• {{n}}\\n{{/items}}\", :answer-type mustache-text.
- :forget evicts vars from sandbox. Emit :forget only when actually dropping vars this iteration.
"
    ;; STEERING is only rendered when the env actually has >1 model to switch
    ;; between. On single-model routers the :next.model dial is a no-op, so
    ;; burning ~200 tokens teaching the LLM to use it is pure waste. The JSON
    ;; schema still lists :next in the spec — this just drops the essay.
    (when (multi-model? env)
      "
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
")
    "
GROUNDING:
- Only tools listed in <tools> exist. Data in :final MUST come from <journal>, <var_index>, or values you explicitly pulled via a tool this turn. Never fabricate.

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
    ;; Runtime environment block — CWD/home/user/platform/shell + the
    ;; relative-paths hint for file tools. Injected HERE, not by each
    ;; adapter, so web/telegram/tui/cli can never drift on this. See
    ;; `environment-block` for the full rationale.
    (environment-block)
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
Set final fields when done: {\"answer\": \"...\", \"answer-type\": \"mustache-text|mustache-markdown\", \"confidence\": \"high|medium|low\"}

RULES:
- ALWAYS test. Untested = wrong. No repeat fail → different approach.
- <var_index>|<journal> answers query → finalize now.
- No prose in :code. Bare string literal = wrong. Prose → :answer (see ARCH / MUSTACHE).
- def only cross-iter state / :answer anchors / expensive recomputes — journal already shows last iter in full.
- Simplest solution. No over-eng. No unused abstractions.

OUTPUT: " OUTPUT_STYLE_GUIDE "
"))
