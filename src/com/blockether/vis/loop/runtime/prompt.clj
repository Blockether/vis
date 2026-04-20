(ns com.blockether.vis.loop.runtime.prompt
  "System prompt construction for the RLM iteration loop.

   The narrative (one-rule preamble, surviving-state list, ARCH, GROUNDING,
   PERF, CLJ, RULES, OUTPUT) is FIXED and carries no tool-specific copy.
   STEERING is gated to multi-model envs only.

   Tool documentation is data-driven via `render-active-tools`: every tool
   in the registry (plus AMBIENT_TOOL_DEFS) contributes its own `:prompt`
   block ONLY when its `:activation-fn` returns truthy for the live env.
   Tool authors own their prompt copy; this namespace just assembles.

   Token budget anchor: stub env with no tools sits ~3.2k tokens
   (o200k_base). See dev/token_count.clj to re-measure."
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
   (one-rule preamble, surviving state, ARCH, GROUNDING, PERF, CLJ,
   RULES, OUTPUT) plus the iteration-spec response format.

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
           system-prompt skill-registry env tool-defs]}]
  (str
    "Clojure SCI agent. Your goal: SATISFY THE USER'S QUERY. Everything you do — every tool call, every line of code, every iteration — serves that single purpose. When the query is answered, stop.

Date: " (.truncatedTo (java.time.LocalDateTime/now) java.time.temporal.ChronoUnit/SECONDS) "

ONE RULE: you model your context via calls. Reasoning happens in :code, not in prose.
`(+ 2 2)` beats \"I think 4\". Asserts always have a message.

EVERY ITERATION:

  STEP 1 — READ. You receive three things:
    <var_index>       every `(def name val)` you've written. Survives until `:forget`.
    <journal>         the PREVIOUS iteration's results only (not N-2). For each :code
                      block: return value (auto-formatted), :stdout, :stderr, timing.
    <prior_thinking>  ONLY your previous iteration's reasoning. Older reasonings are NOT auto-shipped — pull them yourself with `(var-history '*reasoning*)` (oldest first; `take-last N` for a window).
    Plus SYSTEM vars (always present, `:forget` refused):
      *query*      current user query.
      *reasoning*  YOUR thinking from the previous iteration.
      *answer*     final answer from the previous turn in this conversation.
    If a/b/c already answers the query → STEP 4. Otherwise → STEP 2.

  STEP 2 — COMPUTE in :code. State the missing piece as a CLAIM and verify it.
    `(doc fn)` for tool docs. `(shape x)` for schema-only view of any value
    (types, ≤10 levels — beats probing with `(keys x)`/`(first ...)`).
    Aliases: str/ set/ walk/ edn/ json/ zp/ pp/ lt/ test/

  STEP 3 — PERSIST or DON'T:
    • One-shot value used only this iter         → bare expression in :code, no def.
    • Referenced by :answer / Mustache template  → `(def x val)`.
    • Needed >1 iteration ahead                  → `(def x val)`. Always.
    • Updating an existing concept               → REDEF the same name (vars show vN).
                                                    Don't invent `files-2`, `files-final`, etc.
    `(def x \"docstring\" val)` puts the docstring in <var_index>.
    `:forget [\"x\"]` evicts vars from the sandbox (DB rows survive).

  STEP 4 — FINALIZE. Set `:answer` + `:answer-type`. :code still runs first
    (even with :answer set), so it can be `[{:expr \":ok\" :time-ms 1}]` if
    you've nothing left to compute. If `:answer` is a single word matching a
    def, it auto-resolves to that var's value.

  Throughout: :code is an ARRAY — emit independent blocks in parallel,
  sequence only when later blocks depend on earlier results. Prefer one
  `(read-file path {:offset 1 :limit 500})` over many 30-line windows.

DIRECT ANSWER (greetings, plain prose): :code `[{:expr \":ok\" :time-ms 1}]` + `:answer`
directly. Don't wrap prose in `(def reply …)`.

MUSTACHE — :answer-type `mustache-text` | `mustache-markdown` (markdown renders as MD in UI):
  Sandbox vars = context. Tags: {{var}} {{#list}}..{{/list}} {{^val}}..{{/val}} {{.}} {{list.size}}.
  No pipe filters, no {{#each}}. Missing vars rejected — every referenced var must be def'd.
  Ex: :code [(def items [{:n \"A\"} {:n \"B\"}])],
      :answer \"{{items.size}} items:\\n{{#items}}• {{n}}\\n{{/items}}\".
"
    ;; STEERING is only rendered when the env actually has >1 model to switch
    ;; between. On single-model routers the :next.model dial is a no-op, so
    ;; burning ~200 tokens teaching the LLM to use it is pure waste. The JSON
    ;; schema still lists :next in the spec — this just drops the essay.
    (when (multi-model? env)
      "
STEERING (optional `:next` for next turn — omit for default):
- :next.model — 'cost' (trivial lookups/formatting), 'speed' (fast follow-ups), 'intelligence' (hard reasoning, debugging). Only set when current model is clearly mismatched.
- :next.reasoning — 'quick' (simple), 'balanced' (default — leave unset), 'deep' (ambiguous, multi-step, post-failure). Escalating every turn wastes tokens.
")
    "
GROUNDING: only tools in <tools> exist. :answer MUST come from <journal>, <var_index>, or tool values pulled this turn. No fabrication.

QUERY PRIMACY: `*query*` is the CURRENT user request. It overrides EVERYTHING in `*reasoning*` from a prior turn. On EVERY iteration 0 re-read `*query*` — if it contradicts what `*reasoning*` says, `*query*` wins. Prior reasoning is context, not instruction.

PERF: SCI is fast. def=100ms, assert=500ms, heavy=2000ms, grep/list-dir=5000ms, max 10000. Compute, don't scan — never `drop-while` millions, compute the start index. One block = one concern; vars persist across blocks.

TOOL DISCIPLINE:
- ONE broad grep beats many narrow ones. Use alternation: `(grep \"foo|bar|baz\" \".\")` not 3 calls.
- DEF grep results: `(def hits (grep ...))`. Results in a var survive; bare results vanish after the journal.
- DON'T grep a file you're about to `read-file` — the read already gives you the content.
- DON'T `read-file` a path you just grepped — grep already told you the line. Use `read-file path offset limit` to see context around the match.

CLJ:
- Recursion: `letfn`, never `(let [f (fn [] (f))] ...)`.
- `iterate` takes ONE-arg fn. Destructure pairs: `(fn [[a b]] ...)`, not `(fn [a b] ...)`.
- Prefer `(fn [x] ...)` over `#()`. Nested `#()` is illegal.
- Eager > lazy: `mapv` `filterv` `reduce` `into`.
- Quote lists: `'(1 2 3)`. One complete expr per block.
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
- No prose in :code. Bare string literal = wrong. Prose → :answer (see ARCH / MUSTACHE).
- Simplest solution. No over-eng. No unused abstractions.
- <journal> format: `[N] <code> → <value> :: <shape>` — `::` is the type-annotation marker (Haskell-style). LHS is the realized value, RHS is the structural sketch from `(shape v)`. For `(def name …)` the value renders as `*name* = <value> :: <shape>`. Atomic results (int/str/kw/bool) skip the `:: <shape>` suffix — the value already conveys its type.
- Comments inside :expr are accepted: `;; line comment` and `#_ discard` both work and are stripped during parse. Use them to state the WHY of a block before its code, not to restate WHAT the code does. `;; verify auth commits exist\\n(def hits (search-commits \"auth\"))` is good. `;; add 1 and 2\\n(+ 1 2)` is noise. Comment-only blocks (no executable form) are rejected.

OUTPUT: " OUTPUT_STYLE_GUIDE "
"))
