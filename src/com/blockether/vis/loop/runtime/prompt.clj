(ns com.blockether.vis.loop.runtime.prompt
  "System prompt construction for the RLM iteration loop."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.spec :as spec]
   [com.blockether.vis.loop.storage.db :as rlm-db]
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

(defn- format-param
  [{:keys [name type required description default]}]
  (str "      " name " - " (clojure.core/name (or type :any))
    (when-not (true? required) " (optional)")
    (when (some? default) (str ", default: " (pr-str default)))
    (when description (str " — " description))))

(defn- format-tool-doc
  [{:keys [type sym doc params returns examples]}]
  (str "  <" (clojure.core/name type) " name=\"" sym "\">\n"
    (when doc (str "    " doc "\n"))
    (when (seq params)
      (str "    Parameters:\n"
        (str/join "\n" (map format-param params)) "\n"))
    (when returns
      (str "    Returns: " (clojure.core/name (or (:type returns) :any))
        (when (:description returns) (str " — " (:description returns)))
        "\n"))
    (when (seq examples)
      (str "    Examples:\n"
        (str/join "\n" (map #(str "      " %) examples)) "\n"))
    "  </" (clojure.core/name type) ">"))

(defn format-custom-docs
  "Formats custom docs for the system prompt."
  [custom-docs]
  (when (seq custom-docs)
    (str "\n<custom_tools>\n"
      (str/join "\n" (map format-tool-doc custom-docs))
      "\n</custom_tools>\n")))

(defn build-document-summary
  "Builds a compact summary of available documents for the system prompt."
  [db-info]
  (when (:datasource db-info)
    (let [docs (rlm-db/db-list-documents db-info {:include-toc? false})
          page-counts (when (seq docs)
                        (into {}
                          (for [doc docs]
                            [(:id doc)
                             (rlm-db/db-count-document-pages db-info (:id doc))])))
          total-pages (reduce + 0 (vals page-counts))
          types-map (rlm-db/db-entity-type-counts db-info)
          total-entities (reduce + 0 (vals types-map))
          entity-stats {:total total-entities :types types-map}]
      (when (seq docs)
        (str (count docs) " documents, " total-pages " pages"
          (when (pos? total-entities)
            (str ", " total-entities " entities ("
              (str/join ", " (map (fn [[t c]] (str (name t) ": " c))
                               (take 5 (sort-by val > (:types entity-stats)))))
              ")"))
          "\nDocuments:\n"
          (str/join "\n"
            (map (fn [doc]
                   (let [id (:id doc)
                         title (or (:title doc) (:name doc) id)
                         ext (:extension doc)
                         pages (get page-counts id 0)]
                     (str "  [" id "] " title
                       (when ext (str " (" ext ")"))
                       ", " pages " pages")))
              (sort-by :id docs))))))))

(defn- format-git-context
  [git-repos]
  (when (seq git-repos)
    (let [multi? (> (count git-repos) 1)
          blocks (for [rm (sort-by :name git-repos)
                       :let [{:keys [name path head-short branch commits-ingested]} rm]]
                   (str "
GIT REPO: " name " (" path ")
  head: " (or head-short "?") (when branch (str " on " branch)) "
  commits ingested: " (or commits-ingested 0)))
          tools (if multi?
                  "
Git tools this session — all prefixed `git-`. JGit-side tools auto-dispatch:
  path-based (git-blame, git-file-history) → pass ABSOLUTE path inside target repo's worktree
  sha-based  (git-commit-diff) → pass SHA; refs like HEAD are ambiguous, forbidden
  (git-search-commits {:category :bug :document-id \"<name>\" ...})
  (git-commit-history {:limit 20 :document-id \"<name>\"})
  (git-commits-by-ticket \"SVAR-42\")
  (git-commit-parents \"abc123def456\")
  (git-file-history \"/abs/path/to/file.clj\" {:n 10})
  (git-blame \"/abs/path/to/file.clj\" 42 58)
  (git-commit-diff \"abc123def456\")
"
                  "
Git tools available this session — all prefixed `git-`:
  (git-search-commits {:category :bug :since \"2025-06-01\" :path \"src/\" ...})
  (git-commit-history {:limit 20})
  (git-commits-by-ticket \"SVAR-42\")
  (git-commit-parents \"HEAD\")
  (git-file-history \"src/foo.clj\")
  (git-blame \"src/foo.clj\" 42 58)
  (git-commit-diff \"HEAD\")
")]
      (str (str/join "\n" blocks) "\n" tools))))

(defn- history-decision-block
  "Builds the \"BEFORE WRITING CODE, DECIDE\" list, including ONLY the tools
   that are actually active in the current env. If no history tools are
   active (e.g. a fresh env with no conversation), returns nil so the
   whole block is elided instead of listing dead tool names."
  [{:keys [has-conversation?]}]
  (when has-conversation?
    (str "\nBEFORE WRITING CODE, DECIDE:
- Do I need older reasoning from this turn?     → (var-history '*reasoning*) — every iteration's thinking.
- Do I need results from an earlier iteration?  → (conversation-results [:id query-id]) or check <var_index>.
- Do I need an earlier QUERY in this convo?     → (conversation-history) then (conversation-code …) or (conversation-results …).
- Do I need an older *answer*?                  → (var-history '*answer*) — every prior turn's final answer.
- Do I need a prior value of a var?             → (var-history 'sym) / (var-diff 'sym v1 v2).
- Do I need to restore a persisted var?         → (restore-var 'sym).
Don't speculate about past state — pull it. Don't re-derive what you can look up.\n")))

(defn- arch-history-tools-line
  "The `(var-history 'x) / (var-diff 'x 1 3)` ARCH bullet references tools
   that only exist when the env has a conversation. Elide when inactive."
  [{:keys [has-conversation?]}]
  (when has-conversation?
    "\n  (var-history 'x) → all versions. (var-diff 'x 1 3) → structural diff (collections only)."))

(defn- concept-navigation-block
  "Concept-graph navigation tools are only bound when the DB holds
   extracted concepts. Without concepts, listing them would point at
   nil bindings and waste tokens."
  [{:keys [has-concepts? concept-graph-prompt]}]
  (when has-concepts?
    (str (when concept-graph-prompt (str "\n" concept-graph-prompt "\n"))
      "
Concept navigation (cross-document ontology):
- (concept-info \"Term\") → definition, aliases, sources (go-to-definition)
- (remove-concept \"Term\" \"rationale why\") → removes concept (rationale REQUIRED)
- (edit-concept \"Term\" {:definition \"new def\"}) → updates concept (survives rebuild)
")))

(defn build-system-prompt
  "Builds the system prompt — compact, token-efficient.

   Every tool reference is ACTIVATION-GATED: if a tool's activation-fn would
   return false for the current env, the corresponding prompt block is
   elided. This keeps `build-system-prompt` in lockstep with the per-turn
   activation check in `runtime.query.core/prepare-query-context`.

   Required opts:
     :has-documents?    — DB has ≥1 document
     :has-conversation? — env has a conversation-ref (→ history/restore/var tools)
     :has-concepts?     — DB has ≥1 concept
     :git-repos         — non-nil seq means ≥1 repo attached

   Keeping these flags in sync with `register-builtin-tools!` is the one
   invariant this module depends on."
  [{:keys [output-spec custom-docs has-reasoning? has-documents? document-summary
           system-prompt git-repos skill-registry concept-graph-prompt
           has-conversation? has-concepts?]
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

CONTEXT MODEL — the prompt is FIXED size; you PULL what you need:
- <journal>         — previous iteration's execution results (ONLY the previous one).
- <var_index>       — every def'd var, including SYSTEM vars (marked `(SYSTEM, …) …` in the doc column).
- SYSTEM vars are bound by the agent loop, usable like any other SCI var,
  and NEVER forgotten (:forget on them is silently refused):
    *query*         current user query (this turn).
    *reasoning*     YOUR thinking from the previous iteration.
    *answer*        final answer from the previous turn in this conversation.
- Everything older is ON-DEMAND via tools below. Nothing else accumulates.
"
    (history-decision-block opts)
    "
ARCH:
- Single-shot iter. State = def'd vars. <var_index> = vars. <journal> = last iteration's results.
- Cross-query memory is ONLY def'd vars. Plain final answers do not persist.
- (doc fn) for tool docs. Aliases: str/ set/ walk/ edn/ json/ zp/ pp/ lt/ test/
- (def x \"docstring\" val) → docstring in <var_index>. Defs for reusable state only.
- VAR REUSE: ALWAYS redef existing vars instead of creating new names for the same concept.
  Check <var_index> first. (def file-list ...) again, NOT (def files ...). Vars show (vN) when updated.
- STORE RESULTS: Always (def answer-name result) to persist computed answers.
  Plain :final text does NOT persist across queries. Only def'd vars survive."
    (arch-history-tools-line opts)
    "
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
- Only tools listed exist. Data in :final MUST come from <journal> or values you explicitly pulled via history/var tools this turn. Never fabricate.
- :answer-type mustache-text|mustache-markdown → Mustache-rendered. All referenced vars MUST be def'd.

SUB-CALLS:
- (sub-rlm-query \"q\") → {:content :code}. Batch: (sub-rlm-query-batch [\"q1\" \"q2\"]).
- Second arg is an opts map; use it to steer cost/quality per sub-call:
  (sub-rlm-query \"q\" {:routing {:optimize :cost}}) — cheapest model for trivial lookups
  (sub-rlm-query \"q\" {:routing {:optimize :speed}}) — fastest model for quick answers
  (sub-rlm-query \"q\" {:routing {:optimize :intelligence}}) — strongest model for hard reasoning
  (sub-rlm-query \"q\" {:reasoning :deep}) — enable deep thinking when the root model supports it
  (sub-rlm-query \"q\" {:max-iter 5}) — multi-iteration sub-query (default 1, single-shot)
- Default (no opts) uses the same model as the main turn. Only steer when the
  sub-task clearly differs in difficulty from the outer one — e.g. a one-liner
  lookup inside a hard reasoning turn is `:cost`; a tricky derivation inside
  an easy turn is `:intelligence :deep`.

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
    (format-git-context git-repos)
    (when (and has-documents? document-summary)
      (str
        "
DOCUMENTS: " document-summary "

Doc tools (2 fns):
1. (search-documents \"q\") → markdown string
   (search-documents \"q\" {:in :pages})  — narrow to :pages|:toc|:entities
   (search-documents \"q\" {:top-k 20 :document-id \"doc-1\"})
2. (fetch-document-content ref) → full content:
   [:node/id \"id\"] → page text
   [:doc/id \"id\"] → vec of ~4K char pages
   [:toc/id \"id\"] → TOC desc
   [:id \"id\"] → {:entity {...} :relationships [...]} 
(def pages (fetch-document-content [:doc/id \"doc-1\"]))
Search in English. Translate non-EN queries first.
"))
    (concept-navigation-block opts)
    (when system-prompt
      (str "\nINSTRUCTIONS:\n" system-prompt "\n"))
    (format-custom-docs custom-docs)
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
