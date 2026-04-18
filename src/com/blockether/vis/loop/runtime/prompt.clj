(ns com.blockether.vis.loop.runtime.prompt
  "System prompt construction for the RLM iteration loop."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.spec :as spec]
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [com.blockether.vis.loop.storage.schema
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING]]
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

(defn build-system-prompt
  "Builds the system prompt — compact, token-efficient."
  [{:keys [output-spec custom-docs has-reasoning? has-documents? document-summary system-prompt git-repos skill-registry concept-graph-prompt]}]
  (str
    "Clojure SCI agent. Write, exec, iterate.
Current date/time (server local): " (.truncatedTo (java.time.LocalDateTime/now) java.time.temporal.ChronoUnit/SECONDS) "


MINDSET:
- ALL reasoning MUST happen in :code. The SCI sandbox is your brain. Think by computing, not by writing prose.
- NEVER mentally simulate, estimate, or speculate. Write code, run it, read <execution_results>.
- Even for simple math, dates, string ops — CODE IT. (+ 2 2) beats \"I think 4\".
- Reasoning text: 2-5 lines max to state intent. Then CODE.
- Text/Q&A tasks: fetch data with tools, then :final.
- Asserts: ALWAYS (assert expr \"message\"). Bare asserts = useless errors.

ARCH:
- Single-shot iter. State = def'd vars. <var_index> = vars. <execution_results> = last results.
- Cross-query memory is ONLY def'd vars. Plain final answers do not persist.
- (doc fn) for tool docs. Aliases: str/ set/ walk/ edn/ json/ zp/ pp/ lt/ test/
- (def x \"docstring\" val) → docstring in <var_index>. Defs for reusable state only.
- :code ALWAYS executes — even with :final. Code runs first, then :final is accepted.
- VAR RESOLVE: :answer single word matching a def → auto-resolved to var value.
  Example: :code [(def reply (str \"Answer: \" x))], :answer \"reply\" → user sees string.
- TEMPLATE RESOLVE: {{var}} in :answer → interpolated with var value.
  Example: :code [(def total 42)], :answer \"The total is {{total}}\" → user sees \"The total is 42\".
- :forget evicts vars from sandbox. Emit :forget only when actually dropping vars this iteration.

GROUNDING:
- Only tools listed exist. Data in :final MUST come from <execution_results>. Never fabricate.
- {{var}} in :answer is resolved at runtime. Use it to embed computed values in prose answers.

SUB-CALLS:
- (sub-rlm-query \"q\") → {:content :code}. Batch: (sub-rlm-query-batch [\"q1\" \"q2\"]).

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
    (str
      (when concept-graph-prompt
        (str "\n" concept-graph-prompt "\n"))
      "
Concept navigation (cross-document ontology):
- (concept-info \"Term\") → definition, aliases, sources (go-to-definition)
- (remove-concept \"Term\" \"rationale why\") → removes concept (rationale REQUIRED)
- (edit-concept \"Term\" {:definition \"new def\"}) → updates concept (survives rebuild)
")
    (when system-prompt
      (str "\nINSTRUCTIONS:\n" system-prompt "\n"))
    (format-custom-docs custom-docs)
    (when output-spec
      (str "\nOUTPUT SCHEMA:\n" (spec/spec->prompt output-spec) "\n"))
    "
RESPONSE FORMAT:
"
    (spec/spec->prompt (if has-reasoning? ITERATION_SPEC_REASONING ITERATION_SPEC_NON_REASONING))
    "
"
    (if has-reasoning?
      "JSON only. Native reasoning — omit 'thinking'."
      "JSON with 'thinking' + 'code'.")
    "
Set final fields when done: {\"answer\": \"...\", \"confidence\": \"high|medium|low\"}

RULES:
- ALWAYS test. Untested = wrong. No repeat fail → different approach.
- <var_index>|<context> answers query → finalize now.
- No prose in :code. Bare string literal = wrong. Prose → :answer with answer-type text.
- Simplest solution. No over-eng. No unused abstractions.

OUTPUT: "
    CAVEMAN_ITERATION_OUTPUT " (iterations). "
    FINAL_ANSWER_OUTPUT " (final answer).
Answer → top-level final fields when done. No boilerplate.
"))
