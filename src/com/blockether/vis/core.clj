(ns com.blockether.vis.core
  "Public API facade for the Vis RLM (Recursive Language Model).
   Delegates to loop.core, loop.knowledge.pageindex, loop.runtime.query, etc.

   Sections:
   - Unified entrypoint
   - Schema constants
   - Tool definition API
   - Environment lifecycle
   - Tool & hook registration
   - Document ingestion
   - Git ingestion
   - Query execution
   - QA pipeline
   - Concept graph / ontology
   - PageIndex"
  (:require
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [com.blockether.vis.loop.knowledge.git :as rlm-git]
   [com.blockether.vis.loop.knowledge.ontology :as ontology]
   [com.blockether.vis.loop.knowledge.pageindex :as rlm-pageindex]
   [com.blockether.vis.loop.runtime.query.core :as rlm-query]
   [com.blockether.vis.loop.tool :as tool]
   [com.blockether.vis.loop.storage.schema :as schema]
   [com.blockether.vis.loop.storage.trajectory :as trajectory]
   [com.blockether.vis.loop.knowledge.qa :as rlm-qa]
   [taoensso.trove :as trove]))

;; =============================================================================
;; Unified entrypoint
;; =============================================================================

(defn -main
  "Unified entrypoint for all Vis adapters.

   Dispatches to the appropriate adapter based on the first argument:

     vis chat              TUI chat interface (default when no args)
     vis run <prompt>      One-shot CLI query
     vis web [port]        Start web server (default port 3000)
     vis telegram          Start Telegram bot
     vis index <file>      Build a PageIndex from a PDF/text file
     vis qa <index-path>   Generate Q&A pairs from an indexed corpus
     vis help              Show usage

   Examples:
     (com.blockether.vis.core/-main)                    ;; → TUI
     (com.blockether.vis.core/-main \"run\" \"What is 2+2?\")  ;; → CLI
     (com.blockether.vis.core/-main \"web\" \"8080\")        ;; → Web on port 8080
     (com.blockether.vis.core/-main \"telegram\")           ;; → Telegram bot"
  [& args]
  ;; Require at call time to avoid loading all adapters eagerly
  (require 'com.blockether.vis.adapters.cli)
  (apply (resolve 'com.blockether.vis.adapters.cli/-main) args))

;; =============================================================================
;; Schema constants
;; =============================================================================

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def MAX_ITERATIONS
  "Hard ceiling on iteration count per query (default 50)."
  schema/MAX_ITERATIONS)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def DEFAULT_RECURSION_DEPTH
  "Default max nesting depth for sub-rlm-query calls."
  schema/DEFAULT_RECURSION_DEPTH)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:dynamic *max-recursion-depth*
  "Dynamic var controlling max sub-rlm-query recursion depth."
  schema/*max-recursion-depth*)

;; Use schema/*rlm-ctx* directly — do NOT redefine as a separate dynamic var,
;; as binding a local alias won't propagate to core.clj which imports schema/*rlm-ctx*.

;; --- QA spec constants ---

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def GENERATION_PERSONAS
  "Persona map for QA question generation. Keys: :curious, :expert, :contrarian, etc."
  schema/GENERATION_PERSONAS)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def DEDUP_SPEC
  "Spec for QA deduplication LLM call."
  schema/DEDUP_SPEC)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def REVISION_SPEC
  "Spec for QA question revision LLM call."
  schema/REVISION_SPEC)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def CHUNK_SELECTION_SPEC
  "Spec for QA chunk selection LLM call."
  schema/CHUNK_SELECTION_SPEC)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def QUESTIONIFY_SPEC
  "Spec for QA questionify LLM call."
  schema/QUESTIONIFY_SPEC)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def VERIFICATION_SPEC
  "Spec for QA verification LLM call."
  schema/VERIFICATION_SPEC)

;; =============================================================================
;; Tool definition API
;; =============================================================================

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def make-tool-def
  "Build and validate a canonical function tool-def.

   `(make-tool-def sym f tool-def-map)` → validated tool-def with all
   required keys: :sym, :fn, :type, :doc, :arglists, :validate-input,
   :validate-output, :examples. Missing keys are inferred from fn metadata
   or filled with defaults. Throws on invalid shape.

   Use this when registering custom tools via `register-env-fn!`."
  tool/make-tool-def)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def complete-fn-tool-def
  "Return a canonical function tool-def with all required keys populated
   (without validation). Infers :doc, :arglists from fn metadata when missing.

   `(complete-fn-tool-def sym f tool-def-map)` → completed tool-def map.

   Prefer `make-tool-def` which also validates."
  tool/complete-fn-tool-def)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def assert-fn-tool-def!
  "Validate a canonical function tool-def shape. Throws ex-info on invalid input.

   Checks: :sym is symbol, :fn is function, :type is :fn, :doc is non-blank
   string, :arglists is non-empty vector, :validate-input/:validate-output are
   fns, :examples is non-empty vector of non-blank strings."
  tool/assert-fn-tool-def!)

;; =============================================================================
;; Environment lifecycle
;; =============================================================================

(defn create-env
  "Creates an RLM environment for document ingestion and querying.

   `router` — LLM router from `config/make-router`.
   `opts`   — Map with :db (path or :temp), :conversation (nil | :latest | uuid).

   Returns an env map with :db-info, :sci-ctx, :router, :tool-registry-atom, etc."
  [router opts]
  (loop-core/create-env router opts))

(defn dispose-env!
  "Disposes an RLM environment and releases resources.
   The shared SQLite DataSource stays open for sibling envs."
  [env]
  (loop-core/dispose-env! env))

;; =============================================================================
;; Tool & hook registration
;; =============================================================================

(defn register-env-fn!
  "Registers a function tool in the SCI sandbox.

   `env`      — RLM environment from `create-env`.
   `sym`      — Symbol name for the tool (e.g. 'my-tool).
   `f`        — The function implementation.
   `tool-def` — Map with at minimum {:doc \"...\"}. See `make-tool-def`."
  [env sym f tool-def]
  (loop-core/register-env-fn! env sym f tool-def))

(defn register-env-def!
  "Registers a constant/value in the SCI sandbox.

   `env`      — RLM environment from `create-env`.
   `sym`      — Symbol name (e.g. 'MY_CONST).
   `value`    — The value to bind.
   `tool-def` — Map with {:doc \"...\"}."
  [env sym value tool-def]
  (loop-core/register-env-def! env sym value tool-def))

(defn register-hook!
  "Attach a hook to an existing tool's execution chain.

   `env`  — RLM environment.
   `sym`  — Tool symbol to hook into.
   `opts` — Hook spec map, see loop.core/register-hook! for details."
  [env sym opts]
  (loop-core/register-hook! env sym opts))

(defn unregister-hook!
  "Remove a per-tool hook entry by :id.

   `env`   — RLM environment.
   `sym`   — Tool symbol.
   `stage` — Hook stage keyword.
   `id`    — Hook id to remove."
  [env sym stage id]
  (loop-core/unregister-hook! env sym stage id))

(defn list-tool-hooks
  "Return hook chains for tool `sym`. Returns map of stage → hook entries."
  [env sym]
  (loop-core/list-tool-hooks env sym))

(defn list-registered-tools
  "Return vec of registered tool-def maps (without :fn) for the environment."
  [env]
  (loop-core/list-registered-tools env))

;; =============================================================================
;; Document ingestion
;; =============================================================================

(defn ingest-to-env!
  "Ingests PageIndex documents into an RLM environment.

   Stores the complete document structure exactly as PageIndex produces it:
   document metadata, pages, page nodes, and TOC entries. Can be called
   multiple times to add more documents.

   `env`       — RLM environment from `create-env`.
   `documents` — Vector of PageIndex documents (spec-validated).
   `opts`      — Optional map (reserved for future options).

   Returns vector of ingestion results per document:
   [{:document-id \"...\" :pages-stored N :nodes-stored N :toc-entries-stored N}]"
  ([env documents] (ingest-to-env! env documents {}))
  ([env documents _opts]
   (when-not (:db-info env)
     (anomaly/incorrect! "Invalid RLM environment" {:type :rlm/invalid-env}))
   (when-not (schema/valid-documents? documents)
     (anomaly/incorrect! "Invalid documents - must be vector of PageIndex documents"
       {:type :rlm/invalid-documents
        :explanation (schema/explain-documents documents)}))
   (let [db-info (loop-core/db-info env)
         results (mapv #(rlm-db/db-store-pageindex-document! db-info %) documents)]
     (when (seq documents)
       (let [revision (rlm-db/bump-corpus-revision! db-info)]
         (rlm-qa/invalidate-qa-corpus-snapshot-cache! env)
         (when (:qa-corpus-atom env)
           (swap! (:qa-corpus-atom env) update :stats assoc :last-revision revision))))
     results)))

;; =============================================================================
;; Git ingestion
;; =============================================================================

(defn ingest-git!
  "Ingest git commits from a repository into an RLM environment.

   Opens the repo with JGit (pure JVM, no shell-out), reads commits, stores
   them as entities, and persists a :repo entity so SCI git tools can
   re-open it lazily per call.

   SCI sandbox tools (bound automatically when a DB is present):
     (git-search-commits opts)   (git-commit-history opts)
     (git-commits-by-ticket ref) (git-commit-parents sha)
     (git-file-history path)     (git-blame path from to)
     (git-commit-diff sha)

   `env`  — RLM environment from `create-env`.
   `opts` — Map with:
     :repo-path — String, required. Path to the repo.
     :repo-name — String, optional. Default = basename of :repo-path.
     :n         — Integer, optional. Max commits to ingest (default 100).
     :since     — String, optional. ISO-8601 date cutoff.
     :since-sha — String, optional. Only commits newer than this SHA.
     :path      — String, optional. Restrict to commits touching this path.
     :author    — String, optional. Restrict to this author email.

   Returns map:
   {:events-stored N :people-stored N :files-stored N
    :repo-path str :repo-name str :head {:sha :short :branch} :commits-ingested N}"
  [env {:keys [repo-path repo-name n since since-sha path author]
        :or {n 100}
        :as _opts}]
  (when-not (:db-info env)
    (anomaly/incorrect! "Invalid RLM environment (no DB)"
      {:type :rlm/invalid-env}))
  (when-not repo-path
    (anomaly/incorrect! ":repo-path is required"
      {:type :rlm/missing-repo-path}))
  (let [resolved-name (or repo-name
                        (.getName (java.io.File. (str repo-path))))
        resolved-path (.getAbsolutePath (java.io.File. (str repo-path)))
        repo (rlm-git/open-repo resolved-path)]
    (when-not repo
      (anomaly/not-found! (str "Not a git repository: " resolved-path)
        {:type :rlm/not-a-git-repo :repo-path resolved-path}))
    (try
      (let [db-info (loop-core/db-info env)
            commits (rlm-git/read-commits repo
                      (cond-> {:n n}
                        since     (assoc :since since)
                        since-sha (assoc :since-sha since-sha)
                        path      (assoc :path path)
                        author    (assoc :author author)))
            ingest-result (rlm-git/ingest-commits! db-info commits {:repo-name resolved-name})
            head (rlm-git/head-info repo)
            commits-ingested (:events-stored ingest-result)]
        (rlm-db/db-store-repo! db-info
          {:name resolved-name
           :path resolved-path
           :head-sha (:sha head)
           :head-short (:short head)
           :branch (:branch head)
           :commits-ingested commits-ingested})
        (trove/log! {:level :info :id ::ingest-git
                     :data {:repo-path resolved-path
                            :repo-name resolved-name
                            :commits-ingested commits-ingested
                            :head head}
                     :msg "git commits ingested"})
        (merge ingest-result
          {:repo-path resolved-path
           :repo-name resolved-name
           :commits-ingested commits-ingested
           :head head}))
      (finally (.close repo)))))

;; =============================================================================
;; Query execution
;; =============================================================================

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def query-env!
  "Runs a query against an RLM environment using iterative LLM code execution.

   `env`      — RLM environment from `create-env`.
   `messages` — Vector of message maps, e.g. [(llm/user \"...\")].
   `opts`     — Optional map. See `loop.runtime.query.core/query-env!` for all opts.

   Returns map with :answer, :raw-answer, :trace, :iterations, :duration-ms,
   :tokens, :cost, :eval-scores, :refinement-count, :confidence, :sources,
   :reasoning. On failure also :status and :status-id."
  rlm-query/query-env!)

(defn loop-list-conversation-queries
  "Lists query records from an RLM environment's conversation."
  [env & [opts]]
  (trajectory/list-queries (loop-core/db-info env) opts))

(defn loop-export-training-trajectories!
  "Exports filtered trajectories as JSONL for fine-tuning.

   `env`        — RLM environment.
   `output-dir` — Directory path for JSONL output.
   `opts`       — Optional filter map."
  [env output-dir & [opts]]
  (trajectory/export-trajectories! (loop-core/db-info env) output-dir opts))

;; =============================================================================
;; QA pipeline
;; =============================================================================

;; --- Private delegates (tests access via #'sut/...) ---

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private qa-corpus-snapshot
  "Snapshot the QA corpus for a given env. Returns cached digest + stats."
  rlm-qa/qa-corpus-snapshot)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private write-qa-manifest!
  "Write QA manifest EDN file."
  rlm-qa/write-qa-manifest!)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private compute-distribution
  "Compute target distribution across categories/difficulties."
  rlm-qa/compute-distribution)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private deduplicate-questions
  "Deduplicate QA question list via LLM."
  rlm-qa/deduplicate-questions)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private filter-verified-questions
  "Filter QA questions that passed verification."
  rlm-qa/filter-verified-questions)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private build-generation-prompt
  "Build the LLM prompt for QA question generation."
  rlm-qa/build-generation-prompt)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:private build-verification-prompt
  "Build the LLM prompt for QA answer verification."
  rlm-qa/build-verification-prompt)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def qa-corpus-snapshot-stats
  "Returns QA corpus snapshot cache stats for observability.
   Shape: {:hits N :misses N :last-digest-ms ms|nil :last-revision rev}."
  rlm-qa/qa-corpus-snapshot-stats)

;; --- Public QA API ---

(defn qa-generate!
  "Generates question-answer pairs from ingested documents.

   `env`  — RLM environment with documents ingested.
   `opts` — Optional map:
     :count  — Target number of Q&A pairs (default 10).
     :model  — Override generation model.

   Returns map with :questions, :stats, :distribution, etc.
   See `loop.knowledge.qa/query-env-qa!` for full documentation."
  ([env] (qa-generate! env {}))
  ([env opts]
   (let [renamed-opts (if (contains? opts :count)
                        (-> opts
                          (assoc :target-count (:count opts))
                          (dissoc :count))
                        opts)]
     (rlm-qa/query-env-qa! env query-env! renamed-opts))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def qa-save-results!
  "Save QA results to EDN and Markdown files.
   `(qa-save-results! result output-path)` → {:files [paths...]}."
  rlm-qa/save-qa!)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def qa-build-toc-selection-prompt
  "Build a TOC-based chunk selection prompt for QA generation.
   Used in tests and advanced QA workflows."
  rlm-qa/build-toc-based-selection-prompt)

;; =============================================================================
;; Concept graph / ontology
;; =============================================================================

;; --- Phase 1: Per-page concept extraction (call during/after ingestion) ---

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def extract-page-concepts!
  "Extract concepts from a single page's content at index time.
   Grounded to actual page/node IDs with content SHA for change detection.

   `(extract-page-concepts! router db-info doc-id page-id node-id content)`

   Persists to page_concept table. Returns count of concepts extracted."
  ontology/extract-page-concepts!)

;; --- Phase 2: Cross-document bridge building ---

(defn build-concept-graph!
  "Build a cross-document concept graph from grounded page concepts.

   Reads page_concept rows (extracted at index time), cross-links across
   documents, persists to concept/concept_edge tables.
   Preserves user_edited concepts — rebuild won't overwrite them.

   `env`  — RLM environment with documents ingested.
   `opts` — Optional map (reserved for future options).

   Returns:
   {:concepts-stored N :edges-stored N :ambiguities N :user-preserved N}"
  ([env] (build-concept-graph! env {}))
  ([env opts] (ontology/build-concept-graph! env opts)))

;; --- User refinement ---

(defn set-concept-status!
  "Mark a concept as 'active', 'removed', or 'user_edited'.
   Removed concepts disappear from prompt and tools.
   User-edited concepts survive rebuild.
   Removal REQUIRES a rationale — why is this concept being removed?

   `env`        — RLM environment.
   `concept-id` — Concept ID string.
   `status`     — 'active', 'removed', or 'user_edited'.
   `rationale`  — Required when status is 'removed'. Why this concept doesn't belong."
  ([env concept-id status]
   (rlm-db/set-concept-status! (:db-info env) concept-id status))
  ([env concept-id status rationale]
   (rlm-db/set-concept-status! (:db-info env) concept-id status rationale)))

(defn update-concept!
  "Update a concept's definition/group and mark as user_edited.
   Survives rebuild — won't be overwritten by build-concept-graph!."
  [env concept-id updates]
  (rlm-db/update-concept! (:db-info env) concept-id updates))

;; --- Read-only access ---

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def concept-graph-for-prompt
  "Format the concept graph as a compact system prompt section.
   Returns a string for injection into the system prompt, or nil if empty."
  ontology/concept-graph-for-prompt)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def concept-graph->markdown
  "Render a concept graph as DDD ubiquitous language markdown (read-only).
   Does NOT write to disk — returns the markdown string."
  ontology/concept-graph->markdown)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def load-full-concept-graph
  "Load the complete concept graph from DB.
   Returns {:concepts [...] :edges [...]}, each concept with :aliases and :sources."
  rlm-db/load-full-concept-graph)

;; =============================================================================
;; PageIndex
;; =============================================================================

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-normalize-page-spec
  "Normalize a page spec (vector of ints, {:from N :to M}, or nil) into a predicate."
  rlm-pageindex/normalize-page-spec)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-filter-pages
  "Filter pages by a normalized page spec predicate."
  rlm-pageindex/filter-pages)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-group-continuations
  "Group continuation pages into ranges for PageIndex processing."
  rlm-pageindex/group-continuations)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-write-document!
  "Write a PageIndex document to an EDN file at `path`."
  rlm-pageindex/write-document-edn!)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-read-document
  "Read a PageIndex document from an EDN file at `path`."
  rlm-pageindex/read-document-edn)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-load
  "Load a PageIndex from a .pageindex directory. Returns the document map."
  rlm-pageindex/load-index)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-inspect
  "Inspect a PageIndex directory. Returns summary stats."
  rlm-pageindex/inspect)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-build
  "Build a PageIndex from a source file (PDF, etc.) without writing to disk.
   Returns the document map."
  rlm-pageindex/build-index)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pageindex-build-and-write!
  "Build and write a PageIndex to disk.
   `(pageindex-build-and-write! source-path output-dir opts)` → document map."
  rlm-pageindex/index!)
