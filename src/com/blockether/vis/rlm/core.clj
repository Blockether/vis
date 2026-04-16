(ns com.blockether.vis.rlm.core
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.vis.rlm.db :as rlm-db
    :refer [create-rlm-conn dispose-rlm-conn!
            db-list-documents
            db-store-pageindex-document! str-truncate]]
   [com.blockether.vis.rlm.data :as rlm-data]
   [com.blockether.vis.rlm.routing
    :refer [make-routed-sub-rlm-query-fn resolve-root-model provider-has-reasoning?]]
   [com.blockether.vis.rlm.schema :as schema
    :refer [ENTITY_EXTRACTION_SPEC ENTITY_EXTRACTION_OBJECTIVE
            ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING
            *eval-timeout-ms*
            validate-final bytes->base64 *rlm-ctx*]]
   [com.blockether.vis.rlm.skills :as rlm-skills]
   [com.blockether.vis.rlm.tools :refer [create-sci-context realize-value build-var-index]]
   [com.blockether.vis.rlm.paren-repair :as paren-repair]
   [edamame.core :as edamame]
   [com.blockether.svar.internal.spec :as spec]
   [sci.core :as sci]
   [taoensso.trove :as trove]))

(declare build-system-prompt run-iteration format-executions)

(def ^:private CAVEMAN_ITERATION_OUTPUT
  "Caveman mode for iterations. Drop: articles, filler, hedging, conjunctions.
Fragments OK. → for causality. One word when enough. Tech terms exact. Code unchanged.
Pattern: [thing] [action] [reason]. [next step].")

(def ^:private FINAL_ANSWER_OUTPUT
  "Normal English. Clear, direct sentences. No AI filler (no \"As an AI\", \"I believe\",
\"In conclusion\"). No hedging. Factual. Technical terms exact.")

(defn rlm-debug!
  "Logs at :info level only when :rlm-debug? is true in *rlm-ctx*.
   Includes :rlm-phase from context automatically in data."
  [data msg]
  (when (:rlm-debug? *rlm-ctx*)
    (trove/log! {:level :info :data (assoc data :rlm-phase (:rlm-phase *rlm-ctx*)) :msg msg})))

(defn rlm-stage!
  "Always-on structured stage log for RLM pipeline visibility.
   Produces a single compact line per event with visual hierarchy."
  [stage iteration data]
  (let [fmt (fn [& parts] (str/join "  " (remove nil? parts)))
        msg (case stage
              :query-start
              (fmt "── RLM START"
                (str "model=" (:model data))
                (str "max-iter=" (:max-iterations data))
                (when (:reasoning? data) "reasoning=true")
                (str "query=\"" (str/replace (str (:query data)) #"\s+" " ") "\""))

              :iter-start
              (fmt (str "┌─ ITER " iteration)
                (str "msgs=" (:msg-count data))
                (when-let [r (:reasoning data)] (str "reasoning=" r)))

              :llm-response
              (let [thinking (str (:thinking data))]
                (fmt "│  ⇐ LLM"
                  (str "code=" (:code-count data))
                  (when (:has-final data) "FINAL=true")
                  (when (seq thinking)
                    (str "\n│     reasoning: " (str/replace thinking #"\s+" " ")))))

              :code-exec
              (let [code-1line (str/replace (str (:code data)) #"\s+" " ")]
                (fmt (str "│  ▶ EXEC [" (:idx data) "/" (:total data) "]")
                  code-1line
                  (str "budget=" (:time-ms data) "ms")))

              :code-result
              (fmt (str "│  ◀ EXEC [" (:idx data) "/" (:total data) "]")
                (str (:execution-time-ms data) "ms")
                (when (:error data) (str "ERROR: " (str-truncate (str/replace (str (:error data)) #"\s+" " ") 80)))
                (when (:timeout? data) "TIMEOUT")
                (when (and (not (:error data)) (not (:timeout? data)))
                  (let [r (:result data)]
                    (cond
                      (nil? r) "result=✓"
                      (fn? r) "result=fn"
                      :else (str "result=" (str-truncate (str/replace (pr-str r) #"\s+" " ") 80))))))

              :iter-end
              (fmt (str "└─ ITER " iteration)
                (str "blocks=" (:blocks data))
                (str "errors=" (:errors data))
                (str "total=" (reduce + 0 (map #(or % 0) (or (:times data) []))) "ms"))

              :final
              (fmt "│  ══ FINAL"
                (str "answer=\"" (str/replace (str (:answer data)) #"\s+" " ") "\"")
                (str "confidence=" (:confidence data))
                (str "iters=" (:iterations data)))

              :query-end
              (fmt "── RLM END"
                (str (:duration-ms data) "ms")
                (str "iters=" (:iterations data))
                (when (:cost data) (str "cost=$" (:cost data))))

              :error
              (fmt "│  ✘ ERR"
                (str "reason=" (:reason data))
                (when (:max data) (str "max=" (:max data))))

              :empty
              (fmt "│  ⚠ EMPTY" "no code blocks")

              ;; fallback
              (fmt (str "│  " (name stage)) (pr-str data)))
        env-id (:rlm-env-id *rlm-ctx*)
        prefix (when env-id (str "[q=" env-id " i=" iteration "] "))]
    (trove/log! {:level :info :id ::rlm-stage :msg (str prefix msg)})))

(defn- status->id
  [status]
  (when status
    (keyword "rlm.status" (name status))))

(def ^:private reasoning-low "low")
(def ^:private reasoning-medium "medium")
(def ^:private reasoning-high "high")

(def ^:private reasoning-effort-values
  #{reasoning-low reasoning-medium reasoning-high})

(defn- normalize-reasoning-effort
  "Returns normalized reasoning effort string or nil.
   Accepts string/keyword input."
  [v]
  (let [s (cond
            (keyword? v) (name v)
            (string? v) (str/lower-case (str/trim v))
            :else nil)]
    (when (contains? reasoning-effort-values s)
      s)))

(defn- reasoning-effort-for-errors
  "Maps consecutive error count to effective reasoning effort.
   `base-effort` is the no-error level for iteration start/recovery."
  [base-effort consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base-effort
    (= 1 (long consecutive-errors)) (if (= base-effort reasoning-low) reasoning-medium reasoning-high)
    :else reasoning-high))

;; =============================================================================
;; RLM Environment
;; =============================================================================

(defn create-rlm-env
  "Creates an RLM execution environment (internal use only).

   Params:
   `depth-atom` - Atom tracking recursion depth.
   `router` - Router from llm/make-router. Required.
   `opts` - Map, optional:
     - :db - DB spec: nil (no DB), :temp, path string, {:conn c}, {:path p}
     - :documents - Vector of PageIndex documents to preload (stored exactly as-is).

   Returns:
   Map with :sci-ctx, :sub-rlm-query-fn, :db-info, :router."
  ([depth-atom router]
   (create-rlm-env depth-atom router {}))
  ([depth-atom router {:keys [db documents]}]
   (when-not router
     (throw (ex-info "Router is required for RLM environment" {:type :rlm/missing-router})))
   (let [db-info (create-rlm-conn db)
         _ (when (and db-info (seq documents))
             (doseq [doc documents]
               (db-store-pageindex-document! db-info doc)))
         sub-rlm-query-fn (make-routed-sub-rlm-query-fn {} depth-atom router nil nil)
         {:keys [sci-ctx sandbox-ns initial-ns-keys]} (create-sci-context sub-rlm-query-fn db-info nil nil)]
     {:sci-ctx sci-ctx :sandbox-ns sandbox-ns :initial-ns-keys initial-ns-keys
      :sub-rlm-query-fn sub-rlm-query-fn
      :db-info db-info
      :router router})))

(defn dispose-rlm-env! [{:keys [db-info]}]
  (when db-info (dispose-rlm-conn! db-info)))

(defn get-locals
  "Returns {sym → val} of user-defined vars in SCI sandbox (excludes built-ins).
   Direct atom read from SCI env — zero eval overhead."
  [{:keys [sci-ctx initial-ns-keys]}]
  (try
    (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
      (persistent!
        (reduce-kv (fn [acc k v]
                     (if (or (contains? initial-ns-keys k) (keyword? k))
                       acc
                       (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
          (transient {}) sandbox)))
    (catch Exception e
      (trove/log! {:level :warn :id ::get-locals-fallback
                   :data {:error (ex-message e)}
                   :msg "Failed to read sandbox locals, returning empty map"})
      {})))

;; =============================================================================
;; Code Execution
;; =============================================================================

(defn- run-sci-code
  "Evaluate `code` in `sci-ctx` with captured stdout/stderr.
   Uses eval-string+ with :ns to ensure code runs in sandbox namespace.
   Returns {:result :stdout :stderr :error} with writers already closed."
  [sci-ctx code & {:keys [sandbox-ns]}]
  (let [stdout-writer (java.io.StringWriter.)
        stderr-writer (java.io.StringWriter.)
        err-pw       (java.io.PrintWriter. stderr-writer true)
        exec-future (future
                      (try
                        (let [result (sci/binding [sci/out stdout-writer
                                                   sci/err err-pw]
                                       (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                         (:val (sci/eval-string+ sci-ctx code
                                                 (when ns {:ns ns})))))]
                          {:result result :stdout (str stdout-writer) :stderr (str stderr-writer) :error nil})
                        (catch Throwable e
                          {:result nil :stdout (str stdout-writer) :stderr (str stderr-writer)
                           :error (str (.getSimpleName (class e)) ": " (or (ex-message e) (str e)))})))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             {:result nil :stdout "" :stderr "" :error (str (.getSimpleName (class e)) ": " (ex-message e))}))]
    (.close stdout-writer)
    (.close stderr-writer)
    (if (nil? execution-result)
      (do (future-cancel exec-future)
        {:result nil :stdout "" :stderr "" :error (str "Timeout (" (/ timeout-ms 1000) "s)") :timeout? true})
      execution-result)))

(defn- detect-common-mistakes
  "Pre-exec lint: catches common Clojure mistakes BEFORE SCI eval.
   Returns nil if clean, or an error string with actionable fix."
  [code]
  (let [s (str/trim code)]
    (cond
      ;; Nested #() - illegal in Clojure, cryptic SCI error
      (re-find #"#\([^)]*#\(" s)
      "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"
      ;; Nothing wrong
      :else nil)))

(def ^:private edamame-opts
  "Edamame parser options matching Clojure/SCI syntax.
   :all enables fn literals, deref, var, regex, quote, etc."
  {:all true})

(defn- check-syntax
  "Parses code with edamame. Returns parsed forms or throws."
  [code]
  (edamame/parse-string-all code edamame-opts))

(defn- check-bare-list
  "Detects unquoted list literals like (6 7 8) that parse fine but fail at eval.
   Returns error string or nil."
  [forms]
  (let [first-form (first forms)]
    (when (and (= 1 (count forms))
            (list? first-form) (seq first-form)
            (let [head (first first-form)]
              (not (or (symbol? head) (keyword? head)
                     (list? head) (set? head) (map? head) (vector? head)))))
      (str "Bare list literal: " (pr-str first-form)
        ". Quote it: '(" (str/join " " first-form) ")"))))

(defn- parse-clojure-syntax
  "Validates Clojure syntax using edamame (same parser as SCI).
   Runs all checks: syntax parse, bare list detection.
   Returns nil if valid, or an error string if broken."
  [code]
  (try
    (let [forms (check-syntax code)]
      (or (check-bare-list forms)
        nil))
    (catch Throwable e
      (ex-message e))))

(defn execute-code [{:keys [sci-ctx sandbox-ns]} code & {:keys [timeout-ms]}]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :execute-code})]
    (let [bal (paren-repair/paren-balance code)
          start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        ;; Pre-exec lint caught a known mistake - return clear error without eval
        (do (rlm-debug! {:lint-error lint-error} "Pre-exec lint caught mistake")
          {:result nil :stdout "" :stderr "" :error lint-error
           :execution-time-ms 0 :timeout? false})
        (if-let [parse-error (parse-clojure-syntax code)]
          ;; Pre-parse failed — attempt paren repair before giving up
          (let [repaired (try (paren-repair/repair-code code) (catch Throwable _ code))]
            (if (and (not= repaired code) (nil? (parse-clojure-syntax repaired)))
              (do (rlm-debug! {:parse-error parse-error :repaired-len (count repaired)} "Edamame pre-parse failed, paren repair recovered")
                (let [result (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)
                      t (- (System/currentTimeMillis) start-time)]
                  (assoc result :execution-time-ms t :timeout? false :repaired? true)))
              (do (rlm-debug! {:parse-error parse-error} "Edamame pre-parse failed, repair did not help")
                {:result nil :stdout "" :stderr "" :error parse-error
                 :execution-time-ms 0 :timeout? false})))
        ;; Normal execution path
          (let [execution-result (if timeout-ms
                                   (binding [*eval-timeout-ms* (schema/clamp-eval-timeout-ms timeout-ms)]
                                     (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                                   (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                execution-time (- (System/currentTimeMillis) start-time)]
            (if (:timeout? execution-result)
              (do
                (assoc execution-result :execution-time-ms execution-time :timeout? true))
              (let [{:keys [error]} execution-result
                    final-result (if (and error (paren-repair/parse-error? error))
                                   (try
                                     (let [repaired (paren-repair/repair-code code)]
                                       (if (= repaired code)
                                         (do (trove/log! {:level :debug :id ::repair-noop
                                                          :data {:code-len (count code) :error error}
                                                          :msg "Paren repair: no change needed"})
                                           execution-result)
                                         (let [retry (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)]
                                           (if (:error retry)
                                             (do (trove/log! {:level :warn :id ::repair-retry-failed
                                                              :data {:original-error error
                                                                     :retry-error (:error retry)
                                                                     :code-len (count code)
                                                                     :repaired-len (count repaired)
                                                                     :added-chars (- (count repaired) (count code))
                                                                     :repaired-tail (subs repaired (max 0 (- (count repaired) 80)))}
                                                              :msg "Paren repair changed code but retry still failed"})
                                               execution-result)
                                             (do
                                               (trove/log! {:level :info :id ::repair-applied
                                                            :data {:original code :repaired repaired :sci-error error}
                                                            :msg "Paren repair applied successfully"})
                                               (assoc retry :repaired? true))))))
                                     (catch Throwable _
                                       execution-result))
                                   execution-result)
                    {:keys [result stdout stderr error]} final-result]
                (assoc final-result :execution-time-ms execution-time :timeout? false)))))))))

(defn answer-str
  "Extracts a string representation from an RLM answer.
   Answer is {:result value :type type} — returns the :result as a string."
  [answer]
  (let [v (:result answer answer)]
    (if (string? v) v (pr-str v))))

(defn- cleanup-claim-without-forget?
  "True when the final answer claims vars/index cleanup, but the iteration did
   not actually emit :forget. Prevents user-facing mutation claims that were
   never performed by the runtime."
  [final-answer forget-names]
  (let [s (str/lower-case (str final-answer))]
    (and (empty? forget-names)
      (re-find #"(posprz|sprzat|cleaned up|cleanup|wyrzucon|usun|wywal|removed|forgot|forgotten)" s)
      (re-find #"(var|vars|zmienn|var_index|<var_index>|index|indeks)" s))))

(def ^:private BARE_STRING_RE
  "Matches a code block that is just a quoted string literal (double-quoted).
   Allows optional whitespace around it."
  #"^\s*\"[^\"]*\"\s*$")

(defn bare-string-code-block?
  "True when a code expression is a bare string literal — prose that the LLM
   should have put in :answer instead of :code."
  [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(def ^:private PLACEHOLDER_WORDS
  "Single-word placeholders the LLM sometimes emits as :answer instead of
   the real content. Checked AFTER var-resolve — if the word matches a def, it
   is resolved and this check never fires."
  #{"listing" "result" "results" "done" "ok" "output" "ready" "complete"
    "finished" "listed" "wynik" "gotowe" "zrobione" "wypisane"})

(defn placeholder-final-answer?
  "True when :answer is a known placeholder word that was NOT resolved
   to a var (var resolution happens upstream). Returns nil when answer is fine."
  [raw-answer resolved?]
  (and (not resolved?)
    (contains? PLACEHOLDER_WORDS (str/lower-case (str/trim (str raw-answer))))))

;; =============================================================================
;; System Prompt
;; =============================================================================

(defn- format-param
  "Formats a single parameter for the system prompt."
  [{:keys [name type required description default]}]
  (str "      " name " - " (clojure.core/name (or type :any))
    (when-not (true? required) " (optional)")
    (when (some? default) (str ", default: " (pr-str default)))
    (when description (str " — " description))))

(defn- format-tool-doc
  "Formats a tool doc entry for the system prompt."
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

(defn- format-custom-docs
  "Formats custom docs for the system prompt."
  [custom-docs]
  (when (seq custom-docs)
    (str "\n<custom_tools>\n"
      (str/join "\n" (map format-tool-doc custom-docs))
      "\n</custom_tools>\n")))

(defn build-document-summary
  "Builds a compact summary of available documents for the system prompt.
   Returns nil if no documents are loaded.

   Params:
   `db-info` - Map with :conn key.

   Returns string like:
   \"3 documents, 142 pages, 87 entities (person: 23, org: 15, ...)
    Documents:
      [doc-1] Annual Report 2024 (pdf, 48 pages)
      [doc-2] API Reference (html, 12 pages)
      [doc-3] Meeting Notes (md, 82 pages)\""
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
  "Render the GIT REPO context block(s) for the system prompt. Caveman style.
   Multi-repo aware: emits one `GIT REPO: <name>` block per attached repo,
   then a single shared tool list.

   `git-repos` is a vec of `:repo/*` entity maps (from db-list-repos) or nil.
   Returns nil when the vec is nil or empty."
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
  "Builds the system prompt — compact, token-efficient.
   All tool documentation is discoverable via (doc fn-name) in SCI.

   `git-repos` is a vec of `:repo/*` entity maps (from rlm.db/db-list-repos)
   or nil/empty."
  [{:keys [output-spec custom-docs has-reasoning? has-documents? document-summary system-prompt git-repos skill-registry]}]
  (str
    "Clojure SCI agent. Write, exec, iterate.

MINDSET:
- Reasoning: 2-5 lines max. No monologues.
- Code tasks: CODE IT. Don't mentally simulate. Sandbox is here. Code + test + :final in one shot.
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
- :forget evicts vars from sandbox. Emit :forget only when actually dropping vars this iteration.

GROUNDING:
- Only tools listed exist. Data in :final MUST come from <execution_results>. Never fabricate.

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

;; =============================================================================
;; Iteration Loop
;; =============================================================================

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.

   Uses ask! with iteration spec for provider-enforced JSON structured output.
   No regex fallback, no code-level FINAL detection.

   Params:
   `rlm-env` - RLM environment map.
   `messages` - Vector of message maps for the LLM.
   `opts` - Map, optional:
      - :iteration-spec - Spec for ask! (default: ITERATION_SPEC_NON_REASONING).
                         For reasoning providers, pass ITERATION_SPEC_REASONING.
      - :on-chunk - Streaming callback function."
  [rlm-env messages & [{:keys [iteration-spec on-chunk routing iteration reasoning-effort]
                        :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [model-name (resolve-root-model (:router rlm-env))
          effective-reasoning-effort (when (some? reasoning-effort)
                                       (or (normalize-reasoning-effort reasoning-effort)
                                         (throw (ex-info "Invalid :reasoning-effort. Expected one of low|medium|high"
                                                  {:type :rlm/invalid-reasoning-effort
                                                   :got reasoning-effort}))))
          ;; Use ask! with iteration spec — router auto-resolves max_tokens + reasoning_params
          ask-result (binding [llm/*log-context* {:query-id (:env-id rlm-env) :iteration iteration}]
                       (llm/ask! (:router rlm-env)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           on-chunk (assoc :on-chunk on-chunk)
                           effective-reasoning-effort (assoc :extra-body {:reasoning_effort effective-reasoning-effort}))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          ;; Native reasoning takes priority over spec-parsed thinking
          thinking (or model-reasoning (:thinking parsed))
          _ (rlm-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :has-final (some? (:final parsed))
               :code-count (count (:code parsed))
               :duration-ms (:duration-ms ask-result)
               :tokens (:tokens ask-result)
               :thinking thinking})
          ;; LLM's preference for next iteration's model selection
          next-optimize (when-let [opt (:next-optimize parsed)]
                          (keyword opt))
          ;; LLM's request to drop named vars from <var_index>.
          forget-names (vec (keep (fn [n] (when (and n (seq (str n))) (str n)))
                              (:forget parsed)))
          ;; Token usage from ask! result
          api-usage {:prompt_tokens (get-in ask-result [:tokens :input] 0)
                     :completion_tokens (get-in ask-result [:tokens :output] 0)
                     :completion_tokens_details {:reasoning_tokens (get-in ask-result [:tokens :reasoning] 0)}
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}]
      ;; Check for final answer in flat spec response
      (if-let [raw-final-answer (:answer parsed)]
        (let [answer-type (some-> (:answer-type parsed) keyword)
              language (when-let [l (:language parsed)] (keyword l))
              code-answer? (= answer-type :code)
              clojure? (and code-answer? (or (= language :clojure) (nil? language)))
              ;; Extract code blocks — must be maps with :expr and :time-ms
              raw-code (or (:code parsed) [])
              code-entries (vec (keep (fn [block]
                                        (when (map? block)
                                          (let [expr (str (:expr block ""))
                                                time-ms (:time-ms block)]
                                            (when-not (str/blank? expr)
                                              {:expr expr :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}))))
                                  raw-code))
              code-blocks (mapv :expr code-entries)
               ;; Execute code blocks in SCI — ALWAYS when present, regardless of answer-type.
               ;; MUST run BEFORE single-token :answer resolve so freshly-def'd vars are visible.
              exec-results (when (seq code-blocks)
                             (mapv (fn [{:keys [expr time-ms]}]
                                     (if (bare-string-code-block? expr)
                                       {:result nil :error "Bare string literal in :code. Prose belongs in :answer, not :code."
                                        :stdout "" :stderr "" :execution-time-ms 0}
                                       (execute-code rlm-env expr :timeout-ms time-ms)))
                               code-entries))
              exec-errors (when exec-results
                            (seq (filter :error exec-results)))
               ;; NOW read sandbox — code has executed, (def reply ...) is visible.
               ;; Single-word :answer mechanic: if :answer is one token matching a sandbox var,
               ;; substitute the var's value. Lets the LLM finalize with `{:answer "reply"}`
               ;; after building `reply` in :code, instead of copy-pasting the whole string.
              raw-answer (str raw-final-answer)
              locals (try (get-locals rlm-env) (catch Throwable _ {}))
              single-token? (and (re-matches #"\S+" raw-answer)
                              (try (symbol? (read-string raw-answer)) (catch Throwable _ false)))
              resolved-var-value (when single-token?
                                   (let [sym (symbol raw-answer)
                                         resolved (get locals sym)]
                                     (when (some? resolved)
                                       (let [v (if (instance? clojure.lang.IDeref resolved)
                                                 @resolved resolved)]
                                         (cond
                                           (string? v) v
                                           :else (pr-str v))))))
              raw-answer (or resolved-var-value raw-answer)
              _ (when resolved-var-value
                  (rlm-debug! {:token (str raw-final-answer)
                               :resolved-chars (count resolved-var-value)}
                    "Single-word :answer resolved to var value"))
              final-answer (paren-repair/repair-code raw-answer)
              confidence (or (:confidence parsed) :high)
              ;; Only require a self-test when the FINAL answer is Clojure code.
              ;; Text / data answers can finalize on iteration 0 without running code.
              untested? (and clojure? (zero? (or iteration 0)) (empty? code-blocks))
              parse-check (when clojure?
                            (parse-clojure-syntax final-answer))
              validation-error (or (when untested?
                                     "You submitted final without running any code. Run the self-test first.")
                                 (when exec-errors
                                   (str "Code errors before final: " (:error (first exec-errors))))
                                 (when (cleanup-claim-without-forget? final-answer forget-names)
                                   "You claimed vars/index cleanup in the final answer, but this iteration did not emit :forget. Emit :forget with the concrete var names first, then finalize.")
                                 (when (placeholder-final-answer? raw-final-answer (some? resolved-var-value))
                                   (str "Placeholder word '" (str/trim (str raw-final-answer)) "' is not a real answer. Put the actual content in :answer, or def the result and use its var name."))
                                 parse-check
                                 (validate-final {:answer final-answer
                                                  :answer-type (:answer-type parsed)
                                                  :language (:language parsed)}))
              executions (when exec-results
                           (mapv (fn [idx code result]
                                   {:id idx :code code
                                    :result (:result result) :stdout (:stdout result)
                                    :stderr (:stderr result) :error (:error result)
                                    :execution-time-ms (:execution-time-ms result)
                                    :repaired? (:repaired? result)})
                             (range) code-blocks exec-results))]
          (if validation-error
            (do (rlm-debug! {:final-answer (str-truncate final-answer 200)
                             :validation-error validation-error} "FINAL rejected")
              {:thinking thinking :next-optimize next-optimize :forget forget-names
               :executions (or executions
                             [{:id 0 :code final-answer :result nil :stdout "" :stderr ""
                               :error validation-error}])
               :final-result nil :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})
            (let [sources (vec (or (:sources parsed) []))
                  final-result (cond-> {:final? true
                                        :answer {:result final-answer :type String}
                                        :confidence confidence}
                                 (seq sources) (assoc :sources sources)
                                 (:reasoning parsed) (assoc :reasoning (:reasoning parsed)))]
              {:thinking thinking :next-optimize next-optimize :forget forget-names
               :executions (or executions []) :final-result final-result :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})))
        ;; Normal path: execute code blocks — must be maps with :expr and :time-ms
        (let [raw-parsed (or (:code parsed) [])
              ;; Extract: every block must be a map with :expr and :time-ms
              normalized (vec (keep (fn [block]
                                      (when (map? block)
                                        (let [expr (str (:expr block ""))
                                              time-ms (:time-ms block)]
                                          (when-not (str/blank? expr)
                                            {:expr expr :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}))))
                                raw-parsed))
              raw-exprs (mapv :expr normalized)
              ;; Coalesce fragments: when model splits one expression across multiple
              ;; array entries (one line per string), join unbalanced blocks with the
              ;; next until parens balance. Prevents paren-repair from "fixing" each
              ;; line individually into a broken zero-body form.
              coalesced (loop [remaining normalized
                               result []]
                          (if (empty? remaining)
                            result
                            (let [{:keys [expr time-ms]} (first remaining)
                                  bal (paren-repair/paren-balance expr)]
                              (if (and (pos? bal) (next remaining))
                                ;; Unbalanced opener - join with subsequent blocks, sum timeouts
                                (let [[joined-expr joined-time rest-blocks]
                                      (loop [acc expr
                                             t-acc (or time-ms 0)
                                             rem (rest remaining)]
                                        (if (or (<= (paren-repair/paren-balance acc) 0) (empty? rem))
                                          [acc t-acc rem]
                                          (let [nxt (first rem)]
                                            (recur (str acc "\n" (:expr nxt))
                                              (+ t-acc (or (:time-ms nxt) 0))
                                              (rest rem)))))]
                                  (recur rest-blocks
                                    (conj result {:expr joined-expr
                                                  :time-ms joined-time})))
                                (recur (rest remaining) (conj result (first remaining)))))))
              code-blocks (mapv :expr coalesced)
              time-limits (mapv :time-ms coalesced)
              total-blocks (count code-blocks)
              execution-results (mapv (fn [idx code timeout]
                                        (rlm-stage! :code-exec iteration
                                          {:idx (inc idx) :total total-blocks
                                           :code code :time-ms timeout})
                                        (if (bare-string-code-block? code)
                                          ;; Runtime rejection: prose in :code → error, not execution
                                          (let [err "Bare string literal in :code. Prose belongs in :answer with answer-type text, not in :code."]
                                            (rlm-stage! :code-result iteration
                                              {:idx (inc idx) :total total-blocks :error err})
                                            {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0})
                                          (let [r (execute-code rlm-env code :timeout-ms timeout)]
                                            (rlm-stage! :code-result iteration
                                              {:idx (inc idx) :total total-blocks
                                               :execution-time-ms (:execution-time-ms r)
                                               :error (:error r)
                                               :timeout? (:timeout? r)
                                               :result (:result r)})
                                            r)))
                                  (range) code-blocks time-limits)
              ;; Combine code blocks with their execution results
              executions (mapv (fn [idx code result]
                                 {:id idx
                                  :code code
                                  :result (:result result)
                                  :stdout (:stdout result)
                                  :stderr (:stderr result)
                                  :error (:error result)
                                  :execution-time-ms (:execution-time-ms result)
                                  :repaired? (:repaired? result)})
                           (range) code-blocks execution-results)]
          {:thinking thinking :next-optimize next-optimize
           :executions executions :final-result nil :api-usage api-usage
           :duration-ms (or (:duration-ms ask-result) 0)})))))

(defn- error-hint
  "Returns a specialized hint for a known error, or nil. Extracts context
   from the error message for a targeted fix suggestion."
  [error-msg]
  (when error-msg
    (let [e (str error-msg)]
      (cond
        ;; Unable to resolve symbol: X -> tell them exactly which symbol
        (re-find #"Unable to resolve symbol: (\S+)" e)
        (let [[_ sym] (re-find #"Unable to resolve symbol: (\S+)" e)]
          (str "'" sym "' is not defined. (def " sym " ...) or check spelling."))

        ;; Wrong number of args (N) passed to: X
        (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)
        (let [[_ n target] (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)]
          (cond
            (str/includes? target "PersistentVector")
            (str "Vectors take 1 arg (index). Use (nth v idx) or (subvec v start end), not (v " (str/join " " (repeat (parse-long n) "x")) ").")
            :else
            (str "Function expects different arity than " n ". Check with (doc fn-name).")))

        ;; Long cannot be cast to IFn
        (str/includes? e "cannot be cast to clojure.lang.IFn")
        "You're calling a non-function. Bare (1 2 3) calls 1 as fn. Use '(1 2 3) for list literals."

        ;; Nested fn literals
        (str/includes? e "Nested fn")
        "Nested #() is illegal. Rewrite inner #() as (fn [x] ...)."

        ;; Unbound fn
        (re-find #"unbound fn: #'sandbox/(\S+)" e)
        (let [[_ sym] (re-find #"unbound fn: #'sandbox/(\S+)" e)]
          (str "'" sym "' was declared but its defn failed. Fix the defn above first."))

        ;; LazySeq cast
        (str/includes? e "LazySeq")
        "conj/peek/pop need a vector, not a lazy seq. Wrap with (vec ...) first."

        ;; NullPointerException
        (str/includes? e "NullPointerException")
        "Something is nil unexpectedly. Debug with (prn suspect-value) to find which value is nil."

        ;; recur tail position
        (re-find #"recur.*tail" e)
        "recur must be the last expression in a loop/fn body."

        ;; Unmatched delimiter inside map with char literals (\} \{)
        (and (str/includes? e "Unmatched delimiter")
          (str/includes? e "}"))
        "Map literals with \\} or \\{ as keys/values confuse the reader. Use (hash-map \\) \\( \\] \\[ \\} \\{) instead of {\\} \\{ ...}."

        ;; EOF in map/fn context — might be char literal issue
        (and (str/includes? e "EOF while reading")
          (str/includes? e "match {"))
        "If your map contains bracket char literals like \\} \\{, use (hash-map ...) instead of a map literal."

        :else nil))))

(def ^:private SLOW_EXECUTION_MS
  "Threshold in ms above which execution time is flagged as slow."
  5000)

(defn format-executions
  "Formats executions for LLM feedback as EDN.
   All results shown inline — context budget handles size naturally.
   Error hints injected only when an error matches a known pattern.
   Includes execution time for every block; warns when slow."
  [executions]
  (str/join "\n"
    (map (fn [{:keys [code error result stdout repaired? execution-time-ms]}]
           (let [code-str (str/trim (or code ""))
                 hint (when error (error-hint error))
                 val-part (cond
                            error
                            (str ":error " (pr-str error)
                              (when hint (str " :hint " (pr-str hint))))

                            (fn? result)
                            (str ":error \"" code-str " is a function object. Call it: (" code-str ")\"")

                            :else
                            (str ":ok " (pr-str (realize-value result))))

                 stdout-part (when-not (str/blank? stdout)
                               (str " :stdout " (pr-str stdout)))
                 warning-part (when repaired?
                                " :warning \"auto-repaired delimiters\"")
                 time-ms (or execution-time-ms 0)
                 slow? (> time-ms SLOW_EXECUTION_MS)
                 time-part (str " :time-ms " time-ms
                             (when slow?
                               " :perf-warning \"SLOW — optimize algorithm or reduce input size\""))]
             (str "{" code-str " → " val-part (or stdout-part "") (or warning-part "") time-part "}")))
      executions)))

(def ^:private EXECUTION_SAFETY_CAP_CHARS
  "Safety cap for one value rendering in an execution receipt. Receipts show
   the FULL value — lazy seqs are already bounded by realize-value's 100-item
   cap. This guard only protects against pathological non-lazy dumps (e.g.
   a 50MB string). In normal use no truncation happens."
  200000)

(def ^:private EXECUTION_STDERR_CHARS
  "Budget for captured stderr — diagnostics only, not a full data dump."
  2000)

(defn- type-label-of
  "Short human-readable type label for result serialization."
  [v]
  (cond
    (nil? v) "nil"
    (map? v) "map"
    (vector? v) "vector"
    (set? v) "set"
    (sequential? v) "seq"
    (string? v) "string"
    (integer? v) "int"
    (float? v) "float"
    (boolean? v) "bool"
    (keyword? v) "keyword"
    :else (.getSimpleName (class v))))

(defn- size-suffix
  "Optional ' :size N-units' tail for collections / strings, blank otherwise."
  [v]
  (cond
    (nil? v) ""
    (string? v) (str " :size " (count v) "-chars")
    (coll? v) (str " :size " (count v) "-items")
    :else ""))

(defn- format-execution-results
  "Formats previous iteration's executions as an XML receipt. Each block
   shows the FULL evaluated value — no summarisation, no preview. Lazy
   sequences are already bounded by realize-value's 100-item cap; everything
   else is pr-str'd in full, protected only by a pathological-size safety
   guard. The LLM should be able to ground :final on what it sees here.

   (def sym expr) is rendered as :result-kind :var :var-name \"sym\" so the
   LLM can reference the symbol on the next iteration."
  [executions iteration]
  (when (seq executions)
    (str "<execution_results iteration=\"" iteration "\">\n"
      (str/join "\n"
        (map-indexed
          (fn [idx {:keys [code error result stdout stderr execution-time-ms]}]
            (let [code-str (str/trim (or code ""))
                  stdout-part (when-not (str/blank? stdout)
                                (str " :stdout " (pr-str stdout)))
                  stderr-part (when-not (str/blank? stderr)
                                (str " :stderr " (pr-str (str-truncate stderr EXECUTION_STDERR_CHARS))))
                  time-ms (or execution-time-ms 0)
                  slow? (> time-ms SLOW_EXECUTION_MS)
                  time-part (str " :time-ms " time-ms
                              (when slow?
                                " :perf-warning \"SLOW — optimize algorithm, avoid brute-force, reduce input size\""))
                  result-info (cond
                                error
                                (str "{:success? false :error " (pr-str (str-truncate error 400)) time-part "}")

                                (fn? result)
                                (str "{:success? false :error \"Result is a function, not a value\"" time-part "}")

                                (instance? clojure.lang.Var result)
                                (let [^clojure.lang.Var var-obj result
                                      var-name (name (.sym var-obj))
                                      bound (realize-value (.getRawRoot var-obj))
                                      value-str (str-truncate (pr-str bound) EXECUTION_SAFETY_CAP_CHARS)]
                                  (str "{:success? true :result-kind :var"
                                    " :var-name " (pr-str var-name)
                                    " :value-type " (type-label-of bound)
                                    (str/replace (size-suffix bound) ":size" ":value-size")
                                    " :value " value-str
                                    stdout-part stderr-part
                                    time-part
                                    "}"))

                                :else
                                (let [v (realize-value result)
                                      value-str (str-truncate (pr-str v) EXECUTION_SAFETY_CAP_CHARS)]
                                  (str "{:success? true :result-type " (type-label-of v)
                                    (size-suffix v)
                                    " :value " value-str
                                    stdout-part stderr-part
                                    time-part
                                    "}")))]
              (str "  [" (inc idx) "] " code-str " → " result-info)))
          executions))
      "\n</execution_results>")))

(def ^:private SQUASH_INTERVAL
  "Number of iterations per squash cycle."
  5)

(defn- format-journal-entry
  "Formats a single journal entry as text."
  [{:keys [iteration thinking var-names]}]
  (str "[iteration-" (inc iteration) "] "
    (when thinking (str-truncate thinking 300))
    (when (seq var-names)
      (str "\n    vars: " (str/join ", " var-names)))))

(defn- squash-journal
  "Performs cumulative mechanical squash on journal entries.
   Every SQUASH_INTERVAL iterations, all entries up to the boundary get concatenated
   with --- separators into one squashed block. Returns [squashed-entries recent-entries]."
  [journal]
  (let [n (count journal)
        squash-boundary (* SQUASH_INTERVAL (quot n SQUASH_INTERVAL))]
    (if (< squash-boundary SQUASH_INTERVAL)
      ;; Not enough entries to squash yet
      [nil journal]
      ;; Split into squashed + recent
      [(subvec journal 0 squash-boundary)
       (subvec journal squash-boundary)])))

(defn- render-execution-journal
  "Renders the execution journal as XML for prompt injection.
   Squashed entries are concatenated with --- separators.
   Recent entries are shown individually."
  [journal]
  (when (seq journal)
    (let [[squashed recent] (squash-journal journal)]
      (str "<execution_journal>\n"
        (when (seq squashed)
          (str "  [iterations 1-" (count squashed) " squashed]\n    "
            (str/join "\n    ---\n    "
              (map format-journal-entry squashed))
            "\n"))
        (when (seq recent)
          (str/join "\n"
            (map (fn [entry] (str "  " (format-journal-entry entry))) recent)))
        "\n</execution_journal>"))))

(defn- extract-def-names
  "Extracts var names from code blocks via EDN parsing of def-like forms."
  [executions]
  (->> executions
    (mapcat (fn [{:keys [code error]}]
              (when-not error
                (try
                  (->> (edamame/parse-string-all (or code "") {:all true})
                    (keep (fn [form]
                            (when (seq? form)
                              (let [[op name & _] form]
                                (when (and (contains? '#{def defn defn- defonce defmulti defmacro} op)
                                        (symbol? name))
                                  name)))))
                    distinct)
                  (catch Exception e
                    (trove/log! {:level :debug :id ::extract-def-names-fallback
                                 :data {:error (ex-message e)}
                                 :msg "Failed to parse code forms for def names, returning empty"})
                    [])))))
    (map str)
    vec))

(defn- restorable-var-snapshots
  "Returns serializable snapshots of user vars introduced by this iteration."
  [rlm-env executions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error (set (map symbol (extract-def-names [execution]))))])
                          executions)
        defined (into #{} (mapcat second) execution->defs)
        sym->code (reduce (fn [acc [{:keys [code]} defs]]
                            (if (and code (seq defs))
                              (reduce #(assoc %1 %2 code) acc defs)
                              acc))
                    {}
                    execution->defs)
        locals (get-locals rlm-env)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)]
                  (when (or (nil? realized)
                          (string? realized)
                          (number? realized)
                          (keyword? realized)
                          (boolean? realized)
                          (symbol? realized)
                          (map? realized)
                          (vector? realized)
                          (set? realized)
                          (list? realized)
                          (sequential? realized))
                    {:name (str sym)
                     :value realized
                     :code (get sym->code sym)})))))
      vec)))

(defn- forget-vars!
  "Unmap `names` from the SCI sandbox namespace. Mirrors what the LLM asked
   for via the `:forget` iteration-spec field — removes the bindings so they
   stop showing up in <var_index>. The persisted :iteration-var rows in the
   DB are not touched; `(restore-var 'sym)` can bring one back later."
  [sci-ctx names]
  (let [syms (keep (fn [n]
                     (cond (symbol? n) n
                       (string? n) (try (symbol n) (catch Throwable _ nil))
                       :else       nil))
               names)]
    (when (seq syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map syms)))
        (catch Throwable e
          (trove/log! {:level :debug :id ::forget-vars-failed
                       :data {:error (ex-message e) :syms (mapv str syms)}
                       :msg "forget-vars! failed — skipping"}))))))

;; =============================================================================
;; Deterministic Auto-Forget
;; =============================================================================

(def ^:const AUTO_FORGET_STALE_QUERIES
  "Number of recent queries a var must have been defined/redefined in to survive
   auto-forget. Vars without a docstring that were last touched more than this
   many queries ago are automatically removed from the live sandbox at the start
   of each new query. DB rows are untouched — (restore-var 'sym) can bring them back."
  3)

(defn auto-forget-candidates
  "Pure function. Returns the set of sandbox var symbols that should be
   auto-forgotten at the start of a new query.

   A var is a candidate for auto-forget when ALL of:
   1. It is a user var (not in `initial-ns-keys`)
   2. It has NO docstring (runtime SCI meta :doc is nil/blank)
   3. It was last defined/redefined in a query that is NOT among the
      `recent-query-ids` (last N queries of this conversation)

   Params:
   - `sandbox-map`      — SCI sandbox namespace map {symbol → value-or-var}
   - `initial-ns-keys`  — set of symbols that are built-in tools/helpers
   - `var-registry`     — result of `db-latest-var-registry`:
                           {symbol → {:query-id ... :value ... :code ...}}
   - `recent-query-ids` — set of query UUIDs for the last N queries

   Returns: set of symbols to forget."
  [sandbox-map initial-ns-keys var-registry recent-query-ids]
  (let [recent-ids (set recent-query-ids)]
    (into #{}
      (filter
        (fn [sym]
          (let [v (get sandbox-map sym)
                doc (:doc (meta v))
                has-doc? (and doc (not (str/blank? doc)))
                reg-entry (get var-registry sym)
                defining-query-id (:query-id reg-entry)]
            (and
              ;; Not a built-in
              (not (contains? initial-ns-keys sym))
              ;; No docstring — vars with docs are intentionally persisted
              (not has-doc?)
              ;; Has a registry entry (was persisted at some point)
              (some? reg-entry)
              ;; Was last defined in a query that's NOT recent
              (not (contains? recent-ids defining-query-id))))))
      (keys sandbox-map))))
(defn iteration-loop [rlm-env query
                      {:keys [output-spec max-context-tokens custom-docs system-prompt
                              pre-fetched-context query-ref user-messages
                              max-iterations max-consecutive-errors max-restarts
                              hooks cancel-atom current-iteration-atom
                              reasoning-default]}]
  (let [max-iterations (or max-iterations 50)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        ;; Adaptive budget: if rlm-env has a max-iterations-atom (set by query-env!),
        ;; read from it so the LLM can extend its own budget via (request-more-iterations n).
        ;; Otherwise use the static max-iterations parameter.
        max-iter-atom (:max-iterations-atom rlm-env)
        effective-max-iterations (fn [] (if max-iter-atom @max-iter-atom max-iterations))
        ;; Resolve effective model name for token counting
        effective-model (resolve-root-model (:router rlm-env))
        _ (assert effective-model "Router must resolve a root model — check provider config")
        ;; Default max-context-tokens to 60% of model's context window.
        ;; Prevents unbounded history accumulation (quadratic token growth over iterations).
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        ;; Check if root provider has native reasoning (thinking tokens)
        has-reasoning? (boolean (provider-has-reasoning? (:router rlm-env)))
        base-reasoning-effort (or (normalize-reasoning-effort reasoning-default)
                                reasoning-low)
        has-docs? (when-let [db (:db-info rlm-env)]
                    (pos? (count (db-list-documents db {:limit 1 :include-toc? false}))))
        doc-summary (when (and has-docs? (:db-info rlm-env))
                      (build-document-summary (:db-info rlm-env)))
        ;; Git repos are read from SQLite on each iteration (NOT from an
        ;; atom) so persistent conversations resume with attached repos
        ;; intact. Empty list elides the GIT REPO block entirely from the
        ;; system prompt.
        git-repos (when-let [db (:db-info rlm-env)]
                    (rlm-db/db-list-repos db))
        system-prompt (build-system-prompt {:output-spec output-spec
                                            :custom-docs custom-docs
                                            :has-documents? has-docs?
                                            :document-summary doc-summary
                                            :has-reasoning? has-reasoning?
                                            :system-prompt system-prompt
                                            :git-repos git-repos
                                            :max-context-tokens max-context-tokens
                                            :skill-registry (when-let [a (:skill-registry-atom rlm-env)] @a)})
        initial-user-content (str "{:requirement " (pr-str query)
                               (when pre-fetched-context
                                 (str "\n :plan " (pr-str pre-fetched-context)))
                               "}")
        ;; Build initial messages: system + structured context/requirement + original user messages (multimodal)
        initial-messages (into [{:role "system" :content system-prompt}
                                {:role "user" :content initial-user-content}]
                           (when (and user-messages
                                   (some #(sequential? (:content %)) user-messages))
                              ;; Include original multimodal messages (images etc.) as additional context
                             user-messages))
        ;; Store initial messages if history tracking is enabled
        db-info (:db-info rlm-env)
        ;; ── Auto-forget stale vars ──────────────────────────────────────
        ;; Deterministic cleanup at query boundary: remove sandbox vars that
        ;; (a) have no docstring, and (b) were last defined/redefined more
        ;; than AUTO_FORGET_STALE_QUERIES queries ago. This replaces the
        ;; unreliable "ask LLM to emit :forget" pattern for scratch vars.
        ;; DB rows are untouched — (restore-var 'sym) can bring them back.
        _ (when (and db-info (:conversation-ref rlm-env) (:sci-ctx rlm-env))
            (try
              (let [all-queries (sort-by :created-at
                                  (rlm-db/db-list-conversation-queries db-info (:conversation-ref rlm-env)))
                    recent-ids (into #{}
                                 (map :id)
                                 (take-last AUTO_FORGET_STALE_QUERIES all-queries))
                    var-registry (rlm-db/db-latest-var-registry db-info (:conversation-ref rlm-env))
                    sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
                    candidates (auto-forget-candidates sandbox-map (:initial-ns-keys rlm-env)
                                 var-registry recent-ids)]
                (when (seq candidates)
                  (trove/log! {:level :info :id ::auto-forget
                               :data {:forgotten (mapv str candidates) :count (count candidates)}
                               :msg (str "Auto-forget: evicting " (count candidates) " stale vars without docstrings")})
                  (forget-vars! (:sci-ctx rlm-env) candidates)
                  (when-let [via (:var-index-atom rlm-env)]
                    (swap! via update :current-revision inc))))
              (catch Exception e
                (trove/log! {:level :warn :id ::auto-forget-failed
                             :data {:error (ex-message e)}
                             :msg "Auto-forget failed — skipping"}))))
        ;; Cost tracking: accumulate token usage across all iterations
        usage-atom (atom {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0 :cached-tokens 0})
        accumulate-usage! (fn [api-usage]
                            (when api-usage
                              (swap! usage-atom
                                (fn [acc]
                                  (-> acc
                                    (update :input-tokens + (or (:prompt_tokens api-usage) 0))
                                    (update :output-tokens + (or (:completion_tokens api-usage) 0))
                                    (update :reasoning-tokens + (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                    (update :cached-tokens + (or (get-in api-usage [:prompt_tokens_details :cached_tokens]) 0)))))))
        ;; Repetition detection: track individual call→result pairs across iterations
        call-counts-atom (atom {})  ;; {[code result-str] count}
        detect-repetition (fn [executions]
                            (let [pairs (mapv (fn [e] [(:code e) (str-truncate (str (:result e)) 200)]) executions)
                                  counts (swap! call-counts-atom
                                           (fn [m] (reduce (fn [acc p] (update acc p (fnil inc 0))) m pairs)))
                                  repeated (->> pairs
                                             (filter #(>= (get counts % 0) 3))
                                             (map first))]
                              (when (seq repeated)
                                (str "\n\n⚠ REPETITION DETECTED: These calls have been executed 3+ times with the SAME results:\n"
                                  (str/join "\n" (map #(str "  - " (str-truncate (str %) 80)) (distinct repeated)))
                                  "\nRepeating the same action will NOT produce different results. "
                                  "You MUST try a DIFFERENT approach, or call \final\": {\"answer\": \"your answer\", \"confidence\": \"high\"} with what you have."))))
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (router/estimate-cost effective-model input-tokens output-tokens)]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :total total-tokens}
                           :cost cost}))
        ;; Cache var-index by env-level execution revision so it survives across queries.
        ;; SCI may keep stable sandbox map identity across (def ...) updates.
        ;; Use grouped :var-index-atom when available (new layout), fall back to local atom.
        ;; Layout: {:index built-idx :revision last-cache-build-rev :current-revision live-rev}
        ;;   :current-revision — bumped on every SCI mutation (was var-index-revision-atom)
        ;;   :revision         — revision at which :index was last built (was :revision in cache)
        ;; Cache hit: (:revision vi) == (:current-revision vi)
        var-index-atom (or (:var-index-atom rlm-env) (atom {:index nil :revision -1 :current-revision 0}))
        get-var-index (fn []
                        (let [{:keys [index revision current-revision]} @var-index-atom]
                          (if (= revision current-revision)
                            index
                            (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
                                  idx (build-var-index (:sci-ctx rlm-env) (:initial-ns-keys rlm-env) sandbox-map
                                        (:db-info rlm-env) (:conversation-ref rlm-env))
                                  live-rev (:current-revision @var-index-atom)]
                              (swap! var-index-atom assoc :index idx :revision live-rev)
                              idx))))
        on-chunk (:on-chunk hooks)
        on-iteration (:on-iteration hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-msg]
                     (when hook-fn
                       (try
                         (hook-fn payload)
                         (catch Exception e
                           (trove/log! {:level :warn :data {:error (ex-message e)}
                                        :msg log-msg})))))]
    ;; query-start is logged in query.clj — don't duplicate
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      (loop [iteration 0 messages initial-messages trace [] consecutive-errors 0 restarts 0
             prev-executions nil prev-iteration -1
             journal [] prev-optimize nil]
        (when current-iteration-atom
          (reset! current-iteration-atom iteration))
        (cond
          ;; Cooperative cancellation — caller-owned :cancel-atom from query-env!
          ;; (or an internal per-query atom when not supplied).
          (when cancel-atom @cancel-atom)
          (do (rlm-stage! :error iteration {:reason :cancelled})
            (emit-hook! on-cancel {:iteration iteration
                                   :status :cancelled
                                   :status-id (status->id :cancelled)}
              "on-cancel hook threw — swallowing")
            (merge {:answer nil
                    :status :cancelled
                    :status-id (status->id :cancelled)
                    :trace trace
                    :iterations iteration}
              (finalize-cost)))

          (>= iteration (effective-max-iterations))
          (let [debug? (:rlm-debug? *rlm-ctx*)
                locals (when debug? (get-locals rlm-env))]
            (rlm-stage! :error iteration {:reason :max-iterations :max (effective-max-iterations)})
            (merge {:answer nil
                    :status :max-iterations
                    :status-id (status->id :max-iterations)
                    :trace trace
                    :iterations iteration}
              (when debug? {:locals locals})
              (finalize-cost)))

          :else
          (if (>= consecutive-errors max-consecutive-errors)
            ;; Strategy restart: instead of terminating, reset with anti-knowledge
            (if (< restarts max-restarts)
              (let [failed-summary (->> trace
                                     (filter :error)
                                     (take 3)
                                     (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                                     (str/join "\n"))
                    restart-hint (str "{:strategy-restart true\n"
                                   " :errors " (pr-str failed-summary) "\n"
                                   " :instruction \"Start fresh with a DIFFERENT strategy. Do NOT repeat the same approach. Consider: different search terms, different tools, different data access pattern.\"\n"
                                   " :requirement " (pr-str query) "}")
                    restart-messages [{:role "system" :content system-prompt}
                                      {:role "user" :content restart-hint}]]
                (trove/log! {:level :info :data {:iteration iteration :restarts (inc restarts)
                                                 :errors consecutive-errors}
                             :msg "Strategy restart — resetting with anti-knowledge"})
                (rlm-debug! {:failed-summary failed-summary} "Strategy restart triggered")
                (recur (inc iteration) restart-messages trace 0 (inc restarts)
                  nil -1 journal nil))
              (do (trove/log! {:level :warn :data {:iteration iteration :consecutive-errors consecutive-errors
                                                   :restarts restarts}
                               :msg "Error budget exhausted — too many consecutive errors across restarts. Simplify your code or break the task into smaller steps."})
                (merge {:answer nil :status :error-budget-exhausted :trace trace :iterations iteration}
                  {:status-id (status->id :error-budget-exhausted)}
                  (finalize-cost))))
            (let [reasoning-effort (when has-reasoning?
                                     (reasoning-effort-for-errors base-reasoning-effort consecutive-errors))
                  _ (rlm-stage! :iter-start iteration {:msg-count (count messages) :reasoning reasoning-effort})
                  ;; Build single-shot prompt: conversation + journal + execution results + var index
                  var-index-str (get-var-index)
                  exec-results-str (format-execution-results prev-executions prev-iteration)
                  journal-str (render-execution-journal journal)
                  iteration-context (str
                                      (when journal-str (str "\n" journal-str))
                                      (when exec-results-str (str "\n" exec-results-str))
                                      (when var-index-str
                                        (str "\n<var_index>\n" var-index-str "\n</var_index>")))
                  base-messages (vec (take 2 messages)) ;; [system-prompt, user-query]
                  effective-messages (cond-> base-messages
                                       (not (str/blank? iteration-context))
                                       (conj {:role "user" :content iteration-context}))
                  iter-on-chunk (when on-chunk
                                  (fn [{:keys [result reasoning tokens cost done?]}]
                                    (if done?
                                      (on-chunk {:iteration iteration
                                                 :thinking nil
                                                 :code nil
                                                 :final nil
                                                 :tokens tokens
                                                 :cost cost
                                                 :done? true})
                                      (on-chunk {:iteration iteration
                                                 :thinking (or reasoning (:thinking result))
                                                 :code (when-let [c (:code result)]
                                                         (if (sequential? c) (vec c) nil))
                                                 :final nil
                                                 :tokens nil
                                                 :cost nil
                                                 :done? false}))))
                  iteration-result (try
                                     (run-iteration rlm-env effective-messages
                                       (cond-> {:iteration-spec (if has-reasoning?
                                                                  ITERATION_SPEC_REASONING
                                                                  ITERATION_SPEC_NON_REASONING)
                                                :iteration iteration
                                                :reasoning-effort reasoning-effort
                                                :routing (when prev-optimize {:optimize prev-optimize})}
                                         iter-on-chunk (assoc :on-chunk iter-on-chunk)))
                                     (catch Exception e
                                       (let [err-msg (ex-message e)
                                             err-type (:type (ex-data e))]
                                         (trove/log! {:level :warn
                                                      :data {:iteration iteration :error err-msg :type err-type}
                                                      :msg "RLM iteration failed, feeding error to LLM"})
                                         ;; Return ::iteration-error sentinel — loop will feed error to LLM
                                         {::iteration-error {:message err-msg :type err-type}})))]
              (if-let [iter-err (::iteration-error iteration-result)]
                ;; Error path: feed error back to LLM as user message, let it recover
                (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                       "<error>LLM call failed: " (:message iter-err) "</error>\n"
                                       "The previous attempt failed. Adjust your approach or call \final\": {\"answer\": \"your answer\", \"confidence\": \"high\"} with what you have.")
                      trace-entry {:iteration iteration :error iter-err :final? false}]
                  ;; Store error iteration snapshot
                  (rlm-db/store-iteration! db-info
                    {:query-ref query-ref
                     :vars []
                     :executions nil :thinking nil :duration-ms 0})
                  ;; Global observer hook after store-iteration! (error path)
                  (emit-hook! on-iteration
                    {:iteration iteration
                     :status :error
                     :status-id (status->id :error)
                     :thinking nil
                     :executions nil
                     :final-result nil
                     :error iter-err
                     :duration-ms 0}
                    "on-iteration hook threw (error branch) — swallowing")
                  (recur (inc iteration)
                    (conj messages {:role "user" :content error-feedback})
                    (conj trace trace-entry)
                    (inc consecutive-errors)
                    restarts
                    nil -1 journal nil))
                ;; Normal path — accumulate token usage
                (let [_ (accumulate-usage! (:api-usage iteration-result))
                      {:keys [thinking executions final-result next-optimize forget]} iteration-result
                      _ (when (seq forget)
                          (forget-vars! (:sci-ctx rlm-env) forget)
                          (swap! var-index-atom update :current-revision inc))
                      vars-snapshot (restorable-var-snapshots rlm-env executions)
                      ;; Store iteration snapshot — exact input/output for fine-tuning
                      _traj-iter (rlm-db/store-iteration! db-info
                                   {:query-ref query-ref
                                    :executions executions
                                    :vars vars-snapshot
                                    :thinking thinking
                                    :answer (when final-result (answer-str (:answer final-result)))
                                    :duration-ms (or (:duration-ms iteration-result) 0)})
                      ;; Global observer hook after store-iteration! (success/empty/final)
                      _ (emit-hook! on-iteration
                          {:iteration iteration
                           :status (cond
                                     final-result :final
                                     (empty? executions) :empty
                                     :else :success)
                           :status-id (status->id (cond
                                                    final-result :final
                                                    (empty? executions) :empty
                                                    :else :success))
                           :thinking thinking
                           :executions executions
                           :final-result final-result
                           :error nil
                           :duration-ms (or (:duration-ms iteration-result) 0)}
                          "on-iteration hook threw (success branch) — swallowing")
                      trace-entry {:iteration iteration
                                   :thinking thinking
                                   :executions executions
                                   :final? (boolean final-result)}]
                  (if final-result
                    (do (rlm-stage! :final iteration
                          {:answer (str-truncate (answer-str (:answer final-result)) 200)
                           :confidence (:confidence final-result)
                           :iterations (inc iteration)})
                      (rlm-stage! :iter-end iteration
                        {:blocks (count executions)
                         :errors (count (filter :error executions))
                         :times (mapv :execution-time-ms executions)})
                        ;; Fire final streaming callback
                      (when on-chunk
                        (on-chunk {:iteration iteration
                                   :thinking thinking
                                   :code (mapv :code executions)
                                   :final {:answer (:answer final-result)
                                           :confidence (:confidence final-result)
                                           :summary (:summary final-result)
                                           :iterations (inc iteration)
                                           :status :success}
                                   :done? true}))
                        ;; Final result persisted via store-iteration! with :answer
                      (merge (cond-> {:answer (:answer final-result)
                                      :trace (conj trace trace-entry)
                                      :iterations (inc iteration)
                                      :confidence (:confidence final-result)}
                               (:sources final-result)   (assoc :sources (:sources final-result))
                               (:reasoning final-result) (assoc :reasoning (:reasoning final-result)))
                        (finalize-cost)))
                    (if (empty? executions)
                      ;; Empty iteration: DON'T increment iteration counter, DON'T add to trace.
                      ;; Retry immediately with a nudge — this doesn't waste an iteration slot.
                      (let [_ (rlm-stage! :empty iteration {})
                            nudge (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                    "{:requirement " (pr-str (str-truncate query 200)) "}\n"
                                    "⚠ EMPTY — no code executed. You MUST include code. "
                                    (if has-reasoning?
                                      "Respond with code or set final to finish."
                                      "Respond with thinking + code, or set final to finish."))]
                        ;; Store empty iteration snapshot
                        (rlm-db/store-iteration! db-info
                          {:query-ref query-ref
                           :vars []
                           :executions nil :thinking thinking :duration-ms (or (:duration-ms iteration-result) 0)})
                        (recur (inc iteration) ;; still increment to prevent infinite loop
                          (conj messages
                            {:role "assistant" :content (or thinking "[empty]")}
                            {:role "user" :content nudge})
                          trace ;; DON'T add empty trace entry
                          (inc consecutive-errors)
                          restarts
                          nil -1 journal next-optimize))
                      ;; Normal iteration with executions
                      (let [exec-feedback (format-executions executions)
                            iteration-header (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                               "{:requirement " (pr-str (str-truncate query 200)) "}")
                            repetition-warning (detect-repetition executions)
                            remaining-iters (- (effective-max-iterations) (inc iteration))
                            budget-warning (when (<= remaining-iters 5)
                                             (str "\n[SYSTEM_NUDGE] Only " remaining-iters " iterations left! "
                                               "Set final NOW with what you have. DO NOT start new explorations."))
                            force-final-nudge (when (> iteration 20)
                                                (str "\n[SYSTEM_NUDGE] You have been running for " (inc iteration) " iterations. "
                                                  "STOP exploring. Set final IMMEDIATELY with your current findings."))
                            user-feedback (str iteration-header "\n" exec-feedback repetition-warning budget-warning force-final-nudge)]
                        (rlm-stage! :iter-end iteration
                          {:blocks (count executions)
                           :errors (count (filter :error executions))
                           :times (mapv :execution-time-ms executions)})
                        (let [had-successful-execution? (some #(nil? (:error %)) executions)
                              next-errors (if had-successful-execution? 0 (inc consecutive-errors))
                              _ (when had-successful-execution?
                                  (swap! var-index-atom update :current-revision inc))
                              journal-entry {:iteration iteration
                                             :thinking thinking
                                             :var-names (extract-def-names executions)}]
                          (recur (inc iteration)
                            messages
                            (conj trace trace-entry)
                            next-errors
                            restarts
                            executions iteration
                            (conj journal journal-entry) next-optimize))))))))))))))

;; =============================================================================
;; Entity Extraction Functions
;; =============================================================================

(defn extract-entities-from-page!
  "Extracts entities from a page's text nodes using LLM.

   Params:
   `text-content` - String. Combined text from page nodes.
   `rlm-router` - Router from llm/make-router.

   Returns:
   Map with :entities and :relationships keys (empty if extraction fails)."
  [text-content rlm-router]
  (try
    (let [truncated (if (> (count text-content) 8000) (subs text-content 0 8000) text-content)
          response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                         :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                    (llm/user truncated)]
                                         :routing {:optimize :cost}})]
      (or (:result response) {:entities [] :relationships []}))
    (catch Exception e
      (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Entity extraction failed for page"})
      {:entities [] :relationships []})))

(defn extract-entities-from-visual-node!
  "Extracts entities from a visual node (image/table) using vision or text.

   Params:
   `node` - Map. Page node with :type, :image-data, :description.
   `rlm-router` - Router from llm/make-router.

   Returns:
   Map with :entities and :relationships keys (empty if extraction fails)."
  [node rlm-router]
  (try
    (let [image-data (:image-data node)
          description (:description node)]
      (cond
        ;; Has image data - use vision
        image-data
        (let [b64 (bytes->base64 image-data)
              response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                             :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                        (llm/user (or description "Extract entities from this image")
                                                          (llm/image b64 "image/png"))]
                                             :routing {:optimize :cost}})]
          (or (:result response) {:entities [] :relationships []}))
        ;; Has description only - text extraction
        description
        (let [response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                             :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                        (llm/user description)]
                                             :routing {:optimize :cost}})]
          (or (:result response) {:entities [] :relationships []}))
        ;; Neither - skip
        :else
        (do (trove/log! {:level :warn :msg "Visual node has no image-data or description, skipping"})
          {:entities [] :relationships []})))
    (catch Exception e
      (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Visual node extraction failed"})
      {:entities [] :relationships []})))

(defn extract-entities-from-document!
  "Extracts entities from all pages of a document.

   Params:
   `db-info` - Map. Database info with :store key.
   `document` - Map. PageIndex document.
   `rlm-router` - Router from llm/make-router.
   `opts` - Map. Options with :max-extraction-pages, :max-vision-rescan-nodes.

   Returns:
   Map with extraction statistics: :entities-extracted, :relationships-extracted,
   :pages-processed, :extraction-errors, :visual-nodes-scanned."
  [db-info document rlm-router opts]
  (let [max-pages (or (:max-extraction-pages opts) 50)
        max-vision (or (:max-vision-rescan-nodes opts) 10)
        pages (take max-pages (:pages document))
        doc-id (:id document (:name document))
        entities-atom (atom [])
        relationships-atom (atom [])
        errors-atom (atom 0)
        vision-count-atom (atom 0)]
    ;; Process each page
    (doseq [page pages]
      (let [nodes (:nodes page)
            text-nodes (filter #(not (#{:image :table} (:type %))) nodes)
            visual-nodes (filter #(#{:image :table} (:type %)) nodes)]
        ;; Extract from text
        (when (seq text-nodes)
          (let [text (str/join "\n" (keep :content text-nodes))]
            (when (not (str/blank? text))
              (try
                (let [result (extract-entities-from-page! text rlm-router)]
                  (swap! entities-atom into (:entities result))
                  (swap! relationships-atom into (:relationships result)))
                (catch Exception e
                  (trove/log! {:level :warn :data {:page (:index page) :error (ex-message e)}
                               :msg "Entity extraction failed for page"})
                  (swap! errors-atom inc))))))
        ;; Extract from visual nodes (capped)
        (doseq [vnode visual-nodes]
          (when (< @vision-count-atom max-vision)
            (try
              (let [result (extract-entities-from-visual-node! vnode rlm-router)]
                (swap! vision-count-atom inc)
                (swap! entities-atom into (:entities result))
                (swap! relationships-atom into (:relationships result)))
              (catch Exception e
                (trove/log! {:level :warn :data {:node-type (:type vnode) :error (ex-message e)}
                             :msg "Entity extraction failed for visual node"})
                (swap! errors-atom inc)))))))
    ;; Persist extraction results (entity + relationship transactions)
    (let [entities @entities-atom
          relationships @relationships-atom
          persisted (rlm-data/store-extraction-results! db-info doc-id entities relationships)]
      {:entities-extracted (:entities-extracted persisted)
       :relationships-extracted (:relationships-extracted persisted)
       :pages-processed (count pages)
       :extraction-errors @errors-atom
       :visual-nodes-scanned @vision-count-atom})))
