(ns com.blockether.vis.loop.storage.schema
  (:require
   [charred.api :as json]
   [clojure.java.process :as proc]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.form-repair :as form-repair]
   [com.blockether.svar.internal.spec :as spec]
   [fast-edn.core :as edn])
  (:import
   [java.util Base64]))

(def MAX_ITERATIONS
  "Default iteration budget before forcing termination.
   Kept deliberately tight — the LLM must justify its budget. It can extend
   this at runtime via (request-more-iterations n) up to MAX_ITERATION_CAP."
  10)

(def MAX_ITERATION_CAP
  "Absolute ceiling for iterations. No amount of request-more-iterations
   can exceed this. Safety valve against runaway loops."
  500)

(def MAX_EXTENSION_PER_REQUEST
  "Maximum iterations that can be granted per single request-more-iterations call.
   Prevents the LLM from requesting 500 iterations in one shot."
  50)

(def DEFAULT_RECURSION_DEPTH
  "Default maximum depth of nested rlm-query calls. Can be overridden via :max-recursion-depth."
  5)

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in SCI sandbox.
   Must be long enough for nested sub-rlm-query calls."
  120000)

(def MIN_EVAL_TIMEOUT_MS
  "Floor for :eval-timeout-ms. 3 s gives filesystem tools (grep, list-dir)
   headroom on medium-sized repos. Below ~1 s nearly every grep timed out
   at the race boundary; 3 s leaves comfortable margin without masking
   genuine infinite loops."
  3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms to prevent runaway SCI futures.
   30 minutes — anything longer is a bug, not a feature."
  (* 30 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in ms for SCI code eval. Bound per query-env! call via
   :eval-timeout-ms opt. Nested queries inherit outer binding. Clamped at
   the rlm.clj API boundary to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  DEFAULT_EVAL_TIMEOUT_MS)

;; =============================================================================
;; Concurrency Settings
;; =============================================================================

(def DEFAULT_CONCURRENCY
  "Default concurrency settings for sub-rlm-query-batch and nested calls.
   Applied when :concurrency opt is absent from query-env!."
  {:max-parallel-llm   8       ; HTTP calls in flight to LLM provider (reentrant sem)
   :max-skills-per-call 2      ; ceiling on :skills vec length per sub-rlm-query
   :default-timeout-ms 30000   ; total wall-clock per sub-rlm-query call
   :http-timeout-ms    20000}) ; per HTTP request to provider

(def ^:dynamic *sub-rlm-deadline*
  "Absolute wall-clock deadline (java.time.Instant) for the current sub-rlm-query
   call tree. Nested sub-rlm-query calls inherit min(caller, parent-remaining).
   Nil when no deadline is active (unbounded)."
  nil)

(def ^:dynamic *concurrency*
  "Merged concurrency settings for the current query-env! session.
   Bound at query-env! entry, inherited by nested calls via Clojure binding
   propagation through future macro."
  DEFAULT_CONCURRENCY)

(def ^:dynamic *concurrency-semaphore*
  "Query-env-scoped reentrant semaphore (from rlm.concurrency). Bound at
   query-env! entry, used by sub-rlm-query-batch for HTTP slot acquisition.
   Nil when no query-env! session is active."
  nil)

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS].
   `candidate` may be nil — callers should resolve the fallback (opt → dyn var)
   before calling. Accepts any integer, coerces to long. Prevents runaway SCI
   futures from absurdly high values and sub-second timeouts from absurdly low."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

(defn validate-clojure-code
  "Validates a string that looks like Clojure code.
   Returns nil if valid, or an error string if broken.
   Used by FINAL_SPEC and :code field validators."
  [s]
  (let [s (str/trim (str s))]
    (when (re-find #"^\s*[\(#\[\{]" s)
      (cond
        ;; __ placeholder left in answer - model didn't replace it
        (re-find #"\b__\b" s)
        "Your answer contains '__' placeholder. Replace __ with your actual expression."

        ;; Bare % args outside #() - model wrote (first (drop %2 %1)) instead of #(first (drop %2 %1))
        ;; Only flag when % appears but NO #() exists anywhere in the expression
        (and (re-find #"(?<!\w)%[1-9&]?" s)
          (not (re-find #"#\(" s)))
        "Your answer uses % args (%1, %2) outside #(). Wrap it: #(your-expression) or use (fn [a b] ...)."

        ;; Nested #() - illegal in Clojure
        (re-find #"#\([^)]*#\(" s)
        "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"

        ;; Paren balance check - if repair changes the code, it's unbalanced
        (not= s (form-repair/repair-code s))
        "Unbalanced delimiters in code. Check your parens/brackets."

        :else nil))))

(defn validate-json
  "Validates a JSON string. Returns nil if valid, error string if broken."
  [s]
  (try (json/read-json (str s)) nil
    (catch Exception e (str "Invalid JSON: " (ex-message e)))))

(defn validate-edn
  "Validates an EDN string. Returns nil if valid, error string if broken."
  [s]
  (try (edn/read-string (str s)) nil
    (catch Exception e (str "Invalid EDN: " (ex-message e)))))

(defn validate-python
  "Validates Python syntax via python3 compile(). Returns nil if valid."
  [s]
  (try
    (let [code (str s)
          p (proc/start {:err :stdout} "python3" "-c"
              (str "compile(" (pr-str code) ", '<answer>', 'exec')"))
          ok (.waitFor p 5000 java.util.concurrent.TimeUnit/MILLISECONDS)]
      (if (and ok (zero? (.exitValue p)))
        nil
        (let [out (slurp (.getInputStream p))]
          (str "Python syntax error: " (str/trim out)))))
    (catch Exception e (str "Python validation failed: " (ex-message e)))))

(defn validate-final
  "Validates a final answer based on its declared type and language.
   answer-type and language are keyword enums from FINAL_SPEC."
  [{:keys [answer answer-type language]}]
  (let [s (str answer)
        atype (or answer-type :code)
        lang  (or language :clojure)]
    (case atype
      :code (case lang
              :clojure (validate-clojure-code s)
              :python  (validate-python s)
              (validate-clojure-code s))
      :data (case lang
              :json (validate-json s)
              :edn  (validate-edn s)
              nil)
      :text nil
      nil)))

(def CODE_BLOCK_SPEC
  "Spec for a single code block with its expected execution time budget."
  (spec/spec :code_block
    (spec/field {::spec/name :expr
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Clojure expression to execute in the sandbox"})
    (spec/field {::spec/name :time-ms
                 ::spec/type :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Expected max execution time in ms. def 100, assert 500, heavy 2000. Max 5000."})))

(def NEXT_SPEC
  "Per-iteration steering hint. Both fields are optional — emit `:next` only
   when you want to change something for the next turn.

   `:model` picks a model class from the router's configured pool:
     cost         → cheapest available (rough drafts, simple lookups)
     speed        → fastest available (trivial follow-ups)
     intelligence → most capable available (hard reasoning, synthesis)

   `:reasoning` picks a thinking depth. vis translates this to the right
   provider-specific param — `reasoning_effort` on OpenAI GPT-5/o-series,
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
  (spec/spec :next_turn
    (spec/field {::spec/name :model
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required false
                 ::spec/description "Model class to use on the next iteration"
                 ::spec/values ["cost" "speed" "intelligence"]})
    (spec/field {::spec/name :reasoning
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required false
                 ::spec/description "Thinking depth for the next iteration"
                 ::spec/values ["quick" "balanced" "deep"]})))

(defn- make-iteration-spec
  "Builds an iteration response spec.

   Opts:
     :include-thinking? — true for non-reasoning providers (CoT goes in
       JSON), false for reasoning providers (CoT is native, no duplication).
     :include-sources?  — true when at least one document-retrieval tool
       is active in the current env (has-documents? in the agent loop).
       When false, the :sources field is omitted entirely — no tool name
       leaks into the spec and the LLM isn't nudged to produce source IDs
       it cannot possibly have."
  [{:keys [include-thinking? include-sources?]}]
  (let [base-fields (cond-> [(spec/field {::spec/name :code
                                          ::spec/type :spec.type/ref
                                          ::spec/target :code_block
                                          ::spec/cardinality :spec.cardinality/many
                                          ::spec/required false
                                          ::spec/description "Code blocks to execute. Each has :expr and :time-ms. Always executes, even with :final."})
                             (spec/field {::spec/name :next
                                          ::spec/type :spec.type/ref
                                          ::spec/target :next_turn
                                          ::spec/cardinality :spec.cardinality/one
                                          ::spec/required false
                                          ::spec/description "Optional steering hint for the next iteration. Either or both sub-keys may be set."})
                             (spec/field {::spec/name :forget
                                          ::spec/type :spec.type/string
                                          ::spec/cardinality :spec.cardinality/many
                                          ::spec/required false
                                          ::spec/description "Var names to drop from <var_index>. DB rows stay — the binding is just unmapped from the sandbox."})
                             (spec/field {::spec/name :answer
                                          ::spec/type :spec.type/string
                                          ::spec/cardinality :spec.cardinality/one
                                          ::spec/required false
                                          ::spec/description "Final answer. Single-word var names auto-resolve to their runtime value. Send with any needed :code. :code runs first."})
                             ;; Values-only enum (svar 0.3.2+). Mustache
                             ;; semantics are documented once in the ARCH
                             ;; section of `runtime.prompt`; no need to
                             ;; re-paste them per iteration into the JSON
                             ;; schema.
                             (spec/field {::spec/name :answer-type
                                          ::spec/type :spec.type/keyword
                                          ::spec/cardinality :spec.cardinality/one
                                          ::spec/required true
                                          ::spec/description "REQUIRED with :answer. How to render the answer (see ARCH / MUSTACHE)."
                                          ::spec/values ["mustache-text" "mustache-markdown"]})
                             (spec/field {::spec/name :confidence
                                          ::spec/type :spec.type/keyword
                                          ::spec/cardinality :spec.cardinality/one
                                          ::spec/required false
                                          ::spec/description "Confidence level"
                                          ::spec/values ["high" "medium" "low"]})]
                      ;; :sources only shows up when document-retrieval tools
                      ;; are actually callable this turn. Otherwise the LLM
                      ;; would be told to cite sources it cannot fetch.
                      include-sources?
                      (conj (spec/field {::spec/name :sources
                                         ::spec/type :spec.type/string
                                         ::spec/cardinality :spec.cardinality/many
                                         ::spec/required false
                                         ::spec/description "IDs of sources (page.node, document, entity) that grounded the :answer. Required whenever you pulled content from any document-retrieval tool this turn."})))
        fields (if include-thinking?
                 (into [(spec/field {::spec/name :thinking
                                     ::spec/type :spec.type/string
                                     ::spec/cardinality :spec.cardinality/one
                                     ::spec/description "Your reasoning: what you observed, what you learned, what to do next"})]
                   base-fields)
                 base-fields)]
    (apply spec/spec {:refs [CODE_BLOCK_SPEC NEXT_SPEC]} fields)))

(defn iteration-spec
  "Compose the iteration response spec for the CURRENT env state. Callers
   pass :has-reasoning? and :has-documents?; the former selects the
   thinking/non-thinking variant, the latter gates the :sources field.

   Keep this as the single call site — `ITERATION_SPEC_*` constants below
   are legacy defaults kept for tests that don't thread env state."
  [{:keys [has-reasoning? has-documents?]}]
  (make-iteration-spec {:include-thinking? (not has-reasoning?)
                        :include-sources?  (boolean has-documents?)}))

(def ITERATION_SPEC_BASE
  "Legacy default — :sources INCLUDED, no :thinking. Kept for backwards
   compat with existing consumers; agent loop should use `iteration-spec`
   so :sources tracks document-tool activation."
  (make-iteration-spec {:include-thinking? false :include-sources? true}))

(def ITERATION_SPEC_NON_REASONING
  "Legacy default with :thinking. See ITERATION_SPEC_BASE note."
  (make-iteration-spec {:include-thinking? true :include-sources? true}))

(def ITERATION_SPEC_REASONING
  "Legacy default — alias for ITERATION_SPEC_BASE."
  ITERATION_SPEC_BASE)

(def SUB_RLM_QUERY_SPEC
  "Spec for sub-rlm-query responses. Forces structured output: prose content +
   optional Clojure code blocks. Used by make-routed-sub-rlm-query-fn so callers
   receive a provider-enforced {:content :code} shape with no regex parsing.

   :content is always present (prose answer).
   :code is a vec of complete Clojure expressions. Each entry is one form.
   Omit :code or return empty vec when no code applies."
  (spec/spec
    {:refs []}
    (spec/field {::spec/name :content
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Prose answer. Include reasoning and explanations here."})
    (spec/field {::spec/name :code
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/required false
                 ::spec/description "Optional Clojure expressions to execute in the caller sandbox. One complete form per vec entry. Omit when not applicable."})))

(defn bytes->base64
  "Converts raw bytes to a base64 string.
   
   Params:
   `bs` - byte[]. Raw bytes.
   
   Returns:
   String. Base64-encoded representation."
  [^bytes bs]
  (.encodeToString (Base64/getEncoder) bs))

(def ^:dynamic *max-recursion-depth*
  "Dynamic var for max recursion depth. Bound per query-env! call."
  DEFAULT_RECURSION_DEPTH)

(def ^:dynamic *rlm-ctx*
  "Dynamic context for RLM debug logging. Bind with {:rlm-debug? true :rlm-phase :phase-name :rlm-env-id \"...\"}."
  nil)

(def BLOOM_DIFFICULTIES
  "Bloom's taxonomy cognitive levels as difficulty progression."
  {"remember"    "Simple recall of facts, definitions, or terms directly stated in the text"
   "understand"  "Explain concepts, summarize, paraphrase, or interpret meaning from the text"
   "apply"       "Use information from the text to solve a new problem or scenario"
   "analyze"     "Break down information, identify patterns, compare elements across sections"
   "evaluate"    "Judge, assess, or critique claims, arguments, or evidence from the text"
   "create"      "Synthesize information from multiple parts to form a new conclusion or insight"})

(def QUESTION_CATEGORIES
  "Question type categories."
  {"factual"      "Direct fact extraction — answer is explicitly stated"
   "inferential"  "Requires reasoning from stated facts to reach the answer"
   "comparative"  "Compares or contrasts two or more concepts, entities, or processes"
   "analytical"   "Requires breaking down complex information or identifying relationships"
   "definitional" "Asks for definitions, explanations, or descriptions of concepts"
   "procedural"   "Asks about processes, steps, methods, or how something works"})

(def GENERATION_PERSONAS
  "Persona descriptions to diversify question styles."
  {:student     "You are a curious undergraduate student studying this material for the first time. Ask questions that test foundational understanding and clarify key concepts."
   :researcher  "You are an academic researcher looking for precise details and methodological rigor. Ask technical, specific questions that require careful reading."
   :practitioner "You are a working professional who needs to apply this knowledge. Ask practical, application-oriented questions about how to use the information."
   :examiner    "You are a rigorous exam designer creating assessment questions. Ask questions that test deep comprehension and the ability to distinguish subtle details."
   :journalist  "You are an investigative journalist looking for the most important claims and evidence. Ask questions that probe key findings, numbers, and conclusions."})

;; -- Specs --

(def QUESTION_SPEC
  "Spec for a single generated question-answer pair."
  (spec/spec
    :question
    (spec/field {::spec/name :question
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "The question text — must be self-contained and understandable without the source document"})
    (spec/field {::spec/name :answer
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "The answer, grounded in source material"})
    (spec/field {::spec/name :evidence-span
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Exact verbatim quote from the source document that supports the answer"})
    (spec/field {::spec/name :source-document
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Source document ID"})
    (spec/field {::spec/name :source-page
                 ::spec/type :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Source page number (0-based)"})
    (spec/field {::spec/name :source-section
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required false
                 ::spec/description "Source section or heading title"})
    (spec/field {::spec/name :difficulty
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/values BLOOM_DIFFICULTIES
                 ::spec/description "Bloom's taxonomy cognitive level"})
    (spec/field {::spec/name :category
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/values QUESTION_CATEGORIES
                 ::spec/description "Question category"})))

(def QUESTIONIFY_SPEC
  "Spec for query-env-qa! Q&A generation output."
  (spec/spec
    {:refs [QUESTION_SPEC]}
    (spec/field {::spec/name :questions
                 ::spec/type :spec.type/ref
                 ::spec/target :question
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/description "Generated question-answer pairs"})))

(def PASSAGE_SPEC
  "Spec for a selected passage from Phase 1."
  (spec/spec
    :passage
    (spec/field {::spec/name :document-id
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Document ID"})
    (spec/field {::spec/name :page
                 ::spec/type :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Page number (0-based)"})
    (spec/field {::spec/name :section-title
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Section or heading title"})
    (spec/field {::spec/name :content-summary
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Brief summary of what this passage covers (1-2 sentences)"})
    (spec/field {::spec/name :suggested-difficulty
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/values BLOOM_DIFFICULTIES
                 ::spec/description "Suggested Bloom's taxonomy difficulty level for questions from this passage"})
    (spec/field {::spec/name :suggested-category
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/values QUESTION_CATEGORIES
                 ::spec/description "Suggested question category for this passage"})))

(def CHUNK_SELECTION_SPEC
  "Spec for Phase 1 passage selection output."
  (spec/spec
    {:refs [PASSAGE_SPEC]}
    (spec/field {::spec/name :passages
                 ::spec/type :spec.type/ref
                 ::spec/target :passage
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/description "Selected passages for Q&A generation"})))

(def VERIFICATION_RESULT_SPEC
  "Spec for a single verification result."
  (spec/spec
    :verification
    (spec/field {::spec/name :question-index
                 ::spec/type :spec.type/int
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Index of the question being verified (0-based)"})
    (spec/field {::spec/name :grounded
                 ::spec/type :spec.type/bool
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Whether the evidence span actually exists in the source and supports the answer"})
    (spec/field {::spec/name :non-trivial
                 ::spec/type :spec.type/bool
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Whether the question requires reading the document — not answerable from titles or headings alone"})
    (spec/field {::spec/name :self-contained
                 ::spec/type :spec.type/bool
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Whether the question is understandable without the source document context"})
    (spec/field {::spec/name :answerable
                 ::spec/type :spec.type/bool
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Whether the question can be answered from the evidence span alone, without external knowledge"})
    (spec/field {::spec/name :answer-consistent
                 ::spec/type :spec.type/bool
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/description "Whether the provided answer accurately matches the question's intent and the evidence"})
    (spec/field {::spec/name :verdict
                 ::spec/type :spec.type/keyword
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/values {"pass" "Question meets all quality criteria"
                                "fail" "Question has fundamental issues and should be dropped"
                                "needs-revision" "Question has minor issues but contains value"}
                 ::spec/description "Verification verdict"})
    (spec/field {::spec/name :revision-note
                 ::spec/type :spec.type/string
                 ::spec/cardinality :spec.cardinality/one
                 ::spec/required false
                 ::spec/description "Explanation of issues if verdict is not pass"})))

(def VERIFICATION_SPEC
  "Spec for Phase 3 verification output."
  (spec/spec
    {:refs [VERIFICATION_RESULT_SPEC]}
    (spec/field {::spec/name :verifications
                 ::spec/type :spec.type/ref
                 ::spec/target :verification
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/description "Verification results for each question"})))

;; -- Prompt builders --

(def DEDUP_SPEC
  "Spec for LLM-based semantic deduplication output."
  (spec/spec
    (spec/field {::spec/name :keep-indices
                 ::spec/type :spec.type/int
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/description "0-based indices of questions to KEEP — one per semantic group, choosing the highest quality version"})))

(def REVISION_SPEC
  "Spec for revising questions that need improvement."
  (spec/spec
    {:refs [QUESTION_SPEC]}
    (spec/field {::spec/name :questions
                 ::spec/type :spec.type/ref
                 ::spec/target :question
                 ::spec/cardinality :spec.cardinality/many
                 ::spec/description "Revised question-answer pairs"})))

;; =============================================================================
;; PageIndex Specs (merged from internal.pageindex.spec)
;; =============================================================================

;;; ============================================================================
;;; Page Extraction Specs (Node-Based Structure)
;;; ============================================================================

(def ^:private CONTENT_NODE_TYPES
  #{:section :heading :paragraph :list-item :image :table :header :footer :metadata})

(defn- valid-content-node-map?
  [m]
  (and (map? m)
    (contains? m :type)
    (contains? m :id)
    (contains? CONTENT_NODE_TYPES (:type m))
    (string? (:id m))
    (or (not (contains? m :parent-id)) (nil? (:parent-id m)) (string? (:parent-id m)))
    (or (not (contains? m :level)) (string? (:level m)))
    (or (not (contains? m :content)) (string? (:content m)))
    (or (not (contains? m :image-data)) (bytes? (:image-data m)))
    (or (not (contains? m :description)) (nil? (:description m)) (string? (:description m)))
    (or (not (contains? m :continuation?)) (boolean? (:continuation? m)))
    (or (not (contains? m :caption)) (nil? (:caption m)) (string? (:caption m)))
    (or (not (contains? m :kind)) (string? (:kind m)))
    (or (not (contains? m :group-id)) (string? (:group-id m)))
    (or (not (contains? m :bbox))
      (and (vector? (:bbox m)) (= 4 (count (:bbox m))) (every? int? (:bbox m))))
    (or (not (contains? m :image-index)) (int? (:image-index m)))))

(s/def ::content-node valid-content-node-map?)

;; Page map (extraction result - node-based)
(s/def ::page
  (s/and map?
    #(contains? % :index)
    #(contains? % :nodes)
    #(nat-int? (:index %))
    #(vector? (:nodes %))
    #(every? valid-content-node-map? (:nodes %))))

;; Page list (vector of pages)
(s/def ::page-list
  (s/coll-of ::page :kind vector?))

;;; ============================================================================
;;; TOC (Table of Contents) Detection Specs
;;; ============================================================================

;;; ============================================================================
;;; Document Specs (RLM output)
;;; ============================================================================

(def ^:private DOCUMENT_EXTENSIONS #{"pdf" "md" "txt" "docx" "html"})

;;; ============================================================================
;;; TOC Entry Specs (document.toc namespace)
;;; ============================================================================

(defn- valid-toc-entry?
  [m]
  (and (map? m)
    (= :toc-entry (:type m))
    (string? (:id m))
    (string? (:title m))
    (nat-int? (:target-page m))
    (string? (:level m))
    (or (not (contains? m :start-page-label)) (nat-int? (:start-page-label m)))
    (or (not (contains? m :end-page-label)) (nil? (:end-page-label m)) (nat-int? (:end-page-label m)))
    (or (not (contains? m :start-page-index)) (nat-int? (:start-page-index m)))
    (or (not (contains? m :end-page-index)) (nil? (:end-page-index m)) (nat-int? (:end-page-index m)))
    (or (not (contains? m :target-page-index)) (nat-int? (:target-page-index m)))
    (or (not (contains? m :parent-id)) (nil? (:parent-id m)) (string? (:parent-id m)))
    (or (not (contains? m :description)) (nil? (:description m)) (string? (:description m)))
    (or (not (contains? m :target-section-id)) (nil? (:target-section-id m)) (string? (:target-section-id m)))))

;; Complete TOC entry
(s/def ::toc-entry
  valid-toc-entry?)

;; Complete RLM document
(s/def ::document
  (s/and map?
    #(contains? % :name)
    #(contains? % :extension)
    #(contains? % :pages)
    #(contains? % :toc)
    #(and (string? (:name %)) (seq (:name %)))
    #(contains? DOCUMENT_EXTENSIONS (:extension %))
    #(vector? (:pages %))
    #(every? (fn [page] (s/valid? ::page page)) (:pages %))
    #(vector? (:toc %))
    #(every? valid-toc-entry? (:toc %))
    #(or (not (contains? % :title)) (nil? (:title %)) (string? (:title %)))
    #(or (not (contains? % :abstract)) (nil? (:abstract %)) (string? (:abstract %)))
    #(or (not (contains? % :created-at)) (inst? (:created-at %)))
    #(or (not (contains? % :updated-at)) (inst? (:updated-at %)))
    #(or (not (contains? % :author)) (nil? (:author %)) (string? (:author %)))))

;; Vector of documents
(s/def ::documents
  (s/coll-of ::document :kind vector?))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defn valid-document?
  "Returns true if document is valid according to ::document spec."
  [document]
  (s/valid? ::document document))

(defn valid-documents?
  "Returns true if documents vector is valid according to ::documents spec."
  [documents]
  (s/valid? ::documents documents))

(defn explain-document
  "Returns explanation of why document is invalid (or nil if valid)."
  [document]
  (when-not (valid-document? document)
    (s/explain-str ::document document)))

(defn explain-documents
  "Returns explanation of why documents vector is invalid (or nil if valid)."
  [documents]
  (when-not (valid-documents? documents)
    (s/explain-str ::documents documents)))
