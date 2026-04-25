(ns com.blockether.vis.persistance.spec
  (:require
   [charred.api :as json]
   [clojure.java.process :as proc]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]

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

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in SCI sandbox."
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
  "Dynamic timeout in ms for SCI code eval. Bound per vis! call via
   :eval-timeout-ms opt. Nested queries inherit outer binding. Clamped at
   the rlm.clj API boundary to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  DEFAULT_EVAL_TIMEOUT_MS)

;; =============================================================================
;; Concurrency Settings
;; =============================================================================

(def DEFAULT_CONCURRENCY
  "Default concurrency settings. Applied when :concurrency opt is absent from vis!."
  {:max-parallel-llm   8
   :http-timeout-ms    20000})

(def ^:dynamic *concurrency*
  "Merged concurrency settings for the current vis! process."
  DEFAULT_CONCURRENCY)

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
                 ::spec/description "One valid Clojure S-expression to execute in SCI (tool calls must be parenthesized)."})
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
     :include-thinking? — true for non-reasoning providers (CoT goes
       in JSON), false for reasoning providers (CoT is native, no
       duplication).

   The iteration loop ALWAYS sends only the previous iteration's
   `:thinking` under `<prior_thinking>`. There is no spec knob to
   request more — older reasonings live in `(var-history '*reasoning*)`
   and the agent reaches them on demand from `:code`. This keeps the
   per-iteration prompt O(1) and forces the agent to be deliberate
   about what historical context it actually needs."
  [{:keys [include-thinking?]}]
  (let [base-fields
          [(spec/field {::spec/name        :code
                        ::spec/type        :spec.type/ref
                        ::spec/target      :code_block
                        ::spec/cardinality :spec.cardinality/many
                        ::spec/required    true
                        ::spec/description "Required code blocks to execute this iteration; use `[{\"expr\":\":ok\",\"time-ms\":1}]` when no computation is needed."})
           (spec/field {::spec/name        :next
                        ::spec/type        :spec.type/ref
                        ::spec/target      :next_turn
                        ::spec/cardinality :spec.cardinality/one
                        ::spec/required    false
                        ::spec/description "Optional steering for the next iteration."})
           (spec/field {::spec/name        :answer
                        ::spec/type        :spec.type/string
                        ::spec/cardinality :spec.cardinality/one
                        ::spec/required    false
                        ::spec/description "Optional final answer for this iteration. Never use emoji/emoticons."})
           ;; Values-only enum (svar 0.3.2+). Mustache semantics are
           ;; documented once in the ARCH section; no need to re-paste
           ;; them per iteration into the JSON schema.
           (spec/field {::spec/name        :answer-type
                        ::spec/type        :spec.type/keyword
                        ::spec/cardinality :spec.cardinality/one
                        ::spec/required    false
                        ::spec/description "Required with :answer; controls final answer rendering."
                        ::spec/values      ["mustache-text" "mustache-markdown"]})
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
    (apply spec/spec {:refs [CODE_BLOCK_SPEC NEXT_SPEC]} fields)))

(defn iteration-spec
  "Compose the iteration response spec for the CURRENT env state.
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

;; SUB_RLM_QUERY_SPEC was removed — sub-RLMs now run through the same
;; iteration-loop as the main RLM, producing a full trace (thinking +
;; code + final + vars + persistence) instead of a flat {:content :code}

(defn bytes->base64
  "Converts raw bytes to a base64 string.
   
   Params:
   `bs` - byte[]. Raw bytes.
   
   Returns:
   String. Base64-encoded representation."
  [^bytes bs]
  (.encodeToString (Base64/getEncoder) bs))

  "Dynamic var for max recursion depth. Bound per vis! call."

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
  "Spec for vis-qa! Q&A generation output."
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
