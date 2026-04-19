(ns com.blockether.vis.loop.runtime.prompt-compact-test
  "Structural + size regression tests for the rendered system prompt.

   These don't call an LLM. They assert that the prompt:
     1. carries every JSON-schema enum value the loop relies on
     2. mentions MUSTACHE semantics once (not three times)
     3. only includes STEERING when the env actually has more than one
        model to switch between
     4. stays under the token budget we committed to in the compact-prompt
        refactor

   The point is to make the prompt SHAPE a contract — any future edit that
   quietly reintroduces duplication or drops an enum fails loudly here
   before it ever hits a paying provider."
  (:require
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe it expect]]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.core :as core]
   [com.blockether.vis.loop.runtime.prompt :as prompt]
   [com.blockether.vis.loop.runtime.query.routing :as routing])
  (:import
   [com.knuddels.jtokkit Encodings]
   [com.knuddels.jtokkit.api EncodingType]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- router-with-models [model-names]
  (llm/make-router
    [{:id :stub
      :api-key "stub"
      :base-url "http://localhost:1"
      :models (mapv (fn [n] {:name n}) model-names)}]))

(defn- env-with-router [router]
  (core/create-env router {:db :temp}))

(defn- render [env]
  (let [has-reasoning? (boolean (routing/provider-has-reasoning? (:router env)))]
    (prompt/build-system-prompt
      {:env env
       :has-reasoning? has-reasoning?
       :has-documents? false
       :tool-defs (when-let [a (:tool-registry-atom env)] (vals @a))
       :skill-registry (when-let [a (:skill-registry-atom env)] @a)})))

(defn- with-env [model-names f]
  (let [env (env-with-router (router-with-models model-names))]
    (try (f env) (finally (core/dispose-env! env)))))

;; =============================================================================
;; Token counting (jtokkit, same encoding as the live provider path)
;; =============================================================================

(def ^:private registry (Encodings/newDefaultEncodingRegistry))
(def ^:private enc-o200k (.getEncoding registry EncodingType/O200K_BASE))

(defn- tokens [s] (.countTokens enc-o200k s))

;; =============================================================================
;; A. JSON schema enum values survive the compact renderer
;; =============================================================================

(defdescribe schema-completeness-test
  (it "carries every :answer-type enum value"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "mustache-text"))
          (expect (str/includes? p "mustache-markdown"))))))

  (it "carries every :confidence enum value"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "high"))
          (expect (str/includes? p "medium"))
          (expect (str/includes? p "low"))))))

  (it "carries every :next.model enum value"
    (with-env ["gpt-5-mini" "gpt-4o"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "cost"))
          (expect (str/includes? p "speed"))
          (expect (str/includes? p "intelligence"))))))

  (it "carries every :next.reasoning enum value"
    (with-env ["gpt-5-mini" "gpt-4o"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "quick"))
          (expect (str/includes? p "balanced"))
          (expect (str/includes? p "deep"))))))

  (it "names every top-level JSON key the loop parses"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          ;; These are the fields in ITERATION_SPEC_* — if any disappears
          ;; from the schema block, downstream parsing silently fails.
          (doseq [k ["code" "answer" "answer-type" "confidence"
                     "next" "forget"]]
            (expect (str/includes? p k))))))))

;; =============================================================================
;; B. Deduplication — MUSTACHE, steering, output-style
;; =============================================================================

(defn- count-substr [s needle]
  (count (re-seq (re-pattern (java.util.regex.Pattern/quote needle)) s)))

(defdescribe deduplication-test
  (it "mentions `mustache-text` at most 4 times total"
    ;; Legitimate 4 today: (1) ARCH MUSTACHE source-of-truth block,
    ;; (2) ARCH inline example `:answer-type mustache-text.`,
    ;; (3) JSON-schema enum-union `\"mustache-text\" or \"mustache-markdown\"`,
    ;; (4) RESPONSE-FORMAT footer `answer-type: mustache-text|mustache-markdown`.
    ;; Any more = we regressed the dedup done in svar 0.3.2 + ARCH merge.
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)
              hits (count-substr p "mustache-text")]
          (expect (<= hits 4))))))

  (it "has no duplicate OUTPUT style blocks"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)
              ;; 'No AI filler' is unique to the style guide and was
              ;; previously emitted twice (CAVEMAN + FINAL_ANSWER).
              style-hits (count-substr p "No AI filler")]
          (expect (= 1 style-hits))))))

  (it "emits only inline enum unions for obvious enums (no per-value comments)"
    ;; Thanks to svar 0.3.2's values-only vector shorthand, the JSON
    ;; schema for :confidence / :answer-type / :next.model / :next.reasoning
    ;; should no longer carry per-value `//   - \"high\": ...` comment lines.
    (with-env ["gpt-5-mini" "gpt-4o"]
      (fn [env]
        (let [p (render env)]
          (expect (not (str/includes? p "//   - \"high\":")))
          (expect (not (str/includes? p "//   - \"mustache-text\":")))
          (expect (not (str/includes? p "//   - \"cost\":"))))))))

;; =============================================================================
;; C. Activation-gated sections
;; =============================================================================

(defdescribe activation-gating-test
  (it "omits STEERING when the env has a single model"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          (expect (not (str/includes? p "STEERING")))
          ;; :next keys still documented in the schema — just not the essay
          (expect (str/includes? p "next"))))))

  (it "includes STEERING when the env has 2+ models to switch between"
    (with-env ["gpt-5-mini" "gpt-4o" "claude-sonnet-4-6"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "STEERING"))
          (expect (str/includes? p ":next.model")))))))

(defdescribe journal-is-scratchpad-test
  ;; When-to-def guidance: ARCH holds the full explanation, RULES has a
  ;; one-line reminder. Both must mention that <journal> already shows
  ;; the last iteration, otherwise the agent defaults to over-defing.
  (it "ARCH tells the agent <journal> already shows the last iteration"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "DEF ONLY FOR CROSS-ITERATION STATE"))
          (expect (str/includes? p "journal is your scratchpad"))))))

  (it "RULES reiterates the def policy in one line"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [p (render env)]
          (expect (str/includes? p "def only cross-iter state"))
          (expect (str/includes? p "journal already shows last iter in full")))))))

;; =============================================================================
;; D. Token budget
;; =============================================================================

;; Regression budgets pinned to the values observed after the v0.3.2 svar
;; enum shorthand + prompt dedup + ARCH trim. Bumped up from the previous
;; 4306-token baseline (stub env, no tools, no docs).
(defdescribe token-budget-test
  (it "single-model stub env ≤ 3700 tokens"
    (with-env ["gpt-5-mini"]
      (fn [env]
        (let [t (tokens (render env))]
          (expect (< t 3700))))))

  (it "multi-model stub env ≤ 3900 tokens (incl. STEERING block)"
    (with-env ["gpt-5-mini" "gpt-4o" "claude-sonnet-4-6"]
      (fn [env]
        (let [t (tokens (render env))]
          (expect (< t 3900)))))))
