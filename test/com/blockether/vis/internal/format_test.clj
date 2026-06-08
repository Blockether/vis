(ns com.blockether.vis.internal.format-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as fmt]
   [lazytest.core :refer [defdescribe expect it]]))

;; =============================================================================
;; strip-def-docstrings — channel-side rendering helper that removes the
;; mandatory docstring slot from `(def NAME "doc" …)` shapes so human
;; readers do not see the model-forced doc noise. Persisted source
;; (with docstring) stays intact; this is a render-time transform.
;; =============================================================================

(defn- normalize-ws
  "Drop trailing per-line whitespace + collapse runs of blank lines so
   docstring-stripped output can be compared with a stable expected
   string regardless of how zprint normalizes spacing."
  [s]
  (-> s
    (str/replace #"[ \t]+\n" "\n")
    (str/replace #"\n{2,}" "\n")
    str/trim))

(defdescribe format-tokens-test
  (it "omits zero cached input tokens"
    (expect (= "tok 100→20"
              (fmt/format-tokens {:input 100 :output 20 :cached 0})))
    (expect (= "tok 100→20"
              (fmt/format-tokens {:input 100 :output 20}))))

  (it "shows positive cached input tokens"
    (expect (= "tok 100→20 (cached 70)"
              (fmt/format-tokens {:input 100 :output 20 :cached 70})))
    (expect (= "tok 0→20 (cached 70)"
              (fmt/format-tokens {:output 20 :cached-input 70}))))

  (it "shows positive cache write tokens"
    (expect (= "tok 112→69 (cache-write 8777)"
              (fmt/format-tokens {:input 112 :output 69 :cache-created 8777})))
    (expect (= "tok 112→69 (cached 70, cache-write 8777)"
              (fmt/format-tokens {:input 112 :output 69 :cached 70 :cache-created 8777})))))

;; =============================================================================
;; Shared humanized turn-summary line — the SAME formatter the CLI bracket, TUI
;; bubble footer, and Telegram tagline all render. `meta-summary-line` is the
;; clean main line; `meta-fallback-note` is the routing note the TUI floats on a
;; second row and `format-meta-line` folds inline.
;; =============================================================================

(def ^:private normal-result
  {:llm-actual {:provider :openai :model "gpt-4o"}
   :tokens {:input 11461 :output 35 :cached 4096}
   :cost {:total-cost 0.006954} :duration-ms 4900 :iteration-count 3})

(def ^:private fallback-result
  {:llm-actual {:provider :openai :model "gpt-4o"}
   :llm-selected {:provider :blockether :model "glm-5.1"}
   :llm-fallback? true
   :llm-routing-trace [{:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-retry}
                       {:event/type :llm.routing/provider-fallback :status 429}]
   :tokens {:input 11461 :output 35 :cached 4096}
   :cost {:total-cost 0.006954} :duration-ms 4900})

(defdescribe meta-summary-line-test
  (it "humanizes tokens + cost, keeps cache, drops iterations"
    (expect (= "openai/gpt-4o  ·  11.5k→35 (cached 4.1k)  ·  ~$0.0070  ·  4.9s"
              (fmt/meta-summary-line normal-result))))

  (it "suppresses zero-usage / zero-cost slots — no \"0→0\", no \"$0\""
    (expect (= "openai/gpt-4o  ·  4.9s"
              (fmt/meta-summary-line {:provider :openai :model "gpt-4o"
                                      :tokens {:input 0 :output 0} :cost 0
                                      :duration-ms 4900}))))

  (it "keeps sub-cent costs honest (no round-to-zero)"
    (expect (= "glm-5.1  ·  500→12  ·  ~$0.000020  ·  800ms"
              (fmt/meta-summary-line {:model "glm-5.1" :tokens {:input 500 :output 12}
                                      :cost {:total-cost 0.00002} :duration-ms 800}))))

  (it "carries the model that ANSWERED, not the one that bailed"
    (expect (str/starts-with? (fmt/meta-summary-line fallback-result) "openai/gpt-4o"))))

(defdescribe meta-fallback-note-test
  (it "tells the routing story: from <selected> — <status>, retried N×"
    (expect (= "↳ from blockether/glm-5.1 — 429, retried 3×"
              (fmt/meta-fallback-note fallback-result))))

  (it "is nil when there was no fallback"
    (expect (nil? (fmt/meta-fallback-note normal-result)))))

(defdescribe format-meta-line-test
  (it "is identical to meta-summary-line when there's no fallback"
    (expect (= (fmt/meta-summary-line normal-result)
              (fmt/format-meta-line normal-result))))

  (it "folds the fallback note inline for single-line surfaces (CLI/Telegram)"
    (expect (= (str (fmt/meta-summary-line fallback-result)
                 fmt/meta-separator
                 (fmt/meta-fallback-note fallback-result))
              (fmt/format-meta-line fallback-result)))))

(defdescribe strip-def-docstrings-test
  (it "strips the docstring from a plain (def …) form"
    (expect (= "(def x 42)"
              (normalize-ws (fmt/strip-def-docstrings "(def x \"the answer\" 42)")))))

  (it "strips the docstring from (defn …) while preserving args + body"
    (expect (= "(defn add [a b] (+ a b))"
              (normalize-ws (fmt/strip-def-docstrings "(defn add \"adds\" [a b] (+ a b))")))))

  (it "handles every head in DEF_HEADS_TO_STRIP (bare + clojure.core/-qualified)"
    (doseq [[src expected]
            [["(def x \"d\" 1)"                    "(def x 1)"]
             ["(defn f \"d\" [x] x)"               "(defn f [x] x)"]
             ["(defn- pf \"d\" [x] x)"             "(defn- pf [x] x)"]
             ["(defonce o \"d\" (atom 0))"         "(defonce o (atom 0))"]
             ["(defmulti m \"d\" :kind)"           "(defmulti m :kind)"]
             ["(defmacro mc \"d\" [x] x)"          "(defmacro mc [x] x)"]
             ["(clojure.core/def cx \"d\" 1)"      "(clojure.core/def cx 1)"]
             ["(clojure.core/defn cf \"d\" [x] x)" "(clojure.core/defn cf [x] x)"]]]
      (expect (= expected (normalize-ws (fmt/strip-def-docstrings src))))))

  (it "leaves non-def forms untouched"
    (expect (= "(+ 1 2)"           (normalize-ws (fmt/strip-def-docstrings "(+ 1 2)"))))
    (expect (= "(let [a 1] a)"     (normalize-ws (fmt/strip-def-docstrings "(let [a 1] a)"))))
    (expect (= "[:ir [:p \"hi\"]]" (normalize-ws (fmt/strip-def-docstrings "[:ir [:p \"hi\"]]")))))

  (it "leaves a (def NAME val) shape WITHOUT a docstring slot alone"
    ;; The legacy regex stripped the first string literal even when it
    ;; was not at the docstring position. Edamame parsing keeps the
    ;; non-doc-shape forms exactly as written.
    (expect (= "(def x 42)" (normalize-ws (fmt/strip-def-docstrings "(def x 42)"))))
    (expect (= "(defn f [x] (str \"hello\" x))"
              (normalize-ws (fmt/strip-def-docstrings "(defn f [x] (str \"hello\" x))")))))

  (it "handles multi-form sources: strips each def, leaves non-defs alone"
    (let [src "(def a \"alpha\" 1)\n(defn f \"adds\" [x] (+ x a))\n(+ a 1)"
          out (fmt/strip-def-docstrings src)]
      (expect (str/includes? out "(def a 1)"))
      (expect (str/includes? out "(defn f [x] (+ x a))"))
      (expect (str/includes? out "(+ a 1)"))
      ;; None of the docstrings survive.
      (expect (not (str/includes? out "alpha")))
      (expect (not (str/includes? out "adds")))))

  (it "parse failures fall through with the original source"
    (let [bad "(unbalanced [ "]
      (expect (= bad (fmt/strip-def-docstrings bad)))))

  (it "non-string input passes through unchanged"
    (expect (nil? (fmt/strip-def-docstrings nil)))
    (expect (= 42 (fmt/strip-def-docstrings 42)))))
