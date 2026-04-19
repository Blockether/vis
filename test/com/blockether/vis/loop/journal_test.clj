(ns com.blockether.vis.loop.journal-test
  "Regression tests for the compact `<journal>` renderer
   (`format-execution-results` in `loop.core`).

   The journal is the single rolling ledger the LLM sees on every
   iteration. Its format drifted into a verbose EDN-map shape with
   `:success?` / `:result-type` / `:value-size` / `:time-ms 0` fields
   that all duplicated signal the Clojure value itself already carries.
   This suite pins the minimal replacement format so future edits can't
   quietly reintroduce bloat.

   Cross-reference by `[N]` and `:stdout` / `:stderr` / `:truncated?`
   suffixes are structural and stay."
  (:require
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe it expect]]
   [com.blockether.vis.loop.core :as rlm-core]))

(defn- render [executions]
  (#'rlm-core/format-execution-results executions 1))

;; =============================================================================
;; A. Structural envelope
;; =============================================================================

(defdescribe journal-envelope-test
  (it "wraps entries in a bare <journal> / </journal> with no attributes"
    (let [out (render [{:code "(+ 1 2)" :result 3 :error nil}])]
      (expect (str/starts-with? out "<journal>\n"))
      (expect (str/ends-with? out "\n</journal>"))
      (expect (not (str/includes? out "iteration=")))))

  (it "returns nil when there are no executions"
    (expect (nil? (render []))))

  (it "indents entries with two spaces and prefixes each with [N]"
    (let [out (render [{:code "(a)" :result 1 :error nil}
                       {:code "(b)" :result 2 :error nil}])]
      (expect (str/includes? out "  [1] (a) → 1"))
      (expect (str/includes? out "  [2] (b) → 2")))))

;; =============================================================================
;; B. Value rendering — pr-str, no type/size/success boilerplate
;; =============================================================================

(defdescribe journal-value-test
  (it "renders primitive results as pr-str with no wrapper"
    (let [out (render [{:code "(+ 1 2)" :result 3 :error nil}])]
      (expect (str/includes? out "(+ 1 2) → 3"))
      (expect (not (str/includes? out ":success?")))
      (expect (not (str/includes? out ":result-type")))
      (expect (not (str/includes? out ":value-type")))
      (expect (not (str/includes? out ":value-size")))))

  (it "renders collections as pr-str (LLM reads type from syntax)"
    (let [out (render [{:code "[1 2 3]" :result [1 2 3] :error nil}])]
      (expect (str/includes? out "→ [1 2 3]"))
      (expect (not (str/includes? out ":result-type :vector")))
      (expect (not (str/includes? out ":value-size")))))

  (it "renders nil as nil (not null, not omitted)"
    (let [out (render [{:code "(do)" :result nil :error nil}])]
      (expect (str/includes? out "(do) → nil")))))

;; =============================================================================
;; C. (def …) renders with earmuffed *name* = value notation
;; =============================================================================

(defdescribe journal-var-test
  (it "renders (def foo val) as *foo* = value"
    ;; Build a real Var so the rendering path for clojure.lang.Var hits.
    (let [v (intern (create-ns 'journal-test.demo) 'foo 42)
          out (render [{:code "(def foo 42)" :result v :error nil}])]
      (expect (str/includes? out "(def foo 42) → *foo* = 42"))
      (expect (not (str/includes? out ":result-kind")))
      (expect (not (str/includes? out ":var-name"))))))

;; =============================================================================
;; D. Errors marked with ERROR: prefix, no :success? flag
;; =============================================================================

(defdescribe journal-error-test
  (it "marks errors with ERROR: prefix and the raw message"
    (let [out (render [{:code "(bad-fn)" :result nil
                        :error "Unable to resolve symbol: bad-fn"}])]
      (expect (str/includes? out "(bad-fn) → ERROR: Unable to resolve symbol: bad-fn"))
      (expect (not (str/includes? out ":success? false")))))

  (it "marks function-as-value as an ERROR"
    (let [out (render [{:code "some-fn" :result (fn []) :error nil}])]
      (expect (str/includes? out "ERROR:"))
      (expect (str/includes? out "function, not a value")))))

;; =============================================================================
;; E. Time — only flagged when SLOW, never as noise
;; =============================================================================

(defdescribe journal-time-test
  (it "omits time entirely for fast executions"
    (let [out (render [{:code "(+ 1 1)" :result 2 :execution-time-ms 3 :error nil}])]
      (expect (not (str/includes? out ":time-ms")))
      (expect (not (str/includes? out "SLOW")))
      (expect (not (str/includes? out ":perf-warning")))))

  (it "adds (Xms SLOW) suffix when execution exceeds SLOW_EXECUTION_MS"
    (let [out (render [{:code "(scan)" :result :done
                        :execution-time-ms 9999 :error nil}])]
      (expect (str/includes? out "SLOW"))
      (expect (str/includes? out "9999ms"))
      ;; The long "SLOW — optimize algorithm, avoid brute-force…" essay
      ;; lives in the ARCH section of the system prompt, not here.
      (expect (not (str/includes? out ":perf-warning")))
      (expect (not (str/includes? out "optimize algorithm"))))))

;; =============================================================================
;; F. stdout / stderr only appear when non-blank
;; =============================================================================

(defdescribe journal-streams-test
  (it "appends :stdout only when non-blank"
    (let [quiet (render [{:code "(do)" :result nil :stdout "" :error nil}])
          loud  (render [{:code "(println \"hi\")" :result nil :stdout "hi\n" :error nil}])]
      (expect (not (str/includes? quiet ":stdout")))
      (expect (str/includes? loud ":stdout \"hi\\n\""))))

  (it "appends :stderr only when non-blank"
    (let [clean (render [{:code "(do)" :result nil :stderr "" :error nil}])
          dirty (render [{:code "(warn)" :result nil :stderr "WARNING\n" :error nil}])]
      (expect (not (str/includes? clean ":stderr")))
      (expect (str/includes? dirty ":stderr")))))

;; =============================================================================
;; G. Truncation flag on over-cap values
;; =============================================================================

(defdescribe journal-truncation-test
  (it "flags :truncated? true when the pr-str exceeds the safety cap"
    ;; Make a string guaranteed over EXECUTION_SAFETY_CAP_CHARS (200k).
    (let [huge (apply str (repeat 300000 "a"))
          out (render [{:code "huge" :result huge :error nil}])]
      (expect (str/includes? out ":truncated? true"))))

  (it "does NOT flag :truncated? for small values"
    (let [out (render [{:code "(+ 1 2)" :result 3 :error nil}])]
      (expect (not (str/includes? out ":truncated?"))))))
