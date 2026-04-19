(ns com.blockether.vis.loop.runtime.tool-diagnostics-test
  "Tests for the tool-diagnostics telemetry module.

   Covers the three hot-path writers (`record-activation-check!`,
   `record-activation-error!`, `record-execution!`), the derived
   `stats-for` view, and the two rendering entry points (`summary-line`,
   `format-doctor-report`).

   The module is process-global so every `it` resets the atom up front via
   `reset-diagnostics!`. `before` runs once per describe in lazytest, not
   per test, so inline resets are the reliable way to isolate cases."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.tool-diagnostics :as tool-diag]))

(defn- reset!! []
  (tool-diag/reset-diagnostics!))

;; =============================================================================
;; Writers
;; =============================================================================

(defdescribe activation-check-recording
  (describe "record-activation-check!"
    (it "creates a zeroed record on first touch"
      (reset!!)
      (tool-diag/record-activation-check! 'read-file true 1000)
      (let [d (get (tool-diag/get-diagnostics) 'read-file)]
        (expect (= 1 (:activation-checks d)))
        (expect (= 1 (:activation-active d)))
        (expect (= 0 (:activation-errors d)))
        (expect (= 0 (:executions d)))
        (expect (true? (:last-active? d)))
        (expect (= 1000 (:last-activation-ns d)))
        (expect (= 1000 (:total-activation-ns d)))
        (expect (= 1000 (:max-activation-ns d)))))

    (it "accumulates totals and tracks max across calls"
      (reset!!)
      (tool-diag/record-activation-check! 'read-file true 100)
      (tool-diag/record-activation-check! 'read-file false 500)
      (tool-diag/record-activation-check! 'read-file true 300)
      (let [d (get (tool-diag/get-diagnostics) 'read-file)]
        (expect (= 3 (:activation-checks d)))
        (expect (= 2 (:activation-active d)))
        (expect (= 900 (:total-activation-ns d)))
        (expect (= 500 (:max-activation-ns d)))
        (expect (true? (:last-active? d)))
        (expect (= 300 (:last-activation-ns d)))))

    (it "coerces truthy non-boolean values to true"
      (reset!!)
      (tool-diag/record-activation-check! 'read-file "yes" 100)
      (let [d (get (tool-diag/get-diagnostics) 'read-file)]
        (expect (true? (:last-active? d)))
        (expect (= 1 (:activation-active d)))))))

(defdescribe activation-error-recording
  (describe "record-activation-error!"
    (it "bumps :activation-checks AND :activation-errors so the ratio stays meaningful"
      (reset!!)
      (tool-diag/record-activation-error!
        'flaky-tool (RuntimeException. "nope") 2000)
      (let [d (get (tool-diag/get-diagnostics) 'flaky-tool)]
        (expect (= 1 (:activation-checks d)))
        (expect (= 1 (:activation-errors d)))
        (expect (= 0 (:activation-active d)))
        (expect (false? (:last-active? d)))
        (expect (= "nope" (get-in d [:last-activation-error :message])))
        (expect (str/ends-with? (get-in d [:last-activation-error :class]) "RuntimeException"))))

    (it "interleaves cleanly with successful checks"
      (reset!!)
      (tool-diag/record-activation-check! 'flaky-tool true 100)
      (tool-diag/record-activation-error! 'flaky-tool (ex-info "bad" {}) 200)
      (tool-diag/record-activation-check! 'flaky-tool true 300)
      (let [d (get (tool-diag/get-diagnostics) 'flaky-tool)]
        (expect (= 3 (:activation-checks d)))
        (expect (= 2 (:activation-active d)))
        (expect (= 1 (:activation-errors d)))
        (expect (= 600 (:total-activation-ns d)))
        (expect (= 300 (:max-activation-ns d)))
        ;; Most recent successful check cleared :last-activation-error
        (expect (nil? (:last-activation-error d)))))))

(defdescribe execution-recording
  (describe "record-execution!"
    (it "counts successful executions into :total-execution-ns"
      (reset!!)
      (tool-diag/record-execution! 'read-file 1000 false)
      (tool-diag/record-execution! 'read-file 3000 false)
      (let [d (get (tool-diag/get-diagnostics) 'read-file)]
        (expect (= 2 (:executions d)))
        (expect (= 0 (:execution-errors d)))
        (expect (= 4000 (:total-execution-ns d)))
        (expect (= 3000 (:max-execution-ns d)))
        (expect (= 3000 (:last-execution-ns d)))))

    (it "excludes errored executions from :total-execution-ns"
      (reset!!)
      (tool-diag/record-execution! 'read-file 1000 false)
      (tool-diag/record-execution! 'read-file 9000 true)
      (let [d (get (tool-diag/get-diagnostics) 'read-file)]
        (expect (= 2 (:executions d)))
        (expect (= 1 (:execution-errors d)))
        ;; Only the non-error run contributes to the total
        (expect (= 1000 (:total-execution-ns d)))
        ;; But max tracks every run so outliers stay visible
        (expect (= 9000 (:max-execution-ns d)))))))

;; =============================================================================
;; Derived stats
;; =============================================================================

(defdescribe derived-stats
  (describe "stats-for"
    (it "returns nil for an unseen tool"
      (reset!!)
      (expect (nil? (tool-diag/stats-for 'never-called))))

    (it "computes mean activation and execution in nanoseconds"
      (reset!!)
      (tool-diag/record-activation-check! 'read-file true 100)
      (tool-diag/record-activation-check! 'read-file true 300)
      (tool-diag/record-execution! 'read-file 1000 false)
      (tool-diag/record-execution! 'read-file 3000 false)
      (let [s (tool-diag/stats-for 'read-file)]
        (expect (= 200.0 (:mean-activation-ns s)))
        (expect (= 2000.0 (:mean-execution-ns s)))))

    (it "mean-execution excludes errored runs from the denominator"
      (reset!!)
      (tool-diag/record-execution! 'read-file 1000 false)
      (tool-diag/record-execution! 'read-file 9999 true)
      (let [s (tool-diag/stats-for 'read-file)]
        ;; Successful total = 1000 over 1 successful run
        (expect (= 1000.0 (:mean-execution-ns s)))))))

;; =============================================================================
;; Lifecycle helpers
;; =============================================================================

(defdescribe lifecycle
  (describe "reset-diagnostics! and clear-tool!"
    (it "reset-diagnostics! wipes every tool"
      (reset!!)
      (tool-diag/record-execution! 'a 1 false)
      (tool-diag/record-execution! 'b 1 false)
      (tool-diag/reset-diagnostics!)
      (expect (= {} (tool-diag/get-diagnostics))))

    (it "clear-tool! drops one tool and leaves the rest"
      (reset!!)
      (tool-diag/record-execution! 'keep-me 1 false)
      (tool-diag/record-execution! 'drop-me 1 false)
      (tool-diag/clear-tool! 'drop-me)
      (let [d (tool-diag/get-diagnostics)]
        (expect (contains? d 'keep-me))
        (expect (not (contains? d 'drop-me)))))

    (it "clear-tool! on an unknown symbol is a silent no-op"
      (reset!!)
      (tool-diag/record-execution! 'keep-me 1 false)
      (tool-diag/clear-tool! 'never-existed)
      (expect (contains? (tool-diag/get-diagnostics) 'keep-me)))))

;; =============================================================================
;; Rendering
;; =============================================================================

(defn- sample-tool
  "Build the kind of tool descriptor the CLI's activation pass produces.
   Keeping this local avoids depending on the real registry at test time."
  [{:keys [sym group active? ms doc]
    :or   {group "filesystem" active? true ms 0.5}}]
  {:sym            (str sym)
   :group          group
   :active?        active?
   :activation-ms  ms
   :activation-doc doc})

(defdescribe summary-line-rendering
  (describe "summary-line"
    (it "reports counts with singular error wording"
      (reset!!)
      (tool-diag/record-execution! "read-file" 1000 true)
      (let [line (tool-diag/summary-line [(sample-tool {:sym 'read-file :active? true})])]
        (expect (str/includes? line "1 tools registered"))
        (expect (str/includes? line "1 active, 0 inactive"))
        (expect (str/includes? line "1 execution"))
        (expect (str/includes? line "(1 error)"))))

    (it "uses plural error wording for >1 errors"
      (reset!!)
      (tool-diag/record-execution! "a" 1 true)
      (tool-diag/record-execution! "a" 1 true)
      (let [line (tool-diag/summary-line [(sample-tool {:sym 'a :active? true})])]
        (expect (str/includes? line "2 executions"))
        (expect (str/includes? line "(2 errors)"))))

    (it "omits the executions clause entirely when nothing has run"
      (reset!!)
      (let [line (tool-diag/summary-line
                   [(sample-tool {:sym 'a :active? true})
                    (sample-tool {:sym 'b :active? false})])]
        (expect (str/includes? line "2 tools registered"))
        (expect (str/includes? line "1 active, 1 inactive"))
        (expect (not (str/includes? line "execution")))))))

(defdescribe doctor-report-rendering
  (describe "format-doctor-report"
    (it "groups tools alphabetically by :group"
      (reset!!)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'git-blame :group "git"})
                   (sample-tool {:sym 'read-file :group "filesystem"})
                   (sample-tool {:sym 'var-history :group "conversation"})])
            conv-idx (.indexOf out "conversation")
            fs-idx   (.indexOf out "filesystem")
            git-idx  (.indexOf out "git")]
        (expect (pos? conv-idx))
        (expect (< conv-idx fs-idx git-idx))))

    (it "renders a checkmark for active tools and a cross for inactive"
      (reset!!)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'on :active? true})
                   (sample-tool {:sym 'off :active? false
                                 :doc "missing API key"})])]
        (expect (str/includes? out "\u2713 on"))
        (expect (str/includes? out "\u2717 off"))
        (expect (str/includes? out "missing API key"))))

    (it "falls back to the default reason when :activation-doc is nil"
      (reset!!)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'off :active? false :doc nil})])]
        (expect (str/includes? out "activation-fn returned false"))))

    (it "folds in execution stats from the diagnostics atom"
      (reset!!)
      (tool-diag/record-execution! "read-file" 1000000 false)
      (tool-diag/record-execution! "read-file" 3000000 false)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'read-file :active? true})])]
        (expect (str/includes? out "runs=2"))
        (expect (str/includes? out "mean="))
        (expect (str/includes? out "max="))))

    (it "surfaces activation errors inline"
      (reset!!)
      (tool-diag/record-activation-error!
        "flaky-tool" (RuntimeException. "boom") 500)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'flaky-tool :active? false :doc "threw"})])]
        (expect (str/includes? out "activation error"))
        (expect (str/includes? out "boom"))))

    (it "omits the exec line when a tool has never run"
      (reset!!)
      (let [out (tool-diag/format-doctor-report
                  [(sample-tool {:sym 'untouched :active? true})])]
        (expect (not (str/includes? out "runs=")))))

    (it "assigns 'Other' as the group when :group is missing"
      (reset!!)
      (let [out (tool-diag/format-doctor-report
                  [{:sym "homeless" :active? true :activation-ms 0.1}])]
        (expect (str/includes? out "Other"))))))
