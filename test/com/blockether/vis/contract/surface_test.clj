(ns com.blockether.vis.contract.surface-test
  "Contract tests for the JSON Schema-backed language-surface result shapes shared by
   format, lint, run_tests and check."
  (:require [com.blockether.vis.contract.surface :as contract]
            [lazytest.core :refer [defdescribe expect it]]))


(def ^:private lint-ok
  {"error" 0
   "warning" 1
   "info" 0
   "findings" [{"file" "a.clj"
                "row" 1
                "col" 22
                "level" "warning"
                "message" "unused binding b"
                "provider" "clj-kondo"}]
   "providers" ["clj-kondo" "general"]
   "language" "clojure"})

(def ^:private test-ok
  {"mode" "repl"
   "language" "clojure"
   "ns" "my.app.core-test"
   "framework" "clojure.test"
   "total" 3
   "pass" 3
   "fail" 0
   "failures" []
   "output" ""})

(defdescribe
  surface-test
  (it "check tags a schema rejection as a surface contract violation"
      (let [ed (try (contract/check :lint-fn (dissoc lint-ok "findings"))
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (expect (= :surface/contract-violation (:type ed)))
        (expect (= :lint-fn (:capability ed)))
        (expect (some? (:explain-data ed)))))
  (it "passes a capability with no registered contract straight through"
      (expect (= :untouched (contract/check :repl-eval-fn :untouched))))
  (it "capability->definition is the single source for format, lint, and test results"
      (expect (= #{:format-fn :lint-fn :test-fn} (set (keys contract/capability->definition)))))
  (it "completes an error-branch test result onto the TOTAL key set"
      ;; The reported crash: a run that errored out returned NO "failures" key, so
      ;; `r["failures"][:3]` in ordinary model Python blew up on None.
      (let [r (contract/complete-test-result
                "clojure"
                {"mode" "repl" "ns" "my.app.core-test" "port" 7888 "error" "nREPL is down"})]
        (expect (every? #(contains? r %) (keys contract/test-result-base)))
        (expect (= [] (get r "failures")))
        (expect (nil? (get r "total")))
        (expect (false? (get r "is_pass")))
        (expect (false? (get r "timed_out")))))
  (it "never overwrites what the pack measured"
      (let [r (contract/complete-test-result "clojure" test-ok)]
        (expect (= 3 (get r "total")))
        (expect (= 3 (get r "pass")))
        (expect (= 0 (get r "fail")))
        (expect (= "clojure" (get r "language")))
        (expect (true? (get r "is_pass")))))
  (it "completes a pack that speaks the contract's own words, translating nothing"
      (let [r (contract/complete-test-result "python"
                                             {"mode" "cli"
                                              "runner" "project"
                                              "command" "pytest tests"
                                              "exit" 1
                                              "pass" 7
                                              "fail" 3
                                              "errored" 1
                                              "skipped" 3})]
        (expect (= 7 (get r "pass")))
        (expect (= 3 (get r "fail")))
        (expect (= 1 (get r "errored"))) ; the erroring SUBSET of fail
        (expect (= 13 (get r "total"))) ; derived: pass + fail + skipped
        (expect (= "pytest tests" (get r "command")))
        (expect (= "python" (get r "language")))
        (expect (false? (get r "is_pass")))))
  (it "reads no per-pack alias vocabulary — a pack's own words never count here"
      ;; The removed translation layer: `passed`/`failed`/`ok`/`cmd` used to be
      ;; folded onto the canonical names HERE, so the same fact reached a result
      ;; under two spellings and each pack's arithmetic was guessed at from
      ;; outside. Every pack now emits `pass`/`fail`/`errored`/`command` itself.
      (let [r (contract/complete-test-result
                "python"
                {"mode" "cli" "passed" 7 "failed" 2 "ok" true "cmd" ["pytest" "tests"]})]
        (expect (nil? (get r "pass")))
        (expect (nil? (get r "fail")))
        (expect (nil? (get r "total")))
        (expect (nil? (get r "command")))
        (expect (nil? (get r "is_pass")))))
  (it "keeps errored as the erroring SUBSET of fail — never a count to add on top"
      ;; Reported by the pack: fail already contains it, so total stays
      ;; pass + fail + skipped and errored is never added a second time.
      (let [r (contract/complete-test-result
                "clojure"
                {"mode" "repl" "pass" 7 "fail" 3 "errored" 2 "skipped" 1})]
        (expect (= 3 (get r "fail")))
        (expect (= 2 (get r "errored")))
        (expect (= 11 (get r "total"))))
      ;; NOT reported, but every failure is in the typed fault list — counted.
      (let [r (contract/complete-test-result "clojure"
                                             {"mode" "repl"
                                              "fail" 2
                                              "failures" [{"test" "threw" "type" "error"}
                                                          {"test" "asserted" "type" "fail"}]})]
        (expect (= 1 (get r "errored"))))
      ;; Counts with no per-test detail (pytest's summary line): UNKNOWN, not 0.
      (let [r (contract/complete-test-result "python" {"mode" "cli" "fail" 3})]
        (expect (nil? (get r "errored"))))
      ;; Nothing failed, so nothing threw.
      (let [r (contract/complete-test-result "python" {"mode" "cli" "pass" 4 "fail" 0})]
        (expect (= 0 (get r "errored")))))
  (it "derives is_pass from the exit status when the pack reports no counts"
      (expect (true? (get (contract/complete-test-result "typescript" {"exit" 0}) "is_pass")))
      (expect (false? (get (contract/complete-test-result "typescript" {"exit" 2}) "is_pass")))
      (expect (nil? (get (contract/complete-test-result "typescript" {}) "is_pass"))))
  (it "stamps the language that actually ran, and passes a non-map through"
      (expect (= "typescript" (get (contract/complete-test-result "typescript" {}) "language")))
      (expect (= "clojure"
                 (get (contract/complete-test-result "typescript" {"language" "clojure"})
                      "language")))
      (expect (= :untouched (contract/complete-test-result "clojure" :untouched)))))
