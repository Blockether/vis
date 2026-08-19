(ns com.blockether.vis.internal.foundation.surface-contract-test
  "Contract tests for the clojure.spec language-surface result specs: the shape
   format + lint + run_tests share, and `check`'s accept/reject/pass-through
   behaviour."
  (:require [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private format-ok
  {"op" "clj-format"
   "files" [{"path" "a.clj" "changed" true "wrote" true "formatter" "zprint"}
            {"path" "sub/b.clj" "changed" false "wrote" false}]
   "changed" 1
   "formatters" ["zprint"]})

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
  surface-contract-test
  (it "accepts a conforming format result and returns it unchanged"
      (expect (contract/valid? :format-fn format-ok))
      (expect (= format-ok (contract/check :format-fn format-ok)))
      (expect (nil? (contract/explain :format-fn format-ok))))
  (it "accepts a conforming lint result and returns it unchanged"
      (expect (contract/valid? :lint-fn lint-ok))
      (expect (= lint-ok (contract/check :lint-fn lint-ok))))
  (it "accepts a minimal single-file format result (no files)"
      (expect (contract/valid? :format-fn {"op" "clj-format" "changed" true "chars" -3})))
  (it "accepts a single-file format result naming the formatter that ran"
      (expect (contract/valid? :format-fn
                               {"op" "clj-format" "changed" true "chars" -3 "formatter" "zprint"})))
  (it "rejects a format result whose formatters set is not strings"
      (expect (not (contract/valid? :format-fn (assoc format-ok "formatters" [1 2])))))
  (it "rejects a format result missing the op key"
      (expect (not (contract/valid? :format-fn (dissoc format-ok "op")))))
  (it "rejects a lint result whose findings lack level/message"
      (expect (not (contract/valid? :lint-fn (assoc lint-ok "findings" [{"file" "a.clj"}]))))
      (expect (not (contract/valid? :lint-fn (dissoc lint-ok "findings")))))
  (it "check throws a tagged contract-violation ex-info on a bad result"
      (let [ed (try (contract/check :lint-fn (dissoc lint-ok "findings"))
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (expect (= :surface/contract-violation (:type ed)))
        (expect (= :lint-fn (:capability ed)))
        (expect (some? (:explain-data ed)))))
  (it "explain yields a string for a non-conforming result"
      (expect (string? (contract/explain :lint-fn (dissoc lint-ok "findings")))))
  (it "accepts a conforming test result and returns it unchanged"
      (expect (contract/valid? :test-fn test-ok))
      (expect (= test-ok (contract/check :test-fn test-ok))))
  (it "accepts a minimal cli test result"
      (expect (contract/valid?
                :test-fn
                {"mode" "cli" "language" "clojure" "ns" "" "exit" 0 "is_pass" true})))
  (it "rejects a test result whose mode is not repl/cli"
      (expect (not (contract/valid? :test-fn (assoc test-ok "mode" "wat"))))
      (expect (not (contract/valid? :test-fn (dissoc test-ok "mode")))))
  (it "rejects a test result whose pass count is not a number"
      (expect (not (contract/valid? :test-fn (assoc test-ok "pass" "3")))))
  (it "accepts a test failure carrying typed ns/test/file/line (parity with ::finding)"
      (expect (contract/valid? :test-fn
                               (assoc test-ok
                                 "fail" 1
                                 "failures" [{"ns" "my.app.core-test"
                                              "test" "adds-test"
                                              "type" "fail"
                                              "file" "core_test.clj"
                                              "line" 12
                                              "message" "expected 3"}]))))
  (it "types a fault as fail or error INSIDE failures — there is no errors list"
      (expect (contract/valid? :test-fn
                               (assoc test-ok
                                 "fail" 1
                                 "failures" [{"test" "boom" "type" "error" "message" "threw"}])))
      (expect (not (contract/valid? :test-fn
                                    (assoc test-ok
                                      "fail" 1
                                      "failures" [{"test" "boom" "type" "exploded"}]))))
      (expect (not (contains? contract/test-result-base "errors")))
      ;; ONE list of faults, but the erroring TALLY is its own count: a runner
      ;; that reports counts and no per-test detail types nothing.
      (expect (contains? contract/test-result-base "errored")))
  (it "rejects a test failure whose line is not a non-negative int"
      (expect (not (contract/valid? :test-fn
                                    (assoc test-ok "failures" [{"message" "boom" "line" "12"}]))))
      (expect (not (contract/valid? :test-fn
                                    (assoc test-ok "failures" [{"message" "boom" "ns" 7}])))))
  (it "passes a capability with no registered spec straight through"
      (expect (contract/valid? :repl-eval-fn {:anything :goes}))
      (expect (= :untouched (contract/check :repl-eval-fn :untouched))))
  (it "capability->spec is the single source of truth for format + lint + test"
      (expect (= #{:format-fn :lint-fn :test-fn} (set (keys contract/capability->spec)))))
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
        (expect (true? (get r "is_pass")))
        (expect (contract/valid? :test-fn r))))
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
