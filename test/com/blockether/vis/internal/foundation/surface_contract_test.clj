(ns com.blockether.vis.internal.foundation.surface-contract-test
  "Contract tests for the clojure.spec language-surface result specs: the
   directory-nested `by-cwd` shape shared by format + lint, and `check`'s
   accept/reject/pass-through behaviour."
  (:require [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private format-ok
  {"op" "clj-format"
   "files" [{"path" "a.clj" "changed" true "wrote" true "formatter" "zprint"}
            {"path" "sub/b.clj" "changed" false "wrote" false}]
   "changed" 1
   "by-cwd" {"." {"a.clj" {"changed" true "wrote" true}}
             "sub" {"b.clj" {"changed" false "wrote" false}}}
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
   "language" "clojure"
   "by-cwd" {"." {"a.clj" {"warning" [{"level" "warning" "message" "unused binding b"}]}}}})

(def ^:private test-ok
  {"mode" "repl"
   "language" "clojure"
   "ns" "my.app.core-test"
   "framework" "clojure.test"
   "total" 3
   "pass" 3
   "fail" 0
   "failures" []
   "errors" []
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
  (it "accepts a minimal single-file format result (no files / no by-cwd)"
      (expect (contract/valid? :format-fn {"op" "clj-format" "changed" true "chars" -3})))
  (it "accepts a single-file format result naming the formatter that ran"
      (expect (contract/valid? :format-fn
                               {"op" "clj-format" "changed" true "chars" -3 "formatter" "zprint"})))
  (it "rejects a format result whose formatters set is not strings"
      (expect (not (contract/valid? :format-fn (assoc format-ok "formatters" [1 2])))))
  (it "rejects a format result whose by-cwd is not a nested dir->file->map"
      (expect (not (contract/valid? :format-fn (assoc format-ok "by-cwd" ["oops"]))))
      (expect (not (contract/valid? :format-fn (assoc format-ok "by-cwd" {"." ["flat"]})))))
  (it "rejects a format result missing the op key"
      (expect (not (contract/valid? :format-fn (dissoc format-ok "op")))))
  (it "rejects a lint result whose findings lack level/message"
      (expect (not (contract/valid? :lint-fn (assoc lint-ok "findings" [{"file" "a.clj"}]))))
      (expect (not (contract/valid? :lint-fn (dissoc lint-ok "findings")))))
  (it "check throws a tagged contract-violation ex-info on a bad result"
      (let
        [ed (try (contract/check :lint-fn (dissoc lint-ok "findings"))
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
                                              "file" "core_test.clj"
                                              "line" 12
                                              "message" "expected 3"}]))))
  (it "rejects a test failure whose line is not a non-negative int"
      (expect (not (contract/valid? :test-fn
                                    (assoc test-ok "failures" [{"message" "boom" "line" "12"}]))))
      (expect (not (contract/valid? :test-fn
                                    (assoc test-ok "failures" [{"message" "boom" "ns" 7}])))))
  (it "accepts a test result carrying the shared by-cwd grouping"
      (let
        [fail
         {"ns" "my.core-test" "file" "src/com/blockether/vis/core.clj" "line" 12 "message" "boom"}

         err
         {"message" "kaboom"}

         by-cwd
         {"src/com/blockether/vis" {"core.clj" {"failures" [fail]}}
          "." {"<unknown>" {"errors" [err]}}}]

        (expect (contract/valid? :test-fn
                                 (assoc test-ok
                                   "fail" 1
                                   "by-cwd" by-cwd)))))
  (it "rejects a test result whose by-cwd is not a nested dir->file->map"
      (expect (not (contract/valid? :test-fn (assoc test-ok "by-cwd" ["oops"]))))
      (expect (not (contract/valid? :test-fn (assoc test-ok "by-cwd" {"." ["flat"]})))))
  (it "passes a capability with no registered spec straight through"
      (expect (contract/valid? :repl-eval-fn {:anything :goes}))
      (expect (= :untouched (contract/check :repl-eval-fn :untouched))))
  (it "capability->spec is the single source of truth for format + lint + test"
      (expect (= #{:format-fn :lint-fn :test-fn} (set (keys contract/capability->spec)))))
  (it "completes an error-branch test result onto the TOTAL key set"
      ;; The reported crash: a run that errored out returned NO "failures" key, so
      ;; `r["failures"][:3]` in ordinary model Python blew up on None.
      (let
        [r (contract/complete-test-result
             "clojure"
             {"mode" "repl" "ns" "my.app.core-test" "port" 7888 "error" "nREPL is down"})]
        (expect (every? #(contains? r %) (keys contract/test-result-base)))
        (expect (= [] (get r "failures")))
        (expect (= [] (get r "errors")))
        (expect (= {} (get r "by-cwd")))
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
  (it "folds pytest/bun key vocabulary onto the canonical count names"
      (let
        [r (contract/complete-test-result "python"
                                          {"mode" "cli"
                                           "runner" "project"
                                           "cmd" ["pytest" "tests"]
                                           "exit" 1
                                           "passed" 7
                                           "failed" 2
                                           "errored" 1
                                           "skipped" 3})]
        (expect (= 7 (get r "pass")))
        (expect (= 3 (get r "fail"))) ; failed + errored
        (expect (= 13 (get r "total"))) ; derived: pass + fail + skipped
        (expect (= "pytest tests" (get r "command")))
        (expect (= "python" (get r "language")))
        (expect (false? (get r "is_pass")))))
  (it "derives is_pass from ok / exit when the pack reports no counts"
      (expect (true? (get (contract/complete-test-result "python" {"ok" true}) "is_pass")))
      (expect (false? (get (contract/complete-test-result "python" {"ok" false}) "is_pass")))
      (expect (true? (get (contract/complete-test-result "typescript" {"exit" 0}) "is_pass")))
      (expect (false? (get (contract/complete-test-result "typescript" {"exit" 2}) "is_pass")))
      (expect (nil? (get (contract/complete-test-result "typescript" {}) "is_pass"))))
  (it "stamps the language that actually ran, and passes a non-map through"
      (expect (= "typescript" (get (contract/complete-test-result "typescript" {}) "language")))
      (expect (= "clojure"
                 (get (contract/complete-test-result "typescript" {"language" "clojure"})
                      "language")))
      (expect (= :untouched (contract/complete-test-result "clojure" :untouched)))))
