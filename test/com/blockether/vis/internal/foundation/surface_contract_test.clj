(ns com.blockether.vis.internal.foundation.surface-contract-test
  "Contract tests for the clojure.spec language-surface result specs: the
   directory-nested `by-dir` shape shared by format + lint, and `check`'s
   accept/reject/pass-through behaviour."
  (:require [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private format-ok
  {"op" "clj-format"
   "files" [{"path" "a.clj" "changed" true "wrote" true}
            {"path" "sub/b.clj" "changed" false "wrote" false}]
   "changed" 1
   "by-dir" {"." {"a.clj" {"changed" true "wrote" true}}
             "sub" {"b.clj" {"changed" false "wrote" false}}}})

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
   "by-dir" {"." {"a.clj" {"warning" [{"level" "warning" "message" "unused binding b"}]}}}})

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
  (it "accepts a minimal single-file format result (no files / no by-dir)"
      (expect (contract/valid? :format-fn {"op" "clj-format" "changed" true "chars" -3})))
  (it "rejects a format result whose by-dir is not a nested dir->file->map"
      (expect (not (contract/valid? :format-fn (assoc format-ok "by-dir" ["oops"]))))
      (expect (not (contract/valid? :format-fn (assoc format-ok "by-dir" {"." ["flat"]})))))
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
  (it "accepts a test result carrying the shared by-dir grouping"
      (let
        [fail
         {"ns" "my.core-test" "file" "src/com/blockether/vis/core.clj" "line" 12 "message" "boom"}

         err
         {"message" "kaboom"}

         by-dir
         {"src/com/blockether/vis" {"core.clj" {"failures" [fail]}}
          "." {"<unknown>" {"errors" [err]}}}]

        (expect (contract/valid? :test-fn
                                 (assoc test-ok
                                   "fail" 1
                                   "by-dir" by-dir)))))
  (it "rejects a test result whose by-dir is not a nested dir->file->map"
      (expect (not (contract/valid? :test-fn (assoc test-ok "by-dir" ["oops"]))))
      (expect (not (contract/valid? :test-fn (assoc test-ok "by-dir" {"." ["flat"]})))))
  (it "passes a capability with no registered spec straight through"
      (expect (contract/valid? :repl-eval-fn {:anything :goes}))
      (expect (= :untouched (contract/check :repl-eval-fn :untouched))))
  (it "capability->spec is the single source of truth for format + lint + test"
      (expect (= #{:format-fn :lint-fn :test-fn} (set (keys contract/capability->spec))))))
