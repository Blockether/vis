(ns com.blockether.vis.internal.test-contract-test
  "Tests that REUSE the language-neutral test-runner contract.

   Exercises the clojure.spec definitions (`::selectors`, `::result`), the
   spec-derived key vectors (`selector-keys` / `result-keys`), and the shared
   runtime helpers (`normalize-selectors`, `selected?`). The same specs a pack's
   `run_tests` handler builds its result THROUGH are validated here, so a drift
   in the contract shape is caught by this ns."
  (:require [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.test-contract :as contract]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe selector-keys-test
             (describe "selector-keys - derived from the ::selectors spec"
                       (it "lists the four optional selector keys in order"
                           (expect (= [:paths :only :include :exclude] contract/selector-keys)))
                       (it "stays in sync with the ::selectors spec :opt-un keys"
                           (let [opts (apply hash-map (rest (s/form ::contract/selectors)))]
                             (expect (= (mapv (comp keyword name) (:opt-un opts))
                                        contract/selector-keys))))))

(defdescribe result-keys-test
             (describe "result-keys - derived from the ::result spec"
                       (it "lists every uniform result key in order"
                           (expect (= [:language :mode :framework :tool :ns :total :pass :fail
                                       :errored :selected :skipped :failures :output]
                                      contract/result-keys)))
                       (it "stays in sync with the ::result spec :opt-un keys"
                           (let [opts (apply hash-map (rest (s/form ::contract/result)))]
                             (expect (= (mapv (comp keyword name) (:opt-un opts))
                                        contract/result-keys))))))

(defdescribe selectors-spec-test
             (describe
               "::selectors - the selector map a runner accepts"
               (it "accepts paths narrowed by a test name"
                   (expect (s/valid? ::contract/selectors {:paths ["test"] :only ["adds"]})))
               (it "accepts many paths - files and directories alike"
                   (expect (s/valid? ::contract/selectors
                                     {:paths ["test/a/core_test.clj" "extensions/b/test"]
                                      :exclude ["slow"]})))
               (it "accepts the empty selector map (all keys optional)"
                   (expect (s/valid? ::contract/selectors {})))
               (it "rejects :only that is not a coll of strings"
                   (expect (not (s/valid? ::contract/selectors {:only "not-a-vec"}))))
               (it "rejects :paths that is not a coll of strings"
                   (expect (not (s/valid? ::contract/selectors {:paths 42})))
                   (expect (not (s/valid? ::contract/selectors {:paths [42]}))))
               ;; The namespace selector is GONE - paths are the one way to name what runs,
               ;; and `s/keys` ignores unknown keys, so this pins the DERIVED key vector
               ;; rather than the map's validity.
               (it "no longer publishes a namespace selector"
                   (expect (not (some #{:ns :namespace :namespaces} contract/selector-keys))))))

(defdescribe
  result-spec-test
  (describe
    "::result - the uniform map every runner returns"
    (it "validates a full green repl result"
        (expect (s/valid? ::contract/result
                          {:language "clojure"
                           :mode "repl"
                           :framework "lazytest"
                           :ns "my.app.core-test"
                           :total 32
                           :pass 32
                           :fail 0
                           :selected 6
                           :skipped 0
                           :failures []
                           :output "Ran 32 tests."})))
    (it "validates a failing result with a failure entry"
        (expect (s/valid?
                  ::contract/result
                  {:language "clojure"
                   :mode "repl"
                   :framework "clojure.test"
                   :total 3
                   :pass 2
                   :fail 1
                   :selected 3
                   :skipped 0
                   :failures
                   [{:ns "x" :test "adds" :type "fail" :message "boom" :file "x.clj" :line 12}
                    {:ns "x" :test "boom" :type "error" :message "threw"}]})))
    (it "validates a cli result (the ns it RAN, not what was selected)"
        (expect (s/valid? ::contract/result
                          {:language "clojure" :mode "cli" :tool "clj" :ns "my.app.core-test"})))
    (it "rejects a fault whose :type is outside the closed fail/error vocabulary"
        (expect (not (s/valid? ::contract/result
                               {:failures [{:ns "x" :test "adds" :type "exploded"}]}))))
    (it "rejects an unknown :mode value"
        (expect (not (s/valid? ::contract/result {:mode "weird"}))))
    (it "rejects a negative count" (expect (not (s/valid? ::contract/result {:total -1}))))))

(defdescribe
  normalize-selectors-test
  (describe "normalize-selectors - raw dict -> canonical {:paths :only :include :exclude}"
            (it "wraps a single path string into a one-element :paths vec"
                (expect (= {:paths ["test"] :only [] :include [] :exclude []}
                           (contract/normalize-selectors {:paths "test"}))))
            (it "keeps many paths and normalizes the rest"
                (expect (= {:paths ["test/a" "test/b"] :only ["foo"] :include [] :exclude ["slow"]}
                           (contract/normalize-selectors
                             {:paths ["test/a" "test/b"] :only ["foo"] :exclude ["slow"]}))))
            ;; The removed namespace vocabulary: no key rides beside :paths any more, so
            ;; a stale :ns / :namespaces selects NOTHING here instead of quietly
            ;; competing with it. The pack that owns the arg refuses it out loud.
            (it "reads no namespace key at all"
                (expect (= [] (:paths (contract/normalize-selectors {:ns "a-test"}))))
                (expect (= [] (:paths (contract/normalize-selectors {:namespace "a-test"}))))
                (expect
                  (= [] (:paths (contract/normalize-selectors {:namespaces ["a-test" "b-test"]})))))
            (it "drops blank / nil entries and trims"
                (expect (= {:paths [] :only [] :include [] :exclude []}
                           (contract/normalize-selectors {:paths ["  " ""] :only nil}))))
            (it "keeps string tags verbatim (strings-only boundary)"
                (expect (= ["slow"] (:exclude (contract/normalize-selectors {:exclude ["slow"]})))))
            (it "produces selector keys that round-trip through the ::selectors spec"
                (expect (s/valid? ::contract/selectors
                                  (contract/normalize-selectors {:paths "test" :only ["x"]}))))))

(defdescribe selected?-test
             (describe
               "selected? - lazytest precedence over one test"
               (it "runs everything when no selectors are given"
                   (let [sel (contract/normalize-selectors {})]
                     (expect (contract/selected? sel "adds" #{}))))
               (it "only narrows by test name (keeps a match)"
                   (let [sel (contract/normalize-selectors {:only ["adds"]})]
                     (expect (contract/selected? sel "adds" #{}))))
               (it "only narrows by test name (drops a non-match)"
                   (let [sel (contract/normalize-selectors {:only ["adds"]})]
                     (expect (not (contract/selected? sel "subtracts" #{})))))
               (it "include gates by metadata tag"
                   (let [sel (contract/normalize-selectors {:include ["slow"]})]
                     (expect (contract/selected? sel "x" #{"slow"}))
                     (expect (not (contract/selected? sel "x" #{})))))
               (it "exclude drops a tagged test"
                   (let [sel (contract/normalize-selectors {:exclude ["slow"]})]
                     (expect (not (contract/selected? sel "x" #{"slow"})))
                     (expect (contract/selected? sel "x" #{}))))
               (it "exclude OVERRIDES include when a test carries both"
                   (let [sel (contract/normalize-selectors {:include ["slow"] :exclude ["slow"]})]
                     (expect (not (contract/selected? sel "x" #{"slow"})))))))
