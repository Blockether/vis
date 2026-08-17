(ns com.blockether.vis.internal.test-contract-test
  "Tests that REUSE the language-neutral test-runner contract.

   Exercises the clojure.spec definitions (`::selectors`, `::result`), the
   spec-derived key vectors (`selector-keys` / `result-keys`), and the shared
   runtime helpers (`split-node-id`, `normalize-selectors`, `selected?`). The same specs a pack's
   `run_tests` handler builds its result THROUGH are validated here, so a drift
   in the contract shape is caught by this ns."
  (:require [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.test-contract :as contract]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe selector-keys-test
             (describe "selector-keys - derived from the ::selectors spec"
                       (it "lists the three optional selector keys in order"
                           (expect (= [:paths :include :exclude] contract/selector-keys)))
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

(defdescribe
  selectors-spec-test
  (describe
    "::selectors - the selector map a runner accepts"
    (it "accepts a path narrowed by a test name - ONE node-id string"
        (expect (s/valid? ::contract/selectors {:paths ["test/a/core_test.clj::adds-test"]})))
    (it "accepts many paths - files and directories alike"
        (expect (s/valid? ::contract/selectors
                          {:paths ["test/a/core_test.clj" "extensions/b/test"] :exclude ["slow"]})))
    (it "accepts the empty selector map (all keys optional)"
        (expect (s/valid? ::contract/selectors {})))
    ;; The shared map publishes ONE selector vocabulary, and a test NAME rides
    ;; INSIDE a path entry (`file::name`). A pack may read spellings of its own —
    ;; clojure resolves `ns` / `only` against its own workspace — but nothing
    ;; else reaches THIS map to disagree about what the call selected.
    (it "publishes no test-name selector beside :paths"
        (expect (not (some #{:only :filter :var :vars} contract/selector-keys))))
    (it "rejects :paths that is not a coll of strings"
        (expect (not (s/valid? ::contract/selectors {:paths 42})))
        (expect (not (s/valid? ::contract/selectors {:paths [42]}))))
    ;; A namespace is a CLOJURE fact, never a shared one: that pack resolves its
    ;; own `ns` spellings and carries the result in its OWN key. `s/keys` ignores
    ;; unknown keys, so this pins the DERIVED key vector rather than the map's
    ;; validity.
    (it "publishes no namespace selector"
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

(defdescribe split-node-id-test
             (describe
               "split-node-id - one entry -> {:path :var}"
               (it "reads a bare path as a location with no name"
                   (expect (= {:path "test/a/core_test.clj" :var nil}
                              (contract/split-node-id "test/a/core_test.clj"))))
               (it "splits a node id into its path and its test name"
                   (expect (= {:path "test/a/core_test.clj" :var "adds-test"}
                              (contract/split-node-id "test/a/core_test.clj::adds-test"))))
               (it "reads a PATHLESS id as a name with no location"
                   (expect (= {:path nil :var "adds-test"} (contract/split-node-id "::adds-test"))))
               (it "splits on the FIRST :: only, so a pytest parametrized id survives"
                   (expect (= {:path "tests/test_math.py" :var "test_adds[1-2]"}
                              (contract/split-node-id "tests/test_math.py::test_adds[1-2]"))))
               (it "is total on junk - blank halves come back nil"
                   (expect (= {:path nil :var nil} (contract/split-node-id "")))
                   (expect (= {:path nil :var nil} (contract/split-node-id "::")))
                   (expect (= {:path "test" :var nil} (contract/split-node-id "test::  "))))))

(defdescribe
  normalize-selectors-test
  (describe
    "normalize-selectors - raw dict -> canonical {:paths :include :exclude}"
    (it "wraps a single path string into a one-element SPLIT :paths vec"
        (expect (= {:paths [{:path "test" :var nil}] :include [] :exclude []}
                   (contract/normalize-selectors {:paths "test"}))))
    (it "splits every entry and normalizes the rest"
        (expect (= {:paths [{:path "test/a" :var nil} {:path "test/b" :var "adds-test"}]
                    :include []
                    :exclude ["slow"]}
                   (contract/normalize-selectors {:paths ["test/a" "test/b::adds-test"]
                                                  :exclude ["slow"]}))))
    ;; The removed namespace vocabulary: no key rides beside :paths any more, so
    ;; a stale :ns / :namespaces selects NOTHING here instead of quietly
    ;; competing with it. The pack that owns the arg refuses it out loud.
    (it "reads no namespace key at all"
        (expect (= [] (:paths (contract/normalize-selectors {:ns "a-test"}))))
        (expect (= [] (:paths (contract/normalize-selectors {:namespace "a-test"}))))
        (expect (= [] (:paths (contract/normalize-selectors {:namespaces ["a-test" "b-test"]})))))
    (it "reads no test-name key at all"
        (expect (= [] (:paths (contract/normalize-selectors {:only ["adds-test"]}))))
        (expect (= [] (:paths (contract/normalize-selectors {:filter "adds"})))))
    (it "drops blank / nil entries and trims"
        (expect (= {:paths [] :include [] :exclude []}
                   (contract/normalize-selectors {:paths ["  " ""]}))))
    (it "keeps string tags verbatim (strings-only boundary)"
        (expect (= ["slow"] (:exclude (contract/normalize-selectors {:exclude ["slow"]})))))))

(defdescribe
  selected?-test
  (describe
    "selected? - lazytest precedence over one test"
    (it "runs everything when no selectors are given"
        (let [sel (contract/normalize-selectors {})]
          (expect (contract/selected? sel {:ns "a-test" :name "adds" :tags #{}}))))
    (it "a resolved node id narrows by test name (keeps a match)"
        (let [sel (assoc (contract/normalize-selectors {}) :vars [{:ns nil :name "adds"}])]
          (expect (contract/selected? sel {:ns "a-test" :name "adds" :tags #{}}))))
    (it "a resolved node id narrows by test name (drops a non-match)"
        (let [sel (assoc (contract/normalize-selectors {}) :vars [{:ns nil :name "adds"}])]
          (expect (not (contract/selected? sel {:ns "a-test" :name "subtracts" :tags #{}})))))
    ;; The node id's PATH half resolved to a namespace, so the name is scoped to
    ;; it - two files each naming their own test never cross-product.
    (it "a node id that carries its namespace does not select the same name elsewhere"
        (let [sel (assoc (contract/normalize-selectors {}) :vars [{:ns "a-test" :name "adds"}])]
          (expect (contract/selected? sel {:ns "a-test" :name "adds" :tags #{}}))
          (expect (not (contract/selected? sel {:ns "b-test" :name "adds" :tags #{}})))))
    (it "include gates by metadata tag"
        (let [sel (contract/normalize-selectors {:include ["slow"]})]
          (expect (contract/selected? sel {:ns "a-test" :name "x" :tags #{"slow"}}))
          (expect (not (contract/selected? sel {:ns "a-test" :name "x" :tags #{}})))))
    (it "exclude drops a tagged test"
        (let [sel (contract/normalize-selectors {:exclude ["slow"]})]
          (expect (not (contract/selected? sel {:ns "a-test" :name "x" :tags #{"slow"}})))
          (expect (contract/selected? sel {:ns "a-test" :name "x" :tags #{}}))))
    (it "exclude OVERRIDES include when a test carries both"
        (let [sel (contract/normalize-selectors {:include ["slow"] :exclude ["slow"]})]
          (expect (not (contract/selected? sel {:ns "a-test" :name "x" :tags #{"slow"}})))))
    (it "exclude OVERRIDES a node id that named the var outright"
        (let
          [sel (assoc (contract/normalize-selectors {:exclude ["slow"]})
                 :vars [{:ns nil :name "x"}])]
          (expect (not (contract/selected? sel {:ns "a-test" :name "x" :tags #{"slow"}})))))))
