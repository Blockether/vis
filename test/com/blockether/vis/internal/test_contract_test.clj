(ns com.blockether.vis.internal.test-contract-test
  "Tests that REUSE the language-neutral test-runner contract.

   Exercises the clojure.spec definitions (`::selectors`, `::result`), the
   spec-derived key vectors (`selector-keys` / `result-keys`), and the shared
   runtime helpers (`normalize-selectors`, `selected?`). The same specs the
   `clj_test` runner builds its result THROUGH are validated here, so a drift in
   the contract shape is caught by this ns."
  (:require
   [clojure.spec.alpha :as s]
   [com.blockether.vis.internal.test-contract :as contract]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe selector-keys-test
  (describe "selector-keys - derived from the ::selectors spec"
    (it "lists the four optional selector keys in order"
      (expect (= [:ns :only :include :exclude] contract/selector-keys)))
    (it "stays in sync with the ::selectors spec :opt-un keys"
      (let [opts (apply hash-map (rest (s/form ::contract/selectors)))]
        (expect (= (mapv (comp keyword name) (:opt-un opts))
                  contract/selector-keys))))))

(defdescribe result-keys-test
  (describe "result-keys - derived from the ::result spec"
    (it "lists every uniform result key in order"
      (expect (= [:language :mode :framework :tool :ns
                  :total :pass :fail :selected :skipped
                  :failures :errors :output]
                contract/result-keys)))
    (it "stays in sync with the ::result spec :opt-un keys"
      (let [opts (apply hash-map (rest (s/form ::contract/result)))]
        (expect (= (mapv (comp keyword name) (:opt-un opts))
                  contract/result-keys))))))

(defdescribe selectors-spec-test
  (describe "::selectors - the selector map clj_test accepts"
    (it "accepts a single-ns string selector"
      (expect (s/valid? ::contract/selectors
                {:ns "my.app.core-test" :only ["adds"]})))
    (it "accepts a many-ns vector selector"
      (expect (s/valid? ::contract/selectors
                {:ns ["a-test" "b-test"] :exclude ["slow"]})))
    (it "accepts the empty selector map (all keys optional)"
      (expect (s/valid? ::contract/selectors {})))
    (it "rejects :only that is not a coll of strings"
      (expect (not (s/valid? ::contract/selectors {:only "not-a-vec"}))))
    (it "rejects :ns that is neither string nor coll-of-string"
      (expect (not (s/valid? ::contract/selectors {:ns 42}))))))

(defdescribe result-spec-test
  (describe "::result - the uniform map every runner returns"
    (it "validates a full green repl result"
      (expect (s/valid? ::contract/result
                {:language "clojure" :mode "repl" :framework "lazytest"
                 :ns "my.app.core-test"
                 :total 32 :pass 32 :fail 0 :selected 6 :skipped 0
                 :failures [] :errors [] :output "Ran 32 tests."})))
    (it "validates a failing result with a failure entry"
      (expect (s/valid? ::contract/result
                {:language "clojure" :mode "repl" :framework "clojure.test"
                 :total 3 :pass 2 :fail 1 :selected 3 :skipped 0
                 :failures [{:ns "x" :test "adds" :message "boom"
                             :file "x.clj" :line 12}]
                 :errors []})))
    (it "validates a cli result (no selectors apply)"
      (expect (s/valid? ::contract/result
                {:language "clojure" :mode "cli" :tool "clj"
                 :ns "my.app.core-test"})))
    (it "rejects an unknown :mode value"
      (expect (not (s/valid? ::contract/result {:mode "weird"}))))
    (it "rejects a negative count"
      (expect (not (s/valid? ::contract/result {:total -1}))))))

(defdescribe normalize-selectors-test
  (describe "normalize-selectors - raw dict -> canonical {:nses :only :include :exclude}"
    (it "wraps a single-ns string into a one-element :nses vec"
      (expect (= {:nses ["a-test"] :only [] :include [] :exclude []}
                (contract/normalize-selectors {:ns "a-test"}))))
    (it "keeps a many-ns vector and normalizes selectors"
      (expect (= {:nses ["a-test" "b-test"] :only ["foo"]
                  :include [] :exclude ["slow"]}
                (contract/normalize-selectors
                  {:ns ["a-test" "b-test"] :only ["foo"] :exclude ["slow"]}))))
    (it "treats :namespace as an alias for :ns"
      (expect (= ["a-test"] (:nses (contract/normalize-selectors {:namespace "a-test"})))))
    (it "treats :namespaces (plural) as an alias for :ns, string or vector"
      (expect (= ["a-test"] (:nses (contract/normalize-selectors {:namespaces "a-test"}))))
      (expect (= ["a-test" "b-test"] (:nses (contract/normalize-selectors {:namespaces ["a-test" "b-test"]})))))
    (it "drops blank / nil entries and trims"
      (expect (= {:nses [] :only [] :include [] :exclude []}
                (contract/normalize-selectors {:ns ["  " ""] :only nil}))))
    (it "keeps string tags verbatim (strings-only boundary)"
      (expect (= ["slow"] (:exclude (contract/normalize-selectors {:exclude ["slow"]})))))
    (it "produces selector keys that round-trip through the ::selectors spec"
      (let [norm (contract/normalize-selectors {:ns "a-test" :only ["x"]})]
        (expect (s/valid? ::contract/selectors
                  {:ns (:nses norm) :only (:only norm)
                   :include (:include norm) :exclude (:exclude norm)}))))))

(defdescribe selected?-test
  (describe "selected? - lazytest precedence over one test"
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
