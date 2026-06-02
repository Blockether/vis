(ns com.blockether.vis.internal.sci-symbols-test
  (:require
   [com.blockether.vis.internal.sci-symbols :as ss]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- git-ctx []
  (let [git-ns (sci/create-ns 'vis.ext.git)]
    (sci/init {:namespaces {'vis.ext.git
                            {'diff (sci/new-var 'diff (fn [] nil)
                                     {:ns          git-ns
                                      :doc         "diff docs"
                                      :arglists    '([])
                                      :vis/source  "(defn diff [] nil)"})}}
               :ns-aliases {'git 'vis.ext.git}})))

(defdescribe doc-test
  (it "merges docstring + arglists + source into one map under the aliased symbol"
    (let [r (ss/doc (git-ctx) 'git/diff)]
      (expect (:found? r))
      (expect (= 'git/diff (:symbol r)))
      (expect (= 'vis.ext.git/diff (:resolved-symbol r)))
      (expect (= "diff docs" (:doc r)))
      (expect (= '([]) (:arglists r)))
      (expect (false? (:macro? r)))
      (expect (= "(defn diff [] nil)" (:source r)))
      (expect (= 18 (:source-length r)))))

  (it "accepts quoted symbol, keyword, or string forms"
    (doseq [arg ['git/diff :git/diff "git/diff"]]
      (expect (:found? (ss/doc (git-ctx) arg)))))

  (it "returns :found? false (never throws) on a missing symbol"
    (let [r (ss/doc (git-ctx) 'no/such)]
      (expect (false? (:found? r)))
      (expect (string? (:message r)))))

  (it "returns :found? false when no SCI context is available"
    (expect (false? (:found? (ss/doc nil 'git/diff))))))

(defdescribe apropos-test
  (it "fuzzy-matches symbol names + docstrings under the model-facing alias"
    (let [r (ss/apropos (git-ctx) "diff")]
      (expect (= "diff" (:query r)))
      (expect (pos? (:count r)))
      (expect (some #(= 'git/diff (:symbol %)) (:matches r)))
      (expect (some :has-source? (:matches r)))))

  (it "is a plain string search — no quoting trap, returns empty on no match"
    (let [r (ss/apropos (git-ctx) "zzz-nomatch")]
      (expect (= 0 (:count r)))
      (expect (= [] (:matches r))))))

(defdescribe bindings-test
  (it "build-symbol-bindings derefs :sci-ctx from the env-thunk at call time"
    (let [env-atom (atom {})
          binds    (ss/build-symbol-bindings (fn [] @env-atom))]
      (expect (contains? binds 'doc))
      (expect (contains? binds 'apropos))
      ;; sci-ctx wired AFTER bindings built: thunk reads it lazily
      (reset! env-atom {:sci-ctx (git-ctx)})
      (expect (:found? ((get binds 'doc) 'git/diff)))
      (expect (pos? (:count ((get binds 'apropos) "diff")))))))
