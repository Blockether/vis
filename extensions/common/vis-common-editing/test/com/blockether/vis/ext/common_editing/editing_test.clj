(ns com.blockether.vis.ext.common-editing.editing-test
  "Tests for the editing extension.

   The previous version of this file targeted private vars that were
   deleted in the \"Drastically simplify the agent\" cull (commit
   cad5f7d):

     * `rescue-grep-args` / `rescue-path-args` / `rescue-parse-error`
     * `strip-bad-escape` / `extract-bad-escape-char`
     * `line-col->index`
     * `patch` / `apply-one-replacement`

   None of those exist in the current `editing.clj` — the
   on-error-fn rescue chain and the `patch` tool were removed in
   favor of the smaller (vis/cat | ls | rg | edit | write | zedit)
   surface. The unit tests for the deleted internals were left
   orphaned in the tree and broke the entire `clojure -M:test`
   pipeline (compile error: \"Unable to resolve var:
   editing/rescue-grep-args\").

   This file is now a thin wrapper that asserts the live extension
   surface still loads and registers without throwing. The
   functional behaviour of `vis/cat` / `vis/ls` / `vis/rg` /
   `vis/edit` / `vis/write` / `vis/zedit` is exercised end-to-end
   through the agent loop's integration tests; an isolated unit-
   test block for each is not preserved here."
  (:require
   [clojure.string :as string]
   [com.blockether.vis.ext.common-editing.editing :as editing]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe editing-extension-loads-test
  (it "exposes the canonical five-tool symbol vector"
    ;; cat / ls / rg / edit / write. zedit moved to vis-language-clojure.
    (expect (vector? editing/editing-symbols))
    (expect (= 5 (count editing/editing-symbols))))

  (it "every editing symbol carries a non-blank :doc and an :arglists vector"
    (doseq [s editing/editing-symbols
            :let [doc      (:ext.symbol/doc s)
                  arglists (:ext.symbol/arglists s)]]
      (expect (string? doc))
      (expect (not (string/blank? doc)))
      (expect (or (vector? arglists) (seq? arglists)))))

  (it "exposes a non-blank prompt fragment"
    (expect (string? editing/editing-prompt))
    (expect (not (string/blank? editing/editing-prompt)))))
