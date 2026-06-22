(ns com.blockether.vis.internal.foundation.editing.structural
  "Thin Clojure adapter over the pack's Java structural-edit engine
   (`dev.kreuzberg.treesitterlanguagepack.StructuralEdit`). All the work —
   locate the definition by name from the tree-sitter outline, splice its line
   span, and re-parse to refuse syntax-breaking edits — lives in Java so it is
   language-neutral, reusable from any JVM consumer, and native-image clean.
   This namespace only maps vis op keywords onto the Java API."
  (:require [com.blockether.vis.internal.foundation.editing.index :as index]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack StructuralEdit StructuralEdit$Op]))

(def ^:private ops
  {:replace        StructuralEdit$Op/REPLACE
   :insert-before  StructuralEdit$Op/INSERT_BEFORE
   :insert-after   StructuralEdit$Op/INSERT_AFTER
   :append         StructuralEdit$Op/APPEND})

(defn edit-source
  "Return the new file content for a structural edit, or throw with an
   actionable message (StructuralEdit$EditException on missing/ambiguous target
   or a syntax-breaking result). `op` ∈ #{:replace :insert-before :insert-after
   :append}; `:append` ignores `:target`."
  [path source {:keys [op target kind code]}]
  (let [language (or (index/detect-language path)
                     (throw (ex-info (str "Unknown language for " path " — use patch(...) instead.")
                                     {:type :ext.foundation.editing/struct-unknown-language :path path})))
        jop      (or (ops op)
                     (throw (ex-info (str "Unknown structural op: " op)
                                     {:type :ext.foundation.editing/struct-bad-op :op op})))]
    (StructuralEdit/edit source language jop target (some-> kind name) code)))
