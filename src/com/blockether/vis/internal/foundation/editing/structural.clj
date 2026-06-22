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
   :append         StructuralEdit$Op/APPEND
   :replace-doc    StructuralEdit$Op/REPLACE_DOC})

(defn edit-source
  "Return the new file content for a structural edit, or throw with an
   actionable message (StructuralEdit$EditException on missing/ambiguous target,
   no match, or a syntax-breaking result). `op` ∈ #{:replace :insert-before
   :insert-after :append :replace-doc :replace-node}. `:replace-node` replaces
   the unique sub-expression equal to `:match` (optionally scoped to `:target`);
   `:append` ignores `:target`."
  [path source {:keys [op target kind code match]}]
  (let [language (or (index/detect-language path)
                     (throw (ex-info (str "Unknown language for " path " — use patch(...) instead.")
                                     {:type :ext.foundation.editing/struct-unknown-language :path path})))]
    (if (= op :replace-node)
      (StructuralEdit/replaceNode source language match code target (some-> kind name))
      (let [jop (or (ops op)
                    (throw (ex-info (str "Unknown structural op: " op)
                                    {:type :ext.foundation.editing/struct-bad-op :op op})))]
        (StructuralEdit/edit source language jop target (some-> kind name) code)))))
