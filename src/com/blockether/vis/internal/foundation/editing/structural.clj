(ns com.blockether.vis.internal.foundation.editing.structural
  "Thin Clojure adapter over the pack's Java structural-edit engine
   (`dev.kreuzberg.treesitterlanguagepack.StructuralApi`). All the work —
   locate the definition by name from the tree-sitter outline, splice its line
   span, and re-parse to refuse syntax-breaking edits — lives in Java so it is
   language-neutral, reusable from any JVM consumer, and native-image clean.
   This namespace only maps vis op keywords onto the Java API."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.outline :as outline]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack StructuralApi StructuralApi$Op]))

(def ^:private ops
  {:replace        StructuralApi$Op/REPLACE
   :insert-before  StructuralApi$Op/INSERT_BEFORE
   :insert-after   StructuralApi$Op/INSERT_AFTER
   :append         StructuralApi$Op/APPEND
   :replace-doc    StructuralApi$Op/REPLACE_DOC
   :add-doc        StructuralApi$Op/ADD_DOC})

(defn edit-source
  "Return the new file content for a structural edit, or throw with an
   actionable message (StructuralApi$EditException on missing/ambiguous target,
   no match, or a syntax-breaking result). `op` ∈ #{:replace :insert-before
   :insert-after :append :replace-doc :add-doc :replace-node :rename}.
   `:replace-node` replaces the unique sub-expression equal to `:match`
   (optionally scoped to `:target`); `:rename` renames identifier `:target` to
   `:code`; `:append` ignores `:target`."
  [path source {:keys [op target kind code match]}]
  (let [language (or (outline/detect-language path)
                     (throw (ex-info (str "Unknown language for " path " — use patch(...) instead.")
                                     {:type :ext.foundation.editing/struct-unknown-language :path path})))]
    (case op
      :replace-node (StructuralApi/replaceNode source language match code target (some-> kind name))
      :rename       (StructuralApi/rename source language target code)
      (let [jop (or (ops op)
                    (throw (ex-info (str "Unknown structural op: " op)
                                    {:type :ext.foundation.editing/struct-bad-op :op op})))]
        (StructuralApi/edit source language jop target (some-> kind name) code)))))

(defn references
  "Occurrences of identifier `name` in `path` as
   [{:line :column :start-byte :end-byte :anchor} …] (empty if none / unknown
   language). `:anchor` is the line's `<lineno>:<hash>` patch anchor, so a hit
   can be edited directly with patch — same anchors cat / outline emit."
  [path source name]
  (if-let [language (outline/detect-language path)]
    (let [lines (vec (str/split-lines source))]
      (mapv (fn [^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
              (let [line (.line h)]
                {:line line :column (.column h)
                 :start-byte (.startByte h) :end-byte (.endByte h)
                 :anchor (patch/line-anchor line (nth lines (dec line) ""))}))
            (StructuralApi/findReferences source language name)))
    []))
