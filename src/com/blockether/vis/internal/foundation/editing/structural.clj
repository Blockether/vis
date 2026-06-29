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

(defn- vis-ize-error
  "The Java engine is shared with maki, so its messages name maki's `index`
   tool — vis's equivalent is `outline`. Rewrite the leaked name and, for a
   missing-definition miss, add the vis fallback. (All bundled languages now have
   working def queries as of pack 1.10.3-blockether.24 — a miss means the name/kind
   is wrong, not that the language is unsupported.) Keep the engine's specifics;
   only fix the steer."
  [^String msg]
  (let [m (str/replace (str msg) "index(path)" "outline(path)")]
    (if (str/includes? m "No definition named")
      (str m " (Re-check the name/`kind` against outline(path); or edit with"
        " write(path, content) or patch(...).)")
      m)))

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
                   (throw (ex-info (str "Unknown language for " path " — use patch(...) or write(...) instead.")
                            {:type :ext.foundation.editing/struct-unknown-language :path path})))]
    (try
      (case op
        :replace-node (StructuralApi/replaceNode source language match code target (some-> kind name))
        :rename       (StructuralApi/rename source language target code)
        (let [jop (or (ops op)
                    (throw (ex-info (str "Unknown structural op: " op)
                             {:type :ext.foundation.editing/struct-bad-op :op op})))]
          (StructuralApi/edit source language jop target (some-> kind name) code)))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (throw (ex-info (vis-ize-error (.getMessage e))
                 {:type :ext.foundation.editing/struct-edit-failed
                  :op op :target target :language language}
                 e))))))

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
