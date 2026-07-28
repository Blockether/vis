(ns com.blockether.vis.internal.foundation.editing.structural
  "Thin Clojure adapter over the pack's Java structural-edit engine
   (`dev.kreuzberg.treesitterlanguagepack.StructuralApi`). All the work —
   locate the definition by name from the tree-sitter outline, splice its line
   span, and re-parse to refuse syntax-breaking edits — lives in Java so it is
   language-neutral, reusable from any JVM consumer, and native-image clean.
   This namespace only maps vis op keywords onto the Java API."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.index :as index]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack StructuralApi StructuralApi$Op]))

(def ^:private ops
  {:replace StructuralApi$Op/REPLACE
   :insert-before StructuralApi$Op/INSERT_BEFORE
   :insert-after StructuralApi$Op/INSERT_AFTER
   :append StructuralApi$Op/APPEND
   :replace-doc StructuralApi$Op/REPLACE_DOC
   :add-doc StructuralApi$Op/ADD_DOC})

(defn- vis-ize-error
  "The Java engine is shared with maki, so its messages name the `index` tool
   — maki's outline tool, whereas vis's tool is now `struct_index`, so the
   engine's leaked `index` gets vis's steer below. For a missing-definition miss, add
   the vis fallback. (All bundled languages now have working def queries as of
   pack 1.10.3-blockether.24 — a miss means the name/kind is wrong, not that the
   language is unsupported.) Keep the engine's specifics; only add the steer."
  [^String msg]
  (let
    [m (-> (str msg)
           ;; The shared engine names maki's `index` outline tool; vis exposes
           ;; it as `struct_index`. Rewrite that one leaked call site with a
           ;; plain literal swap — the engine only ever emits `Use index(`.
           (str/replace "Use index(" "Use struct_index("))]
    (if (str/includes? m "No definition named")
      (str m
           " (Re-check the name/`kind` against struct_index(path); or edit with"
           " write(path, content) or patch(...).)")
      m)))

(defn- slice-lines
  "Inclusive 0-based `[start end]` line slice of `source` as a string."
  [^String source ^long start ^long end]
  (->> (str/split-lines source)
       (drop start)
       (take (inc (- end start)))
       (str/join "\n")))

(defn- cut-node-lines
  "Remove the inclusive 0-based `[s e]` line span (the moved node) from `source`,
   and ONLY at that seam collapse a doubled blank line back to one — so the move
   leaves no ragged gap WITHOUT touching whitespace anywhere else in the file.
   Returns the source with the span removed."
  [^String source ^long s ^long e]
  (let
    [lines
     (vec (str/split-lines source))

     before
     (subvec lines 0 (min s (count lines)))

     after
     (vec (subvec lines (min (inc e) (count lines))))

     ;; seam = end of `before` meets start of `after`. If both sides are blank
     ;; (the node had a blank line above AND below), drop one so a single blank
     ;; remains — local to this seam, nothing else.
     after
     (if (and (str/blank? (str (last before))) (seq after) (str/blank? (str (first after))))
       (vec (rest after))
       after)]

    (str/join "\n" (concat before after))))

(defn- move-source
  "RELOCATE the top-level node named `target` to before/after `anchor` (both
   located BY NAME) in one step: extract target's exact source text, delete it,
   then re-insert at the anchor. `target` and `anchor` must differ. SAFETY: the
   deletion only removes the target's own line span (seam-local blank cleanup,
   never a file-wide whitespace rewrite), and the final write RE-PARSES the result
   — any edit that would break OTHER code (dangling form, syntax error) is refused.
   Solves 'I defined X before its dependency Y — move X to after Y' with no manual
   cut-and-paste."
  [^String source ^String language target kind anchor position]
  (when (str/blank? (str anchor))
    (throw (ex-info "move requires an `anchor` (the node to move next to)."
                    {:type :ext.foundation.editing/struct-move-no-anchor :target target})))
  (when (= target anchor)
    (throw (ex-info (str "move: `target` and `anchor` are the same node (" target ").")
                    {:type :ext.foundation.editing/struct-move-same-node :target target})))
  (let
    [span
     (or (index/node-span source
                          language
                          target
                          (some-> kind
                                  name))
         (throw (ex-info
                  (str "No definition named '" target "' to move (check struct_index(path)).")
                  {:type :ext.foundation.editing/struct-move-no-target :target target})))

     text
     (slice-lines source (first span) (second span))

     ;; delete the target ONLY (its own line span), seam-local cleanup
     deleted
     (cut-node-lines source (first span) (second span))

     jop
     (case position
       :before
       StructuralApi$Op/INSERT_BEFORE

       StructuralApi$Op/INSERT_AFTER)]

    ;; Re-insert next to the anchor. INSERT_* now supplies its own blank-line
    ;; separator, so `text` goes in verbatim (no leading \n — that would double the
    ;; gap). INSERT_* runs on the deleted (still-valid) tree; the caller's write
    ;; re-parses and REFUSES the whole edit if the relocate broke anything.
    (StructuralApi/edit deleted language jop anchor nil text)))

(defn edit-source
  "Return the new file content for a structural edit, or throw with an
   actionable message (StructuralApi$EditException on missing/ambiguous target,
   no match, or a syntax-breaking result). `op` ∈ #{:replace :insert-before
   :insert-after :append :replace-doc :add-doc :replace-node :rename
   :move-before :move-after}.
   `:replace-node` replaces the unique sub-expression equal to `:match`
   (optionally scoped to `:target`); `:rename` renames identifier `:target` to
   `:code`; `:append` ignores `:target`; `:move-before`/`:move-after` relocate
   the node named `:target` next to the node named `:anchor`."
  [path source {:keys [op target kind code match anchor]}]
  (let
    [language (or (index/detect-language path)
                  (throw
                    (ex-info
                      (str "Unknown language for " path " — use patch(...) or write(...) instead.")
                      {:type :ext.foundation.editing/struct-unknown-language :path path})))]
    (try
      (case op
        :replace-node
        (StructuralApi/replaceNode source
                                   language
                                   match
                                   code
                                   target
                                   (index/resolve-edit-kind source language target kind))

        :rename
        (StructuralApi/rename source language target code)

        :move-before
        (move-source source language target kind anchor :before)

        :move-after
        (move-source source language target kind anchor :after)

        (let
          [jop (or (ops op)
                   (throw (ex-info (str "Unknown structural op: " op)
                                   {:type :ext.foundation.editing/struct-bad-op :op op})))]
          (StructuralApi/edit source
                              language
                              jop
                              target
                              (index/resolve-edit-kind source language target kind)
                              code)))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (let
          [raw (vis-ize-error (.getMessage e))
           synx? (str/includes? (str raw) "syntax error")
           hint
           (when (and synx? (string? code) (not (str/blank? code)))
             (try
               (or
                 (zipper/describe-syntax-errors language code)
                 ;; `code` parses clean alone → the fault is at the seam.
                 (str
                   "the replacement parses fine in isolation, so the fault is at the INSERTION SEAM"
                   " — an enclosing delimiter was consumed/duplicated or the indentation disagrees"
                   " with the parens; check the code's OUTER balance against the surrounding form."))
               (catch Throwable _ nil)))]

          (throw (ex-info (cond-> raw
                            hint
                            (str "  " hint))
                          {:type :ext.foundation.editing/struct-edit-failed
                           :op op
                           :target target
                           :language language}
                          e)))))))

(defn references
  "Occurrences of identifier `name` in `path` as
   [{:line :column :start-byte :end-byte :anchor} …] (empty if none / unknown
   language). `:anchor` is the line's `<lineno>:<hash>` patch anchor, so a hit
   can be edited directly with patch — same anchors cat / index emit."
  [path source name]
  (if-let [language (index/detect-language path)]
    (let [lines (vec (str/split-lines source))]
      (mapv (fn [^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
              (let [line (.line h)]
                {:line line
                 :column (.column h)
                 :start-byte (.startByte h)
                 :end-byte (.endByte h)
                 :anchor (patch/line-anchor line (nth lines (dec line) ""))}))
            (StructuralApi/findReferences source language name)))
    []))

(defn occurrences
  "Every occurrence of identifier `name` in `path` — the DEFINITION occurrences
   ENRICHED — as ONE list (empty if none / unknown language):

     {:anchor}                                                     ; a plain use
     {:anchor :is-definition true :kind :visibility :signature     ; a DEFINITION
      :doc :end-anchor}                                            ;   span = :anchor..:end-anchor

   Every entry's SOLE position is its patch-ready `lineno:hash` `:anchor` (the
   lineno lives in the anchor — no redundant :line/:column/byte fields). A use is
   just that anchor; a definition also carries its kind / visibility (public|
   private) / signature / doc-gist and an `:end-anchor` (`:anchor`..`:end-anchor`
   is the whole def, patchable in one edit). Syntactic (tree-sitter identifier
   boundaries, no scope resolution — so N same-named definitions are each marked).

   Definition detection: the FIRST occurrence inside each definition's
   `:anchor`..`:end-anchor` line span IS its declaration name (findReferences
   returns hits in source order), so it survives decorators / attributes above it."
  [path source name]
  (if-let [language (index/detect-language path)]
    (let
      [lines (vec (str/split-lines source))
       line-anchor #(patch/line-anchor % (nth lines (dec (long %)) ""))
       hits (vec (StructuralApi/findReferences source language name))
       defs (index/definitions source language name)
       ;; claim: def → index of the first still-unclaimed hit inside its span.
       ;; The span is recovered from the def's anchors (its sole position).
       claimed
       (reduce (fn [acc d]
                 (let
                   [lo (patch/anchor->line (:anchor d))
                    hi (patch/anchor->line (:end-anchor d))]

                   (if-let
                     [i (first
                          (keep-indexed
                            (fn
                              [i ^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
                              (when (and (not (contains? acc i)) (<= lo (.line h) hi)) i))
                            hits))]
                     (assoc acc i d)
                     acc)))
               {}
               defs)]

      (vec (map-indexed (fn [i ^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
                          (let [base {:anchor (line-anchor (.line h))}]
                            (if-let [d (get claimed i)]
                              (assoc base
                                :is-definition true
                                :kind (:kind d)
                                :visibility (:visibility d)
                                :signature (:signature d)
                                :doc (:doc d)
                                :end-anchor (:end-anchor d))
                              base)))
                        hits)))
    []))
