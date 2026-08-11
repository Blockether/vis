(ns com.blockether.vis.internal.foundation.editing.structural
  "Thin Clojure adapter over the pack's Java structural-edit engine
   (`dev.kreuzberg.treesitterlanguagepack.StructuralApi`). All the work —
   locate the definition by name from the tree-sitter outline, splice its line
   span, and re-parse to refuse syntax-breaking edits — lives in Java so it is
   language-neutral, reusable from any JVM consumer, and native-image clean.
   This namespace only maps vis op keywords onto the Java API."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.index :as index]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack StructuralApi StructuralApi$FileReferences
            StructuralApi$FileSource StructuralApi$Op]))

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
      (str m " (Re-check the name/`kind` against struct_index(path).)")
      m)))

(defn- split-keep-lines
  "Split `source` on \"\\n\" KEEPING every element — including the trailing \"\" of a
   newline-terminated file and any \\r of CRLF endings. `clojure.string/split-lines`
   drops both, which silently strips the file's final newline (and de-CRLFs it) from
   any line surgery done on top of it."
  [^String source]
  (vec (str/split source #"\n" -1)))

(defn- slice-lines
  "Inclusive 0-based `[start end]` line slice of `source` as a string."
  [^String source ^long start ^long end]
  (->> (split-keep-lines source)
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
     (split-keep-lines source)

     before
     (subvec lines 0 (min s (count lines)))

     after
     (vec (subvec lines (min (inc e) (count lines))))

     ;; seam = end of `before` meets start of `after`. If both sides are blank
     ;; (the node had a blank line above AND below), drop one so a single blank
     ;; remains — local to this seam, nothing else. A LONE trailing "" is not a
     ;; blank line, it is the file's final newline: never collapse that away.
     after
     (if (and (str/blank? (str (last before))) (> (count after) 1) (str/blank? (str (first after))))
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
                  (throw (ex-info
                           (str "Unknown language for " path " — no structural edit is possible.")
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
               (or (zipper/describe-syntax-errors language code)
                   ;; `code` parses clean alone → the fault is at the seam.
                   (str
                     "the replacement parses fine on its own, so the fault is at the INSERTION SEAM"
                     " — an enclosing delimiter was consumed or duplicated; check the replacement's"
                     " OUTER balance against the node it replaces, not its inside."))
               (catch Throwable _ nil)))]

          (throw (ex-info (cond-> raw
                            hint
                            (str "\n" hint))
                          {:type :ext.foundation.editing/struct-edit-failed
                           :op op
                           :target target
                           :language language}
                          e)))))))
(defn- occurrence-entries
  "Enrich the source-ordered `hits` of ONE identifier with `defs` — that same
   identifier's definitions — into the entry list `occurrences` returns."
  [defs hits]
  (let
    [;; claim: def → index of the first still-unclaimed hit inside its span.
     claimed
     (reduce (fn [acc d]
               (let
                 [lo (long (:line d))
                  hi (long (:end-line d))]

                 (if-let
                   [i (first
                        (keep-indexed
                          (fn [i ^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
                            (when (and (not (contains? acc i)) (<= lo (.line h) hi)) i))
                          hits))]
                   (assoc acc i d)
                   acc)))
             {}
             defs)]
    (vec (map-indexed (fn [i ^dev.kreuzberg.treesitterlanguagepack.StructuralApi$ReferenceHit h]
                        (let [base {:line (.line h)}]
                          (if-let [d (get claimed i)]
                            (assoc base
                              :is-definition true
                              :kind (:kind d)
                              :visibility (:visibility d)
                              :signature (:signature d)
                              :doc (:doc d)
                              :end-line (:end-line d))
                            base)))
                      hits))))

(defn- reference-entries
  "Enrich ONE file's raw `{name [ReferenceHit …]}` reference map — from either
   `findReferences` form, single-file or many-file — into `{name [entry …]}`, a
   name with no hit simply absent. ONE definition walk serves the whole map."
  [^String source ^String language refs]
  (let [defs-by-name (group-by :name (index/definitions source language))]
    (persistent!
      (reduce (fn [acc e]
                (let [hits (vec (val e))]
                  (if (seq hits)
                    (assoc! acc (key e) (occurrence-entries (get defs-by-name (key e)) hits))
                    acc)))
              (transient {})
              refs))))

(defn occurrences-in
  "The BATCH form of `occurrences`: every occurrence of EACH identifier in
   `names` in `path`, as `{name [entry …]}` — a name that never occurs is simply
   absent. Entries are exactly what `occurrences` returns; see its docstring.

   ONE parse, ONE line split and ONE definition walk serve the whole batch, so
   the cost tracks the FILE, not the name count. Per-name calls re-parse `source`
   for every name, which is what made tracing N names over M files quadratic —
   use this whenever more than one name is traced through the same file.

   Tracing the same `names` through MANY files? Call `occurrences-in-files`: it
   hands the whole file set to the pack in one parallel batch."
  [path source names]
  (let [wanted (into [] (comp (map str) (distinct)) names)]
    (if-let [language (when (seq wanted) (index/detect-language path))]
      (reference-entries
        source
        language
        (StructuralApi/findReferences ^String source ^String language ^java.util.Collection wanted))
      {})))

(defn occurrences
  "Every occurrence of identifier `name` in `path` — the DEFINITION occurrences
   ENRICHED — as ONE list (empty if none / unknown language):

     {:line}                                                       ; a plain use
     {:line :is-definition true :kind :visibility :signature       ; a DEFINITION
      :doc :end-line}                                              ;   span = :line..:end-line

   Every entry's SOLE position is its 1-based `:line` (no redundant :column/byte
   fields). A use is just that line; a definition also carries its kind /
   visibility (public|private) / signature / doc-gist and an `:end-line`
   (`:line`..`:end-line` is the whole def). Syntactic (tree-sitter identifier
   boundaries, no scope resolution — so N same-named definitions are each marked).

   Definition detection: the FIRST occurrence inside each definition's
   `:line`..`:end-line` span IS its declaration name (findReferences
   returns hits in source order), so it survives decorators / attributes above it.

   Tracing SEVERAL names through the same file? Call `occurrences-in` once
   instead of this per name."
  [path source name]
  ;; Cheap reject: an identifier that never occurs as raw text in `source` has no
  ;; reference AND no definition, so the whole batch below is skippable.
  (if (str/includes? source name) (get (occurrences-in path source [name]) name []) []))

;; -----------------------------------------------------------------------------
;; Batch scanning — the fan-out lives in the PACK, next to the parse.
;;
;; `StructuralApi/mapParallel` and the many-file `findReferences` own the worker
;; pool, the request ordering and the per-file failure rows, so every JVM consumer
;; of the pack gets the same scheduler, and vis keeps only what is vis's: which
;; paths, how to read them (`safe-path` confinement) and the vis entry shape.
;; -----------------------------------------------------------------------------

(defn scan-mapv
  "`mapv` over `items` across the pack's scan pool, in REQUEST ORDER.

   Straight delegation to `StructuralApi/mapParallel`: workers pull the next index
   off a shared cursor, so one huge file cannot strand a worker while the others
   idle. The first exception is rethrown AS THROWN (never wrapped in an
   `ExecutionException`) so a tool's `:on-error-fn` still sees the original
   `ex-info`; every worker is awaited, so no task outlives the call.

   The caller's thread bindings are CONVEYED into every worker, exactly as
   `future`/`pmap` convey theirs. The pool is plain Java, so without this `f` runs
   with the ROOT bindings: `safe-path` would read the per-turn workspace roots as
   unbound and refuse a path the calling thread accepts — and, because a one-item
   batch runs inline, indexing two files would reject what indexing one allowed.
   Each worker restores the frame it found, so a long-lived pooled thread never
   inherits the previous caller's bindings."
  [f items]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (vec (StructuralApi/mapParallel
           ^java.util.List (vec items)
           (reify
             java.util.function.Function
               (apply [_ item]
                 (let [prev (clojure.lang.Var/getThreadBindingFrame)]
                   (clojure.lang.Var/resetThreadBindingFrame frame)
                   (try (f item) (finally (clojure.lang.Var/resetThreadBindingFrame prev))))))))))

(defn occurrences-in-files
  "`occurrences-in` over MANY paths at once — one `{:path :occurrences}` map per
   path, in REQUEST ORDER, traced in ONE pack batch.

   `read-fn` turns a path into its source, so the CALLER keeps path confinement
   (vis resolves through `safe-path`) while the parse and the fan-out stay in the
   pack: the reads run on the pack's pool, then every readable file goes into a
   single `StructuralApi/findReferences` over `FileSource`s, which resolves each
   language once up front and walks the files in parallel. Each file is read AND
   PARSED once per call, not once per name.

   TOTAL per path — a read, language or parse failure becomes `{:path :error}`
   (the message) instead of failing the batch, so one unreadable file cannot sink
   a repo-wide trace. Returns `[]` when `names` is empty."
  [paths names read-fn]
  (let [wanted (into [] (comp (map str) (remove str/blank?) (distinct)) names)]
    (if (seq wanted)
      (let
        [prepared (scan-mapv (fn [path]
                               (try (if-let [language (index/detect-language path)]
                                      {:path path :language language :source (read-fn path)}
                                      {:path path :occurrences {}})
                                    (catch Exception e
                                      {:path path :error (or (ex-message e) (str (class e)))})))
                             paths)
         ;; Only files vis could read AND name a language for reach the pack; the
         ;; rest already carry their own row, so the batch is never asked to fail.
         scanned (into []
                       (keep-indexed (fn [i p]
                                       (when (:source p) i)))
                       prepared)
         rows (zipmap scanned
                      (StructuralApi/findReferences
                        ^java.util.List
                        (mapv (fn [p]
                                (StructuralApi$FileSource. (:path p) (:language p) (:source p)))
                              (filterv :source prepared))
                        ^java.util.Collection wanted))]

        (scan-mapv
          (fn [[i p]]
            (if-let [^StructuralApi$FileReferences row (get rows i)]
              (try (if (.isFailed row)
                     {:path (:path p) :error (.error row)}
                     {:path (:path p)
                      :occurrences (reference-entries (:source p) (:language p) (.references row))})
                   (catch Exception e {:path (:path p) :error (or (ex-message e) (str (class e)))}))
              p))
          (vec (map-indexed vector prepared))))
      [])))
