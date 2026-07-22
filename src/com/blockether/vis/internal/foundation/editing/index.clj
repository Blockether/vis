(ns com.blockether.vis.internal.foundation.editing.index
  "Structural INDEX: a high-level, line-ranged skeleton of a source file
   produced via tree-sitter (com.blockether/tree-sitter-language-pack, which
   sources Clojure from our own grammar fork).

   Every item carries FULL anchors — the same `<lineno>:<hash>` anchors `cat`
   emits and `patch` consumes — for its first and last line, so you can replace
   a whole definition straight from the index with one
   `patch([{path P, from_anchor start, to_anchor end, replace …}])`, no intermediate
   `cat`. Each line is:

     <kind> <name>  <signature>  @<start-anchor>..<end-anchor>

   e.g.

     class Greeter  @3:a1b..7:c2d
       function hello  @6:1b2..7:c2d
     function main  @9:3c4..10:5d6

   Read it BEFORE `cat`: a cheap map of a file so you jump straight to the right
   range (and its anchors) instead of reading the whole file.

   Requiring this namespace also requires the native resolver, which selects the
   right per-platform FFI library at runtime."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack TreeSitterLanguagePack ProcessConfig ProcessResult
            StructureItem Span ImportInfo DocstringInfo]))

(def ^:private extra-extension->language
  "Clojure-family file extensions the pack's grammar table does NOT map, but that
   the `clojure` grammar parses cleanly — EDN is a subset of the Clojure reader,
   so `deps.edn` / `vis.edn` / config data get real structural editing (struct_node,
   node replace) instead of a refused-`patch` fallback. Consulted ONLY when the
   pack's own `detectLanguageFromPath` returns nil, so it never overrides the
   pack. Drop an entry here once the pack ships it on the `clojure` grammar."
  {"edn" "clojure"})

(defn- path-extension
  "Lower-cased extension of `path`'s final segment (no leading dot), or nil when
   the file name has none."
  [^String path]
  (let
    [name
     (str/replace path #"^.*[/\\]" "")

     dot
     (.lastIndexOf name ".")]

    (when (pos? dot) (str/lower-case (subs name (inc dot))))))

(defn detect-language
  "tree-sitter language name for `path` (by extension/shebang), or nil. NOTE: the
   pack recognizes HUNDREDS of grammars, including prose/markup — `.txt` maps to
   `vimdoc` (Vim `:help` files), `.md`→markdown, `.csv`→csv — which parse WITH error
   nodes on ordinary content. For 'is a syntax error meaningful here?' use
   `code-language`, not this.

   Falls back to `extra-extension->language` (currently `.edn`→`clojure`) ONLY
   when the pack returns nil, covering Clojure-family extensions the pack's table
   omits so their files still get structural editing."
  [^String path]
  (or (TreeSitterLanguagePack/detectLanguageFromPath path)
      (get extra-extension->language (path-extension path))))

(def code-languages
  "Curated allowlist of tree-sitter languages vis treats as CODE — where a parse
   ERROR means a genuinely broken file, worth refusing an edit over. Deliberately
   EXCLUDES the pack's prose / markup / loose grammars (vimdoc — remember `.txt`
   maps to it! — markdown, rst, csv, html, xml, ini …) that carry error nodes on
   normal content and would fire false positives. Real programming languages plus
   the STRICT structured-config formats (json/yaml/toml). This is the vetted subset
   the syntax guard runs on; extend it as vis takes on more languages."
  #{"clojure" "python" "rust" "javascript" "typescript" "tsx" "java" "kotlin" "go" "ruby" "c" "cpp"
    "csharp" "php" "scala" "swift" "dart" "zig" "lua" "bash" "elixir" "haskell" "ocaml" "elm"
    "julia" "r" "perl" "vim" "json" "yaml" "toml"})

(defn code-language
  "The tree-sitter language for `path` IFF it is a vetted CODE language
   (`code-languages`) — else nil. Use this (not `detect-language`) wherever a
   syntax-error refusal must be meaningful: prose / markup / data / `.log` /
   extensionless files all return nil, so they're never falsely guarded."
  [^String path]
  (let [l (detect-language path)]
    (when (contains? code-languages l) l)))

(defn- root-cause
  "Deepest cause in `t`'s exception chain — the throwable that actually
   explains the failure, below any wrapper layers."
  ^Throwable [^Throwable t]
  (loop [t t]
    (if-let [c (.getCause t)]
      (recur c)
      t)))

(defn- process-source
  "`ProcessResult` for `source` parsed as `language`, with STRUCTURE and IMPORTS
   populated in a SINGLE pass (both default on; requested explicitly).

   The pack surfaces every native/decode failure as one opaque
   `TreeSitterLanguagePackRsException: FFI call failed`, burying the real
   reason in a nested cause (e.g. a `StructureItem` field the Java binding
   can't deserialize). A single unparseable item would otherwise nuke the
   whole index with zero signal, so we rethrow with the deepest cause's
   message attached — the actionable detail — while keeping the language
   and original chain for callers."
  ^ProcessResult [^String source ^String language]
  (let
    [cfg (-> (ProcessConfig/builder)
             (.withLanguage language)
             (.withStructure true)
             (.withImports true)
             (.withDocstrings true)
             (.build))]
    (try (TreeSitterLanguagePack/process source cfg)
         (catch Throwable t
           (let [cause (root-cause t)]
             (throw (ex-info (str "tree-sitter structure extraction failed for language " language
                                  ": " (.getMessage cause))
                             {:language language :cause-type (.getName (class cause))}
                             t)))))))

(defn- structure-items
  "List<StructureItem> for `source` parsed as `language` (empty when none)."
  [^String source ^String language]
  (or (.structure (process-source source language)) []))

(def ^:private kind-aliases
  "Terse canonical names for the pack's verbose `StructureKind`s — what `:kind`
   carries in the `struct_index`/`struct_occurrences` DATA (and what struct_patch's `kind`
   disambiguator matches). Only `function` → `fn` so far; all else passes through."
  {"function" "fn"})

(def ^:private kind-aliases-inverse
  "Reverse of `kind-aliases`: terse kind → the pack's raw StructureKind name."
  (into {} (map (juxt val key)) kind-aliases))

(defn- canonical-kind
  "Normalise a raw kind to the terse canonical form the DATA carries
   (`function` → `fn`), lower-cased; nil/blank → nil."
  [kind]
  (when-some
    [k (some-> kind
               str
               str/lower-case
               not-empty)]
    (get kind-aliases k k)))

(defn pack-kind
  "Inverse of `canonical-kind`: map a terse `:kind` (`fn`) BACK to the pack's raw
   StructureKind name (`function`) so struct_patch's `kind` disambiguator matches
   the pack. Unknown/other kinds pass through unchanged; nil/blank → nil."
  [kind]
  (when-some
    [k (some-> kind
               str
               str/lower-case
               not-empty)]
    (get kind-aliases-inverse k k)))

(defn node-span
  "0-based inclusive `[start-line end-line]` of the TOP-LEVEL structural node named
   `target` (optionally narrowed by `kind`, case-insensitive), or nil if not found.
   Used by the structural `move` op to extract a node's exact source text by name."
  [^String source ^String language ^String target kind]
  (let [k (canonical-kind kind)]
    (some (fn [^StructureItem it]
            (when (and (= target (.name it)) (or (nil? k) (= k (canonical-kind (.kind it)))))
              (let [^Span span (.span it)]
                [(.startLine span) (.endLine span)])))
          (structure-items source language))))

(defn- flatten-items
  "Depth-first flatten of the structural items (top-level + nested children),
   mirroring the pack engine's own outline flattening used by its name+kind
   locator."
  [items]
  (mapcat (fn [^StructureItem it]
            (cons it (flatten-items (or (.children it) []))))
          items))

(defn resolve-edit-kind
  "The raw StructureKind name to hand the pack engine's name+kind locator for
   `target`, made resilient to an over-specific or mismatched `kind` (e.g. the
   source def-form head `defdescribe`/`deftest`, which the pack reports as `fn`):
   - no `kind` → nil (locate by name alone);
   - `kind` matches a def named `target` → that pack kind (normal disambiguation);
   - `kind` matches NO def named `target`, yet exactly ONE def carries that name
     → nil, so a wrong/over-specific kind never blocks an unambiguous by-name edit;
   - otherwise the pack kind unchanged (let the engine report the real miss or
     ambiguity)."
  [^String source ^String language ^String target kind]
  (let
    [raw (some-> kind
                 name
                 pack-kind)]
    (if (nil? raw)
      nil
      (let
        [k (canonical-kind kind)
         named (filter (fn [^StructureItem it]
                         (= target (.name it)))
                       (flatten-items (structure-items source language)))
         kind-match? (boolean (some (fn [^StructureItem it]
                                      (= k (canonical-kind (.kind it))))
                                    named))]

        (cond kind-match? raw
              (= 1 (count named)) nil
              :else raw)))))

(defn- line-text
  [lines ln]
  ;; lines is 0-based; ln is 1-based.
  (nth lines (dec (long ln)) ""))

(def ^:private ^:dynamic *docstrings*
  "Result-level docstrings for the file currently being indexed, bound by the
   entry points so `doc-snippet` can fall back to them. For languages whose
   doc lives INSIDE the body (Python triple-quote, …) the structure tagger
   leaves `docComment` empty and the pack surfaces the doc via this separate
   list instead — keyed to a def by name + span. nil outside an index run."
  nil)

(defn- strip-doc-delims
  "Strip the surrounding string delimiters from a raw docstring so the gist
   reads clean — a Python triple-quote / single-quote body (with an optional
   r/b/u/f prefix). A leading-comment `docComment` has no delimiters, so this
   only ever fires on the docstrings-list fallback."
  [^String s]
  (-> s
      str/trim
      (str/replace #"(?s)^[rRbBuUfF]{0,3}(\"\"\"|'''|\"|')" "")
      (str/replace #"(\"\"\"|'''|\"|')\s*$" "")))

(defn- docstring-for
  "Docstring text for `it` from the result-level `*docstrings*` list (the doc a
   language carries inside the body, e.g. Python), matched by name + span
   containment and de-delimited. nil when nothing matches."
  [^StructureItem it]
  (when-let [ds *docstrings*]
    (let
      [^Span isp (.span it)
       is (.startLine isp)
       ie (.endLine isp)
       nm (.name it)]

      (some
        (fn [^DocstringInfo d]
          (let [^Span dsp (.span d)]
            (when (and (= nm (.associatedItem d)) (>= (.startLine dsp) is) (<= (.endLine dsp) ie))
              (strip-doc-delims (.text d)))))
        ds))))

(defn- doc-snippet
  "First non-blank line of a definition's doc string, trimmed and clipped to a
   single readable gist (nil when there is none). The pack populates `docComment`
   from the def's own doc string / leading comment — Clojure docstrings, and the
   `//` / JSDoc block written directly above a JS/TS/TSX def."
  [^StructureItem it]
  (when-let [d (or (.docComment it) (docstring-for it))]
    (when-let
      [line (->> (str/split-lines d)
                 (map str/trim)
                 (remove str/blank?)
                 first)]
      (if (> (count line) 72) (str (subs line 0 71) "…") line))))

(defn- fmt-item
  [lines ^StructureItem it depth]
  (let
    [^Span span
     (.span it)

     ;; tree-sitter rows are 0-based; report 1-based inclusive line ranges.
     start
     (inc (.startLine span))

     end
     (inc (.endLine span))

     kind
     (some-> (.kind it)
             str
             str/lower-case)

     ;; The pack reports the clean name + a structured `visibility`; the
     ;; skeleton surfaces only the noteworthy `private` marker — public is
     ;; the default, so it stays implicit and out of the way.
     private?
     (= "private"
        (some-> (.visibility it)
                str
                str/lower-case))

     nm
     (.name it)

     sig
     (some-> (.signature it)
             str/trim
             not-empty)

     indent
     (apply str (repeat depth "  "))

     label
     (str/trim
       (str kind (when private? " private") (when nm (str " " nm)) (when sig (str "  " sig))))

     from
     (patch/line-anchor start (line-text lines start))

     to
     (patch/line-anchor end (line-text lines end))

     doc
     (doc-snippet it)]

    ;; The anchors already carry the line numbers, so no separate [start-end].
    ;; A doc string, when present, rides on an indented continuation line.
    (str indent label "  @" from ".." to (when doc (str "\n" indent "    " (pr-str doc))))))

(defn- walk-items
  [lines items ^long depth]
  (mapcat (fn [^StructureItem it]
            (cons (fmt-item lines it depth) (walk-items lines (or (.children it) []) (inc depth))))
          items))

(defn- item->def
  [lines ^StructureItem it depth]
  (let
    [^Span span
     (.span it)

     ;; tree-sitter rows are 0-based; anchors carry 1-based line numbers (like
     ;; the skeleton + `cat`).
     start
     (inc (.startLine span))

     end
     (inc (.endLine span))]

    {:name (.name it)
     :kind (canonical-kind (.kind it))
     :visibility (some-> (.visibility it)
                         str
                         str/lower-case
                         not-empty)
     :signature (some-> (.signature it)
                        str/trim
                        not-empty)
     :doc (doc-snippet it)
     ;; The def's span as patch-ready `lineno:hash` anchors — the SOLE position
     ;; (the lineno lives in the anchor, so no redundant start-line/end-line). One
     ;; hop `outline` DATA → `patch`; same anchors the skeleton (`@from..to`) + `cat`
     ;; emit; `patch/anchor->line` recovers the number when arithmetic is needed.
     :anchor (patch/line-anchor start (line-text lines start))
     :end-anchor (patch/line-anchor end (line-text lines end))
     ;; Nesting depth (0 = top-level). The flat list drops parent linkage; depth
     ;; lets a consumer rebuild the tree. struct_occurrences rows are flat (no depth);
     ;; every other def field is name-for-name the SAME as a struct_occurrences def row.
     :depth depth}))

(defn- defs-tree
  "Flatten structure `items` (from `structure-items`) into depth-tagged def rows
   via `item->def` — the shared walk behind `definitions` and `file-index`."
  [lines items]
  (letfn [(walk [items depth]
            (mapcat (fn [^StructureItem it]
                      (cons (item->def lines it depth)
                            (walk (or (.children it) []) (inc (long depth)))))
                    items))]
    (walk items 0)))

(defn definitions
  "The DATA behind `file-skeleton`: every definition in `source` (parsed as
   `language`), flattened across nesting, as
   `[{:name :kind :visibility :signature :doc :anchor :end-anchor :depth} …]`
   where the def's span is patch-ready `lineno:hash` anchors — the SOLE position
   (no redundant start/end line; `patch/anchor->line` recovers the number). So
   `struct_index` → `patch` needs no re-cat. With `name`, only the definitions with that
   exact name (there may be several — same name in different scopes). Empty when
   the language is unsupported or nothing structural was found."
  ([source language] (definitions source language nil))
  ([source language name]
   (let
     [res
      (process-source source language)

      items
      (or (.structure res) [])

      lines
      (str/split-lines source)]

     (binding [*docstrings* (.docstrings res)]
       (cond->> (defs-tree lines items)
         (some? name)
         (filterv #(= name (:name %))))))))

(defn file-skeleton
  "Skeleton string for `path` (items + line ranges + full start..end anchors),
   or nil when the language is unsupported or nothing structural was found.
   `source` may be passed to avoid a re-read (e.g. for unsaved buffers)."
  ([path] (file-skeleton path (slurp path)))
  ([path source]
   (when-let [language (detect-language path)]
     (let
       [res (process-source source language)
        items (or (.structure res) [])]

       (when (seq items)
         (binding [*docstrings* (.docstrings res)]
           (let [lines (str/split-lines source)]
             (str/join "\n" (walk-items lines items 0)))))))))

(defn- basename
  "Final path segment of `path` — its display name in the index header."
  [^String path]
  (str/replace path #"^.*[/\\]" ""))

(defn- indent-block
  "Prefix every non-blank physical line of `s` with `prefix` — indents a whole
   def entry (doc-continuation lines included) under a section header."
  [^String prefix ^String s]
  (->> (str/split-lines s)
       (map (fn [line]
              (if (str/blank? line) line (str prefix line))))
       (str/join "\n")))

(defn- import->row
  "One `ImportInfo` → a structured import row: `{:source :items :alias :wildcard
   :anchor}`. `:anchor` is the statement's first-line `lineno:hash` — the same
   patch-ready handle a def row carries, so an import line is a jump target too.
   `:items`/`:alias`/`:wildcard` are the pack's parsed detail when a grammar fills
   them (some only populate `:source` with the raw statement text)."
  [lines ^ImportInfo imp]
  (let
    [start
     (inc (.startLine (.span imp)))

     items
     (vec (or (.items imp) []))

     alias
     (some-> (.alias imp)
             str/trim
             not-empty)]

    (cond->
      {:source (some-> (.source imp)
                       str/trim
                       not-empty)
       :anchor (patch/line-anchor start (line-text lines start))}
      (seq items)
      (assoc :items items)

      alias
      (assoc :alias alias)

      (.isWildcard imp)
      (assoc :wildcard true))))

(defn- import-line
  "One import row → skeleton line: `<source>[ :as <alias>][ (<items>|*)]  @<anchor>`."
  [{:keys [source items alias wildcard anchor]}]
  (str "  "
       source
       (when alias (str " :as " alias))
       (cond wildcard " (*)"
             (seq items) (str " (" (str/join " " items) ")"))
       "  @"
       anchor))

(def ^:private kind->section
  "Section-header label (pluralised) for a definition `kind` — the Maki-style
   bucket the index groups same-kind defs under, so a kind is named ONCE per
   section header instead of repeated on every def row."
  {"namespace" "namespaces"
   "constant" "constants"
   "function" "fn"
   "macro" "macros"
   "class" "classes"
   "method" "methods"
   "interface" "interfaces"
   "struct" "structs"
   "enum" "enums"
   "field" "fields"
   "property" "properties"
   "type" "types"
   "trait" "traits"
   "variable" "variables"
   "module" "modules"
   "protocol" "protocols"
   "other" "other"})

(defn- section-label
  "Section header for `kind` — a curated plural, else a naive `<kind>s`."
  [^String kind]
  (or (kind->section kind) (str kind "s")))

(defn- item-line
  "One definition → an anchored skeleton line WITHOUT its kind (the kind lives in
   the enclosing section header): `<indent>[private ]<name>[  <sig>]  @from..to`,
   with a `pr-str`'d doc gist on an indented continuation line when present."
  [lines ^StructureItem it ^long depth]
  (let
    [^Span span
     (.span it)

     start
     (inc (.startLine span))

     end
     (inc (.endLine span))

     private?
     (= "private"
        (some-> (.visibility it)
                str
                str/lower-case))

     nm
     (.name it)

     sig
     (some-> (.signature it)
             str/trim
             not-empty)

     indent
     (apply str (repeat depth "  "))

     label
     (str/trim (str (when private? "private ") nm (when sig (str "  " sig))))

     from
     (patch/line-anchor start (line-text lines start))

     to
     (patch/line-anchor end (line-text lines end))

     doc
     (doc-snippet it)]

    (str indent label "  @" from ".." to (when doc (str "\n" indent "    " (pr-str doc))))))

(defn- count-items
  "Total definitions in the structure tree `items` (children included)."
  [items]
  (reduce (fn [n ^StructureItem it]
            (+ (long n) 1 (long (count-items (or (.children it) [])))))
          0
          items))

(defn- grouped-items
  "Render sibling `items` grouped by kind: a `<section>:` header per kind (in
   first-appearance order), each def under it as an anchored, kind-less
   `item-line`; a def's own children recurse one level deeper, themselves
   grouped. Returns a seq of skeleton lines — no kind word repeated per row."
  [lines items ^long depth]
  (let
    [indent
     (apply str (repeat depth "  "))

     kind-of
     (fn [^StructureItem it]
       (or (some-> (.kind it)
                   str
                   str/lower-case)
           "other"))

     by-kind
     (group-by kind-of items)]

    (mapcat (fn [k]
              (cons (str indent (section-label k) ":")
                    (mapcat (fn [^StructureItem it]
                              (cons (item-line lines it (inc depth))
                                    (let [children (or (.children it) [])]
                                      (when (seq children)
                                        (grouped-items lines children (+ depth 2))))))
                            (get by-kind k))))
            (distinct (map kind-of items)))))

(defn- index-skeleton
  "Maki-style skeleton string: a `<file> · <language> · <N> lines` header, an
   optional anchored `imports:` section, then the `definitions:` tree GROUPED by
   kind (one `<section>:` header per kind, so the kind is not repeated on every
   row) and structure-nested — every line still carrying its `lineno:hash`
   anchors."
  [path language line-count lines items import-rows]
  (let
    [header
     (str (basename path) " · " language " · " line-count (if (= 1 line-count) " line" " lines"))

     imports-sec
     (when (seq import-rows)
       (str "imports (" (count import-rows) "):\n" (str/join "\n" (map import-line import-rows))))

     defs-lines
     (grouped-items lines items 0)

     defs-sec
     (when (seq defs-lines)
       (str "definitions (" (count-items items)
            "):\n" (indent-block "  " (str/join "\n" defs-lines))))]

    (str/join "\n\n" (remove nil? [header imports-sec defs-sec]))))

(defn- span-overlaps?
  "Whether a 0-based tree-sitter `Span` intersects the 1-based inclusive line
   range `[lo hi]` — the overlap test behind `file-index`'s optional `range`."
  [^Span span ^long lo ^long hi]
  (let
    [start
     (inc (.startLine span))

     end
     (inc (.endLine span))]

    (and (<= start hi) (>= end lo))))

(defn- span-overlaps-any?
  "Whether `span` intersects ANY window in `windows` (each a 1-based inclusive
   `[lo hi]`) — the multi-range overlap test behind `file-index`'s `ranges`."
  [^Span span windows]
  (boolean (some (fn [[lo hi]]
                   (span-overlaps? span lo hi))
                 windows)))

(defn- normalize-windows
  "Coerce `range` into a seq of ordered 1-based inclusive `[lo hi]` windows, or
   nil for none. Accepts a SINGLE `[lo hi]` pair OR a COLLECTION of such pairs;
   malformed/empty pairs are dropped."
  [range]
  (when (seq range)
    (let [pairs (if (sequential? (first range)) range [range])]
      (seq (keep (fn [p]
                   (when (and (sequential? p) (first p) (second p))
                     (let
                       [a (long (first p))
                        b (long (second p))]

                       [(min a b) (max a b)])))
                 pairs)))))

(defn file-index
  "Maki-style structural INDEX of `path`, produced in a SINGLE tree-sitter pass:

     {:language str :line-count int
      :skeleton str        ; header + imports + nested definitions, all anchored
      :definitions [row …] ; machine rows (== `definitions`) — anchor/end-anchor
      :imports [row …]}    ; machine import rows, each anchored

   nil when the language is unsupported, or the file has no imports and nothing
   structural. `source` may be passed to avoid a re-read (e.g. unsaved buffers).
   `range` narrows the index to the imports and TOP-LEVEL definitions whose span
   intersects it — either a SINGLE 1-based inclusive `[lo hi]` (either order) or a
   COLLECTION of such windows `[[lo hi] …]` (a def kept when it hits ANY window).
   Each kept def's own children stay intact; `:line-count` still reports the WHOLE
   file. With a
   range set, a hit-nothing window still returns a (header-only) index rather
   than nil, so the caller can tell 'empty window' from 'unsupported'."
  ([path] (file-index path (slurp path) nil))
  ([path source] (file-index path source nil))
  ([path source range]
   (when-let [language (detect-language path)]
     (let
       [res (process-source source language)
        all-items (or (.structure res) [])
        all-imps (or (.imports res) [])
        windows (normalize-windows range)
        items (if windows
                (filterv #(span-overlaps-any? (.span ^StructureItem %) windows) all-items)
                all-items)
        imps
        (if windows (filterv #(span-overlaps-any? (.span ^ImportInfo %) windows) all-imps) all-imps)
        lines (str/split-lines source)]

       (when (or (seq items) (seq imps) windows)
         (binding [*docstrings* (.docstrings res)]
           (let [import-rows (mapv #(import->row lines %) imps)]
             {:language language
              :line-count (count lines)
              :skeleton (index-skeleton path language (count lines) lines items import-rows)
              :definitions (defs-tree lines items)
              :imports import-rows})))))))
