(ns com.blockether.vis.internal.foundation.editing.index
  "Structural INDEX: a high-level, line-ranged skeleton of a source file
   produced via tree-sitter (com.blockether/tree-sitter-language-pack, which
   sources Clojure from our own grammar fork).

   Every item carries its FIRST and LAST 1-based line, so a definition's whole
   span is readable straight from the index — no intermediate read. Each line is:

     <kind> <name>  <signature>  @<start-line>..<end-line>

   e.g.

     class Greeter  @3..7
       function hello  @6..7
     function main  @9..10

   Read it FIRST: a cheap map of a file so you jump straight to the right range
   instead of reading the whole file.

   Requiring this namespace also requires the native resolver, which selects the
   right per-platform FFI library at runtime."
  (:require [clojure.string :as str]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack TreeSitterLanguagePack ProcessConfig ProcessResult
            StructureItem Span ImportInfo DocstringInfo]))

(def ^:private extra-extension->language
  "Clojure-family file extensions the pack's grammar table does NOT map, but that
   the `clojure` grammar parses cleanly — EDN is a subset of the Clojure reader,
   so `deps.edn` / `vis.edn` / config data get real structural editing (struct_nodes,
   node replace) instead of a refused-`struct_patch` fallback. Consulted ONLY when the
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
    "csharp" "php" "scala" "swift" "dart" "zig" "lua" "bash" "elixir" "haskell" "ocaml"
    "ocaml_interface" "elm" "julia" "r" "perl" "vim" "groovy" "nix" "hcl" "terraform" "graphql"
    "svelte" "vue" "json" "yaml" "toml"})

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

(def ^:private parse-cache-size
  "How many parsed files `parse-cache` and `defs-cache` keep before evicting the oldest.

   256 covers a whole ordinary repo-wide request (the former 64-entry bound thrashed
   completely on this repo's 140 Clojure files when indexing was followed by occurrence
   tracing). Entries retain roughly 38KB each here, so each cache stays under about 10MB."
  256)

(def ^:private parse-cache
  "CONTENT-ADDRESSED cache of `process-source` results: `[language source]` →
   `ProcessResult`, as `{:m {k res} :order [k …]}` (FIFO, bounded by
   `parse-cache-size`).

   The native parse DOMINATES indexing — measured over 140 repo files / 99k lines:
   498 ms for `file-index`, of which 436 ms is the pack call and 277 ms is a parse
   that extracts NOTHING. Every real flow then re-parses the same bytes several
   times: `struct_index` → `struct_patch` on that file, `resolve-edit-kind` before
   the pack's own edit, `definitions` per name — and above all
   `include_occurrences`, which walks every declared name × every path (47 names ×
   2 files = 94 parses of 2 files).

   Keyed by the CONTENT, so it can never serve a stale tree: an edited file is a
   different key, and a failed parse throws instead of being remembered."
  (atom {:m {} :order []}))

(defn- remember!
  "Remember `v` under `k` in the FIFO `cache` atom (`{:m {k v} :order [k …]}`),
   evicting the oldest entry past `parse-cache-size`. Returns `v`."
  [cache k v]
  (swap! cache (fn [{:keys [m order] :as c}]
                 (cond (contains? m k) c
                       (>= (count order) (long parse-cache-size)) {:m (-> m
                                                                          (dissoc (nth order 0))
                                                                          (assoc k v))
                                                                   :order (conj (subvec order 1) k)}
                       :else {:m (assoc m k v) :order (conj order k)})))
  v)

(def ^:private defs-cache
  "CONTENT-ADDRESSED cache of the FULL, unfiltered definition rows for
   `[language source]` — same FIFO shape and bound as `parse-cache`.

   `definitions` with a `name` is only a filter over that vector, yet `occurrences`
   asks for ONE name at a time while tracing runs every declared name over every
   indexed path. Without this the flatten + docstring walk is rebuilt, identically,
   once per name per file: 28.5 s of a 122 s 140-file trace."
  (atom {:m {} :order []}))

(defn clear-parse-cache!
  "Drop every cached parse. Only needed to reclaim memory or to measure a cold
   parse — correctness never needs it, since the cache is keyed by content."
  []
  (reset! parse-cache {:m {} :order []})
  (reset! defs-cache {:m {} :order []})
  nil)

(defn- parse-source
  "Uncached `ProcessResult` for `source` parsed as `language`, with STRUCTURE,
   IMPORTS and DOCSTRINGS populated in a SINGLE pass (all requested explicitly).

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

(defn- process-source
  "`ProcessResult` for `source` parsed as `language` — the SINGLE entry point every
   index/edit path goes through, served from the content-addressed `parse-cache`
   when the exact same bytes were parsed recently."
  ^ProcessResult [^String source ^String language]
  (let [k [language source]]
    (if-let [hit (get (:m @parse-cache) k)]
      hit
      (remember! parse-cache k (parse-source source language)))))

(defn- structure-items
  "List<StructureItem> for `source` parsed as `language` (empty when none)."
  [^String source ^String language]
  (or (.structure (process-source source language)) []))

(def ^:private kind-aliases
  "Terse canonical names for the pack's verbose `StructureKind`s — what `:kind`
   carries in the `struct_index` DATA (and what struct_patch's `kind`
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

(defn- item-kind
  "The item's kind WORD, lower-cased. `StructureKind/Other` is a payload-carrying
   Rust variant (`{\"Other\": \"resource\"}` on the wire) that a Java enum cannot
   hold, so the pack now surfaces the payload separately as `.kindLabel` — a
   GraphQL `type`, a Terraform `resource`, an Elixir `Macro`. Prefer it: without
   it every language-specific construct collapses into one indistinguishable
   `other` bucket. nil for a kind-less item."
  [^StructureItem it]
  (or (some-> (.kindLabel it)
              str
              str/trim
              not-empty
              str/lower-case)
      (some-> (.kind it)
              str
              str/lower-case)))

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

(defn- span-end-line
  "0-based LAST CONTENT row of `span`. tree-sitter end positions are exclusive, and
   several grammars let a definition node swallow its terminating newline and even
   the blank rows up to the next sibling (Groovy `command`). Given `lines` those
   trailing blank rows are trimmed too; without them only the column-0 overshoot
   is. Trimming is what keeps a kind-targeted replace from eating the next
   definition."
  (^long [^Span span] (span-end-line nil span))
  (^long [lines ^Span span]
   (let
     [start
      (.startLine span)

      e
      (long (cond-> (.endLine span)
              (and (zero? (.endColumn span)) (> (.endLine span) start))
              dec))]

     (if (nil? lines)
       e
       (loop [e e]
         (if (and (> e start) (str/blank? (nth lines e ""))) (recur (dec e)) e))))))

(defn node-span
  "0-based inclusive `[start-line end-line]` of the TOP-LEVEL structural node named
   `target` (optionally narrowed by `kind`, case-insensitive), or nil if not found.
   Used by the structural `move` op to extract a node's exact source text by name."
  [^String source ^String language ^String target kind]
  (let
    [k
     (canonical-kind kind)

     lines
     (str/split-lines source)]

    (some (fn [^StructureItem it]
            (when (and (= target (.name it)) (or (nil? k) (= k (canonical-kind (item-kind it)))))
              (let [^Span span (.span it)]
                [(.startLine span) (span-end-line lines span)])))
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
                                      (= k (canonical-kind (item-kind it))))
                                    named))]

        (cond kind-match? raw
              (= 1 (count named)) nil
              :else raw)))))

(def ^:private ^:dynamic *docstrings*
  "Result-level docstrings for the file currently being indexed, bound by the
   entry points so `doc-snippet` can fall back to them. For languages whose
   doc lives INSIDE the body (Python triple-quote, …) the structure tagger
   leaves `docComment` empty and the pack surfaces the doc via this separate
   list instead — keyed to a def by name + span.

   Held as the `docstring-index` MAP (associated name → its docstrings in
   source order), never the raw list. nil outside an index run."
  nil)

(defn- docstring-index
  "Group a result's docstrings by `associatedItem` — the exact name
   `docstring-for` matches on — preserving source order inside each group.
   nil when there are none, so the lookup stays a single `when-let`.

   Built ONCE per index run because the lookup is per DEFINITION and both entry
   points render every definition twice (skeleton line + machine row): scanning
   the whole list each time was O(defs x docstrings). Measured on a Python file
   of documented defs: 4 -> 14 -> 54 -> 212 ms across 500/1000/2000/4000 —
   quadrupling per doubling, on top of a parse that only doubles."
  [ds]
  (when (seq ds)
    (group-by (fn [^DocstringInfo d]
                (.associatedItem d))
              ds)))

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
  "Docstring text for `it` from the `*docstrings*` index (the doc a language
   carries inside the body, e.g. Python), matched by name + span containment
   and de-delimited. nil when nothing matches.

   The name match is the MAP lookup; only the handful of docstrings sharing that
   name (`__init__` across classes) is then scanned for span containment, in the
   same source order the flat list had — so the first hit is unchanged."
  [^StructureItem it]
  (when-let [ds *docstrings*]
    (let
      [^Span isp (.span it)
       is (.startLine isp)
       ie (.endLine isp)]

      (some (fn [^DocstringInfo d]
              (let [^Span dsp (.span d)]
                (when (and (>= (.startLine dsp) is) (<= (.endLine dsp) ie))
                  (strip-doc-delims (.text d)))))
            (get ds (.name it))))))

(defn- datum-end
  "Index just past the datum starting at `i`: a balanced `{…}` / `[…]` / `(…)`
   form scanned string- and escape-aware, else a bare whitespace-delimited token."
  ^long [^String s ^long i]
  (let
    [n
     (count s)

     open?
     #{\{ \[ \(}

     close?
     #{\} \] \)}]

    (if (>= i n)
      i
      (if (open? (.charAt s i))
        (loop
          [j
           i

           depth
           0

           in-str?
           false

           esc?
           false]

          (if (>= j n)
            j
            (let [ch (.charAt s j)]
              (cond esc? (recur (inc j) depth in-str? false)
                    (and in-str? (= ch \\)) (recur (inc j) depth true true)
                    in-str? (recur (inc j) depth (not= ch \") false)
                    (= ch \") (recur (inc j) depth true false)
                    (open? ch) (recur (inc j) (inc depth) false false)
                    (close? ch) (if (= 1 depth) (inc j) (recur (inc j) (dec depth) false false))
                    :else (recur (inc j) depth false false)))))
        (loop [j i]
          (if (or (>= j n) (Character/isWhitespace (.charAt s j))) j (recur (inc j))))))))

(defn- clj-meta-head
  "Concatenated `^…` metadata forms sitting between a Clojure `(def…` head token
   and the var NAME, or nil. Balanced and string-aware, so neither a `:doc` key
   inside the VALUE nor a name that also occurs inside the doc TEXT can be
   mistaken for the var's own metadata."
  [^String text]
  (when-let [m (re-find #"^\(def\S*" text)]
    (let
      [n (count text)
       skip-ws (fn ^long [^long k]
                 (loop [k k]
                   (if (and (< k n) (Character/isWhitespace (.charAt text k))) (recur (inc k)) k)))]

      (loop
        [i (long (skip-ws (count m)))
         acc nil]

        (if (and (< i n) (= \^ (.charAt text i)))
          (let [e (min n (datum-end text (inc i)))]
            (recur (long (skip-ws e)) (str acc (subs text i e))))
          acc)))))

(def ^:private def-with-meta-re
  "A `(def…` head whose var COULD carry metadata: either a `^` follows on the same
   line, or the head token ends the line (the rare `(def\n  ^{:doc …} nm` shape).
   Anything else has the var NAME right after the head token, so there is no
   metadata to read.

   This is the cheap reject that keeps `meta-doc` from materialising a definition's
   whole source text: without it the `^{:doc …}` scan cost 31 ms of the 55 ms the
   Clojure layer spends on 140 files — paid by every UNDOCUMENTED def, in every
   language, twice per definition."
  #"^\(def\S*(?:\s*$|\s+\^)")

(defn- meta-doc
  "Docstring carried in a Clojure-family `^{:doc \"…\"}` VAR METADATA map — the one
   doc shape the pack reports nowhere: for `(def ^{:doc \"…\"} nm v)` /
   `(defonce ^:private ^{:doc \"…\"} nm v)` it fills neither `docComment` nor the
   docstrings list, so every metadata-documented var (the sandbox tool vars
   `read_session`, `list_sessions`, `shell`, `mcp_*`, …) indexed blank while
   its whole contract lives in that map.

   Read from `it`'s OWN source span, and only from its metadata head — and only
   once `def-with-meta-re` says the head can have one, so the span text is built
   for the handful of candidates instead of for every def. nil for every other
   shape (and for every non-`(def…` language)."
  [lines ^StructureItem it]
  (let
    [^Span span
     (.span it)

     s
     (long (.startLine span))

     n
     (count lines)]

    (when (and (< s n) (re-find def-with-meta-re (nth lines s "")))
      (let
        [e
         (long (span-end-line lines span))

         text
         (if (vector? lines)
           (str/join "\n" (subvec lines s (min n (inc e))))
           (str/join "\n" (take (- (inc e) s) (drop s lines))))]

        (some-> text
                (clj-meta-head)
                (->> (re-find #"(?s):doc\s+\"((?:[^\"\\]|\\.)*)\""))
                (second))))))

(def ^:private ^:dynamic *doc-memo*
  "Per-run `IdentityHashMap` memo for `doc-snippet`, bound by `file-index` because
   that entry point renders every definition TWICE — once as a skeleton line, once
   as a machine row — and the doc gist is the expensive half (docstrings lookup +
   `^{:doc …}` span scan). Keyed by StructureItem IDENTITY, so it is valid only for
   the one `lines`/result pair it was bound around. nil elsewhere: the other entry
   points render each definition once."
  nil)

(defn- first-gist-line
  "First non-blank line of `s`, trimmed — scanned in place. A doc comment is often
   dozens of lines and only ONE of them is ever shown, so splitting the whole text
   (and trimming every line) to keep the head was the largest remaining cost of the
   gist."
  ^String [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (when (< i n)
        (let
          [nl (.indexOf s "\n" i)
           e (if (neg? nl) n nl)
           line (.trim (.substring s i e))]

          (if (.isEmpty line) (when (< e n) (recur (inc e))) line))))))

(defn- doc-gist
  "Uncached `doc-snippet`."
  [lines ^StructureItem it]
  (when-let [d (or (.docComment it) (docstring-for it) (meta-doc lines it))]
    (when-let [line (first-gist-line d)]
      (if (> (count line) 72) (str (subs line 0 71) "…") line))))

(defn- doc-snippet
  "First non-blank line of a definition's doc string, trimmed and clipped to a
   single readable gist (nil when there is none). The pack populates `docComment`
   from the def's own doc string / leading comment — Clojure docstrings, and the
   `//` / JSDoc block written directly above a JS/TS/TSX def; `docstring-for`
   covers in-body docs (Python); `meta-doc` covers Clojure `^{:doc …}` metadata,
   which the pack surfaces through neither.

   Memoised per item through `*doc-memo*` when one is bound (nil results included,
   so a doc-less def is computed once too)."
  [lines ^StructureItem it]
  (if-let [^java.util.IdentityHashMap memo *doc-memo*]
    (if (.containsKey memo it)
      (.get memo it)
      (let [v (doc-gist lines it)]
        (.put memo it v)
        v))
    (doc-gist lines it)))

(defn- fmt-item
  [lines ^StructureItem it depth]
  (let
    [^Span span
     (.span it)

     ;; tree-sitter rows are 0-based; report 1-based inclusive line ranges.
     start
     (inc (.startLine span))

     end
     (inc (span-end-line lines span))

     kind
     (item-kind it)

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

     doc
     (doc-snippet lines it)]

    ;; A doc string, when present, rides on an indented continuation line.
    (str indent label "  @" start ".." end (when doc (str "\n" indent "    " (pr-str doc))))))

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

     ;; tree-sitter rows are 0-based; the rows report 1-based line numbers (like
     ;; the skeleton).
     start
     (inc (.startLine span))

     end
     (inc (span-end-line lines span))]

    {:name (.name it)
     :kind (canonical-kind (item-kind it))
     :visibility (some-> (.visibility it)
                         str
                         str/lower-case
                         not-empty)
     :signature (some-> (.signature it)
                        str/trim
                        not-empty)
     :doc (doc-snippet lines it)
     ;; The def's span as 1-based inclusive line numbers — the SOLE position, and
     ;; the same pair the skeleton prints as `@from..to`.
     :line start
     :end-line end
     ;; Nesting depth (0 = top-level). The flat list drops parent linkage; depth
     ;; lets a consumer rebuild the tree. `name`-mode rows are flat (no depth);
     ;; every other def field is name-for-name the SAME as a `name`-mode def row.
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

(defn- all-definitions
  "EVERY definition row in `source` (parsed as `language`) — the unfiltered vector
   `definitions` is a view over — memoised per `[language source]` in `defs-cache`.

   The rows are forced INSIDE the `*docstrings*` binding, so the docs they carry
   are always the ones for this parse."
  [source language]
  (let [k [language source]]
    (if-let [hit (get (:m @defs-cache) k)]
      hit
      (let
        [res (process-source source language)
         items (or (.structure res) [])
         lines (str/split-lines source)]

        (remember! defs-cache
                   k
                   (binding [*docstrings* (docstring-index (.docstrings res))]
                     (vec (defs-tree lines items))))))))

(defn definitions
  "The DATA behind `file-skeleton`: every definition in `source` (parsed as
   `language`), flattened across nesting, as
   `[{:name :kind :visibility :signature :doc :line :end-line :depth} …]`
   where the def's span is 1-based inclusive line numbers — the SOLE position, so
   `struct_index` → `struct_nodes` needs no re-read. With `name`, only the
   definitions with that exact name (there may be several — same name in different scopes). Empty when
   the language is unsupported or nothing structural was found."
  ([source language] (all-definitions source language))
  ([source language name]
   (cond->> (all-definitions source language)
     (some? name)
     (filterv #(= name (:name %))))))

(defn file-skeleton
  "Skeleton string for `path` (items + full start..end line ranges),
   or nil when the language is unsupported or nothing structural was found.
   `source` may be passed to avoid a re-read (e.g. for unsaved buffers)."
  ([path] (file-skeleton path (slurp path)))
  ([path source]
   (when-let [language (detect-language path)]
     (let
       [res (process-source source language)
        items (or (.structure res) [])]

       (when (seq items)
         (binding [*docstrings* (docstring-index (.docstrings res))]
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
   :line}`. `:line` is the statement's first line — the same handle a def row
   carries, so an import line is a jump target too.
   `:items`/`:alias`/`:wildcard` are the pack's parsed detail when a grammar fills
   them (some only populate `:source` with the raw statement text)."
  [^ImportInfo imp]
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
       :line start}
      (seq items)
      (assoc :items items)

      alias
      (assoc :alias alias)

      (.isWildcard imp)
      (assoc :wildcard true))))

(defn- import-line
  "One import row → skeleton line: `<source>[ :as <alias>][ (<items>|*)]  @<line>`."
  [{:keys [source items alias wildcard line]}]
  (str "  "
       source
       (when alias (str " :as " alias))
       (cond wildcard " (*)"
             (seq items) (str " (" (str/join " " items) ")"))
       "  @"
       line))

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
   "other" "other"
   ;; `StructureKind/Other` labels (see `item-kind`) — only the ones a naive
   ;; `<kind>s` would mangle or that read badly pluralised.
   "query" "queries"
   "data" "data"
   "locals" "locals"
   "schema" "schema"
   "terraform" "terraform"})

(defn- section-label
  "Section header for `kind` — a curated plural, else a naive `<kind>s`."
  [^String kind]
  (or (kind->section kind) (str kind "s")))

(defn- item-line
  "One definition → a line-ranged skeleton line WITHOUT its kind (the kind lives in
   the enclosing section header): `<indent>[private ]<name>[  <sig>]  @from..to`,
   with a `pr-str`'d doc gist on an indented continuation line when present."
  [lines ^StructureItem it ^long depth]
  (let
    [^Span span
     (.span it)

     start
     (inc (.startLine span))

     end
     (inc (span-end-line lines span))

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

     doc
     (doc-snippet lines it)]

    (str indent label "  @" start ".." end (when doc (str "\n" indent "    " (pr-str doc))))))

(defn- count-items
  "Total definitions in the structure tree `items` (children included)."
  [items]
  (reduce (fn [n ^StructureItem it]
            (+ (long n) 1 (long (count-items (or (.children it) [])))))
          0
          items))

(defn- grouped-items
  "Render sibling `items` grouped by kind: a `<section>:` header per kind (in
   first-appearance order), each def under it as a line-ranged, kind-less
   `item-line`; a def's own children recurse one level deeper, themselves
   grouped. Returns a seq of skeleton lines — no kind word repeated per row.

   Each item's kind is resolved ONCE (the grouping and the header order share the
   one pass), never once per grouping and again per header."
  [lines items ^long depth]
  (let
    [indent
     (apply str (repeat depth "  "))

     pairs
     (mapv (fn [^StructureItem it]
             [(or (item-kind it) "other") it])
           items)

     by-kind
     (reduce (fn [m [k it]]
               (update m k (fnil conj []) it))
             {}
             pairs)]

    (mapcat (fn [k]
              (cons (str indent (section-label k) ":")
                    (mapcat (fn [^StructureItem it]
                              (cons (item-line lines it (inc depth))
                                    (let [children (or (.children it) [])]
                                      (when (seq children)
                                        (grouped-items lines children (+ depth 2))))))
                            (get by-kind k))))
            (distinct (map first pairs)))))

(defn- index-skeleton
  "Maki-style skeleton string: a `<file> · <language> · <N> lines` header, an
   optional `imports:` section, then the `definitions:` tree GROUPED by
   kind (one `<section>:` header per kind, so the kind is not repeated on every
   row) and structure-nested — every line still carrying its `@from..to` line
   range."
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
     (inc (span-end-line span))]

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
      :skeleton str        ; header + imports + nested definitions, all line-ranged
      :definitions [row …] ; machine rows (== `definitions`) — line/end-line
      :imports [row …]}    ; machine import rows, each with its line

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
         (binding
           [*docstrings* (docstring-index (.docstrings res))
            *doc-memo* (java.util.IdentityHashMap.)]

           (let [import-rows (mapv import->row imps)]
             {:language language
              :line-count (count lines)
              :skeleton (index-skeleton path language (count lines) lines items import-rows)
              :definitions (defs-tree lines items)
              :imports import-rows})))))))
