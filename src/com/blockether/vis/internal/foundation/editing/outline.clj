(ns com.blockether.vis.internal.foundation.editing.outline
  "Structural outline: a high-level, line-ranged skeleton of a source file
   produced via tree-sitter (com.blockether/tree-sitter-language-pack, which
   sources Clojure from our own grammar fork).

   Every item carries FULL anchors — the same `<lineno>:<hash>` anchors `cat`
   emits and `patch` consumes — for its first and last line, so you can replace
   a whole definition straight from the index with one
   `patch([{from_anchor start, to_anchor end, replace …}])`, no intermediate
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
            StructureItem Span]))

(def ^:private extra-extension->language
  "Clojure-family file extensions the pack's grammar table does NOT map, but that
   the `clojure` grammar parses cleanly — EDN is a subset of the Clojure reader,
   so `deps.edn` / `vis.edn` / config data get real structural editing (sexpr,
   node replace) instead of a refused-`patch` fallback. Consulted ONLY when the
   pack's own `detectLanguageFromPath` returns nil, so it never overrides the
   pack. Drop an entry here once the pack ships it on the `clojure` grammar."
  {"edn" "clojure"})

(defn- path-extension
  "Lower-cased extension of `path`'s final segment (no leading dot), or nil when
   the file name has none."
  [^String path]
  (let [name
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

(defn- structure-items
  "List<StructureItem> for `source` parsed as `language` (empty when none).

   The pack surfaces every native/decode failure as one opaque
   `TreeSitterLanguagePackRsException: FFI call failed`, burying the real
   reason in a nested cause (e.g. a `StructureItem` field the Java binding
   can't deserialize). A single unparseable item would otherwise nuke the
   whole outline with zero signal, so we rethrow with the deepest cause's
   message attached — the actionable detail — while keeping the language
   and original chain for callers."
  [^String source ^String language]
  (let [cfg (-> (ProcessConfig/builder)
                (.withLanguage language)
                (.withStructure true)
                (.build))]
    (try (let [^ProcessResult res (TreeSitterLanguagePack/process source cfg)]
           (or (.structure res) []))
         (catch Throwable t
           (let [cause (root-cause t)]
             (throw (ex-info (str "tree-sitter structure extraction failed for language " language
                                  ": " (.getMessage cause))
                             {:language language :cause-type (.getName (class cause))}
                             t)))))))

(defn node-span
  "0-based inclusive `[start-line end-line]` of the TOP-LEVEL structural node named
   `target` (optionally narrowed by `kind`, case-insensitive), or nil if not found.
   Used by the structural `move` op to extract a node's exact source text by name."
  [^String source ^String language ^String target kind]
  (let [k (some-> kind
                  str
                  str/lower-case)]
    (some (fn [^StructureItem it]
            (when (and (= target (.name it))
                       (or (nil? k)
                           (= k
                              (some-> (.kind it)
                                      str
                                      str/lower-case))))
              (let [^Span span (.span it)]
                [(.startLine span) (.endLine span)])))
          (structure-items source language))))

(defn- line-text
  [lines ln]
  ;; lines is 0-based; ln is 1-based.
  (nth lines (dec (long ln)) ""))

(defn- doc-snippet
  "First non-blank line of a definition's doc string, trimmed and clipped to a
   single readable gist (nil when there is none). The pack populates `docComment`
   from the def's own doc string / leading comment — Clojure docstrings, and the
   `//` / JSDoc block written directly above a JS/TS/TSX def."
  [^StructureItem it]
  (when-let [d (.docComment it)]
    (when-let [line (->> (str/split-lines d)
                         (map str/trim)
                         (remove str/blank?)
                         first)]
      (if (> (count line) 72) (str (subs line 0 71) "…") line))))

(defn- fmt-item
  [lines ^StructureItem it depth]
  (let [^Span span
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
  (let [^Span span
        (.span it)

        ;; tree-sitter rows are 0-based; anchors carry 1-based line numbers (like
        ;; the skeleton + `cat`).
        start
        (inc (.startLine span))

        end
        (inc (.endLine span))]

    {:name (.name it)
     :kind (some-> (.kind it)
                   str
                   str/lower-case)
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
     ;; lets a consumer rebuild the tree. occurrences rows are flat (no depth);
     ;; every other def field is name-for-name the SAME as an occurrences def row.
     :depth depth}))

(defn definitions
  "The DATA behind `file-skeleton`: every definition in `source` (parsed as
   `language`), flattened across nesting, as
   `[{:name :kind :visibility :signature :doc :anchor :end-anchor :depth} …]`
   where the def's span is patch-ready `lineno:hash` anchors — the SOLE position
   (no redundant start/end line; `patch/anchor->line` recovers the number). So
   `outline` → `patch` needs no re-cat. With `name`, only the definitions with that
   exact name (there may be several — same name in different scopes). Empty when
   the language is unsupported or nothing structural was found."
  ([source language] (definitions source language nil))
  ([source language name]
   (let [lines (str/split-lines source)]
     (letfn [(walk [items depth]
               (mapcat (fn [^StructureItem it]
                         (cons (item->def lines it depth)
                               (walk (or (.children it) []) (inc (long depth)))))
                       items))]
       (cond->> (walk (structure-items source language) 0)
         (some? name)
         (filterv #(= name (:name %))))))))

(defn file-skeleton
  "Skeleton string for `path` (items + line ranges + full start..end anchors),
   or nil when the language is unsupported or nothing structural was found.
   `source` may be passed to avoid a re-read (e.g. for unsaved buffers)."
  ([path] (file-skeleton path (slurp path)))
  ([path source]
   (when-let [language (detect-language path)]
     (let [items (structure-items source language)]
       (when (seq items)
         (let [lines (str/split-lines source)]
           (str/join "\n" (walk-items lines items 0))))))))
