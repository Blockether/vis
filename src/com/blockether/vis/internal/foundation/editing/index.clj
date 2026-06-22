(ns com.blockether.vis.internal.foundation.editing.index
  "Structural index: a high-level, line-ranged skeleton of a source file
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
  (:import [dev.kreuzberg.treesitterlanguagepack
            TreeSitterLanguagePack ProcessConfig ProcessResult StructureItem Span]))

(defn detect-language
  "tree-sitter language name for `path` (by extension/shebang), or nil."
  [^String path]
  (TreeSitterLanguagePack/detectLanguageFromPath path))

(defn- structure-items
  "List<StructureItem> for `source` parsed as `language` (empty when none)."
  [^String source ^String language]
  (let [cfg            (-> (ProcessConfig/builder)
                         (.withLanguage language)
                         (.withStructure true)
                         (.build))
        ^ProcessResult res (TreeSitterLanguagePack/process source cfg)]
    (or (.structure res) [])))

(defn- line-text [lines ln]
  ;; lines is 0-based; ln is 1-based.
  (nth lines (dec ln) ""))

(defn- fmt-item [lines ^StructureItem it depth]
  (let [^Span span (.span it)
        ;; tree-sitter rows are 0-based; report 1-based inclusive line ranges.
        start  (inc (.startLine span))
        end    (inc (.endLine span))
        kind   (some-> (.kind it) str str/lower-case)
        nm     (.name it)
        sig    (some-> (.signature it) str/trim not-empty)
        indent (apply str (repeat depth "  "))
        label  (str/trim (str kind
                           (when nm (str " " nm))
                           (when sig (str "  " sig))))
        from   (patch/line-anchor start (line-text lines start))
        to     (patch/line-anchor end (line-text lines end))]
    ;; The anchors already carry the line numbers, so no separate [start-end].
    (str indent label "  @" from ".." to)))

(defn- walk-items [lines items depth]
  (mapcat (fn [^StructureItem it]
            (cons (fmt-item lines it depth)
              (walk-items lines (or (.children it) []) (inc depth))))
    items))

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
