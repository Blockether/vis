(ns com.blockether.vis.ext.channel-tui.highlight
  "Tree-sitter syntax highlighting for TUI code fences.

   The eval / code fences the model emits are colored by *parsing* them with the
   bundled tree-sitter grammars — the SAME `com.blockether/tree-sitter-language-pack`
   the structural editors use — and painting each node by the grammar's OWN
   `highlights.scm` capture scheme. That makes the classification accurate (a `;`
   inside a string is not a comment, a number inside a symbol is not a number)
   and GENERAL: Clojure, Python, JavaScript, JSON, … every fence we render is
   colored off each grammar's canonical highlight rules.

   The heavy lifting — parse, tree walk, per-byte capture labeling, ANSI run
   coalescing — lives in the pack's JVM-native `Highlighter` (Java, on the hot
   path), NOT in Clojure. This namespace is a thin edge: it maps a fence `:lang`
   to a grammar, hands the pack our capture→SGR theme, and caches the result. The
   SGR codes we pick are the ones `render/ansi-code->fg` already maps to theme fg
   slots (string/number/keyword/special/comment), so no painter change is needed
   and the escapes stay zero-width (column alignment on verbatim fences is
   untouched).

   Fail-open: if the native pack isn't loadable (e.g. a bare unit-test JVM
   without the platform lib on the classpath) every entry point returns nil and
   the caller falls back to plain, uncolored text."
  (:require [clojure.string :as str])
  (:import [dev.kreuzberg.treesitterlanguagepack Highlighter TreeSitterLanguagePack]
           [java.util HashMap]))

;; =============================================================================
;; Native availability (fail-open)
;; =============================================================================

(def ^:private native-ready?
  "True once the platform native lib is selected + loaded. The require is the
   pack's side-effecting platform loader; guarded so a JVM without the native
   lib (bare unit tests) degrades to no color instead of throwing at load."
  (delay (try (require 'com.blockether.tree-sitter-language-pack)
              ;; Touch the FFI so a missing native lib fails HERE, not mid-render.
              (TreeSitterLanguagePack/getHighlightsQuery "clojure")
              true
              (catch Throwable _ false))))

;; =============================================================================
;; capture → ANSI SGR (reuses the painter's existing theme fg slots)
;; =============================================================================

(def ^:private capture->sgr
  "Highlight capture → SGR foreground code. Codes are the ones
   `render/ansi-code->fg` already maps to theme slots: 31 string, 34 number,
   35 special, 36 keyword, 90 comment. Unlisted captures (variable, operator,
   function, punctuation, …) stay the default fg — coloring every identifier
   would be noise (and, without a query engine, unreliable)."
  {"string" "31"
   "number" "34"
   "constant" "36"
   "constant.builtin" "35"
   "keyword" "35"
   "comment" "90"
   "escape" "35"
   "type" "36"
   "constructor" "36"})

(def ^:private theme-map
  "`capture->sgr` as a `java.util.Map` for the Java highlighter (built once)."
  (delay (let [m (HashMap.)]
           (doseq [[k v] capture->sgr]
             (.put m ^String k ^String v))
           m)))

;; =============================================================================
;; Fence lang → grammar
;; =============================================================================

(def ^:private lang->grammar
  "Fence `:lang` (lower-cased) → tree-sitter grammar name. Only langs we
   deliberately colorize; anything else renders plain."
  {"clojure" "clojure"
   "clj" "clojure"
   "cljc" "clojure"
   "cljs" "clojure"
   "cljd" "clojure"
   "edn" "clojure"
   "bb" "clojure"
   "python" "python"
   "py" "python"
   "javascript" "javascript"
   "js" "javascript"
   "jsx" "javascript"
   "mjs" "javascript"
   "typescript" "typescript"
   "ts" "typescript"
   "json" "json"
   "rust" "rust"
   "rs" "rust"
   "go" "go"
   "java" "java"
   "bash" "bash"
   "sh" "bash"
   "shell" "bash"
   "zsh" "bash"})

(defn grammar-for
  "Tree-sitter grammar name for a fence `lang`, or nil if we don't colorize it."
  [lang]
  (when lang (get lang->grammar (str/lower-case lang))))

;; =============================================================================
;; Highlight (delegates to the pack's JVM-native Highlighter)
;; =============================================================================

(def ^:private cache
  "Bounded [grammar source] → colored-string cache: renders re-run on every
   scroll, so identical fences aren't re-parsed. Reset wholesale when large
   rather than tracking LRU — fences are small and churn fast."
  (atom {}))

(defn highlight
  "ANSI-colorize `source` as tree-sitter `grammar`, returning the colored string
   (same newline structure as the input, ready to `str/split-lines`), or nil
   when the grammar is unknown / the native lib is unavailable / parsing fails.
   Callers treat nil as \"render plain\"."
  [grammar ^String source]
  (when (and grammar (seq source) @native-ready?)
    (let [key [grammar source]]
      (if (contains? @cache key)
        (get @cache key)
        (let [v (try (Highlighter/highlightAnsi source grammar @theme-map) (catch Throwable _ nil))]
          (when (> (count @cache) 512) (reset! cache {}))
          (swap! cache assoc key v)
          v)))))
