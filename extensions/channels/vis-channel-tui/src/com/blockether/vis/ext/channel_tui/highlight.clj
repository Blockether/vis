(ns com.blockether.vis.ext.channel-tui.highlight
  "Tree-sitter syntax highlighting for TUI code fences.

   The eval / code fences the model emits are colored by *parsing* them with the
   bundled tree-sitter grammars — the SAME `com.blockether/tree-sitter-language-pack`
   the structural editors use — and painting each node by the grammar's OWN
   `highlights.scm` capture scheme. That makes the classification accurate (a `;`
   inside a string is not a comment, a number inside a symbol is not a number)
   and GENERAL: Clojure, Python, JavaScript, JSON, … every fence we render is
   colored off each grammar's canonical highlight rules.

   The heavy lifting — parse, a real tree-sitter query interpreter (field-scoped
   captures, alternations, `#match?`/`#eq?` predicates), per-byte capture
   labeling, ANSI run coalescing — lives in the pack's JVM-native `Highlighter`
   (Java, on the hot path), NOT in Clojure. This namespace is a thin edge: it
   maps a fence `:lang` to a grammar, hands the pack our capture→SGR theme, and
   caches the result. The SGR codes we pick are the ones `render/ansi-code->fg`
   already maps to theme fg slots (string/number/keyword/special/comment plus
   function→warning-fg and type→success-fg), so no painter change is needed
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
   `render/ansi-code->fg` already maps to theme slots: 31 string, 32 type
   (success-fg), 33 function (warning-fg), 34 number, 35 special/constant,
   36 keyword, 90 comment. A dotted capture (`function.builtin`,
   `type.builtin`, `string.special`, …) falls back to its top-level category
   in the Java highlighter, so only the categories need listing. Identifiers
   (`variable`), operators and punctuation stay the default fg — coloring
   every token would be noise."
  {"keyword" "36"
   "string" "31"
   "escape" "35"
   "number" "34"
   "constant" "35"
   "constant.builtin" "35"
   "comment" "90"
   "function" "33"
   "type" "32"
   "constructor" "32"
   "property" "35"})

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

(def ^:dynamic *live?*
  "Bound true by the renderer while laying out a LIVE / streaming bubble. When set
   (and the kill switch below is on) `highlight` takes the incremental streaming
   path — a memoized settled prefix + a standalone last line — instead of
   re-parsing the whole growing fence every tick. Finalized bubbles render with
   this false, so their colouring is byte-identical to a full parse."
  false)

(def ^:private incremental-live?
  "Kill switch for the streaming incremental path (default ON). Set
   VIS_STREAM_INCREMENTAL_HL=0/false/no/off to force a full parse even while live."
  (delay (not (contains? #{"0" "false" "no" "off"}
                         (some-> (System/getenv "VIS_STREAM_INCREMENTAL_HL")
                                 str/trim
                                 str/lower-case)))))

(def ^:private stream-memo
  "One settled-prefix slot per grammar for the live streaming path:
   grammar → {:prefix P :colored C}. While only the growing last line changes, the
   settled prefix P is constant, so its expensive parse is reused across ticks and
   only the short trailing line is (re)highlighted. Tiny + self-superseding: a new
   settled line replaces the slot, so memory stays O(#grammars)."
  (atom {}))

(defn- highlight*
  "Raw native parse + colorize; nil on any failure (fail-open)."
  [grammar ^String source]
  (try (Highlighter/highlightAnsi source grammar @theme-map) (catch Throwable _ nil)))

(defn- exact-highlight
  "Full-source colorize behind the wholesale [grammar source] cache. Output is
   identical to a bare native parse — used for finalized fences and every scroll."
  [grammar ^String source]
  (let [key [grammar source]]
    (if (contains? @cache key)
      (get @cache key)
      (let [v (highlight* grammar source)]
        (when (> (count @cache) 512) (reset! cache {}))
        (swap! cache assoc key v)
        v))))

(defn- last-nonblank-index
  "Index of the last non-empty line in `lines`, or -1 when all are empty. Used to
   peel the still-being-typed trailing line off the settled prefix."
  [lines]
  (loop [i (dec (count lines))]
    (cond (neg? i) -1
          (pos? (count ^String (nth lines i))) i
          :else (recur (dec i)))))

(defn- live-highlight
  "Streaming colorize. Split the source (the markdown parser newline-normalises it,
   so it ends in a newline) into a SETTLED prefix — every line up to the last
   non-empty one — plus that trailing, still-typed line. The prefix is parsed IN
   CONTEXT and memoized per grammar, so while only the last line grows it is reused
   with NO re-parse; the short trailing line is highlighted on its own. The lone
   approximation is that trailing line (and the settled line whose form it closes),
   which is genuinely incomplete mid-stream and self-heals once the fence finalizes
   and `exact-highlight` runs. Split/concat is lossless, so a single-line source or
   one without a trailing newline still reconstructs verbatim."
  [grammar ^String source]
  (let
    [lines
     (str/split source #"\n" -1)

     idx
     (last-nonblank-index lines)]

    (if (<= (long idx) 0)
      ;; nothing settled ahead of the growing line — just parse it whole.
      (highlight* grammar source)
      (let
        [prefix
         (str (str/join "\n" (subvec lines 0 idx)) "\n")

         tail
         (str/join "\n" (subvec lines idx))

         m
         (get @stream-memo grammar)

         cprefix
         (if (= (:prefix m) prefix)
           (:colored m)
           (let [c (or (highlight* grammar prefix) prefix)]
             (swap! stream-memo assoc grammar {:prefix prefix :colored c})
             c))

         ctail
         (if (pos? (count tail)) (or (highlight* grammar tail) tail) tail)]

        (str cprefix ctail)))))

(defn highlight
  "ANSI-colorize `source` as tree-sitter `grammar`, returning the colored string
   (same newline structure as the input, ready to `str/split-lines`), or nil
   when the grammar is unknown / the native lib is unavailable / parsing fails.
   Callers treat nil as \"render plain\".

   While a bubble streams (`*live?*` bound true) the growing fence is colorized
   incrementally — the settled prefix is memoized and only the last, still-typed
   line is re-parsed per tick — instead of re-parsing the whole growing fence every
   tick. Finalized bubbles use the exact full-source path, so their colouring is
   byte-identical to a full parse."
  [grammar ^String source]
  (when (and grammar (seq source) @native-ready?)
    (if (and *live?* @incremental-live?)
      (live-highlight grammar source)
      (exact-highlight grammar source))))
