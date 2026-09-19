(ns com.blockether.vis.internal.foundation.editing.parse
  "Language detection and PARSE VERDICTS for the anchored `patch` gate.

   Two questions, answered through tree-sitter (com.blockether/tree-sitter-language-pack,
   which sources Clojure from our own grammar fork):

     1. what language is this file, and is it a language where a parse error
        means the file is genuinely broken (`code-languages`)?
     2. where exactly does the new content fail to parse (`error-nodes`)?

   `patch` spends both: it re-parses what a write would produce and refuses an
   edit that introduces a syntax error the file did not already have, naming the
   line and the unpaired delimiter instead of a bare error count.

   A registered language surface may answer the SYNTAX question itself: a
   `:syntax-fn` declared under `:ext/language-tools` is consulted before
   tree-sitter for the language it serves, and the file extensions such a surface
   claims are detected here too. Tree-sitter answers for everything else, and
   whenever a handler throws or breaks the `syntax_result` contract.

   All native handles (Parser/Tree/Node) are opened and closed inside each call;
   only plain Clojure data escapes. Requiring this namespace also requires the
   native resolver, which selects the right per-platform FFI library at runtime."
  (:require [clojure.string :as str]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack]
            [com.blockether.vis.contract.surface :as contract-surface]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.util :as util])
  (:import [dev.kreuzberg.treesitterlanguagepack TreeSitterLanguagePack Parser Tree Node Point]
           [java.nio.charset StandardCharsets]
           [java.util Arrays]))

(def ^:private extra-extension->language
  "Clojure-family file extensions the pack's grammar table does NOT map, but that
   the `clojure` grammar parses cleanly — EDN is a subset of the Clojure reader,
   so `deps.edn` / `manifest.edn` / config data are parse-checked like any other
   Clojure source instead of slipping past the gate unparsed. Consulted ONLY when
   the pack's own `detectLanguageFromPath` returns nil, so it never overrides the
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

(defn- registered-surfaces
  "The language-tool entries active extensions declare, or nil when the registry
   cannot be read — nothing has registered yet, or a registration is mid-flight.
   Both mean the built-in answer stands."
  []
  (try (seq (mapcat :ext/language-tools (extension/registered-extensions)))
       (catch Throwable _ nil)))

(defn- surface-language
  "The language a surface entry serves, lower-cased, or nil. Registrations declare
   a STRING, exactly as the language-tool dispatcher reads them."
  [entry]
  (some-> (:language entry)
          str
          str/lower-case
          not-empty))

(defn- normalized-extension
  "A declared file extension spelled the way [[path-extension]] answers it:
   lower-cased, without a leading dot. nil when it is not a usable extension."
  [ext]
  (when-let [e (some-> ext
                       str
                       str/trim
                       str/lower-case
                       not-empty)]
    (not-empty (if (str/starts-with? e ".") (subs e 1) e))))

(defn- surface-extension->language
  "File extension -> language for every extension a registered surface CLAIMS.
   This is how a language the pack never heard of earns a name here; the surface
   that claims an extension owns it."
  []
  (reduce (fn [acc entry]
            (if-let [lang (surface-language entry)]
              (reduce (fn [m ext]
                        (if-let [e (normalized-extension ext)]
                          (assoc m e lang)
                          m))
                      acc
                      (:extensions entry))
              acc))
          {}
          (registered-surfaces)))

(defn detect-language
  "tree-sitter language name for `path` (by extension/shebang), or nil. NOTE: the
   pack recognizes HUNDREDS of grammars, including prose/markup — `.txt` maps to
   `vimdoc` (Vim `:help` files), `.md`→markdown, `.csv`→csv — which parse WITH error
   nodes on ordinary content. For 'is a syntax error meaningful here?' test the
   result against `code-languages`.

   Falls back to `extra-extension->language` (currently `.edn`→`clojure`) ONLY
   when the pack returns nil, covering Clojure-family extensions the pack's table
   omits so their files are still parse-checked, and then to the extensions a
   registered language surface claims. The built-ins answer first: a surface ADDS
   a file type here, it never re-routes one vis already detects."
  [^String path]
  (or (TreeSitterLanguagePack/detectLanguageFromPath path)
      (when-let [ext (path-extension path)]
        (or (get extra-extension->language ext) (get (surface-extension->language) ext)))))

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
    "svelte" "vue" "json" "yaml" "toml" "ada" "bicep" "c3" "cairo" "capnp" "clarity" "commonlisp"
    "crystal" "cuda" "cue" "d" "dhall" "elisp" "erlang" "fish" "fortran" "fsharp" "gdscript" "gleam"
    "glsl" "haxe" "hlsl" "json5" "jsonnet" "kdl" "matlab" "nim" "objc" "odin" "pascal" "pkl"
    "powershell" "prisma" "proto" "purescript" "racket" "rego" "rescript" "ron" "scheme" "solidity"
    "starlark" "systemverilog" "tcl" "thrift" "typespec" "v" "verilog" "vhdl" "wat" "wgsl" "zsh"})

(defn- syntax-handler
  "The `:syntax-fn` a registered language surface declares for `lang`, or nil. It
   is called with `{:language :source}` and answers a `syntax_result` document —
   the verdict [[error-nodes]] prefers over tree-sitter for that one language. The
   first matching registration answers."
  [lang]
  (when-let [want (some-> lang
                          str
                          str/lower-case
                          not-empty)]
    (some (fn [entry]
            (let [f (:syntax-fn entry)]
              (when (and (ifn? f) (= want (surface-language entry))) f)))
          (registered-surfaces))))

(defn guarded-language
  "The detected language for `path` when Vis treats its parse errors as real syntax
   failures, otherwise nil. This is the single policy boundary shared by `patch` and
   sandboxed Python writers; broad language detection alone must never gate prose.

   A language a registered surface answers for with its own `:syntax-fn` is guarded
   as well — the extension that owns the verdict says its language is code. With no
   such surface this is exactly the `code-languages` allowlist."
  [path]
  (let [lang (detect-language (str path))]
    (when (and lang (or (contains? code-languages lang) (some? (syntax-handler lang)))) lang)))

(defn- byte-slice
  ^String [^bytes bs ^long start ^long end]
  (String. (Arrays/copyOfRange bs (int start) (int end)) StandardCharsets/UTF_8))

(defn- parse-tree
  "Parse `source` as `lang` → a Tree (CALLER CLOSES), or nil. The tree is
   independent of the parser once parsed, so the parser is closed immediately. A
   name no grammar answers to is nil rather than a throw: a language vis detects
   from a surface declaration reaches here too."
  ^Tree [^String lang ^String source]
  (let [p (Parser/create)]
    (try (.setLanguage p lang)
         (.orElse (.parse p source) nil)
         (catch Throwable _ nil)
         (finally (.close p)))))

(def ^:private quote-kinds
  "Literal quote tokens grammars may leave directly under an ERROR when a string
   consumes the rest of the file."
  #{"\"" "'" "`" "\"\"\"" "'''"})

(def ^:private delimiter-kinds
  "Literal syntax delimiters that are actionable when left directly beneath an
   ERROR node. Keywords and identifiers are deliberately excluded."
  (into #{"(" ")" "[" "]" "{" "}"} quote-kinds))

(def ^:private detail-budget
  "How many error rows get the expensive fields: `fault-delimiter`, the source
   line, the sliced text. Rows past it are still found, counted and positioned —
   capping the WALK would make `(count errors)` lie, and that count is what says
   whether one line broke or the file did.

   Below it, shown rows arrive undescribed; above it, we describe rows nobody
   sees."
  5)

(defn- fault-delimiter
  "The most actionable unpaired delimiter directly inside ERROR node `n`, as
   {:line :byte-col :kind}, or nil.

   tree-sitter normally re-parents each well-formed sibling as a NAMED child of
   an ERROR, leaving the delimiter that failed to pair as an unnamed child. A
   recovery wrapper can contain more than one such delimiter, though: Java, for
   example, leaves both the class `{` and the actual unterminated string quote
   beneath one file-wide ERROR. Prefer the last quote (the lexical fault), then
   the last bracket (the closest structural fault), rather than blaming the
   first innocent opener at the start of the file.

   Walks every child, so a file-wide ERROR costs the whole token stream —
   [[detail-budget]] is what bounds the number of calls."
  [^Node n]
  (loop [i
         0

         quote
         nil

         bracket
         nil]

    (if (< i (.childCount n))
      (if-let [^Node c (.orElse (.child n (int i)) nil)]
        (let [kind (.kind c)
              hit? (and (not (.isNamed c)) (contains? delimiter-kinds kind))
              data (when hit?
                     (let [^Point sp (.startPosition c)]
                       {:line (inc (.row sp)) :byte-col (.column sp) :kind kind}))]

          (.close c)
          (recur (inc i)
                 (if (and data (contains? quote-kinds kind)) data quote)
                 (if (and data (not (contains? quote-kinds kind))) data bracket)))
        (recur (inc i) quote bracket))
      (or quote bracket))))

(defn- character-column
  "Convert tree-sitter's 0-based UTF-8 byte column to a user-facing Unicode
   code-point column on `line`. Parser points always fall on UTF-8 boundaries."
  ^long [^String line ^long byte-col]
  (let [^bytes bs
        (util/utf8 line)

        end
        (min (max 0 byte-col) (alength bs))

        ^String prefix
        (byte-slice bs 0 end)]

    (.codePointCount prefix 0 (.length prefix))))

(defn- source-line
  "1-based line `line` of `source`, or nil."
  [^String source ^long line]
  (let [ls (str/split-lines (str source))]
    (when (<= 1 line (count ls)) (nth ls (dec line)))))

(defn- tree-sitter-nodes
  "The BUILT-IN verdict [[error-nodes]] falls back to: every ERROR / MISSING node
   tree-sitter finds in `source` (parsed as `lang`), in document order. Empty when
   the source parses clean, when `lang` is nil, and when no grammar answers to that
   name."
  [lang ^String source]
  (if-let [^Tree tree (and lang (parse-tree lang source))]
    (let [src-bytes (util/utf8 source)
          acc (transient [])]

      (try
        (let [^Node root (.rootNode tree)]
          (try
            (letfn
              [(described [^Node n ^Point sp row]
                 (let [d (when (.isError n) (fault-delimiter n))
                       line (long (or (:line d) (:line row)))
                       byte-col (long (or (:byte-col d) (:byte-col row)))
                       line-text (source-line source line)]

                   (cond-> (assoc row
                             :line line
                             :byte-col byte-col
                             :col (if line-text (character-column line-text byte-col) byte-col)
                             :text (or (when d line-text)
                                       (byte-slice src-bytes (.startByte n) (.endByte n))))
                     d
                     (assoc :delimiter
                       (:kind d) :error-line
                       (inc (.row sp))))))
               (walk [^Node n]
                 (when (or (.isError n) (.isMissing n))
                   (let [^Point sp (.startPosition n)
                         ^Point ep (.endPosition n)
                         row {:line (inc (.row sp))
                              :col (.column sp)
                              :byte-col (.column sp)
                              :end-line (inc (.row ep))
                              :end-col (.column ep)
                              :start-byte (.startByte n)
                              :end-byte (.endByte n)
                              :kind (.kind n)
                              :missing? (.isMissing n)}]

                     (conj! acc (if (< (count acc) detail-budget) (described n sp row) row))))
                 (dotimes [i (.childCount n)]
                   (when-let [^Node c (.orElse (.child n (int i)) nil)]
                     (try (walk c) (finally (.close c))))))]
              (walk root))
            (finally (.close root))))
        (finally (.close tree)))
      (persistent! acc))
    []))

(defn- normalized-finding
  "One handler finding as the row the gate reads — `:line`, `:col`, `:kind`,
   `:missing?` and `:text` — keeping every other field the surface reported. Keys
   arrive as Clojure keywords or as the snake_case strings a JSON-shaped handler
   answers, so [[wire/->engine]] is the one inbound spelling rule."
  [finding]
  (when (map? finding)
    (let [row
          (wire/->engine finding)

          kind
          (some-> (:kind row)
                  name
                  str/lower-case
                  not-empty)]

      (assoc row
        :line (long (:line row))
        :col (long (or (:col row) 0))
        :kind (or kind "parse")
        :missing? (boolean (or (:missing? row) (:is-missing row) (= "missing" kind)))
        :text (or (:text row) (:message row))))))

(defn- handler-findings
  "The rows a registered `:syntax-fn` answers for `source` under `lang`, or nil when
   the built-in verdict stands: no surface serves the language, the handler threw,
   or its result does not satisfy the `syntax_result` contract. An empty vector is a
   VERDICT — the handler read the source and found it clean — so a caller must tell
   `[]` from nil."
  [lang ^String source]
  (when-let [f (syntax-handler lang)]
    (try (let [result (wire/->engine (contract-surface/check :syntax-fn
                                                             (f {:language lang :source source})))
               rows (into [] (keep normalized-finding) (:findings result))]

           (cond (:is-clean result) []
                 (seq rows) rows
                 ;; A verdict of "not clean" must never read as clean downstream: with no
                 ;; usable row the write is still refused, at the top of the file.
                 :else [{:line 1 :col 0 :kind "parse" :missing? false :text nil}]))
         (catch Throwable _ nil))))

(defn error-nodes
  "Every syntax fault in `source` read as `lang`, in document order, as
   [{:line :col :byte-col :end-line :end-col :start-byte :end-byte :kind
   :missing? :text} …] (1-based line, 0-based Unicode code-point col; `:byte-col`
   preserves tree-sitter's raw UTF-8 column). Empty when the source parses clean or
   the language can't be parsed. Public so an edit guard can turn a bare
   \"N syntax error(s)\" rejection into a LOCATED, actionable message — a MISSING
   node even NAMES the delimiter the parser expected (`:kind` = `]`, `)`, …).

   An ERROR node reports the most actionable UNBALANCED DELIMITER directly inside
   it, not necessarily the node's own start: an unclosed form can make tree-sitter
   open one ERROR over the whole file whose start is line 1. Those rows carry
   `:delimiter` and `:error-line` (where recovery began), and `:text` is the
   offending LINE. Raw byte spans remain available so a diagnostic can recognize
   and look through a broad recovery wrapper that contains a more specific ERROR.

   A registered language surface answers FIRST for the language it serves: its
   `:syntax-fn` owns the verdict, and its rows carry the same `:line :col :kind
   :missing? :text` keys plus whatever else it reported. Tree-sitter answers when
   no surface claims the language, when the handler throws, and when its result
   breaks the `syntax_result` contract — a misbehaving extension never opens the
   gate."
  [lang ^String source]
  (or (handler-findings lang source) (tree-sitter-nodes lang source)))

(defn transition-verdict
  "Compare `original` and `candidate` under `lang`. Returns a plain-data verdict:
   `:clean`, `:still-broken`, or `:introduced-error`, with parser rows when relevant.
   A pre-existing broken file remains writable so a caller can repair it. Callers own
   policy beyond this verdict: `patch` may attempt an explicit delimiter repair while
   raw writers must either preserve the exact candidate or refuse it."
  [lang ^String original ^String candidate]
  (if-not lang
    {:status :unguarded :language nil :before [] :after []}
    (let [after (error-nodes lang candidate)]
      (if (empty? after)
        {:status :clean :language lang :before [] :after []}
        (let [before (error-nodes lang original)]
          {:status (if (seq before) :still-broken :introduced-error)
           :language lang
           :before before
           :after after})))))

(defn top-level-nodes
  "The NAMED direct children of the parse root of `source` (read as `lang`), in
   document order: [{:kind :text :children [{:kind :text} …]} …]. One level of
   grandchildren comes along, which is what a DECLARATION scan needs — punctuation
   tokens are skipped, so a TOML table's own header key IS its child 0. Empty when
   `lang` is nil; input that does not parse answers the recovery nodes tree-sitter
   produced (`ERROR` …), never a declaration.

   PARSED, never scanned: a `[tool.uv]` written inside a comment or a string stays a
   comment / string node here, so a caller reading declarations out of a config file
   never mistakes prose for a declaration."
  [lang ^String source]
  (if-let [^Tree tree (and lang (parse-tree lang source))]
    (let [src-bytes (util/utf8 source)]
      (try (let [^Node root (.rootNode tree)]
             (try (letfn [(node->data [^Node n children]
                            {:kind (.kind n)
                             :text (byte-slice src-bytes (.startByte n) (.endByte n))
                             :children children})
                          (children-of [^Node n f]
                            (into []
                                  (keep (fn [i]
                                          (when-let [^Node c (.orElse (.child n (int i)) nil)]
                                            (try (when (.isNamed c) (f c)) (finally (.close c))))))
                                  (range (.childCount n))))]
                    (children-of root
                                 (fn [^Node c]
                                   (node->data c
                                               (children-of c
                                                            (fn [^Node g]
                                                              (node->data g [])))))))
                  (finally (.close root))))
           (finally (.close tree))))
    []))
