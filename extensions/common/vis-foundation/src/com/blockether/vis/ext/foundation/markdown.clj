(ns com.blockether.vis.ext.foundation.markdown
  "Programmatic markdown construction under the SCI alias `md/`.

   Replaces the dropped Mustache layer entirely. Every fn here is a
   pure string builder — `(str …)` your way to the answer instead of
   shipping a templating language.

   Surface (every fn returns a String, never nil; missing args are
   coerced via `str`):

     Headings    md/h1 … md/h6, md/h
     Inline      md/bold, md/italic, md/bold-italic, md/strike,
                 md/code, md/kbd, md/link, md/image
     Block       md/p, md/code-block, md/blockquote, md/hr, md/br,
                 md/details
     Lists       md/ul, md/ol, md/checklist
     Tables      md/table
     Composing   md/join (blank-line between), md/lines
                 (newline between), md/section, md/escape

   Convention: block-producing fns return text WITHOUT a trailing
   newline. Compose with `(md/join …)` (blank line between) or
   `(md/lines …)` (single newline between) and pass the final string
   to `(answer …)` from the iteration loop.

   Example:

     (answer
       (md/join
         (md/h1 \"Patch report\")
         (md/p \"Three files touched.\")
         (md/table [\"file\" \"+/-\"]
                   [[\"core.clj\" \"+12 / -4\"]
                    [\"loop.clj\" \"+0 / -38\"]])
         (md/h2 \"Next\")
         (md/ul [\"Run verify.sh\" \"Update CHANGELOG\"])))"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as sdk]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- ->str
  "Coerce `x` to a String. nil -> empty string. Strings pass through
   verbatim. Other scalars route through `(str x)`.

   Sequential collections (lists, lazy seqs, vectors of blocks) are
   refused with a clear ex-info — silently calling `(str lazy-seq)`
   produces `clojure.lang.LazySeq@<hex>` text that bakes into the
   final answer and the iteration loop has no way to detect it.
   Callers must splice collections via `(md/join …)` / `(md/lines …)`
   or eagerly stringify rows themselves (e.g. `mapv md/p xs`).

   Markdown builders display text — pretty-printing data is the
   caller's job."
  ^String [x]
  (cond
    (nil? x)        ""
    (string? x)     x
    (sequential? x) (throw (ex-info
                             (str "md helper got a sequential collection where a string was expected. "
                               "Splice with (md/join …) / (md/lines …), or build the row eagerly: "
                               "(mapv md/p xs), (into [] (map render) xs), (str/join \"\\n\" xs).")
                             {:value-class (.getName (class x))
                              :sample      (->> x (take 3) (mapv #(if (string? %) % (pr-str %))))}))
    :else           (str x)))

;; =============================================================================
;; Headings
;; =============================================================================

(defn h
  "Render a heading at level `n` (clamped to [1, 6])."
  ^String [n s]
  (let [lvl (max 1 (min 6 (long n)))]
    (str (apply str (repeat lvl "#")) " " (->str s))))

(defn h1 ^String [s] (h 1 s))
(defn h2 ^String [s] (h 2 s))
(defn h3 ^String [s] (h 3 s))
(defn h4 ^String [s] (h 4 s))
(defn h5 ^String [s] (h 5 s))
(defn h6 ^String [s] (h 6 s))

;; =============================================================================
;; Inline
;; =============================================================================

(defn p ^String [s] (->str s))
(defn bold ^String [s] (str "**" (->str s) "**"))
(defn italic ^String [s] (str "*" (->str s) "*"))
(defn bold-italic ^String [s] (str "***" (->str s) "***"))
(defn strike ^String [s] (str "~~" (->str s) "~~"))
(defn code ^String [s] (str "`" (->str s) "`"))
(defn kbd ^String [s] (str "<kbd>" (->str s) "</kbd>"))

(defn- escape-title-attr
  "Escape `\"` so a Markdown link/image title attr stays well-formed."
  ^String [title]
  (str/replace (->str title) "\"" "\\\""))

(defn link
  "Inline hyperlink. `text` and `url` are coerced to strings.
   Optional 3-arg form embeds a tooltip title attribute:
     `(md/link \"docs\" \"https://example.com\" \"Project docs\")`
   renders as `[docs](https://example.com \"Project docs\")`."
  (^String [text url] (link text url nil))
  (^String [text url title]
   (let [t (->str title)]
     (if (str/blank? t)
       (str "[" (->str text) "](" (->str url) ")")
       (str "[" (->str text) "](" (->str url)
         " \"" (escape-title-attr t) "\")")))))

(defn image
  "Inline image. `alt` and `url` are coerced to strings. Optional
   3-arg form embeds a tooltip title attribute, same shape as
   `md/link`."
  (^String [alt url] (image alt url nil))
  (^String [alt url title]
   (str "!" (link alt url title))))

(defn file-link
  "Cite a workspace file in an answer. The link target is the
   literal `path` so a TUI / web channel can resolve it relative to
   `(fs/cwd)` and turn it into a clickable jump.

     (md/file-link \"src/foo.clj\")     ; [src/foo.clj](src/foo.clj)
     (md/file-link \"src/foo.clj\" 142) ; [src/foo.clj:142](src/foo.clj#L142)

   Reach for this whenever an answer references source code; it
   beats hand-rolling `[…](…)` calls and keeps line anchors
   uniform."
  (^String [path] (file-link path nil))
  (^String [path line]
   (let [p (->str path)]
     (if line
       (link (str p ":" line) (str p "#L" line))
       (link p p)))))

(defn anchor
  "Same-document anchor link to a heading. `text` is the visible
   label; `slug` is the heading's GitHub-style slug. Omit `slug` to
   slugify `text` automatically (lower-case, drop punctuation,
   collapse whitespace to `-`).

     (md/anchor \"Jump to summary\" \"summary\") ; [Jump to summary](#summary)
     (md/anchor \"Patch report\")               ; [Patch report](#patch-report)"
  (^String [text] (anchor text nil))
  (^String [text slug]
   (let [raw  (->str text)
         slug (or slug
                (-> raw
                  str/lower-case
                  (str/replace #"[^a-z0-9\s-]" "")
                  (str/replace #"\s+" "-")
                  (str/replace #"-+" "-")
                  (str/replace #"^-|-$" "")))]
     (link raw (str "#" slug)))))

;; =============================================================================
;; Block
;; =============================================================================

(defn code-block
  "Fenced code block. 1-arg form omits the language tag; 2-arg form
   embeds it in the opening fence (e.g. `clojure`, `bash`, `edn`)."
  (^String [code] (code-block code nil))
  (^String [code lang]
   (let [body (->str code)
         body (if (str/ends-with? body "\n") body (str body "\n"))]
     (str "```" (when lang (->str lang)) "\n"
       body
       "```"))))

(defn blockquote
  "Quote every line of `s` with a `> ` prefix. Multi-line input is
   preserved; empty input renders as a single `>`."
  ^String [s]
  (let [text (->str s)]
    (if (str/blank? text)
      ">"
      (->> (str/split-lines text)
        (map #(if (str/blank? %) ">" (str "> " %)))
        (str/join "\n")))))

(def hr
  "Horizontal rule (`---`)."
  "---")

(def br
  "Hard line break suffix. Append to a line and follow it with a
   newline to force a `<br/>` in the rendered output (CommonMark
   trailing-spaces convention)."
  "  ")

(defn details
  "GitHub-style collapsible block.

     <details>
     <summary>summary</summary>

     body

     </details>"
  ^String [summary body]
  (str "<details>\n<summary>" (->str summary) "</summary>\n\n"
    (->str body)
    "\n\n</details>"))

;; =============================================================================
;; Lists
;; =============================================================================

(defn ul
  "Unordered list. `items` is a sequence of strings (or values
   coercible via `str`). Returns one `- item` line per element joined
   by single newlines, no trailing newline."
  ^String [items]
  (->> (or items [])
    (map #(str "- " (->str %)))
    (str/join "\n")))

(defn ol
  "Ordered list. Numbering starts at 1."
  ^String [items]
  (->> (or items [])
    (map-indexed (fn [i x] (str (inc i) ". " (->str x))))
    (str/join "\n")))

(defn checklist
  "GitHub-style task list. `items` may be:
     - vec of `[text done?]` pairs, OR
     - vec of `{:text … :done? bool}` maps."
  ^String [items]
  (->> (or items [])
    (map (fn [it]
           (let [[t d?] (cond
                          (map? it)        [(:text it) (:done? it)]
                          (sequential? it) [(first it) (second it)]
                          :else            [it false])]
             (str "- [" (if d? "x" " ") "] " (->str t)))))
    (str/join "\n")))

;; =============================================================================
;; Tables
;; =============================================================================

(defn- pipe-escape
  ^String [s]
  (-> (->str s)
    (str/replace "\\" "\\\\")
    (str/replace "|" "\\|")
    (str/replace "\n" " ")))

(defn- align-spec
  ^String [a]
  (case a
    :center " :---: "
    :right  " ---: "
    :left   " :--- "
    " --- "))

(defn table
  "Render a markdown table.

   `headers` — vec of column header values.
   `rows`    — vec of row vecs; rows shorter than `headers` are
               right-padded with empty cells.

   Optional opts:
     :align — vec of `:left | :center | :right | :default` per column."
  (^String [headers rows] (table headers rows nil))
  (^String [headers rows {:keys [align]}]
   (let [n        (count headers)
         pad-row  (fn [r]
                    (let [v (vec r)]
                      (vec (for [i (range n)] (nth v i nil)))))
         hdr      (str "| " (str/join " | " (map pipe-escape headers)) " |")
         sep      (str "|"
                    (str/join "|"
                      (for [i (range n)]
                        (align-spec (nth (or align []) i :default))))
                    "|")
         body     (->> (or rows [])
                    (map (fn [r] (str "| "
                                   (str/join " | " (map pipe-escape (pad-row r)))
                                   " |")))
                    (str/join "\n"))]
     (if (str/blank? body)
       (str hdr "\n" sep)
       (str hdr "\n" sep "\n" body)))))

;; =============================================================================
;; Composing
;; =============================================================================

(defn- expand-parts
  "Flatten ONE level of sequential collections so callers can mix
   variadic block args with seq-producing forms — `(map …)`,
   `(for …)`, `(map-indexed …)`, `(keep …)` — without sneaking a
   `LazySeq` toString into the final markdown. Nested collections
   beyond one level remain the caller's responsibility (intentional —
   `(md/join nested-tree)` should still surface as an error rather
   than silently dropping structure)."
  [parts]
  (persistent!
    (reduce
      (fn [acc p]
        (cond
          (nil? p)        acc
          (sequential? p) (reduce conj! acc (remove nil? p))
          :else           (conj! acc p)))
      (transient [])
      parts)))

(defn join
  "Stitch block-level pieces with one BLANK line between them
   (i.e. `\\n\\n`). nil entries are dropped. Sequential args are
   spliced one level deep, so `(md/join (mapv render xs))` and
   `(md/join (md/h1 \"…\") (map render xs))` Just Work."
  ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n\n")))

(defn lines
  "Stitch `parts` with a single newline between them. nil entries
   are dropped. Sequential args are spliced one level deep — same
   rules as `join`."
  ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n")))

(defn section
  "Heading + body shortcut. Default level 2.
   `(md/section \"Summary\" body)`              -> ## Summary\\n\\nbody
   `(md/section 3 \"Details\" body)`           -> ### Details\\n\\nbody"
  (^String [title body] (section 2 title body))
  (^String [level title body]
   (str (h level title) "\n\n" (->str body))))

(defn escape
  "Backslash-escape every CommonMark special character in `s` so the
   result renders as literal text."
  ^String [s]
  (str/replace (->str s) #"([\\`*_{}\[\]()#+\-!|>])" "\\\\$1"))

;; =============================================================================
;; SCI symbol entries
;; =============================================================================

(def ^:private symbol-entries
  [(sdk/symbol 'h1 h1
     {:doc "Render a level-1 heading: `# text`."
      :arglists '([text])
      :examples ["(md/h1 \"Patch report\")"]})
   (sdk/symbol 'h2 h2
     {:doc "Render a level-2 heading: `## text`."
      :arglists '([text])
      :examples ["(md/h2 \"Summary\")"]})
   (sdk/symbol 'h3 h3
     {:doc "Render a level-3 heading: `### text`."
      :arglists '([text])
      :examples ["(md/h3 \"Details\")"]})
   (sdk/symbol 'h4 h4
     {:doc "Render a level-4 heading: `#### text`."
      :arglists '([text])
      :examples ["(md/h4 \"Notes\")"]})
   (sdk/symbol 'h5 h5
     {:doc "Render a level-5 heading: `##### text`."
      :arglists '([text])
      :examples ["(md/h5 \"Caveat\")"]})
   (sdk/symbol 'h6 h6
     {:doc "Render a level-6 heading: `###### text`."
      :arglists '([text])
      :examples ["(md/h6 \"Footnote\")"]})
   (sdk/symbol 'h h
     {:doc "Render a heading at the given level (clamped to [1, 6])."
      :arglists '([level text])
      :examples ["(md/h 3 \"Step 1\")"]})

   (sdk/symbol 'p p
     {:doc "Plain paragraph; coerces nil to empty string."
      :arglists '([text])
      :examples ["(md/p \"Done.\")"]})
   (sdk/symbol 'bold bold
     {:doc "Bold inline span: `**text**`."
      :arglists '([text])
      :examples ["(md/bold \"important\")"]})
   (sdk/symbol 'italic italic
     {:doc "Italic inline span: `*text*`."
      :arglists '([text])
      :examples ["(md/italic \"subtle\")"]})
   (sdk/symbol 'bold-italic bold-italic
     {:doc "Bold-italic inline span: `***text***`."
      :arglists '([text])
      :examples ["(md/bold-italic \"!!!\")"]})
   (sdk/symbol 'strike strike
     {:doc "Strikethrough inline span: `~~text~~`."
      :arglists '([text])
      :examples ["(md/strike \"obsolete\")"]})
   (sdk/symbol 'code code
     {:doc "Inline code span: `` `text` ``."
      :arglists '([text])
      :examples ["(md/code \"vis/cat\")"]})
   (sdk/symbol 'kbd kbd
     {:doc "Keyboard-key inline span: `<kbd>text</kbd>`."
      :arglists '([text])
      :examples ["(md/kbd \"Ctrl+K\")"]})
   (sdk/symbol 'link link
     {:doc "Inline hyperlink: `[text](url)`. 3-arg form adds a tooltip title attribute. The TUI + Telegram channels render this as a clickable link wherever the surface supports it."
      :arglists '([text url] [text url title])
      :examples ["(md/link \"docs\" \"https://example.com\")"
                 "(md/link \"spec\" \"docs/spec.md\" \"Full spec\")"]})
   (sdk/symbol 'image image
     {:doc "Inline image: `![alt](url)`. 3-arg form embeds a tooltip title."
      :arglists '([alt url] [alt url title])
      :examples ["(md/image \"diagram\" \"./diagram.png\")"
                 "(md/image \"flow\" \"./flow.png\" \"Iteration flow\")"]})
   (sdk/symbol 'file-link file-link
     {:doc "Cite a workspace file. 1-arg renders `[path](path)`; 2-arg renders `[path:line](path#Lline)` so channels can jump to the exact line. Use this for every source-code reference an answer makes."
      :arglists '([path] [path line])
      :examples ["(md/file-link \"src/main.clj\")"
                 "(md/file-link \"src/main.clj\" 142)"]})
   (sdk/symbol 'anchor anchor
     {:doc "Same-document anchor link to a heading. 1-arg auto-slugifies the text; 2-arg takes an explicit slug."
      :arglists '([text] [text slug])
      :examples ["(md/anchor \"Patch report\")"
                 "(md/anchor \"Jump to summary\" \"summary\")"]})

   (sdk/symbol 'code-block code-block
     {:doc "Fenced code block. 1-arg omits language tag; 2-arg embeds it."
      :arglists '([code] [code lang])
      :examples ["(md/code-block \"(println :ok)\" \"clojure\")"
                 "(md/code-block \"plain text\")"]})
   (sdk/symbol 'blockquote blockquote
     {:doc "Quote every line of `text` with `> `."
      :arglists '([text])
      :examples ["(md/blockquote \"caveat\")"]})
   (sdk/value 'hr hr
     {:doc "Horizontal rule (`---`)."})
   (sdk/value 'br br
     {:doc "Hard line break suffix (CommonMark trailing-space convention)."})
   (sdk/symbol 'details details
     {:doc "GitHub-style collapsible block: `<details><summary>…</summary>…</details>`."
      :arglists '([summary body])
      :examples ["(md/details \"trace\" trace-text)"]})

   (sdk/symbol 'ul ul
     {:doc "Unordered list. `items` is a sequence; each entry becomes one `- item` line."
      :arglists '([items])
      :examples ["(md/ul [\"a\" \"b\" \"c\"])"]})
   (sdk/symbol 'ol ol
     {:doc "Ordered list, 1-based numbering."
      :arglists '([items])
      :examples ["(md/ol [\"first\" \"second\"])"]})
   (sdk/symbol 'checklist checklist
     {:doc "Task list: items can be `[text done?]` pairs or `{:text :done?}` maps."
      :arglists '([items])
      :examples ["(md/checklist [[\"done\" true] [\"todo\" false]])"]})

   (sdk/symbol 'table table
     {:doc "Markdown table. `headers` = vec of column titles; `rows` = vec of row vecs. Optional opts: `{:align [:left :center :right ...]}`."
      :arglists '([headers rows] [headers rows opts])
      :examples ["(md/table [\"file\" \"lines\"] [[\"a\" 12] [\"b\" 30]])"
                 "(md/table [\"k\" \"v\"] [[\"x\" 1]] {:align [:left :right]})"]})

   (sdk/symbol 'join join
     {:doc "Stitch block-level pieces with one BLANK line between (`\\n\\n`)."
      :arglists '([& parts])
      :examples ["(md/join (md/h1 \"x\") (md/p \"y\"))"]})
   (sdk/symbol 'lines lines
     {:doc "Stitch lines with a single newline between."
      :arglists '([& parts])
      :examples ["(md/lines \"a\" \"b\")"]})
   (sdk/symbol 'section section
     {:doc "Heading + body shortcut. Default level 2; 3-arg picks level."
      :arglists '([title body] [level title body])
      :examples ["(md/section \"Summary\" \"...\")"
                 "(md/section 3 \"Details\" \"...\")"]})
   (sdk/symbol 'escape escape
     {:doc "Backslash-escape every CommonMark special character in `s`."
      :arglists '([s])
      :examples ["(md/escape \"1 + 2 = *3*\")"]})])

(def markdown-symbols
  "All `md/`-aliased symbols exposed in the SCI sandbox."
  symbol-entries)

(def markdown-prompt
  "Prompt fragment listing the `md/` surface for the iteration prompt."
  (str
    "`md/` = programmatic markdown for `(answer …)`. Pure string builders, no templating layer.\n"
    "  Headings   (md/h1 \"...\") (md/h2 \"...\") (md/h3 \"...\") (md/h level \"...\")\n"
    "  Inline     (md/bold s) (md/italic s) (md/code s) (md/kbd s) (md/strike s)\n"
    "  Links      (md/link text url) (md/link text url title)        ; tooltip via title\n"
    "             (md/image alt url) (md/image alt url title)\n"
    "             (md/file-link path) (md/file-link path line)        ; CITE source files this way\n"
    "             (md/anchor text) (md/anchor text slug)               ; same-doc heading link\n"
    "  Block      (md/p s) (md/code-block s lang?) (md/blockquote s) md/hr md/br (md/details summary body)\n"
    "  Lists      (md/ul items) (md/ol items) (md/checklist [[t done?] ...])\n"
    "  Tables     (md/table headers rows) (md/table headers rows {:align [:left :center :right]})\n"
    "  Compose    (md/join …blocks) (md/lines …lines) (md/section title body) (md/escape s)\n"
    "Block fns return text WITHOUT trailing newline. Compose with (md/join …) and feed to (answer …).\n"
    "Cite source files with (md/file-link path line) so the TUI can jump to the line; use (md/link …) for URLs and (md/image alt url) for diagrams. Hand-rolled `[…](…)` strings drift from channel link conventions — the helpers stay aligned."))

(def markdown-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.markdown
     :ext/doc       "Programmatic markdown construction under the `md/` SCI alias. Headings, inline emphasis, lists, tables, code blocks, joiners. Pure string builders for assembling the argument to `(answer …)`."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.md :alias 'md}
     :ext/group     "foundation"
     :ext/prompt    (fn [_env] markdown-prompt)
     :ext/symbols   markdown-symbols}))

(sdk/register-extension! markdown-extension)
