(ns com.blockether.vis.ext.foundation.markdown
  "Programmatic markdown for SCI alias `md/`. Pure string builders.

   Every fn returns String, nil -> \"\", missing args via `(str x)`.

     Headings    md/h1 … md/h6, md/h
     Inline      md/bold, md/italic, md/bold-italic, md/strike,
                 md/code, md/kbd, md/link, md/image
     Block       md/p, md/code-block, md/blockquote, md/hr, md/br,
                 md/details
     Lists       md/ul, md/ol, md/checklist
     Tables      md/table
     Compose     md/join (blank line), md/lines (newline),
                 md/section, md/escape

   Block fns return text WITHOUT trailing newline. Stitch with
   md/join / md/lines, feed to (answer …).

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
  "Coerce x to String. nil -> \"\". String passes through. Other scalars
   via `(str x)`. Sequential (list / lazy-seq / vec) -> throw ex-info
   so a stray `(str lazy-seq)` doesn't leak `clojure.lang.LazySeq@<hex>`
   into the answer. Splice via md/join / md/lines, or eagerly
   stringify (mapv md/p xs)."
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

(declare expand-parts)

;; =============================================================================
;; Headings
;; =============================================================================

(defn h
  "Heading at level n (clamped [1, 6])."
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

(defn p
  "Paragraph. Joins parts with single space. nil dropped. Seqs splice
   one level (matches md/join / md/lines).

     (md/p \"Done.\")                  => \"Done.\"
     (md/p nil)                      => \"\"
     (md/p \"Patched\" n \"files\")       => \"Patched 12 files\"
     (md/p \"Status:\" (md/bold \"OK\")) => \"Status: **OK**\"
     (md/p \"Tags:\" (map md/code tags)) ; spliced + space-joined

   Glued concat -> use `(str …)`; md/p adds whitespace, never markdown."
  ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join " ")))

(defn bold ^String [s] (str "**" (->str s) "**"))
(defn italic ^String [s] (str "*" (->str s) "*"))
(defn bold-italic ^String [s] (str "***" (->str s) "***"))
(defn strike ^String [s] (str "~~" (->str s) "~~"))
(defn code ^String [s] (str "`" (->str s) "`"))
(defn kbd ^String [s] (str "<kbd>" (->str s) "</kbd>"))

(defn- escape-title-attr
  "Escape `\"` so link/image title attr stays well-formed."
  ^String [title]
  (str/replace (->str title) "\"" "\\\""))

(defn link
  "Inline hyperlink. text + url coerced. 3-arg adds tooltip title attr:
     (md/link \"docs\" \"https://example.com\" \"Project docs\")
   -> `[docs](https://example.com \"Project docs\")`."
  (^String [text url] (link text url nil))
  (^String [text url title]
   (let [t (->str title)]
     (if (str/blank? t)
       (str "[" (->str text) "](" (->str url) ")")
       (str "[" (->str text) "](" (->str url)
         " \"" (escape-title-attr t) "\")")))))

(defn image
  "Inline image. alt + url coerced. 3-arg adds tooltip title (same
   shape as md/link)."
  (^String [alt url] (image alt url nil))
  (^String [alt url title]
   (str "!" (link alt url title))))

(defn file-link
  "Cite a workspace file. Target = literal path; channels resolve
   relative to `(fs/cwd)` and make it clickable.

     (md/file-link \"src/foo.clj\")     ; [src/foo.clj](src/foo.clj)
     (md/file-link \"src/foo.clj\" 142) ; [src/foo.clj:142](src/foo.clj#L142)

   Use for every source-code citation; beats hand-rolled `[…](…)` and
   keeps line anchors uniform."
  (^String [path] (file-link path nil))
  (^String [path line]
   (let [p (->str path)]
     (if line
       (link (str p ":" line) (str p "#L" line))
       (link p p)))))

(defn anchor
  "Same-doc heading anchor. text = label; slug = GitHub-style slug.
   Omit slug -> auto-slug (lower-case, drop punct, collapse ws to `-`).

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
  "Fenced code block. 1-arg = no language; 2-arg embeds it in the
   opening fence (`clojure`, `bash`, `edn`, …)."
  (^String [code] (code-block code nil))
  (^String [code lang]
   (let [body (->str code)
         body (if (str/ends-with? body "\n") body (str body "\n"))]
     (str "```" (when lang (->str lang)) "\n"
       body
       "```"))))

(defn blockquote
  "Quote each line of s with `> `. Multi-line preserved; empty -> `>`."
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
  "Hard line break suffix. Append + newline -> `<br/>` (CommonMark
   trailing-spaces)."
  "  ")

(defn summary
  "Standalone `<summary>x</summary>` tag — disclosure label inside
   `(md/details …)`. Use when the label needs styled spans (md/bold,
   md/code, …); md/details lifts a wrapped <summary> part to the
   canonical first-child slot regardless of arg position.

     (md/details (md/summary (md/bold \"Logs\")) body)
     ; -> <summary><strong>Logs</strong></summary>…"
  ^String [s]
  (str "<summary>" (->str s) "</summary>"))

(defn- summary-tagged?
  "True when s is a `<summary>…</summary>` block (md/summary or hand-
   rolled). Drives md/details summary-lifting."
  [s]
  (and (string? s)
    (str/starts-with? s "<summary>")
    (str/ends-with? s "</summary>")))

(defn details
  "GitHub-style collapsible block. One API: `(md/details & parts)`.
   Parts splice one level (nil dropped, seqs flattened — same rules
   as md/join / md/lines), each ->str-coerced, body parts blank-line
   joined.

   Plain strings = body. For a disclosure label, write `(md/summary X)`
   explicitly. No auto-wrap, no first-arg magic — one shape, one rule.

   Summary lifting: a `<summary>…</summary>` part placed anywhere
   among the args is moved to the canonical first-child slot inside
   `<details>`. HTML5 + GitHub require the label as first flow child;
   the lift means callers never have to remember the order. Body
   order preserved.

   At most one <summary> per block; ≥2 throws ex-info. Zero summary
   parts OK — block has no label, browsers default to 'Details'.

     (md/details (md/summary \"Logs\") body)              ; canonical
     (md/details intro snippet (md/summary \"Trace\"))    ; lifted
     (md/details para1 para2)                           ; label-less
     (md/details (md/summary (md/bold \"Trace\")) body)   ; styled label
     (md/details (md/summary \"Files\") (mapv md/p xs))   ; spliced body"
  ^String [& parts]
  (let [parts (expand-parts parts)
        strs  (mapv ->str parts)
        {sums true bodies false} (group-by summary-tagged? strs)
        sum   (first sums)
        body  (when (seq bodies) (str/join "\n\n" bodies))]
    (when (> (count sums) 1)
      (throw (ex-info
               (str "md/details got " (count sums) " <summary>…</summary> "
                 "parts — at most one is allowed per block. Lift the "
                 "extras into the body, or compose nested <details> "
                 "blocks if you need multiple disclosures.")
               {:summary-count (count sums)})))
    (cond
      (and sum body) (str "<details>\n" sum "\n\n" body "\n\n</details>")
      sum            (str "<details>\n" sum "\n\n</details>")
      body           (str "<details>\n" body "\n\n</details>")
      :else          "<details>\n\n</details>")))

;; =============================================================================
;; Lists
;; =============================================================================

(defn ul
  "Unordered list. items = seq of strings (or str-coercible). One
   `- item` per element, newline-joined, no trailing newline."
  ^String [items]
  (->> (or items [])
    (map #(str "- " (->str %)))
    (str/join "\n")))

(defn ol
  "Ordered list, 1-based numbering."
  ^String [items]
  (->> (or items [])
    (map-indexed (fn [i x] (str (inc i) ". " (->str x))))
    (str/join "\n")))

(defn checklist
  "GitHub task list. items =
     - vec of [text done?] pairs, OR
     - vec of {:text … :done? bool} maps."
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
  "Markdown table.

     headers — vec of column header values.
     rows    — vec of row vecs; short rows right-padded with empty cells.

   Opts:
     :align — vec of :left | :center | :right | :default per column."
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
   variadic args with seq-producing forms (map, for, map-indexed,
   keep) without LazySeq toString leaks. Nested beyond one level
   stays caller's responsibility — (md/join nested-tree) surfaces as
   an error rather than silently dropping structure."
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
  "Stitch block pieces with one BLANK line (`\\n\\n`). nil dropped.
   Seqs splice one level — `(md/join (mapv render xs))` and
   `(md/join (md/h1 \"…\") (map render xs))` both Just Work."
  ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n\n")))

(defn lines
  "Stitch parts with single newline. nil dropped. Seqs splice one
   level — same rules as join."
  ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n")))

(defn section
  "Heading + body shortcut. Default level 2.
     (md/section \"Summary\" body)    -> ## Summary\\n\\nbody
     (md/section 3 \"Details\" body)  -> ### Details\\n\\nbody"
  (^String [title body] (section 2 title body))
  (^String [level title body]
   (str (h level title) "\n\n" (->str body))))

(defn escape
  "Backslash-escape every CommonMark special character in s -> renders
   as literal text."
  ^String [s]
  (str/replace (->str s) #"([\\`*_{}\[\]()#+\-!|>])" "\\\\$1"))

;; =============================================================================
;; SCI symbol entries
;; =============================================================================

(def ^:private symbol-entries
  [(sdk/symbol 'h1 h1
     {:doc "H1: `# text`."
      :arglists '([text])
      :examples ["(md/h1 \"Patch report\")"]})
   (sdk/symbol 'h2 h2
     {:doc "H2: `## text`."
      :arglists '([text])
      :examples ["(md/h2 \"Summary\")"]})
   (sdk/symbol 'h3 h3
     {:doc "H3: `### text`."
      :arglists '([text])
      :examples ["(md/h3 \"Details\")"]})
   (sdk/symbol 'h4 h4
     {:doc "H4: `#### text`."
      :arglists '([text])
      :examples ["(md/h4 \"Notes\")"]})
   (sdk/symbol 'h5 h5
     {:doc "H5: `##### text`."
      :arglists '([text])
      :examples ["(md/h5 \"Caveat\")"]})
   (sdk/symbol 'h6 h6
     {:doc "H6: `###### text`."
      :arglists '([text])
      :examples ["(md/h6 \"Footnote\")"]})
   (sdk/symbol 'h h
     {:doc "Heading at level n (clamped [1, 6])."
      :arglists '([level text])
      :examples ["(md/h 3 \"Step 1\")"]})

   (sdk/symbol 'p p
     {:doc "Paragraph. Joins parts with single space; nil dropped; seqs splice one level (matches md/join / md/lines)."
      :arglists '([& parts])
      :examples ["(md/p \"Done.\")"
                 "(md/p \"Patched\" n \"files\")"
                 "(md/p \"Status:\" (md/bold \"OK\"))"]})
   (sdk/symbol 'bold bold
     {:doc "Bold span: `**text**`."
      :arglists '([text])
      :examples ["(md/bold \"important\")"]})
   (sdk/symbol 'strong bold
     {:doc "Bold span: `**text**` (HTML-semantic alias for `md/bold`)."
      :arglists '([text])
      :examples ["(md/strong \"important\")"]})
   (sdk/symbol 'italic italic
     {:doc "Italic span: `*text*`."
      :arglists '([text])
      :examples ["(md/italic \"subtle\")"]})
   (sdk/symbol 'em italic
     {:doc "Italic span: `*text*` (HTML-semantic alias for `md/italic`)."
      :arglists '([text])
      :examples ["(md/em \"subtle\")"]})
   (sdk/symbol 'bold-italic bold-italic
     {:doc "Bold-italic span: `***text***`."
      :arglists '([text])
      :examples ["(md/bold-italic \"!!!\")"]})
   (sdk/symbol 'strike strike
     {:doc "Strikethrough span: `~~text~~`."
      :arglists '([text])
      :examples ["(md/strike \"obsolete\")"]})
   (sdk/symbol 'code code
     {:doc "Inline code span: `` `text` ``."
      :arglists '([text])
      :examples ["(md/code \"v/cat\")"]})
   ;; `summary` is registered alongside the inline tag-style helpers
   ;; (kbd / bold / italic) because it produces a single bare HTML
   ;; tag, not a block. Composes with `details` via tag-passthrough.
   ;; Intentionally NOT mentioned in `markdown-prompt` — callers who
   ;; need it can discover it via `(symbol-info 'md/summary)`; we
   ;; don't want every answer reaching for collapsible UI.
   (sdk/symbol 'summary summary
     {:doc "Standalone `<summary>text</summary>` tag for use inside (md/details …). md/details lifts it to the canonical first-child slot regardless of arg position."
      :arglists '([text])
      :examples ["(md/summary \"Logs\")"
                 "(md/details (md/summary (md/bold \"Logs\")) body)"
                 "(md/details intro snippet (md/summary \"Trace\"))"]})

   (sdk/symbol 'kbd kbd
     {:doc "Keyboard span: `<kbd>text</kbd>`."
      :arglists '([text])
      :examples ["(md/kbd \"Ctrl+K\")"]})
   (sdk/symbol 'link link
     {:doc "Hyperlink: `[text](url)`. 3-arg adds tooltip title attr. TUI + Telegram render as clickable wherever the surface supports it."
      :arglists '([text url] [text url title])
      :examples ["(md/link \"docs\" \"https://example.com\")"
                 "(md/link \"spec\" \"docs/spec.md\" \"Full spec\")"]})
   (sdk/symbol 'image image
     {:doc "Image: `![alt](url)`. 3-arg embeds tooltip title."
      :arglists '([alt url] [alt url title])
      :examples ["(md/image \"diagram\" \"./diagram.png\")"
                 "(md/image \"flow\" \"./flow.png\" \"Iteration flow\")"]})
   (sdk/symbol 'file-link file-link
     {:doc "Cite a workspace file. 1-arg -> `[path](path)`; 2-arg -> `[path:line](path#Lline)` so channels can jump to the exact line. Use for every source-code reference."
      :arglists '([path] [path line])
      :examples ["(md/file-link \"src/main.clj\")"
                 "(md/file-link \"src/main.clj\" 142)"]})
   (sdk/symbol 'anchor anchor
     {:doc "Same-doc heading anchor. 1-arg auto-slugifies; 2-arg takes explicit slug."
      :arglists '([text] [text slug])
      :examples ["(md/anchor \"Patch report\")"
                 "(md/anchor \"Jump to summary\" \"summary\")"]})

   (sdk/symbol 'code-block code-block
     {:doc "Fenced code block. 1-arg = no language; 2-arg embeds it."
      :arglists '([code] [code lang])
      :examples ["(md/code-block \"(println :ok)\" \"clojure\")"
                 "(md/code-block \"plain text\")"]})
   (sdk/symbol 'blockquote blockquote
     {:doc "Quote each line of text with `> `."
      :arglists '([text])
      :examples ["(md/blockquote \"caveat\")"]})
   (sdk/symbol 'quote blockquote
     {:doc "Quote each line of text with `> ` (shorter alias for `md/blockquote`)."
      :arglists '([text])
      :examples ["(md/quote \"caveat\")"]})
   (sdk/value 'hr hr
     {:doc "Horizontal rule (`---`)."})
   (sdk/value 'br br
     {:doc "Hard line break suffix (CommonMark trailing-spaces)."})
   (sdk/symbol 'details details
     {:doc (str "Collapsible block: `<details>…</details>`. Variadic: parts splice "
             "(nil dropped, seqs flattened), body parts blank-line joined. A `<summary>…</summary>` "
             "part — produced by md/summary or hand-rolled — is lifted to the canonical "
             "first-child slot regardless of arg position. Plain strings = body; no auto-wrap.")
      :arglists '([& parts])
      :examples ["(md/details (md/summary \"Logs\") body)"
                 "(md/details intro snippet (md/summary \"Trace\"))"
                 "(md/details para1 para2)"]})

   (sdk/symbol 'ul ul
     {:doc "Unordered list. items = seq; each entry becomes one `- item` line."
      :arglists '([items])
      :examples ["(md/ul [\"a\" \"b\" \"c\"])"]})
   (sdk/symbol 'ol ol
     {:doc "Ordered list, 1-based numbering."
      :arglists '([items])
      :examples ["(md/ol [\"first\" \"second\"])"]})
   (sdk/symbol 'checklist checklist
     {:doc "Task list: items = `[text done?]` pairs OR `{:text :done?}` maps."
      :arglists '([items])
      :examples ["(md/checklist [[\"done\" true] [\"todo\" false]])"]})

   (sdk/symbol 'table table
     {:doc "Markdown table. headers = vec of column titles; rows = vec of row vecs. Opts: `{:align [:left :center :right …]}`."
      :arglists '([headers rows] [headers rows opts])
      :examples ["(md/table [\"file\" \"lines\"] [[\"a\" 12] [\"b\" 30]])"
                 "(md/table [\"k\" \"v\"] [[\"x\" 1]] {:align [:left :right]})"]})

   (sdk/symbol 'join join
     {:doc "Stitch block pieces with one BLANK line (`\\n\\n`)."
      :arglists '([& parts])
      :examples ["(md/join (md/h1 \"x\") (md/p \"y\"))"]})
   (sdk/symbol 'lines lines
     {:doc "Stitch lines with single newline."
      :arglists '([& parts])
      :examples ["(md/lines \"a\" \"b\")"]})
   (sdk/symbol 'section section
     {:doc "Heading + body shortcut. Default level 2; 3-arg picks level."
      :arglists '([title body] [level title body])
      :examples ["(md/section \"Summary\" \"…\")"
                 "(md/section 3 \"Details\" \"…\")"]})
   (sdk/symbol 'escape escape
     {:doc "Backslash-escape every CommonMark special character in s."
      :arglists '([s])
      :examples ["(md/escape \"1 + 2 = *3*\")"]})])

(def markdown-symbols
  "All `md/`-aliased symbols exposed in the SCI sandbox."
  symbol-entries)

(def markdown-prompt
  "Prompt fragment listing the `md/` surface for the iteration prompt."
  (str
    "`md/` = markdown for (answer …). Pure string builders, no templating.\n"
    "  Headings   (md/h1 \"…\") (md/h2 \"…\") (md/h3 \"…\") (md/h level \"…\")\n"
    "  Inline     (md/bold s | md/strong s) (md/italic s | md/em s) (md/code s) (md/kbd s) (md/strike s)\n"
    "             ; md/strong = md/bold; md/em = md/italic; md/quote = md/blockquote (semantic aliases)\n"
    "  Links      (md/link text url) (md/link text url title)   ; tooltip via title\n"
    "             (md/image alt url) (md/image alt url title)\n"
    "             (md/file-link path) (md/file-link path line)  ; CITE source files this way\n"
    "             (md/anchor text) (md/anchor text slug)         ; same-doc heading link\n"
    "  Block      (md/p …parts) (md/code-block s lang?) (md/blockquote s) md/hr md/br (md/details …parts)\n"
    "             ; md/p joins parts with single space; nil dropped; seqs spliced (like md/join / md/lines).\n"
    "  Lists      (md/ul items) (md/ol items) (md/checklist [[t done?] …])\n"
    "  Tables     (md/table headers rows) (md/table headers rows {:align [:left :center :right]})\n"
    "  Compose    (md/join …blocks) (md/lines …lines) (md/section title body) (md/escape s)\n"
    "Block fns return text WITHOUT trailing newline. Stitch with (md/join …), feed to (answer …).\n"
    "Cite source via (md/file-link path line) -> TUI jumps to line. (md/link …) for URLs, (md/image alt url) for diagrams. Hand-rolled `[…](…)` drifts from channel conventions; the helpers stay aligned."))

(def markdown-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.markdown
     :ext/doc       "Markdown under SCI alias `md/`. Headings, inline emphasis, lists, tables, code blocks, joiners. Pure string builders for the (answer …) argument."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.md :alias 'md}
     :ext/kind      "foundation"
     :ext/prompt    (fn [_env] markdown-prompt)
     :ext/symbols   markdown-symbols}))

(sdk/register-extension! markdown-extension)
