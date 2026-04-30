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
   [com.blockether.vis.core :as vis]))

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

(defn- compose-text
  "Variadic-friendly text composer used by every inline / heading /
   block helper that takes \"text-ish\" parts. Mirrors `md/p`'s
   contract — nil dropped, sequential collections spliced one level,
   scalar parts coerced via `->str` — BUT joins with empty string so
   author-supplied whitespace stays authoritative. The LLM naturally
   writes
     (md/h3 \"Propozycja: \" (md/code \":foo/bar\") \" sentinel\")
   with the spaces baked into the literals; we don't second-guess.

   1-arg string short-circuits (no allocation, no expand-parts walk)
   so the original single-string shape pays nothing extra."
  ^String [parts]
  (cond
    ;; Common case: 1 string -> no allocation, no walk.
    (and (= 1 (count parts)) (string? (first parts)))
    (first parts)
    ;; 1 nil/scalar -> ->str directly. Must NOT enter this branch
    ;; for sequential single arg — those need splice via expand-parts
    ;; so `(md/h1 ["a" "b"])` reads as parts, not a thrown coll.
    (and (= 1 (count parts)) (not (sequential? (first parts))))
    (->str (first parts))
    :else
    (->> (expand-parts parts)
      (mapv ->str)
      (apply str))))

(defn h
  "Heading at level n (clamped [1, 6]). Variadic body — see md/h1."
  ^String [n & parts]
  (let [lvl (max 1 (min 6 (long n)))]
    (str (apply str (repeat lvl "#")) " " (compose-text parts))))

;; Variadic, like md/p: 1 string short-circuits (the common case);
;; multi-arg parts are concatenated (nil dropped, seqs spliced) so
;; an inline (md/code …) / (md/bold …) call inside a heading just
;; works instead of triggering ArityException.
(defn h1 ^String [& parts] (str "# "      (compose-text parts)))
(defn h2 ^String [& parts] (str "## "     (compose-text parts)))
(defn h3 ^String [& parts] (str "### "    (compose-text parts)))
(defn h4 ^String [& parts] (str "#### "   (compose-text parts)))
(defn h5 ^String [& parts] (str "##### "  (compose-text parts)))
(defn h6 ^String [& parts] (str "###### " (compose-text parts)))

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

;; Inline emphasis / spans — every helper variadic for the same
;; reason headings are: the LLM naturally composes (md/bold "foo "
;; (md/code "bar")) and would otherwise hit ArityException. 1-arg
;; string short-circuits in compose-text, so the canonical shape
;; pays nothing extra.
(defn bold        ^String [& parts] (str "**"     (compose-text parts) "**"))
(defn italic      ^String [& parts] (str "*"      (compose-text parts) "*"))
(defn bold-italic ^String [& parts] (str "***"    (compose-text parts) "***"))
(defn strike      ^String [& parts] (str "~~"     (compose-text parts) "~~"))
(defn code        ^String [& parts] (str "`"      (compose-text parts) "`"))
(defn kbd         ^String [& parts] (str "<kbd>"  (compose-text parts) "</kbd>"))

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
  "Quote each line with `> `. Variadic body — nil dropped, seqs
   spliced, parts concatenated; the resulting text is split on `\n`
   so every line gets the `> ` prefix. Empty -> `>`."
  ^String [& parts]
  (let [text (compose-text parts)]
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
  "Standalone `<summary>…</summary>` tag — disclosure label inside
   `(md/details …)`. Variadic body, same rules as the inline
   helpers: nil dropped, seqs spliced, parts concatenated.

     (md/summary \"Logs\")
     (md/summary (md/bold \"Logs\") \" (\" (md/code \"42\") \")\")

   `md/details` lifts a wrapped <summary> part to the canonical
   first-child slot regardless of arg position."
  ^String [& parts]
  (str "<summary>" (compose-text parts) "</summary>"))

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

(defn- normalize-list-items
  "Variadic helper: if the caller passed exactly one sequential arg,
   unwrap it; otherwise treat `items` itself as the list. Then drop
   nils so `(md/ul nil)`, `(md/ul [])`, `(md/ul nil nil)` all yield
   an empty seq. Returning a SEQ of plain values — callers map their
   own per-item formatting on top."
  [items]
  (let [unwrapped (if (and (= 1 (count items)) (sequential? (first items)))
                    (first items)
                    items)]
    (remove nil? unwrapped)))

(defn- item-text
  "Render a single list item. Strings pass through; sequentials
   are composed via compose-text so inline helpers inside a vector
   item just work:

     (item-text "plain string")              ;=> "plain string"
     (item-text ["a " (md/code "b") " c"])  ;=> "a `b` c"
  [x]
  (if (sequential? x)
    (compose-text x)
    (->str x)))

(defn ul
  "Unordered list. Accepts a single seq or variadic args.
   Each item may be a string or a sequential of parts (strings +
   inline helpers) composed into one item text.

     (md/ul [\"a\" \"b\"])
     (md/ul [[\"The \" (md/code \"foo\") \" works\"] \"plain\"])

   One `- item` per element, newline-joined, no trailing newline."
  [& items]
  (->> (normalize-list-items items)
    (map #(str "- " (item-text %)))
    (str/join "\n")))

(defn ol
  "Ordered list, 1-based numbering. Accepts a single seq or variadic args.
   Each item may be a string or a sequential of parts composed into
   one item text.

     (md/ol [\"a\" \"b\"])
     (md/ol [[\"Step \" (md/code \"1\") \": go\"] \"done\"])"
  [& items]
  [& items]
  (->> (normalize-list-items items)
    (map-indexed (fn [i x] (str (inc i) ". " (item-text x))))
    (str/join "\n")))

(defn checklist
  "GitHub task list. Accepts a single seq or variadic args.
   items are [text done?] pairs or {:text … :done? bool} maps.
   (checklist [[\"a\" true] [\"b\" false]]) and
   (checklist [\"a\" true] [\"b\" false]) both work."
  [& items]
  (->> (normalize-list-items items)
    (map (fn [it]
           (let [[t d?] (cond
                          (map? it)        [(:text it) (:done? it)]
                          (sequential? it) [(first it) (second it)]
                          :else            [it false])]
             (str "- [" (if d? "x" " ") "] " (item-text t)))))
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
  [(vis/symbol 'h1 h1
     {:doc "H1: `# text`. Variadic body — nil dropped, seqs spliced, parts concatenated (author owns whitespace)."
      :arglists '([& parts])
      :examples ["(md/h1 \"Patch report\")"
                 "(md/h1 \"Build of \" (md/code \"v1.2.3\"))"]})
   (vis/symbol 'h2 h2
     {:doc "H2: `## text`. Variadic body — see md/h1."
      :arglists '([& parts])
      :examples ["(md/h2 \"Summary\")"]})
   (vis/symbol 'h3 h3
     {:doc "H3: `### text`. Variadic body — see md/h1."
      :arglists '([& parts])
      :examples ["(md/h3 \"Proposal: \" (md/code \":vis/silent\") \" sentinel\")"]})
   (vis/symbol 'h4 h4
     {:doc "H4: `#### text`. Variadic body — see md/h1."
      :arglists '([& parts])
      :examples ["(md/h4 \"Notes\")"]})
   (vis/symbol 'h5 h5
     {:doc "H5: `##### text`. Variadic body — see md/h1."
      :arglists '([& parts])
      :examples ["(md/h5 \"Caveat\")"]})
   (vis/symbol 'h6 h6
     {:doc "H6: `###### text`. Variadic body — see md/h1."
      :arglists '([& parts])
      :examples ["(md/h6 \"Footnote\")"]})
   (vis/symbol 'h h
     {:doc "Heading at level n (clamped [1, 6]). Variadic body — see md/h1."
      :arglists '([level & parts])
      :examples ["(md/h 3 \"Step 1\")"
                 "(md/h 2 \"Build of \" (md/code \"v1.2.3\"))"]})

   (vis/symbol 'p p
     {:doc "Paragraph. Joins parts with single space; nil dropped; seqs splice one level (matches md/join / md/lines)."
      :arglists '([& parts])
      :examples ["(md/p \"Done.\")"
                 "(md/p \"Patched\" n \"files\")"
                 "(md/p \"Status:\" (md/bold \"OK\"))"]})
   (vis/symbol 'bold bold
     {:doc "Bold span: `**text**`. Variadic — parts concatenated, nil dropped, seqs spliced."
      :arglists '([& parts])
      :examples ["(md/bold \"important\")"
                 "(md/bold \"build \" (md/code \"v1.2.3\"))"]})
   (vis/symbol 'strong bold
     {:doc "Bold span: `**text**` (HTML-semantic alias for `md/bold`). Variadic — see md/bold."
      :arglists '([& parts])
      :examples ["(md/strong \"important\")"]})
   (vis/symbol 'italic italic
     {:doc "Italic span: `*text*`. Variadic — see md/bold."
      :arglists '([& parts])
      :examples ["(md/italic \"subtle\")"]})
   (vis/symbol 'em italic
     {:doc "Italic span: `*text*` (HTML-semantic alias for `md/italic`). Variadic — see md/bold."
      :arglists '([& parts])
      :examples ["(md/em \"subtle\")"]})
   (vis/symbol 'bold-italic bold-italic
     {:doc "Bold-italic span: `***text***`. Variadic — see md/bold."
      :arglists '([& parts])
      :examples ["(md/bold-italic \"!!!\")"]})
   (vis/symbol 'strike strike
     {:doc "Strikethrough span: `~~text~~`. Variadic — see md/bold."
      :arglists '([& parts])
      :examples ["(md/strike \"obsolete\")"]})
   (vis/symbol 'code code
     {:doc "Inline code span: `` `text` ``. Variadic — parts concatenated."
      :arglists '([& parts])
      :examples ["(md/code \"v/cat\")"
                 "(md/code \"v/\" tool-name)"]})
   ;; `summary` is registered alongside the inline tag-style helpers
   ;; (kbd / bold / italic) because it produces a single bare HTML
   ;; tag, not a block. Composes with `details` via tag-passthrough.
   ;; Intentionally NOT mentioned in `markdown-prompt` — callers who
   ;; need it can discover it via `(symbol-info 'md/summary)`; we
   ;; don't want every answer reaching for collapsible UI.
   (vis/symbol 'summary summary
     {:doc "Standalone `<summary>…</summary>` tag for use inside (md/details …). Variadic — parts concatenated. md/details lifts it to the canonical first-child slot regardless of arg position."
      :arglists '([& parts])
      :examples ["(md/summary \"Logs\")"
                 "(md/summary (md/bold \"Logs\") \" (\" (md/code \"42\") \")\")"
                 "(md/details (md/summary (md/bold \"Logs\")) body)"]})

   (vis/symbol 'kbd kbd
     {:doc "Keyboard span: `<kbd>text</kbd>`. Variadic — parts concatenated."
      :arglists '([& parts])
      :examples ["(md/kbd \"Ctrl+K\")"]})
   (vis/symbol 'link link
     {:doc "Hyperlink: `[text](url)`. 3-arg adds tooltip title attr. TUI + Telegram render as clickable wherever the surface supports it."
      :arglists '([text url] [text url title])
      :examples ["(md/link \"docs\" \"https://example.com\")"
                 "(md/link \"spec\" \"docs/spec.md\" \"Full spec\")"]})
   (vis/symbol 'image image
     {:doc "Image: `![alt](url)`. 3-arg embeds tooltip title."
      :arglists '([alt url] [alt url title])
      :examples ["(md/image \"diagram\" \"./diagram.png\")"
                 "(md/image \"flow\" \"./flow.png\" \"Iteration flow\")"]})
   (vis/symbol 'file-link file-link
     {:doc "Cite a workspace file. 1-arg -> `[path](path)`; 2-arg -> `[path:line](path#Lline)` so channels can jump to the exact line. Use for every source-code reference."
      :arglists '([path] [path line])
      :examples ["(md/file-link \"src/main.clj\")"
                 "(md/file-link \"src/main.clj\" 142)"]})
   (vis/symbol 'anchor anchor
     {:doc "Same-doc heading anchor. 1-arg auto-slugifies; 2-arg takes explicit slug."
      :arglists '([text] [text slug])
      :examples ["(md/anchor \"Patch report\")"
                 "(md/anchor \"Jump to summary\" \"summary\")"]})

   (vis/symbol 'code-block code-block
     {:doc "Fenced code block. 1-arg = no language; 2-arg embeds it."
      :arglists '([code] [code lang])
      :examples ["(md/code-block \"(println :ok)\" \"clojure\")"
                 "(md/code-block \"plain text\")"]})
   (vis/symbol 'blockquote blockquote
     {:doc "Quote each line with `> `. Variadic — parts concatenated then split on \"\\n\"."
      :arglists '([& parts])
      :examples ["(md/blockquote \"caveat\")"
                 "(md/blockquote \"line1\\nline2\")"]})
   (vis/symbol 'quote blockquote
     {:doc "Quote each line with `> ` (shorter alias for `md/blockquote`). Variadic — see md/blockquote."
      :arglists '([& parts])
      :examples ["(md/quote \"caveat\")"]})
   (vis/value 'hr hr
     {:doc "Horizontal rule (`---`)."})
   (vis/value 'br br
     {:doc "Hard line break suffix (CommonMark trailing-spaces)."})
   (vis/symbol 'details details
     {:doc (str "Collapsible block: `<details>…</details>`. Variadic: parts splice "
             "(nil dropped, seqs flattened), body parts blank-line joined. A `<summary>…</summary>` "
             "part — produced by md/summary or hand-rolled — is lifted to the canonical "
             "first-child slot regardless of arg position. Plain strings = body; no auto-wrap.")
      :arglists '([& parts])
      :examples ["(md/details (md/summary \"Logs\") body)"
                 "(md/details intro snippet (md/summary \"Trace\"))"
                 "(md/details para1 para2)"]})

   (vis/symbol 'ul ul
     {:doc "Unordered list. items = seq; each entry becomes one `- item` line."
      :arglists '([items])
      :examples ["(md/ul [\"a\" \"b\" \"c\"])"]})
   (vis/symbol 'ol ol
     {:doc "Ordered list, 1-based numbering."
      :arglists '([items])
      :examples ["(md/ol [\"first\" \"second\"])"]})
   (vis/symbol 'checklist checklist
     {:doc "Task list: items = `[text done?]` pairs OR `{:text :done?}` maps."
      :arglists '([items])
      :examples ["(md/checklist [[\"done\" true] [\"todo\" false]])"]})

   (vis/symbol 'table table
     {:doc "Markdown table. headers = vec of column titles; rows = vec of row vecs. Opts: `{:align [:left :center :right …]}`."
      :arglists '([headers rows] [headers rows opts])
      :examples ["(md/table [\"file\" \"lines\"] [[\"a\" 12] [\"b\" 30]])"
                 "(md/table [\"k\" \"v\"] [[\"x\" 1]] {:align [:left :right]})"]})

   (vis/symbol 'join join
     {:doc "Stitch block pieces with one BLANK line (`\\n\\n`)."
      :arglists '([& parts])
      :examples ["(md/join (md/h1 \"x\") (md/p \"y\"))"]})
   (vis/symbol 'lines lines
     {:doc "Stitch lines with single newline."
      :arglists '([& parts])
      :examples ["(md/lines \"a\" \"b\")"]})
   (vis/symbol 'section section
     {:doc "Heading + body shortcut. Default level 2; 3-arg picks level."
      :arglists '([title body] [level title body])
      :examples ["(md/section \"Summary\" \"…\")"
                 "(md/section 3 \"Details\" \"…\")"]})
   (vis/symbol 'escape escape
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
    "  Headings   (md/h1 …parts) (md/h2 …parts) (md/h3 …parts) (md/h level …parts)\n"
    "             ; variadic: (md/h3 \"Build \" (md/code \"v1.2.3\")) just works — nil dropped, seqs spliced, no whitespace inserted.\n"
    "  Inline     (md/bold …parts | md/strong …) (md/italic … | md/em …) (md/code …) (md/kbd …) (md/strike …)\n"
    "             ; semantic aliases: md/strong=md/bold, md/em=md/italic, md/quote=md/blockquote.\n"
    "             ; ALL inline / heading helpers are variadic (nil dropped, seqs spliced, parts concatenated):\n"
    "             ; (md/bold \"build \" (md/code \"v1.2.3\")) -> **build `v1.2.3`** — author owns whitespace.\n"
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
  (vis/extension
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

(vis/register-extension! markdown-extension)
