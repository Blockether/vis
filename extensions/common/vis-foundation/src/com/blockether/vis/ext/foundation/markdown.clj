(ns com.blockether.vis.ext.foundation.markdown
  "Programmatic markdown for SCI alias `v/`. Pure string builders.

   Every fn returns String, nil -> \"\", missing args via `(str x)`.

     Headings    v/h1 … v/h6, v/h
     Inline      v/bold, v/italic, v/bold-italic, v/strike,
                 v/code, v/kbd, v/link, v/image
     Block       v/p, v/code-block, v/blockquote, v/hr, v/br,
                 v/details
     Lists       v/li, v/ul, v/ol, v/checklist
     Tables      v/table
     Compose     v/join (blank line), v/lines (newline),
                 v/section, v/escape

   Block fns return text WITHOUT trailing newline. Stitch with
   v/join / v/lines, feed to (answer …).

     (answer
       (v/join
         (v/h1 \"Patch report\")
         (v/p \"Three files touched.\")
         (v/table [\"file\" \"+/-\"]
                   [[\"core.clj\" \"+12 / -4\"]
                    [\"loop.clj\" \"+0 / -38\"]])
         (v/h2 \"Next\")
         (v/ul [\"Run verify.sh\" \"Update CHANGELOG\"])))"
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
   into the answer. Splice via v/join / v/lines, or eagerly
   stringify (mapv v/p xs)."
  ^String [x]
  (cond
    (nil? x)        ""
    (string? x)     x
    (sequential? x) (throw (ex-info
                             (str "md helper got a sequential collection where a string was expected. "
                               "Splice with (v/join …) / (v/lines …), or build the row eagerly: "
                               "(mapv v/p xs), (into [] (map render) xs), (str/join \"\\n\" xs).")
                             {:value-class (.getName (class x))
                              :sample      (->> x (take 3) (mapv #(if (string? %) % (pr-str %))))}))
    :else           (str x)))

(declare expand-parts)

;; =============================================================================
;; Headings
;; =============================================================================

(defn- compose-text
  "Variadic-friendly text composer used by every inline / heading /
   block helper that takes \"text-ish\" parts. Mirrors `v/p`'s
   contract — nil dropped, sequential collections spliced one level,
   scalar parts coerced via `->str` — BUT joins with empty string so
   author-supplied whitespace stays authoritative. The LLM naturally
   writes
     (v/h3 \"Propozycja: \" (v/code \":foo/bar\") \" sentinel\")
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
    ;; so `(v/h1 ["a" "b"])` reads as parts, not a thrown coll.
    (and (= 1 (count parts)) (not (sequential? (first parts))))
    (->str (first parts))
    :else
    (->> (expand-parts parts)
      (mapv ->str)
      (apply str))))

(defn h
  "Heading at level n (clamped [1, 6]). Variadic body — see v/h1."
  ^String [n & parts]
  (let [lvl (max 1 (min 6 (long n)))]
    (str (apply str (repeat lvl "#")) " " (compose-text parts))))

;; Variadic, like v/p: 1 string short-circuits (the common case);
;; multi-arg parts are concatenated (nil dropped, seqs spliced) so
;; an inline (v/code …) / (v/bold …) call inside a heading just
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
   one level (matches v/join / v/lines).

     (v/p \"Done.\")                  => \"Done.\"
     (v/p nil)                      => \"\"
     (v/p \"Patched\" n \"files\")       => \"Patched 12 files\"
     (v/p \"Status:\" (v/bold \"OK\")) => \"Status: **OK**\"
     (v/p \"Tags:\" (map v/code tags)) ; spliced + space-joined

   Glued concat -> use `(str …)`; v/p adds whitespace, never markdown."
  ^String [& parts]
  (let [joined (str/join " " (keep (fn [p] (when-not (str/blank? p) (str/trim p)))
                               (mapv ->str (expand-parts parts))))]
    (loop [s joined]
      (let [next (str/replace s "  " " ")]
        (if (str/includes? next "  ")
          (recur next)
          next)))))

;; Inline emphasis / spans — every helper variadic for the same
;; reason headings are: the LLM naturally composes (v/bold "foo "
;; (v/code "bar")) and would otherwise hit ArityException. 1-arg
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
     (v/link \"docs\" \"https://example.com\" \"Project docs\")
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
   shape as v/link)."
  (^String [alt url] (image alt url nil))
  (^String [alt url title]
   (str "!" (link alt url title))))

(defn file-link
  "Cite a workspace file. Target = literal path; channels resolve
   relative to `(fs/cwd)` and make it clickable.

     (v/file-link \"src/foo.clj\")     ; [src/foo.clj](src/foo.clj)
     (v/file-link \"src/foo.clj\" 142) ; [src/foo.clj:142](src/foo.clj#L142)

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

     (v/anchor \"Jump to summary\" \"summary\") ; [Jump to summary](#summary)
     (v/anchor \"Patch report\")               ; [Patch report](#patch-report)"
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

(def ^{:doc "Hard line break suffix. Append + newline -> `<br/>` (CommonMark
   trailing-spaces). Usable as both a value (stringified to `\"  \"`) and a
   zero-arg function `((v/br))` returning the same string."}
  br
  "Callable hard line break: value or zero-arg fn."
  (reify
    CharSequence
    (toString [_] "  ")
    clojure.lang.IFn
    (invoke [_] "  ")
    (applyTo [_ _] "  ")))

(defn summary
  "Standalone `<summary>…</summary>` tag — disclosure label inside
   `(v/details …)`. Variadic body, same rules as the inline
   helpers: nil dropped, seqs spliced, parts concatenated.

     (v/summary \"Logs\")
     (v/summary (v/bold \"Logs\") \" (\" (v/code \"42\") \")\")

   `v/details` lifts a wrapped <summary> part to the canonical
   first-child slot regardless of arg position."
  ^String [& parts]
  (str "<summary>" (compose-text parts) "</summary>"))

(defn- summary-tagged?
  "True when s is a `<summary>…</summary>` block (v/summary or hand-
   rolled). Drives v/details summary-lifting."
  [s]
  (and (string? s)
    (str/starts-with? s "<summary>")
    (str/ends-with? s "</summary>")))

(defn details
  "GitHub-style collapsible block. One API: `(v/details & parts)`.
   Parts splice one level (nil dropped, seqs flattened — same rules
   as v/join / v/lines), each ->str-coerced, body parts blank-line
   joined.

   Plain strings = body. For a disclosure label, write `(v/summary X)`
   explicitly. No auto-wrap, no first-arg magic — one shape, one rule.

   Summary lifting: a `<summary>…</summary>` part placed anywhere
   among the args is moved to the canonical first-child slot inside
   `<details>`. HTML5 + GitHub require the label as first flow child;
   the lift means callers never have to remember the order. Body
   order preserved.

   At most one <summary> per block; ≥2 throws ex-info. Zero summary
   parts OK — block has no label, browsers default to 'Details'.

     (v/details (v/summary \"Logs\") body)              ; canonical
     (v/details intro snippet (v/summary \"Trace\"))    ; lifted
     (v/details para1 para2)                           ; label-less
     (v/details (v/summary (v/bold \"Trace\")) body)   ; styled label
     (v/details (v/summary \"Files\") (mapv v/p xs))   ; spliced body"
  ^String [& parts]
  (let [parts (expand-parts parts)
        strs  (mapv ->str parts)
        {sums true bodies false} (group-by summary-tagged? strs)
        sum   (first sums)
        body  (when (seq bodies) (str/join "\n\n" bodies))]
    (when (> (count sums) 1)
      (throw (ex-info
               (str "v/details got " (count sums) " <summary>…</summary> "
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
   nils so `(v/ul nil)`, `(v/ul [])`, `(v/ul nil nil)` all yield
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

     (item-text \"plain string\")              ;=> \"plain string\"
     (item-text [\"a \" (v/code \"b\") \" c\"])  ;=> \"a `b` c\""
  [x]
  (if (sequential? x)
    (compose-text x)
    (->str x)))
(defn li
  "Returns a single unordered-list item string: `\"- text\"`.
   Variadic — parts are joined like other inline fns (nil dropped, seqs spliced)."
  [& parts]
  (str "- " (compose-text parts)))

(defn ul
  "Unordered list. Accepts a single seq or variadic args.
   Each item may be a string or a sequential of parts (strings +
   inline helpers) composed into one item text.

     (v/ul [\"a\" \"b\"])
     (v/ul [[\"The \" (v/code \"foo\") \" works\"] \"plain\"])

   One `- item` per element, newline-joined, no trailing newline."
  [& items]
  (->> (normalize-list-items items)
    (map #(str "- " (item-text %)))
    (str/join "\n")))

(defn ol
  "Ordered list, 1-based numbering. Accepts a single seq or variadic args.
   Each item may be a string or a sequential of parts composed into
   one item text.

     (v/ol [\"a\" \"b\"])
     (v/ol [[\"Step \" (v/code \"1\") \": go\"] \"done\"])"
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
   stays caller's responsibility — (v/join nested-tree) surfaces as
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
   Seqs splice one level — `(v/join (mapv render xs))` and
   `(v/join (v/h1 \"…\") (map render xs))` both Just Work."
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
     (v/section \"Summary\" body)    -> ## Summary\\n\\nbody
     (v/section 3 \"Details\" body)  -> ### Details\\n\\nbody"
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
      :examples ["(v/h1 \"Patch report\")"
                 "(v/h1 \"Build of \" (v/code \"v1.2.3\"))"]})
   (vis/symbol 'h2 h2
     {:doc "H2: `## text`. Variadic body — see v/h1."
      :arglists '([& parts])
      :examples ["(v/h2 \"Summary\")"]})
   (vis/symbol 'h3 h3
     {:doc "H3: `### text`. Variadic body — see v/h1."
      :arglists '([& parts])
      :examples ["(v/h3 \"Proposal: \" (v/code \":vis/silent\") \" sentinel\")"]})
   (vis/symbol 'h4 h4
     {:doc "H4: `#### text`. Variadic body — see v/h1."
      :arglists '([& parts])
      :examples ["(v/h4 \"Notes\")"]})
   (vis/symbol 'h5 h5
     {:doc "H5: `##### text`. Variadic body — see v/h1."
      :arglists '([& parts])
      :examples ["(v/h5 \"Caveat\")"]})
   (vis/symbol 'h6 h6
     {:doc "H6: `###### text`. Variadic body — see v/h1."
      :arglists '([& parts])
      :examples ["(v/h6 \"Footnote\")"]})
   (vis/symbol 'h h
     {:doc "Heading at level n (clamped [1, 6]). Variadic body — see v/h1."
      :arglists '([level & parts])
      :examples ["(v/h 3 \"Step 1\")"
                 "(v/h 2 \"Build of \" (v/code \"v1.2.3\"))"]})

   (vis/symbol 'p p
     {:doc "Paragraph. Joins parts with single space; nil dropped; seqs splice one level (matches v/join / v/lines)."
      :arglists '([& parts])
      :examples ["(v/p \"Done.\")"
                 "(v/p \"Patched\" n \"files\")"
                 "(v/p \"Status:\" (v/bold \"OK\"))"]})
   (vis/symbol 'bold bold
     {:doc "Bold span: `**text**`. Variadic — parts concatenated, nil dropped, seqs spliced."
      :arglists '([& parts])
      :examples ["(v/bold \"important\")"
                 "(v/bold \"build \" (v/code \"v1.2.3\"))"]})
   (vis/symbol 'strong bold
     {:doc "Bold span: `**text**` (HTML-semantic alias for `v/bold`). Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/strong \"important\")"]})
   (vis/symbol 'italic italic
     {:doc "Italic span: `*text*`. Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/italic \"subtle\")"]})
   (vis/symbol 'em italic
     {:doc "Italic span: `*text*` (HTML-semantic alias for `v/italic`). Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/em \"subtle\")"]})
   (vis/symbol 'bold-italic bold-italic
     {:doc "Bold-italic span: `***text***`. Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/bold-italic \"!!!\")"]})
   (vis/symbol 'strike strike
     {:doc "Strikethrough span: `~~text~~`. Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/strike \"obsolete\")"]})
   (vis/symbol 'code code
     {:doc "Inline code span: `` `text` ``. Variadic — parts concatenated."
      :arglists '([& parts])
      :examples ["(v/code \"v/cat\")"
                 "(v/code \"v/\" tool-name)"]})
   ;; `summary` is registered alongside the inline tag-style helpers
   ;; (kbd / bold / italic) because it produces a single bare HTML
   ;; tag, not a block. Composes with `details` via tag-passthrough.
   ;; Intentionally NOT mentioned in `markdown-prompt` — callers who
   ;; need it can discover it via `(symbol-info 'v/summary)`; we
   ;; don't want every answer reaching for collapsible UI.
   (vis/symbol 'summary summary
     {:doc "Standalone `<summary>…</summary>` tag for use inside (v/details …). Variadic — parts concatenated. v/details lifts it to the canonical first-child slot regardless of arg position."
      :arglists '([& parts])
      :examples ["(v/summary \"Logs\")"
                 "(v/summary (v/bold \"Logs\") \" (\" (v/code \"42\") \")\")"
                 "(v/details (v/summary (v/bold \"Logs\")) body)"]})

   (vis/symbol 'kbd kbd
     {:doc "Keyboard span: `<kbd>text</kbd>`. Variadic — parts concatenated."
      :arglists '([& parts])
      :examples ["(v/kbd \"Ctrl+K\")"]})
   (vis/symbol 'link link
     {:doc "Hyperlink: `[text](url)`. 3-arg adds tooltip title attr. TUI + Telegram render as clickable wherever the surface supports it."
      :arglists '([text url] [text url title])
      :examples ["(v/link \"docs\" \"https://example.com\")"
                 "(v/link \"spec\" \"docs/spec.md\" \"Full spec\")"]})
   (vis/symbol 'image image
     {:doc "Image: `![alt](url)`. 3-arg embeds tooltip title."
      :arglists '([alt url] [alt url title])
      :examples ["(v/image \"diagram\" \"./diagram.png\")"
                 "(v/image \"flow\" \"./flow.png\" \"Iteration flow\")"]})
   (vis/symbol 'file-link file-link
     {:doc "Cite a workspace file. 1-arg -> `[path](path)`; 2-arg -> `[path:line](path#Lline)` so channels can jump to the exact line. Use for every source-code reference."
      :arglists '([path] [path line])
      :examples ["(v/file-link \"src/main.clj\")"
                 "(v/file-link \"src/main.clj\" 142)"]})
   (vis/symbol 'anchor anchor
     {:doc "Same-doc heading anchor. 1-arg auto-slugifies; 2-arg takes explicit slug."
      :arglists '([text] [text slug])
      :examples ["(v/anchor \"Patch report\")"
                 "(v/anchor \"Jump to summary\" \"summary\")"]})

   (vis/symbol 'code-block code-block
     {:doc "Fenced code block. 1-arg = no language; 2-arg embeds it."
      :arglists '([code] [code lang])
      :examples ["(v/code-block \"(println :ok)\" \"clojure\")"
                 "(v/code-block \"plain text\")"]})
   (vis/symbol 'blockquote blockquote
     {:doc "Quote each line with `> `. Variadic — parts concatenated then split on \"\\n\"."
      :arglists '([& parts])
      :examples ["(v/blockquote \"caveat\")"
                 "(v/blockquote \"line1\\nline2\")"]})
   (vis/symbol 'quote blockquote
     {:doc "Quote each line with `> ` (shorter alias for `v/blockquote`). Variadic — see v/blockquote."
      :arglists '([& parts])
      :examples ["(v/quote \"caveat\")"]})
   (vis/value 'hr hr
     {:doc "Horizontal rule (`---`)."})
   (vis/value 'br br
     {:doc "Hard line break suffix (CommonMark trailing-spaces)."})
   (vis/symbol 'details details
     {:doc (str "Collapsible block: `<details>…</details>`. Variadic: parts splice "
             "(nil dropped, seqs flattened), body parts blank-line joined. A `<summary>…</summary>` "
             "part — produced by v/summary or hand-rolled — is lifted to the canonical "
             "first-child slot regardless of arg position. Plain strings = body; no auto-wrap.")
      :arglists '([& parts])
      :examples ["(v/details (v/summary \"Logs\") body)"
                 "(v/details intro snippet (v/summary \"Trace\"))"
                 "(v/details para1 para2)"]})
   (vis/symbol 'li li
     {:doc "Single unordered-list item: `\"- text\"`. Variadic — nil dropped, seqs spliced."
      :arglists '([& parts])
      :examples ["(v/li \"hello\")" "(v/li \"build \" (v/code \"v1.2.3\"))"]})
   (vis/symbol 'ul ul
     {:doc "Unordered list. items = seq; each entry becomes one `- item` line."
      :arglists '([items])
      :examples ["(v/ul [\"a\" \"b\" \"c\"])"]})
   (vis/symbol 'ol ol
     {:doc "Ordered list, 1-based numbering."
      :arglists '([items])
      :examples ["(v/ol [\"first\" \"second\"])"]})
   (vis/symbol 'checklist checklist
     {:doc "Task list: items = `[text done?]` pairs OR `{:text :done?}` maps."
      :arglists '([items])
      :examples ["(v/checklist [[\"done\" true] [\"todo\" false]])"]})

   (vis/symbol 'table table
     {:doc "Markdown table. headers = vec of column titles; rows = vec of row vecs. Opts: `{:align [:left :center :right …]}`."
      :arglists '([headers rows] [headers rows opts])
      :examples ["(v/table [\"file\" \"lines\"] [[\"a\" 12] [\"b\" 30]])"
                 "(v/table [\"k\" \"v\"] [[\"x\" 1]] {:align [:left :right]})"]})

   (vis/symbol 'join join
     {:doc "Stitch block pieces with one BLANK line (`\\n\\n`)."
      :arglists '([& parts])
      :examples ["(v/join (v/h1 \"x\") (v/p \"y\"))"]})
   (vis/symbol 'lines lines
     {:doc "Stitch lines with single newline."
      :arglists '([& parts])
      :examples ["(v/lines \"a\" \"b\")"]})
   (vis/symbol 'section section
     {:doc "Heading + body shortcut. Default level 2; 3-arg picks level."
      :arglists '([title body] [level title body])
      :examples ["(v/section \"Summary\" \"…\")"
                 "(v/section 3 \"Details\" \"…\")"]})
   (vis/symbol 'escape escape
     {:doc "Backslash-escape every CommonMark special character in s."
      :arglists '([s])
      :examples ["(v/escape \"1 + 2 = *3*\")"]})])

(def markdown-symbols
  "All `v/`-aliased symbols exposed in the SCI sandbox."
  symbol-entries)

(def markdown-prompt
  "Prompt fragment listing the `v/` surface for the iteration prompt."
  (str
    "`v/` = markdown for (answer …). Pure string builders, no templating.\n"
    "  Headings   (v/h1 …parts) (v/h2 …parts) (v/h3 …parts) (v/h level …parts)\n"
    "             ; variadic: (v/h3 \"Build \" (v/code \"v1.2.3\")) just works — nil dropped, seqs spliced, no whitespace inserted.\n"
    "  Inline     (v/bold …parts | v/strong …) (v/italic … | v/em …) (v/code …) (v/kbd …) (v/strike …)\n"
    "             ; semantic aliases: v/strong=v/bold, v/em=v/italic, v/quote=v/blockquote.\n"
    "             ; ALL inline / heading helpers are variadic (nil dropped, seqs spliced, parts concatenated):\n"
    "             ; (v/bold \"build \" (v/code \"v1.2.3\")) -> **build `v1.2.3`** — author owns whitespace.\n"
    "  Links      (v/link text url) (v/link text url title)   ; tooltip via title\n"
    "             (v/image alt url) (v/image alt url title)\n"
    "             (v/file-link path) (v/file-link path line)  ; CITE source files this way\n"
    "             (v/anchor text) (v/anchor text slug)         ; same-doc heading link\n"
    "  Block      (v/p …parts) (v/code-block s lang?) (v/blockquote s) v/hr v/br (v/details …parts)\n"
    "             ; v/p joins parts with single space; nil dropped; seqs spliced (like v/join / v/lines).\n"
    "  Lists      (v/li parts…)                  ; single list item: \"- text\"\n"
    "             (v/ul items) (v/ol items) (v/checklist [[t done?] …])\n"
    "  Tables     (v/table headers rows) (v/table headers rows {:align [:left :center :right]})\n"
    "  Compose    (v/join …blocks) (v/lines …lines) (v/section title body) (v/escape s)\n"
    "Block fns return text WITHOUT trailing newline. Stitch with (v/join …), feed to (answer …).\n"
    "Cite source via (v/file-link path line) -> TUI jumps to line. (v/link …) for URLs, (v/image alt url) for diagrams. Hand-rolled `[…](…)` drifts from channel conventions; the helpers stay aligned."))

;; NOTE: this ns is implementation-only now. The actual sandbox surface
;; is registered by `foundation/core.clj`, which re-exports
;; `markdown-symbols` and `markdown-prompt` under the unified `v/`
;; alias alongside introspection, filesystem, and environment helpers.