(ns com.blockether.vis.ext.foundation.markdown
  "Thin `v/` markdown shim.

   All markdown implementation lives in `com.blockether.vis.internal.markdown`.
   This namespace re-exports that pure surface for tests/Clojure callers and
   registers the same fns as SCI symbols under the unified `v/` foundation
   alias."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.markdown :as md]
   [edamame.core :as edamame]
   [sci.core :as sci]))

(set! *warn-on-reflection* true)

(def h md/h)
(def h1 md/h1)
(def h2 md/h2)
(def h3 md/h3)
(def h4 md/h4)
(def h5 md/h5)
(def h6 md/h6)
(def p md/p)
(def bold md/bold)
(def strong md/strong)
(def italic md/italic)
(def em md/em)
(def bold-italic md/bold-italic)
(def strike md/strike)
(def code md/code)
(def summary md/summary)
(def kbd md/kbd)
(def link md/link)
(def image md/image)
(def file-link md/file-link)
(def anchor md/anchor)
(def code-block md/code-block)
(def blockquote md/blockquote)
(def hr md/hr)
(def br md/br)
(def details md/details)
(def li md/li)
(def ul md/ul)
(def ol md/ol)
(def checklist md/checklist)
(def table md/table)
(def join md/join)
(def needs-input md/needs-input)
(def lines md/lines)
(def section md/section)
(def escape md/escape)

(def ^:private prose-helper-names
  '#{h h1 h2 h3 h4 h5 h6
     p bold strong italic em bold-italic strike
     code summary kbd link image file-link anchor
     code-block blockquote quote details li ul ol checklist table join
     needs-input lines section escape})

(def ^:private prose-helper-symbols
  (set (concat prose-helper-names
         (map #(symbol "v" (name %)) prose-helper-names))))

(defn- sci-resolves-symbol?
  [{:keys [sci-ctx sandbox-ns]} sym]
  (when sci-ctx
    (try
      (let [sandbox (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
        (boolean (:val (sci/eval-string+ sci-ctx (str "(resolve '" sym ")") (when sandbox {:ns sandbox})))))
      (catch Throwable _ false))))

(defn- prose-symbol?
  [environment sym]
  (and (symbol? sym)
    (nil? (namespace sym))
    (not (sci-resolves-symbol? environment sym))))

(defn- auto-quote-prose-form
  [environment form]
  (letfn [(rewrite [x direct-markdown-arg?]
            (cond
              (seq? x)
              (let [head (first x)]
                (if (contains? prose-helper-symbols head)
                  (apply list head (map #(rewrite % true) (rest x)))
                  (apply list (map #(rewrite % false) x))))

              (vector? x)
              (mapv #(rewrite % false) x)

              (map? x)
              (into (empty x) (map (fn [[k v]] [(rewrite k false) (rewrite v false)])) x)

              (set? x)
              (into (empty x) (map #(rewrite % false)) x)

              (and direct-markdown-arg? (prose-symbol? environment x))
              (name x)

              :else x))]
    (rewrite form false)))

(defn rescue-markdown-prose
  "Foundation source-rewrite hook. Repairs valid Clojure that would fail while
   building final Markdown because prose was emitted as bare symbols inside
   direct `v/` markdown helper args, e.g. `(v/p Spokojnie — gotowe)`. Bound
   symbols are preserved for interpolation. Returns rewritten source or nil."
  [{:keys [code environment]}]
  (try
    (let [forms    (edamame/parse-string-all (or code ""))
          repaired (mapv #(auto-quote-prose-form environment %) forms)]
      (when (not= forms repaired)
        (str/join "\n" (map pr-str repaired))))
    (catch Throwable _ nil)))

(def ^:private symbol-entries
  [(vis/symbol 'h1 h1
     {:doc "H1: `# text`. Variadic parts concatenate."
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
     {:doc "Paragraph. Parts join with one space."
      :arglists '([& parts])
      :examples ["(v/p \"Done.\")"
                 "(v/p \"Patched\" n \"files\")"
                 "(v/p \"Status:\" (v/bold \"OK\"))"]})
   (vis/symbol 'bold bold
     {:doc "Bold span: `**text**`."
      :arglists '([& parts])
      :examples ["(v/bold \"important\")"
                 "(v/bold \"build \" (v/code \"v1.2.3\"))"]})
   (vis/symbol 'strong strong
     {:doc "Bold span: `**text**` (HTML-semantic alias for `v/bold`). Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/strong \"important\")"]})
   (vis/symbol 'italic italic
     {:doc "Italic span: `*text*`. Variadic — see v/bold."
      :arglists '([& parts])
      :examples ["(v/italic \"subtle\")"]})
   (vis/symbol 'em em
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
   (vis/symbol 'summary summary
     {:doc "`<summary>...</summary>` for `v/details`."
      :arglists '([& parts])
      :examples ["(v/summary \"Logs\")"
                 "(v/summary (v/bold \"Logs\") \" (\" (v/code \"42\") \")\")"
                 "(v/details (v/summary (v/bold \"Logs\")) body)"]})
   (vis/symbol 'kbd kbd
     {:doc "Keyboard span: `<kbd>text</kbd>`. Variadic — parts concatenated."
      :arglists '([& parts])
      :examples ["(v/kbd \"Ctrl+K\")"]})
   (vis/symbol 'link link
     {:doc "Link: `[text](url)`. 3-arg adds title."
      :arglists '([text url] [text url title])
      :examples ["(v/link \"docs\" \"https://example.com\")"
                 "(v/link \"spec\" \"docs/spec.md\" \"Full spec\")"]})
   (vis/symbol 'image image
     {:doc "Image: `![alt](url)`. 3-arg embeds tooltip title."
      :arglists '([alt url] [alt url title])
      :examples ["(v/image \"diagram\" \"./diagram.png\")"
                 "(v/image \"flow\" \"./flow.png\" \"Iteration flow\")"]})
   (vis/symbol 'file-link file-link
     {:doc "Workspace file link. 2-arg adds line anchor."
      :arglists '([path] [path line])
      :examples ["(v/file-link \"src/main.clj\")"
                 "(v/file-link \"src/main.clj\" 142)"]})
   (vis/symbol 'anchor anchor
     {:doc "Same-doc heading anchor. 1-arg auto-slugifies; 2-arg takes explicit slug."
      :arglists '([text] [text slug])
      :examples ["(v/anchor \"Patch report\")"
                 "(v/anchor \"Jump to summary\" \"summary\")"]})

   (vis/symbol 'code-block code-block
     {:doc "Fenced code block. 1-arg = no language; 2-arg is language first, code second. Lang accepts string/keyword/symbol."
      :arglists '([code] [lang code])
      :examples ["(v/code-block \"clojure\" \"(println :ok)\")"
                 "(v/code-block :clojure (pp/pprint-str {:k 1}))"
                 "(v/code-block 'edn \"{:k 1}\")"
                 "(v/code-block \"plain text\")"]})
   (vis/symbol 'blockquote blockquote
     {:doc "Quote each line with `> `. Variadic — parts concatenated then split on `\\n`."
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
     {:doc "Collapsible `<details>...</details>`. `v/summary` becomes first child."
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
     {:doc "Markdown table. Opts: `{:align [:left :center :right]}`."
      :arglists '([headers rows] [headers rows opts])
      :examples ["(v/table [\"file\" \"lines\"] [[\"a\" 12] [\"b\" 30]])"
                 "(v/table [\"k\" \"v\"] [[\"x\" 1]] {:align [:left :right]})"]})
   (vis/symbol 'join join
     {:doc "Join blocks with blank lines. Use for `(answer ...)`."
      :arglists '([& parts])
      :examples ["(v/join (v/h1 \"x\") (v/p \"y\"))"]})
   (vis/symbol 'needs-input needs-input
     {:doc "Explicit answer marker for missing user input. Use as `(answer (v/needs-input ...))` to ask for required material without creating an intent."
      :arglists '([ask] [{:keys [missing ask]}])
      :examples ["(answer (v/needs-input \"Please paste the ideas you want reviewed.\"))"
                 "(answer (v/needs-input {:missing \"the ideas to review\" :ask \"Please paste the ideas you currently have.\"}))"]})
   (vis/symbol 'lines lines
     {:doc "Stitch lines with single newline."
      :arglists '([& parts])
      :examples ["(v/lines \"a\" \"b\")"]})
   (vis/symbol 'section section
     {:doc "Heading + body shortcut. Default level 2; 3-arg picks level."
      :arglists '([title body] [level title body])
      :examples ["(v/section \"Summary\" \"...\")"
                 "(v/section 3 \"Details\" \"...\")"]})
   (vis/symbol 'escape escape
     {:doc "Backslash-escape every CommonMark special character in s."
      :arglists '([s])
      :examples ["(v/escape \"1 + 2 = *3*\")"]})])

(defn- with-source-rewrite
  [entry]
  (cond-> entry
    (:ext.symbol/fn entry) (assoc :ext.symbol/source-rewrite-fn rescue-markdown-prose)))

(def markdown-symbols
  "All `v/`-aliased symbols exposed in the SCI sandbox."
  (mapv with-source-rewrite symbol-entries))

(def markdown-prompt
  "Prompt fragment listing the `v/` surface for the iteration prompt."
  (str
    "`v/` answer Markdown: headings (v/h1 …) (v/h2 …) (v/h3 …) (v/h n …); blocks (v/p …) (v/code-block lang? code) (v/blockquote …) v/hr v/br (v/details …); lists (v/ul xs) (v/ol xs) (v/checklist xs); table (v/table headers rows opts?).\n"
    "Inline/link helpers: (v/bold …) (v/italic …) (v/code …) (v/kbd …) (v/strike …), (v/link text url), (v/image alt url), (v/file-link path line?), (v/anchor text slug?).\n"
    "Compose answers with (v/join …blocks), (v/lines …lines), (v/section title body), (v/escape s). Cite files with v/file-link. Missing required user material: (answer (v/needs-input ask-or-{:missing :ask}))."))
