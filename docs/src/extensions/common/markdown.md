# Markdown extension (`md/`)

Package: `com.blockether/vis-foundation`. Source:
[`extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/markdown.clj`](https://github.com/blockether/vis/blob/main/extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/markdown.clj).
SCI alias: `md/`.

The agent's only supported way to construct an answer body is the
`md/` surface. There is **no Mustache layer**, no template language,
no `{{var}}` interpolation: every fn here is a pure string builder.
Whatever `(answer (md/join …))` produces *is* the answer markdown
verbatim, with no post-processing pass.

Why a programmatic surface instead of a templating layer:

- **Composable.** `(md/join (md/h1 …) (md/p …) (md/table …))` is a
  Clojure expression — `let`, `for`, `cond`, `into`, all the
  iteration tools the model already uses for data work continue to
  work for output construction.
- **No new failure modes.** Templating layers fail by silent
  variable-misses, by quoting bugs, by HTML-vs-Markdown escaping
  surprises. Plain `str` concatenation has none of those.
- **Channel-aware citations.** `md/link`, `md/image`, `md/file-link`,
  `md/anchor` produce hyperlinks the TUI / web / Telegram channels
  can render as clickable jumps. Hand-rolled `[…](…)` strings drift
  out of sync with channel conventions; the helpers don't.

## Usage

```clojure
(answer
  (md/join
    (md/h1 "Patch report")
    (md/p "Three files touched.")
    (md/table ["file" "+/-"]
              [["core.clj" "+12 / -4"]
               ["loop.clj" "+0 / -38"]])
    (md/h2 "Next")
    (md/ul ["Run verify.sh" "Update CHANGELOG"])
    (md/code-block "(println :done)" "clojure")))
```

Block fns **return text without a trailing newline**. Compose with
`(md/join …)` (blank-line between blocks) or `(md/lines …)` (single
newline between lines), then hand the final string to
`(answer …)` from the iteration loop. See
[Iteration flow](../../architecture/iteration-flow.md) for how
`(answer …)` interacts with the rest of the loop.

## Surface

### Headings

| Form | Output |
|------|--------|
| `(md/h1 "Title")` | `# Title` |
| `(md/h2 "Title")` | `## Title` |
| `(md/h3 "Title")` | `### Title` |
| `(md/h4 "Title")` | `#### Title` |
| `(md/h5 "Title")` | `##### Title` |
| `(md/h6 "Title")` | `###### Title` |
| `(md/h n "Title")` | `n` clamped to `[1, 6]` |

### Inline emphasis

| Form | Output |
|------|--------|
| `(md/bold "x")`         | `**x**` |
| `(md/italic "x")`       | `*x*` |
| `(md/bold-italic "x")`  | `***x***` |
| `(md/strike "x")`       | `~~x~~` |
| `(md/code "x")`         | `` `x` `` |
| `(md/kbd "Ctrl+K")`     | `<kbd>Ctrl+K</kbd>` |

### Hyperlinks, images, citations

This is the section the model should reach for whenever an answer
references something the user can click.

```clojure
(md/link text url)             ; [text](url)
(md/link text url title)       ; [text](url "title")  — tooltip on hover

(md/image alt url)             ; ![alt](url)
(md/image alt url title)       ; ![alt](url "title")

(md/file-link path)            ; [src/foo.clj](src/foo.clj)
(md/file-link path line)       ; [src/foo.clj:142](src/foo.clj#L142)

(md/anchor text)               ; auto-slugifies — [Patch report](#patch-report)
(md/anchor text slug)          ; explicit slug   — [Jump](#summary)
```

Conventions:

- **Always cite source code via `md/file-link`.** The 2-arg form
  embeds a `#Lline` anchor so a TUI / web channel can resolve the
  click to that exact line. Hand-rolling `(str "[" path ":" line
  "](" path "#L" line ")")` produces the same output today and
  drifts the moment the channel layer adds (e.g.) a `?branch=main`
  query suffix.
- **`md/link` and `md/image` share the same 3-arg shape.** The
  third arg is the tooltip / `title` attribute. It is escaped so
  embedded `"` survives.
- **`md/anchor` is for in-document jumps**, e.g. linking to a
  later section of the same answer.

### Block elements

| Form | Output |
|------|--------|
| `(md/p "text")`                 | `text` (paragraph; nil → `""`) |
| `(md/code-block "code")`        | unfenced \`\`\` block |
| `(md/code-block "code" "lang")` | \`\`\`lang fenced block |
| `(md/blockquote "a\nb")`        | `> a\n> b` |
| `md/hr`                         | `---` (constant) |
| `md/br`                         | `"  "` — CommonMark trailing-spaces line break |
| `(md/details "summary" "body")` | GitHub-style collapsible block |

### Lists

```clojure
(md/ul ["a" "b" "c"])
;; - a
;; - b
;; - c

(md/ol ["first" "second"])
;; 1. first
;; 2. second

(md/checklist [["done" true] ["todo" false]])
;; - [x] done
;; - [ ] todo

(md/checklist [{:text "done" :done? true}
               {:text "todo" :done? false}])
;; same output; map shape lets you build entries from `assoc`
```

### Tables

```clojure
(md/table ["file" "lines"]
          [["core.clj" 12]
           ["loop.clj" 30]])

;; | file | lines |
;; | --- | --- |
;; | core.clj | 12 |
;; | loop.clj | 30 |
```

Optional `{:align [...]}` opt vec lets you override per-column
alignment; values are `:left`, `:center`, `:right`, or `:default`.
Pipes and embedded newlines in cells are escaped automatically.

```clojure
(md/table ["k" "v"] [["x" 1]] {:align [:left :right]})
```

### Composing

| Form | Joins with | Drops nils |
|------|-----------|------------|
| `(md/join …blocks)` | `\n\n` (blank line)   | yes |
| `(md/lines …lines)` | `\n` (single newline) | yes |
| `(md/section title body)`         | heading + blank line + body | level 2 default |
| `(md/section level title body)`   | same, custom heading level  | n/a |

`(md/escape s)` backslash-escapes every CommonMark special character
in `s` so the string renders as literal text — useful when echoing
user input back into the answer.

## Discovery from inside the agent

Every `md/` symbol is registered through the standard extension
machinery, so the model can introspect the surface itself:

```clojure
(vis/extensions)                            ; -> includes :md
(vis/extension-doc 'md 'file-link)          ; full descriptor
(vis/extension-doc 'md 'link)               ; arglists, examples, doc
```

See [Extension overview](../overview.md) for the underlying
machinery.

## Why no Mustache

The previous answer-rendering layer ran every assistant message
through a `jmustache` pass with the SCI sandbox's locals as the
context map. Three things made that wrong:

1. **Schema drift.** The model would compute a value, bind it to
   `(def …)`, then reference it from `{{name}}`. Whether the model
   used `{{name}}` vs `{{ name }}` vs `{{ #section }}` vs raw
   string interpolation depended on which model and which prompt
   variant — answer rendering would silently produce different
   output across providers for the same code.
2. **Hidden failure modes.** A typo in `{{undefined-var}}` raised
   a strict-mode error from inside the renderer, surfacing as
   "iteration failed" rather than "your final-answer template
   referenced an unbound name".
3. **Redundant.** `(answer …)` is a real Clojure call; the argument
   passes through `str`. The model can already use `clojure.string`,
   `format`, `pr-str`, `let` — every interpolation primitive — to
   build the answer body. A templating layer was a second way to do
   the same thing, only worse.

`md/` replaces all of that with one rule: build the answer string
yourself, in Clojure, using these helpers. See [Iteration
flow](../../architecture/iteration-flow.md#answer-protocol) for the
end-to-end shape of the new path.
