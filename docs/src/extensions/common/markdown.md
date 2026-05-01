# Markdown builders under `v/`

Package: `com.blockether/vis-foundation`. Source:
[`extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/markdown.clj`](https://github.com/blockether/vis/blob/main/extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/markdown.clj).
SCI alias: `v/`.

This page documents the markdown-builder subset of the unified `v/`
surface.

`v/` is the only supported way the agent constructs an answer body.
No Mustache, no template language, no `{{var}}` interpolation —
every fn is a pure string builder. Whatever `(answer (v/join …))`
produces *is* the answer markdown verbatim, no post-processing.

Why programmatic over templating:

- **Composable.** `(v/join (v/h1 …) (v/p …) (v/table …))` is a
  Clojure expression. `let`, `for`, `cond`, `into` — every
  iteration tool the model already uses for data work works for
  output too.
- **No new failure modes.** Templating fails by silent variable
  miss, quoting bug, HTML-vs-Markdown escape surprise. Plain `str`
  has none of those.
- **Channel-aware citations.** `v/link`, `v/image`, `v/file-link`,
  `v/anchor` produce hyperlinks the TUI / web / Telegram channels
  render as clickable jumps. Hand-rolled `[…](…)` drifts out of
  sync; the helpers stay aligned.

## Usage

```clojure
(answer
  (v/join
    (v/h1 "Patch report")
    (v/p "Three files touched.")
    (v/table ["file" "+/-"]
              [["core.clj" "+12 / -4"]
               ["loop.clj" "+0 / -38"]])
    (v/h2 "Next")
    (v/ul ["Run verify.sh" "Update CHANGELOG"])
    (v/code-block "(println :done)" "clojure")))
```

Block fns **return text without a trailing newline**. Stitch with
`(v/join …)` (blank line between blocks) or `(v/lines …)` (single
newline between lines), then hand the final string to `(answer …)`.
See [Iteration flow](../../architecture/iteration-flow.md) for how
`(answer …)` interacts with the loop.

## Surface

### Headings

| Form | Output |
|------|--------|
| `(v/h1 "Title")` | `# Title` |
| `(v/h2 "Title")` | `## Title` |
| `(v/h3 "Title")` | `### Title` |
| `(v/h4 "Title")` | `#### Title` |
| `(v/h5 "Title")` | `##### Title` |
| `(v/h6 "Title")` | `###### Title` |
| `(v/h n "Title")` | `n` clamped to `[1, 6]` |

### Inline emphasis

| Form | Output |
|------|--------|
| `(v/bold "x")`         | `**x**` |
| `(v/italic "x")`       | `*x*` |
| `(v/bold-italic "x")`  | `***x***` |
| `(v/strike "x")`       | `~~x~~` |
| `(v/code "x")`         | `` `x` `` |
| `(v/kbd "Ctrl+K")`     | `<kbd>Ctrl+K</kbd>` |

### Hyperlinks, images, citations

Reach for this section whenever an answer references something the
user can click.

```clojure
(v/link text url)             ; [text](url)
(v/link text url title)       ; [text](url "title")  — hover tooltip

(v/image alt url)             ; ![alt](url)
(v/image alt url title)       ; ![alt](url "title")

(v/file-link path)            ; [src/foo.clj](src/foo.clj)
(v/file-link path line)       ; [src/foo.clj:142](src/foo.clj#L142)

(v/anchor text)               ; auto-slug — [Patch report](#patch-report)
(v/anchor text slug)          ; explicit slug — [Jump](#summary)
```

Conventions:

- **Cite source code via `v/file-link`.** 2-arg embeds a `#Lline`
  anchor so the channel resolves the click to that exact line.
  Hand-rolling `(str "[" path ":" line "](" path "#L" line ")")`
  matches today and drifts the moment the channel layer adds (e.g.)
  a `?branch=main` suffix.
- **`v/link` and `v/image` share the 3-arg shape.** Third arg =
  tooltip / `title` attr. Embedded `"` is escaped.
- **`v/anchor`** is for in-document jumps — linking to a later
  section of the same answer.

## Clicking links in the TUI

The Lanterna TUI renders every `v/link` / `v/image` /
`v/file-link` reference in the assistant’s answer as a one-row
clickable strip at the bottom of the bubble:

```text
  📷 diagram → ./flow.png
  🔗 docs    → https://example.com/spec
  📄 src/loop.clj:142
```

Hover the row → band lights pale blue. Click → URL goes to the host
OS opener (`open` on macOS; `xdg-open` / `gio open` / `kde-open` /
`gnome-open` chain on Linux/BSD; `cmd /c start "" …` on Windows).
Opener runs side-thread so a slow desktop helper never freezes the
redraw loop.

What is and isn’t clickable:

- `http://`, `https://`, `file://` URLs that resolve under
  `(fs/cwd)`, plus bare relative paths (`src/foo.clj`,
  `./diagram.png`) — clickable.
- `javascript:`, `data:`, `mailto:`, `ssh:`, etc. — anything not on
  the whitelist — paint with 🚫 and refuse to open.
- `..`-traversal escaping the working dir — same blocked marker.
  Same guard the editing tools enforce.
- Anchor-only links (`[text](#section)`) — dropped: no in-document
  anchor to jump to in a chat surface.

### Terminal compatibility

Mouse capture relies on the terminal forwarding mouse-mode escape
sequences. Modern terminals work out of the box; common macOS
catch:

- **Terminal.app**: enable **View → Send Mouse Events** (or use
  iTerm2 / WezTerm / Ghostty / Alacritty / kitty — all pass clicks
  by default).
- **tmux**: `set -g mouse on` in `.tmux.conf`.
- **SSH**: works as long as the local terminal honours mouse mode —
  escapes pass through transparently.

If the host terminal silently drops mouse-mode escapes, chrome rows
still paint but stay inert. No keyboard fallback by design — the
click affordance IS the picker.

### Block elements

| Form | Output |
|------|--------|
| `(v/p …parts)`                | parts space-joined; nil dropped; seqs spliced |
| `(v/code-block "code")`       | unfenced \`\`\` block |
| `(v/code-block "code" "lang")`| \`\`\`lang fenced block |
| `(v/blockquote "a\nb")`       | `> a\n> b` |
| `v/hr`                        | `---` (constant) |
| `v/br`                        | `"  "` — CommonMark trailing-spaces line break |
| `(v/details …parts)`          | GitHub-style collapsible block (variadic) |

### Lists

```clojure
(v/ul ["a" "b" "c"])
;; - a
;; - b
;; - c

(v/ol ["first" "second"])
;; 1. first
;; 2. second

(v/checklist [["done" true] ["todo" false]])
;; - [x] done
;; - [ ] todo

(v/checklist [{:text "done" :done? true}
               {:text "todo" :done? false}])
;; same output; map shape lets you build entries from `assoc`
```

### Tables

```clojure
(v/table ["file" "lines"]
          [["core.clj" 12]
           ["loop.clj" 30]])

;; | file | lines |
;; | --- | --- |
;; | core.clj | 12 |
;; | loop.clj | 30 |
```

Optional `{:align [...]}` opt vec overrides per-column alignment;
values: `:left`, `:center`, `:right`, `:default`. Pipes and embedded
newlines in cells are escaped automatically.

```clojure
(v/table ["k" "v"] [["x" 1]] {:align [:left :right]})
```

### Composing

| Form | Joins with | Drops nils |
|------|-----------|------------|
| `(v/join …blocks)` | `\n\n` (blank line)   | yes |
| `(v/lines …lines)` | `\n` (single newline) | yes |
| `(v/section title body)`         | heading + blank line + body | level 2 default |
| `(v/section level title body)`   | same, custom level          | n/a |

`(v/escape s)` backslash-escapes every CommonMark special character
in `s` so the string renders as literal text — useful when echoing
user input back into the answer.

## Discovery from inside the agent

The markdown builders are part of the unified `vis-foundation`
extension, so introspection goes through the `v` extension id:

```clojure
(v/extensions)                           ; -> includes :v
(v/extension-doc 'v 'file-link)          ; full descriptor
(v/extension-doc 'v 'link)               ; arglists, examples, doc
(v/extension-readme 'v)                  ; unified README
```

See [Extension overview](../overview.md) for the underlying
machinery.

## Why no Mustache

The previous answer-rendering layer ran every assistant message
through `jmustache` with the SCI sandbox's locals as the context
map. Three things made it wrong:

1. **Schema drift.** Model computes a value, binds it via `(def …)`,
   references it from `{{name}}`. `{{name}}` vs `{{ name }}` vs
   `{{ #section }}` vs raw string interpolation depended on which
   model + which prompt variant — same code, different answer
   markup across providers.
2. **Hidden failure modes.** Typo in `{{undefined-var}}` → strict-
   mode error from inside the renderer, surfacing as "iteration
   failed" rather than "your final-answer template referenced an
   unbound name".
3. **Redundant.** `(answer …)` is a real Clojure call; arg passes
   through `str`. The model already has `clojure.string`, `format`,
   `pr-str`, `let` — every interpolation primitive — for building
   the answer body. Templating was a second way to do the same
   thing, only worse.

`v/` replaces all of that with one rule: build the answer string
yourself, in Clojure, using these helpers. See [Iteration
flow](../../architecture/iteration-flow.md#answer-protocol) for the
end-to-end shape.
