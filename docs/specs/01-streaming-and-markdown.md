# Specification — Telegram Streaming, Markdown Pipeline, and Mixed-Content Answers

Status: design-locked, pre-implementation.
Single PR, single spec. Earlier drafts split this into two PRs (markdown
pipeline first, mixed-content blocks later). The split was wrong — the
two are one design when you let `(vis/answer ...)` accept either a
markdown string OR a Hiccup vector OR a variadic mix.

This document supersedes the previous draft and the deleted
`02-mixed-content-answer.md`.

---

## 1. Problem statement

### 1.1 Current state is over-engineered AND still produces broken output

```
src/.../internal/markdown.clj                                      1155 LOC
extensions/common/vis-foundation/src/.../foundation/markdown.clj    259 LOC
extensions/channels/vis-channel-telegram/.../api.clj  ~150 LOC of regex MD→HTML
LLM system prompt section listing 38 v/* helpers                   ~660 chars
Call-sites using v/ DSL                                            17 files
                                                                  ─────────
                                                                  ~1500 LOC owned + prompt verbiage
```

The `v/` DSL forces the LLM to construct markdown via `(v/h1 ...)`,
`(v/p ...)`, `(v/ul ...)`, `(v/code-block ...)`, `(v/join ...)`. Empirically
(per the autoresearch session that just ran on
`autoresearch/markdown-formatting-glm-...`):

- LLM still emits raw HTML (`<details>`, `<Details>`).
- LLM still emits Unicode arrows (`→`) where ASCII (`->`) belongs.
- LLM still misuses `v/join` for inline composition.
- Channel-side regex MD→HTML fails on unbalanced inline markers
  (defect #10 — one stray `*` collapses the whole message to plain text
  via Telegram's parse-mode fallback).

The DSL doesn't prevent the bugs it was supposed to prevent, the prompt
cost of advertising it is high, and downstream channels still need a real
renderer.

### 1.2 No streaming UX

`bot.clj/handle-user-text!` calls `(vis/send! ...)` blocking, then ships
the full answer in one `tg/send-message!`. Multi-iteration turns and
deep-reasoning models leave the user staring at `typing…` for 10–60 s
with **zero progress signal**. Cancellation requires `/cancel` text
command — not discoverable, not one-tap.

### 1.3 No structured-content path

`(vis/answer x)` flattens anything non-string via `(str v)` (loop.clj:257).
LLM ceremoniously stringifies tool-call results into markdown fences:

```clojure
(answer (str "Result:\n```\n" (with-out-str (pp/pprint result)) "\n```\nDone."))
```

This is channel-blind (LLM guesses what looks good in Telegram vs CLI vs
TUI), loses structure (Telegram can't pretty-print as `<pre>`, TUI can't
tree-collapse, CLI can't pipe), and risks nested-fence parser collisions
(LangChain bugs #2241, #9535 are exactly this class).

Industry has converged on **typed content blocks** (Anthropic content
blocks, OpenAI Responses API, LangChain 1.0 `content_blocks`,
AgentsKit, Callsphere). Vis is the holdout.

### 1.4 Why one PR

Streaming, markdown rendering, and mixed-content blocks share the same
chokepoint (`:channel/messages-renderer-fn`) and the same internal AST
(Hiccup). Designing them together costs one spec; splitting them ships
broken intermediate states.

---

## 2. Goals / non-goals

### Goals

- LLM emits plain GitHub-flavored Markdown for prose, OR Hiccup vectors
  for structured blocks, OR a variadic mix. All work day one.
- `vis/answer` contract is 100 % backward compatible with existing string
  answers and existing persisted rows.
- Markdown renders correctly on Telegram (defect #10 dies).
- Mid-stream rendering robust against partial / unbalanced input.
- Single Hiccup-shaped AST flows from any input form to all channel
  renderers.
- User sees progress within ~1 s; one-tap cancellation from chat UI.
- Net code reduction in `vis-foundation` and `internal/markdown`.

### Non-goals

- Per-token typewriter streaming of the assistant's prose (architectural
  — Vis is code-as-output; `(answer …)` lands a complete value at form
  execution time).
- Hand-rolled markdown parser (use `commonmark-java` directly).
- Rich layout primitives (rows / columns / grids). Block list is flat or
  nested-by-content, not a layout engine.
- Image generation pipelines. `[:vis/image {...}]` carries a reference;
  producing the image is some other extension's job.

---

## 3. Architectural context

### 3.1 Existing streaming surface (already in core, unused by Telegram)

`vis/make-progress-tracker` (`src/.../internal/progress.clj`). Wraps
`:on-chunk`, accumulates per-iteration timeline (`:thinking`, `:code`,
`:results`, etc.), fires `(on-update timeline chunk)` per event. TUI
uses it. Telegram doesn't. **Phase 4 wires it up.**

`:on-chunk` phases (from `loop.clj`): `:reasoning`, `:form-start`,
`:form-result`, `:iteration-final`, `:iteration-error`. Reasoning carries
`:thinking` text deltas — real LLM token stream. Forms are discrete
events.

### 3.2 Why answer prose can't be token-streamed

Vis is code-as-output. The LLM emits Clojure forms; one form is
`(vis/answer …)`. The args are fully assembled in the LLM's source before
any form executes. Streaming applies to **reasoning** and **tool/form
activity**, not answer-prose deltas.

### 3.3 commonmark-java — the parser substrate

`org.commonmark/commonmark` 0.27.1, BSD-2 license. ~200 KB core, zero
transitive deps. GFM extensions are separate artifacts (~50 KB each):
`commonmark-ext-gfm-tables`, `commonmark-ext-autolink`,
`commonmark-ext-gfm-strikethrough`. Total ~350 KB. Active project on
commonmark.org.

We import it directly (no Clojure wrapper). Our `->hiccup` visitor
(~80 LOC) firewalls Java types — past the visitor, it's pure Clojure
data.

### 3.4 Persistence — already block-shaped

`iteration.blocks` (`extensions/persistance/vis-persistance-sqlite/.../V1__schema.sql`
~line 241) is **Nippy-encoded BLOB** holding per-form result values.
Nippy roundtrips vectors+keywords+maps+strings natively. **Zero schema
migration needed** — Hiccup AST persists as-is alongside legacy strings.

Read-path normalization (§5) wraps legacy string answers as
`[:answer [:text "..."]]` on demand; old conversations stay readable
forever.

---

## 4. Pipeline design

### 4.1 The unified flow (one diagram)

```
                LLM emits one of:

  (vis/answer "## Result\n- foo")        ← markdown string  (most common)
  (vis/answer [[:h2 "Result"]            ← Hiccup vector    (structured)
              [:ul [:li "foo"]]])
  (vis/answer "## Intro"                 ← variadic mix     (prose + structure)
              [:vis/data {:value x}]
              "## Outro")
  (vis/answer [:answer "..." [:vis/file ...]])  ← single [:answer ...] root
                       │
                       ▼
            answer-value->ast   (loop.clj, ~30 LOC)
            normalize all surface forms → [:answer & blocks]
                       │
                       ▼
                 canonical AST
              (Nippy-persisted as-is)
                       │
                       ▼
              md/render input flavor opts
                       │
              per-block dispatch:
                  string?  → commonmark-java.parse → ->hiccup (visitor)
                  vector?  → use directly (no parse)
                       │
                       ▼
                unified Hiccup tree
                       │
                       ▼
            render-html / render-plain
              (multimethod walks tree)
                       │
       ┌───────────────┼───────────────┐
       ▼               ▼               ▼
   Telegram         TUI            CLI/run
   (parse_mode      screen         stdout
    HTML)           append
```

Key insight: **commonmark runs only on the string path**. Hiccup vectors
arrive at the renderer pre-AST. Defect #10 risk is confined to string
blocks; structured blocks are balanced-by-construction.

### 4.2 Public API

```clojure
(ns com.blockether.vis.internal.markdown ...)

;; survives unchanged from current file:
(defn conversation->markdown ...)        ;; DB → MD doc exporter

;; new public surface (entire 42-fn v/ DSL is GONE):
(defn parse [text] ...)                  ;; String → Hiccup via commonmark-java
(defn ->ast [input] ...)                 ;; normalize string|vector|other → [:answer ...]
(defn render
  "Render answer input to a flavor.
   Input: string, Hiccup vector, [:answer ...] AST, or variadic seq.
   flavor ∈ #{:plain :html}.
   opts:
     :partial?  bool   - mid-stream lenient (drop trailing unclosed
                         block elements; emit unclosed inline markers
                         as literal text)
     :context   #{:thinking :answer :status :error}
     :max-length int   - hard cap; renderer truncates with ellipsis"
  ([input flavor]      (render input flavor nil))
  ([input flavor opts] ...))
(defn render-plain [ast opts] ...)
(defn render-html  [ast opts] ...)
```

`core.clj` re-exports as `vis/md-parse`, `vis/md-render`, `vis/md-render-html`,
`vis/md-render-plain`, `vis/md->ast`. Exposed in SCI sandbox so LLM-emitted
code can preview/inspect rendering when needed (rare).

### 4.3 Block taxonomy (v1)

Standard Hiccup tags emitted by commonmark visitor (text-content blocks):

| Tag | Source |
|---|---|
| `[:answer ...]` | normalization root |
| `[:h1] … [:h6]` | commonmark `Heading` |
| `[:p]` | `Paragraph` |
| `[:strong]`, `[:em]`, `[:s]`, `[:u]`, `[:code]` (inline) | inline emphasis / code |
| `[:pre [:code {:language L} src]]` | `FencedCodeBlock` (lang attr from info) |
| `[:ul]`, `[:ol]`, `[:li]` | bullet / ordered lists |
| `[:a {:href}]` | `Link` |
| `[:img {:src :alt}]` | `Image` |
| `[:blockquote]` | `BlockQuote` |
| `[:hr]` | `ThematicBreak` |
| `[:br]` | `HardLineBreak` |
| `[:table] [:thead] [:tbody] [:tr] [:td] [:th]` | GFM tables ext |

Vis-specific structured tags (LLM-emitted, no parser involvement):

| Tag | Attrs | Semantics |
|---|---|---|
| `[:vis/text s]` | none | sugar for raw markdown text; equivalent to passing `s` directly. Mostly redundant; offered for symmetry. |
| `[:vis/code {:lang STR}? src]` | `:lang` optional | code block; channel renders syntax-highlighted if it can. Equivalent to `[:pre [:code {:language L} src]]` post-parse, but explicit. |
| `[:vis/data {:value V}]` | `:value` any value | structured Clojure data; channel pretty-prints per medium. |
| `[:vis/table {:rows [[...]] :headers [STR]?}]` | rows, headers? | tabular data; renders as native table where supported, `<pre>` aligned-text fallback. |
| `[:vis/file {:path STR :title STR?}]` | path, title? | file reference; Telegram attaches via `sendDocument`, CLI prints path. |
| `[:vis/image {:url STR? :path STR? :alt STR?}]` | url xor path, alt? | image reference; Telegram attaches via `sendPhoto`. |
| `[:vis/needs-input {:text STR}]` | text | clarification request; equivalent to legacy `:vis/answer-mode :needs-input` map. |
| `[:vis/* ...]` | any | **fallthrough.** Unknown `:vis/*` tag → renderer drops the tag wrapper and renders children. Forgiving. |

`v1` deliberately omits `:vis/audio`, `:vis/video`, `:vis/card`,
`:vis/actions`, `:vis/chart`. Each added when a real consumer needs it.

### 4.4 LLM-facing helpers (SCI sandbox)

```clojure
(md/text "string")                ;; ⇒ [:vis/text "string"]
(md/code "(+ 1 1)" :clojure)      ;; ⇒ [:vis/code {:lang "clojure"} "(+ 1 1)"]
(md/data x)                       ;; ⇒ [:vis/data {:value x}]
(md/table rows {:headers [...]})  ;; ⇒ [:vis/table {:rows rows :headers [...]}]
(md/file "/p" "Title")            ;; ⇒ [:vis/file {:path "/p" :title "Title"}]
(md/image {:url "..."})           ;; ⇒ [:vis/image {:url "..."}]
```

Sugar only. The LLM may emit raw vectors directly. Helpers exist for
prompt-side ergonomics ("call `md/data` to embed Clojure data, channel
pretty-prints").

### 4.5 Normalization (`answer-value->ast`)

Replace `loop.clj/answer-value-string` with `answer-value->ast`. Pure,
total, never throws on shape:

```clojure
(defn answer-value->ast [v]
  (cond
    ;; already canonical
    (and (vector? v) (= :answer (first v)))      v
    ;; needs-input legacy map
    (needs-input-answer? v)
    [:answer [:vis/needs-input {:text (:answer/text v)}]]
    ;; bare string → single text "block" (rendered via commonmark)
    (string? v)                                  [:answer v]
    ;; single Hiccup-shaped vector
    (and (vector? v) (keyword? (first v)))       [:answer v]
    ;; vector of mixed → recurse over elements
    (vector? v)
    (into [:answer] (mapv coerce-elem v))
    ;; sequence of args (variadic captured as list)
    (sequential? v)
    (into [:answer] (mapv coerce-elem v))
    ;; anything else → debug-render as data
    :else [:answer [:vis/data {:value v}]]))

(defn- coerce-elem [x]
  (cond
    (string? x)                            x
    (and (vector? x) (keyword? (first x))) x
    :else                                  [:vis/data {:value x}])))
```

Adjacent string blocks coalesce with `\n\n` paragraph break:
`[:answer "a" "b"]` → `[:answer "a\n\nb"]`. Lets the LLM emit
`(answer "intro" data "outro")` and get one prose flow on either side.

### 4.6 Telegram-HTML renderer mapping

Telegram-HTML allowlist: `<b> <i> <u> <s> <code> <pre> <a> <blockquote>
<blockquote expandable> <tg-spoiler> <pre><code class="language-…">`.
Anything else → parse error → message rejected. Renderer never emits
anything outside this set.

| Hiccup tag | Telegram-HTML |
|---|---|
| `[:answer & blocks]` | concat each block's render, top-level blocks separated by `\n\n` |
| string block (parsed via commonmark) | rendered per below |
| `[:h1 ... text]` … `[:h6 ...]` | `<b>{text}</b>\n` (no level distinction) |
| `[:p {} & c]` | `{c}` |
| `[:strong]` / `[:em]` / `[:u]` / `[:s]` | `<b>` / `<i>` / `<u>` / `<s>` |
| `[:code & c]` | `<code>{c}</code>` |
| `[:pre [:code {:language L} src]]` | `<pre><code class="language-{L}">{escape src}</code></pre>` (no `:language` → plain `<pre>`) |
| `[:a {:href URL} & c]` | `<a href="{URL}">{c}</a>` |
| `[:ul]` / `[:ol]` | walk children, prefix each with `• ` (ul) or `N. ` (ol); nested = 2-space indent + bullet |
| `[:li & c]` | `{c}\n` |
| `[:blockquote]` | `<blockquote>{c}</blockquote>` |
| `[:hr]` | `─────────\n` (Telegram has no `<hr>`) |
| `[:table]` etc. | `<pre>{aligned-text}</pre>` (table-walker preserves header/separator) |
| `[:img {:src :alt}]` (markdown-derived) | `<i>🖼 {alt or "image"}</i>` text placeholder |
| `[:vis/code {:lang} src]` | `<pre><code class="language-{lang}">{escape src}</code></pre>` |
| `[:vis/data {:value v}]` | `<pre><code class="language-clojure">{escape (with-out-str (pp/pprint v))}</code></pre>` (truncated >3500 chars) |
| `[:vis/table {:rows :headers}]` | `<pre>{aligned-text}</pre>` |
| `[:vis/file {:path :title}]` | text-bubble: `<i>📎 {title or basename}</i>`; **side effect:** schedule `sendDocument` after text bubble |
| `[:vis/image {:url|:path :alt}]` | text-bubble: `<i>🖼 {alt or basename}</i>`; **side effect:** schedule `sendPhoto` after text bubble |
| `[:vis/needs-input {:text}]` | `(md/render text :html opts)` — needs-input renders as plain text answer |
| `[:vis/* & children]` | render children, drop wrapper |
| Unknown tag | render children, drop wrapper |
| Text node (string) | HTML-escape `&<>"`; never trust raw |

**Special — `:context :thinking`:** wrap whole rendered output in
`<blockquote expandable>...</blockquote>`. Streaming live-bubble uses
this.

**Special — `:context :status`:** render as `<i>...</i>` single-line,
drop block elements.

**Special — `:partial? true`:** drop trailing unclosed code-fence;
unclosed inline emphasis renders as literal text; renderer invariant:
result is well-formed HTML, never fails Telegram parse.

### 4.7 Plain renderer mapping

`render-plain` is fallback / TUI default / CLI default. Strips
formatting to readable text. Same dispatch on Hiccup tags; `[:vis/data]`
pretty-prints with `pp/pprint`; `[:vis/table]` aligned monospace;
`[:vis/file]` → `📎 {path}`; `[:vis/image]` → `🖼 {alt}: {url|path}`.

### 4.8 Channel renderer registry — `:channel/messages-renderer-fn`

New optional key in extension `:ext/channels` registration:

```clojure
{:ext/channels
 [{:channel/id      :telegram
   :channel/cmd     "telegram"
   :channel/main-fn #'channel-main
   :channel/messages-renderer-fn  #'render-for-telegram   ;; ← new
   ...}]}

(defn render-for-telegram [input opts]
  (md/render input :html opts))
```

**Contract:** `(fn [input opts] → renderer-output)`. Input is
string|vector|AST (renderer normalizes via `md/->ast`). Output type is
channel-defined (Telegram: `String`; TUI: `String` today, possibly
styled-segments later).

Channel calls renderer at its single emit chokepoint
(`tg/send-message!` for Telegram; screen-emit boundary for TUI). Bot
and screen never call renderer manually.

### 4.9 Telegram attachment ordering

Mixed-content with attachments needs a deterministic flow:

1. Render the entire AST as **one HTML payload** for the text bubble.
   Each `[:vis/file]` / `[:vis/image]` emits an italic placeholder line
   in the text payload.
2. Send the text bubble first.
3. Walk the AST again; for each attachment block, in source order:
   `sendDocument` / `sendPhoto` with `reply_to_message_id` =
   text-bubble's message id. Anchors attachments to the answer
   visually.
4. If only attachments and no text content, send attachments without a
   text bubble.

### 4.10 Streaming UX

#### One live-edited bubble while turn runs

```
[💭 Thinking…
 <blockquote expandable>{last ~3500 chars of CoT, sliding window}</blockquote>
 ⏳ Running: tool-name                       ← swapped per form-start/result
 [⊘ Cancel]                                 ← inline keyboard
]
```

On stream end:

- Final-edit thinking bubble → `<blockquote expandable>{collapsed thinking}</blockquote>`,
  keyboard removed.
- Render answer through registered `:html` renderer → post as separate
  bubble.
- If answer >4096 chars, split into N bubbles via existing `chunk-text`.
- Footer `_🤖 model · in/out · duration · cost_` on the last answer
  bubble.
- After text bubble(s): walk attachments per §4.9.

#### Phase routing

| `:on-chunk` phase | Effect on live bubble |
|---|---|
| `:reasoning` (delta) | Append to thinking blockquote; sliding window keeps last ~3500 chars |
| `:form-start` | Update status line to `⏳ Running: {tool/form-id}`; refresh `chat-action "typing"` |
| `:form-result` | Status line back to `⏳ Thinking…` |
| `:iteration-final` mid-turn | Flush pending edit; status `⏳ Iteration N+1…` |
| `:iteration-final` `:done?=true` | **Stream end.** Collapse thinking, render answer, drop keyboard, send attachments |
| `:iteration-error` | Final-edit bubble to error state with collapsed thinking |

#### Throttle

- Min interval between edits: **1200 ms** (Telegram soft-limit headroom).
- Edit when (≥1200 ms elapsed) AND (Δ ≥40 chars OR newline arrived).
- Stream end: flush immediately, ignore interval.
- First paint: post initial bubble immediately on first `:on-chunk`.
- HTTP 400 `message is not modified`: swallow.
- HTTP 429: read `retry_after`, double next two intervals, decay back.
- Idle 3 s: skip edit.

#### Cancel button

```clojure
{:inline_keyboard
 [[{:text "⊘ Cancel" :callback_data (str "cancel:" chat-id)}]]}
```

`callback_data = "cancel:<chat-id>"` (fits Telegram 64-byte cap; bot
guarantees one in-flight turn per chat via `set-in-flight!`).
`handle-callback-query!` matches `cancel:` prefix → cancel turn-token in
`chat-state` atom. Outcome: thinking bubble final-edited to `⊘
Cancelled by user.` + collapsed thinking + cancel-flavored short footer;
no new message. Pre-first-paint cancel falls back to fresh `Cancelled.`
message.

`/cancel` text command kept alongside the button.

#### Voice-mode interplay

Voice-output chats stream the thinking bubble normally. After turn
completion: existing voice flow runs unchanged (`record_voice`
chat-action → synth WAV → send voice note → optional text-answer bubble
gated by `:telegram-send-answer-text?`). Streaming primary value
(liveness signal) applies regardless of output modality.

For mixed-content answers in voice mode: TTS reads concatenated
`[:p]` / `[:vis/text]` content only (extracted via
`md/extract-text-blocks`); `[:vis/code]`, `[:vis/data]`, `[:vis/table]`
summarized as "see chat for code / data / table"; attachments still
ship per §4.9 (3).

### 4.11 Footer

Footer (`format-footer`) renders at the end of the last answer bubble.
Single bubble in the common case; last chunk in the multi-chunk case.
Thinking bubble's collapsed-summary state has no footer. Cancelled
mid-stream: thinking bubble's `⊘ Cancelled by user.` final state gets a
short footer (`_⊘ Cancelled · {duration} · {model}_`).

---

## 5. Persistence

### 5.1 Zero schema migration

`iteration.blocks` Nippy-encodes per-form result values. `(answer …)`
writes the normalized AST `[:answer & blocks]` into the answer-atom; the
loop persists the form result as part of the iteration row. Nippy
roundtrips vectors+keywords+maps+strings natively.

Existing string answers persist as strings → on read, `db-turn-answer-ast`
wraps as `[:answer s]`. Existing needs-input maps persist as maps →
wrapped as `[:answer [:vis/needs-input {:text "..."}]]`. Old conversations
remain readable forever.

### 5.2 Read path

```clojure
(defn db-turn-answer-ast
  "Return the canonical answer AST for a turn, normalizing legacy
   string / needs-input shapes to [:answer ...] form."
  [db turn-id]
  (-> (raw-answer-value db turn-id) answer-value->ast))
```

Channels and exporters call this instead of reading raw answer-text.

### 5.3 `conversation->markdown` exporter

The existing exporter uses `db-turn-answer-ast` and renders each turn's
AST through `(md/render ast :markdown)` (a `:markdown` flavor that
walks Hiccup → markdown-string). String-only answers continue to render
verbatim because their normalized AST is `[:answer "..."]` and rendering
a string back to markdown is identity (commonmark-java has a
`MarkdownRenderer`).

`:markdown` flavor coverage:
- Each Hiccup tag → standard markdown construct.
- `[:vis/data]` → ` ```clojure ... ``` ` block.
- `[:vis/table]` → GFM table.
- `[:vis/file]` → `[title](file://path)` link.
- `[:vis/image]` → `![alt](url)`.
- `[:vis/needs-input]` → blockquote with the text.

---

## 6. Implementation phases

### Phase 0 — Demolition (its own PR, no new behavior)

1. Remove from `extensions/common/vis-foundation/src/.../foundation/markdown.clj`
   all `v/` markdown helpers (`h h1 h2 h3 h4 h5 h6 p ul ol checklist
   blockquote code-block code bold italic i kbd strike underline link
   image file-link anchor hr br details table join lines section
   escape`). Keep namespace; rewrite docstring.
2. Remove same fns from `src/.../internal/markdown.clj`. Keep
   `conversation->markdown` (separate concern). File shrinks
   ~1100→~400 LOC.
3. Update 17 call-sites (rg `vis.internal.markdown` and
   `foundation.markdown`) to inline plain markdown strings.
4. Update `src/.../internal/prompt.clj` system-prompt section: drop the
   v/ catalogue (~660 chars), replace with one sentence: "Answer in
   GitHub-flavored Markdown. Use fenced code blocks with language tags.
   For structured data, use Hiccup vectors via `md/data`, `md/code`,
   `md/table`, `md/file`, `md/image`." (~170 chars net.)
5. Remove `convert-md-to-html` + helpers from
   `vis-channel-telegram/.../api.clj`. `tg/send-message!` temporarily
   ships **plain text** (no parse_mode). Cosmetic regression for one
   PR; closes in Phase 3.
6. Delete tests for removed helpers (foundation markdown_test for the
   helper coverage; keep `conversation->markdown` tests).
7. Update `vis-foundation/resources/META-INF/vis-extension/vis.edn` to
   drop removed symbol exports.
8. `./verify.sh` passes; existing channels/tests green; LLM still
   answers (raw markdown now); Telegram chats see plain-text answers.

### Phase 1 — New markdown core

1. Add to root `deps.edn`:
   ```clojure
   org.commonmark/commonmark                       {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-gfm-tables        {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-autolink          {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-gfm-strikethrough {:mvn/version "0.27.1"}
   ```
2. In `src/.../internal/markdown.clj`:
   - `(parse text)` — wrap commonmark `Parser/builder` with three
     extensions; return `org.commonmark.node.Document`.
   - `->hiccup` visitor — `condp instance?` over node classes (~80
     LOC, see §4.6 of original spec drafts; same shape).
   - `(->ast input)` — normalize string|vector|other to `[:answer ...]`.
   - `(render input flavor opts)` — string→parse→visit→render or
     vector→render directly.
   - `render-html` / `render-plain` / `render-markdown` multimethods.
   - Lenient `:partial?` mode: detect last block is incomplete; drop or
     downgrade to text node.
3. In `src/.../core.clj`: re-export.
4. SCI sandbox bindings: same names under `md/` alias.
5. New tests `src/.../internal/markdown_test.clj`:
   - parse roundtrip for each construct;
   - per-renderer per-flavor output for the same;
   - normalization: all four surface forms → identical canonical AST;
   - adversarial: lonely `*`, lonely backtick, unclosed fence, nested
     emphasis, GFM table no-separator;
   - `:partial?` lenient mode covers each adversarial case mid-stream;
   - `:context :thinking` wraps in expandable blockquote;
   - `:max-length` truncates at paragraph boundary;
   - mixed-content: `(answer "prose" [:vis/data x] "more")` →
     normalized → rendered correctly per flavor.

### Phase 2 — Channel renderer registry

1. `src/.../internal/extension.clj`: add `:channel/messages-renderer-fn`
   to channel registration spec. Optional. Validator update.
2. Tests: registration with and without slot; lazy resolution at emit
   time so var redefs during dev pick up.

### Phase 3 — Telegram renderer + answer-AST + attachments

1. Telegram extension registers `:channel/messages-renderer-fn
   #'render-for-telegram`.
2. `tg/send-message!` looks up renderer-fn, calls it. Bot stops passing
   pre-rendered text.
3. `bot.clj/handle-user-text!` reads
   `(db-turn-answer-ast (:turn-id result))` instead of raw answer
   string; passes AST to renderer; walks attachments per §4.9.
4. Add `tg/send-document!`, `tg/send-photo!` to `api.clj` (multipart,
   similar to existing `send-voice!`).
5. Cosmetic regression from Phase 0 closes — answers re-gain HTML
   formatting via commonmark + visitor + multimethod.
6. `loop.clj/answer-value-string` removed; replaced by
   `answer-value->ast` (returns AST, not string). `answer-str` shim
   does `(md/render ast :plain)` for code paths that want a flat
   string (logs, error formatting, etc.).
7. Tests: existing telegram tests pass with HTML output; new tests for
   exact mappings in §4.6; attachment ordering test (text bubble first,
   then files/images as replies-to).

### Phase 4 — Streaming + cancel button (the big behavioral change)

1. `tg/edit-message!` in `api.clj` (HTTP `editMessageText`, accepts
   `reply_markup`).
2. `tg/delete-message!` for edge cleanup.
3. `bot.clj/live-bubble!` helper — owns `chat_id` + `message_id` +
   render state + throttle (§4.10 throttle policy). Calls
   `tg/edit-message!` with renderer-fn output.
4. `bot.clj/handle-user-text!`:
   - Build progress-tracker whose `:on-update` drives `live-bubble!`.
   - Pass tracker's `:on-chunk` via `:hooks {:on-chunk ...}` to
     `vis/send!` opts.
   - On `:iteration-final :done?=true`: final-edit thinking bubble
     (collapse), remove keyboard, render answer-AST through renderer,
     post bubble(s), walk attachments.
5. `bot.clj/handle-callback-query!` matches `cancel:<chat-id>` → fires
   cancellation on stored turn-token.
6. Tests `bot_streaming_test.clj`: throttle 1200 ms; final flush
   ignores interval; not-modified swallowed; 429 backoff doubles
   interval; cancel button removes keyboard + edits in place;
   pre-first-paint cancel → fresh message; sliding-window thinking
   truncation; multi-iteration boundary flush.

### Phase 5 — Voice-mode integration

1. Voice path streams thinking bubble. After stream end: extract
   `[:p]` / `[:vis/text]` content via `md/extract-text-blocks`;
   synth → voice note. Attachments still ship per §4.9.
2. Tests: voice + streaming + mixed-content together.

### Phase 6 — TUI through registry (separate PR, deferred)

1. TUI extension registers `(fn [input opts] (md/render input :plain opts))`.
2. Screen-emit boundary uses it.
3. Future: `:tui-styled` flavor — own design discussion when needed.

---

## 7. Risks & mitigations

| Risk | Mitigation |
|---|---|
| commonmark-java dep wedge | Pin 0.27.1 across all four artifacts; AGENTS.md S1 noted; deps.edn diff is 4 lines. |
| LLM emits malformed `[:vis/*]` (missing required attrs, unknown tag) | `[:vis/*]` fallthrough renders children only; missing attrs render block as `[:vis/data {:value <whole-block>}]` debug surface. Defect-#10 invariant extends to structured input. Renderer never crashes. |
| Phase 0 cosmetic regression (plain-text Telegram answers) blocks too long | Phases 0–3 sequenced tight; window ~days. |
| LLM output drifts after dropping `v/` DSL guidance | New prompt is two sentences. Models default to GFM out of the box. Worst case: prompt grows back. Cheap. |
| Telegram rate-limit during streaming | Throttle constants centralized; 429 backoff in spec. |
| Inline button spammed | Telegram dedupes rapid taps; cancellation-token idempotent. |
| Attachments silently dropped by channels not implementing them | Renderer fallback: italic placeholder. Worst case user sees `_📎 path/to/file.txt_` — recoverable. |
| `progress-tracker` semantics drift in core | TUI uses same tracker; cross-channel safety net. |
| commonmark-java SCI compat | Library is host-Java-only; renderer is pure Clojure. SCI smoke test in Phase 1 confirms `md/render` works in sandbox via the Clojure renderer; LLM-emitted `(md/render ...)` calls execute in host context where Java is available. |
| Persistence size growth | Nippy is compact; a 5-block answer ~2× the bytes of an equivalent string. KB-scale. |
| Existing extensions wrapping `(:answer result)` as String break | `answer-str` shim returns rendered-plain string for back-compat. Affected: `vis/format-error`, `tg/answer-details-message`, voice synth. All migrated in Phase 3. |
| Block taxonomy fan-out unbounded | v1 = 7 `:vis/*` tags + standard Hiccup. Each new tag → design discussion + PR. `:vis/*` fallthrough means unknown tags don't break old channels. |

---

## 8. Out of scope (explicitly)

- Per-token answer-prose streaming (§3.2).
- Streaming voice synth (audio is post-completion).
- Telegram MarkdownV2 parse-mode (HTML is target; MdV2 is a one-line
  flip later).
- Reasoning-as-permanent-transcript (CoT remains live signal; full
  transcript in Vis DB unchanged).
- TUI styled-segment renderer (Phase 6 + future).
- DB schema changes (zero — Nippy already roundtrips).
- Configuration UX for throttle constants.
- Image generation / chart rendering pipelines.
- Layout primitives.

---

## 9. Open items

None at spec time. Implementation proceeds Phases 0 → 5 sequentially;
Phase 6 is explicit follow-up PR.
