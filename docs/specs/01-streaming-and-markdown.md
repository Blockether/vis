# Specification — Answer IR (Hiccup-EDN), Streaming, and Channel Rendering

Status: design-locked, pre-implementation.
Single PR, single spec.

This spec adopts a **Hiccup-EDN intermediate representation (IR)** as the
canonical answer shape. The IR is MDAST-equivalent (Markdown abstract
syntax tree) but uses Clojure's vector syntax for compactness — measured
~65–70 % token reduction vs JSON-MDAST on real prose. The LLM emits
either bare markdown strings (parsed via commonmark-java) or Hiccup-EDN
vectors (used directly); both routes produce the same canonical IR;
all channels render off it.

`needs-input` (legacy `:vis/answer-mode :needs-input` map) is removed.
The concept is over-engineering — when the LLM wants clarification, it
asks in prose; the user follows up. Same as every other chat agent.

This document supersedes the previous draft and the deleted
`02-mixed-content-answer.md`.

---

## 1. Problem statement

### 1.1 Current state — over-engineered AND still produces broken output

```
src/.../internal/markdown.clj                                      1155 LOC
extensions/common/vis-foundation/src/.../foundation/markdown.clj    259 LOC
extensions/channels/vis-channel-telegram/.../api.clj  ~150 LOC of regex MD→HTML
LLM system prompt section listing 38 v/* helpers                   ~660 chars
Call-sites using v/ DSL                                            17 files
needs-input gate                                                   2 ns + prompt logic
                                                                  ─────────
                                                                  ~1500 LOC owned + prompt verbiage
```

The `v/` DSL forces the LLM to construct markdown via `(v/h1 ...)`,
`(v/p ...)`, `(v/ul ...)`, `(v/code-block ...)`, `(v/join ...)`.
Empirically (per the autoresearch session that just ran):

- LLM still emits raw HTML (`<details>`).
- LLM still emits Unicode arrows (`→`) where ASCII (`->`) belongs.
- Channel-side regex MD→HTML fails on unbalanced inline markers
  (defect #10).
- `needs-input` adds prompt-flow conditional logic for a behavior the
  user can get by just asking.

### 1.2 No streaming UX

`bot.clj/handle-user-text!` blocks on `(vis/send! ...)`, ships full
answer in one `tg/send-message!`. Users stare at `typing…` for
10–60 s with no signal. Cancellation is `/cancel` text command — not
discoverable.

### 1.3 No structured-content path

`(vis/answer x)` flattens non-string via `(str v)`. LLM stringifies
tool results into markdown fences, losing structure, channel-blind,
risking nested-fence parser collisions (LangChain bugs #2241 / #9535).

Industry has converged on **typed content blocks** (Anthropic, OpenAI,
LangChain 1.0 `content_blocks`). Vis is the holdout.

### 1.4 Why one IR

The IR pays for itself five times:
- **Parser robustness** — commonmark on string blocks; vector blocks
  bypass parsing entirely (defect #10 ceiling).
- **Channel symmetry** — render-html / render-plain / render-md /
  future render-tg-mdv2 / future render-ansi are mechanical
  tree-walks off the same IR.
- **Persistence transparency** — Nippy already roundtrips
  vectors+keywords+strings; zero schema migration.
- **Domain extensions** — same IR carries Vis-specific tags
  (`:vis/data`, `:vis/file`, future `:vis/cite`, `:vis/conf`) that
  channels can render specially without parsing tricks.
- **Grammar-constrainable** — the IR is a small EDN sublanguage;
  future structured-output decoding (xgrammar / llguidance / Outlines)
  can constrain the LLM to emit valid IR. Out of v1 scope but the
  shape doesn't preclude it.

### 1.5 Token efficiency — measured

Same clinical sentence three ways:

```
JSON-MDAST:
  {"type":"paragraph","children":[
    {"type":"text","value":"Client showed signs of "},
    {"type":"strong","children":[{"type":"text","value":"abandonment"}]},
    {"type":"text","value":" schema activation."}]}
  → ~70 tokens

Hiccup-EDN:
  [:p "Client showed signs of " [:strong "abandonment"] " schema activation."]
  → ~22 tokens

Markdown:
  Client showed signs of **abandonment** schema activation.
  → ~12 tokens
```

Markdown is still cheapest for prose. Hiccup is ~3× MD but ~⅓ of
JSON-MDAST. **Cost-aware design:** prose stays markdown (LLM cheap,
fluent); structure uses Hiccup (token-acceptable, no parsing
ambiguity).

---

## 2. Goals / non-goals

### Goals

- One IR — Hiccup-EDN, MDAST-equivalent, namespace-aware
  (`:p`, `:strong`, … from MD; `:vis/data`, `:vis/file`, … from us).
- LLM emits markdown strings, Hiccup vectors, or any mix. All work
  day one.
- Multimethod renderers per `[target tag]` for plain / html / md.
- 100 % backward compatible with persisted string answers.
- Defect #10 dies. Unbalanced markdown never crashes a channel.
- Live-bubble streaming with one-tap cancel.
- Net code reduction in `vis-foundation` and `internal/markdown`.

### Non-goals

- Per-token answer-prose streaming (architectural).
- Hand-rolled markdown parser.
- `needs-input` semantic (removed).
- Grammar-constrained LLM decoding (future, IR-compatible).
- Image generation / chart rendering.
- Layout primitives (rows, columns, grids).
- Domain-ontology validation (the `:schema`, `:mode`, `:cite`, `:conf`
  ideas from the IR-design literature are extension points; v1 ships
  the slot, not validators).

---

## 3. The IR — Hiccup-EDN

### 3.1 Node algebra

```
Node      = Block | Inline | Text
Text      = String                        ; bare string is the text node
Block     = [BlockTag Attrs? & (Block | Inline)*]
Inline    = [InlineTag Attrs? & Inline*]

Attrs     = {Keyword Any}                 ; optional; absent ≡ {}
```

`{}` attrs are normalized in at `->ast` boundary so renderers can
always rely on `(second node)` being a map.

### 3.2 Standard tags (commonmark visitor produces these)

#### Root

| Tag | Form | Semantics |
|---|---|---|
| `:doc` | `[:doc & blocks]` | Root container. Always present after normalization. |

#### Blocks

| Tag | Form | MDAST equiv |
|---|---|---|
| `:p` | `[:p & inlines]` | `paragraph` |
| `:h` | `[:h {:level 1-6} & inlines]` | `heading` (single tag, level in attrs — token-efficient vs `:h1`–`:h6`) |
| `:quote` | `[:quote & blocks]` | `blockquote` |
| `:hr` | `[:hr]` | `thematicBreak` |
| `:code` | `[:code {:lang String?} String]` | `code` (block-level fenced; single string child) |
| `:ul` | `[:ul & items]` | `list` (unordered) |
| `:ol` | `[:ol {:start Int?} & items]` | `list` (ordered) |
| `:li` | `[:li & content]` | `listItem`. **Content rule:** all blocks OR all inlines, never mixed. Loose inlines wrapped in `:p` at validation. |
| `:table` | `[:table & rows]` | GFM `table` |
| `:tr` | `[:tr & cells]` | `tableRow` |
| `:th` | `[:th & inlines]` | `tableCell` (header) |
| `:td` | `[:td & inlines]` | `tableCell` (body) |

#### Inlines

| Tag | Form | MDAST equiv |
|---|---|---|
| `:em` | `[:em & inlines]` | `emphasis` |
| `:strong` | `[:strong & inlines]` | `strong` |
| `:del` | `[:del & inlines]` | `delete` (strikethrough) |
| `:c` | `[:c String]` | `inlineCode`. **One-char tag** — saves tokens; visitor emits `:c` not `:code` for inline (block code uses `:code` with `{:lang}` and exactly one string child). |
| `:a` | `[:a {:href String} & inlines]` | `link` |
| `:img` | `[:img {:src String :alt String?}]` | `image` |
| `:br` | `[:br]` | `break` (hard line break) |

Soft line breaks become `" "` text nodes (commonmark `softBreak` →
single space).

### 3.3 Vis-specific tags (LLM-emitted; no parser involvement)

| Tag | Form | Semantics |
|---|---|---|
| `:vis/code` | `[:vis/code {:lang String?} String]` | Code block. Equivalent post-normalization to `[:code {:lang L} src]`. Offered for sugar/symmetry from `md/code` helper. |
| `:vis/data` | `[:vis/data {:value Any}]` | Structured Clojure data. Channel pretty-prints. |
| `:vis/table` | `[:vis/table {:rows [[Cell]] :headers [Cell]?}]` | Tabular. **Cell = String \| Inline** — strings parsed as inline markdown; inline nodes used directly. |
| `:vis/file` | `[:vis/file {:path String :title String?}]` | File reference. Telegram attaches via `sendDocument`. |
| `:vis/image` | `[:vis/image (\| {:url String :alt String?} {:path String :alt String?})]` | Image. **Mutually exclusive `:url` xor `:path`** at type level. Telegram attaches via `sendPhoto`. |

`:vis/*` is the **extension namespace**. Any unknown `:vis/*` tag →
renderer drops the wrapper, renders children. Forgiving by design.
Future tags (`:vis/cite`, `:vis/conf`, `:vis/spoiler`, …) plug in via
multimethod arms; channels that don't implement them get
graceful-degrade-to-children.

### 3.4 Domain extensions — open slot, no v1 validators

The IR-design literature (psychotherapy RLM example) advocates domain
tags like `:schema`, `:mode`, `:cite`, `:conf` whose `:id` values are
validated against a loaded ontology. v1 reserves the **mechanism**
(extension-registered renderer arms; default fallthrough) but ships
**no validators or ontology**. Extensions that want them register a
fn under the `:vis/*` tag and the spec contract is "renderer-fn
returns a renderer-output, validation is the extension's problem."

### 3.5 What's removed

| Concept | Why |
|---|---|
| `:vis/answer-mode :needs-input` map | Over-engineering. LLM asks in prose, user replies. |
| `(v/needs-input …)` helper | ditto |
| `loop.clj/needs-input-answer?` predicate | ditto |
| Per-iteration "model wants more input" prompt-flow gate | ditto |
| `[:vis/text]` block (was in earlier draft) | Bare strings already are text nodes. |
| `[:answer …]` root (was in earlier draft) | Replaced by `[:doc …]` (MDAST convention; matches literature). |
| `:h1` … `:h6` separate tags | Replaced by `[:h {:level N}]` (single tag, fewer dispatch arms, token-equivalent). |
| `:blockquote` | Renamed `:quote` (shorter, same semantic). |
| `:s` | Renamed `:del` (matches MDAST `delete`). |
| `:code` inline tag (ambiguous) | Inline code → `:c`; block code keeps `:code` with `{:lang}`. |
| `:thead`/`:tbody` | Dropped. GFM tables → flat `[:table & rows]`; first row's tag is `:tr` whose cells are `:th` (header) or `:td` (body). |

---

## 4. Surface forms

What the LLM can pass to `(vis/answer …)`:

```clojure
;; (1) Plain markdown string — most common, cheapest tokens
(vis/answer "## Result\n\n- foo\n- bar")

;; (2) Hiccup vector — structured, no parsing ambiguity
(vis/answer
  [:doc
   [:h {:level 2} "Result"]
   [:ul [:li "foo"] [:li "bar"]]])

;; (3) Variadic mix — prose strings + structured blocks
(vis/answer
  "## Top matches"
  [:vis/table {:rows results :headers ["path" "size"]}]
  "Inspecting the largest:"
  [:vis/code {:lang "clojure"} (slurp largest-path)]
  [:vis/file {:path largest-path}]
  "Looks like a refactor candidate.")

;; (4) Single Hiccup vector with non-:doc root — coerced to wrap in :doc
(vis/answer [:p "one paragraph"])

;; (5) Pre-built [:doc ...] — passed through
(vis/answer [:doc [:p "explicit"]])
```

All five normalize to the same canonical `[:doc & blocks]` AST.

### 4.1 Normalization (`answer-value->ast`)

```clojure
(defn answer-value->ast
  "Pure, total. Never throws on shape."
  [v]
  (cond
    ;; already canonical
    (and (vector? v) (= :doc (first v)))    (normalize-attrs v)
    ;; bare string → single text block, parsed at render time
    (string? v)                             [:doc v]
    ;; single Hiccup-shaped vector that isn't :doc → wrap
    (and (vector? v) (keyword? (first v)))  [:doc (normalize-attrs v)]
    ;; vector of mixed → recurse
    (vector? v)
    (into [:doc] (mapv coerce-elem v))
    ;; sequence (variadic args) → same
    (sequential? v)
    (into [:doc] (mapv coerce-elem v))
    ;; anything else → debug-render as data
    :else
    [:doc [:vis/data {:value v}]]))

(defn- coerce-elem [x]
  (cond
    (string? x)                            x          ; bare strings stay
    (and (vector? x) (keyword? (first x))) (normalize-attrs x)
    :else                                  [:vis/data {:value x}]))
```

**No auto-coalesce of adjacent strings.** Critique #6 from the prior
review: auto-merging `(answer "Hello" " world")` to one paragraph
was wrong — the LLM should call `(str ...)` if it wants concat,
otherwise we treat each string as its own paragraph block. Strings
adjacent at top level remain separate; renderer joins with `\n\n`
between them at render time.

`normalize-attrs` ensures every node has an attrs map (inserts `{}`
where absent). Done once at AST entry. Renderers can rely on
`(second node)` being a map.

---

## 5. Pipeline

```
                LLM emits one of:

  (vis/answer "## Result\n- foo")        ← markdown string  (most common)
  (vis/answer [:doc [:h {:level 2}      ← Hiccup vector    (structured)
                     "Result"]
                    [:ul [:li "foo"]]])
  (vis/answer "## Intro"                 ← variadic mix     (prose + structure)
              [:vis/data {:value x}]
              "## Outro")
                       │
                       ▼
            answer-value->ast   (loop.clj, ~30 LOC)
            normalize all surface forms → [:doc & blocks]
                       │
                       ▼  canonical AST  (Nippy-persisted as-is)
                       │
                       ▼
              md/render input flavor opts
                       │
              per-block dispatch:
                  string?  → commonmark-java.parse → ->hiccup (visitor)
                  vector?  → use directly
                       │
                       ▼
                unified Hiccup tree
                       │
                       ▼
            multimethod render dispatch:
              (defmulti render
                (fn [target node]
                  [target (if (string? node) ::text (first node))]))
                       │
       ┌───────────────┼───────────────┐
       ▼               ▼               ▼
   :html           :plain          :markdown
   (Telegram,      (TUI default,   (CLI, exporter,
    parse_mode      stripped text)  legacy roundtrip)
    HTML)
```

Commonmark runs **only** on the string-block path. Vector blocks
arrive at the renderer pre-parsed.

### 5.1 Public API

```clojure
(ns com.blockether.vis.internal.markdown ...)

(defn parse [text] ...)             ; String → [:doc ...] via commonmark-java
(defn ->ast [input] ...)            ; normalize any surface form
(defn render
  "Render answer input to a flavor.
   flavor ∈ #{:plain :html :markdown}.
   opts:
     :partial?    Boolean   - mid-stream lenient
     :context     #{:answer :thinking :status :error}
     :max-length  Int       - hard cap; truncate at paragraph boundary"
  ([input flavor]      (render input flavor nil))
  ([input flavor opts] ...))

(defmulti render-node (fn [target node opts]
                        [target (if (string? node) ::text (first node))]))

;; existing public surface kept:
(defn conversation->markdown ...)   ; DB → MD doc exporter
```

`core.clj` re-exports as `vis/md-parse`, `vis/md-render`, `vis/md->ast`.
SCI sandbox bindings: same names under `md/`. Constructor helpers
also exposed:

```clojure
(md/code src "clojure")           ⇒ [:code {:lang "clojure"} src]
(md/data x)                       ⇒ [:vis/data {:value x}]
(md/table rows {:headers [...]})  ⇒ [:vis/table {:rows rows :headers [...]}]
(md/file path "Title")            ⇒ [:vis/file {:path path :title "Title"}]
(md/image {:url u :alt a})        ⇒ [:vis/image {:url u :alt a}]
```

### 5.2 commonmark visitor (~80 LOC)

`condp instance?` over `org.commonmark.node.*`:

```
Document       → [:doc {} & children]
Heading n      → [:h {:level n} & children]
Paragraph      → [:p {} & children]
Text           → string literal
StrongEmphasis → [:strong {} & children]
Emphasis       → [:em {} & children]
Code           → [:c (.getLiteral n)]                   ; inline
FencedCodeBlock→ [:code {:lang (.getInfo n)} (.getLiteral n)]
BulletList     → [:ul {} & children]
OrderedList    → [:ol {:start (.getStartNumber n)} & children]
ListItem       → [:li {} & children]
Link           → [:a {:href (.getDestination n)} & children]
Image          → [:img {:src (.getDestination n) :alt …}]
BlockQuote     → [:quote {} & children]
ThematicBreak  → [:hr {}]
HardLineBreak  → [:br {}]
SoftLineBreak  → " "
Strikethrough  → [:del {} & children]
TableBlock     → [:table {} & rows-flattened]           ; thead/tbody dropped
TableHead      → splice children                        ; rows pass through
TableBody      → splice children
TableRow       → [:tr {} & children]
TableCell      → [:th {}|:td {} ...]                    ; header? from parent
unknown        → [:span {} & children]
```

After the visitor, **no Java types remain** — pure Clojure data.

### 5.3 Multimethod render dispatch

```clojure
(defmulti render-node (fn [target node opts]
                        [target (if (string? node) ::text (first node))]))

;; ---------------- HTML target (Telegram) -----------------------------

(defmethod render-node [:html ::text] [_ s _]
  (escape-html (str s)))

(defmethod render-node [:html :doc] [t [_ _ & cs] o]
  (apply str (interpose "\n\n" (map #(render-node t % o) cs))))

(defmethod render-node [:html :p] [t [_ _ & cs] o]
  (apply str (map #(render-node t % o) cs)))

(defmethod render-node [:html :h] [t [_ {:keys [level]} & cs] o]
  ;; Telegram has no headings → bold + blank line; level captured as a class for future tg-mdv2
  (str "<b>" (apply str (map #(render-node t % o) cs)) "</b>\n"))

(defmethod render-node [:html :strong] [t [_ _ & cs] o]
  (str "<b>" (apply str (map #(render-node t % o) cs)) "</b>"))

(defmethod render-node [:html :em] [t [_ _ & cs] o]
  (str "<i>" (apply str (map #(render-node t % o) cs)) "</i>"))

(defmethod render-node [:html :del] [t [_ _ & cs] o]
  (str "<s>" (apply str (map #(render-node t % o) cs)) "</s>"))

(defmethod render-node [:html :c] [_ [_ s] _]
  (str "<code>" (escape-html s) "</code>"))

(defmethod render-node [:html :code] [_ [_ {:keys [lang]} src] _]
  (if (seq lang)
    (str "<pre><code class=\"language-" (escape-html lang) "\">"
         (escape-html src) "</code></pre>")
    (str "<pre>" (escape-html src) "</pre>")))

(defmethod render-node [:html :a] [t [_ {:keys [href]} & cs] o]
  (str "<a href=\"" (escape-html-attr href) "\">"
       (apply str (map #(render-node t % o) cs))
       "</a>"))

(defmethod render-node [:html :ul] [t [_ _ & items] o]
  (apply str (map (fn [li] (str "• " (render-node t li o) "\n")) items)))

(defmethod render-node [:html :ol] [t [_ {:keys [start]} & items] o]
  (apply str (map-indexed (fn [i li]
                            (str (+ i (or start 1)) ". "
                                 (render-node t li o) "\n"))
                          items)))

(defmethod render-node [:html :li] [t [_ _ & cs] o]
  (apply str (map #(render-node t % o) cs)))

(defmethod render-node [:html :quote] [t [_ _ & cs] o]
  (let [inner (apply str (interpose "\n" (map #(render-node t % o) cs)))]
    (if (= :thinking (:context o))
      (str "<blockquote expandable>" inner "</blockquote>")
      (str "<blockquote>" inner "</blockquote>"))))

(defmethod render-node [:html :hr]  [_ _ _] "─────────\n")
(defmethod render-node [:html :br]  [_ _ _] "\n")
(defmethod render-node [:html :img] [_ [_ {:keys [alt]}] _]
  (str "<i>🖼 " (escape-html (or alt "image")) "</i>"))

;; tables: render as <pre> aligned monospace (Telegram has no <table>)
(defmethod render-node [:html :table] [t node o] (render-table-as-pre t node o))

;; ---------------- vis-specific blocks --------------------------------

(defmethod render-node [:html :vis/data] [_ [_ {:keys [value]}] _]
  (let [pp  (with-out-str (clojure.pprint/pprint value))
        max 3500
        s   (if (<= (count pp) max)
              pp
              (str (subs pp 0 (- max 60))
                   "\n…(truncated; "
                   (count pp) " chars total)"))]
    (str "<pre><code class=\"language-clojure\">" (escape-html s) "</code></pre>")))

(defmethod render-node [:html :vis/code] [t [_ attrs src] o]
  (render-node t [:code attrs src] o))

(defmethod render-node [:html :vis/table] [t [_ {:keys [rows headers]}] o]
  ;; build [:table [:tr [:th ...]] [:tr [:td ...]] ...] then re-dispatch
  (render-node t (vis-table->md-table rows headers) o))

(defmethod render-node [:html :vis/file] [_ [_ {:keys [path title]}] _]
  (str "<i>📎 " (escape-html (or title (last (str/split path #"/")))) "</i>"))

(defmethod render-node [:html :vis/image] [_ [_ {:keys [alt url path]}] _]
  (str "<i>🖼 " (escape-html (or alt (last (str/split (or url path) #"/")))) "</i>"))

;; ---------------- fallthrough ----------------------------------------

(defmethod render-node :default [t [_ _ & cs] o]
  ;; unknown tag → render children, drop wrapper
  (apply str (map #(render-node t % o) cs)))
```

`render-node [:plain ...]` and `render-node [:markdown ...]` follow
the same shape, target-specific. Each ~150 LOC total.

### 5.4 Per-channel rendering — MDAST equivalence table

| IR tag | HTML (Telegram) | MD (CLI/export) | Plain (TUI) | Future TG-MDv2 |
|---|---|---|---|---|
| `:doc` | concat with `\n\n` | concat with `\n\n` | concat with `\n` | concat with `\n\n` |
| `:p` | passthrough | `text\n\n` | `text\n` | `text\n\n` |
| `:h` (level 1-6) | `<b>x</b>\n` (no levels) | `# x` … `###### x` | `x\n` (uppercase optional) | `*x*\n\n` (no headers) |
| `:quote` | `<blockquote>x</blockquote>` | `> x` | `│ x` | `>x` (mdv2 line prefix) |
| `:hr` | `─────────` | `---` | `─────` | `─────────` |
| `:code` | `<pre><code class="language-X">…</code></pre>` | ` ```X\n…\n``` ` | indented 2sp | ` ```X\n…\n``` ` |
| `:ul` / `:ol` | `• x\n` / `1. x\n` | `- x` / `1. x` | `• x\n` / `1. x\n` | `\\- x` / `1\\. x` |
| `:em` / `:strong` / `:del` | `<i>` / `<b>` / `<s>` | `*x*` / `**x**` / `~~x~~` | bare text | `_x_` / `*x*` / `~x~` |
| `:c` | `<code>x</code>` | `` `x` `` | bare text | `` `x` `` |
| `:a` | `<a href>` | `[x](u)` | `x` (or `u` if `x=u`) | `[x](u)` |
| `:img` | `<i>🖼 alt</i>` | `![alt](u)` | `🖼 alt: u` | `[alt](u)` |
| `:br` | `\n` | `  \n` | `\n` | `\n` |

Renderers produce well-formed output for their target. Telegram-HTML
renderer produces only allowlisted tags (defect-#10-safe by
construction).

### 5.5 Special render contexts

- `:context :thinking` — `:quote` blocks render as
  `<blockquote expandable>` (HTML) or `▸ x` (plain) etc.
- `:context :status` — drops block elements; renders as single-line
  `<i>...</i>` (HTML) / dim text (plain).
- `:partial? true` — visitor's tail-incomplete-fenced-code drops the
  block; unclosed inline emphasis renders as literal text. Renderer
  invariant: result is well-formed for the target, never fails parse.

---

## 6. Channel renderer registry

New optional key in `vis/register-extension!`'s `:ext/channels`:

```clojure
{:ext/channels
 [{:channel/id      :telegram
   :channel/cmd     "telegram"
   :channel/main-fn #'channel-main
   :channel/messages-renderer-fn  #'render-for-telegram   ;; ← NEW
   ...}]}

(defn render-for-telegram [input opts]
  (md/render input :html opts))
```

Contract: `(fn [AnswerInput RenderOpts] -> RenderOutput)`. Output type
is channel-defined (Telegram: `String`; TUI: `String` today). Channel
calls renderer at its single emit chokepoint
(`tg/send-message!` for Telegram). Bot/screen never call renderer
manually.

---

## 7. Persistence

### 7.1 Zero schema migration

`iteration.blocks` is Nippy-encoded BLOB. Nippy roundtrips
vectors+keywords+maps+strings natively. Hiccup AST persists as-is
alongside legacy strings.

### 7.2 Read path

```clojure
(defn db-turn-answer-ast
  "Return the canonical [:doc ...] AST for a turn, normalizing legacy
   string shapes."
  [db turn-id]
  (-> (raw-answer-value db turn-id) answer-value->ast))
```

Channels and exporters call this. Legacy string answers wrap as
`[:doc s]`. Old conversations remain readable forever.

### 7.3 `conversation->markdown` exporter

Uses `db-turn-answer-ast` and renders each turn's AST through
`(md/render ast :markdown)`. **Legacy strings pass through verbatim**
(critique #10): if the underlying value is a String, the exporter
writes it unmodified instead of round-tripping through parse → render
(which would lose whitespace/indent fidelity). Vector ASTs render via
multimethod.

---

## 8. Streaming UX (Telegram only in this PR)

[unchanged from prior draft — abbreviated here]

### 8.1 Live-bubble shape

```
[💭 Thinking…
 <blockquote expandable>{last ~3500 chars of CoT, sliding window}</blockquote>
 ⏳ Running: tool-name                       ← swapped per form-start/result
 [⊘ Cancel]                                 ← inline keyboard
]
```

### 8.2 Phase routing

| `:on-chunk` phase | Effect on live bubble |
|---|---|
| `:reasoning` (delta) | Append to thinking blockquote; sliding window keeps last ~3500 chars |
| `:form-start` | Status line `⏳ Running: {tool/form-id}` |
| `:form-result` | Status line `⏳ Thinking…` |
| `:iteration-final` mid-turn | Flush; status `⏳ Iteration N+1…` |
| `:iteration-final :done?=true` | Stream end. Collapse thinking, render answer-AST, drop keyboard, send attachments. |
| `:iteration-error` | Final-edit bubble to error state |

### 8.3 Throttle

- Min interval: **1200 ms**.
- Edit when `(elapsed ≥ 1200)` AND `(Δ ≥ 40 chars OR newline)`.
- Idle 3 s resets clock; first chunk after idle fires immediately
  (critique #13).
- Stream end flushes immediately.
- HTTP 400 `not-modified` swallowed.
- HTTP 429: read `retry_after`, double next two intervals, decay.

### 8.4 Cancel button

```clojure
{:inline_keyboard
 [[{:text "⊘ Cancel" :callback_data (str "cancel:" chat-id)}]]}
```

Outcome: thinking bubble final-edited to `⊘ Cancelled by user.` +
collapsed thinking + cancel-flavored short footer; no new message.
Pre-first-paint cancel falls back to fresh `Cancelled.` message.
Spam-tap during cancel-in-flight: `answerCallbackQuery` with text
`"Already cancelling…"` (critique #12).

`/cancel` text command kept alongside.

### 8.5 Voice mode

Streams thinking bubble normally. After turn completion: existing
voice flow unchanged. Mixed-content TTS reads `[:p]` content only;
`[:vis/code]` / `[:vis/data]` / `[:vis/table]` summarized as "see
chat for code/data/table"; attachments still ship.

### 8.6 Attachments — clarified semantics (critique #3)

Multi-iteration `(answer …)` with attachments: **only the
turn-terminal `(answer …)`'s attachments ship.** Mid-turn `(answer
…)` calls (in non-final iterations) are read for prompt-flow
purposes but their attachments are not sent to the user. This
matches spec §3.2 atomicity ("answer arrives whole at form-execution
time" applies only at the terminal iteration). Attachment ordering
within the terminal answer is source-order per §4.9.

---

## 9. Implementation phases

### Phase 0+1+2+3 — atomic PR (critique #1)

The Phase-0 cosmetic regression is closed by shipping demolition +
new core + registry + Telegram wiring **together**. Sub-tasks:

1. Remove `v/` markdown DSL helpers (~38 fns) from
   `vis-foundation/.../markdown.clj` and `internal/markdown.clj`.
   Keep `conversation->markdown`.
2. Remove `convert-md-to-html` from `vis-channel-telegram/.../api.clj`.
3. Remove `:vis/answer-mode :needs-input` map handling:
   `loop.clj/needs-input-answer?`, related prompt-flow gate,
   foundation `(v/needs-input ...)`. Update tests.
4. Update 17 call-sites to inline plain markdown strings.
5. Update `prompt.clj` system-prompt section: drop v/ catalogue,
   replace with: "Answer in GitHub-flavored Markdown. For
   structured data, use Hiccup vectors via `md/data`, `md/code`,
   `md/table`, `md/file`, `md/image`."
6. Add to root `deps.edn`:
   ```clojure
   org.commonmark/commonmark                       {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-gfm-tables        {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-autolink          {:mvn/version "0.27.1"}
   org.commonmark/commonmark-ext-gfm-strikethrough {:mvn/version "0.27.1"}
   ```
7. New `internal/markdown.clj`:
   - `parse` (commonmark wrapper)
   - `->hiccup` visitor (~80 LOC)
   - `->ast` normalization (~30 LOC)
   - `render` + `render-node` multimethod (~400 LOC across three
     flavors)
   - `extract-text-blocks` for voice
8. `core.clj` re-exports + SCI sandbox bindings.
9. `extension.clj` adds `:channel/messages-renderer-fn` slot +
   validator.
10. `vis-channel-telegram/bot.clj` registers
    `:channel/messages-renderer-fn #'render-for-telegram`.
11. `tg/send-message!` looks up renderer-fn, calls it.
12. `tg/send-document!`, `tg/send-photo!` added to `api.clj`.
13. `bot.clj/handle-user-text!` reads
    `(db-turn-answer-ast (:turn-id result))`, passes AST to renderer,
    walks attachments per §8.6.
14. `loop.clj/answer-value-string` removed; replaced by
    `answer-value->ast`. `answer-str` shim does
    `(md/render ast :plain)` for code paths wanting flat string.
15. Tests for every above ns per AGENTS.md S3.

### Phase 4 — streaming + cancel button

[unchanged from prior draft]

### Phase 5 — voice integration

[unchanged]

### Phase 6 — TUI through registry (separate PR, deferred)

---

## 10. Risks & mitigations

| Risk | Mitigation |
|---|---|
| commonmark-java version drift | Pin 0.27.1 across all four artifacts. |
| LLM emits malformed `[:vis/*]` | `:default` multimethod arm renders children; missing required attrs degrade to `[:vis/data {:value <whole-block>}]` debug surface. Renderer never crashes. |
| LLM regresses without `v/` DSL guidance | New prompt is two sentences. Models default to GFM. Cheap to iterate. |
| Telegram `<blockquote expandable>` requires Bot API ≥ 7.0 | Documented requirement. Older Bot API silently shows non-expandable blockquote (full thinking visible) — degraded but functional. |
| Tables: cell formatting | `Cell = String \| Inline`. String cells parsed as inline markdown; Hiccup cells used directly. Spec-defined. |
| `:vis/image` `:url` xor `:path` | Type-level mutual exclusion; `->ast` validates and degrades to `[:vis/data]` debug if both/neither. |
| Multi-iteration `(answer)` attachments | Only terminal-iteration attachments ship (§8.6). |
| Streaming throttle race during cancel | `answerCallbackQuery "Already cancelling…"` on duplicate taps. |
| commonmark-java SCI compat | Library is host-Java-only. Renderer is pure Clojure. SCI smoke test in Phase 1. LLM-side `md/render` calls work because they execute in host context where Java is available. |
| Persistence size growth | Nippy compact; ~2× string-equivalent size. KB-scale. |
| Existing extensions wrapping `(:answer result)` as String break | `answer-str` shim returns rendered-plain string for back-compat. |
| Block taxonomy fan-out | v1 = ~5 `:vis/*` tags + standard MD. Each new tag → design discussion. `:vis/*` fallthrough means unknown tags don't break old channels. |
| Parse perf in streaming hot path (critique #4) | Phase 1 includes regression test asserting `(parse 4KB)` < 50 ms on CI hardware. |
| Long `:vis/data` truncation hides info (critique #2) | Truncation footer: `…(truncated; N chars total)`. User can re-ask; full data is in DB. |

---

## 11. Out of scope

- Per-token answer-prose streaming.
- Streaming voice synth.
- Telegram MarkdownV2 parse-mode (HTML primary; MdV2 is one renderer
  arm + parse_mode flip later — IR-compatible).
- Reasoning-as-permanent-transcript.
- TUI styled-segment renderer.
- DB schema changes (zero — Nippy already roundtrips).
- Configuration UX for throttle constants.
- Image generation / chart rendering.
- Layout primitives.
- Grammar-constrained LLM decoding.
- Domain-ontology validation (`:schema`, `:cite`, `:conf`,
  `:emotion`, `:mode` from the IR-design literature). Extension slot
  exists; v1 ships no validators.
- `needs-input` semantic (removed entirely).

---

## 12. Open items

None at spec time. Implementation proceeds Phase 0+1+2+3 atomically;
Phase 4 then 5; Phase 6 explicit follow-up.
