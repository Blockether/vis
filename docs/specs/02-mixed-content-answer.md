# Specification — PR #2: Mixed-Content Answer Container

Status: design-locked, pre-implementation. Depends on PR #1 (`/Users/fierycod/SPECIFICATION.md`) shipping first.
Companion: PR #1 ships streaming + Hiccup-internal-AST + markdown rendering chokepoint. This PR widens `(vis/answer ...)` from "string only" to "Hiccup-shaped mixed content blocks", end-to-end through every channel.

---

## 1. Problem statement

`(vis/answer x)` accepts any value, but `loop.clj/answer-value-string` flattens everything that isn't `:vis/answer-mode :needs-input` map to a string via `(str v)`. In practice the LLM is forced to produce one **string** of markdown for the answer, manually pre-stringifying any structured data:

```clojure
(answer (str "Here's the data:\n```\n" (with-out-str (pp/pprint result)) "\n```\nDone."))
```

Consequences:

- **Channel-blind rendering.** LLM has to guess what looks good across Telegram + TUI + CLI. Telegram wants `<pre>`, TUI wants a tree-collapse widget, CLI wants raw pprint. One string can't serve all.
- **Loss of structure on transit.** Tool-call results that ARE structured Clojure data get re-flattened at the `(answer ...)` boundary. Channels can never recover the structure to render it natively.
- **Token waste.** LLM emits `with-out-str` + `pp/pprint` ceremony in every answer that surfaces data. Repeated boilerplate across thousands of turns.
- **Image / file / attachment cases impossible today.** Telegram supports voice notes, images, documents — none reachable from `(answer ...)` because the contract is "string".
- **Defect-#10 cascade risk.** Mashing structured data into a markdown string introduces nested code-fence collisions (LangChain bug #2241), backtick-in-data parser breakage (LangChain bug #9535), Llama-32 markdown-wrapped-JSON failures (60% turn loss in some setups). All known failure modes documented in industry.

Industry consensus (Anthropic Claude API, OpenAI Responses API, LangChain 1.0 `content_blocks`, AgentsKit, Callsphere agent-formatting guide): **answer is an ordered list of typed content blocks, not a single string.** Vis is the holdout.

## 2. Goals / non-goals

### Goals

- `(vis/answer ...)` accepts mixed content: prose strings + structured Hiccup blocks, in any order, in any of four equivalent surface forms.
- 100 % backward compatible: every existing call-site, persisted row, test fixture, doc example continues to work unchanged.
- Channel-aware rendering: each channel renders each block type optimally for its medium (Telegram-HTML, TUI styled, CLI pprint).
- Lossless persistence: the original block list survives a write→read roundtrip and can be re-rendered against any channel.
- Streamable: blocks integrate with PR #1's streaming flow; no architectural conflict.
- Extensible: adding a new block type touches one place (renderer multimethod arm) per channel; no AST refactor.

### Non-goals

- Re-defining the LLM's transport-layer content blocks (Anthropic/OpenAI internal). We define **agent-presentation** blocks; the LLM emits Clojure code that produces them.
- Image generation pipelines. `[:image {...}]` carries a reference; producing the image is an extension's job (e.g., a future `vis-image` extension).
- Backporting blocks to channels that don't register a renderer-fn. Default fallback = render as plain markdown.
- Per-block streaming inside one block. The unit of streaming stays "the whole answer arrives atomically per `(answer ...)` form execution" (matching PR #1 §3.2 architectural reality). Live thinking/status is a channel UI concern, not block streaming.
- Rich layout primitives (rows, columns, grids). The block list is a flat-or-nested tree, not a layout engine.

---

## 3. Public API

### 3.1 Surface forms — all four equivalent

```clojure
;; (1) String — back-compat, sugar for [:text "..."]
(vis/answer "just prose")

;; (2) Variadic mixed
(vis/answer "prose" data "more prose")
;;   each non-string arg auto-wraps in [:data {:value arg}] unless already a Hiccup block

;; (3) Explicit Hiccup blocks, variadic
(vis/answer [:text "prose"] [:data {:value x}] [:text "more"])

;; (4) Single Hiccup vector with [:answer ...] root
(vis/answer [:answer "prose" [:data {:value x}] "more"])
```

Normalization (§4) reduces all four to the same canonical AST:

```clojure
[:answer
 [:text "prose"]
 [:data {:value x}]
 [:text "more"]]
```

### 3.2 Block taxonomy (v1)

| Tag | Attrs | Children | Semantics |
|---|---|---|---|
| `[:answer ...]` | none | block list | Root container. Emitted by normalization; the LLM may also use it explicitly. |
| `[:text "..."]` | none | one string | Markdown text. Parsed by `md/parse` at render time. |
| `[:code {:lang STR}? "..."]` | `:lang` (optional) | one string | Code block. Channel renders syntax-highlighted if it can. Equivalent to a markdown fenced block with language tag, but explicit. |
| `[:data {:value V}]` | `:value` (any Clojure value) | none | Structured Clojure data. Channel pretty-prints per medium. |
| `[:table {:rows [[...]] :headers [STR]?}]` | `:rows`, `:headers?` | none | Tabular data. Channel renders as native table where supported. |
| `[:file {:path STR :title STR?}]` | `:path`, `:title?` | none | File reference. Channel may attach (Telegram), link (CLI), or open-in-pane (TUI). |
| `[:image {:url STR? :path STR? :alt STR?}]` | `:url` xor `:path`, `:alt?` | none | Image reference. Channel attaches if it can; falls back to markdown link. |
| `[:* ...]` | any | block list | **Fallthrough.** Unknown tag → renderer drops the tag and renders children. Forgiving by design. |

**v1 deliberately omits:** `:audio`, `:video`, `:card`, `:actions`, `:chart`, `:embed`. Each is a follow-up block type added when a real consumer needs it. v1 covers the cases identified as bleeding today: prose, code, data, tables, files, images.

### 3.3 LLM-facing helpers (SCI sandbox bindings)

PR #1 already exposes `md/parse`, `md/render`, etc. PR #2 adds construction helpers so the LLM-emitted code is concise:

```clojure
(md/text "string")                ;; ⇒ [:text "string"]
(md/code "(+ 1 1)" :clojure)      ;; ⇒ [:code {:lang "clojure"} "(+ 1 1)"]
(md/data x)                       ;; ⇒ [:data {:value x}]
(md/table rows {:headers ["A"]})  ;; ⇒ [:table {:rows rows :headers ["A"]}]
(md/file "/tmp/x.txt" "Output")   ;; ⇒ [:file {:path "/tmp/x.txt" :title "Output"}]
(md/image {:url "..." :alt "..."}) ;; ⇒ [:image {:url "..." :alt "..."}]
```

Helpers are **sugar only**. The LLM may emit raw Hiccup vectors directly. The helpers exist for prompt-side ergonomics ("call `md/data` to embed Clojure data, channel pretty-prints").

### 3.4 What `(vis/answer ...)` returns

Identical to today: writes the *normalized AST* to `answer-atom`, returns nil. The form-execution loop reads the atom at form-result time; the renderer reads it from persistence at display time.

### 3.5 Co-existence with `:vis/answer-mode :needs-input`

The needs-input map (`{:vis/answer-mode :needs-input :answer/text "..."}`) keeps its current shape. Internal normalization treats it as a single-block AST `[:answer [:needs-input {:text "..."}]]` for renderers, but the loop keeps reading `:answer/text` directly for prompt-flow gating. Two surfaces, one underlying representation.

---

## 4. Normalization (`answer-value-string` rewrite)

Replace `loop.clj/answer-value-string` with `answer-value->ast`. New contract:

```clojure
(defn answer-value->ast
  "Normalize the value passed to (vis/answer ...) into the canonical
   [:answer ...] AST. Pure. Total. Never throws on shape; unknown
   shapes wrap as [:data {:value v}]."
  [v]
  ...)
```

### 4.1 Normalization rules (in order)

1. **Already canonical:** `[:answer ...]` with valid block children → return as-is after recursive child-normalization.
2. **Needs-input map:** `{:vis/answer-mode :needs-input :answer/text s}` → `[:answer [:needs-input {:text s}]]`. Other map keys passed through to `:needs-input` attrs.
3. **String:** `s` → `[:answer [:text s]]`.
4. **Hiccup block (single):** `[:tag ...]` where `tag` is in known set or starts with `:vis/` → `[:answer node]`.
5. **Vector of mixed:** `[a b c ...]` (not Hiccup, i.e., first elem is not a tag keyword) → recursively normalize each element, wrap in `[:answer ...]`.
6. **Multiple args (variadic case):** `(answer x y z)` → loop coerces to `[x y z]` → rule 5.
7. **Anything else:** `v` → `[:answer [:data {:value v}]]`.

### 4.2 Adjacent text-block coalescing

`[:answer [:text "a"] [:text "b"]]` → `[:answer [:text "a\n\nb"]]`. Two adjacent text blocks merge with a paragraph break. Rationale: lets the LLM emit `(answer "intro" data "outro")` and get one prose flow on either side without double-rendering markdown ASTs.

### 4.3 Validation

Validation is **lenient** — the renderer must not crash on malformed input. Validation policy:

- Unknown tag → renders via `[:*]` fallthrough (children only).
- Missing required attr → block renders as `[:data {:value <whole-block>}]` (debug-friendly: user sees what came through).
- Wrong attr type → same, plus a single-line warning logged via `tel/log!`.

No throws from the answer pipeline. **Defect-#10 invariant extends: malformed structured input degrades to literal data display, never crashes the channel.**

---

## 5. Internal AST shape

Every block is a Clojure vector: `[tag-keyword attrs-map? & children]`. Attrs map is optional (Hiccup convention). Children are nested blocks or strings.

Standard predicates (live in `markdown.clj`):

```clojure
(defn block? [x])
(defn tag-of [block])
(defn attrs-of [block])    ;; returns {} when no attrs map
(defn children-of [block]) ;; returns [] when no children
```

The internal AST is identical to PR #1's Hiccup AST — no separate "answer AST" vs "markdown AST". `[:text "..."]` children produced by `md/parse` are the same Hiccup nodes as `[:p ...]`, `[:ul ...]`, `[:b ...]` etc. Rendering walks one tree.

---

## 6. Persistence

### 6.1 No schema migration needed

`iteration.blocks` is already a **Nippy-encoded BLOB** holding the per-form result values (V1 schema, line ~241). `(vis/answer x)` writes `x` into the answer-atom; the loop persists the form result as part of the iteration row. Nippy handles vectors+keywords+maps+strings natively, so a Hiccup AST roundtrips losslessly with **zero schema change**.

Existing string answers persist as strings → on read, normalization wraps as `[:answer [:text "..."]]`. Existing needs-input maps persist as maps → normalization wraps as `[:answer [:needs-input {:text "..."}]]`. Rolled-out conversations stay readable forever.

### 6.2 Read path

`persistance/db-list-conversation-turns` and friends already return the iteration blocks. New helper:

```clojure
(defn db-turn-answer-ast
  "Return the canonical answer AST for a turn, normalizing legacy
   string/needs-input shapes to [:answer ...] form. Pure projection
   over the iteration row's stored blocks."
  [db turn-id]
  ...)
```

Channels and exporters call this instead of reading raw answer-text.

### 6.3 `conversation->markdown` exporter

`markdown.clj/conversation->markdown` (the existing exporter — distinct from the new render pipeline) starts using `db-turn-answer-ast` and renders each turn's AST through `(md/render ast :markdown)` (the new flavor). String answers continue to render verbatim because their AST is `[:answer [:text "..."]]` and rendering `[:text]` to `:markdown` flavor is identity.

---

## 7. Channel renderer contract

### 7.1 `:channel/messages-renderer-fn` signature change

PR #1 defined the slot as `(fn [text opts] → output)`. PR #2 widens the input:

```clojure
;; PR #1:  (fn [text opts] → output)             text  : String (markdown)
;; PR #2:  (fn [ast-or-text opts] → output)      input : String OR Hiccup block / [:answer ...] AST
```

Backward compat: a string input is auto-promoted via `answer-value->ast` before dispatch. Existing channels that registered a string-only renderer keep working — they just receive an AST they can pass to `md/render` first.

Recommended idiom for channels:

```clojure
(defn render-for-telegram [input opts]
  (md/render (md/->ast input) :html opts))
;; md/->ast: identity on AST, normalize on string/value
```

### 7.2 Per-block rendering — Telegram (`:html` flavor)

| Block | Telegram output |
|---|---|
| `[:answer & children]` | concat children renders, separated by `\n\n` between top-level blocks |
| `[:text s]` | `(md/render s :html opts)` (PR #1 markdown→HTML) |
| `[:code {:lang L} s]` | `<pre><code class="language-L">{escape s}</code></pre>` (no lang → `<pre>{escape s}</pre>`) |
| `[:data {:value v}]` | `<pre><code class="language-clojure">{escape (with-out-str (pp/pprint v))}</code></pre>`. Long values truncated with `…(truncated, see DB)` at ~3500 chars. |
| `[:table {:rows :headers}]` | aligned monospace text inside `<pre>` (reuses PR #1 table renderer) |
| `[:file {:path :title}]` | **side effect:** schedule a `sendDocument` after the text bubble; in the text bubble, render `<i>📎 {title or basename}</i>`. See §7.5 for attachment ordering. |
| `[:image {:url|:path :alt}]` | **side effect:** schedule a `sendPhoto` after the text bubble; in the text bubble, render `<i>🖼 {alt or basename}</i>`. |
| `[:needs-input {:text}]` | `(md/render text :html opts)` — needs-input renders as plain text answer; the prompt-flow side reads `:answer/text` directly. |
| `[:* & children]` | concat children renders, no wrapper |
| Any unknown tag | treat as `[:*]` |

### 7.3 Per-block rendering — TUI (`:plain` flavor in v1; `:tui-styled` later)

| Block | TUI output (v1, plain) |
|---|---|
| `[:answer & children]` | concat with double newline between top-level |
| `[:text s]` | `(md/render s :plain opts)` (text with soft formatting) |
| `[:code {:lang} s]` | indented 2 spaces, optionally a `// lang` header |
| `[:data {:value v}]` | `(with-out-str (pp/pprint v))` indented 2 spaces |
| `[:table ...]` | aligned monospace (current behavior, kept) |
| `[:file ...]` | `📎 {path}` link line (clickable in modern terminals via OSC-8 escape — future) |
| `[:image ...]` | `🖼 {alt}: {url|path}` text line |
| Other | as Telegram, minus HTML escaping |

### 7.4 Per-block rendering — CLI (`vis run`, default `:markdown` flavor)

CLI dumps the raw markdown rendering (channel-agnostic). `[:data]` becomes a fenced ` ```clojure ` block; `[:image]` becomes a `[alt](url)` markdown image; `[:file]` becomes a `[title](file://path)` link. Trivial, and the existing `vis run "..."` UX is preserved.

### 7.5 Telegram attachment ordering

Mixed-content with attachments needs a deterministic flow:

1. Render the entire AST as **one HTML payload** for the text bubble, with placeholder italic lines for each `[:file]` / `[:image]` block.
2. Send the text bubble first.
3. Walk the AST again and `sendDocument` / `sendPhoto` for each attachment block, in source order, as **reply-to** the text bubble (`reply_to_message_id`). This anchors attachments to the answer visually.
4. If only attachments and no text, send attachments without a text bubble.

Voice mode (`(answer ...)` with audio output enabled): same as today — final `[:text]` content is synth-to-speech; attachments still send via §7.5 (3); `[:data]` / `[:code]` are summarized in TTS as "code block" / "data structure" markers, not read aloud verbatim. (Behavior knob — defer; v1 ignores attachments + data in TTS, only reads `[:text]` blocks.)

---

## 8. Streaming integration with PR #1

PR #1's live thinking bubble + answer bubble flow doesn't change shape:

- **Thinking phase**: unchanged. Streams reasoning into the live bubble.
- **Answer phase**: when the turn completes and `(:answer result)` is read, it is now an AST (post-normalization) instead of a string. The Telegram channel calls its renderer-fn on the AST → produces the HTML for the answer bubble(s). Multi-bubble overflow (B3) still applies on the rendered string length.
- **Per-block streaming inside an answer**: explicitly **out of scope** (§2 non-goals). The LLM emits `(answer ...)` once at form-execution time; the AST arrives whole. Streaming applies to the *thinking* phase only, as in PR #1.

No changes to `make-progress-tracker`, no new phases on `:on-chunk`.

---

## 9. Implementation plan

### 9.1 Track A — core (`src/com/blockether/vis/internal/`)

1. **`markdown.clj`**: add `block?`, `tag-of`, `attrs-of`, `children-of`, `->ast` (normalize-or-passthrough). Extend `render`/renderers to dispatch on AST root tag (already does for inline tags from PR #1). Add per-block multimethod arms for `:answer`, `:text`, `:code`, `:data`, `:table`, `:file`, `:image`, `:needs-input`, `:*`.
2. **`loop.clj`**: replace `answer-value-string` with `answer-value->ast`. Update every call site (`answer-str`, persistence write, transcript projection). Keep an `answer-value->display-string` shim that does `(md/render ast :plain)` for code paths that still want a flat string (e.g., logs).
3. **`core.clj`**: re-export `md-text`, `md-code`, `md-data`, `md-table`, `md-file`, `md-image` helpers; expose them in SCI sandbox.
4. **`extension.clj`**: update `:channel/messages-renderer-fn` spec docstring to reflect "string OR AST" input.
5. **`progress.clj`**: no change (streaming phases unchanged).

### 9.2 Track B — Telegram (`extensions/channels/vis-channel-telegram/`)

1. **`bot.clj` `render-for-telegram`**: switch input from string to AST-or-string; add the attachment-walk pass (§7.5).
2. **`api.clj`**: add `send-document!`, `send-photo!` (multipart, similar to existing `send-voice!`).
3. **`bot.clj` `handle-user-text!`**: after the answer arrives, call `render-for-telegram` on the AST; send text bubble; then walk AST attachments and send each as reply-to-text-bubble.
4. **Voice mode**: when `voice-output-mode?`, extract concatenated `[:text]` content via `(md/extract-text-blocks ast)`, synth that, send voice note. Attachments still ship per §7.5 (3).

### 9.3 Track C — TUI (`extensions/channels/vis-channel-tui/`)

1. **`screen.clj`**: switch render boundary to call the registered renderer-fn with the AST. Initially register `(fn [ast opts] (md/render ast :plain opts))`. Future flavor `:tui-styled` is a separate PR.
2. **No layout/widget changes in v1.** Just text rendering through the new pipeline.

### 9.4 Track D — exporter & CLI

1. **`markdown.clj/conversation->markdown`**: render each turn's AST via `(md/render ast :markdown)` instead of stringifying answer text. String-only answers still render verbatim (rule §6.3).
2. **`main.clj/cli-run!`**: render answer AST via `(md/render ast :markdown)` for default output; `--json` and `--edn` flags now serialize the AST natively (was stringified). Add a brief flag `--render text|html|markdown` only if there's demand — defer per PR #1 conclusion.
3. **`transcript.clj`** and any other AST consumer: switch to `db-turn-answer-ast`.

### 9.5 Track E — tests

Per AGENTS.md S3, every namespace touched needs matching test coverage:

- **`markdown_test.clj`**: AST normalization (all four surface forms → identical canonical AST); per-block rendering for `:html`, `:plain`, `:markdown` flavors; adjacency coalescing; unknown-tag fallthrough; malformed-input degradation.
- **`loop_test.clj`**: `answer-value->ast` covers strings, needs-input maps, single Hiccup blocks, vectors of mixed, variadic args, deeply nested blocks, anything-else fallback.
- **`bot_test.clj`** (Telegram): renderer dispatch, attachment ordering, voice-mode text extraction, mixed text+image+data answer ships text bubble first then attachments as replies.
- **`screen_test.clj`** (TUI): renderer dispatch through registered fn.
- **`conversation->markdown_test.clj`**: existing string answers render unchanged; new block answers render to clean MD.

---

## 10. Migration & rollout

### 10.1 Code

- PR #1 lands first. PR #2 begins with the channel renderer-fn signature already widened.
- Land in two commits within PR #2: (i) core normalization + AST renderers; (ii) channels switching their renderer-fn input.

### 10.2 Data

- **Zero migration.** Existing turn rows have string-shaped Nippy values in `iteration.blocks`. Read-path normalization promotes them to AST on demand. Old conversations remain readable forever.
- New writes after PR #2 deploy carry vector-shaped values when the LLM uses mixed-content. Single-string answers still write strings (smaller, no need to wrap).

### 10.3 LLM prompt update

Vis's system prompt will gain a short section:

> When your answer mixes prose and structured data, prefer:
> - `(md/text "...")` for prose
> - `(md/code source :lang)` for code blocks
> - `(md/data value)` for Clojure data the user should see structured
> - `(md/table rows {:headers [...]})` for tabular data
> - `(md/file path)` / `(md/image {...})` for attachments
> Compose with `(answer block-1 block-2 ...)`. Pure-prose answers stay `(answer "string")` — no need to wrap.

This is the LLM-side ergonomic story. Channels handle the rest.

---

## 11. Risks & mitigations

| Risk | Mitigation |
|---|---|
| LLM emits malformed Hiccup (e.g., `[:unknown-tag ...]`, missing required attrs) | `[:*]` fallthrough + lenient validation (§4.3). Defect-#10 invariant extends: never crash, always render *something* sensible. |
| Channels silently drop attachments because they don't know `[:file]` / `[:image]` | Renderer fallback: unknown blocks render as italic placeholder text. Worst case: user sees `_📎 path/to/file.txt_` and can fetch it manually. |
| Streaming throttle conflict with attachments | Attachments are sent **after** the text bubble's final state — outside the streaming loop entirely. No throttle interaction. |
| Persistence size growth from Nippy-encoded vectors vs strings | Nippy is compact; a 5-block answer is ~2× the bytes of the equivalent stringified answer. Acceptable given typical answer sizes (KB, not MB). |
| Existing extensions that wrap `(:answer result)` as a string break | `answer-value->display-string` shim returns the rendered-plain string for back-compat. Affected callers: `vis/format-error`, `tg/answer-details-message`, voice synth. All updated in PR #2. |
| LLM regresses to string-only answers despite prompt | Acceptable. Mixed content is opt-in. The renderer handles both. No quality regression from the LLM ignoring the new helpers. |
| Renderer multimethod fan-out grows unbounded as block types accumulate | Block taxonomy is intentionally minimal (v1 = 7 tags). Each new tag requires a design discussion + PR. `[:*]` fallthrough means unknown tags don't break old channels. |
| `iteration.blocks` Nippy schema change in future Vis core breaks reads | Out of scope for PR #2 — same risk exists today for any persisted form result. Persistence team owns the Nippy version story. |

---

## 12. Open items

1. **Voice TTS over mixed blocks.** Today voice = full answer string. With blocks: which blocks read aloud? Recommendation in §7.5: `[:text]` only; `[:code]` / `[:data]` summarized as "see the chat for the code block" or skipped. Needs UX call.
2. **Attachment captions.** `sendPhoto`/`sendDocument` accept a caption with HTML. Should the rendered text bubble's content also become the photo's caption when text + single image? Saves a message. Defer to v1.1.
3. **Telegram inline keyboard on attachments.** Could ship `[:cancel]` button on the text bubble (PR #1 design). Streamed bubble already has it. Final answer bubble has none in PR #1 — should it? Likely yes for "view more / open in TUI" buttons later. Out of scope for v1.
4. **`[:needs-input]` vs `:vis/answer-mode :needs-input` map.** Can the LLM emit `[:answer [:needs-input {:text "..."}]]` directly? Yes — equivalent. The map shape stays for back-compat but the block form is now the recommended way. Foundation extension's `(v/needs-input ...)` helper updated to emit the block form.
5. **Streaming preview of forming AST?** Today the answer arrives whole at form-result time. A future AST-aware streaming where the LLM's tool-output forms can each push a block to the live bubble (`[:tool-progress ...]`) would extend PR #1's thinking bubble. **Out of scope; design deferred.**

---

## 13. What this delivers

After PR #2, this works:

```clojure
;; LLM emits:
(answer
  "Here are the matching files:"
  (md/table
    {:rows    (mapv (juxt :path :size :modified) results)
     :headers ["Path" "Size" "Modified"]})
  "Inspecting the largest one:"
  (md/code (slurp largest-path) :clojure)
  (md/file largest-path "Source")
  "Looks like a candidate for refactoring.")
```

Telegram sees: text bubble with markdown-rendered intro + aligned `<pre>` table + code block + italic `📎 Source` placeholder + closing prose, followed by a `sendDocument` for the file as reply-to-bubble.

TUI sees: scrollback text with monospace table + code block + clickable file link.

CLI sees: clean markdown, ready to pipe.

DB sees: a single Nippy-encoded vector. Re-render any conversation against any future channel without re-running the LLM. **One source of truth, N channel projections.**
