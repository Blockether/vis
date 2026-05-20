# Vis answer pipeline rewrite

## Mission

1. **Etap 1 — Markdown is the answer.**
   `(done {:answer "markdown"})`. DB stores Markdown. IR is derived render cache only.

2. **Etap 2 — Iteration display collapse.**
   Five parallel arrays (`:render-segments`, `:results`, `:result-kinds`, `:result-details`, `:code`)
   become one per-form vector of maps with a single derivation function. Primitives stay in
   persistence (`:code`, `:result`, `:error`); the display projection is computed at render time
   and cached by content hash.

No backwards compatibility. No migration. Old database discarded. Old `[:ir ...]` API removed
from prompt and `done`. Both steps must end on a green `./verify.sh`.

## Rationale

### Why Markdown as answer truth

Model-authored `[:ir ...]` is brittle: one missing attrs map, one misnested block, and the
final answer renders as an EDN code block. Models write Markdown natively. Markdown survives
copy/export/transcript without round-tripping. CommonMark is a single well-defined parser.
Treating Markdown as source means:

- prompt collapses from a 20-line tag/attrs/canonical-form section to three lines
- final answers stop turning into EDN artifacts
- DB is human-readable
- exports/transcripts are byte-identical to what the model wrote
- IR stays useful as a render AST — but as derivation, not source

### Why IR stays inside extensions

`:render-fn` on extension symbols returns IR. That stays. Reasons:

- programmatic compose via `combine-render-values` / `default-error-ir`
- per-channel routing without round-tripping through Markdown
- structural introspection from channels (badges, role colors, kbd, mark)
- extension authors are power users; Markdown is the wrong contract for them

The model never sees IR. Extensions still produce it.

### Why iteration display arrays collapse

Per-iteration we currently carry five parallel arrays indexed by form position:

| field              | role                              | derivation                            |
| ------------------ | --------------------------------- | ------------------------------------- |
| `:code`            | raw source per form               | primitive                              |
| `:render-segments` | source classification             | `(parse-block-display code)`           |
| `:results`         | pre-rendered tool result IR       | `(render-tool-result result)`          |
| `:result-kinds`    | result style tag                  | `(classify result error)`              |
| `:result-details`  | tool result metadata              | `(extract-metadata result)`            |

Real primitives in persistence: `:code`, `:result`, `:error`. The other four are pure
derivations cached at the message-map level. They are not duplicates but the cache is leaking
into the data model. One per-form map plus one derivation function is enough.

## Etap 1 — Markdown answer

### Model contract

Canonical:

```clojure
(done {:answer "Summary\n\n- item one\n- item two"})
```

No string sugar. The map shape leaves room for future metadata
(`:format`, `:lang`, `:tags`, `:attachments`, ...). No vector form. No IR.

Needs-input stays a separate predicate path:

```clojure
{:vis/answer-mode :needs-input
 :answer/text "Question for the user"}
```

### Prompt

`src/com/blockether/vis/internal/prompt.clj` `CORE_SYSTEM_PROMPT`:

- delete the whole `ANSWER_IR` block (tag taxonomy, attrs rules, canonical-form
  invariants, soft-break collapsing).
- delete every example showing `(done [:ir ...])`.
- replace with a short Markdown contract:

  ```text
  ANSWER
  Emit the final answer with (done {:answer "..."}). The string is GitHub-flavored
  Markdown: headings, lists, code fences, tables, links, **bold**, *italic*,
  `inline code`. No Hiccup, no EDN trees, no [:ir ...]. Tool results stay
  destructurable Clojure data; the answer is a human-facing Markdown summary.
  ```

### Loop / done

`src/com/blockether/vis/internal/loop.clj`:

- `answer-fn` (the SCI `done` binding) must:
  - accept `{:answer string}` only — reject every other shape with a clear error
  - accept the needs-input map shape unchanged
  - store the raw Markdown string on `answer-atom` — no `(render/->ast s)`
- delete `append-runtime-appendices` IR canonicalization for answers; keep it only
  if it is still needed for needs-input maps, otherwise inline.
- `run-turn!` must persist the raw Markdown:
  - replace `:answer (answer-str a)` with `:answer-markdown (answer-markdown a)`
- `answer-str` keeps a plain-text helper, but operates on Markdown via
  `(render/extract-text (render/markdown->ir s))` — no IR canonicalization of the
  source value.
- every error/cancel fallback that put a stringified IR into the answer slot
  now writes a plain Markdown string.

### Render namespace

`src/com/blockether/vis/internal/render.clj`:

- public API gains an unambiguous Markdown entry point:

  ```clojure
  (render/markdown->ir markdown-string) ; CommonMark parse, canonical IR
  ```

  Implemented by renaming today's `text->ir` (which already does this).
- `->ast` stays internal for literal/data fallback used inside `:render-fn`
  pipelines. It is no longer invoked on final answers.
- `(render value :markdown)` on the answer path receives the raw Markdown
  string and returns it unchanged (no IR round-trip).
- `search-text` indexes via `markdown->ir` -> plain-text walker; one entry point.

### DB schema

`extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`:

- replace

  ```sql
  answer BLOB        -- Nippy-frozen IR
  ```

  with

  ```sql
  answer_markdown TEXT
  ```

- no cache columns yet; render cache lives in memory only.
- discard existing local DB; no migration path.

### Persistence implementation

`extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`:

- `db-update-session-turn!` writes `:answer_markdown` (TEXT), not `(->blob answer)`.
- `row->turn` returns `:answer-markdown` directly. No legacy `:answer` IR field.
- `db-turn-history` returns `:answer-markdown`.
- `reindex-search!` indexes the Markdown string through `vis/search-text`
  (which already lifts strings via `markdown->ir`).

### TUI

`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`
and the rest of the TUI channel:

- assistant message shape becomes:

  ```clojure
  {:role :assistant
   :text answer-markdown            ; raw source — copy/export uses this
   :ir   (vis/markdown->ir answer-markdown) ; derived for layout
   :timestamp ts}
  ```

- live progress assistant message: same shape, IR derived once.
- resume rebuild: read `:answer-markdown`, derive IR.
- copy / clipboard / export prefer `:text` (raw Markdown), not a re-rendered
  IR projection.
- `vis/render :markdown` on the answer path simply returns the source.

### Telegram channel

`extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/bot.clj`:

- answer path: `markdown -> markdown->ir -> telegram render`.
- never send raw model Markdown to Telegram without target-specific escaping.

### Voice

`extensions/common/vis-voice/src/com/blockether/vis/ext/voice/core.clj`:

- answer path: `markdown -> markdown->ir -> extract-text -> TTS`.

### Transcript / export

`extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/transcript.clj`
and `render/session->markdown`:

- assistant turns emit `:answer-markdown` verbatim.
- no IR -> Markdown round-trip on the answer.

### Tests

- prompt must not contain `(done [:ir`.
- `(done {:answer "**bold**"})` round-trips: stored exactly, derived IR renders
  bold in TUI.
- code fences, tables, lists, links survive DB round-trip exactly.
- raw HTML in answers is rendered visibly, never executed.
- resume reconstructs the assistant bubble from raw Markdown.
- export/copy emits raw Markdown.
- FTS finds plain text inside Markdown answers.
- tool results remain destructurable maps.
- needs-input answers still work.

### Definition of done — Etap 1

- `./verify.sh` green
- prompt contains zero `[:ir`
- DB column is `answer_markdown TEXT`
- TUI / Telegram / voice / transcript render assistant answers from Markdown source
- single commit landing the cut

## Etap 2 — Iteration display collapse

### Target shape

Per-iteration timeline entry today (parallel arrays):

```clojure
{:iteration N
 :code             ["(def x 1)" "(done {:answer \"..\"})"]
 :comments         [nil nil]
 :render-segments  [[{:kind :code :source "(def x 1)"}] [{:kind :answer-ref}]]
 :results          ["1" nil]
 :result-kinds     [:value :value]
 :result-details   [nil nil]
 :errors           [nil nil]
 :durations        [12 1]
 :successes        [true true]
 :started-at-ms    [... ...]
 :silents          [false true]
 ...}
```

Target:

```clojure
{:iteration N
 :forms [{:position 0
          :code "(def x 1)"
          :comment nil
          :result raw-value-or-tool-result
          :error nil
          :duration-ms 12
          :started-at-ms ...
          :success? true
          :silent? false}
         {:position 1
          :code "(done {:answer \"..\"})"
          :result :vis/answer
          :silent? true
          ...}]
 :thinking ...
 :error nil
 :final nil
 :done? false
 ...}
```

Renderable view derived at consumer:

```clojure
(defn form-display [{:keys [code result error] :as form}]
  (assoc form
    :source-segments (parse-block-display code)
    :result-render   (render-result result error)
    :result-kind     (classify-kind result error)
    :result-detail   (extract-metadata result)))
```

Cached by content hash `[code result error]` -> render. Live progress and resume share the
same projection. Fingerprint becomes `:forms` content hash, not five separate vectors.

### Code changes

- `src/com/blockether/vis/internal/progress.clj`
  - replace parallel-array `pad-to` / `assoc` / `drop-slot` / `insert-slot` machinery
    with `:forms` vector ops indexed by `:position`
  - emit chunks shaped per-form, not per-axis
  - `hide-form-slot` / `elide-form-slots` operate on `:forms`
  - keep `:thinking`, `:error`, `:final`, `:done?`, `:provider-fallbacks`, `:activity`,
    `:recaps`, `:elided-form-idxs` at top level (truly iteration-scoped)

- `src/com/blockether/vis/internal/loop.clj`
  - `on-chunk :form-start` / `:form-result` carry one form map, no parallel axes
  - block validation / persistence still uses `:code` / `:result` / `:error` / `:render-segments`
    on the persisted iteration block (DB layer untouched in this Etap)
  - the display side only uses `:forms`

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`
  - history rebuild produces `:forms` directly
  - `result-kind`, `tool-result-detail`, `result-strs`, `result-details` local fns
    become `form-display` helper invoked per form (single function, single test)

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`
  - `format-iteration-entry-entries` consumes `:forms`
  - `iteration-fingerprint` keys off `:forms` content + iteration-level scalars
  - `code-source-from-render-segments` reads `:source-segments` from the form display

- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/bot.clj`
  - live-bubble formatter consumes `:forms`

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/transcript.clj`
  - `render-block-code-segments` reads `:source-segments` from the form display

### Tests

- progress tracker emits `:forms`, all per-axis tests rewritten.
- TUI render snapshot tests rewritten against `:forms`.
- Telegram live-bubble test rewritten.
- new tests for `form-display` purity and cache-key shape.

### Definition of done — Etap 2

- `./verify.sh` green
- progress chunk shape is `:forms`-based
- consumers no longer reference `:render-segments`, `:results`, `:result-kinds`,
  `:result-details` at top level of iteration entries
- one derivation function (`form-display`) shared across live and resume paths
- single commit landing the cut

## Order of operations

1. Etap 1 first. Smaller blast radius, immediate prompt simplification, decouples
   final-answer truth from iteration display.
2. Etap 2 second, on top of a green Etap 1.

No half-merged middle states. Each etap = one commit, green `verify.sh` before commit.

## Out of scope

- migrating existing user DBs
- preserving `(done [:ir ...])`
- preserving `(done "string")` sugar
- markdown tool results
- render cache columns in SQLite
- promoting Markdown as the contract for `:render-fn`

## Non-goals

- backwards compatibility of any kind
- partial rollouts
- supporting both shapes during transition

## Risks

- string ambiguity (`String` = Markdown source / literal text / Clojure source) — mitigated
  by naming the entry points: `markdown->ir`, `literal->ir`, `extract-text`, `search-text`.
- Telegram/Web escaping — channel-specific escape stays mandatory after parsing.
- raw HTML in Markdown — rendered as visible text, never executed.
- terminal control sequences in answers — stripped at the TUI boundary.

## Final principle

Markdown is the canonical answer. IR is derived rendering. Iteration display is one
function over one per-form vector. Tools return data. Channels render from
content-addressed projections.
