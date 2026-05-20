# Markdown Answer Source of Truth Plan

## Rationale

Model-authored `[:ir ...]` is too brittle. Models naturally produce Markdown, not EDN/Hiccup render trees. One malformed vector, missing attrs map, or nested block list can turn a final answer into an EDN-looking artifact instead of user-facing prose.

Markdown should be the canonical answer source because it is:

- model-native: lower syntax burden in `(done ...)`
- human-readable in storage
- stable for copy/export/transcript use
- portable across channels
- easy to index after CommonMark plain-text extraction

IR should remain, but only as a derived rendering layer:

- parse Markdown with CommonMark
- derive canonical IR/render AST
- render TUI/Telegram/Web/voice from that AST
- cache derived render output by Markdown hash + parser/render version + renderer opts

Source of truth becomes Markdown. IR becomes disposable cache.

## Target architecture

```text
(done {:answer markdown})
  -> validate markdown string
  -> persist answer_markdown TEXT
  -> derive IR via CommonMark when needed
  -> render channel-specific output
```

Canonical data flow:

```text
DB answer_markdown
  -> markdown->ir
  -> TUI render / Telegram render / voice extract / search text
```

Never store model-authored IR as answer truth.

## Model-facing contract

Preferred final answer:

```clojure
(done {:answer "Summary\n\n- Item one\n- Item two"})
```

Optional sugar:

```clojure
(done "Summary\n\n- Item one")
```

Rejected for normal model use:

```clojure
(done [:ir {} [:p {} [:span {} "text"]]])
```

`[:ir ...]` remains internal/debug-only renderer data, not prompt syntax.

Needs-input payloads stay data-shaped:

```clojure
{:vis/answer-mode :needs-input
 :answer/text "Question for user"}
```

## Tool-result contract

Tools must continue returning plain Clojure data, not Markdown.

Good:

```clojure
{:path "src/foo.clj"
 :matches [{:line 12 :text "..."}]}
```

Bad:

```clojure
"### Matches\n\n- src/foo.clj:12 ..."
```

Reason: RLM needs destructurable runtime data. Markdown tool results would force heuristic parsing, reduce reliability, and increase prompt/UI injection risk.

Renderer/display functions may derive Markdown/IR previews from tool result data, but tool results themselves remain data.

## DB shape

Current schema has:

```sql
answer BLOB -- Nippy-frozen IR
```

Replace with Markdown truth:

```sql
answer_markdown TEXT
answer_plaintext TEXT NULL        -- optional derived FTS/search projection
answer_render_cache BLOB NULL     -- optional derived IR/layout cache
answer_cache_key TEXT NULL        -- hash(markdown + parser version + render opts)
```

If old database compatibility is not needed, edit `V1__schema.sql` inline and discard existing DBs.

Cache is optional and invalidatable. Missing cache must reparse Markdown.

## Render/cache rules

Cache key must include:

- raw Markdown hash
- CommonMark/parser version
- enabled Markdown extensions
- renderer id
- renderer options, e.g. width/theme/flavor

Examples:

```clojure
{:source-sha sha256
 :parser-version "commonmark-java+x"
 :renderer :tui
 :width 100
 :theme theme-id}
```

Never treat cache as canonical data.

## Security/safety rules

Markdown is untrusted user/model content.

Required:

- parse raw HTML as visible text or sanitize strictly
- escape per target renderer
- never pass raw Markdown directly to Telegram MarkdownV2/HTML
- allowlist link schemes
- avoid auto-fetching images/resources
- strip or neutralize terminal control sequences for TUI
- FTS indexes derived plain text, not raw syntax only

## Code areas to change

### Prompt

File:

- `src/com/blockether/vis/internal/prompt.clj`

Change:

- replace `(done [:ir ...])` docs with `(done {:answer "markdown"})`
- remove model-facing `ANSWER_IR` section
- add `ANSWER_MARKDOWN` section
- explicitly say: do not author `[:ir ...]`

### Loop / done handling

File:

- `src/com/blockether/vis/internal/loop.clj`

Change:

- `done` accepts Markdown string or `{:answer string}`
- `done` stores answer value as Markdown source
- derive IR only for live rendering and validation
- stop calling `render/->ast` as final answer source canonicalization
- stop persisting `(answer-str answer)` because it loses Markdown

Needed helpers:

```clojure
(answer-markdown x)
(markdown-answer? x)
(answer-markdown->ir s)
```

### Render namespace

File:

- `src/com/blockether/vis/internal/render.clj`

Change:

- clarify string semantics
- prefer `markdown->ir` name for CommonMark parsing
- keep `literal->ir` or `->ast` for literal/data fallback
- ensure `render` on answer paths parses Markdown, not literal string

Current ambiguity:

```clojure
(render/render "**bold**" :markdown)
```

Currently behaves like literal text. Answer path should treat string as Markdown source.

### Persistence schema

File:

- `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`

Change:

- replace `answer BLOB` with `answer_markdown TEXT`
- optionally add cache columns

### Persistence implementation

File:

- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`

Change:

- `db-update-session-turn!` writes `answer_markdown`, not Nippy IR blob
- `row->turn` returns `:answer-markdown`
- compatibility may also return derived `:answer` IR during transition
- `reindex-search!` indexes `answer_markdown` via Markdown -> IR -> plain text

### TUI channel

Files:

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/state.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/virtual.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render*.clj`

Change:

- assistant messages carry Markdown source plus derived IR:

```clojure
{:role :assistant
 :text answer-markdown
 :ir   (vis/text->ir answer-markdown)}
```

- resume path reads `:answer-markdown`
- TUI render path derives IR from Markdown
- copy/export prefers raw Markdown source

### Telegram channel

File:

- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/bot.clj`

Change:

- answer Markdown -> IR -> Telegram renderer
- never send raw Markdown without target escaping

### Voice

File:

- `extensions/common/vis-voice/src/com/blockether/vis/ext/voice/core.clj`

Change:

- answer Markdown -> IR -> plain text extraction

### Transcript/export

Files likely:

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/transcript.clj`
- `src/com/blockether/vis/internal/render.clj` (`session->markdown`)

Change:

- export assistant answer from raw `answer_markdown`
- avoid IR -> Markdown round-trip when source exists

## Migration strategy

Since old DB can be discarded:

1. edit V1 schema directly
2. delete/ignore existing local DB during development
3. update code/tests to new schema
4. no legacy repair path required

If compatibility later needed:

- if old row has `answer BLOB`, render IR to Markdown once
- persist `answer_markdown`
- discard old IR blob after migration

## Test plan

Required tests:

- prompt no longer contains `(done [:ir`
- `(done {:answer "**bold**"})` stores exact Markdown
- derived IR renders bold in TUI
- code fences survive DB round-trip exactly
- tables parse/render acceptably
- links render safely
- raw HTML is visible/safe, not executed
- resume path reconstructs assistant message from Markdown
- export/copy uses raw Markdown
- search finds plain text inside Markdown
- tool results remain destructurable Clojure data
- needs-input still works

## Rollout order

1. Add Markdown answer helpers and tests.
2. Change `done` to accept/store Markdown while still deriving IR for live result.
3. Change prompt contract.
4. Change DB schema and persistence read/write.
5. Update TUI resume/live paths.
6. Update Telegram/voice/CLI/export.
7. Remove model-facing IR docs/tests.
8. Optional: add render cache columns and cache invalidation.

## Non-goals

- Do not make tools return Markdown.
- Do not remove IR renderer internals.
- Do not make raw Markdown target-specific output.
- Do not preserve old local DB if product decision is to drop it.

## Final principle

Markdown is answer truth. IR is derived rendering/cache structure. Tools return data. Channels render from Markdown-derived IR.
