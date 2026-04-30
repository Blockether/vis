# Conversation/Turn Model Migration + TUI Collapsibles (Atomic Plan)

Status: approved direction, execution in progress.

## Non-negotiable decisions

1. Full rename: `query -> turn` (no compatibility wrappers).
2. Canonical hierarchy: `conversation -> turn -> iteration -> block`.
3. One-time hard break: manually delete `~/.vis/vis.db` before running new build.
4. No runtime compatibility detection; no legacy migration path.
5. Canonical DB/API terminology:
   - tables: `conversation_turn_soul`, `conversation_turn_state`
   - id key: `:conversation-turn-id`
   - function names: verbose (`db-store-conversation-turn!`, etc.)
6. TUI collapsibles:
   - `<details>` collapsible anywhere
   - nested `<details>` supported
   - default collapsed
   - mouse-only toggle, full summary row clickable
   - collapsed row text shows hidden size hint
   - IDs visible as **suffix** on header row
   - auto-wrap only `stdout`/`result`
   - threshold: `> 12 lines OR > 700 chars`
   - finalized messages only
   - collapse state in memory only, keyed by `conversation-id + node-id`

---

## Phase order (must follow)

### Phase 1 — Documentation first (current phase)

Goal: replace query model docs with turn model docs before code changes.

Files:
- `docs/src/architecture/overview.md`
- `docs/src/architecture/database.md`
- `docs/src/architecture/iteration-flow.md`
- `docs/src/architecture/state.md`
- `docs/src/architecture/packages.md`
- `docs/src/architecture/channels.md`
- `docs/src/extensions/environment.md`

Acceptance:
- Architecture docs contain no `query_*` model terms.
- Turn hierarchy is explicit everywhere.

### Phase 2 — SQLite schema switch (in-place V1)

Goal: introduce new canonical schema with turn tables/columns.

Files:
- `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`
- update docs/comments/tests that still mention query-era table names

Core changes:
- `conversation_turn_soul` -> `conversation_turn_soul`
- `conversation_turn_state` -> `conversation_turn_state`
- `iteration.conversation_turn_state_id` -> `iteration.conversation_turn_state_id`
- log FK columns/indexes and FTS trigger sources renamed accordingly

Acceptance:
- Fresh DB boots from updated V1 schema (after manual wipe).
- No remaining runtime dependency on `query_*` tables.

### Phase 3 — Persistence facade + backend API rename

Goal: full API rename to turn terminology.

Files:
- `src/com/blockether/vis/internal/persistance.clj`
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`

Representative function renames:
- `db-store-conversation-turn!` -> `db-store-conversation-turn!`
- `db-update-conversation-turn!` -> `db-update-conversation-turn!`
- `db-retry-conversation-turn!` -> `db-retry-conversation-turn!`
- `db-list-conversation-turns` -> `db-list-conversation-turns`
- `db-list-conversation-turn-iterations` -> `db-list-conversation-turn-iterations`

Acceptance:
- No public/backend API still exposing query vocabulary.
- Core compiles against renamed API only.

### Phase 4 — Runtime, env, lifecycle, and channel rename

Goal: remove query vocabulary from runtime symbols and payloads.

Files (minimum):
- `src/com/blockether/vis/core.clj`
- `src/com/blockether/vis/internal/loop.clj`
- `src/com/blockether/vis/internal/main.clj`
- `src/com/blockether/vis/internal/progress.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/state.clj`

Contract updates:
- payload key `:conversation-turn-id` -> `:conversation-turn-id`
- env/query naming -> turn/next-turn naming
- prompt/system-var naming updates where required

Acceptance:
- `rg` shows no query model symbols on runtime/channel surfaces.

### Phase 5 — TUI details/collapsible implementation

Goal: real collapsibles with stable IDs and nested support.

Files:
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/click_regions.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/screen.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/state.clj`

Implementation points:
- Parse `<details>/<summary>` into nested nodes.
- Node ID graph across turn/iteration/block/details paths.
- Mouse click toggles collapse by node-id.
- Auto-wrap oversized stdout/result into synthetic details.
- Render collapsed summary row:
  - e.g. `▸ STDOUT (code 3) · 38 lines hidden · [t:7b18 i:2 b:3]`

Acceptance:
- `<details>` no longer cosmetic.
- Nested toggles work.
- Collapsed defaults and auto-wrap behavior match decisions.

### Phase 6 — Prompt/docs/tests cleanup

Goal: remove stale vocabulary and align contracts.

Targets:
- user/system prompt literals referencing query model
- docs beyond architecture where model terms appear
- tests updated for renamed APIs and schema

Acceptance:
- no stale query model references except intentional historical notes.

### Phase 7 — Verification gate

Mandatory:
- `./verify.sh`

Ship only after green.

---

## Execution checklist

- [ ] Phase 1 docs complete
- [ ] Phase 2 schema switch complete
- [ ] Phase 3 persistence API rename complete
- [ ] Phase 4 runtime/channel rename complete
- [ ] Phase 5 TUI collapsibles complete
- [ ] Phase 6 prompt/docs/tests cleanup complete
- [ ] Phase 7 verify green
