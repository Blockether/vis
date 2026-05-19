# Metadata Usage Spec

## Goal

Document every generic metadata payload in core persistence: where it is written, where it is read, why it exists, and whether that use is correct.

This spec is about metadata ownership. LLM retry/routing behavior is implemented in svar's router and Vis's persistence/TUI layers (see `CHANGELOG.md` for the trace contract); this file only judges whether routing facts belong in metadata. They do not.

## Rule

```text
If Vis core reads it, filters by it, renders it, resumes from it, or tests it -> named column/table.
If extension owns it and only extension interprets it -> extension sidecar payload is allowed.
If log event owns it -> log.data is allowed.
```

Generic metadata in core domain tables is schema debt. It hides contracts, blocks constraints/indexes, and makes resume/render behavior depend on JSON shape.

## Implementation mode

Current dev policy allows deleting/recreating SQLite DB. Therefore schema specs describe target `CREATE TABLE` shape in `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`, not incremental migration steps.

## Inventory

| Storage | Used for | Where read/written | Correct? | Target |
| --- | --- | --- | --- | --- |
| `conversation_soul.metadata` | channel, external id | SQLite persistence conversation create/list/find | No | `channel`, `external_id` columns |
| `conversation_state.metadata` | system prompt, root provider/model | SQLite persistence conversation create/get/list | No | `system_prompt`, `llm_root_provider`, `llm_root_model` columns |
| `conversation_turn_soul.metadata` | nothing in normal path | Mostly dead | No | remove column |
| `conversation_turn_state.metadata` | messages, iteration count, duration, tokens, cost, provider/model | turn update/read, transcript/list summaries | No | named turn outcome columns |
| `conversation_turn_iteration.metadata` | LLM routing summary/trace, engine timing, extension snapshots | loop writes; TUI resume reads; diagnostics read | Mixed, mostly no | routing columns/table, `engine_timing`, `runtime_extension_snapshot` |
| `extension_aggregate.metadata` | extension-owned query/index fields | extension aggregate APIs | Yes, at extension boundary | keep for now; conceptually `index_data` |
| `log.data` | event payload | log/event diagnostics | Yes, event-owned | keep as `data` |

## Source trace

Core metadata writes/reads currently concentrate in:

- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`
  - writes conversation/turn/iteration metadata
  - reads metadata into public conversation/turn maps
  - filters `conversation_soul.metadata` with JSON extraction for channel/external id
  - filters `extension_aggregate.metadata` for extension-owned aggregate queries
- `src/com/blockether/vis/internal/loop.clj`
  - builds LLM routing maps
  - writes per-iteration `:metadata` containing `:llm`, `:engine-timing`, `:extensions`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`
  - rebuilds history from iteration metadata `[:metadata :llm]`
  - forwards routing trace into rendered assistant message state
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`
  - renders message fields derived from public iteration routing fields rehydrated from routing rows

## Decisions by table

### `conversation_soul.metadata`

Current payload:

```clojure
{:channel :tui
 :external-id "telegram-chat-id-or-other"}
```

Why it exists: bind Vis conversation identity to channel-specific external identity.

Where used:

```clojure
(db-get-conversation db id)        ;; returns :channel :external-id
(db-list-conversations db :tui)    ;; filters channel
(db-find-conversation-by-external) ;; filters external id
```

Judgment: incorrect metadata use. These are identity/index fields.

Target V1 shape:

```sql
CREATE TABLE conversation_soul (
  id           TEXT PRIMARY KEY NOT NULL,
  channel      TEXT NOT NULL DEFAULT 'tui',
  external_id  TEXT,
  created_at   INTEGER NOT NULL
);

CREATE INDEX idx_conversation_soul_channel_created
  ON conversation_soul(channel, created_at DESC);

CREATE UNIQUE INDEX idx_conversation_soul_channel_external
  ON conversation_soul(channel, external_id)
  WHERE external_id IS NOT NULL;
```

### `conversation_state.metadata`

Current payload:

```clojure
{:system-prompt "..."
 :provider :anthropic-coding-plan
 :model "claude-opus-4-7"}
```

Why it exists: snapshot conversation-level prompt/provider/model defaults.

Where used:

```clojure
(db-get-conversation db id) ;; returns :system-prompt :provider :model
(row->conversation)         ;; title/model/provider display
```

Judgment: incorrect metadata use. Conversation snapshot facts are core state.

Target V1 shape excerpt:

```sql
CREATE TABLE conversation_state (
  id                    TEXT PRIMARY KEY NOT NULL,
  conversation_soul_id  TEXT NOT NULL REFERENCES conversation_soul(id) ON DELETE CASCADE,
  parent_state_id       TEXT REFERENCES conversation_state(id) ON DELETE CASCADE,
  title                 TEXT,
  version               INTEGER NOT NULL CHECK (version >= 0),
  system_prompt         TEXT,
  llm_root_provider     TEXT,
  llm_root_model        TEXT,
  created_at            INTEGER NOT NULL,

  UNIQUE (conversation_soul_id, version)
);
```

### `conversation_turn_soul.metadata`

Current payload: none in normal write path.

Why it exists: historical generic slot.

Where used: no product path should depend on it.

Judgment: incorrect/dead metadata use.

Target: remove column from V1. Turn soul already has identity facts: `conversation_state_id`, `position`, `user_request`, `created_at`.

### `conversation_turn_state.metadata`

Current payload:

```clojure
{:messages [...]
 :iteration-count 3
 :duration-ms 12345
 :input-tokens 100
 :output-tokens 20
 :reasoning-tokens 0
 :cached-tokens 50
 :total-cost 0.0123
 :provider :anthropic-coding-plan
 :model "claude-opus-4-7"}
```

Why it exists: persist turn outcome and summary metrics without schema changes.

Where used:

```clojure
(row->turn row)
;; returns :iteration-count :duration-ms tokens/cost/model/provider
```

Judgment: incorrect metadata use. Turn list, transcript, cost summaries, provider summaries, and fallback status are core product facts.

Target V1 shape excerpt:

```sql
CREATE TABLE conversation_turn_state (
  id                           TEXT PRIMARY KEY NOT NULL,
  conversation_turn_soul_id    TEXT NOT NULL REFERENCES conversation_turn_soul(id) ON DELETE CASCADE,
  forked_from_conversation_turn_state_id TEXT REFERENCES conversation_turn_state(id) ON DELETE SET NULL,
  version                      INTEGER NOT NULL CHECK (version >= 0),
  status                       TEXT NOT NULL CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  answer                       BLOB,
  messages                     TEXT,
  iteration_count              INTEGER DEFAULT 0 CHECK (iteration_count >= 0),
  duration_ms                  INTEGER DEFAULT 0 CHECK (duration_ms >= 0),
  llm_input_tokens             INTEGER DEFAULT 0 CHECK (llm_input_tokens >= 0),
  llm_output_tokens            INTEGER DEFAULT 0 CHECK (llm_output_tokens >= 0),
  llm_reasoning_tokens         INTEGER DEFAULT 0 CHECK (llm_reasoning_tokens >= 0),
  llm_cached_tokens            INTEGER DEFAULT 0 CHECK (llm_cached_tokens >= 0),
  llm_total_cost_usd           REAL DEFAULT 0 CHECK (llm_total_cost_usd >= 0),
  llm_selected_provider        TEXT,
  llm_selected_model           TEXT,
  llm_actual_provider          TEXT,
  llm_actual_model             TEXT,
  llm_fallback                 INTEGER NOT NULL DEFAULT 0 CHECK (llm_fallback IN (0, 1)),
  prior_outcome                TEXT CHECK (prior_outcome IS NULL OR prior_outcome IN ('complete', 'cancelled', 'error')),
  created_at                   INTEGER NOT NULL,

  UNIQUE (conversation_turn_soul_id, version)
);
```

### `conversation_turn_iteration.metadata`

Current payload:

```clojure
{:llm {:selected {:provider "..." :model "..."}
       :actual {:provider "..." :model "..."}
       :fallback? true
       :trace [{:event/type :llm.routing/provider-fallback
                :from-provider "..."
                :from-model "..."
                :to-provider "..."
                :to-model "..."}]}
 :engine-timing {...}
 :extensions [{:name 'com.blockether.vis.ext.foundation.core
               :version "..."
               :source-paths [...]
               :source-mtime-max 123
               :source-sha256 "..."}]}
```

Why it exists: one generic slot stores three unrelated concerns: LLM routing UI state, engine timing diagnostics, extension provenance.

Where used:

```clojure
;; TUI rebuild-history
(get-in it [:metadata :llm])
(get-in it [:metadata :llm :trace])

;; change detector / diagnostics
(:extensions (:metadata iteration))
(:engine-timing (:metadata iteration))
```

Judgment: mixed, mostly incorrect.

- `:llm` is core routing/resume state -> dedicated `conversation_turn_iteration.llm_*` columns + `llm_routing_event` table (V1 schema).
- `:engine-timing` is core diagnostics -> explicit `engine_timing` column is acceptable JSON because timing shape may evolve but ownership is named.
- `:extensions` is runtime provenance -> separate `runtime_extension_snapshot` rows.

Target V1 shape excerpt:

```sql
CREATE TABLE conversation_turn_iteration (
  id                         TEXT PRIMARY KEY NOT NULL,
  conversation_turn_state_id TEXT NOT NULL REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  position                   INTEGER NOT NULL CHECK (position >= 1),
  status                     TEXT NOT NULL CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  llm_provider               TEXT,
  llm_model                  TEXT,
  llm_selected_provider      TEXT,
  llm_selected_model         TEXT,
  llm_actual_provider        TEXT,
  llm_actual_model           TEXT,
  llm_fallback               INTEGER NOT NULL DEFAULT 0 CHECK (llm_fallback IN (0, 1)),
  engine_timing              TEXT,
  code                       TEXT NOT NULL,
  result                     BLOB,
  error                      BLOB,
  duration_ms                INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0),
  created_at                 INTEGER NOT NULL,
  finished_at                INTEGER,

  UNIQUE (conversation_turn_state_id, position)
);
```

Extension snapshot target:

```sql
CREATE TABLE runtime_extension_snapshot (
  id TEXT PRIMARY KEY,
  conversation_turn_iteration_id TEXT NOT NULL REFERENCES conversation_turn_iteration(id) ON DELETE CASCADE,
  extension_name TEXT NOT NULL,
  extension_version TEXT,
  source_paths TEXT,
  source_mtime_max INTEGER,
  source_sha256 TEXT,
  created_at INTEGER NOT NULL,
  UNIQUE(conversation_turn_iteration_id, extension_name)
);
```

### `extension_aggregate.metadata`

Current payload examples:

```clojure
{:path "src/core.clj"}
{:source "core/run" :target "db/query" :edge-kind "calls"}
{:kind "node"}
```

Why it exists: extension-owned secondary query/index data.

Where used:

```clojure
(extension-list-aggregates env {:kind :bridge/edge
                                :metadata {:source "core/run"}})
```

Judgment: acceptable at extension boundary. Core Vis should not interpret it as product state.

Target: keep for now; optionally rename concept/API to `index_data` later to avoid confusing it with core metadata.

### `log.data`

Why it exists: event-owned payload.

Judgment: acceptable. This is not generic domain metadata.

Target: keep as `data`.

## Implementation plan

1. Edit V1 schema inline.
2. Update writes to named columns/tables.
3. Update reads to named columns/tables.
4. Keep compatibility fallback only if needed for current local DB, with diagnostic logging.
5. Remove compatibility fallback after dev DB rebuild.

## Summary verdict

```text
conversation_soul.metadata           -> remove; use channel, external_id
conversation_state.metadata          -> remove; use system_prompt, llm_root_provider, llm_root_model
conversation_turn_soul.metadata      -> remove
conversation_turn_state.metadata     -> remove; use named outcome/usage/routing columns
conversation_turn_iteration.metadata -> remove; use routing columns/table, engine_timing, runtime_extension_snapshot
extension_aggregate.metadata         -> keep as extension-owned sidecar for now
log.data                             -> keep as event payload
```
