# Database Schema

Single SQLite DB for everything: `~/.vis/vis.mdb/rlm.db`.

Schema source of truth: `packages/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.

Flyway migration location: `classpath:db/migration`.

## Entity Tree

```
conversation_soul
  └─ conversation_state
       ├─ query_soul                 (user-ask identity, branch-local)
       │    └─ query_state           (one run/retry)
       │         └─ iteration        (one LLM round-trip)
       │              └─ expression_state  (code execution results, versioned)
       └─ expression_soul            (var / call / literal identity, branch-local)
            └─ expression_dependency (directed edges between expression souls)

log     — optional FKs to: conversation_soul, conversation_state, query_soul,
                            query_state, iteration, expression_soul, expression_state
search  — FTS5 virtual table; populated by triggers on:
                            query_soul.query, expression_state.expr
```

## Tables

### 1) `conversation_soul`
Conversation identity.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Index: `idx_conv_soul_created(created_at DESC)`

### 2) `conversation_state`
Forkable mutable state for a conversation soul.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_soul_id` | TEXT FK | → `conversation_soul.id`, cascade delete |
| `parent_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `title` | TEXT | |
| `version` | INTEGER | `>= 0` |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `UNIQUE(conversation_soul_id, version)`

### 3) `query_soul`
Immutable identity of a user ask (branch-local).

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `title` | TEXT | |
| `query` | TEXT | |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Index: `idx_query_soul_state(conversation_state_id, created_at)`

### 4) `query_state`
One run/retry state for `query_soul`.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `query_soul_id` | TEXT FK | → `query_soul.id`, cascade delete |
| `forked_from_query_state_id` | TEXT FK | → `query_state.id`, set null on delete |
| `version` | INTEGER | `>= 0` |
| `llm_provider` | TEXT | |
| `llm_root_model` | TEXT | |
| `prompt_enrichment` | TEXT | |
| `subtitle` | TEXT | |
| `run_label` | TEXT | |
| `status` | TEXT | `running\|done\|error\|interrupted` |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `UNIQUE(query_soul_id, version)`

### 5) `iteration`
One LLM round-trip inside a `query_state`.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `query_state_id` | TEXT FK | → `query_state.id`, cascade delete |
| `position` | INTEGER | `>= 0` |
| `status` | TEXT | `running\|done\|error\|interrupted` |
| `llm_system_prompt` | TEXT | |
| `llm_user_prompt` | TEXT | multimodal JSON envelope |
| `llm_provider` | TEXT | |
| `llm_model` | TEXT | |
| `llm_response` | TEXT | final selected LLM response |
| `llm_traces` | TEXT | all LLM attempts/traces |
| `llm_full_duration_ms` | INTEGER | nullable, `>= 0` |
| `llm_thinking` | TEXT | |
| `llm_error` | TEXT | |
| `llm_returned_empty_expressions` | INTEGER | 0/1, default 0 |
| `metadata` | TEXT | JSON — active extensions, etc. |
| `created_at` | INTEGER | |
| `finished_at` | INTEGER | nullable |

Constraints: `UNIQUE(query_state_id, position)`

#### Iteration Metadata

The `metadata` column stores per-iteration context as JSON:

```json
{"extensions": [
  {"namespace": "com.blockether.vis.ext.editing",
   "version": "0.1.0"},
  {"namespace": "com.acme.ext.git",
   "version": "2.3.0"}
]}
```

Records which extensions (with full source namespace and version) were
active when the iteration ran, enabling post-mortem analysis and
reproducibility.

### 6) `expression_soul`
Branch-local identity for var/call/literal expression nodes.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `kind` | TEXT | `var\|call\|literal` |
| `state_mode` | TEXT | `stateless\|stateful` |
| `name` | TEXT | nullable |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `CHECK(kind <> 'literal' OR state_mode = 'stateless')`

Unique partial index: `uq_expression_soul_state_name(conversation_state_id, name) WHERE name IS NOT NULL`

### 7) `expression_dependency`
Directed dependency edges between expression souls.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `downstream_expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `upstream_expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `CHECK(downstream <> upstream)`, `UNIQUE(downstream, upstream)`

Triggers enforce same `conversation_state_id` across endpoints.

### 8) `expression_state`
Versioned expression output snapshots emitted per iteration.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `iteration_id` | TEXT FK | → `iteration.id`, cascade delete |
| `version` | INTEGER | `>= 0` |
| `success` | INTEGER | 0/1, default 1 |
| `expr` | TEXT | nullable, non-blank when set |
| `result` | BLOB | Nippy-encoded |
| `error` | BLOB | Nippy-encoded |
| `stdout` | TEXT | |
| `stderr` | TEXT | |
| `duration_ms` | INTEGER | nullable, `>= 0` |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `UNIQUE(expression_soul_id, version)`, `CHECK((success=1 AND error IS NULL) OR (success=0 AND error IS NOT NULL))`

Triggers enforce: first version = 0, stateless expressions only get version 0.

### 9) `log`
Structured logs with optional scope references.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `level` | TEXT | `trace\|debug\|info\|warn\|error\|fatal` |
| `event` | TEXT | machine-stable event key |
| `data` | TEXT | JSON-encoded |
| `conversation_soul_id` | TEXT FK | nullable |
| `conversation_state_id` | TEXT FK | nullable |
| `query_soul_id` | TEXT FK | nullable |
| `query_state_id` | TEXT FK | nullable |
| `iteration_id` | TEXT FK | nullable |
| `expression_soul_id` | TEXT FK | nullable |
| `expression_state_id` | TEXT FK | nullable |
| `created_at` | INTEGER | |

Scoped partial indexes for each nullable FK.

### 10) `search` (FTS5)
Full-text search virtual table.

| Column | Notes |
|--------|-------|
| `owner_table` | unindexed |
| `owner_id` | unindexed |
| `field` | unindexed |
| `text` | indexed |

Tokenizer: `porter unicode61 remove_diacritics 2`

Indexed sources via triggers:
- `query_soul.query`
- `expression_state.expr`

## Persistence Rules

1. All DB code lives under `persistance/*` — nowhere else.
2. HoneySQL only — no raw SQL outside `persistance/sqlite/*.clj`.
3. Callers use `persistance/core.clj` — never import `sqlite/core.clj` directly.
4. Schema changes in `V1__schema.sql` MUST update this page.
5. If this doc and SQL diverge, **`V1__schema.sql` is authoritative** — fix the doc.
