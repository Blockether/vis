# Database schema

Single SQLite DB for everything: `~/.vis/vis.mdb/vis.db`.

Schema source of truth: `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`. The SQL ships with the SQLite backend because it is dialect-specific. The persistence facade and migration runner live in **`vis-persistance`**; the SQLite/Flyway runtime lives in `vis-persistance-sqlite` and points the runner at the classpath resource via `MIGRATIONS = "classpath:db/sqlite/migration"`.

Flyway migration location: `classpath:db/sqlite/migration`.

## Entity tree

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

### Conversation soul

Table: `conversation_soul`. Conversation identity.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Index: `idx_conv_soul_created(created_at DESC)`

### Conversation state

Table: `conversation_state`. Forkable mutable state for a conversation soul.

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

### Query soul

Table: `query_soul`. Immutable identity of a user ask (branch-local).

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `title` | TEXT | |
| `query` | TEXT | |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Index: `idx_query_soul_state(conversation_state_id, created_at)`

### Query state

Table: `query_state`. One run/retry state for `query_soul`.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `query_soul_id` | TEXT FK | → `query_soul.id`, cascade delete |
| `forked_from_query_state_id` | TEXT FK | → `query_state.id`, set null on delete |
| `version` | INTEGER | `>= 0` |
| `llm_root_model` | TEXT | |
| `prompt_enrichment` | TEXT | |
| `subtitle` | TEXT | |
| `run_label` | TEXT | |
| `status` | TEXT | `running\|done\|error\|interrupted` |
| `metadata` | TEXT | JSON-encoded |
| `prior_outcome` | TEXT | nullable; one of `complete\|abandoned\|cancelled\|error`. Set on the terminal iteration (and by `db-sweep-orphaned-running-queries!` for crashed runs) so the next turn's handover digest can summarize the previous turn without scanning every iteration. |
| `created_at` | INTEGER | |

Constraints: `UNIQUE(query_soul_id, version)`
Indexes: `idx_query_state_soul(query_soul_id, version)`, `idx_query_state_forked_from(forked_from_query_state_id)`

### Iteration

Table: `iteration`. One LLM round-trip inside a `query_state`.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `query_state_id` | TEXT FK | → `query_state.id`, cascade delete |
| `position` | INTEGER | `>= 0` |
| `status` | TEXT | `running\|done\|error\|interrupted` |
| `llm_system_prompt` | TEXT | |
| `llm_user_prompt` | TEXT | multimodal JSON envelope |
| `llm_model` | TEXT | |
| `llm_full_duration_ms` | INTEGER | nullable, `>= 0` |
| `llm_thinking` | TEXT | |
| `llm_error` | TEXT | |
| `llm_returned_empty_blocks` | INTEGER | 0/1, default 0 |
| `metadata` | TEXT | JSON — active extensions, etc. |
| `created_at` | INTEGER | |
| `finished_at` | INTEGER | nullable |

Constraints: `UNIQUE(query_state_id, position)`
Indexes: `idx_iteration_query_state(query_state_id, position)`, `idx_iteration_query_state_created(query_state_id, created_at)`

#### Iteration metadata

The `metadata` column stores per-iteration context as JSON:

```json
{"extensions": [
  {"namespace": "com.blockether.vis.ext.foundation.editing.core",
   "version": "0.3.0"},
  {"namespace": "com.acme.ext.git",
   "version": "2.3.0"}
]}
```

Records which extensions (with full source namespace and version) were
active when the iteration ran, enabling post-mortem analysis and
reproducibility.

### Expression soul

Table: `expression_soul`. Branch-local identity for var/call/literal expression nodes.

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
Indexes: `idx_expression_soul_state_kind(conversation_state_id, kind, created_at)`
Unique partial index: `uq_expression_soul_state_name(conversation_state_id, name) WHERE name IS NOT NULL`

### Expression dependency

Table: `expression_dependency`. Directed dependency edges between expression souls.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `downstream_expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `upstream_expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraints: `CHECK(downstream <> upstream)`, `UNIQUE(downstream, upstream)`
Indexes: `idx_expr_dep_downstream`, `idx_expr_dep_upstream`, `idx_expr_dep_state(conversation_state_id)`
Triggers enforce same `conversation_state_id` across endpoints (`trg_expr_dep_same_state_ai/au`).

### Expression state

Table: `expression_state`. Versioned expression output snapshots emitted per iteration.

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
Indexes: `idx_expression_state_soul(expression_soul_id, version)`, `idx_expression_state_iteration(iteration_id)`
Triggers (`trg_expression_state_stateless_ai/au`) enforce: first version = 0, stateless blocks only ever get version 0 and at most one row.

### Log

Table: `log`. Structured logs with optional scope references.

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

Indexes: `idx_log_level(level, created_at)`, `idx_log_created(created_at)`, `idx_log_event_created(event, created_at)`, plus a partial index per nullable FK (`idx_log_conv_soul`, `idx_log_conv_state`, `idx_log_query_soul`, `idx_log_query_state`, `idx_log_iteration`, `idx_log_expression_soul`, `idx_log_expression_state`).

### Full-text search

Table: `search` (FTS5 virtual table).

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

## Persistence rules

1. All DB code lives under `persistance/*` — nowhere else.
2. HoneySQL only — no raw SQL outside `persistance/sqlite/*.clj`.
3. Callers use `persistance/core.clj` — never import `sqlite/core.clj` directly.
4. Schema changes in `V1__schema.sql` MUST update this page.
5. If this doc and SQL diverge, **`V1__schema.sql` is authoritative** — fix the doc.
