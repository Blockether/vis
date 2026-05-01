# Database schema

Single SQLite DB for everything: `~/.vis/vis.mdb/vis.db`.

Schema source of truth: `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.

> This document defines the **turn model**.
> Canonical hierarchy: `conversation -> turn -> iteration -> block`.

Flyway migration location: `classpath:db/sqlite/migration`.

## Entity tree

```
conversation_soul
  └─ conversation_state
       ├─ conversation_turn_soul          (one user turn identity, branch-local)
       │    └─ conversation_turn_state    (one run/retry of that turn)
       │         └─ iteration             (one LLM round-trip)
       │              ├─ blocks BLOB      (per-block code/result/stdout/stderr/error)
       │              └─ expression_state (versioned var snapshots)
       └─ expression_soul                 (var identity, branch-local)
            └─ expression_dependency      (dependency edges)

log     — optional FKs to: conversation_soul, conversation_state,
                            conversation_turn_soul, conversation_turn_state,
                            iteration, expression_soul, expression_state
search  — FTS5 virtual table; populated by triggers on:
                            conversation_turn_soul.user_request,
                            expression_state.expr
```

## Terminology contract

- **Conversation**: full chat thread.
- **Turn**: one user ask + assistant answer pair inside a conversation.
- **Iteration**: one internal LLM/eval cycle inside a turn.
- **Block**: one executed code block entry inside an iteration.

Conversation turns use canonical turn/user-request terminology.

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

Table: `conversation_state`. Forkable mutable snapshot of one conversation soul.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_soul_id` | TEXT FK | → `conversation_soul.id`, cascade delete |
| `parent_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `title` | TEXT | |
| `version` | INTEGER | `>= 0` |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Constraint: `UNIQUE(conversation_soul_id, version)`

### Conversation turn soul

Table: `conversation_turn_soul`. Immutable identity of one turn.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | canonical `conversation-turn-id` |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `title` | TEXT | optional turn title |
| `user_request` | TEXT | raw human-authored turn text |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

Index: `idx_turn_soul_state(conversation_state_id, created_at)`

### Conversation turn state

Table: `conversation_turn_state`. One run/retry for a turn soul.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_turn_soul_id` | TEXT FK | → `conversation_turn_soul.id`, cascade delete |
| `forked_from_conversation_turn_state_id` | TEXT FK | → `conversation_turn_state.id`, set null on delete |
| `version` | INTEGER | `>= 0` |
| `llm_root_provider` | TEXT | |
| `llm_root_model` | TEXT | |
| `prompt_enrichment` | TEXT | |
| `subtitle` | TEXT | |
| `run_label` | TEXT | |
| `status` | TEXT | `running|done|error|interrupted` |
| `metadata` | TEXT | JSON-encoded |
| `prior_outcome` | TEXT | nullable; `complete|abandoned|cancelled|error` |
| `created_at` | INTEGER | |

Constraints and indexes:
- `UNIQUE(conversation_turn_soul_id, version)`
- `idx_turn_state_soul(conversation_turn_soul_id, version)`
- `idx_turn_state_forked_from(forked_from_conversation_turn_state_id)`

### Iteration

Table: `iteration`. One LLM round-trip inside one turn state.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_turn_state_id` | TEXT FK | → `conversation_turn_state.id`, cascade delete |
| `position` | INTEGER | `>= 0` |
| `status` | TEXT | `running|done|error|interrupted` |
| `llm_system_prompt` | TEXT | |
| `llm_user_prompt` | TEXT | multimodal JSON envelope |
| `llm_provider` | TEXT | |
| `llm_model` | TEXT | |
| `llm_full_duration_ms` | INTEGER | nullable, `>= 0` |
| `llm_thinking` | TEXT | |
| `llm_error` | TEXT | |
| `llm_returned_empty_blocks` | INTEGER | 0/1, default 0 |
| `llm_input_tokens` | INTEGER | nullable, `>= 0` |
| `llm_output_tokens` | INTEGER | nullable, `>= 0` |
| `llm_reasoning_tokens` | INTEGER | nullable, `>= 0` |
| `llm_cached_tokens` | INTEGER | nullable, `>= 0` |
| `llm_cost_usd` | REAL | nullable, `>= 0` |
| `metadata` | TEXT | JSON-encoded |
| `blocks` | BLOB | nippy vec of per-block maps |
| `answer_form_idx` | INTEGER | nullable, `>= 0` |
| `created_at` | INTEGER | |
| `finished_at` | INTEGER | nullable |

Constraints and indexes:
- `UNIQUE(conversation_turn_state_id, position)`
- `idx_iteration_turn_state(conversation_turn_state_id, position)`
- `idx_iteration_turn_state_created(conversation_turn_state_id, created_at)`

### Expression soul

Table: `expression_soul`. Branch-local identity for user-defined vars.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`, cascade delete |
| `kind` | TEXT | `var|literal` |
| `state_mode` | TEXT | `stateless|stateful` |
| `name` | TEXT | nullable |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

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

### Expression state

Table: `expression_state`. Versioned expression snapshots emitted per iteration.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `expression_soul_id` | TEXT FK | → `expression_soul.id`, cascade delete |
| `iteration_id` | TEXT FK | → `iteration.id`, cascade delete |
| `version` | INTEGER | `>= 0` |
| `success` | INTEGER | 0/1 |
| `expr` | TEXT | nullable |
| `result` | BLOB | nippy |
| `error` | BLOB | nippy |
| `stdout` | TEXT | |
| `stderr` | TEXT | |
| `duration_ms` | INTEGER | nullable, `>= 0` |
| `metadata` | TEXT | JSON-encoded |
| `created_at` | INTEGER | |

### Log

Table: `log`. Structured logs with optional scope references.

| Column | Type | Notes |
|--------|------|-------|
| `id` | TEXT PK | |
| `level` | TEXT | `trace|debug|info|warn|error|fatal` |
| `event` | TEXT | machine-stable event key |
| `data` | TEXT | JSON-encoded |
| `conversation_soul_id` | TEXT FK | nullable |
| `conversation_state_id` | TEXT FK | nullable |
| `conversation_turn_soul_id` | TEXT FK | nullable |
| `conversation_turn_state_id` | TEXT FK | nullable |
| `iteration_id` | TEXT FK | nullable |
| `expression_soul_id` | TEXT FK | nullable |
| `expression_state_id` | TEXT FK | nullable |
| `created_at` | INTEGER | |

### Full-text search

Table: `search` (FTS5 virtual table).

Indexed sources:
- `conversation_turn_soul.user_request`
- `expression_state.expr`

## Persistence rules

1. All DB code lives under `persistance/*` — nowhere else.
2. HoneySQL only — no raw SQL outside `persistance/sqlite/*.clj` (except migration/FTS/DDL exceptions already documented in code).
3. Callers use `persistance/core.clj` — never import SQLite backend internals directly.
4. Schema changes MUST update this page in the same PR.
5. If this doc and SQL diverge, SQL is authoritative — fix the doc immediately.
