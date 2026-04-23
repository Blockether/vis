# vis — Data Model

Single SQLite database. Single migration file: `V1__schema.sql`.

## Architecture

```
┌─────────────────────────────────────────────┐
│  Host repo (untouched)                      │
│  mounted read-only into container           │
└──────────────┬──────────────────────────────┘
               │ $RUNTIME run -v /repo:/repo:ro
    ┌──────────┴──────────┐
    │  Container (COW)    │  ← any OCI runtime gives you overlay for free
    │  cp -r /repo /work  │  ← or just use the COW layer directly
    │  agent makes changes│
    │  cd /work && git diff → stdout  ← THE DIFF
    └─────────────────────┘
```

Each conversation fork = one container. When done:

```bash
# From host — get the diff out ($RUNTIME = docker | podman | nerdctl | lima)
$RUNTIME exec $CONTAINER_ID sh -c "cd /work && git diff" > fork-a.patch

# Review it, approve it
git apply fork-a.patch
```

For testing with real services (databases, APIs), just docker-compose:

```yaml
# Works with docker-compose or podman-compose
services:
  agent-fork-a:
    image: vis-agent
    volumes:
      - ./repo:/repo:ro
  postgres:
    image: postgres:16
  redis:
    image: redis:7
```

Each fork gets its own compose stack. Agent tests against real services.
When done → `git diff` → patch out.

### Supported container runtimes

All OCI-compatible runtimes work — same CLI interface:

| Runtime   | Notes                                         |
|-----------|-----------------------------------------------|
| `docker`  | Default. Most common.                         |
| `podman`  | Rootless, daemonless. Drop-in Docker replacement. |
| `nerdctl` | containerd-native. Docker-compatible CLI.      |
| `lima`    | Linux VMs on macOS. Wraps containerd/nerdctl.  |

vis auto-detects the available runtime or respects the `VIS_CONTAINER_RUNTIME`
environment variable. The runtime name is stored on `conversation_state.container_runtime`
so replays know which binary to call.

## Soul/State Pattern

Two entities use identity/state separation:

- **`conversation`** (soul) → **`conversation_state`** (state, forkable)
- **`iteration_var`** (soul) → **`iteration_var_state`** (state, versioned)

Forking creates a new `conversation_state` with `parent_state_id` pointing
to the branch point. Queries attach to a state, not the soul.

## Schema → Docker Mapping

| Schema                              | Docker                                             |
|-------------------------------------|----------------------------------------------------|
| `conversation` (soul)               | The repo/project identity                          |
| `conversation_state` (state)        | One container                                      |
| `conversation_state.parent_state_id`| Container forked from same base image              |
| `fork_after_query_id`               | The repo state (commit) when container was created |
| queries/iterations                  | Agent work inside the container                    |
| `patch`                             | `git diff` = the output                            |
| `patch_status`                      | `pending → approved → applied` (or `rejected`)     |

## Entity Hierarchy

```
conversation (soul — channel, external_id, title)
  ├─ conversation_state (state — prompt, model, title override, container, patch)
  │    └─ query (one per user turn)
  │         └─ iteration (one per LLM round-trip)
  │              ├─ execution (one per code block)
  │              └─ iteration_var_state ──→ iteration_var (soul)
  ├─ iteration_var (soul — one per unique var name)
  └─ log (structured logs, scoped to any level in the hierarchy)
```

## Tables

### `conversation` — soul
Stable identity that persists across forks.

| Column       | Type    | Description                     |
|--------------|---------|---------------------------------|
| `id`         | TEXT PK | UUID                            |
| `channel`    | TEXT    | `vis`, `telegram`, `cli`        |
| `external_id`| TEXT    | e.g. Telegram chat-id           |
| `title`      | TEXT    | display name                    |
| `created_at` | INTEGER | epoch-ms                        |

### `conversation_state` — state (forkable)
Mutable properties. Each fork = new row.

| Column                | Type    | Description                                    |
|-----------------------|---------|------------------------------------------------|
| `id`                  | TEXT PK | UUID                                           |
| `conversation_id`     | TEXT FK | → `conversation.id`                            |
| `parent_state_id`     | TEXT FK | → `conversation_state.id` (null = root state)  |
| `fork_after_query_id` | TEXT    | branch point query id in parent state           |
| `system_prompt`       | TEXT    | LLM system prompt                              |
| `model`               | TEXT    | model name                                     |
| `title`               | TEXT    | conversation title                             |
| `version`             | INTEGER | monotonically increasing per conversation       |
| `container_runtime`   | TEXT    | `docker`, `podman`, `nerdctl`, `lima` (null = host) |
| `container_id`        | TEXT    | container id from the runtime                    |
| `container_image`     | TEXT    | image used to create the container               |
| `container_status`    | TEXT    | `created`, `running`, `stopped`, `removed`       |
| `patch`               | TEXT    | `git diff` output                              |
| `patch_status`        | TEXT    | `pending`, `approved`, `applied`, `rejected`    |
| `created_at`          | INTEGER | epoch-ms                                       |

### `query` — one per user turn
Linked to a `conversation_state`, not the soul.

| Column                  | Type    | Description                              |
|-------------------------|---------|------------------------------------------|
| `id`                    | TEXT PK | UUID                                     |
| `conversation_state_id` | TEXT FK | → `conversation_state.id`                |
| `name`                  | TEXT    | first 100 chars of text                  |
| `messages`              | TEXT    | pr-str'd message history                 |
| `text`                  | TEXT    | raw user query                           |
| `answer`                | TEXT    | final answer (pr-str'd)                  |
| `iterations`            | INTEGER | iteration count                          |
| `duration_ms`           | INTEGER | total time                               |
| `status`                | TEXT    | `running`, `done`, `error`, `interrupted`|
| `eval_score`            | REAL    | quality score                            |
| `model`                 | TEXT    | actual model used                        |
| `input_tokens`          | INTEGER | token counts                             |
| `output_tokens`         | INTEGER |                                          |
| `reasoning_tokens`      | INTEGER |                                          |
| `cached_tokens`         | INTEGER |                                          |
| `total_cost`            | REAL    | cost in USD                              |
| `created_at`            | INTEGER | epoch-ms                                 |
| `updated_at`            | INTEGER | epoch-ms                                 |

### `iteration` — one per LLM round-trip

| Column       | Type    | Description                            |
|--------------|---------|----------------------------------------|
| `id`         | TEXT PK | UUID                                   |
| `query_id`   | TEXT FK | → `query.id`                           |
| `answer`     | TEXT    | set on final iteration                 |
| `thinking`   | TEXT    | LLM reasoning (non-reasoning providers)|
| `error`      | TEXT    | pr-str'd error if failed               |
| `duration_ms`| INTEGER | time for this iteration                 |
| `created_at` | INTEGER | epoch-ms                               |
| `updated_at` | INTEGER | epoch-ms                               |

### `execution` — one per code block in an iteration

| Column        | Type    | Description                    |
|---------------|---------|--------------------------------|
| `id`          | TEXT PK | UUID                           |
| `iteration_id`| TEXT FK | → `iteration.id`              |
| `position`    | INTEGER | ordering within iteration      |
| `code`        | TEXT    | source code                    |
| `result`      | TEXT    | pr-str'd result value          |
| `error`       | TEXT    | error message                  |
| `stdout`      | TEXT    | captured stdout                |
| `stderr`      | TEXT    | captured stderr                |
| `duration_ms` | INTEGER | execution time                 |
| `timeout`     | INTEGER | 0/1                            |
| `repaired`    | INTEGER | 0/1                            |
| `created_at`  | INTEGER | epoch-ms                       |

### `iteration_var` — soul (one per var name per conversation)

| Column            | Type    | Description              |
|-------------------|---------|--------------------------|
| `id`              | TEXT PK | UUID                     |
| `conversation_id` | TEXT FK | → `conversation.id`      |
| `name`            | TEXT    | var name                 |
| `created_at`      | INTEGER | epoch-ms                 |

UNIQUE constraint on `(conversation_id, name)`.

### `iteration_var_state` — each mutation of a var

| Column            | Type    | Description                    |
|-------------------|---------|--------------------------------|
| `id`              | TEXT PK | UUID                           |
| `iteration_var_id`| TEXT FK | → `iteration_var.id`           |
| `iteration_id`    | TEXT FK | → `iteration.id`               |
| `version`         | INTEGER | monotonically increasing       |
| `value`           | TEXT    | pr-str'd EDN                   |
| `code`            | TEXT    | pr-str'd `{:expr :time-ms}`    |
| `created_at`      | INTEGER | epoch-ms                       |

### `log` — structured logs at the SQLite level

| Column            | Type    | Description                              |
|-------------------|---------|------------------------------------------|
| `id`              | TEXT PK | UUID                                     |
| `level`           | TEXT    | `trace`, `debug`, `info`, `warn`, `error`, `fatal` |
| `ns`              | TEXT    | source namespace                         |
| `message`         | TEXT    | human-readable message                   |
| `data`            | TEXT    | pr-str'd EDN payload                     |
| `conversation_id` | TEXT FK | scope to conversation (optional)         |
| `state_id`        | TEXT FK | scope to conversation_state (optional)   |
| `query_id`        | TEXT FK | scope to query (optional)                |
| `iteration_id`    | TEXT FK | scope to iteration (optional)            |
| `execution_id`    | TEXT FK | scope to execution (optional)            |
| `created_at`      | INTEGER | epoch-ms                                 |

Set only the deepest applicable FK; the rest are derivable from joins.
Unscoped logs (all FKs null) are system-level.

### `search` — FTS5 virtual table

Full-text search over query text. Maintained by triggers on `query`.
