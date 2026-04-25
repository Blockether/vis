# State Ownership

## Lifetime Table

| State | Location | Lifetime |
|-------|----------|----------|
| LLM Router | `query/core.clj :: router-atom` | Process |
| Conversation cache | `conversation/core.clj :: cache` | Process |
| SCI sandbox | `environment :sci-ctx` | Conversation |
| Extensions | `environment :extensions` | Conversation |
| Var-index cache | `environment :var-index-atom` | Conversation |
| QA corpus cache | `environment :qa-corpus-atom` | Conversation |
| Recursion depth | `environment :depth-atom` | Conversation |
| Iteration budget | `environment :max-iterations-atom` | Query |
| Current iteration ref | `environment :current-iteration-id-atom` | Query |
| Token usage | `usage-atom` (local in iteration-loop) | Query |
| Repetition counts | `call-counts-atom` (local in iteration-loop) | Query |

## Environment Map

The environment is the runtime map representing one live conversation.
See [Environment Map](../extensions/environment.md) for every key, its
type, and what you can/cannot do with it from extension code.

## Conversation Cache

`conversation/core.clj` maintains a process-level `(defonce cache (atom {}))`
mapping conversation ID strings to `{:env environment :lock Object}`.

- `ensure-env!` — find-or-create in cache
- `cache-env!` — insert into cache
- `close!` — dispose environment + remove from cache
- `close-all!` — dispose all + reset cache (process shutdown)

The per-conversation `:lock` object serializes `send!` calls — only one
turn runs at a time per conversation.
