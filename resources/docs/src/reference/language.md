# Ubiquitous Language

Consistent terminology across code, docs, UI, logs, and conversations.

## Required Terms

| Term | Use for | Never use |
|------|---------|-----------|
| **conversation** | The product concept across web, TUI, Telegram, CLI | ~~session~~ |
| **turn** | Product-level ask+answer | (internal: `query` + `iteration`) |
| **tool** | An executable function in the sandbox | ~~capability~~ (as catch-all) |
| **skill** | A higher-level agent skill | ~~capability~~ |
| **channel** | `:vis`, `:telegram`, `:cli` | ~~adapter~~ (in user-facing context) |
| **environment** | The runtime map from `create-environment` | ~~env~~ (in public API) |

## Allowed Internal Terms

These are fine inside runtime code but not in user-facing surfaces:

| Term | Scope |
|------|-------|
| `query` | One user turn at the runtime level |
| `iteration` | One LLM round-trip inside a query |
| `env` | Local variable names in internal code |

## Reserved

| Term | Only when |
|------|-----------|
| `capability` / `capabilities` | An external provider/router API requires that word |

## `:vis/` Metadata & Marker Keys

All internal qualified keywords use the `:vis/` namespace.
The legacy `:rlm/` prefix was retired — do not reintroduce it.

### Value Metadata (attached to SCI values via `with-meta`)

| Key | Type | Purpose |
|-----|------|---------|
| `:vis/format` | `(fn [value] string)` | Formatting function — called at render time to produce the string shown in `<journal>`. Tools attach this so their output renders as a table, summary, etc. instead of raw `pr-str`. |
| `:vis/formatted` | `string` | Precomputed formatted output — takes priority over `:vis/format`. Avoids re-running the format fn on every render. |
| `:vis/def-source` | `string` | The original `(def ...)` source expression. Attached to SCI vars so `<var_index>` can show provenance even when the DB has no record yet. |

### Persistence Markers (stored in DB blobs via Nippy)

| Key | Value | Purpose |
|-----|-------|---------|
| `:vis/ref` | `:expr` | Placeholder for non-serializable values (fns, lazy seqs, SCI vars). At restore time, the `:expr` source code is re-evaluated in the sandbox to reconstruct the value. |
| `:vis/ref` | `:depth-exceeded` | Nested structure exceeded the serialization depth limit. |

### Iteration Eval Surrogates

| Key | Type | Purpose |
|-----|------|---------|
| `:vis/var-id` | `string` | Present in the result map of a `(def ...)` expression — carries the var name so the iteration loop can bind it in the sandbox. |
| `:vis/var-value` | `any` | The bound value accompanying `:vis/var-id`. |

### Error Types (in `ex-info` data maps)

| Key | Where |
|-----|-------|
| `:vis/invalid-db-spec` | `persistance.sqlite.core` — bad DB spec |
| `:vis/invalid-env` | `loop.core`, `query.core` — malformed environment map |
| `:vis/invalid-messages` | `query.core` — messages arg is not a vector |
| `:vis/invalid-eval-timeout` | `query.core` — eval-timeout out of range |
| `:vis/invalid-reasoning-level` | `loop.core`, `iteration.core` — unknown reasoning level |
| `:vis/missing-router` | `loop.core` — environment created without a router |
