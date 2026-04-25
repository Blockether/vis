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
