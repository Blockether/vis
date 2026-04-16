# Conversation-Centric Functional Architecture Refactor

## Objective

- remove `session` from product and code vocabulary
- replace catch-all `capability` language with explicit `tool` or `skill`
- organize namespaces by bounded context and layer
- treat conversations as an RLM subcontext, not a top-level bounded context separate from RLM
- move long-term surface code under `adapters/*`
- keep the migration incremental and shippable

## Execution Status (2026-04-16)

### Completed

- moved web surface to `adapters/web/*` and introduced `adapters.web.conversations`
- moved TUI surface to `adapters/tui/*`
- moved Telegram surface to `adapters/telegram/*`
- moved CLI entrypoint to `adapters/cli.clj`
- folded `agent.clj` into CLI — top-level file deleted
- split conversation ownership into:
  - `rlm/conversations/shared.clj`
  - `rlm/conversations/core.clj`
  - `rlm/conversations/persistence.clj`
- deleted top-level `conversations.clj` and `conversations/schema.clj`
- removed `requiring-resolve` wiring from web adapter path
- moved runtime persistence modules:
  - `rlm/db.clj` -> `rlm/persistence/db.clj`
  - `rlm/sqlite.clj` -> `rlm/persistence/sqlite.clj`
  - `rlm/schema.clj` -> `rlm/persistence/schema.clj`
  - `rlm/trajectory.clj` -> `rlm/persistence/trajectory.clj`
- moved corpus modules:
  - `rlm/git.clj` -> `rlm/corpus/git.clj`
  - `rlm/pageindex*.clj` -> `rlm/corpus/pageindex*`
  - `rlm/qa.clj` -> `rlm/corpus/qa.clj`
  - `rlm/data.clj` -> `rlm/corpus/data.clj`
- split `rlm/tools.clj` into `rlm/tools/*`:
  - `rlm/tools/shared.clj` — helpers, EXTRA_BINDINGS, date + document helpers, document/citation tool factories
  - `rlm/tools/conversation.clj` — conversation-history / conversation-code / conversation-results factories
  - `rlm/tools/restore.clj` — restore-var / restore-vars factories
  - `rlm/tools/git.clj` — git-* SCI bindings + JGit wrappers
  - `rlm/tools/sci.clj` — SCI sandbox creation, var-index, hook system
- swept CLAUDE.md + plan so namespace paths match 1:1 with reality

## Ubiquitous Language

- `conversation` — top-level product concept across web, TUI, Telegram, and CLI
- `turn` — one product-level ask+answer
- `query` — internal RLM record for one turn
- `iteration` — one runtime reasoning/code step inside a query
- `tool` — executable function exposed to the runtime
- `skill` — procedural workflow/document loaded by the runtime
- `channel` — source partition such as `:vis`, `:telegram`, `:cli`
- `runtime env` — technical RLM object, never a user-facing concept

## Terms To Kill

- `session` -> `conversation`
- `capability` -> `tool` or `skill`
- keep `capability` / `capabilities` only where an external provider/router API requires that word

## Bounded Contexts

1. `RLM`
Owns env, conversations, queries, iterations, trace, routing, execution, and runtime persistence.

2. `Corpus`
Owns documents, pages, nodes, TOC, entities, relationships, and git ingest.

3. `Tools`
Owns executable tool bindings, wrappers, and tool-facing runtime helpers.

4. `Skills`
Owns skill discovery, parsing, ingest, loading, and mutation flows.

5. `Surface Adapters`
Own web, TUI, Telegram, and CLI entrypoints. They compose bounded contexts; they do not define domain language.

## Namespace Layers

### `*.shared`

Reusable functions for one bounded context.

### `*.core`

Use cases and orchestration for one bounded context.

### `*.persistence*`

Storage/schema/DB boundary for one bounded context.

### `*.presentation*`

Pure rendering/view-model formatting for one bounded context or adapter.

### `adapters.*`

External surface code only: web, TUI, Telegram, and CLI.

### Facades

Thin public entrypoints such as `rlm`.

## Canonical Target Structure

```text
src/com/blockether/vis/
├── config.clj
├── adapters/
│   ├── cli.clj
│   ├── telegram/
│   │   ├── api.clj
│   │   └── bot.clj
│   ├── tui/
│   │   ├── chat.clj
│   │   ├── dialogs.clj
│   │   ├── input.clj
│   │   ├── primitives.clj
│   │   ├── provider.clj
│   │   ├── render.clj
│   │   ├── screen.clj
│   │   ├── state.clj
│   │   └── theme.clj
│   └── web/
│       ├── app.clj
│       ├── conversations.clj
│       ├── executor.clj
│       ├── routes.clj
│       ├── presentation.clj
│       └── presentation/
│           ├── input_bar.clj
│           ├── message.clj
│           ├── sheet.clj
│           ├── sidebar.clj
│           ├── tool_render.clj
│           └── topbar.clj
├── logging.clj
├── redact.clj
├── rlm.clj
├── rlm/
│   ├── env.clj
│   ├── shared.clj
│   ├── core.clj
│   ├── conversations/
│   │   ├── shared.clj
│   │   ├── core.clj
│   │   └── persistence.clj
│   ├── batch.clj
│   ├── concurrency.clj
│   ├── routing.clj
│   ├── sub.clj
│   ├── paren_repair.clj
│   ├── persistence/
│   │   ├── db.clj
│   │   ├── schema.clj
│   │   ├── sqlite.clj
│   │   └── trajectory.clj
│   ├── corpus/
│   │   ├── data.clj
│   │   ├── git.clj
│   │   ├── pageindex.clj
│   │   ├── pageindex/
│   │   │   ├── markdown.clj
│   │   │   ├── pdf.clj
│   │   │   └── vision.clj
│   │   └── qa.clj
│   ├── tools/
│   │   ├── shared.clj
│   │   ├── conversation.clj
│   │   ├── git.clj
│   │   ├── restore.clj
│   │   └── sci.clj
│   └── skills/
│       └── core.clj
└── languages/commons/
    ├── edit.clj
    ├── list.clj
    ├── read.clj
    └── write.clj
```

## Runtime Ownership

- `config.clj`
Owns config and router construction only.

- `rlm.clj`
Thin public facade over RLM internals.

- `rlm/env.clj`
Owns runtime env construction and state.

- `rlm/shared.clj`
Owns reusable runtime/kernel functions.

- `rlm/core.clj`
Owns top-level RLM use cases and orchestration.

- `rlm/conversations/shared.clj`
Owns reusable conversation functions inside RLM.

- `rlm/conversations/core.clj`
Owns conversation lifecycle, locking, send orchestration, and vis-facing conversation workflows inside RLM.

- `rlm/conversations/persistence.clj`
Owns conversation persistence, including the vis sidecar metadata needed for channel/external-id/title.

- `rlm/persistence/*`
Owns runtime storage contracts and DB access.

- `rlm/corpus/*`
Owns corpus modules.

- `rlm/tools/*`
Owns tool-surface assembly and helper modules.

- `rlm/skills/*`
Owns skills subsystem.

## Adapter Ownership

- `adapters/web/*`
HTTP/process wiring and web-specific projections.

- `adapters/web/presentation*`
Pure web rendering only.

- `adapters/tui/*`
TUI-specific surface code.

- `adapters/telegram/*`
Telegram-specific surface code.

- `adapters/cli.clj`
CLI adapter.

- `agent.clj`
Not a separate adapter. It is CLI-owned helper code and should be folded into CLI or deleted.

## Web Target

- `adapters.web.app`
Boot only.

- `adapters.web.routes`
HTTP only. No DB/env poking.

- `adapters.web.conversations`
Deep web-facing module for conversation list/page/title/projection/cache.

- `adapters.web.executor`
Async turn execution and live progress.

- `adapters.web.presentation*`
Pure rendering only.

## Migration Map

- `src/com/blockether/vis/conversations.clj` -> split into:
  `src/com/blockether/vis/rlm/conversations/shared.clj`
  `src/com/blockether/vis/rlm/conversations/core.clj`
  `src/com/blockether/vis/rlm/conversations/persistence.clj`
- `src/com/blockether/vis/conversations/schema.clj` -> fold into RLM conversation persistence
- `src/com/blockether/vis/web/server.clj` -> split into:
  `src/com/blockether/vis/adapters/web/app.clj`
  `src/com/blockether/vis/adapters/web/conversations.clj`
  `src/com/blockether/vis/adapters/web/presentation.clj` and `presentation/*`
- `src/com/blockether/vis/web/*` -> `src/com/blockether/vis/adapters/web/*`
- `src/com/blockether/vis/tui/*` -> `src/com/blockether/vis/adapters/tui/*`
- `src/com/blockether/vis/telegram/*` -> `src/com/blockether/vis/adapters/telegram/*`
- `src/com/blockether/vis/cli.clj` -> `src/com/blockether/vis/adapters/cli.clj`
- `src/com/blockether/vis/agent.clj` -> fold into CLI-owned code or delete
- `src/com/blockether/vis/rlm/db.clj` -> `src/com/blockether/vis/rlm/persistence/db.clj`
- `src/com/blockether/vis/rlm/sqlite.clj` -> `src/com/blockether/vis/rlm/persistence/sqlite.clj`
- `src/com/blockether/vis/rlm/schema.clj` -> `src/com/blockether/vis/rlm/persistence/schema.clj`
- `src/com/blockether/vis/rlm/trajectory.clj` -> `src/com/blockether/vis/rlm/persistence/trajectory.clj`
- `src/com/blockether/vis/rlm/git.clj` -> `src/com/blockether/vis/rlm/corpus/git.clj`
- `src/com/blockether/vis/rlm/pageindex*.clj` -> `src/com/blockether/vis/rlm/corpus/pageindex*`
- `src/com/blockether/vis/rlm/qa.clj` -> `src/com/blockether/vis/rlm/corpus/qa.clj`
- `src/com/blockether/vis/rlm/data.clj` -> `src/com/blockether/vis/rlm/corpus/data.clj`
- `src/com/blockether/vis/rlm/tools.clj` -> split into:
  `src/com/blockether/vis/rlm/tools/shared.clj`
  `src/com/blockether/vis/rlm/tools/sci.clj`
  `src/com/blockether/vis/rlm/tools/conversation.clj`
  `src/com/blockether/vis/rlm/tools/git.clj`
  `src/com/blockether/vis/rlm/tools/restore.clj`
- `session-history` / `session-code` / `session-results` -> `conversation-history` / `conversation-code` / `conversation-results`

## Migration Phases

### Phase 1 — Freeze Language

- stop adding new `session` names
- stop using catch-all `capability` for tool/skill behavior
- rename obvious locals and docs in touched files

### Phase 2 — Introduce Canonical Layers

- introduce `rlm/conversations/shared.clj`, `rlm/conversations/core.clj`, and `rlm/conversations/persistence.clj`
- introduce adapter paths under `adapters/*`
- introduce `presentation*` for render-only namespaces

### Phase 3 — Move Web To Target Shape

- split `web/server.clj`
- move web modules under `adapters/web/*`
- move presenters under `adapters/web/presentation*`
- rename routes and UI copy from `session` to `conversation`

### Phase 4 — Move Runtime Modules

- move DB/schema modules under `rlm/persistence/*`
- move corpus modules under `rlm/corpus/*`
- split `rlm/tools.clj`
- move conversation lifecycle/persistence under `rlm.conversations.*`

### Phase 5 — Remove Transitional Facades

- fold `agent.clj` into CLI-owned code or delete it
- delete top-level `conversations.clj` once callers use `rlm.conversations.*` (done)
- remove stale route names, stale namespace aliases, and `requiring-resolve` shims

## Refactor Rules

- no big-bang namespace shuffle
- one concept per slice: extract first, rename second, move callers third
- prefer deep modules over many shallow helpers
- if a route or presentation module needs raw env access, the boundary is wrong
- do not add backward-compat aliases unless there is a concrete external consumer
- update docs and names in the same slice as code moves

## Done Means

- no new `session` names remain in product-facing code or docs
- `tool` and `skill` are used consistently
- namespace names clearly express `shared`, `core`, `persistence`, `presentation`, and `adapters`
- conversations are owned inside `RLM`, not as a separate top-level bounded context
- adapter code lives under `adapters/*`
- presentation code is pure and does not reach into env/DB directly
