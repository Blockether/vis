# Architecture Overview

Layer stack, top to bottom:

```
Channels (TUI, Telegram, CLI)            external surfaces
    ↓
Conversation layer                       lifecycle + per-conversation lock
    ↓
Environment                              SCI sandbox + extensions + var-index + DB handle
    ↓
Turn engine                              one user turn
    ↓
Iteration engine                         one LLM round-trip; assembles O(1) context
    ↓
Persistence                              single SQLite DB; versioned snapshots
```

## Layer responsibilities

**Channels** — external surfaces only. HTTP routes, terminal rendering,
bot polling, CLI argument parsing. No business logic. Each channel calls
into the conversation layer.

**Conversation Layer** — owns the in-process cache of live environments,
per-conversation locking, and the send → turn bridge. One conversation
= one environment = one SCI sandbox.

**Environment** — the runtime map representing one live conversation.
Holds the SCI sandbox, registered extensions, var-index cache, DB
handle, and router. See
[Environment Map](../extensions/environment.md) for every key.

**Turn Engine** — one user turn. Validates inputs, stores the turn
entity, enters the iteration loop, finalizes cost/duration/tokens.

**Iteration Engine** — one LLM round-trip. Assembles context (journal,
var-index, nudges, prior thinking), calls the LLM, executes code blocks
in SCI, persists results. Context is O(1) — never grows with iteration
count. The system prompt is built by `loop-core/assemble-system-prompt`
— single source of truth shared by both loop paths and the TUI inspector.

**Persistence** — single SQLite DB for everything. Every `(def ...)`,
every iteration, every thinking step persisted as versioned snapshots.
Full provenance for post-mortem and conversation resume.

## Evidence and completion model

Runtime work follows one dataflow:

```text
evidence producers -> journal refs -> diagnostics -> intent/plan/gate resolution -> final answer guard
```

Evidence producers include SCI eval blocks, tool results, `provider-limits` reports, and runtime snapshots. Diagnostic enrichers such as parse diagnosis and doctor checks explain evidence. Resolution state — intents, plans, gates, and proof slots — consumes observed canonical refs to decide done or impeded.

Do not model this as a separate proof layer. Proof is gate state plus cited provenance refs. See [Evidence, Diagnostics, and Resolution](evidence.md) and [Conversation Intents, Plans, and Gates](completion-contract.md).
