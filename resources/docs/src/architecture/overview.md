# Architecture Overview

```mermaid
graph TD
    subgraph Channels
        Web["Web (Jetty)"]
        TUI["TUI (Lanterna)"]
        Telegram["Telegram"]
        CLI["CLI"]
    end

    subgraph Conversation["Conversation Layer"]
        Conv["Lifecycle + locking<br/>one conversation = one environment"]
    end

    subgraph Environment["Environment"]
        Env["Runtime map for one live conversation"]
        SCI["SCI Sandbox<br/>deny-by-default execution"]
        Ext["Extensions<br/>only way to add capabilities"]
        VarIdx["Var Index<br/>persistent named state"]
    end

    subgraph Query["Query Engine"]
        QE["One user turn<br/>validate, persist, iterate"]
    end

    subgraph Iteration["Iteration Engine"]
        IE["One LLM round-trip<br/>context, ask, execute, persist"]
        Prompt["Prompt + Nudges<br/>O 1 context per iteration"]
    end

    subgraph Persistence["Persistence"]
        DB["Single SQLite DB<br/>versioned snapshots of everything"]
    end

    Web --> Conv
    TUI --> Conv
    Telegram --> Conv
    CLI --> Conv
    Conv --> Env
    Env --> QE
    QE --> IE
    IE --> Prompt
    IE --> DB
    Env --- SCI
    Env --- Ext
    Env --- VarIdx
```

## Layer Responsibilities

**Channels** — external surfaces only. HTTP routes, terminal rendering,
bot polling, CLI argument parsing. No business logic. Each channel calls
into the conversation layer.

**Conversation Layer** — owns the in-process cache of live environments,
per-conversation locking, and the send → query bridge. One conversation
= one environment = one SCI sandbox.

**Environment** — the runtime map representing one live conversation.
Holds the SCI sandbox, registered extensions, var-index cache, DB
handle, and router. See
[Environment Map](../extensions/environment.md) for every key.

**Query Engine** — one user turn. Validates inputs, stores the query
entity, enters the iteration loop, finalizes cost/duration/tokens.

**Iteration Engine** — one LLM round-trip. Assembles context (journal,
var-index, nudges, prior thinking), calls the LLM, executes code blocks
in SCI, persists results. Context is O(1) — never grows with iteration
count. The system prompt is built by `loop-core/assemble-system-prompt`
— single source of truth shared by both loop paths and the TUI inspector.

**Persistence** — single SQLite DB for everything. Every `(def ...)`,
every iteration, every thinking step persisted as versioned snapshots.
Full provenance for post-mortem and conversation resume.
