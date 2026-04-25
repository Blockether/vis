<p align="center">
  <img src="logo.png" alt="Vis logo" width="200">
</p>

**A from-the-ground-up coding agent inspired by
[Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang,
Kraska & Khattab, 2025). Works with any text-based model.**

Vis takes a fundamentally different approach from current coding agent
harnesses. Instead of accumulating messages into an ever-growing context
window — then desperately compacting when it overflows — Vis treats the
context as an **external environment** the model interacts with through
code. The model writes Clojure, a sandboxed interpreter executes it,
and results flow back as a compact journal. State lives in named vars
and a SQLite DB, not in the token budget.

No compaction. No sliding windows. No "summarize the last 50 messages".
The model sees exactly what it needs: the previous iteration's results,
a var index of everything it has defined, and system nudges. Everything
else is one function call away.

## Quick Links

| | |
|---|---|
| **[Rationale](rationale.md)** | Why code-eval over tool-calls. Why SCI. What we learned. |
| **[Architecture](architecture/overview.md)** | How the layers fit together |
| **[Iteration Flow](architecture/iteration-flow.md)** | Step-by-step: message to answer |
| **[Extensions](extensions/overview.md)** | How to extend the agent with tools and nudges |
| **[Database](architecture/database.md)** | Entity tree and SQLite schema |

## Rules for Contributors

> Any change to source code that affects architecture, environment shape,
> extension spec, iteration flow, or public API **MUST** be accompanied by
> an update to these docs in the same commit. If docs and code diverge,
> code wins — fix the docs immediately.
