# Introduction

Vis is a from-the-ground-up coding agent inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang, Kraska & Khattab, 2025).
It works with any text-based model.

## Why Vis exists

Most coding agents accumulate work as an ever-growing message transcript.
Every observation, tool result, failed attempt, and summary competes for the same context window.
Eventually the system has to compact, summarize, slide the window, or drop history.
That makes long-running work fragile: state lives in tokens, and tokens are the thing being thrown away.

Vis takes the opposite route.
The context is an **external environment** the model interacts with through code.
The model writes Clojure, Vis executes it in a sandboxed [SCI](https://github.com/babashka/sci) interpreter, and the results flow back as a compact journal.
State lives in named vars and a SQLite database, not in the prompt.
No compaction, no sliding windows, no "summarize the last 50 messages" loop.

The design goal is not merely "LLM plus tools".
Tool calls are individual requests across a boundary; Vis gives the model a small programmable runtime where it can define names, inspect results, reuse state, and make progress through explicit evaluated steps.
Long-running work becomes a sequence of durable state transitions instead of a pile of chat messages.

The point of this book is to explain how to use Vis, how the runtime is shaped, and how to extend it without reverse-engineering the source tree.

Start here:

- [Getting Started](usage.md) — install, auth, CLI, TUI, Telegram, and embedding.
- [Architecture Overview](architecture/overview.md) — the runtime layers and ownership boundaries.
- [Packages](architecture/packages.md) — how the repo is split into jars/extensions.
- [Extension System](extensions/overview.md) — how tools and channels plug into Vis.

