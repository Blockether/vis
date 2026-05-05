# Vis

Vis is a from-the-ground-up coding agent inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang, Kraska & Khattab, 2025): instead of spending the context window on an ever-growing transcript, the model writes Clojure into a sandboxed [SCI](https://github.com/babashka/sci) runtime, keeps state in named vars and SQLite, and treats context as an external environment it can inspect and change.

Read the full guide in the mdBook: [docs/src/README.md](docs/src/README.md). Start with [Getting Started](docs/src/usage.md) for install layouts, fork/dev setup, auth, CLI, TUI, Telegram, and embedding.
