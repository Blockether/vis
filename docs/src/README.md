<p align="center">
  <img src="logo.png" alt="Vis logo" width="200">
</p>

# Vis

**A from-the-ground-up coding agent inspired by Recursive Language Models.
Works with any text-based model.**

Vis treats the context window as an **external environment** the model
interacts with through code, rather than an ever-growing message log that must
be compacted when it overflows. The model writes Clojure, a sandboxed
interpreter executes it, and results flow back as a compact journal. State
lives in named vars and a SQLite DB, not in the token budget.

No compaction. No sliding windows. The model sees what it needs: the previous
iteration's results, a var index of everything it has defined, and system
nudges. Everything else is one function call away.

## This site

- **[Gateway](GATEWAY.md)** — the HTTP / SSE API for driving Vis as a service.
- **[Workspaces](WORKSPACES.md)** — copy-on-write workspace capabilities.
- **[GraalVM native-image](NATIVE_IMAGE.md)** — building and running the native binary.
- **[maki-style redesign](MAKI_REDESIGN_SPEC.md)** — execution-engine design notes.
- **[TUI: trace toggle & jump](tui-trace-toggle-jump.md)** — terminal UI behavior.
