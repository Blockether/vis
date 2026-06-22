## Why Vis

Most coding agents accumulate every message into an ever-growing context window, then scramble to compact it when it overflows. Vis takes the opposite stance: the context window is an **external environment** the model interacts with through code — not a transcript it has to carry.

The model writes Clojure. A sandboxed interpreter runs it. Results flow back as a compact journal. State lives in named vars and a SQLite database, not in the token budget.

> No compaction. No sliding windows. No "summarize the last 50 messages." The model sees exactly what it needs — the previous iteration's results, a var index of everything it has defined, and system nudges. Everything else is one function call away.

## What makes it different

- **Token efficiency by construction** — the agent reads structure before bytes, edits by name instead of by diff, and keeps large intermediate values in vars rather than the prompt.
- **A real runtime** — embedded GraalPython for the sandbox, a JVM core compiled to a native binary, and tree-sitter for language-aware reading and editing across 30+ languages.
- **One binary** — ships as a GraalVM native-image: fast startup, no JVM install, per-platform native distributions.
- **Model-agnostic** — works with any text-based model; nothing here depends on a specific provider's tools.

## Start here

- **[Token optimization](token-optimization.md)** — the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)** — the in-process interpreter that executes the agent's actions.
- **[JVM & native-image](jvm-native-image.md)** — how the Clojure core becomes a standalone binary.
- **[Custom distributions](distributions.md)** — per-platform native artifacts and how they're built.
