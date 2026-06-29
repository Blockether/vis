## What is Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects and changes your project through tools. It's written in Clojure, ships as a single native binary, and works with any text-based model.

The core idea: **context is an environment the model interacts with through code — not a transcript it has to carry.** No compaction. No sliding windows. No "summarize the last 50 messages."

## Install

**macOS & Linux** (bash):

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

**Windows** (PowerShell):

```powershell
iwr https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source.ps1 -OutFile install-vis.ps1
powershell -ExecutionPolicy Bypass -File .\install-vis.ps1
```

(The PowerShell installer uses a `param()` block, so it runs as a file rather than piped — the first line downloads it, the second runs it with the execution policy lifted for that process.)

Both clone Vis, verify the runtime tools, and put the `vis` launcher on your PATH. Then confirm:

```bash
vis help
```

**Needs:** `java` 21+, the [Clojure CLI](https://clojure.org/guides/install_clojure), and `git`. The installer checks for them and tells you what's missing. These are required to **run** Vis; the native build (below) is what removes the JVM dependency for daily use.

**Update:** `vis update` does a fast-forward `git pull` of your source checkout — keeping you on the latest source. It does **not** fetch a binary.

## Native or JVM?

Vis runs in two builds. The launcher picks the best one it can find; you rarely choose.

| | **Native** (preferred) | **JVM** (fallback) |
|---|---|---|
| Startup | ~instant | a few seconds |
| Needs | nothing — single binary | Java 21+ and Clojure CLI |
| Where it comes from | you build it once (`vis native`) | the source checkout itself |
| Use when | everyday work | hacking on Vis, `--jvm`, or before you've built native |
| Force it | default if a binary is present | `vis --jvm …` |

`vis` falls back through, in order: a repo native binary (`target/vis`) → the repo uberjar (`target/vis.jar`) → live source (`clojure -M:vis`). Building the native binary needs Oracle GraalVM or GraalVM CE 25+ with ≥ 16 GB RAM — see **[Custom distributions](distributions.md)**.

## Features

- **Context as an environment** — the model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction** — structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime** — an embedded GraalPython sandbox executes the agent's actions, a JVM core compiles to a native binary, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One binary** — ships as a GraalVM native-image: fast startup, no JVM install required, with per-platform native distributions.
- **Model-agnostic** — works with any text-based model. Nothing here depends on a specific provider's tools.

## Learn more

- **[Token optimization](token-optimization.md)** — the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)** — the in-process interpreter that executes the agent's actions.
- **[JVM & native-image](jvm-native-image.md)** — how the Clojure core becomes a standalone binary.
- **[Custom distributions](distributions.md)** — per-platform native artifacts and how they're built.
