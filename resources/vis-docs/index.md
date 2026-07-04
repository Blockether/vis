## What is Vis

Other agents shovel every message into a growing window and panic-compact when it overflows. Vis keeps state in a real runtime the model talks to through code: vars, a database, query results. The window holds only what the model needs right now. **No emergency compaction. No sliding windows. Works with any text model.**

Concretely, Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects and changes your project through tools. It is written in Clojure and ships as a single native binary.

Vis also knows itself. Ask a running Vis what it can do and it describes its own tools and features by reading these same docs. And it doesn't just *use* extensions — it can **write** them: when a task needs a tool Vis doesn't have, it can author a Python extension, `/reload` it into the live session, and keep going without a restart. The agent grows its own surface mid-task.

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

Both clone Vis, verify the runtime tools, and put the `vis` launcher on your PATH. Then confirm:

```bash
vis help
```

On Windows, use the PowerShell command above: the bash installer creates unix symlinks and won't produce a working launcher under Git Bash. (WSL users: run the macOS/Linux command from inside WSL.)

**Needs:** `java` 21+, the [Clojure CLI](https://clojure.org/guides/install_clojure), and `git`. The installer checks for them and tells you what is missing. These are required to **run** Vis; the native build (below) is what removes the JVM dependency for daily use.

**Update:** `vis update` does a fast-forward `git pull` of your source checkout, keeping you on the latest source. It does **not** fetch a binary.

## Native or JVM?

Vis runs in two builds. The launcher picks the best one it can find; you rarely choose.

| | **Native** (preferred) | **JVM** (fallback) |
|---|---|---|
| Startup | ~instant | a few seconds |
| Needs | nothing, single binary | Java 21+ and Clojure CLI |
| Where it comes from | you build it once (`vis native`) | the source checkout itself |
| Use when | everyday work | hacking on Vis, `--jvm`, or before you've built native |
| Force it | default if a binary is present | `vis --jvm …` |

`vis` falls back through, in order: a repo native binary (`target/vis`), then the repo uberjar (`target/vis.jar`), then live source (`clojure -M:vis`). Building the native binary needs Oracle GraalVM or GraalVM CE 25+ with at least 16 GB RAM. See **[Custom distributions](distributions.md)**.

## Features

- **Context as an environment.** The model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction.** Structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime.** An embedded GraalPython sandbox executes the agent's actions, a JVM core compiles to a native binary, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One binary.** Ships as a GraalVM native-image: fast startup, no JVM install required, with per-platform native distributions.
- **Model-agnostic.** Works with any text-based model. Nothing here depends on a specific provider's tools.

## Learn more

- **[Token optimization](token-optimization.md)**: the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)**: the in-process interpreter that executes the agent's actions.
- **[JVM & native-image](jvm-native-image.md)**: how the Clojure core becomes a standalone binary.
- **[Custom distributions](distributions.md)**: per-platform native artifacts and how they're built.
- **[Configuration](configuration.md)**: providers and models, system-prompt overrides, router tuning, the database.
- **[Python extensions](python-extensions.md)**: drop a `.py` file into `.vis/extensions/` — project-local tools, prompts, slash commands and guards, no rebuild, `/reload`able. Vis can write these for itself mid-session.
- **[Clojure extensions](extending.md)**: the full-surface path — new tools, channels, providers, slash commands and doc pages, compiled into the binary.

Vis can also answer these questions itself: ask a running `vis` how to configure or extend it and it reads these same pages through its `vis_docs` tool.
