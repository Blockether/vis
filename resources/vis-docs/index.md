## What is Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects and changes your project through tools. It's written in Clojure, ships as a single native binary, and works with any text-based model.

The core idea: the context window is an **environment** the model interacts with through code — not a transcript it has to carry. No compaction. No sliding windows. No "summarize the last 50 messages."

## Install

Clone once, put the launcher on `PATH`, then let `vis` install the right runtime for your machine.

```bash
git clone https://github.com/Blockether/vis.git
cd vis
echo 'export PATH="'"$PWD"'/bin:$PATH"' >> ~/.zshrc   # bash/zsh; or symlink bin/vis
exec $SHELL
vis update latest
vis help
```

Windows `cmd.exe` uses the same repo and launcher:

```bat
git clone https://github.com/Blockether/vis.git
cd vis
setx PATH "%CD%\bin;%PATH%"
vis update latest
vis help
```

**Prerequisites**

- Daily / native install: Git plus `curl` (macOS/Linux) or PowerShell (Windows).
- JVM / source fallback or `vis update <git-sha>`: [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ and a JRE/JDK.
- Building native locally: Oracle GraalVM or GraalVM CE 25+ with at least 16 GB RAM.

## Features

- **Context as an environment** — the model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction** — structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime** — an embedded GraalPython sandbox executes the agent's actions, a JVM core compiles to a native binary, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One binary** — ships as a GraalVM native-image: fast startup, no JVM install required, with per-platform native distributions.
- **Model-agnostic** — works with any text-based model. Nothing here depends on a specific provider's tools.

## How `vis` runs

`vis` is the stable command. It proxies to the best available distribution, in this order:

1. managed native binary from `vis update` (`$VIS_HOME/install`, default `~/.vis/install`)
2. repo native binary (`target/vis` or `target/vis.exe`)
3. repo JVM uberjar (`target/vis.jar`)
4. live source (`clojure -M:vis`)

Use `vis --jvm ...` to skip native and force the JVM path.

## Learn more

- **[Token optimization](token-optimization.md)** — the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)** — the in-process interpreter that executes the agent's actions.
- **[JVM & native-image](jvm-native-image.md)** — how the Clojure core becomes a standalone binary.
- **[Custom distributions](distributions.md)** — per-platform native artifacts and how they're built.
