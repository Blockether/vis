## What is Vis

Vis is a coding agent with a different memory model. Instead of piling every
message into one growing chat transcript, it keeps its working state in a real
runtime — Python vars, a database, query results — that the model talks to
through code. The context window holds only what the model needs *right now*;
everything else is one call away. It is written in Clojure, ships as a single
native binary, and works with **any** text model.

## Why it's different

Every coding agent fights the same enemy: the context window fills up, gets
expensive, and eventually has to be compacted. What sets Vis apart is *who*
manages that window and *when*.

- **The engine owns the context, not the transcript.** Each step is tagged and
  addressable. At the start of a new turn, after the new intent is understood,
  completed earlier-turn work can collapse into a one-line summary
  (`session_fold`) or disappear from the wire (`session_fold` with no summary).

- **Compaction happens at a safe boundary, not under pressure.** Vis never emits
  a context-pressure nudge and rejects folds aimed at the current or a future
  turn. Reproduction output, reads, patch anchors, edits, and verification stay
  live until the turn completes.

- **The compression is structural, done by the same agent.** Because the agent
  that did the work is the one that folds it, the summary is written with full
  understanding of what mattered and what didn't — "http timeout fixed @
  src/vis/net/http.clj:52" (a full workspace-relative path, always clickable),
  not a lossy mechanical digest of raw bytes. Same agent, same
  task, a fundamentally different *view* of what the context should contain.

The payoff is cost. A long task might touch forty steps, but only a handful
stay "live" at any moment — the rest are folded to a sentence each. You pay for
a working set, not a full transcript, on every turn.

## Two gears: native tools and the Python sandbox

Vis is hybrid by design, and the two modes trade off directness against context
cost:

- **Native tools** — call a tool directly and its result comes straight back.
  Simple, low-latency, ideal for a quick read or a single edit on a small task.

- **The Python sandbox** — engine-bound native tools are also callables inside
  an embedded GraalPython runtime; native-only handlers are the exception. The agent writes Python that runs many tools,
  filters and chains their output, and `print()`s only the slice worth keeping.
  Ten file reads, one search, and a transform can happen in a single step — and
  the context only ever sees what the agent chose to print.

That second gear is where context utilization drops on advanced tasks: the raw
tool output lives in Python vars, never in the window, and the model decides
what surfaces.

Native contracts have one source: tool descriptions own routing and semantics; JSON Schemas own exact inputs. The agent discovers the live surface with `apropos` → `doc`, then follows `struct_index` → `struct_node` → `struct_patch` for supported code. See [Token optimization](token-optimization.md) and [Clojure extensions](extending.md#native-tool-contracts).

```text
        NATIVE TOOL                    PYTHON SANDBOX
   ┌───────────────────┐        ┌────────────────────────────┐
   │ cat(a)  ──► ctx    │        │ rows = [cat(f) for f in fs] │  20 files
   │ cat(b)  ──► ctx    │        │ hits = grep(rows, "TODO")   │  in vars
   │ cat(c)  ──► ctx    │        │ print(hits[:3])  ──► ctx    │  3 lines out
   └───────────────────┘        └────────────────────────────┘
     every result lands            the agent chooses what
     in the context window          reaches the context window
```

## Extensible

- **Python extensions.** Drop a `.py` file into `.vis/extensions/` to add
  project-local tools, prompts, and slash commands — no rebuild, `/reload`able
  in a live session. Vis can even **write these for itself**: when a task needs
  a tool it doesn't have, it authors one, reloads it, and keeps going.
- **Clojure extensions.** The full-surface path — new tools, channels,
  providers, slash commands, and doc pages — compiled into the binary.
- **Two runtimes.** Run it from source on the JVM, or build a GraalVM
  native-image and ship a single self-contained binary with no JVM install.

## Install

### Install from source manually (git clone over HTTPS)

Clone Vis over HTTPS, then symlink the launcher onto your `PATH`:

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
ln -sfn ~/.vis/sourcecode/bin/vis ~/.local/bin/vis
vis help
```

`~/.vis/sourcecode` is the default checkout path (`vis update` pulls it); `~/.local/bin/vis` is the launcher symlink. Both are configurable — set `VIS_SOURCE_DIR` and `VIS_LOCAL_BIN_DIR`, or clone anywhere and point the symlink at `<checkout>/bin/vis`. If `~/.local/bin` is not already on your `PATH`, add it:

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
```

To pin a branch or tag, add `--branch NAME` to the clone. To pull from a fork, clone its HTTPS URL instead of `Blockether/vis`. Update later with a plain `git pull` in the checkout, or `vis update`.

### One-liner installer

**macOS & Linux** (bash):

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

This clones Vis, verifies the runtime tools, and puts the `vis` launcher on your PATH. Then confirm:

```bash
vis help
```

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

> **JVM path (`--jvm`) JDK requirement.** Vis embeds GraalPy/Truffle pinned to a specific version (currently `25.1.3`). On the JVM you must run on **either** a stock (non-GraalVM) **JDK 25** — e.g. `sdk install java 25.0.3-tem` — **or** a GraalVM whose version matches the pinned line (`graalvm-community-jdk-25i1` / `graal-25.1.3`). Running on a *mismatched* GraalVM (e.g. GraalVM CE 25.0.2) puts that JDK's built-in Truffle on the path where it collides with the pinned one, and Vis aborts at session start with an actionable version-mismatch message. JDK 21–24 are too old (a dependency is compiled for Java 25).

## Features

- **Context as an environment.** The model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction.** Structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime.** An embedded GraalPython sandbox executes the agent's actions, a JVM core compiles to a native binary, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One binary.** Ships as a GraalVM native-image: fast startup, no JVM install required, with per-platform native distributions.
- **Model-agnostic.** Works with any text-based model. Nothing here depends on a specific provider's tools.

## Learn more

- **[Token optimization](token-optimization.md)**: the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)**: the in-process interpreter that executes the agent's actions.
- **[Process sandbox and gateway egress](sandbox.md)**: Seatbelt, filesystem/network policy, MITM, managed processes, trust boundaries, and verification.
- **[JVM & native-image](jvm-native-image.md)**: how the Clojure core becomes a standalone binary.
- **[Custom distributions](distributions.md)**: per-platform native artifacts and how they're built.
- **[Configuration](configuration.md)**: providers and models, system_prompt overrides, router tuning, the database.
- **[Python extensions](python-extensions.md)**: drop a `.py` file into `.vis/extensions/` — project-local tools, prompts, slash commands and guards, no rebuild, `/reload`able. Vis can write these for itself mid-session.
- **[Clojure extensions](extending.md)**: the full-surface path — new tools, channels, providers, slash commands and doc pages, compiled into the binary.
- **[Content-block protocol](content-blocks.md)**: the canonical role-labelled message, typed block, persistence, and streaming contract.

Vis can also answer these questions itself: ask a running `vis` how to configure or extend it and it reads these same pages through its `vis_docs` tool.
