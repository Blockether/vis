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

Native contracts have one source: tool descriptions own routing and semantics; JSON Schemas own exact inputs. The agent discovers the live surface with `apropos` → `doc`, then follows `struct_index` → `struct_nodes` (a node's verbatim source + zipper cursor) → `struct_patch` for supported code. See [Token optimization](token-optimization.md) and [Extending Vis](extending.md#native-tool-contracts).

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

Three supported methods. Pick by your **network**, not by taste — each ends with `vis` on your `PATH`.

> **Corporate networks: use method 1 or 2.** The one-line installer downloads from
> `raw.githubusercontent.com`, which corporate proxies very commonly block; a hang or
> `curl: (22)/(35)` there is exactly that. Cloning over `github.com` and downloading a
> release asset both avoid that host entirely.

### 1. Install from source — git clone over HTTPS (recommended; corporate-safe)

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
~/.vis/sourcecode/bin/install-source   # checks java/clojure/git, symlinks ~/.local/bin/vis
vis help
```

`bin/install-source` is idempotent: on an existing checkout it fast-forwards it and re-points the symlink, so it is safe to rerun. Pure-manual equivalent, if you would rather not run the script:

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
ln -sfn ~/.vis/sourcecode/bin/vis ~/.local/bin/vis
vis help
```

`~/.vis/sourcecode` is the default checkout path (`vis update` pulls it); `~/.local/bin/vis` is the launcher symlink. Both are configurable — set `VIS_SOURCE_DIR` and `VIS_LOCAL_BIN_DIR`, pass `--dest`, or clone anywhere and point the symlink at `<checkout>/bin/vis`; the launcher resolves its repo through the symlink. If `~/.local/bin` is not already on your `PATH`, add it:

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
```

To pin a branch or tag, add `--branch NAME`. To use a fork, clone its HTTPS URL instead of `Blockether/vis`. Update later with `git pull` in the checkout, or `vis update`.

**Needs:** `java` 25+, the [Clojure CLI](https://clojure.org/guides/install_clojure), and `git`. The installer checks for them and tells you what is missing. These are required to **run** Vis from source; the native binary removes the JVM dependency for daily use.

### 2. Download a prebuilt native binary — no JVM, no Clojure CLI

Release assets are named `vis-<os>-<arch>-community` on the [Releases page](https://github.com/Blockether/vis/releases):

```bash
curl -fL -o ~/.local/bin/vis \
  https://github.com/Blockether/vis/releases/download/v0.1.13/vis-linux-arm64-community
chmod +x ~/.local/bin/vis
vis help
```

Releases currently carry **Linux x64 and arm64 only**, and not every tag carries both — open the tag and take the asset that is actually published there. There is no macOS job by design (free macOS runners have too little RAM, and GraalVM CE publishes no macOS-x64 JDK), so **on macOS install from source and build the binary once with `vis native`**. From a checkout, `vis update --native` fetches the same asset through `api.github.com`; when the release has nothing for your platform it says so and points at `vis native`.

### 3. One-liner installer (personal machines)

**macOS & Linux** (bash):

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

This is the same script as method 1 — it clones Vis, verifies the runtime tools, and puts the `vis` launcher on your PATH — only fetched over `raw.githubusercontent.com`. Then confirm:

```bash
vis help
```

### Hosts to allowlist

| Host | Needed for |
|---|---|
| `github.com` | `git clone`, release-asset downloads |
| `api.github.com` | `vis update` release lookup |
| `release-assets.githubusercontent.com` | release binary bytes (redirect target of a release download) |
| `repo1.maven.org`, `repo.clojars.org` | dependency resolution for source/JVM runs |
| `raw.githubusercontent.com` | **only** the one-liner installer (method 3) |
| your model provider's API | running the agent |

**Update:** `vis update` fetches and fast-forwards your source checkout, keeping you on the latest source. It does **not** fetch a binary. If your branch has diverged from its upstream (usually an upstream force-push) it prints how far each side moved and stops; `vis update --reset` then hard-resets onto the upstream and prints the old HEAD so `git reset --hard <sha>` can bring your commits back. With uncommitted changes it refuses either way — commit or stash first.

## Native or JVM?

Vis runs in two builds. The launcher picks the best one it can find; you rarely choose.

| | **Native** (preferred) | **JVM** (fallback) |
|---|---|---|
| Startup | ~instant | a few seconds |
| Needs | nothing, single binary | Java 25+ and Clojure CLI |
| Where it comes from | you build it once (`vis native`) | the source checkout itself |
| Use when | everyday work | hacking on Vis, `--source`, or before you've built native |
| Force it | `vis --native …` (default if a binary is present) | `vis --source …` (`--jvm` is the old alias) |

`vis` falls back through, in order: **live source inside a Vis checkout** (`clojure -M:vis`, so working-tree edits always win — `VIS_PREBUILT=1` opts out), then a managed native binary from `vis update` (`$VIS_HOME/install`), then a repo native binary (`target/vis`), then live source. The uberjar (`target/vis.jar`) is never auto-selected — ask for it with `vis --jar …`. Building the native binary needs GraalVM CE 25.2.4 (exactly, see `.graalvm-version`) with at least 16 GB RAM. See **[Custom distributions](distributions.md)**.

> **JVM path (`--jvm`) JDK requirement.** Vis embeds GraalPy/Truffle pinned to a specific version (currently `25.2.4`). On the JVM you must run on **either** a stock (non-GraalVM) **JDK 25** — e.g. `sdk install java 25.0.3-tem` — **or** a GraalVM whose version matches the pinned line (`graalvm-community-jdk-25i2` / `graal-25.2.4`). Running on a *mismatched* GraalVM (e.g. GraalVM CE 25.0.2) puts that JDK's built-in Truffle on the path where it collides with the pinned one, and Vis aborts at session start with an actionable version-mismatch message. JDK 21–24 are too old (a dependency is compiled for Java 25).

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
- **[Extending Vis](extending.md)**: one guide to both flavors — drop-in [Python extensions](extending.md#python-extensions) (`.py` in `.vis/extensions/`, no rebuild, `/reload`able, Vis can write them for itself mid-session) and [Clojure extensions](extending.md#clojure-extensions) (the full-surface path: tools, channels, providers, slash commands, doc pages, compiled into the binary).
- **[Content-block protocol](content-blocks.md)**: the canonical role-labelled message, typed block, persistence, and streaming contract.
- **[Reporting bugs](reporting-bugs.md)**: filing an issue that is reproducible for us and safe for you — what to include, what never to paste, and how to sanitize a session export.

Vis can also answer these questions itself: ask a running `vis` how to configure or extend it and it reads these same pages through its `vis_docs` tool.
