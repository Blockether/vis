## What is Vis

Vis is a coding agent with a different memory model. Instead of piling every
message into one growing chat transcript, it keeps its working state in a real
runtime — Python vars, a database, query results — that the model talks to
through code. The context window holds only what the model needs *right now*;
everything else is one call away. It is written in Clojure, runs through one
Bash wrapper with native and JVM runtime options, and works with **any** text model.

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
- **One wrapper, two runtimes.** `vis-agent` always remains the command. It can
  launch live JVM source or a private GraalVM native sidecar.

## Install

The public command is **`vis-agent`**, not `vis`. Linux already has an unrelated
`vis` utility, so the shorter name is intentionally left alone.

Every installation includes the Bash wrapper. A native release is not installed
as a standalone Vis executable: its archive contains `vis-agent` plus the private
`vis-agent-native` runtime sidecar.

### Native release bundle

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-native | bash
vis-agent help
```

The installer resolves `vis-agent-<os>-<arch>-community.tar.gz`, installs both
files in `~/.local/bin`, persists native as the selected runtime, and adds the
directory to your shell profile when PATH does not already contain it. New shells
can therefore invoke `vis-agent` globally from any working directory.

For an all-users installation:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-native -o /tmp/install-vis-agent
sudo bash /tmp/install-vis-agent --install-dir /usr/local/bin
```

Release CI currently publishes Linux x64 and arm64 bundles. On an unsupported
platform, install the JVM source runtime. A maintainer can still build the private
native sidecar with `vis-agent native` when GraalVM CE 25.1.3 and enough RAM are
available.

### JVM source distribution

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
~/.vis/sourcecode/bin/install-source
vis-agent help
```

`bin/install-source` is idempotent. It checks Java 25+, the Clojure CLI, and git;
fast-forwards an existing checkout; copies the public wrapper to
`~/.local/bin/vis-agent`; records the source checkout under `~/.vis/source-dir`;
and updates the shell profile when needed. `--dest PATH` and `--install-dir PATH`
override those locations.

The one-line equivalent is:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

Corporate proxies commonly block `raw.githubusercontent.com`; cloning from
`github.com` and running the checked-out script avoids that host.

### Hosts to allowlist

| Host | Needed for |
|---|---|
| `github.com` | git clone and release bytes |
| `api.github.com` | release resolution for install/update |
| `release-assets.githubusercontent.com` | release bundle bytes |
| `repo1.maven.org`, `repo.clojars.org` | JVM/source dependency resolution |
| `raw.githubusercontent.com` | one-line installers only |
| your model provider's API | running the agent |

## Choose native or JVM source

The wrapper owns runtime selection:

```bash
vis-agent runtime show
vis-agent runtime use native
vis-agent runtime use jvm

vis-agent --native help       # one-launch override
vis-agent --jvm help          # one-launch override; --source is an alias
```

A persisted choice wins. Without one, source wins while developing inside a Vis
checkout; elsewhere the installed native sidecar wins when present, with JVM
source as the fallback. If the selected runtime is unavailable, the wrapper
stops and explains how to install or switch—it never silently picks another.

There is no jar distribution and no `--jar` selector. `target/vis.jar` exists only
as an intermediate build artifact. The JVM runtime means live source plus Java
25 and the Clojure CLI; the native runtime remains private behind the wrapper.
GraalVM builds require Community Edition 25.1.3 exactly and at least 16 GB RAM;
25.2.x is unsupported because its native-image analysis does not converge within
memory.

## Features

- **Context as an environment.** The model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction.** Structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime.** An embedded GraalPython sandbox executes the agent's actions, a JVM core can compile to a native runtime, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One stable command.** Every installation exposes the Bash `vis-agent` wrapper; native bundles include a private per-platform sidecar and require no JVM.
- **Model-agnostic.** Works with any text-based model. Nothing here depends on a specific provider's tools.

## Learn more

- **[Token optimization](token-optimization.md)**: the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)**: the in-process interpreter that executes the agent's actions.
- **[Process sandbox and gateway egress](sandbox.md)**: Seatbelt, filesystem/network policy, MITM, managed processes, trust boundaries, and verification.
- **[JVM & native-image](jvm-native-image.md)**: how the Clojure core becomes the wrapper's private native runtime.
- **[Runtime distributions](distributions.md)**: wrapper bundles, runtime selection, and platform builds.
- **[Configuration](configuration.md)**: providers and models, system_prompt overrides, router tuning, the database.
- **[Extending Vis](extending.md)**: one guide to both flavors — drop-in [Python extensions](extending.md#python-extensions) (`.py` in `.vis/extensions/`, no rebuild, `/reload`able, Vis can write them for itself mid-session) and [Clojure extensions](extending.md#clojure-extensions) (the full-surface path: tools, channels, providers, slash commands, doc pages, compiled into the binary).
- **[Content-block protocol](content-blocks.md)**: the canonical role-labelled message, typed block, persistence, and streaming contract.
- **[Reporting bugs](reporting-bugs.md)**: filing an issue that is reproducible for us and safe for you — what to include, what never to paste, and how to sanitize a session export.

Vis can also answer these questions itself: ask a running `vis-agent` how to configure or extend it and it reads these same pages through its `vis_docs` tool.
