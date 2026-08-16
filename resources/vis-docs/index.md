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
  (`fold_session`) or disappear from the wire (`fold_session` with no summary).

- **Compaction happens at a safe boundary, not under pressure.** Vis never emits
  a context-pressure nudge and rejects folds aimed at the current or a future
  turn. Reproduction output, reads, edits, and verification stay
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

## One gear: the Python sandbox

Vis hands the model exactly ONE tool — `python_execution` — and everything else is
a function inside it:

- **The sandbox** — every capability (`grep`, `cat`, `patch`, `struct_index`, `shell`, `attach`, an
  MCP tool) is a bare Python name in an embedded GraalPython runtime. The
  agent writes Python that runs many of them, filters and chains their output, and
  `print()`s only the slice worth keeping. Ten file reads, one search, and a
  transform happen in a single step — and the context only ever sees what the agent
  chose to print.

- **Why not many tools** — a JSON Schema per tool is a second, weaker copy of a
  Python signature, pushed into every request whether it is used or not. One tool
  means one schema, and a model that never has to guess which of eighteen doors to
  open.

That is where context utilization drops on advanced tasks: the raw tool output
lives in Python vars, never in the window, and the model decides what surfaces.

Contracts are PULLED, not pushed: `apropos(text)` is full-text search over every
function docstring, documentation page, skill body and MCP tool description, and
`doc(name)` returns the authoritative contract for one of them. From there,
`struct_index` → `struct_nodes` (a node's verbatim source + zipper cursor) →
`struct_patch` for supported code; `grep` → `cat` → `patch` for everything
addressed by line rather than by name. See [Token
optimization](token-optimization.md) and [Extending Vis](extending.md#one-tool-and-it-is-python_execution).

```text
      ONE TOOL PER RESULT                 PYTHON SANDBOX
   ┌───────────────────┐        ┌────────────────────────────┐
   │ read(a) ──► ctx    │        │ rows = [read(f) for f in fs]│  20 files
   │ read(b) ──► ctx    │        │ hits = [r for r in rows …]  │  in vars
   │ read(c) ──► ctx    │        │ print(hits[:3])  ──► ctx    │  3 lines out
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

The public command is **`vis-agent`**. The installer and the wrapper are served
as GitHub release assets, because `raw.githubusercontent.com` is unreachable on
many corporate networks. They ride the rolling `installer` release, refreshed by
every commit on `main` that touches them, so the one-liner is never older than
the branch. What they install is a different matter: no release tag is checked
out, because a published tag can carry broken source and a fix lands on the
branch first.

Install the command first; it fetches its own runtime:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
vis-agent help
```

The installer puts `vis-agent` in `~/.local/bin`, adds that directory to your
shell profile when PATH lacks it, and then runs `vis-agent update` so the
wrapper checks the source it owns out at the newest commit. That
runtime needs git. A JVM launch reuses a matching GraalVM CE 25.1.3 already installed (including through SDKMAN), or installs the pinned JDK automatically when no Java is available. Vis also installs the Clojure CLI automatically when the JVM runtime first needs it; set `VIS_NO_AUTO_INSTALL=1` to disable automatic tool installation.

Afterwards every runtime action belongs to vis-agent:

```bash
vis-agent runtime           # what runs, where it lives, what it is pinned to
vis-agent update            # command + runtime, moved to the newest commit
```

The installer only ever installs the wrapper; `vis-agent update` then acquires
the runtime — source checked out at the newest commit — so the wrapper and its
runtime cannot drift apart.

A native runtime still exists — release bundles for Linux x64/arm64 and macOS
arm64 carry it beside the command, and `vis-agent update --rebuild` builds one
from an installed source runtime (GraalVM CE 25.1.3, ≥16 GB RAM). The installer
never downloads one.

### Clojars packages

Every release deploys the whole monorepo to Clojars at one shared version: the
agent itself plus each channel, provider, language, foundation, and workspace
extension.

```clojure
;; deps.edn
{:deps {com.blockether/vis {:mvn/version "0.1.40"}}}
```

`com.blockether/vis` already depends on every bundled extension, so that single
coordinate gives the full agent. Depend on one package
(`com.blockether/vis-channel-tui`, `com.blockether/vis-provider-anthropic`,
`com.blockether/vis-language-python`, …) only when you embed a part of it.

## What runs: native, or the source Vis owns

There is nothing to select. How Vis was installed decides: the native sidecar
when one sits beside the command (a release bundle or the container image),
otherwise the JVM source Vis owns, pinned to main's newest commit.

```bash
vis-agent runtime           # what runs, where it lives, what the source is pinned to
vis-agent update            # move that runtime — and the command — to the newest
```

A launch with nothing installed stops with the command that fixes it — it never
silently picks another runtime.

`vis-agent update` updates the runtime that is installed: the newest release
bundle, or the checkout Vis owns (`~/.vis/install/src`) moved onto main's newest
commit. Naming a released `vX.Y.Z` takes that version instead of the newest;
anything else is refused. Full matrix:
[Runtime distributions](distributions.md).

There is no jar runtime; `target/vis.jar` exists only as an intermediate build
artifact. The JVM runtime means source plus Java 25 and the Clojure CLI; the
native runtime stays private behind the wrapper. GraalVM builds require
Community Edition 25.1.3 exactly and at least 16 GB RAM; 25.2.x is unsupported
because its native-image analysis does not converge within memory.

## Features

- **Context as an environment.** The model writes code to query its world and keeps state in named vars and a SQLite database, not in the token budget. It sees exactly what it needs; everything else is one call away.
- **Token-efficient by construction.** Structure is read before bytes, edits happen by name rather than by diff, and large intermediate values live in vars instead of the prompt.
- **A real runtime.** An embedded GraalPython sandbox executes the agent's actions, a JVM core can compile to a native runtime, and tree-sitter gives language-aware reading and editing across 30+ languages.
- **One stable command.** Every installation exposes the Bash `vis-agent` wrapper; native bundles include a private per-platform sidecar and require no JVM.
- **Model-agnostic.** Works with any text-based model. Nothing here depends on a specific provider's tools.

## Learn more

- **[Token optimization](token-optimization.md)**: the context-as-environment model and the tools that make it cheap.
- **[GraalPython sandbox](graalpython.md)**: the in-process interpreter that executes the agent's actions.
- **[Process jail and gateway egress](jail.md)**: Seatbelt, filesystem/network policy, MITM, managed processes, trust boundaries, and verification.
- **[JVM & native-image](jvm-native-image.md)**: how the Clojure core becomes the wrapper's private native runtime.
- **[Runtime distributions](distributions.md)**: wrapper bundles, runtime selection, and platform builds.
- **[Configuration](configuration.md)**: providers and models, system_prompt overrides, router tuning, the database.
- **[Extending Vis](extending.md)**: one guide to both flavors — drop-in [Python extensions](extending.md#python-extensions) (`.py` in `.vis/extensions/`, no rebuild, `/reload`able, Vis can write them for itself mid-session) and [Clojure extensions](extending.md#clojure-extensions) (the full-surface path: tools, channels, providers, slash commands, doc pages, compiled into the binary).
- **[Content-block protocol](content-blocks.md)**: the canonical role-labelled message, typed block, persistence, and streaming contract.
- **[Reporting bugs](reporting-bugs.md)**: filing an issue that is reproducible for us and safe for you — what to include, what never to paste, and how to sanitize a session export.

Vis can also answer these questions itself: ask a running `vis-agent` how to configure or extend it and it reads these same pages through `apropos(text)` and `doc(slug)` — they are documents in the same corpus as every function contract.
