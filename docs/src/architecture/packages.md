# Packages

Vis is a polylith-style monorepo split into four host packages plus a
sibling tree of classpath plug-ins:

- `packages/` — the host. Four packages that every binary needs:
  - `packages/vis-persistance/` — backend-agnostic storage facade and
    Flyway-driven migration runner. Leaf package.
  - `packages/vis-extension/` — extension/channel/provider/CLI-command
    contracts plus the unified classpath discovery loader.
  - `packages/vis-runtime/` — the iteration loop, the SCI sandbox, the
    conversation lifecycle, the cross-channel state, and the public
    API facade.
  - `packages/vis-main/` — the `vis` binary's CLI surface: dispatcher,
    built-in commands (`run`, `auth`, `doctor`, `conversations`,
    `extensions list`), the one-shot agent helper, and the
    persistence-backed Telemere `:db` handler.
- `extensions/` — every classpath plug-in, grouped by surface
  category. Each category is a directory under `extensions/`; each
  package lives at `extensions/<category>/<pkg>/`.
  - `extensions/channels/` — channel front-ends (`vis-channel-tui`,
    `vis-channel-telegram`).
  - `extensions/providers/` — LLM provider integrations
    (`vis-provider-github-copilot`).
  - `extensions/persistance/` — persistence backends
    (`vis-persistance-sqlite`).
  - `extensions/common/` — SCI sandbox surface contributions used by
    the agent (`vis-common-meta` for self-introspection,
    `vis-common-editing` for filesystem and code-editing tools).
  Each package ships a `META-INF/vis-extension/vis.edn` and
  self-registers via `com.blockether.vis.core/register-global!`
  at namespace load. Every extension's `deps.edn` declares
  `:local/root "../../../packages/vis-runtime"`; vis-runtime transitively
  pulls vis-extension and vis-persistance. vis-runtime does not require
  any extension by namespace.
- `benchmarks/` — the benchmark harness (4clojure, HumanEval,
  SWE-bench Verified). NOT a classpath plug-in; pulled in via the
  `:bench` alias only.

The repo-root `deps.edn` is a thin aggregator: its top-level `:deps`
points at `vis-cli` (which transitively pulls every host package);
its `:vis` alias adds every classpath plug-in so a single
`clojure -M:vis` (or `bin/vis` once on `PATH`) has the whole product
on the classpath. Downstream consumers depend on whichever individual
package they need.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Package map

### Host packages

| Package | Path | Purpose | Notes |
| ------- | ---- | ------- | ----- |
| `vis-persistance` | `packages/vis-persistance/` | Backend-agnostic persistence facade and Flyway-driven migration runner. Defines `com.blockether.vis-persistance.{base,core,migration}` — the storage API every backend implements via `register-backend!`. | Leaf package: depends only on Clojure + `flyway-core`. No internal vis dependencies. Concrete backends live in `extensions/persistance/`. |
| `vis-extension` | `packages/vis-extension/` | Extension/channel/provider/CLI-command contracts plus the unified classpath discovery loader (`com.blockether.vis.core/discover-extensions!`). Owns `com.blockether.vis-extension.{extension,channel,provider,error,commandline.base}`. | Depends on `vis-persistance` because `register-global!` dispatches the `:ext/persistance` slot to `com.blockether.vis.core/register-backend!`. |
| `vis-runtime` | `packages/vis-runtime/` | The iteration loop, the SCI sandbox, the conversation lifecycle, the cross-channel state, and the public API facade. Public surface: `com.blockether.vis.core` (`create-environment`, `query!`, `register-extension!`, `assemble-system-prompt`, `dispose-environment!`). | Depends on `vis-extension` and `vis-persistance`. Every classpath plug-in depends on this package directly (and transitively pulls `vis-extension` + `vis-persistance`). The `META-INF/vis/VERSION` resource is stamped into this jar by `build.clj`. |
| `vis-cli` | `packages/vis-main/` | The `vis` binary. Owns the dispatcher (`commandline.main`), the built-in commands `run`/`auth`/`doctor`/`conversations`/`extensions list` (`channels.cli`), the one-shot agent helper (`channels.cli.agent`), and the persistence-backed Telemere `:db` handler (`logging`). Conversations-channel keyword: `:cli` (used by `vis run` agent runs; not a `channel/register-global!` registration). | Depends on `vis-runtime` (and transitively on `vis-extension` + `vis-persistance`). Ships its own `META-INF/vis-extension/vis.edn` so the dispatcher discovers itself and its built-in commands at boot. |

### Classpath plug-ins

| Package | Path | Purpose | Channel id | Notes |
| ------- | ---- | ------- | ---------- | ----- |
| `vis-persistance-sqlite` | `extensions/persistance/vis-persistance-sqlite/` | SQLite backend for the persistence facade. Owns the SQLite schema resources and JDBC stack: sqlite-jdbc, next.jdbc, HoneySQL, HikariCP, nippy, and the SQLite Flyway driver. Internal namespace: `com.blockether.vis.ext.persistance-sqlite.core`. | — | Depends on `vis-runtime`. Loading the namespace auto-registers the `:sqlite` backend with the persistence facade. The canonical SQLite schema resource ships from this package at `db/sqlite/migration/V1__schema.sql`. |
| `vis-provider-github-copilot` | `extensions/providers/vis-provider-github-copilot/` | GitHub Copilot OAuth device-flow provider. Internal namespace: `com.blockether.vis.ext.provider-github-copilot`. | — | Depends on `vis-runtime`. Ships a `META-INF/vis-extension/vis.edn` entry that registers the provider on startup. |
| `vis-channel-tui` | `extensions/channels/vis-channel-tui/` | Lanterna-based TUI chat UI. Internal namespace: `com.blockether.vis.ext.channel-tui.*`. | `:tui` | `:channel/owns-tty? true`. Conversations are stored under `:tui` (the same id used for CLI dispatch). |
| `vis-channel-telegram` | `extensions/channels/vis-channel-telegram/` | Telegram Bot API long-poll loop wired into `conversations/for-telegram-chat!`. Internal namespace: `com.blockether.vis.ext.channel-telegram.*`. | `:telegram` | Optional. Resolves into vis-runtime lazily via `requiring-resolve` — omitting the jar leaves the rest of vis usable. |
| `vis-common-meta` | `extensions/common/vis-common-meta/` | SCI sandbox introspection extension (`(meta/turn)`, `(meta/diagnose)`, etc.). Internal namespace: `com.blockether.vis.ext.common-meta.*`. | — | Pure `:ext/symbols` contribution. See [vis-common-meta](../extensions/common/vis-common-meta.md). |
| `vis-common-editing` | `extensions/common/vis-common-editing/` | Filesystem / shell / search tools exposed to the agent (`vis/rg`, `vis/patch`, etc.). Internal namespace: `com.blockether.vis.ext.common-editing.*`. | — | Pure `:ext/symbols` contribution. |

### Dev/research

| Package | Path | Purpose | Notes |
| ------- | ---- | ------- | ----- |
| `vis-benchmark` | `benchmarks/` | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working dirs under `benchmarks/data/`. | NOT loaded by the default product distribution. Pulled in via the repo-root `:bench` alias only. NOT a classpath plug-in (no `META-INF/vis-extension/vis.edn`). |

> **Two senses of "channel".** A *registered channel* (`:tui`,
> `:telegram`) is a CLI front-end registered through
> `com.blockether.vis.core/register-global!` and exposed under
> `vis channels <name>`. A *conversations channel* is the keyword
> stored in `conversation_soul.metadata.channel` — the namespace
> conversations are grouped under. Registered channels use the same
> keyword for both senses (`:tui`, `:telegram`). The CLI agent (in
> vis-main) is the lone exception: it doesn't register a channel
> descriptor (the `vis` dispatcher itself is its surface), but it
> writes its conversations under `:cli`.

## Auto-discovery

ONE classpath-scan mechanism, ONE resource per jar
(`META-INF/vis-extension/vis.edn`), ONE entry point on the **author**
side (`com.blockether.vis.core/register-global!`), ONE
loader on the **runtime** side
(`com.blockether.vis.core/discover-extensions!`). The
resource is a flat EDN vector of namespace symbols; the loader
`require`s every namespace exactly once, and each `register-global!`
call at namespace-load time lands the extension in the global
registry.

An extension is a single map; everything it contributes lives in a
slot keyed under `:ext/<surface>`. `register-global!` dispatches each
slot to the matching sub-registry as a side effect:

| Slot | Per-entry shape | Internal sub-registry |
| ---- | --------------- | --------------------- |
| `:ext/symbols` | `(ext/symbol …)` / `(ext/value …)` entries (SCI sandbox bindings) | Bound into the env's SCI namespace at `register-extension!` time |
| `:ext/cli` | `cmd/command` maps (`:cmd/name`, `:cmd/run-fn`, `:cmd/parent`, …) | `com.blockether.vis.core/register-global!` |
| `:ext/channels` | `channel/channel` maps (`:channel/id`, `:channel/cmd`, `:channel/main-fn`, …) | `com.blockether.vis.core/register-global!` |
| `:ext/providers` | `provider/provider` maps (`:provider/id`, `:provider/auth-fn`, …) | `com.blockether.vis.core/register-global!` |
| `:ext/persistance` | `{:persistance/id <kw> :persistance/ns <sym>}` entries | `com.blockether.vis.core/register-backend!` |

**Authors only call `ext/register-global!`.** They can require the
sub-registry namespaces directly for embedded / programmatic use, but
normal extension code treats those namespaces as implementation
details inside `vis-extension` and `vis-persistance`.

The loader is **type-agnostic**: it does not care which slots an
extension populates. New surfaces (themes, skills, anything future)
require zero loader changes — add a slot key to the extension spec,
ship a jar with a `META-INF/vis-extension/vis.edn`, populate the slot, done.

Drop a jar on the classpath that ships a
`META-INF/vis-extension/vis.edn` and every namespace inside is
auto-discovered at the next process boot — no edits to vis-runtime, no
`:require`s in user code. The CLI dispatcher calls
`discover-extensions!` once at `-main` startup; SDK callers that
bypass the CLI also get a lazy safety-net call from
`com.blockether.vis.core/create-environment` and from
`com.blockether.vis.core/db-create-connection!`.

## Dependency direction

Layered from leaf to binary:

```
packages/vis-persistance        (leaf — clojure + flyway-core only)
        ↑
packages/vis-extension          (depends on vis-persistance)
        ↑
packages/vis-runtime               (depends on vis-extension + vis-persistance)
        ↑
packages/vis-main                (depends on vis-runtime)
```

Every classpath plug-in (and the benchmark harness) depends on
vis-runtime and through it transitively pulls vis-extension and
vis-persistance:

```
extensions/channels/vis-channel-tui                     ─┐
extensions/channels/vis-channel-telegram                ─┤
extensions/persistance/vis-persistance-sqlite   ─┤
extensions/providers/vis-provider-github-copilot─┼─→ packages/vis-runtime
extensions/common/vis-common-meta                      ─┤
extensions/common/vis-common-editing                   ─┤
benchmarks/                                     ─┘
```

vis-runtime does not require any extension by namespace; channel
adapters that vis-runtime refers to in passing (e.g. the TUI screen
namespace) are resolved via `requiring-resolve` at runtime so the
hard dep stays one-way.

## Per-package source path

Host packages live at `packages/<pkg>/src/com/blockether/<pkg-with-underscores>/…`:

- `packages/vis-persistance/src/com/blockether/vis_persistance/{base,core,migration}.clj`
- `packages/vis-extension/src/com/blockether/vis_extension/{extension,channel,provider,error}.clj`
  plus `commandline/base.clj`
- `packages/vis-runtime/src/com/blockether/vis_runtime/…`  (`core`, `config`,
  `channels/{core,cancellation}`, and the entire `loop/…` subtree
  carrying the iteration runtime)
- `packages/vis-main/src/com/blockether/vis_main/{commandline/main,channels/cli,channels/cli/agent,logging}.clj`

Classpath plug-ins live under
`extensions/<category>/<pkg>/src/com/blockether/vis/…` and retain the
`com.blockether.vis.<surface>.<pkg>` ns convention (e.g.
`com.blockether.vis.ext.channel-tui.screen`,
`com.blockether.vis.ext.persistance-sqlite.core`,
`com.blockether.vis.ext.common-meta.core`). The benchmark harness lives
under `benchmarks/src/…`.

The full Clojure source is intentionally not enumerated here — use
`find packages extensions benchmarks -name '*.clj'` instead. The
architecture pages call out the namespaces that matter for each
subsystem (see [Iteration flow](iteration-flow.md),
[State ownership](state.md), [Channels](channels.md), and
[Database schema](database.md)).
