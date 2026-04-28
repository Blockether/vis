# Packages

Vis is a polylith-style monorepo with one host package and a sibling
tree of classpath plug-ins:

- **Repo root (`src/`, `test/`, `resources/`)** — vis-core itself.
  vis-core IS the project: the host runtime, the SCI sandbox, the
  iteration loop, the CLI dispatcher, the extension/channel/provider
  registries, and the persistence facade. The root `deps.edn`
  carries vis-core's library deps and is what `build.clj` copies
  into the published `com.blockether/vis-core` POM. Aliases (`:vis`,
  `:dev`, `:test`, `:bench`) pull in optional plug-ins for
  development and the binary; aliases never reach the published POM.
- `extensions/` — every classpath plug-in, grouped by surface
  category. Each category is a directory under `extensions/`; each
  package lives at `extensions/<category>/<pkg>/`.
  - `extensions/channels/` — channel front-ends (e.g. `vis-tui`,
    `vis-telegram`).
  - `extensions/providers/` — LLM provider integrations (e.g.
    `vis-provider-github-copilot`).
  - `extensions/persistance/` — persistence backends (e.g.
    `vis-persistance-sqlite`).
  - `extensions/common/` — SCI sandbox surface contributions used
    by the agent (`vis-meta` for self-introspection, `vis-editing`
    for filesystem and code-editing tools).
  Each package ships a `META-INF/vis-extension/vis.edn` and
  self-registers via `com.blockether.vis.extension/register-global!`
  at namespace load. Every extension's `deps.edn` declares
  `:local/root "../../.."` against vis-core (i.e. the project root
  three levels up). vis-core does not require any extension by
  namespace.
- `benchmarks/` — the benchmark harness (4clojure, HumanEval,
  SWE-bench Verified). NOT a classpath plug-in; pulled in via the
  `:bench` alias only.

A single `clojure -M:vis` (or `bin/vis` once on `PATH`) from the repo
root has the whole product on the classpath; downstream consumers
only depend on the packages they need.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Package map

| Package | Path | Purpose | Channel id | Notes |
| ------- | ---- | ------- | ---------- | ----- |
| `vis-core` | repo root (`src/`, `test/`, `resources/`) | Public API facade, runtime loop, query/iteration engine, iteration response specs, SCI sandbox, conversation lifecycle, CLI dispatcher, command registry, extension registry, channel registry bridge, provider registry, persistence facade, migration runner, and Telemere logging setup. | `:cli` | The only package extension authors and runtime consumers must depend on directly. Namespaces such as `com.blockether.vis.extension`, `com.blockether.vis.commandline.base`, `com.blockether.vis.channel`, `com.blockether.vis.provider`, `com.blockether.vis.persistance.core`, and `com.blockether.vis.logging` are shipped from this jar. The `:cli` channel id is what the CLI agent's `conversations/create!` uses; it is not a `channel/register-global!` registration. |
| `vis-persistance-sqlite` | `extensions/persistance/vis-persistance-sqlite` | SQLite backend for the persistence facade. Owns the SQLite schema resources and JDBC stack: sqlite-jdbc, next.jdbc, HoneySQL, HikariCP, nippy, and the SQLite Flyway driver. | — | Depends on `vis-core`. Loading `com.blockether.vis.persistance.sqlite.core` auto-registers the `:sqlite` backend with the facade. The canonical SQLite schema resource ships from this package at `db/sqlite/migration/V1__schema.sql`. |
| `vis-provider-github-copilot` | `extensions/providers/vis-provider-github-copilot` | GitHub Copilot OAuth device-flow provider. | — | Depends on `vis-core` for the extension and provider registries. Ships a `META-INF/vis-extension/vis.edn` entry that registers the provider on startup. |
| `vis-tui` | `extensions/channels/vis-tui` | Lanterna-based TUI chat UI. | `:tui` | `:channel/owns-tty? true`. Uses `:vis` as its conversations-channel namespace (i.e. `(conversations/create! :vis)`). |
| `vis-telegram` | `extensions/channels/vis-telegram` | Telegram Bot API long-poll loop wired into `conversations/for-telegram-chat!`. | `:telegram` | Optional. Resolves into `vis-core` lazily via `requiring-resolve` — omitting the jar leaves the rest of vis usable. |
| `vis-meta` | `extensions/common/vis-meta` | SCI sandbox introspection extension (`(meta/turn)`, `(meta/diagnose)`, etc.). | — | Pure `:ext/symbols` contribution. See [vis-meta](../extensions/vis-meta.md). |
| `vis-editing` | `extensions/common/vis-editing` | Filesystem / shell / search tools exposed to the agent (`vis/rg`, `vis/patch`, etc.). Internal namespace is still `com.blockether.vis.ext.common-operations.*` pending a follow-up rename. | — | Pure `:ext/symbols` contribution. |
| `vis-benchmark` | `benchmarks/` | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working dirs under `benchmarks/data/`. | — | NOT loaded by the default product distribution. Pulled in via the repo-root `:bench` alias. Lives at the repo root because it is not a classpath plug-in. |

> **Two senses of "channel".** A *registered channel* (`:tui`,
> `:telegram`) is a CLI front-end registered through
> `com.blockether.vis.channel/register-global!` and exposed under
> `vis channels <name>`. A *conversations channel* is the keyword
> stored in `conversation_soul.metadata.channel` — the namespace
> conversations are grouped under (`:vis`, `:telegram`, `:cli`).
> `vis-tui` registers as `:tui` but writes its conversations under
> `:vis`. The CLI agent doesn't register any channel at all but
> writes its conversations under `:cli`.

## Auto-discovery

ONE classpath-scan mechanism, ONE resource per jar (`META-INF/vis-extension/vis.edn`),
ONE entry point on the **author** side
(`com.blockether.vis.extension/register-global!`), ONE loader on the
**runtime** side (`com.blockether.vis.extension/discover-extensions!`).
The resource is a flat EDN vector of namespace symbols; the loader
`require`s every namespace exactly once, and each `register-global!`
call at namespace-load time lands the extension in the global
registry.

An extension is a single map; everything it contributes lives in a
slot keyed under `:ext/<surface>`. `register-global!` dispatches each
slot to the matching sub-registry as a side effect:

| Slot | Per-entry shape | Internal sub-registry |
| ---- | --------------- | --------------------- |
| `:ext/symbols` | `(ext/symbol …)` / `(ext/value …)` entries (SCI sandbox bindings) | Bound into the env's SCI namespace at `register-extension!` time |
| `:ext/cli` | `cmd/command` maps (`:cmd/name`, `:cmd/run-fn`, `:cmd/parent`, …) | `com.blockether.vis.commandline.base/register-global!` |
| `:ext/channels` | `channel/channel` maps (`:channel/id`, `:channel/cmd`, `:channel/main-fn`, …) | `com.blockether.vis.channel/register-global!` |
| `:ext/providers` | `provider/provider` maps (`:provider/id`, `:provider/auth-fn`, …) | `com.blockether.vis.provider/register-global!` |
| `:ext/persistance` | `{:persistance/id <kw> :persistance/ns <sym>}` entries | `com.blockether.vis.persistance.core/register-backend!` |

**Authors only call `ext/register-global!`.** They can require the
sub-registry namespaces directly for embedded / programmatic use, but
normal extension code treats those namespaces as implementation
details inside `vis-core`.

The loader is **type-agnostic**: it does not care which slots an
extension populates. New surfaces (themes, skills, anything future)
require zero loader changes — add a slot key to the extension spec,
ship a jar with a `META-INF/vis-extension/vis.edn`, populate the slot, done.

Drop a jar on the classpath that ships a `META-INF/vis-extension/vis.edn` and every
namespace inside is auto-discovered at the next process boot — no
edits to `vis-core`, no `:require`s in user code. The CLI dispatcher
calls `discover-extensions!` once at `-main` startup; SDK callers
that bypass the CLI also get a lazy safety-net call from
`vis-core/loop.core/create-environment` and from
`com.blockether.vis.persistance.core/create-rlm-conn`.

## Dependency direction

```
extensions/channels/vis-tui                     ─┐
extensions/channels/vis-telegram                ─┤
extensions/persistance/vis-persistance-sqlite   ─┤
extensions/providers/vis-provider-github-copilot─┤
extensions/common/vis-meta                      ─┤
extensions/common/vis-editing                   ─┤
benchmarks/                                     ─┤
                                                 └─→ vis-core (repo root)
```

Every plug-in package depends on vis-core for the registries it
self-registers into and for the conversations API. vis-core depends
on none of them. Extension authors depend on vis-core and require
`com.blockether.vis.extension` for the authoring API.

## Per-package source path

vis-core's source lives at `src/com/blockether/vis/…` (repo root);
classpath plug-ins live under `extensions/<category>/<pkg>/src/com/blockether/vis/…`;
the benchmark harness lives under `benchmarks/src/…`.
The full Clojure source is intentionally not enumerated here — use
`find src extensions benchmarks -name '*.clj'` instead. The
architecture pages call out the namespaces that matter for each
subsystem (see [Iteration flow](iteration-flow.md),
[State ownership](state.md), [Channels](channels.md), and
[Database schema](database.md)).
