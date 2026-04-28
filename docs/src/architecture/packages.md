# Packages

Vis is a polylith-style monorepo. The product ships as a small set of
publishable packages under `packages/`. The repo-root `deps.edn`
aggregates them via `:local/root` so a single `clojure -M:vis` from
the repo root (or `bin/vis` once on `PATH`) has the whole product on
the classpath; downstream consumers only depend on the packages they
need.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Package map

| Package | Purpose | Channel id | Notes |
| ------- | ------- | ---------- | ----- |
| `vis-core` | Public API facade, runtime loop, query/iteration engine, iteration response specs, SCI sandbox, conversation lifecycle, CLI dispatcher, command registry, extension registry, channel registry bridge, provider registry, persistence facade, migration runner, and Telemere logging setup. | `:cli` | The only package extension authors and runtime consumers must depend on directly. Namespaces such as `com.blockether.vis.extension`, `com.blockether.vis.commandline.base`, `com.blockether.vis.channel`, `com.blockether.vis.provider`, `com.blockether.vis.persistance.core`, and `com.blockether.vis.logging` are shipped from this jar. The `:cli` channel id is what the CLI agent's `conversations/create!` uses; it is not a `channel/register-global!` registration. |
| `vis-persistance-sqlite` | SQLite backend for the persistence facade. Owns the SQLite schema resources and JDBC stack: sqlite-jdbc, next.jdbc, HoneySQL, HikariCP, nippy, and the SQLite Flyway driver. | — | Depends on `vis-core`. Loading `com.blockether.vis.persistance.sqlite.core` auto-registers the `:sqlite` backend with the facade. The canonical SQLite schema resource ships from this package at `db/sqlite/migration/V1__schema.sql`. |
| `vis-provider-github-copilot` | GitHub Copilot OAuth device-flow provider. | — | Depends on `vis-core` for the extension and provider registries. Ships a `META-INF/vis.edn` entry that registers the provider on startup. |
| `vis-tui` | Lanterna-based TUI chat UI. | `:tui` | `:channel/owns-tty? true`. Uses `:vis` as its conversations-channel namespace (i.e. `(conversations/create! :vis)`). |
| `vis-telegram` | Telegram Bot API long-poll loop wired into `conversations/for-telegram-chat!`. | `:telegram` | Optional. Resolves into `vis-core` lazily via `requiring-resolve` — omitting the jar leaves the rest of vis usable. |
| `vis-benchmark` | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working dirs under `data/`. | — | NOT loaded by the default product distribution. Pulled in via the repo-root `:bench` alias. |

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

ONE classpath-scan mechanism, ONE resource per jar (`META-INF/vis.edn`),
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
ship a jar with a `META-INF/vis.edn`, populate the slot, done.

Drop a jar on the classpath that ships a `META-INF/vis.edn` and every
namespace inside is auto-discovered at the next process boot — no
edits to `vis-core`, no `:require`s in user code. The CLI dispatcher
calls `discover-extensions!` once at `-main` startup; SDK callers
that bypass the CLI also get a lazy safety-net call from
`vis-core/loop.core/create-environment` and from
`com.blockether.vis.persistance.core/create-rlm-conn`.

## Dependency direction

```
vis-tui ──┐
vis-telegram ─┤
vis-benchmark ┤
vis-persistance-sqlite ┤
vis-provider-github-copilot ┘
              └─→ vis-core
```

Channel packages depend on `vis-core` for the conversations API and
nothing else. Backend/provider packages depend on `vis-core` for the
registries they self-register into. Extension authors depend on
`vis-core` and require `com.blockether.vis.extension` for the
authoring API.

## Per-package source path

Every package's source lives under `packages/<package>/src/com/blockether/vis/…`.
The full Clojure source is intentionally not enumerated here — use
`find packages -name '*.clj'` instead. The architecture pages call
out the namespaces that matter for each subsystem (see
[Iteration flow](iteration-flow.md), [State ownership](state.md),
[Channels](channels.md), and [Database schema](database.md)).
