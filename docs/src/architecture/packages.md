# Packages

Vis is a polylith-style monorepo. The product ships as a set of small,
independently publishable packages under `packages/`. The repo-root
`deps.edn` aggregates them via `:local/root` so a single
`clojure -M:vis` from the repo root (or `bin/vis` once on `PATH`)
has the whole product on the classpath; downstream consumers only
depend on the packages they need.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Package map

| Package                  | Purpose                                                                                        | Channel id  | Notes                                                                  |
| ------------------------ | ---------------------------------------------------------------------------------------------- | ----------- | ---------------------------------------------------------------------- |
| `vis-core`               | Public API facade, runtime loop, query/iteration engine, SCI sandbox, conversation lifecycle, CLI dispatcher (`channels.cli`) and one-shot agent (`channels.cli.agent`). | `:cli`      | The only package consumers must depend on directly. The `:cli` channel id is what the CLI agent's `conversations/create!` uses; it is not a `channel/register-global!` registration. |
| `vis-extension`          | Standalone extension contract: `com.blockether.vis.extension` (SCI tools, the unified `META-INF/vis.edn` loader) + `com.blockether.vis.channel` (CLI front-ends). Slim deps (telemere + clojure.spec). | —           | Every extension surface (ext symbols, channels, third-party CLI commands, providers, persistance entries) depends on this, **not** on `vis-core`. The runtime helpers that need a live env (`active-extensions`, `assemble-system-prompt`, `register-extension!`) stay on `vis-core`. |
| `vis-commandline`        | Reusable command-tree primitives (`command`, `dispatch!`, arg parsing, help rendering, global registry). The CLI dispatcher (`commandline.main`) calls `vis-extension/discover-extensions!` via `requiring-resolve` so this jar stays standalone. | —           | `vis-core/channels.cli` builds the whole `vis` command tree out of these. Third-party packages can register top-level or nested commands without touching `vis-core`. |
| `vis-persistance`        | Persistence FACADE: public API (`persistance.core`), spec (`persistance.spec`), id/kw/now-ms helpers (`persistance.base`). | —           | No JDBC dep. Concrete backends self-register at load time through the unified `META-INF/vis.edn` loader. |
| `vis-persistance-sqlite` | SQLite + Flyway backend. Owns every JDBC dep (sqlite-jdbc, next.jdbc, honeysql, flyway, nippy) and the migration runner; the `db/sqlite/migration/V1__schema.sql` resource itself ships from `vis-persistance` so the schema travels with the API. | —           | Loading `com.blockether.vis.persistance.sqlite.core` auto-registers the `:sqlite` backend with the facade. |
| `vis-logging`            | Telemere setup + persistence-backed log handler. Uses `borkdude/dynaload` to look up `persistance.core/log!` so it has no hard dep on any persistence jar. | —           | Opt-in. Adopters who only want console logging can pull this in without dragging the JDBC stack. |
| `vis-provider`           | Vendor auth/token plumbing (currently GitHub Copilot OAuth + device flow + token exchange).   | —           | Zero `vis-core` deps. `vis-core` resolves these via `requiring-resolve`. |
| `vis-tui`                | Lanterna-based TUI chat UI.                                                                    | `:tui`      | `:channel/owns-tty? true`. Uses `:vis` as its conversations-channel namespace (i.e. `(conversations/create! :vis)`). |
| `vis-telegram`           | Telegram Bot API long-poll loop wired into `conversations/for-telegram-chat!`.                | `:telegram` | Optional. Resolves into `vis-core` lazily via `requiring-resolve` — omitting the jar leaves the rest of vis usable. |
| `vis-benchmark`          | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working dirs under `data/`.       | —           | NOT loaded by the default product distribution. Pulled in via the repo-root `:bench` alias. |

> **Two senses of "channel".** A *registered channel* (`:tui`,
> `:telegram`) is a CLI front-end registered through
> `com.blockether.vis.channel/register-global!` and exposed under
> `vis channels <name>`. A *conversations channel* is the keyword
> stored in `conversation_soul.metadata.channel` — the namespace
> conversations are grouped under (`:vis`, `:telegram`, `:cli`).
> `vis-tui` registers as `:tui` but writes its conversations under
> `:vis`. The CLI agent doesn't register any channel at all but
> writes its conversations under `:cli`.

## Auto-discovery resource

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

| `:ext/<slot>`     | Per-entry shape                                                              | Internal sub-registry                                          |
| ----------------- | ----------------------------------------------------------------------------  | -------------------------------------------------------------- |
| `:ext/symbols`    | `(ext/symbol …)` / `(ext/value …)` entries (SCI sandbox bindings)         | bound into the env's SCI namespace at `register-extension!` time |
| `:ext/cli`        | `cmd/command` maps (`:cmd/name`, `:cmd/run-fn`, `:cmd/parent`, …)           | `com.blockether.vis.commandline/register-global!`              |
| `:ext/channels`   | `channel/channel` maps (`:channel/id`, `:channel/cmd`, `:channel/main-fn`, …)| `com.blockether.vis.channel/register-global!`                  |
| `:ext/providers`  | `provider/provider` maps (`:provider/id`, `:provider/auth-fn`, …)           | `com.blockether.vis.provider/register-global!` (via `requiring-resolve`) |
| `:ext/persistance`| `{:persistance/id <kw> :persistance/ns <sym>}` entries                       | `com.blockether.vis.persistance.core/register-backend!` (via `requiring-resolve`) |

**Authors only call `ext/register-global!`.** They never need to
require `vis-channel`, `vis-commandline`, `vis-provider`, or
`vis-persistance` directly — the dispatch table above wires each slot
to the right sub-registry. The sub-registry namespaces remain
callable for embedded / programmatic use, but in normal extension
code they are an implementation detail.

The loader is **type-agnostic**: it does not care which slots an
extension populates. New surfaces (themes, skills, anything future)
require zero loader changes — add a slot key to `vis-extension`, ship
a jar with a `META-INF/vis.edn`, populate the slot, done.

Drop a jar on the classpath that ships a `META-INF/vis.edn` and every
namespace inside is auto-discovered at the next process boot — no
edits to `vis-core`, no `:require`s in user code. The CLI dispatcher
calls `discover-extensions!` once at `-main` startup; SDK callers
that bypass the CLI also get a lazy safety-net call from
`vis-core/loop.core/create-environment` and from
`vis-persistance/core/create-rlm-conn`.

## Dependency direction

```
vis-tui ──┐
vis-telegram ─┤
              ├─→ vis-core ──┬─→ vis-extension
vis-benchmark ┘              ├─→ vis-commandline
                             ├─→ vis-persistance ──→ vis-persistance-sqlite (auto-registers)
                             ├─→ vis-logging      (optional, dynaloads persistance/log!)
                             └─→ vis-provider     (resolved via requiring-resolve)
```

Channel packages depend on `vis-core` for the conversations API and
nothing else. Extension authors depend on `vis-extension` only —
they never need `vis-core` on their build path.

## Per-package source path

Every package's source lives under `packages/<package>/src/com/blockether/vis/…`.
The full Clojure source is intentionally not enumerated here — use
`find packages -name '*.clj'` instead. The architecture pages call
out the namespaces that matter for each subsystem (see
[Iteration Flow](iteration-flow.md), [State Ownership](state.md),
[Channels](channels.md), and [Database Schema](database.md)).
