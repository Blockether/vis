# Packages

Vis is a polylith-style monorepo. The product ships as a set of small,
independently publishable packages under `packages/`. The repo-root
`deps.edn` aggregates them via `:local/root` so a single
`clojure -M:run` from the repo root has the whole product on the
classpath; downstream consumers only depend on the packages they need.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Package map

| Package                  | Purpose                                                                                        | Channel id  | Notes                                                                  |
| ------------------------ | ---------------------------------------------------------------------------------------------- | ----------- | ---------------------------------------------------------------------- |
| `vis-core`               | Public API facade, runtime loop, query/iteration engine, SCI sandbox, conversation lifecycle, CLI dispatcher (`channels.cli`) and one-shot agent (`channels.cli.agent`). | `:cli`      | The only package consumers must depend on directly. The `:cli` channel id is what the CLI agent's `conversations/create!` uses; it is not a `channel/register-global!` registration. |
| `vis-extension`          | Standalone plug-in contract: `com.blockether.vis.extension` (SCI tools) + `com.blockether.vis.channel` (CLI front-ends). Slim deps (telemere + clojure.spec). | —           | Extensions and third-party channels depend on this, **not** on `vis-core`. The runtime helpers that need a live env (`active-extensions`, `assemble-system-prompt`, `register-extension!`) stay on `vis-core`. |
| `vis-commandline`        | Reusable command-tree primitives (`command`, `dispatch!`, arg parsing, help rendering, global registry, `META-INF/vis/commandline.edn` auto-discovery). | —           | `vis-core/channels.cli` builds the whole `vis` command tree out of these. Third-party packages can register top-level or nested commands without touching `vis-core`. |
| `vis-persistance`        | Persistence FACADE: public API (`persistance.core`), spec (`persistance.spec`), id/kw/now-ms helpers (`persistance.base`). | —           | No JDBC dep. Concrete backends self-register at load time via `META-INF/vis/persistance-backends.edn`. |
| `vis-persistance-sqlite` | SQLite + Flyway backend. Owns every JDBC dep (sqlite-jdbc, next.jdbc, honeysql, flyway, nippy) and the `db/sqlite/migration/V1__schema.sql` resource. | —           | Loading `com.blockether.vis.persistance.sqlite.core` auto-registers the `:sqlite` backend with the facade. |
| `vis-logging`            | Telemere setup + persistence-backed log handler. Uses `borkdude/dynaload` to look up `persistance.core/log!` so it has no hard dep on any persistence jar. | —           | Opt-in. Adopters who only want console logging can pull this in without dragging the JDBC stack. |
| `vis-provider`           | Vendor auth/token plumbing (currently GitHub Copilot OAuth + device flow + token exchange).   | —           | Zero `vis-core` deps. `vis-core` resolves these via `requiring-resolve`. |
| `vis-tui`                | Lanterna-based TUI chat UI.                                                                    | `:tui`      | `:channel/owns-tty? true`. Uses `:vis` as its conversations-channel namespace (i.e. `(conversations/create! :vis)`). |
| `vis-telegram`           | Telegram Bot API long-poll loop wired into `conversations/for-telegram-chat!`.                | `:telegram` | Optional. Resolves into `vis-core` lazily via `requiring-resolve` — omitting the jar leaves the rest of vis usable. |
| `vis-benchmark`          | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working dirs under `data/`.       | —           | NOT loaded by the default product distribution. Pulled in via the repo-root `:bench` alias. |

> **Two senses of "channel".** A *registered channel* (`:tui`,
> `:telegram`) is a CLI front-end registered through
> `com.blockether.vis.channel/register-global!` and exposed under
> `vis channel <name>`. A *conversations channel* is the keyword
> stored in `conversation_soul.metadata.channel` — the namespace
> conversations are grouped under (`:vis`, `:telegram`, `:cli`).
> `vis-tui` registers as `:tui` but writes its conversations under
> `:vis`. The CLI agent doesn't register any channel at all but
> writes its conversations under `:cli`.

## Auto-discovery resources

Three independent classpath-scan mechanisms — same shape, different
scope. Each is an EDN vector of namespace symbols; loading them
triggers a `register-global!` side effect.

| Resource                          | Registry                                                | Scanned by                                             |
| --------------------------------- | ------------------------------------------------------- | ------------------------------------------------------ |
| `META-INF/vis/extensions.edn`     | `com.blockether.vis.extension/register-global!`         | `extension/discover-extensions!` — called from `create-environment` |
| `META-INF/vis/channels.edn`       | `com.blockether.vis.channel/register-global!`           | `channel/discover-channels!` — called once at `cli/-main` boot |
| `META-INF/vis/commandline.edn`    | `com.blockether.vis.commandline/register-global!`       | `commandline/discover-commands!` — called once at `cli/-main` boot |
| `META-INF/vis/persistance-backends.edn` | persistence facade backend registry               | called from `persistance.core/create-rlm-conn`          |

Drop a jar on the classpath that ships any of these resources and the
relevant subsystem picks it up at the next process boot — no edits to
`vis-core`, no `:require`s in user code.

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
