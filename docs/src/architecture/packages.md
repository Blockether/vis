# Packages

Vis is a **single-package monorepo** plus a sibling tree of classpath
plug-ins.

- The host runtime ships as **one jar, one namespace
  (`com.blockether.vis.core`)**. Source lives at the repo root under
  `src/`. The previous three-package split (vis-sdk + vis-runtime +
  vis-main) collapsed because the "library / runtime / binary"
  boundary was not buying anything: there is only ever one runtime,
  and the binary, library, and SDK ship in the same classpath
  together for every consumer.
- Every classpath plug-in lives under `extensions/<category>/<name>/`
  and is genuinely optional (drop the jar, drop the feature). Each
  plug-in declares its own `deps.edn` with
  `com.blockether/vis {:local/root "../../.."}`.
- The benchmark harness lives under `benchmarks/` and is pulled via
  the `:bench` alias only — not a classpath plug-in.

The repo-root `deps.edn` declares the host runtime's dependencies
plus every classpath plug-in as a `:local/root` entry, so a single
`clojure -M:vis` (or `bin/vis` once on `PATH`) has the whole product
on the classpath. Downstream embedders depend on `com.blockether/vis`
plus whichever extension jars they want.

This page is the **single source of truth** for the package layout.
Every other doc that mentions a package links here instead of
re-listing it.

## Host runtime

| Path | Purpose |
| ---- | ------- |
| `src/com/blockether/vis/core.clj` | Public API facade. Re-exports every supported symbol from `internal/*`. **The names in `core.clj` are the contract**; the `internal/` tree is free to be split, merged, and renamed as the architecture evolves. |
| `src/com/blockether/vis/internal/cancellation.clj` | In-flight query cancellation tokens. |
| `src/com/blockether/vis/internal/commandline.clj` | Argument parser, command tree renderer, dispatcher. |
| `src/com/blockether/vis/internal/config.clj` | Config loader, db-path (`~/.vis/vis.mdb`), router builder, provider presets. |
| `src/com/blockether/vis/internal/env.clj` | SCI sandbox, var-index, `SYSTEM_VAR_NAMES`, `system-var-sym?`. |
| `src/com/blockether/vis/internal/error.clj` | Shared exception → user message formatter. |
| `src/com/blockether/vis/internal/extension.clj` | Extension spec, slot dispatch (`:ext/symbols`, `:ext/cli`, `:ext/channels`, `:ext/providers`, `:ext/persistance`), `register-extension!`, `discover-extensions!`. |
| `src/com/blockether/vis/internal/format.clj` | `format-clojure`, `format-tokens`, `format-cost`, `format-iterations`. |
| `src/com/blockether/vis/internal/loop.clj` | Iteration loop, query engine, conversation lifecycle, environment lifecycle, router management. |
| `src/com/blockether/vis/internal/main.clj` | `vis` binary surface: `-main`, dispatcher wiring, persistence-backed Telemere `:db` handler, built-in commands (`run` / `auth` / `doctor` / `conversations` / `extensions list`), one-shot `agent` + `run!` helpers. |
| `src/com/blockether/vis/internal/manifest.clj` | `META-INF/vis-extension/vis.edn` classpath scanner. |
| `src/com/blockether/vis/internal/markdown.clj` | Conversation -> Markdown exporter. Single host helper for projecting a persisted conversation (every turn: user prompt + final answer + optional metadata) into a paste-friendly Markdown string. Surface: `conversation->markdown`. Lives in the runtime so every channel (TUI, Telegram, CLI agent, third-party) ships a `Copy as Markdown` / `Export conversation` affordance through the same projection. |
| `src/com/blockether/vis/internal/persistance.clj` | Storage facade: `db-create-connection!`, every `db-store-*` / `db-list-*` / `db-update-*`, backend self-registration (`register-backend!`). |
| `src/com/blockether/vis/internal/progress.clj` | Streaming progress tracker. |
| `src/com/blockether/vis/internal/prompt.clj` | `assemble-system-prompt` (single source of truth), `build-iteration-context`, `CORE_SYSTEM_PROMPT`. |
| `src/com/blockether/vis/internal/registry.clj` | Channel + CLI command + provider registries. |

## Classpath plug-ins

Every plug-in ships exactly one classpath manifest at
`resources/META-INF/vis-extension/vis.edn` and self-registers via
`(sdk/register-extension! …)` (where `sdk` is
`com.blockether.vis.core`) at namespace load.

| Package | Path | Purpose | Channel id | Notes |
| ------- | ---- | ------- | ---------- | ----- |
| `vis-persistance-sqlite` | `extensions/persistance/vis-persistance-sqlite/` | SQLite backend for the persistence facade. Owns the SQLite schema and JDBC stack: sqlite-jdbc, next.jdbc, HoneySQL, HikariCP, nippy, the SQLite Flyway driver. Internal namespace: `com.blockether.vis.ext.persistance-sqlite.core`. | — | Ships the canonical SQLite schema at `resources/db/sqlite/migration/V1__schema.sql`. Loading the namespace auto-registers the `:sqlite` backend with the persistence facade. |
| `vis-provider-github-copilot` | `extensions/providers/vis-provider-github-copilot/` | GitHub Copilot OAuth device-flow provider. Internal namespace: `com.blockether.vis.ext.provider-github-copilot`. | — | Registers a provider in the global registry; surfaces under `vis auth github-copilot`. |
| `vis-provider-openai-codex` | `extensions/providers/vis-provider-openai-codex/` | OpenAI Codex / ChatGPT OAuth provider with PKCE, localhost callback, token refresh, and account-id extraction. Internal namespace: `com.blockether.vis.ext.provider-openai-codex`. | — | Registers a provider in the global registry; surfaces under `vis auth openai-codex`. |
| `vis-provider-zai` | `extensions/providers/vis-provider-zai/` | Z.ai (ZhipuAI) static-API-key provider — covers both the coding-plan and pay-as-you-go endpoints in one extension. Internal namespace: `com.blockether.vis.ext.provider-zai`. | — | Surfaces under `vis auth <plan>` per registered plan. |
| `vis-channel-tui` | `extensions/channels/vis-channel-tui/` | Lanterna-based TUI chat. Internal namespace: `com.blockether.vis.ext.channel-tui.*` (state / dialogs / screen / chat / render / input / footer / theme / primitives / provider). | `:tui` | `:channel/owns-tty? true`. Conversations are stored under the same `:tui` keyword used for CLI dispatch. |
| `vis-channel-telegram` | `extensions/channels/vis-channel-telegram/` | Telegram Bot API long-poll loop wired into `for-telegram-chat!`. Internal namespace: `com.blockether.vis.ext.channel-telegram.*`. | `:telegram` | Optional channel; omitting the jar leaves the rest of vis usable. |
| `vis-foundation` | `extensions/common/vis-foundation/` | SCI sandbox introspection extension (`(v/turn)`, `(v/diagnose)`, `(v/conversation)`, `(v/find-attempts)`, `(v/var-history)`, `(v/extensions)`, `(v/extension-readme)` …). Internal namespace: `com.blockether.vis.ext.foundation.introspection`. | — | Pure `:ext/symbols` contribution. See [vis-foundation](../extensions/common/vis-foundation.md). |
| `vis-foundation` | `extensions/common/vis-foundation/` | cwd / OS / git-facts SCI helpers. Internal namespace: `com.blockether.vis.ext.foundation.environment.core`. | — | Pure `:ext/symbols` contribution. |
| `vis-foundation` | `extensions/common/vis-foundation/` | Filesystem / shell / search / patch tools (`v/cat`, `v/ls`, `v/rg`, `v/edit`, `v/sh`, …). Internal namespace: `com.blockether.vis.ext.foundation.editing.core`. | — | Pure `:ext/symbols` contribution. |
| `vis-language-clojure` | `extensions/languages/clojure/` | Clojure structured-editing wrapper (`(z/zedit …)` and the rewrite-clj zipper API, all under the single `z/` alias). Internal namespace: `com.blockether.vis.ext.lang-clojure.core`. | — | Add new `extensions/languages/<lang>/` directories when shipping tools that only make sense for one source language. |

## Dev / research

| Package | Path | Purpose | Notes |
| ------- | ---- | ------- | ----- |
| `vis-benchmark` | `benchmarks/` | Benchmark harness (4clojure, HumanEval, SWE-bench Verified). Working directories created on demand under `benchmarks/`. | NOT a classpath plug-in (no `META-INF/vis-extension/vis.edn`). Pulled in via the repo-root `:bench` alias only. |

> **Two senses of "channel".** A *registered channel* (`:tui`,
> `:telegram`) is a CLI front-end registered through the `:ext/channels`
> slot of an extension and exposed under `vis channels <name>`. A
> *conversations channel* is the keyword stored in
> `conversation_soul.metadata.channel` — the namespace conversations
> are grouped under. Registered channels use the same keyword for both
> senses. The CLI agent (`vis run`) is the lone exception: it does not
> register a channel descriptor — the `vis` dispatcher itself is its
> surface — but it writes its conversations under `:cli`.

## Auto-discovery

ONE classpath-scan mechanism, ONE resource per jar
(`META-INF/vis-extension/vis.edn`), ONE entry point on the **author**
side (`(sdk/register-extension! …)`), ONE loader on the **runtime**
side (`com.blockether.vis.core/discover-extensions!`).

The resource is an EDN map keyed by extension id symbol, holding a
`:nses` vec (namespaces the loader `require`s) and a `:docs` map
(structured Markdown descriptors). The loader walks every URL on the
classpath, parses each map, validates the doc descriptors, `require`s
every namespace listed under `:nses` exactly once, and merges the
result into a process-level registry. Each `(sdk/register-extension! …)`
call at namespace-load time lands the extension in the global
registry.

Authors declare everything an extension contributes — sandbox
symbols, channels, CLI commands, providers, persistance backends,
and inline docs — in a single map; the registrar dispatches each
populated `:ext/<slot>` to its matching sub-registry as a side
effect:

| Slot | Per-entry shape | What happens at registration |
| ---- | --------------- | ---------------------------- |
| `:ext/symbols` | `(sdk/symbol …)` / `(sdk/value …)` entries | Bound into the dedicated SCI namespace declared via `:ext/ns-alias`. |
| `:ext/cli` | `:cmd/*` maps (`:cmd/name`, `:cmd/run-fn`, `:cmd/parent`, …) | `registry/register-cmd!`. Auto-mounted under `["extensions"]` unless `:cmd/parent` overrides it (and only `["extensions" …]` parents are accepted from the slot). |
| `:ext/channels` | `:channel/*` maps (`:channel/id`, `:channel/cmd`, `:channel/main-fn`, …) | `registry/register-channel!`. Surfaces under `vis channels <cmd>`. |
| `:ext/providers` | `:provider/*` maps (`:provider/id`, `:provider/auth-fn`, …) | `registry/register-provider!`. Surfaces under `vis auth <id>`. |
| `:ext/persistance` | `{:persistance/id <kw> :persistance/ns <sym>}` entries | `persistance/register-backend!`. Picked up by `db-create-connection!`. |

**Authors only call `(sdk/register-extension! …)`.** The lower-level
`register-cmd!` / `register-channel!` / `register-provider!` /
`register-backend!` calls still exist on `com.blockether.vis.core`
for embedded / programmatic use, but normal extension code treats
them as implementation details. `register-extension!` is the contract.

The loader is **type-agnostic**: it does not care which slots an
extension populates. New surfaces (themes, skills, anything future)
require zero loader changes — add a slot key to the extension spec,
ship a jar with a `META-INF/vis-extension/vis.edn`, populate the
slot, done.

Drop a jar on the classpath that ships a
`META-INF/vis-extension/vis.edn` and every namespace inside is
auto-discovered at the next process boot — no edits to the host
runtime, no `:require`s in user code. The CLI dispatcher calls
`discover-extensions!` once at `-main` startup; SDK callers that
bypass the CLI also get a lazy safety-net call from
`com.blockether.vis.core/create-environment` and from
`com.blockether.vis.core/db-create-connection!`.

## Dependency direction

```
src/                                 (host runtime — ONE jar, ONE namespace)
  ↑
extensions/channels/vis-channel-tui                  ─┐
extensions/channels/vis-channel-telegram             ─┤
extensions/persistance/vis-persistance-sqlite        ─┤
extensions/providers/vis-provider-github-copilot     ─┤
extensions/providers/vis-provider-openai-codex       ─┤
extensions/providers/vis-provider-zai                ─┤  → com.blockether/vis (root)
extensions/common/vis-foundation                    ─┤
extensions/common/vis-foundation             ─┤
extensions/common/vis-foundation                 ─┤
extensions/languages/clojure                         ─┤
benchmarks/                                          ─┘
```

The host runtime does not require any extension namespace. Channel
adapters and other plug-ins that the runtime refers to in passing
are resolved via `requiring-resolve` at call sites, so the hard
dependency stays one-way.

## Per-extension source path

Every extension lives under
`extensions/<category>/<name>/src/com/blockether/vis/ext/<id>/…`
and follows the `com.blockether.vis.ext.<id>.<module>` namespace
convention. Examples:

- `com.blockether.vis.ext.channel-tui.screen`
- `com.blockether.vis.ext.channel-telegram.bot`
- `com.blockether.vis.ext.persistance-sqlite.core`
- `com.blockether.vis.ext.foundation.introspection`
- `com.blockether.vis.ext.foundation.environment.core`
- `com.blockether.vis.ext.foundation.editing.core`
- `com.blockether.vis.ext.lang-clojure.core`
- `com.blockether.vis.ext.provider-github-copilot`
- `com.blockether.vis.ext.provider-openai-codex`
- `com.blockether.vis.ext.provider-zai`

The benchmark harness lives under `benchmarks/src/…`.

The full Clojure source is intentionally not enumerated here — use
`find src extensions benchmarks -name '*.clj'`. The architecture
pages call out the namespaces that matter for each subsystem (see
[Iteration flow](iteration-flow.md),
[State ownership](state.md), [Channels](channels.md),
and [Database schema](database.md)).
