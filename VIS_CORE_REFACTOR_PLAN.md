# Vis core refactor plan

## Decision

`vis-core` is the product kernel. Backward compatibility for the old split packages is not a constraint.

Public API should collapse to two human-facing facades:

1. `com.blockether.vis.core` — runtime facade for environments, conversations, queries, and public runtime helpers.
2. `com.blockether.vis.extension` — extension-author facade for symbols, values, global extension registration, prompt rendering, and discovery.

Everything else inside `vis-core` is implementation detail unless it is a process entry point:

- `com.blockether.vis.commandline.main` remains the CLI `-main` entry point.
- `com.blockether.vis.channels.cli` remains the built-in CLI command registration namespace.

The following namespaces may still exist, but consumers should not treat them as public API:

- `com.blockether.vis.commandline.*`
- `com.blockether.vis.channel`
- `com.blockether.vis.provider`
- `com.blockether.vis.persistance.*`
- `com.blockether.vis.logging`
- deep `com.blockether.vis.loop.*` implementation namespaces

## Gates

Every slice must pass these gates before the next slice starts:

1. Format and lint:

   ```bash
   ./verify.sh --quick
   ```

2. Package-local core tests:

   ```bash
   cd packages/vis-core && clojure -M:test
   ```

3. Repo aggregate tests after any public facade or runtime-loop move:

   ```bash
   clojure -M:test
   ```

4. Full verification before shipping the refactor:

   ```bash
   ./verify.sh
   ```

5. Docs updated in the same change whenever namespace ownership, public API, or architecture changes.

## Phase 0 — Make the test gate honest

Problem: the repo-root aggregate tests can hide package-local failures.

Fix:

- Keep SQLite-specific tests and helpers in `packages/vis-persistance-sqlite/test`.
- Keep `packages/vis-core` tests independent of `vis-persistance-sqlite` and extension packages.
- If a core test needs extension behavior, use a local fixture extension instead of depending on an extension jar.

Acceptance:

```bash
cd packages/vis-core && clojure -M:test
clojure -M:test
./verify.sh --quick
```

## Phase 1 — Establish the facade boundary

Make docs and namespace comments clear:

- Runtime consumers use `com.blockether.vis.core`.
- Extension authors use `com.blockether.vis.extension`.
- All other namespaces are internal unless explicitly documented as process entry points.

Then stop preserving old package boundaries in names/comments.

Acceptance:

- `docs/src/architecture/packages.md` names only `vis-core` as the core API package.
- `docs/src/extensions/overview.md` and `docs/src/extensions/spec.md` point authors to `com.blockether.vis.extension` from `vis-core`.
- SQLite schema resources are documented as owned by `vis-persistance-sqlite`, not `vis-core`.
- No docs claim `vis-extension`, `vis-commandline`, `vis-provider`, `vis-persistance`, or `vis-logging` are separate packages.

## Phase 2 — Split `com.blockether.vis.extension`

Current problem: `extension.clj` is over 1300 lines and owns unrelated behavior.

Keep `com.blockether.vis.extension` as the only public extension-author facade.

Create implementation namespaces:

```text
com.blockether.vis.extension.spec
com.blockether.vis.extension.registry
com.blockether.vis.extension.slots
com.blockether.vis.extension.prompt
com.blockether.vis.extension.discovery
com.blockether.vis.extension.invocation
com.blockether.vis.extension.commandline
```

Responsibilities:

- `extension.spec` — all clojure.spec definitions and validators.
- `extension.registry` — global extension registry and dependency ordering.
- `extension.slots` — dispatch `:ext/cli`, `:ext/channels`, `:ext/providers`, `:ext/persistance` into internal registries.
- `extension.prompt` — extension prompt rendering.
- `extension.discovery` — `META-INF/vis.edn` classpath loader.
- `extension.invocation` — symbol invocation wrapper and decorators.
- `extension.commandline` — `vis extensions` CLI bridge.
- `extension` — public facade only.

Acceptance:

- `extension.clj` becomes a small facade.
- Extension-author tests still require only `com.blockether.vis.extension`.
- Internal tests may target `com.blockether.vis.extension.*` namespaces.

## Phase 3 — Split the iteration engine

Current problem: `iteration/core.clj` is around 1500 lines.

Keep orchestration in:

```text
com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
```

Create focused implementation namespaces:

```text
iteration.execution
iteration.parse-rescue
iteration.persistence
iteration.progress
iteration.final
iteration.plan
iteration.context
iteration.result
```

Acceptance:

- `iteration/core.clj` reads as a pipeline.
- Parse rescue, plan slot, final answer, progress chunks, and persistence have direct tests.

## Phase 4 — Split the query engine

Current problem: `query/core.clj` is over 1100 lines.

Create:

```text
query.prompt
query.system-state
query.provider
query.retry
query.result
query.hooks
query.redundancy
```

Acceptance:

- `query/core.clj` owns turn orchestration only.
- Retry/schema rejection tests target `query.retry`.
- Redundancy tests target `query.redundancy`.

## Phase 5 — Split environment sandbox internals

Current problem: `environment/core.clj` mixes SCI initialization, sandbox I/O, bindings, restore, and var-index rendering.

Create:

```text
environment.sci
environment.bindings
environment.sandbox-io
environment.var-index
environment.restore
```

Acceptance:

- `environment/core.clj` is a facade for environment internals.
- Var-index and sandbox I/O have direct tests.

## Phase 6 — Split loop core

Current problem: `loop/core.clj` mixes environment lifecycle, extension registration, prompt assembly, sandbox cleanup, and var lifecycle.

Create:

```text
loop.environment
loop.extensions
loop.prompt
loop.sandbox-cleanup
loop.var-lifecycle
```

Acceptance:

- `loop/core.clj` stays as runtime facade over loop internals.
- Auto-forget tests target `loop.sandbox-cleanup`.

## Phase 7 — Split persistence tests

Implementation is acceptable for now, but `persistance/db_test.clj` is too large.

Split tests into:

```text
persistance.schema-test
persistance.conversation-test
persistance.query-test
persistance.iteration-test
persistance.expression-test
persistance.search-test
persistance.log-test
persistance.backend-registry-test
```

Acceptance:

- No single persistence test file is a god file.
- Package-local and aggregate tests both pass.

## Phase 8 — Optional commandline split

`commandline/base.clj` can stay for now. If it grows or becomes painful, split it into:

```text
commandline.spec
commandline.parser
commandline.help
commandline.registry
commandline.dispatch
```

Keep `commandline.main` as the process entry point.

## Final success criteria

- Two public facades are documented: `com.blockether.vis.core` and `com.blockether.vis.extension`.
- `com.blockether.vis.extension` is a small author-facing facade.
- No orchestration namespace is a 1000+ line god file.
- Root and package-local tests pass.
- `./verify.sh` passes.
