# Vis repository guidance

Keep only repository-specific decisions here. General engineering practice is assumed.
Read area contracts relevant to the task. Keep API documentation in its namespace and
enforceable invariants in tests; do not duplicate them here.

## Scope and completion

Continue in-scope local edits, verification and fixes without asking at each step. Finish the
requested behavior, not just a first implementation. If blocked, report the concrete blocker and
what remains; do not substitute an adjacent fix or claim an unverified result.

Choose verification for the changed files and behavior. Code changes need affected tests, formatting and lint
(including reflection for Clojure); documentation-only changes need content, link and diff checks,
not a full application build. Reproduce reported bugs before fixing them; reference the issue in
regression-test comments when an issue exists. Use existing test infrastructure, not ad-hoc demos.

For simple, unambiguous bug-fix requests, the user expects the complete workflow: reproduce, fix,
verify, then commit the scoped changes and push to `main` without another permission round, unless
that request says otherwise. Treat such requests as authorization for that commit and push, not for
unrelated changes, releases, deployments or service restarts. Never include unrelated working-tree
changes. Do not finish a verified bug fix as local-only: commit and push before reporting completion.
An unrelated full-build failure does not by itself block delivery when affected tests and checks pass
and the failure is demonstrably outside the scoped diff; report that failure separately. If affected
verification, hooks or a safe push are blocked, report the exact blocker and the remaining action;
never bypass checks or hooks.
For other work, commit, push, publish, close issues or mutate external systems only when requested.
Permission for local work is not permission to restart a live gateway or deploy. Confirm destructive
actions and history rewrites; never bypass hooks. When committing, use the configured human identity,
not `root`, and a conventional `type(scope): imperative summary` under 72 characters, with
`Vis-Session: <bare-uuid>` as a trailer. Keep the body to the reason the diff cannot explain.

## Repository decisions

- No profanity or vulgarity in tracked content, including quoted reports, fixtures and commit text;
  paraphrase reports instead.
- Use direct, literal language in documentation. Avoid metaphors, slogans and rhetorical filler.
- This repository is public. Private deployment details and credentials belong in `infrastructure`,
  never here. Examples use `127.0.0.1`, `10.0.0.5`, `gateway.example.com` and `visgw`.
- No compatibility layers or migrations for obsolete APIs: update consumers and remove old paths.
- One engine/package, organized by domain under `src/com/blockether/vis/internal/`; mirror tests
  under `test/`. Add to an existing owner rather than another flat namespace or extension jar.
  Registration order is the explicit vector in `resources/META-INF/vis/manifest.edn`, not classpath
  discovery. `build.clj` derives native entrypoints from it.
- Shared leaf primitives belong to `internal.util`; one-caller helpers stay local. Production outbound
  Clojure HTTP uses `babashka.http-client`. Do not introduce Clojure `declare`.
- Clojure formatting uses `.zprint.edn` and the Vis formatter: one blank line between top-level forms,
  attached comments preserved, one final newline. `.clj-kondo/imports/` is tracked source, not cache.

## Read only for the area being changed

Paths below are relative to this repository. Internal namespace paths begin at
`src/com/blockether/vis/internal/`.

| Area | Canonical owner and non-obvious boundary |
|---|---|
| Python host API | `packages/vis-agent/src/blockether/vis/extension.py` is also executed by the engine; never mirror it. |
| Sandbox | `sandbox/` defines host policy and process integration; `python/` implements host execution. Interpreter, handles, descriptor limits and guest runtime Python belong in `vis-python-runtime`; read that repo's `AGENTS.md` before changing it. Keep host-call shims in `resources/vis-shims/` and host guest modules in `resources/vis-guest/`; do not copy runtime code. |
| Shims | `attach` and `ls` expose host functions; they do not replace Python packages. Python docstrings in `resources/vis-shims/` generate apropos resources; `apropos-resource-test/regenerate!` updates them. |
| Contracts | `packages/vis-contract/resources/vis-contract/` owns canonical JSON documents and same-named schemas; Skjema validates portable shapes. Callbacks, IO and mutable state remain local. |
| Tool declarations | `extension/core.clj` and its mirrored test own description/result/params, requiredness and wire keys. |
| Gateway transport | `gateway/wire.clj` defines snake_case wire keys, kebab-case engine keys and total JSON encoding. Use `wire/->wire` and `wire/json-str`; transport encoding failures can break event replay. |
| Config | `config/` owns merged configuration; toggle IDs are snake_case strings and reload from merged config. |
| Docs | `resources/vis-docs/` serves both the site and `doc()`. `resources/META-INF/vis/apropos/docs.edn` is the catalog; `resources/vis-docs/site.edn` alone owns titles and navigation. |
| UI | Load `.vis/skills/design/SKILL.md` for visual design, UI implementation or review. Companion controls are in `apps/vis-companion/src/components/ui.tsx`; TUI rendering is in `apps/vis-tui/`. |
| Licensing | `audit/README.md` is generated by `bb scripts/gen-audit.bb` (network required), never hand-edited. `audit_inventory_test` checks dependency pins against it. |

### Gateway diagnostics

Every agent-initiated gateway request uses the canonical Clojure client, including health checks:

```clojure
(require '[com.blockether.vis.internal.gateway.client :as gateway-client])
(gateway-client/request! :get "/healthz")
```

Use the route's actual method/path and optional `{:body … :headers … :timeout-ms …}`. The client
resolves or starts the daemon, acquires a lease and supplies authentication. Do not read the gateway
registry, copy secrets or hand-build curl/httpx authentication. Responses are non-throwing maps;
4xx/5xx are data in `:status`. Print only fields needed for the diagnosis, never secret-bearing bodies.

### Tests and native builds

Vis uses Lazytest, **not `clojure.test`** (which is silently undiscovered here):
`[lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]`.
Use `lazytest.core/set-ns-context!` and `around-each` instead of `use-fixtures`.

Use `run_tests` for the affected paths. A clean JVM runs `clojure -M:test`, optionally
`--namespace my.ns-test` or `--var my.ns-test/my-test`. A reused REPL reloads test namespaces only:
reload changed production namespaces or stop that REPL before rerunning. ClojureScript tests belong
to the project's shadow-cljs build; inspect printed counts, not just its exit code.

Passing JVM tests or a successful native build do not prove the binary runs. Interop changes need the relevant
`test-native/` coverage against the built image (`-M:test-native`); reachability metadata lives inside
jars under `META-INF/native-image/`. The engine metadata is pinned by `native_reachability_test`.
Read `.graalvm-version` before changing the locked GraalVM CE pin; use `bin/require-graalvm` for setup.
Never substitute Oracle GraalVM. Companion Android Gradle uses stock JDK 21 instead.

`e2e/run.py` makes paid model calls: use it for editing tools and workflows, not routine docs.
Delimiter/Parinfer repair is syntax-only. A host/bootstrap/extension boundary change needs coverage
across that boundary; do not drop required keys or validation just because one consumer ignores them.

### Skills and plans

`design` is the repository-authored skill. The other seven skills under `.vis/skills/` are upstream
installs, kept verbatim rather than silently forked. Load them only when the user explicitly requests
that skill or its specific workflow: ordinary coding does not activate Ponytail. Their style,
persistence, test shortcuts and publishing recipes do not override repository contracts or the
user's scope. Never infer authorization for remote actions from a skill.

Use `PLAN.md` for work that needs a maintained multi-phase plan, not every edit. When used, it has:
title, phrase, context (current paths, problem and rejected alternatives), numbered phases with
Rationale / Data / Acceptance criteria / Unknowns, then plan state. Update it with the work it tracks.

### Releases (only when requested)

`VIS_VERSION` is the version source. `npm run sync:version` in `apps/vis-companion` mirrors it to
package manifests and the SDK pyproject file; do not hand-edit those version fields. Product releases
bump, mirror, commit as `chore(release): vX.Y.Z`, push main, then push the annotated `v<VIS_VERSION>`
tag. Tag, version and current main must agree. Never move published tags; publish a new version instead.

App-only rebuilds keep the version and use the git commit count as build number. Choose either
`npm run release:ios:store` / `release:android:store`, or `npm run release:mobile`; only the latter
creates `companion-v<version>-build.<N>`. Never hand-tag it or submit the same build both ways.
`CHANGELOG.md` is hand-authored; CI writes the release notes section.
