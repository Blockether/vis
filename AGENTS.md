# Vis repository guidance

Only decisions that the code cannot show. Contracts live in namespaces, invariants in tests.

## Commands

Run each project's tools from its own directory.

- One namespace: `clojure -M:test --namespace my.ns-test` or `--var my.ns-test/my-test`.
- Full suite: `clojure -M:test` (one JVM). Locally it skips `release-bundle-test` when no launcher, workflow or build file changed; CI and `VIS_TEST_ALL=1` run it. Lint: `clojure -M:lint`, reflection warnings included.
- Native: `clojure -T:build native`, then `clojure -M:test-native`. JVM tests do not prove the binary.
- Companion Android Gradle uses stock JDK 21.
- `clojure -M:audit` generates `audit/README.md` (network required). Never edit that file by hand.
- `harbor run --path e2e` makes paid model calls. Use it only for editing tools and workflows.

## Layout and gotchas

- `apps/vis-tui/` is a separate project. It uses the gateway over HTTP and never loads the engine.
- `packages/vis-contract/` holds the shared schemas and wire keys. Change engine, TUI, Companion and SDK with it.
- Derive vocabulary and bounds from the `vis-contract` schemas, never from paired catalogs.
- `apps/vis-companion/` controls are in `src/components/ui.tsx`.
- `apps/vis-docs/` is the public Extension Center. It is never a `doc()` page.
- The engine also runs `packages/vis-agent/src/blockether/vis/extension.py`. Never mirror it.
- `sandbox/` is host policy, `python/` is host execution. Guest runtime code belongs in `~/vis-python-runtime`.
- Put host shims flat in `resources/vis-host-python/`. Regenerate apropos with `apropos-resource-test/regenerate!`.
- Encode gateway output with `wire/->wire` and `wire/json-str`. An encoding failure can break event replay.
- `extension/core.clj` owns tool descriptions, params and wire keys. `config/` owns merged configuration.
- Only `resources/vis-docs/site.edn` owns doc titles and navigation.
- Use GraalVM CE from `.graalvm-version` through `bin/require-graalvm`. Never use Oracle GraalVM.
- The Companion relay has no device-token store and accepts no OAuth callbacks.
- Send every gateway request through `gateway-client/request!` (`com.blockether.vis.internal.gateway.client`).
  It starts the daemon and adds authentication. Never build curl authentication by hand.

## Tests

- Use only `lazytest.core` and `set-ns-context!`/`around-each`. The runner ignores `clojure.test` silently.
- Write a test only for a defect that a person or a program would see. See it fail without the fix.
- Do not test constants, prose, styles, markup, snapshots or delegation.
- Test a value that another program reads against a schema or fixture in `packages/vis-contract/`.
- To delete a test that names an issue, a report or a fixture, add a `Drops-Test:` commit trailer.
  Run `bin/check-test-drops` before each push; it prints the trailer to copy.

## Writing

Write tracked text (prompts, docs, commits) in Simplified English with the ASD-STE100 rules. Chat replies are exempt.

## Work and Git

- A request that changes files is authorized to completion: verify, commit only its changes, push to `main`.
- Only an explicit analysis-only, preview or local-only request cancels this. Releases and deployments need a request.
- An end-to-end task covers each touched sibling repository and its `deps.edn` pin bump.
- Keep other sessions' changes out of commits. If you cannot separate them, stop and report.
- Commit as the human identity: `type(scope): summary` under 72 characters, trailer `Vis-Session: <bare-uuid>`.
- Put the issue number in the subject. After the push, comment on the issue; close it only when fully fixed.
- Never bypass checks or hooks. Report a full-build failure outside your diff.
- Read the `vis-docs` skill before you change `resources/vis-docs/` or the README.
