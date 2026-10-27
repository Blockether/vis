# Vis repository guidance

This file has only decisions that the code cannot show. Area contracts live in their namespaces;
invariants live in tests.

## Commands

Run each project's tools from its own directory.

- Test one namespace in a clean JVM: `clojure -M:test --namespace my.ns-test` or `--var my.ns-test/my-test`.
- Run the full suite: `clojure -M:test:test-parallel`. Each namespace runs in its own worker JVM.
- Lint: `clojure -M:lint`. Fix reflection warnings too.
- Format Clojure with `.zprint.edn` and the Vis formatter: one blank line between top-level forms, attached comments kept, one final newline.
- Check the native image: `clojure -T:build native`, then `clojure -M:test-native`. Passing JVM tests do not prove that the binary runs.
- Run Companion npm commands with `env -u NODE_OPTIONS`. Companion Android Gradle uses stock JDK 21.
- `clojure -M:audit` generates `audit/README.md` (network required). Never edit that file by hand.
- `harbor run --path e2e` with `blockether.vis.evals.agent:VisAgent` makes paid model calls. Use it only for editing tools and workflows.

## Layout

- The engine and gateway are in `src/com/blockether/vis/internal/`, organized by domain. Tests mirror them under `test/`.
- `apps/vis-tui/` is a separate Clojure project. It uses the gateway over HTTP and never loads the engine.
- `packages/vis-contract/` holds the shared JSON Schemas and wire keys. Change every consumer with it: engine, TUI, Companion and SDK.
- `apps/vis-companion/` is one React and Vite bundle, wrapped by Capacitor (mobile) and Pake (desktop). Its controls are in `src/components/ui.tsx`.
- `apps/vis-docs/` is the Extension Center Worker and catalog. It is on the public site only, never a `doc()` page.

## Gotchas

- The engine also runs `packages/vis-agent/src/blockether/vis/extension.py`. Never mirror it.
- Sandbox: `sandbox/` is host policy, `python/` is host execution. Interpreter and guest runtime code belong in `~/vis-python-runtime`. Put host shims in `resources/vis-host-python/`, without subdirectories.
- Shim docstrings generate the apropos resources. Regenerate them with `apropos-resource-test/regenerate!`.
- Derive vocabulary and bounds from the schemas in `packages/vis-contract/resources/vis-contract/schema/`, never from paired catalogs.
- Encode gateway output with `wire/->wire` and `wire/json-str`. A transport encoding failure can break event replay.
- `extension/core.clj` owns tool descriptions, params and wire keys.
- The vector in `resources/META-INF/vis/manifest.edn` sets the registration order, not classpath discovery.
- `.clj-kondo/imports/` is tracked source, not a cache.
- `config/` owns the merged configuration. Toggle IDs are snake_case strings and reload from it.
- Native reachability metadata is inside jars under `META-INF/native-image/`. `native_reachability_test` pins the engine metadata.
- Only `resources/vis-docs/site.edn` owns doc titles and navigation. `resources/META-INF/vis/apropos/docs.edn` is the doc catalog.
- Read `.graalvm-version` before you change the GraalVM CE pin. Use `bin/require-graalvm`. Never use Oracle GraalVM.
- The Companion relay holds the APNs/FCM keys and has no device-token store. It accepts no OAuth callbacks.
- Send every gateway request through the Clojure client, health checks included. It starts the daemon and adds authentication. Do not read the gateway registry or build curl authentication by hand:

```clojure
(require '[com.blockether.vis.internal.gateway.client :as gateway-client])
(gateway-client/request! :get "/healthz") ; a 4xx or 5xx is data in :status
```

## Code

- Log every caught error with its cause and context. Never discard an error silently, also in best-effort code.
- Do not add compatibility layers or migrations for obsolete APIs. Update the consumers and remove the old paths.
- Add code to an existing owner, not to a new flat namespace or extension jar.
- Put shared leaf primitives in `internal.util`. Keep a helper with one caller local.
- Use `babashka.http-client` for outbound Clojure HTTP in production.
- Do not add Clojure `declare`.
- Read the `AGENTS.md` of `~/vis-python-runtime` before you change that repository.

## Tests

- Use Lazytest's own API: `[lazytest.core :refer [defdescribe describe it expect]]`. The runner ignores `clojure.test` without a warning.
- Do not use Lazytest's experimental interfaces. Use `set-ns-context!` and `around-each`, not `use-fixtures`.
- Group cases with `describe`. An `it` inside another `it` never runs.
- If you add a regression test for an issue, name the issue in its comment.
- For a host, bootstrap or extension boundary change, test across that boundary. Do not drop required keys or validation because one consumer ignores them.
- Delimiter (Parinfer) repair fixes syntax only. It does not prove that the code is correct.

## Product rules

- Keep profanity and vulgarity out of tracked content, also in quoted reports and commit text. Paraphrase user reports.
- This repository is public. Put private deployment details in `infrastructure`. In examples, use `127.0.0.1`, `10.0.0.5`, `gateway.example.com` and `visgw`.
- Give every tool binding an explicit Activity presentation, also each exported Python method. Follow `resources/vis-docs/extension-api.md#activity-presentation`.
  - Labels are capitalized English for people ("Run tests"), not code identifiers.
  - Quick reads, patches and lookups show only the end (`show_start=False`, `:show-start false`). Slow work shows progress.
  - Keep errors, counts and diffs. Never replace them with a generic preview.
- In TUI lists and trees, ← folds the selected row and → unfolds it. On a row inside a fold, ← selects and folds that fold row. Enter and a click also toggle. Show ▸ or ▾, and list the keys in that surface's help.
- In the TUI and the Companion, avoid unnecessary secondary labels and descriptions. Let the layout do the explaining.
- Before you change `resources/vis-docs/` or the README, read the `vis-docs` skill.

## Simplified English

Write tracked text in Simplified English: prompts, rules, docs and commits, not chat replies. Use the
[ASD-STE100](https://www.asd-ste100.org/) writing rules, not its dictionary:

- One instruction in each sentence, imperative and active, with any condition first.
- At most 20 words in an instruction, 25 in a description, 6 sentences in a paragraph, 3 words in a noun cluster.
- One topic in each paragraph and one meaning for each term. Use the most common word.
- Use lists for steps and conditions. In a warning, give the command first, then the risk.
- Keep names, code, paths, limits, links and test-pinned phrases exact.

## Work, Git and issues

- Every request that changes files is authorized to completion: verify, commit only the task's changes and push to `main`. Do not ask first.
- An end-to-end task also covers commits and pushes in each touched repository. It also covers the `deps.edn` pin bump of a changed sibling, and its release when its native libraries change.
- Only an explicit analysis-only, diff-preview, local-only or no-push request cancels this. It never covers unrelated changes, product releases, deployments, live restarts or history rewrites.
- For a product release, read the `vis-release` skill. Do one only when the user asks.
- Check documentation-only changes with content, link and diff checks, not a full build.
- If a full build fails outside your diff, deliver when the affected checks pass, and report that failure.
- If you are blocked, report the blocker and the remaining work. Never bypass checks or hooks.
- Record the scope of a Git request at its start. For `add all`, that is the staged, unstaged and untracked content at the start. Keep other sessions' changes out. If they overlap and you cannot separate them, stop and report.
- Commit as the configured human identity, not `root`. Use `type(scope): imperative summary` under 72 characters, and a `Vis-Session: <bare-uuid>` trailer. Limit the body to the reason that the diff cannot show.
- For issue work, put the issue number in the subject, for example `#191`. Write `Fixes #191` in the body when the commit fully resolves it.
- After you push an issue fix, comment on the issue: the user-visible change, the verification and the commit. Close it only when it is fully fixed. Leave a partial fix open and say what remains.
- Report every commit, push, release and issue status in the final reply.

## Skills, plans and computer use

- Load a skill only when the user asks or its workflow applies. Keep upstream skills verbatim. A skill never overrides these rules or authorizes remote actions.
- Use `PLAN.md` only for multi-phase work: title, context, numbered phases with Rationale / Data / Acceptance criteria / Unknowns, and the plan state.
- For a user-authorized task, you can use computer-use automation with macOS apps and websites. Inspect the UI before you act and verify the result. This does not authorize unrelated account changes.
- Keep passwords, private keys and recovery codes out of transcripts and tracked files. Use private human input for authentication.
