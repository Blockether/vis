# Vis repository guidance

This file has only decisions that the code cannot show. Area contracts live in their namespaces;
invariants live in tests.

## Commands

Run each project's tools from its own directory.

- Test one namespace in a clean JVM: `clojure -M:test --namespace my.ns-test` or `--var my.ns-test/my-test`.
- Run the full suite: `clojure -M:test:test-parallel`. Each namespace runs in its own worker JVM.
- Lint: `clojure -M:lint`. Fix reflection warnings too.
- Check the native image: `clojure -T:build native`, then `clojure -M:test-native`. Passing JVM tests do not prove that the binary runs.
- Run Companion npm commands with `env -u NODE_OPTIONS`. Companion Android Gradle uses stock JDK 21.
- `clojure -M:audit` generates `audit/README.md` (network required). Never edit that file by hand.
- `harbor run --path e2e` with `blockether.vis.evals.agent:VisAgent` makes paid model calls. Use it only for editing tools and workflows.

## Layout

- `apps/vis-tui/` is a separate Clojure project. It uses the gateway over HTTP and never loads the engine.
- `packages/vis-contract/` holds the shared JSON Schemas and wire keys. Change every consumer with it: engine, TUI, Companion and SDK.
- `apps/vis-companion/` is one React and Vite bundle, wrapped by Capacitor (mobile) and Pake (desktop). Its controls are in `src/components/ui.tsx`.
- `apps/vis-docs/` is the Extension Center Worker and catalog. It is on the public site only, never a `doc()` page.

## Gotchas

- The engine also runs `packages/vis-agent/src/blockether/vis/extension.py`. Never mirror it.
- Sandbox: `sandbox/` is host policy, `python/` is host execution. Interpreter and guest runtime code belong in `~/vis-python-runtime`. Read its `AGENTS.md` before you change that repository. Put host shims in `resources/vis-host-python/`, without subdirectories.
- Shim docstrings generate the apropos resources. Regenerate them with `apropos-resource-test/regenerate!`.
- Derive vocabulary and bounds from the schemas in `packages/vis-contract/resources/vis-contract/schema/`, never from paired catalogs.
- Encode gateway output with `wire/->wire` and `wire/json-str`. A transport encoding failure can break event replay.
- `extension/core.clj` owns tool descriptions, params and wire keys.
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

## Tests

Write a test only for a defect that a person or another program would see.

### Write a test

- Name the defect before you write its test. If you cannot name a defect, do not write the test.
- Make each test fail when its behaviour breaks. To prove this, return nil or invert a condition, then see the test fail.
- For a bug fix, write the test first. See it fail for the reported reason.
- For a regression test, name the issue or the user report in its comment.
- Read the tests of a function before you add a test. Write one test for each rule or branch.
- Add a table row only for a case that takes a different branch.
- Put logic in a pure function, and test that function.
- Test a component only for a decision inside it, for example focus, order, paging or a race.
- Do not write a test only for coverage or a CRAP score. Split the function, or test a real branch.
- For a host, bootstrap or extension boundary change, test across that boundary. Do not drop required keys or validation because one consumer ignores them.

### Do not write these tests

- A value that goes in and comes back unchanged.
- A style, class, icon, DOM shape or exact column.
- A constant, default, declaration, registry entry or literal list.
- A control that calls its callback. Test the state change or the request instead.
- Prose: a prompt, label, help text or document.
- An internal call, a call count or a delegation.
- A test helper or a story fixture.
- A snapshot test, or a story play that only reads markup.

### Keep these tests

- A measured invariant, for example a contrast of 4.5 or more, or no line wider than the terminal.
- A value that another program reads. Test it against a schema or a fixture in `packages/vis-contract/`.
- A call count that is the requirement, for example a cache, a performance limit or idempotency. Name the report in its comment.
- A test that names an issue or a report, while its behaviour exists.

### Delete a test

Delete a test only when the lists above reject it or its behaviour is gone. A test that moves to another file, or keeps its body or a similar name, stays.

A deleted test is marked when it names an issue, a regression, a user report or `vis-contract/fixtures/`. Its body, the comment lines directly above it and a deleted file count. For each marked test, put a trailer with a real reason in the commit that deletes the test:

```text
Drops-Test: <file> <test name> - <reason>
Drops-Test: <file> - <reason>
```

The second form covers all tests of the file. Put these lines in the trailer block, with `Vis-Session:`.

- Before each push, run `bin/check-test-drops`. It checks the commits from the upstream branch to `HEAD`.
- If it reports a test, restore the test or add its trailer. Copy the trailer line from the report, and replace `<reason>`.
- If the commit is already pushed, add the trailer to an empty commit.

The `Test drops` workflow runs the same check on each push and pull request. A push to `main` gets its result only after the push.

### Lazytest

- Use Lazytest's own API: `[lazytest.core :refer [defdescribe describe it expect]]`. The runner ignores `clojure.test` without a warning.
- Do not use Lazytest's experimental interfaces. Use `set-ns-context!` and `around-each`, not `use-fixtures`.
- Group cases with `describe`. An `it` inside another `it` never runs.
- Delimiter (Parinfer) repair fixes syntax only. It does not prove that the code is correct.

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

- Before you change `resources/vis-docs/` or the README, read the `vis-docs` skill.
- Load a skill only when the user asks or its workflow applies. Keep upstream skills verbatim. A skill never overrides these rules or authorizes remote actions.
- Use `PLAN.md` only for multi-phase work: title, context, numbered phases with Rationale / Data / Acceptance criteria / Unknowns, and the plan state.
- For a user-authorized task, you can use computer-use automation with macOS apps and websites. Inspect the UI before you act and verify the result. This does not authorize unrelated account changes.
- Keep passwords, private keys and recovery codes out of transcripts and tracked files. Use private human input for authentication.
