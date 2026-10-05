# Vis repository guidance

This file has only repository-specific decisions. General engineering practice applies. Read the
area contracts that your task touches. Keep API documentation in its namespace and invariants in
tests; do not copy them here.

## Simplified English

Write prompts, rules, descriptions, docs and commits in Simplified English, so that readers with
basic English and translation tools get the same meaning. Use the
[ASD-STE100](https://www.asd-ste100.org/) writing rules, not its dictionary:

- Write one instruction in each sentence, in the imperative and active voice, with any condition first.
- Use at most 20 words in an instruction, 25 in a description, 6 sentences in a paragraph and 3 words in a noun cluster.
- Give each paragraph one topic and each term one meaning. Use the most common word: "use", not "utilize".
- Use vertical lists for steps and conditions. In a warning, give the command first, then the risk.
- Keep names, code, paths, examples, limits, links, anchors and test-pinned phrases exact. Simplify the words, not the meaning or the rules.

## Work and verification

- Continue in-scope local edits, checks and fixes without asking at each step.
- Finish the requested behavior, not only a first implementation.
- If you are blocked, report the blocker and the remaining work. Do not substitute an adjacent fix or claim an unverified result.
- Choose checks for the changed files and behavior. Code needs the affected tests, formatting and lint, with reflection for Clojure. Documentation-only changes need content, link and diff checks, not a full application build.
- Reproduce a reported bug before you fix it. If an issue exists, reference it in the regression-test comment.
- Use the existing test infrastructure, not ad-hoc demos.
- If a full build fails outside your diff, you can still deliver when the affected tests and checks pass. Report that failure separately.
- If affected checks, hooks or a safe push are blocked, report the blocker and the remaining action. Never bypass checks or hooks.

## Reading budget

Each tool print goes to the model again on every later request of the session.

- Read a window around the region that you edit with `cat(path, start, end)`, not the whole file.
- Limit `grep` to the directories that can hold the symbol, and keep its default context.
- Batch reads for each edit target, not for each repository.
- Read a whole file only if a rule tells you to or you rewrite the file.

## Authorization

Simple, clear change requests, including regression fixes, have standing authorization: verify,
commit only the task's changes and push to `main` without asking again. The required checks must
pass, and the task's changes must separate safely from other work. An analysis-only, diff-preview,
local-only or no-commit/push request cancels this default. It never covers unrelated changes,
releases, deployments, live service restarts or history rewrites. For other tasks, commit and push
only when the user asks.

A task requested end to end is authorized to completion. This includes commits and pushes in every
repository that it touches. It also includes the `deps.edn` pin bump for a changed sibling repository,
and its release when its native libraries change. Decide these steps yourself. Only a Vis product release needs its
own explicit request. Report every commit, push and release in the final reply.

## Issue fixes

- After you push the verified fix, always inform the reporter and other users on the issue. Summarize the user-visible change, the verification and the commit.
- Close the issue when the fix fully resolves it; a closing commit can do this. Leave a partial or blocked fix open, and state what remains.
- An issue-fix request authorizes this closeout. A local-only or no-remote request cancels it.
- Report the fix and the issue status in the final reply.

## Git requests and commits

- Record the intended changes of a Git request before verification or staging.
- For `add all`, the scope is the staged, unstaged and untracked content at the start, not later work of other sessions.
- Check again before you stage and commit. Keep out-of-scope changes.
- If concurrent edits overlap the scope and you cannot separate them safely, pause the Git operation and report the conflict.
- Commit as the configured human identity, not `root`.
- Use a conventional subject, `type(scope): imperative summary`, under 72 characters, and a `Vis-Session: <bare-uuid>` trailer.
- For issue work, always put the issue number (for example, `#191`) in the subject. Write `Fixes #191` in the body when the commit fully resolves the issue.
- Limit other body text to the reason that the diff cannot show.

## Repository decisions

### Content

- Keep profanity and vulgarity out of tracked content: documentation, examples, activity text, quoted reports, fixtures and commit text.
- Paraphrase reports. Do not copy a user's words into documentation.
- Use the same terms in extension guides and executable examples.
- This repository is public. Put private deployment details and credentials in `infrastructure`, never here.
- In examples, use `127.0.0.1`, `10.0.0.5`, `gateway.example.com` and `visgw`.

### Activity presentation

- Give every observed tool binding an explicit Activity presentation, including each exported Python object method.
- Write activities for people: capitalized natural-language labels in clear English ("Run tests", "Search files"), not code identifiers or all-caps sentences.
- Set start visibility at the binding. Quick local reads, patches and lookups show only the end (`show_start=False` in Python, `:show-start false` in Clojure). Slow work shows running progress.
- Internal lifecycle tracking always keeps timing, failures and cancellation.
- Keep errors, meaningful counts and diffs. Never replace them with a generic result preview.
- Follow `resources/vis-docs/extension-api.md#activity-presentation`. Test registration and the running, success, failure and empty states.

### Code

- Do not add compatibility layers or migrations for obsolete APIs. Update the consumers and remove the old paths.
- Keep one engine and package, organized by domain under `src/com/blockether/vis/internal/`, with tests mirrored under `test/`.
- Add code to an existing owner, not to a new flat namespace or extension jar.
- The explicit vector in `resources/META-INF/vis/manifest.edn` sets the registration order, not classpath discovery. `build.clj` AOT-compiles every namespace; the manifest controls registration at runtime.
- Put shared leaf primitives in `internal.util`. Keep a helper with one caller local.
- Use `babashka.http-client` for outbound Clojure HTTP in production.
- Do not add Clojure `declare`.
- Format Clojure with `.zprint.edn` and the Vis formatter: one blank line between top-level forms, attached comments kept, one final newline.
- Treat `.clj-kondo/imports/` as tracked source, not a cache.

## Documentation

The README and `resources/vis-docs/` are for people, also when `doc()` serves them; they are not
instructions for agents. User guides address people who use Vis. Extension guides and the API
reference address developers who build with Vis.

- Start with the reader's goal: what the feature does, when to use it and how to get a useful result. If a chat or UI workflow exists, show it before internal calls.
- Start each page outside the Intro module with a `When to use` section. State the readers' problems in their words, link each to the section that solves it and name the better page for nearby problems. Describe the reader's situation, not the feature's capabilities.
- Address the reader as "you", in a clear, conversational and professional tone. Be direct and literal: no unexplained jargon, metaphors, slogans, filler or forced friendliness.
- The page contract in `docs/core.clj` enforces the sentence, paragraph and semicolon limits.
- Do not write apologies, defensive text, AI or generated-content disclaimers, or notes on how text or screenshots were made. State facts, actions and limitations directly. Give provenance only when it changes how the reader uses or verifies the information.
- Organize guides around tasks, with realistic examples and expected results. Before the reader acts, explain prerequisites, costs, permissions and destructive effects.
- Keep tutorials, task guides, explanations and API reference separate. Put low-level protocol details in labeled reference sections, not at the start. Do not copy agent prompts or operating checklists into user guides; explain their user-visible effects.
- Review headings, introductions, navigation labels and page descriptions, not only body text. A new reader must understand the purpose and the next step without knowledge of Vis internals.
- Also follow [Google: tone and style](https://developers.google.com/style/tone), [Microsoft: simple and human](https://learn.microsoft.com/en-us/style-guide/brand-voice-above-all-simple-human) and [Diátaxis: how-to guides](https://diataxis.fr/how-to-guides/) within these rules.

### Manual structure

Keep the modules of `resources/vis-docs/site.edn` in this order:

1. **Intro**: Rationale, Getting started, Running a gateway, then Reporting a bug.
2. **Concepts**: one page for each feature, such as sessions, drafts, council, automations, forms, live views and decision models.
3. **Programmatic access**: the **Basics** group (Python SDK, HTTP API and Extension API), then the **Feature APIs** group.
4. **Extensions**, then **Reference**.

- Give each concept `X` that a program can use one page `X-api.md` in **Feature APIs**.
- On an `X-api` page, give each example as a Python block, then as an HTTP block. Follow the `PAIRED VARIANTS` rule in `docs/core.clj`.
- Name gateway routes only in HTTP blocks and on the `http-api` page.
- If a feature has an `X-api` page, put its SDK calls and gateway routes there, not on the concept page.
- Link the `X-api` page from the concept page's `When to use` and `See also`.
- Inside a group, give a page a sidebar `:label` without the group name, for example `Sessions` under **Feature APIs**. Keep the full name in `:title`.
- On the public site, show the Extension Center link at the end of the **Extensions** module.
- In the README, call the introduction `Rationale` and list the manual in module order.
- `docs-modules-test` and `packages/vis-agent/tests/test_api_guides.py` check these rules.

## Computer-use automation

- For a user-authorized task, you can use the available computer-use automation (CUA) with macOS applications, including Xcode, and with websites.
- Inspect the current UI before you act, and verify the result.
- This permission does not make unavailable tools available. It does not authorize unrelated account changes.
- Keep passwords, private keys and recovery codes out of transcripts and tracked files. When authentication is necessary, use private human input.

## Area owners

Read a row only when you change that area. Paths are relative to this repository. Internal
namespace paths start at `src/com/blockether/vis/internal/`.

| Area | Owner and boundary |
|---|---|
| Python host API | The engine also runs `packages/vis-agent/src/blockether/vis/extension.py`. Never mirror it. |
| Sandbox | `sandbox/` defines host policy and process integration; `python/` implements host execution. The interpreter, handles, descriptor limits and guest runtime Python belong in `vis-python-runtime`; read its `AGENTS.md` before you change it. Host-call shims go in `resources/vis-shims/`, host guest modules in `resources/vis-guest/`. Do not copy runtime code. |
| Shims | `attach` and `ls` expose host functions; they do not replace Python packages. The Python docstrings in `resources/vis-shims/` generate the apropos resources; `apropos-resource-test/regenerate!` updates them. |
| Contracts | `packages/vis-contract/resources/vis-contract/schema/` owns the canonical JSON Schemas; Skjema validates portable shapes. Derive vocabulary and bounds from the schemas, never from paired catalogs. Callbacks, IO and mutable state stay local. |
| Tool declarations | `extension/core.clj` and its mirrored test own description, result, params, requiredness and wire keys. |
| Gateway transport | `packages/vis-contract/src/com/blockether/vis/contract/wire.clj` defines snake_case wire keys, kebab-case engine keys and total JSON encoding. Use `wire/->wire` and `wire/json-str`; a transport encoding failure can break event replay. |
| Config | `config/` owns the merged configuration. Toggle IDs are snake_case strings and reload from it. |
| Docs | `resources/vis-docs/` serves the site and `doc()`. `resources/META-INF/vis/apropos/docs.edn` is the catalog; only `resources/vis-docs/site.edn` owns titles and navigation. The `docs/core.clj` namespace docstring defines the page contract that `docs-page-canon-test` enforces. |
| Extension Center | The catalog is in `apps/vis-docs/`, not `resources/vis-docs/`. `worker.js` owns `/extensions/` and `/api/*` over D1 (`schema.sql`); `web/render.js` renders catalog pages in the Worker and the browser; `web/style.css` adds the catalog layout over the shared `resources/vis-docs/assets/theme.css`. The catalog is on the public site only, never a `doc()` page. |
| UI | Companion controls: `apps/vis-companion/src/components/ui.tsx`. TUI rendering: `apps/vis-tui/`. |
| Companion relay | `apps/vis-companion-relay/` hosts Push and Council Rooms. Push holds APNs/FCM keys and seals delivery grants in `src/seal.ts`, without a device-token store. `src/rooms/` uses separate credentials and D1 storage. Neither service accepts OAuth callbacks. `gateway/relay.clj` pushes through a grant; `gateway/push.clj` uses a self-hosted APNs key; `apps/vis-companion/src/lib/relay.ts` registers the grant. |
| Licensing | `bb scripts/gen-audit.bb` generates `audit/README.md` (network required); never edit it by hand. `audit_inventory_test` checks the dependency pins against it. |

## Gateway diagnostics

Send every gateway request that an agent starts through the canonical Clojure client, health checks
included:

```clojure
(require '[com.blockether.vis.internal.gateway.client :as gateway-client])
(gateway-client/request! :get "/healthz")
```

- Use the method and path of the route, and the optional `{:body … :headers … :timeout-ms …}`.
- The client finds or starts the daemon, gets a lease and supplies authentication.
- Do not read the gateway registry, copy secrets or build curl/httpx authentication by hand.
- Responses are maps and do not throw. A 4xx or 5xx response is data in `:status`.
- Print only the fields that the diagnosis needs. Never print a body that contains secrets.

## Tests and native builds

- Write Vis tests with Lazytest's own API: `[lazytest.core :refer [defdescribe describe it expect]]`.
- Do not use `clojure.test`: the runner does not find it and gives no warning. Do not use Lazytest's experimental interfaces, including `lazytest.experimental.interfaces.clojure-test`. `lazytest_policy_test` enforces this.
- Group cases with `describe`; an `it` inside another `it` never runs.
- Use `lazytest.core/set-ns-context!` and `around-each`, not `use-fixtures`.
- Run the affected namespaces in a clean JVM with `clojure -M:test`, optionally with `--namespace my.ns-test` or `--var my.ns-test/my-test`.
- Passing JVM tests or a successful native build do not prove that the binary runs. Interop changes need the relevant `test-native/` coverage against the built image (`-M:test-native`).
- Reachability metadata is inside jars under `META-INF/native-image/`; `native_reachability_test` pins the engine metadata.
- Read `.graalvm-version` before you change the locked GraalVM CE pin, and use `bin/require-graalvm` for setup. Never substitute Oracle GraalVM. Companion Android Gradle uses stock JDK 21.
- `e2e/run.py` makes paid model calls. Use it for editing tools and workflows, not for routine docs.
- Delimiter (Parinfer) repair fixes syntax only.
- A host, bootstrap or extension boundary change needs coverage across that boundary. Do not drop required keys or validation because one consumer ignores them.

## Skills and plans

- Keep upstream skills verbatim. Do not fork them silently.
- Load a skill only when the user asks for it or for its specific workflow. Ordinary coding does not activate a skill.
- A skill's style, persistence, test shortcuts and publishing recipes do not override repository contracts or the user's scope.
- Never infer authorization for remote actions from a skill.
- Use `PLAN.md` only for work that needs a maintained plan with many phases, not for every edit. Update it with the work that it tracks.
- A `PLAN.md` has a title, a phrase and context: current paths, problem and rejected alternatives. Then come numbered phases with Rationale / Data / Acceptance criteria / Unknowns, and the plan state.

## Releases (only when the user asks)

- `VIS_VERSION` is the version source. `npm run sync:version` in `apps/vis-companion` copies it to the package manifests and the SDK pyproject file; do not edit those version fields by hand.
- A product release bumps, mirrors, commits as `chore(release): vX.Y.Z`, pushes `main`, then pushes the annotated `v<VIS_VERSION>` tag. The tag, the version and the current `main` must agree.
- Never move a published tag. Publish a new version instead.
- An app-only rebuild keeps the version and uses the git commit count as the build number.
- Use `npm run release:ios:store` / `release:android:store` or `npm run release:mobile`, not both. Only `release:mobile` creates `companion-v<version>-build.<N>`. Never create that tag by hand or submit the same build both ways.
- Write the root `CHANGELOG.md` by hand: each product release commit adds its dated `## [vX.Y.Z] - YYYY-MM-DD` section. CI does not edit it; the release workflow drafts the GitHub Release with generated notes.
