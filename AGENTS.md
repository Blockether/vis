# Vis repository guidance

Only what you would otherwise get wrong: repo decisions, local traps, and contracts you cannot infer from the code in front of you. General engineering practice is assumed, not restated. A rule owned by ONE namespace lives in that namespace's docstring and is only POINTED AT from here — the copy far from the code is the one that goes stale. Doctrine only some sessions need is a SKILL, PULLED with `doc("…")`; this file is PUSHED into every request of every session, so a paragraph here is paid for by every task that never touches its area.

## Hard rules

- **No profanity or vulgarity anywhere in the tree**, in every language — code, comments, docstrings, tests, test names, fixtures, commit messages, branch names, docs, UI copy, logs, error text, including scratch edits about to be deleted. **Quoting a bug report is not an exception:** paraphrase it (`;; Regression, issue #N: the dialog height jumped on toggle`), because the report's MEANING is what the comment owes the next reader. Remove any occurrence you find in the same commit as whatever brought you there.
- **This repo is public: never document Blockether's own deployment.** The hosted gateway's public hostname, private bind address, ingress/Endpoints chain, server names, systemd units and playbooks belong only in the private `infrastructure` repo. Docs and examples use neutral placeholders: `127.0.0.1`, `10.0.0.5`, `gateway.example.com`, `visgw`. `test/com/blockether/vis/private_deployment_hygiene_test.clj` scans the whole tree and fails on a reference; delete the reference, never add an exception.

## Area doctrine is PULLED, not pushed

Read the skill before working in its area — each one is the full contract, kept beside the code it governs:

- `doc("plan-md")` — the five parts of `PLAN.md`, before writing or restructuring the plan in flight.
- `doc("human-input")` — the HITL contract: parser, spec vocabulary, five check seams, TUI/companion mirrors.
- `doc("tui-rendering")` — TUI paint contracts, the virtual-terminal REPL, the screenshot gate.
- `doc("python-shims")` — adding or changing a sandbox Python shim, and in-process ruff.
- `doc("companion-ui")` — every Companion screen: control vocabulary, density, proposals, proof by numbers.
- `doc("release-vis")` / `doc("release-companion-hotfix")` — a product release / an app-only rebuild.
- `doc("issue-triage")`, `doc("ios-crash-triage")`, `doc("spel")` — triage and browser/native automation.

## Engineering defaults that differ from the usual ones

- **Do not preserve backward compatibility.** Remove obsolete paths instead of adding compatibility layers, fallbacks or migrations. Decide for the long term — a stopgap meant to be replaced later is not acceptable here.
- **All outbound HTTP in production Clojure uses `babashka.http-client`** — timeouts, HTTP versions, streaming, headers, non-throwing status. Never the JDK HTTP client, `URLConnection` or `HttpURLConnection`; keep direct JDK networking to URIs, sockets and embedded servers.
- **Never introduce Clojure `declare`.** Order definitions so every dependency precedes its consumers; refactor cycles instead of forward-declaring Vars.
- **Feature toggles** use snake_case string IDs and hydrate from merged config so `/reload` respects project overrides; test registry, coercion and wire round-trips.

## Clojure tests: Lazytest, not `clojure.test`

- **Never require `clojure.test` — it is silently undiscovered**, so the tests appear to pass by not running. Use `[lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]`, and `lazytest.core/set-ns-context!` plus `around-each` instead of `use-fixtures`.
- `run_tests` selects by `paths` ONLY — a test file, a directory, or the source file whose `*-test` namespace should run (there is no `ns`/`namespaces` selector; the pack refuses it). Prefer the smallest relevant path; `only` takes fully qualified top-level Lazytest vars. For tests-only verification call `run_tests` directly — its runner owns runtime setup. Use `repl_eval` when interactive inspection or stateful evaluation is part of the task.
- A managed REPL retains Vars, and `run_tests` reloads the namespaces it RUNS but never their dependencies: reload every changed PRODUCTION namespace before rerunning. There is no restart op — `stop` then `start` when a clean load is safer.
- Clean JVM: `clojure -M:test`, `--namespace my.ns-test`, `--var my.ns-test/my-test`.

## Fixing a reported bug

Reproduce from the report's own steps first; if it does not reproduce, that IS the finding — narrow or refute the report instead of fixing something adjacent. Watch the test fail against the unfixed code **for the reported reason**, not for a typo or a missing require. The fix and its test ship in the same commit.

- **Every regression test names its issue in a comment on the test** — `;; Regression, issue #N: <what used to happen>` above the `defdescribe`/`it`. It describes the WRONG behavior, not what the code now does; after merge it is the only link back to the report.
- **Keep extension layers proven and separate.** Clojure host, Python bootstrap and user-extension code are separate contracts. Begin in the layer the reproduction implicates; before crossing a language boundary, name the existing host callback, wire payload or failing end-to-end test that requires it, and add a test exercising the whole boundary. Never delete a semantic key, validation rule or requiredness marker because one path does not use it — trace its declaration, consumers and governing contract first.
- **Structural repair is syntax-only.** Delimiter/Parinfer repair makes the smallest mechanical change that restores parseable source — never a semantic rewrite, a broadened feature or a cross-language change. Inspect the repaired diff; reject it if it escaped the intended enclosing form.

## Shipping verified work

- "Ready" is exactly: the smallest relevant `run_tests` namespaces pass, `lint_code` (clj-kondo + reflection) is clean, `format_code` has run, and the behavior is pinned by a suite test. Anything short of that stays uncommitted and is reported as unfinished.
- Ready work is **committed and pushed to `main` in the same session without being asked again** — finished features must never be left sitting in the working tree. No scratch, notes or report files in a commit. Never pass `--no-verify`; the hooks are the gate.
- **Push to `main` is the only automatic remote action.** Tags, releases, store submissions, force pushes and history rewrites still require an explicit request.

## Toolchain: GraalVM CE 25.1.3 is locked

`.graalvm-version` is the sole source of truth and carries the full rationale — read it before touching the pin. `GRAAL_PIN_LOCKED`, `GRAAL_MAX_VERSION`, `GRAAL_VERSION` stay equal at **25.1.3**; `bin/require-graalvm`, `build.clj`, the setup action and the pin test enforce it, so do not hardcode the JDK version elsewhere. **Community Edition, never Oracle GraalVM** (Truffle/SVM and the pinned jars must match) and **never 25.2.x** (native-image's points-to analysis does not converge in memory). A deliberate lift changes both keys, tag/assets/digests, `.sdkmanrc` and the `deps.edn` `org.graalvm.*` pins, and requires a green `clojure -T:build native` first. Set up with `bin/require-graalvm --install`, `sdk env` or `eval "$(bin/require-graalvm --export)"`; native builds set their own `-J-Xmx`/`-J-Xms` (~12 GiB live set), and on a constrained runner `VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g' clojure -T:build native` wins. **Only `apps/vis-companion`'s Android Gradle differs: stock JDK 21, never GraalVM.**

## Gateway

- **Every agent-initiated gateway HTTP call goes through the canonical Clojure client** — debugging, reproduction, setup, health checks, verification, diagnostics. No one-off exceptions. Never parse `~/.vis/gateway/registry`, copy a gateway secret, or hand-build `curl`/`httpx` authentication. Require `[com.blockether.vis.internal.gateway.client :as gateway-client]` and call `(gateway-client/request! :get "/healthz")` with the route's real method/path and optional `{:body … :headers … :timeout-ms …}`: it resolves or starts the daemon, registers the client lease, supplies protocol/auth headers and JSON-encodes through the production `babashka.http-client` transport. It returns the raw non-throwing response map — 4xx/5xx are data in `:status`. Inspect only what the check needs (normally `(select-keys response [:status :body])`) and never print token- or secret-bearing bodies.
- **Wire contract** — `src/com/blockether/vis/internal/gateway/wire.clj` is the deterministic boundary: wire keys are snake_case strings, engine keys mechanical kebab-case keywords, booleans `is_<foo>` / `:is-<foo>` and never `:foo?`. Use `wire/->wire` and `wire/json-str`, never hand-encoded keyword keys. Encoding is total — non-string keys, NaN and infinities must be rendered by `wire/->wire`/`persistance/->json` before transport, because a transport throw after `append-event!` poisons SSE and `/poll` replay for that session.

## Companion (`apps/vis-companion`)

- **UI work pulls its doctrine first: `doc("companion-ui")`** — the closed `ui.tsx` control vocabulary, `className` may only POSITION, tokens, header bands, touch density, the fleet model, the dev server, RENDERED-and-ATTACHED design proposals, and proving a shipped screen with numbers off the live DOM. Verify there with `npm run lint` and `npm run build`; never edit generated `ios/` or `android/` — native behavior goes in the idempotent `scripts/ios-prepare.mjs` / `scripts/android-prepare.mjs`.
- **Releases pull theirs**: `doc("release-vis")` for a product release, `doc("release-companion-hotfix")` for an app-only rebuild. `VIS_VERSION` is the single version source for CLI, native image, iOS and Android; app package/lock versions are mirrors stamped by `scripts/version.mjs`, so never hand-edit one store's marketing version. `CHANGELOG.md` is authoritative and hand-edited.

