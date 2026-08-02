# Vis repository guidance

Read only the section relevant to the change. Keep this file for durable, repo-wide contracts; put local implementation detail beside the code and inspect source/tests before changing a contract.

## This repo is public: never document Blockether's own deployment

- Blockether's hosted gateway is **private infrastructure**. Its public hostname, private bind address, ingress/Endpoints chain, server names, systemd units, and playbooks belong **only** in the private `infrastructure` repo — never in `README.md`, `AGENTS.md`, `resources/vis-docs/**`, `docker-compose.yml`, `Dockerfile`, code comments, tests, or commit messages.
- Docs and examples use neutral placeholders only: `127.0.0.1`, `10.0.0.5`, `gateway.example.com`, `visgw`.
- `com.blockether.vis.private-deployment-hygiene-test` scans the whole tree and fails on such a reference. Delete the reference; never add an exception for it.

## Toolchain: GraalVM CE 25.1.3 is locked

- `.graalvm-version` is the sole source of truth. `GRAAL_PIN_LOCKED`, `GRAAL_MAX_VERSION`, and `GRAAL_VERSION` must remain equal at **25.1.3**. `bin/require-graalvm`, `build.clj`, the setup action, and the pin test enforce this.
- Do not hardcode the JDK version elsewhere. A deliberate pin lift changes both keys, tag/assets/digests, `.sdkmanrc`, and `deps.edn` `org.graalvm.*` pins, and requires a successful end-to-end `clojure -T:build native` first.
- Never move to 25.2.x: native-image's points-to analysis does not converge within memory. The measurements and failure detail live in `.graalvm-version`.
- Use **GraalVM Community Edition**, not Oracle GraalVM. The exact version matters because Truffle/SVM and the pinned jars must match.
- Native builds normally set `-J-Xmx`/`-J-Xms` themselves (about 12 GiB live set). On constrained runners: `VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g' clojure -T:build native`; appended arguments win.
- Setup/check with `bin/require-graalvm --install`, `sdk env`, or `eval "$(bin/require-graalvm --export)"`; `clojure -T:build native` auto-installs/re-execs unless explicitly disabled.
- **Only Android Gradle is different:** `apps/vis-companion` needs stock JDK 21; do not switch it to GraalVM.

## Clojure tests: Lazytest

- Prefer the smallest relevant `run_tests` namespace; `only` takes fully qualified top-level Lazytest vars.
- Never require `clojure.test`: it is silently undiscovered. Use `[lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]` when that style is useful, and use `lazytest.core/set-ns-context!` plus `around-each` instead of `use-fixtures`.
- A managed REPL retains Vars. After editing, reload every changed production namespace, then every changed test namespace, before tests; there is no restart op — `stop` then `start` a fresh REPL when a clean load is safer.
- Clean JVM commands from the owning project: `clojure -M:test`, `clojure -M:test --namespace my.ns-test`, or `clojure -M:test --var my.ns-test/my-test`.

## Companion UI (`apps/vis-companion`)

- One responsive web/iOS/Android product: account for phone and desktop widths, touch, overflow, safe areas, virtual keyboards, and both themes.
- Tailwind v4 utilities only: no component CSS, CSS modules, CSS-in-JS, or inline styles. Use only `text-chip`, `text-meta`, `text-ui`, `text-body`, `text-title`, `text-subhead`, `text-head`, and `text-display`; no ad-hoc sizing or `leading-*`.
- Verify UI work with `npm run lint` and `npm run build` in this directory.
- Never edit generated `ios/` or `android/`. Put native behavior in the idempotent prepare scripts: `ios-prepare.mjs` removes legacy host stamping; `android-prepare.mjs` stamps Android capabilities.

## Companion releases

- `VIS_VERSION` is the single version source for CLI, native image, iOS, and Android. App package/lock versions are mirrors stamped by `scripts/version.mjs`; never hand-edit or override one store's marketing version.
- **Regular release:** update `VIS_VERSION`, sync the companion version, commit/push, then push `vX.Y.Z`. The regular workflow invokes mobile release from that tag.
- **App-only rebuild:** verify, commit, and push the app fix to `main`; then run exactly `npm run release:mobile` in `apps/vis-companion`. It creates immutable `companion-vX.Y.Z-build.N` for the current commit and releases both stores. Never create, move, reuse, or delete that tag manually. If `vX.Y.Z` is already at `HEAD`, it intentionally does nothing.
- `release:ios`/`release:android` are the same two-store orchestrator; `release:*:store` is only for recovery, never ordinary release flow. Notes derive from committed history; `CHANGELOG.md` is authoritative and existing entries are hand-edited, not regenerated.
- Confirm required keychain credentials before a requested release. Android release uses stock JDK 21, never GraalVM. TestFlight builds can only be expired, not deleted; use `release:expire` deliberately (`--yes` is destructive).

## Gateway wire contract

- `gateway/wire.clj` is the deterministic boundary: wire keys are snake_case strings and engine keys are mechanical kebab-case keywords. Booleans are `is_<foo>` / `:is-<foo>`, never `:foo?` aliases.
- Use `wire/->wire` and `wire/json-str`, never hand-encoded keyword keys.
- Encoding is total: non-string keys, NaN, and infinities must be rendered by `wire/->wire`/`persistance/->json` before transport. A transport throw after `append-event!` poisons SSE and `/poll` replay for that session.

## Feature toggles

Use snake_case string IDs. Hydrate from merged config so `/reload` respects project overrides; test registry, coercion, and wire round-trips.

## Sandbox Python shims and ruff

- One lazy `shim_*.clj` per shim, one registered extension, and `builtin-extension-nses` inclusion. Python lives in `resources/vis-shims/<name>.py`, referenced as `:shim/source "vis-shims/<name>.py"`; never embed it as a Clojure string. Verify lazy import behavior and native resource inclusion.
- Python format/lint uses in-process `com.blockether/ruff`, not a subprocess or PyPI install. Honor ruff's nearest-config resolution; missing targets fail, and only syntax plus `E9xx`/`F6xx`/`F7xx`/`F82x` are errors.
- The sandbox `ruff` shim supports `vis-agent python -m ruff check|format <paths>`; root `ruff.toml` configures this repo. Upgrade ruff in sibling `clj-ruff`, release it, then move the `deps.edn` pin.

## TUI rendering

Render paint work in the `vis-channel-tui` REPL using Lanterna `DefaultVirtualTerminal` and inspect its back-buffer. Dialog chrome uses `dialogs/draw-dialog-chrome!` on flat `t/terminal-bg`, without tint or shadow.
