# Vis repository guidance

Keep only non-obvious project contracts here; inspect nearby source and tests for detail.

## The JDK: GraalVM CE 25.1.3, always

- `.graalvm-version` is the SINGLE source of truth (edition, version, release tag, asset digests, expected `java.vendor.version`). `.github/actions/setup-graalvm-25`, the `Dockerfile`, `build.clj` and `bin/require-graalvm` all read it — never hardcode a version anywhere else. Bump it together with the `org.graalvm.*` pins in `deps.edn`.
- **Community Edition, not Oracle GraalVM**: CE is GPLv2 + Classpath Exception, which is the only reason the shipped binary can be redistributed as FOSS (`audit/README.md` §4.1). Oracle GraalVM builds fine and is still wrong.
- **Exact version**: Truffle/SVM hard-refuse a JDK whose built-in Truffle differs from the pinned `org.graalvm.*` jars. "25.1.x" is not good enough.
- Local setup: `bin/require-graalvm --install` then `sdk env` (`.sdkmanrc`), or `eval "$(bin/require-graalvm --export)"`. `bin/require-graalvm --check` verifies the active JDK; `clojure -T:build native` refuses to start on anything else.
- **The one exception**: the Android Gradle build in `apps/vis-companion` needs a *stock JDK 21* (Capacitor 8 compiles with `source 21`, and GraalVM's `jlink` cannot run AGP's `JdkImageTransform`). That is Gradle-only, deliberate, and enforced in `apps/vis-companion/scripts/android-release.mjs`. Do not "fix" it to GraalVM.

## Clojure tests (Lazytest)

- Tests use Lazytest. Prefer the `run_tests` language tool with the smallest relevant test namespace; `only` entries are fully qualified top-level test vars (usually `defdescribe` vars).
- Never require `clojure.test` in a test namespace: the Lazytest runner does not discover `clojure.test/deftest`, so such a namespace is silently skipped (prints nothing, exit 0). For `deftest`/`is`/`testing` style use `[lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]`, and replace `use-fixtures` with `lazytest.core/set-ns-context!` + `around-each`.
- A managed Clojure REPL runs already-loaded Vars and does **not** reload edited namespaces. After disk edits, evaluate `(require 'changed.production.ns :reload)` for every changed production namespace and then `(require 'changed.test.ns :reload)` for every changed test namespace before `run_tests`. Restart the REPL when a clean load is safer; do not use `:reload-all`.
- For a clean-JVM CLI run from the owning project directory: full suite `clojure -M:test`; namespace `clojure -M:test --namespace my.ns-test`; top-level var `clojure -M:test --var my.ns-test/my-test`. Repeat `--namespace` or `--var` to select several. Use the CLI when there is no suitable REPL or when clean process state matters.

## Companion UI (`apps/vis-companion`)

- One web/iOS/Android product. UI changes must handle phone and desktop widths, touch, overflow, safe areas, virtual keyboards, and light/dark themes.
- Use Tailwind CSS v4 utilities only; no component CSS, CSS modules, CSS-in-JS, or inline style objects.
- Use canonical type steps only: `text-chip`, `text-meta`, `text-ui`, `text-body`, `text-title`, `text-subhead`, `text-head`, `text-display`; no ad-hoc sizes or `leading-*`.
- Verify with `npm run lint` and `npm run build` in `apps/vis-companion`.

## Gateway wire contract

- `gateway/wire.clj` is the deterministic boundary. Wire keys are snake_case strings; engine keys are mechanical kebab-case keyword mirrors.
- Boolean flags use wire `is_<foo>` and engine `:is-<foo>`; no `:foo?` aliases or endpoint-specific restoration.
- Use `wire/->wire` and `wire/json-str`; never hand-encode keyword keys.
- Encoding must be TOTAL. Charred rejects non-string map keys, NaN and ±Infinity, and the throw lands at the transport: `append-event!` has already stored the event, so one exotic value (a Python `Counter`, a pandas NaN) kills SSE and `/poll` for that session on every replay. `wire/->wire` and `persistance/->json` render those instead of throwing — keep any new encoder behind them.

## Feature toggles

IDs are snake_case strings. Hydrate from merged config so `/reload` applies project overrides; test registry, config coercion, and wire round-trips.

## Sandbox Python shims

One lazy shim per `shim_*.clj`, one registered extension, and inclusion in `builtin-extension-nses`. Verify imports are absent at context creation and present after import.

## TUI rendering

Render paint code in the `vis-channel-tui` REPL with Lanterna `DefaultVirtualTerminal`; inspect the back-buffer. Dialogs use `dialogs/draw-dialog-chrome!` on flat `t/terminal-bg`, without panel tint or shadow.
