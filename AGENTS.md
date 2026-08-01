# Vis repository guidance

Keep only non-obvious project contracts here; inspect nearby source and tests for detail.

## The JDK: GraalVM CE 25.1.3, always — LOCKED, not upgradable

- **The pin is LOCKED at 25.1.3 — nothing higher, for now.** `.graalvm-version` carries `GRAAL_PIN_LOCKED="true"` and `GRAAL_MAX_VERSION="25.1.3"`, and `GRAAL_VERSION` must equal `GRAAL_MAX_VERSION` exactly. That equality is *enforced*, not documented: `bin/require-graalvm` (`check_pins`, so every build path), `build.clj`'s `assert-graal-pins!`, `.github/actions/setup-graalvm-25` and `com.blockether.vis.graalvm-pin-test` all refuse a mismatch. Lifting the lock means moving both keys together — plus the tag, asset version, shas, `.sdkmanrc` and the `org.graalvm.*` pins — and only after `clojure -T:build native` has been proven to finish end to end on the new version.

- `.graalvm-version` is the SINGLE source of truth (edition, version, release tag, asset digests, expected `java.vendor.version`). `.github/actions/setup-graalvm-25`, the `Dockerfile`, `build.clj` and `bin/require-graalvm` all read it — never hardcode a version anywhere else. Bump it together with the `org.graalvm.*` pins in `deps.edn`.
- **Never move the pin to 25.2.x.** The 25.2 `native-image` builder cannot build this image: its points-to analysis needs a ~13–15 GiB live set for vis and never converges, so the build dies with `Terminating due to java.lang.OutOfMemoryError: Java heap space` at `-J-Xmx12g`, `-J-Xmx14g`, `-J-Xmx18g` and at the default cap (80% of RAM, which swap-thrashes the machine first). It is not a deadlock or an infinite loop — every ForkJoin worker is RUNNABLE inside the analysis while the old gen sits at 100% with ~41 full GCs per 95 s, advancing ~1k methods. 25.1.3 builds the same tree. Detail and measurements are in `.graalvm-version`.
- Native builds are memory-hungry even on 25.1.3 (~12 GiB live set), so `build.clj` no longer lets `native-image` size the builder JVM from RAM: it passes `-J-Xmx` (60% of physical RAM, clamped to 6–18 GiB) and `-J-Xms` itself. On a small/CI runner override it — `VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g' clojure -T:build native`; those env args are spliced last and win.
- **Community Edition, not Oracle GraalVM**: CE is GPLv2 + Classpath Exception, which is the only reason the shipped binary can be redistributed as FOSS (`audit/README.md` §4.1). Oracle GraalVM builds fine and is still wrong.
- **Exact version**: Truffle/SVM hard-refuse a JDK whose built-in Truffle differs from the pinned `org.graalvm.*` jars. "25.1.x" is not good enough.
- Local setup: `bin/require-graalvm --install` then `sdk env` (`.sdkmanrc`), or `eval "$(bin/require-graalvm --export)"`. `bin/require-graalvm --check` verifies the active JDK. `clojure -T:build native` never builds on the wrong JDK: it asks `bin/require-graalvm` for the pinned home, re-execs the task under an already-installed pinned CE, and when none is installed it installs the pin first and then re-execs — automatically, on a stock JDK and on Oracle GraalVM alike. Opt out with `:auto-install-graalvm false` (or `VIS_AUTO_INSTALL_GRAALVM=0`) to get the hard refusal back.
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
- `ios/` and `android/` are **gitignored**: Capacitor regenerates them, so native edits made there vanish on a fresh clone without anything failing. There is no custom iOS host any more — the app runs stock `CAPBridgeViewController`, and `scripts/ios-prepare.mjs` exists only to UN-stamp the old `VisBridgeViewController` from an `ios/` that predates its removal. Android capabilities are stamped by `scripts/android-prepare.mjs` (idempotent, run from `postsync`/`preandroid` and the release scripts). Never hand-edit `ios/` or `android/`; put it in the prepare script.

## Releasing the app ("release" = ship the companion to testers)

**One version everywhere.** The repo-root `VIS_VERSION` file is the single source of truth for CLI, native image, iOS, and Android. It must equal the latest regular `vX.Y.Z` Vis release; the regular release workflow no longer bumps it to a speculative next version. `apps/vis-companion/package.json` and its lockfile entries are MIRRORS stamped by `apps/vis-companion/scripts/version.mjs`. Never hand-edit an app-only version and never use `--version` to make the stores diverge from Vis.

There are two release paths:

1. **Regular Vis release:** update `VIS_VERSION` to `X.Y.Z`, run the companion version sync, commit and push, then create/push `vX.Y.Z`. The regular release must succeed first; its workflow then invokes the mobile workflow from that exact tag, releasing the same version to iOS and Android.
2. **App-only rebuild while Vis remains X.Y.Z:** commit and push the app fix, then run `npm run release:ios`, `npm run release:android`, or `npm run release:mobile` from `apps/vis-companion` (all three intentionally invoke the same two-store orchestrator). It atomically force-moves `companion-vX.Y.Z` to the newer `main` commit with a lease; that tag push releases both stores with unchanged marketing version and a new `git rev-list --count HEAD` build number. If `vX.Y.Z` already points at `HEAD`, it exits without retagging because the regular release owns that build.

Always commit and push first. `scripts/release-notes.mjs` derives TestFlight "What to Test" from committed history, and both stores share the git-derived build number. `release:ios:store` and `release:android:store` are workflow internals / one-store recovery tools; they must never use a marketing-version override during a normal release.

Before releasing, confirm the credentials exist (`security find-generic-password -s vis-ios -a asc_key_id -w`, `-s vis-play -a service_account -w`). CI reads the corresponding repository secrets. Android needs stock JDK 21 (`JAVA_HOME=~/.sdkman/candidates/java/21.0.11-tem`, never GraalVM).

- A TestFlight build can never be deleted, only **expired** (`expired: true`, one-way). Testers see builds grouped by the version string, and the newest *installable* one wins — an uploaded-but-never-distributed build keeps the older one on screen. `npm run release:expire` lists builds and, with `--yes`, expires all but the newest (`--keep N`, `--build <n>`, `--version <v>`, `--list`); it needs the same ASC key as `release:testflight`.
- Store recovery and notes need the App Store Connect API key in the macOS keychain service `vis-ios` (`asc_key_id`, `asc_key`, `asc_issuer_id`, `team_id`): `node scripts/secrets.mjs asc <AuthKey_XXXX.p8> --issuer <uuid> --team <id>`. Without it, CI skips iOS and `release:ios:store` falls back to the Apple ID signed into Xcode but cannot push notes or TestFlight distribution.
- `apps/vis-companion/CHANGELOG.md` is the source of truth for notes and an existing entry is never regenerated — hand-edit it, then re-push with `npm run release:notes -- --build <number>`.

## Gateway wire contract

- `gateway/wire.clj` is the deterministic boundary. Wire keys are snake_case strings; engine keys are mechanical kebab-case keyword mirrors.
- Boolean flags use wire `is_<foo>` and engine `:is-<foo>`; no `:foo?` aliases or endpoint-specific restoration.
- Use `wire/->wire` and `wire/json-str`; never hand-encode keyword keys.
- Encoding must be TOTAL. Charred rejects non-string map keys, NaN and ±Infinity, and the throw lands at the transport: `append-event!` has already stored the event, so one exotic value (a Python `Counter`, a pandas NaN) kills SSE and `/poll` for that session on every replay. `wire/->wire` and `persistance/->json` render those instead of throwing — keep any new encoder behind them.

## Feature toggles

IDs are snake_case strings. Hydrate from merged config so `/reload` applies project overrides; test registry, config coercion, and wire round-trips.

## Sandbox Python shims

One lazy shim per `shim_*.clj`, one registered extension, and inclusion in `builtin-extension-nses`. The Python body is NEVER a Clojure string: it lives in `resources/vis-shims/<name>.py` and the spec carries `:shim/source "vis-shims/<name>.py"` (read by `extension/shim-src`, embedded natively by build.clj's `-H:IncludeResources=vis-shims/.*`). Verify imports are absent at context creation and present after import.

## Python format/lint (ruff, in-process)

`format_code`/`lint_code` for Python run **ruff via the `com.blockether/ruff` FFI** (`extensions/languages/vis-language-python/.../ruff.clj`), never a `ruff` subprocess or a PyPI install — so it works in the native image and needs nothing on PATH. Configuration is ruff's own: the nearest `pyproject.toml` `[tool.ruff]` / `ruff.toml` / `.ruff.toml` above the target wins, including `select`/`ignore`/`per-file-ignores`/`line-length`, and relative globs are anchored at the config file's own directory. With no config file anywhere, ruff's own defaults apply and `lint_code` says so in its note — write a `ruff.toml` to make it project-specific. Only syntax errors and `E9xx`/`F6xx`/`F7xx`/`F82x` are reported at `error` level, everything else is a warning. A missing target is a failure, never a silent clean run.

The same engine is exposed to sandbox Python as the `ruff` shim (`src/com/blockether/vis/internal/foundation/shim_ruff.clj` + `resources/vis-shims/ruff.py`), so `vis python -m ruff check|format <paths>` works with no ruff installed. `ruff.toml` at the repo root is the project config, and the shims themselves are formatted with it.

Bump ruff itself in the sibling `Blockether/clj-ruff` repo (Rust `native/ruff-c`), release it (tag `vX.Y.Z` → the "Release & Deploy to Clojars" workflow), then move the pin in `deps.edn`.

## TUI rendering

Render paint code in the `vis-channel-tui` REPL with Lanterna `DefaultVirtualTerminal`; inspect the back-buffer. Dialogs use `dialogs/draw-dialog-chrome!` on flat `t/terminal-bg`, without panel tint or shadow.
