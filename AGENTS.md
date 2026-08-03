# Vis repository guidance

Read only the section relevant to the change. Keep this file for durable, repo-wide contracts; put local implementation detail beside the code and inspect source/tests before changing a contract.

## Engineering defaults

- Do not preserve backward compatibility. Remove obsolete paths instead of adding compatibility layers, fallbacks, or migrations.
- Choose the simplest implementation that fully meets the current requirement. Avoid speculative abstraction, configuration, and indirection.
- Grow the system in layers: start from the smallest version that works end to end, and add each capability on top of a product that already works. Never trade a working product for unfinished complexity.
- Keep components modular and concerns clearly separated.
- Prefer established, well-maintained libraries when they reduce overall complexity or improve reliability; do not reimplement common functionality without a clear reason.
- Lean on the dependencies already in the project before writing your own implementation or adding packages. Do not assume a library lacks a capability without checking its documentation and types.
- Make architectural decisions for the long term. Do not accept a stopgap that only works for now and is meant to be replaced later.

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
- The PIL shim's `ImageFont.truetype` takes a family NAME or a file path, and the requested face must reach both the draw op and the measurement or text is measured in one font and painted in another. Family comes from the file/name, weight and italic from the name stem (`…-Bold.ttf` → 700), and a variable font honors the `wght` axis — which is why one file can serve two weights.

## TUI rendering

Render paint work in the `vis-channel-tui` REPL using Lanterna `DefaultVirtualTerminal` and inspect its back-buffer. Dialog chrome uses `dialogs/draw-dialog-chrome!` on flat `t/terminal-bg`, without tint or shadow. A magit-style transient is a band INSIDE its host's frame: it repaints the frame edge on every row it covers and closes with the host's own `├───┤` rule directly above the hint bar, so the bottom chrome is never swallowed.

- **Look at the pixels, not only at assertions.** `test/…/channel_tui/capture.clj` drives a real paint and journals every frame: `(cap/capture-json! "/tmp/vis-frames.json" {:cols 120 :rows 40 :keys [\c :esc] :paint! (fn [{:keys [screen]}] …)})`; `cap/frame-text` flattens one frame to greppable text.
- `tools/tui_png.py` rasterizes that journal with the real theme colors. In `python_execution`: `exec(open("extensions/channels/vis-channel-tui/tools/tui_png.py").read(), globals())`, then `show_frames("/tmp/vis-frames.json", "/tmp/prefix", indexes=[1], label="…")` — it renders the PNGs **and** attaches them with `vis_attach`, so both the human and the model see the actual frame.
- The default face is the **JetBrains Mono this repo bundles** (`resources/vis-docs/assets/fonts/jetbrains-mono.woff2`): `_bundled` decompresses it once into `/tmp/vis-fonts/JetBrainsMono-{Regular,Bold}.ttf`, so a render is identical on any machine and needs nothing installed. Both weights are the SAME variable file — the host reads the weight off the file NAME and varies the `wght` axis. Host monospaces (`FONT_FAMILIES`) and the imaging-embedded `"Noto Sans Mono"` name (`EMBEDDED_FAMILIES`) are fallbacks only; to render in another face, put it first in `FONT_FAMILIES`. Never hardcode a font size: `_fit` measures real ink and sizes glyphs to the cell.
- Rendering reads the repo asset and writes `/tmp`, which a bare `ep/create-python-context {}` denies; pass the 2-arity roots-fn `(constantly ["/tmp" "/private/tmp" (System/getProperty "user.dir")])`.
- Keep both: the capture journal is what you eyeball, the lazytest terminal-grid assertions are the regression gate.

## Shipping verified work

- A change that is lint-clean, formatted, and covered by its own passing tests is **ready to push**. Commit it and push it to `main` in the same session, without being asked again — finished features must never be left sitting in the working tree.
- "Ready" is exactly: the smallest relevant `run_tests` namespaces pass, `lint_code` (clj-kondo + reflection) is clean, `format_code` has run, and the behavior is pinned by a test in the suite. Anything short of that stays uncommitted and is reported as unfinished.
- One commit per feature, imperative subject, production code and its tests together; no scratch, notes, or report files. Never pass `--no-verify` to this repo's own commits — the hooks are the gate.
- Push to `main` is the only automatic remote action. Tags, releases, store submissions, force pushes, and history rewrites still require an explicit request (see **Companion releases**).
