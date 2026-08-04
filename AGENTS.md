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

## Fixing a reported bug: reproduce, RED, then GREEN

- Reproduce first, from the report's own steps, before touching the implementation. If it does not reproduce, that IS the finding: narrow or refute the report instead of fixing something adjacent.
- Turn the reproduction into a test in the suite and watch it **fail against the unfixed code** (RED), for the reported reason — not for a typo, a missing require, or a different error. A regression test nobody saw red proves nothing.
- Then apply the fix and rerun the same test unchanged (GREEN). In a managed REPL: load the pre-fix namespace, run the test, keep the failure text, reload the fixed namespace, rerun. Report both.
- Every regression test names its issue in a comment **on the test** — `;; Regression, issue #N: <what used to happen>` directly above the `defdescribe`/`it` (or a section banner carrying `(issue #N)`). The comment describes the wrong behavior, not what the code now does; it is the only link back to the report after the branch is merged.
- The fix and its test ship in the same commit. A fix without a red-then-green test is unfinished and stays uncommitted.

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
- **A machine owns its projects.** The sessions list is a fleet of paired gateways (`src/lib/fleet.ts`): machine → project → session, and the same repo checked out on two machines is two projects. Never merge rows by folder name across machines.
- The scope chip strip answers "which machine", and it answers it once: scoping hides the machine headers, retitles the header, narrows every count, and pins "New session" to that gateway. With one machine paired the strip, the chips, the machine count and even the section's machine landmark all disappear — the feature costs a solo user nothing, not one row and not one screen-reader stop.
- "New session" is a split control. `newSessionTarget` is `null` only when several machines are in scope; then both halves open the same portal menu, which asks "Create the session on" BEFORE the draft question — a workspace only exists on a machine. Unreachable machines are never offered (`creatableMachines`).
- One dead machine is a degraded section, not an error page (`fleetError`); but scoped to that machine its failure IS the screen (`scopeError`) — say "not answering", show the message and a Retry, and disable the create buttons instead of rendering "No sessions yet".
- The filter is a FLEET question: title/project matching and the server-side transcript search both fan out over `scopedConns`, so "All" searches every paired gateway and a chip searches exactly one. While a query is live the header stops reporting scope totals and reports the search instead (`searchTally`): "178 matches across 2 of 3 machines" is the only proof the query left this gateway, and a machine with no hit says "No matches on this machine", never "No sessions yet".

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

- **A form is an emphasis, not a list.** Exactly one field is where the keyboard is, and the paint says so: `human_input.clj` stamps every row of that field with `:is-active-field` (`:is-focused` stays the caret/stop meaning that scrolling and column tests depend on), so its label is bold `t/dialog-fg` and its description readable while every other label and description recede to `t/dialog-hint`. An input is drawn as an INPUT — `dialogs/draw-field-row!` fills the whole field width, `t/input-field-bg` when active and `(t/field-resting-bg)` when not, and marks the active one with a `▎` accent ring — so no `› ` prompt is needed and none is painted. OTP boxes ride the very same surface, with the terminal cursor parked inside the box the next digit lands in. Never hardcode `t/input-field-bg` for a placeholder or any other field detail: that is what made a resting empty field look focused. In `solarized-dark` all three surfaces coincide by design; the ring and the bold ink carry focus there.

- **Look at the pixels, not only at assertions.** One call does it: `(cap/shot! {:paint! (fn [{:keys [screen]}] …)})` from `test/…/channel_tui/capture.clj` drives a real paint and returns the PATH of a PNG (`<tmpdir>/vis-tui/shot.png`) — attach it with `vis_attach` so both the human and the model see the frame. `:out` names it, `:keys [\c :esc]` walks a dialog through its own key loop, `:frame` picks a flush (`:last` by default), `:grid` photographs a grid you already hold, `:font-size` sizes it, and `:trim` (on) crops the blank paper so the picture is the dialog and not an ocean. `cap/shots!` writes one numbered PNG per flush, `cap/frame-text` is the greppable check before rasterizing, `cap/png-rows` and `cap/ink` are the pixel assertions after it. Nothing else: there is one screenshot API and it hands back paths, never buffers.
- One frame per flush — always at least one, even from a paint that never refreshed — and every cell carries what the TERMINAL would show: `cinema/cell` keeps the character, fg, bg and `:bold`/`:italic`/`:underline`, resolves Lanterna's `DEFAULT` colour to the THEME's ink/paper (it reads back as ANSI black, which used to rasterize the whole app onto black paper) and applies the `REVERSE` swap at capture time. A paint that throws still yields its frames: `capture!` reports `:error`, `shot!` writes the picture and then re-throws.
- **A style the font does not ship is SYNTHESISED, never asked for.** The embedded mono face is one upright weight with no oblique cut, so `:weight 700`/`:italic true` paint the very same outlines back. `cinema/paint-grid!` strokes a bold run in its own colour (`size/24`), shears an italic run about its baseline on its own layer, and draws an underline as a bar under it — which is why the PNG finally reads like the real terminal.
- `cinema/grid->png!` draws one captured grid with the imaging cdylib at the same `cell-metrics` the MP4 screencast uses, and both go through `paint-grid!`, so a still and a frame of the video are the same picture. Box-drawing cells are painted as BARS through the cell centre, not as glyphs, so a border has no seams between columns. Nothing in this path needs Python, PIL, a JSON journal, or a font installed on the machine.
- Keep both: the PNG is what you eyeball, the lazytest terminal-grid assertions are the regression gate.

## Companion GUI design shots

UX proposals are **made in the app and photographed**, never described in prose. The TUI has `cap/shot!`; the companion has the design gallery.

- The harness lives in `apps/vis-companion`: `src/dev/fleet.ts` (fixture data), `src/dev/variants.tsx` (one exported component per proposal, driven by a `state` prop), `src/dev/DesignGallery.tsx` (the `DESIGN_VARIANTS` registry), `scripts/design-shots.mjs` (`npm run design:shots`).
- Dev-only route: `#/__design` lists the proposals, `#/__design?v=<id>&state=<state>&theme=light|dark` renders exactly one alone, so the viewport IS the proposal. `main.tsx` reaches it behind `import.meta.env.DEV`; nothing shipped imports `src/dev/**`.
- The **page** owns the matrix, not the script: the gallery publishes `window.__designShots` (`{id, state}` per registered state) and `design-shots.mjs` reads it, so adding a variant needs no script edit.
- Variants reuse the real chrome (`Header`, `TabBar`, `Shell` exported from `App.tsx`) and only Tailwind design tokens — a proposal that cannot be built with the design system cannot be photographed either.
- `design:shots` spawns vite, then per shot runs `spel open --viewport WxH`, `spel wait --fn window.__designReady`, `spel screenshot`, writing `/tmp/vis-ui/<variant>-<state>-<theme>-<viewport>.png`. Flags: `--only`, `--viewport`, `--theme`, `--out`, `--keep`.
- Always **both viewports** (390x844 phone, 1280x900 desktop) and **both palettes** (`applyTheme` with `BUNDLED_LIGHT`/`BUNDLED_DARK` in a layout effect, before the ready flag): the same amber that reads as an accent on paper is a flare on ink.
- Flip `window.__designReady` only after `document.fonts.ready` AND a paint. A shot taken earlier renders in a fallback face and every measurement in it is a lie.
- **Look at the pixels, not at the file list.** Attach every shot with `vis_attach("/tmp/vis-ui/<name>.png")` and actually read it: that is how `state === 'filter'` against a registry declaring `'filtered'` was caught — three "different" states had produced byte-identical PNGs. `PIL.Image.paste` is not bound in this sandbox, so attach the raw PNGs individually instead of composing a contact sheet.
- Give every design a state that can **falsify** it: the solo state (one machine paired, the whole concept must disappear) and a degraded state (a machine offline) are what expose a layout that only works in the demo.
- The gallery is production code: `npm run typecheck`, `npm run lint`, and `npm run build` must be clean before it ships.
- A design that survived the gallery is still unproven: shoot the SHIPPED screen against **real gateways** too. Point the dev server's connection list at live registry entries (`~/.vis/gateway/registry/*.edn`) with `spel storage local set CapacitorStorage.vis.connections '<json>'` (and the bare `vis.connections` key), include one unreachable URL, reload, and photograph the same states into `/tmp/vis-e2e/`. That is where "scoped to the offline machine" turned out to render `No sessions yet`.
- Drive that browser by **`@ref` from `spel snapshot -i -c`**, not by `find role button --name`: name matching is a substring match, so `--name 'New session'` also matches `aria-label="Choose which machine the new session runs on"` and the ambiguous click silently does nothing. If a control has no name in the snapshot, that is a real accessibility bug — give it an `aria-label` instead of working around it.
- When a click seems to do nothing, confirm it against `spel eval-js "…element.click()"` before blaming the app: the DOM path proves whether the handler or the locator is at fault. `spel console` dumps every entry ever captured — read `spel errors` instead.

## Shipping verified work

- A change that is lint-clean, formatted, and covered by its own passing tests is **ready to push**. Commit it and push it to `main` in the same session, without being asked again — finished features must never be left sitting in the working tree.
- "Ready" is exactly: the smallest relevant `run_tests` namespaces pass, `lint_code` (clj-kondo + reflection) is clean, `format_code` has run, and the behavior is pinned by a test in the suite. Anything short of that stays uncommitted and is reported as unfinished.
- One commit per feature, imperative subject, production code and its tests together; no scratch, notes, or report files. Never pass `--no-verify` to this repo's own commits — the hooks are the gate.
- Push to `main` is the only automatic remote action. Tags, releases, store submissions, force pushes, and history rewrites still require an explicit request (see **Companion releases**).
