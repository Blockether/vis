# Vis repository guidance

Read only the section relevant to the change. This file holds **only what you would otherwise get wrong** — repo decisions, local traps, and contracts you cannot infer from the code in front of you. General engineering practice is assumed, not restated. A rule that belongs to ONE namespace lives in that namespace's docstring and is only POINTED AT from here — two copies of a contract drift, and the copy far from the code is the one that goes stale.

## No profanity, anywhere (HARD RULE)

No profanity or vulgarity anywhere in the tree — code, comments, docstrings, tests, test names, fixtures, commit messages, branch names, docs, UI copy, logs, error text — in every language, including scratch edits about to be deleted. **Quoting a bug report is not an exception:** paraphrase it (`;; Regression, issue #N: the dialog height jumped on toggle`), because the report's MEANING is what the comment owes the next reader. Remove any occurrence you find in the same commit as whatever brought you there.

## Engineering defaults that differ from the usual ones

- **Do not preserve backward compatibility.** Remove obsolete paths instead of adding compatibility layers, fallbacks, or migrations. Decide for the long term — a stopgap meant to be replaced later is not acceptable here.
- **All outbound HTTP in production Clojure uses `babashka.http-client`** — timeouts, HTTP versions, streaming, headers, non-throwing status. Never the JDK HTTP client, `URLConnection`, or `HttpURLConnection`; keep direct JDK networking to URIs, sockets, and embedded servers.
- **Never introduce Clojure `declare`.** Order definitions so every dependency precedes its consumers; refactor cycles instead of forward-declaring Vars.

## Planning artifacts: `PLAN.md` has exactly five parts

`PLAN.md` at the repo root is the plan for the work in flight, and there is **one at a time**. It has these five parts in order and nothing else — no status chatter, no changelog, no notes to self.

1. **Title** — `# PLAN — <the change, imperative>`. Names what is being done, not the area it touches.
2. **Catchy phrase** — one line: the thesis someone repeats back in review. *Python is the general instrument; a native tool survives only if it is a jail.*
3. **Context** — the STATE BEFORE (`file:line` for every claim, measurements not impressions), the ROOT PROBLEM (the force under the symptom), WHAT WE SOLVE and what we explicitly do not, and the ALTERNATIVES CONSIDERED each with the reason it lost. An alternative with no recorded reason gets re-proposed at the next review.
4. **Proposal phases** — one `## Phase N — <the verb it performs>` each, ordered so every phase lands on a product that already works. Each carries exactly four things: **Rationale** (what stays broken without it); **Data** (a `clojure.spec.alpha` block ONLY when the phase changes data that CROSSES a boundary — persisted on disk, sent over the wire, or a contract another language mirrors — written in the plan before the code, no prose schemas and no second schema library; a function's internal argument shape, a map that never leaves one namespace, and data the phase merely deletes are NOT data changes, and that phase writes `**Data.** None.` with the one line saying why); **Acceptance criteria** (the files it changes, one line each, plus the test that proves it done); **Unknowns** (as questions — a phase with none says so).
5. **State of the plan** — **ACCEPTED**, **REQUIRES WORK**, or **DONE**, then what is done per phase with its commit, and a TODO list of the rest in order. The only part edited as work lands, and it is edited in the same commit as the work it records.

## This repo is public: never document Blockether's own deployment

Blockether's hosted gateway is **private infrastructure**. Its public hostname, private bind address, ingress/Endpoints chain, server names, systemd units, and playbooks belong only in the private `infrastructure` repo — never in this tree. Docs and examples use neutral placeholders: `127.0.0.1`, `10.0.0.5`, `gateway.example.com`, `visgw`. `test/com/blockether/vis/private_deployment_hygiene_test.clj` scans the whole tree and fails on a reference; delete the reference, never add an exception.

## Toolchain: GraalVM CE 25.1.3 is locked

- `.graalvm-version` is the sole source of truth and carries the full rationale. `GRAAL_PIN_LOCKED`, `GRAAL_MAX_VERSION`, `GRAAL_VERSION` stay equal at **25.1.3**; `bin/require-graalvm`, `build.clj`, the setup action and the pin test enforce it. Do not hardcode the JDK version elsewhere.
- **Community Edition, never Oracle GraalVM** — Truffle/SVM and the pinned jars must match. **Never 25.2.x:** native-image's points-to analysis does not converge in memory.
- A deliberate pin lift changes both keys, tag/assets/digests, `.sdkmanrc` and the `deps.edn` `org.graalvm.*` pins, and requires a green `clojure -T:build native` first.
- Setup with `bin/require-graalvm --install`, `sdk env`, or `eval "$(bin/require-graalvm --export)"`. Native builds set their own `-J-Xmx`/`-J-Xms` (~12 GiB live set); on constrained runners `VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g' clojure -T:build native` — appended arguments win.
- **Only Android Gradle differs:** `apps/vis-companion` needs stock JDK 21; never GraalVM.

## Fixing a reported bug

Reproduce from the report's own steps first; if it does not reproduce, that IS the finding — narrow or refute the report instead of fixing something adjacent. Watch the test fail against the unfixed code **for the reported reason**, not for a typo or a missing require. The fix and its test ship in the same commit.

- **Every regression test names its issue in a comment on the test** — `;; Regression, issue #N: <what used to happen>` above the `defdescribe`/`it`. It describes the WRONG behavior, not what the code now does; after merge it is the only link back to the report.
- **Keep extension layers proven and separate.** Clojure host, Python bootstrap and user-extension code are separate contracts. Begin in the layer the reproduction implicates; before crossing a language boundary, name the existing host callback, wire payload or failing end-to-end test that requires it, and add a test exercising the whole boundary. Never delete a semantic key, validation rule or requiredness marker because one path does not use it — trace its declaration, consumers and governing contract first.
- **Structural repair is syntax-only.** Delimiter/Parinfer repair makes the smallest mechanical change that restores parseable source — never a semantic rewrite, a broadened feature or a cross-language change. Inspect the repaired diff; reject it if it escaped the intended enclosing form.

## Clojure tests: Lazytest, not `clojure.test`

- **Never require `clojure.test` — it is silently undiscovered**, so the tests appear to pass by not running. Use `[lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]`, and `lazytest.core/set-ns-context!` plus `around-each` instead of `use-fixtures`.
- Prefer the smallest relevant `run_tests` namespace; `only` takes fully qualified top-level Lazytest vars. For tests-only verification call `run_tests` directly — its runner owns runtime setup. Use `repl_eval` when interactive inspection or stateful evaluation is part of the task.
- A managed REPL retains Vars: reload every changed production namespace, then every changed test namespace, before rerunning. There is no restart op — `stop` then `start` when a clean load is safer.
- Clean JVM: `clojure -M:test`, `--namespace my.ns-test`, `--var my.ns-test/my-test`.

## Gateway HTTP API calls

- **Every agent-initiated gateway HTTP call goes through the canonical Clojure client — debugging, reproduction, setup, health checks, verification, diagnostics. No one-off exceptions.** Never parse `~/.vis/gateway/registry`, copy a gateway secret, or hand-build `curl`/`httpx` authentication. Require `[com.blockether.vis.internal.gateway.client :as gateway-client]` and call `(gateway-client/request! :get "/healthz")`, passing the route's real method/path and optional `{:body … :headers … :timeout-ms …}`. It resolves or starts the daemon, registers the client lease, supplies protocol/auth headers, and JSON-encodes through the production `babashka.http-client` transport.
- `request!` returns the raw non-throwing response map — 4xx/5xx are data in `:status`. Inspect only what the check needs (normally `(select-keys response [:status :body])`), and never print token- or secret-bearing bodies.

## Companion web dev server

- For Companion development and browser inspection run exactly `npm run dev` from `apps/vis-companion` — never Vite directly, never hand-copied gateway URLs/tokens. It discovers every live entry in `~/.vis/gateway/registry/*.edn`, maps wildcard binds to loopback, and seeds both Companion storage mirrors before boot. Its token-bearing page defaults to `127.0.0.1`; pass `--host` only when deliberately exposing it. This is not permission to bypass `gateway-client/request!` for direct API calls.
- Report the URL only after it responds. **Reuse an already-running server** serving that app: a healthy user-requested dev server is persistent user infrastructure, not a disposable test process. Stop it only when the user asks, it is unhealthy, or it must be replaced — never as automatic REPL-lifecycle cleanup, and if a higher-level policy demands a stop, state that conflict first.

## Companion UI (`apps/vis-companion`)

One responsive web/iOS/Android product: phone and desktop, touch, overflow, safe areas, virtual keyboards, both themes. These rules say what the screen should FEEL like. The figures that prove a screen obeys them are never written here — they are read off the live DOM (see **Proving SHIPPED companion UI**) and pinned in `ui.test.tsx` and `scripts/touch-density.test.mjs`.

- **The control vocabulary is CLOSED and `src/components/ui.tsx` owns it** — `Button`, `IconButton`, `KebabButton`, `NewSessionButton`, `DialogClose`, `Modal`, `DialogFrame`/`DialogHeader`, `Menu`/`MenuHeading`/`MenuItem`, `SectionHeader`, `HeaderTitle`, `HeaderActions`, `HeaderMeta`/`HeaderTally`, `LiveCount`, `MachineRail`/`MachineMark`, `MachineSwitcher`/`MachineTab`. Use them by name; never hand-roll a `<button className="…">` that resembles one. Two controls that mean the same thing must never look like two different things. Grep `ui.tsx` first; at the second call site extract it in the same commit and pin it in `ui.test.tsx`.
- **`className` at a call site may only POSITION** (`absolute`, `flex-1`, `shrink-0`, a grid cell) — anything you can SEE is a prop or a variant on the component, because competing utilities are settled by Tailwind's emission order and never by which call site typed them. Spacing belongs to the owner, outer edge included: `HeaderActions` pads the header's trailing side, so the header does not. `ui.test.tsx` scans every screen for a paint utility handed to a `ui.tsx` control and fails on it, so the rule is a test rather than a memo.
- **Tokens only.** Tailwind v4 utilities — no component CSS, CSS modules, CSS-in-JS or inline styles — and only the scale `text-chip`, `text-meta`, `text-ui`, `text-body`, `text-title`, `text-subhead`, `text-head`, `text-display`; no ad-hoc sizing, no `leading-*`.
- **A header is a BAND, and the list has one of it.** Machine header, project header and the loading skeleton are all `SectionHeader` — `tone` is the only difference, so scrolling past them feels like one rhythm rather than a stack of near-misses. The band owns its own height and paper; a control inside centres itself and never sets the row height, and the band never pads around a control that is already a full touch target. Neither side edge is spelled on the band: the leading one belongs to `HeaderTitle` so a name's hover reaches the screen edge, the trailing one to `HeaderActions`. A header REPORTS through `HeaderMeta` and counts through `HeaderTally` — never a fixed column inside the title, which starves the name the user actually came for to keep a tally whole.
- **A field is tall enough for the line it holds.** Text must never be clipped inside its own box, and the box must not jump on the first keystroke.
- **An iPad is a wide TOUCH device, so density follows the pointer, never the width.** `sm:` answers "is there room" and owns LAYOUT — which columns exist, where the tab bar goes, how wide the paper is. Only `mouse:` (defined in `src/index.css`) may make a control TIGHTER, because only a cursor earns that rhythm; a trackpad earns it too. A wide screen may breathe — what it may never do is shrink what a finger has to hit. `scripts/touch-density.test.mjs` reads the component tree and fails on a violation.
- **A machine owns its projects.** The sessions list is a fleet of paired gateways (`src/lib/fleet.ts`): machine → project → session, and the same repo on two machines is two projects. Never merge rows by folder name across machines.
- The scope chip strip answers "which machine" **once**: scoping hides the machine headers, retitles the header, narrows every count and pins "New session" to that gateway. With one machine paired the strip, chips, machine count and the machine landmark all disappear — the feature costs a solo user nothing, not one row and not one screen-reader stop.
- **"New session" is a yellow BUTTON on every project header, immediately before that header's `⋯`** (`NewSessionButton`, inside `HeaderActions`): the verb the screen exists for costs no menu. It creates in the project whose header it sits on and sends that group's canonical workspace root, never the home-shortened display path; it NAMES its machine (`aria-label`) and puts the project on its `title`, because several machines are on screen at once. An unreachable machine offers no create button (`creatableMachines`). The machine header's `⋯` keeps the rarer half — a draft, another project, Manage projects, machine settings — and when `newSessionTarget` is `null` the menu asks "Create the session on" BEFORE the draft question, since a workspace only exists on a machine.
- One dead machine is a degraded section, not an error page (`fleetError`); scoped to that machine its failure IS the screen (`scopeError`) — say "not answering", show the message and a Retry, and disable the create buttons instead of rendering "No sessions yet".
- The filter is a FLEET question: title/project matching and server-side transcript search both fan out over `scopedConns`. While a query is live the header reports the search instead of scope totals (`searchTally`) — "178 matches across 2 of 3 machines" is the only proof the query left this gateway, and a machine with no hit says "No matches on this machine", never "No sessions yet".
- Verify with `npm run lint` and `npm run build` here. Never edit generated `ios/` or `android/`; native behavior goes in the idempotent `scripts/ios-prepare.mjs` / `scripts/android-prepare.mjs`.

## Companion UI proposals are RENDERED and ATTACHED (HARD RULE)

**A design proposal is a real, RESPONSIVE Tailwind page attached to the answer as an `.html` file with `vis_attach`. Never a PNG, never a screenshot, never ASCII, never in the application.** A picture cannot be resized, so it cannot prove a layout; box-drawing characters cannot show a token, a weight, a radius or a touch target. One `index.html` per answer holds every numbered version, and the human opens it and drags the window.

- **One shared `index.html`, attached as HTML.** `vis_attach(path, filename="index.html")` — the same filename every time, so each revision becomes that proposal's next version. Never attach a rendering as an image, and never split versions across files.
- **IMPORT the product, do not re-type it.** The proposal is a React entry that imports `apps/vis-companion/src/components/ui.tsx` and `src/index.css` DIRECTLY, built by Vite into one self-contained `index.html`. A hand-written copy of a class string is a fork of the design system that drifts the moment `ui.tsx` changes, which is exactly how a proposal ends up not looking like the app. The recipe, in a scratch dir outside the tree (e.g. `/tmp/vis-mock`): `node_modules` symlinked to `apps/vis-companion/node_modules`; a `mock.css` that is `@import '<app>/src/index.css'; @source '<app>/src'; @source './main.tsx';` — **both `@source` lines are required**, or Tailwind emits nothing for `ui.tsx` and every control renders unstyled; a `vite.config.mts` with `react()` + `tailwindcss()`, `assetsInlineLimit` huge and `cssCodeSplit: false`; then inline the built CSS/JS into `dist/index.html` and attach that one file. Only exported members are available (`LIST_EDGE`, `LIST_EDGE_END`, `LIST_FRAME` are; `HEADER_BAND`, `LIST_TRAIL` are not) — compose with the exported components rather than re-typing a private constant.
- **Responsive BY DEFAULT, phone first.** The page is authored at 390px and must stay correct — no clipping, no horizontal scroll, no shrunken touch target — from 320px up through desktop, using the product's real `sm:` and `mouse:` rules. Versions stack in one column on a phone and sit side by side on a wide screen; a fixed-width mockup canvas is a rejected proposal. Check it at 320, 390, 768 and 1440 with `spel set viewport` before attaching.
- **Zero repo files.** The proposal HTML lives in a scratch directory outside the tree; it is never committed, never a variant component, a dev route, a gallery, a fixture screen or a screenshot script. `spel` is only for CHECKING it (`spel open`, `set viewport`, `set media dark`, `eval-js`); the artifact reaches the human through `vis_attach`.
- **Use the components by NAME, with their real props** — `SectionHeader`, `HeaderTitle` (`name`/`qualifier`), `HeaderActions`, `HeaderMeta`/`HeaderTally`/`LiveCount`, `NewSessionButton` (`machine`/`where`/`onPress`), `KebabButton` (`label`/`isOpen`), `MachineSwitcher`/`MachineTab`/`MachineRail`/`MachineGap` (rail colour from `lib/machine-colors`), `ListRow`, `Modal`/`DialogFrame`/`DialogHeader`, `Button`, `BackButton`, `Pager`. A control invented for the mockup is a proposal about a product that does not exist; a variant that does not exist yet is a NEW component named in the answer, written beside them in the same file, and it is the thing the human is accepting. `className` at a call site still only POSITIONS.
- **The mock is INTERACTIVE — every control does the thing it claims.** A dead picture in HTML is still a picture. React state drives it: a session row PUSHES the detail screen (with a Back that returns), `NewSessionButton` opens the real `Modal` naming which machine and which canonical workspace root it would send, `KebabButton` opens its menu with the actual items, a `MachineTab` scopes and says what scoping changes. Verify every path with `spel eval-js "…element.click()"` and check `spel errors` before attaching — and read the DOM in a SEPARATE `eval-js` call after the click, because a React state flush is not visible in the same synchronous expression that dispatched it.
- Always offer SEVERAL numbered versions, each rendered in the state that can falsify it: one machine paired, a machine not answering, a long name, a project with no sessions.
- Nothing is written to the repo until the human picks a version; then it is built once, in the real screen, with its tests. Proof of the SHIPPED screen is still numbers off the live DOM (see the next section) — a proposal page is never evidence about shipped code.
- Legacy `src/dev/**` design-gallery files are not to be extended — delete them when a change brings you into that code.

## Proving SHIPPED companion UI

Proof is about the screen that ships, never about a proposal. Run the real app with `npm run dev` and inspect it.

- **`spel` is the ONE browser instrument, and every step of a proof is one of its subcommands** — `spel open`, `spel set viewport 390 844` / `set device`, `set media dark`, `snapshot -i -c`, `get box`, `eval-js`, `errors`. Never bring a second driver to the same screen (a hand-written Playwright/Puppeteer script, a browser extension, a one-off headless Chrome): a figure produced by a tool the next reader does not run is a figure nobody can reproduce.
- Drive by **`@ref` from `spel snapshot -i -c`**, not `find role button --name`: name matching is a SUBSTRING match, so `--name 'New session'` also matches `aria-label="Choose which machine the new session runs on"` and the ambiguous click silently does nothing. A control with no name in the snapshot is a real accessibility bug — give it an `aria-label` rather than working around it.
- When a click seems to do nothing, confirm against `spel eval-js "…element.click()"` before blaming the app: the DOM path proves whether the handler or the locator is at fault. Read `spel errors`, not `spel console` (which dumps every entry ever captured).
- **A layout claim is proven by NUMBERS, never by looking at a screenshot.** Every geometric assertion — an edge, a gutter, an indent, a hit box, a baseline, an overflow — is read out of the live DOM with `spel eval-js` and `getBoundingClientRect()` (`spel get box <sel>` when one element is the whole question), once per viewport that decides the layout, and REPORTED as the figures. Measure after fonts load and after any open/close transition settles. A screenshot may illustrate the figures; it is never the evidence for them:

  ```
  spel --session s eval-js '(() => {
    const at = (sel) => { const r = document.querySelector(sel)?.getBoundingClientRect();
      return r && { l: Math.round(r.left), r: Math.round(r.right), w: Math.round(r.width), h: Math.round(r.height) }; };
    return JSON.stringify({ kebab: at("button[aria-label^=Actions]"), row: at("[data-session-id]") });
  })()'
  ```

  What that catches and an eye does not: two trailing controls ending at 378 and 390; a name at x=28 above a name at x=36; a 44px thumb target that quietly became 32; a panel whose `top + height` exceeds `innerHeight`, which is a primary button below the fold.

## Companion releases

- `VIS_VERSION` is the single version source for CLI, native image, iOS and Android. App package/lock versions are mirrors stamped by `scripts/version.mjs` — never hand-edit one store's marketing version.
- **Regular release:** update `VIS_VERSION`, sync the companion version, commit/push, then push `vX.Y.Z`; that workflow invokes mobile release from the tag.
- **App-only rebuild:** verify, commit and push the fix to `main`, then PREFLIGHT this machine (`npm run secrets -- doctor`, `xcodebuild -version`, stock JDK 21). If credentials and toolchains are here, build and upload LOCALLY with `npm run release:ios:store` and `npm run release:android:store` — no tag, no CI wait. Only when a leg is not runnable here, fall back to `npm run release:mobile`, which creates immutable `companion-vX.Y.Z-build.N` for the current commit. Never create, move, reuse or delete that tag manually, and **never tag after a local upload** — that ships the same build number twice. If `vX.Y.Z` is already at `HEAD`, `release:mobile` intentionally does nothing.
- `release:ios`/`release:android` are the same two-store orchestrator; `release:*:store` is for an app-only rebuild and recovery, never a regular product release. Notes derive from committed history; `CHANGELOG.md` is authoritative and hand-edited.
- Confirm keychain credentials before a requested release. Android release uses stock JDK 21, never GraalVM. TestFlight builds can only be expired, not deleted — `release:expire` is deliberate (`--yes` is destructive).

## Gateway wire contract

`src/com/blockether/vis/internal/gateway/wire.clj` is the deterministic boundary: wire keys are snake_case strings, engine keys are mechanical kebab-case keywords, booleans are `is_<foo>` / `:is-<foo>` and never `:foo?`. Use `wire/->wire` and `wire/json-str`, never hand-encoded keyword keys. Encoding is total — non-string keys, NaN and infinities must be rendered by `wire/->wire`/`persistance/->json` before transport, because a transport throw after `append-event!` poisons SSE and `/poll` replay for that session.

## Human input (HITL) contract

The contract documents itself where it lives; read the docstring before changing either layer. `src/com/blockether/vis/internal/human_input.clj` PARSES, `internal/human_input/spec.clj` DECLARES the normalized form and OWNS the closed vocabulary, `internal/human_input/validation.clj` decides validator arity, `src/com/blockether/vis/human_input.clj` is the builder surface that refuses at the call site, and `internal/extension_check.clj` judges a Python extension without running it. Repo-wide rules that no single namespace can own:

- **No schema library beyond `clojure.spec.alpha`,** and never a second copy of the vocabulary beside the parser. Keys are added once in `spec.clj`; the parser DERIVES the snake_case spellings it accepts (`wire-keys`), so a key reaches the wire from one edit. The spec is checked at five seams and nowhere else — `checked-field`, `checked-group`, `checked-decor`, `checked-request`, and `checked-answer` inside `settle!`, the one funnel every answer passes through — once per request, never per keystroke. Views are NOT specced: `request->view` strips `:is-secret` and `:validate`.
- **The spec is the vocabulary of every SURFACE, not only the engine.** The TUI reads `text-types`, `choice-types`, `range-defaults` and `otp-defaults` off `human-input.spec`; the companion, which cannot require a Clojure namespace, mirrors them as `HUMAN_INPUT_FIELD_TYPES`, `HUMAN_INPUT_DECOR_TYPES`, `HUMAN_INPUT_NODE_TYPES`, `HUMAN_INPUT_RANGE_DEFAULTS`, `HUMAN_INPUT_OTP_DEFAULTS`, `HUMAN_INPUT_CHOICE_MARKS`. That mirror is not trusted: `human_input_cross_channel_test.clj` READS the TypeScript and fails when a type, a bound or a choice glyph drifts from the engine's table or from `dialogs/choice-mark`, so `●`/`○`/`[✓]`/`[ ]` mean the same thing in the terminal and in the app.
- **`human-input.fixture.json` is `request->view` verbatim,** and the Clojure suite pins that it holds one node of EVERY kind — so rendering it in `HumanInputPrompt.test.tsx` is the app's proof of complete support.
- **The two builder surfaces are ONE vocabulary with one spelling per node** — `com.blockether.vis.human-input` (Clojure) and the `vis.*` block in `resources/vis-python/extension_bootstrap.py`. A new host callback is registered in `host-member-names` or the static checker breaks with a `NameError`.

## Feature toggles

Use snake_case string IDs. Hydrate from merged config so `/reload` respects project overrides; test registry, coercion, and wire round-trips.

## Sandbox Python shims and ruff

- One lazy `shim_*.clj` per shim, one registered extension, and `builtin-extension-nses` inclusion. Python lives in `resources/vis-shims/<name>.py`, referenced as `:shim/source "vis-shims/<name>.py"` — never embedded as a Clojure string. Verify lazy import behavior and native resource inclusion.
- Python format/lint uses in-process `com.blockether/ruff`, never a subprocess or PyPI install. Honor ruff's nearest-config resolution; missing targets fail, and only syntax plus `E9xx`/`F6xx`/`F7xx`/`F82x` are errors. The shim supports `vis-agent python -m ruff check|format <paths>`; root `ruff.toml` configures this repo. Upgrade ruff in sibling `clj-ruff`, release it, then move the `deps.edn` pin.
- The PIL shim's `ImageFont.truetype` takes a family NAME or a file path, and the requested face must reach both the draw op and the measurement, or text is measured in one font and painted in another. Family comes from the file/name, weight and italic from the name stem (`…-Bold.ttf` → 700), and a variable font honors the `wght` axis — which is why one file can serve two weights.

## TUI rendering

The paint contracts document themselves on the functions that hold them, in `extensions/channels/vis-channel-tui/`: `dialogs/draw-row-surface!` (the ONE geometry every focusable form row shares), `draw-field-row!` vs `draw-toggle-row!` (a pale field surface means "type here", so only typed lines and OTP boxes wear it; options, checkboxes and sliders are toggles on the dialog's own paper), `draw-selectable-row!` and `p/selection-prefix` (the `• ` marker belongs to LIST dialogs and nothing else), `choice-mark` (`●`/`○` exclusive, `[✓]`/`[ ]` inclusive), and `draw-dialog-chrome!`. Read them before changing a paint. Repo-wide:

- Render paint work in the `vis-channel-tui` REPL against Lanterna `DefaultVirtualTerminal` and inspect its back-buffer. Dialog chrome sits on flat `t/terminal-bg` without tint or shadow. A magit-style transient is a band INSIDE its host's frame: it repaints the frame edge on every row it covers and closes with the host's own `├───┤` rule directly above the hint bar, so the bottom chrome is never swallowed.
- **A form is an emphasis, not a list.** Exactly one field is where the keyboard is, and the paint says so: `human_input.clj` stamps that field's rows `:is-active-field` (`:is-focused` keeps its caret/stop meaning, which scrolling and column tests depend on), so its label is bold `t/dialog-fg` while every other label and description recede to `t/dialog-hint`. Never hardcode `t/input-field-bg` for a placeholder or any other field detail — that is what made a resting empty field look focused. In `solarized-dark` all three surfaces coincide by design; the `▎` ring and the bold ink carry focus there. A decoration paints no surface and no ring and is never focusable.
- **Required is a red `*`, in every dialog** — the web's own mark, so the terminal wears it too. `required-marker` is `" *"` (the leading space IS the gap), and `paint-required!` re-inks that one cell in `t/footer-error-fg` on whatever paper the row already wears. The mark is DATA (`:is-required` on the row) and is only inked when it survived the ellipsis; the companion renders the same `*` with an `sr-only` "required". Spelling `REQUIRED` beside every label shouted the same word down the whole form.
- **Look at the pixels, and keep the assertions.** `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/capture.clj` documents the whole screenshot API (`cap/shot!` returns a PNG PATH to attach with `attach`; `cap/shots!`, `cap/frame-text`, `cap/png-rows`, `cap/ink`). There is ONE screenshot API and it hands back paths, never buffers. The PNG is what you eyeball; the lazytest terminal-grid assertions are the regression gate — keep both.

## Shipping verified work

- A change that is lint-clean, formatted and covered by its own passing tests is **ready to push**: commit and push it to `main` in the same session without being asked again. Finished features must never be left sitting in the working tree.
- "Ready" is exactly: the smallest relevant `run_tests` namespaces pass, `lint_code` (clj-kondo + reflection) is clean, `format_code` has run, and the behavior is pinned by a suite test. Anything short of that stays uncommitted and is reported as unfinished.
- No scratch, notes or report files in a commit. Never pass `--no-verify` — the hooks are the gate.
- **Push to `main` is the only automatic remote action.** Tags, releases, store submissions, force pushes and history rewrites still require an explicit request (see **Companion releases**).
