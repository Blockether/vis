# Vis repository guidance

Keep only non-obvious project contracts here; inspect nearby source and tests for detail.

## Clojure tests (Lazytest)

- Tests use Lazytest. Prefer the `run_tests` language tool with the smallest relevant test namespace; `only` entries are fully qualified top-level test vars (usually `defdescribe` vars).
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

## Feature toggles

IDs are snake_case strings. Hydrate from merged config so `/reload` applies project overrides; test registry, config coercion, and wire round-trips.

## Sandbox Python shims

One lazy shim per `shim_*.clj`, one registered extension, and inclusion in `builtin-extension-nses`. Verify imports are absent at context creation and present after import.

## TUI rendering

Render paint code in the `vis-channel-tui` REPL with Lanterna `DefaultVirtualTerminal`; inspect the back-buffer. Dialogs use `dialogs/draw-dialog-chrome!` on flat `t/terminal-bg`, without panel tint or shadow.
