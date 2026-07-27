# Vis repository guidance

Keep only non-obvious project contracts here; inspect nearby source and tests for detail.

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
