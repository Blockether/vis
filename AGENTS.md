# Vis repository guidance

Keep this file small and durable. Inspect source and nearby guidance for details instead of expanding this file with procedures.

## Vis-native execution

- Use Vis native tools: `grep` first when location is unknown, then `struct_index`/`cat`; edit supported code with `struct_patch`; use `lint_code`, `format_code`, and `run_tests` rather than shell equivalents.
- A managed Clojure REPL is guaranteed for this project. Read its live state from the session, reuse/start it with `repl`, reproduce behavior with `repl_eval`, reload edited namespaces, and execute the changed path. Use the smallest relevant test only when REPL proof is insufficient.
- Runtime beats source; source beats docs. Reproduce bugs before editing when feasible.
- Preserve unrelated work. Stop only resources you started.

## Companion UI (`apps/vis-companion`)

- One universal web/iOS/Android product. Every UI change must work at narrow phone and desktop widths, with touch, overflow, safe-area, virtual-keyboard, light-theme, and dark-theme behavior considered.
- Use Tailwind CSS v4 utilities only. No component CSS, CSS modules, CSS-in-JS, or inline style objects. Keep `src/index.css` to Tailwind imports, shared theme/font tokens, and unavoidable document base rules.
- Use only the canonical type steps: `text-chip`, `text-meta`, `text-ui`, `text-body`, `text-title`, `text-subhead`, `text-head`, `text-display`. Never use ad-hoc/default font sizes or `leading-*`; each step owns its line height.
- Preserve TUI hierarchy, role colors, compactness, and transcript semantics without sacrificing mobile usability.
- There is no ESLint. Verify frontend changes in `apps/vis-companion` with `npm run lint` and `npm run build`; inspect phone and desktop viewports when browser tooling is available.

## Gateway wire contract

- `gateway/wire.clj` is the single deterministic boundary. Wire keys are snake_case strings; engine keys are their mechanical kebab-case keyword mirrors.
- Boolean wire flags use `is_<foo>` and engine `:is-<foo>`. Do not introduce `:foo?` aliases or endpoint-specific field restoration.
- Use `wire/->wire` and `wire/json-str`; never hand-encode keyword keys.

## Feature toggles

- IDs are plain snake_case strings.
- Register with a clear description, retain the validated top-level `toggles:` config block, and hydrate from merged config so `/reload` applies project overrides.
- Test registry/spec, config hydration/coercion, and settings wire round-trips.

## Sandbox Python shims

- One shim per `shim_*.clj`, one registered extension, and inclusion in `builtin-extension-nses` in `extension.clj`.
- Shims must be lazy. Their preamble must expose import triggers through `sys.modules`/`builtins`; verify absence at context init and presence after import.
- Document supported imports and explicit limitations. Add a real compatibility test using `ep/create-python-context` and observable behavior.

## Runtime diagnostics

- Default memory telemetry: `~/.vis/vis-pyblock.log` and env-reaper lines in `~/.vis/logs/vis.log`; disable both with a falsey `VIS_MEM_LOG`.
- `VIS_PY_BLOCK_LOG_EVERY` controls Python-block sampling. Use `--jfr` only for method/allocation-level profiling; dumps land under `~/.vis/logs/`.

## TUI rendering

- Do not infer dialog geometry visually. Render actual paint code in the `vis-channel-tui` REPL with Lanterna `DefaultVirtualTerminal`, then inspect the back-buffer.
- Dialogs use `dialogs/draw-dialog-chrome!` on the flat `t/terminal-bg`; no panel tint or shadow. Full-frame checks include header, dialog, composer, and footer.
