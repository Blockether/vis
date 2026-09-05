# TUI implementation and review

Paths are relative to `apps/vis-tui/`. Paint contracts live beside their implementation:
`draw-row-surface!`, `draw-field-row!`, `draw-toggle-row!`, `draw-selectable-row!`,
`selection-prefix`, `choice-mark` and `draw-dialog-chrome!`. Use them rather than a parallel renderer.
A reusable state has a deterministic production-component `HtmlTerminalView` fixture.

Use Spel, not MCP or a second browser layer. Read command help when an argument is unknown.
Use one task-specific browser session and close only that session after review. Do not stop a
healthy user-owned application server as incidental cleanup.

## Inspect the shipped render

Serve the exact production component with `HtmlTerminalView.serve(...)`, or launch the full
channel with `vis-agent channels tui-html`. In one Spel session, test keyboard, paste, pointer, wheel,
resize, focus, every changed show/hide transition and any image/video/audio layer. Inspect resolved
cell boxes and computed SGR styles; `Ctrl+Shift+G` exposes the cell grid when geometry is disputed.
CSS Grid is only the projection: the JVM's `GridLayout`/`LinearLayout` and terminal buffer must have
already resolved every integer cell.

## Attach a design review

The default user-facing artifact for a visual proposal or review is a self-contained HTML file
exported from the final production render. It is temporary evidence, never tracked source.

1. The live review is the production `HtmlTerminal` backend: `HtmlTerminalView` for one GUI2
   component, or the complete Vis `TerminalScreen` over `HtmlTerminal`. Never transcribe cells or
   reimplement layout in HTML/CSS.
2. Spel exercises the live loopback URL and verifies interaction, computed styles, geometry, Unicode
   width and media persistence before export.
3. Export the reviewed frame with `writeHtml(Path)` and attach that one self-contained file. It has no
   token, loopback dependency or external asset. Because arbitrary terminal callbacks remain in the
   JVM, call the attachment a portable exact frame, not a still-interactive application.
4. HTML is the primary make/review artifact. After it passes, the real `DefaultVirtualTerminal` PNG
   capture and terminal-grid assertions remain the final parity gate for terminal-specific glyph width
   and back-buffer behaviour; the PNG is private verification evidence, not the review attachment.

## Completion

For UI code changes, run affected TUI tests, format and lint. Work against
Lanterna `DefaultVirtualTerminal` and inspect its back-buffer; the screenshot API is documented in
`test/com/blockether/vis/tui/capture.clj`. Terminal-grid assertions and PNG inspection verify parity
with the HTML review; neither is replaced by a successful build. Report any unverified platform or
interaction explicitly.
