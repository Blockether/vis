# TUI implementation and review

Paths are relative to `apps/vis-tui/`. Paint contracts live beside their implementation:
`draw-row-surface!`, `draw-field-row!`, `draw-toggle-row!`, `draw-selectable-row!`,
`selection-prefix`, `choice-mark` and `draw-dialog-chrome!`. Use them rather than a parallel renderer.
A reusable state has a deterministic production-component `HtmlTerminalView` fixture.

Use Spel, not MCP or a second browser layer. Read command help when an argument is unknown.
Use one task-specific browser session and close only that session after review. Do not stop a
healthy user-owned application server as incidental cleanup.

## Inspect the shipped render

For local HTML-backend development, build the sibling Lanterna checkout (`mvn test` in
`~/lanterna`), then run from `apps/vis-tui`:

```bash
clojure -M:html-review /absolute/path/activity-tui.html
```

This starts the production Activity fixture at 40 columns, prints its loopback URL and updates
that HTML file after each repaint. The `:html-review` alias explicitly selects the sibling's
compiled classes; the normal application dependency stays pinned to the published library.
Stop the process after review. From a REPL with that alias, `vis-tui.review/start!` returns a
Closeable handle with `:url`.

For another GUI2 component, use `HtmlTerminalView.serve(component, configuredTerminal)`;
for an existing application loop, use `HtmlTerminalPreview.start(terminal)`. Close the returned
handle; do not handwrite another HTTP/SSE relay. Production gateway hosting continues to use
its authenticated, framework-neutral endpoint instead of this local helper.

In one Spel session, test the component's supported keyboard, paste, pointer, wheel, resize,
focus, visibility and media behavior. The Activity fixture supplies pointer disclosures and
resize, not the full channel keyboard map. Inspect cell positions and SGR styles;
`Ctrl+Shift+G` displays the cell grid. CSS Grid displays positions calculated by the JVM.

## Attach a design review

The default user-facing artifact for a visual proposal or review is a self-contained HTML file
exported from the final production render. It is temporary evidence, never tracked source.

1. The live review is the production `HtmlTerminal` backend: `HtmlTerminalView` for one GUI2
   component, or the complete Vis `TerminalScreen` over `HtmlTerminal`. Never transcribe cells or
   reimplement layout in HTML/CSS.
2. Spel exercises the live loopback URL and verifies interaction, computed styles, geometry, Unicode
   width and media persistence before export.
3. Configure the terminal's default foreground/background to match the application theme. Export
   the intended content height with `writeHtml(Path, visibleRows)`. Do not export unused terminal
   rows or assume a desktop frame will reflow on a phone. Render phone-sized columns for phone
   review; Fit width is an overview, Actual size provides zoom and pan. Open the **exported file**
   at phone, tablet and desktop sizes and inside Companion's unchanged sandboxed `DocFrame`.
   Check the right edge, background, Fit width/Actual size and absence of network dependencies.
   Attach that one self-contained file. JVM callbacks are absent: label it a static frame.
4. HTML is the primary make/review artifact. After it passes, the real `DefaultVirtualTerminal` PNG
   capture and terminal-grid assertions remain the final parity gate for terminal-specific glyph width
   and back-buffer behaviour; the PNG is private verification evidence, not the review attachment.

## Completion

For UI code changes, run affected TUI tests, format and lint. Work against
Lanterna `DefaultVirtualTerminal` and inspect its back-buffer; the screenshot API is documented in
`test/com/blockether/vis/tui/capture.clj`. Terminal-grid assertions and PNG inspection verify parity
with the HTML review; neither is replaced by a successful build. Report any unverified platform or
interaction explicitly.
