# Vis Answer-IR Tag Reference

Single source of truth for every tag the channel pipeline can paint, the
renderer can serialize (HTML / Markdown / plain text), and the TUI walker
can lift into per-row paint entries.

Sources: `src/com/blockether/vis/internal/render.clj` (canonical IR),
`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render_ir.clj`
(TUI block / inline walker).

> **Triage workflow**: every row below carries a `Status` column the user fills in
> while reviewing. `OK` = keep, `Bad` = redesign needed, `?` = revisit. The
> `Notes` column captures the concrete concern.

## Canonical shape

```
[:ir {} <block> <block> ...]
```

Every vector node has its attrs map at index 1 (`{}` when absent). Children
of `:ir` are exclusively blocks. Text only lives in `:span` bodies and in
the raw bodies of `:code`, `:c`, `:kbd`.

---

## Root

| Tag  | Attrs | Children   | Renders as                                  | Status | Notes |
| ---- | ----- | ---------- | ------------------------------------------- | ------ | ----- |
| `:ir`| `{}`  | blocks only| Top-level container. No own chrome.         |        |       |

## Blocks (11)

| Tag       | Attrs                                           | Children                       | Renders as                                                                             | Status | Notes |
| --------- | ----------------------------------------------- | ------------------------------ | -------------------------------------------------------------------------------------- | ------ | ----- |
| `:p`      | `{}`                                            | inlines                        | Paragraph row. Wraps to bubble width.                                                  |        |       |
| `:h`      | `{:level 1..6}`                                 | inlines                        | Heading. Level clamped to 1-6. Markdown writer emits `#`...`######`.                   |        |       |
| `:code`   | `{:lang string?}`                               | single raw string body         | Fenced code block. `:lang text/plain/output/log` → soft-wrap; other langs verbatim.    |        |       |
| `:ul`     | `{}`                                            | `:li` only                     | Bullet list. TUI bullet marker.                                                        |        |       |
| `:ol`     | `{:start integer?}`                             | `:li` only                     | Ordered list. `:start` defaults to 1.                                                  |        |       |
| `:li`     | `{}`                                            | all blocks OR single `:p` inline-wrapped | List item. Coerces loose inlines into one `:p`.                              |        |       |
| `:quote`  | `{}`                                            | blocks                         | Blockquote. TUI paints italic + bar gutter.                                            |        |       |
| `:table`  | `{}`                                            | `:tr` only                     | Table. First row treated as header.                                                    |        |       |
| `:tr`     | `{}`                                            | `:th` / `:td` only             | Table row.                                                                             |        |       |
| `:th`     | `{}`                                            | inlines                        | Header cell. Bold + separator below.                                                   |        |       |
| `:td`     | `{}`                                            | inlines                        | Data cell.                                                                             |        |       |

## Inlines (11)

| Tag        | Attrs                                          | Children                       | Renders as                                                                              | Status | Notes |
| ---------- | ---------------------------------------------- | ------------------------------ | --------------------------------------------------------------------------------------- | ------ | ----- |
| `:span`    | `{:preserve-ws? bool? :nowrap? bool?}`         | single text string             | Plain text run. `:preserve-ws?` inherits inside `:code`/`:c`/`:kbd`.                    |        |       |
| `:br`      | `{}`                                           | void                           | Hard line break.                                                                        |        |       |
| `:strong`  | `{}`                                           | inlines                        | Bold. TUI paints SGR bold; markdown emits `**...**`; HTML `<b>`.                        |        |       |
| `:em`      | `{}`                                           | inlines                        | Italic. TUI SGR italic; markdown `*...*`; HTML `<i>`.                                   |        |       |
| `:c`       | `{}`                                           | single raw string body         | Inline code (monospace). TUI code-bg span; markdown `` `...` ``; HTML `<code>`.         |        |       |
| `:a`       | `{:href string}`                               | inlines                        | Link. TUI underline + chrome row; markdown `[text](url)`; HTML `<a href>`.              |        |       |
| `:img`     | `{:src string :alt string}`                    | void                           | Image. TUI paints `🖼 alt` placeholder; markdown `![alt](src)`; HTML `<i>🖼 alt</i>`.    |        |       |
| `:kbd`     | `{}`                                           | single raw string body         | Keyboard key. TUI code-style; markdown `<kbd>...</kbd>`; HTML `<kbd>`.                  |        |       |
| `:mark`    | `{}`                                           | inlines                        | Highlight. TUI bold (proxy for accent); markdown `==...==`; HTML `<mark>`.              |        |       |
| `:sup`     | `{}`                                           | inlines                        | Superscript. TUI passthrough (no superscript glyphs); markdown `<sup>...</sup>`.        |        |       |
| `:sub`     | `{}`                                           | inlines                        | Subscript. TUI passthrough; markdown `<sub>...</sub>`.                                  |        |       |

## Disallowed shapes

These are NOT supported. The canonicalizer collapses or rejects them:

| Anti-pattern                       | What the canonicalizer does                                                  |
| ---------------------------------- | ---------------------------------------------------------------------------- |
| Bare strings inside `:ir`          | Wrapped in `[:p [:span text]]`.                                              |
| Mixed block + inline siblings      | Loose inlines buffered into a single `:p` per run.                           |
| `:details` / `:summary`            | INTENTIONALLY UNSUPPORTED. No disclosure widgets in answer IR.               |
| `<details>` HTML                   | Same. Use sectioning headers + body blocks.                                  |
| Multiple text segments in `:code`  | Joined into one raw string.                                                  |

## TUI-only sentinel inline pairs (paint hint, NOT IR)

These live in `extensions/channels/vis-channel-tui/src/.../primitives.clj`
as PUA codepoints and ride embedded in already-rendered text rows. They
are NEVER part of the answer IR; the inline span walker emits them.

| Marker pair                                       | Effect                          |
| ------------------------------------------------- | ------------------------------- |
| `INLINE_BOLD_ON` / `INLINE_BOLD_OFF`              | Bold span. Stacks on italic.    |
| `INLINE_ITALIC_ON` / `INLINE_ITALIC_OFF`          | Italic span.                    |
| `INLINE_STRIKE_ON` / `INLINE_STRIKE_OFF`          | Strikethrough.                  |
| `INLINE_CODE_ON` / `INLINE_CODE_OFF`              | Code colors (code-fg / code-bg).|
| `INLINE_LINK_ON` / `INLINE_LINK_OFF`              | Underlined link span.           |

## Row markers (TUI only, NOT IR)

Per-line PUA glyphs the bubble painter dispatches on. Used by recap rows,
code rows, answer rows, etc. Document only the IR-visible ones the
extensions emit; the rest is render-layer chrome.

| Marker name              | Purpose                                                |
| ------------------------ | ------------------------------------------------------ |
| `MARKER_RECAP`           | Bold + italic recap row on terminal-bg.                |
| `MARKER_THINKING`        | Italic thinking row on dim-bg.                         |
| `MARKER_CODE` (+ `_OK` / `_ERR`) | Code row with status bg.                       |
| `MARKER_ANSWER_TXT`      | Answer-zone row with answer-bg.                        |
| `MARKER_MD_*`            | Markdown chrome inside answer (headers, bullets, ...). |

---

## How to grow this list

1. Edit `internal/render.clj`'s tag taxonomy (`block-tags` / `inline-tags`).
2. Update the canonicalizer for the new shape.
3. Add HTML / markdown / plain writers.
4. Wire the TUI walker (`render_ir.clj`) if it needs special paint.
5. Append the row here with `Status: ?` so it lands in triage.

A tag without `Status: OK` is not stable. Channels SHOULD NOT depend on
unstable tags.
