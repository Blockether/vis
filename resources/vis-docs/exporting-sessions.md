# Exporting sessions

Every conversation Vis runs is stored in its session database, and any stored
session can be exported for sharing, archiving, or showing off. One command
covers all three formats:

```bash
vis sessions export <SESSION-ID> [--md | --html PATH | --mp4 PATH]
```

`<SESSION-ID>` is the full UUID or any unambiguous prefix (the short 8-char id
Vis prints in `vis sessions list` works). Pick **exactly one** format — passing
more than one is an error.

## Formats

| Flag | Output | Where it goes |
|---|---|---|
| `--md` (default) | Markdown transcript | stdout |
| `--html PATH` | Styled, self-contained HTML | file at `PATH` |
| `--mp4 PATH` | H.264 screencast of the TUI transcript | file at `PATH` |

### Markdown — `--md`

The default. Prints the whole transcript — user turns, thinking, answers,
tool calls — as Markdown to stdout, so you can pipe or redirect it:

```bash
vis sessions export 3a7b2c1d > session.md
vis sessions export 3a7b2c1d --md | pbcopy
```

### HTML — `--html`

Writes a styled, standalone HTML rendering of the session to the given path.
Requires the web extension on the classpath (present in the standard build).

```bash
vis sessions export 3a7b2c1d --html report.html
```

### MP4 screencast — `--mp4`

Renders a **headless, humanized replay** of the session as a pure-JVM H.264
`.mp4` — no browser, no screen recorder, no ffmpeg. It re-enacts the
conversation as if it were happening live rather than dumping the transcript and
scrolling through it:

- **User turns are typed in**, character by character, at a brisk
  human-keyboard pace.
- **Before each answer**, the real TUI progress bubble holds in place — spinner
  animating, elapsed clock ticking (`⠿ Vis is calling the provider… 1.2s`) — so
  it reads as real work being done.
- **The finished answer jumps in** near the bottom of the viewport, holds a
  beat, then **scrolls slowly** into view.
- Disclosures render **collapsed**, in their natural resting view, instead of a
  fully-expanded dump.

```bash
vis sessions export 3a7b2c1d --mp4 session.mp4
```

The MP4 path needs the `channel-tui` extension (present in the standard build).
On completion Vis prints the frame count and approximate duration, e.g.
`Exported MP4: ~/session.mp4  (1303 frames, ~108s)`.

## Nice-to-knows

- **The extension is auto-appended.** If you forget it, Vis adds the right one
  for you — `--mp4 siema` writes `siema.mp4`, `--html report` writes
  `report.html` (case-insensitive, so `siema.MP4` is left alone).
- **Output paths are home-abbreviated.** The "Exported …" line shows `~/…`
  instead of your full `/Users/you/…` home path.
- **Parent directories are created** as needed for `--html` and `--mp4`.

## Examples

```bash
# Markdown to stdout (default)
vis sessions export 3a7b2c1d --md

# Styled HTML report
vis sessions export 3a7b2c1d --html out.html

# Humanized MP4 screencast (extension auto-added)
vis sessions export 3a7b2c1d --mp4 demo
```
