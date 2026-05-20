# TUI trace toggle screen jump

## Problem

When the Settings/Menu toggle for showing the full execution trace is clicked (`trace` / `untrace`, internally `:show-iterations`), the TUI screen can visibly jump. The click itself is probably only the trigger; the actual issue is that the toggle changes the rendered height of the transcript and the redraw path does not preserve a scroll anchor.

## Observed source path

### Settings toggle

In `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/dialogs.clj`, the Settings option for the execution trace is backed by `:show-iterations` and labeled `Show full execution trace`.

The toggle path updates the settings value by applying `not` to the current value. So the trace/untrace action is effectively a layout-affecting change of `:show-iterations`.

### Settings change redraw

The settings change notification calls both callbacks:

- `:on-change` — dispatches the updated settings into app state.
- `:redraw-ui` — immediately redraws the main UI while the dialog is still open.

In `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/screen.clj`, the Settings dialog callback path wires this to `render-frame!` and then stores the new layout via `[:set-layout layout]`.

### Trace rendering changes transcript height

In `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`, trace rendering depends on:

- `:show-iterations`
- `:show-silent`
- the available iteration/form/result data

When `:show-iterations` is enabled, trace entries are inserted into assistant bubbles. When disabled, those entries disappear. This can change the transcript height by many lines, not just one or two.

### Scroll/layout coupling

`render-frame!` reads `messages-scroll` from app-db and passes it into the virtual message layout calculation. The layout then computes effective scroll/total height and the result is stored back for scroll handlers.

That means after the trace toggle, the same stored `messages-scroll` is interpreted against a document whose total rendered height may have changed drastically.

## Likely mechanism

1. User opens Settings/Menu.
2. User toggles `Show full execution trace`.
3. Settings update flips `:show-iterations`.
4. Dialog callback immediately redraws the main UI underneath.
5. Rendering recomputes message layout with a different transcript height.
6. Existing `messages-scroll` is reused/clamped against the new layout.
7. The viewport no longer points at the same visual content.
8. The user sees this as a screen jump.

Short diagnosis:

> The screen jumps because `Show full execution trace` changes the number of rendered transcript lines, and the redraw triggered from Settings recomputes layout without preserving a scroll anchor.

## Why this is especially visible for trace/untrace

Other settings usually have a small visual impact:

- theme changes mostly affect colors,
- timestamps affect labels/inline text,
- footer/header contributions affect a small amount of chrome,
- silent calls may add/remove some lines,
- full execution trace can add/remove large chunks of transcript.

So weak or missing scroll anchoring becomes obvious when toggling the execution trace.

## Suspect area

The suspicious boundary is not the toggle itself, but the layout/redraw path:

`screen.clj` Settings callback → `render-frame!` → virtual message layout → `messages-scroll` / effective scroll / total height → `[:set-layout layout]`

The observed callback path redraws with the updated settings, but there is no obvious anchor-preservation step such as:

- remember whether the user was at bottom,
- remember top visible message/line,
- compute old/new rendered height around the viewport,
- compensate `messages-scroll`,
- restore bottom position when the user was already at bottom.

## Fix direction

Do not special-case the mouse click. Treat `:show-iterations` and similar settings as layout-affecting settings and preserve scroll position across the settings update.

Recommended policy:

### If the user is at bottom

Before applying the setting, detect that the viewport is at the bottom. After relayout, set `messages-scroll` to the new bottom. This preserves the expected chat behavior.

### If the user is not at bottom

Preserve a concrete scroll anchor, preferably:

- message id/index,
- row inside that rendered message/bubble,
- current screen row offset.

After relayout, find that same anchor and set `messages-scroll` so it appears in the same screen position.

### Minimal fallback

If message-level anchoring is too large for the first patch:

- capture old `total-h`, viewport height, effective scroll, and bottom state,
- apply settings,
- compute new layout,
- if previously at bottom, scroll to new bottom,
- otherwise clamp the old scroll carefully.

This is less precise than message anchoring, especially when the height change happens above the current viewport, but it should reduce the most visible jump.

## Suggested next implementation step

Inspect and modify the reducer/render path around `[:update-settings ...]`, `render-frame!`, virtual layout output, and `[:set-layout layout]` so layout-affecting settings can run through a `preserve-scroll-anchor` step.
