---
name: tui-rendering
description: >
  How the Vis terminal UI paints: where each paint contract's docstring lives,
  the virtual-terminal REPL workflow, active-field emphasis, the required
  marker, and the screenshot plus terminal-grid assertion gate. Read before
  changing any TUI paint.
---

# TUI rendering

The paint contracts document themselves on the functions that hold them, in `extensions/channels/vis-channel-tui/`: `dialogs/draw-row-surface!` (the ONE geometry every focusable form row shares), `draw-field-row!` vs `draw-toggle-row!` (a pale field surface means "type here", so only typed lines and OTP boxes wear it; options, checkboxes and sliders are toggles on the dialog's own paper), `draw-selectable-row!` and `p/selection-prefix` (the `• ` marker belongs to LIST dialogs and nothing else), `choice-mark` (`●`/`○` exclusive, `[✓]`/`[ ]` inclusive), and `draw-dialog-chrome!`. Read them before changing a paint. Repo-wide:

- Render paint work in the `vis-channel-tui` REPL against Lanterna `DefaultVirtualTerminal` and inspect its back-buffer. Dialog chrome sits on flat `t/terminal-bg` without tint or shadow. A magit-style transient is a band INSIDE its host's frame: it repaints the frame edge on every row it covers and closes with the host's own `├───┤` rule directly above the hint bar, so the bottom chrome is never swallowed.
- **A form is an emphasis, not a list.** Exactly one field is where the keyboard is, and the paint says so: `human_input.clj` stamps that field's rows `:is-active-field` (`:is-focused` keeps its caret/stop meaning, which scrolling and column tests depend on), so its label is bold `t/dialog-fg` while every other label and description recede to `t/dialog-hint`. Never hardcode `t/input-field-bg` for a placeholder or any other field detail — that is what made a resting empty field look focused. In `solarized-dark` all three surfaces coincide by design; the `▎` ring and the bold ink carry focus there. A decoration paints no surface and no ring and is never focusable.
- **Required is a red `*`, in every dialog** — the web's own mark, so the terminal wears it too. `required-marker` is `" *"` (the leading space IS the gap), and `paint-required!` re-inks that one cell in `t/footer-error-fg` on whatever paper the row already wears. The mark is DATA (`:is-required` on the row) and is only inked when it survived the ellipsis; the companion renders the same `*` with an `sr-only` "required". Spelling `REQUIRED` beside every label shouted the same word down the whole form.
- **Look at the pixels, and keep the assertions.** `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/capture.clj` documents the whole screenshot API (`cap/shot!` returns a PNG PATH to attach with `attach`; `cap/shots!`, `cap/frame-text`, `cap/png-rows`, `cap/ink`). There is ONE screenshot API and it hands back paths, never buffers. The PNG is what you eyeball; the lazytest terminal-grid assertions are the regression gate — keep both.
