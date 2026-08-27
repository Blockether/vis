---
name: apple-design
description: >
  The binding design contract for every Vis surface — the companion app (web/iOS/Android) and the
  TUI. Read it BEFORE writing or changing a screen, a row, a dialog, a mockup or a screenshot
  review, and again before saying a design is done. Distilled from Apple's Human Interface
  Guidelines and bound to this repo's real tokens: the eight-step type scale, the shared TUI/app
  palette with measured contrast ratios, touch and pointer target sizes, one-edge chrome, required
  states, motion budget, and the anti-slop list. Use it whenever the task mentions design, UI, UX,
  layout, spacing, typography, colour, icons, accessibility, dark mode, "does this look good",
  "modernize", "redesign", or hands over a screenshot of a screen.
---

# Apple design, as this repo builds it

Adapted from Apple's HIG (via `github.com/dickwu/apple-design-skill`) into rules that hold for a
**terminal-native product**: JetBrains Mono, one accent, hairlines. The aesthetic is not up for
review — the *hierarchy, density, contrast and states* are. Nothing here overrides
`AGENTS.md` → **Companion UI** (`ui.tsx` closed vocabulary, Tailwind v4 tokens only, `sm:` for
room / `mouse:` for a cursor) or **TUI paint** (the paint docstrings in
`extensions/channels/vis-channel-tui/`). Those say what you build *with*; this says what is
worth building.

## 0. The one question

**What leads?** Every row, card, dialog and screen has exactly one element that wins the eye, and
everything else is subordinate by size, weight, colour or position — in that order. A layout where
five things share one weight is not "clean", it is undesigned, and it is the single most common
defect in this repo's screens.

## 1. Type — the scale is closed

`src/index.css` clears Tailwind's scale on purpose. The eight steps and their roles:

| step | px/line | what it is allowed to be |
|---|---|---|
| `text-display` | 24/30 | one per screen: the screen's own name |
| `text-head` | 17/24 | screen or sheet title |
| `text-subhead` | 15/22 | section lead, empty-state headline |
| `text-title` | 13/20 | **the thing that leads a row** |
| `text-body` | 12/18 | prose, descriptions, transcript copy |
| `text-ui` | 11/16 | control labels, buttons, fields |
| `text-meta` | 10/16 | metadata beside a title |
| `text-chip` | 8/14 | a chip's tag, never a sentence |

- Never `text-[Npx]`, never a `leading-*` utility (the step owns its line-height, an override is
  how vertical rhythm drifts), never a size not on this table.
- **At most three steps in one row, four in one screen.** More than that is noise wearing a
  hierarchy costume.
- `text-chip` (8px) may never be the ONLY place a fact appears, and may never be the label of a
  control a finger presses. On a touch pointer, anything a user must read to act is `text-meta`
  or larger.
- Sentence case for everything a human reads. ALL CAPS is reserved for chips of ≤ 10 characters
  (`IDLE`, `LIVE`, `PDF`) and for nothing else — a shouted button label is slop, not emphasis.

## 2. Colour — measured, never eyeballed

Contrast minimums (HIG / WCAG AA): **4.5:1 for text under 18.66px or non-bold, 3:1 for larger or
bold text and for any glyph, icon or hairline that CARRIES MEANING**. Decorative separators are
exempt; a status dot is not.

Measured against each theme's own `--bg` (recompute after any palette edit — see the snippet):

| token | `blockether-dark` | `blockether-light` | verdict |
|---|---|---|---|
| `--fg` | 17.2 | 13.8 | body text, both themes |
| `--footer-fg` | 12.8 | 9.6 | secondary text, both |
| `--footer-muted` | 7.4 | 4.9 | metadata, both — the floor for 10px |
| `--dim` | 5.3 | **3.2** | dark only; on light it is a hairline, not ink |
| `--accent` | 11.8 | **1.8** | on light, a FILL only — never text, never a meaningful glyph |
| `--ok` | 10.8 | **3.0** | on light, pair it with a glyph or use `--warning`-grade ink |
| `--err` | 6.8 | **4.4** | on light it misses 4.5 — use `--footer-error` (5.0) for error text |

- **Never encode meaning in colour alone.** A green dot and a grey dot are one dot to a
  colour-blind reader and to an 8-colour terminal: the state also needs a glyph, a word, or a
  position.
- Semantic tokens only. A hex literal or a Tailwind palette colour (`text-green-500`) in a
  component is a defect: the TUI and the app read one generated palette
  (`src/com/blockether/vis/internal/theme.clj` → `themes.generated.css`).
- Both appearances every time. If you only checked dark, you did not check.

```python
def cr(a, b):  # contrast ratio of two #rrggbb strings
    L = lambda h: sum(w * (v / 12.92 if (v := int(h.lstrip("#")[i:i+2], 16) / 255) <= 0.03928
                           else ((v + 0.055) / 1.055) ** 2.4)
                      for w, i in ((0.2126, 0), (0.7152, 2), (0.0722, 4)))
    x, y = sorted((L(a), L(b)))
    return round((y + 0.05) / (x + 0.05), 2)
```

## 3. Targets and density

- **Touch ≥ 44×44 px**, including the invisible box: a 20px glyph needs padding, not a bigger
  glyph. **Pointer (`mouse:`) ≥ 28×28 px**, and only the `mouse:` variant may ever make a control
  smaller — an iPad is a ≥ 640px screen driven by a hand.
- **≥ 8px between two adjacent targets**, and a destructive action is never adjacent to a common
  one. Delete lives behind an overflow (`⋯`) or a swipe, never one pixel from Fork.
- The row IS the control. A row that opens something needs no `Open` button beside it; on touch a
  chevron states where the press goes, on a pointer the hover tint already did.
- Density is a *pointer* question, never a *screen-width* question. `scripts/touch-density.test.mjs`
  is the gate.

## 4. Chrome — one edge

- **One container edge per group.** A bordered section holding bordered cards holding bordered rows
  is three rectangles saying one thing; the container keeps its edge, rows get a hairline
  (`divide-y`) or nothing but hover tint. Box-in-box is the signature of a generated design.
- Group by **proximity and alignment** before you reach for a line: 4px base rhythm, one gap scale,
  and everything in a column shares one left edge.
- The title takes the space (`flex-1` + truncate); metadata clusters at the trailing edge in
  reading order and never sits in fixed columns across a 1440px window.
- **Say a state only when it is worth saying.** A status repeated on 19 of 20 rows is not
  information — show `LIVE`, omit `IDLE`. Same for an empty checkbox, a chevron on a row that is
  not pressable, or an icon that repeats the word next to it.

## 5. Platform conventions

**Mobile (companion on iOS/Android)** — large title that collapses on scroll; search pinned under
it; inset-grouped lists; safe areas honoured (notch, home indicator, keyboard); two-line rows when
one line cannot hold title + metadata at ≥ `text-meta`; swipe actions carry ICONS with an
accessible label, not text; commit-level haptics only. Support system text scaling: nothing may
clip or overlap at 130%.

**Desktop (companion on a fine pointer)** — filter and keyboard-first navigation instead of
pagination; `/` focuses the filter, `Esc` clears, `↑↓` walks, `Enter` opens; a virtualized list
before a pager; hover reveals actions in a slot that already existed, so nothing reflows; every
destructive action confirmable and every dialog dismissible with `Esc`.

**TUI** — the grid is the layout engine: measure in CELLS, budget columns explicitly, and leave
≥ 1 cell of gutter. Box-drawing for the container only. Truncate with `…` at a known column, never
let a long title push a column off screen. Assume 8 colours and no italics somewhere. Verify with
the capture API (`…/test/com/blockether/vis/ext/channel_tui/capture.clj`): the PNG is what you
eyeball, the terminal-grid assertions are the regression gate — keep both.

## 6. States, feedback, motion

- Every asynchronous surface owns four paints: **loading, empty, error, and partial/stale**. An
  empty state names what would be here and offers the verb that creates it.
- Every error says what failed and what to do next; "Something went wrong" is not a design.
- Destructive actions confirm with the OBJECT named ("Delete 3 transcripts from `visgw`?"), and the
  confirming button carries the verb, never "OK".
- Motion: 120–200ms for a state change, 300ms ceiling, ease-out on entry. Nothing animates on list
  mount. Honour `prefers-reduced-motion` — the reduced path is instant, not slower.
- Feedback is immediate: a press changes something within one frame, even if the result is pending.

## 7. The anti-slop list

Reject on sight, in a mockup or in a diff:

1. Everything in a bordered card; nested rounded boxes; `rounded-2xl` on every element.
2. Purple/blue gradients, glass/blur used as decoration, more than one shadow depth.
3. Emoji as an icon system; decorative icons that duplicate the adjacent word.
4. A centred hero on a screen that is a working tool.
5. Repeated identical status text down a list; a chevron on every row including the dead ones.
6. Six font sizes and four greys on one screen; grey text on grey fill under 4.5:1.
7. Fixed metadata columns leaving 600px of nothing between title and value.
8. Pagination where a filter belongs; a hamburger where a sidebar fits.
9. Labels like "Manage", "Settings", "Options" where a verb and an object would say it.
10. Any placeholder, sample or lorem text left in a shipped screen.

## 8. Review protocol

When asked to review, audit, critique or modernize a design — screenshot, mockup, or code:

1. **Context**: which surface (app / TUI), which pointer (touch / fine), which theme(s), what the
   screen is FOR.
2. **Audit in this order**, because that is the order defects cost users: accessibility →
   platform conventions → hierarchy and density → visual system → interaction states →
   content and wording.
3. **Report** as:

```
## Design review: <screen>
### Verdict — Excellent / Good / Needs work / Critical
### Critical    what · why (cite the rule) · fix (in this repo's tokens)
### Improvements
### Keep        what already works — say so, do not invent 20 issues
```

4. **Cite the rule and the number**: "`text-chip` 8px is the only label on a 33px row — §1 and §3:
   ≥ `text-meta` and ≥ 44px on touch" beats "the text is small". A number is falsifiable; taste is
   not.
5. **Improving** (not just reviewing) adds: rank by severity × effort, propose in the app's own
   vocabulary (`ListRow`, `Button variant=…`, a `text-*` step, a palette token — never a raw
   utility), and sequence the work: accessibility, then conventions, then polish.

## 9. Done means

- Contrast recomputed for both themes; no meaning carried by colour alone.
- Targets measured on touch AND pointer; nothing shrank on a screen-width query.
- Type steps and palette tokens only — no literal size, no literal colour.
- Loading, empty, error paints exist; destructive path confirms; `Esc`/back leaves.
- Text scaling to 130% and reduced motion both survive.
- App: `npm test -- <files>`, `npm run lint`, `npm run build` clean, and any new control pinned in
  `ui.test.tsx`. TUI: a capture PNG eyeballed and grid assertions added.
