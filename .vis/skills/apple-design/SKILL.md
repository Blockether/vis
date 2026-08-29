---
name: apple-design
description: >
  The binding design contract for every Vis surface — the companion app (web/iOS/Android) and the
  TUI. Read it BEFORE writing or changing a screen, a row, a dialog, a mockup or a screenshot
  review, and again before saying a design is done. Distilled from Apple's Human Interface
  Guidelines and bound to this repo's real tokens and real components: the eight-step type scale,
  the two shipped font families, the shared TUI/app palette with measured contrast ratios, the
  painted face vs the finger's target, one-edge chrome, the control contracts already written in
  `ui.tsx`, required states, motion budget, and the anti-slop list. Use it whenever the task
  mentions design, UI, UX, layout, spacing, typography, colour, icons, accessibility, dark mode,
  "does this look good", "modernize", "redesign", or hands over a screenshot of a screen.
---

# Apple design, as this repo builds it

Adapted from Apple's HIG (via `github.com/dickwu/apple-design-skill`) into rules that hold for a
**terminal-native product**: JetBrains Mono, one accent, hairlines. The aesthetic is not up for
review — the *hierarchy, density, contrast, states and honesty* are. Nothing here overrides
`AGENTS.md` → **Companion UI** (`ui.tsx` closed vocabulary, Tailwind v4 tokens only, `sm:` for
room / `mouse:` for a cursor) or **TUI paint** (the paint docstrings in
`extensions/channels/vis-channel-tui/`). Those say what you build *with*; this says what is
worth building.

Beautiful here is not "less". Apple's screens are dense — a Mail row carries five facts. They read
calm because ONE thing leads, the paint is tight, the ink is measured and every control is exactly
where the platform put it. Deleting facts to make a screen quiet is the cheap move, and it is the
one that reads as generated.

## 0. The one question

**What leads?** Every row, card, dialog and screen has exactly one element that wins the eye, and
everything else is subordinate by size, weight, colour or position — in that order. A layout where
five things share one weight is not "clean", it is undesigned, and it is the single most common
defect in this repo's screens.

## 1. Type — the scale is closed

**Families.** Two in the app, declared once, never chosen per screen:

| where | family | declared in |
|---|---|---|
| app UI and prose — the `<body>` default (`font-sans`) | **Inter Variable** | `apps/vis-companion/src/index.css` → `--font-sans` |
| everything the engine printed — ids, paths, counts, durations, model names, code, tables, live view, chips, and every `ui.tsx` control label (`font-mono`) | **JetBrains Mono Variable** | same file → `--font-mono` |
| docs site (`resources/vis-docs`) | **Hanken Grotesk** (`--sans`/`--display`) + **JetBrains Mono** (`--mono`) | `src/com/blockether/vis/internal/docs.clj` |
| TUI | the terminal's own font — the design IS the character grid; capture PNGs paint that grid at `:font-size 18` | `…/channel_tui/cinema.clj` |

The split is not decoration: **mono means "this is a fact the machine produced and you may compare
it character by character"** — `fd3c03f9`, `~/vis`, `14 turns`, `$3.18`, `12:04`. Inter carries
what a person wrote or reads as prose: session titles, transcript body, empty states, settings
labels. A session title is prose (Inter), its id is a fact (mono), and putting either in the
other's family is the fastest way to make our app look like a different app.

- Both app families are imported upright AND italic, and `font-synthesis: none` forbids a
  synthesized face: a weight or slant whose file was never imported paints as plain upright
  regular. Feature settings (`cv02 cv03 cv04 cv11`) are set once on `body`, never per component.
- The files are in the tree — `@fontsource-variable/{inter,jetbrains-mono}/files/*-latin-wght-normal.woff2`
  (48 KB and 40 KB; the italic is a third file) and the tracked
  `resources/vis-docs/assets/fonts/{hanken-grotesk,jetbrains-mono}.woff2`. Anything that renders a
  screen loads THOSE, by `@font-face` with the bytes inlined as a `data:font/woff2;base64` URI —
  **naming the family in `font-family` loads nothing.** Neither Inter nor JetBrains Mono is
  installed on a Mac, so `font-family:'Inter',-apple-system,…` silently renders in SF Pro and
  reviews someone else's typeface. Verify, do not assume: §12.

`src/index.css` clears Tailwind's scale on purpose. The eight steps and their roles:

| step | px/line | what it is allowed to be |
|---|---|---|
| `text-display` | 24/30 | one per screen: the screen's own name |
| `text-head` | 17/24 | screen or sheet title |
| `text-subhead` | 15/22 | section lead, empty-state headline, **a two-line row's title on touch** |
| `text-title` | 13/20 | the thing that leads a row on a pointer |
| `text-body` | 12/18 | prose, descriptions, transcript copy |
| `text-ui` | 11/16 | control labels, buttons, fields — **the metadata floor on touch** |
| `text-meta` | 10/16 | metadata beside a title — **pointer only** |
| `text-chip` | 8/14 | a chip's tag, never a sentence |

- Never `text-[Npx]`, never a `leading-*` utility (the step owns its line-height, an override is
  how vertical rhythm drifts), never a size not on this table.
- **At most three steps in one row, four in one screen.** More than that is noise wearing a
  hierarchy costume.
- **Hierarchy is weight and ink before it is size.** Apple separates a title from its subtitle by
  one step and a grey, not by 15px against 10px. Two adjacent steps plus `--fg` against
  `--footer-muted` outranks a three-step jump, and it is what keeps a dense row calm.
- iOS sets 11pt as the floor for anything a finger acts on; macOS allows 10pt. So `text-meta` is a
  **pointer** step: on touch, metadata is `text-ui` 11/16. `text-chip` (8px) may never be the ONLY
  place a fact appears and may never label a control.
- Sentence case for everything a human reads. ALL CAPS is reserved for chips of ≤ 10 characters
  (`LIVE`, `PDF`, `DRAFT`) and for nothing else — a shouted button label is slop, not emphasis.

**Marks — one library, and it is Lucide, wrapped in one file.** `lucide-react` (ISC: no attribution in the UI,
no NOTICE file, ~1 700 marks, one ES module per icon) DRAWS; `apps/vis-companion/src/components/icons.tsx` owns
the VOCABULARY — the app's name for each mark (`ClipIcon`, not `Paperclip`), the size floor, and the states a
mark has (a chevron that turns, a star that fills, a sort that points). A screen names the job, the library
draws it. **Nothing else in `src/**` imports `lucide-react`, and nothing else draws an `<svg>`;
`icons.test.tsx` reads the shipped source and fails.**

- **Not SF Symbols.** They are the native marks on Apple platforms and licensed for Apple platforms alone, so
  the Android and web builds could never draw them — one set that renders identically on all three beats a set
  that is legal on one. Never a second family either: a missing mark is ADDED to `icons.tsx` under an app name,
  never imported at a call site.
- **A glyph is TYPE, not an icon.** `✕ ▾ ▸ ▶ ↓ ↗ ✓ ● ◇` wear the font's weight instead of the control's, do not
  align to the label beside them, cannot take a stroke, and on a phone render in whatever face the OS
  substituted. The TUI is the one surface where a glyph IS the mark (a cell holds a character,
  `HUMAN_INPUT_CHOICE_MARKS`) — which is why the app never borrows from it.
  - The composer's send button proves the rule: its mark is
    `SendIcon` — Lucide's `ArrowUp` under the name of the ACT — and `icons.test.tsx` refuses `↑` in shipped
    source exactly as it refuses `✕` and `▾`.
- **A mark with no call site is deleted, not kept.** `icons.test.tsx` fails on an export nothing draws, so the
  vocabulary can never grow into a library of its own; and the library is a dependency of that ONE module —
  `import { Check } from "lucide-react"` in a screen is the vocabulary going away one call site at a time.
- **The box comes from the type step beside it, and Lucide's stroke `2` is never touched.** Inter and JetBrains
  Mono both measure cap 0.719em, stem 0.0875em, so ONE table serves both families:

  | type step | icon box | stroke on screen | stem of the type beside it |
  |---|---|---|---|
  | `text-head` 17/24 | 18px | 1.50px | 1.49px |
  | `text-title` 13/20 | 14px (`size-3.5`) | 1.17px | 1.14px |
  | `text-ui` 11/16, `text-chip` | 12px (`size-3`) | 1.00px | 0.96px |
  | `text-meta` 10/16 | none — the row's mark sits one step up, in the gutter | — | — |

  Scale the BOX, never the path: a re-drawn `d` to "fix" a weight is a second family with one member. The box is
  a `size-*` utility (rem), so at 130% system text scaling the mark grows with its row — and `size-3.5` is a
  FLOOR applied unless the caller's own classes name a size, never a default parameter, which a caller silently
  replaces (that is how a paste disclosure once shipped an `<svg>` with no width at all).
- **One column, one shape.** Where marks stack — a status column — they are one 20-unit ring with a different
  interior (`circle-check`, `circle-x`, `circle-dot`, `circle-dashed`, `circle-slash`, `circle-alert`). Ink is
  what the eye measures, not the box: `check` is 16 × 11 of it beside `circle-dashed`'s 20 × 20.
- **A state is a SHAPE; the colour only repeats it.** Every status column in the app draws the ring set above —
  settings rows, provider rows, machine health, the live view's node list — so `text-ok` green and `text-err` red are
  the second telling, never the only one; a viewer with deuteranopia, a greyscale screenshot and a printed review all
  still read the interior. Two states that differ ONLY in colour are one state drawn twice, which is what
  `providerStatusDot` was before it became `providerStatusMark`.
- **A mark leading a two-line row needs `MARK_NUDGE`, and nothing else does.** `icons.tsx` exports
  `translate-y-[2px]`: a mark is centred on its own box, but the eye centres it on the CAP of the title beside it, and
  in a title-over-subtitle row the flex box centres on the pair, so the mark floats. Two pixels is the measured
  correction for `text-body` 12 through `text-title` 13 — a one-line row (a machine chip) centres correctly on its
  own and must NOT carry it.
## 2. Colour — measured, never eyeballed

Contrast minimums (HIG / WCAG AA): **4.5:1 for text under 18.66px or non-bold, 3:1 for larger or
bold text and for any glyph, icon or hairline that CARRIES MEANING**. Decorative separators are
exempt; a status dot is not.

Measure a mark against the surface it SITS on, not the page `--bg`: on `blockether-light` `--ok` is
3.0 against the paper but **2.86 on a panel**, under the floor for a mark that carries meaning — so a
passing green mark on light is `#15803d` (4.35 on that panel), or the state is also said in a word.

Measured against each theme's own `--bg` (recompute after any palette edit — see the snippet):

| token | `blockether-dark` | `blockether-light` | verdict |
|---|---|---|---|
| `--fg` | 17.2 | 13.8 | body text, both themes |
| `--footer-fg` | 12.8 | 9.6 | secondary text, both |
| `--footer-muted` | 7.4 | 4.9 | metadata, both — the floor for small ink |
| `--dim` | 5.3 | **3.2** | dark only; on light it is a hairline, not ink |
| `--accent` | 11.8 | **1.8** | on light, a FILL only — never text, never a meaningful glyph |
| `--warning` | 8.6 | 6.8 | the readable amber ink on light paper |
| `--ok` | 10.8 | **3.0** | on light, pair it with a glyph or a word |
| `--err` | 6.8 | **4.4** | on light it misses 4.5 — use `--footer-error` (5.0) for error text |
| `--primary` (fill) | — | 1.45 | a FILL only, and its ink is `--primary-fg` (10.4 on that fill) |

**The paint of a control is already decided — look it up before you invent one.** These are the
faces `ui.tsx` ships; a mockup that repaints them is proposing a change and has to say so:

| control | paper | ink |
|---|---|---|
| composer send | `--dialog-title` (dark plate), hover `--accent-2` | `--dialog-title-fg`, bold |
| composer stop | `--cancelled` + `border-err`, fills the send's own slot | `--err` |
| composer attach `+` / mic at rest | none; hover `--hover` | `--dialog-hint` |
| mic while recording | `--warn-surface`, pulsing | `--err` |
| mic in voice-conversation mode | `--accent` | `--accent-fg` |
| the project's new-session verb | `--primary` (amber fill) | `--primary-fg` |
| band close (`CloseButton isBand`) | Blockether Light: compact `--primary` disk; Blockether Dark: that same disk in `--primary-fg`; elsewhere the band's paper | `--primary-fg` on Light, `--primary` on Dark; elsewhere the band's ink |
| star on a session | — | `--warning` (never `--warning-border`: 2.9:1) |
| LIVE / WAITING chip | `--ok-bg` / `--warning-bg` | `--ok` / `--warning`, plus the word |

- **One filled primary VERB per screen** — or one verb repeated where the object repeats. Amber
  marks the single most important thing you can start; the project list paints it once per project
  band because the verb belongs to the project, and that is one control repeated, not two competing
  acts. Two DIFFERENT filled verbs in one view are a tie, not a hierarchy. The sole chrome exception
  is `CloseButton isBand`: a fixed 32px circle (28px on a pointer), carrying only `CloseIcon`, whose
  branded fill identifies the app's way out on both Blockether palettes — amber on Light's dark band,
  and the band's own ink under an amber mark on Dark's amber band. It may coexist with the primary verb
  because it is navigation, not another offered act; it never licenses a filled label or rectangle.
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

## 3. The face is small, the target is large

This is the rule that decides whether a screen looks like our app or like a toy, and this repo
already implements it:

- **The painted box rides the row's rhythm**: bar and header controls are a **32px face on touch,
  24–28px on a pointer** (`Button density="compact"`, ui.tsx:66); the search field is `h-8
  mouse:h-6` (ui.tsx:1790) because "a framed box 12px taller than every control beside it is the
  loudest thing on the row"; composer glyphs are `h-8 w-7` → `mouse:h-7 w-6`, the send is `size-8`
  → `mouse:size-7` (ui.tsx:1123).
- **The 44px target arrives as invisible slop around that face** — `Button`'s pseudo-element, the
  `SearchField`'s two strips above and below, `NewSessionButton`'s "compact 32px header rhythm
  while `Button` preserves a 44px touch target outside the painted box". **Never inflate the paint
  to the target.** Four 44px painted boxes in one strip is why a mockup looks fat; the shipping
  composer already avoids it.
- Pointer (`mouse:`) targets ≥ 28×28 px, and only the `mouse:` variant may ever make a control
  smaller — an iPad is a ≥ 640px screen driven by a hand.
- **≥ 8px between two adjacent targets**, and a destructive action is never adjacent to a common
  one. Delete lives behind an overflow (`⋯`) or after a gap in a swipe, never one pixel from Fork.
- Where a face stretches instead: a control that ENDS a row takes the row's own height
  (`h-auto self-stretch`), because a 32px face centred in a 44px row hovers as a floating band with
  a 6px dead strip above and below it (ui.tsx:236).
- The row IS the control. A row that opens something needs no `Open` button beside it; on touch a
  chevron states where the press goes, on a pointer the hover tint already did.
- Density is a *pointer* question, never a *screen-width* question. `scripts/touch-density.test.mjs`
  is the gate.

## 4. Chrome — one edge

- **One container edge per group.** A bordered section holding bordered cards holding bordered rows
  is three rectangles saying one thing; the container keeps its edge, rows get a hairline
  (`divide-y`) or nothing but hover tint. Box-in-box is the signature of a generated design.
- Group by **proximity and alignment** before you reach for a line: 4px base rhythm, one gap scale,
  and everything in a column shares one left edge. A separator is inset to the TEXT, not to the
  screen edge — that inset is what makes a list read as one column instead of a stack of slabs.
- The title takes the space (`flex-1` + truncate); metadata clusters at the trailing edge in
  reading order and never sits in fixed columns across a 1440px window.
- **Say a state only when it is worth saying.** A status repeated on 19 of 20 rows is not
  information — show `LIVE`, omit `IDLE`. Same for an empty checkbox, a chevron on a row that is
  not pressable, or an icon that repeats the word next to it.

## 5. Draw the product, not a product

A mockup is a claim about OUR app. Every control in it must exist in `apps/vis-companion/src`, in
the same place, at the same size, in the same token — or be labelled, in the artifact itself, as a
proposal with the reason it beats what ships. An invented control is worse than an ugly one: it
reviews a product nobody can build, and it is why a "modernized" screen reads as a stock template.

**The frames you draw at.** One canonical viewport per surface, and the frame is a plain rectangle: phone
**393×852pt** (safe areas 59pt top, 34pt bottom), small phone 375×812, tablet 834×1194 (crosses the app's `sm:`
40rem breakpoint, so density still follows the pointer), desktop window **1280×800**, TUI 120×40 cells.
**Never hand-draw the device.** An island, a status bar, a home indicator, a bezel, traffic lights or a browser
bar drawn by hand is fiction at the pixel level and is the fastest way to make real work read as a mockup —
draw the viewport, hatch the safe areas, and let the caption carry the size.

**Restyling changes paint, never behaviour.** A design pass may move a control inside its own screen and change
its box, its token or its type step. It may NOT remove a fact, merge two controls, drop a state, re-order a
flow, or "simplify" what a tool result shows — that is a product change wearing a design change's clothes, and
it is the one thing a design review is never asked for. If the shape argues for a functional change, say it in
one sentence and leave it out of the artifact.

**Draw the components before the screens.** An artifact opens with a component sheet: every control drawn ONCE,
at the size the code paints it, captioned with the file and line that owns it. The screens are then assembled
from that sheet and from nothing else — two rows that disagree about the same control mean the sheet is wrong,
not the screen.
**Before drawing a control, `grep` it and read the docstring above it.** Those docstrings are the
argument for the shape, usually written after a bug report; the standing ones:

- **The composer has ONE control per act** (`ComposerButton`, ui.tsx:1074). Attach is a **`+`**, not
  a paperclip: on native it opens a menu, because "the OS gallery sheet never opens a shutter"
  (SessionScreen.tsx:1442), and the glyph rotates 45° while that menu is open. Dictation and voice
  conversation are ONE microphone — tap acts in the current mode, press-and-hold switches it, and a
  pointer that cannot hold gets the same switch from a right-click or Shift+Enter
  (SessionScreen.tsx:6215). Send and stop share ONE slot, "which is already the right size: taking
  the whole of it is how the two never disagree about where the strip ends".
- **A press is a pointer event, not a click** (`useTapPress`, ui.tsx:1021): WKWebView can decide a
  tap was a hover and dispatch no click at all. Anything a finger presses must show its state on
  press — and an affordance that only exists on hover does not exist on touch.
- **A gesture reports itself while it happens**: the hold's paper rises through the button over
  450ms, "the one confirmation available to an app with no haptics" (ui.tsx:1110).
- **The new-session verb belongs to the project it starts in** (`NewSessionButton`, ui.tsx:3422):
  one amber `+` per project header, at the compact 32px face. There is **no global new button**, in
  either surface — a session needs a machine and a root, and a bar has neither. The trailing
  cluster of that band is (pager, verb) and nothing else, because measured at 320px "the count, the
  live pulse and the yellow verb take this cluster's width first and leave the project name 24px"
  (SessionsScreen.tsx:2863).
- **The list's pager rides in the project band**, not on a shelf under it: that shelf cost "a second
  paper, a second hairline and a second sticky layer for one heading, 40px of the screen".
- **Search leads with the glass, inside the box** (ui.tsx:1790), and clear is the app's one
  `CloseButton` at the field's own inset.
- **A band close is the branded way out** (`CloseButton isBand`, ui.tsx:1735): its 48px touch / 36px
  pointer target carries a 32px / 28px circle. Blockether Light fills that circle with `--primary` under
  a dark `--primary-fg` `CloseIcon`; Blockether Dark carries the same pair swapped — `--primary-fg` under
  an amber mark — because its title band is already amber, and a face left unfilled there is a way out
  fainter than everything it sits beside. This compact navigation identity is the one allowed fill beside
  a screen's primary verb. A close inside another control remains unfilled.
- **What the turn will run as is one line under the composer** (`MetaButton`, ui.tsx:1168): model
  and reasoning level, both pressable, the dotted rule marking the one that opens a picker.

If a screenshot review says "this looks wrong", the first question is not "what should it be" but
**"what does the code do, and what did its author say it was for"**.

## 6. Platform conventions

**Mobile (companion on iOS/Android)** — large title that collapses into a 44pt inline bar on
scroll; search pinned under it; inset-grouped lists; safe areas honoured (notch, home indicator,
keyboard). **The navigation bar carries navigation, not the screen's verb**: back stays in ink; close
is the shared compact `CloseButton isBand` disk; at most two trailing items. No other filled colour
block belongs in the bar — especially no filled label or rectangle that outranks the large title.
Two-line rows when one line cannot hold title + metadata at ≥ `text-ui`; swipe actions carry ICONS
with an accessible label, not text; commit-level haptics only. Support system text scaling: nothing
may clip or overlap at 130%.

**Entering data** (HIG) — pre-gather everything the system already knows; say what is wanted with a
label or a placeholder, not both; secure fields show a filled dot per character and are never
prefilled; offer a choice instead of typing where a list exists; accept paste and drag-and-drop as
first-class input (the composer does); validate as the value is entered rather than on submit; and
keep the submitting verb disabled until the required fields hold data. The keyboard is part of the
layout: the composer sits above it, the field grows to a bounded number of lines and then scrolls,
and a hardware pointer makes Enter send (`isEnterSendKeyboard`).

**Desktop (companion on a fine pointer)** — filter and keyboard-first navigation instead of
pagination; `/` focuses the filter, `Esc` clears, `↑↓` walks, `Enter` opens; a virtualized list
before a pager; hover reveals actions in a slot that already existed, so nothing reflows; any
truncated row or field carries the full text as an expansion tooltip (`title=`); every destructive
action confirmable and every dialog dismissible with `Esc`.

**iPad and split view** — density follows the pointer, so a 1/3-width iPad column stays a TOUCH
layout at a phone's rhythm; the layout may drop a pane, never shrink a target.

**Live things** — an item that is running says so with a tint and a word, and the badge belongs to
the item, not to the bar (HIG live-viewing). One live marker per row; the pulse is the exception,
not the ambience.

**TUI** — the grid is the layout engine: measure in CELLS, budget columns explicitly, and leave
≥ 1 cell of gutter. Box-drawing for the container only. Truncate with `…` at a known column, never
let a long title push a column off screen. Assume 8 colours and no italics somewhere. Verify with
the capture API (`…/test/com/blockether/vis/ext/channel_tui/capture.clj`): the PNG is what you
eyeball, the terminal-grid assertions are the regression gate — keep both.

## 7. States, feedback, motion

- Every asynchronous surface owns four paints: **loading, empty, error, and partial/stale**. An
  empty state names what would be here and offers the verb that creates it.
- Every error says what failed and what to do next; "Something went wrong" is not a design.
- Destructive actions confirm with the OBJECT named ("Delete 3 transcripts from `visgw`?"), and the
  confirming button carries the verb, never "OK".
- Motion: 120–200ms for a state change, 300ms ceiling, ease-out on entry; the app's own tokens are
  200/160/180ms. Nothing animates on list mount. Honour `prefers-reduced-motion` — the reduced path
  is instant, not slower.
- Feedback is immediate: a press changes something within one frame, even if the result is pending.

## 8. The anti-slop list

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
11. A mockup set in a font we do not ship — `system-ui`/SF Pro because the file only NAMED Inter,
    DejaVu, Arial, PIL's default — or app prose set in the mono family.
12. A filled accent block floating in a navigation bar, except the compact circular `CloseButton isBand`.
13. A control that does not exist in the code, drawn without saying it is a proposal — and its
    twin, a control that exists being drawn as something else (a paperclip for the composer `+`).
14. A painted face inflated to the 44px target: fat strips, a search field taller than its bar.
15. Two different filled primary verbs on one screen; the compact band-close disk is navigation, not a verb.
16. Facts deleted to make a screen look calm.
17. A mark drawn by hand where a name in `icons.tsx` exists — a bespoke `<path>`, a terminal glyph
    (`✓ ✗ ● ◇ → ▾`), or two icon families in one app.

## 9. Review protocol

When asked to review, audit, critique or modernize a design — screenshot, mockup, or code:

1. **Context**: which surface (app / TUI), which pointer (touch / fine), which theme(s), what the
   screen is FOR.
2. **Read the code that paints it first** (§5). A defect that is already explained by a docstring
   is a disagreement with an argument, and you have to answer the argument.
3. **Audit in this order**, because that is the order defects cost users: accessibility →
   platform conventions → hierarchy and density → visual system → interaction states →
   content and wording.
4. **Report** as:

```
## Design review: <screen>
### Verdict — Excellent / Good / Needs work / Critical
### Critical    what · why (cite the rule) · fix (in this repo's tokens)
### Improvements
### Keep        what already works — say so, do not invent 20 issues
```

5. **Cite the rule and the number**: "`text-chip` 8px is the only label on a 33px row — §1 and §3:
   ≥ `text-ui` and ≥ 44px on touch" beats "the text is small". A number is falsifiable; taste is
   not.
6. **Improving** (not just reviewing) adds: rank by severity × effort, propose in the app's own
   vocabulary (`ListRow`, `Button variant=…`, a `text-*` step, a palette token — never a raw
   utility), and sequence the work: accessibility, then conventions, then polish.

7. **Score it before you show it**, 1–5, honestly, and print the scores beside the artifact: **hierarchy**
   (exactly one element leads each screen), **fidelity** (every control exists in `src/**`), **restraint**
   (nothing grew because it could), **measurement** (every number checked, not felt), **variety** (two screens
   differ where the product differs). Anything below 3 is a revision, not a caveat.
8. **The tokens are locked.** No literal hex, no literal `font-family`, no literal px type size anywhere in an
   artifact — every colour is a token from `themes.generated.css`, every size a step from `index.css`, every
   mark a name from `icons.tsx`. A value invented mid-render is the most reliable tell of generated UI.

## 10. Done means

- Contrast recomputed for both themes; no meaning carried by colour alone; one filled primary verb
  (the compact `CloseButton isBand` navigation disk is the sole chrome exception).
- Targets measured on touch AND pointer; the FACE is on the row's rhythm and the target is slop
  around it; nothing shrank on a screen-width query.
- Type steps, families and palette tokens only — no literal size, no literal colour, no third font.
- Every control in the frame exists in `src/**` at the same size and token, or is marked a proposal.
- Loading, empty, error paints exist; destructive path confirms; `Esc`/back leaves.
- Text scaling to 130% and reduced motion both survive.
- App: `npm test -- <files>`, `npm run lint`, `npm run build` clean, and any new control pinned in
  `ui.test.tsx`. TUI: a capture PNG eyeballed and grid assertions added.
- Any mockup or review PNG is set in the shipped families (§1), loaded from the repo's own files,
  and the render was CHECKED to have used them (§12).
- Every mark is an app name from `src/components/icons.tsx` over a Lucide drawing, at the box its type step
  allows (§1): no second family, no hand-drawn path, no glyph, stroke `2` untouched, and the light theme's mark
  measured on its own panel.
- The frame is a viewport, not a drawn device, and no control or behaviour in it is invented (§5).
- Self-critique scored and printed; nothing below 3 (§9).

## 11. Component recipes — the measured defaults

Numbers a reviewer can check with `get box`. Deviate only with a reason in the diff.

| component | touch | pointer (`mouse:`) |
|---|---|---|
| list row | ≥60px, two lines: `text-subhead` 15/22 title (Inter) + `text-ui` 11/16 metadata (mono) | 34px, one line, fixed tracks per fact |
| row leading | 16px gutter carrying the unread mark; text at 32px | 7px mark, no gutter |
| separator | hairline inset to the TEXT (32px), never to the screen edge | inset 16px |
| trailing slot | time, then chips; swipe holds the actions | ONE 124px slot: time normally, four 28px icon buttons on hover — nothing reflows |
| project band | 36px sticky, `--color-level-project` paper: name · count · live · pager (`‹ 1 / 3 ›`) · the amber `+` (32px face) | 28px band, 24px face, pager paints the NUMBERS |
| search | 32px face (`h-8`) + 44px target as strips; NO keyboard-shortcut chip on a phone | 24px face, `/` chip, max 380px |
| segmented rail | 44px band, 28px painted chip | 26px chip |
| sheet / dialog | title band 44px, content top ≥104px, footer buttons 48px, backdrop ≤26% ink | centred panel, same bands |
| form control | 44px (select, password, text); OTP box 44×52; chip 32px; slider thumb 22 inside a 44 band; checkbox 20 inside a 44 row | 30px, thumb 16 |
| swipe action | 72px per cell, ICON ONLY at ≤72px — a 10px label collides with its neighbour; destructive after an 8px gap in its own tint | n/a |
| transcript | role label `text-ui` mono, body `text-title` 13/20, user message behind a 2px rule, tool call = 2px rule + `--code-bg` + `--result-bg` | same, but the column is capped at 720px and centred |
| composer | the FIELD carries the height (min 44px); its controls are 32×28 faces and one 32px send on the dark plate; a 28px tally + model line under it | 28×24 faces, 28px send, field 60px |
| settings | group label `text-ui`, rows 48px, switch 46×28 | rows 34px |
| theme swatch | ≥3 per row so the theme's real name fits at `text-ui` | ≥4 per row |
| artifact card | preview shows the CONTENT's shape (bars for an image, lines for a page, cells for a table) — never a generic file glyph; one edge (the preview), text unboxed below | same, 32px inline icon in a docked panel |
| row mark | 14px `circle-*` in a 16px gutter, centred on the first line | 12px in the same gutter; it never becomes a tap target |

Two derived rules that catch most of the damage:

- **Density follows the pointer, never the width.** An iPad is a large screen driven by a hand: the
  query is `mouse:`, and a `sm:` breakpoint may never shrink a target.
- **Fill the window.** A 1280×800 window whose list ends two thirds down is under-populated, not
  clean: at 34px that is ~21 rows plus headers. A mockup that paints eight rows is measuring nothing.

## 12. Paint it, then LOOK at it

A design answer that was never rendered and inspected is a guess, and it will read as one.

**Load the fonts, then prove they loaded.** The frame declares `@font-face` for `Inter Variable` and
`JetBrains Mono Variable` with the repo's own `.woff2` inlined as base64 — §1 owns the files and the
naming rule; a `file://` reference is refused by the renderer's origin rules. Then PROVE it in the
rendered page with a width probe, because nothing else reports a silent
fallback: paint two off-screen spans holding the same string, one in the family under test and one
in `system-ui`, and measure both.

```
#probe-a{font-family:'Inter Variable'}  #probe-b{font-family:system-ui}
spel --session <s> get box '#probe-a'   # widths must DIFFER
spel --session <s> get box '#probe-b'
```

Equal widths mean the face never loaded, and a frame that fell back to SF Pro invalidates every
judgement about density and hierarchy in it — SF is narrower than Inter, and JetBrains Mono is
wider than SF Mono, so every column in the frame measured the wrong product.

1. Build the screens as ONE self-contained HTML file at §5's canonical viewport for the surface —
   a frame that omits the safe areas lies about the space.
   Paint from `themes.generated.css` variables and the `text-*` steps only, both appearances.
2. Render and crop:
   `spel --session agent-<ts> set viewport 1440 900 && … open file:///… && … get box '#p1' && … screenshot -f /tmp/full.png`
   — chain the commands in ONE shell (the daemon lives with that process), then crop each frame with
   PIL from the `get box` figures. Close the session when the task ends.
3. Put the PNG in front of yourself (`attach(path, audience="model")`) and read it as a stranger.
   The first render is ALWAYS wrong in ways prose cannot predict: labels collide inside a cell, a
   glyph clips at the device edge, a path chip wraps mid-word, a keyboard chip appears on a phone.
4. Fix, re-render, look again. Ship the HTML as the artifact — it is inspectable at any zoom, and it
   is what §10 is checked against.
