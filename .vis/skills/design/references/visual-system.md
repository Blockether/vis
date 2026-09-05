# Visual system

### Type

The app ships JetBrains Mono for every word and fact: prose, controls, paths, ids, counts, durations,
models and code. The TUI uses the terminal face. Never add a second family or rely on a system
fallback.

| step | px/line | role |
|---|---:|---|
| `text-display` | 24/30 | one screen name |
| `text-head` | 17/24 | screen or sheet title |
| `text-subhead` | 15/22 | section lead; two-line touch-row title |
| `text-title` | 13/20 | pointer-row lead; transcript body |
| `text-body` | 12/18 | prose and descriptions |
| `text-ui` | 11/16 | controls; metadata floor on touch |
| `text-meta` | 10/16 | pointer-only metadata |
| `text-chip` | 8/14 | a short tag, never the only copy |

Use at most three steps in a row and four on a screen. Use weight and ink before another size. No
literal type size, `leading-*`, synthesized face or all-caps sentence; caps are only for short tags.

### Colour and marks

- Use semantic theme tokens only. No literal colour, palette utility, gradient or decorative blur.
- Measure against the surface the ink actually touches in every shipped theme: **4.5:1** for small
  text, **3:1** for large/bold text and any meaningful icon, glyph or rule.
- Colour repeats meaning; it never carries meaning alone. Pair state colour with shape, word or
  position.
- One accent and one filled primary verb per screen. Navigation is not a competing verb.
- App marks come through `src/components/icons.tsx` from Lucide. No hand-drawn SVG, font glyph,
  emoji or second icon family. TUI marks are cells and follow its own vocabulary.
- Match the mark box to the adjacent type: 18px at `text-head`, 14px at `text-title`, 12px at
  `text-ui`. Keep Lucide's stroke unchanged.

### Geometry and density

- Touch targets are at least **44×44px**; fine-pointer targets at least **28×28px**; adjacent targets
  keep **8px** between them. The painted face may be smaller: invisible hit slop supplies the target.
- Density follows the pointer (`mouse:`), never viewport width. An iPad remains touch-first.
- One container edge per group. Prefer proximity, alignment and one inset hairline to nested cards.
  Vis planes are square and hairline-ruled; a component may round only the face its contract owns.
- Every column shares one left edge. Titles take spare width; metadata stays at the trailing edge.
  Hover actions occupy a reserved slot and never reflow the row.
- The row is the control. Do not add an `Open` button to a row that already opens.

### States, motion and words

- Every asynchronous surface has loading, empty, error and partial/stale paints. An error says what
  failed and what to do; an empty state names what belongs there and offers the real verb.
- A press responds within one frame. State motion is 120–200ms, never over 300ms, and becomes instant
  under `prefers-reduced-motion`; lists do not animate on mount.
- Destruction names the object and confirms with the verb, never `OK`.
- Write from the user's side: sentence case, active voice, plain verbs, no filler. One act keeps one
  name through control, progress and result. Every word in a story is product copy, not lorem text.

## 3. Surface conventions

- **Touch app:** honour safe areas, keyboard and 130% text scaling. A navigation bar carries
  navigation, not the screen's primary verb. Two-line rows keep metadata at `text-ui` or larger.
- **Pointer app:** prefer filter and keyboard navigation to pagination; `/` focuses search, `Esc`
  leaves or clears, arrows move and Enter opens. Reveal actions without moving content. Truncated
  facts expose their full value.
- **TUI:** budget cells explicitly, leave at least one-cell gutters, assume eight colours and no
  italics, and truncate at a known column. Box drawing belongs to the outer container, not every row.

## 4. Reject generated slop

Reject on sight:

1. nested bordered or rounded cards;
2. a centred hero on a working screen;
3. decorative icons, gradients, glass or multiple shadow depths;
4. repeated status text, chevrons on inert rows or labels that say nothing but “Manage”;
5. fixed metadata columns that strand space;
6. six type sizes or weak grey-on-grey text;
7. a 44px painted face where only the target needed 44px;
8. two primary verbs, two icon families or meaning carried only by colour;
9. facts removed merely to make the screen look quiet;
10. a control, token, font or behaviour drawn in a review that does not ship.
