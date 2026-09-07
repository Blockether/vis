# Visual system

### Type

Use JetBrains Mono for all app text, including controls, metadata and code.
The TUI uses the terminal's font. Do not add another font family or depend on
a system fallback.

| step | px/line | role |
|---|---:|---|
| `text-display` | 24/30 | one screen name |
| `text-head` | 17/24 | screen or sheet title |
| `text-subhead` | 15/22 | section heading; two-line touch-row title |
| `text-title` | 13/20 | pointer-row title; transcript body |
| `text-body` | 12/18 | prose and descriptions |
| `text-ui` | 11/16 | controls; minimum metadata size on touch devices |
| `text-meta` | 10/16 | pointer-only metadata |
| `text-chip` | 8/14 | short tags, not standalone descriptions |

Use at most three text sizes per row and four per screen. Prefer weight and
color changes before adding a size. Do not use literal sizes, `leading-*`,
synthesized fonts or all-caps sentences. Capitals are allowed for short tags.

### Colour and marks

- Use semantic theme tokens only. No literal colour, palette utility, gradient or decorative blur.
- Measure contrast against the actual background in every supported theme:
  **4.5:1** for small text; **3:1** for large/bold text and meaningful icons or lines.
- Pair state colors with a shape, word or position; color alone is insufficient.
- Use one accent and one filled primary-action control per screen. Keep
  navigation visually secondary.
- App icons come from Lucide through `src/components/icons.tsx`. Do not add
  custom SVGs, font glyphs, emoji or another icon family. Use the TUI's
  existing cell-based symbols.
- Match icon size to adjacent text: 18px for `text-head`, 14px for `text-title`,
  12px for `text-ui`. Keep Lucide's stroke width unchanged.

### Geometry and density

- Touch targets must be at least **44×44px** and mouse targets **28×28px**, with
  **8px** between adjacent targets. The visible control can be smaller than
  its clickable area.
- Use `mouse:` for density, not viewport width. An iPad uses touch sizing.
- Use one border per group. Prefer spacing, alignment and an inset separator
  over nested cards. Containers are square with thin borders. Round a control
  only where its component contract permits.
- Align each column to one left edge. Titles use available width; metadata
  aligns to the end. Reserve space for hover actions to prevent layout shifts.
- Do not add an `Open` button to a row that already opens when selected.

### States, motion and words

- Async components need loading, empty, error and partial/stale states. Errors
  explain the failure and recovery action. Empty states explain what belongs
  there and provide an appropriate action.
- Respond to a press within one frame. Transitions last 120–200ms, never more
  than 300ms, and are disabled under `prefers-reduced-motion`. Do not animate
  lists on mount.
- Destructive confirmations name the object and action; do not label them `OK`.
- Use sentence case, active voice and direct language. Keep action names
  consistent across controls, progress and results. Stories use realistic
  product text rather than placeholder text.

## Platform conventions

- **Touch app:** respect safe areas, keyboards and 130% text scaling. Keep the
  primary action out of the navigation bar. Use `text-ui` or larger metadata
  in two-line rows.
- **Pointer app:** prefer filtering and keyboard navigation to pagination.
  `/` focuses search, `Esc` leaves or clears it, arrows move and Enter opens.
  Reveal actions without moving content. Make truncated values accessible.
- **TUI:** calculate cell sizes explicitly, leave at least one cell between
  columns, assume eight colors and no italics, and truncate at a defined
  column. Draw borders around containers, not every row.

## Avoid

1. Nested bordered or rounded cards.
2. Oversized centered headings on task screens.
3. Decorative icons, gradients, translucent backgrounds or multiple shadow sizes.
4. Repeated status text, chevrons on inactive rows or vague `Manage` labels.
5. Fixed metadata columns that waste available width.
6. Excessive text sizes or low-contrast text.
7. A 44px visible control when only its touch target needs that size.
8. Multiple primary actions, icon families or states indicated only by color.
9. Removing information solely to simplify appearance.
10. Review mockups with controls, fonts or behavior absent from production.
