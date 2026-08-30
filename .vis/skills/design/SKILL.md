---
name: design
description: >
  The binding design contract for every Vis surface: companion web/iOS/Android and TUI. Read it
  before changing or reviewing layout, controls, type, colour, motion, copy, stories, interactive
  previews or screenshots, and again before calling the result done. Storybook renders the shipped
  UI; Spel drives and verifies it; companion reviews receive self-contained HTML from the same code.
---

# Design

This skill owns the taste that no test or single namespace can own. It does not own component
implementation: `AGENTS.md`, the docstring above the component, and its tests do. Read those before
changing the shape. The app builds with the closed vocabulary in `src/components/ui.tsx`; the TUI
builds with the paint contracts under `extensions/channels/vis-channel-tui/`.

## 1. Decide before drawing

Write four lines before UI code:

1. **Lead** — the one element that wins the eye; subordinate everything else by size, weight, ink,
   then position.
2. **Type** — the allowed steps and family for each fact.
3. **Structure** — bands, planes, shared left edge and the one edge that groups them.
4. **Signature** — one memorable move that is true of this screen, not decoration.

Compare that plan with a similar Vis screen. Anything unchanged merely because it is a familiar UI
pattern is a reflex; revise it. A restyle may change paint and placement, never facts, states,
controls or flow. A functional change is a separate decision.

## 2. The visual system is closed

### Type

The app ships Inter for human prose and JetBrains Mono for machine facts, paths, ids, counts,
durations, models, code and control labels. The TUI uses the terminal face. Never add a third family
or rely on a system fallback.

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

## 5. Draw, verify and attach the product

There is no hand-built mockup. A proposal is a small diff in `src/**`, rendered from the app's own
build, then kept or reverted. Storybook draws components and deterministic states; the running app
draws screens with real data. **Use stable Storybook as a renderer. Do not add MCP, prerelease agent
tools or a second browser layer: Spel already navigates, measures and exercises it.**

Split by ownership, not line count or DOM shape. A screen owns routing, data loading and
orchestration; a feature component may own one coherent interaction and all of its visible states so
it can be rendered and tested alone. Keep a one-use screen fragment local. A generic control enters
`ui.tsx` only at its second real call site, with both callers converted in the same commit. Never
extract a wrapper, a speculative prop matrix or a preview-only twin of production markup.

Every reusable visual component and meaningful state has a story in the same commit. Vocabulary
controls live in `ui.stories.tsx`; data-heavy components use their colocated story and the one fixture
module `src/dev/story-data.ts`. Stories never fetch, wait on timers or generate random data.

### Inspect the shipped render

From `apps/vis-companion`:

```bash
npm run storybook                         # 127.0.0.1:6006
# isolated shipped story, in a real theme:
STORY='http://127.0.0.1:6006/iframe.html?id=<story-id>&viewMode=story&globals=theme:<theme-id>'
SESSION="agent-$(date +%s)"
spel --session "$SESSION" set device "iPhone 14" &&
spel --session "$SESSION" --content-boundaries open "$STORY" &&
spel --session "$SESSION" wait --text '<story-owned copy>' &&
spel --session "$SESSION" --content-boundaries snapshot -i -c
```

For a fine pointer, replace device emulation with `set viewport 1280 800`. If the story id is unknown,
open the Storybook manager, take `snapshot -i -c -a`, select the story through its fresh `@ref`, then
open its isolated iframe route. Wait for copy or a role owned by the story — the preview shell and its
spinner are not readiness. Use one unique Spel session for the whole task, re-snapshot after repaint,
and close only that session when every comparison is finished. Read `spel <command> --help` before
guessing an argument.

Canonical review frames are phone 393×852, tablet 834×1194 and desktop 1280×800. At each relevant
frame and theme, use `snapshot -i -c`, `get box` and `styles` to settle claims; exercise every changed
interaction and inspect a screenshot yourself. A green build is not a visual review.

### Attach the interactive render

For companion design choices, the default user-facing artifact is a self-contained **interactive
`.html` attachment**, not a screenshot. Build it from the current source after the last design change,
open it with Spel, exercise it, then attach it. Generated HTML is temporary evidence and never enters
the repository.

**One-to-one means one implementation, not similar pixels:**

1. The artifact imports the exact production component or composed story from `src/**`; it never
   copies JSX, serializes `outerHTML`, redraws a control or carries preview-only CSS.
2. It uses the same story args and deterministic fixture, decorators/providers, theme, production
   `index.css`, fonts, icons, viewport and input mode as the reviewed render. Tailwind scans both
   `src/**` and the artifact entry so no production class silently disappears.
3. Vite bundles React and inlines JS, CSS, fonts and images into one file. The result has no
   `localhost`, network fetch, external script, stylesheet or asset dependency.
4. A backend, gateway or native API may be replaced only at its existing boundary. Name that fixture
   or adapter beside the attachment; never claim that boundary is live.
5. Spel opens both the Storybook/running-app state and the standalone file at the same frame, then
   checks accessible names and states, representative boxes and styles, and each important
   interaction result. A mismatch rejects the artifact.

This is exact production rendering and behaviour **inside the component boundary**, with explicit
fixture data; it is not a claim that the attachment has the app's credentials or native shell.
Screenshots remain private inspection and measurement evidence. TUI review still uses the real
`DefaultVirtualTerminal` capture PNG because terminal cells are its shipped surface.

## 6. Review and finish

Review in this order: accessibility → platform convention → hierarchy/density → visual system →
states → words. Report only defects that survive the code's existing argument, with the measured
value, the rule above and the smallest fix in the shipped vocabulary. Say what should stay.

Done means:

- exactly one lead and one signature; no generic reflex survived the comparison pass;
- type, tokens, marks, contrast and targets satisfy §2 in every relevant theme and input mode;
- loading, empty, error and partial/stale states exist; motion, 130% type and reduced motion survive;
- every shown control and word ships from `src/**`; every reusable state has a deterministic story;
- app: relevant tests, `npm run lint`, `npm run test:storybook` and `npm run build` pass;
- companion review: the attached HTML satisfies the one-to-one checks in §5 and its fixture seams
  are named; TUI review: its capture PNG was inspected and terminal-grid assertions pass;
- the final artifact came from the production component or running app, never a drawing or copied
  preview implementation;
- one last pass removed anything that did not earn its place.
