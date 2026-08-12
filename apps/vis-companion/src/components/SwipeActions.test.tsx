import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { StarIcon } from './icons';

// Regression (reported: "make the star icon truly yellow"): the filled star used
// `fill-current`, so it inherited `text-accent-ink` — the legible amber INK
// (`--warning`, a dark brown #7a4a00 in light theme) — and a starred session
// showed a brown glyph, never the brand yellow. A filled star is an amber FILL,
// and the design system's own rule is "amber fills use accent" (#ffc420), so the
// filled state paints itself `accent` instead of borrowing the text colour.
describe('StarIcon', () => {
  it('fills the starred glyph with the brand accent, not the inherited ink', () => {
    const svg = renderToStaticMarkup(<StarIcon filled />);
    expect(svg).toContain('fill-accent');
    // The edge is the amber INK: the yellow fill alone is 1.45:1 on the row's
    // paper, so the mark the human left had no visible shape (see icons.tsx).
    expect(svg).toContain('stroke-accent-ink');
    // It must NOT fall back to currentColor, which is what let the muted ink win.
    expect(svg).not.toContain('fill-current');
  });

  it('leaves the un-starred outline adaptive so it reads among other actions', () => {
    const svg = renderToStaticMarkup(<StarIcon />);
    expect(svg).toContain('fill-none');
    expect(svg).toContain('stroke-current');
  });
});

// Regression, user report ("the colour is the same as rename"): every action on the
// strip wore one neutral ink, so the star — the mark the human types in themselves,
// and the only yellow thing in the list — looked like one more grey verb.
describe('SwipeActions tones', () => {
  const strip = (tone?: 'neutral' | 'accent' | 'danger') =>
    renderToStaticMarkup(
      <SwipeActions
        label="a session"
        actions={[{ key: 'favorite', label: 'Star', icon: <StarIcon />, tone, onSelect: () => {} }]}
      >
        <span>row</span>
      </SwipeActions>,
    );

  // Regression, user report ("see why the starred stuff looks so disgusting"): the
  // amber SLAB was right and the amber CAPTION was not — `text-accent` is the
  // #ffc420 button FILL, and on this cell's own 15% tint it measures 1.37:1, so
  // "STAR" arrived as a smear the width of a word. The slab keeps the meaning;
  // the caption takes the ink the palette ships for amber text (6.4:1 here).
  it('paints an accent action on the brand yellow, in the legible amber ink', () => {
    const html = strip('accent');
    expect(html).toContain('bg-accent/15');
    expect(html).toContain('text-accent-ink');
    // The fill is spent on hover, where it becomes the background and takes its
    // own foreground — never on 9px text.
    expect(html).toContain('hover:bg-accent hover:text-accent-foreground');
  });

  // The strip's own colour lives in the slab, so a neutral verb has none: it is
  // the ink alone on the panel, which is what an accent action must not look like.
  it('leaves a neutral action in the shared verb ink', () => {
    const html = strip();
    expect(html).toContain('text-accent-ink');
    expect(html).toContain('bg-panel-2');
    expect(html).not.toContain('bg-accent/15');
  });

  // The same split, in red: `--err` is a badge fill and reads 3.50:1 as a caption
  // on its own tint, under the 4.5 a 9px bold label owes. The list-safe pair
  // (`err-surface` + `err-ink`, the tokens the in-row delete confirm already uses)
  // reads 5.4:1 without turning half the row into an alarm.
  it('paints a danger action in the list-safe red, not the badge fill', () => {
    const html = strip('danger');
    expect(html).toContain('bg-err-surface');
    expect(html).toContain('text-err-ink');
    expect(html).not.toContain('bg-err/15');
  });

  // Regression, user report ("this also has not full height of the parent"): the swipe
  // track is as tall as its TALLEST panel, and the action strip — a 16px icon over a
  // 10px caption — measured 34px against a 32px desktop session row. The row panel
  // stretched to 34 but the row inside it stayed 32, so the button's hover slab stopped
  // 2px short of the rule under it. The panel is a GRID: one child, stretched on both
  // axes, so whatever height the track ends up with is the row's height too.
  it('lets the row fill the swipe track', () => {
    expect(strip()).toContain('grid w-full shrink-0 snap-start');
  });
});
