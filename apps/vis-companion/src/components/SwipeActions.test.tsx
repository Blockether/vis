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
    expect(svg).toContain('stroke-accent');
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

  it('paints an accent action in the brand yellow', () => {
    const html = strip('accent');
    expect(html).toContain('text-accent');
    expect(html).not.toContain('text-accent-ink');
  });

  it('leaves a neutral action in the shared verb ink', () => {
    expect(strip()).toContain('text-accent-ink');
  });
});
