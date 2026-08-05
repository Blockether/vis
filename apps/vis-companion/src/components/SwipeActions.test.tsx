import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { StarIcon } from './SwipeActions';

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
