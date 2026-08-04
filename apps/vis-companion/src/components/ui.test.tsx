import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { LiveTally, UnreadBadge } from './ui';

// The live count wears the SAME filled block as the unread badge, in green:
// `macbook \u25ae3\u25ae\u25ae4\u25ae` — one shape, two colours, running then
// waiting. It replaced a bracketed `[3]`, which read lighter than the badge
// beside it, and before that a `\u25cf` whose metrics sat below the digits.
describe('LiveTally', () => {
  it('is a filled green block, not bracketed text or a glyph', () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).toContain('bg-ok');
    expect(html).toContain('text-ok-foreground');
    expect(html).toContain('>5<');
    expect(html).not.toContain('[');
    expect(html).not.toContain('\u25cf');
  });

  it('says what the number counts, for a reader that cannot see green', () => {
    const html = renderToStaticMarkup(<LiveTally count={1} />);

    expect(html).toContain('<span class="sr-only"> live</span>');
  });

  it('renders nothing when nothing is running', () => {
    expect(renderToStaticMarkup(<LiveTally count={0} />)).toBe('');
  });
});

// Unread is a notification, not a second tally: beside the bracketed live
// count it has to be told apart from it WITHOUT the reader remembering a colour
// code, so it wears the same filled amber block the session row uses for "new".
describe('UnreadBadge', () => {
  it('is a filled block, not a bare number beside the live count', () => {
    const html = renderToStaticMarkup(<UnreadBadge count={3} />);

    expect(html).toContain('bg-accent');
    expect(html).toContain('text-accent-foreground');
    expect(html).toContain('>3<');
    expect(html).not.toContain('[');
  });

  it('says what the number counts, for a reader that cannot see amber', () => {
    const html = renderToStaticMarkup(<UnreadBadge count={1} />);

    expect(html).toContain('<span class="sr-only"> unread</span>');
  });

  it('renders nothing when there is nothing new', () => {
    expect(renderToStaticMarkup(<UnreadBadge count={0} />)).toBe('');
  });
});
