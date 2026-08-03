import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { LiveTally, UnreadBadge } from './ui';

// A scope chip has no room for the word "live", so the count is parenthesised
// and only the NUMBER is green: `All (5)`. The `\u25cf` glyph it replaces carried its
// own metrics and sat below the digits' optical centre, which is what made the
// green read as one low, smudged token in an otherwise centred chip.
describe('LiveTally', () => {
  it('colours the number and nothing else, with no glyph', () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).not.toContain('\u25cf');
    expect(html).toContain('(<span class="font-bold text-ok">5</span>)');
  });

  it('says what the number counts, for a reader that cannot see green', () => {
    const html = renderToStaticMarkup(<LiveTally count={1} />);

    expect(html).toContain('<span class="sr-only"> live</span>');
  });
});

// Unread is a notification, not a second tally: beside the parenthesised live
// count it has to be told apart from it WITHOUT the reader remembering a colour
// code, so it wears the same filled amber block the session row uses for "new".
describe('UnreadBadge', () => {
  it('is a filled block, not a bare number beside the live count', () => {
    const html = renderToStaticMarkup(<UnreadBadge count={3} />);

    expect(html).toContain('bg-accent');
    expect(html).toContain('text-accent-foreground');
    expect(html).toContain('>3<');
    expect(html).not.toContain('(');
  });

  it('says what the number counts, for a reader that cannot see amber', () => {
    const html = renderToStaticMarkup(<UnreadBadge count={1} />);

    expect(html).toContain('<span class="sr-only"> unread</span>');
  });

  it('renders nothing when there is nothing new', () => {
    expect(renderToStaticMarkup(<UnreadBadge count={0} />)).toBe('');
  });
});
