import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { MACHINE_COLORS } from '../lib/machine-colors';
import {
  Button,
  LiveTally,
  MachineBanner,
  MachineGap,
  MachineMark,
  MachineRail,
  UnreadBadge,
} from './ui';

// Regression (reported: "why we still have this chevron here showing something is
// collapsible if we cannot click it — let's have just one color"): the caret half of
// the New session split control was painted in the dark title-bar ink, so an amber
// primary carried a charcoal slab that read as switched-off chrome — the one half
// that DOES open a menu looked like the one half nobody may press.
describe('split button', () => {
  const split = () =>
    renderToStaticMarkup(
      <span className="flex items-stretch">
        <Button pressEffect="none" className="border-r-0">New session</Button>
        <Button pressEffect="none" aria-haspopup="menu" className="border-l-accent-foreground/30">
          <span aria-hidden>▾</span>
        </Button>
      </span>,
    );

  it('paints both halves in the one accent, split by a hairline of its own ink', () => {
    const html = split();

    expect(html.match(/(?<!:)bg-accent(?![/-])/g)).toHaveLength(2);
    expect(html).toContain('border-l-accent-foreground/30');
    expect(html).not.toContain('bg-dialog-title');
    expect(html).not.toContain('text-dialog-title-foreground');
  });

  it('leaves the caret pressable, so the chevron is a promise the control keeps', () => {
    const html = split();

    expect(html.match(/<button/g)).toHaveLength(2);
    expect(html).toContain('aria-haspopup="menu"');
    expect(html).not.toContain('disabled=""');
  });
});

// The live count wears the SAME filled block as the unread badge, in green:
// `macbook \u25ae3\u25ae\u25ae4\u25ae` — one shape, two colours, running then
// waiting. It replaced a bracketed `[3]`, which read lighter than the badge
// beside it, and before that a `\u25cf` whose metrics sat below the digits.
describe('LiveTally', () => {
  it('is a filled green block, not bracketed text or a glyph', () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).toContain('bg-ok-surface');
    expect(html).toContain('text-ok-foreground');
    expect(html).toContain('>5<');
    expect(html).not.toContain('[');
    expect(html).not.toContain('\u25cf');
  });

  // `--ok` is the app's green INK (LIVE text, the 6px machine dot). Poured into
  // a badge it is a slab twice as dark as the amber block beside it and carries
  // its digit at 5:1; the fill has to be the lightened `ok-surface` peer.
  it('fills with the green surface, never with the green ink', () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).not.toMatch(/bg-ok(?!-surface)/);
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

// The sessions list used to close a whole computer with the SAME 1px
// `border-dialog-edge` hairline that closes a project, so the second machine's
// header read as a third project of the first one (see the `machine-shipped`
// design proposal, kept as the before picture). The boundary is now space plus
// a promoted name.
describe('MachineGap', () => {
  it('is a band of the page ink, not another hairline', () => {
    const html = renderToStaticMarkup(<MachineGap />);

    expect(html).toContain('h-3');
    expect(html).toContain('bg-ink');
    expect(html).toContain('border-edge-strong');
    expect(html).not.toContain('border-dialog-edge');
  });

  it('is decoration, so a screen reader never stops on it', () => {
    expect(renderToStaticMarkup(<MachineGap />)).toContain('aria-hidden="true"');
  });
});

describe('MachineBanner', () => {
  it('is set as a banner, not as one more row', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('<header');
    expect(html).toContain('uppercase');
    expect(html).toContain('tracking-[0.12em]');
    expect(html).toContain('font-bold');
    expect(html).toContain('border-edge-strong');
    expect(html).not.toContain('border-dialog-edge');
  });

  it('matches a project header’s 44px minimum touch height', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('min-h-11');
  });

  // A machine can hold hundreds of sessions; scrolled past, the name is the
  // only thing that answers "which computer is this row on".
  it('sticks to the top of the scroller', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('sticky');
    expect(html).toContain('top-0');
    expect(html).toContain('studio-mbp');
  });

  // Regression: on iOS the sticky band is composited over WKWebView's overlay
  // scrollbar, so the thumb disappeared behind every machine header.
  it('leaves the overlay scrollbar visible on phones', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('mr-2');
    expect(html).toContain('sm:mr-0');
  });
});

// A machine's hue is what separates two computers before a single word is read:
// the rail runs down everything it owns, and the same hue marks its banner and
// its scope chip so the chip you tapped and the rows you got back match.
describe('MachineRail', () => {
  it('draws the machine colour as a 2px rail down its block', () => {
    const html = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[3]}>rows</MachineRail>,
    );

    expect(html).toContain('border-l-2');
    expect(html).toContain(MACHINE_COLORS[3].rail);
    expect(html).toContain('rows');
  });

  it('gives two machines two DIFFERENT rails', () => {
    const first = renderToStaticMarkup(<MachineRail color={MACHINE_COLORS[0]}>a</MachineRail>);
    const second = renderToStaticMarkup(<MachineRail color={MACHINE_COLORS[1]}>a</MachineRail>);

    expect(first).not.toBe(second);
  });

  // One machine paired is not a boundary, so the concept has to vanish entirely
  // rather than indent the whole list by two pixels of some arbitrary hue.
  it('is nothing at all without a colour', () => {
    expect(renderToStaticMarkup(<MachineRail>rows</MachineRail>)).toBe('rows');
  });
});

describe('MachineMark', () => {
  it('is the rail hue as a solid block, and decoration only', () => {
    const html = renderToStaticMarkup(<MachineMark color={MACHINE_COLORS[7]} />);

    expect(html).toContain(MACHINE_COLORS[7].dot);
    expect(html).toContain('aria-hidden="true"');
    expect(html).not.toContain('bg-ok');
  });
});
