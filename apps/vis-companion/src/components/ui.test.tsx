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
  NewSessionButton,
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

// A machine gap separates computers with space; the banner supplies the promoted name.
describe('MachineGap', () => {
  it('is a band of the page ink, not another hairline', () => {
    const html = renderToStaticMarkup(<MachineGap />);

    expect(html).toContain('h-3');
    expect(html).toContain('-mb-px');
    expect(html).toContain('bg-ink');
    expect(html).toContain('border-edge-strong');
    expect(html).not.toContain('border-dialog-edge');
  });

  it('is decoration, so a screen reader never stops on it', () => {
    expect(renderToStaticMarkup(<MachineGap />)).toContain('aria-hidden="true"');
  });
});

describe('MachineBanner', () => {
  // Regression, reported machine block overflow: its banner was narrower than
  // every session row and used a stronger rule, so the machine looked clipped
  // instead of belonging to the same list.
  it('fills the machine block and uses the session-row boundary', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('<header');
    expect(html).toContain('border-y border-dialog-edge');
    expect(html).not.toContain('border-edge-strong');
    expect(html).not.toContain('mr-2');
    expect(html).not.toContain('sm:mr-0');
  });

  // Regression, issue: adjacent list edges rendered as two visible rules when the machine banner followed the filter or machine gap.
  it('keeps both edges visible without a sticky negative margin', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('border-y border-dialog-edge');
    expect(html).not.toContain('-mt-px');
  });

  it('uses the same unforced title and metadata typography as project headers', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).not.toContain('uppercase');
    expect(html).not.toContain('tracking-[0.12em]');
    expect(html).not.toContain('font-bold');
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

// Regression (reported: "the new session is button so frequently used that we should
// take it from the ⋯ and put on every machine header before the ⋯, as a yellow
// button"): the verb this whole screen exists for was the first row of a menu, so the
// thing people do all day cost a tap, a popover and a read before it could be pressed.
describe('NewSessionButton', () => {
  const html = (props: Partial<Parameters<typeof NewSessionButton>[0]> = {}) =>
    renderToStaticMarkup(<NewSessionButton machine="tower" onPress={() => {}} {...props} />);

  it('is the yellow one: the verb of the screen, not a row of a menu', () => {
    expect(html()).toContain('bg-accent');
    expect(html()).toContain('New session');
  });

  it('names the machine it will start on, because every header carries one', () => {
    expect(html({ machine: 'nuc' })).toContain('aria-label="New session on nuc"');
  });

  it('puts the project on the tooltip, where the header has no room for a path', () => {
    expect(html({ where: 'vis' })).toContain('title="New session on tower, in vis"');
    expect(html()).toContain('title="New session on tower"');
  });

  it('does not move under the press: it anchors the folder browser', () => {
    expect(html()).not.toContain('active:scale');
  });

  // Reported visual defect: after the stretched button was given `mouse:h-7`, the inherited
  // `sm:min-h-8` still held it at 32px and flex-start pinned it to the project's top edge.
  it('uses a compact line-safe mouse box centered in the project row', () => {
    expect(html()).toContain('mouse:h-7');
    expect(html()).toContain('mouse:min-h-7');
    expect(html()).toContain('mouse:self-center');
    expect(html()).toContain('mouse:text-meta');
  });

  it('is refused while the machine is busy or not answering', () => {
    expect(html({ disabled: true })).toContain('disabled=""');
  });
});
