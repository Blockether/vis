import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { MACHINE_COLORS } from '../lib/machine-colors';
import {
  Button,
  DialogClose,
  HeaderActions,
  HeaderMeta,
  IconButton,
  KebabButton,
  LiveCount,
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
  it('is a band of page ink, not another pair of overlapping rules', () => {
    const html = renderToStaticMarkup(<MachineGap />);

    expect(html).toContain('h-3');
    expect(html).toContain('bg-ink');
    expect(html).not.toContain('border-');
    expect(html).not.toContain('-mb-px');
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
    expect(html).toContain('border-b border-dialog-edge');
    expect(html).not.toContain('border-edge-strong');
    expect(html).not.toContain('mr-2');
    expect(html).not.toContain('sm:mr-0');
  });

  // Regression, user report: the banner's top rule stacked on the filter or machine-gap
  // boundary, so one visual seam had multiple DOM owners and rendered heavier than the next.
  it('owns only its outgoing edge and never overlaps a neighboring band', () => {
    const html = renderToStaticMarkup(<MachineBanner>studio-mbp</MachineBanner>);

    expect(html).toContain('border-b border-dialog-edge');
    expect(html).not.toContain('border-y');
    expect(html).not.toContain('border-t');
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

  // Regression, user report: the 28px desktop box still read as a tall slab beside the
  // 24px machine action. The shared 16px line box fits safely inside a 24px control.
  it('uses the same compact mouse height as the neighboring small action', () => {
    expect(html()).toContain('mouse:h-6');
    expect(html()).toContain('mouse:min-h-6');
    expect(html()).toContain('self-center');
    expect(html()).toContain('h-11');
    expect(html()).toContain('mouse:text-meta');
    expect(html()).not.toContain('mouse:h-7');
  });

  it('is refused while the machine is busy or not answering', () => {
    expect(html({ disabled: true })).toContain('disabled=""');
  });
});

// Regression, user report ("this new session button should be the same as other
// buttons"): every icon-only control was written by hand at its call site, so the
// machine header's `⋯` was a 32px bordered box while the project header's, one row
// below it, was a 44px borderless slab with a bigger glyph — and neither of them
// looked like the yellow button they stood beside.
describe('IconButton', () => {
  const html = (props: Partial<Parameters<typeof IconButton>[0]> = {}) =>
    renderToStaticMarkup(
      <IconButton label="Actions for tower" {...props}>
        <span aria-hidden>⋯</span>
      </IconButton>,
    );

  it('is the app’s button with a glyph where its word would be', () => {
    expect(html()).toContain('border-edge-strong');
    expect(html()).toContain('min-h-7');
    expect(html()).toContain('focus-visible:ring-accent/60');
  });

  it('wears the same compact desktop box as the yellow button beside it', () => {
    const primary = renderToStaticMarkup(<NewSessionButton machine="tower" onPress={() => {}} />);

    for (const rhythm of ['h-11', 'self-center', 'mouse:h-6', 'mouse:min-h-6']) {
      expect(html()).toContain(rhythm);
      expect(primary).toContain(rhythm);
    }
  });

  it('is named, because it carries no word', () => {
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it('does not move under the press: it anchors a menu', () => {
    expect(html()).not.toContain('active:scale');
  });
});

// Regression, user report ("there is this exit button in the artifacts and it also
// looks awful"): the artifacts sheet, an opened artifact and every dialog each spelled
// their own close out again, so the sheet ended up wearing a bordered chip in a strip
// of bordered chips where every other surface wears chrome.
describe('DialogClose', () => {
  const html = (props: Partial<Parameters<typeof DialogClose>[0]> = {}) =>
    renderToStaticMarkup(<DialogClose label="Close artifacts" onClose={() => {}} {...props} />);

  it('is welded to the band it closes, by that band’s own hairline', () => {
    expect(html()).toContain('border-l');
    expect(html()).not.toMatch(/class="[^"]*\bborder\s/);
  });

  // Closing is not a destructive act until you mean it.
  it('goes red only under the pointer', () => {
    expect(html()).toContain('hover:bg-err/15');
    expect(html()).toContain('hover:text-err');
    expect(html()).not.toContain('text-err"');
  });

  it('changes nothing but the paper it sits on', () => {
    expect(html()).toContain('border-dialog-title-foreground/20');
    expect(html({ tone: 'panel' })).toContain('border-dialog-edge');
  });

  it('is named for what it closes', () => {
    expect(html()).toContain('aria-label="Close artifacts"');
  });
});

// Regression, user report ("still the ⋯ between the machine and project are different
// fix it! MARGIN RIGHT DIFFERS AND ALSO WHY THERE ARE FUCKING BORDERS"): the two
// kebabs had become the same Button, but each call site still spelled out its own
// popup semantics and glyph, and the app's default bordered box turned a header
// glyph into a second rival to the yellow verb standing beside it.
describe('KebabButton', () => {
  const html = (props: Partial<Parameters<typeof KebabButton>[0]> = {}) =>
    renderToStaticMarkup(<KebabButton label="Actions for tower" {...props} />);

  it('is one control: the machine’s and the project’s render the same box', () => {
    expect(html({ label: 'Actions for tower' }).replace('Actions for tower', 'X')).toBe(
      html({ label: 'Actions for vis' }).replace('Actions for vis', 'X'),
    );
  });

  it('wears no resting border: a header glyph is ink, not a rival box', () => {
    expect(html()).toContain('border-transparent');
    expect(html().replaceAll('hover:border-edge-strong', '')).not.toContain('border-edge-strong');
  });

  it('carries the popup semantics itself, so no call site can forget them', () => {
    expect(html()).toContain('aria-haspopup="menu"');
    expect(html({ isOpen: true })).toContain('aria-expanded="true"');
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it('keeps the header’s compact rhythm and one glyph size', () => {
    expect(html()).toContain('h-11');
    expect(html()).toContain('mouse:h-6');
    expect(html()).not.toContain('active:scale');
  });

  // Over a thumbnail the app's paper is not underneath it, so the same control brings
  // its own ink instead of a call-site `bg-*` that Tailwind's emission order decides.
  it('has an overlay face for the artifact tile, with no height of its own', () => {
    const over = html({ variant: 'overlay', density: 'default' });
    expect(over).toContain('bg-ink/80');
    expect(over).not.toContain('h-11');
  });
});

// Regression, user report ("MARGIN RIGHT DIFFERS AND ALSO WHY THERE IS NO MARGIN
// BEFORE NEW SESSION"): the machine header padded its own right edge while the project
// header one row below ended flush against the screen, and the yellow verb was welded
// to the words beside it. The trailing cluster is one component now, so all three gaps
// are decided once.
describe('HeaderActions', () => {
  const html = renderToStaticMarkup(
    <HeaderActions>
      <HeaderMeta>2 projects</HeaderMeta>
      <KebabButton label="Actions for tower" />
    </HeaderActions>,
  );

  it('owns the right edge of every header in the list', () => {
    expect(html).toContain('pr-3');
    expect(html).toContain('sm:pr-4');
  });

  it('puts a gap in front of the cluster and between its controls', () => {
    expect(html).toContain('pl-2');
    expect(html).toContain('gap-2');
  });

  it('never stretches: a header control is centred in whatever row it landed in', () => {
    expect(html).toContain('shrink-0');
    expect(html).toContain('self-center');
  });

  it('is the only one padding that side, so the header stops doing it', () => {
    const banner = renderToStaticMarkup(<MachineBanner>machine</MachineBanner>);
    expect(banner).toContain('pl-3');
    expect(banner).toContain('sm:pl-4');
    expect(banner).not.toContain('px-3');
    expect(banner).not.toContain('pr-');
  });
});

describe('LiveCount', () => {
  it('says nothing when nothing is running', () => {
    expect(renderToStaticMarkup(<LiveCount count={0} />)).toBe('');
  });

  it('wears the same pulse a live session row does', () => {
    const html = renderToStaticMarkup(<LiveCount count={3} />);
    expect(html).toContain('animate-pulse bg-ok motion-reduce:animate-none');
    expect(html).toContain('3 live');
  });
});
