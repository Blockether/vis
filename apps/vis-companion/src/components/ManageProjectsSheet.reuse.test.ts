import { describe, expect, it } from 'vitest';
import source from './ManageProjectsSheet.tsx?raw';

// Regression, user report ("the fucking manage projects looks absolutely fucking awful
// on the desktop and iphone too — buttons are not canonicalized, components are not
// reused"). The sheet painted every box it stood in by hand, each a near-copy of
// something `Menu` already shipped, and each drifted:
//
//   * its own panel — the one surface in the app with NO way out but the scrim, and on
//     a 1440x900 desktop it rendered from y=300 with a 630px budget, putting `Use
//     project` 30px below the window with nothing able to scroll it back;
//   * its own `BAND`/`QUIET_BAND`, a second spelling of `MenuHeading`;
//   * its own `ROW`, a second spelling of `MenuItem` — and the one list in the app
//     that stayed at 44px rows under a mouse;
//   * its own `CHIP`, a second spelling of `MenuItem`'s badge;
//   * its own 44px pencil, borderless, answering neither hover nor focus;
//   * `quiet` for the secondary verb where every other dialog footer uses `ghost`.

describe('ManageProjectsSheet paints no box of its own', () => {
  // It was an anchored popover hanging off the machine's `⋯` — which is what forced
  // all the placement maths, and what made a surface that MANAGES things feel like a
  // dropdown. It is the app's normalized dialog now: `Modal` + `DialogFrame`, the
  // same header and the same way out as every other dialog on the screen.
  it('is the app’s normalized dialog, not a panel of its own', () => {
    expect(source).toContain('<Modal size="lg"');
    expect(source).toContain('<DialogFrame');
    expect(source).not.toContain('<AnchoredPanel');
    expect(source).not.toContain('createPortal');
    expect(source).not.toContain('sm:w-96');
    expect(source).not.toContain("'--menu-top'");
    // Being centred, it needs no anchor at all — the placement props are gone.
    expect(source).not.toContain('MenuPosition');
  });

  it('names itself with the shipped header and offers the shipped way out', () => {
    expect(source).toContain('title="Manage projects"');
    expect(source).toContain('subtitle={label}');
    expect(source).toContain('onClose={onCancel}');
    expect(source).not.toContain('const BAND');
    expect(source).not.toContain('const QUIET_BAND');
  });

  it('lists folders with the shipped menu row and its badge', () => {
    expect(source).toContain('<MenuItem');
    expect(source).not.toContain('const ROW');
    expect(source).not.toContain('const CHIP');
  });

  it('uses the shipped icon button for the pencil, ink at rest', () => {
    expect(source).toContain('<IconButton');
    expect(source).toContain('variant="quiet"');
    expect(source).not.toContain('inline-flex size-11 shrink-0');
  });

  it('commits with the footer every other dialog in the app commits with', () => {
    expect(source).toContain('justify-end gap-2');
    expect(source).toContain('variant="ghost"');
    expect(source).not.toContain('justify-between gap-2');
  });

  it('says the path in ONE language, the same one the crumbs speak', () => {
    // The footer printed the raw absolute path under crumbs reading `~ › vis`.
    expect(source).toContain('homeify(aiming, home)');
  });

  it('keeps a crumb a real target rather than 14px of bare text', () => {
    expect(source).toContain('min-h-11 truncate px-1');
  });

  it('takes the rows out of play with inert, never with aria-hidden alone', () => {
    expect(source).toContain('inert={folder !== null}');
    expect(source).not.toContain('aria-hidden={folder !== null}');
  });
});
