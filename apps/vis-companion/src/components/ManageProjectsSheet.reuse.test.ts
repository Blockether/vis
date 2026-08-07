import { describe, expect, it } from 'vitest';
import source from './ManageProjectsSheet.tsx?raw';
import { startingDir } from './ManageProjectsSheet';

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

  it('does not caption the footer with the path the crumbs already say', () => {
    // The footer repeated the destination as a third spelling of it — the crumb bar
    // above already names the folder, and the button says what will happen to it.
    expect(source).not.toContain('homeify(aiming, home)');
    expect(source).not.toContain('const aiming');
  });

  it('docks the commit footer instead of scrolling it away', () => {
    // The list is the only part of this sheet that scrolls; the two verbs stay put.
    expect(source).toContain('min-h-0 flex-1 touch-pan-y overflow-y-auto');
    expect(source).toContain('shrink-0 border-t border-dialog-edge bg-panel-2');
  });

  it('keeps a crumb a real target rather than 14px of bare text', () => {
    expect(source).toContain('min-h-11 truncate px-1');
  });

  it('takes the rows out of play with inert, never with aria-hidden alone', () => {
    expect(source).toContain('inert={folder !== null}');
    expect(source).not.toContain('aria-hidden={folder !== null}');
  });
});

// Regression, user report ("let it start ../ from the current project"). Browsing
// opened INSIDE the machine's current project, so adding the next checkout beside it
// began with a tap on the parent crumb, and the first list you saw was that project's
// own `src/`.
describe('browsing opens one level above the current project', () => {
  it('lists the project’s siblings, not its contents', () => {
    expect(startingDir('/Users/me/code/vis')).toBe('/Users/me/code');
    expect(startingDir('/Users/me/code/vis/')).toBe('/Users/me/code');
  });

  it('stays put where there is no `..`', () => {
    expect(startingDir(null)).toBe(null);
    expect(startingDir('/')).toBe('/');
    expect(startingDir('vis')).toBe('vis');
  });

  it('is what the sheet opens on', () => {
    expect(source).toContain('startingDir(startAt)');
  });

  // ...and the project you came from is named in that listing, so a folder one level
  // up is still recognisable as where you already are.
  it('badges the current project in both lists', () => {
    expect(source).toContain("entry.root === startAt ? 'current'");
    expect(source).toContain("entry.path === startAt");
  });
});

// Regression, user report ("Use project + New folder should be disabled and say it's
// already a project"). Aiming at a folder this machine ALREADY runs sessions in left
// both verbs live: "Use project" re-added an existing root and said nothing.
describe('a folder that is already a project offers no verb', () => {
  it('reads the aim against the machine\u2019s known roots, browsing only', () => {
    expect(source).toContain(
      'const alreadyProject = folder === null && !!target && knownRoots.has(target);',
    );
  });

  it('takes both footer buttons down', () => {
    expect(source).toContain('disabled={saving || !here || alreadyProject}');
    expect(source).toContain(
      "saving || !target || alreadyProject || (folder !== null && !folder.trim())",
    );
  });

  it('says why, on the leading edge of the footer', () => {
    expect(source).toContain(
      '<p className="mr-auto text-meta text-dialog-hint">It\u2019s already a project</p>',
    );
  });
});
