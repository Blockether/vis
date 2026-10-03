// @vitest-environment jsdom
import type { ReactElement } from 'react';
import { describe, expect, it, vi } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { AnchoredPanel, MenuItem } from './Menu';
import { BandButton, IconButton } from './ui';
import { ProjectFolderSheet, startingDir } from './ProjectFolderSheet';
import type { GatewayClient } from '../lib/gateway';
import type { BrowseEntry, BrowseListing } from '../lib/types';

const HOME = '/Users/me';
const CODE = `${HOME}/code`;
const VIS = `${CODE}/vis`;
const DEMO = `${CODE}/demo`;

function entry(name: string, at: string, extra: Partial<BrowseEntry> = {}): BrowseEntry {
  return {
    name,
    path: `${at}/${name}`,
    entry_count: 4,
    is_repo: false,
    ...extra,
  };
}

const TREE: Record<string, BrowseEntry[]> = {
  [HOME]: [entry('code', HOME)],
  [CODE]: [
    entry('vis', CODE, { is_repo: true, branch: 'main' }),
    entry('demo', CODE, { is_repo: true, branch: 'main' }),
    entry('tools', CODE, { is_repo: true, branch: 'trunk' }),
    entry('notes', CODE, { entry_count: 1 }),
  ],
  [VIS]: [entry('src', VIS)],
};

/** The gateway's own answer for a path, `~` included, so the sheet can browse. */
function listing(asked: string): BrowseListing {
  const trimmed = asked.replace(/\/+$/, '') || '/';
  const path = trimmed.startsWith('~') ? `${HOME}${trimmed.slice(1)}` : trimmed;
  const cut = path.lastIndexOf('/');
  return {
    path,
    parent: cut > 0 ? path.slice(0, cut) : path === '/' ? null : '/',
    home: HOME,
    is_truncated: false,
    entries: TREE[path] ?? [],
  };
}

function machine() {
  return {
    browse: vi.fn(async (path?: string) => listing(path ?? '~')),
    createDirectory: vi.fn(async (at: string, name: string) => ({
      path: `${at}/${name}`,
    })),
  };
}

type SheetProps = Parameters<typeof ProjectFolderSheet>[0];

function sheet(props: Partial<SheetProps> = {}) {
  const client = machine();
  const onCancel = vi.fn();
  const onChoose = vi.fn();
  const view = render(
    <ProjectFolderSheet
      label="tower"
      mode="add"
      at={null}
      client={client as unknown as GatewayClient}
      startAt={VIS}
      knownRoots={new Set([VIS, DEMO])}
      onCancel={onCancel}
      onChoose={onChoose}
      {...props}
    />,
  );
  const panel = () => screen.getByRole('dialog', { name: 'New project on tower' });
  return { view, client, onCancel, onChoose, panel };
}

const classesOf = (element: Element) => new Set(element.classList);

/** Every utility a node wears, order-free — a class string may hold blanks. */
const paint = (element: Element) => [...element.classList].sort();

/**
 * What a shipped control PAINTS, read off a rendering of that control on its own.
 * A screen reuses `ui.tsx`/`Menu.tsx` when its own node wears exactly this skin —
 * a class string copied into a screen drifts the first time the owner changes.
 */
function skinOf(element: ReactElement, select: string): string[] {
  const view = render(element);
  // Its OWN container first: a reference control is rendered while the sheet is
  // already on screen, and `document.body` holds both.
  const node = view.container.querySelector(select) ?? document.querySelector(select);
  if (!node) throw new Error(`nothing matched ${select}`);
  const classes = paint(node);
  view.unmount();
  return classes;
}

// Regression, user report ("the manage projects looks absolutely awful on the desktop
// and iphone too — buttons are not canonicalized, components are not reused"). The
// sheet painted every box it stood in by hand, each a near-copy of something `Menu`
// already shipped, and each drifted: its own panel (the one surface in the app with no
// way out but the scrim, whose `Use project` landed 30px below a 1440x900 window), its
// own heading band, its own 44px row that never shrank under a mouse, its own badge,
// own borderless pencil, and `quiet` for the secondary verb where every other
// task heading uses `secondary`.
describe('ProjectFolderSheet paints no box of its own', () => {
  it('is the app’s one anchored panel, not a dialog of its own', async () => {
    const { panel } = sheet();
    await screen.findByRole('button', { name: /^vis/ });

    // Byte for byte the canonical browsing panel: the sheet adds no box, no height cap
    // of its own (`max-h-[80vh]` at the call site stopped it 169px short of the glass)
    // and no second way of arriving.
    expect(paint(panel())).toEqual(
      skinOf(
        <AnchoredPanel size="browse" role="dialog" label="reference" at={null} onDismiss={() => {}}>
          <p>x</p>
        </AnchoredPanel>,
        '[aria-label="reference"]',
      ),
    );
  });

  // Regression, user report: adding a project offered no way out of its own — the band
  // was a BACK arrow into the project inventory. Adding closes, like every other panel:
  // its band carries the close, and the scrim dismisses it.
  it("names itself with the menu's own band and its one way out", async () => {
    const { onCancel } = sheet();
    // The band says what the rows act on, and never the machine's address; the
    // machine survives in the way out, which a screen reader still reaches.
    expect(await screen.findByText('New project')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /back/i })).toBeNull();

    await userEvent.click(screen.getByRole('button', { name: 'Close new project on tower' }));
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  // A project moves from its own menu: the same browser, saying what its commit does.
  it('moves a project with the same browser and its own words', async () => {
    const { onChoose } = sheet({ mode: 'move', knownRoots: new Set([DEMO]) });
    await screen.findByRole('button', { name: /^vis/ });

    expect(screen.getByRole('dialog', { name: 'Change folder on tower' })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Use project' })).toBeNull();
    await userEvent.click(screen.getByRole('button', { name: 'Move here' }));
    expect(onChoose).toHaveBeenCalledWith(CODE);
  });

  it('lists folders with the shipped menu row and its badge', async () => {
    sheet();
    const row = await screen.findByRole('button', { name: /tools/ });

    expect(paint(row)).toEqual(
      skinOf(<MenuItem title="reference" hint="4 entries" onSelect={() => {}} />, 'button'),
    );
    // The badge is the row's own word, not a chip this file invented: the folder Vis
    // already runs sessions in says so, a bare checkout only says `git`.
    expect(screen.getByRole('button', { name: /^demo/ }).textContent).toContain('project');
    expect(row.textContent).toContain('git');
    expect(screen.getByRole('button', { name: /notes/ }).textContent).not.toContain('git');
  });

  it('uses the shipped icon button for the pencil, ink at rest', async () => {
    sheet();
    const pencil = await screen.findByRole('button', { name: 'Type a path' });

    expect(paint(pencil)).toEqual(
      skinOf(
        <IconButton variant="quiet" label="reference" onClick={() => {}}>
          <span />
        </IconButton>,
        '[aria-label="reference"]',
      ),
    );
  });

  // Regression, user report (paraphrased: the sheet's colours look wrong): the two
  // verbs were `Button`s, sheets of paper standing on the dark title band, and
  // `secondary` carries `text-white` — which in this app is the PAGE's ink (#262626)
  // — so "New folder" was dark on dark beside an amber slab 40px under the panel's
  // own amber rule: one control unreadable, one charging the accent twice.
  it("commits with the band's own cells, not with paper parked on the band", async () => {
    sheet();
    await screen.findByRole('button', { name: /^vis/ });
    const use = screen.getByRole('button', { name: 'Use project' });
    const make = screen.getByRole('button', { name: 'New folder' });

    expect(paint(make)).toEqual(
      skinOf(
        <BandButton label="reference" onClick={() => {}}>
          <span aria-hidden="true" />
        </BandButton>,
        'button',
      ),
    );
    expect(paint(use)).toEqual(
      skinOf(
        <BandButton isPrimary label="reference" onClick={() => {}}>
          <span aria-hidden="true" />
        </BandButton>,
        'button',
      ),
    );
    // Both verbs are marks in the band's own ink: one drawing each, no word riding
    // the title, and neither the page's ink, a box nor a slab of paper on the band.
    expect(make.textContent).toBe('');
    expect(use.textContent).toBe('');
    expect(make.querySelectorAll('svg')).toHaveLength(1);
    expect(use.querySelectorAll('svg')).toHaveLength(1);
    expect(classesOf(use).has('bg-accent')).toBe(false);
    expect(classesOf(make).has('text-white')).toBe(false);
    expect(classesOf(make).has('border-edge-strong')).toBe(false);

    // The band ends in one run of cells: the two verbs, then the app's one way out.
    const band = use.closest('header')!;
    expect(make.parentElement).toBe(band);
    expect(use.parentElement).toBe(band);
    const cells = [...band.children];
    expect(cells.indexOf(use)).toBeGreaterThan(cells.indexOf(make));
    expect(cells.at(-1)).toBe(screen.getByRole('button', { name: /^Close new project/ }));
  });

  it('does not caption the heading actions with the path the crumbs already say', async () => {
    sheet();
    const use = await screen.findByRole('button', { name: 'Use project' });

    // The path bar already names the folder, and each verb is a mark that says what
    // will happen to it through its own name, so the band carries one title only.
    expect(use.closest('header')?.textContent).toBe('New project');
  });

  it('keeps the commit verbs in the heading instead of scrolling them away', async () => {
    sheet();
    const row = await screen.findByRole('button', { name: /tools/ });
    const use = screen.getByRole('button', { name: 'Use project' });

    const scroller = row.closest('.overflow-y-auto');
    expect(scroller).toBeVisible();
    expect(scroller!.contains(use)).toBe(false);
    expect(use.closest('header')).toBeInTheDocument();
  });

  it('keeps a crumb a real target rather than 14px of bare text', async () => {
    const { client } = sheet();
    const home = await screen.findByRole('button', { name: '~' });

    expect(home.tagName).toBe('BUTTON');
    // The crumb you are standing on is not a way to anywhere.
    expect(screen.getByRole('button', { name: 'code' })).toBeDisabled();
    expect(screen.getByRole('button', { name: 'code' })).toHaveAttribute(
      'aria-current',
      'location',
    );

    await userEvent.click(home);
    await waitFor(() => expect(client.browse).toHaveBeenCalledWith(HOME, expect.anything()));
    expect(await screen.findByRole('button', { name: /code/ })).toBeInTheDocument();
  });

  it('takes the rows out of play with inert, never with aria-hidden alone', async () => {
    sheet();
    const rows = (await screen.findByRole('button', { name: /tools/ })).parentElement;

    expect(rows).not.toHaveAttribute('inert');
    await userEvent.click(screen.getByRole('button', { name: 'New folder' }));

    // A container merely hidden from the a11y tree still hands its buttons to Tab.
    expect(rows).toHaveAttribute('inert');
    expect(rows).not.toHaveAttribute('aria-hidden');
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

  it('is what the sheet opens on', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    expect(client.browse.mock.calls[0][0]).toBe(CODE);
    expect(screen.queryByRole('button', { name: /src/ })).toBeNull();
  });

  it('marks known projects without implying that the gateway has one current project', async () => {
    sheet();
    const knownProject = await screen.findByRole('button', { name: /vis\// });
    expect(knownProject.textContent).toContain('project');
    expect(knownProject.textContent).not.toContain('current');
  });
});

// Regression, user report ("Use project + New folder should be disabled and say it's
// already a project"). Aiming at a folder this machine ALREADY runs sessions in left
// both verbs live: "Use project" re-added an existing root and said nothing.
describe('a folder that is already a project offers no verb', () => {
  it('takes both heading buttons down and says why', async () => {
    sheet({ knownRoots: new Set([CODE]) });
    // The aim is the folder being LISTED, so the verbs only answer once it lands.
    await screen.findByRole('button', { name: /^vis/ });
    const use = screen.getByRole('button', { name: 'Use project' });

    expect(use).toBeDisabled();
    expect(screen.getByRole('button', { name: 'New folder' })).toBeDisabled();
    expect(screen.getByText('It’s already a project')).toBeInTheDocument();
  });

  it('leaves the verbs live for a folder the machine does not run yet', async () => {
    const { onChoose } = sheet();
    // The aim is the folder being LISTED, so the verbs wake with the listing.
    await screen.findByRole('button', { name: /^vis/ });
    const use = screen.getByRole('button', { name: 'Use project' });

    expect(use).toBeEnabled();
    expect(screen.queryByText('It’s already a project')).toBeNull();
    await userEvent.click(use);
    expect(onChoose).toHaveBeenCalledWith(CODE);
  });
});

// Regression, user report ("clicking the edit button in choosing the project flickers
// and reflows"): the pencil handed the input `~/vis`, whose DIR is the parent — so the
// toggle re-listed one level up, filtered it to `vis*`, and the rows jumped; and even
// the same folder under its other spelling triggered a fresh fetch.
describe('taking the pencil does not move the list', () => {
  it('hands over a path that names the folder itself, and re-lists nothing', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });
    const reads = client.browse.mock.calls.length;

    await userEvent.click(screen.getByRole('button', { name: 'Type a path' }));
    expect(screen.getByLabelText('Path on this machine')).toHaveValue('~/code/');

    // Past the typing debounce: the folder on screen is the folder asked for, so the
    // gateway is not asked for it again and no row moves.
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(client.browse.mock.calls.length).toBe(reads);
    expect(screen.getByRole('button', { name: /tools/ })).toBeInTheDocument();
  });

  it('narrows the same listing as the leaf is typed', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });
    await userEvent.click(screen.getByRole('button', { name: 'Type a path' }));
    const reads = client.browse.mock.calls.length;

    await userEvent.type(screen.getByLabelText('Path on this machine'), 'to');
    await waitFor(() => expect(screen.queryByRole('button', { name: /notes/ })).toBeNull());
    expect(screen.getByRole('button', { name: /tools/ })).toBeInTheDocument();
    expect(client.browse.mock.calls.length).toBe(reads);
  });
});

// Regression, user report ("when I click it the height a little bit jumps a little"):
// the crumb bar and the typed-path bar were both `min-h-11`, but the crumb row settled
// at 45px and the input row at 44, so the pencil toggle shifted everything below by 1px.
describe('the path band', () => {
  const heights = (band: Element) =>
    [...band.classList].filter((token) => /h-\d|min-h/.test(token)).sort();

  it('is one fixed-height band under both of its spellings', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });
    const crumbs = screen.getByRole('button', { name: '~' }).closest('div.bg-panel-2')!;

    expect(heights(crumbs)).toEqual(['h-11', 'mouse:h-9']);

    await userEvent.click(screen.getByRole('button', { name: 'Type a path' }));
    const typed = screen.getByLabelText('Path on this machine').closest('div.bg-panel-2')!;

    // Same paper, same height: the pencil toggles what is INSIDE the band.
    expect(heights(typed)).toEqual(heights(crumbs));
  });

  it('gives the new-folder line that same band', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });
    await userEvent.click(screen.getByRole('button', { name: 'New folder' }));
    const naming = screen.getByLabelText('New folder name').closest('div.h-11');

    expect(naming).toBeVisible();
    expect(heights(naming!)).toEqual(['h-11', 'mouse:h-9']);

    // Regression, user report ("we have an icon library, use it"): the line led with a
    // `+` typed into the band. It leads with the MARK of the cell that opened it — one
    // drawn icon, and not one glyph of type anywhere on the line.
    expect(naming!.querySelectorAll('svg')).toHaveLength(1);
    expect(naming!.textContent).toBe('');
  });

  // Regression, user report: while a folder is being named from the start flow's own
  // `New project`, the cell that leaves the LINE wore this app's close stroke — a plus
  // turned 45° — one cell away from the band's ✕ that leaves the PANEL. Two identical
  // crosses side by side meaning different things. Taking the line back is an UNDO.
  it('cancels the new-folder line with its own mark, never a second close', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });
    await userEvent.click(screen.getByRole('button', { name: 'New folder' }));

    const cancel = screen.getByRole('button', { name: 'Cancel' });
    const close = screen.getByRole('button', { name: /^Close new project/ });

    expect(cancel.textContent).toBe('');
    expect(cancel.querySelectorAll('svg')).toHaveLength(1);
    expect(cancel.querySelector('svg')!.innerHTML).not.toBe(close.querySelector('svg')!.innerHTML);
    expect(cancel.querySelector('svg')!.getAttribute('class')).not.toContain('rotate-45');
  });

  // Regression, user report: the two project verbs were docked below the folder list,
  // where a phone could hide them instead of keeping them with the task they commit.
  it('keeps both project verbs in the task heading, never in a footer', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });

    const use = screen.getByRole('button', { name: 'Use project' });
    const folder = screen.getByRole('button', { name: 'New folder' });
    const heading = use.closest('header');
    expect(heading).toBeInTheDocument();
    expect(heading).toBe(folder.closest('header'));
    expect(use.closest('footer')).toBeNull();
  });
});
