// @vitest-environment jsdom
import type { ReactElement } from 'react';
import { describe, expect, it, vi } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { AnchoredPanel } from './Menu';
import {
  ProjectFolderSheet,
  isFolderName,
  isPathQuery,
  projectPlaces,
  startingDir,
} from './ProjectFolderSheet';
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
    entry('scratch', CODE, { entry_count: 0 }),
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
    // of its own and no second way of arriving, so it never slides up as a bottom sheet.
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
    expect(screen.queryByRole('button', { name: /^Use project/ })).toBeNull();
    await userEvent.click(screen.getByRole('button', { name: /^Move here/ }));
    expect(onChoose).toHaveBeenCalledWith(CODE);
  });

  it('lists folders with the shipped menu row, its badge and a trailing chevron', async () => {
    sheet();
    const row = await screen.findByRole('button', { name: /tools/ });

    // The badge is the row's own word: the folder Vis already runs sessions in says so,
    // a bare checkout only says `git`.
    // A folder row OPENS a level: its chevron stands on the trailing edge, after the badge.
    const marks = row.querySelectorAll('svg');
    expect(marks).toHaveLength(2);
    expect(row.lastElementChild).toBe(marks[1]);
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
    expect(await screen.findByRole('button', { name: /^code\// })).toBeInTheDocument();
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

});

// Regression, user report ("Use project + New folder should be disabled and say it's
// already a project"). Aiming at a folder this machine ALREADY runs sessions in left
// both verbs live: "Use project" re-added an existing root and said nothing.
describe('a folder that is already a project offers no verb', () => {
  it('takes both verbs down and the commit says why', async () => {
    sheet({ knownRoots: new Set([CODE]) });
    // The aim is the folder being LISTED, so the verbs only answer once it lands.
    await screen.findByRole('button', { name: /^vis/ });

    expect(screen.getByRole('button', { name: 'It’s already a project' })).toBeDisabled();
    expect(screen.queryByRole('button', { name: /^Use project/ })).toBeNull();
    // A folder inside a project is not a new project either.
    await userEvent.type(screen.getByLabelText('Filter folders or type a path'), 'fresh');
    expect(screen.queryByRole('button', { name: /^Create project/ })).toBeNull();
  });

  it('leaves the verbs live for a folder the machine does not run yet', async () => {
    const { onChoose } = sheet();
    // The aim is the folder being LISTED, so the verbs wake with the listing.
    await screen.findByRole('button', { name: /^vis/ });
    const use = screen.getByRole('button', { name: /^Use project/ });

    expect(use).toBeEnabled();
    expect(screen.queryByText('It’s already a project')).toBeNull();
    await userEvent.click(use);
    expect(onChoose).toHaveBeenCalledWith(CODE);
  });
});

// User request: the place and the search were two bands of the same weight, and the
// search did not say which folder it looked in. One field now holds both: the crumbs
// lead it, and the text after them filters that folder.
describe('the location field', () => {
  const field = () => screen.getByLabelText('Filter folders or type a path');

  it('wears the Input frame and focus, and the field itself draws none', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });
    const frame = classesOf(field().parentElement!);

    for (const token of ['border', 'border-edge', 'bg-input', 'focus-within:border-accent']) {
      expect(frame.has(token)).toBe(true);
    }
    expect(classesOf(field()).has('border')).toBe(false);
  });

  it('filters anywhere in the name without asking the gateway again', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });
    const reads = client.browse.mock.calls.length;

    await userEvent.type(field(), 'ot');
    expect(screen.getByRole('button', { name: /^notes/ })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^tools/ })).toBeNull();
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(client.browse.mock.calls.length).toBe(reads);
  });

  it('opens the first match on Enter and goes up on Backspace in an empty field', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'vi{Enter}');
    await waitFor(() => expect(client.browse).toHaveBeenCalledWith(VIS, expect.anything()));
    expect(field()).toHaveValue('');
    expect(await screen.findByRole('button', { name: /^src/ })).toBeInTheDocument();

    await userEvent.type(field(), '{Backspace}');
    expect(await screen.findByRole('button', { name: /tools/ })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'code' })).toHaveAttribute('aria-current', 'location');
  });

  it('reads text that starts like a path as a path', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    expect(isPathQuery('~/code')).toBe(true);
    expect(isPathQuery('/tmp')).toBe(true);
    expect(isPathQuery('vis')).toBe(false);

    await userEvent.type(field(), '~/');
    await waitFor(() => expect(client.browse).toHaveBeenCalledWith('~', expect.anything()));
    // A typed path replaces the crumbs, so the field never reads `~/code/~/`.
    expect(screen.queryByRole('button', { name: 'code', current: 'location' })).toBeNull();
    expect(await screen.findByRole('button', { name: /^code\// })).toBeInTheDocument();
  });
});

describe('the chips and the sections', () => {
  it('uses the shipped chip and hides empty folders by default', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });
    const hideEmpty = screen.getByRole('button', { name: 'Hide empty' });

    expect(hideEmpty).toHaveAttribute('aria-pressed', 'true');
    expect(screen.queryByRole('button', { name: /^scratch/ })).toBeNull();

    await userEvent.click(hideEmpty);
  });

  it('keeps only git checkouts under Git only', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.click(screen.getByRole('button', { name: 'Git only' }));
    expect(screen.getByRole('button', { name: 'Git only' })).toHaveAttribute('aria-pressed', 'true');
    expect(screen.queryByRole('button', { name: /^notes/ })).toBeNull();
    expect(screen.getByRole('button', { name: /^tools/ })).toBeInTheDocument();
  });

  it('lists git repos before other folders, each under its own quiet band', async () => {
    const FOLLOWING = Node.DOCUMENT_POSITION_FOLLOWING;
    sheet();
    await screen.findByRole('button', { name: /tools/ });

    const repos = screen.getByText('Git repos');
    const others = screen.getByText('Other folders');
    expect(repos.compareDocumentPosition(others) & FOLLOWING).toBe(FOLLOWING);
    const tools = screen.getByRole('button', { name: /^tools/ });
    const notes = screen.getByRole('button', { name: /^notes/ });
    expect(repos.compareDocumentPosition(tools) & FOLLOWING).toBe(FOLLOWING);
    expect(others.compareDocumentPosition(notes) & FOLLOWING).toBe(FOLLOWING);
    expect(tools.compareDocumentPosition(others) & FOLLOWING).toBe(FOLLOWING);
  });

  it('offers the folders that hold projects as places to jump to', async () => {
    const { client } = sheet({ startAt: CODE });
    const place = await screen.findByRole('button', { name: /^~\/code\// });

    await userEvent.click(place);
    await waitFor(() => expect(client.browse).toHaveBeenCalledWith(CODE, expect.anything()));
  });

  it('never offers the folder on screen as a place', () => {
    expect(projectPlaces(new Set([VIS, DEMO]), CODE)).toEqual([]);
    expect(projectPlaces(new Set([VIS, DEMO]), HOME)).toEqual([[CODE, 2]]);
  });
});

describe('the commit', () => {

  it('stands in the panel footer, outside the scrolling list', async () => {
    const { panel } = sheet();
    const row = await screen.findByRole('button', { name: /tools/ });
    const use = screen.getByRole('button', { name: /^Use project/ });

    const scroller = row.closest('.overflow-y-auto')!;
    expect(scroller.contains(use)).toBe(false);
    const footer = use.closest('footer')!;
    expect(footer.parentElement).toBe(panel());
    expect(panel().lastElementChild).toBe(footer);
  });

});

// User report (paraphrased: the folder creation still annoys). A band cell opened a
// second field, faded the rows, turned into an undo and changed the commit. The name
// is now the field's own text, and one row creates it.
describe('creating a folder', () => {
  const field = () => screen.getByLabelText('Filter folders or type a path');

  it('accepts one plain folder name only', () => {
    expect(isFolderName('fresh')).toBe(true);
    expect(isFolderName('')).toBe(false);
    expect(isFolderName('a/b')).toBe(false);
    expect(isFolderName('.')).toBe(false);
    expect(isFolderName('..')).toBe(false);
    expect(isFolderName('x'.repeat(65))).toBe(false);
  });

  it('creates the typed name and adds it as the project in one tap', async () => {
    const { client, onChoose } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'fresh');
    const create = screen.getByRole('button', { name: 'Create project ~/code/fresh' });
    // The commit in the footer does not change while a name is typed.

    await userEvent.click(create);
    expect(client.createDirectory).toHaveBeenCalledWith(CODE, 'fresh');
    expect(onChoose).toHaveBeenCalledWith(`${CODE}/fresh`);
  });

  it('lists the folders that match before the row that creates', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'to');
    const tools = screen.getByRole('button', { name: /^tools/ });
    const create = screen.getByRole('button', { name: 'Create project ~/code/to' });
    expect(tools.compareDocumentPosition(create) & Node.DOCUMENT_POSITION_FOLLOWING).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING,
    );
  });

  it('never offers a name that a folder here already has, hidden ones included', async () => {
    sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'Tools');
    expect(screen.queryByRole('button', { name: /^Create project/ })).toBeNull();
    // `scratch` is empty, so the default chip hides it, and it still exists.
    await userEvent.clear(field());
    await userEvent.type(field(), 'scratch');
    expect(screen.queryByRole('button', { name: /^Create project/ })).toBeNull();
  });

  it('creates on Enter when no folder matches', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'fresh{Enter}');
    expect(client.createDirectory).toHaveBeenCalledWith(CODE, 'fresh');
  });

  it('creates the leaf of a typed path in the folder that path lists', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), '~/code/fresh');
    const create = await screen.findByRole('button', { name: 'Create project ~/code/fresh' });
    await userEvent.click(create);
    expect(client.createDirectory).toHaveBeenCalledWith(CODE, 'fresh');
  });

  it('uses its own words when it moves a project', async () => {
    sheet({ mode: 'move', knownRoots: new Set([DEMO]) });
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'fresh');
    expect(screen.getByRole('button', { name: 'Create and move ~/code/fresh' })).toBeInTheDocument();
  });
});
