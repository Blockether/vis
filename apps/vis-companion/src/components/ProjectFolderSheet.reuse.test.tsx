// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Capacitor } from '@capacitor/core';
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
});

// User request: the place and the search were two bands of the same weight, and the
// search did not say which folder it looked in. One field now holds both: the crumbs
// lead it, and the text after them filters that folder.
describe('the location field', () => {
  const field = () => screen.getByLabelText('Filter folders or type a path');

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

// User report: the add-project sheet opened with no focus in its field, and each
// press on a chip or a folder took the focus away from it.
describe('the field keeps the focus', () => {
  it('has it when the sheet opens and after a chip, a folder and a crumb', async () => {
    sheet();
    const field = screen.getByLabelText('Filter folders or type a path');
    expect(field).toHaveFocus();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.click(screen.getByRole('button', { name: 'Hide empty' }));
    expect(field).toHaveFocus();
    await userEvent.click(screen.getByRole('button', { name: /^vis/ }));
    expect(await screen.findByRole('button', { name: /^src/ })).toBeInTheDocument();
    expect(field).toHaveFocus();
    await userEvent.click(screen.getByRole('button', { name: 'code' }));
    expect(await screen.findByRole('button', { name: /tools/ })).toBeInTheDocument();
    expect(field).toHaveFocus();
  });
});

// User request: on a phone the focus raises the virtual keyboard, so the sheet opens
// without it in the Android and iOS apps, also under a mouse, and under a touch pointer.
describe('the field stays without focus on a phone', () => {
  it('in the Android and iOS apps', async () => {
    const native = vi.spyOn(Capacitor, 'isNativePlatform').mockReturnValue(true);
    try {
      sheet();
      await screen.findByRole('button', { name: /tools/ });
      expect(screen.getByLabelText('Filter folders or type a path')).not.toHaveFocus();
    } finally {
      native.mockRestore();
    }
  });

  it('under a touch pointer', async () => {
    const media = vi
      .spyOn(window, 'matchMedia')
      .mockImplementation((query: string) => ({ matches: query === '(pointer: coarse)' }) as MediaQueryList);
    try {
      sheet();
      await screen.findByRole('button', { name: /tools/ });
      expect(screen.getByLabelText('Filter folders or type a path')).not.toHaveFocus();
    } finally {
      media.mockRestore();
    }
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

  it('never offers the folder on screen as a place', () => {
    expect(projectPlaces(new Set([VIS, DEMO]), CODE)).toEqual([]);
    expect(projectPlaces(new Set([VIS, DEMO]), HOME)).toEqual([[CODE, 2]]);
  });
});

// User report (paraphrased: the folder creation still annoys). A band cell opened a
// second field, faded the rows, turned into an undo and changed the commit. The name
// is now the field's own text, and one button in the field creates it.
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

  it('creates a name that is part of a folder name, and the folder stays listed', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), 'to');
    expect(screen.getByRole('button', { name: /^tools/ })).toBeInTheDocument();
    await userEvent.click(screen.getByRole('button', { name: 'Create project ~/code/to' }));
    expect(client.createDirectory).toHaveBeenCalledWith(CODE, 'to');
  });

  // User report: a `New folder…` row stood last in the list and only moved the caret.
  it('lists folders only: neither an empty field nor a new name adds a row', async () => {
    sheet();
    const list = (await screen.findByRole('button', { name: /^tools/ })).parentElement!;
    const rows = () => list.querySelectorAll('button').length;

    // vis, demo, tools and notes; `scratch` is empty and hidden.
    expect(rows()).toBe(4);
    await userEvent.type(field(), 'o');
    expect(screen.getByRole('button', { name: 'Create project ~/code/o' })).toBeInTheDocument();
    expect(rows()).toBe(3);
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

  it('creates the leaf of a typed path in the folder that path lists', async () => {
    const { client } = sheet();
    await screen.findByRole('button', { name: /tools/ });

    await userEvent.type(field(), '~/code/fresh');
    const create = await screen.findByRole('button', { name: 'Create project ~/code/fresh' });
    await userEvent.click(create);
    expect(client.createDirectory).toHaveBeenCalledWith(CODE, 'fresh');
  });
});
