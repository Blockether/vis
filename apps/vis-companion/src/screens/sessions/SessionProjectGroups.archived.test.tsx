// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

import type { SessionRowCommands } from '../../components/SessionList';
import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { ArchiveView, BandWindow, GatewayConn, Session, SessionGroup } from '../../lib/types';
import { ProjectGroup, type ProjectCreation } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const ROOT = STORY_NEWER_PROJECT.root;
const MENU = `Groups in ${ROOT}`;
const SESSIONS_MENU = `Sessions in ${ROOT}`;

/** The project reads the wall of bands a page at a time, standing on its first page. */
const BANDS: BandWindow = { limit: 10, offset: 0 };

/** Still active, and it holds one active session and one a reader archived inside it. */
const WALLET: SessionGroup = {
  id: 'group-wallet',
  project_id: STORY_NEWER_PROJECT.projectId,
  name: 'Wallet work',
  color: 'blue',
  position: 0,
  session_count: 2,
};

/** Archived whole, with its one session: a band the project only paints while revealing. */
const RECEIPTS: SessionGroup = {
  id: 'group-receipts',
  project_id: STORY_NEWER_PROJECT.projectId,
  name: 'Receipts',
  color: 'amber',
  position: 1,
  session_count: 1,
  archived_at: 1730000000,
};

const [FIRST, SECOND, THIRD, FOURTH] = STORY_NEWER_PROJECT.rows;
const ACTIVE: Session = { ...FIRST, group_id: WALLET.id };
/** Archived inside a group that is STILL ACTIVE: the group's own business, not this list's. */
const AWAY_IN_ACTIVE: Session = { ...SECOND, group_id: WALLET.id, archived_at: 1730000100 };
const AWAY_FILED: Session = { ...THIRD, group_id: RECEIPTS.id, archived_at: 1730000200 };
const AWAY_LOOSE: Session = { ...FOURTH, archived_at: 1730000300 };

type Page = {
  rows: Session[];
  total: number;
  awaiting: Session[];
  grouped: Session[];
  nextCursor: string;
};

/** What `?archived=exclude` answers: the live list, with its one filed session beside it. */
const ACTIVE_PAGE: Page = {
  rows: [],
  total: 0,
  awaiting: [],
  grouped: [ACTIVE],
  nextCursor: '',
};

/**
 * What `?archived=only&grouped=aside` answers: the loose archived rows in the window, every
 * filed archived row beside it — including the one filed under a group that is still active.
 */
const ARCHIVE_PAGE: Page = {
  rows: [AWAY_LOOSE],
  total: 1,
  awaiting: [],
  grouped: [AWAY_FILED, AWAY_IN_ACTIVE],
  nextCursor: '',
};

const EMPTY_PAGE: Page = { rows: [], total: 0, awaiting: [], grouped: [], nextCursor: '' };

type Machine = Record<string, unknown>;

/**
 * A machine that answers each view with its own page, and holds NOTHING of the archive:
 * this device has only ever read the active list, so a reveal has to go and ask.
 */
function machine(archive: Page = ARCHIVE_PAGE, archivedBands: SessionGroup[] = [RECEIPTS]) {
  return {
    base: 'https://story.example.com',
    heldProjectPage: (
      _root: string,
      _limit: number,
      _after: string,
      _pins: Map<string, string>,
      view: ArchiveView = 'exclude',
    ) => (view === 'exclude' ? ACTIVE_PAGE : null),
    listProjectPage: vi.fn(
      async (
        _root: string,
        _limit: number,
        _after: string,
        _pins: Map<string, string>,
        _signal?: AbortSignal,
        _persistHead?: boolean,
        view: ArchiveView = 'exclude',
      ) => (view === 'only' ? archive : ACTIVE_PAGE),
    ),
    isSessionDeleted: () => false,
    // THE WALL ANSWERS FOR ITSELF: one page of bands carries the whole wall's own tally, and
    // that is what the reveal counts beside the rows it painted.
    listSessionGroups: vi.fn(
      async (
        _root: string,
        _signal?: AbortSignal,
        view: ArchiveView = 'exclude',
        _bands?: BandWindow,
      ) => {
        const groups = view === 'only' ? archivedBands : [WALLET];
        return {
          project_id: STORY_NEWER_PROJECT.projectId,
          groups,
          total: groups.length,
          session_total: groups.reduce((sum, group) => sum + group.session_count, 0),
          limit: BANDS.limit,
          offset: BANDS.offset,
          has_more: false,
        };
      },
    ),
    assignSessionGroup: vi.fn(async () => ACTIVE),
  };
}

/** The gateway changes a group's view without stamping the sessions on its shelf. */
function archivableMachine() {
  const client = machine(EMPTY_PAGE, []);
  let archived = false;
  return {
    ...client,
    updateSessionGroup: vi.fn(async (_gid: string, fields: { archived: boolean }) => {
      archived = fields.archived;
      return { ...WALLET, archived_at: archived ? 1730000400 : null };
    }),
    listProjectPage: vi.fn(async (
      _root: string,
      _limit: number,
      _after: string,
      _pins: Map<string, string>,
      _signal?: AbortSignal,
      _persistHead?: boolean,
      view: ArchiveView = 'exclude',
    ) => (view === 'only'
      ? { ...EMPTY_PAGE, grouped: archived ? [ACTIVE] : [] }
      : archived ? EMPTY_PAGE : ACTIVE_PAGE)),
    listSessionGroups: vi.fn(async (
      _root: string,
      _signal?: AbortSignal,
      view: ArchiveView = 'exclude',
    ) => {
      const groups = (view === 'only') === archived
        ? [{ ...WALLET, session_count: 1, archived_at: archived ? 1730000400 : null }]
        : [];
      return {
        project_id: STORY_NEWER_PROJECT.projectId,
        groups,
        total: groups.length,
        session_total: groups.length,
        limit: BANDS.limit,
        offset: BANDS.offset,
        has_more: false,
      };
    }),
  };
}

function mount(
  client: Machine = machine(),
  held: Session[] = [ACTIVE],
  archive?: SessionRowCommands['archive'],
  needle = '',
) {
  const creation: ProjectCreation = { state: null, start: vi.fn(async () => {}) };
  const view = render(
    <ProjectGroup
      group={{
        root: ROOT,
        label: STORY_NEWER_PROJECT.name,
        projectId: STORY_NEWER_PROJECT.projectId,
        tally: { count: held.length, live: 0, awaiting: 0, unread: 0 },
        sessions: held,
      }}
      machine={{ conn, sessions: held }}
      context={{
        getClient: () => client as unknown as GatewayClient,
        drafts: {},
        matches: null,
        needle,
        openRow: null,
        actions: {
          commands: {
            open: vi.fn(),
            rename: vi.fn(async () => {}),
            requestDelete: vi.fn(),
            toggleStar: vi.fn(),
            archive,
          },
          deletion: {
            target: null,
            isBusy: false,
            error: null,
            confirm: vi.fn(),
            cancel: vi.fn(),
          },
        },
      }}
      reading={{
        pageSize: 10,
        epoch: null,
        admitted: new Set<string>(),
        isVisible: true,
        pendingByRoot: new Map(),
        acceptUpdates: vi.fn(),
      }}
      creation={creation}
      initiallyOpen
    />,
  );
  return { client, user: userEvent.setup(), unmount: () => view.unmount() };
}

/** The band and the rows filed under it, as one block inside the project's list. */
async function band(name: string): Promise<HTMLElement> {
  const disclosure = await screen.findByRole('button', { name: `Collapse ${name}` });
  return disclosure.closest('div')!.parentElement as HTMLElement;
}

/** Every session painted here, in the order the project paints them. */
function painted(scope: HTMLElement = document.body): (string | null)[] {
  return Array.from(scope.querySelectorAll('[data-session-id]')).map((slab) =>
    slab.getAttribute('data-session-id'),
  );
}

/** Open the menu for one set and choose its archive verb. */
async function press(
  user: ReturnType<typeof userEvent.setup>,
  verb: string,
  set: 'groups' | 'sessions' = 'groups',
): Promise<void> {
  const label = set === 'groups' ? MENU : SESSIONS_MENU;
  await user.click(await screen.findByRole('button', { name: `Actions for ${set} in ${ROOT}` }));
  await user.click(within(screen.getByRole('dialog', { name: label })).getByText(verb));
}

async function showBoth(user: ReturnType<typeof userEvent.setup>): Promise<void> {
  await press(user, 'Show archived groups');
  await press(user, 'Show archived sessions', 'sessions');
}

describe('a project shows the sessions it archived', () => {
  beforeEach(() => {
    localStorage.clear();
  });

  it('keeps archived groups and archived sessions independent', async () => {
    const { client, user } = mount();
    await band('Wallet work');

    await press(user, 'Show archived groups');
    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
    expect(painted()).toEqual([AWAY_FILED.id]);
    expect(client.listSessionGroups).toHaveBeenLastCalledWith(
      ROOT,
      expect.any(AbortSignal),
      'only',
      BANDS,
    );

    await press(user, 'Show archived sessions', 'sessions');
    await waitFor(() => expect(painted()).toEqual([AWAY_FILED.id, AWAY_LOOSE.id]));

    await press(user, 'Hide archived groups');
    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);
    await waitFor(() => expect(painted()).toEqual([ACTIVE.id, AWAY_LOOSE.id]));
  });

  it('remembers an archived group view without archiving loose sessions', async () => {
    const first = mount();
    await band('Wallet work');
    await press(first.user, 'Show archived groups');
    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
    first.unmount();

    const again = mount();
    await waitFor(() => expect(painted()).toEqual([AWAY_FILED.id]));
    expect(screen.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull();
    expect(again.client.listProjectPage).toHaveBeenCalledWith(
      ROOT,
      10,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      true,
      'exclude',
      BANDS,
    );
    expect(again.client.listProjectPage).toHaveBeenCalledWith(
      ROOT,
      1,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      false,
      'only',
      BANDS,
    );
  });

  it('searches across both archives without changing either saved view', async () => {
    const first = mount();
    await band('Wallet work');
    await press(first.user, 'Show archived groups');
    await band('Receipts');
    first.unmount();

    const query = mount(machine(), [ACTIVE, AWAY_FILED, AWAY_LOOSE], undefined, 'match');
    await waitFor(() =>
      expect(painted()).toEqual([ACTIVE.id, AWAY_FILED.id, AWAY_LOOSE.id]),
    );
    expect(query.client.listProjectPage).not.toHaveBeenCalled();
    query.unmount();

    mount();
    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
    expect(painted()).toEqual([AWAY_FILED.id]);
  });

  it('shows an empty archived session set beside active groups', async () => {
    const { client, user } = mount(machine(EMPTY_PAGE));
    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);
    await press(user, 'Show archived sessions', 'sessions');

    await screen.findByText('No archived sessions in this project.');
    await waitFor(() => expect(painted()).toEqual([ACTIVE.id]));
    expect(screen.queryByText('No archived groups in this project.')).toBeNull();
    expect(client.listProjectPage).toHaveBeenCalledWith(
      ROOT,
      1,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      false,
      'exclude',
      BANDS,
    );
  });

  it('paints the archived bands and the archived loose rows, and nothing live', async () => {
    const { client, user } = mount();
    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);

    await showBoth(user);

    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
    // The archive's own bands first, then the sessions it archived on their own. The live
    // list is not mixed into it: one list at a time, never two pictures at once.
    expect(painted()).toEqual([AWAY_FILED.id, AWAY_LOOSE.id]);
    expect(client.listProjectPage).toHaveBeenLastCalledWith(
      ROOT,
      10,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      true,
      'only',
      BANDS,
    );
    expect(client.listSessionGroups).toHaveBeenLastCalledWith(
      ROOT,
      expect.any(AbortSignal),
      'only',
      BANDS,
    );
  });

  it('archives a whole group and restores its unstamped sessions from the archive', async () => {
    const client = archivableMachine();
    const { user } = mount(client);
    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);

    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(screen.getByRole('dialog', { name: MENU })).getByText('Archive group'));
    await waitFor(() => expect(client.updateSessionGroup).toHaveBeenCalledWith(WALLET.id, { archived: true }));
    await waitFor(() => expect(screen.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull());
    expect(painted()).toEqual([]);

    await press(user, 'Show archived groups');
    const wallet = await band('Wallet work');
    await waitFor(() => expect(painted(wallet)).toEqual([ACTIVE.id]));
    expect(within(wallet).getByText('ARCHIVED')).toBeInTheDocument();
    expect(within(wallet).queryByRole('button', { name: 'New session in Wallet work' })).toBeNull();
    expect(within(wallet).queryByRole('button', { name: 'Unarchive' })).toBeNull();
    await user.click(within(wallet).getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(screen.getByRole('dialog', { name: MENU })).getByText('Unarchive group'));
    await waitFor(() => expect(client.updateSessionGroup).toHaveBeenCalledWith(WALLET.id, { archived: false }));
    await waitFor(() => expect(screen.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull());

    await press(user, 'Hide archived groups');
    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);
  });

  it('accepts a later gateway restoration instead of keeping its own archive answer forever', async () => {
    const client = archivableMachine();
    const { user } = mount(client);
    await band('Wallet work');
    const pageReads = client.listProjectPage.mock.calls.length;
    const groupReads = client.listSessionGroups.mock.calls.length;

    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(screen.getByRole('dialog', { name: MENU })).getByText('Archive group'));
    await waitFor(() => expect(client.listProjectPage.mock.calls.length).toBeGreaterThan(pageReads));
    await waitFor(() => expect(client.listSessionGroups.mock.calls.length).toBeGreaterThan(groupReads));
    // Another client restores the group without notifying this view.
    await client.updateSessionGroup(WALLET.id, { archived: false });

    await press(user, 'Show archived groups');
    await press(user, 'Hide archived groups');
    await waitFor(() => expect(painted()).toEqual([ACTIVE.id]));
  });

  it('shows empty archived groups so they can be restored', async () => {
    const empty = { ...RECEIPTS, session_count: 0 };
    const { user } = mount(machine(EMPTY_PAGE, [empty]));
    await band('Wallet work');
    await press(user, 'Show archived groups');
    const receipts = await band('Receipts');
    expect(painted(receipts)).toEqual([]);
    await user.click(within(receipts).getByRole('button', { name: 'Actions for Receipts' }));
    expect(within(screen.getByRole('dialog', { name: MENU })).getByText('Unarchive group')).toBeInTheDocument();
  });
  it('opens the archive when no active sessions remain in the project', async () => {
    const { user } = mount(machine(), []);
    await press(user, 'Show archived groups');
    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
  });
  it('keeps a busy group visible and explains why archiving was refused', async () => {
    const client = archivableMachine();
    client.updateSessionGroup.mockRejectedValueOnce(
      new GatewayError(409, 'session-busy', { error: { type: 'session-busy', session_id: ACTIVE.id } }),
    );
    const { user } = mount(client);
    const wallet = await band('Wallet work');
    await user.click(within(wallet).getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(screen.getByRole('dialog', { name: MENU })).getByText('Archive group'));
    expect(await screen.findByText('A session in this group is still active. Archive it once its turn is done.')).toBeInTheDocument();
    expect(painted(wallet)).toEqual([ACTIVE.id]);
  });

  it('leaves a session archived inside an active group to that group', async () => {
    const { user } = mount();
    await band('Wallet work');

    await press(user, 'Show archived groups');
    await band('Receipts');

    // The gateway answers it beside the window, because it IS archived. The project still
    // does not paint it: it was put away inside a group that is still on the live list,
    // and inventing a band here would file it under a shelf this view does not have.
    expect(screen.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull();
    expect(painted()).not.toContain(AWAY_IN_ACTIVE.id);
});

  it('lets go of a row the moment its own archive verb answers', async () => {
    // The verb belongs to the screen; the BAND is what paints its answer. The row the
    // gateway stamped is held here until the list's window catches up, so the band the
    // reader is looking at lets it go on the press, not on the next poll.
    const archive = vi.fn(async (session: Session, _conn: GatewayConn, _away: boolean) => ({
      ...session,
      archived_at: 1730000400,
    }));
    const { user } = mount(machine(), [ACTIVE], archive);
    const wallet = await band('Wallet work');
    expect(painted(wallet)).toEqual([ACTIVE.id]);

    const actions = within(wallet).getByRole('group', { name: `${ACTIVE.title} actions` });
    await user.click(within(actions).getByRole('button', { name: 'Archive' }));

    await waitFor(() => expect(painted()).not.toContain(ACTIVE.id));
    expect(archive).toHaveBeenCalledTimes(1);
    expect(archive.mock.calls[0][2]).toBe(true);
  });

  it('says where the reader is standing, and counts what the archive holds', async () => {
    const { user } = mount();
    const live = await screen.findByText('1 session');
    expect(live.parentElement?.textContent).toBe('1 session');

    await showBoth(user);

    // The loose archived row plus the band's own session, and none of the live states: a
    // session that was put away is not running.
    await waitFor(() =>
      expect(screen.getByText('2 sessions').parentElement?.textContent).toBe(
        // The separator's own span carries the space as margin, the way the live states do.
        '2 sessions\u00b7Archived',
      ),
    );
  });

  it('keeps the reveal on this device, so the project opens where it was left', async () => {
    const first = mount();
    await band('Wallet work');
    await showBoth(first.user);
    await band('Receipts');
    first.unmount();

    const again = mount(machine());

    expect(painted(await band('Receipts'))).toEqual([AWAY_FILED.id]);
    expect(painted()).toEqual([AWAY_FILED.id, AWAY_LOOSE.id]);
    // Nothing was asked of the live list on the way back, and no page of it was painted
    // from what this device still holds.
    expect(again.client.listProjectPage).toHaveBeenCalledTimes(1);
    expect(again.client.listProjectPage).toHaveBeenLastCalledWith(
      ROOT,
      10,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      true,
      'only',
      BANDS,
    );
  });

  it('says so when this project has archived nothing', async () => {
    const { user } = mount(machine(EMPTY_PAGE, []));
    await band('Wallet work');

    await showBoth(user);

    await screen.findByText('No archived groups in this project.');
    await screen.findByText('No archived sessions in this project.');
    expect(painted()).toEqual([]);
    expect(screen.queryByRole('button', { name: 'Collapse Receipts' })).toBeNull();
  });

  it('offers the way back to the live list under the same menu', async () => {
    const { user } = mount();
    await band('Wallet work');
    await showBoth(user);
    await band('Receipts');

    await user.click(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }));
    const menu = screen.getByRole('dialog', { name: MENU });
    expect(within(menu).queryByText('Show archived groups')).toBeNull();
    await user.click(within(menu).getByText('Hide archived groups'));
    await press(user, 'Hide archived sessions', 'sessions');

    expect(painted(await band('Wallet work'))).toEqual([ACTIVE.id]);
    expect(painted()).toEqual([ACTIVE.id]);
  });
});
