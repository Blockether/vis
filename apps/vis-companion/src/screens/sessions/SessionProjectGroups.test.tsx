// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import {
  DROP_TARGET_ATTRIBUTE,
  beginLift,
  carryOver,
  dropTargetAt,
  releaseLift,
} from '../../lib/session-drag';
import type { ArchiveView, BandWindow, Session, SessionGroup } from '../../lib/types';
import { ProjectGroup, type ProjectCreation } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const ROOT = STORY_NEWER_PROJECT.root;
const WALLET = 'group-wallet';

const WALLET_GROUP: SessionGroup = {
  id: WALLET,
  project_id: STORY_NEWER_PROJECT.projectId,
  name: 'Wallet work',
  color: 'blue',
  position: 0,
  session_count: 2,
};

/** The steps over a project's wall of bands move this many at a time (`GROUPS_PAGE`). */
const GROUPS_PAGE = 10;
/** The window the project stands on when it opens: the first page of the wall. */
const BANDS: BandWindow = { limit: GROUPS_PAGE, offset: 0 };

/**
 * What `GET /v1/session-groups` answers: ONE PAGE of the wall, and the WHOLE wall's own
 * tallies beside it — how many bands the project has, and how many sessions are filed
 * across all of them.
 */
function wall(groups: SessionGroup[], total = groups.length, offset = 0) {
  return {
    project_id: STORY_NEWER_PROJECT.projectId,
    groups,
    total,
    session_total: groups.reduce((sum, group) => sum + group.session_count, 0),
    limit: GROUPS_PAGE,
    offset,
    has_more: offset + groups.length < total,
  };
}

/** A wall deeper than one page of it: 24 bands, and no session filed under any of them. */
const WIDE_WALL: SessionGroup[] = Array.from({ length: 24 }, (_, index) => ({
  ...WALLET_GROUP,
  id: `group-${String(index).padStart(2, '0')}`,
  name: `Band ${String(index).padStart(2, '0')}`,
  position: index,
  session_count: 0,
}));

/**
 * A machine holding that wall, cut to whatever window a read asks for, over rows that name
 * none of its bands: the WALL is the list these tests page.
 */
function shelves() {
  const loose = {
    rows: STORY_NEWER_PROJECT.rows,
    total: STORY_NEWER_PROJECT.rows.length,
    awaiting: [],
    grouped: [],
    nextCursor: '',
  };
  return machine({
    heldProjectPage: () => loose,
    listProjectPage: vi.fn(async () => loose),
    listSessionGroups: vi.fn(
      async (_root: string, _signal?: AbortSignal, _view?: ArchiveView, bands?: BandWindow) => {
        const offset = bands?.offset ?? 0;
        const limit = bands?.limit ?? WIDE_WALL.length;
        return wall(WIDE_WALL.slice(offset, offset + limit), WIDE_WALL.length, offset);
      },
    ),
  });
}

/** Two of this project's sessions are filed under one group; the rest are not. */
const ROWS: Session[] = STORY_NEWER_PROJECT.rows.map((row, index) =>
  index < 2
    ? { ...row, group_id: WALLET }
    : row,
);
const LOOSE = ROWS[2];

type Machine = Record<string, unknown>;

function machine(overrides: Machine = {}) {
  const page = { rows: ROWS, total: ROWS.length, awaiting: [], grouped: [], nextCursor: '' };
  return {
    base: 'https://story.example.com',
    heldProjectPage: () => page,
    listProjectPage: vi.fn(async () => page),
    isSessionDeleted: () => false,
    listSessionGroups: vi.fn(async () => wall([WALLET_GROUP])),
    createSessionGroup: vi.fn(async (_root: string, name: string) => ({
      ...WALLET_GROUP,
      id: 'group-new',
      name,
      session_count: 0,
    })),
    updateSessionGroup: vi.fn(async () => WALLET_GROUP),
    deleteSessionGroup: vi.fn(async () => ({ detached: [ROWS[0].id, ROWS[1].id], deleted: [] })),
    assignSessionGroup: vi.fn(async (sid: string, gid: string | null) => ({
      ...(ROWS.find((row) => row.id === sid) ?? ROWS[0]),
      group_id: gid,
    })),
    ...overrides,
  };
}

function mount(
  client: Machine = machine(),
  creation?: ProjectCreation,
  needle = '',
  rows: Session[] = ROWS,
) {
  const started: ProjectCreation = creation ?? { state: null, start: vi.fn(async () => {}) };
  const open = vi.fn();
  // The rows this MACHINE is holding — its own window of the fleet. A poll that lands
  // hands the project a new one (`hold`), which is how a row arrives carrying an answer
  // it did not have a moment ago.
  const project = (held: Session[]) => (
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
            open,
            rename: vi.fn(async () => {}),
            requestDelete: vi.fn(),
            toggleStar: vi.fn(),
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
      creation={started}
      initiallyOpen
    />
  );
  const view = render(project(rows));
  return {
    client,
    creation: started,
    open,
    user: userEvent.setup(),
    /** The next poll landed: this machine now holds these rows. */
    hold: (held: Session[]) => view.rerender(project(held)),
  };
}

/** The band and the rows filed under it, as one block inside the project's list. */
async function band(name: string): Promise<HTMLElement> {
  const disclosure = await screen.findByRole('button', { name: `Collapse ${name}` });
  return disclosure.closest('div')!.parentElement as HTMLElement;
}

const sheet = (label: string) => screen.getByRole('dialog', { name: label });

/** One row's action drawer, where its verbs stand: the wrapper a reader drags. */
function strip(scope: HTMLElement, sid: string): HTMLElement {
  const slab = scope.querySelector(`[data-session-id="${sid}"]`) as HTMLElement;
  return slab.closest('[draggable="true"]') as HTMLElement;
}

const surface = (sid: string) => document.querySelector(`[data-session-id="${sid}"]`) as HTMLElement;

function finePointer() {
  const matchMedia = window.matchMedia;
  vi.spyOn(window, 'matchMedia').mockImplementation((query) => ({
    ...matchMedia(query),
    matches: query === '(pointer: fine)',
  }));
}

function dragCarrier() {
  const values = new Map<string, string>();
  return {
    setData: vi.fn((type: string, value: string) => values.set(type, value)),
    getData: vi.fn((type: string) => values.get(type) ?? ''),
    setDragImage: vi.fn(),
    effectAllowed: '',
    dropEffect: '',
  };
}

describe('ProjectGroup groups', () => {
  // A fold is REMEMBERED (`lib/project-fold`), so one test's shut band must not
  // arrive shut in the next one.
  beforeEach(() => window.localStorage.clear());
  afterEach(() => vi.restoreAllMocks());

  it('nests a group band under the project header, with its colour and filed rows', async () => {
    mount();
    const wallet = await band('Wallet work');
    expect(within(wallet).queryByText('2 sessions')).toBeNull();
    // The band wears its colour ONCE, as the rail down the leading edge it shares with
    // every row filed under it (BLO-167): band rail + one per filed row, nothing else.
    // A dot beside the name said the same thing a second time, so it is gone.
    expect(wallet.querySelectorAll('.bg-group-blue')).toHaveLength(3);
    const groupButton = await screen.findByRole('button', { name: 'Collapse Wallet work' });
    expect(groupButton.querySelector('[class*="bg-group-"]')).toBeNull();
    expect(groupButton).toHaveClass('pl-3', 'gap-0');
    const groupHeader = groupButton.parentElement!;
    expect(groupHeader).not.toHaveClass('border-t');
    expect(groupHeader).toHaveClass('border-b', 'border-b-edge-strong');
    // The group fold shares the project mark's column without moving either name.
    const groupChevron = groupButton.querySelector('svg.lucide-chevron-right');
    expect(groupChevron).toHaveClass('-translate-x-1.5');
    expect(within(groupButton).getByText('Wallet work')).toHaveClass(
      'font-mono', 'text-body', 'font-medium', 'text-white',
    );
    expect([...wallet.querySelectorAll('[data-session-id]')].map((row) =>
      row.getAttribute('data-session-id'),
    )).toEqual([ROWS[0].id, ROWS[1].id]);
    // What nobody filed keeps the project's own order, below every band.
    expect(wallet.querySelector(`[data-session-id="${LOOSE.id}"]`)).toBeNull();
    const list = wallet.closest('[data-project-root]') as HTMLElement;
    const painted = [...list.querySelectorAll('[data-session-id]')].map((row) =>
      row.getAttribute('data-session-id'),
    );
    expect(painted).toEqual([ROWS[0].id, ROWS[1].id, ROWS[2].id, ROWS[3].id]);
  });

  // Reported in this Vis session with a screenshot (paraphrased: choosing a colour changed
  // some of the rows and left the others alone): every row used to carry a COPY of its
  // group's name and colour, stamped on it when the gateway read the row, so rows read
  // before a recolour kept the old token while the band over them, painted from the
  // groups, already wore the new one. The colour is the GROUP's; a row names its group
  // and nothing else, so there is nothing left to go stale.
  it('paints every filed row in the colour its group wears now', async () => {
    const recoloured = { ...WALLET_GROUP, color: 'violet' };
    const page = {
      rows: [ROWS[2], ROWS[3]],
      total: 2,
      awaiting: [],
      grouped: [ROWS[0], ROWS[1]],
      nextCursor: '',
    };
    mount(
      machine({
        heldProjectPage: () => page,
        listProjectPage: vi.fn(async () => page),
        listSessionGroups: vi.fn(async () => wall([recoloured])),
      }),
    );
    const wallet = await band('Wallet work');

    expect(wallet.querySelectorAll('.bg-group-blue')).toHaveLength(0);
    // The band's rail, and one down each of the two rows filed under it.
    expect(wallet.querySelectorAll('.bg-group-violet')).toHaveLength(3);
  });

  // The two sets name their rows without repeating counts from the project header.
  // Filing a session moves it into its group, not into the loose Sessions set.
  it('names both sets without repeated tallies and keeps filed sessions out of Sessions', async () => {
    const page = {
      rows: [ROWS[2], ROWS[3]],
      total: 2,
      awaiting: [],
      grouped: [ROWS[0], ROWS[1]],
      nextCursor: '',
    };
    mount(machine({ heldProjectPage: () => page, listProjectPage: vi.fn(async () => page) }));
    const wallet = await band('Wallet work');
    const list = wallet.closest('[data-project-root]') as HTMLElement;

    const groupsHeader = within(list).getByText('Groups').closest('div') as HTMLElement;
    expect(within(groupsHeader).queryByText('1 group')).toBeNull();
    // Set labels use full-strength ink; the bands remain quieter through their smaller type.
    // Their tinted upper rules distinguish the shelves; the lower rules close each shelf.
    expect(groupsHeader).toHaveClass(
      'border-t', 'border-b', 'border-t-set-groups-border', 'border-b-edge-strong', 'bg-set-groups',
    );
    expect(groupsHeader).toHaveClass('min-h-14', 'py-1', 'mouse:min-h-8', 'mouse:py-0');
    // Phone set headings fit the pager's 44px touch target plus their border rules.
    expect(groupsHeader).toHaveClass('max-sm:min-h-11.5', 'max-sm:py-0');
    expect(within(groupsHeader).getByText('Groups')).toHaveClass(
      'font-mono', 'text-ui', 'font-medium', 'text-white',
    );
    expect(within(wallet).queryByText('2 sessions')).toBeNull();
    // The project still owns its total; each set keeps only its own heading and actions.
    const sessionsHeader = within(list).getByText('Sessions').closest('div') as HTMLElement;
    expect(within(sessionsHeader).queryByText('2 sessions')).toBeNull();
    expect(sessionsHeader).toHaveClass(
      'border-t', 'border-b', 'border-t-set-sessions-border', 'border-b-edge-strong', 'bg-set-sessions',
    );
    const sessionPaper = groupsHeader.parentElement!.parentElement!;
    expect(sessionPaper).toHaveClass('bg-set-sessions');
    expect(sessionPaper).toContainElement(wallet.querySelector(`[data-session-id="${ROWS[0].id}"]`));
    expect(sessionPaper).toContainElement(list.querySelector(`[data-session-id="${LOOSE.id}"]`));
    for (const id of [ROWS[0].id, LOOSE.id]) {
      const row = list.querySelector(`[data-session-id="${id}"]`)!;
      expect(row.closest('[data-swipe-track]')?.firstElementChild).toHaveClass('bg-set-sessions');
    }
    expect(sessionsHeader).toHaveClass('min-h-14', 'py-1', 'mouse:min-h-8', 'mouse:py-0');
    expect(sessionsHeader).toHaveClass('max-sm:min-h-11.5', 'max-sm:py-0');
    expect(within(sessionsHeader).getByText('Sessions')).toHaveClass(
      'font-mono', 'text-ui', 'font-medium', 'text-white',
    );
    const projectHeader = within(list).getByText(STORY_NEWER_PROJECT.name).closest('header')!;
    expect(within(projectHeader).getByText('4 sessions')).toBeInTheDocument();

    // The session set starts under its own word and holds neither filed row.
    const loose: string[] = [];
    for (let node = sessionsHeader.nextElementSibling; node; node = node.nextElementSibling) {
      const id = node.querySelector('[data-session-id]')?.getAttribute('data-session-id');
      if (id) loose.push(id);
    }
    expect(loose).toEqual([ROWS[2].id, ROWS[3].id]);
  });

  // A group takes a session out of the list, never out of the search: under a query the
  // project paints ONE set, and every hit is in it.
  it('answers a query with one set that still holds the filed sessions', async () => {
    mount(machine(), undefined, 'session');

    expect(await screen.findByText('Sessions')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull();
    expect(screen.queryByText('Groups')).toBeNull();
    expect(
      [...document.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).toEqual(ROWS.map((row) => row.id));
  });

  // BLO-167, user report (paraphrased: "groups should be outside the paging, and there is
  // no way to tell a session is in one"): a band was cut from the page under it, so a
  // filed session deeper in the project was missing from its own group.
  it('paints a filed session the page does not hold, out of the shelves beside it', async () => {
    const offPage: Session = {
      ...ROWS[3],
      id: 'off-page-session',
      title: 'Filed forty pages down',
      group_id: WALLET,
    };
    const page = {
      rows: [ROWS[2], ROWS[3]],
      total: 2,
      awaiting: [],
      grouped: [ROWS[0], ROWS[1], offPage],
      nextCursor: '',
    };
    mount(machine({ heldProjectPage: () => page, listProjectPage: vi.fn(async () => page) }));
    const wallet = await band('Wallet work');
    expect(
      [...wallet.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).toEqual([ROWS[0].id, ROWS[1].id, offPage.id]);
    // The pager below walks the LOOSE sessions only; the shelf is not part of that walk.
    const list = wallet.closest('[data-project-root]') as HTMLElement;
    expect(
      [...list.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).toEqual([ROWS[0].id, ROWS[1].id, offPage.id, ROWS[2].id, ROWS[3].id]);
  });

  // Regression, user report: a session filed in a group never reported a new answer.
  // Read marks used to be a per-device watermark seeded from the FLEET WINDOW — the
  // newest twenty rows across every machine — while a group shelf is answered COMPLETE
  // however deep its sessions sit (`?grouped=aside`). A filed row below that window
  // therefore carried no watermark at all and read as already seen. The gateway owns
  // the mark now, so a shelved row arrives carrying its own answer.
  it('raises NEW on a filed session the fleet window never held', async () => {
    const shelved: Session = {
      ...ROWS[0],
      id: 'filed-below-the-window',
      title: 'Filed forty pages down',
      group_id: WALLET,
      answer_count: 3,
    };
    const page = {
      rows: [LOOSE],
      total: 1,
      awaiting: [],
      grouped: [shelved],
      nextCursor: '',
    };
    // This machine holds the loose row alone; the filed one sits below its window.
    const { hold } = mount(
      machine({ heldProjectPage: () => page, listProjectPage: vi.fn(async () => page) }),
      undefined,
      '',
      [LOOSE],
    );
    await band('Wallet work');

    // The answer lands, and the row rides up into the window carrying the gateway's
    // own unread mark.
    await act(async () => {
      hold([LOOSE, { ...shelved, answer_count: 4, is_unread: true, unread_answers: 1 }]);
    });

    expect(await screen.findByText('NEW')).toBeInTheDocument();
  });

  // BLO-167: a session started on a group band is minted inside that group.
  it('starts a session in its group from the group menu', async () => {
    const start = vi.fn(async () => {});
    const { user } = mount(machine(), { state: null, start });
    await user.click(await screen.findByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('New session'));
    expect(start).toHaveBeenCalledWith(conn, ROOT, WALLET);
  });

  it('disables only the in-flight group creation', async () => {
    const { user } = mount(machine(), {
      state: { at: `https://story.example.com\u0000${ROOT}\u0000${WALLET}`, label: 'Creating...' },
      start: vi.fn(async () => {}),
    });
    await user.click(await screen.findByRole('button', { name: 'Actions for Wallet work' }));
    expect(within(sheet(`Groups in ${ROOT}`)).getByText('New session').closest('button')).toBeDisabled();
    await user.click(document.body);
    await user.click(screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` }));
    expect(within(sheet(`Sessions in ${ROOT}`)).getByText('New session').closest('button')).toBeEnabled();
  });

  // The single band menu owns creation and its other actions; no separate plus stands beside it.
  it('ends a group band with its menu alone', async () => {
    mount();
    const wallet = await band('Wallet work');
    const cluster = within(wallet).getByRole('button', { name: 'Actions for Wallet work' })
      .parentElement as HTMLElement;
    expect(Array.from(cluster.children).map((child) => child.getAttribute('aria-label'))).toEqual([
      'Actions for Wallet work',
    ]);
  });

  // The project owns neither set's verbs: each list has its own menu, with no plus.
  it('puts group and session actions on their set headers, not on the project', async () => {
    mount();
    await band('Wallet work');
    const header = screen.getByText(STORY_NEWER_PROJECT.name).closest('header') as HTMLElement;
    expect(within(header).queryByRole('button', { name: /Actions for (groups|sessions)/ })).toBeNull();
    expect(within(header).queryByRole('button', { name: /^New session/ })).toBeNull();
    expect(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^New session/ })).toBeNull();
  });

  it('starts an ungrouped session from the Sessions menu', async () => {
    const start = vi.fn(async () => {});
    const { user } = mount(machine(), { state: null, start });
    await user.click(await screen.findByRole('button', { name: `Actions for sessions in ${ROOT}` }));
    await user.click(within(sheet(`Sessions in ${ROOT}`)).getByText('New session'));
    expect(start).toHaveBeenCalledWith(conn, ROOT);
  });

  // A new conversation is composed, not played; both entry points share the same mark.
  it('uses a compose icon for new sessions in both menus', async () => {
    const { user } = mount();
    await user.click(await screen.findByRole('button', { name: `Actions for sessions in ${ROOT}` }));
    const sessionsAction = within(sheet(`Sessions in ${ROOT}`)).getByRole('button', {
      name: 'New session',
    });
    expect(sessionsAction.querySelectorAll('svg.lucide-square-pen')).toHaveLength(1);
    expect(sessionsAction.querySelector('svg.lucide-play')).toBeNull();
    await user.click(document.body);
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    const groupAction = within(sheet(`Groups in ${ROOT}`)).getByRole('button', {
      name: 'New session',
    });
    expect(groupAction.querySelectorAll('svg.lucide-square-pen')).toHaveLength(1);
    expect(groupAction.querySelector('svg.lucide-play')).toBeNull();
  });

  // Regression: the trailing rail is one column on set headers, bands and session rows.
  it('stands every trailing mark in one column, set, band and row alike', async () => {
    mount();
    const wallet = await band('Wallet work');
    const row = strip(document.body, LOOSE.id);
    const disclosure = within(row).getByRole('button', { name: /^Show details for / });
    const rowMenu = within(row).getByRole('button', { name: /^Actions for / });
    const marks = [
      screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }),
      screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` }),
      within(wallet).getByRole('button', { name: 'Actions for Wallet work' }),
      disclosure,
      rowMenu,
    ];
    for (const mark of marks) {
      expect(mark).toHaveClass('size-8', 'mouse:size-7');
      expect(mark).not.toHaveClass('mouse:size-6');
      expect(mark.parentElement).toHaveClass('pr-2', 'mouse:pr-2.5');
    }
    expect(disclosure.parentElement).toHaveClass('gap-2', 'mouse:gap-2.5');
  });

  it('keeps both set menus reachable in an empty project after folding and expanding it', async () => {
    const empty = { rows: [], total: 0, awaiting: [], grouped: [], nextCursor: '' };
    const { user } = mount(
      machine({
        heldProjectPage: () => empty,
        listProjectPage: vi.fn(async () => empty),
        listSessionGroups: vi.fn(async () => wall([])),
      }),
      undefined,
      '',
      [],
    );
    expect(await screen.findByRole('button', { name: `Actions for groups in ${ROOT}` })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` })).toBeInTheDocument();
    await user.click(screen.getByRole('button', { name: `Collapse ${STORY_NEWER_PROJECT.name}` }));
    expect(screen.queryByRole('button', { name: `Actions for groups in ${ROOT}` })).toBeNull();
    await user.click(screen.getByRole('button', { name: `Expand ${STORY_NEWER_PROJECT.name}` }));
    expect(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` })).toBeInTheDocument();
  });

  // The open project's chevron and (in non-light themes) surface identify the state.
  // Neither state paints an ornamental rail over the project boundary.
  it('folds a project without a decorative leading rail', async () => {
    const { user } = mount();
    const disclosure = await screen.findByRole('button', {
      name: `Collapse ${STORY_NEWER_PROJECT.name}`,
    });
    const header = disclosure.closest('header')!;
    expect(header).toHaveClass('bg-project-header-active');
    expect(header.className).not.toContain('before:');
    expect(header).not.toHaveClass('bg-project-header');
    expect(header).toHaveClass('sticky', 'top-0', 'mouse:focus-within:bg-hover');
    expect(header.className).toContain('var(--color-project-header-active)');

    await user.click(disclosure);
    expect(disclosure).toHaveAttribute('aria-expanded', 'false');
    expect(header).toHaveClass('bg-project-header');
    expect(header).not.toHaveClass('bg-project-header-active');
    expect(header.className).not.toContain('before:');
    expect(header.className).toContain('var(--color-project-header)');
    await user.click(disclosure);
    expect(disclosure).toHaveAttribute('aria-expanded', 'true');
    expect(header).toHaveClass('bg-project-header-active');
    expect(header.className).not.toContain('before:');
  });

  // The project total remains a quiet caption when the bands stop repeating counts.
  it('keeps the project total in its quiet caption', async () => {
    mount();
    const header = screen.getByText(STORY_NEWER_PROJECT.name).closest('header') as HTMLElement;
    const total = within(header).getByText(`${ROWS.length} sessions`);
    expect(total).not.toHaveClass('font-bold');
    const caption = total.closest(`[title="${ROOT}"]`) as HTMLElement;
    expect(caption).toHaveClass('mouse:text-chip');
    expect(caption).not.toHaveClass('mouse:text-meta');
    const wallet = await band('Wallet work');
    expect(within(wallet).queryByText('2 sessions')).toBeNull();
  });

  it('folds one group without folding the project', async () => {
    const { user } = mount();
    const wallet = await band('Wallet work');
    await user.click(within(wallet).getByRole('button', { name: 'Collapse Wallet work' }));
    expect(wallet.querySelector(`[data-session-id="${ROWS[0].id}"]`)).toBeNull();
    expect(within(wallet).getByRole('button', { name: 'Expand Wallet work' }).parentElement)
      .not.toHaveClass('border-b');
    // The project stays open: only the band it was folded in lost its rows.
    const list = wallet.closest('[data-project-root]') as HTMLElement;
    expect(list.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1);
  });

  it('creates a group from the Groups header menu', async () => {
    const { client, user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('New group'));
    await user.type(screen.getByLabelText('Group name'), 'Receipts');
    await user.click(screen.getByRole('button', { name: 'Create' }));
    await waitFor(() => expect(client.createSessionGroup).toHaveBeenCalledWith(ROOT, 'Receipts'));
  });

  // The Groups menu offers its verbs without repeating the list of groups below it.
  it('offers the verb without naming the project back or listing its groups', async () => {
    const { user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }));
    const menu = sheet(`Groups in ${ROOT}`);

    expect(within(menu).getByText('New group')).toBeInTheDocument();
    expect(within(menu).queryByText(`Groups in ${ROOT}`)).toBeNull();
    expect(within(menu).queryByText(WALLET_GROUP.name)).toBeNull();
  });

  it('says so when the project already has that name', async () => {
    const { user } = mount(
      machine({
        createSessionGroup: vi.fn(async () => {
          throw new GatewayError(409, 'group-exists');
        }),
      }),
    );
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('New group'));
    await user.type(screen.getByLabelText('Group name'), 'Wallet work');
    await user.click(screen.getByRole('button', { name: 'Create' }));
    expect(
      await screen.findByText('This project already has a group with that name.'),
    ).toBeInTheDocument();
  });

  // Regression, user report: renaming a group opened a second question over the list
  // instead of letting its band be edited like a session title.
  it('renames the group in its band without a second dialog', async () => {
    let current = WALLET_GROUP;
    const client = machine({
      listSessionGroups: vi.fn(async () => wall([current])),
      updateSessionGroup: vi.fn(async (_id: string, change: { name: string }) => {
        current = { ...current, ...change };
        return current;
      }),
    });
    const { user } = mount(client);
    const wallet = await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Rename group'));

    expect(screen.queryByRole('dialog', { name: `Groups in ${ROOT}` })).toBeNull();
    const field = within(wallet).getByRole('textbox', { name: 'Rename Wallet work' });
    expect(field).toHaveValue('Wallet work');
    expect(field).toHaveFocus();
    expect(within(wallet).getByRole('button', { name: 'Collapse Wallet work' })).toBeVisible();
    await user.clear(field);
    await user.type(field, '  Payments  {Enter}');
    await waitFor(() =>
      expect(client.updateSessionGroup).toHaveBeenCalledWith(WALLET, { name: 'Payments' }),
    );
    expect(await screen.findByRole('button', { name: 'Collapse Payments' })).toBeVisible();
  });

  it('saves an inline group rename when the field loses focus', async () => {
    let current = WALLET_GROUP;
    const client = machine({
      listSessionGroups: vi.fn(async () => wall([current])),
      updateSessionGroup: vi.fn(async (_id: string, change: { name: string }) => {
        current = { ...current, ...change };
        return current;
      }),
    });
    const { user } = mount(client);
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Rename group'));
    const field = screen.getByRole('textbox', { name: 'Rename Wallet work' });
    await user.clear(field);
    await user.type(field, 'Invoices');
    await user.tab();
    await waitFor(() =>
      expect(client.updateSessionGroup).toHaveBeenCalledWith(WALLET, { name: 'Invoices' }),
    );
    expect(await screen.findByRole('button', { name: 'Collapse Invoices' })).toBeVisible();
  });

  it('cancels an inline group rename and keeps refused names editable', async () => {
    const client = machine({
      updateSessionGroup: vi.fn(async () => {
        throw new GatewayError(409, 'group-exists');
      }),
    });
    const { user } = mount(client);
    await band('Wallet work');
    const start = async () => {
      await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
      await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Rename group'));
      return screen.getByRole('textbox', { name: 'Rename Wallet work' });
    };
    let field = await start();
    await user.clear(field);
    await user.type(field, 'Keep me{Escape}');
    expect(screen.queryByRole('textbox', { name: 'Rename Wallet work' })).toBeNull();
    expect(client.updateSessionGroup).not.toHaveBeenCalled();

    field = await start();
    await user.clear(field);
    await user.type(field, '  {Enter}');
    expect(client.updateSessionGroup).not.toHaveBeenCalled();
    expect(screen.getByRole('textbox', { name: 'Rename Wallet work' })).toHaveFocus();
    await user.clear(field);
    await user.type(field, 'Existing{Enter}');
    expect(await screen.findByRole('status')).toHaveTextContent(
      'This project already has a group with that name.',
    );
    expect(field).toHaveValue('Existing');
  });

  // A session joins a group from its own row, not from the group's action menu.
  // That menu offers rename, colour and delete actions for the group itself.
  it('keeps session filing out of the group menu', async () => {
    const { user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    const menu = sheet(`Groups in ${ROOT}`);
    expect(within(menu).getByText('Rename group')).toBeInTheDocument();
    expect(within(menu).getByText('Delete group')).toBeInTheDocument();
    expect(within(menu).getByText('Choose colour')).toBeInTheDocument();
    // A TITLE THAT SAYS WHAT THE ROW DOES CARRIES NO SENTENCE UNDER IT.
    expect(within(menu).queryByText(/Asks what becomes/)).toBeNull();
    // The sheet hangs under the band's own ⋮, so nothing repeats the name just pressed
    // and no way back to a project root that says nothing about this group.
    expect(within(menu).queryByText('Wallet work')).toBeNull();
    expect(within(menu).queryByText(/Back to/)).toBeNull();
    expect(within(menu).queryByText(String(LOOSE.title))).toBeNull();
  });

  // A GROUP'S COLOUR IS A PALETTE, NOT A COLUMN, and nothing is titled over it: eight
  // named rows stood open under the verbs, each as tall as a verb and reading like one,
  // under a band that spelled the group's name a third time.
  it('picks a colour off a palette a step in and returns to the verbs', async () => {
    const { client, user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    const menu = sheet(`Groups in ${ROOT}`);
    expect(within(menu).queryByRole('button', { name: 'Slate' })).toBeNull();
    await user.click(within(menu).getByText('Choose colour'));
    expect(within(menu).queryByText(/^Colour /)).toBeNull();
    expect(within(menu).queryByText(/Back to/)).toBeNull();
    // The tile says what it does by being the colour, and the group's own is pressed.
    expect(within(menu).getByRole('button', { name: 'Blue' })).toHaveAttribute(
      'aria-pressed',
      'true',
    );
    expect(within(menu).queryByText('Violet')).toBeNull();
    await user.click(within(menu).getByRole('button', { name: 'Violet' }));
    await waitFor(() =>
      expect(client.updateSessionGroup).toHaveBeenCalledWith(WALLET, { color: 'violet' }),
    );
    await waitFor(() => expect(within(menu).getByText('Rename group')).toBeInTheDocument());
  });
  it('asks what becomes of the sessions before it deletes a group', async () => {
    const { client, user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Delete group'));
    // BOTH answers are offered, each behind its own mark and nothing else: no band
    // repeating the name the reader just pressed, and no sentence under either title.
    // Nothing has reached the machine yet.
    expect(
      within(sheet(`Groups in ${ROOT}`)).queryByText(`Delete ${WALLET_GROUP.name}`),
    ).toBeNull();
    const keep = screen.getByText('Keep its sessions').closest('button') as HTMLElement;
    const wipe = screen.getByText('Delete its sessions too').closest('button') as HTMLElement;
    expect(keep.textContent).toBe('Keep its sessions');
    expect(wipe.textContent).toBe('Delete its sessions too');
    // One mark each, and nothing else beside the title.
    expect(keep.querySelectorAll('svg')).toHaveLength(1);
    expect(wipe.querySelectorAll('svg')).toHaveLength(1);
    expect(client.deleteSessionGroup).not.toHaveBeenCalled();
    await user.click(keep);
    await waitFor(() =>
      expect(client.deleteSessionGroup).toHaveBeenCalledWith(WALLET, 'detach'),
    );
  });

  it('deletes the sessions with the group when that is the answer', async () => {
    const { client, user } = mount(
      machine({
        deleteSessionGroup: vi.fn(async () => ({
          detached: [],
          deleted: [ROWS[0].id, ROWS[1].id],
        })),
      }),
    );
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Delete group'));
    await user.click(screen.getByText('Delete its sessions too'));
    await waitFor(() =>
      expect(client.deleteSessionGroup).toHaveBeenCalledWith(WALLET, 'with-sessions'),
    );
  });

  // Regression, user report: Move to... opened another question over the list.
  it('files a session from its own inline group choices', async () => {
    const { client, user } = mount();
    const wallet = await band('Wallet work');
    const list = wallet.closest('[data-project-root]') as HTMLElement;
    const slab = list.querySelector(`[data-session-id="${LOOSE.id}"]`) as HTMLElement;
    const row = slab.closest('[draggable="true"]') as HTMLElement;
    await user.click(within(row).getByText('Move to...'));
    expect(screen.queryByRole('dialog', { name: `Groups in ${ROOT}` })).toBeNull();
    const choices = screen.getByRole('group', { name: `Move ${LOOSE.title} to group` });
    await user.click(within(choices).getByRole('button', { name: 'Wallet work' }));
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1),
    );
    expect(screen.queryByRole('group', { name: `Move ${LOOSE.title} to group` })).toBeNull();
  });

  // ONE GROUP, AND THIS ROW IS FILED UNDER IT: `Move to...` opened a sheet whose only
  // choice was the band the row already sits in. Reported from the app with a screenshot
  // of the row's menu — the strip now names the one filing left and runs it in that press.
  it("names the row's verb Ungroup when its group is the project's only one", async () => {
    const { client, user } = mount();
    const wallet = await band('Wallet work');
    const filed = ROWS[0];
    const row = strip(wallet, filed.id);
    expect(within(row).queryByText('Move to...')).toBeNull();
    await user.click(within(row).getByText('Ungroup'));
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(filed.id, null));
    // Nothing to choose means nothing to ask: no sheet opens over the list.
    expect(screen.queryByRole('dialog', { name: `Groups in ${ROOT}` })).toBeNull();
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${filed.id}"]`)).toHaveLength(0),
    );
  });

  it('keeps inline move choices visible and retryable after a failed filing', async () => {
    const assignSessionGroup = vi.fn()
      .mockRejectedValueOnce(new Error('Offline'))
      .mockImplementation(async (sid: string, gid: string) => ({ ...LOOSE, id: sid, group_id: gid }));
    const { user } = mount(machine({ assignSessionGroup }));
    const wallet = await band('Wallet work');
    const list = wallet.closest('[data-project-root]') as HTMLElement;
    await user.click(within(strip(list, LOOSE.id)).getByText('Move to...'));
    const choices = screen.getByRole('group', { name: `Move ${LOOSE.title} to group` });
    await user.click(within(choices).getByRole('button', { name: 'Wallet work' }));
    expect(await within(choices).findByRole('status')).toHaveTextContent('Try again.');
    expect(screen.queryByRole('dialog', { name: `Groups in ${ROOT}` })).toBeNull();
    await user.click(within(choices).getByRole('button', { name: 'Wallet work' }));
    await waitFor(() => expect(assignSessionGroup).toHaveBeenCalledTimes(2));
    await waitFor(() =>
      expect(screen.queryByRole('group', { name: `Move ${LOOSE.title} to group` })).toBeNull(),
    );
  });

  // More than one destination remains an inline choice, not another sheet.
  it('keeps Move to... on a filed row while another group could take it', async () => {
    const { user } = mount(
      machine({
        listSessionGroups: vi.fn(async () =>
          wall([
            WALLET_GROUP,
            { ...WALLET_GROUP, id: 'group-notes', name: 'Notes', session_count: 0 },
          ]),
        ),
      }),
    );
    const wallet = await band('Wallet work');
    const row = strip(wallet, ROWS[0].id);
    expect(within(row).queryByText('Ungroup')).toBeNull();
    await user.click(within(row).getByText('Move to...'));
    const choices = screen.getByRole('group', { name: `Move ${ROWS[0].title} to group` });
    expect(within(choices).getByRole('button', { name: 'Notes' })).toBeInTheDocument();
    expect(within(choices).getByRole('button', { name: 'Take out of its group' })).toBeInTheDocument();
    expect(screen.queryByRole('dialog', { name: `Groups in ${ROOT}` })).toBeNull();
    await user.keyboard('{Escape}');
    expect(screen.queryByRole('group', { name: `Move ${ROWS[0].title} to group` })).toBeNull();
    await user.click(within(row).getByText('Move to...'));
    await user.click(screen.getByRole('button', { name: 'Collapse Wallet work' }));
    expect(screen.queryByRole('group', { name: `Move ${ROWS[0].title} to group` })).toBeNull();
  });

  it('selects a visible range across groups on Shift-click without opening another session', async () => {
    finePointer();
    const { open } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    expect(open).toHaveBeenCalledTimes(1);
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    expect(open).toHaveBeenCalledTimes(1);
    for (const session of ROWS) {
      expect(surface(session.id)).toHaveAttribute('aria-pressed', 'true');
      expect(strip(document.body, session.id)).toHaveClass('bg-accent/15');
    }
    fireEvent.click(surface(ROWS[1].id), { shiftKey: true });
    expect(surface(ROWS[0].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[1].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
    fireEvent.click(surface(ROWS[2].id));
    expect(open).toHaveBeenCalledTimes(2);
    for (const session of ROWS) expect(surface(session.id)).toHaveAttribute('aria-pressed', 'false');
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    fireEvent.keyDown(window, { key: 'Escape' });
    for (const session of ROWS) expect(surface(session.id)).not.toHaveAttribute('aria-pressed', 'true');
  });

  it('keeps the batch selected when pressing a selected row to begin dragging', async () => {
    finePointer();
    const { open, user } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    const picked = surface(ROWS[2].id);
    fireEvent.pointerDown(picked, { button: 0, pointerType: 'mouse' });
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'true');
    fireEvent.click(picked, { detail: 1 });
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'true');
    expect(open).toHaveBeenCalledTimes(1);
    const carrier = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[2].id), { dataTransfer: carrier });
    expect(carrier.getData('application/vnd.vis.sessions+json')).toBe(JSON.stringify(ROWS.map((row) => row.id)));
    fireEvent.click(picked, { detail: 2 });
    expect(open).toHaveBeenCalledTimes(2);
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'false');

    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    picked.focus();
    await user.keyboard('{Enter}');
    expect(open).toHaveBeenCalledTimes(4);
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'false');
  });

  it('keeps a touch-style Shift-click as a normal open, not a multi-selection', async () => {
    const { open } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[2].id), { shiftKey: true });
    expect(open).toHaveBeenCalledTimes(2);
    expect(surface(ROWS[2].id)).not.toHaveAttribute('aria-pressed', 'true');
  });

  it('toggles individual rows with Command on Apple, then shifts from the last toggled row', async () => {
    finePointer();
    vi.spyOn(window.navigator, 'platform', 'get').mockReturnValue('MacIntel');
    const { open } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    fireEvent.click(surface(ROWS[1].id), { metaKey: true });
    expect(ROWS.map((row) => surface(row.id).getAttribute('aria-pressed'))).toEqual([
      'true', 'false', 'true', 'true',
    ]);
    fireEvent.click(surface(ROWS[1].id), { metaKey: true });
    fireEvent.click(surface(ROWS[3].id), { metaKey: true });
    expect(ROWS.map((row) => surface(row.id).getAttribute('aria-pressed'))).toEqual([
      'true', 'true', 'true', 'false',
    ]);
    fireEvent.click(surface(ROWS[1].id), { shiftKey: true });
    expect(ROWS.map((row) => surface(row.id).getAttribute('aria-pressed'))).toEqual([
      'false', 'true', 'true', 'true',
    ]);
    expect(open).toHaveBeenCalledTimes(1);
    fireEvent.keyDown(window, { key: 'Escape' });
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'false');
  });

  it('keeps Mac Control-click for the row context menu without opening or changing selection', async () => {
    finePointer();
    vi.spyOn(window.navigator, 'platform', 'get').mockReturnValue('MacIntel');
    const { open } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id), { metaKey: true });
    fireEvent.click(surface(ROWS[1].id), { ctrlKey: true });
    expect(open).not.toHaveBeenCalled();
    expect(surface(ROWS[0].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[1].id)).toHaveAttribute('aria-pressed', 'false');
    fireEvent.contextMenu(surface(ROWS[1].id), { ctrlKey: true, clientX: 50, clientY: 50 });
    expect(await screen.findByRole('dialog', { name: `${ROWS[1].title} actions` })).toBeInTheDocument();
  });

  it.each(['Win32', 'Linux x86_64'])(
    'toggles with Control on %s, including a disconnected drag set, but not Command',
    async (platform) => {
      finePointer();
      vi.spyOn(window.navigator, 'platform', 'get').mockReturnValue(platform);
      const { client, open } = mount();
      const wallet = await band('Wallet work');
      fireEvent.click(surface(ROWS[0].id), { ctrlKey: true });
      fireEvent.click(surface(ROWS[2].id), { ctrlKey: true });
      fireEvent.click(surface(ROWS[3].id), { ctrlKey: true });
      fireEvent.click(surface(ROWS[2].id), { ctrlKey: true });
      const carrier = dragCarrier();
      fireEvent.dragStart(strip(document.body, ROWS[3].id), { dataTransfer: carrier });
      expect(carrier.getData('application/vnd.vis.sessions+json')).toBe(
        JSON.stringify([ROWS[0].id, ROWS[3].id]),
      );
      const picture = carrier.setDragImage.mock.calls[0][0] as HTMLElement;
      expect(picture).toHaveTextContent('2 sessions');
      expect(picture).toHaveTextContent(ROWS[0].title!);
      expect(picture).toHaveTextContent(ROWS[3].title!);
      expect(picture).not.toHaveTextContent(ROWS[2].title!);
      fireEvent.drop(wallet, { dataTransfer: carrier });
      await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledTimes(1));
      expect(client.assignSessionGroup).toHaveBeenCalledWith(ROWS[3].id, WALLET);
      expect(open).not.toHaveBeenCalled();
      fireEvent.click(surface(ROWS[1].id), { metaKey: true });
      expect(open).toHaveBeenCalledTimes(1);
      for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'false');
    },
  );

  it('drops a Control selection when the visible Groups page changes', async () => {
    finePointer();
    vi.spyOn(window.navigator, 'platform', 'get').mockReturnValue('Win32');
    const { user } = mount(shelves(), undefined, '', STORY_NEWER_PROJECT.rows);
    await band('Band 00');
    fireEvent.click(surface(ROWS[0].id), { ctrlKey: true });
    fireEvent.click(surface(ROWS[2].id), { ctrlKey: true });
    const groups = (await screen.findByText('Groups')).parentElement as HTMLElement;
    const steps = within(groups).getByRole('navigation', {
      name: `Pages of ${STORY_NEWER_PROJECT.name} groups`,
    });
    await user.click(within(steps).getByRole('button', { name: 'Next page' }));
    await waitFor(() => expect(within(steps).getByText('Page 2 of 3')).toBeInTheDocument());
    expect(surface(ROWS[0].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
    fireEvent.click(surface(ROWS[3].id), { ctrlKey: true });
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    const carrier = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[3].id), { dataTransfer: carrier });
    expect(carrier.getData('application/vnd.vis.sessions+json')).toBe('');
  });

  it('opens a row instead of selecting on a touch-only device even with a modifier', async () => {
    vi.spyOn(window.navigator, 'platform', 'get').mockReturnValue('MacIntel');
    const { open } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id), { metaKey: true });
    fireEvent.click(surface(ROWS[1].id), { ctrlKey: true });
    expect(open).toHaveBeenCalledTimes(2);
    for (const row of ROWS) expect(surface(row.id)).toHaveAttribute('aria-pressed', 'false');
  });

  it('keeps visible selection when a group folds, without restoring hidden rows', async () => {
    finePointer();
    const { user } = mount();
    const wallet = await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    await user.click(within(wallet).getByRole('button', { name: 'Collapse Wallet work' }));
    expect(surface(ROWS[0].id)).toBeNull();
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    await user.click(within(wallet).getByRole('button', { name: 'Expand Wallet work' }));
    expect(surface(ROWS[0].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[1].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
  });

  it('keeps a loose selection while clicking an unrelated group header', async () => {
    finePointer();
    const { user, open } = mount();
    const wallet = await band('Wallet work');
    fireEvent.click(surface(ROWS[2].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    await user.click(within(wallet).getByRole('button', { name: 'Collapse Wallet work' }));
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    await user.click(within(wallet).getByRole('button', { name: 'Expand Wallet work' }));
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
    expect(open).toHaveBeenCalledTimes(1);
  });

  it('drags a selected range with every row visible and files only rows outside the target group', async () => {
    finePointer();
    const { client } = mount();
    const wallet = await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    const carrier = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[3].id), { dataTransfer: carrier });
    expect(carrier.setData).toHaveBeenCalledWith(
      'application/vnd.vis.sessions+json', JSON.stringify(ROWS.map((row) => row.id)),
    );
    expect(carrier.getData('text/plain')).toBe(ROWS[3].id);
    const picture = carrier.setDragImage.mock.calls[0][0] as HTMLElement;
    expect(picture).toHaveTextContent('4 sessions');
    expect(picture.querySelectorAll('[data-row-surface]')).toHaveLength(ROWS.length);
    for (const item of ROWS) expect(picture).toHaveTextContent(item.title!);
    fireEvent.drop(wallet, { dataTransfer: carrier });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledTimes(2));
    expect(client.assignSessionGroup).toHaveBeenNthCalledWith(1, ROWS[2].id, WALLET);
    expect(client.assignSessionGroup).toHaveBeenNthCalledWith(2, ROWS[3].id, WALLET);
    await waitFor(() => expect(wallet.querySelectorAll('[data-session-row]')).toHaveLength(4));
    for (const item of ROWS) expect(surface(item.id)).toHaveAttribute('aria-pressed', 'true');
    const next = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[0].id), { dataTransfer: next });
    expect(next.getData('application/vnd.vis.sessions+json')).toBe(JSON.stringify(ROWS.map((row) => row.id)));
  });

  it('ungroups a selected batch but drags an unselected row alone', async () => {
    finePointer();
    const { client } = mount();
    await band('Wallet work');
    fireEvent.click(surface(ROWS[0].id));
    fireEvent.click(surface(ROWS[1].id), { shiftKey: true });
    const single = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[2].id), { dataTransfer: single });
    expect(single.setData).not.toHaveBeenCalledWith('application/vnd.vis.sessions+json', expect.anything());
    const sessions = screen.getByText('Sessions').parentElement!.parentElement as HTMLElement;
    const batch = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[1].id), { dataTransfer: batch });
    fireEvent.drop(sessions, { dataTransfer: batch });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledTimes(2));
    expect(client.assignSessionGroup).toHaveBeenNthCalledWith(1, ROWS[0].id, null);
    expect(client.assignSessionGroup).toHaveBeenNthCalledWith(2, ROWS[1].id, null);
    await waitFor(() => expect(surface(ROWS[0].id)).toHaveAttribute('aria-pressed', 'true'));
    expect(surface(ROWS[1].id)).toHaveAttribute('aria-pressed', 'true');
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
  });

  it('does not restore moved selection when the target group is closed', async () => {
    finePointer();
    const { user, client } = mount();
    const wallet = await band('Wallet work');
    fireEvent.click(surface(ROWS[2].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    await user.click(within(wallet).getByRole('button', { name: 'Collapse Wallet work' }));
    const batch = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[3].id), { dataTransfer: batch });
    fireEvent.drop(wallet, { dataTransfer: batch });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledTimes(2));
    await waitFor(() => expect(surface(ROWS[3].id)).toBeNull());
    await user.click(within(wallet).getByRole('button', { name: 'Expand Wallet work' }));
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'false');
  });

  it('reports partial failures and leaves unsuccessful sessions selected for retry', async () => {
    finePointer();
    const client = machine({
      assignSessionGroup: vi.fn(async (sid: string, gid: string | null) => {
        if (sid === ROWS[3].id) throw new Error('unreachable');
        return { ...ROWS.find((row) => row.id === sid)!, group_id: gid };
      }),
    });
    mount(client);
    const wallet = await band('Wallet work');
    fireEvent.click(surface(ROWS[2].id));
    fireEvent.click(surface(ROWS[3].id), { shiftKey: true });
    const batch = dragCarrier();
    fireEvent.dragStart(strip(document.body, ROWS[2].id), { dataTransfer: batch });
    fireEvent.drop(wallet, { dataTransfer: batch });
    expect(await screen.findByRole('alert')).toHaveTextContent('1 of 2 sessions could not be moved');
    expect(client.assignSessionGroup).toHaveBeenCalledTimes(2);
    expect(surface(ROWS[2].id)).toHaveAttribute('aria-pressed', 'false');
    expect(surface(ROWS[3].id)).toHaveAttribute('aria-pressed', 'true');
  });

  it('ignores foreign batches and malformed drag data', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const foreign = dragCarrier();
    foreign.setData('application/vnd.vis.sessions+json', JSON.stringify(['another-project']));
    foreign.setData('text/plain', ROWS[2].id);
    fireEvent.drop(wallet, { dataTransfer: foreign });
    const invalid = dragCarrier();
    invalid.setData('application/vnd.vis.sessions+json', '{broken');
    invalid.setData('text/plain', ROWS[2].id);
    fireEvent.drop(wallet, { dataTransfer: invalid });
    expect(client.assignSessionGroup).not.toHaveBeenCalled();
  });

  it('files a session dropped onto a group band', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const header = within(wallet).getByRole('button', { name: 'Collapse Wallet work' })
      .parentElement as HTMLElement;
    const dataTransfer = { getData: () => LOOSE.id, setData: vi.fn(), dropEffect: '' };
    fireEvent.dragOver(header, { dataTransfer });
    fireEvent.drop(header, { dataTransfer });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1),
    );
  });

  it('files a session dropped onto any row in a group', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const row = strip(wallet, ROWS[0].id);
    expect(wallet).toHaveAttribute('data-session-drop');
    const dataTransfer = { getData: () => LOOSE.id, setData: vi.fn(), dropEffect: '' };
    fireEvent.dragOver(row, { dataTransfer });
    expect(wallet).toHaveClass('bg-white/10');
    // Crossing between rows is still inside this group's drop area.
    const leave = new Event('dragleave', { bubbles: true });
    Object.defineProperty(leave, 'relatedTarget', { value: strip(wallet, ROWS[1].id) });
    fireEvent(row, leave);
    expect(wallet).toHaveClass('bg-white/10');
    fireEvent.drop(row, { dataTransfer });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1),
    );
  });

  it('takes a session out of its group over a loose row or the rest of the Sessions area', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const sessions = screen.getByText('Sessions').parentElement!.parentElement as HTMLElement;
    const row = strip(sessions, LOOSE.id);
    expect(sessions).toHaveAttribute('data-session-drop');
    const dataTransfer = { getData: () => ROWS[0].id, setData: vi.fn(), dropEffect: '' };
    fireEvent.dragOver(row, { dataTransfer });
    expect(sessions).toHaveClass('bg-white/10');
    // Hovering a loose row highlights the drop area without adding a banner beside Sessions.
    expect(within(sessions).queryByText('Drop to ungroup')).toBeNull();
    fireEvent.drop(row, { dataTransfer });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(ROWS[0].id, null));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${ROWS[0].id}"]`)).toHaveLength(0),
    );

    const other = ROWS[1].id;
    fireEvent.drop(sessions, {
      dataTransfer: { getData: () => other, setData: vi.fn(), dropEffect: '' },
    });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(other, null));
  });
  it('finds the full group and Sessions areas under a carried row on touch', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const sessions = screen.getByText('Sessions').parentElement!.parentElement as HTMLElement;
    // jsdom does not lay out the list; give the two real drop areas their screen boxes.
    for (const [area, top, bottom] of [
      [wallet, 100, 400],
      [sessions, 400, 700],
    ] as const) {
      vi.spyOn(area, 'getBoundingClientRect').mockReturnValue({
        left: 0,
        right: 390,
        top,
        bottom,
        width: 390,
        height: bottom - top,
      } as DOMRect);
    }
    const at = (y: number) =>
      dropTargetAt({ x: 100, y }, document.querySelectorAll(`[${DROP_TARGET_ATTRIBUTE}]`));
    expect(at(260)).toBe(wallet.getAttribute(DROP_TARGET_ATTRIBUTE));
    expect(at(550)).toBe(sessions.getAttribute(DROP_TARGET_ATTRIBUTE));

    act(() => {
      beginLift(LOOSE.id);
      carryOver(at(260));
      releaseLift();
    });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET));

    act(() => {
      beginLift(ROWS[0].id);
      carryOver(at(550));
      releaseLift();
    });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(ROWS[0].id, null));
  });

  // A group's area files into itself; the loose Sessions area takes a filed row back out.
  it('takes a session out of its group when it is dropped on the ungrouped set', async () => {
    const { client } = mount();
    const wallet = await band('Wallet work');
    const sessions = screen.getByText('Sessions').parentElement as HTMLElement;
    const dataTransfer = { getData: () => ROWS[0].id, setData: vi.fn(), dropEffect: '' };
    fireEvent.dragOver(sessions, { dataTransfer });
    expect(within(sessions).queryByText('Drop to ungroup')).toBeNull();
    fireEvent.drop(sessions, { dataTransfer });
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(ROWS[0].id, null));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${ROWS[0].id}"]`)).toHaveLength(0),
    );
    expect(screen.getByText('Sessions').parentElement).not.toHaveTextContent('Drop to ungroup');
  });

  // A row that is in no group is already where this drop would put it.
  it('leaves an ungrouped session alone when it is dropped on the ungrouped set', async () => {
    const { client } = mount();
    await band('Wallet work');
    const sessions = screen.getByText('Sessions').parentElement as HTMLElement;
    const dataTransfer = { getData: () => LOOSE.id, setData: vi.fn(), dropEffect: '' };
    fireEvent.drop(sessions, { dataTransfer });
    expect(client.assignSessionGroup).not.toHaveBeenCalled();
  });

  // THE WALL OF BANDS IS A LIST OF ITS OWN. A reader can keep making groups, so the wall is
  // paged the way the sessions under it are — and the steps stand on the `Groups` header,
  // over the set they move.
  it('pages the wall of bands from the header over it', async () => {
    mount(shelves(), undefined, '', STORY_NEWER_PROJECT.rows);

    const groups = (await screen.findByText('Groups')).parentElement as HTMLElement;
    const emptyFirst = (await screen.findByRole('button', { name: 'Collapse Band 00' })).parentElement!;
    const emptyNext = (await screen.findByRole('button', { name: 'Collapse Band 01' })).parentElement!;
    // Empty bands get a single edge from the set header or the next band, never two.
    expect(emptyFirst).not.toHaveClass('border-t');
    expect(emptyFirst).not.toHaveClass('border-b');
    expect(emptyNext).toHaveClass('border-t');
    expect(emptyNext).not.toHaveClass('border-b');
    // Removing the visible tally does not change the underlying number of group pages.
    expect(within(groups).queryByText('24 groups')).toBeNull();
    const steps = within(groups).getByRole('navigation', {
      name: `Pages of ${STORY_NEWER_PROJECT.name} groups`,
    });
    expect(within(steps).getByText('Page 1 of 3')).toBeInTheDocument();
    // One page of sessions is one page: that set has nothing to step through.
    const sessions = screen.getByText('Sessions').parentElement as HTMLElement;
    expect(within(sessions).queryByRole('navigation')).toBeNull();
  });

  // ONE WINDOW, BOTH READS. The rows are asked for with the window that cut the bands, so
  // the sessions under this page of shelves are the ones the gateway files there.
  it('asks both reads for the page of bands the reader stepped to', async () => {
    const client = shelves();
    const { user } = mount(client, undefined, '', STORY_NEWER_PROJECT.rows);
    const groups = (await screen.findByText('Groups')).parentElement as HTMLElement;
    expect(client.listSessionGroups).toHaveBeenLastCalledWith(
      ROOT,
      expect.any(AbortSignal),
      'exclude',
      BANDS,
    );

    await user.click(within(groups).getByRole('button', { name: 'Next page' }));

    await screen.findByText('Band 10');
    expect(screen.queryByText('Band 00')).toBeNull();
    const turned = { limit: GROUPS_PAGE, offset: GROUPS_PAGE };
    expect(client.listSessionGroups).toHaveBeenLastCalledWith(
      ROOT,
      expect.any(AbortSignal),
      'exclude',
      turned,
    );
    expect(client.listProjectPage).toHaveBeenLastCalledWith(
      ROOT,
      10,
      '',
      expect.any(Map),
      expect.any(AbortSignal),
      true,
      'exclude',
      turned,
    );
  });

  it('keeps the Sessions page when the Groups archive opens', async () => {
    const deep = { rows: ROWS, total: 24, awaiting: [], grouped: [], nextCursor: '' };
    const client = machine({ heldProjectPage: () => deep, listProjectPage: vi.fn(async () => deep) });
    const { user } = mount(client);
    const sessions = (await screen.findByText('Sessions')).parentElement as HTMLElement;
    const steps = within(sessions).getByRole('navigation', {
      name: `Pages of ${STORY_NEWER_PROJECT.name} sessions`,
    });
    await user.click(within(steps).getByRole('button', { name: 'Next page' }));
    await waitFor(() => expect(within(steps).getByText('Page 2 of 3')).toBeInTheDocument());

    await user.click(screen.getByRole('button', { name: `Actions for groups in ${ROOT}` }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Show archived groups'));
    await waitFor(() =>
      expect(client.listSessionGroups).toHaveBeenLastCalledWith(
        ROOT, expect.any(AbortSignal), 'only', BANDS,
      ),
    );
    expect(within((await screen.findByText('Groups')).parentElement!).getByText('Archived')).toHaveClass(
      'font-mono', 'text-ui', 'text-white',
    );
    expect(within(steps).getByText('Page 2 of 3')).toBeInTheDocument();
  });

  it('keeps the Groups page when the Sessions archive opens', async () => {
    const client = shelves();
    const { user } = mount(client, undefined, '', STORY_NEWER_PROJECT.rows);
    const groups = (await screen.findByText('Groups')).parentElement as HTMLElement;
    const steps = within(groups).getByRole('navigation', {
      name: `Pages of ${STORY_NEWER_PROJECT.name} groups`,
    });
    await user.click(within(steps).getByRole('button', { name: 'Next page' }));
    await screen.findByText('Band 10');

    await user.click(screen.getByRole('button', { name: `Actions for sessions in ${ROOT}` }));
    await user.click(within(sheet(`Sessions in ${ROOT}`)).getByText('Show archived sessions'));
    await waitFor(() => expect(within(steps).getByText('Page 2 of 3')).toBeInTheDocument());
    expect(within((await screen.findByText('Sessions')).parentElement!).getByText('Archived')).toHaveClass(
      'font-mono', 'text-ui', 'text-white',
    );
    expect(client.listSessionGroups).toHaveBeenLastCalledWith(
      ROOT, expect.any(AbortSignal), 'exclude', { limit: GROUPS_PAGE, offset: GROUPS_PAGE },
    );
  });

  // Reported over the project header while the pager stood on its trailing edge: a paged
  // project put its plus and its menu in the middle of the band while an unpaged one kept
  // them flush right. The steps stand over the SET they move now, and the band is one shape.
  it('stands the session steps on the Sessions header, not on the project band', async () => {
    const deep = { rows: ROWS, total: 24, awaiting: [], grouped: [], nextCursor: '' };
    mount(machine({ heldProjectPage: () => deep, listProjectPage: vi.fn(async () => deep) }));

    const sessions = (await screen.findByText('Sessions')).parentElement as HTMLElement;
    const steps = within(sessions).getByRole('navigation', {
      name: `Pages of ${STORY_NEWER_PROJECT.name} sessions`,
    });
    expect(within(steps).getByText('Page 1 of 3')).toBeInTheDocument();
    expect(steps.nextElementSibling).toBe(
      within(sessions).getByRole('button', { name: `Actions for sessions in ${ROOT}` }),
    );
    const header = screen.getByText(STORY_NEWER_PROJECT.name).closest('header') as HTMLElement;
    expect(within(header).queryByRole('navigation')).toBeNull();
  });
});
