// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { Session, SessionGroup } from '../../lib/types';
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

/** Two of this project's sessions are filed under one group; the rest are not. */
const ROWS: Session[] = STORY_NEWER_PROJECT.rows.map((row, index) =>
  index < 2
    ? { ...row, group_id: WALLET, group_name: WALLET_GROUP.name, group_color: WALLET_GROUP.color }
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
    listSessionGroups: vi.fn(async () => ({
      project_id: STORY_NEWER_PROJECT.projectId,
      groups: [WALLET_GROUP],
    })),
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
      group_name: gid === null ? null : WALLET_GROUP.name,
      group_color: gid === null ? null : WALLET_GROUP.color,
    })),
    ...overrides,
  };
}

function mount(client: Machine = machine(), creation?: ProjectCreation) {
  const started: ProjectCreation = creation ?? { state: null, start: vi.fn(async () => {}) };
  render(
    <ProjectGroup
      group={{
        root: ROOT,
        label: STORY_NEWER_PROJECT.name,
        projectId: STORY_NEWER_PROJECT.projectId,
        tally: { count: ROWS.length, live: 0, awaiting: 0, unread: 0 },
        sessions: ROWS,
      }}
      machine={{ conn, sessions: ROWS }}
      context={{
        getClient: () => client as unknown as GatewayClient,
        drafts: {},
        matches: null,
        needle: '',
        actions: {
          commands: {
            open: vi.fn(),
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
    />,
  );
  return { client, creation: started, user: userEvent.setup() };
}

/** The band and the rows filed under it, as one block inside the project's list. */
async function band(name: string): Promise<HTMLElement> {
  const disclosure = await screen.findByRole('button', { name: `Collapse ${name}` });
  return disclosure.closest('div')!.parentElement as HTMLElement;
}

const sheet = (label: string) => screen.getByRole('dialog', { name: label });

describe('ProjectGroup groups', () => {
  // A fold is REMEMBERED (`lib/project-fold`), so one test's shut band must not
  // arrive shut in the next one.
  beforeEach(() => window.localStorage.clear());

  it('nests a group band under the project header, with its colour and the gateway count', async () => {
    mount();
    const wallet = await band('Wallet work');
    expect(within(wallet).getByText('2 sessions')).toBeInTheDocument();
    // The band wears its colour twice over: the swatch beside its name, and the rail
    // down the leading edge it shares with every row filed under it (BLO-167).
    expect(wallet.querySelectorAll('.bg-group-blue').length).toBeGreaterThanOrEqual(4);
    expect([...wallet.querySelectorAll('[data-session-id]')].map((row) =>
      row.getAttribute('data-session-id'),
    )).toEqual([ROWS[0].id, ROWS[1].id]);
    // What nobody filed keeps the project's own order, below every band.
    expect(wallet.querySelector(`[data-session-id="${LOOSE.id}"]`)).toBeNull();
    const list = wallet.parentElement as HTMLElement;
    const painted = [...list.querySelectorAll('[data-session-id]')].map((row) =>
      row.getAttribute('data-session-id'),
    );
    expect(painted).toEqual([ROWS[0].id, ROWS[1].id, ROWS[2].id, ROWS[3].id]);
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
      group_name: WALLET_GROUP.name,
      group_color: WALLET_GROUP.color,
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
    const list = wallet.parentElement as HTMLElement;
    expect(
      [...list.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).toEqual([ROWS[0].id, ROWS[1].id, offPage.id, ROWS[2].id, ROWS[3].id]);
  });

  // BLO-167: a session started ON a band is minted inside that group, so it opens at
  // the top of the band the reader asked on instead of loose in the project.
  it('starts a session inside the group whose band offered the plus', async () => {
    const start = vi.fn(async () => {});
    const { user } = mount(machine(), { state: null, start });

    await user.click(await screen.findByRole('button', { name: 'New session in Wallet work' }));

    expect(start).toHaveBeenCalledWith(conn, ROOT, WALLET);
  });

  it('spins only the band that asked, never the project header above it', async () => {
    mount(machine(), {
      state: { at: `https://story.example.com\u0000${ROOT}\u0000${WALLET}`, label: 'Creating...' },
      start: vi.fn(async () => {}),
    });

    expect(await screen.findByRole('button', { name: 'New session in Wallet work' })).toBeDisabled();
    expect(screen.getByRole('button', { name: 'New session on tower' })).toBeEnabled();
  });

  // Regression, user report (paraphrased: a plus standing on the left is unacceptable,
  // the three dots belong on the right): every band ends with its own menu, and the
  // plus that starts a session stands one slot inside it.
  it('ends a band with its menu, the plus one slot inside', async () => {
    mount();
    const wallet = await band('Wallet work');
    const cluster = within(wallet).getByRole('button', { name: 'Actions for Wallet work' })
      .parentElement as HTMLElement;

    expect(Array.from(cluster.children).map((child) => child.getAttribute('aria-label'))).toEqual([
      'New session in Wallet work',
      'Actions for Wallet work',
    ]);
  });

  it('ends the project header with its menu, the plus one slot inside', async () => {
    mount();
    const cluster = (await screen.findByRole('button', { name: `Groups in ${ROOT}` }))
      .parentElement as HTMLElement;

    expect(Array.from(cluster.children).map((child) => child.getAttribute('aria-label'))).toEqual([
      'New session on tower',
      `Groups in ${ROOT}`,
    ]);
  });

  it('folds one group without folding the project', async () => {
    const { user } = mount();
    const wallet = await band('Wallet work');
    await user.click(within(wallet).getByRole('button', { name: 'Collapse Wallet work' }));
    expect(wallet.querySelector(`[data-session-id="${ROWS[0].id}"]`)).toBeNull();
    // The project stays open: only the band it was folded in lost its rows.
    const list = wallet.parentElement as HTMLElement;
    expect(list.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1);
  });

  it('creates a group from the project header menu', async () => {
    const { client, user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: `Groups in ${ROOT}` }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('New group'));
    await user.type(screen.getByLabelText('Group name'), 'Receipts');
    await user.click(screen.getByRole('button', { name: 'Create' }));
    await waitFor(() => expect(client.createSessionGroup).toHaveBeenCalledWith(ROOT, 'Receipts'));
  });

  // Reported (paraphrased: the project's own menu should not open by announcing that
  // a group is what gets made here): the sheet under that ⋮ carries the verb and
  // nothing else. The groups it used to list are the bands right below the header,
  // and each band's own ⋮ holds that group's verbs.
  it('offers the verb without naming the project back or listing its groups', async () => {
    const { user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: `Groups in ${ROOT}` }));
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
    await user.click(screen.getByRole('button', { name: `Groups in ${ROOT}` }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('New group'));
    await user.type(screen.getByLabelText('Group name'), 'Wallet work');
    await user.click(screen.getByRole('button', { name: 'Create' }));
    expect(
      await screen.findByText('This project already has a group with that name.'),
    ).toBeInTheDocument();
  });

  // A SESSION JOINS A GROUP FROM ITS OWN ROW ("Move to..."), never from the group's
  // settings: that sheet renames, recolours and deletes the group, and nothing else.
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

  // A GROUP'S COLOUR IS A PALETTE, NOT A COLUMN. Eight named rows stood open under the
  // verbs of every group sheet, each as tall as a verb and reading like one.
  it('picks a colour off a palette a step in and returns to the verbs', async () => {
    const { client, user } = mount();
    await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    const menu = sheet(`Groups in ${ROOT}`);
    expect(within(menu).queryByRole('button', { name: 'Slate' })).toBeNull();
    await user.click(within(menu).getByText('Choose colour'));
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
    // BOTH answers are spelled out; nothing has reached the machine yet.
    expect(screen.getByText('Keep its sessions')).toBeInTheDocument();
    expect(screen.getByText('Delete its sessions too')).toBeInTheDocument();
    expect(client.deleteSessionGroup).not.toHaveBeenCalled();
    await user.click(screen.getByText('Keep its sessions'));
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

  it('files a session from the row\'s own Move to... verb', async () => {
    const { client, user } = mount();
    const wallet = await band('Wallet work');
    const list = wallet.parentElement as HTMLElement;
    const slab = list.querySelector(`[data-session-id="${LOOSE.id}"]`) as HTMLElement;
    // The row's own action drawer carries the verb; the wrapper around it is what a
    // reader drags.
    const row = slab.closest('[draggable="true"]') as HTMLElement;
    await user.click(within(row).getByText('Move to...'));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText('Wallet work'));
    await waitFor(() => expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET));
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1),
    );
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
});
