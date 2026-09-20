// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { Session, SessionGroup } from '../../lib/types';
import { ProjectGroup } from './SessionProjectGroups';

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
  const page = { rows: ROWS, total: ROWS.length, awaiting: [], nextCursor: '' };
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

function mount(client: Machine = machine()) {
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
      creation={{ state: null, start: vi.fn(async () => {}) }}
      initiallyOpen
    />,
  );
  return { client, user: userEvent.setup() };
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
    expect(wallet.querySelectorAll('.bg-group-blue')).toHaveLength(1);
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

  it('files a session from the band menu and paints it in that band', async () => {
    const { client, user } = mount();
    const wallet = await band('Wallet work');
    await user.click(screen.getByRole('button', { name: 'Actions for Wallet work' }));
    await user.click(within(sheet(`Groups in ${ROOT}`)).getByText(String(LOOSE.title)));
    await waitFor(() =>
      expect(client.assignSessionGroup).toHaveBeenCalledWith(LOOSE.id, WALLET),
    );
    await waitFor(() =>
      expect(wallet.querySelectorAll(`[data-session-id="${LOOSE.id}"]`)).toHaveLength(1),
    );
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
