import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_FLEET_CONNS,
  STORY_NEWER_PROJECT,
  STORY_PROJECT_CLIENT,
  storyFleetFetch,
} from '../../dev/story-data';
import { machineKey } from '../../lib/fleet';
import { projectFoldKey, projectRevealKey, writeProjectFold } from '../../lib/project-fold';
import type { Session, SessionGroup } from '../../lib/types';
import { ProjectGroup } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const fixture = STORY_NEWER_PROJECT;
const MENU = `Groups in ${fixture.root}`;

/** Still on the live list, and one of the sessions filed under it was archived inside it. */
const WALLET: SessionGroup = {
  id: 'group-wallet',
  project_id: fixture.projectId,
  name: 'Wallet work',
  color: 'blue',
  position: 0,
  session_count: 2,
};

/** Archived whole, with its session: a band the project paints only while revealing. */
const RECEIPTS: SessionGroup = {
  id: 'group-receipts',
  project_id: fixture.projectId,
  name: 'Receipts',
  color: 'amber',
  position: 1,
  session_count: 1,
  archived_at: 1730000000,
};

const [FILED, FILED_AWAY, SHELVED_AWAY, LOOSE_AWAY]: Session[] = [
  { ...fixture.rows[0], group_id: WALLET.id },
  { ...fixture.rows[1], group_id: WALLET.id, archived_at: 1730000100 },
  { ...fixture.rows[2], group_id: RECEIPTS.id, archived_at: 1730000200 },
  { ...fixture.rows[3], archived_at: 1730000300 },
];
const ROWS = [FILED, FILED_AWAY, SHELVED_AWAY, LOOSE_AWAY];

/** What this device holds of the project: its LIVE window, which the archive is not in. */
const HELD = [FILED];

const meta = {
  title: 'Session/Project archive',
  component: ProjectGroup,
  parameters: { layout: 'fullscreen' },
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([{ ...fixture, rows: ROWS, groups: [WALLET, RECEIPTS] }]);
    writeProjectFold(projectFoldKey(machineKey(conn), fixture.root), true);
    // The reveal is this device's decision and it outlives a screen, so every story here
    // starts on the live list and opens the archive itself.
    writeProjectFold(projectRevealKey(machineKey(conn), fixture.root), false);
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    group: {
      root: fixture.root,
      label: fixture.name,
      projectId: fixture.projectId,
      tally: { count: HELD.length, live: 0, awaiting: 0, unread: 0 },
      sessions: HELD,
    },
    machine: { conn, sessions: HELD },
    context: {
      getClient: () => STORY_PROJECT_CLIENT,
      drafts: {},
      matches: null,
      needle: '',
      openRow: null,
      actions: {
        commands: { open: fn(), rename: fn(async () => {}), requestDelete: fn(), toggleStar: fn() },
        deletion: { target: null, isBusy: false, error: null, confirm: fn(), cancel: fn() },
      },
    },
    reading: {
      pageSize: 10,
      epoch: null,
      admitted: new Set<string>(),
      isVisible: true,
      pendingByRoot: new Map(),
      acceptUpdates: fn(),
    },
    creation: { state: null, start: fn(async () => {}) },
    initiallyOpen: true,
  },
  render: (args) => (
    <div className="@container min-h-dvh bg-page">
      <ProjectGroup {...args} />
    </div>
  ),
} satisfies Meta<typeof ProjectGroup>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Every session this project is painting, in the order it paints them. */
const painted = (scope: HTMLElement) =>
  Array.from(scope.querySelectorAll('[data-session-id]')).map((slab) =>
    slab.getAttribute('data-session-id'),
  );

/** The band and the rows filed under it, as one block inside the project's list. */
const band = async (page: ReturnType<typeof within>, name: string) => {
  const disclosure = await page.findByRole('button', { name: `Collapse ${name}` });
  return disclosure.closest('div')!.parentElement as HTMLElement;
};

export const LiveList: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const wallet = await band(page, 'Wallet work');

    // What was put away is not on this list: the live group paints the one session left in
    // it, the archived group has no band here, and the archived loose session is not below.
    await expect(painted(wallet)).toEqual([FILED.id]);
    await expect(painted(canvasElement)).toEqual([FILED.id]);
    await expect(within(wallet).getByText('1 session')).toBeVisible();
    await expect(page.queryByRole('button', { name: 'Collapse Receipts' })).toBeNull();
  },
};

export const ShowsWhatItArchived: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const sheets = within(canvasElement.ownerDocument.body);
    await band(page, 'Wallet work');

    await userEvent.click(await page.findByRole('button', { name: MENU }));
    await userEvent.click(within(sheets.getByRole('dialog', { name: MENU })).getByText('Show archived'));

    // The archive is a place the reader goes to: the archived group's band, then the
    // sessions archived on their own, and none of the live list underneath it.
    const receipts = await band(page, 'Receipts');
    await expect(painted(receipts)).toEqual([SHELVED_AWAY.id]);
    await expect(painted(canvasElement)).toEqual([SHELVED_AWAY.id, LOOSE_AWAY.id]);
    // The session archived INSIDE the live group stays that group's business.
    await expect(page.queryByRole('button', { name: 'Collapse Wallet work' })).toBeNull();

    const header = canvasElement.querySelector('header')!;
    await expect(within(header).getByText('Archived')).toBeVisible();
    await expect(within(header).getByText('2 sessions')).toBeVisible();
  },
};
