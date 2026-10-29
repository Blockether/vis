import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_FLEET_CONNS,
  STORY_NEWER_PROJECT,
  STORY_PROJECT_CLIENT,
  storyFleetFetch,
} from '../../dev/story-data';
import { machineKey } from '../../lib/fleet';
import { projectFoldKey, writeProjectFold } from '../../lib/project-fold';
import type { SessionGroup } from '../../lib/types';
import { ProjectGroup } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const fixture = STORY_NEWER_PROJECT;

const meta = {
  title: 'Session list/Projects',
  component: ProjectGroup,
  parameters: { layout: 'fullscreen' },
  beforeEach: ({ args }) => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([{ ...fixture, rows: args.group.sessions }]);
    writeProjectFold(projectFoldKey(machineKey(conn), fixture.root), args.initiallyOpen);
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    group: {
      root: fixture.root,
      label: fixture.name,
      projectId: fixture.projectId,
      tally: { count: fixture.rows.length, live: 0, awaiting: 0, unread: 0 },
      sessions: fixture.rows,
    },
    machine: { conn, sessions: fixture.rows },
    context: {
      getClient: () => STORY_PROJECT_CLIENT,
      drafts: {},
      previewId: null,
      preview: null,
      needle: '',
      openRow: null,
      actions: {
        commands: { open: fn(), rename: fn(async () => {}), requestDelete: fn(), toggleStar: fn() },
        deletion: { target: null, isBusy: false, error: null, confirm: fn(), cancel: fn() },
      },
    },
    reading: {
      pageSize: 10,
      isVisible: true,
    },
    creation: { state: null, start: fn(async () => {}) },
    initiallyOpen: true,
  },
  render: function Render(args) {
    return (
      <div className="@container min-h-dvh bg-page">
        <ProjectGroup {...args} />
      </div>
    );
  },
} satisfies Meta<typeof ProjectGroup>;

export default meta;
type Story = StoryObj<typeof meta>;

export const NewerSession: Story = {};

export const Collapsed: Story = { args: { initiallyOpen: false } };

/** The bands this project HAS. A row names one; the name and the colour live here. */
const GROUPED_BANDS: SessionGroup[] = [
  {
    id: 'wallet',
    project_id: fixture.projectId,
    name: 'Wallet work',
    color: 'blue',
    session_count: 2,
  },
  {
    id: 'receipts',
    project_id: fixture.projectId,
    name: 'Receipts',
    color: 'amber',
    session_count: 1,
  },
];

/** Groups nest INSIDE the project: named bands first, then whatever nobody filed. */
const GROUPED = [
  { ...fixture.rows[0], group_id: 'wallet' },
  { ...fixture.rows[1], group_id: 'wallet' },
  { ...fixture.rows[2], group_id: 'receipts' },
  fixture.rows[3],
];

export const Groups: Story = {
  // The rows name their band and nothing more, so this story serves the GROUPS as well:
  // that is where a band takes its name and its colour from.
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([{ ...fixture, rows: GROUPED, groups: GROUPED_BANDS }]);
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    group: { ...meta.args.group, sessions: GROUPED },
    machine: { conn, sessions: GROUPED },
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const wallet = await page.findByRole('button', { name: /(Collapse|Expand) Wallet work/ });
    const band = wallet.closest('div')!.parentElement!;
    await expect(within(band).queryByText('2 sessions')).toBeNull();
    await expect(band.querySelectorAll('[data-session-id]')).toHaveLength(2);
    await expect(page.getByRole('button', { name: /(Collapse|Expand) Receipts/ })).toBeVisible();
    // A group folds on its own, and the rest of the project stays where it was.
    await userEvent.click(page.getByRole('button', { name: 'Collapse Wallet work' }));
    await expect(band.querySelectorAll('[data-session-id]')).toHaveLength(0);
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(2);
    await userEvent.click(page.getByRole('button', { name: 'Expand Wallet work' }));
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(4);
    // The Groups header owns group creation; the project header has no menu.
    await userEvent.click(
      page.getByRole('button', { name: `Actions for groups in ${fixture.root}` }),
    );
    const sheet = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: `Groups in ${fixture.name}`,
    });
    await expect(within(sheet).getByText('New group')).toBeVisible();
    await expect(within(sheet).queryByText('Wallet work')).toBeNull();
    await expect(within(sheet).queryByText('Receipts')).toBeNull();
  },
};

// THE BAND'S OWN ⋮ CARRIES THE VERBS — one mark each, and nothing that only repeats the
// name the reader pressed. The palette waits a step behind a row that names the verb.
export const GroupVerbs: Story = {
  ...Groups,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await userEvent.click(await page.findByRole('button', { name: 'Actions for Wallet work' }));
    const sheet = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: `Groups in ${fixture.name}`,
    });
    await expect(within(sheet).getByText('Rename group')).toBeVisible();
    const newSession = within(sheet).getByRole('button', { name: 'New session' });
    await expect(newSession.querySelector('svg.lucide-square-pen')).not.toBeNull();
    await expect(within(sheet).getByText('Delete group')).toBeVisible();
    await expect(within(sheet).queryByText('Wallet work')).toBeNull();
    await expect(within(sheet).queryByRole('button', { name: 'Slate' })).toBeNull();
    // Eight tiles arrive only when asked for, under no band of their own, with the
    // one in use wearing the frame.
    await userEvent.click(within(sheet).getByText('Choose colour'));
    await expect(within(sheet).queryByText(/^Colour /)).toBeNull();
    await expect(within(sheet).getByRole('button', { name: 'Slate' })).toBeVisible();
    await expect(within(sheet).getByRole('button', { name: 'Blue' })).toHaveAttribute(
      'aria-pressed',
      'true',
    );
  },
};
