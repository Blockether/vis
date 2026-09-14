import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_FLEET_CONNS,
  STORY_NEWER_PROJECT,
  STORY_PROJECT_CLIENT,
  storyFleetFetch,
} from '../../dev/story-data';
import { machineKey } from '../../lib/fleet';
import { projectFoldKey, writeProjectFold } from '../../lib/project-fold';
import { ProjectGroup } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const fixture = STORY_NEWER_PROJECT;
const epoch = fixture.rows.slice(1).map((row) => row.id);
const pendingIds = [fixture.rows[0].id];

const meta = {
  title: 'Session/Project updates',
  component: ProjectGroup,
  parameters: { layout: 'fullscreen' },
  beforeEach: ({ args }) => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([fixture]);
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
      matches: null,
      needle: '',
      actions: {
        commands: { open: fn(), rename: fn(async () => {}), requestDelete: fn(), toggleStar: fn() },
        deletion: { target: null, isBusy: false, error: null, confirm: fn(), cancel: fn() },
      },
    },
    reading: {
      pageSize: 10,
      epoch,
      admitted: new Set<string>(),
      isVisible: true,
      pendingByRoot: new Map([[fixture.root, pendingIds]]),
      acceptUpdates: fn(),
    },
    creation: { state: null, start: fn(async () => {}) },
    initiallyOpen: true,
  },
  render: function Render(args) {
    const [admitted, setAdmitted] = useState(new Set<string>());
    return (
      <div className="@container min-h-dvh bg-page">
        <ProjectGroup
          {...args}
          reading={{
            ...args.reading,
            admitted,
            pendingByRoot: admitted.size ? new Map() : args.reading.pendingByRoot,
            acceptUpdates: (ids) => {
              args.reading.acceptUpdates(ids);
              setAdmitted(new Set(ids));
            },
          }}
        />
      </div>
    );
  },
} satisfies Meta<typeof ProjectGroup>;

export default meta;
type Story = StoryObj<typeof meta>;

export const NewerSession: Story = {};

export const Collapsed: Story = { args: { initiallyOpen: false } };

/** A path-derived project name appears once; the second line carries only counts. */
export const ProjectPathName: Story = {
  args: {
    reading: { ...meta.args.reading, epoch: null, pendingByRoot: new Map() },
  },
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    const heading = page.getByRole('button', { name: `Collapse ${args.group.label}` });
    await expect(within(heading).getAllByText(args.group.label)).toHaveLength(1);
    await expect(heading.querySelector('[title]')?.textContent).toBe('4 sessions');
    await userEvent.click(heading);
    await expect(page.getByRole('button', { name: `Expand ${args.group.label}` })).toBeVisible();
    await expect(canvasElement.querySelector('[data-session-id]')).toBeNull();
    await userEvent.click(heading);
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(4);
  },
};

export const AcceptNewerSession: Story = {
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    await page.findByText('Fix balance refresh');
    const disclosure = page.getByRole('button', { name: 'Collapse /CryptoSafe' });
    const title = within(disclosure).getByText('/CryptoSafe');
    const header = title.closest('header')!;
    const updates = within(header).getByRole('button', { name: 'Show 1 newer session' });
    const style = (element: Element) => getComputedStyle(element);
    const rows = header.nextElementSibling!;

    // Regression: the update action is metadata, never a divider or a nested disclosure.
    await expect(updates.closest('button[aria-expanded]')).toBeNull();
    const text = canvasElement.ownerDocument.createRange();
    text.selectNodeContents(updates);
    await expect(
      Math.abs(text.getBoundingClientRect().left - title.getBoundingClientRect().left),
    ).toBeLessThan(1);
    await expect(updates.getBoundingClientRect().top).toBeGreaterThan(
      title.getBoundingClientRect().bottom,
    );
    await expect(style(updates).backgroundColor).toBe('rgba(0, 0, 0, 0)');
    await expect(style(updates).borderTopColor).toBe('rgba(0, 0, 0, 0)');
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderTopWidth).toBe('0px');
    await expect(style(rows.firstElementChild!).borderTopWidth).toBe('0px');
    await expect(within(header).getByText('4 sessions')).toBeVisible();

    await userEvent.click(page.getByRole('button', { name: 'Collapse /CryptoSafe' }));
    await expect(canvasElement.querySelector('[data-session-id]')).toBeNull();
    await expect(style(header).borderBottomWidth).toBe('1px');
    updates.focus();
    await userEvent.keyboard('{Enter}');
    await expect(args.reading.acceptUpdates).toHaveBeenCalledWith(pendingIds);
    await expect(await page.findByText('Check transaction confirmations')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Collapse /CryptoSafe' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(4);
    await expect(page.queryByRole('button', { name: /newer session/ })).toBeNull();
    await expect(style(header).borderBottomWidth).toBe('1px');
  },
};
