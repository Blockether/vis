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
    // Regression: accepting arrivals must not resize the project header.
    const pendingBounds = header.getBoundingClientRect();
    const style = (element: Element) => getComputedStyle(element);
    const rows = header.nextElementSibling!;

    // The update action shares the regular band, outside the project disclosure.
    await expect(updates.closest('button[aria-expanded]')).toBeNull();
    const updateBounds = updates.getBoundingClientRect();
    await expect(updateBounds.left).toBeGreaterThanOrEqual(
      disclosure.getBoundingClientRect().right,
    );
    await expect(updateBounds.top).toBeGreaterThanOrEqual(pendingBounds.top);
    await expect(updateBounds.bottom).toBeLessThanOrEqual(pendingBounds.bottom);
    await expect(header.scrollWidth).toBe(header.clientWidth);
    for (const control of header.querySelectorAll('button, input')) {
      const bounds = control.getBoundingClientRect();
      await expect(bounds.left).toBeGreaterThanOrEqual(pendingBounds.left);
      await expect(bounds.right).toBeLessThanOrEqual(pendingBounds.right);
      await expect(
        canvasElement.ownerDocument
          .elementFromPoint(bounds.x + bounds.width / 2, bounds.y + bounds.height / 2)
          ?.closest('button, input'),
      ).toBe(control);
    }
    await expect(title.getBoundingClientRect().width).toBeGreaterThan(0);
    await expect(style(updates).backgroundColor).toBe('rgba(0, 0, 0, 0)');
    await expect(style(updates).borderTopColor).toBe('rgba(0, 0, 0, 0)');
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderTopWidth).toBe('0px');
    await expect(style(rows.firstElementChild!).borderTopWidth).toBe('0px');
    await expect(within(header).getByText(`${args.group.tally.count} sessions`)).toBeVisible();
    const pageCount = Math.ceil(args.group.tally.count / args.reading.pageSize);
    if (pageCount > 1) {
      const pager = within(header).getByRole('navigation', {
        name: 'Pages of /CryptoSafe sessions',
      });
      await expect(pager).toBeVisible();
      await expect(within(pager).getByText(`Page 1 of ${pageCount}`)).toBeInTheDocument();
    }
    await expect(
      within(updates).getByText(pendingBounds.width < 512 ? '1 new' : '1 newer session'),
    ).toBeVisible();

    await userEvent.click(page.getByRole('button', { name: 'Collapse /CryptoSafe' }));
    await expect(canvasElement.querySelector('[data-session-id]')).toBeNull();
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(header.getBoundingClientRect().height).toBe(pendingBounds.height);
    updates.focus();
    await userEvent.keyboard('{Enter}');
    await expect(args.reading.acceptUpdates).toHaveBeenCalledWith(pendingIds);
    await expect(await page.findByText('Check transaction confirmations')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Collapse /CryptoSafe' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(
      Math.min(args.reading.pageSize, args.group.sessions.length),
    );
    await expect(page.queryByRole('button', { name: /newer session/ })).toBeNull();
    await expect(style(header).borderBottomWidth).toBe('1px');
    const acceptedBounds = header.getBoundingClientRect();
    await expect({ width: acceptedBounds.width, height: acceptedBounds.height }).toEqual({
      width: pendingBounds.width,
      height: pendingBounds.height,
    });
  },
};

// The reported header had a three-digit page total and more than a thousand sessions.
const pagedRows = [
  ...fixture.rows,
  ...Array.from({ length: 1460 }, (_, index) => ({
    ...fixture.rows[fixture.rows.length - 1],
    id: `header-layout-${index}`,
  })),
];
const pagedArgs = {
  group: {
    ...meta.args.group,
    tally: { count: pagedRows.length, live: 2, awaiting: 0, unread: 0 },
    sessions: pagedRows,
  },
  machine: { conn, sessions: pagedRows },
  reading: { ...meta.args.reading, pageSize: 14 },
};

export const PhoneWithPaging: Story = {
  ...AcceptNewerSession,
  args: pagedArgs,
};

export const SmallPhoneWithPaging: Story = {
  ...PhoneWithPaging,
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
};

export const DesktopWithPaging: Story = {
  ...PhoneWithPaging,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};
