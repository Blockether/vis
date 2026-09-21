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
    const header = heading.closest('header')!;
    await expect(within(header).getAllByText(args.group.label)).toHaveLength(1);
    await expect(header.querySelector('[title]')?.textContent).toBe('4 sessions');
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
    const header = disclosure.closest('header')!;
    const title = within(header).getByText('/CryptoSafe');
    const updates = within(header).getByRole('button', { name: 'Show 1 newer session' });
    // Regression: accepting arrivals must not resize the project header.
    const pendingBounds = header.getBoundingClientRect();
    const style = (element: Element) => getComputedStyle(element);
    const rows = header.closest('section')!.lastElementChild!;

    // Arrivals sit beside the total, on its baseline, not beside the trailing actions.
    await expect(updates.closest('button[aria-expanded]')).toBeNull();
    const total = within(header).getByText(`${args.group.tally.count} sessions`);
    await expect(total.parentElement).toContainElement(updates);
    await expect(total.parentElement).toHaveTextContent(`${args.group.tally.count} sessions | 1 new`);
    const captionGap = () => total.getBoundingClientRect().top - title.getBoundingClientRect().bottom;
    const totalBounds = total.getBoundingClientRect();
    const updateBounds = updates.getBoundingClientRect();
    await expect(updateBounds.left).toBeGreaterThan(totalBounds.right);
    await expect(updateBounds.left - totalBounds.right).toBeLessThan(32);
    await expect(Math.abs(updateBounds.bottom - totalBounds.bottom)).toBeLessThanOrEqual(2);
    await expect(updateBounds.top).toBeGreaterThanOrEqual(pendingBounds.top);
    await expect(updateBounds.bottom).toBeLessThanOrEqual(pendingBounds.bottom);
    await expect(updateBounds.right).toBeLessThanOrEqual(
      total.parentElement!.getBoundingClientRect().right,
    );
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
    await expect(style(updates).borderTopWidth).toBe('0px');
    await expect(style(header).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderTopWidth).toBe('0px');
    await expect(style(rows.firstElementChild!).borderTopWidth).toBe('0px');
    // The final session needs the same thin divider as the internal rows.
    await expect(style(rows).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderBottomStyle).toBe('solid');
    await expect(style(rows).borderBottomColor).toBe(style(rows.children[1]).borderTopColor);
    await expect(rows.getBoundingClientRect().bottom).toBe(
      rows.lastElementChild!.getBoundingClientRect().bottom + 1,
    );
    await expect(within(header).getByText(`${args.group.tally.count} sessions`)).toBeVisible();
    const pageCount = Math.ceil(args.group.tally.count / args.reading.pageSize);
    if (pageCount > 1) {
      const pager = page.getByRole('navigation', {
        name: 'Pages of /CryptoSafe sessions',
      });
      await expect(pager).toBeVisible();
      await expect(captionGap()).toBeLessThanOrEqual(2);
      await expect(pendingBounds.height).toBe(
        matchMedia('(min-width: 640px) and (pointer: fine)').matches ? 48 : 52,
      );
      const pagerBounds = pager.getBoundingClientRect();
      await expect(pagerBounds.top).toBeGreaterThanOrEqual(pendingBounds.top);
      await expect(pagerBounds.bottom).toBeLessThanOrEqual(pendingBounds.bottom);
      const menu = within(header).getByRole('button', { name: /^Groups in / });
      const create = within(header).getByRole('button', { name: /^New session on / });
      // Reported after BLO-167 (paraphrased: a plus standing on the left is unacceptable,
      // the three dots belong on the right): the header's own controls hold the band's
      // trailing edge and the page steps stand just inside them.
      await expect(pagerBounds.right).toBeLessThanOrEqual(create.getBoundingClientRect().left);
      await expect(create.getBoundingClientRect().right).toBeLessThanOrEqual(
        menu.getBoundingClientRect().left,
      );
      await expect(menu.getBoundingClientRect().right).toBeLessThanOrEqual(pendingBounds.right);
      for (const button of within(pager).getAllByRole('button')) {
        const bounds = button.getBoundingClientRect();
        await expect(
          canvasElement.ownerDocument.elementFromPoint(
            bounds.x + bounds.width / 2,
            bounds.y + bounds.height / 2,
          )?.closest('button'),
        ).toBe(button);
      }
      await expect(within(pager).getByText(`Page 1 of ${pageCount}`)).toBeInTheDocument();
    }
    await expect(updates).toHaveTextContent(/^1 new$/);
    await expect(updates).toBeVisible();

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

// Regression: scrolling a narrow session pane must not paint rows through its pager.
export const ScrolledNarrowPane: Story = {
  args: pagedArgs,
  render: (args) => (
    <div
      className="@container h-80 w-full max-w-[414px] overflow-y-auto bg-page"
      data-testid="scroll-pane"
    >
      <ProjectGroup {...args} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const pane = page.getByTestId('scroll-pane');
    const pager = page.getByRole('navigation', { name: 'Pages of /CryptoSafe sessions' });
    const header = pane.querySelector('header')!;
    pane.scrollTop = 160;
    await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    await expect(pane.scrollTop).toBe(160);
    await expect(header.getBoundingClientRect().top).toBe(pane.getBoundingClientRect().top);
    await expect(pager.getBoundingClientRect().top).toBeGreaterThanOrEqual(
      header.getBoundingClientRect().top,
    );
    await expect(pager.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      header.getBoundingClientRect().bottom,
    );
    await expect(getComputedStyle(header).position).not.toBe('sticky');
    for (const button of within(pager).getAllByRole('button')) {
      const bounds = button.getBoundingClientRect();
      await expect(
        document
          .elementFromPoint(bounds.x + bounds.width / 2, bounds.y + bounds.height / 2)
          ?.closest('button'),
      ).toBe(button);
    }
  },
};

export const ScrolledNarrowDesktopPane: Story = {
  ...ScrolledNarrowPane,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Groups nest INSIDE the project: named bands first, then whatever nobody filed. */
const GROUPED = [
  { ...fixture.rows[0], group_id: 'wallet', group_name: 'Wallet work', group_color: 'blue' },
  { ...fixture.rows[1], group_id: 'wallet', group_name: 'Wallet work', group_color: 'blue' },
  { ...fixture.rows[2], group_id: 'receipts', group_name: 'Receipts', group_color: 'amber' },
  fixture.rows[3],
];

export const Groups: Story = {
  args: {
    group: { ...meta.args.group, sessions: GROUPED },
    machine: { conn, sessions: GROUPED },
    reading: { ...meta.args.reading, epoch: null, pendingByRoot: new Map() },
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const wallet = await page.findByRole('button', { name: /(Collapse|Expand) Wallet work/ });
    const band = wallet.closest('div')!.parentElement!;
    await expect(within(band).getByText('2 sessions')).toBeVisible();
    await expect(band.querySelectorAll('[data-session-id]')).toHaveLength(2);
    await expect(page.getByRole('button', { name: /(Collapse|Expand) Receipts/ })).toBeVisible();
    // A group folds on its own, and the rest of the project stays where it was.
    await userEvent.click(page.getByRole('button', { name: 'Collapse Wallet work' }));
    await expect(band.querySelectorAll('[data-session-id]')).toHaveLength(0);
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(2);
    await userEvent.click(page.getByRole('button', { name: 'Expand Wallet work' }));
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(4);
    // The sheet on the project's own name carries the VERB. The bands under that
    // header are the inventory, and the menu no longer repeats them back.
    await userEvent.click(page.getByRole('button', { name: `Groups in ${fixture.name}` }));
    const sheet = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: `Groups in ${fixture.name}`,
    });
    await expect(within(sheet).getByText('New group')).toBeVisible();
    await expect(within(sheet).queryByText('Wallet work')).toBeNull();
    await expect(within(sheet).queryByText('Receipts')).toBeNull();
  },
};
