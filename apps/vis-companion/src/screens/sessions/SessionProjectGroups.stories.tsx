import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

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
      openRow: null,
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

    // Arrivals sit at the END of the caption, on the total's baseline, not beside the
    // trailing actions — and never between the total and the states it qualifies
    // (reported: a project's arrival count belongs to the RIGHT of live, not its left).
    await expect(updates.closest('button[aria-expanded]')).toBeNull();
    const total = within(header).getByText(`${args.group.tally.count} sessions`);
    const live = args.group.tally.live;
    // The live control follows the eliding total; arrivals remain the caption's last word.
    const run = total.parentElement!;
    const caption = updates.parentElement!;
    await expect(caption).toContainElement(total);
    await expect(caption).toHaveTextContent(
      live > 0
        ? `${args.group.tally.count} sessions·${live} live | 1 new`
        : `${args.group.tally.count} sessions | 1 new`,
    );
    const captionGap = () => total.getBoundingClientRect().top - title.getBoundingClientRect().bottom;
    const totalBounds = total.getBoundingClientRect();
    const liveControl = within(header).queryByRole('button', { name: /^Open the (live|newest)/ });
    const runBounds = (liveControl ?? run).getBoundingClientRect();
    const updateBounds = updates.getBoundingClientRect();
    await expect(updateBounds.left).toBeGreaterThanOrEqual(runBounds.right);
    await expect(updateBounds.left - runBounds.right).toBeLessThan(32);
    await expect(Math.abs(updateBounds.bottom - totalBounds.bottom)).toBeLessThanOrEqual(2);
    await expect(updateBounds.top).toBeGreaterThanOrEqual(pendingBounds.top);
    await expect(updateBounds.bottom).toBeLessThanOrEqual(pendingBounds.bottom);
    // Whatever the width, the run gives way and the arrival keeps its box on the line.
    await expect(updateBounds.right).toBeLessThanOrEqual(caption.getBoundingClientRect().right);
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
    // The SETS carry the rule between them: where a project paints one, its header holds the
    // line and the first session under it adds none; where it paints none, the band's own
    // line is the only one above the page.
    const set = within(rows as HTMLElement).queryByText('Sessions')?.parentElement;
    const firstRow = set ? set.nextElementSibling! : rows.firstElementChild!;
    if (set) await expect(style(set).borderTopWidth).toBe('1px');
    await expect(style(firstRow).borderTopWidth).toBe('0px');
    // The final session needs the same thin divider as the internal rows.
    await expect(style(rows).borderBottomWidth).toBe('1px');
    await expect(style(rows).borderBottomStyle).toBe('solid');
    await expect(style(rows).borderBottomColor).toBe(
      style(firstRow.nextElementSibling!).borderTopColor,
    );
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
      // The project header no longer owns the lists' actions or their creation controls.
      await expect(within(header).queryByRole('button', { name: /^Actions for / })).toBeNull();
      await expect(within(header).queryByRole('button', { name: /^New session/ })).toBeNull();
      // The pager and Sessions actions share the set header beneath the project band.
      await expect(within(header).queryByRole('navigation')).toBeNull();
      await expect(pagerBounds.top).toBeGreaterThanOrEqual(pendingBounds.bottom);
      const set = within(rows as HTMLElement).getByText('Sessions').parentElement!;
      await expect(set).toContainElement(pager);
      await expect(pagerBounds.right).toBeLessThanOrEqual(set.getBoundingClientRect().right);
      const actions = within(set).getByRole('button', {
        name: `Actions for sessions in ${args.group.root}`,
      });
      await expect(actions.getBoundingClientRect().left).toBeGreaterThanOrEqual(pagerBounds.right);
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

// Regression: scrolling a narrow session pane must not paint rows through the band that
// stays on top of it. The steps ride with the set they move now, so what has to stay opaque
// is the project header itself.
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
    await expect(header).not.toContainElement(pager);
    pane.scrollTop = 160;
    await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    await expect(pane.scrollTop).toBe(160);
    await expect(header.getBoundingClientRect().top).toBe(pane.getBoundingClientRect().top);
    await expect(getComputedStyle(header).position).toBe('sticky');
    // Nothing that scrolls under the band shows through it: every point across the band
    // belongs to the band or to one of its own controls.
    const bounds = header.getBoundingClientRect();
    for (const x of [bounds.left + 8, bounds.x + bounds.width / 2, bounds.right - 8]) {
      const hit = document.elementFromPoint(x, bounds.y + bounds.height / 2);
      await expect(header.contains(hit)).toBe(true);
    }
  },
};

export const ScrolledNarrowDesktopPane: Story = {
  ...ScrolledNarrowPane,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** The bands this project HAS. A row names one; the name and the colour live here. */
const GROUPED_BANDS: SessionGroup[] = [
  {
    id: 'wallet',
    project_id: fixture.projectId,
    name: 'Wallet work',
    color: 'blue',
    position: 0,
    session_count: 2,
  },
  {
    id: 'receipts',
    project_id: fixture.projectId,
    name: 'Receipts',
    color: 'amber',
    position: 1,
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
    reading: { ...meta.args.reading, epoch: null, pendingByRoot: new Map() },
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const wallet = await page.findByRole('button', { name: /(Collapse|Expand) Wallet work/ });
    const band = wallet.closest('div')!.parentElement!;
    // The unpaged Groups strip is as tall as Sessions with its own actions.
    const groupsHeader = page.getByText('Groups').parentElement!;
    const sessionsHeader = page.getByText('Sessions').parentElement!;
    await expect(groupsHeader.getBoundingClientRect().height).toBe(
      sessionsHeader.getBoundingClientRect().height,
    );
    await expect(within(band).getByText('2 sessions')).toBeVisible();
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

/**
 * ONE DRAG, done by the BROWSER whenever a browser is running this story: Playwright's
 * own pointer is the only thing that proves the engine starts a drag on this row at all.
 * Under Storybook's dev server there is no such pointer, so the events are synthesised
 * and the story still plays.
 */
async function dragOnto(source: HTMLElement, target: HTMLElement) {
  if ('__vitest_browser_runner__' in globalThis) {
    const { userEvent: pointer } = await import('vitest/browser');
    await pointer.dragAndDrop(source, target);
    return;
  }
  const dataTransfer = new DataTransfer();
  source.dispatchEvent(new DragEvent('dragstart', { bubbles: true, dataTransfer }));
  target.dispatchEvent(new DragEvent('dragover', { bubbles: true, cancelable: true, dataTransfer }));
  target.dispatchEvent(new DragEvent('drop', { bubbles: true, cancelable: true, dataTransfer }));
}

/**
 * FILING BY HAND, BOTH WAYS. A band takes the row dropped on it, and the `Sessions`
 * header takes one back out of its group — the half a band cannot offer, since a band
 * only ever files INTO itself. Reported from the desktop app: dragging a session did
 * nothing, and there was nowhere to drop one to take it out of a group. The picture the
 * pointer carries is read here as well: one row, the size of the row that was taken.
 */
export const GroupDragAndDrop: Story = {
  ...Groups,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await page.findByRole('button', { name: 'Collapse Wallet work' });
    const wallet = () =>
      page.getByRole('button', { name: 'Collapse Wallet work' }).closest('div')!.parentElement!;
    const bandHeader = () =>
      page.getByRole('button', { name: 'Collapse Wallet work' }).parentElement!;
    const strip = (sid: string) =>
      canvasElement
        .querySelector(`[data-session-id="${sid}"]`)!
        .closest('[draggable="true"]') as HTMLElement;

    // THE PICTURE THE POINTER CARRIES, caught as the engine starts the drag. It has to be
    // the row and nothing else: one row big, and laid out the way that row is laid out in
    // the list. Reported from the desktop app - first as a picture of the whole LIST behind
    // the row, then, once the row handed over a copy of itself, as a copy that lost the
    // list's own width and left its group rail short of the card's end.
    const shape = (root: HTMLElement) => {
      const outer = root.getBoundingClientRect();
      const placed = (node: Element | null) => {
        if (!node) return 'none';
        const box = node.getBoundingClientRect();
        return [box.left - outer.left, box.top - outer.top, box.width, box.height]
          .map(Math.round)
          .join('/');
      };
      return {
        rows: root.querySelectorAll('[data-row-surface]').length,
        size: `${Math.round(outer.width)}x${Math.round(outer.height)}`,
        track: placed(root.querySelector('[data-swipe-track]')),
        rail: placed(root.querySelector('span[aria-hidden][class*="w-1"]')),
      };
    };
    const carried: { picture: ReturnType<typeof shape>; row: ReturnType<typeof shape> }[] = [];
    const watchDrag = (event: Event) => {
      const taken = (event.target as HTMLElement).closest('[draggable="true"]');
      const card = [...canvasElement.ownerDocument.body.children].find(
        (node) =>
          node instanceof HTMLElement &&
          node.style.position === 'fixed' &&
          node.getAttribute('aria-hidden') === 'true',
      ) as HTMLElement | undefined;
      if (taken && card) carried.push({ picture: shape(card), row: shape(taken as HTMLElement) });
    };
    canvasElement.ownerDocument.addEventListener('dragstart', watchDrag);

    await dragOnto(strip(fixture.rows[3].id), bandHeader());
    await within(wallet()).findByText('3 sessions');
    await expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(3);

    await dragOnto(strip(GROUPED[0].id), page.getByText('Sessions').parentElement!);
    await within(wallet()).findByText('2 sessions');
    await expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(2);

    canvasElement.ownerDocument.removeEventListener('dragstart', watchDrag);
    // A loose row carries no group rail and the filed one does; either way the picture is
    // the shape of the row it was taken from, down to where its parts stand inside it.
    await expect(carried.length).toBeGreaterThanOrEqual(2);
    await expect(carried.map((seen) => seen.picture)).toEqual(carried.map((seen) => seen.row));
    await expect(carried.map((seen) => seen.picture.rows)).toEqual(carried.map(() => 1));
    await expect(carried.at(-1)!.picture.rail).not.toBe('none');
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

export const GroupsDesktop: Story = {
  ...Groups,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

const SCROLL_ROWS = [
  ...GROUPED.slice(0, 3),
  ...Array.from({ length: 21 }, (_, index) => ({
    ...fixture.rows[3],
    id: `paging-scroll-${index}`,
    title: `Session page row ${index}`,
  })),
];

// Regression: paging below the groups must keep the Sessions strip under the eye,
// including when the next page is shorter than the one it replaces.
export const PagingBelowGroups: Story = {
  ...Groups,
  beforeEach: () => {
    const previous = globalThis.fetch;
    const serve = storyFleetFetch([{ ...fixture, rows: SCROLL_ROWS, groups: GROUPED_BANDS }]);
    globalThis.fetch = async (input, init) => {
      // Let uncached turns spend time waiting for a reply, just like a remote gateway.
      await new Promise((resolve) => setTimeout(resolve, 60));
      return serve(input, init);
    };
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    ...Groups.args,
    group: {
      ...meta.args.group,
      tally: { count: SCROLL_ROWS.length, live: 0, awaiting: 0, unread: 0 },
      sessions: SCROLL_ROWS,
    },
    machine: { conn, sessions: SCROLL_ROWS },
  },
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
    await page.findByRole('button', { name: 'Collapse Wallet work' });
    await page.findByText('Session page row 9');
    const set = page.getByText('Sessions').parentElement!;
    await expect(page.getByText('Groups').parentElement!.getBoundingClientRect().height).toBe(
      set.getBoundingClientRect().height,
    );
    const pager = within(set).getByRole('navigation');
    const next = within(pager).getByRole('button', { name: 'Next page' });
    const previous = within(pager).getByRole('button', { name: 'Previous page' });
    // Start higher, still reading groups; then repeat with Sessions near the top.
    for (const offset of [190, 60]) {
      pane.scrollTop += set.getBoundingClientRect().top - pane.getBoundingClientRect().top - offset;
      await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
      const top = set.getBoundingClientRect().top;
      for (const target of [2, 3, 2, 1]) {
        const current = Number((within(pager).getByRole('textbox') as HTMLInputElement).value);
        await userEvent.click(target > current ? next : previous);
        await within(pager).findByText(`Page ${target} of 3`);
        await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
        await waitFor(() =>
          expect(
            set.getBoundingClientRect().top,
            `Page ${target}, header offset ${offset}`,
          ).toBeCloseTo(top, 0),
        );
        await expect(
          pane.querySelector(`[data-session-id="paging-scroll-${(target - 1) * 10}"]`),
        ).toBeVisible();
      }
    }
  },
};

export const PagingBelowGroupsDesktop: Story = {
  ...PagingBelowGroups,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

const NEXT_ROOT = '/After';
const NEXT_ROWS = Array.from({ length: 12 }, (_, index) => ({
  ...fixture.rows[index % fixture.rows.length],
  id: `after-${index}`,
  workspace: { root: NEXT_ROOT },
}));
const NEXT_PROJECT = {
  ...fixture,
  root: NEXT_ROOT,
  name: NEXT_ROOT,
  projectId: 'after',
  rows: NEXT_ROWS,
};

// Regression: on a phone the active set stays beneath its own project while scrolling.
// The next project takes both sticky levels away and shows Groups even with no bands.
export const StickySectionHeaders: Story = {
  ...PagingBelowGroups,
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([
      { ...fixture, rows: SCROLL_ROWS, groups: GROUPED_BANDS },
      NEXT_PROJECT,
    ]);
    writeProjectFold(projectFoldKey(machineKey(conn), NEXT_ROOT), true);
    return () => {
      globalThis.fetch = previous;
    };
  },
  render: (args) => (
    <div className="@container h-80 w-full max-w-[390px] overflow-y-auto bg-page" data-testid="scroll-pane">
      <ProjectGroup {...args} />
      <ProjectGroup
        {...args}
        group={{
          ...args.group,
          root: NEXT_ROOT,
          label: NEXT_ROOT,
          projectId: NEXT_PROJECT.projectId,
          tally: { count: NEXT_ROWS.length, live: 0, awaiting: 0, unread: 0 },
          sessions: NEXT_ROWS,
        }}
        machine={{ conn, sessions: NEXT_ROWS }}
        reading={{ ...args.reading, epoch: null, pendingByRoot: new Map() }}
      />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const pane = page.getByTestId('scroll-pane');
    const first = pane.querySelector('[data-project-root="/CryptoSafe"]')!;
    const next = pane.querySelector('[data-project-root="/After"]')!;
    await within(first as HTMLElement).findByText('Session page row 0');
    await within(next as HTMLElement).findAllByText('Check transaction confirmations');
    const project = first.querySelector('header')!;
    const groups = within(first as HTMLElement).getByText('Groups').parentElement!;
    const sessions = within(first as HTMLElement).getByText('Sessions').parentElement!;
    const nextProject = next.querySelector('header')!;
    const nextGroups = within(next as HTMLElement).getByText('Groups').parentElement!;
    const nextSessions = within(next as HTMLElement).getByText('Sessions').parentElement!;
    const frame = () => new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    const style = (element: Element) => getComputedStyle(element);
    // Distinct, opaque set surfaces keep the words readable over scrolling rows.
    for (const set of [groups, sessions]) {
      await expect(style(set).backgroundColor).not.toBe('rgba(0, 0, 0, 0)');
      await expect(style(set).backgroundColor).not.toBe(style(project).backgroundColor);
    }
    await expect(style(groups).backgroundColor).not.toBe(style(sessions).backgroundColor);
    await expect(style(groups.firstElementChild!).fontSize).toBe('11px');
    await expect(style(sessions.firstElementChild!).fontSize).toBe('11px');
    await expect(style(nextSessions).backgroundColor).toBe(style(sessions).backgroundColor);
    await expect(pane.scrollWidth).toBe(pane.clientWidth);
    pane.scrollTop = 135;
    await frame();
    await expect(getComputedStyle(groups).position).toBe('sticky');
    await expect(project.getBoundingClientRect().top).toBeCloseTo(pane.getBoundingClientRect().top, 0);
    await expect(groups.getBoundingClientRect().top).toBeCloseTo(project.getBoundingClientRect().bottom, 0);
    const groupHit = document.elementFromPoint(
      groups.getBoundingClientRect().left + 22,
      groups.getBoundingClientRect().top + 12,
    );
    await expect(groups.contains(groupHit)).toBe(true);

    pane.scrollTop += sessions.getBoundingClientRect().top - project.getBoundingClientRect().bottom + 75;
    await frame();
    await expect(sessions.getBoundingClientRect().top).toBeCloseTo(project.getBoundingClientRect().bottom, 0);
    await expect(groups.getBoundingClientRect().bottom).toBeLessThanOrEqual(sessions.getBoundingClientRect().top + 1);
    const sessionHit = document.elementFromPoint(
      sessions.getBoundingClientRect().left + 22,
      sessions.getBoundingClientRect().top + 12,
    );
    await expect(sessions.contains(sessionHit)).toBe(true);
    const actions = within(sessions).getByRole('button', {
      name: `Actions for sessions in ${fixture.root}`,
    });
    await expect(actions.getBoundingClientRect().top).toBeGreaterThanOrEqual(
      sessions.getBoundingClientRect().top,
    );

    pane.scrollTop += nextProject.getBoundingClientRect().top - pane.getBoundingClientRect().top + 30;
    await frame();
    await expect(nextProject.getBoundingClientRect().top).toBeCloseTo(
      pane.getBoundingClientRect().top,
      0,
    );
    await expect(nextGroups).toBeInTheDocument();
    await expect(nextSessions.getBoundingClientRect().top).toBeGreaterThan(
      nextProject.getBoundingClientRect().bottom,
    );
    pane.scrollTop +=
      nextSessions.getBoundingClientRect().top - nextProject.getBoundingClientRect().bottom + 40;
    await frame();
    await expect(nextSessions.getBoundingClientRect().top).toBeCloseTo(
      nextProject.getBoundingClientRect().bottom,
      0,
    );
    const nextHit = document.elementFromPoint(
      nextSessions.getBoundingClientRect().left + 22,
      nextSessions.getBoundingClientRect().top + 12,
    );
    await expect(nextSessions.contains(nextHit)).toBe(true);
  },
};

export const StaticSectionHeadersDesktop: Story = {
  ...StickySectionHeaders,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const pane = page.getByTestId('scroll-pane');
    const first = pane.querySelector('[data-project-root="/CryptoSafe"]')!;
    const groups = within(first as HTMLElement).getByText('Groups').parentElement!;
    const sessions = within(first as HTMLElement).getByText('Sessions').parentElement!;
    await within(first as HTMLElement).findByText('Session page row 0');
    await expect(getComputedStyle(groups).position).toBe('static');
    await expect(getComputedStyle(sessions).position).toBe('static');
  },
};

export const StickySectionHeadersDark: Story = {
  ...StickySectionHeaders,
  globals: { theme: 'blockether-dark' },
};

// A long project name, group name and a three-digit group count exercise the entire 320px rail.
const LONG_GROUPS: SessionGroup[] = [
  { ...GROUPED_BANDS[0], name: 'Wallet work: archived transactions and reconciliation' },
  ...GROUPED_BANDS.slice(1),
  ...Array.from({ length: 121 }, (_, index) => ({
    ...GROUPED_BANDS[1],
    id: `extra-group-${index}`,
    name: `Record group ${index}`,
    position: index + 2,
    session_count: 0,
  })),
];

export const NarrowSectionHeaders: Story = {
  ...PagingBelowGroups,
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch([{ ...fixture, rows: SCROLL_ROWS, groups: LONG_GROUPS }]);
    writeProjectFold(projectFoldKey(machineKey(conn), fixture.root), true);
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    ...PagingBelowGroups.args,
    group: {
      ...meta.args.group,
      label: '/CryptoSafe/archived/records/2026',
      tally: { count: SCROLL_ROWS.length, live: 0, awaiting: 0, unread: 0 },
      sessions: SCROLL_ROWS,
    },
  },
  render: (args) => (
    <div className="@container h-80 w-full max-w-[320px] overflow-y-auto bg-page" data-testid="scroll-pane">
      <ProjectGroup {...args} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const pane = page.getByTestId('scroll-pane');
    const longBand = await page.findByRole('button', { name: /Collapse Wallet work: archived/ });
    const project = pane.querySelector('header')!;
    const groups = page.getByText('Groups').parentElement!;
    const sessions = page.getByText('Sessions').parentElement!;
    await expect(within(groups).getByText('123 groups')).toBeVisible();
    await expect(longBand).toBeVisible();
    await expect(pane.clientWidth).toBe(320);
    await expect(pane.scrollWidth).toBe(pane.clientWidth);
    const controlsFit = (header: Element) => {
      const bounds = header.getBoundingClientRect();
      for (const control of header.querySelectorAll('button, input')) {
        const box = control.getBoundingClientRect();
        expect(box.left).toBeGreaterThanOrEqual(bounds.left);
        expect(box.right).toBeLessThanOrEqual(bounds.right);
      }
    };
    controlsFit(groups);
    controlsFit(sessions);
    const groupPager = within(groups).getByRole('navigation');
    await userEvent.click(within(groupPager).getByRole('button', { name: 'Next page' }));
    await within(groupPager).findByText('Page 2 of 13');
    const frame = () => new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    pane.scrollTop = 150;
    await frame();
    await expect(groups.getBoundingClientRect().top).toBeCloseTo(
      project.getBoundingClientRect().bottom,
      0,
    );
    const hit = document.elementFromPoint(
      groups.getBoundingClientRect().left + 20,
      groups.getBoundingClientRect().top + 12,
    );
    await expect(groups.contains(hit)).toBe(true);
    controlsFit(groups);
    const actions = within(sessions).getByRole('button', {
      name: `Actions for sessions in ${fixture.root}`,
    });
    pane.scrollTop += sessions.getBoundingClientRect().top - project.getBoundingClientRect().bottom + 40;
    await frame();
    await expect(sessions.getBoundingClientRect().top).toBeCloseTo(
      project.getBoundingClientRect().bottom,
      0,
    );
    await expect(groups.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      sessions.getBoundingClientRect().top + 1,
    );
    controlsFit(sessions);
    const box = actions.getBoundingClientRect();
    await expect(document.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)).toBe(
      actions,
    );
    const sessionPager = within(sessions).getByRole('navigation');
    await userEvent.click(within(sessionPager).getByRole('button', { name: 'Next page' }));
    await within(sessionPager).findByText('Page 2 of 3');
    await expect(pane.scrollWidth).toBe(pane.clientWidth);
  },
};

export const NarrowSectionHeadersHighContrast: Story = {
  ...NarrowSectionHeaders,
  globals: { theme: 'high-contrast-dark' },
};

/** Tapping the live count uses the same open action as the session row. */
export const LiveCountOpensTheRun: Story = {
  args: {
    group: {
      root: fixture.root,
      label: fixture.name,
      projectId: fixture.projectId,
      tally: { count: fixture.rows.length + 1, live: 1, awaiting: 0, unread: 0 },
      sessions: [
        ...fixture.rows,
        { ...fixture.rows[0], id: 'live-run', title: 'Nightly fleet scan', live: true },
      ],
    },
  },
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    const heading = page.getByRole('button', { name: `Collapse ${args.group.label}` });
    const header = heading.closest('header')!;
    const live = within(header).getByRole('button', { name: 'Open the live session' });
    await expect(live.textContent).toMatch(/^1 live$/);
    // The caption still reads as one line of type: totals, the running count, arrivals.
    await expect(live.parentElement).toHaveTextContent(
      `${args.group.tally.count} sessions·1 live | 1 new`,
    );
    if ('__vitest_browser_runner__' in globalThis) {
      const { userEvent: pointer } = await import('vitest/browser');
      await pointer.click(live);
    } else {
      await userEvent.click(live);
    }
    await expect(args.context.actions.commands.open).toHaveBeenCalledWith(conn, 'live-run');
    await expect(heading).toHaveAttribute('aria-expanded', 'true');
  },
};

export const LiveCountInNarrowPane: Story = {
  ...LiveCountOpensTheRun,
  render: (args) => (
    <div className="@container w-full max-w-[320px] bg-page">
      <ProjectGroup {...args} />
    </div>
  ),
};
