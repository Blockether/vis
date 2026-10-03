import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fireEvent, fn, userEvent, waitFor, within } from 'storybook/test';

import {
  STORY_FLEET_CONNS,
  STORY_NEWER_PROJECT,
  STORY_PROJECT_CLIENT,
  storyFleetFetch,
} from '../../dev/story-data';
import { machineKey } from '../../lib/fleet';
import { projectFoldKey, projectRevealKey, writeProjectFold } from '../../lib/project-fold';
import { THEMES } from '../../lib/themes.generated';
import type { SessionGroup } from '../../lib/types';
import { ProjectGroup } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const fixture = STORY_NEWER_PROJECT;

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

/** A path-derived project name appears once, with counts on its right. */
export const ProjectPathName: Story = {
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    const heading = page.getByRole('button', { name: `Collapse ${args.group.label}` });
    const header = heading.closest('header')!;
    await expect(within(header).getAllByText(args.group.label)).toHaveLength(1);
    await expect(header.querySelector('[title]')?.textContent).toBe(args.group.label);
    await expect(within(header).getByText('4 sessions')).toBeVisible();
    await userEvent.click(heading);
    await expect(page.getByRole('button', { name: `Expand ${args.group.label}` })).toBeVisible();
    await expect(canvasElement.querySelector('[data-session-id]')).toBeNull();
    await userEvent.click(heading);
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(4);
  },
};

export const ShowNewerSession: Story = {
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    await page.findByText('Fix balance refresh');
    const disclosure = page.getByRole('button', { name: 'Collapse /CryptoSafe' });
    const header = disclosure.closest('header')!;
    const rows = header.closest('section')!.lastElementChild!;

    // Fresh rows arrive in gateway order without an extra acceptance action.
    await expect(await page.findByText('Check transaction confirmations')).toBeVisible();
    await expect(within(header).getByText(`${args.group.tally.count} sessions`)).toBeVisible();
    const pageCount = Math.ceil(args.group.tally.count / args.reading.pageSize);
    if (pageCount > 1) {
      const pager = page.getByRole('navigation', {
        name: 'Pages of /CryptoSafe sessions',
      });
      await expect(pager).toBeVisible();
      // The project header no longer owns the lists' actions or their creation controls.
      await expect(within(header).queryByRole('button', { name: /^Actions for / })).toBeNull();
      await expect(within(header).queryByRole('button', { name: /^New session/ })).toBeNull();
      // The pager and Sessions actions share the set header beneath the project band.
      await expect(within(header).queryByRole('navigation')).toBeNull();
      const set = within(rows as HTMLElement).getByText('Sessions').parentElement!;
      await expect(set).toContainElement(pager);
      await expect(within(pager).getByText(`Page 1 of ${pageCount}`)).toBeInTheDocument();
    }

    await userEvent.click(page.getByRole('button', { name: 'Collapse /CryptoSafe' }));
    await expect(canvasElement.querySelector('[data-session-id]')).toBeNull();
    await userEvent.click(page.getByRole('button', { name: 'Expand /CryptoSafe' }));
    await expect(await page.findByText('Check transaction confirmations')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Collapse /CryptoSafe' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
    await expect(canvasElement.querySelectorAll('[data-session-id]')).toHaveLength(
      Math.min(args.reading.pageSize, args.group.sessions.length),
    );
    await expect(page.queryByRole('button', { name: /newer session/ })).toBeNull();
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
  ...ShowNewerSession,
  args: pagedArgs,
};

export const SmallPhoneWithPaging: Story = {
  ...PhoneWithPaging,
  tags: ['!test'],
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
};

export const DesktopWithPaging: Story = {
  ...PhoneWithPaging,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async (context) => {
    const page = within(context.canvasElement);
    await page.findByRole('navigation', { name: 'Pages of /CryptoSafe sessions' });
    (await page.findAllByRole('button', { name: /^Show details for / }))[0];
  },
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
    await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
  },
};

export const ScrolledNarrowDesktopPane: Story = {
  ...ScrolledNarrowPane,
  tags: ['!test'],
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

/** Drag one element onto another the way a desktop pointer does: take hold, hover, drop. */
function dragOnto(source: HTMLElement, target: HTMLElement) {
  const values = new Map<string, string>();
  const dataTransfer = {
    setData: (type: string, value: string) => void values.set(type, value),
    getData: (type: string) => values.get(type) ?? '',
    setDragImage: () => {},
  };
  fireEvent.dragStart(source, { dataTransfer });
  fireEvent.dragOver(target, { dataTransfer });
  fireEvent.drop(target, { dataTransfer });
}

/**
 * FILING BY HAND, BOTH WAYS. A band takes the row dropped on it, and the `Sessions`
 * header takes one back out of its group — the half a band cannot offer, since a band
 * only ever files INTO itself. Reported from the desktop app: dragging a session did
 * nothing, and there was nowhere to drop one to take it out of a group.
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

    dragOnto(strip(fixture.rows[3].id), bandHeader());
    await waitFor(() => expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(3));
    await expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(3);

    dragOnto(strip(GROUPED[0].id), page.getByText('Sessions').parentElement!);
    await waitFor(() => expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(2));
    await expect(wallet().querySelectorAll('[data-session-id]')).toHaveLength(2);
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
  tags: ['!test'],
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

// Regression: paging below the groups, including onto a page shorter than the one it replaces.
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
    const pager = within(set).getByRole('navigation');
    const next = within(pager).getByRole('button', { name: 'Next page' });
    const previous = within(pager).getByRole('button', { name: 'Previous page' });
    for (const target of [2, 3, 2, 1]) {
      const current = Number((within(pager).getByRole('textbox') as HTMLInputElement).value);
      await userEvent.click(target > current ? next : previous);
      await within(pager).findByText(`Page ${target} of 3`);
      await expect(
        pane.querySelector(`[data-session-id="paging-scroll-${(target - 1) * 10}"]`),
      ).toBeVisible();
    }
  },
};

export const PagingBelowGroupsDesktop: Story = {
  ...PagingBelowGroups,
  tags: ['!test'],
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
    const nextProject = next.querySelector('header')!;
    const nextGroups = within(next as HTMLElement).getByText('Groups').parentElement!;
    const frame = () => new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    await frame();
    await frame();
    await frame();
    await expect(nextGroups).toBeInTheDocument();
    await frame();
    const disclosure = within(nextProject).getByRole('button', { name: 'Collapse /After' });
    await userEvent.click(disclosure);
    await expect(disclosure).toHaveAttribute('aria-expanded', 'false');
    await userEvent.click(disclosure);
    await expect(disclosure).toHaveAttribute('aria-expanded', 'true');
  },
};

export const StaticSectionHeadersDesktop: Story = {
  ...StickySectionHeaders,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const pane = page.getByTestId('scroll-pane');
    const first = pane.querySelector('[data-project-root="/CryptoSafe"]')!;
    await within(first as HTMLElement).findByText('Session page row 0');
    const header = first.querySelector('header')!;
    const disclosure = within(header).getByRole('button', { name: 'Collapse /CryptoSafe' });
    await userEvent.tab();
    disclosure.focus();
    await expect(disclosure).toHaveFocus();
    disclosure.blur();
  },
};

export const StaticSectionHeadersDesktopDark: Story = {
  ...StaticSectionHeadersDesktop,
  tags: ['!test'],
  globals: { theme: 'blockether-dark', viewport: { value: 'desktop', isRotated: false } },
};

export const StaticSectionHeadersDesktopHighContrast: Story = {
  ...StaticSectionHeadersDesktop,
  tags: ['!test'],
  globals: { theme: 'high-contrast-dark', viewport: { value: 'desktop', isRotated: false } },
};

export const StickySectionHeadersDark: Story = {
  ...StickySectionHeaders,
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
};

export const StickySectionHeadersHighContrast: Story = {
  ...StickySectionHeaders,
  tags: ['!test'],
  globals: { theme: 'high-contrast-dark' },
};

// Expanded project surfaces vary by theme; the chevron still marks folding in light.
// Sticky/focused cases have their own plays.
export const ExpandedHeaderAcrossThemes: Story = {
  ...StickySectionHeaders,
  play: async ({ canvasElement }) => {
    const first = canvasElement.querySelector('[data-project-root="/CryptoSafe"]')!;
    const next = canvasElement.querySelector('[data-project-root="/After"]')!;
    await within(first as HTMLElement).findByText('Session page row 0');
    await within(next as HTMLElement).findAllByText('Check transaction confirmations');
    const collapsed = next.querySelector('header')!;
    await userEvent.click(within(collapsed).getByRole('button', { name: 'Collapse /After' }));
    const root = canvasElement.ownerDocument.documentElement;
    const previousTheme = root.dataset.theme;
    try {
      for (const { id } of THEMES) {
        root.dataset.theme = id;
      }
    } finally {
      if (previousTheme === undefined) delete root.dataset.theme;
      else root.dataset.theme = previousTheme;
    }
  },
};

export const ExpandedHeaderAcrossThemesPhone: Story = {
  ...ExpandedHeaderAcrossThemes,
  tags: ['!test'],
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
};

// Long project and group names still fit the 320px rail. Live bands stand whole with no pager;
// only the archived wall, the one that keeps growing, steps through pages in the same header.
const LONG_GROUPS: SessionGroup[] = [
  { ...GROUPED_BANDS[0], name: 'Wallet work: archived transactions and reconciliation' },
  ...GROUPED_BANDS.slice(1),
  ...Array.from({ length: 121 }, (_, index) => ({
    ...GROUPED_BANDS[1],
    id: `extra-group-${index}`,
    name: `Record group ${index}`,
    position: index + 2,
    session_count: 0,
    archived_at: 1730000000,
  })),
];

export const NarrowSectionHeaders: Story = {
  ...PagingBelowGroups,
  beforeEach: () => {
    const previous = globalThis.fetch;
    const revealed = projectRevealKey(machineKey(conn), fixture.root, 'groups');
    globalThis.fetch = storyFleetFetch([{ ...fixture, rows: SCROLL_ROWS, groups: LONG_GROUPS }]);
    writeProjectFold(projectFoldKey(machineKey(conn), fixture.root), true);
    // The play opens the archive, and that choice is saved: start and end on the live bands.
    writeProjectFold(revealed, false);
    return () => {
      globalThis.fetch = previous;
      writeProjectFold(revealed, false);
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
    const sheets = within(canvasElement.ownerDocument.body);
    const longBand = await page.findByRole('button', { name: /Collapse Wallet work: archived/ });
    const groups = page.getByText('Groups').parentElement!;
    const sessions = page.getByText('Sessions').parentElement!;
    await expect(longBand).toBeVisible();
    // Live bands are never paged: the Groups header carries no steps over them.
    await expect(within(groups).queryByRole('navigation')).toBeNull();
    await userEvent.click(page.getByRole('button', { name: `Actions for groups in ${fixture.root}` }));
    await userEvent.click(
      within(sheets.getByRole('dialog', { name: /^Groups in / })).getByText('Show archived groups'),
    );
    const groupPager = await within(groups).findByRole('navigation');
    await userEvent.click(within(groupPager).getByRole('button', { name: 'Next page' }));
    await within(groupPager).findByText('Page 2 of 9');
    const frame = () => new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    await frame();
    await frame();
    const sessionPager = within(sessions).getByRole('navigation');
    await userEvent.click(within(sessionPager).getByRole('button', { name: 'Next page' }));
    await within(sessionPager).findByText('Page 2 of 3');
  },
};

export const NarrowSectionHeadersHighContrast: Story = {
  ...NarrowSectionHeaders,
  tags: ['!test'],
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
    await expect(live.textContent).toMatch(/^1 LIVE$/);
    // The total and live action share the right-hand count column.
    await expect(within(live.parentElement!.parentElement!).getByText(`${args.group.tally.count} sessions`)).toBeVisible();
    await expect(within(live.parentElement!).getByText('|')).toHaveAttribute('aria-hidden', 'true');
    await userEvent.click(live);
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
