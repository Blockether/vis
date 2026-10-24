import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, within } from 'storybook/test';

import { sidebarRailClass } from '../App';
import {
  STORY_FLEET_CONNS,
  STORY_FLEET_PROJECTS,
  STORY_SESSION_SEARCH_MATCH,
  storyFleetFetch,
} from '../dev/story-data';
import { EmptyPane } from './EmptyPane';
import { SessionsScreen } from './SessionsScreen';

/** A session of the story fleet, as a list row. */
const fleetRow = (sid: string) =>
  STORY_FLEET_PROJECTS.flatMap((project) => project.rows).find((row) => row.id === sid)!;

/** The gateway's answer to "windows": two sessions, each with the messages it matched in. */
const ANSWER = {
  query: 'windows',
  sessions: [
    {
      ...fleetRow(STORY_SESSION_SEARCH_MATCH.sessionId),
      match: {
        rank: 1,
        is_in_request: true,
        is_in_reply: true,
        is_in_thinking: true,
        hits: STORY_SESSION_SEARCH_MATCH.hits,
      },
    },
    {
      ...fleetRow('41d78df4'),
      match: {
        rank: 2,
        is_in_request: true,
        hits: [
          {
            side: 'request',
            snippet: 'Does the scroll jump on **Windows** as well?',
            at: Date.UTC(2030, 0, 2, 10, 30, 0),
          },
        ],
      },
    },
  ],
  total: 2,
  next_cursor: null,
  has_more: false,
};

/**
 * SEARCH OPENS ITS OWN DIALOG: the sessions a query found on one side of a border, and
 * the messages it found in the picked session on the other, as the terminal's switcher
 * draws them. A desk puts the two side by side; a phone stacks the messages under the
 * sessions. The list behind the dialog keeps every row. The test run plays these stories
 * in jsdom, which has no boxes; the browser Storybook measures the real layout.
 */
const meta = {
  title: 'Session/Search split',
  parameters: { layout: 'fullscreen' },
  beforeEach: () => {
    const previous = globalThis.fetch;
    const fleet = storyFleetFetch();
    globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit) => {
      const href = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
      if (new URL(href).pathname === '/v1/sessions/actions/search') return Response.json(ANSWER);
      return fleet(input, init);
    }) as typeof fetch;
    return () => {
      globalThis.fetch = previous;
    };
  },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

function SearchedList() {
  return (
    <SessionsScreen
      conns={STORY_FLEET_CONNS}
      primary={STORY_FLEET_CONNS[0]}
      query="windows"
      onQuery={fn()}
      subscriptions={null}
      onOpen={fn()}
      onSearch={null}
      isSearchOpen
      onCloseSearch={fn()}
      isVisible
    />
  );
}

/** The boxes of the painted message pane and of the sessions it explains. */
async function boxes(canvasElement: HTMLElement) {
  const dialog = within(
    await within(canvasElement.ownerDocument.body).findByRole('dialog', { name: 'Search sessions' }),
  );
  const pane = await dialog.findByRole('region', { name: 'Matching messages' });
  await within(pane).findByText('thinking');
  const list = dialog.getByRole('region', { name: 'Matching sessions' });
  await expect(within(list).getByRole('region', { name: 'tower search results' })).toBeVisible();
  return { pane: pane.getBoundingClientRect(), list: list.getBoundingClientRect() };
}

export const BesideTheList: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => (
    <main className="flex h-dvh min-h-0 w-full min-w-0 overflow-hidden bg-ink">
      <div className={sidebarRailClass(true)} data-testid="rail">
        <SearchedList />
      </div>
      <EmptyPane sidebar={{ isShown: true, onToggle: fn() }} />
    </main>
  ),
  play: async ({ canvasElement }) => {
    const { pane, list } = await boxes(canvasElement);
    // Side by side: the messages start where the sessions end, from the same top.
    await expect(pane.left).toBeGreaterThanOrEqual(list.right - 1);
    await expect(Math.abs(pane.top - list.top)).toBeLessThan(1);
    // The dialog stands over the app, no narrower than the rail it was opened from.
    const rail = within(canvasElement).getByTestId('rail').getBoundingClientRect();
    await expect(pane.right - list.left).toBeGreaterThanOrEqual(rail.width);
  },
};

export const UnderTheListOnAPhone: Story = {
  render: () => (
    <main className="h-dvh w-full bg-ink">
      <div className="h-full">
        <SearchedList />
      </div>
    </main>
  ),
  play: async ({ canvasElement }) => {
    const { pane, list } = await boxes(canvasElement);
    // Stacked: the messages sit under the sessions, across the whole width.
    await expect(pane.top).toBeGreaterThanOrEqual(list.bottom - 1);
    await expect(Math.abs(pane.width - list.width)).toBeLessThan(1);
  },
};

/** A search catalog of its own: the fleet rows carry no project or group ids. */
type ScopeCatalog = {
  projects: { id: string; name: string; workspace_root: string }[];
  groups: { id: string; name: string; project_id: string; color: string }[];
  /** The project and group of each answered session, by session id. */
  places: Record<string, { project_id: string; group_id: string | null }>;
};

const WORKBENCH = { id: 'p-workbench', name: 'Workbench', workspace_root: '/work' };
const ARCHIVE = { id: 'p-archive', name: 'Archive', workspace_root: '/archive' };
const PLANNING = { id: 'g-planning', name: 'Planning', project_id: WORKBENCH.id, color: 'red' };
const REVIEW = { id: 'g-review', name: 'Review', project_id: WORKBENCH.id, color: 'blue' };
const HISTORY = { id: 'g-history', name: 'History', project_id: ARCHIVE.id, color: 'green' };

/** Serve the catalog and scope the search answer the way the gateway does. */
function scopeFetch(catalog: ScopeCatalog): typeof fetch {
  const fleet = storyFleetFetch();
  return (async (input: RequestInfo | URL, init?: RequestInit) => {
    const href = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
    const url = new URL(href);
    if (url.pathname === '/v1/projects') return Response.json({ projects: catalog.projects });
    if (url.pathname === '/v1/session-groups') {
      const project = catalog.projects.find((one) => one.workspace_root === url.searchParams.get('root'));
      return Response.json({ groups: catalog.groups.filter((group) => group.project_id === project?.id) });
    }
    if (url.pathname === '/v1/sessions/actions/search') {
      const projectId = url.searchParams.get('project_id');
      const groupIds = url.searchParams.get('group_ids')?.split(',') ?? null;
      const sessions = ANSWER.sessions
        .filter((session) => catalog.places[session.id])
        .map((session) => ({ ...session, ...catalog.places[session.id] }))
        .filter((session) => (!projectId || session.project_id === projectId)
          && (!groupIds || groupIds.includes(session.group_id ?? '')));
      return Response.json({ ...ANSWER, sessions, total: sessions.length });
    }
    return fleet(input, init);
  }) as typeof fetch;
}

/** The search dialog over the list, on a catalog of its own. */
function scopedStory(catalog: ScopeCatalog, check: (dialog: ReturnType<typeof within>) => Promise<void>): Story {
  return {
    globals: { viewport: { value: 'desktop', isRotated: false } },
    beforeEach: () => {
      const previous = globalThis.fetch;
      globalThis.fetch = scopeFetch(catalog);
      return () => {
        globalThis.fetch = previous;
      };
    },
    render: () => (
      <main className="h-dvh w-full bg-ink">
        <div className="h-full">
          <SearchedList />
        </div>
      </main>
    ),
    play: async ({ canvasElement }) => {
      const dialog = within(
        await within(canvasElement.ownerDocument.body).findByRole('dialog', { name: 'Search sessions' }),
      );
      await dialog.findByText('2 matches');
      await check(dialog);
    },
  };
}

/** Two projects: the project is a choice; groups wait until one project narrows the search. */
export const ScopesAcrossProjects: Story = scopedStory({
  projects: [WORKBENCH, ARCHIVE],
  groups: [PLANNING, REVIEW, HISTORY],
  places: {
    [STORY_SESSION_SEARCH_MATCH.sessionId]: { project_id: WORKBENCH.id, group_id: PLANNING.id },
    '41d78df4': { project_id: ARCHIVE.id, group_id: null },
  },
}, async (dialog) => {
  await expect(await dialog.findByRole('combobox', { name: 'Project' })).toBeVisible();
  await expect(dialog.queryByRole('combobox', { name: 'Groups' })).toBeNull();
});

/** One project with two groups: nothing to choose about the project, its groups are the choice. */
export const ScopesInOneProject: Story = scopedStory({
  projects: [WORKBENCH],
  groups: [PLANNING, REVIEW],
  places: {
    [STORY_SESSION_SEARCH_MATCH.sessionId]: { project_id: WORKBENCH.id, group_id: PLANNING.id },
    '41d78df4': { project_id: WORKBENCH.id, group_id: REVIEW.id },
  },
}, async (dialog) => {
  await expect(await dialog.findByRole('combobox', { name: 'Groups' })).toBeVisible();
  await expect(dialog.queryByRole('combobox', { name: 'Project' })).toBeNull();
});

/** One project and one group: nothing to choose and nothing to tell the rows apart by. */
export const ScopesInOneProjectAndGroup: Story = scopedStory({
  projects: [WORKBENCH],
  groups: [PLANNING],
  places: {
    [STORY_SESSION_SEARCH_MATCH.sessionId]: { project_id: WORKBENCH.id, group_id: PLANNING.id },
    '41d78df4': { project_id: WORKBENCH.id, group_id: PLANNING.id },
  },
}, async (dialog) => {
  await expect(dialog.queryByRole('combobox', { name: 'Project' })).toBeNull();
  await expect(dialog.queryByRole('combobox', { name: 'Groups' })).toBeNull();
});
