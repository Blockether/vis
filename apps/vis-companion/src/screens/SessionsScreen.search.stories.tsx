import type { Meta, StoryObj } from '@storybook/react-vite';
import { fn } from 'storybook/test';

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
  title: 'Session list/Search/Split',
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

export const Default: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => (
    <main className="flex h-dvh min-h-0 w-full min-w-0 overflow-hidden bg-ink">
      <div className={sidebarRailClass(true)} data-testid="rail">
        <SearchedList />
      </div>
      <EmptyPane sidebar={{ isShown: true, onToggle: fn() }} />
    </main>
  ),
};
