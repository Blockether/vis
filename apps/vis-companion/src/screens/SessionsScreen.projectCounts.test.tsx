// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// Regression, user report (paraphrased: switching between gateways flickered the
// project list and then the numbers in it): the counts were a tally of the session
// windows this device had paged in, so a project of 400 read `1` until the whole
// list had drained, and re-derived itself on every gateway switch.
describe('what a project header counts', () => {
  it('gets rows and stable gateway totals in one list request', async () => {
    const overview = {
      projects: [
        {
          root: '/Users/dev/project',
          project_id: 'p-a',
          name: 'project',
          session_count: 400,
          live_count: 3,
          awaiting_count: 1,
          last_activity_ms: 1,
        },
      ],
      project_count: 1,
      session_count: 400,
      live_count: 3,
      awaiting_count: 1,
    };
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
          routes: {
            '/v1/sessions': {
              sessions: [listSession({ id: 'a1', title: 'First' })],
              total: 1,
              has_more: false,
              overview,
            },
          },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');

    // One row is on screen; the header still says what the project holds.
    expect(await screen.findByText('400 sessions')).toBeVisible();
    expect(screen.getAllByText(/2 live/).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/1 needs input/).length).toBeGreaterThan(0);
    // One list read, and a project's own page is the only other (`listProjectPage`).
    expect(
      view.requests.filter(
        ({ path }) => path.startsWith('/v1/sessions?') && !path.includes('root='),
      ),
    ).toHaveLength(1);
    expect(view.requests.some(({ path }) => path === '/v1/projects/overview')).toBe(false);
  });
});

// Regression: path-derived project names were repeated above the session count.
describe('what a project header names', () => {
  it.each([
    { name: '~/project', root: '/Users/dev/project', qualifier: '' },
    { name: '/project', root: '/project', qualifier: '' },
    { name: '~/project', root: 'C:\\Users\\dev\\project', qualifier: '' },
    { name: 'project', root: '/Users/dev/project', qualifier: '' },
    { name: 'project', root: '/Users/dev/work/project', qualifier: '~/work' },
  ])(
    'shows $name once and preserves distinct checkout details',
    async ({ name, root, qualifier }) => {
      const session = listSession({ id: 'project-row', workspace: { root } });
      const view = renderSessionsScreen({
        machines: [
          {
            label: 'alpha',
            sessions: [session],
            routes: {
              '/v1/sessions': {
                sessions: [session],
                total: 1,
                has_more: false,
                overview: {
                  projects: [
                    {
                      root,
                      project_id: 'p-a',
                      name,
                      session_count: 1,
                      live_count: 0,
                      awaiting_count: 0,
                      last_activity_ms: 1,
                    },
                  ],
                  project_count: 1,
                  session_count: 1,
                  live_count: 0,
                  awaiting_count: 0,
                },
              },
            },
          },
        ],
      });
      restore = view.restore;
      const heading = await screen.findByRole('button', { name: `Collapse ${name}` });
      const header = heading.closest('header')!;
      expect(within(header).getAllByText(name)).toHaveLength(1);
      const detail = header.querySelector('[title]');
      expect(detail).toHaveAttribute('title', root);
      expect(detail?.textContent).toBe(`${qualifier ? `${qualifier} ·` : ''}1 session`);
    },
  );
});

// Regression, measured against a 1192-session machine: opening the list drained the
// WHOLE fleet — twelve serial windows per machine per poll, ~315 KB of rows re-cut
// into a page of ten — and every project header counted the part that had landed so
// far. The head window is all this device reads now, the totals ride beside it, and a
// project's own page is asked for by the group that paints it.
describe('a fleet far deeper than one window', () => {
  it('costs one list read per machine, and counts what the gateway holds', async () => {
    const deep = (prefix: string, count: number, perProject: number) =>
      Array.from({ length: count }, (_, index) =>
        listSession({
          id: `${prefix}-${index}`,
          title: `${prefix} ${index}`,
          workspace: { root: `/Users/dev/${prefix}-p${Math.floor(index / perProject)}` },
          modified_at: new Date(Date.UTC(2024, 4, 1, 0, 0, count - index)).toISOString(),
        }),
      );
    const view = renderSessionsScreen({
      machines: [
        { label: 'alpha', sessions: deep('alpha', 1200, 150) },
        { label: 'beta', sessions: deep('beta', 30, 30) },
      ],
    });
    restore = view.restore;
    await screen.findByText('alpha 0');

    // Every project count is the gateway's own, not a tally of the rows that landed.
    expect(screen.getAllByText('150 sessions').length).toBeGreaterThan(0);
    // Every project the machine holds has a band, whether or not a row of it was
    // in the window: eight of alpha's, however deep the last one sits.
    expect(view.getByLabelText('Expand alpha-p7')).toBeVisible();

    // ONE fleet read per machine, for the head window alone — no `after`, no walk.
    const fleetReads = view.requests.filter(
      ({ path }) => path.startsWith('/v1/sessions?') && !path.includes('root='),
    );
    expect(fleetReads).toHaveLength(2);
    expect(fleetReads.every(({ path }) => !path.includes('after='))).toBe(true);
    expect(new Set(fleetReads.map(({ machine }) => machine)).size).toBe(2);
  });
});

// Regression, user report (paraphrased: the "1 live" in a project header was only
// text — tapping it should open that live run, the way tapping its row does): the
// count is now the door to the newest run the project still has going, and a run
// parked on a human answer does not steal the tap.
describe('the live count on a project band', () => {
  const row = (title: string) =>
    screen.getByText(title).closest('[data-session-id]') as HTMLElement;
  const renderDeepLive = (opened: string[]) =>
    renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ title: 'Another project', workspace: { root: '/Users/dev/other' } }),
            ...Array.from({ length: 120 }, (_, index) =>
              listSession({ id: `quiet-${index}`, title: `Quiet ${index}` }),
            ),
            listSession({ id: 'deep-live', title: 'Deep live run', live: true }),
          ],
        },
      ],
      onOpen: (_conn, sid) => opened.push(sid),
    });

  it('opens the live run from the band, and the rows keep opening themselves', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's1', title: 'Working', live: true }),
            listSession({ id: 's2', title: 'Quiet' }),
          ],
        },
      ],
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;

    const live = await screen.findByRole('button', { name: 'Open the live session' });
    expect(live.textContent).toMatch(/^1 live$/);
    fireEvent.click(live);
    expect(opened).toEqual(['s1']);

    fireEvent.click(screen.getByText('Quiet'));
    expect(opened).toEqual(['s1', 's2']);
  });

  it('opens the newest run when several are live', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: [
        {
          // A starred older run leads the list, but is not the newest live run.
          sessions: [
            listSession({
              id: 'settled',
              title: 'Settled earlier today',
              modified_at: new Date('2024-05-01T11:00:00Z').toISOString(),
            }),
            listSession({
              id: 'newer',
              title: 'Newer run',
              live: true,
              modified_at: new Date('2024-05-01T10:00:00Z').toISOString(),
            }),
            listSession({
              id: 'older',
              title: 'Older run',
              live: true,
              favorite_rank: 0,
              modified_at: new Date('2024-05-01T09:00:00Z').toISOString(),
            }),
          ],
        },
      ],
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;

    const live = await screen.findByRole('button', {
      name: 'Open the newest of 2 live sessions',
    });
    expect(live.textContent).toMatch(/^2 live$/);
    fireEvent.click(live);
    expect(opened).toEqual(['newer']);
  });

  it('skips a run parked on input and opens the one still working', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 'parked', title: 'Parked', live: true, is_awaiting_input: true }),
            listSession({ id: 'working', title: 'Working', live: true }),
          ],
        },
      ],
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;

    // The band counts runs this device could watch, so the parked one — still
    // waiting on its human — is not what the tap walks into.
    const live = await screen.findByRole('button', { name: 'Open the live session' });
    expect(live.textContent).toMatch(/^1 live$/);
    fireEvent.click(live);
    expect(opened).toEqual(['working']);
    expect(within(row('Parked')).getByText('INPUT NEEDED')).toBeInTheDocument();
  });

  it('opens a live run outside the loaded window without expanding its project', async () => {
    const opened: string[] = [];
    const view = renderDeepLive(opened);
    restore = view.restore;

    await screen.findByRole('button', { name: 'Expand project' });
    expect(screen.queryByText('Deep live run')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Open the live session' }));
    await waitFor(() => expect(opened).toEqual(['deep-live']));
    expect(screen.getByRole('button', { name: 'Expand project' })).toBeVisible();
    expect(
      view.requests.some(({ path }) => path.includes('root=') && path.includes('after=')),
    ).toBe(true);
  });

  it('allows retrying when the live-session lookup cannot reach the machine', async () => {
    const opened: string[] = [];
    const view = renderDeepLive(opened);
    restore = view.restore;
    const live = await screen.findByRole('button', { name: 'Open the live session' });
    const fetchPage = globalThis.fetch;
    globalThis.fetch = async () => {
      throw new TypeError('Offline');
    };

    fireEvent.click(live);
    expect(await screen.findByRole('alert')).toHaveTextContent(
      'Could not open the live session. Try again.',
    );
    expect(live).toBeEnabled();
    expect(opened).toEqual([]);

    globalThis.fetch = fetchPage;
    fireEvent.click(live);
    await waitFor(() => expect(opened).toEqual(['deep-live']));
    expect(screen.queryByRole('alert')).toBeNull();
  });

  it('does not navigate after the list is hidden during a lookup', async () => {
    const opened: string[] = [];
    const view = renderDeepLive(opened);
    restore = view.restore;
    const live = await screen.findByRole('button', { name: 'Open the live session' });
    view.holdList();
    fireEvent.click(live);
    fireEvent.click(live);
    expect(live).toBeDisabled();
    expect(live).toHaveAttribute('aria-busy', 'true');
    const lookups = () => view.requests.filter(({ path }) => path.includes('limit=100'));
    await waitFor(() => expect(lookups()).toHaveLength(1));

    view.setVisible(false);
    expect(lookups()[0].signal?.aborted).toBe(true);
    await act(async () => view.releaseList());
    expect(opened).toEqual([]);
  });

  it('explains when the counted run has already finished', async () => {
    const opened: string[] = [];
    const view = renderDeepLive(opened);
    restore = view.restore;
    const live = await screen.findByRole('button', { name: 'Open the live session' });
    view.setRows(0, []);

    fireEvent.click(live);
    expect(await screen.findByRole('alert')).toHaveTextContent(
      'No sessions are running in this project now.',
    );
    expect(opened).toEqual([]);
    expect(live).toBeEnabled();
  });

  it('does not show the active-live count while revealing the archive', async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ title: 'Working', live: true })] }],
    });
    restore = view.restore;

    await screen.findByRole('button', { name: 'Open the live session' });
    fireEvent.click(screen.getByRole('button', { name: 'Groups in project' }));
    fireEvent.click(await screen.findByText('Show archived'));
    await screen.findByText('Archived');
    expect(screen.queryByRole('button', { name: 'Open the live session' })).toBeNull();
  });
});
