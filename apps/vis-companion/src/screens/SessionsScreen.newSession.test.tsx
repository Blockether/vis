// @vitest-environment jsdom
import { screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

const named = (pattern: RegExp) =>
  screen.queryAllByRole('button').filter((button) => {
    const name = button.getAttribute('aria-label') ?? button.textContent ?? '';
    return pattern.test(name);
  });

const alpha = () => [
  {
    label: 'alpha',
    sessions: [listSession({ id: 'a1', title: 'First' })],
  },
];

const sessionsMenu = async (root = '/Users/dev/project', project = 'project') => {
  await userEvent.click(screen.getByRole('button', { name: `Actions for sessions in ${root}` }));
  return within(await screen.findByRole('dialog', { name: `Sessions in ${project}` }));
};

const startSession = async () => {
  await userEvent.click((await sessionsMenu()).getByRole('button', { name: 'New session' }));
};

// Regression, user report (new sessions belong to a project, not a machine): the create
// action must use the project's canonical root, never a home-shortened display path.
describe('where "New session" lives', () => {
  // Regression, user report (paraphrased: creating a new session from the app took
  // several seconds): the screen used to await its own full fleet re-read — one
  // request per 100-row window, serially, per machine — before opening the session
  // it had just created, so the wait the user paid for was a list they were leaving.
  it('opens the created session without waiting for the list to reload', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: alpha(),
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText('First');

    // The post-create fleet re-read never lands while this test watches.
    view.holdList();
    view.requests.length = 0;
    await startSession();

    await waitFor(() => expect(opened).toHaveLength(1));
    expect(opened[0]).toMatch(/^created-/);
    // ...and it was opened as a FRESH session, with the list read still in flight.
    view.releaseList();
  });

  // Regression, user report (paraphrased: why must I tap a banner to see the session
  // I just created — it should simply be on the list): the row this device had just
  // minted was counted as a promotion from elsewhere and parked behind the
  // "1 newer session" pill.
  it('puts the session it just created on the list without a tap', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: alpha(),
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText('First');

    view.holdList();
    await startSession();
    await waitFor(() => expect(opened).toHaveLength(1));

    // The gateway now carries the created session, fresher than everything held.
    view.setRows(0, [
      listSession({
        id: opened[0]!,
        title: 'Just made',
        modified_at: new Date('2024-05-01T11:00:00Z').toISOString(),
      }),
      listSession({ id: 'a1', title: 'First' }),
    ]);
    view.releaseList();

    await screen.findByText('Just made');
    expect(named(/newer session/)).toHaveLength(0);
  });

  it('sends the canonical workspace root, never the home-shortened display path', async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText('First');
    // The header names the project only. The canonical root never shows on the screen,
    // not even as a hover title, but the create request still carries it.
    expect(screen.queryByText(/~\/project|\/Users\/dev\/project/)).toBeNull();
    expect(screen.queryByTitle('/Users/dev/project')).toBeNull();
    expect(screen.getByRole('button', { name: 'Collapse project' })).toBeVisible();
    expect(screen.getByText('1 session')).toBeVisible();

    view.requests.length = 0;
    await startSession();

    // ...and SENDS the real one.
    const create = view.requests.find(
      (request) => request.method === 'POST' && request.path === '/v1/sessions',
    );
    expect(create?.body).toEqual({
      channel: 'web',
      root: '/Users/dev/project',
      idempotency_key: expect.stringMatching(/^companion:/),
    });
  });

  // Regression, issue #384: a retry after a create that never answered made a second
  // session. The retry now sends the same key, so the gateway answers the first session.
  it('retries a failed create with the same key and uses a new key after success', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: [{ ...alpha()[0], failCreates: 1 }],
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText('First');

    const createKeys = () =>
      view.requests
        .filter((request) => request.method === 'POST' && request.path === '/v1/sessions')
        .map((request) => (request.body as { idempotency_key: string }).idempotency_key);

    view.requests.length = 0;
    await startSession();
    await waitFor(() => expect(createKeys()).toHaveLength(1));
    expect(opened).toHaveLength(0);

    await startSession();
    await waitFor(() => expect(opened).toHaveLength(1));
    await startSession();
    await waitFor(() => expect(opened).toHaveLength(2));

    const [failed, retried, next] = createKeys();
    expect(retried).toBe(failed);
    expect(next).not.toBe(retried);
  });

  // Regression, user report, Vis session 2692dc5c-b2fd-4502-b7fa-5fe440a16c1d (paraphrased:
  // with two projects added, a new session took its prompt to the first project).
  it('starts the session in the project it was asked on, not the first one', async () => {
    const added = (root: string, id: string) => ({
      root,
      project_id: id,
      name: '',
      session_count: 0,
      live_count: 0,
      awaiting_count: 0,
      last_activity_ms: 0,
    });
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [],
          projects: [added('/Users/dev/wallet', 'p-wallet'), added('/Users/dev/notes', 'p-notes')],
        },
      ],
    });
    restore = view.restore;
    await screen.findByLabelText('notes sessions');

    view.requests.length = 0;
    const menu = await sessionsMenu('/Users/dev/notes', 'notes');
    await userEvent.click(menu.getByRole('button', { name: 'New session' }));

    await waitFor(() =>
      expect(
        view.requests.find((request) => request.method === 'POST' && request.path === '/v1/sessions')
          ?.body,
      ).toEqual({
        channel: 'web',
        root: '/Users/dev/notes',
        idempotency_key: expect.stringMatching(/^companion:/),
      }),
    );
  });

  // Regression, user report: `Use project` said it added a project but created and opened
  // a session instead. A project may be empty; the gateway's idempotent ensure route owns it.
  it('adds the chosen folder as a project without inventing a session', async () => {
    const opened: string[] = [];
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
          routes: {
            '/v1/fs': {
              path: '/Users/dev',
              parent: '/Users',
              home: '/Users/dev',
              is_truncated: false,
              entries: [],
            },
          },
        },
      ],
      onOpen: (_conn, sid) => opened.push(sid),
    });
    restore = view.restore;
    await screen.findByText('First');

    await userEvent.click(screen.getByRole('button', { name: 'New project on alpha' }));
    await screen.findByText('No folders in here.');
    view.requests.length = 0;
    await userEvent.click(screen.getByRole('button', { name: /^Use project/ }));

    await waitFor(() =>
      expect(
        view.requests.find(
          (request) => request.method === 'POST' && request.path === '/v1/projects/actions/ensure',
        )?.body,
      ).toEqual({ root: '/Users/dev' }),
    );
    expect(
      view.requests.some((request) => request.method === 'POST' && request.path === '/v1/sessions'),
    ).toBe(false);
    expect(opened).toEqual([]);
  });

  // Regression, user report, Vis session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
  // the empty state pointed at an overflow menu that did not exist, then adding the first
  // project left that dead end over its header and made New session unreachable.
  it('guides the first project into a usable session action', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [],
          routes: {
            '/v1/fs': {
              path: '/Users/dev',
              parent: '/Users',
              home: '/Users/dev',
              is_truncated: false,
              entries: [],
            },
          },
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('No projects yet')).toBeInTheDocument();
    expect(screen.queryByText(/Open the .* menu/)).toBeNull();

    await userEvent.click(screen.getByRole('button', { name: 'New project on alpha' }));
    await screen.findByText('No folders in here.');
    await userEvent.click(screen.getByRole('button', { name: /^Use project/ }));

    const project = await screen.findByLabelText('dev sessions');
    expect(
      within(project).getByRole('button', { name: 'Actions for sessions in /Users/dev' }),
    ).toBeVisible();
    expect(
      (await sessionsMenu('/Users/dev', 'dev')).getByRole('button', { name: 'New session' }),
    ).toBeEnabled();
  });
});

// Regression, user report ("there is no much difference visually between the machine and
// the project"): the list carried TWO header bands one hairline apart. A machine is not a
// header any more — it is a tag and the spine down everything it owns — and the project
// header stays the only header BAND in the list.
describe('machine, project and session are three different shapes', () => {
  // Regression, user report ("offline stuff should just not be accessible"): a machine
  // that was not answering was still a tab that scoped the whole screen to it.
  it('refuses to scope a fleet to a machine that is not answering', async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: 'alpha', sessions: [listSession({ id: 'a1', title: 'First' })] },
        { label: 'beta', down: true },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');
    const strip = within(screen.getByLabelText('Machines'));

    await userEvent.click(await strip.findByRole('button', { name: /^Reconnect to beta/ }));
    // Pressing it retries beta but never displaces the active answering machine.
    await waitFor(() =>
      expect(strip.getByRole('button', { name: /^alpha/ }).getAttribute('aria-pressed')).toBe(
        'true',
      ),
    );
    expect(screen.getByText('First')).toBeVisible();
  });
});

// Regression, user report (paraphrased: the machine tab must be rendered on the left
// whether we have one machine or more): the strip appeared only above two machines, so a
// solo user's list said nowhere which computer it was on, and pairing a second machine
// rearranged the screen. Pairing itself still lives in Preferences, not on this strip.
describe('the machine strip', () => {
  it('stands for a fleet of one too: one tab, already pressed, and no All', async () => {
    const view = renderSessionsScreen({ machines: alpha() });
    restore = view.restore;
    await screen.findByText('First');

    const strip = within(screen.getByLabelText('Machines'));
    const tabs = strip.getAllByRole('button');
    // "Every machine" and "this machine" would be the same list under two names.
    expect(tabs.map((tab) => tab.firstChild?.textContent)).toEqual(['alpha']);
    expect(tabs[0]!.getAttribute('aria-pressed')).toBe('true');
  });
});
