// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

const machines = () => [
  {
    label: 'alpha',
    sessions: [
      listSession({ id: 'a1', title: 'First' }),
      listSession({ id: 'a2', title: 'Second' }),
    ],
  },
];

// Deleting ONE session is a row-level answer to a row-level question, so it is
// asked IN the row: two full-width answers standing the row's own height. Renaming
// still needs a field; project deletion asks inside the project's own menu.
describe('deleting one session confirms inside its own row', () => {
  it('asks in the row, not in a dialog, and only that row is asked', async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText('First');

    fireEvent.click(
      screen
        .getByRole('group', { name: 'First actions' })
        .querySelector("button[aria-label='Delete']")!,
    );

    const strip = await screen.findByRole('group', { name: 'Delete First?' });
    expect(strip.querySelectorAll('button')).toHaveLength(2);
    expect(screen.queryByRole('dialog')).toBeNull();
    // The frame must not add border pixels around the row-height answer strip.
    expect(strip.classList.contains('border')).toBe(false);
    // The neighbour keeps its own row: one question, one row.
    expect(screen.queryByRole('group', { name: 'Delete Second?' })).toBeNull();
    expect(screen.getByText('Second')).toBeVisible();
  });

  it('deletes on yes and asks the gateway for exactly that session', async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    fireEvent.click(
      screen
        .getByRole('group', { name: 'First actions' })
        .querySelector("button[aria-label='Delete']")!,
    );
    fireEvent.click(await screen.findByText('Yes, delete'));

    await waitFor(() =>
      expect(
        view.requests.some(
          (request) => request.method === 'DELETE' && request.path === '/v1/sessions/a1',
        ),
      ).toBe(true),
    );
    await waitFor(() => expect(screen.queryByText('First')).toBeNull());
    expect(screen.getByText('Second')).toBeVisible();
  });

  it('no keeps the session: the row comes back and nothing is sent', async () => {
    const view = renderSessionsScreen({ machines: machines() });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    fireEvent.click(
      screen
        .getByRole('group', { name: 'First actions' })
        .querySelector("button[aria-label='Delete']")!,
    );
    fireEvent.click(await screen.findByText('No, keep'));

    await waitFor(() => expect(screen.queryByRole('group', { name: 'Delete First?' })).toBeNull());
    expect(screen.getByText('First')).toBeVisible();
    expect(view.requests.some((request) => request.method === 'DELETE')).toBe(false);
  });
});

// Regression, user report (paraphrased: a project needs the same right-click and
// three-dot menu as every other row): the project band holds exactly one menu, its own.
// It holds no row actions and no swipe track, and creation stays in the Sessions set.
describe('a project band holds only its own menu', () => {
  it.each([0, 2])('keeps one project menu on the band with %i sessions', async (count) => {
    const view = renderSessionsScreen({
      machines: [
        {
          ...machines()[0],
          sessions: machines()[0].sessions.slice(0, count),
          projects: [
            {
              project_id: 'p-project',
              root: '/Users/dev/project',
              name: 'project',
              session_count: count,
              live_count: 0,
              awaiting_count: 0,
              last_activity_ms: 0,
            },
          ],
        },
      ],
    });
    restore = view.restore;
    const project = await screen.findByRole('region', { name: 'project sessions' });
    const header = project.querySelector('header')!;

    const menus = within(header).getAllByRole('button', { name: /^Actions for / });
    expect(menus).toHaveLength(1);
    expect(menus[0]).toHaveAccessibleName('Actions for project');
    expect(menus[0]).toHaveAttribute('aria-haspopup', 'dialog');
    expect(within(header).queryByRole('group', { name: 'project actions' })).toBeNull();
    expect(header.querySelector('[data-swipe-track]')).toBeNull();
    // Creation stays in the Sessions set, even when it has no rows.
    const sessionsAction = within(project).getByRole('button', {
      name: 'Actions for sessions in /Users/dev/project',
    });
    expect(sessionsAction).toBeEnabled();
    fireEvent.click(sessionsAction);
    const menu = within(await screen.findByRole('dialog', { name: 'Sessions in project' }));
    expect(menu.getByRole('button', { name: 'New session' })).toBeEnabled();
    if (count > 0) {
      fireEvent.click(within(header).getByRole('button', { name: 'Collapse project' }));
      expect(within(project).queryByText('First')).toBeNull();
      fireEvent.click(within(header).getByRole('button', { name: 'Expand project' }));
      expect(await within(project).findByText('First')).toBeInTheDocument();
    } else {
      expect(within(header).getByRole('button', { name: 'Collapse project' })).toBeEnabled();
    }
    expect(screen.getByRole('button', { name: 'New project on alpha' })).toBeEnabled();
    expect(view.requests.some((request) => request.method === 'DELETE')).toBe(false);
  });

  // Regression, user report: a completed project deletion left its "Deleting..."
  // rectangle in place. A same-root group can survive the saved project's removal
  // (for example, sessions not assigned to that project); success must settle the UI.
  it('clears the completed confirmation when the root group stays mounted', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'Unassigned session' })],
          projects: [
            {
              project_id: 'p-project',
              root: '/Users/dev/project',
              name: 'project',
              session_count: 0,
              live_count: 0,
              awaiting_count: 0,
              last_activity_ms: 0,
            },
          ],
          routes: { '/v1/projects/p-project': { deleted_session_ids: [] } },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('Unassigned session');
    view.requests.length = 0;
    let complete!: () => void;
    const response = new Promise<void>((resolve) => {
      complete = resolve;
    });
    const gateway = globalThis.fetch;
    globalThis.fetch = async (input, init) => {
      if (init?.method === 'DELETE') await response;
      return gateway(input, init);
    };

    fireEvent.click(screen.getByRole('button', { name: 'Actions for project' }));
    const menu = await screen.findByRole('dialog', { name: 'project' });
    fireEvent.click(within(menu).getByRole('button', { name: 'Delete project' }));
    fireEvent.click(await screen.findByRole('button', { name: /^Delete it and its sessions/ }));
    expect(await screen.findByRole('button', { name: /^Deleting\.\.\./ })).toBeDisabled();
    await act(async () => complete());

    await waitFor(() => expect(screen.queryByRole('dialog', { name: 'project' })).toBeNull());
    expect(screen.queryByText('Deleting...')).toBeNull();
    expect(screen.getByText('Unassigned session')).toBeInTheDocument();
    expect(
      view.requests.filter((request) => request.method === 'DELETE').map((request) => request.path),
    ).toEqual(['/v1/projects/p-project?is_recursive=true']);
  });
});

// The project's own menu asks before it deletes, in the same sheet: no second dialog,
// and nothing reaches the machine until the reader confirms.
describe('deleting a project asks inside its own menu', () => {
  it('keeps one sheet, offers a way back, and deletes only on the second answer', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');
    fireEvent.click(screen.getByRole('button', { name: 'Actions for project' }));
    const sheet = await screen.findByRole('dialog', { name: 'project' });
    view.requests.length = 0;
    fireEvent.click(within(sheet).getByRole('button', { name: 'Delete project' }));

    await screen.findByRole('button', { name: /^Delete it and its sessions/ });
    expect(screen.getAllByRole('dialog')).toEqual([sheet]);
    expect(view.requests.some((request) => request.method === 'DELETE')).toBe(false);

    fireEvent.click(screen.getByRole('button', { name: 'Keep the project' }));
    fireEvent.click(await screen.findByRole('button', { name: 'Delete project' }));
    fireEvent.click(await screen.findByRole('button', { name: /^Delete it and its sessions/ }));

    await waitFor(() =>
      expect(
        view.requests.some(
          (request) => request.method === 'DELETE' && request.path === '/v1/sessions/a1',
        ),
      ).toBe(true),
    );
    await waitFor(() => expect(screen.queryByRole('dialog', { name: 'project' })).toBeNull());
  });
});

// Regression, issue #2216: deleting ONE session re-read the WHOLE fleet's session
// list — every paired machine, every window of it — although the app already knew
// exactly which row had gone. On a few-hundred-session store that was ~315 KB
// re-downloaded, on every machine, to remove one row the app had just removed.
describe('deleting a session does not re-download the fleet', () => {
  const fleet = () => [
    {
      label: 'alpha',
      sessions: [
        listSession({ id: 'a1', title: 'First' }),
        listSession({ id: 'a2', title: 'Second' }),
      ],
    },
    { label: 'beta', sessions: [listSession({ id: 'b1', title: 'Elsewhere' })] },
  ];

  const listReads = (view: { requests: { method: string; path: string }[] }) =>
    view.requests.filter(
      (request) =>
        request.method === 'GET' &&
        request.path.startsWith('/v1/sessions?') &&
        // A project's page is a read of ITS own (`GatewayClient.listProjectPage`).
        !request.path.includes('root='),
    );

  it('drops the active row locally and re-lists nothing', async () => {
    const view = renderSessionsScreen({ machines: fleet() });
    restore = view.restore;
    await screen.findByText('First');
    expect(screen.queryByText('Elsewhere')).toBeNull();
    view.requests.length = 0;

    fireEvent.click(
      screen
        .getByRole('group', { name: 'First actions' })
        .querySelector("button[aria-label='Delete']")!,
    );
    fireEvent.click(await screen.findByText('Yes, delete'));

    // The row goes because the delete succeeded, not because a fresh list said so.
    await waitFor(() => expect(screen.queryByText('First')).toBeNull());
    expect(screen.getByText('Second')).toBeVisible();
    expect(screen.queryByText('Elsewhere')).toBeNull();
    expect(listReads(view)).toEqual([]);
  });

  it("renames from the gateway's own answer, re-listing nothing", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
          routes: { '/v1/sessions/a1': listSession({ id: 'a1', title: 'Renamed' }) },
        },
        { label: 'beta', sessions: [listSession({ id: 'b1', title: 'Elsewhere' })] },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');
    expect(screen.queryByText('Elsewhere')).toBeNull();
    view.requests.length = 0;

    fireEvent.click(
      screen
        .getByRole('group', { name: 'First actions' })
        .querySelector("button[aria-label='Rename']")!,
    );
    const field = await screen.findByRole('textbox', { name: 'Rename First' });
    expect(screen.queryByRole('dialog')).toBeNull();
    fireEvent.change(field, { target: { value: 'Renamed' } });
    fireEvent.keyDown(field, { key: 'Enter' });

    await waitFor(() => expect(screen.getByText('Renamed')).toBeVisible());
    expect(
      view.requests
        .filter((request) => request.method === 'PATCH')
        .map((request) => [request.path, request.body]),
    ).toEqual([['/v1/sessions/a1', { title: 'Renamed' }]]);
    expect(screen.queryByText('Elsewhere')).toBeNull();
    expect(listReads(view)).toEqual([]);
  });
});

// User request: Shift-select several sessions, then the regular delete deletes all of them.
describe('deleting a Shift-selected batch of sessions', () => {
  const batch = () => [
    {
      label: 'alpha',
      sessions: [
        listSession({ id: 'b1', title: 'One' }),
        listSession({ id: 'b2', title: 'Two' }),
        listSession({ id: 'b3', title: 'Three' }),
      ],
    },
  ];
  const surface = (sid: string) =>
    document.querySelector(`[data-session-id="${sid}"]`) as HTMLElement;
  const deletes = (view: ReturnType<typeof renderSessionsScreen>) =>
    view.requests
      .filter((request) => request.method === 'DELETE')
      .map((request) => request.path)
      .sort();

  async function selectAll() {
    const matchMedia = window.matchMedia;
    vi.spyOn(window, 'matchMedia').mockImplementation((query) => ({
      ...matchMedia(query),
      matches: query === '(pointer: fine)',
    }));
    const view = renderSessionsScreen({ machines: batch() });
    restore = () => {
      view.restore();
      vi.restoreAllMocks();
    };
    await screen.findByText('One');
    fireEvent.click(surface('b1'));
    fireEvent.click(surface('b3'), { shiftKey: true });
    for (const sid of ['b1', 'b2', 'b3']) {
      expect(surface(sid)).toHaveAttribute('aria-pressed', 'true');
    }
    view.requests.length = 0;
    return view;
  }

  it('the Delete key asks once and deletes every selected session', async () => {
    const view = await selectAll();
    fireEvent.keyDown(window, { key: 'Delete' });
    await screen.findByRole('group', { name: 'Delete 3 selected sessions?' });
    expect(deletes(view)).toEqual([]);
    fireEvent.click(await screen.findByText('Yes, delete'));
    await waitFor(() =>
      expect(deletes(view)).toEqual(['/v1/sessions/b1', '/v1/sessions/b2', '/v1/sessions/b3']),
    );
    await waitFor(() => expect(screen.queryByText('Two')).toBeNull());
    expect(screen.queryByText('One')).toBeNull();
    expect(screen.queryByText('Three')).toBeNull();
  });

  it('the Backspace key asks too, and No keeps every session', async () => {
    const view = await selectAll();
    fireEvent.keyDown(window, { key: 'Backspace' });
    fireEvent.click(await screen.findByText('No, keep'));
    await waitFor(() =>
      expect(screen.queryByRole('group', { name: 'Delete 3 selected sessions?' })).toBeNull(),
    );
    expect(deletes(view)).toEqual([]);
    expect(screen.getByText('Two')).toBeVisible();
  });

  it("a selected row's own Delete action deletes the whole selection", async () => {
    const view = await selectAll();
    fireEvent.click(
      screen
        .getByRole('group', { name: 'Two actions' })
        .querySelector("button[aria-label='Delete']")!,
    );
    fireEvent.click(await screen.findByText('Yes, delete'));
    await waitFor(() =>
      expect(deletes(view)).toEqual(['/v1/sessions/b1', '/v1/sessions/b2', '/v1/sessions/b3']),
    );
  });

  it('a key typed into a field never deletes the selection', async () => {
    const view = await selectAll();
    const field = document.createElement('input');
    document.body.append(field);
    fireEvent.keyDown(field, { key: 'Backspace' });
    field.remove();
    expect(screen.queryByRole('group', { name: 'Delete 3 selected sessions?' })).toBeNull();
    expect(deletes(view)).toEqual([]);
  });
});
