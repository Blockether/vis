// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// The archive is a fact about the session and the GATEWAY holds it. The list asks for the
// sessions that still stand and is answered with exactly those; a row put away from the
// screen leaves on the answer that stamped it, not on a poll five seconds later.
describe('the sessions list', () => {
  it('does not stand a session the gateway put away', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({
              id: 'filed',
              title: 'Filed session',
              modified_at: '2024-05-01T11:00:00Z',
              archived_at: 1717,
            }),
            listSession({
              id: 'open',
              title: 'Open session',
              modified_at: '2024-05-01T09:00:00Z',
            }),
          ],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('Open session');
    expect(screen.queryByText('Filed session')).not.toBeInTheDocument();
  });

  it('lets an archived row go at once, and asks the machine exactly once', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 'open', title: 'Open session' }),
            listSession({
              id: 'other',
              title: 'Other session',
              modified_at: '2024-05-01T09:00:00Z',
            }),
          ],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('Open session');

    const actions = screen.getByRole('group', { name: 'Open session actions' });
    await userEvent.click(within(actions).getByRole('button', { name: 'Archive' }));

    await waitFor(() => expect(screen.queryByText('Open session')).not.toBeInTheDocument());
    expect(screen.getByText('Other session')).toBeInTheDocument();
    const stamped = view.requests.filter((request) => request.method === 'PATCH');
    expect(stamped).toHaveLength(1);
    expect(stamped[0].path).toContain('/v1/sessions/open');
    expect(stamped[0].body).toEqual({ archived: true });
  });

  // Regression for #387: an archive left a hole in the project's page and kept the old
  // pager until a reload. The page and the count must read again after the archive.
  it('fills the page and the count again after an archive on a deeper page', async () => {
    window.innerHeight = 844;
    const at = (rank: number) => new Date(Date.UTC(2024, 4, 1, 10, 0, rank)).toISOString();
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: Array.from({ length: 46 }, (_, index) =>
            listSession({
              id: `s${index}`,
              title: `alpha ${String(index).padStart(2, '0')}`,
              workspace: { root: '/Users/dev/alpha' },
              modified_at: at(60 - index),
            }),
          ),
        },
      ],
    });
    restore = () => {
      view.unmount();
      view.restore();
      window.innerHeight = 768;
    };
    const shown = () => view.queryAllByText(/^alpha \d\d$/).map((node) => node.textContent);
    await waitFor(() => expect(shown()).toHaveLength(15));
    fireEvent.click(view.getByLabelText('Next page'));
    await waitFor(() => expect(shown()[0]).toBe('alpha 15'));
    const pageCount = () => view.getByRole('navigation', { name: 'Pages of alpha sessions' }).textContent;
    expect(pageCount()).toContain('4');

    const actions = screen.getByRole('group', { name: 'alpha 25 actions' });
    await userEvent.click(within(actions).getByRole('button', { name: 'Archive' }));

    // Page two moves up by one row, and the project now has three pages, not four. The list
    // polls every five seconds, so a shorter wait proves that the archive itself re-reads.
    await waitFor(() => expect(shown()).toContain('alpha 30'), { timeout: 2_000 });
    expect(shown()).not.toContain('alpha 25');
    expect(shown()).toHaveLength(15);
    await waitFor(() => expect(pageCount()).not.toContain('4'), { timeout: 2_000 });
  });

  // #387: the Sessions set archives all of its sessions at once. It asks first with the
  // count, keeps a working session and reads the list again.
  it('archives every session of the set after a confirmation, and keeps a working one', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            ...['one', 'two', 'three'].map((name, index) =>
              listSession({
                id: `alpha-${name}`,
                title: `alpha ${name}`,
                workspace: { root: '/Users/dev/alpha' },
                modified_at: `2024-05-01T1${index}:00:00Z`,
              }),
            ),
            listSession({
              id: 'alpha-busy',
              title: 'alpha busy',
              status: 'running',
              workspace: { root: '/Users/dev/alpha' },
            }),
          ],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('alpha one');

    await userEvent.click(screen.getByRole('button', { name: 'Actions for sessions in /Users/dev/alpha' }));
    await userEvent.click(await screen.findByRole('button', { name: 'Archive all' }));

    // The dry run counts the set and changes nothing.
    const confirm = await screen.findByRole('button', { name: 'Archive 3 sessions' });
    expect(screen.getByText('1 working session stays in the list.')).toBeInTheDocument();
    const bulk = () =>
      view.requests.filter((request) => request.path.startsWith('/v1/sessions/actions/archive'));
    expect(bulk().map((request) => request.body)).toEqual([
      { root: '/Users/dev/alpha', loose: true, dry_run: true },
    ]);
    expect(screen.getByText('alpha one')).toBeInTheDocument();

    await userEvent.click(confirm);
    expect(await screen.findByText(/Archived 3 sessions\./)).toBeInTheDocument();
    expect(bulk().at(-1)?.body).toEqual({ root: '/Users/dev/alpha', loose: true, dry_run: false });
    await waitFor(() => expect(screen.queryByText('alpha one')).not.toBeInTheDocument(), {
      timeout: 2_000,
    });
    expect(screen.queryByText('alpha three')).not.toBeInTheDocument();
    expect(screen.getByText('alpha busy')).toBeInTheDocument();
  });
});
