// @vitest-environment jsdom
import { act, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient } from '../lib/gateway';
import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  vi.restoreAllMocks();
  restore();
});

// Regression, user report: the list lost its whole-session Fork action, requiring
// opening a transcript and choosing a turn even when the whole session is wanted.
describe('forking a session from its list row', () => {
  it('forks a grouped session on one press and shows the fresh copy in that group', async () => {
    const groupId = 'group-forks';
    const source = listSession({ id: 's1', title: 'A session', group_id: groupId });
    const copy = listSession({ id: 'forked', title: 'A session (fork)', group_id: groupId });
    const opened: Array<[string, boolean | undefined]> = [];
    let view: ReturnType<typeof renderSessionsScreen>;
    view = renderSessionsScreen({
      machines: [
        {
          sessions: [source],
          groups: [
            {
              id: groupId,
              project_id: null,
              name: 'Forks',
              color: 'blue',
              position: 0,
              session_count: 1,
            },
          ],
          routes: { '/v1/sessions/s1/forks': { session: copy } },
        },
      ],
      onOpen: (conn, sid, fresh) => {
        opened.push([sid, fresh]);
        view.setRows(0, [copy, source]);
        view.setOpenSession({ conn, sid, fresh });
      },
    });
    restore = view.restore;
    await screen.findByText('A session');
    expect(await screen.findByRole('button', { name: 'Collapse Forks' })).toBeVisible();

    await userEvent.click(screen.getByRole('button', { name: 'Actions for A session' }));
    const actions = within(await screen.findByRole('dialog', { name: 'A session actions' }));
    await userEvent.click(actions.getByRole('button', { name: 'Fork A session' }));

    await waitFor(() => expect(opened).toEqual([['forked', true]]));
    expect(view.requests.filter((request) => request.path === '/v1/sessions/s1/forks')).toEqual([
      expect.objectContaining({ method: 'POST', body: {} }),
    ]);
    expect(await screen.findByText('A session (fork)')).toBeVisible();
    expect(screen.queryByRole('button', { name: /newer session/ })).toBeNull();
    expect(screen.getByText('A session')).toBeInTheDocument();
    expect(screen.queryByRole('dialog', { name: 'Fork this session' })).toBeNull();
  });

  it('keeps one request in flight and shows a refusal on the source row', async () => {
    let rejectFork!: (cause: Error) => void;
    const fork = vi.spyOn(GatewayClient.prototype, 'forkSession').mockImplementation(
      () => new Promise((_resolve, reject) => {
        rejectFork = reject;
      }),
    );
    const opened = vi.fn();
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: 's1', title: 'A session' })] }],
      onOpen: opened,
    });
    restore = view.restore;
    await screen.findByText('A session');

    const actions = within(screen.getByRole('group', { name: 'A session actions' }));
    const button = actions.getByRole('button', { name: 'Fork A session' });
    await userEvent.click(button);
    expect(button).toHaveTextContent('Forking...');
    await userEvent.click(button);
    expect(fork).toHaveBeenCalledTimes(1);
    expect(fork).toHaveBeenCalledWith('s1');

    await act(async () => rejectFork(new Error('This session has no turns to fork yet')));
    expect(await screen.findByText('This session has no turns to fork yet')).toBeVisible();
    expect(button).toHaveTextContent('Fork');
    expect(opened).not.toHaveBeenCalled();
  });
});
