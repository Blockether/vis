// @vitest-environment jsdom
import { act, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it } from 'vitest';

import { GatewayClient } from '../lib/gateway';
import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
});

// The gateway owns unread answer counts across devices. This list also remembers a
// visit until the gateway's read mark reaches its own paged row; NEW must not flash
// back when leaving the transcript. The row's status mark is otherwise IDLE or DIRTY.
describe('the NEW badge', () => {
  it('paints the unread answers the gateway counted, in the status mark', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({
              id: 's1',
              turn_count: 6,
              answer_count: 3,
              is_unread: true,
              unread_answers: 2,
            }),
          ],
        },
      ],
    });
    restore = view.restore;

    const mark = await screen.findByText('NEW ×2');
    // ONE mark per row: a chip of its own beside the title was a second status line,
    // saying in one place what the row already says in the other.
    expect(mark.closest('[data-session-status]')).toBeVisible();
  });

  // Regression, user report: returning from a NEW session showed NEW for one frame
  // before the list poll finally replaced it with IDLE.
  it('keeps a visited session read when returning before the list refresh', async () => {
    const row = listSession({ id: 's1', answer_count: 2, is_unread: true, unread_answers: 1 });
    const view = renderSessionsScreen({ machines: [{ sessions: [row] }] });
    restore = view.restore;
    expect(await screen.findByText('NEW')).toBeVisible();

    view.holdList();
    act(() => {
      view.setOpenSession({ conn: view.conns[0], sid: 's1' });
      view.setVisible(false);
    });
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
    act(() => {
      view.setOpenSession(null);
      view.setVisible(true);
    });
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
    expect(screen.getByText('IDLE')).toBeInTheDocument();

    // Even a list read answered before the gateway processes the mark must not
    // resurrect the badge after the new row lands.
    view.setRows(0, [{ ...row, title: 'Read loaded' }]);
    act(() => view.releaseList());
    expect(await screen.findByText('Read loaded')).toBeVisible();
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();

    // A genuinely later answer is still NEW, but only that answer is counted.
    view.setRows(0, [{ ...row, title: 'Next answer', answer_count: 3, unread_answers: 2 }]);
    act(() => view.setVisible(false));
    act(() => view.setVisible(true));
    expect(await screen.findByText('Next answer')).toBeVisible();
    expect(screen.getByText('NEW')).toBeVisible();
    expect(screen.queryByText('NEW ×2')).not.toBeInTheDocument();
  });

  it('keeps answers read during an open transcript read on return', async () => {
    const row = listSession({ id: 's1', answer_count: 2, is_unread: true, unread_answers: 1 });
    const arrived = { ...row, answer_count: 3, unread_answers: 2 };
    const machine = {
      sessions: [row],
      routes: {
        '/v1/sessions/s1': arrived,
        '/v1/sessions/s1/read': { is_unread: false, seen_answers: 3 },
      },
    };
    const view = renderSessionsScreen({ machines: [machine] });
    restore = view.restore;
    expect(await screen.findByText('NEW')).toBeVisible();

    act(() => {
      view.setOpenSession({ conn: view.conns[0], sid: 's1' });
      view.setVisible(false);
    });
    const client = new GatewayClient({ url: view.conns[0].url, token: view.conns[0].token });
    await client.session('s1');
    await client.markSessionRead('s1', 3);

    view.holdList();
    act(() => {
      view.setOpenSession(null);
      view.setVisible(true);
    });
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
    // The fleet window was read before the mark: it still counts both answers.
    view.setRows(0, [{ ...arrived, title: 'Stale page' }]);
    act(() => view.releaseList());
    expect(await screen.findByText('Stale page')).toBeVisible();
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
  });

  it('does not clear a matching session ID on another machine', async () => {
    const row = listSession({ id: 'shared', answer_count: 1, is_unread: true, unread_answers: 1 });
    const view = renderSessionsScreen({
      machines: [
        { label: 'alpha', sessions: [row] },
        { label: 'beta', sessions: [{ ...row, title: 'On beta' }] },
      ],
    });
    restore = view.restore;
    expect(await screen.findByText('NEW')).toBeVisible();

    act(() => {
      view.setOpenSession({ conn: view.conns[0], sid: 'shared' });
      view.setVisible(false);
    });
    act(() => {
      view.setOpenSession(null);
      view.setVisible(true);
    });
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();

    await userEvent.click(screen.getByRole('button', { name: /^beta/ }));
    expect(await screen.findByText('On beta')).toBeVisible();
    expect(screen.getByText('NEW')).toBeVisible();
  });

  it('says nothing about a session the gateway calls read', async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: 's1', turn_count: 6, answer_count: 3 })] }],
    });
    restore = view.restore;

    expect(await screen.findByText('A session')).toBeInTheDocument();
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
  });
});
