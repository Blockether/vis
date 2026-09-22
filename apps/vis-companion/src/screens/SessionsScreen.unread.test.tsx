// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
});

// The NEW badge is the GATEWAY's answer, painted straight from the row it serves: it
// counts the settled answers that have landed since this owner last read the session,
// so the badge agrees with the TUI, survives a reinstall, and retires as soon as a
// listing comes back with the session read. It stands in the row's ONE status mark,
// where the same row otherwise reads IDLE or DIRTY.
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
    expect(mark.closest('[data-session-status]')).not.toBeNull();
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
