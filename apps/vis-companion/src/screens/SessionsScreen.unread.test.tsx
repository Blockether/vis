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
// listing comes back with the session read.
describe('the NEW badge', () => {
  it('paints the unread answers the gateway counted', async () => {
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

    expect(await screen.findByText('2 new')).toBeInTheDocument();
  });

  it('says nothing about a session the gateway calls read', async () => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: 's1', turn_count: 6, answer_count: 3 })] }],
    });
    restore = view.restore;

    expect(await screen.findByText('A session')).toBeInTheDocument();
    expect(screen.queryByText('new')).not.toBeInTheDocument();
  });
});
