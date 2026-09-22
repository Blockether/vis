// @vitest-environment jsdom
import { screen, waitFor, within } from '@testing-library/react';
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
});
