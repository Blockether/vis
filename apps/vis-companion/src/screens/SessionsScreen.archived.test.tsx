// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// The screen paints the rows the gateway sent it, so the archive has to read on the row
// itself — not only in the row component's own stories.
describe('the sessions list', () => {
  it('reads ARCHIVED on a row the gateway stamped, and leaves the others alone', async () => {
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
    await screen.findByText('Filed session');

    const status = (id: string) =>
      document.querySelector(`[data-session-id="${id}"] [data-session-status]`)?.textContent;
    expect(status('filed')).toContain('ARCHIVED');
    expect(status('open')).not.toContain('ARCHIVED');
  });
});
