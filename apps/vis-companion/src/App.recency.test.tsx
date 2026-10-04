// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

const recentOrder = () => {
  const rows = screen
    .getByRole('region', { name: 'Recent sessions' })
    .querySelectorAll('[data-session-id]');
  return [...rows].map((row) => row.getAttribute('data-session-id'));
};

// Regression: opening a session moved it to the top of the list. Only a sent message
// moves a session (`SessionsScreen.hidden.test.tsx` pins the send).
describe('opening a session keeps the recency order', () => {
  it('keeps an older session in its place after a person opens it and goes back', async () => {
    window.location.hash = '';
    const newer = listSession({
      id: 'newer',
      title: 'Newer conversation',
      modified_at: '2026-01-02T00:00:00Z',
    });
    const older = listSession({
      id: 'older',
      title: 'Older conversation',
      modified_at: '2026-01-01T00:00:00Z',
    });
    const view = renderApp({ machines: [{ sessions: [newer, older] }] });
    restore = () => {
      view.unmount();
      view.restore();
    };
    const baseFetch = globalThis.fetch;
    const rowWrites: string[] = [];
    globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
      const url = new URL(
        typeof input === 'string' ? input : input instanceof URL ? input.href : input.url,
      );
      if (url.pathname === '/v1/sessions/older' && init?.method === 'PATCH') {
        rowWrites.push(String(init.body));
      }
      return baseFetch(input, init);
    }) as typeof fetch;

    await screen.findByText('Newer conversation');
    fireEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    await waitFor(() => expect(recentOrder()).toEqual(['newer', 'older']));
    fireEvent.click(
      within(screen.getByRole('region', { name: 'Recent sessions' })).getByText('Older conversation'),
    );
    fireEvent.click(await screen.findByRole('button', { name: 'Back to sessions' }));
    await screen.findByRole('button', { name: 'Search sessions' });
    fireEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    await waitFor(() => expect(recentOrder()).toEqual(['newer', 'older']));
    expect(rowWrites).toEqual([]);
  });
});
