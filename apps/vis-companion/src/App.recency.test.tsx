// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
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

describe('opening a session updates shared recency', () => {
  it('paints immediately and puts the selected session first each time search is reopened', async () => {
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
    const openings: string[] = [];
    let completeOpening: (() => void) | undefined;
    globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
      const url = new URL(
        typeof input === 'string' ? input : input instanceof URL ? input.href : input.url,
      );
      if (url.pathname === '/v1/sessions/older' && init?.method === 'PATCH') {
        expect(JSON.parse(String(init.body))).toEqual({ opened: true });
        openings.push('older');
        return new Promise<Response>((resolve) => {
          completeOpening = () => {
            older.last_opened_at = Date.parse('2026-01-03T00:00:00Z');
            resolve(
              new Response(JSON.stringify(older), {
                headers: { 'Content-Type': 'application/json' },
              }),
            );
          };
        });
      }
      return baseFetch(input, init);
    }) as typeof fetch;

    await screen.findByText('Newer conversation');
    expect(openings).toEqual([]);
    fireEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    await waitFor(() => expect(recentOrder()).toEqual(['newer', 'older']));
    fireEvent.click(
      within(screen.getByRole('region', { name: 'Recent sessions' })).getByText('Older conversation'),
    );
    // Navigation must not wait for the recency write or session detail response.
    const back = await screen.findByRole('button', { name: 'Back to sessions' });
    await waitFor(() => expect(openings).toEqual(['older']));
    expect(older.last_opened_at).toBeUndefined();
    await act(async () => completeOpening!());
    fireEvent.click(back);
    await screen.findByRole('button', { name: 'Search sessions' });
    fireEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    await waitFor(() => expect(recentOrder()).toEqual(['older', 'newer']));
    fireEvent.click(screen.getByRole('button', { name: 'Close search' }));
    fireEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    await waitFor(() => expect(recentOrder()).toEqual(['older', 'newer']));
    expect(openings).toEqual(['older']);
  });
});
