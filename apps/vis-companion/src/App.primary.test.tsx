// @vitest-environment jsdom
import { act, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';
import { loadConnections, loadConnectionsSync } from './lib/storage';
let restore = () => {};
afterEach(() => {
  restore();
  vi.useRealTimers();
});

// Regression, user report: making the second paired machine primary left the first
// paired machine first and selected in the sessions list.
describe('the primary machine owns the first sessions scope', () => {
  it('puts the primary machine first and selects its sessions', async () => {
    const view = renderApp({
      machines: [
        { label: 'alpha', sessions: [listSession({ id: 'a1', title: 'Alpha session' })] },
        { label: 'beta', sessions: [listSession({ id: 'b1', title: 'Beta session' })] },
      ],
      primary: 1,
    });
    restore = view.restore;

    const strip = within(await screen.findByLabelText('Machines'));
    await waitFor(() =>
      expect(
        strip
          .getAllByRole('button')
          .map((button) => button.getAttribute('aria-label') ?? button.firstChild?.textContent),
      ).toEqual(['beta', 'alpha']),
    );

    expect(strip.getByRole('button', { name: /^beta/ }).getAttribute('aria-pressed')).toBe('true');
    expect(await screen.findByText('Beta session')).toBeVisible();
    expect(screen.queryByText('Alpha session')).toBeNull();
  });
  it('renders and caches the primary server order rather than pairing order', async () => {
    const view = renderApp({
      machines: [{ label: 'alpha' }, { label: 'beta' }, { label: 'gamma' }],
      machineOrder: [0, 2, 1],
    });
    restore = view.restore;
    const strip = within(await screen.findByLabelText('Machines'));
    await waitFor(() =>
      expect(strip.getAllByRole('button').map((button) => button.firstChild?.textContent)).toEqual([
        'alpha',
        'gamma',
        'beta',
      ]),
    );
    expect(loadConnectionsSync().map((conn) => conn.label)).toEqual(['alpha', 'gamma', 'beta']);
    expect((await loadConnections()).map((conn) => conn.label)).toEqual(['alpha', 'gamma', 'beta']);
    expect(
      view.requests
        .filter((url) => url.endsWith('/v1/machines/order'))
        .every((url) => url.startsWith(view.conns[0]!.url)),
    ).toBe(true);
  });
  // Audit of Companion polls: the order sync ran every 30 s and each run takes the
  // gateway's `state.yml` lock, though the order changes only on a pairing or a reorder.
  it('syncs the order on start, not on a timer', async () => {
    vi.useFakeTimers({ shouldAdvanceTime: true });
    const view = renderApp({ machines: [{ label: 'alpha' }, { label: 'beta' }] });
    restore = view.restore;
    const orderSyncs = () => view.requests.filter((url) => url.endsWith('/v1/machines/order')).length;
    await screen.findByLabelText('Machines');
    await waitFor(() => expect(orderSyncs()).toBeGreaterThan(0));
    const started = orderSyncs();
    await act(() => vi.advanceTimersByTimeAsync(120_000));
    expect(orderSyncs()).toBe(started);
  });
});
