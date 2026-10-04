// @vitest-environment jsdom
import { act, cleanup, fireEvent, screen, within } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';
import { machineOutage } from '../lib/fleet-outage';

let view: ReturnType<typeof renderSessionsScreen>;

beforeEach(() => {
  localStorage.clear();
  vi.useFakeTimers();
});

afterEach(() => {
  cleanup();
  view?.restore();
  vi.useRealTimers();
});

const alpha = { label: 'alpha', sessions: [listSession({ title: 'Active session' })] };
const beta = { label: 'beta', down: true };
const strip = () => within(screen.getByRole('group', { name: 'Machines' }));
const betaTab = () => strip().queryByRole('button', { name: /beta/ });

async function advance(ms = 50) {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
}

// Regression: repeated connection failures must hide a server across app restarts.
// A new connection attempt must not show it before a successful response.
describe('persistent hiding of unavailable gateways', () => {
  it('keeps two failures visible and hides the third without hiding another server', async () => {
    view = renderSessionsScreen({ machines: [alpha, beta] });
    await advance();
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeNull();
    expect(strip().getByRole('button', { name: 'alpha' })).toBeVisible();
    expect(screen.getByText('Active session')).toBeVisible();
  });

  it('counts timed-out background attempts as failures', async () => {
    view = renderSessionsScreen({ machines: [alpha, { ...beta, hangs: true }] });
    await advance();
    await advance(20_000);
    expect(betaTab()).toBeVisible();
    await advance(20_000);
    expect(betaTab()).toBeNull();
    expect(screen.getByText('Active session')).toBeVisible();
  });

  it('counts manual retry deadlines as failures', async () => {
    view = renderSessionsScreen({ machines: [alpha, { ...beta, hangs: true }] });
    await advance();
    fireEvent.click(strip().getByRole('button', { name: 'Reconnect to beta' }));
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    fireEvent.click(strip().getByRole('button', { name: 'Reconnect to beta' }));
    await advance(5_000);
    expect(betaTab()).toBeNull();
  });

  it('keeps the counter across remounts before the third failure', async () => {
    view = renderSessionsScreen({ machines: [alpha, beta] });
    await advance();
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    const conns = view.conns;
    view.unmount();
    view.restore();

    view = renderSessionsScreen({ machines: [alpha, beta], at: conns });
    await advance();
    expect(betaTab()).toBeNull();
  });

  it('stays hidden through relaunch and returns only after an answer', async () => {
    view = renderSessionsScreen({ machines: [alpha, beta] });
    await advance();
    await advance(5_000);
    await advance(5_000);
    const conns = view.conns;
    view.unmount();
    view.restore();

    view = renderSessionsScreen({
      machines: [alpha, { label: 'beta', holdsList: true }],
      at: conns,
    });
    expect(betaTab()).toBeNull();
    await advance();
    expect(betaTab()).toBeNull();
    view.releasePages();
    await advance();
    expect(strip().getByRole('button', { name: 'beta' })).toBeVisible();
    expect(machineOutage(conns[1].url)).toBeNull();

    view.unmount();
    view.restore();
    view = renderSessionsScreen({ machines: [alpha, beta], at: conns });
    await advance();
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeNull();
  });

  it('restores a hidden server on a background poll and resets its counter', async () => {
    view = renderSessionsScreen({
      machines: [alpha, { label: 'beta', drops: [1, 2, 3, 5, 6, 7] }],
    });
    await advance();
    await advance(5_000);
    await advance(5_000);
    expect(betaTab()).toBeNull();
    await advance(5_000);
    expect(strip().getByRole('button', { name: 'beta' })).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeVisible();
    await advance(5_000);
    expect(betaTab()).toBeNull();
  });
});
