// @vitest-environment jsdom
import { act, screen, waitFor, within } from '@testing-library/react';
import { afterEach, expect, describe, it, vi } from 'vitest';

import { GatewayClient } from '../lib/gateway';
import { rememberMachineOutage } from '../lib/fleet-outage';
import { listSession, renderSessionsScreen } from './sessions-screen-harness';
import type { FleetRequest } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

/** Let every poll, probe and repaint that fits inside `ms` happen. */
const settle = async (ms = 0) => {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
};

/** The fleet window this list owns: the `/v1/sessions` read WITHOUT a `root=`. */
const windowReads = (requests: FleetRequest[]) =>
  requests.filter(
    (request) => request.path.startsWith('/v1/sessions?') && !request.path.includes('root='),
  ).length;

/** One project's own page of its own history — the read a prefetch multiplies. */
const projectReads = (requests: FleetRequest[]) =>
  requests.filter(
    (request) => request.path.startsWith('/v1/sessions?') && request.path.includes('root='),
  ).length;

/** The wall of bands inside a project, which no fleet frame announces. */
const groupReads = (requests: FleetRequest[]) =>
  requests.filter((request) => request.path.startsWith('/v1/session-groups')).length;

/** The machine switch over the list: the only place a machine's own state is spoken. */
const strip = () => within(screen.getByLabelText('Machines'));

const row = (id: string, title: string) =>
  listSession({ id, title, workspace: { root: '/w/one' } });

// Regression, user report (paraphrased: the phone shows a machine as broken the moment the
// app opens, then shows a finished-looking list of old rows, and keeps refreshing). Three
// separate claims the screen made before the gateway had said anything in this run.
describe('a machine that has not spoken yet says so', () => {
  it('reads a remembered outage as connecting, not as a failure', async () => {
    const conns = [{ url: 'http://connecting-tower.example.com', token: 't', label: 'tower' }];
    // What the run before this one wrote down; it is kept for up to thirty days.
    rememberMachineOutage(conns[0].url, 'Failed to fetch');

    const view = renderSessionsScreen({
      machines: [{ label: 'tower', holdsList: true }],
      at: conns,
    });
    restore = view.restore;

    const tile = strip().getByRole('button', { name: /tower/ });
    // A memory is not this run's answer: a read of this machine is in flight behind
    // this very frame, and the tile says that instead of offering a retry.
    expect(tile.textContent).toBe('towerConnecting…');
    expect(tile.querySelector('.text-err')).toBeNull();
    expect(strip().queryByRole('button', { name: /^Reconnect to tower/ })).toBeNull();
  });

  it('does not let a cached list read as settled before the machine answers', async () => {
    const beta = { label: 'beta', sessions: [row('b1', 'Second')] };
    const first = renderSessionsScreen({ machines: [beta] });
    restore = first.restore;
    expect(await screen.findByText('Second')).toBeVisible();
    first.unmount();

    // The relaunch: the saved window is seeded in the first frame, and this gateway is
    // slow to speak.
    const again = renderSessionsScreen({
      machines: [{ ...beta, holdsList: true }],
      at: first.conns,
    });
    restore = () => {
      again.restore();
      first.restore();
    };
    // The saved rows still paint in this very frame, which is why they are saved.
    expect(new GatewayClient(first.conns[0]).cachedSessions()).toHaveLength(1);
    expect(screen.getByText('Second')).toBeVisible();
    // They are paint, not proof — and the screen says so in both places a reader looks:
    // the tile over the list, and the list's own footer.
    expect(strip().getByRole('button', { name: /beta/ }).textContent).toBe('betaConnecting…');
    expect(screen.getByText('Reading sessions...')).toBeVisible();

    again.releasePages();
    // The gateway speaks: the same rows, now confirmed, and nothing claiming a wait.
    await waitFor(() => expect(screen.queryByText('Reading sessions...')).toBeNull());
    expect(screen.getByText('Second')).toBeVisible();
    expect(strip().getByRole('button', { name: /beta/ }).textContent).toBe('beta');
  });

  // Same report, the case the tile and the footer used to disagree about: a saved outage
  // puts `error` on the machine before this run has read it, and the footer read that as
  // an answer while the tile beside it said `Connecting…` over the same saved rows.
  it('says it is still reading while a remembered outage is being retried', async () => {
    const beta = { label: 'beta', sessions: [row('b1', 'Second')] };
    const first = renderSessionsScreen({ machines: [beta] });
    restore = first.restore;
    expect(await screen.findByText('Second')).toBeVisible();
    first.unmount();
    // What the run before this one wrote down, over rows this device also saved.
    rememberMachineOutage(first.conns[0].url, 'Failed to fetch');

    const again = renderSessionsScreen({
      machines: [{ ...beta, holdsList: true }],
      at: first.conns,
    });
    restore = () => {
      again.restore();
      first.restore();
    };
    expect(screen.getByText('Second')).toBeVisible();
    expect(strip().getByRole('button', { name: /beta/ }).textContent).toBe('betaConnecting…');
    expect(screen.getByText('Reading sessions...')).toBeVisible();

    again.releasePages();
    await waitFor(() => expect(screen.queryByText('Reading sessions...')).toBeNull());
    expect(screen.getByText('Second')).toBeVisible();
  });

  // Regression, same report: cb30d0f39 revalidated every project window on every
  // ACCEPTED snapshot, including the idle polls whose answer is the rows already on
  // screen — so a phone paid a full project prefetch every five seconds to learn
  // nothing. The wall of bands still revalidates, because nothing else announces it.
  it('does not re-read a project window for a poll that is not news', async () => {
    vi.useFakeTimers();
    const view = renderSessionsScreen({
      machines: [{ label: 'alpha', sessions: [row('a1', 'First')] }],
    });
    restore = () => {
      vi.useRealTimers();
      view.restore();
    };
    await settle(200);
    expect(screen.getByText('First')).toBeVisible();
    const windows = windowReads(view.requests);
    const pages = projectReads(view.requests);
    const walls = groupReads(view.requests);
    expect(pages).toBeGreaterThan(0);

    // Thirty seconds of the safety-net poll, every tick answering with the list on screen.
    await settle(30_000);
    expect(windowReads(view.requests)).toBeGreaterThan(windows);
    expect(projectReads(view.requests)).toBe(pages);
    expect(groupReads(view.requests)).toBeGreaterThan(walls);
  });
});
