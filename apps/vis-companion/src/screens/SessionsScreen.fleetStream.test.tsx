// @vitest-environment jsdom
import { act, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { SseEvent } from '../lib/types';
import { STORY_GOAL } from '../dev/story-data';
import { GatewayClient } from '../lib/gateway';
import { listSession, renderSessionsScreen, sessionsWindow } from './sessions-screen-harness';

/** Let every poll, repaint and effect that fits inside `ms` happen. */
const settle = async (ms = 0) => {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
};

// The read this list owns is the fleet window — the one WITHOUT a `root=`, since a
// project's page is a read of its own (`GatewayClient.listProjectPage`).
const listReads = (requests: { path: string }[]) =>
  requests.filter(
    (request) => request.path.startsWith('/v1/sessions?') && !request.path.includes('root='),
  ).length;

/**
 * A hub whose fleet stream is delivering: the frames the gateway sends on
 * `GET /v1/events?scope=fleet`, and the streaming state the list paces itself by.
 */
function fleetHub() {
  let deliver: ((event: SseEvent) => void) | null = null;
  let report: ((streaming: boolean) => void) | null = null;
  return {
    hub: {
      gatewayUrl: '',
      subscribeFleet(listener: (event: SseEvent) => void) {
        deliver = listener;
        return () => {
          deliver = null;
        };
      },
      subscribeFleetState(listener: (streaming: boolean) => void) {
        report = listener;
        listener(true);
        return () => {
          report = null;
        };
      },
    },
    /** Inject from a fetch callback already running inside the test's act scope. */
    dispatch: (event: SseEvent) => deliver?.(event),
    emit: async (event: SseEvent) => {
      await act(async () => {
        deliver?.(event);
      });
    },
    /** The stream dropped (or came back): the list's safety net changes cadence. */
    streaming: async (live: boolean) => {
      await act(async () => {
        report?.(live);
      });
    },
  };
}

let restore = () => {};

// Regression, user report in this Vis session (paraphrased: "the companion is slow,
// it keeps refreshing and struggling to see over the network"): the ONLY way this list
// learned that a run had started, parked on a human or ended was to re-read its whole
// window every five seconds — a payload whose ETag any one active session invalidates,
// so a phone paid the window over and over to discover a single boolean.
describe('a session list carried by the fleet stream', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    vi.useRealTimers();
  });

  const oneRow = (fleet: ReturnType<typeof fleetHub>) => {
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: 's1', title: 'First' })] }],
      subscriptions: fleet.hub as never,
    });
    fleet.hub.gatewayUrl = view.conns[0]!.url;
    return view;
  };

  it('removes a remotely deleted row immediately and keeps its neighbour', async () => {
    const fleet = fleetHub();
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's1', title: 'First' }),
            listSession({ id: 's2', title: 'Second' }),
          ],
        },
        { sessions: [listSession({ id: 's1', title: 'Other machine' })] },
      ],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    fleet.hub.gatewayUrl = view.conns[0]!.url;
    await settle(200);
    const read = listReads(view.requests);
    await fleet.emit({ type: 'session.deleted', session_id: 's1' });
    await fleet.emit({ type: 'session.deleted', session_id: 's1' });
    expect(screen.queryByText('First')).toBeNull();
    expect(screen.getByText('Second')).toBeVisible();
    expect(listReads(view.requests)).toBe(read);
    expect(new GatewayClient(view.conns[1]!).isSessionDeleted('s1')).toBe(false);
  });

  it('does not let an older in-flight window resurrect a deleted row', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(200);
    view.holdList();
    await settle(30_000);
    await fleet.emit({ type: 'session.deleted', session_id: 's1' });
    view.releaseList();
    await settle();
    expect(screen.queryByText('First')).toBeNull();
    expect(new GatewayClient(view.conns[0]!).cachedSessions()).toEqual([]);
  });

  it('repairs a deletion missed while the fleet stream was disconnected', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(200);
    await fleet.streaming(false);
    view.setRows(0, []);
    await fleet.emit({ type: 'subscription.ready', scope: 'fleet' });
    await settle(200);
    expect(screen.queryByText('First')).toBeNull();
  });

  it('paints a run the stream announced without reading the window again', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    expect(screen.getByText('First')).toBeVisible();
    expect(screen.queryByText('LIVE')).toBeNull();
    const read = listReads(view.requests);

    await fleet.emit({
      type: 'session.status',
      session_id: 's1',
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: 't1',
    });

    expect(screen.getByText('LIVE')).toBeVisible();
    expect(listReads(view.requests)).toBe(read);
  });

  // Regression, Vis session 448b3266-8836-4115-9cf5-6ed0679aa2f9: a settled fleet
  // frame painted NEW from metadata alone, before the finished transcript was warm.
  it('reads the settled row before replacing LIVE with its finished state', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);

    await fleet.emit({
      type: 'session.status',
      session_id: 's1',
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: 't1',
    });
    const read = listReads(view.requests);

    await fleet.emit({
      type: 'session.status',
      session_id: 's1',
      is_live: false,
      is_awaiting_input: false,
      current_turn_id: null,
    });
    await settle(200);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });

  it("takes a row's new title from the frame alone", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await fleet.emit({
      type: 'session.title_updated',
      session_id: 's1',
      title: 'Renamed by the engine',
    });

    expect(screen.getByText('Renamed by the engine')).toBeVisible();
    expect(listReads(view.requests)).toBe(read);
  });

  // Regression, Vis session 27689168-9320-46fa-b015-8dce5a2c80c8: a title event
  // copied into every watched session's replay ring temporarily gave those sibling
  // rows the renamed session's title because `session_id` names the ring, not the subject.
  it("takes a copied title frame's subject from titled_session_id", async () => {
    const fleet = fleetHub();
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's1', title: 'First' }),
            listSession({ id: 's2', title: 'Second' }),
          ],
        },
      ],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    await settle(50);

    await fleet.emit({
      type: 'session.title_updated',
      session_id: 's2',
      titled_session_id: 's1',
      title: 'First renamed',
    });

    expect(screen.getByText('First renamed')).toBeVisible();
    expect(screen.getByText('Second')).toBeVisible();
  });

  // A frame about a session this window does not hold is news about MEMBERSHIP, and
  // where that row belongs is the gateway's arithmetic, never this device's.
  it('re-reads the window for a session it does not hold', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await fleet.emit({
      type: 'session.status',
      session_id: 'somewhere-else',
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: 't9',
    });
    await settle(200);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });

  // A long-running goal must not stay IDLE because its start crossed a list read
  // or happened while the fleet connection was down.
  it.each(['connection', 'ready'])('refreshes current status on fleet %s', async (signal) => {
    const fleet = fleetHub();
    const row = listSession({ id: 's1', title: 'First', live: false, goal: STORY_GOAL });
    const view = renderSessionsScreen({
      machines: [{ sessions: [row] }],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    await settle(200);
    expect(screen.queryByText('LIVE')).toBeNull();
    if (signal === 'connection') await fleet.streaming(false);
    view.setRows(0, [{ ...row, live: true, current_turn_id: 'goal-turn' }]);
    const read = listReads(view.requests);
    if (signal === 'connection') await fleet.streaming(true);
    else await fleet.emit({ type: 'subscription.ready', scope: 'fleet' });
    await settle(200);
    expect(listReads(view.requests)).toBeGreaterThan(read);
    expect(screen.getByText('LIVE')).toBeVisible();
  });

  it('does not overwrite a reconnect snapshot with an older cold response', async () => {
    const fleet = fleetHub();
    const row = listSession({ id: 's1', title: 'First', live: false, goal: STORY_GOAL });
    const routes: Record<string, unknown> = {};
    const view = renderSessionsScreen({
      machines: [{ sessions: [row], holdsList: true, routes }],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    await settle();
    routes['/v1/sessions'] = sessionsWindow(
      [{ ...row, live: true }],
      new URL('http://gateway.example.com/v1/sessions'),
    );
    await fleet.emit({ type: 'subscription.ready', scope: 'fleet' });
    await settle(200);
    expect(screen.getByText('LIVE')).toBeVisible();
    view.releasePages();
    await settle();
    expect(screen.getByText('LIVE')).toBeVisible();
  });

  it('does not count superseded read failures against a newer successful connection', async () => {
    const fleet = fleetHub();
    const rejectReads: (() => void)[] = [];
    const holdFailure = () =>
      new Promise<ReturnType<typeof listSession>[]>((_resolve, reject) => {
        rejectReads.push(() => reject(new Error('obsolete read')));
      });
    const read = vi.spyOn(GatewayClient.prototype, 'listSessions')
      .mockImplementationOnce(holdFailure)
      .mockImplementationOnce(holdFailure);
    const view = renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: 's1', title: 'First', live: true })] }],
      subscriptions: fleet.hub as never,
    });
    restore = () => {
      read.mockRestore();
      view.restore();
    };
    await settle(200);
    expect(rejectReads).toHaveLength(2);
    view.setVisible(false);
    view.setVisible(true);
    await settle(200);
    expect(screen.getByText('LIVE')).toBeVisible();
    await act(async () => {
      for (const reject of rejectReads) reject();
    });
    expect(screen.queryByText('obsolete read')).toBeNull();
    expect(screen.getByText('LIVE')).toBeVisible();
    // One genuine failure is still only a blip, not the second failure of an outage.
    read.mockRejectedValueOnce(new Error('current blip'));
    await fleet.streaming(false);
    await settle(5_000);
    expect(screen.getByText('LIVE')).toBeVisible();
  });

  it.each([false, true])('does not let a stale list erase a goal start (cold=%s)', async (cold) => {
    const fleet = fleetHub();
    const row = listSession({ id: 's1', title: 'First', live: false, goal: STORY_GOAL });
    const routes: Record<string, unknown> = {};
    const view = renderSessionsScreen({
      machines: [{ sessions: [row], holdsList: cold, routes }],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    await settle(cold ? 0 : 200);
    if (!cold) {
      expect(screen.queryByText('LIVE')).toBeNull();
      view.holdList();
      await settle(30_000);
    }
    await fleet.emit({
      type: 'session.status',
      session_id: 's1',
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: 'goal-turn',
    });
    // The held response already passed the routes seam and still carries IDLE.
    // A read begun after the start receives the gateway's current LIVE row.
    const url = new URL('http://gateway.example.com/v1/sessions');
    routes['/v1/sessions'] = sessionsWindow(
      [{ ...row, live: true, current_turn_id: 'goal-turn' }],
      url,
    );
    const read = listReads(view.requests);
    if (cold) view.releasePages();
    else view.releaseList();
    await settle();
    expect(screen.getByText('LIVE')).toBeVisible();
    expect(listReads(view.requests)).toBe(read);
    await settle(200);

    routes['/v1/sessions'] = sessionsWindow([row], url);
    await fleet.emit({
      type: 'session.status',
      session_id: 's1',
      is_live: false,
      is_awaiting_input: false,
      current_turn_id: null,
    });
    await settle(200);
    expect(screen.queryByText('LIVE')).toBeNull();
  });

  it('accepts snapshots during continual fleet activity without retrying them', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(200);
    const fetch = globalThis.fetch;
    let reads = 0;
    let announce = true;
    globalThis.fetch = async (input, init) => {
      const response = await fetch(input, init);
      const url = new URL(String(input));
      if (url.pathname === '/v1/sessions' && !url.searchParams.has('root')) {
        reads += 1;
        // Every returning snapshot crosses a newer status. Cap the fixture at three
        // announcements so the former retry loop fails a count rather than hanging.
        if (announce && reads <= 3)
          fleet.dispatch({
            type: 'session.status',
            session_id: 's1',
            is_live: true,
            current_turn_id: `turn-${reads}`,
          });
      }
      return response;
    };
    await settle(30_000);
    expect(screen.getByText('LIVE')).toBeVisible();
    expect(reads).toBe(1);

    announce = false;
    await fleet.emit({ type: 'session.status', session_id: 's1', is_live: false });
    await settle(200);
    expect(screen.queryByText('LIVE')).toBeNull();
    expect(reads).toBe(2);
  });

  it('keeps a terminal resync queued behind a slow poll', async () => {
    const fleet = fleetHub();
    const row = listSession({ id: 's1', title: 'First', live: true });
    const routes: Record<string, unknown> = {};
    const view = renderSessionsScreen({
      machines: [{ sessions: [row], routes }],
      subscriptions: fleet.hub as never,
    });
    restore = view.restore;
    await settle(200);
    view.holdList();
    await settle(30_000);
    const read = listReads(view.requests);
    await fleet.emit({ type: 'session.status', session_id: 's1', is_live: false });
    routes['/v1/sessions'] = sessionsWindow(
      [{ ...row, live: false }],
      new URL('http://gateway.example.com/v1/sessions'),
    );
    await settle(200);
    expect(screen.getByText('LIVE')).toBeVisible();
    view.releaseList();
    await settle();
    expect(screen.queryByText('LIVE')).toBeNull();
    expect(listReads(view.requests)).toBe(read + 1);
  });

  it('slows its safety net while the stream delivers, and speeds back up when it drops', async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(200);
    const read = listReads(view.requests);

    // The five-second reachability poll is what the stream replaces.
    await settle(5_000);
    expect(listReads(view.requests)).toBe(read);

    // The net is still there, just slack.
    await settle(25_000);
    expect(listReads(view.requests)).toBe(read + 1);

    // No stream, no slack: a dead gateway must not keep looking alive for half a minute.
    await fleet.streaming(false);
    await settle(5_000);
    expect(listReads(view.requests)).toBe(read + 2);
  });
});
