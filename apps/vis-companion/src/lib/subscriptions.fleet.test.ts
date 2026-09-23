// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';

import { SessionSubscriptionHub } from './subscriptions';
import type { GatewayClient } from './gateway';
import type { SseEvent } from './types';

/** A client that is nothing but the two streams the hub can open. */
function fakeClient() {
  const state = {
    opened: 0,
    stopped: 0,
    deliver: null as ((event: SseEvent) => void) | null,
  };
  const client = {
    streamFleetStatus(onEvent: (event: SseEvent) => void, opts: { onOpen?: () => void } = {}) {
      state.opened += 1;
      state.deliver = onEvent;
      opts.onOpen?.();
      return () => {
        state.stopped += 1;
        state.deliver = null;
      };
    },
    streamSessionEvents() {
      return () => {};
    },
  };
  return { state, client: client as unknown as GatewayClient };
}

// Regression, user report in this Vis session (paraphrased: "the companion is slow and
// keeps refreshing"): the list had no push channel of its own, so it re-read its whole
// window on a timer to notice that a run had started or ended. The fleet stream is that
// channel — and a machine must not stream to a list nobody is looking at.
describe("the hub's fleet stream", () => {
  it('runs only while somebody is listening', () => {
    const { state, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    expect(state.opened).toBe(0);

    const stopFirst = hub.subscribeFleet(() => {});
    expect(state.opened).toBe(1);

    // A second listener rides the connection that is already open.
    const stopSecond = hub.subscribeFleet(() => {});
    expect(state.opened).toBe(1);

    stopFirst();
    expect(state.stopped).toBe(0);
    stopSecond();
    expect(state.stopped).toBe(1);

    // ...and the next listener opens it again.
    hub.subscribeFleet(() => {});
    expect(state.opened).toBe(2);
    hub.dispose();
    expect(state.stopped).toBe(2);
  });

  it('hands every frame to its listeners and says whether it is delivering', () => {
    const { state, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    const streaming: boolean[] = [];
    hub.subscribeFleetState((live) => streaming.push(live));
    // Nothing is listening for frames yet, so there is no stream to report.
    expect(streaming).toEqual([false]);

    const seen: SseEvent[] = [];
    const stop = hub.subscribeFleet((event) => seen.push(event));
    expect(streaming).toEqual([false, true]);

    state.deliver?.({ type: 'session.status', session_id: 's1', is_live: true });
    expect(seen).toEqual([{ type: 'session.status', session_id: 's1', is_live: true }]);

    stop();
    expect(streaming).toEqual([false, true, false]);
    hub.dispose();
  });
  it('uses one stream for session cursors and fleet updates while both are watched', () => {
    // Regression: three visible HTTP/1.1 tabs used six event sockets and blocked normal GETs.
    const starts: Array<{
      opts: { includeFleet?: boolean; onOpen?: () => void };
      emit: (event: SseEvent) => void;
      stop: ReturnType<typeof vi.fn>;
    }> = [];
    const standaloneFleet = vi.fn(() => vi.fn());
    const client = {
      streamSessionEvents(
        _cursors: Map<string, number>,
        emit: (event: SseEvent) => void,
        opts: { includeFleet?: boolean; onOpen?: () => void },
      ) {
        const stop = vi.fn();
        starts.push({ opts, emit, stop });
        opts.onOpen?.();
        return stop;
      },
      streamFleetStatus: standaloneFleet,
    } as unknown as GatewayClient;
    const hub = new SessionSubscriptionHub(client);
    const fleetEvents: SseEvent[] = [];
    const streaming: boolean[] = [];
    hub.subscribeFleetState((live) => streaming.push(live));
    hub.watchSessions(['s1']);
    expect(starts).toHaveLength(1);
    const stopFleet = hub.subscribeFleet((event) => fleetEvents.push(event));
    expect(starts).toHaveLength(2);
    expect(starts[0]!.stop).toHaveBeenCalledOnce();
    expect(starts[1]!.opts.includeFleet).toBe(true);
    expect(standaloneFleet).not.toHaveBeenCalled();
    // An older gateway sends only session frames for scope=both. Do not claim
    // fleet coverage until its ready frame arrives; the list still polls.
    starts[1]!.emit({ type: 'subscription.ready', session_id: 's1', cursor: 2 });
    expect(streaming).toEqual([false]);

    const ready: SseEvent = { type: 'subscription.ready', scope: 'fleet', seq: 0 };
    const status: SseEvent = { type: 'session.status', scope: 'fleet', session_id: 's2', seq: 10 };
    starts[1]!.emit(ready);
    starts[1]!.emit(status);
    expect(streaming).toEqual([false, true]);
    expect(fleetEvents).toEqual([ready, status]);

    stopFleet();
    expect(streaming).toEqual([false, true, false]);
    expect(starts).toHaveLength(3);
    expect(starts[2]!.opts.includeFleet).toBe(false);
    expect(standaloneFleet).not.toHaveBeenCalled();
    hub.dispose();
  });
  it('replaces fleet-only transport when a session is watched and recovers both feeds', () => {
    const { state, client } = fakeClient();
    const starts: Array<{
      opts: NonNullable<Parameters<GatewayClient['streamSessionEvents']>[2]>;
      emit: (event: SseEvent) => void;
      stop: ReturnType<typeof vi.fn>;
    }> = [];
    vi.spyOn(client, 'streamSessionEvents').mockImplementation((_cursors, emit, opts) => {
      const stop = vi.fn();
      const options = opts ?? {};
      starts.push({ opts: options, emit, stop });
      options.onOpen?.();
      return stop;
    });
    const hub = new SessionSubscriptionHub(client);
    const streaming: boolean[] = [];
    hub.subscribeFleetState((live) => streaming.push(live));
    const stopFleet = hub.subscribeFleet(() => {});
    expect(state.opened).toBe(1);
    expect(streaming).toEqual([false, true]);

    hub.watchSessions(['s1']);
    expect(state.stopped).toBe(1);
    expect(state.opened).toBe(1);
    expect(starts).toHaveLength(1);
    expect(starts[0]!.opts.includeFleet).toBe(true);
    expect(streaming).toEqual([false, true, false]);
    starts[0]!.emit({ type: 'subscription.ready', scope: 'fleet', seq: 0 });
    expect(streaming.at(-1)).toBe(true);

    starts[0]!.opts.onError?.(new Error('stream dropped'));
    expect(streaming.at(-1)).toBe(false);
    hub.resync();
    expect(starts[0]!.stop).toHaveBeenCalledOnce();
    expect(starts).toHaveLength(2);
    expect(state.opened).toBe(1);
    starts[1]!.emit({ type: 'subscription.ready', scope: 'fleet', seq: 0 });
    expect(streaming.at(-1)).toBe(true);

    stopFleet();
    hub.dispose();
  });
});
