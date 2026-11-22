// @vitest-environment jsdom
import { ReadableStream } from 'node:stream/web';
import { TextEncoder } from 'node:util';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from './gateway';
import type { SseEvent } from './types';

const native = vi.hoisted(() => new Map<string, (state?: { isActive: boolean }) => void>());

vi.mock('@capacitor/app', () => ({
  App: {
    addListener: (event: string, listener: (state?: { isActive: boolean }) => void) => {
      native.set(event, listener);
      return Promise.resolve({ remove: () => void native.delete(event) });
    },
  },
}));

function fakeClient() {
  const state = {
    sessionOpened: 0,
    sessionStopped: 0,
    fleetOpened: 0,
    fleetStopped: 0,
  };
  const stoppable = (kind: 'session' | 'fleet') => {
    let stopped = false;
    return () => {
      if (stopped) return;
      stopped = true;
      state[`${kind}Stopped`] += 1;
    };
  };
  const client = {
    streamSessionEvents(
      _cursors: Map<string, number>,
      _onEvent: (event: SseEvent) => void,
      opts: { onOpen?: () => void } = {},
    ) {
      state.sessionOpened += 1;
      opts.onOpen?.();
      return stoppable('session');
    },
    streamFleetStatus(_onEvent: (event: SseEvent) => void, opts: { onOpen?: () => void } = {}) {
      state.fleetOpened += 1;
      opts.onOpen?.();
      return stoppable('fleet');
    },
  };
  return { state, client: client as unknown as GatewayClient };
}

beforeEach(() => {
  vi.useFakeTimers();
  native.clear();
});

afterEach(() => {
  vi.useRealTimers();
  vi.resetModules();
});

describe('multiplexed session subscriptions', () => {
  it('shares the stream across views and keeps replay when a view remounts', async () => {
    const { SessionSubscriptionHub } = await import('./subscriptions');
    const { state, client } = fakeClient();
    const stream = vi.spyOn(client, 'streamSessionEvents');
    const hub = new SessionSubscriptionHub(client);
    try {
      hub.watchSessions(['session-1', 'session-2']);
      const first = vi.fn();
      const second = vi.fn();
      const stopFirst = hub.subscribeSession('session-1', first);
      const stopSecond = hub.subscribeSession('session-2', second);
      expect(stream).toHaveBeenCalledTimes(1);
      const [cursors, emit] = stream.mock.calls[0]!;
      expect([...cursors].sort(([a], [b]) => a.localeCompare(b))).toEqual([
        ['session-1', -1],
        ['session-2', -1],
      ]);
      const event: SseEvent = {
        type: 'turn.started',
        session_id: 'session-1',
        seq: 10,
      };
      emit(event);
      expect(first).toHaveBeenCalledExactlyOnceWith(event);
      expect(second).not.toHaveBeenCalled();
      stopFirst();
      const remounted = vi.fn();
      const stopRemounted = hub.subscribeSession('session-1', remounted);
      expect(remounted).toHaveBeenCalledExactlyOnceWith(event);
      stopRemounted();
      stopSecond();
      expect(state.sessionStopped).toBe(0);
      expect(stream).toHaveBeenCalledTimes(1);
    } finally {
      hub.dispose();
    }
    expect(state.sessionStopped).toBe(1);
    await vi.advanceTimersByTimeAsync(30_000);
    expect(stream).toHaveBeenCalledTimes(1);
  });
});

// Regression, Vis session 1bd4284d-861b-48e6-8639-ef8eafb22f0a: killing the gateway
// while the app was backgrounded left WebKit holding its fetch stream; after resume,
// opening or creating another session waited behind that parked socket until restart.
describe('gateway streams across native backgrounding', () => {
  it('retires them before suspension and opens fresh streams after resume', async () => {
    const { SessionSubscriptionHub } = await import('./subscriptions');
    const { state, client } = fakeClient();
    const stream = vi.spyOn(client, 'streamSessionEvents');
    const hub = new SessionSubscriptionHub(client);
    hub.watchSessions(['session-1']);
    hub.subscribeFleet(() => {});
    expect(state).toEqual({
      sessionOpened: 2,
      sessionStopped: 1,
      fleetOpened: 0,
      fleetStopped: 0,
    });

    native.get('appStateChange')?.({ isActive: false });
    expect(state.sessionStopped).toBe(2);
    expect(state.fleetStopped).toBe(0);

    // The supervisor must not undo the retirement while the app is still away.
    await vi.advanceTimersByTimeAsync(30_000);
    expect(state.sessionOpened).toBe(2);
    expect(state.fleetOpened).toBe(0);

    native.get('appStateChange')?.({ isActive: true });
    await vi.advanceTimersByTimeAsync(250);
    expect(state.sessionOpened).toBe(3);
    expect(stream.mock.calls.at(-1)?.[2]?.reason).toBe('wake');
    expect(state.fleetOpened).toBe(0);

    hub.dispose();
  });
});

// A WebView can stop running JS with no native or DOM lifecycle event. After
// it runs again, an open but silent stream must not hold the turn in flight.
it('replays a missed turn completion after a silent foreground freeze', async () => {
  const { GatewayClient } = await import('./gateway');
  const { SessionSubscriptionHub } = await import('./subscriptions');
  const attempts: { url: URL; signal: AbortSignal; send: (event: SseEvent) => void }[] = [];
  vi.stubGlobal('fetch', vi.fn(async (url: URL | string, { signal }: { signal: AbortSignal }) => ({
    ok: true,
    status: 200,
    body: new ReadableStream<Uint8Array>({
      start(controller) {
        attempts.push({
          url: new URL(url),
          signal,
          send: (event) => controller.enqueue(new TextEncoder().encode(`data: ${JSON.stringify(event)}\n\n`)),
        });
      },
    }),
  })));
  const hub = new SessionSubscriptionHub(new GatewayClient({ url: 'https://gateway.example.com', token: 'test' }));
  try {
    const listener = vi.fn();
    hub.subscribeSession('session-1', listener);
    await vi.advanceTimersByTimeAsync(0);
    const started: SseEvent = { type: 'turn.started', session_id: 'session-1', turn_id: 'turn-1', seq: 10 };
    attempts[0]!.send(started);
    await vi.advanceTimersByTimeAsync(0);
    expect(listener).toHaveBeenCalledExactlyOnceWith(started);

    // Jump wall time without running any JS timers or firing lifecycle events.
    vi.setSystemTime(Date.now() + 300_000);
    expect(attempts).toHaveLength(1);
    await vi.advanceTimersByTimeAsync(6_000);

    expect(attempts[0]!.signal.aborted).toBe(true);
    expect(attempts).toHaveLength(2);
    expect(attempts[1]!.url.searchParams.get('sids')).toBe('session-1:10');
    const completed: SseEvent = { type: 'turn.completed', session_id: 'session-1', turn_id: 'turn-1', seq: 11 };
    attempts[1]!.send(completed);
    await vi.advanceTimersByTimeAsync(0);
    expect(listener).toHaveBeenLastCalledWith(completed);
    expect(hub.hasEndedTurn('session-1', 'turn-1')).toBe(true);
  } finally {
    hub.dispose();
    await vi.advanceTimersByTimeAsync(0);
    vi.unstubAllGlobals();
  }
});
