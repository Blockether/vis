// @vitest-environment jsdom
import { ReadableStream } from 'node:stream/web';
import { TextEncoder } from 'node:util';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, cleanup } from '@testing-library/react';

import { GatewayClient } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import { renderSessionScreen, sessionFixture } from './session-screen-harness';

let hub: SessionSubscriptionHub | undefined;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date('2026-10-03T08:00:00Z'));
});

afterEach(async () => {
  cleanup();
  hub?.dispose();
  hub = undefined;
  await act(() => vi.advanceTimersByTimeAsync(0));
  vi.useRealTimers();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

async function quietTurn() {
  const streams: ReadableStreamDefaultController<Uint8Array>[] = [];
  const fetches = vi.fn(async () => ({
    ok: true,
    status: 200,
    body: new ReadableStream<Uint8Array>({
      start(controller) {
        streams.push(controller);
      },
    }),
  }));
  vi.stubGlobal('fetch', fetches);
  hub = new SessionSubscriptionHub(new GatewayClient({ url: 'https://gateway.example.com' }));
  const resync = vi.spyOn(hub, 'resync');
  const turnStatus = vi.fn<() => Promise<{ status: string } | null>>(async () => ({
    status: 'running',
  }));
  renderSessionScreen({
    session: sessionFixture({
      live: true,
      current_turn_id: 'turn-1',
      running_started_at: Date.now(),
      running_request: 'A quiet tool call',
    }),
    client: { turnStatus, turnTrace: async () => [] },
    subscriptions: {
      subscribeSession: hub.subscribeSession.bind(hub),
      subscribeConnection: hub.subscribeConnection.bind(hub),
      resync: hub.resync.bind(hub),
    },
  });
  await act(() => vi.advanceTimersByTimeAsync(0));
  return {
    fetches,
    resync,
    turnStatus,
    heartbeat: () => streams.at(-1)!.enqueue(new TextEncoder().encode(': ping\n\n')),
  };
}

describe('quiet running turns', () => {
  // Regression: the turn timer ignored SSE comments and restarted a healthy stream.
  it('keeps one stream while heartbeats arrive during a quiet tool call', async () => {
    const { fetches, resync, turnStatus, heartbeat } = await quietTurn();
    for (let tick = 0; tick < 6; tick += 1) {
      await act(async () => {
        await vi.advanceTimersByTimeAsync(15_000);
        heartbeat();
        await vi.advanceTimersByTimeAsync(0);
      });
    }
    expect(turnStatus).toHaveBeenCalled();
    expect(resync).not.toHaveBeenCalled();
    expect(fetches).toHaveBeenCalledTimes(1);
  });

  it('lets the transport retry when both events and heartbeats stop', async () => {
    const { fetches, resync } = await quietTurn();
    await act(() => vi.advanceTimersByTimeAsync(44_999));
    expect(fetches).toHaveBeenCalledTimes(1);
    await act(() => vi.advanceTimersByTimeAsync(401));
    expect(fetches).toHaveBeenCalledTimes(2);
    expect(resync).not.toHaveBeenCalled();
  });

  it('replays only after two missing-turn verdicts agree', async () => {
    const { fetches, resync, turnStatus } = await quietTurn();
    turnStatus
      .mockResolvedValueOnce(null)
      .mockResolvedValueOnce({ status: 'running' })
      .mockResolvedValue(null);
    await act(() => vi.advanceTimersByTimeAsync(15_000));
    expect(resync).not.toHaveBeenCalled();
    await act(() => vi.advanceTimersByTimeAsync(5_000));
    expect(resync).not.toHaveBeenCalled();
    await act(() => vi.advanceTimersByTimeAsync(5_000));
    expect(resync).toHaveBeenCalledExactlyOnceWith('turn_unknown');
    expect(fetches).toHaveBeenCalledTimes(2);
  });

  it.each(['completed', 'failed', 'cancelled'])(
    'settles a lost %s event from the registry',
    async (status) => {
      const { fetches, resync, turnStatus } = await quietTurn();
      turnStatus.mockResolvedValue({ status });
      await act(() => vi.advanceTimersByTimeAsync(10_000));
      await act(() => vi.advanceTimersByTimeAsync(20_000));
      expect(turnStatus).toHaveBeenCalledExactlyOnceWith('s1', 'turn-1');
      expect(resync).not.toHaveBeenCalled();
      expect(fetches).toHaveBeenCalledTimes(1);
    },
  );
});
