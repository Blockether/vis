// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from './gateway';
import { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent } from './types';

// A new gateway connection builds a new hub, but the cached bubble and the device cursor
// outlive it. A screen that seeds from an older bubble must still get the frames after it.

let hub: SessionSubscriptionHub | null = null;

afterEach(() => {
  hub?.dispose();
  hub = null;
  vi.restoreAllMocks();
});

function streamClient() {
  const streams: { from: number | undefined; emit: (event: SseEvent) => void }[] = [];
  const client = {
    streamSessionEvents(cursors: Map<string, number>, emit: (event: SseEvent) => void) {
      streams.push({ from: cursors.get('s'), emit });
      return vi.fn();
    },
    streamFleetStatus: () => vi.fn(),
  } as unknown as GatewayClient;
  return { client, streams };
}

function frame(value: Record<string, unknown>): SseEvent {
  return { session_id: 's', turn_id: 't', ...value } as unknown as SseEvent;
}

function ready(cursor: number): SseEvent {
  return frame({ type: 'subscription.ready', cursor, running_turn_id: 't' });
}

function progress(seq: number): SseEvent {
  return frame({ type: 'turn.progress', seq, iteration: seq, phase: 'tool' });
}

function seqs(events: SseEvent[]): number[] {
  return events.flatMap((event) => (typeof event.seq === 'number' ? [event.seq] : []));
}

function setup() {
  const { client, streams } = streamClient();
  hub = new SessionSubscriptionHub(client);
  const fleet: SseEvent[] = [];
  hub.subscribeFleet((event) => fleet.push(event));
  return { hub, streams, fleet };
}

describe('SessionSubscriptionHub seeded screens', () => {
  it('replays the gap after a cached bubble only to the screen that asks for it', async () => {
    const { hub, streams, fleet } = setup();
    const other: SseEvent[] = [];
    hub.subscribeSession('s', (event) => other.push(event));
    streams.at(-1)!.emit(ready(10));
    streams.at(-1)!.emit(progress(11));

    const screen: SseEvent[] = [];
    hub.subscribeSession('s', (event) => screen.push(event), { from: 4 });
    const replay = streams.at(-1)!;
    expect(replay.from).toBe(4);

    replay.emit(ready(4));
    for (let seq = 5; seq <= 12; seq += 1) replay.emit(progress(seq));

    expect(seqs(screen)).toEqual([5, 6, 7, 8, 9, 10, 11, 12]);
    expect(seqs(other)).toEqual([11, 12]);
    expect(seqs(fleet)).toEqual([11, 12]);
    await Promise.resolve();
    expect(streams.at(-1)).toBe(replay);
  });

  it('keeps the stream when the hub already covers the cached bubble', () => {
    const { hub, streams } = setup();
    hub.subscribeSession('s', () => {});
    streams.at(-1)!.emit(ready(10));
    const count = streams.length;

    hub.subscribeSession('s', () => {}, { from: 10 });
    hub.subscribeSession('s', () => {}, { from: 15 });

    expect(streams.length).toBe(count);
  });

  it('asks again after the echo when the screen subscribes first', async () => {
    const { hub, streams } = setup();
    const screen: SseEvent[] = [];
    hub.subscribeSession('s', (event) => screen.push(event), { from: 4 });
    const first = streams.at(-1)!;
    first.emit(ready(10));

    await Promise.resolve();
    const replay = streams.at(-1)!;
    expect(replay).not.toBe(first);
    expect(replay.from).toBe(4);
    replay.emit(ready(4));
    for (let seq = 5; seq <= 11; seq += 1) replay.emit(progress(seq));

    expect(seqs(screen)).toEqual([5, 6, 7, 8, 9, 10, 11]);
  });

  it('replays nothing when the gateway resumes before the cached bubble', async () => {
    const { hub, streams } = setup();
    const screen: SseEvent[] = [];
    hub.subscribeSession('s', (event) => screen.push(event), { from: 4 });
    const first = streams.at(-1)!;
    first.emit(ready(2));
    for (let seq = 3; seq <= 6; seq += 1) first.emit(progress(seq));

    await Promise.resolve();
    expect(streams.at(-1)).toBe(first);
    expect(seqs(screen)).toEqual([3, 4, 5, 6]);
  });
});
