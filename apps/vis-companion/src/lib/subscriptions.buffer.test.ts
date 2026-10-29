// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import activityFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json';
import type { GatewayClient } from './gateway';
import { reduceRunningTurnEvent, type RunningTurn } from './running-turn';
import { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent } from './types';

let hub: SessionSubscriptionHub | null = null;

afterEach(() => {
  hub?.dispose();
  hub = null;
  vi.restoreAllMocks();
});

function streamClient() {
  const streams: { emit: (event: SseEvent) => void }[] = [];
  const client = {
    streamSessionEvents(_cursors: Map<string, number>, emit: (event: SseEvent) => void) {
      streams.push({ emit });
      return vi.fn();
    },
  } as unknown as GatewayClient;
  return { client, streams };
}

function frame(value: Record<string, unknown>): SseEvent {
  return { session_id: 's', turn_id: 't', ...value } as unknown as SseEvent;
}

function delta(seq: number, block: string, field: string, offset: number, text: string): SseEvent {
  return frame({
    type: 'content.block.delta',
    seq,
    iteration: 1,
    block_id: `t:${block}:1`,
    field,
    text,
    offset,
  });
}

function activity(seq: number, state: string): SseEvent {
  return frame({ type: 'block.activity', seq, iteration: 1, form_index: 0, activity: { ...activityFixture, state } });
}

const turn = [
  frame({ type: 'turn.started', seq: 1 }),
  frame({ type: 'block.started', seq: 2, iteration: 1, form_index: 0, scope: 'python', code: 'work()' }),
  delta(3, 'reasoning', 'text', 0, 'p'),
  delta(4, 'assistant-prose', 'markdown', 0, 'A'),
  delta(5, 'reasoning', 'text', 1, 'l'),
  activity(6, 'running'),
  frame({ type: 'turn.progress', seq: 7, iteration: 1, phase: 'tool' }),
  delta(8, 'reasoning', 'text', 2, 'an'),
  activity(9, 'succeeded'),
  delta(10, 'assistant-prose', 'markdown', 1, 'll done'),
];

function streamTurn(): SseEvent[] {
  const { client, streams } = streamClient();
  hub = new SessionSubscriptionHub(client);
  hub.watchSessions(['s']);
  for (const event of turn) streams.at(-1)!.emit(event);
  const replayed: SseEvent[] = [];
  hub.subscribeSession('s', (event) => replayed.push(event))();
  return replayed;
}

function reduce(events: SseEvent[]): RunningTurn | null {
  return events.reduce<RunningTurn | null>((running, event) => reduceRunningTurnEvent(running, event), null);
}

// Memory report: a long running turn buffered every revision of each block's text
// and Activity for every watched session, several megabytes per running session.
describe('running-turn replay buffer', () => {
  it('keeps only the newest text and Activity of each block, in stream order', () => {
    expect(streamTurn().map((event) => event.seq)).toEqual([1, 2, 7, 8, 9, 10]);
  });

  it('replays to the same running turn as the whole stream', () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_000);
    const replayed = reduce(streamTurn());
    expect(replayed?.iterations[0]?.thinking).toBe('plan');
    expect(replayed?.iterations[0]?.forms?.[0]?.activity?.state).toBe('succeeded');
    expect(replayed).toEqual(reduce(turn));
  });
});

// A `vis.notify` notice is live only: a screen that mounts during the turn must not show it again.
describe('session notifications', () => {
  it('reach the live screen once and never the replay of a running turn', () => {
    const { client, streams } = streamClient();
    hub = new SessionSubscriptionHub(client);
    hub.watchSessions(['s']);
    const live: SseEvent[] = [];
    const stopLive = hub.subscribeSession('s', (event) => live.push(event));
    streams.at(-1)!.emit(frame({ type: 'turn.started', seq: 1 }));
    streams.at(-1)!.emit(frame({ type: 'notification', seq: 2, text: 'Deployed', level: 'success' }));
    streams.at(-1)!.emit(frame({ type: 'notification', seq: 2, text: 'Deployed', level: 'success' }));
    stopLive();
    expect(live.map((event) => event.type)).toEqual(['turn.started', 'notification']);
    const replayed: SseEvent[] = [];
    hub.subscribeSession('s', (event) => replayed.push(event))();
    expect(replayed.map((event) => event.type)).toEqual(['turn.started']);
  });
});

// Regression, Vis session 034e287e-9fcc-4a95-b993-65bea2053f60: one ended turn marked the
// whole session as ended, so a later running turn was settled as cancelled on re-entry.
describe('ended turns', () => {
  it('reports the ending of the turn that ended, not of a later one', () => {
    const { client, streams } = streamClient();
    hub = new SessionSubscriptionHub(client);
    hub.watchSessions(['s']);
    streams.at(-1)!.emit(frame({ type: 'turn.started', seq: 1, turn_id: 't-done' }));
    streams.at(-1)!.emit(frame({ type: 'turn.completed', seq: 2, turn_id: 't-done' }));

    expect(hub.hasEndedTurn('s')).toBe(true);
    expect(hub.hasEndedTurn('s', 't-done')).toBe(true);
    expect(hub.hasEndedTurn('s', 't-live')).toBe(false);

    streams.at(-1)!.emit(frame({ type: 'turn.started', seq: 3, turn_id: 't-live' }));
    expect(hub.hasEndedTurn('s', 't-done')).toBe(false);
  });
});
