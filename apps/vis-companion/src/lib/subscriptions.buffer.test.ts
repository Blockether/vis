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

function delta(seq: number, block: string, field: string, cumulative: string): SseEvent {
  return frame({
    type: 'content.block.delta',
    seq,
    iteration: 1,
    block_id: `t:${block}:1`,
    field,
    text: cumulative.slice(-1),
    cumulative,
  });
}

function activity(seq: number, state: string): SseEvent {
  return frame({ type: 'block.activity', seq, iteration: 1, form_index: 0, activity: { ...activityFixture, state } });
}

const turn = [
  frame({ type: 'turn.started', seq: 1 }),
  frame({ type: 'block.started', seq: 2, iteration: 1, form_index: 0, scope: 'python', code: 'work()' }),
  delta(3, 'reasoning', 'text', 'p'),
  delta(4, 'assistant-prose', 'markdown', 'A'),
  delta(5, 'reasoning', 'text', 'pl'),
  activity(6, 'running'),
  frame({ type: 'turn.progress', seq: 7, iteration: 1, phase: 'tool' }),
  delta(8, 'reasoning', 'text', 'plan'),
  activity(9, 'succeeded'),
  delta(10, 'assistant-prose', 'markdown', 'All done'),
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
