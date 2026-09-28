import { afterEach, describe, expect, it, vi } from 'vitest';

import { ACTIVITY_SETTLED } from '../dev/story-data';
import { activityProjectionFromWire } from './activity';
import { reduceRunningTurnEvent, type RunningTurn } from './running-turn';
import { bufferStreamEvent, sessionEventBatch, supersedingStreamKey } from './session-stream';
import type { SseEvent } from './types';

function event(type: string, seq: number, kind?: 'live'): SseEvent {
  return { type, seq, ...(kind ? { kind } : {}) } as SseEvent;
}

// Stream-isolation contract, session 3d6dc388-a21c-4005-b498-87c02668cb34: Activity frames
// entered both reducers, making each visual update schedule a redundant running-turn pass.
describe('Activity stream isolation', () => {
  it('keeps live-view frames out of the turn reducer', () => {
    expect(
      sessionEventBatch([
        event('turn.started', 1),
        event('view.patch', 2, 'live'),
        event('content.block.delta', 3),
      ]).map((frame) => frame.type),
    ).toEqual(['turn.started', 'content.block.delta']);
  });

  it('keeps only the newest canonical cumulative delta for one stream', () => {
    const first = {
      ...event('content.block.delta', 1),
      iteration: 1,
      block_id: 't:content:1',
      field: 'markdown',
      text: 'a',
      cumulative: 'a',
    };
    const second = { ...first, seq: 2, text: 'b', cumulative: 'ab' };

    expect(sessionEventBatch([first, second])).toEqual([second]);
  });
});

describe('superseding streams', () => {
  const delta = (block: string, field: string, cumulative?: string) =>
    ({ ...event('content.block.delta', 1), iteration: 1, block_id: `t:${block}:1`, field, cumulative }) as SseEvent;
  const activity = (iteration: number, form: number) =>
    ({ ...event('block.activity', 1), iteration, form_index: form }) as SseEvent;

  it('gives cumulative deltas of one block and field the same stream', () => {
    expect(supersedingStreamKey(delta('content', 'markdown', 'a'))).toBe(
      supersedingStreamKey(delta('content', 'markdown', 'ab')),
    );
    expect(supersedingStreamKey(delta('content', 'markdown', 'a'))).not.toBe(
      supersedingStreamKey(delta('reasoning', 'markdown', 'a')),
    );
    expect(supersedingStreamKey(delta('content', 'markdown', 'a'))).not.toBe(
      supersedingStreamKey(delta('content', 'text', 'a')),
    );
  });

  it('gives Activity snapshots of one form the same stream', () => {
    expect(supersedingStreamKey(activity(1, 0))).toBe(supersedingStreamKey(activity(1, 0)));
    expect(supersedingStreamKey(activity(1, 0))).not.toBe(supersedingStreamKey(activity(1, 1)));
    expect(supersedingStreamKey(activity(1, 0))).not.toBe(supersedingStreamKey(activity(2, 0)));
  });

  it('never supersedes a delta without cumulative text or any other frame', () => {
    expect(supersedingStreamKey(delta('content', 'markdown'))).toBeNull();
    expect(supersedingStreamKey(event('turn.progress', 1))).toBeNull();
    expect(supersedingStreamKey(event('block.output', 1))).toBeNull();
  });
});

describe('the replay buffer', () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  const started = { type: 'turn.started', seq: 1, turn_id: 't1', request: 'Check' } as SseEvent;
  const form = { type: 'block.started', seq: 2, iteration: 1, form_index: 0, code: '(ls)' } as SseEvent;
  const prose = (seq: number, cumulative: string) =>
    ({
      type: 'content.block.delta',
      seq,
      iteration: 1,
      block_id: 't1:assistant-prose:1',
      field: 'markdown',
      cumulative,
    }) as SseEvent;
  const history = { id: '12345678-1234-1234-1234-123456789012', total: 3, after: 0, next_after: null };
  const activity = (seq: number, revision: number) =>
    ({
      type: 'block.activity',
      seq,
      iteration: 1,
      form_index: 0,
      activity: { ...ACTIVITY_SETTLED, history: { ...history, revision } },
    }) as SseEvent;
  const replay = (frames: SseEvent[]) =>
    frames.reduce<RunningTurn | null>((turn, frame) => reduceRunningTurnEvent(turn, frame), null);

  it('rebuilds what the live frames built, from their latest revisions only', () => {
    // Both replays start the turn at the same instant, so only the frames can differ.
    vi.useFakeTimers({ now: new Date('2026-01-01T12:00:00Z') });
    // A fixture the reducer cannot parse would make every frame look stale.
    expect(activityProjectionFromWire(activity(0, 1).activity)?.history?.revision).toBe(1);
    const live = [
      started,
      form,
      prose(3, 'I'),
      activity(4, 1),
      prose(5, 'I will'),
      activity(6, 2),
      // Late and malformed Activity: the reducer keeps revision 2 over both.
      activity(7, 1),
      { type: 'block.activity', seq: 8, iteration: 1, form_index: 0, activity: {} } as SseEvent,
      prose(9, 'I will check'),
    ];
    const buffered: SseEvent[] = [];
    for (const frame of live) bufferStreamEvent(buffered, frame);

    expect(buffered.map((frame) => frame.seq)).toEqual([1, 2, 6, 9]);
    expect(replay(buffered)).toEqual(replay(live));
  });
});
