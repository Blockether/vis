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

  it('joins the deltas of one stream into one frame from the first offset', () => {
    const first = {
      ...event('content.block.delta', 1),
      iteration: 1,
      block_id: 't:content:1',
      field: 'markdown',
      text: 'a',
      offset: 0,
    };
    const second = { ...first, seq: 2, text: 'b', offset: 1 };

    expect(sessionEventBatch([first, second])).toEqual([{ ...second, text: 'ab', offset: 0 }]);
    // A repeated frame changes nothing.
    expect(sessionEventBatch([first, second, second])).toEqual([{ ...second, text: 'ab', offset: 0 }]);
  });

  it('keeps a delta that starts after a gap as its own frame', () => {
    const first = {
      ...event('content.block.delta', 1),
      iteration: 1,
      block_id: 't:content:1',
      field: 'markdown',
      text: 'a',
      offset: 0,
    };
    const later = { ...first, seq: 2, text: 'z', offset: 5 };

    expect(sessionEventBatch([first, later])).toEqual([first, later]);
  });
});

describe('superseding streams', () => {
  const delta = (block: string, field: string) =>
    ({ ...event('content.block.delta', 1), iteration: 1, block_id: `t:${block}:1`, field }) as SseEvent;
  const activity = (iteration: number, form: number) =>
    ({ ...event('block.activity', 1), iteration, form_index: form }) as SseEvent;

  it('gives deltas of one block and field the same stream', () => {
    expect(supersedingStreamKey(delta('content', 'markdown'))).toBe(supersedingStreamKey(delta('content', 'markdown')));
    expect(supersedingStreamKey(delta('content', 'markdown'))).not.toBe(
      supersedingStreamKey(delta('reasoning', 'markdown')),
    );
    expect(supersedingStreamKey(delta('content', 'markdown'))).not.toBe(supersedingStreamKey(delta('content', 'text')));
  });

  it('gives Activity snapshots of one form the same stream', () => {
    expect(supersedingStreamKey(activity(1, 0))).toBe(supersedingStreamKey(activity(1, 0)));
    expect(supersedingStreamKey(activity(1, 0))).not.toBe(supersedingStreamKey(activity(1, 1)));
    expect(supersedingStreamKey(activity(1, 0))).not.toBe(supersedingStreamKey(activity(2, 0)));
  });
});

describe('the replay buffer', () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  const started = { type: 'turn.started', seq: 1, turn_id: 't1', request: 'Check' } as SseEvent;
  const form = { type: 'block.started', seq: 2, iteration: 1, form_index: 0, code: '(ls)' } as SseEvent;
  const prose = (seq: number, offset: number, text: string) =>
    ({
      type: 'content.block.delta',
      seq,
      iteration: 1,
      block_id: 't1:assistant-prose:1',
      field: 'markdown',
      text,
      offset,
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
      prose(3, 0, 'I'),
      activity(4, 1),
      prose(5, 1, ' will'),
      activity(6, 2),
      // Late and malformed Activity: the reducer keeps revision 2 over both.
      activity(7, 1),
      { type: 'block.activity', seq: 8, iteration: 1, form_index: 0, activity: {} } as SseEvent,
      prose(9, 6, ' check'),
    ];
    const buffered: SseEvent[] = [];
    for (const frame of live) bufferStreamEvent(buffered, frame);

    expect(buffered.map((frame) => frame.seq)).toEqual([1, 2, 6, 9]);
    expect(buffered.at(-1)).toMatchObject({ text: 'I will check', offset: 0 });
    expect(replay(buffered)).toEqual(replay(live));
  });
});
