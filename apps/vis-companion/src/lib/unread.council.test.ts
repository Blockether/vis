import { expect, it } from 'vitest';
import type { Session, TranscriptTurn } from './types';
import { unreadTurnCount, visibleAnsweredTurnCount } from './unread';

it('paints the unread answers the gateway counted, and nothing else', () => {
  // Regression: a background Council wake increased turn_count and produced NEW.
  // The gateway counts settled HUMAN answers only, and the row wears its answer.
  const session = { id: 'leader', status: 'idle', live: false, current_turn_id: null,
    turn_count: 5, answer_count: 2, server_time_ms: 0 } satisfies Session & { answer_count: number };
  expect(unreadTurnCount(session)).toBe(0);
  expect(unreadTurnCount({ ...session, turn_count: 9 })).toBe(0);
  expect(unreadTurnCount({ ...session, is_unread: true, unread_answers: 1 })).toBe(1);
  // A session still running is busy, not unread.
  expect(unreadTurnCount({ ...session, live: true, is_unread: true, unread_answers: 1 })).toBe(0);
});

it('does not consume the next human answer watermark for a settled Council bubble', () => {
  const session = { id: 'leader', answer_count: 1, running_request_kind: 'council' } as Session;
  const turns = [
    { turn_id: 'human', request_kind: 'user', status: 'done' },
    { turn_id: 'coordination', request_kind: 'council', status: 'done' },
    { turn_id: 'cancelled', request_kind: 'user', status: 'interrupted' },
  ] as TranscriptTurn[];
  expect(visibleAnsweredTurnCount(session, turns, 'done', 'council')).toBe(1);
  expect(visibleAnsweredTurnCount(session, turns, 'done', 'user')).toBe(2);
  expect(visibleAnsweredTurnCount(session, turns, 'running', 'user')).toBe(1);
});
