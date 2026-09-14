// @vitest-environment jsdom
import { beforeEach, expect, it, vi } from 'vitest';
import type { Session, TranscriptTurn } from './types';

vi.mock('@capacitor/preferences', () => ({ Preferences: { get: async () => ({ value: null }), set: async () => {} } }));
beforeEach(() => { vi.resetModules(); localStorage.clear(); });

it('does not mark Council coordination as a new human answer', async () => {
  // Regression: a background Council wake increased turn_count and produced NEW.
  const unread = await import('./unread');
  const session = { id: 'leader', status: 'idle', live: false, current_turn_id: null,
    turn_count: 5, answer_count: 2, server_time_ms: 0 } satisfies Session & { answer_count: number };
  unread.markSessionRead(session.id, 2);
  expect(unread.unreadTurnCount(session)).toBe(0);
  expect(unread.unreadTurnCount({ ...session, answer_count: 3 })).toBe(1);
 });

it('does not consume the next human answer watermark for a settled Council bubble', async () => {
  const unread = await import('./unread');
  const session = { id: 'leader', answer_count: 1, running_request_kind: 'council' } as Session;
  const turns = [
    { turn_id: 'human', request_kind: 'user', status: 'done' },
    { turn_id: 'coordination', request_kind: 'council', status: 'done' },
    { turn_id: 'cancelled', request_kind: 'user', status: 'interrupted' },
  ] as TranscriptTurn[];
  const visible = unread.visibleAnsweredTurnCount(session, turns, 'done', 'council');
  expect(visible).toBe(1);
  unread.markSessionRead(session.id, visible);
  expect(unread.unreadTurnCount({ ...session, answer_count: 2 })).toBe(1);
  expect(unread.visibleAnsweredTurnCount(session, turns, 'done', 'user')).toBe(2);
  expect(unread.visibleAnsweredTurnCount(session, turns, 'running', 'user')).toBe(1);
});
