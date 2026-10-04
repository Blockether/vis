import { describe, expect, it } from 'vitest';
import { sessionMillis } from './fleet';
import type { Session } from './types';

function session(extra: Partial<Session>): Session {
  return {
    id: 'session',
    title: 'Session',
    live: false,
    current_turn_id: null,
    turn_count: 1,
    server_time_ms: 0,
    ...extra,
  };
}

// Recency is the newest message. Opening a session is not a message and moves nothing.
describe('session recency', () => {
  it('ranks a session by its newest message', () => {
    const sent = session({
      created_at: '2026-01-01T00:00:00Z',
      modified_at: '2026-01-03T00:00:00Z',
    });
    expect(sessionMillis(sent)).toBe(Date.parse('2026-01-03T00:00:00Z'));
  });

  it('falls back to the creation time for a session without a message', () => {
    const empty = session({ created_at: '2026-01-01T00:00:00Z' });
    expect(sessionMillis(empty)).toBe(Date.parse('2026-01-01T00:00:00Z'));
  });
});
