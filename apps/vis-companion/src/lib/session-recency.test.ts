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

describe('session opening recency', () => {
  it('ranks an explicit opening after older conversation activity', () => {
    const opened = session({
      modified_at: '2026-01-01T00:00:00Z',
      last_opened_at: Date.parse('2026-01-02T00:00:00Z'),
    });
    expect(sessionMillis(opened)).toBe(Date.parse('2026-01-02T00:00:00Z'));
  });

  it('lets later conversation activity overtake an earlier opening', () => {
    const changed = session({
      modified_at: '2026-01-03T00:00:00Z',
      last_opened_at: Date.parse('2026-01-02T00:00:00Z'),
    });
    expect(sessionMillis(changed)).toBe(Date.parse('2026-01-03T00:00:00Z'));
  });

  it('keeps the creation fallback when a session has never been opened', () => {
    const neverOpened = session({
      created_at: '2026-01-01T00:00:00Z',
      last_opened_at: null,
    });
    expect(sessionMillis(neverOpened)).toBe(Date.parse('2026-01-01T00:00:00Z'));
  });
});
