import { describe, expect, it } from 'vitest';
import type { Session, TranscriptTurn } from '../lib/types';
import { visibleAnsweredTurnCount } from '../lib/unread';

// Regression, issue #402ce442-abbf-40ae-b0c5-23b248bc4003: an answer that visibly
// finished in the running-turn bubble could still be marked NEW after returning to the list
// because the read mark ignored that bubble while its transcript row was persisting.
describe('session read mark', () => {
  it('counts a visibly settled live answer before its transcript row arrives', () => {
    const session = {
      id: 'race',
      live: true,
      current_turn_id: 'running',
      status: 'running',
      turn_count: 2,
      server_time_ms: 0,
    } satisfies Session;
    const persistedTurns = [
      { turn_id: 'previous', status: 'completed' },
    ] satisfies TranscriptTurn[];

    expect(visibleAnsweredTurnCount(session, persistedTurns, 'running')).toBe(1);
    expect(visibleAnsweredTurnCount(session, persistedTurns, 'completed')).toBe(2);
  });
});
