// @vitest-environment jsdom
import { act, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { markSessionRead } from '../lib/unread';
import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
});

// Reported: a gateway died mid-turn, and after the restart the sessions it had killed
// looked exactly like sessions the reader had finished with. The engine already knew —
// `db-sweep-orphaned-running-turns!` flips every orphaned turn to `interrupted` — but
// the list row flattened that to `idle` and the unread count never moved either, so
// neither the dot nor the NEW badge said anything.
describe('a session whose last turn was cut off', () => {
  it('wears a red STOPPED mark in place of the NEW badge', async () => {
    markSessionRead('s-stopped', 2);
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's-stopped', turn_count: 6, answer_count: 3, was_interrupted: true }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('STOPPED')).toBeInTheDocument();
    expect(screen.queryByText('new')).not.toBeInTheDocument();
    // One mark per row: a chip beside the title used to repeat the status mark.
    expect(screen.queryByText('stopped')).not.toBeInTheDocument();

    const dot = view.container.querySelector('[data-session-status-dot]');
    expect(dot?.className).toContain('bg-err');
    // Solid, never pulsing: an interrupted session is the opposite of a live one.
    expect(dot?.className).not.toContain('animate-pulse');
  });

  // The mark must be BOUNDED. `was_interrupted` stays true on the row until that
  // session's next turn settles, which for an abandoned session never happens — so it
  // rides the read mark and retires the moment the reader has seen the session.
  it('retires the mark once the session is read', async () => {
    markSessionRead('s-clears', 2);
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's-clears', turn_count: 6, answer_count: 3, was_interrupted: true }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('STOPPED')).toBeInTheDocument();

    await act(async () => {
      markSessionRead('s-clears', 3);
    });

    expect(screen.queryByText('STOPPED')).not.toBeInTheDocument();
  });

  it('says nothing about an interrupted session the reader has already seen', async () => {
    markSessionRead('s-seen', 3);
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 's-seen', turn_count: 6, answer_count: 3, was_interrupted: true }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('A session')).toBeInTheDocument();
    expect(screen.queryByText('STOPPED')).not.toBeInTheDocument();
    expect(screen.getByText('IDLE')).toBeInTheDocument();
  });

  it('leaves a live session alone, whatever its last settled turn did', async () => {
    markSessionRead('s-live', 2);
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({
              id: 's-live',
              turn_count: 6,
              answer_count: 3,
              was_interrupted: true,
              live: true,
              status: 'running',
              current_turn_id: 't-9',
            }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('LIVE')).toBeInTheDocument();
    expect(screen.queryByText('STOPPED')).not.toBeInTheDocument();
  });
});
