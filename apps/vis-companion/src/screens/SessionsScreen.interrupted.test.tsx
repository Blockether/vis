// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
});

// Reported: a gateway died mid-turn, and after the restart the sessions it had killed
// looked exactly like sessions the reader had finished with. The engine already knew —
// `db-sweep-orphaned-running-turns!` flips every orphaned turn to `interrupted` — but
// the list row flattened that to `idle`, and the gateway's unread count never moved
// either, so neither the dot nor the NEW badge said anything.
describe('a session whose last turn was cut off', () => {
  it('wears a red STOPPED mark in place of the NEW badge', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({
              id: 's-stopped',
              turn_count: 6,
              answer_count: 3,
              was_interrupted: true,
              is_unread: true,
              unread_answers: 1,
            }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('STOPPED')).toBeInTheDocument();
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
    // One mark per row: a chip beside the title used to repeat the status mark.
    expect(screen.queryByText('stopped')).not.toBeInTheDocument();

    const dot = view.container.querySelector('[data-session-status-dot]');
    expect(dot?.className).toContain('bg-err');
    // Solid, never pulsing: an interrupted session is the opposite of a live one.
    expect(dot?.className).not.toContain('animate-pulse');
  });

  // The mark must be BOUNDED. `was_interrupted` stays true on the row until that
  // session's next turn settles, which for an abandoned session never happens — so it
  // rides the gateway's unread mark and retires the moment the reader has seen it.
  it('says nothing about an interrupted session the reader has already seen', async () => {
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

  // Vis session 32bcc713: a Python timeout failed the turn without cancellation.
  // The unread error must be visible as STOPPED, not an ordinary NEW answer.
  it('marks an unread failed turn as STOPPED', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({
              id: 's-failed',
              turn_count: 3,
              answer_count: 2,
              was_failed: true,
              is_unread: true,
              unread_answers: 1,
            }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText('STOPPED')).toBeInTheDocument();
    expect(screen.queryByText('NEW')).not.toBeInTheDocument();
  });

  it('leaves a live session alone, whatever its last settled turn did', async () => {
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
              is_unread: true,
              unread_answers: 1,
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
