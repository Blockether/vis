// @vitest-environment jsdom
import { act, screen, waitFor } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import { STORY_GOAL } from '../dev/story-data';
import { renderSessionScreen, sessionFixture, subscriptionHub } from './session-screen-harness';

it('updates the header from a goal event without another HTTP request', async () => {
  const hub = subscriptionHub();
  let row = sessionFixture({ goal: STORY_GOAL });
  const read = vi.fn(() => Promise.resolve(row));
  const note = vi.fn((_sid, goal) => (row = { ...row, goal }));
  renderSessionScreen({
    session: row,
    subscriptions: hub,
    client: { session: read, noteSessionGoal: note },
  });
  await screen.findByRole('button', { name: /^Goal: Active/ });
  const before = read.mock.calls.length;
  act(() =>
    hub.emit({
      type: 'session.goal_updated',
      goal: { ...STORY_GOAL, revision: 4, status: 'complete' },
    } as never),
  );
  await waitFor(() =>
    expect(screen.getByRole('button', { name: /^Goal: Complete/ })).toBeInTheDocument(),
  );
  expect(note).toHaveBeenCalledOnce();
  expect(read).toHaveBeenCalledTimes(before);
});

it('refreshes an idle goal from the canonical reconnect snapshot', async () => {
  const hub = subscriptionHub();
  let row = sessionFixture({ goal: STORY_GOAL });
  const note = vi.fn((_sid, goal) => (row = { ...row, goal }));
  renderSessionScreen({ session: row, subscriptions: hub, client: { noteSessionGoal: note } });
  await screen.findByRole('button', { name: /^Goal: Active/ });
  act(() =>
    hub.emit({
      type: 'subscription.ready',
      is_live: false,
      goal: { ...STORY_GOAL, revision: 4, status: 'paused' },
    } as never),
  );
  await waitFor(() =>
    expect(screen.getByRole('button', { name: /^Goal: Paused/ })).toBeInTheDocument(),
  );
  expect(note).toHaveBeenCalledOnce();
});
