// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';
import activityFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json';
import { STORY_LIVE_VIEW } from '../dev/story-data';

// The engine opens a view inside a tool call, so the view names that call as its owner.
const view = { ...STORY_LIVE_VIEW, owner: { invocation_id: 'call-1' } };

const connected = (on: (live: boolean) => void) => {
  on(true);
  return () => {};
};

// The picture of a run shows only after a click. Its progress meter is in the picture, not in
// the one-line controls that open it.
const expectNoPicture = () => {
  expect(screen.queryByRole('progressbar')).toBeNull();
  expect(screen.queryByRole('dialog')).toBeNull();
};

describe('live views on session open', () => {
  // Regression, user report: on a switch to a session with a running live view, the whole
  // picture showed in a frame, then folded into the step digest when the block arrived.
  it('keeps the picture behind a click while the owning block arrives', async () => {
    const liveViews = vi.fn(() => Promise.resolve([view]));
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'Watch the fleet scan.',
      }),
      // The block that opened the view has not reached the trace yet.
      client: { liveViews, turnTrace: () => Promise.resolve([]) },
      subscriptions: {
        subscribeConnection: connected,
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });
    await screen.findByText('Watch the fleet scan.');
    await waitFor(() => expect(liveViews).toHaveBeenCalled());
    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(1));
    await act(async () => {});
    expect(screen.queryByText(view.title)).toBeNull();
    expectNoPicture();

    act(() => {
      for (const on of listeners) {
        on({
          type: 'block.started',
          session_id: 's1',
          turn_id: 't-live',
          block_id: 'code-1',
          iteration: 1,
          form_index: 0,
          code: 'monitor()',
        });
        on({
          type: 'block.activity',
          session_id: 's1',
          turn_id: 't-live',
          iteration: 1,
          form_index: 0,
          activity: activityFixture,
        });
      }
    });

    const control = await screen.findByRole('button', {
      name: `Open running live view: ${view.title}`,
    });
    expectNoPicture();
    fireEvent.click(control);
    const dialog = await screen.findByRole('dialog', { name: view.title });
    expect(within(dialog).getByRole('progressbar')).toBeVisible();
    // An opened run stays open across mounts, so close it before the next test.
    fireEvent.click(within(dialog).getByRole('button', { name: `Close ${view.title}` }));
  });

  it('paints nothing for a view that outlives its running turn', async () => {
    const liveViews = vi.fn(() => Promise.resolve([view]));
    renderSessionScreen({
      client: { liveViews },
      subscriptions: { subscribeConnection: connected },
    });
    await waitFor(() => expect(liveViews).toHaveBeenCalled());
    await act(async () => {});
    expect(screen.queryByText(view.title)).toBeNull();
    expectNoPicture();
  });
});
