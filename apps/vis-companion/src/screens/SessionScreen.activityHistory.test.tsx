// @vitest-environment jsdom
// #212: removing the toolbar must not lose independently retained histories.
import { afterEach, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, screen, waitFor } from '@testing-library/react';
import { renderSessionScreen } from './session-screen-harness';
import {
  groupedActivityHistoryPage,
  GROUPED_ACTIVITY_HISTORY_IDS as ids,
} from '../dev/activity-history';

afterEach(() => {
  cleanup();
  vi.clearAllMocks();
});

it.each([false, true])('loads grouped history without bulk controls (failure: %s)', async (fails) => {
  const activities = ids.map((id) => groupedActivityHistoryPage(id));
  delete activities[1].history;
  const activityPage = vi.fn(async (_sid: string, id: string, after: number) => {
    if (fails) throw new Error('Later history unavailable');
    return groupedActivityHistoryPage(id, after);
  });
  renderSessionScreen({
    client: {
      activityPage,
      cachedRunningTurn: () => ({
        turn: {
          id: 'grouped-activity-turn',
          request: 'Inspect grouped activity',
          answer: '',
          status: 'running',
          startedAt: Date.now(),
          iterations: [
            {
              id: 'grouped-iteration',
              position: 1,
              forms: activities.map((activity, block_id) => ({
                block_id,
                source: 'inspect_run()',
                activity,
              })),
            },
          ],
        },
        seq: 42,
      }),
    },
  });
  fireEvent.click(await screen.findByRole('button', { name: 'Expand Activity' }));
  expect(activityPage).not.toHaveBeenCalled();
  expect(screen.queryByRole('searchbox', { name: 'Search all operations' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Copy all activity' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Export all activity' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: /Read ×6/ }));
  fireEvent.click(screen.getByRole('button', { name: 'Show more operations' }));
  await waitFor(() =>
    expect(activityPage).toHaveBeenCalledExactlyOnceWith(
      expect.any(String),
      ids[2],
      2,
      '',
      expect.any(AbortSignal),
    ),
  );
  if (fails) {
    expect(await screen.findByRole('alert')).toHaveTextContent('Later history unavailable');
    expect(screen.getByText('review-1-1.clj')).toBeVisible();
    expect(screen.getByText('review-2-1.clj')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Reload operations' })).toBeVisible();
  } else {
    expect(await screen.findByText('review-3-3.clj')).toBeVisible();
    expect(screen.queryByText('review-1-1.clj')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Show more operations' })).toBeNull();
  }
});
