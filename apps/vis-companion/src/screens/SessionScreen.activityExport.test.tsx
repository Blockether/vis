// @vitest-environment jsdom
// #212: an incomplete HTTP 200 export must never reach native sharing or saving.
import { afterEach, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, screen, waitFor } from '@testing-library/react';
import { renderSessionScreen } from './session-screen-harness';
import {
  activityHistoryPage,
  groupedActivityHistoryPage,
  GROUPED_ACTIVITY_HISTORY_IDS as ids,
} from '../dev/activity-history';
import { GatewayClient } from '../lib/gateway';
import { shareArtifact } from '../lib/artifact-share';

vi.mock('../lib/artifact-share', () => ({ shareArtifact: vi.fn() }));

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
  vi.clearAllMocks();
});

it('shows the export error instead of reporting Shared or Saved', async () => {
  vi.stubGlobal(
    'fetch',
    vi
      .fn()
      .mockResolvedValue(
        new Response(
          'Partial activity\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n',
          { status: 200 },
        ),
      ),
  );
  const gateway = new GatewayClient({ url: 'http://gateway.example.com' });
  renderSessionScreen({
    client: {
      activityExport: gateway.activityExport.bind(gateway),
      cachedRunningTurn: () => ({
        turn: {
          id: 'activity-turn',
          request: 'Inspect activity',
          answer: '',
          status: 'running',
          startedAt: Date.now(),
          iterations: [
            {
              id: 'iteration-41',
              position: 41,
              forms: [{ block_id: 0, source: 'inspect_run()', activity: activityHistoryPage() }],
            },
          ],
        },
        seq: 42,
      }),
    },
  });
  fireEvent.click(await screen.findByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: 'Export all activity' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('Activity changed. Reload and retry.');
  expect(shareArtifact).not.toHaveBeenCalled();
  expect(screen.queryByText(/Shared|Saved/)).toBeNull();
});

it.each([false, true])(
  'exports one complete grouped file, never partial source results (failure: %s)',
  async (fails) => {
    const activities = ids.map((id) => groupedActivityHistoryPage(id));
    delete activities[1].history;
    const activityExport = vi.fn(async (_sid: string, id: string) => {
      if (fails && id === ids[2]) throw new Error('Later history unavailable');
      return new Blob([`Complete history ${id}`], { type: 'text/plain' });
    });
    vi.mocked(shareArtifact).mockResolvedValue('Saved Activity.');
    renderSessionScreen({
      client: {
        activityExport,
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
    fireEvent.click(screen.getByRole('button', { name: 'Export all activity' }));
    if (fails) {
      expect(await screen.findByRole('alert')).toHaveTextContent('Later history unavailable');
      expect(shareArtifact).not.toHaveBeenCalled();
    } else {
      await waitFor(() => expect(shareArtifact).toHaveBeenCalledOnce());
      const blob = vi.mocked(shareArtifact).mock.calls[0][0];
      const text = await new Promise<string>((resolve) => {
        const reader = new FileReader();
        reader.onload = () => resolve(String(reader.result));
        reader.readAsText(blob as Blob);
      });
      expect(text.indexOf(`Complete history ${ids[0]}`)).toBe(0);
      expect(text).toContain('Read result 2-1');
      expect(text).toContain('Read result 2-2');
      expect(text.endsWith(`Complete history ${ids[2]}`)).toBe(true);
      expect(activityExport.mock.calls.map((call) => call[1])).toEqual([ids[0], ids[2]]);
    }
  },
);
