// @vitest-environment jsdom
// #212: an incomplete HTTP 200 export must never reach native sharing or saving.
import { afterEach, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, screen } from '@testing-library/react';
import { renderSessionScreen } from './session-screen-harness';
import { activityHistoryPage } from '../dev/activity-history';
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
