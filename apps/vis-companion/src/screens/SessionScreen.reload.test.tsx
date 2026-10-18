// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';
import { act, screen, waitFor } from '@testing-library/react';

import { renderSessionScreen, sessionFixture, subscriptionHub } from './session-screen-harness';
import type { SseEvent } from '../lib/types';

const linger = (ms: number) => new Promise((done) => setTimeout(done, ms));

// #393: `/reload` runs extension code again, and extensions register providers.
// The end of that turn must drop the cached provider fleet, so the picker shows a
// new provider at once instead of after the five-minute router cache.
describe('a finished /reload turn', () => {
  function mount(request: string) {
    const events = subscriptionHub();
    const extensionsReloaded = vi.fn();
    renderSessionScreen({
      session: sessionFixture({
        live: true,
        status: 'running',
        current_turn_id: 'gw-1',
        running_request: request,
      }),
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: 'gw-1',
            request,
            answer: '',
            iterations: [],
            startedAt: Date.now(),
            status: 'running' as const,
          },
          seq: 5,
        }),
        cachedTranscript: () => [],
        extensionsReloaded,
      },
      subscriptions: { subscribeSession: events.subscribeSession },
    });
    const finish = () =>
      act(() =>
        events.emit({ type: 'turn.completed', turn_id: 'gw-1', seq: 6, status: 'completed' } as unknown as SseEvent),
      );
    return { extensionsReloaded, finish };
  }

  it('tells the gateway client that the provider fleet is stale', async () => {
    const { extensionsReloaded, finish } = mount('/reload');
    expect(await screen.findByText('/reload')).toBeInTheDocument();
    finish();
    // Stream frames reach the screen in throttled batches.
    await waitFor(() => expect(extensionsReloaded).toHaveBeenCalledTimes(1));
  });

  it('keeps the provider fleet after any other turn', async () => {
    const { extensionsReloaded, finish } = mount('/reloading is not a command');
    expect(await screen.findByText('/reloading is not a command')).toBeInTheDocument();
    finish();
    await linger(300);
    expect(extensionsReloaded).not.toHaveBeenCalled();
  });
});
