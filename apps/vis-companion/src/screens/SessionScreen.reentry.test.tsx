// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { act, screen } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';
import type { SseEvent } from '../lib/types';

// User report: back in a live session, the cached bubble painted first and the steps
// that ran while the reader was away landed one stream throttle later, so the end of
// the transcript jumped. The hub gives those frames inside `subscribeSession`.
describe('a live session that the reader opens again', () => {
  it('paints the frames that the hub kept while the reader was away at once', async () => {
    const MISSED = 'A step that ran while the reader was away.';
    const bubble = {
      id: 'gw-1',
      request: 'run the checks',
      answer: '',
      iterations: [{ position: 0, thinking: 'weighing it up' }],
      startedAt: Date.now(),
      status: 'running' as const,
    };
    const kept = {
      type: 'content.block.delta',
      turn_id: 'gw-1',
      seq: 10,
      iteration: 1,
      block_id: 'gw-1:content:1',
      field: 'markdown',
      text: MISSED,
      offset: 0,
    } as unknown as SseEvent;
    renderSessionScreen({
      session: sessionFixture({ live: true, status: 'running', current_turn_id: 'gw-1', running_request: 'run the checks' }),
      client: {
        cachedRunningTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([]),
      },
      subscriptions: {
        subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
          listener(kept);
          return () => {};
        },
      },
    });
    await act(async () => {});
    expect(screen.getByText(MISSED)).toBeInTheDocument();
  });
});
