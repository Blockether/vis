// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { screen, waitFor } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';
import type { TranscriptTurn } from '../lib/types';

const runningActivity = {
  state: 'running',
  counts: { running: 1, succeeded: 0, failed: 0, cancelled: 0 },
  rows: [
    {
      id: 'watch-1',
      sequence: 1,
      operation: 'gh.watch',
      presenter: 'generic',
      signal: 'observation',
      state: 'running',
      summary: 'Watch workflow',
      resources: [],
      evidence: [],
    },
  ],
};

const iterations = [
  {
    id: 'it-1',
    position: 1,
    assistant_prose: 'Watching the deploy.',
    forms: [{ op: 'python', source: 'await gh.watch()', activity: runningActivity }],
  },
];

const runningRow = {
  turn_id: 't-live',
  position: 2,
  request: 'watch the deploy',
  status: 'running',
  iterations,
} as unknown as TranscriptTurn;

function stepTally(): string {
  return [...document.querySelectorAll('*')]
    // The innermost node with the whole word: a counter splits it for the phone (`1 run` + `ning`).
    .filter(
      (node) =>
        /cancel|running/i.test(node.textContent ?? '') &&
        ![...node.children].some((child) => /cancel|running/i.test(child.textContent ?? '')),
    )
    .map((node) => node.textContent)
    .join(' | ');
}

// Regression, user report in Vis session 034e287e-9fcc-4a95-b993-65bea2053f60 (paraphrased:
// after a switch back into a session, its running steps showed as cancelled and its live
// view did not come back). The hub still held the PREVIOUS turn's terminal frame, which
// settled the cached bubble of the turn that was still running.
describe('switching back into a running session', () => {
  it('keeps the running steps when only an earlier turn ended', async () => {
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'watch the deploy',
      }),
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: 't-live',
            request: 'watch the deploy',
            answer: '',
            status: 'running',
            startedAt: Date.now(),
            iterations,
          },
          seq: 9,
        }),
        cachedTranscript: () => [runningRow],
        transcript: () => Promise.resolve([runningRow]),
        turnTrace: () => Promise.resolve(iterations),
      },
      // The hub saw `t-done` end and missed `t-live` start.
      subscriptions: { hasEndedTurn: (_sid: string, turnId?: string) => turnId !== 't-live' },
    });

    await screen.findByText('Watching the deploy.');
    await waitFor(() => expect(stepTally()).toMatch(/1 running/));
    expect(stepTally()).not.toMatch(/cancel/i);
  });

  it('reopens a settled bubble while the gateway still runs its turn', async () => {
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'watch the deploy',
      }),
      client: {
        // An earlier visit settled this bubble, but the turn never ended.
        cachedRunningTurn: () => ({
          turn: {
            id: 't-live',
            request: 'watch the deploy',
            answer: '',
            status: 'completed',
            startedAt: Date.now(),
            iterations,
          },
          seq: 9,
        }),
        cachedTranscript: () => [runningRow],
        transcript: () => Promise.resolve([runningRow]),
        turnTrace: () => Promise.resolve(iterations),
      },
      subscriptions: { hasEndedTurn: () => false },
    });

    await screen.findByText('Watching the deploy.');
    await waitFor(() => expect(stepTally()).toMatch(/1 running/));
    expect(stepTally()).not.toMatch(/cancel/i);
  });
});
