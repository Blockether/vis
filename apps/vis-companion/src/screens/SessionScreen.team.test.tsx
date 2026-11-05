// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { screen, within } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';

// User report 2026-11-05: on open, the team block of a turn came after the rest of the
// transcript. The block waited for a second read of the team; the transcript turn holds it.
describe('the team of a stored turn', () => {
  it('paints with its turn, without the team read', async () => {
    const session = sessionFixture({ team: { active: 0, total: 1 } });
    const row = {
      turn_id: 'turn-with-team',
      request: 'review the contract',
      status: 'completed',
      created_at: Date.now() - 60_000,
      iterations: [{ position: 0, answer: 'Both reviews pass.' }],
      agents: [{
        session_id: 'child', parent_id: session.id, leader_id: session.id, team_id: 'turn-with-team',
        title: 'Contract review', task: 'Review the contract', status: 'completed',
        iteration_budget: 4, created_at: Date.now() - 50_000, finished_at: Date.now() - 40_000,
      }],
    };
    renderSessionScreen({
      session,
      client: {
        transcript: () => Promise.resolve([row]),
        agents: () => new Promise(() => {}),
      },
    });
    expect(await screen.findByText('review the contract')).toBeInTheDocument();
    const team = screen.getByRole('region', { name: 'Subagents' });
    expect(within(team).getAllByText('Contract review').length).toBeGreaterThan(0);
  });
});
