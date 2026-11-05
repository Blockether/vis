// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';
import { screen, waitFor, within } from '@testing-library/react';

import type { Subagent } from '../lib/types';
import { renderSessionScreen, sessionFixture } from './session-screen-harness';

const NOW = Date.now();

function child(session: string, change: Partial<Subagent> = {}): Subagent {
  return {
    session_id: 'child', parent_id: session, leader_id: session, team_id: 'turn-with-team',
    title: 'Contract review', task: 'Review the contract', status: 'completed',
    iteration_budget: 4, created_at: NOW - 50_000, finished_at: NOW - 40_000, ...change,
  } as Subagent;
}

function turn(agents: Subagent[]) {
  return {
    turn_id: 'turn-with-team',
    request: 'review the contract',
    status: 'completed',
    created_at: NOW - 60_000,
    iterations: [{ position: 0, answer: 'Both reviews pass.' }],
    agents,
  };
}

// User report 2026-11-05: on open, the team block of a turn came after the rest of the
// transcript. The block waited for a second read of the team; the transcript turn holds it.
describe('the team of a stored turn', () => {
  it('paints with its turn, and the open reads no team that the transcript holds', async () => {
    const session = sessionFixture({ team: { active: 0, total: 1 } });
    const agents = vi.fn(() => new Promise(() => {}));
    renderSessionScreen({
      session,
      client: { transcript: () => Promise.resolve([turn([child(session.id)])]), agents },
    });
    expect(await screen.findByText('review the contract')).toBeInTheDocument();
    const team = screen.getByRole('region', { name: 'Subagents' });
    expect(within(team).getAllByText('Contract review').length).toBeGreaterThan(0);
    expect(agents).not.toHaveBeenCalled();
  });

  it('reads the team on open when the transcript does not hold every child', async () => {
    const session = sessionFixture({ team: { active: 0, total: 2 } });
    const agents = vi.fn(() => new Promise(() => {}));
    renderSessionScreen({
      session,
      client: { transcript: () => Promise.resolve([turn([child(session.id)])]), agents },
    });
    expect(await screen.findByText('review the contract')).toBeInTheDocument();
    await waitFor(() => expect(agents).toHaveBeenCalled());
  });

  // A row that this device cached on an earlier visit is older than the transcript row. The
  // cached RUNNING row put the team in the dock, out of its turn, until the team read ended.
  it('keeps the transcript row over an older cached row', async () => {
    const session = sessionFixture({ team: { active: 0, total: 2 } });
    const running = child(session.id, { status: 'running', finished_at: undefined });
    renderSessionScreen({
      session,
      client: {
        transcript: () => Promise.resolve([turn([child(session.id)])]),
        cachedAgents: () => [running, child(session.id, { session_id: 'other', title: 'Docs review' })],
        agents: () => new Promise(() => {}),
      },
    });
    expect(await screen.findByText('review the contract')).toBeInTheDocument();
    const team = screen.getByRole('region', { name: 'Subagents' });
    expect(within(team).getAllByText('Contract review').length).toBeGreaterThan(0);
    expect(within(team).queryByText(/running/i)).not.toBeInTheDocument();
  });
});
