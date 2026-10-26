// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { Subagent } from '../lib/types';
import { AgentCrumb } from './AgentCrumb';
import { AgentDock } from './AgentDock';

const base: Subagent = { session_id: 'a1', parent_id: 'lead', leader_id: 'lead', team_id: 'turn-1',
  task: 'Read the logs', role: 'research', status: 'running', iteration_budget: 30, iterations_used: 12 };
const asking: Subagent = { ...base, session_id: 'a2', task: 'Fix the bug', role: 'implement',
  question: 'Keep the old API?', question_entry_id: 40 };

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

describe('AgentDock', () => {
  const timeline = vi.fn(async () => ({ session_id: 'a1', items: [] }));
  const dock = (agents: Subagent[], runningTurnId?: string) =>
    render(<AgentDock agents={agents} sid="lead" runningTurnId={runningTurnId} pending={null} onOpen={vi.fn()} onCancel={vi.fn()} timeline={timeline} />);

  it('shows nothing for an empty team, or a finished team after its turn', () => {
    expect(dock([]).container).toBeEmptyDOMElement();
    cleanup();
    expect(dock([{ ...base, status: 'completed' }]).container).toBeEmptyDOMElement();
  });

  it('keeps the whole team of the running turn, finished agents too, one row each', () => {
    dock([{ ...base, status: 'completed', result: 'Found 3 routes' }, { ...asking, title: 'Fix it' }], 'turn-1');
    const section = screen.getByRole('region', { name: 'Subagents of this turn' });
    const rows = within(section).getAllByRole('button');
    expect(rows.map((row) => row.textContent)).toEqual([
      expect.stringMatching(/^research#1doneFound 3 routes/),
      expect.stringMatching(/^implementFix itaskingKeep the old API\?/),
    ]);
    expect(section.querySelector('svg')).toBeNull();
  });

  it('keeps a team after its turn while one of its agents still works, and only that team', () => {
    dock([base, { ...base, session_id: 'old', team_id: 'turn-0', status: 'completed' }], 'turn-2');
    const rows = within(screen.getByRole('region', { name: 'Subagents of this turn' })).getAllByRole('button');
    expect(rows).toHaveLength(1);
    expect(rows[0]).toHaveTextContent(/research#1running/);
  });
});

describe('AgentCrumb', () => {
  it('names the agent and opens its leader', () => {
    const onOpen = vi.fn();
    render(<AgentCrumb sid="a2" parentId="lead" agents={[base, asking]} onOpen={onOpen} />);
    const crumb = screen.getByRole('navigation', { name: 'Agent' });
    expect(crumb).toHaveTextContent(/Leaderimplement#2asking/);
    expect(within(crumb).getByText('#2')).toHaveAttribute('title', 'Fix the bug');
    expect(crumb.querySelectorAll('svg')).toHaveLength(1);
    fireEvent.click(within(crumb).getByRole('button', { name: /Leader/ }));
    expect(onOpen).toHaveBeenCalledWith('lead');
  });

  it('keeps the way back before the team rows arrive', () => {
    render(<AgentCrumb sid="a2" parentId="lead" agents={[]} onOpen={vi.fn()} />);
    expect(screen.getByRole('button', { name: /Leader/ })).toBeInTheDocument();
  });
});
