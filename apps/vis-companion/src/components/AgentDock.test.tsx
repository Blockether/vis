// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { Subagent } from '../lib/types';
import { AgentCrumb } from './AgentCrumb';
import { AgentDock } from './AgentDock';

const base: Subagent = { session_id: 'a1', parent_id: 'lead', leader_id: 'lead', team_id: 'turn-1',
  task: 'Read the logs', role: 'research', status: 'running', iteration_budget: 30, iterations_used: 12 };
// A subagent that asks its leader. The leader answers it; the person never sees the question.
const asking: Subagent = { ...base, session_id: 'a2', task: 'Fix the bug', role: 'implement',
  question: 'Keep the old API?', question_entry_id: 40 };

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

describe('AgentDock', () => {
  const timeline = vi.fn(async () => ({ session_id: 'a1', items: [] }));
  const dock = (agents: Subagent[], runningTurnId?: string) =>
    render(<AgentDock agents={agents} sid="lead" runningTurnId={runningTurnId} pending={null} onOpen={vi.fn()} onCancel={vi.fn()} timeline={timeline} />);
  const region = () => screen.getByRole('region', { name: 'Subagents of this turn' });
  const rows = () => within(region()).queryAllByRole('button').filter((button) => button.hasAttribute('data-agent-row'));

  it('shows nothing for an empty team, or a finished team after its turn', () => {
    expect(dock([]).container).toBeEmptyDOMElement();
    cleanup();
    expect(dock([{ ...base, status: 'completed' }]).container).toBeEmptyDOMElement();
  });

  it('folds the whole team of the running turn to one line: its time and its counts by status', () => {
    dock([{ ...base, status: 'completed', result: 'Found 3 routes' }, { ...asking, title: 'Fix it' }], 'turn-1');
    const line = within(region()).getByRole('button', { expanded: false });
    expect(line).toHaveTextContent(/^team(\d+[hms][\d ms]*)?1 running1 done$/);
    expect(rows()).toEqual([]);
    expect(region().querySelectorAll('svg')).toHaveLength(1);
  });

  it('opens the table of the team from its line, and folds it again', () => {
    dock([{ ...base, status: 'completed', result: 'Found 3 routes' }, { ...asking, title: 'Fix it' }], 'turn-1');
    const line = within(region()).getByRole('button', { expanded: false });
    fireEvent.click(line);
    expect(line).toHaveAttribute('aria-expanded', 'true');
    expect(document.getElementById(line.getAttribute('aria-controls')!)).toBeVisible();
    expect(rows().map((row) => row.textContent)).toEqual([
      'research#1Found 3 routesdone',
      'implementFix itFix the bugrunning',
    ]);
    expect(within(region()).queryByText(/old API/)).not.toBeInTheDocument();
    fireEvent.click(line);
    expect(line).toHaveAttribute('aria-expanded', 'false');
    expect(rows()).toEqual([]);
  });

  it('keeps a team after its turn while one of its agents still works, and only that team', () => {
    dock([base, { ...base, session_id: 'old', team_id: 'turn-0', status: 'completed' }], 'turn-2');
    const line = within(region()).getByRole('button', { expanded: false });
    expect(line).toHaveTextContent(/^team(\d+[hms][\d ms]*)?1 running$/);
    fireEvent.click(line);
    expect(rows()).toHaveLength(1);
    expect(rows()[0]).toHaveTextContent(/^research#1Read the logsrunning$/);
  });
});

describe('AgentCrumb', () => {
  it('names the agent and opens its leader', () => {
    const onOpen = vi.fn();
    render(<AgentCrumb sid="a2" parentId="lead" agents={[base, asking]} onOpen={onOpen} />);
    const crumb = screen.getByRole('navigation', { name: 'Agent' });
    expect(crumb).toHaveTextContent(/Leaderimplement#2running/);
    expect(within(crumb).getByText('implement')).toHaveClass('uppercase', 'text-role-implement');
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
