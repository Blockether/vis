// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { Subagent } from '../lib/types';
import { agentPath } from './AgentCrumb';
import { AgentDock } from './AgentDock';

const base: Subagent = { session_id: 'a1', parent_id: 'lead', leader_id: 'lead', team_id: 'turn-1',
  task: 'Read the logs', role: 'research', status: 'running', iteration_budget: 30, iterations_used: 12 };
// A subagent that asks its leader. The leader answers it; the person never sees the question.
const asking: Subagent = { ...base, session_id: 'a2', task: 'Fix the bug', role: 'implement',
  question: 'Keep the old API?', question_entry_id: 40 };

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

describe('AgentDock', () => {
  const dock = (agents: Subagent[], runningTurnId?: string) =>
    render(<AgentDock agents={agents} sid="lead" runningTurnId={runningTurnId} onOpen={vi.fn()} />);
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
    expect(line).toHaveAccessibleName(/^team (\d+[hms][\d ms]*)?1 running, 1 done$/);
    expect(rows()).toEqual([]);
    expect(region().querySelectorAll('svg')).toHaveLength(1);
  });

  it('keeps a team after its turn while one of its agents still works, and only that team', () => {
    dock([base, { ...base, session_id: 'old', team_id: 'turn-0', status: 'completed' }], 'turn-2');
    const line = within(region()).getByRole('button', { expanded: false });
    expect(line).toHaveAccessibleName(/^team (\d+[hms][\d ms]*)?1 running$/);
    fireEvent.click(line);
    expect(rows()).toHaveLength(1);
    expect(rows()[0]).toHaveTextContent(/^research#1Read the logsrunning$/);
  });
});

// User report: a subagent session had two headers. Its one header names the path from the
// leader, like the TUI header, and opens the parent session.
describe('agentPath', () => {
  const lead = { parent_id: 'lead', leader_id: 'lead' };

  it('keeps the way back before the team rows arrive', () => {
    expect(agentPath('a2', lead, [], 'Fix the bug')).toEqual({
      parentId: 'lead', parent: { role: { text: 'leader', ink: 'text-role-leader' } }, self: { label: 'Fix the bug' } });
  });

  it('names a child of the leader with its role and its #N', () => {
    expect(agentPath('a2', lead, [base, asking], 'title')).toEqual({ parentId: 'lead', parent: { role: { text: 'leader', ink: 'text-role-leader' } },
      self: { role: { text: 'implement', ink: 'text-role-implement' }, label: '#2' } });
  });

  it('names the parent of a grandchild by its role and name', () => {
    const grandchild: Subagent = { ...base, session_id: 'g1', parent_id: 'a1', team_id: 'a1-turn', title: 'Grep logs' };
    expect(agentPath('g1', { parent_id: 'a1', leader_id: 'lead' }, [base, grandchild], 'title')).toEqual({
      parentId: 'a1',
      parent: { role: { text: 'research', ink: 'text-role-research' }, label: '#1' },
      self: { role: { text: 'research', ink: 'text-role-research' }, label: 'Grep logs' },
    });
  });

  it('gives no path to a leader session', () => {
    expect(agentPath('lead', { leader_id: 'lead' }, [base], 'title')).toBeUndefined();
  });
});
