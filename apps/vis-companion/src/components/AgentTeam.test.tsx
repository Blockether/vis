// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
import type { Subagent } from '../lib/types';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import { ROLES, TurnAgents, agentsByTurn, useAgentTeam } from './AgentTeam';
import { ROLE_ICONS } from './icons';

const child: Subagent = { session_id: 'child', parent_id: 'leader', leader_id: 'leader', team_id: 'turn-1',
  task: 'Verify the contract', status: 'running', iteration_budget: 4, iterations_used: 2,
  provider: 'p', model: 'small', routing_locked: true, usage: { cost_usd: 0.0123 } };
const props = { pending: null, onOpen: vi.fn(), onCancel: vi.fn() };

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => { resolve = done; });
  return { promise, resolve };
}

afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); });

describe('turn agents', () => {
  it('gives each agent one line with its state, and folds its details', () => {
    render(<TurnAgents {...props} agents={[child]} />);
    const row = screen.getByRole('button', { expanded: false });
    expect(row).toHaveTextContent('A1');
    expect(row).toHaveTextContent('2/4');
    expect(row).toHaveTextContent('Verify the contract');
    expect(screen.queryByText(/Human model lock/)).not.toBeInTheDocument();
    fireEvent.click(row);
    expect(row).toHaveAttribute('aria-expanded', 'true');
    expect(screen.getByText(/small · Human model lock · 2 \/ 4 iterations · \$0.0123/)).toBeInTheDocument();
  });

  it('marks each contract role with its own Lucide twin of the TUI glyph', () => {
    expect(ROLES.map((role) => role.const)).toEqual(['research', 'implement', 'verify', 'audit']);
    expect(Object.keys(ROLE_ICONS)).toEqual(ROLES.map((role) => role.const));
    expect(new Set(Object.values(ROLE_ICONS)).size).toBe(ROLES.length);
    expect(agentsSchema.$defs.role.oneOf.every((role) => role['x-vis-mark'].length === 1)).toBe(true);
  });

  it('keeps one role together and the A-numbers in start order', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, session_id: 'check', task: 'Run the tests', role: 'verify' },
      { ...child, session_id: 'plain', task: 'No role' },
      { ...child, session_id: 'read', task: 'Read the logs', role: 'research' },
      { ...child, session_id: 'fix', task: 'Fix the bug', role: 'implement' },
    ]} />);
    const rows = screen.getAllByRole('button', { expanded: false });
    expect(rows.map((row) => row.textContent)).toEqual([
      expect.stringMatching(/^A3research.*Read the logs$/),
      expect.stringMatching(/^A4implement.*Fix the bug$/),
      expect.stringMatching(/^A1verify.*Run the tests$/),
      expect.stringMatching(/^A2.*No role$/),
    ]);
    expect(screen.getByTitle('research: Finds facts').querySelector('svg')).toHaveClass('lucide-search');
    fireEvent.click(rows[0]);
    expect(screen.getByText(/research · small/)).toBeInTheDocument();
  });

  it('inspects an agent and confirms a stop', () => {
    const onOpen = vi.fn(); const onCancel = vi.fn();
    render(<TurnAgents {...props} agents={[child]} onOpen={onOpen} onCancel={onCancel} />);
    fireEvent.click(screen.getByRole('button', { expanded: false }));
    fireEvent.click(screen.getByRole('button', { name: 'Inspect agent' }));
    expect(onOpen).toHaveBeenCalledWith('child');
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    expect(onCancel).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' }));
    expect(onCancel).toHaveBeenCalledWith('child');
  });

  it('names done, blocked, asking and terminal agents, and offers no stop after the end', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, session_id: 'done', status: 'completed' },
      { ...child, session_id: 'blocked', pending_input: true, pending_input_count: 2 },
      { ...child, session_id: 'asking', question: 'Count only src/?' },
      { ...child, session_id: 'spent', status: 'budget_limited' },
    ]} />);
    expect(screen.getByText('done')).toBeInTheDocument();
    expect(screen.getByText('needs you ×2')).toBeInTheDocument();
    expect(screen.getByText('asks leader')).toBeInTheDocument();
    expect(screen.getByText('budget used')).toBeInTheDocument();
    // A child that waited on its leader showed only its iterations, as if it still worked.
    fireEvent.click(screen.getByRole('button', { name: /asks leader/ }));
    expect(screen.getByText('Count only src/?')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /asks leader/ }));
    expect(screen.queryByRole('button', { name: 'Stop agent' })).not.toBeInTheDocument();
  });

  it('puts an agent that waits for a person or the leader first, then the roles', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, session_id: 'read', task: 'Read the logs', role: 'research' },
      { ...child, session_id: 'spend', task: 'Wants budget', role: 'verify', budget_request: { iterations: 8 }, question: 'More?' },
      { ...child, session_id: 'form', task: 'Needs a form', role: 'audit', pending_input: true },
    ]} />);
    expect(screen.getAllByRole('button', { expanded: false }).map((row) => row.textContent)).toEqual([
      expect.stringMatching(/^A2verify.*Wants budget$/),
      expect.stringMatching(/^A3audit.*Needs a form$/),
      expect.stringMatching(/^A1research.*Read the logs$/),
    ]);
  });

  it('shows what a working agent does now with a bar for its budget', () => {
    const { container } = render(<TurnAgents {...props} agents={[
      { ...child, activity: 'Patched ~/src/a.clj' },
      { ...child, session_id: 'done', status: 'completed', activity: 'Old line' },
      { ...child, session_id: 'asking', question: 'Which branch?', activity: 'Waits' },
    ]} />);
    expect(screen.queryByText('Waits')).not.toBeInTheDocument();
    expect(screen.getByText('Patched ~/src/a.clj')).toBeInTheDocument();
    expect(screen.queryByText('Old line')).not.toBeInTheDocument();
    const bars = container.querySelectorAll('[data-agent-progress]');
    expect(bars).toHaveLength(1);
    expect((bars[0].firstElementChild as HTMLElement).style.width).toBe('50%');
    expect(screen.getByText('2/4')).toBeInTheDocument();
  });

  it('renders nothing for a turn without agents', () => {
    const { container } = render(<TurnAgents {...props} agents={[]} />);
    expect(container).toBeEmptyDOMElement();
  });

  it('groups agents by the turn that started them, in start order', () => {
    const groups = agentsByTurn([
      child,
      { ...child, session_id: 'later', team_id: 'turn-2' },
      { ...child, session_id: 'second', team_id: 'turn-1' },
    ]);
    expect(groups.get('turn-1')?.map((agent) => agent.session_id)).toEqual(['child', 'second']);
    expect(groups.get('turn-2')?.map((agent) => agent.session_id)).toEqual(['later']);
  });
});

function Team({ client, sid, running = false }: { client: GatewayClient; sid: string; running?: boolean }) {
  const team = useAgentTeam(client, sid, running);
  return <TurnAgents agents={team.agents} pending={team.pending} error={team.error} onOpen={vi.fn()} onCancel={team.cancel} />;
}

describe('agent team reader', () => {
  it('polls single-flight while a turn runs, pauses while hidden and aborts on replacement and unmount', async () => {
    vi.useFakeTimers();
    const first = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(first.promise).mockResolvedValue([child]);
    const client = { agents, cancelAgent: vi.fn() } as unknown as GatewayClient;
    const { rerender, unmount } = render(<Team client={client} sid="leader" running />);
    await act(async () => {});
    const signal = agents.mock.calls[0][1] as AbortSignal;
    await act(async () => { vi.advanceTimersByTime(9000); });
    expect(agents).toHaveBeenCalledTimes(1);
    await act(async () => { first.resolve([child]); });
    expect(screen.getByText('Verify the contract')).toBeInTheDocument();
    const hidden = vi.spyOn(document, 'hidden', 'get').mockReturnValue(true);
    await act(async () => { vi.advanceTimersByTime(3000); });
    expect(agents).toHaveBeenCalledTimes(1);
    hidden.mockReturnValue(false);
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(2);
    rerender(<Team client={client} sid="next" running />);
    expect(signal.aborted).toBe(true);
    await act(async () => {});
    expect(agents).toHaveBeenLastCalledWith('next', expect.any(AbortSignal));
    const lastSignal = agents.mock.calls.at(-1)![1] as AbortSignal;
    const calls = agents.mock.calls.length;
    unmount();
    expect(lastSignal.aborted).toBe(true);
    await act(async () => { vi.advanceTimersByTime(10000); });
    expect(agents).toHaveBeenCalledTimes(calls);
  });

  it('reads a settled team once and stops polling', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([{ ...child, status: 'completed' }]);
    render(<Team client={{ agents, cancelAgent: vi.fn() } as unknown as GatewayClient} sid="leader" />);
    await act(async () => {});
    await act(async () => { vi.advanceTimersByTime(30000); });
    expect(agents).toHaveBeenCalledTimes(1);
    expect(screen.getByText('done')).toBeInTheDocument();
  });

  it('keeps its rows on a malformed answer and ignores a late answer for another session', async () => {
    const old = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(old.promise).mockResolvedValueOnce({}).mockResolvedValue([]);
    const client = { agents, cancelAgent: vi.fn() } as unknown as GatewayClient;
    const { rerender } = render(<Team client={client} sid="old" />);
    await act(async () => {});
    rerender(<Team client={client} sid="new" />);
    await act(async () => {});
    await act(async () => { old.resolve([child]); });
    expect(screen.queryByText('Verify the contract')).not.toBeInTheDocument();
  });

  it('keeps a stop failure visible across background reads until a stop succeeds', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([child]);
    const cancelAgent = vi.fn().mockRejectedValue(new Error('Stop failed. Try again.'));
    render(<Team client={{ agents, cancelAgent } as unknown as GatewayClient} sid="leader" />);
    await act(async () => {});
    fireEvent.click(screen.getByRole('button', { expanded: false }));
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(cancelAgent).toHaveBeenCalledWith('leader', 'child');
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    await act(async () => { vi.advanceTimersByTime(3000); });
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    cancelAgent.mockResolvedValue(undefined);
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
  });
});
