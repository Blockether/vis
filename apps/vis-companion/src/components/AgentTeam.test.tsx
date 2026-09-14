// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
import type { Subagent } from '../lib/types';
import { AgentTeam, AgentTeamView } from './AgentTeam';

const child: Subagent = { session_id: 'child', parent_id: 'leader', leader_id: 'leader', team_id: 'task',
  task: 'Verify the contract', status: 'running', depth: 1, iteration_budget: 4, iterations_used: 2,
  provider: 'p', model: 'small', pending_input: true, routing_locked: true, usage: { cost_usd: 0.0123 } };
const props = { agents: [child], loading: false, error: null, pending: null,
  onRefresh: vi.fn(), onOpen: vi.fn(), onCancel: vi.fn() };

describe('agent team controls', () => {
  it('shows task, budget, input, routing lock and cost; confirms cancellation', () => {
    const onOpen = vi.fn(); const onCancel = vi.fn();
    render(<AgentTeamView {...props} parentId="leader" onOpen={onOpen} onCancel={onCancel} />);
    expect(screen.getByText('Needs your input')).toBeInTheDocument();
    expect(screen.getByText('Verify the contract')).toBeInTheDocument();
    expect(screen.getByText('2 / 4 iterations')).toBeInTheDocument();
    expect(screen.getByText(/Human model lock/)).toBeInTheDocument();
    expect(screen.getByText(/0.0123/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Open parent' }));
    expect(onOpen).toHaveBeenLastCalledWith('leader');
    fireEvent.click(screen.getByRole('button', { name: 'Inspect agent' }));
    expect(onOpen).toHaveBeenLastCalledWith('child');
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    expect(onCancel).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' }));
    expect(onCancel).toHaveBeenCalledWith('child');
  });
  it('keeps loading, failure, empty and terminal states distinct', () => {
    const { rerender } = render(<AgentTeamView {...props} agents={[]} loading />);
    expect(screen.getByRole('status')).toHaveTextContent('Loading team');
    rerender(<AgentTeamView {...props} agents={[]} error="Gateway unavailable" />);
    expect(screen.getByRole('alert')).toHaveTextContent('Gateway unavailable');
    expect(screen.queryByText(/No subagents/)).not.toBeInTheDocument();
    rerender(<AgentTeamView {...props} agents={[]} />);
    expect(screen.getByText(/No subagents yet/)).toBeInTheDocument();
    rerender(<AgentTeamView {...props} agents={[{ ...child, status: 'budget_limited', pending_input: false }]} />);
    expect(screen.getByText('Budget reached')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Stop agent' })).not.toBeInTheDocument();
  });
 });

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => { resolve = done; });
  return { promise, resolve };
}

afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); });

describe('agent team controller', () => {
  it('polls single-flight, pauses while hidden and aborts on replacement and unmount', async () => {
    vi.useFakeTimers();
    const first = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(first.promise).mockResolvedValue([child]);
    const client = { agents, setting: vi.fn().mockResolvedValue({ enabled: true }), cancelAgent: vi.fn() } as unknown as GatewayClient;
    const { rerender, unmount } = render(<AgentTeam client={client} sid="leader" onOpen={vi.fn()} />);
    await act(async () => {});
    const signal = agents.mock.calls[0][1] as AbortSignal;
    await act(async () => { vi.advanceTimersByTime(15000); });
    expect(agents).toHaveBeenCalledTimes(1);
    await act(async () => { first.resolve([child]); });
    expect(screen.getByRole('button', { name: /Agents: 1 total, 1 active, 1 need input/ })).toBeInTheDocument();
    const hidden = vi.spyOn(document, 'hidden', 'get').mockReturnValue(true);
    await act(async () => { vi.advanceTimersByTime(5000); });
    expect(agents).toHaveBeenCalledTimes(1);
    hidden.mockReturnValue(false);
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(2);
    rerender(<AgentTeam client={client} sid="next" onOpen={vi.fn()} />);
    expect(signal.aborted).toBe(true);
    await act(async () => {});
    expect(agents).toHaveBeenLastCalledWith('next', expect.any(AbortSignal));
    const lastSignal = agents.mock.calls.at(-1)![1] as AbortSignal;
    unmount();
    expect(lastSignal.aborted).toBe(true);
    await act(async () => { vi.advanceTimersByTime(10000); });
    expect(agents).toHaveBeenCalledTimes(3);
  });

  it('keeps a cancellation failure visible across successful background polling', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([child]);
    const cancelAgent = vi.fn().mockRejectedValue(new Error('Stop failed. Try again.'));
    const client = { agents, setting: vi.fn().mockResolvedValue({ enabled: true }), cancelAgent } as unknown as GatewayClient;
    render(<AgentTeam client={client} sid="leader" onOpen={vi.fn()} />);
    await act(async () => {});
    fireEvent.click(screen.getByRole('button', { name: /Agents: 1/ }));
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(cancelAgent).toHaveBeenCalledWith('leader', 'child');
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    await act(async () => { vi.advanceTimersByTime(5000); });
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    cancelAgent.mockResolvedValue(undefined);
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
  });

  it('rejects malformed responses, recovers on refresh and opens the parent', async () => {
    const agents = vi.fn().mockResolvedValueOnce({}).mockResolvedValue([child]);
    const onOpen = vi.fn();
    const client = { agents, setting: vi.fn().mockResolvedValue({ enabled: true }), cancelAgent: vi.fn() } as unknown as GatewayClient;
    render(<AgentTeam client={client} sid="child" parentId="leader" onOpen={onOpen} />);
    fireEvent.click(await screen.findByRole('button', { name: /Agents: 0/ }));
    expect(await screen.findByRole('alert')).toHaveTextContent('invalid agent team');
    fireEvent.click(screen.getByRole('button', { name: 'Refresh team' }));
    expect(await screen.findByText('Verify the contract')).toBeInTheDocument();
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Open parent' }));
    expect(onOpen).toHaveBeenCalledWith('leader');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });
  it('ignores a late response from an aborted session request', async () => {
    const old = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(old.promise).mockResolvedValue([]);
    const client = { agents, setting: vi.fn().mockResolvedValue({ enabled: true }), cancelAgent: vi.fn() } as unknown as GatewayClient;
    const { rerender } = render(<AgentTeam client={client} sid="old" onOpen={vi.fn()} />);
    await act(async () => {});
    rerender(<AgentTeam client={client} sid="new" onOpen={vi.fn()} />);
    await act(async () => {});
    await act(async () => { old.resolve([child]); });
    expect(screen.getByRole('button', { name: /Agents: 0 total/ })).toBeInTheDocument();
  });
  it('keeps the team hidden and does not fetch agents until the feature is enabled', async () => {
    vi.useFakeTimers();
    const setting = vi.fn().mockResolvedValue({ enabled: false });
    const agents = vi.fn().mockResolvedValue([child]);
    const client = { setting, agents } as unknown as GatewayClient;
    render(<AgentTeam client={client} sid="leader" onOpen={vi.fn()} />);
    await act(async () => {});
    expect(screen.queryByRole('button', { name: /Agents:/ })).toBeNull();
    expect(agents).not.toHaveBeenCalled();
    setting.mockResolvedValue({ enabled: true });
    await act(async () => { vi.advanceTimersByTime(5000); });
    expect(screen.getByRole('button', { name: /Agents: 1/ })).toBeInTheDocument();
    setting.mockResolvedValue({ enabled: false });
    await act(async () => { vi.advanceTimersByTime(5000); });
    expect(screen.queryByRole('button', { name: /Agents:/ })).toBeNull();
    expect(agents).toHaveBeenCalledTimes(1);
    expect(setting).toHaveBeenCalledWith('subagents', expect.any(AbortSignal));
    setting.mockRejectedValue(new Error('Settings unavailable'));
    await act(async () => { vi.advanceTimersByTime(5000); });
    expect(screen.queryByRole('button', { name: /Agents:/ })).toBeNull();
    expect(agents).toHaveBeenCalledTimes(1);
  });
});
