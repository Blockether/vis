// @vitest-environment jsdom
import { readFileSync } from 'node:fs';
import { act, cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
import type { AgentTimeline, Subagent } from '../lib/types';
import {
  ROLES,
  TurnAgents,
  agentLabel,
  agentRole,
  agentStatus,
  agentTime,
  agentUpdate,
  agentsByTurn,
  clockTime,
  dockTeams,
  formatAgentTime,
  teamCounts,
  useAgentTeam,
} from './AgentTeam';

// Vitest stubs CSS imports, so read the shipped stylesheet itself. Under jsdom,
// `import.meta.url` is no file URL: the path is relative to the app directory.
const css = readFileSync('src/index.css', 'utf8');

const T0 = new Date(2026, 4, 3, 14, 3, 7).getTime();
const child: Subagent = { session_id: 'child', parent_id: 'leader', leader_id: 'leader', team_id: 'turn-1',
  task: 'Verify the contract', status: 'running', iteration_budget: 4, iterations_used: 2,
  provider: 'p', model: 'small', routing_locked: true, usage: { cost_usd: 0.0123 }, created_at: T0 };
const empty = async (): Promise<AgentTimeline> => ({ session_id: 'child', items: [] });
const props = { pending: null, onOpen: vi.fn(), onCancel: vi.fn(), timeline: vi.fn(empty) };

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => { resolve = done; });
  return { promise, resolve };
}

const rowTexts = () => screen.getAllByRole('button').map((row) => row.textContent);

afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); });

describe('agent rows', () => {
  // A subagent asks only its leader. Its question or request is no status for a person.
  it('reads every status as words, and a question to the leader as running', () => {
    const words = [
      { status: 'queued' }, { status: 'running' }, { question: 'Which branch?' }, { budget_request: { iterations: 8 } },
      { files_request: { reason: 'Docs' } }, { pending_input: true }, { status: 'completed' },
      { status: 'completed', outcome: 'passed' }, { status: 'completed', outcome: 'failed' }, { status: 'failed' },
      { status: 'cancelled' }, { status: 'budget_limited' },
    ].map((change) => agentStatus({ ...child, ...change } as Subagent).text);
    expect(words).toEqual([
      'queued', 'running', 'running', 'running', 'running', 'running', 'done',
      'passed', 'failed', 'failed', 'cancelled', 'out of budget',
    ]);
  });

  it('names an agent by its title, else by #N in the spawn order of its team', () => {
    const team = [
      { ...child, session_id: 'b', created_at: T0 + 2 },
      { ...child, session_id: 'a', created_at: T0 + 1, title: '  Map routes ' },
      { ...child, session_id: 'other', team_id: 'turn-2', created_at: T0 },
    ];
    expect(team.map((agent) => agentLabel(agent, team))).toEqual(['#2', 'Map routes', '#1']);
  });

  it('times the current task: so far while it runs, its duration after, none while queued', () => {
    const resumed = { ...child, resumed_at: T0 + 60_000 };
    expect(agentTime(child, T0 + 45_000)).toBe(45_000);
    expect(agentTime(resumed, T0 + 90_000)).toBe(30_000);
    expect(agentTime({ ...resumed, status: 'completed', finished_at: T0 + 244_000 }, 0)).toBe(184_000);
    expect(agentTime({ ...child, status: 'queued' }, T0 + 9_000)).toBeUndefined();
    expect(agentTime({ ...child, status: 'failed' }, T0 + 9_000)).toBeUndefined();
    expect(agentTime({ ...child, created_at: undefined }, T0)).toBeUndefined();
    expect([0, 45_999, 184_000, 3_725_000].map(formatAgentTime)).toEqual(['0s', '45s', '3m 04s', '1h 02m']);
  });

  it('gives each agent one table row: role, title, update, time, status; without icons', () => {
    vi.useFakeTimers({ now: T0 + 65_000 });
    const { container } = render(<TurnAgents {...props} agents={[
      { ...child, role: 'research', activity: 'Search files AgentTeam' },
      { ...child, session_id: 'v', role: 'verify', title: 'Gateway tests', status: 'completed', outcome: 'failed',
        result: '2 of 40 tests fail', activity: 'Old line', finished_at: T0 + 30_000, created_at: T0 + 1 },
      { ...child, session_id: 'q', status: 'queued', created_at: T0 + 2 },
    ]} />);
    expect(rowTexts()).toEqual([
      'research#1Search files AgentTeam1m 05srunning',
      'verifyGateway tests2 of 40 tests fail29sfailed',
      'agent#3Verify the contractqueued',
    ]);
    expect(container.querySelector('svg')).toBeNull();
    const [role, , , , status] = container.querySelector('[data-agent-row="v"]')!.children;
    expect(role).toHaveClass('uppercase', 'text-role-verify');
    expect(status).toHaveClass('uppercase', 'text-err-ink');
    expect(screen.getByText('Gateway tests')).toHaveAttribute('title', 'Verify the contract');
    // The time ticks while an agent runs; no message is needed for it.
    act(() => { vi.advanceTimersByTime(2000); });
    expect(rowTexts()[0]).toBe('research#1Search files AgentTeam1m 07srunning');
  });

  it('shows what a working agent does now: its newest note, its activity or its task, then its result', () => {
    const asks = { question: 'Keep the old API?', budget_request: { iterations: 8, reason: 'Slow tests' } };
    expect(agentUpdate({ ...child, ...asks, note: 'The bug is in tokenize', activity: 'Read ~/a.clj' })).toBe('The bug is in tokenize');
    expect(agentUpdate({ ...child, activity: 'Read ~/a.clj' })).toBe('Read ~/a.clj');
    expect(agentUpdate({ ...child, status: 'queued', task: '  Check the links.\nReport the dead ones.' })).toBe('Check the links.');
    expect(agentUpdate({ ...child, status: 'completed', note: 'Old note', result: 'All pass' })).toBe('All pass');
    expect(agentUpdate({ ...child, status: 'cancelled', note: 'Old note' })).toBeUndefined();
    render(<TurnAgents {...props} agents={[{ ...child, ...asks, note: 'The bug is in tokenize' }]} />);
    expect(rowTexts()[0]).toMatch(/^agent#1The bug is in tokenize/);
    expect(screen.queryByText(/old API|Slow tests/)).not.toBeInTheDocument();
  });

  it('wraps a long title and clamps a long update, never with an ellipsis on the title', () => {
    const long = 'A result line that is much longer than the row can show on a phone or a desk';
    const { container } = render(<TurnAgents {...props} agents={[{ ...child, title: 'T'.repeat(40), status: 'completed', result: long }]} />);
    expect(screen.getByText('T'.repeat(40))).not.toHaveClass('truncate');
    expect(screen.getByText('T'.repeat(40)).className).toContain('[overflow-wrap:anywhere]');
    expect(screen.getByText(long)).toHaveClass('line-clamp-2');
    expect(screen.getByText(long)).toHaveAttribute('title', long);
    // The title column fits the longest title of the team, so every row keeps one grid.
    const table = container.querySelector<HTMLElement>('[style]')!;
    expect(table.style.getPropertyValue('--title')).toBe('41ch');
    expect(table.style.getPropertyValue('--role')).toBe('5ch');
    expect(table.style.getPropertyValue('--status')).toBe('6ch');
  });

  it('takes one line on a phone for a row without an update, not a blank second line', () => {
    const { container } = render(<TurnAgents {...props} agents={[{ ...child, status: 'cancelled' }]} />);
    const [, , update] = container.querySelector('[data-agent-row="child"]')!.children;
    expect(update).toHaveClass('max-sm:hidden');
  });

  it('keeps the spawn order whatever the status', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, session_id: 'late', created_at: T0 + 5, pending_input: true },
      { ...child, session_id: 'early', created_at: T0 + 1, status: 'completed' },
    ]} />);
    expect(rowTexts()).toEqual([expect.stringMatching(/^agent#1done$/), expect.stringMatching(/^agent#2Verify the contract.*running$/)]);
  });

  // A form of a subagent opens in the form prompt of the leader session, like any
  // other form. Its row only runs and opens the timeline, never the child session.
  it('shows an agent with an open form as running, with no control for its requests', () => {
    const onOpen = vi.fn();
    render(<TurnAgents {...props} onOpen={onOpen} agents={[
      { ...child, pending_input: true },
      { ...child, session_id: 'asks', question: 'Keep the old API?', budget_request: { iterations: 8 } },
    ]} />);
    expect(screen.queryByText('needs you')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^(Answer|Grant|Deny|Later)/ })).not.toBeInTheDocument();
    expect(screen.queryByText('Keep the old API?')).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /^agent#1/ }));
    expect(onOpen).not.toHaveBeenCalled();
    expect(screen.getByRole('dialog')).toBeInTheDocument();
  });

  it('renders nothing for a turn without agents', () => {
    const { container } = render(<TurnAgents {...props} agents={[]} />);
    expect(container).toBeEmptyDOMElement();
  });

  it('keeps the contract roles in their order', () => {
    expect(ROLES.map((role) => role.const)).toEqual(['research', 'implement', 'verify', 'audit']);
  });

  it('gives every contract role its own ink from the theme, and any other role the hint ink', () => {
    const inks = ROLES.map((role) => agentRole({ ...child, role: role.const }).ink);
    expect(inks).toEqual(ROLES.map((role) => `text-role-${role.const}`));
    for (const role of ROLES) expect(css).toContain(`--color-role-${role.const}:`);
    expect(agentRole({ ...child, role: 'boss' })).toEqual({ text: 'boss', ink: 'text-dialog-hint' });
    expect(agentRole({ ...child, role: 'constructor' }).ink).toBe('text-dialog-hint');
    expect(agentRole(child)).toEqual({ text: 'agent', ink: 'text-dialog-hint' });
  });
});

describe('agent timeline', () => {
  const steps: AgentTimeline = { session_id: 'child', items: [
    { at: T0, kind: 'started', text: 'Verify the contract' },
    { at: T0 + 12_000, kind: 'note', text: 'The schema has the new fields.' },
    { at: T0 + 40_000, kind: 'message', text: 'Asked: keep the old API?' },
    { at: T0 + 61_000, kind: 'finished', status: 'completed', outcome: 'passed', text: 'All 40 tests pass' },
  ] };

  it('reads the steps of the selected agent with their times, and opens its session', async () => {
    const timeline = vi.fn(async () => steps);
    const onOpen = vi.fn();
    render(<TurnAgents {...props} timeline={timeline} onOpen={onOpen} agents={[
      { ...child, role: 'verify', status: 'completed', outcome: 'passed', finished_at: T0 + 61_000 },
    ]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /passed/ })); });
    expect(timeline).toHaveBeenCalledWith('child');
    const sheet = screen.getByRole('dialog', { name: '#1' });
    const items = within(sheet).getAllByRole('listitem').map((item) => item.textContent);
    expect(items).toEqual([
      `${clockTime(T0)}StartedVerify the contract`,
      `${clockTime(T0 + 12_000)}NoteThe schema has the new fields.`,
      `${clockTime(T0 + 40_000)}MessageAsked: keep the old API?`,
      `${clockTime(T0 + 61_000)}Finishedpassed · All 40 tests pass`,
    ]);
    expect(clockTime(T0)).toBe('14:03:07');
    expect(sheet).toHaveTextContent('verify1m 01spassed');
    expect(sheet.querySelectorAll('svg')).toHaveLength(1);
    expect(within(sheet).queryByRole('button', { name: 'Stop agent' })).not.toBeInTheDocument();
    fireEvent.click(within(sheet).getByRole('button', { name: 'Open session' }));
    expect(onOpen).toHaveBeenCalledWith('child');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  it('says when there are no steps, and shows a read error', async () => {
    const timeline = vi.fn().mockResolvedValueOnce({ session_id: 'child', items: [] }).mockRejectedValue(new Error('Not found'));
    const { rerender } = render(<TurnAgents {...props} timeline={timeline} agents={[child]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    expect(screen.getByText('No steps yet.')).toBeInTheDocument();
    // A step of the agent reads the timeline again; a clock tick does not.
    await act(async () => { rerender(<TurnAgents {...props} timeline={timeline} agents={[{ ...child, iterations_used: 3 }]} />); });
    expect(screen.getByRole('alert')).toHaveTextContent('Not found');
    expect(timeline).toHaveBeenCalledTimes(2);
  });

  it('confirms a stop of a working agent', async () => {
    const onCancel = vi.fn();
    render(<TurnAgents {...props} onCancel={onCancel} agents={[child]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    expect(onCancel).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' }));
    expect(onCancel).toHaveBeenCalledWith('child');
  });
});

describe('team line', () => {
  it('counts the agents by role in contract order and by status, the work first', () => {
    const { roles, statuses } = teamCounts([
      { ...child, role: 'verify' },
      { ...child, session_id: 'b', role: 'research', status: 'completed' },
      { ...child, session_id: 'c', role: 'research', status: 'cancelled' },
      { ...child, session_id: 'd' },
      { ...child, session_id: 'e', role: 'audit', status: 'queued' },
    ]);
    expect(roles.map(({ count, text }) => `${count} ${text}`)).toEqual(['2 research', '1 verify', '1 audit', '1 agent']);
    expect(statuses.map(({ count, text }) => `${count} ${text}`)).toEqual(['2 running', '1 queued', '1 done', '1 cancelled']);
    expect(roles[0].ink).toBe('text-role-research');
    expect(statuses[0].ink).toBe('text-accent-ink');
    expect(teamCounts([])).toEqual({ roles: [], statuses: [] });
  });

  it('frames the table of a turn under its team line, with the column headings', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, role: 'research' },
      { ...child, session_id: 'v', role: 'verify', status: 'completed', outcome: 'passed' },
    ]} />);
    const team = screen.getByRole('region', { name: 'Subagents' });
    expect(team).toHaveClass('border');
    expect(team).toHaveAttribute('data-transcript-part');
    expect(team.querySelector('[data-team-line]')).toHaveTextContent('team1 research1 verify1 running1 passed');
    expect(team.querySelector('[aria-hidden]')).toHaveTextContent('roletitleupdatetimestatus');
  });
});

describe('teams', () => {
  it('groups agents by the turn that started them, in start order', () => {
    const groups = agentsByTurn([
      child,
      { ...child, session_id: 'later', team_id: 'turn-2' },
      { ...child, session_id: 'second', team_id: 'turn-1' },
    ]);
    expect(groups.get('turn-1')?.map((agent) => agent.session_id)).toEqual(['child', 'second']);
    expect(groups.get('turn-2')?.map((agent) => agent.session_id)).toEqual(['later']);
  });

  it('docks the team of the running turn and each team with a live agent, of this leader only', () => {
    const done = { ...child, status: 'completed' as const };
    const agents = [
      { ...done, session_id: 'now', team_id: 'turn-3' },
      { ...done, session_id: 'old', team_id: 'turn-1' },
      { ...child, session_id: 'still', team_id: 'turn-2' },
      { ...child, session_id: 'grandchild', team_id: 'child-turn', parent_id: 'still' },
    ];
    expect(dockTeams(agents, 'leader', 'turn-3').map((team) => team[0].team_id)).toEqual(['turn-3', 'turn-2']);
    expect(dockTeams(agents, 'leader').map((team) => team[0].team_id)).toEqual(['turn-2']);
    expect(dockTeams([], 'leader', 'turn-3')).toEqual([]);
  });
});

function Team({ client, sid, running = false }: { client: GatewayClient; sid: string; running?: boolean }) {
  const team = useAgentTeam(client, sid, running);
  return <TurnAgents agents={team.agents} pending={team.pending} error={team.error} onOpen={vi.fn()} onCancel={team.cancel} timeline={team.timeline} />;
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
    expect(screen.getByRole('button', { name: /^agent#1/ })).toBeInTheDocument();
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
    expect(screen.queryByRole('button', { name: /^agent#1/ })).not.toBeInTheDocument();
  });

  it('keeps a stop failure visible across background reads until a stop succeeds', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([child]);
    const cancelAgent = vi.fn().mockRejectedValue(new Error('Stop failed. Try again.'));
    render(<Team client={{ agents, cancelAgent, agentTimeline: vi.fn(empty) } as unknown as GatewayClient} sid="leader" />);
    await act(async () => {});
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
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
