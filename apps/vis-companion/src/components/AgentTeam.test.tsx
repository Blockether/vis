// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from '../lib/gateway';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { AgentTimeline, SseEvent, Subagent } from '../lib/types';
import {
  TurnAgents,
  agentLabel,
  agentOutcome,
  agentStatus,
  agentTime,
  agentBars,
  agentUpdate,
  agentsByTurn,
  leaderLane,
  leaderParts,
  axisMarks,
  clockTime,
  dockTeams,
  formatAgentTime,
  teamCounts,
  teamTime,
  timelineRows,
  timelineStep,
  useAgentTeam,
} from './AgentTeam';

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

afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); vi.unstubAllGlobals(); });

describe('agent rows', () => {
  // A subagent asks only its leader. Its question or request is no status for a person.
  // Four words only: a passed verdict is done; a failed verdict, a cancel and a spent budget are failed.
  it('reads every status as one of four words, and a question to the leader as running', () => {
    const words = [
      { status: 'queued' }, { status: 'running' }, { question: 'Which branch?' }, { budget_request: { iterations: 8 } },
      { files_request: { reason: 'Docs' } }, { pending_input: true }, { status: 'completed' },
      { status: 'completed', outcome: 'passed' }, { status: 'completed', outcome: 'failed' }, { status: 'failed' },
      { status: 'cancelled' }, { status: 'budget_limited' },
    ].map((change) => agentStatus({ ...child, ...change } as Subagent).text);
    expect(words).toEqual([
      'queued', 'running', 'running', 'running', 'running', 'running', 'done',
      'done', 'failed', 'failed', 'failed', 'failed',
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

  // The bars of a wide screen show which agents worked at the same time, and for how long.
  it('puts each task on the team time: an end to its end, a run to now, a queue from its spawn to the end', () => {
    const team = [
      { ...child, session_id: 'done', status: 'completed', finished_at: T0 + 30_000 },
      { ...child, session_id: 'run', created_at: T0 + 30_000, resumed_at: T0 + 60_000 },
      { ...child, session_id: 'wait', status: 'queued', created_at: T0 + 90_000 },
      { ...child, session_id: 'lost', status: 'failed', created_at: T0 + 10_000 },
    ] as Subagent[];
    expect(Object.fromEntries(agentBars(team, T0 + 120_000))).toEqual({
      done: { start: 0, end: 0.25, isWaiting: false },
      run: { start: 0.5, end: 1, isWaiting: false },
      wait: { start: 0.75, end: 1, isWaiting: true },
    });
    expect(agentBars([{ ...child, created_at: undefined }], T0).size).toBe(0);
  });

  // User report: two agents resumed two hours after their spawn stood as slivers at the right end of a 2h axis.
  it('starts the chart at the current tasks, so a resume does not squeeze them to the end', () => {
    const later = T0 + 7_200_000;
    const team = [
      { ...child, session_id: 'done', status: 'completed', resumed_at: later, finished_at: later + 40_000 },
      { ...child, session_id: 'run', resumed_at: later + 40_000 },
    ] as Subagent[];
    expect(Object.fromEntries(agentBars(team, later + 80_000))).toEqual({
      done: { start: 0, end: 0.5, isWaiting: false },
      run: { start: 0.5, end: 1, isWaiting: false },
    });
    // The leader lane widens the chart to the start of its turn.
    const leader = { start: later - 80_000, waits: [] };
    expect(agentBars(team, later + 80_000, leader).get('done')).toEqual({ start: 0.5, end: 0.75, isWaiting: false });
  });

  // The leader lane shows where the leader waited for its agents: in the blocks that call council.run or council.wait.
  it('reads the waits of a leader from its council.run and council.wait blocks', () => {
    const form = (code: string, started_at_ms?: number, duration_ms?: number) => ({ code, started_at_ms, duration_ms });
    const steps = [{ forms: [
      form('print(grep({"query": ["x"]}))', T0 + 1_000, 500),
      form('r = await council.run([task])', T0 + 2_000, 3_000),
      form('council.wait(session_ids=ids)', T0 + 9_000),
      form('await council.wait()'),
    ] }];
    expect(leaderLane(T0, undefined, steps)).toEqual({
      start: T0, end: undefined, waits: [{ start: T0 + 2_000, end: T0 + 5_000 }, { start: T0 + 9_000 }],
    });
    // A finished turn drops a wait that never ended; a turn without a start has no lane.
    expect(leaderLane(T0, T0 + 20_000, steps)?.waits).toEqual([{ start: T0 + 2_000, end: T0 + 5_000 }]);
    expect(leaderLane(undefined, undefined, steps)).toBeUndefined();
  });

  it('splits the leader lane into work and waits, cut to the lane, an overlap counted once', () => {
    const leader = { start: T0, end: T0 + 10_000, waits: [
      { start: T0 + 2_000, end: T0 + 4_000 }, { start: T0 + 3_000, end: T0 + 5_000 }, { start: T0 + 8_000 },
    ] };
    expect(leaderParts(leader, T0 + 60_000).map((part) => [part.start - T0, part.end - T0, part.isWaiting])).toEqual([
      [0, 2_000, false], [2_000, 4_000, true], [4_000, 5_000, true], [5_000, 8_000, false], [8_000, 10_000, true],
    ]);
  });

  it('marks the time axis on a round step, with at most five marks', () => {
    expect(axisMarks(262_000)).toEqual([0, 60_000, 120_000, 180_000, 240_000]);
    expect(axisMarks(40_000)).toEqual([0, 10_000, 20_000, 30_000, 40_000]);
    expect(axisMarks(50_000)).toEqual([0, 15_000, 30_000, 45_000]);
  });

  // On a wide screen the note of an agent stands beside its title, and the timeline replaces the notes.
  it('shows the notes on a wide screen, swaps them for the timeline, and opens the sheet from a row', async () => {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
    }));
    render(<TurnAgents {...props} agents={[
      { ...child, note: 'Reading the routes' },
      { ...child, session_id: 'done', status: 'completed', finished_at: T0 + 9_000 },
    ]} />);
    expect(screen.getByText('Reading the routes')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Open timeline' }));
    // User report: the timeline lost the newest notes. The legend keeps them.
    expect(within(screen.getByRole('list', { name: 'Legend' })).getByText('Reading the routes')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Close timeline' })).toHaveAttribute('aria-pressed', 'true');
    const [, done] = screen.getAllByRole('button').filter((row) => row.hasAttribute('data-agent-row'));
    await act(async () => { fireEvent.click(done); });
    expect(screen.getByRole('dialog', { name: '#2' })).toBeInTheDocument();
  });

  // User request: the chart names each lane by a short key, L for the leader and A1, A2 for the agents,
  // and the legend under it gives each key its role, title and newest note.
  it('keys the lanes of the chart, with the leader first, and explains each key with its note in the legend', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
    }));
    const leader = { start: T0 - 10_000, end: T0 + 20_000, waits: [{ start: T0, end: T0 + 12_000 }] };
    render(<TurnAgents {...props} leader={leader} agents={[
      { ...child, title: 'Companion review', role: 'verify', status: 'completed', finished_at: T0 + 9_000, result: 'All **12** keys pass' },
      { ...child, session_id: 'tui', title: 'TUI review', role: 'audit', status: 'completed', finished_at: T0 + 15_000 },
    ]} />);
    fireEvent.click(screen.getByRole('button', { name: 'Open timeline' }));
    const lanes = within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem').map((lane) => lane.textContent);
    expect(lanes).toEqual(['L leader30sdone', 'A1 Companion review9sdone', 'A2 TUI review15sdone']);
    const legend = within(screen.getByRole('list', { name: 'Legend' })).getAllByRole('listitem').map((row) => row.textContent);
    expect(legend).toEqual(['Lleaderwaited 12s of 30s', 'A1verifyCompanion reviewAll 12 keys pass', 'A2auditTUI review']);
  });

  it('shows what a working agent does now: its newest note, its activity or its task, then its result', () => {
    const asks = { question: 'Keep the old API?', budget_request: { iterations: 8, reason: 'Slow tests' } };
    expect(agentUpdate({ ...child, ...asks, note: 'The bug is in tokenize', activity: 'Read ~/a.clj' })).toBe('The bug is in tokenize');
    expect(agentUpdate({ ...child, activity: 'Read ~/a.clj' })).toBe('Read ~/a.clj');
    expect(agentUpdate({ ...child, status: 'queued', task: '  Check the links.\nReport the dead ones.' })).toBe('Check the links.');
    expect(agentUpdate({ ...child, status: 'completed', note: 'Old note', result: 'All pass' })).toBe('All pass');
    // Without a result, the update gives the end that `failed` hides.
    expect(agentUpdate({ ...child, status: 'cancelled', note: 'Old note' })).toBe('cancelled');
    expect(agentUpdate({ ...child, status: 'budget_limited' })).toBe('out of budget');
    expect(agentUpdate({ ...child, status: 'budget_limited', result: 'Read 41 of 60 files' })).toBe('Read 41 of 60 files');
    expect(agentUpdate({ ...child, status: 'failed' })).toBeUndefined();
    render(<TurnAgents {...props} agents={[{ ...child, ...asks, note: 'The bug is in tokenize' }]} />);
    expect(rowTexts()[0]).toMatch(/^agent#1The bug is in tokenize/);
    expect(screen.queryByText(/old API|Slow tests/)).not.toBeInTheDocument();
  });

  it('keeps the spawn order whatever the status', () => {
    render(<TurnAgents {...props} agents={[
      { ...child, session_id: 'late', created_at: T0 + 5, pending_input: true },
      { ...child, session_id: 'early', created_at: T0 + 1, status: 'completed' },
    ]} />);
    expect(rowTexts()).toEqual([expect.stringMatching(/^agent#1done$/), expect.stringMatching(/^agent#2Verify the contract.*running\d+[hms]/)]);
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
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /done/ })); });
    expect(timeline).toHaveBeenCalledWith('child');
    const sheet = screen.getByRole('dialog', { name: '#1' });
    const items = within(sheet).getAllByRole('listitem').map((item) => item.textContent);
    expect(items).toEqual([
      `${clockTime(T0)}StartedVerify the contract`,
      `${clockTime(T0 + 12_000)}The schema has the new fields.`,
      `${clockTime(T0 + 40_000)}Asked: keep the old API?`,
      `${clockTime(T0 + 61_000)}doneAll 40 tests pass`,
    ]);
    expect(clockTime(T0)).toBe('14:03:07');
    expect(within(sheet).queryByRole('button', { name: 'Stop agent' })).not.toBeInTheDocument();
    fireEvent.click(within(sheet).getByRole('button', { name: 'Open session' }));
    expect(onOpen).toHaveBeenCalledWith('child');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  it('gives the reason of a failed end without a text', async () => {
    const timeline = vi.fn(async (): Promise<AgentTimeline> => ({ session_id: 'child', items: [
      { at: T0 + 9_000, kind: 'finished', status: 'budget_limited' },
    ] }));
    render(<TurnAgents {...props} timeline={timeline} agents={[{ ...child, status: 'budget_limited', finished_at: T0 + 9_000 }]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /failed/ })); });
    const sheet = screen.getByRole('dialog', { name: '#1' });
    expect(sheet.querySelector('[data-agent-outcome]')).toHaveTextContent('out of budget');
    expect(within(sheet).getAllByRole('listitem').map((item) => item.textContent)).toEqual([
      `${clockTime(T0 + 9_000)}failed`,
    ]);
  });

  // User report: the sheet showed the Markdown of a task as raw text, with `##`, `-` and backticks.
  it('shows the task as Markdown, not as its raw markers', async () => {
    const task = 'Make the state known.\n\n## User report\n- The dock shows late.\n- Read `useAgentTeam` first.';
    render(<TurnAgents {...props} agents={[{ ...child, task }]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    const part = within(screen.getByRole('dialog', { name: '#1' })).getByRole('region', { name: 'Task' });
    expect(within(part).getByRole('heading', { name: 'User report' })).toBeInTheDocument();
    expect(within(part).getAllByRole('listitem').map((item) => item.textContent)).toEqual([
      'The dock shows late.',
      'Read useAgentTeam first.',
    ]);
    expect(part.textContent).not.toMatch(/##|`/);
  });

  // User report 2026-11-02: timeline notes and messages showed raw Markdown, such as `##` and `**`.
  it('shows the notes and messages of the timeline as Markdown', async () => {
    const timeline = vi.fn(async (): Promise<AgentTimeline> => ({ session_id: 'child', items: [
      { at: T0, kind: 'message', text: '## Python SDK `timeline`: done\n**Files changed:**\n- `_contracts.py`: lane' },
      { at: T0 + 5_000, kind: 'note', text: 'Read **all** tests' },
    ] }));
    render(<TurnAgents {...props} timeline={timeline} agents={[child]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    const steps = within(screen.getByRole('dialog', { name: '#1' })).getByRole('list', { name: 'Timeline' });
    expect(within(steps).getByRole('heading', { name: 'Python SDK timeline: done' })).toBeInTheDocument();
    expect(within(steps).getByText('Files changed:').tagName).toBe('STRONG');
    expect(within(steps).getByText('all').tagName).toBe('STRONG');
    expect(steps.textContent).not.toMatch(/##|\*\*|`/);
  });

  // User report 2026-11-02: a row showed the Markdown markers of a note.
  it('shows a Markdown note in its row without the markers', () => {
    expect(agentUpdate({ ...child, note: '## Done\n**Fixing** the `parser`' })).toBe('Done Fixing the parser');
  });

  // User report: the sheet showed the result at its end and the end step said it again.
  it('shows the outcome above the steps, and the end step does not repeat it', () => {
    const end = { at: T0 + 61_000, kind: 'finished' as const, status: 'completed' as const, outcome: 'passed' as const, text: 'All 40 tests pass' };
    const done = { ...child, status: 'completed' as const, outcome: 'passed' as const, result: 'All 40 tests pass' };
    expect(agentOutcome(done)).toBe('All 40 tests pass');
    expect(timelineStep(end, done)).toEqual({ word: { text: 'done', ink: 'text-ok-ink', isStatus: true }, text: undefined });
    expect(timelineStep(end, { ...done, result: 'Other words' }).text).toBe('All 40 tests pass');
    // While the agent works, it has no outcome: its current step is in the timeline.
    expect(agentOutcome({ ...child, note: 'Reading the schema', activity: ' Search files agents ' })).toBeUndefined();
    expect(timelineStep({ at: T0, kind: 'note', text: 'Plain prose' }, child)).toEqual({ word: undefined, text: 'Plain prose' });
  });

  // User report 2026-11-02: the current Activity row said NOW. It shows the clock time, like the other rows.
  it('gives the current Activity row the clock time, not NOW', async () => {
    vi.useFakeTimers({ now: T0 + 30_000, shouldAdvanceTime: true });
    const timeline = vi.fn(async (): Promise<AgentTimeline> => ({ session_id: 'child', items: [{ at: T0, kind: 'started' }] }));
    render(<TurnAgents {...props} timeline={timeline} agents={[{ ...child, activity: 'Patched AgentDock.tsx' }]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    const steps = within(screen.getByRole('dialog', { name: '#1' })).getAllByRole('listitem');
    expect(steps.at(-1)?.textContent).toBe(`${clockTime(T0 + 30_000)}Patched AgentDock.tsx`);
  });

  // User report: the current action of a working agent stood above its task, apart from its timeline.
  it('marks the current step of a working agent as the last row of its timeline', () => {
    const items = steps.items.slice(0, 2);
    const current = (rows: ReturnType<typeof timelineRows>) => rows.filter((row) => row.isCurrent).map((row) => [row.at, row.text]);
    expect(current(timelineRows(items, { ...child, activity: ' Patched AgentDock.tsx ' }))).toEqual([[undefined, 'Patched AgentDock.tsx']]);
    expect(timelineRows(items, { ...child, activity: 'Patched AgentDock.tsx' })).toHaveLength(3);
    expect(current(timelineRows(items, child))).toEqual([[T0 + 12_000, 'The schema has the new fields.']]);
    // An Activity line that says the last step again marks that step, not a second row.
    expect(timelineRows(items, { ...child, activity: 'The schema has the new fields.' }).map((row) => row.isCurrent)).toEqual([false, true]);
    expect(current(timelineRows(steps.items, { ...child, status: 'completed', activity: 'Patched AgentDock.tsx' }))).toEqual([]);
  });

  it('says when there are no steps, and shows a read error', async () => {
    const timeline = vi.fn().mockResolvedValueOnce({ session_id: 'child', items: [] }).mockRejectedValue(new Error('Not found'));
    const { rerender } = render(<TurnAgents {...props} timeline={timeline} agents={[child]} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
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
    // A blur disarms the stop, so a later stray press does not stop the agent.
    fireEvent.blur(screen.getByRole('button', { name: 'Confirm stop' }));
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    expect(onCancel).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' }));
    expect(onCancel).toHaveBeenCalledWith('child');
  });
});

describe('team line', () => {
  // The team line shows no role counts: the table names each role.
  it('counts the agents by status, the work first', () => {
    const statuses = teamCounts([
      { ...child, role: 'verify' },
      { ...child, session_id: 'b', role: 'research', status: 'completed' },
      { ...child, session_id: 'c', role: 'research', status: 'cancelled' },
      { ...child, session_id: 'd' },
      { ...child, session_id: 'e', role: 'audit', status: 'queued' },
    ]);
    expect(statuses.map(({ count, text }) => `${count} ${text}`)).toEqual(['2 running', '1 queued', '1 done', '1 failed']);
    expect(teamCounts([])).toEqual([]);
  });

  it('times the team from its first spawn to now while it works, else to its last end', () => {
    const ended = { ...child, status: 'completed' as const };
    expect(teamTime([], T0)).toBeUndefined();
    expect(teamTime([{ ...child, created_at: T0 + 5_000 }, { ...child, session_id: 'b', status: 'queued' }], T0 + 90_000)).toBe(90_000);
    expect(teamTime([ended, { ...ended, session_id: 'b', created_at: T0 + 1, finished_at: T0 + 40_000 }], T0 + 999_000)).toBe(40_000);
    expect(teamTime([ended], T0 + 999_000)).toBeUndefined();
  });

  it('ticks the team time on the team line while an agent runs', () => {
    vi.useFakeTimers({ now: T0 + 200_000 });
    render(<TurnAgents {...props} agents={[{ ...child, role: 'research' }]} />);
    const line = () => screen.getByRole('region', { name: 'Subagents' }).querySelector('[data-team-line]');
    expect(line()).toHaveTextContent(/^team3m 20s1 running$/);
    act(() => { vi.advanceTimersByTime(2000); });
    expect(line()).toHaveTextContent(/^team3m 22s1 running$/);
    vi.useRealTimers();
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

function fakeHub() {
  const listeners = new Set<(event: SseEvent) => void>();
  const connection = new Set<(connected: boolean) => void>();
  const hub = {
    subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
    subscribeConnection: (listener: (connected: boolean) => void) => {
      connection.add(listener);
      listener(true);
      return () => connection.delete(listener);
    },
  } as unknown as SessionSubscriptionHub;
  return {
    hub,
    emit: (event: SseEvent) => listeners.forEach((listener) => listener(event)),
    connect: (connected: boolean) => connection.forEach((listener) => listener(connected)),
  };
}

function TeamView({ client, sid, total, legacy = null }: { client: GatewayClient; sid: string; total?: number | null; legacy?: boolean | null }) {
  const team = useAgentTeam(client, sid, total, legacy);
  return <TurnAgents agents={team.agents} pending={team.pending} error={team.error} onOpen={vi.fn()} onCancel={team.cancel} timeline={team.timeline} />;
}

function Team({ hub = null, ...props }: { client: GatewayClient; sid: string; total?: number | null; legacy?: boolean | null; hub?: SessionSubscriptionHub | null }) {
  return <SessionEventsContext.Provider value={hub}><TeamView {...props} /></SessionEventsContext.Provider>;
}

const fakeClient = (agents: ReturnType<typeof vi.fn>, more: Record<string, unknown> = {}) =>
  ({ agents, cancelAgent: vi.fn(), ...more }) as unknown as GatewayClient;

describe('agent team reader', () => {
  it('reads single-flight, pauses while hidden and aborts on replacement and unmount', async () => {
    const first = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(first.promise).mockResolvedValue([child]);
    const client = fakeClient(agents);
    const { rerender, unmount } = render(<Team client={client} sid="leader" />);
    await act(async () => {});
    const signal = agents.mock.calls[0][1] as AbortSignal;
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(1);
    await act(async () => { first.resolve([child]); });
    expect(screen.getByRole('button', { name: /^agent#1/ })).toBeInTheDocument();
    const hidden = vi.spyOn(document, 'hidden', 'get').mockReturnValue(true);
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(1);
    hidden.mockReturnValue(false);
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(2);
    rerender(<Team client={client} sid="next" />);
    expect(signal.aborted).toBe(true);
    await act(async () => {});
    expect(agents).toHaveBeenLastCalledWith('next', expect.any(AbortSignal));
    const lastSignal = agents.mock.calls.at(-1)![1] as AbortSignal;
    unmount();
    expect(lastSignal.aborted).toBe(true);
  });

  // Requirement (agent team push, protocol 21): the Companion read GET /agents every 3 s
  // while a team worked. The gateway now pushes changed rows, so no timer reads the team.
  it('reads a working team once and never on a timer', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([child, { ...child, session_id: 'second' }]);
    render(<Team client={fakeClient(agents)} sid="leader" total={2} hub={fakeHub().hub} />);
    await act(async () => {});
    await act(async () => { vi.advanceTimersByTime(60_000); });
    expect(agents).toHaveBeenCalledTimes(1);
  });

  // A gateway before protocol 21 sends only counts, so the rows of its team change only on a read.
  it('reads the team of an older gateway every 3 s while it is live, and stops when it ends', async () => {
    vi.useFakeTimers();
    const agents = vi.fn().mockResolvedValue([child]);
    const view = render(<Team client={fakeClient(agents)} sid="leader" total={1} legacy={false} hub={fakeHub().hub} />);
    await act(async () => {});
    const opened = agents.mock.calls.length;
    await act(async () => { await vi.advanceTimersByTimeAsync(9_000); });
    expect(agents).toHaveBeenCalledTimes(opened + 3);
    agents.mockResolvedValue([{ ...child, status: 'completed' }]);
    await act(async () => { await vi.advanceTimersByTimeAsync(3_000); });
    const ended = agents.mock.calls.length;
    await act(async () => { await vi.advanceTimersByTimeAsync(60_000); });
    expect(agents).toHaveBeenCalledTimes(ended);
    view.unmount();
  });

  it('draws the pushed rows without a read, and reads on an event without rows', async () => {
    const { hub, emit } = fakeHub();
    const agents = vi.fn().mockResolvedValue([child]);
    const applyTeamUpdate = vi.fn().mockReturnValueOnce([{ ...child, status: 'completed' }]).mockReturnValue(null);
    render(<Team client={fakeClient(agents, { applyTeamUpdate })} sid="leader" total={1} hub={hub} />);
    await act(async () => {});
    expect(screen.getByText('running')).toBeInTheDocument();
    act(() => emit({ type: 'session.team_updated', team: { active: 0, total: 1 }, agents: [] } as SseEvent));
    expect(screen.getByText('done')).toBeInTheDocument();
    expect(agents).toHaveBeenCalledTimes(1);
    await act(async () => { emit({ type: 'session.team_updated', team: { active: 0, total: 1 } } as SseEvent); });
    expect(agents).toHaveBeenCalledTimes(2);
  });

  // A pushed row that arrives while a read runs is newer than the read's answer.
  it('reads again when an event arrives during a read', async () => {
    const { hub, emit } = fakeHub();
    const first = deferred<Subagent[]>();
    const agents = vi.fn().mockReturnValueOnce(first.promise).mockResolvedValue([{ ...child, status: 'completed' }]);
    const applyTeamUpdate = vi.fn().mockReturnValue([{ ...child, status: 'completed' }]);
    render(<Team client={fakeClient(agents, { applyTeamUpdate })} sid="leader" total={1} hub={hub} />);
    await act(async () => {});
    act(() => emit({ type: 'session.team_updated', team: { active: 0, total: 1 }, agents: [] } as SseEvent));
    await act(async () => { first.resolve([child]); });
    expect(agents).toHaveBeenCalledTimes(2);
    expect(screen.getByText('done')).toBeInTheDocument();
  });

  // The stream drops events while it is down, so the team reads again after a reconnect.
  it('reads again after a stream reconnect, not on the first connect', async () => {
    const { hub, connect } = fakeHub();
    const agents = vi.fn().mockResolvedValue([child]);
    render(<Team client={fakeClient(agents)} sid="leader" total={1} hub={hub} />);
    await act(async () => {});
    expect(agents).toHaveBeenCalledTimes(1);
    act(() => connect(false));
    await act(async () => { connect(true); });
    expect(agents).toHaveBeenCalledTimes(2);
  });

  it('does not read a session without children, but reads one with cached rows', async () => {
    const agents = vi.fn().mockResolvedValue([]);
    const cachedAgents = vi.fn().mockReturnValue(null);
    const client = fakeClient(agents, { cachedAgents });
    const { unmount } = render(<Team client={client} sid="leader" total={0} />);
    await act(async () => {});
    expect(agents).not.toHaveBeenCalled();
    unmount();
    cachedAgents.mockReturnValue([child]);
    render(<Team client={client} sid="leader" total={0} />);
    await act(async () => {});
    expect(agents).toHaveBeenCalledTimes(1);
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
    const agents = vi.fn().mockResolvedValue([child]);
    const cancelAgent = vi.fn().mockRejectedValue(new Error('Stop failed. Try again.'));
    render(<Team client={{ agents, cancelAgent, agentTimeline: vi.fn(empty) } as unknown as GatewayClient} sid="leader" />);
    await act(async () => {});
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: /running/ })); });
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(cancelAgent).toHaveBeenCalledWith('leader', 'child');
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')); });
    expect(agents).toHaveBeenCalledTimes(3);
    expect(screen.getByRole('alert')).toHaveTextContent('Stop failed. Try again.');
    cancelAgent.mockResolvedValue(undefined);
    fireEvent.click(screen.getByRole('button', { name: 'Stop agent' }));
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Confirm stop' })); });
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
  });

  // User report 2026-11-01: the running agent team showed only after 3-4 seconds, and a
  // session switch lost it until a new read finished.
  it('shows the last team of a session at once after a switch back, before the new read', async () => {
    const reads = new Map<string, (rows: Subagent[]) => void>();
    vi.stubGlobal('fetch', vi.fn((input: RequestInfo | URL) => new Promise<Response>((done) => {
      reads.set(new URL(String(input)).pathname.split('/')[3], (rows) =>
        done(new Response(JSON.stringify(rows), { headers: { 'Content-Type': 'application/json' } })));
    })));
    const client = new GatewayClient({ url: 'http://team-switch.example.com' });
    const { rerender } = render(<Team client={client} sid="switch-leader" />);
    await act(async () => {});
    await act(async () => { reads.get('switch-leader')!([child]); });
    expect(screen.getByRole('button', { name: /^agent#1/ })).toBeInTheDocument();
    rerender(<Team client={client} sid="switch-other" />);
    await act(async () => {});
    await act(async () => { reads.get('switch-other')!([]); });
    expect(screen.queryByRole('button', { name: /^agent#1/ })).not.toBeInTheDocument();
    rerender(<Team client={client} sid="switch-leader" />);
    expect(screen.getByRole('button', { name: /^agent#1/ })).toBeInTheDocument();
    vi.unstubAllGlobals();
  });
});
