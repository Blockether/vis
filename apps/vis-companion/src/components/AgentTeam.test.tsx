// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
import teamFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/team-updated.json';
import { SessionEventsContext } from '../lib/session-events';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent, Subagent, TranscriptTurn } from '../lib/types';
import { AgentDock } from './AgentDock';
import {
  TurnAgents,
  agentLabel,
  agentStatus,
  agentTime,
  agentBars,
  agentUpdate,
  agentsByTurn,
  leaderLane,
  leaderParts,
  storedLeaderLane,
  axisMarks,
  dockTeams,
  formatAgentTime,
  teamCounts,
  teamTime,
  useAgentTeam,
} from './AgentTeam';

const T0 = new Date(2026, 4, 3, 14, 3, 7).getTime();
const child: Subagent = { session_id: 'child', parent_id: 'leader', leader_id: 'leader', team_id: 'turn-1',
  task: 'Verify the contract', status: 'running', iteration_budget: 4, iterations_used: 2,
  provider: 'p', model: 'small', routing_locked: true, usage: { cost_usd: 0.0123 }, created_at: T0 };
const props = { onOpen: vi.fn() };

const rowTexts = () => screen.getAllByRole('button').filter((row) => row.hasAttribute('data-agent-row')).map((row) => row.textContent);

afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); vi.unstubAllGlobals(); });

describe('agent rows', () => {
  // A subagent asks only its leader. Its question or request is no status for a person.
  // A passed verdict is done; a failed verdict, a cancel and a spent budget are failed.
  // Council run dependencies: a task that waits for earlier tasks is waiting, not failed.
  it('reads every status as one word, and a question to the leader as running', () => {
    const words = [
      { status: 'queued' }, { status: 'running' }, { question: 'Which branch?' }, { budget_request: { iterations: 8 } },
      { files_request: { reason: 'Docs' } }, { pending_input: true }, { status: 'completed' },
      { status: 'completed', outcome: 'passed' }, { status: 'completed', outcome: 'failed' }, { status: 'failed' },
      { status: 'cancelled' }, { status: 'budget_limited' }, { status: 'waiting' }, { status: 'blocked' },
    ].map((change) => agentStatus({ ...child, ...change } as Subagent).text);
    expect(words).toEqual([
      'queued', 'running', 'running', 'running', 'running', 'running', 'done',
      'done', 'failed', 'failed', 'failed', 'failed', 'waiting', 'blocked',
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
      done: [{ start: 0, end: 0.25, isWaiting: false }],
      run: [{ start: 0.5, end: 1, isWaiting: false }],
      wait: [{ start: 0.75, end: 1, isWaiting: true }],
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
      done: [{ start: 0, end: 0.5, isWaiting: false }],
      run: [{ start: 0.5, end: 1, isWaiting: false }],
    });
    // The leader lane widens the chart to the start of its turn.
    const leader = { start: later - 80_000, waits: [] };
    expect(agentBars(team, later + 80_000, leader).get('done')).toEqual([{ start: 0.5, end: 0.75, isWaiting: false }]);
  });

  // User report: an agent failed on a provider error and resumed; the chart showed only its short
  // resumed task, as if the hour before it never happened.
  it('keeps a failed earlier run on the chart in the failed ink, with the time down to the resume', () => {
    const resumed = teamFixture.merged.find((row) => row.session_id === 'child-c') as Subagent;
    const team = [{ ...resumed, status: 'completed', finished_at: 8_000 }] as Subagent[];
    expect(agentBars(team, 8_000).get('child-c')).toEqual([
      { start: 0, end: 0.4, isWaiting: false, status: 'failed' },
      { start: 0.4, end: 0.6, isWaiting: true, status: 'failed' },
      { start: 0.6, end: 1, isWaiting: false },
    ]);
    // A completed earlier run is a done bar with no line after it.
    const done = [{ ...team[0], runs: [{ start: 3_000, end: 5_000, status: 'completed' }] }] as Subagent[];
    expect(agentBars(done, 8_000).get('child-c')).toEqual([
      { start: 0, end: 0.4, isWaiting: false, status: 'done' },
      { start: 0.6, end: 1, isWaiting: false },
    ]);
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
      start: T0, end: undefined, waits: [{ start: T0 + 2_000, end: T0 + 5_000 }, { start: T0 + 9_000 }], note: undefined,
    });
    // A finished turn drops a wait that never ended; a turn without a start has no lane.
    expect(leaderLane(T0, T0 + 20_000, steps)?.waits).toEqual([{ start: T0 + 2_000, end: T0 + 5_000 }]);
    expect(leaderLane(undefined, undefined, steps)).toBeUndefined();
    // A turn without a step list still has its lane.
    expect(leaderLane(T0, undefined, null)).toEqual({ start: T0, end: undefined, waits: [], note: undefined });
  });

  // User request: the leader row says what the leader does now, like the note of an agent.
  it('notes the newest prose of a working leader, and its answer once its turn ended', () => {
    const steps = [
      { assistant_prose: 'I start **two** reviewers.' },
      { assistant_prose: 'Both run; I read the dock code meanwhile.', answer: 'Both reviews pass.' },
      { assistant_prose: '  ' },
    ];
    expect(leaderLane(T0, undefined, steps)?.note).toBe('Both run; I read the dock code meanwhile.');
    expect(leaderLane(T0, T0 + 9_000, steps)?.note).toBe('Both reviews pass.');
    expect(leaderLane(T0, undefined, [steps[0]])?.note).toBe('I start two reviewers.');
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
  it('shows the notes on a wide screen, swaps them for the timeline, and opens the session from a row', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
    }));
    const onOpen = vi.fn();
    render(<TurnAgents {...props} onOpen={onOpen} agents={[
      { ...child, note: 'Reading the routes' },
      { ...child, session_id: 'done', status: 'completed', finished_at: T0 + 9_000 },
    ]} />);
    expect(screen.getByText('Reading the routes')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Timeline' }));
    // User report: the timeline lost the newest notes. The legend keeps them.
    expect(within(screen.getByRole('list', { name: 'Legend' })).getByText('Reading the routes')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Timeline' })).toHaveAttribute('aria-pressed', 'true');
    const [, done] = screen.getAllByRole('button').filter((row) => row.hasAttribute('data-agent-row'));
    fireEvent.click(done);
    expect(onOpen).toHaveBeenCalledWith('done');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  // User report: in the dock, the titles of an AUDIT team and an IMPLEMENT team did not line up.
  it('gives the ROLE column one width in every team', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
    }));
    render(<>
      <TurnAgents {...props} placement="dock" agents={[{ ...child, session_id: 'a', role: 'verify' }]} />
      <TurnAgents {...props} placement="dock" agents={[{ ...child, session_id: 'i', team_id: 'turn-2', role: 'implement' }]} />
    </>);
    const widths = ['a', 'i'].map((sid) =>
      (document.querySelector(`[data-agent-row="${sid}"]`)?.closest('[style]') as HTMLElement).style.getPropertyValue('--role'));
    expect(widths[0]).not.toBe('');
    expect(widths[0]).toBe(widths[1]);
  });

  // User request: the chart names each lane by a short key, L for the leader and A1, A2 for the agents,
  // and the legend under it gives each key its role, title and newest note.
  it('keys the lanes of the chart, with the leader first, and explains each key with its note in the legend', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
    }));
    const leader = { start: T0 - 10_000, end: T0 + 20_000, waits: [{ start: T0, end: T0 + 12_000 }], note: 'Both reviews pass.' };
    render(<TurnAgents {...props} leader={leader} agents={[
      { ...child, title: 'Companion review', role: 'verify', status: 'completed', finished_at: T0 + 9_000, result: 'All **12** keys pass' },
      { ...child, session_id: 'tui', title: 'TUI review', role: 'verify', status: 'completed', finished_at: T0 + 15_000 },
    ]} />);
    fireEvent.click(screen.getByRole('button', { name: 'Timeline' }));
    const lanes = within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem').map((lane) => lane.textContent);
    expect(lanes).toEqual(['L leader30s (waited 12s)done', 'A1 Companion review9sdone', 'A2 TUI review15sdone']);
    const legend = within(screen.getByRole('list', { name: 'Legend' })).getAllByRole('listitem').map((row) => row.textContent);
    expect(legend).toEqual(['Lleader(waited 12s)Both reviews pass.', 'A1verifyCompanion reviewAll 12 keys pass', 'A2verifyTUI review']);
  });

  // User report 2026-11-10: a team finished on a phone, and the phone had no control to open its timeline.
  it('opens the timeline of a team on a phone, in the transcript and in the dock', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({ matches: false, media: query, addEventListener: () => {}, removeEventListener: () => {} }));
    const agents = [
      { ...child, title: 'Companion review', status: 'completed' as const, finished_at: T0 + 9_000 },
      { ...child, session_id: 'tui', title: 'TUI review', status: 'running' as const },
    ];
    const { unmount } = render(<TurnAgents {...props} agents={agents.map((agent) => ({ ...agent, status: 'completed' as const, finished_at: T0 + 9_000 }))} />);
    fireEvent.click(screen.getByRole('button', { name: 'Timeline' }));
    expect(within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem')).toHaveLength(2);
    unmount();
    render(<AgentDock sid="leader" runningTurnId="turn-1" onOpen={vi.fn()} agents={agents} />);
    fireEvent.click(screen.getByRole('button', { name: 'Timeline' }));
    expect(within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem')).toHaveLength(2);
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
    expect(rowTexts()).toEqual([expect.stringMatching(/^agent#1done$/), expect.stringMatching(/^agent#2Verify the contract.*\d+[hms]running$/)]);
  });

  // A form of a subagent opens in the form prompt of the leader session, like any
  // other form. Its row has no control for it.
  it('shows an agent with an open form as running, with no control for its requests', () => {
    const onOpen = vi.fn();
    render(<TurnAgents {...props} onOpen={onOpen} agents={[
      { ...child, pending_input: true },
      { ...child, session_id: 'asks', question: 'Keep the old API?', budget_request: { iterations: 8 } },
    ]} />);
    expect(screen.queryByText('needs you')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^(Answer|Grant|Deny|Later)/ })).not.toBeInTheDocument();
    expect(screen.queryByText('Keep the old API?')).not.toBeInTheDocument();
  });

  // User report 2026-11-03: a row opened a sheet with an Open button before the agent session.
  it('opens the session of an agent from its row, with no sheet between', () => {
    const onOpen = vi.fn();
    render(<TurnAgents {...props} onOpen={onOpen} agents={[child, { ...child, session_id: 'second' }]} />);
    fireEvent.click(screen.getByRole('button', { name: /^agent#2/ }));
    expect(onOpen).toHaveBeenCalledExactlyOnceWith('second');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

});

describe('agent row notes', () => {
  // User report 2026-11-02: a row showed the Markdown markers of a note.
  it('shows a Markdown note in its row without the markers', () => {
    expect(agentUpdate({ ...child, note: '## Done\n**Fixing** the `parser`' })).toBe('Done Fixing the parser');
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
      { ...child, session_id: 'e', role: 'verify', status: 'queued' },
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
    expect(line()).toHaveTextContent(/^team3m 20s1 run/);
    act(() => { vi.advanceTimersByTime(2000); });
    expect(line()).toHaveTextContent(/^team3m 22s1 run/);
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

// User report 2026-11-09: a stored team card showed its agents and no leader. Each team table
// starts with the leader row; it opens nothing, and the team counts stay agent counts.
describe('leader row', () => {
  const wide = (isWide: boolean) => vi.stubGlobal('matchMedia', (query: string) => ({
    matches: isWide && query.includes('min-width: 640px'), media: query, addEventListener: () => {}, removeEventListener: () => {},
  }));
  const turn = (status: string) => ({
    turn_id: 'turn-1', status, created_at: T0 - 10_000, duration_ms: 30_000,
    iterations: [{ forms: [{ code: 'council.run([task])', started_at_ms: T0, duration_ms: 12_000 }], answer: '**Both** reviews pass.' }],
  }) as unknown as TranscriptTurn;
  const team = [
    { ...child, title: 'Companion review', role: 'verify', status: 'completed' as const, finished_at: T0 + 9_000 },
    { ...child, session_id: 'tui', title: 'TUI review', role: 'verify', status: 'completed' as const, finished_at: T0 + 15_000 },
  ];
  const tableRows = () => [...document.querySelectorAll('[data-leader-row], [data-agent-row]')].map((row) => row.textContent);

  it.each([true, false])('puts the leader of a stored turn first, with its wait, answer, time and status (wide: %s)', (isWide) => {
    wide(isWide);
    render(<TurnAgents {...props} agents={team} leader={storedLeaderLane(turn('done'))} />);
    const rows = tableRows();
    expect(rows).toHaveLength(3);
    expect(rows[0]).toMatch(/^leaderBoth reviews pass\.30s \(waited 12s\)done$/);
    expect(rows[1]).toMatch(/^verifyCompanion review/);
    expect(screen.getByRole('region', { name: 'Subagents' }).querySelector('[data-team-line]')).toHaveTextContent(/2 done/);
    expect(document.querySelector('[data-leader-row]')?.closest('button')).toBeNull();
  });

  // Verifier report 2026-11-09: on a real stored turn the leader row said `waited 0s` and showed no
  // answer. A stored form has `src`, no `code` and no start; a stored step has no `answer`, and the
  // answer is the prose of the turn. Fields from `GET /v1/sessions/<id>/transcript`.
  it('reads the waits and the answer of a stored turn in the shape that the gateway sends', () => {
    wide(true);
    const stored = {
      turn_id: '4cc04bcd', status: 'done', created_at: 1794213400187, duration_ms: 12414,
      content: [{ id: 'p1', type: 'prose', markdown: 'Both subagents finished, and each replied with the text it was given:\n\n- **Echo one** (research, `a1`): **one**' }],
      iterations: [
        { created_at: 1794213407814, duration_ms: 3239, forms: [{ duration_ms: 3239, src: 'w = await memo.wake()\nr = await council.run([\n    {"task": "Reply"},\n])' }] },
        { created_at: 1794213410289, duration_ms: 36, assistant_prose: "Both are still running. I'll wait for them to finish.",
          forms: [{ duration_ms: 36, src: 'res = await council.wait(session_ids=ids)' }] },
        { created_at: 1794213412601, duration_ms: 0, code: '' },
      ],
    } as unknown as TranscriptTurn;
    const leader = storedLeaderLane(stored);
    expect(leader?.waits).toEqual([
      { start: 1794213407814 - 3239, end: 1794213407814 },
      { start: 1794213410289 - 36, end: 1794213410289 },
    ]);
    render(<TurnAgents {...props} agents={team} leader={leader} />);
    expect(tableRows()[0]).toBe('leaderBoth subagents finished, and each replied with the text it was given:12s (waited 3s)done');
  });

  it('marks the leader of a stored turn that ended in an error or a stop as failed', () => {
    wide(true);
    render(<TurnAgents {...props} agents={team} leader={storedLeaderLane(turn('interrupted'))} />);
    expect(document.querySelector('[data-leader-row]')).toHaveTextContent(/30s \(waited 12s\)failed$/);
  });

  // Regression, issue #460: four children of an earlier turn and two of the running turn split the
  // dock into two charts. The new children were A1 and A2 again, and the leader stood in both.
  it('draws every docked child on one chart, keyed by its task id, under one leader lane', () => {
    wide(true);
    vi.useFakeTimers({ now: T0 + 5_000 });
    const leader = leaderLane(T0 - 10_000, undefined, [{ assistant_prose: 'I wait for both implementers.' }]);
    const early = [1, 2, 3, 4].map((n) => ({ ...child, session_id: `r${n}`, id: `a${n}`, team_id: 'turn-0', role: 'research', title: `Read ${n}`, created_at: T0 - 7_000_000 + n }));
    const late = [5, 6].map((n) => ({ ...child, session_id: `i${n}`, id: `a${n}`, role: 'implement', title: `Write ${n}`, created_at: T0 - 9_000 + n }));
    render(<AgentDock sid="leader" runningTurnId="turn-1" onOpen={vi.fn()} leader={leader} agents={[...early, ...late]} />);
    fireEvent.click(screen.getByRole('button', { name: 'Timeline' }));
    const lanes = within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem').map((lane) => lane.textContent?.split(' ')[0]);
    expect(lanes).toEqual(['L', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6']);
    const legend = within(screen.getByRole('list', { name: 'Legend' })).getAllByRole('listitem').map((row) => row.textContent);
    expect(legend.slice(1)).toEqual([1, 2, 3, 4, 5, 6].map((n) => `A${n}${n < 5 ? 'researchRead' : 'implementWrite'} ${n}Verify the contract`));
  });

  // Issue #460: a team of a later turn keeps the task ids of its children, so the keys match a5 and a6.
  it('keys the lanes of a later team by the task ids of its children', () => {
    wide(true);
    render(<TurnAgents {...props} isTimeline agents={[5, 6].map((n) => ({ ...child, session_id: `i${n}`, id: `a${n}`, status: 'completed' as const, finished_at: T0 + n * 1_000 }))} />);
    const lanes = within(screen.getByRole('list', { name: 'Timeline' })).getAllByRole('listitem').map((lane) => lane.textContent?.split(' ')[0]);
    expect(lanes).toEqual(['A5', 'A6']);
  });

  it('puts the running leader first in the dock, once for all docked teams', () => {
    wide(true);
    vi.useFakeTimers({ now: T0 + 5_000 });
    const leader = leaderLane(T0 - 10_000, undefined, [{ assistant_prose: 'I wait for the reviews.' }]);
    render(<AgentDock sid="leader" runningTurnId="turn-1" onOpen={vi.fn()} leader={leader}
      agents={[child, { ...child, session_id: 'old', team_id: 'turn-0' }]} />);
    fireEvent.click(screen.getByRole('button', { expanded: false }));
    expect(tableRows()).toEqual([
      'leaderI wait for the reviews.15s (waited 0s)running',
      expect.stringMatching(/^agent#1/),
      expect.stringMatching(/^agent#1/),
    ]);
    expect(screen.getByRole('button', { expanded: true })).toHaveAccessibleName(/2 running$/);
  });
});

/** A real session hub over a fake stream: a test emits gateway frames into it. */
function streamHub(rememberAgents = vi.fn()) {
  const streams: ((event: SseEvent) => void)[] = [];
  const client = {
    streamSessionEvents: (_cursors: Map<string, number>, emit: (event: SseEvent) => void) => {
      streams.push(emit);
      return vi.fn();
    },
    streamFleetStatus: () => vi.fn(),
    rememberAgents,
  } as unknown as GatewayClient;
  const hub = new SessionSubscriptionHub(client);
  // The session screen watches the session, which opens the stream.
  hub.subscribeSession('leader', () => {});
  return { hub, emit: (event: Record<string, unknown>) => act(() => streams.at(-1)!(event as SseEvent)) };
}

function TeamView({ client, sid, stored }: { client: GatewayClient; sid: string; stored?: Subagent[] }) {
  const team = useAgentTeam(client, sid, stored);
  return <TurnAgents agents={team.agents} onOpen={vi.fn()} />;
}

function Team({ hub = null, ...props }: { client: GatewayClient; sid: string; stored?: Subagent[]; hub?: SessionSubscriptionHub | null }) {
  return <SessionEventsContext.Provider value={hub}><TeamView {...props} /></SessionEventsContext.Provider>;
}

const fakeClient = (more: Record<string, unknown> = {}) => ({ ...more }) as unknown as GatewayClient;

const ready = (agents?: Subagent[]) => ({ type: 'subscription.ready', session_id: 'leader', cursor: 1, ...(agents ? { agents } : {}) });

describe('agent team reader', () => {
  let hub: SessionSubscriptionHub | null = null;
  afterEach(() => {
    hub?.dispose();
    hub = null;
  });

  // Protocol 22: the ready frame of each stream connect is the only team read. A reconnect
  // replaces the team, so a row that the stream lost while it was down does not stay.
  it('shows the rows of the ready frame, merges each push, and replaces them on a reconnect', () => {
    const remember = vi.fn();
    const stream = streamHub(remember);
    hub = stream.hub;
    render(<Team client={fakeClient()} sid="leader" hub={hub} />);
    stream.emit(ready([child]));
    expect(rowTexts()).toEqual([expect.stringMatching(/running$/)]);
    stream.emit({ type: 'session.team_updated', session_id: 'leader', team: { active: 1, total: 2 },
      agents: [{ ...child, session_id: 'second', status: 'queued' }] });
    expect(rowTexts().filter((text) => text?.startsWith('agent#'))).toHaveLength(2);
    stream.emit(ready([{ ...child, status: 'completed' }]));
    expect(screen.getByText('done')).toBeInTheDocument();
    expect(rowTexts().filter((text) => text?.startsWith('agent#'))).toHaveLength(1);
    expect(remember).toHaveBeenLastCalledWith('leader', [{ ...child, status: 'completed' }]);
    stream.emit(ready());
    expect(screen.queryByRole('button', { name: /^agent#1/ })).not.toBeInTheDocument();
  });

  // Before the first ready frame of this app run, the team of the last visit shows at once.
  // A transcript row is newer than that cached row, but older than a stream row.
  it('shows the cached team before the ready frame, under the transcript rows', () => {
    const stream = streamHub();
    hub = stream.hub;
    const cachedAgents = vi.fn().mockReturnValue([child]);
    render(<Team client={fakeClient({ cachedAgents })} sid="leader" stored={[{ ...child, status: 'completed' }]} hub={hub} />);
    expect(screen.getByText('done')).toBeInTheDocument();
    stream.emit(ready([{ ...child, status: 'running' }]));
    expect(rowTexts()).toEqual([expect.stringMatching(/running$/)]);
  });
});
