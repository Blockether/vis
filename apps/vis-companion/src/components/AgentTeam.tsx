import {
  type CSSProperties,
  type ReactNode,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
  useId,
  useSyncExternalStore,
} from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import { mergeAgents, type GatewayClient } from '../lib/gateway';
import type { CouncilRequest, Subagent, TeamClock, TranscriptIteration, TranscriptTurn } from '../lib/types';
import { councilLine, councilMarkdown } from '../lib/council-markdown';
import { SessionEventsContext } from '../lib/session-events';
import { axisLabel, axisMarks, formatDuration as formatAgentTime } from '../lib/time-axis';
export { axisMarks, formatAgentTime };
import { Markdown } from './ChatContent';
import { ChevronIcon, MARK_NUDGE, TimelineIcon } from './icons';
import { HoverNote } from './ui';

/** The roles of the agents contract, in its order. A role is a label: it sets no limit. */
export const ROLES: readonly { const: string; title: string }[] = agentsSchema.$defs.role.oneOf;

// The word of an agent without a role, in its row.
const NO_ROLE = 'agent';

// The ROLE column fits the longest role word of the contract, so all teams line up.
const ROLE_WIDTH = Math.max(NO_ROLE.length, ...ROLES.map((role) => role.const.length));

// The ink of each contract role. A role that the Companion does not know reads like no role.
const ROLE_INK: ReadonlyMap<string, string> = new Map([
  ['research', 'text-role-research'],
  ['implement', 'text-role-implement'],
  ['verify', 'text-role-verify'],
]);

/** The role word of an agent, with its ink. The views write the word in capitals. */
export function agentRole(agent: Subagent): { text: string; ink: string } {
  const role = agent.role?.trim() || NO_ROLE;
  return { text: role, ink: ROLE_INK.get(role) ?? 'text-dialog-hint' };
}

export const isLive = (agent: Subagent) => ['queued', 'running'].includes(agent.status);

/** A `waiting` or `blocked` task did not start: it has no session to open yet. */
export const hasStarted = (agent: Subagent) => agent.status !== 'waiting' && agent.status !== 'blocked';

/**
 * The tasks in the `after` of a task that did not start, which did not complete: a task id such
 * as `a2`. An id without a row of its parent stays, because nothing shows that it completed.
 */
export function waitsFor(agent: Subagent, agents: readonly Subagent[]): string[] {
  return (agent.after ?? []).filter(
    (id) => !agents.some((other) => other.parent_id === agent.parent_id && other.id === id && other.status === 'completed'),
  );
}

/** Why a task did not start, like `waiting for a2`, or undefined for a task that started. */
export function waitLine(agent: Subagent, agents: readonly Subagent[]): string | undefined {
  if (hasStarted(agent)) return undefined;
  const ids = waitsFor(agent, agents).join(', ');
  if (!ids) return END_REASON[agent.status];
  return agent.status === 'waiting' ? `waiting for ${ids}` : `${ids} did not complete`;
}

/** The agents of one team in spawn order: by `created_at`, then in the order of the gateway. */
export function spawnOrder(agents: readonly Subagent[]): Subagent[] {
  return agents
    .map((agent, index) => ({ agent, index }))
    .sort((a, b) => (a.agent.created_at ?? 0) - (b.agent.created_at ?? 0) || a.index - b.index)
    .map(({ agent }) => agent);
}

/** The name of an agent for people: its title, else `#N`, its place in the spawn order of its team. */
export function agentLabel(agent: Subagent, agents: readonly Subagent[]): string {
  if (agent.title?.trim()) return agent.title.trim();
  const team = spawnOrder(agents.filter((other) => other.team_id === agent.team_id));
  return `#${team.findIndex((other) => other.session_id === agent.session_id) + 1}`;
}

// The characters of a child's Council message that its one transcript line keeps.
const NOTE_WIDTH = 120;

const notes = new WeakMap<CouncilRequest, { content: string; note: CouncilRequest }>();

/**
 * The Council entry that the transcript of a leader shows for the request of a turn. An entry
 * that an agent of the leader wrote shows as one line that names the agent, such as
 * `IMPLEMENT | Fix parser: Fix round in progress.`: the team rows show the full message, and the
 * line tells why the turn started. Mirrors the TUI. An unchanged line keeps its object, so a
 * memoized row does not paint again.
 */
export function childWakeCouncil(
  council: CouncilRequest | undefined,
  agents: readonly Subagent[],
  leader: string | undefined,
): CouncilRequest | undefined {
  const author = council?.author_session_id;
  const agent = author == null ? undefined : agents.find((a) => a.session_id === author && a.parent_id === leader);
  if (!council || !agent) return council;
  const first = (council.content ?? '').trim().split('\n')[0].replace(/\s+/g, ' ').trim();
  // The outcome entry of the host starts `Subagent <id> — completed`: the agent name already tells who.
  const outcome = `Subagent ${author} — `;
  const line = first.startsWith(outcome) ? first.slice(outcome.length) : first;
  const short = line.length > NOTE_WIDTH ? `${line.slice(0, NOTE_WIDTH - 1)}…` : line;
  const role = agent.role?.trim() ? `${agent.role.trim().toUpperCase()} | ` : '';
  const content = `${role}${agentLabel(agent, agents)}: ${short}`;
  const cached = notes.get(council);
  if (cached?.content === content) return cached.note;
  const note = { ...council, content };
  notes.set(council, { content, note });
  return note;
}

/**
 * The status word of an agent, with the theme ink that agrees with it: `waiting`, `queued`,
 * `running`, `done`, `failed` or `blocked`. The views write the word in capitals.
 * `waiting` and `blocked` tasks did not start: a task in their `after` did not complete yet, or
 * never will. A failed verdict, a cancel and a
 * spent budget are all `failed`; the update of the row tells which. A question to its leader
 * is no status: the leader answers it, not a person.
 */
export function agentStatus(agent: Subagent): { text: string; ink: string } {
  switch (agent.status) {
    case 'waiting':
      return { text: 'waiting', ink: 'text-dialog-hint' };
    case 'blocked':
      return { text: 'blocked', ink: 'text-err-ink' };
    case 'queued':
      return { text: 'queued', ink: 'text-dialog-hint' };
    case 'running':
      return { text: 'running', ink: 'text-accent-ink' };
    case 'completed':
      if (agent.outcome === 'failed') return { text: 'failed', ink: 'text-err-ink' };
      return { text: 'done', ink: 'text-ok-ink' };
    default:
      return { text: 'failed', ink: 'text-err-ink' };
  }
}

// The end that `failed` hides, for an agent without a result.
const END_REASON: Partial<Record<Subagent['status'], string>> = {
  cancelled: 'cancelled',
  budget_limited: 'out of budget',
  waiting: 'waits for earlier tasks',
  blocked: 'an earlier task did not complete',
};

// The start of the current task: the spawn, or the last resume.
function startOf(agent: Subagent): number | undefined {
  const marks = [agent.created_at, agent.resumed_at].filter((mark): mark is number => typeof mark === 'number');
  return marks.length ? Math.max(...marks) : undefined;
}

/**
 * The time of an agent in ms. The store keeps the time of the tasks that ended (`worked_ms`) and
 * the start of the work that runs now (`busy_since`), so this adds only the time from that start
 * to now, as for a team clock. An agent that does not work shows the stored time only, so the
 * time stops. A gateway from before `worked_ms` gives the time of the current task only.
 * Undefined while queued without earlier work.
 */
export function agentTime(agent: Subagent, now: number): number | undefined {
  const worked = agent.worked_ms ?? 0;
  if (typeof agent.busy_since === 'number') return worked + Math.max(0, now - agent.busy_since);
  if (worked > 0) return worked;
  const start = startOf(agent);
  if (agent.worked_ms !== undefined || start === undefined) return undefined;
  if (agent.status === 'running') return Math.max(0, now - start);
  if (agent.status === 'queued' || agent.finished_at == null) return undefined;
  return Math.max(0, agent.finished_at - start);
}

/**
 * The update of a row. While the agent works: its newest progress note, the note that its
 * leader also reads, else its latest Activity line, else the first line of its task. After
 * its task: its result, else the reason of its end. A note, a result and a task are Markdown,
 * so the row shows them without their markers.
 */
export function agentUpdate(agent: Subagent, agents: readonly Subagent[] = []): string | undefined {
  if (!hasStarted(agent)) return waitLine(agent, agents);
  if (!isLive(agent)) return agent.result == null ? END_REASON[agent.status] : councilLine(agent.result);
  if (agent.note != null) return councilLine(agent.note);
  return agent.activity ?? (councilLine(agent.task.trim().split('\n', 1)[0]) || undefined);
}

const marks = (values: readonly (number | null | undefined)[]) => values.filter((mark): mark is number => typeof mark === 'number');

/**
 * The time of a team in ms at `now`, from its stored clock: the work that ended, plus the work
 * that runs now. The store joins the work of the agents, so this reads no agent row. Undefined
 * without a clock.
 */
export function teamClockTime(clock: TeamClock | undefined, now: number): number | undefined {
  if (!clock) return undefined;
  return clock.worked_ms + (clock.busy_since === undefined ? 0 : Math.max(0, now - clock.busy_since));
}

/** The clock of several docked teams: the one with the longest time at `now`. Undefined without a clock. */
export function longestClock(clocks: readonly (TeamClock | undefined)[], now: number): TeamClock | undefined {
  let longest: TeamClock | undefined;
  for (const clock of clocks) {
    if (clock && (!longest || teamClockTime(clock, now)! > teamClockTime(longest, now)!)) longest = clock;
  }
  return longest;
}

// The dot of a status word on the team line, in the ink of that word.
const STATUS_DOT: Record<string, string> = {
  waiting: 'bg-dialog-hint',
  queued: 'bg-dialog-hint',
  running: 'bg-accent-ink',
  done: 'bg-ok-ink',
  failed: 'bg-err-ink',
  blocked: 'bg-err-ink',
};

// The most dots on a team line. `+N more` counts the other agents.
const MAX_DOTS = 3;

/** One status dot for each agent of a team in spawn order: three at most, then `+N more` (user decision). */
function StatusDots({ agents }: { agents: readonly Subagent[] }) {
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const rest = rows.length - MAX_DOTS;
  return (
    <span className="flex shrink-0 items-center gap-1.5" data-team-dots>
      <span role="img" aria-label={rows.map((agent) => `${agentLabel(agent, rows)} ${agentStatus(agent).text}`).join(', ')} className="flex items-center gap-1">
        {rows.slice(0, MAX_DOTS).map((agent) => (
          <span key={agent.session_id} className={`size-[7px] rounded-full ${STATUS_DOT[agentStatus(agent).text]}`} />
        ))}
      </span>
      {rest > 0 && <span aria-hidden="true" className="whitespace-nowrap text-dialog-hint">+{rest} more</span>}
    </span>
  );
}

/**
 * A team on one line: a status dot for each agent, then its time: `TEAM ●●● +2 more 3m 20s`. `action` stands at the
 * end of the line. `labelId` names the word and the time `${labelId}-team` and `${labelId}-time`, so a control can
 * take them as its name.
 */
export function TeamLine({ agents, clock, action, labelId }: { agents: readonly Subagent[]; clock?: TeamClock; action?: ReactNode; labelId?: string }) {
  const now = useNow(clock?.busy_since !== undefined);
  const time = teamClockTime(clock, now);
  return (
    <span className="flex min-w-0 flex-1 items-center gap-x-3 sm:min-h-5" data-team-line>
      <span className="flex min-w-0 flex-1 flex-wrap items-center gap-x-3 max-sm:flex-nowrap">
        <span id={labelId && `${labelId}-team`} className="font-bold uppercase text-white">team</span>
        <StatusDots agents={agents} />
        {time !== undefined && (
          <span id={labelId && `${labelId}-time`} className="whitespace-nowrap text-dialog-hint tabular-nums" data-team-time>
            {formatAgentTime(time)}
          </span>
        )}
      </span>
      {action}
    </span>
  );
}

/**
 * Whether the team of `agents` has a timeline to show: two agents or more, or one agent beside
 * its leader, with a time to draw.
 */
export function hasTimeline(agents: readonly Subagent[], leader?: LeaderLane): boolean {
  return (agents.length > 1 || (agents.length > 0 && leader != null)) && !!chartSpan(agents, Date.now(), leader);
}

/**
 * The body of a team fold. It grows from its top and shrinks back, so a click on the team line shows motion, not a jump.
 * The content mounts at the first open and stays, so the fold can also close with motion. A closed body is inert and
 * hidden from assistive technology.
 */
export function TeamFold({ id, isOpen, className, children }: { id: string; isOpen: boolean; className: string; children: ReactNode }) {
  return (
    <div
      id={id}
      aria-hidden={!isOpen}
      inert={!isOpen}
      className={`grid transition-[grid-template-rows] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
        isOpen ? 'grid-rows-[1fr]' : 'grid-rows-[0fr]'
      }`}
    >
      <div className="min-h-0 overflow-hidden">
        <div className={className}>{children}</div>
      </div>
    </div>
  );
}

/**
 * The control that swaps the notes of a team for its timeline, and back: a quiet mark at the end of the team line,
 * so the line reads as status first (user decision). It takes the page ink, with no border, while the timeline shows.
 */
export function TimelineToggle({ isOn, onToggle }: { isOn: boolean; onToggle: () => void }) {
  return (
    <button
      type="button"
      aria-label="Timeline"
      title="Timeline"
      aria-pressed={isOn}
      onClick={onToggle}
      className={`pointer-events-auto relative -my-0.5 inline-flex min-h-6 min-w-6 shrink-0 items-center justify-end pr-0.5 after:absolute after:-inset-2 after:content-[""] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 mouse:after:content-none ${
        isOn ? 'text-white' : 'text-dialog-hint hover:text-white'
      }`}
    >
      <TimelineIcon className="size-3.5" />
    </button>
  );
}

// The clock of the rows and of the team time. It ticks once a second only while `ticking`.
function useNow(ticking: boolean): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!ticking) return undefined;
    setNow(Date.now());
    const timer = window.setInterval(() => setNow(Date.now()), 1000);
    return () => window.clearInterval(timer);
  }, [ticking]);
  return now;
}

// A wide screen: the `sm` width of the theme. A row there shows the note of its agent beside its
// title.
const WIDE = '(min-width: 640px)';

export function useWide(): boolean {
  return useSyncExternalStore(
    (change) => {
      const query = window.matchMedia(WIDE);
      query.addEventListener?.('change', change);
      return () => query.removeEventListener?.('change', change);
    },
    () => window.matchMedia(WIDE).matches,
    () => false,
  );
}

/** A time in epoch ms. An open end runs to now. */
export interface Span {
  start: number;
  end?: number;
}

/** The leader turn of a team: its own time, the steps in which it waited for its agents, and its newest line. */
export interface LeaderLane extends Span {
  waits: readonly Span[];
  note?: string;
  /** The ended turn failed or stopped: an error, an interrupt or a cancel. */
  isFailed?: boolean;
}

// The newest line of a leader: the answer once its turn ended, else the prose of its newest step.
// Prose is Markdown, so the note shows it without its markers.
function leaderNote(iterations: readonly TranscriptIteration[], isEnded: boolean): string | undefined {
  for (let index = iterations.length - 1; index >= 0; index -= 1) {
    const text = (isEnded && iterations[index].answer?.trim()) || iterations[index].assistant_prose?.trim();
    if (text) return councilLine(text) || undefined;
  }
  return undefined;
}

// The start of each form of a step. A live form carries its own start. A stored form does not:
// the forms of a step ran one after another and ended when the step was written, at its `created_at`.
function formStarts(iteration: TranscriptIteration): (number | undefined)[] {
  const forms = iteration.forms ?? [];
  const written = typeof iteration.created_at === 'number' ? iteration.created_at : undefined;
  let end = written;
  const starts: (number | undefined)[] = new Array(forms.length);
  for (let index = forms.length - 1; index >= 0; index -= 1) {
    const { started_at_ms: at, duration_ms: duration } = forms[index];
    end = end != null && typeof duration === 'number' ? end - duration : undefined;
    starts[index] = typeof at === 'number' ? at : end;
  }
  return starts;
}

/**
 * The leader lane of a turn. A recorded wait starts in its form and lasts for its recorded
 * duration, or to now if it is still active. A form without wait evidence counts as work.
 */
export function leaderLane(
  start: number | null | undefined,
  end: number | null | undefined,
  given: readonly TranscriptIteration[] | null = [],
): LeaderLane | undefined {
  if (start == null) return undefined;
  // A turn row or a story may carry no step list at all.
  const iterations = given ?? [];
  const waits: Span[] = [];
  for (const iteration of iterations) {
    const starts = formStarts(iteration);
    (iteration.forms ?? []).forEach((form, index) => {
      const at = starts[index];
      if (at === undefined) return;
      for (const row of form.activity?.rows ?? []) {
        if (row.operation !== 'council.wait') continue;
        if (typeof row.duration_ms === 'number') waits.push({ start: at, end: at + row.duration_ms });
        else if (end == null && row.state === 'running') waits.push({ start: at });
      }
    });
  }
  return { start, end: end ?? undefined, waits: waits.sort((a, b) => a.start - b.start), note: leaderNote(iterations, end != null) };
}

// The stored states of a leader turn that did not end well.
const FAILED_TURN = new Set(['error', 'failed', 'interrupted', 'cancelled']);

/**
 * The leader lane of a stored turn. Undefined when the turn gives no start or no end. A stored step
 * has no answer: the answer of the turn is its first prose block, and it is the note of the leader.
 */
export function storedLeaderLane(turn: TranscriptTurn): LeaderLane | undefined {
  const start = turn.created_at;
  const end = turn.completed_at ?? (start != null && turn.duration_ms != null ? start + turn.duration_ms : undefined);
  const lane = end == null ? undefined : leaderLane(start, end, turn.iterations);
  if (!lane) return undefined;
  const answer = turn.content?.find((block) => block.type === 'prose' && block.markdown?.trim())?.markdown;
  const firstLine = answer?.trim().split('\n', 1)[0];
  const note = (firstLine && councilLine(firstLine)) || lane.note;
  const isFailed = FAILED_TURN.has(turn.status ?? '') || turn.prior_outcome === 'cancelled';
  return { ...lane, note, ...(isFailed ? { isFailed } : {}) };
}

/**
 * The time of the chart of a team, in epoch ms: from the first task or the start of the leader,
 * to now while anything works, else to the last end. A resumed agent counts from its first
 * earlier run, else from its resume, so a spawn without a run does not push the current work to
 * the end of the axis.
 */
export function chartSpan(rows: readonly Subagent[], now: number, leader?: LeaderLane): Required<Span> | undefined {
  const starts = marks([
    ...rows.map((agent) => agent.runs?.[0]?.start ?? (agent.status === 'queued' ? agent.created_at : startOf(agent))),
    leader?.start,
  ]);
  if (starts.length === 0) return undefined;
  const start = Math.min(...starts);
  const ends = marks([...rows.map((agent) => agent.finished_at), leader?.end]);
  const isWorking = rows.some(isLive) || (leader != null && leader.end == null);
  const end = isWorking ? Math.max(now, ...ends) : ends.length ? Math.max(...ends) : undefined;
  return end != null && end > start ? { start, end } : undefined;
}

/**
 * Where a part of a lane stands on the chart: its start and end, from 0 to 1. `status` is the
 * status word of an earlier run, or `failed` for the time between a failed run and the next start.
 */
export interface AgentBar {
  start: number;
  end: number;
  isWaiting: boolean;
  status?: string;
}

const shareOf = ({ start, end }: Required<Span>) => (at: number) => Math.min(1, Math.max(0, (at - start) / (end - start)));

/**
 * The bars of each agent on the chart of its team, by session id, in time order. Each earlier run
 * has a bar in the ink of its status. After a failed run, a failed dashed line goes to the next
 * start, so the time when the agent was down stays visible. The current task goes from its start
 * to its end, or to now while it runs. A queued agent waits from its spawn to the end of the
 * chart. An agent without the marks of its task has no bar for it.
 */
export function agentBars(rows: readonly Subagent[], now: number, leader?: LeaderLane): Map<string, AgentBar[]> {
  const bars = new Map<string, AgentBar[]>();
  const span = chartSpan(rows, now, leader);
  if (!span) return bars;
  const share = shareOf(span);
  for (const agent of rows) {
    const parts: AgentBar[] = [];
    const start = agent.status === 'queued' ? agent.created_at : startOf(agent);
    const end = agent.status === 'queued' ? span.end : agent.status === 'running' ? now : agent.finished_at;
    const runs = agent.runs ?? [];
    runs.forEach((run, index) => {
      const status = agentStatus({ ...agent, status: run.status, outcome: run.outcome }).text;
      parts.push({ start: share(run.start), end: share(run.end), isWaiting: false, status });
      const next = runs[index + 1]?.start ?? start;
      if (status === 'failed' && next != null && next > run.end) {
        parts.push({ start: share(run.end), end: share(next), isWaiting: true, status });
      }
    });
    if (start != null && end != null) {
      parts.push({ start: share(start), end: share(end), isWaiting: agent.status === 'queued' });
    }
    if (parts.length) bars.set(agent.session_id, parts);
  }
  return bars;
}

/**
 * The parts of a leader lane in epoch ms, in time order: its work, and its waits for its agents.
 * A wait is cut to the time of the lane, and an overlap counts once.
 */
export function leaderParts(leader: LeaderLane, now: number): { start: number; end: number; isWaiting: boolean }[] {
  const end = leader.end ?? now;
  const parts: { start: number; end: number; isWaiting: boolean }[] = [];
  let at = leader.start;
  for (const wait of leader.waits) {
    const from = Math.max(at, wait.start);
    const to = Math.min(end, wait.end ?? now);
    if (to <= from) continue;
    if (from > at) parts.push({ start: at, end: from, isWaiting: false });
    parts.push({ start: from, end: to, isWaiting: true });
    at = to;
  }
  if (end > at) parts.push({ start: at, end, isWaiting: false });
  return parts;
}

/**
 * The key of an agent on the chart: its task id in capitals, so task `a5` is `A5` in every team of
 * its parent. A child from before task ids gets its place in the spawn order. The leader is `L`.
 */
export const agentKey = (agent: Subagent, index: number) => agent.id?.toUpperCase() ?? `A${index + 1}`;
const LEADER_KEY = 'L';
/** The role word of the leader and its ink, in the team chart and the header path. */
export const LEADER_ROLE = 'leader';
export const LEADER_INK = 'text-role-leader';
const LEADER_TIME_WIDTH = '59m 59s (waited 59m 59s)'.length;

/** The status word of a leader with its ink: running while its turn runs, then done, or failed after an error or a stop. */
export function leaderStatus(leader: LeaderLane): { text: string; ink: string } {
  return agentStatus({ status: leader.end == null ? 'running' : leader.isFailed ? 'failed' : 'completed' } as Subagent);
}

// The time of a leader in ms, and the part of it in which it waited for its agents.
function leaderTimes(leader: LeaderLane, now: number): { waited: number; total: number } {
  const waited = leaderParts(leader, now).reduce((sum, part) => sum + (part.isWaiting ? part.end - part.start : 0), 0);
  return { waited, total: Math.max(0, (leader.end ?? now) - leader.start) };
}

// The time of a leader, then the part of it that it waited for its agents.
const leaderWaited = ({ waited }: { waited: number }) => `(waited ${formatAgentTime(waited)})`;
const leaderTime = (times: { waited: number; total: number }) =>
  `${formatAgentTime(times.total)}${times.waited > 0 ? ` ${leaderWaited(times)}` : ''}`;

// The bar ink of each status word.
const BAR_INK: Record<string, string> = {
  running: 'bg-accent-ink',
  done: 'bg-ok-ink',
  failed: 'bg-err-ink',
};

// The status word of a row on a wide screen, in capitals and in its ink, such as DONE or FAILED.
function StatusWord({ status }: { status: { text: string; ink: string } }) {
  return <span className={`whitespace-nowrap uppercase ${status.ink}`}>{status.text}</span>;
}

// The lines of a team row on a phone: ROLE, then TIME and STATUS at the end; the TITLE; then the
// UPDATE. Each line takes the whole width.
const PHONE_ROW = 'grid w-full min-w-0 grid-cols-[minmax(0,1fr)_auto] items-baseline gap-x-3 py-1 text-left';

// The columns of a team on a wide screen: ROLE, TITLE, then the NOTE, or the time bar while the
// timeline shows, then TIME and STATUS. A hairline stands after ROLE, TITLE, NOTE and TIME.
// TIME fits `59m 59s`, or the leader time with its wait, and STATUS fits `running`, so a ticking time never moves a column. TITLE takes at most a third
// of the row; a longer title wraps. A wide row carries the frame's own inset, so its hover fills
// the frame and its text lines up with the team line.
const WIDE_ROW = 'grid w-full min-w-0 grid-cols-[var(--role)_min(var(--title),32%)_minmax(0,1fr)_var(--time)_7ch] items-baseline gap-x-3 px-3';

// The hairline after a cell: in the middle of the gap, through the row inset, so the rows join it.
const CELL_RULE = 'relative self-stretch after:absolute after:-inset-y-1 after:-right-1.5 after:w-px after:bg-dialog-edge';

// The TIME cell of a chart row also has the hairline before it, after the time bar.
const TIME_RULES = `${CELL_RULE} before:absolute before:-inset-y-1 before:-left-1.5 before:w-px before:bg-dialog-edge`;

const FOCUS = 'rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent';

// The widths of the columns, in `ch` of the monospace font. ROLE is the same for every team. TITLE
// fits the team: its heading is the least width, and the bold title gets one more `ch`. TIME fits
// `59m 59s (waited 59m 59s)` when a leader row shows, so it keeps one width while its clock ticks.
function columnWidths(rows: readonly Subagent[], leader?: LeaderLane): CSSProperties {
  const widest = (least: string, words: string[]) => Math.max(least.length, ...words.map((word) => word.length));
  return {
    '--role': `${Math.max(leader ? LEADER_ROLE.length : 0, ROLE_WIDTH)}ch`,
    '--title': `${widest('title', rows.map((agent) => agentLabel(agent, rows))) + 1}ch`,
    '--time': `${leader ? LEADER_TIME_WIDTH : 7}ch`,
  } as CSSProperties;
}

// The columns of the chart of a team: the KEY of each lane, its time BAR, its TIME and its
// STATUS. The legend under the chart gives each KEY its ROLE, TITLE and the NOTE of its row. A phone
// keeps TIME at `59m 59s` so the bars keep their room; the legend gives the wait of the leader.
const CHART_TIME = 'max-sm:[--time:7ch]';
const CHART_ROW = `grid w-full min-w-0 grid-cols-[var(--key)_minmax(0,1fr)_var(--time)_7ch] items-baseline gap-x-3 px-3 ${CHART_TIME}`;
// A phone legend has one line for each lane and no note: the rows of the team show the notes.
const LEGEND_ROW = 'grid w-full min-w-0 grid-cols-[var(--key)_var(--role)_minmax(0,1fr)] max-sm:py-0.5 sm:grid-cols-[var(--key)_var(--role)_min(var(--title),32%)_minmax(0,1fr)] items-baseline gap-x-3 px-3';
const LEGEND_NOTE = 'min-w-0 max-sm:hidden';
// A phone legend draws only the hairline after the KEY.
const LEGEND_LAST = 'max-sm:after:hidden';

// The bar column of a chart row: the inset of the row, the KEY and one gap on the left; one gap,
// TIME, one gap, STATUS and the inset on the right. The grid lines stand there behind the bars.
const BAR_COLUMN: CSSProperties = { left: 'calc(1.5rem + var(--key))', right: 'calc(2.25rem + var(--time) + 7ch)' };

// The time axis above the bars: a label at each mark.
function AxisLabels({ total }: { total: number }) {
  return axisMarks(total).map((at, index) => (
    <span key={at} className={`absolute top-0 ${index === 0 ? '' : '-translate-x-1/2'}`} style={{ left: `${(at / total) * 100}%` }}>
      {axisLabel(at)}
    </span>
  ));
}

// A faint line at each mark of the axis, through every lane of the chart.
function GridLines({ total }: { total: number }) {
  return (
    <span aria-hidden className="pointer-events-none absolute inset-y-0" style={BAR_COLUMN}>
      {axisMarks(total).map((at) => (
        <span key={at} className="absolute inset-y-0 w-px bg-dialog-edge/40" style={{ left: `${(at / total) * 100}%` }} />
      ))}
    </span>
  );
}

// One part of a lane: a solid bar for work, a dashed line for a wait. The time after a failed run is
// a failed dashed line.
function LanePart({ bar, ink }: { bar: AgentBar; ink: string }) {
  const wait = bar.status === 'failed' ? 'border-err-ink' : 'border-dialog-hint';
  return (
    <span
      className={`absolute ${bar.isWaiting ? `inset-y-1/2 border-t border-dashed ${wait}` : `inset-y-0 ${ink}`}`}
      style={{ left: `${bar.start * 100}%`, width: `max(2px, ${(bar.end - bar.start) * 100}%)` }}
    />
  );
}

// The task of an agent in the note of its row: Markdown, like the Task part of its sheet.
function taskNote(task: string): ReactNode {
  return task.trim() ? <Markdown compact hardBreaks>{councilMarkdown(task)}</Markdown> : null;
}

interface TableProps {
  rows: readonly Subagent[];
  now: number;
  onSelect: (sid: string) => void;
  /** Show the time bars of the team instead of the notes. */
  isTimeline?: boolean;
  /** The leader turn, drawn as the first lane of the chart. */
  leader?: LeaderLane;
}

// The table of one team. A row opens the session of its agent; a task that did not start has no
// session yet, so its row is disabled. A wide screen shows the note of
// each agent beside its title. The time bars of the team on one axis show on every screen.
function AgentTable(props: TableProps) {
  const isWide = useWide();
  if (props.isTimeline && hasTimeline(props.rows, props.leader)) return <TeamChart {...props} />;
  return isWide ? <WideAgentTable {...props} /> : <PhoneAgentTable {...props} />;
}

// The end of the first line of a phone row: the time, then the status.
function PhoneEnd({ status, time }: { status: { text: string; ink: string }; time?: string }) {
  return (
    <span className="order-3 whitespace-nowrap text-right">
      {time && <span className="mr-2 text-dialog-hint tabular-nums">{time}</span>}
      <span className={`uppercase ${status.ink}`}>{status.text}</span>
    </span>
  );
}

// The leader row on a phone, in the shape of an agent row. It opens nothing: the leader is this session.
function PhoneLeaderRow({ leader, now }: { leader: LeaderLane; now: number }) {
  const times = leaderTimes(leader, now);
  const status = leaderStatus(leader);
  return (
    <li className={PHONE_ROW} data-leader-row data-hover-note-frame>
      <span className={`order-1 font-bold uppercase ${LEADER_INK}`}>{LEADER_ROLE}</span>
      {leader.note && (
        <HoverNote className="order-5 col-span-2 min-w-0" note={leader.note}>
          <span className="line-clamp-2 text-dialog-hint [overflow-wrap:anywhere]">{leader.note}</span>
        </HoverNote>
      )}
      <PhoneEnd status={status} time={leaderTime(times)} />
    </li>
  );
}

function PhoneAgentTable({ rows, now, onSelect, leader }: TableProps) {
  return (
    <ul className="divide-y divide-dialog-edge/40" style={columnWidths(rows, leader)}>
      {leader && <PhoneLeaderRow leader={leader} now={now} />}
      {rows.map((agent) => {
        const role = agentRole(agent);
        const status = agentStatus(agent);
        const update = agentUpdate(agent, rows);
        const time = agentTime(agent, now);
        return (
          <li key={agent.session_id}>
            <button
              type="button"
              data-agent-row={agent.session_id}
              disabled={!hasStarted(agent)}
              onClick={() => onSelect(agent.session_id)}
              className={`group ${FOCUS} ${PHONE_ROW}`}
            >
              <span className={`order-1 uppercase ${role.ink}`}>{role.text}</span>
              <HoverNote className="order-4 col-span-2 min-w-0" note={taskNote(agent.task)}>
                <span className="font-bold [overflow-wrap:anywhere] group-enabled:group-hover:underline">{agentLabel(agent, rows)}</span>
              </HoverNote>
              {update && (
                <HoverNote className="order-5 col-span-2 min-w-0" note={update}>
                  <span className="line-clamp-2 text-dialog-hint [overflow-wrap:anywhere]">{update}</span>
                </HoverNote>
              )}
              <PhoneEnd status={status} time={time === undefined ? undefined : formatAgentTime(time)} />
            </button>
          </li>
        );
      })}
    </ul>
  );
}

function WideAgentTable(props: TableProps) {
  const { rows, now, onSelect, leader } = props;
  return (
    <div style={columnWidths(rows, leader)}>
      <ul>
        {leader && <WideLeaderRow leader={leader} now={now} />}
        {rows.map((agent) => {
          const role = agentRole(agent);
          const status = agentStatus(agent);
          const update = agentUpdate(agent, rows);
          const time = agentTime(agent, now);
          return (
            <li key={agent.session_id}>
              <button
                type="button"
                data-agent-row={agent.session_id}
                disabled={!hasStarted(agent)}
                onClick={() => onSelect(agent.session_id)}
                className={`group py-1 text-left enabled:hover:bg-white/5 ${FOCUS} ${WIDE_ROW}`}
              >
                <span className={`uppercase ${CELL_RULE} ${role.ink}`}>{role.text}</span>
                <HoverNote className={`min-w-0 ${CELL_RULE}`} note={taskNote(agent.task)}>
                  <span className="font-bold [overflow-wrap:anywhere] group-enabled:group-hover:underline">{agentLabel(agent, rows)}</span>
                </HoverNote>
                <HoverNote className={`min-w-0 ${CELL_RULE}`} note={update}>
                  <span className="line-clamp-1 [overflow-wrap:anywhere] text-dialog-hint">{update ?? ''}</span>
                </HoverNote>
                <span className={`whitespace-nowrap text-right text-dialog-hint tabular-nums ${CELL_RULE}`}>{time === undefined ? '' : formatAgentTime(time)}</span>
                <StatusWord status={status} />
              </button>
            </li>
          );
        })}
      </ul>
    </div>
  );
}

// The leader row on a wide screen, in the columns of an agent row: no title, so its newest line takes the title cell too, its
// time with the part that it waited for its agents, and its status. A hairline under it divides it from its agents. It opens
// nothing: the leader is this session.
function WideLeaderRow({ leader, now }: { leader: LeaderLane; now: number }) {
  const times = leaderTimes(leader, now);
  return (
    <li className={`border-b border-dialog-edge py-1 ${WIDE_ROW}`} data-leader-row data-hover-note-frame>
      <span className={`font-bold uppercase ${CELL_RULE} ${LEADER_INK}`}>{LEADER_ROLE}</span>
      <HoverNote className={`col-span-2 min-w-0 ${CELL_RULE}`} note={leader.note}>
        <span className="line-clamp-1 [overflow-wrap:anywhere] text-dialog-hint">{leader.note ?? ''}</span>
      </HoverNote>
      <span className={`whitespace-nowrap text-right text-dialog-hint tabular-nums ${CELL_RULE}`}>{leaderTime(times)}</span>
      <StatusWord status={leaderStatus(leader)} />
    </li>
  );
}

// The chart of a team: the time axis, the lane of the leader, then one lane for each agent in
// spawn order, on one scale. A lane shows only its key; the legend under the rule gives each key
// its role and title. An agent lane opens the sheet of its agent. Its rows carry their own inset,
// so on a phone it cancels the inset of the frame.
function TeamChart({ rows, now, onSelect, leader }: TableProps) {
  const span = chartSpan(rows, now, leader);
  if (!span) return null;
  const total = span.end - span.start;
  const share = shareOf(span);
  const bars = agentBars(rows, now, leader);
  const parts = leader ? leaderParts(leader, now) : [];
  const times = leader ? leaderTimes(leader, now) : { waited: 0, total: 0 };
  const keys = rows.map(agentKey);
  const widths = {
    ...columnWidths(rows, leader),
    '--key': `${Math.max(2, ...keys.map((key) => key.length))}ch`,
  } as CSSProperties;
  return (
    <div style={widths} className="-mx-3 pb-1 sm:mx-0" data-team-chart>
      <div aria-hidden className={`pt-1 text-dialog-hint ${CHART_ROW}`}>
        <span className={CELL_RULE} />
        <span className="relative h-[calc(1lh+0.25rem)] border-b border-dialog-edge/50"><AxisLabels total={total} /></span>
        <span className={TIME_RULES} />
      </div>
      <div className={`relative ${CHART_TIME}`}>
        <GridLines total={total} />
        <ul aria-label="Timeline">
          {leader && (
            <li className={`py-1 ${CHART_ROW}`} data-leader-lane>
              <span className={`font-bold ${CELL_RULE} ${LEADER_INK}`}>{LEADER_KEY}<span className="sr-only"> {LEADER_ROLE}</span></span>
              <span aria-hidden className="relative h-2 self-center">
                {parts.map((part) => (
                  <LanePart key={part.start} bar={{ start: share(part.start), end: share(part.end), isWaiting: part.isWaiting }} ink="bg-white" />
                ))}
              </span>
              <span className={`whitespace-nowrap text-right text-dialog-hint tabular-nums ${TIME_RULES}`}>
                {formatAgentTime(times.total)}
                {times.waited > 0 && <span className="max-sm:hidden"> {leaderWaited(times)}</span>}
              </span>
              <StatusWord status={leaderStatus(leader)} />
            </li>
          )}
          {rows.map((agent, index) => {
            const role = agentRole(agent);
            const status = agentStatus(agent);
            const time = agentTime(agent, now);
            const parts = bars.get(agent.session_id) ?? [];
            return (
              <li key={agent.session_id}>
                <button
                  type="button"
                  data-agent-row={agent.session_id}
                  disabled={!hasStarted(agent)}
                  onClick={() => onSelect(agent.session_id)}
                  className={`group py-1 text-left enabled:hover:bg-white/5 ${FOCUS} ${CHART_ROW}`}
                >
                  <span className={`group-enabled:group-hover:underline ${CELL_RULE} ${role.ink}`}>
                    {keys[index]}
                    <span className="sr-only"> {agentLabel(agent, rows)}</span>
                  </span>
                  <span aria-hidden className="relative h-2 self-center">
                    {parts.map((part) => (
                      <LanePart key={`${part.start}-${part.isWaiting}`} bar={part} ink={BAR_INK[part.status ?? status.text]} />
                    ))}
                  </span>
                  <span className={`whitespace-nowrap text-right text-dialog-hint tabular-nums ${TIME_RULES}`}>{time === undefined ? '' : formatAgentTime(time)}</span>
                  <StatusWord status={status} />
                </button>
              </li>
            );
          })}
        </ul>
      </div>
      <ul aria-label="Legend" className="border-t border-dialog-edge pt-1">
        {leader && (
          <li className={LEGEND_ROW} data-hover-note-frame>
            <span className={`font-bold ${CELL_RULE} ${LEADER_INK}`}>{LEADER_KEY}</span>
            <span className={`font-bold uppercase ${CELL_RULE} ${LEGEND_LAST} ${LEADER_INK}`}>{LEADER_ROLE}</span>
            {times.waited > 0 && <span className="whitespace-nowrap text-dialog-hint sm:hidden">{leaderWaited(times)}</span>}
            <HoverNote className={`${LEGEND_NOTE} sm:col-span-2`} note={leader.note}>
              <span className="line-clamp-1 [overflow-wrap:anywhere] text-dialog-hint">{leader.note ?? ''}</span>
            </HoverNote>
          </li>
        )}
        {rows.map((agent, index) => {
          const role = agentRole(agent);
          const update = agentUpdate(agent, rows);
          return (
            <li key={agent.session_id} className={LEGEND_ROW} data-hover-note-frame>
              <span className={`${CELL_RULE} ${role.ink}`}>{keys[index]}</span>
              <span className={`uppercase ${CELL_RULE} ${LEGEND_LAST} ${role.ink}`}>{role.text}</span>
              <HoverNote className={`min-w-0 ${CELL_RULE} ${LEGEND_LAST}`} note={taskNote(agent.task)}>
                <span className="font-bold [overflow-wrap:anywhere]">{agentLabel(agent, rows)}</span>
              </HoverNote>
              <HoverNote className={LEGEND_NOTE} note={update}>
                <span className="line-clamp-1 [overflow-wrap:anywhere] text-dialog-hint">{update ?? ''}</span>
              </HoverNote>
            </li>
          );
        })}
      </ul>
    </div>
  );
}

/** The props of a team block. */
export interface TeamProps {
  agents: readonly Subagent[];
  onOpen: (sid: string) => void;
  /** The leader turn of the team, for the first row of its table and the first lane of its chart. */
  leader?: LeaderLane;
  /** The stored clock of the team, for the time of its team line. */
  clock?: TeamClock;
}

/**
 * The subagents of one turn as a table in a frame: the team line with its time, then the row of the leader, then one row for each agent in spawn order. An agent
 * row opens the session of its agent; the leader row opens nothing. The dock shows the table without the frame. A subagent never asks a person: its
 * leader answers it, so a row has no question and no control for one. In the transcript the frame starts folded to its team line (user decision);
 * the line opens the table, and Timeline opens it with the time bars.
 */
export function TurnAgents({ agents, onOpen, leader, clock, placement = 'transcript', isTimeline }: TeamProps & {
  /** The dock above the composer while the team works, or the turn of the transcript after. */
  placement?: 'dock' | 'transcript';
  /** The dock owns the timeline switch of its teams; the transcript owns its own. */
  isTimeline?: boolean;
}) {
  const [isOwnTimeline, setOwnTimeline] = useState(false);
  const [isOpen, setOpen] = useState(false);
  const [hasOpened, setOpened] = useState(false);
  if (isOpen && !hasOpened) setOpened(true);
  const body = useId();
  const line = useId();
  const now = useNow(agents.some((agent) => agent.status === 'running') || (leader != null && leader.end == null));
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const table = <AgentTable rows={rows} now={now} onSelect={onOpen} isTimeline={isTimeline ?? isOwnTimeline} leader={leader} />;
  const isTimelineOn = isOpen && isOwnTimeline;
  const toggle = hasTimeline(rows, leader) ? (
    <TimelineToggle
      isOn={isTimelineOn}
      onToggle={() => {
        setOwnTimeline(!isTimelineOn);
        if (!isTimelineOn) setOpen(true);
      }}
    />
  ) : undefined;
  return placement === 'transcript' ? (
    <section
      aria-label="Subagents"
      className="my-3 border border-dialog-edge font-mono text-meta text-white sm:text-ui"
      data-transcript-part
    >
      {/* The fold control lies under the whole line, so the timeline control can stand inside it. */}
      <div className="relative flex min-w-0 items-baseline gap-2 bg-panel px-3 py-1 sm:py-1.5">
        <button
          type="button"
          aria-expanded={isOpen}
          aria-controls={body}
          aria-labelledby={`${line}-team ${line}-time`}
          onClick={() => setOpen((value) => !value)}
          className="absolute inset-0 rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
        />
        <span className="pointer-events-none relative flex min-w-0 flex-1 items-baseline gap-2">
          <ChevronIcon open={isOpen} className={`size-3 shrink-0 text-dialog-hint ${MARK_NUDGE}`} />
          <TeamLine agents={rows} clock={clock} action={toggle} labelId={line} />
        </span>
      </div>
      <TeamFold id={body} isOpen={isOpen} className="border-t border-dialog-edge px-3 sm:px-0">
        {hasOpened && table}
      </TeamFold>
    </section>
  ) : table;
}

/**
 * The team rows that the transcript turns hold: each turn has the agents that worked in it. An
 * agent that a later turn pinged stands in two turns; it is one row here, the newest.
 */
export function storedAgents(turns: readonly TranscriptTurn[]): Subagent[] {
  return [...new Map(turns.flatMap((turn) => turn.agents ?? []).map((agent) => [agent.session_id, agent])).values()];
}

/** The team clocks that the transcript turns hold, by leader turn. */
export function storedClocks(turns: readonly TranscriptTurn[]): ReadonlyMap<string, TeamClock> {
  return new Map(turns.flatMap((turn): [string, TeamClock][] => (turn.team_clock ? [[turn.turn_id, turn.team_clock]] : [])));
}

/** The leader turns in which the agent worked, oldest first. A row without `turn_ids` worked only in the turn that started it. */
export function turnsOf(agent: Subagent): readonly string[] {
  return agent.turn_ids?.length ? agent.turn_ids : [agent.team_id];
}

/** The leader turn of the agent's current task: the last turn that started or pinged it. */
export function currentTurn(agent: Subagent): string {
  return turnsOf(agent).at(-1) ?? agent.team_id;
}

/** The session's subagents grouped by each turn in which they worked: the turn that started them and each turn that pinged them. */
export function agentsByTurn(agents: readonly Subagent[]): ReadonlyMap<string, Subagent[]> {
  const byTurn = new Map<string, Subagent[]>();
  for (const agent of agents) {
    for (const turn of turnsOf(agent)) byTurn.set(turn, [...(byTurn.get(turn) ?? []), agent]);
  }
  return byTurn;
}

/**
 * The teams of `sid` in the dock, keyed by the turn of their current task (`currentTurn`): the
 * team of the running turn, also the agents that it pinged, and each team that still has a queued
 * or running agent after its turn ended. Spawn order.
 */
export function dockTeams(agents: readonly Subagent[], sid: string, runningTurnId?: string | null): Subagent[][] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) {
    if (agent.parent_id === sid) teams.set(currentTurn(agent), [...(teams.get(currentTurn(agent)) ?? []), agent]);
  }
  return [...teams.entries()]
    .filter(([id, team]) => id === runningTurnId || team.some(isLive))
    .map(([, team]) => team);
}

/** No agents: one value, so a default argument keeps its identity. */
const NO_AGENTS: readonly Subagent[] = [];

/** No clocks: one value, so a default argument keeps its identity. */
const NO_CLOCKS: ReadonlyMap<string, TeamClock> = new Map();

const NO_SUBSCRIPTION = () => () => {};

/**
 * One reader for a session's subagents. It reads nothing from the gateway. The
 * `subscription.ready` frame of each stream connect gives the whole team of `sid`, and each
 * `session.team_updated` after it changes rows; the session hub keeps both. Before the first
 * ready frame, the last team that this device knew stands in. The rows of the transcript turns
 * (`stored`) are under them, so a team of an older turn still shows. A newer source replaces
 * the row of an older one. `clocks` has the stored clock of each team by leader turn: the clocks
 * of the transcript turns (`storedTimes`), then the clocks of the stream over them. The row of a
 * child that the stream reported as deleted shows from no source.
 */
export function useAgentTeam(
  client: GatewayClient,
  sid: string,
  stored: readonly Subagent[] = NO_AGENTS,
  storedTimes: ReadonlyMap<string, TeamClock> = NO_CLOCKS,
) {
  const hub = useContext(SessionEventsContext);
  const subscribe = useCallback(
    (changed: () => void) => (hub ? hub.subscribeTeam(sid, changed) : NO_SUBSCRIPTION()),
    [hub, sid],
  );
  const streamed = useSyncExternalStore(subscribe, () => hub?.team(sid));
  const streamedClocks = useSyncExternalStore(subscribe, () => hub?.teamClocks(sid));
  const clocks = useMemo(
    () => (streamedClocks?.size ? new Map([...storedTimes, ...streamedClocks]) : storedTimes),
    [storedTimes, streamedClocks],
  );
  const gone = useSyncExternalStore(subscribe, () => hub?.teamGone(sid));
  const current = useMemo(() => {
    const kept = gone?.size ? stored.filter((row) => !gone.has(row.session_id)) : stored;
    // The stream rows are newer than the transcript rows. The cached rows of an earlier visit are older.
    if (streamed) {
      if (!streamed.length || !kept.length) return streamed.length ? streamed : kept;
      return mergeAgents(kept, streamed);
    }
    const cached = client.cachedAgents?.(sid);
    return cached?.length ? mergeAgents(cached, kept) : kept;
  }, [client, sid, stored, streamed, gone]);
  // A person stops subagents only through their leader: the stop of a leader turn also stops them.
  return { agents: current, clocks };
}

/** What `useAgentTeam` gives its views. */
export type AgentTeam = ReturnType<typeof useAgentTeam>;
