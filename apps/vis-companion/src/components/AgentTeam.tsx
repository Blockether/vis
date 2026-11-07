import {
  type CSSProperties,
  type ReactNode,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
  useSyncExternalStore,
} from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import { mergeAgents, type GatewayClient } from '../lib/gateway';
import type { Subagent, TranscriptIteration, TranscriptTurn } from '../lib/types';
import { councilLine, councilMarkdown } from '../lib/council-markdown';
import { SessionEventsContext } from '../lib/session-events';
import { axisLabel, axisMarks, formatDuration as formatAgentTime } from '../lib/time-axis';
export { axisMarks, formatAgentTime };
import { Markdown } from './ChatContent';
import { Button, HoverNote } from './ui';

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
  ['audit', 'text-role-audit'],
]);

/** The role word of an agent, with its ink. The views write the word in capitals. */
export function agentRole(agent: Subagent): { text: string; ink: string } {
  const role = agent.role?.trim() || NO_ROLE;
  return { text: role, ink: ROLE_INK.get(role) ?? 'text-dialog-hint' };
}

export const isLive = (agent: Subagent) => ['queued', 'running'].includes(agent.status);

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
 * The time of the current task in ms: the time so far while the agent runs, the
 * duration when its task ended. A queued agent has no time yet.
 */
export function agentTime(agent: Subagent, now: number): number | undefined {
  const start = startOf(agent);
  if (start === undefined || agent.status === 'queued') return undefined;
  if (agent.status === 'running') return Math.max(0, now - start);
  return agent.finished_at == null ? undefined : Math.max(0, agent.finished_at - start);
}

/**
 * The update of a row. While the agent works: its newest progress note, the note that its
 * leader also reads, else its latest Activity line, else the first line of its task. After
 * its task: its result, else the reason of its end. A note, a result and a task are Markdown,
 * so the row shows them without their markers.
 */
export function agentUpdate(agent: Subagent): string | undefined {
  if (!isLive(agent)) return agent.result == null ? END_REASON[agent.status] : councilLine(agent.result);
  if (agent.note != null) return councilLine(agent.note);
  return agent.activity ?? (councilLine(agent.task.trim().split('\n', 1)[0]) || undefined);
}

/** One count of a team line, such as `1 running`, with its ink. */
export interface TeamCount {
  text: string;
  count: number;
  ink: string;
}

// The order of the status counts: the work first, then the ends.
const STATUS_ORDER = ['running', 'queued', 'done', 'failed'];

function counted(words: readonly { text: string; ink: string }[], order: readonly string[]): TeamCount[] {
  const counts = new Map<string, TeamCount>();
  for (const word of words) counts.set(word.text, { ...word, count: (counts.get(word.text)?.count ?? 0) + 1 });
  const rank = (text: string) => (order.includes(text) ? order.indexOf(text) : order.length);
  return [...counts.values()].sort((a, b) => rank(a.text) - rank(b.text));
}

/** The agents counted by status word, the work first. */
export function teamCounts(agents: readonly Subagent[]): TeamCount[] {
  return counted(agents.map(agentStatus), STATUS_ORDER);
}

const marks = (values: readonly (number | null | undefined)[]) => values.filter((mark): mark is number => typeof mark === 'number');

/**
 * The time of a team in ms: from the spawn of its first agent to now while one of its agents
 * works or waits, else to the end of its last agent. Undefined without a spawn or an end.
 */
export function teamTime(agents: readonly Subagent[], now: number): number | undefined {
  const starts = marks(agents.map((agent) => agent.created_at));
  if (starts.length === 0) return undefined;
  const start = Math.min(...starts);
  if (agents.some(isLive)) return Math.max(0, now - start);
  const ends = marks(agents.map((agent) => agent.finished_at));
  return ends.length ? Math.max(0, Math.max(...ends) - start) : undefined;
}

// A hairline between the parts of a team line.
function Divider() {
  return <span aria-hidden="true" className="h-3 w-px shrink-0 bg-dialog-edge" data-team-divider />;
}

function Counts({ counts, id }: { counts: readonly TeamCount[]; id?: string }) {
  return (
    <span id={id} className="flex flex-wrap gap-x-3">
      {counts.map(({ text, count, ink }) => (
        <span key={text} className={`whitespace-nowrap uppercase tabular-nums ${ink}`}>{`${count} ${text}`}</span>
      ))}
    </span>
  );
}

/**
 * A team on one line: its time, then its counts by status: `TEAM | 3m 20s  1 RUNNING  2 DONE`.
 * `action` stands right before the counts. `labelId` names the word, the time and the counts
 * `${labelId}-team`, `${labelId}-time` and `${labelId}-counts`, so a control can take them as its name.
 */
export function TeamLine({ agents, action, labelId }: { agents: readonly Subagent[]; action?: ReactNode; labelId?: string }) {
  const now = useNow(agents.some(isLive));
  const time = teamTime(agents, now);
  return (
    <span className="flex min-w-0 flex-1 flex-wrap items-center gap-x-3 sm:min-h-5" data-team-line>
      <span id={labelId && `${labelId}-team`} className="font-bold uppercase text-white">team</span>
      {time !== undefined && <Divider />}
      {time !== undefined && (
        <span id={labelId && `${labelId}-time`} className="whitespace-nowrap text-dialog-hint tabular-nums" data-team-time>
          {formatAgentTime(time)}
        </span>
      )}
      <span className="flex flex-wrap items-center gap-x-3 sm:ml-auto">
        {action}
        {action && <Divider />}
        <Counts counts={teamCounts(agents)} id={labelId && `${labelId}-counts`} />
      </span>
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

/** The control that swaps the notes of a team for its timeline, and back. */
export function TimelineToggle({ isOn, onToggle }: { isOn: boolean; onToggle: () => void }) {
  return (
    <Button variant="secondary" density="inline" aria-pressed={isOn} onClick={onToggle} className="pointer-events-auto">
      {isOn ? 'Close timeline' : 'Open timeline'}
    </Button>
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
// title, and the team can show its timeline.
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

// A leader step that waits for its agents. `council.run` and `council.wait` return only when an
// agent ends, asks or writes, so the time of such a block is the time that the leader waited.
const WAITS_FOR_AGENTS = /\bcouncil\s*\.\s*(?:run|wait)\s*\(/;

/**
 * The leader lane of a turn from its steps. A step that runs or waits for agents is a wait from
 * its start for its duration; a running step of a live turn waits to now. A step that the client
 * did not load counts as work. The note is the newest line of the leader. Undefined without a start.
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
  for (const form of iterations.flatMap((iteration) => iteration.forms ?? [])) {
    const at = form.started_at_ms;
    if (typeof at !== 'number' || !WAITS_FOR_AGENTS.test(form.code ?? '')) continue;
    if (typeof form.duration_ms === 'number') waits.push({ start: at, end: at + form.duration_ms });
    else if (end == null) waits.push({ start: at });
  }
  return { start, end: end ?? undefined, waits: waits.sort((a, b) => a.start - b.start), note: leaderNote(iterations, end != null) };
}

/** The leader lane of a stored turn. Undefined when the turn gives no start or no end. */
export function storedLeaderLane(turn: TranscriptTurn): LeaderLane | undefined {
  const start = turn.created_at;
  const end = turn.completed_at ?? (start != null && turn.duration_ms != null ? start + turn.duration_ms : undefined);
  return end == null ? undefined : leaderLane(start, end, turn.iterations);
}

/**
 * The time of the chart of a team, in epoch ms: from the first current task or the start of the
 * leader, to now while anything works, else to the last end. A resumed agent counts from its
 * resume, so an old spawn does not push the current work to the end of the axis.
 */
export function chartSpan(rows: readonly Subagent[], now: number, leader?: LeaderLane): Required<Span> | undefined {
  const starts = marks([...rows.map((agent) => (agent.status === 'queued' ? agent.created_at : startOf(agent))), leader?.start]);
  if (starts.length === 0) return undefined;
  const start = Math.min(...starts);
  const ends = marks([...rows.map((agent) => agent.finished_at), leader?.end]);
  const isWorking = rows.some(isLive) || (leader != null && leader.end == null);
  const end = isWorking ? Math.max(now, ...ends) : ends.length ? Math.max(...ends) : undefined;
  return end != null && end > start ? { start, end } : undefined;
}

/** Where a part of a lane stands on the chart: its start and end, from 0 to 1. */
export interface AgentBar {
  start: number;
  end: number;
  isWaiting: boolean;
}

const shareOf = ({ start, end }: Required<Span>) => (at: number) => Math.min(1, Math.max(0, (at - start) / (end - start)));

/**
 * The bar of each agent on the chart of its team, by session id. A task goes from its start to
 * its end, or to now while it runs. A queued agent waits from its spawn to the end of the chart.
 * An agent without the marks of its task has no bar.
 */
export function agentBars(rows: readonly Subagent[], now: number, leader?: LeaderLane): Map<string, AgentBar> {
  const bars = new Map<string, AgentBar>();
  const span = chartSpan(rows, now, leader);
  if (!span) return bars;
  const share = shareOf(span);
  for (const agent of rows) {
    const start = agent.status === 'queued' ? agent.created_at : startOf(agent);
    const end = agent.status === 'queued' ? span.end : agent.status === 'running' ? now : agent.finished_at;
    if (start == null || end == null) continue;
    bars.set(agent.session_id, { start: share(start), end: share(end), isWaiting: agent.status === 'queued' });
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

/** The key of an agent on the chart: `A1`, `A2`, by spawn order. The leader is `L`. */
export const agentKey = (index: number) => `A${index + 1}`;
const LEADER_KEY = 'L';
/** The role word of the leader and its ink, in the team chart and the header path. */
export const LEADER_ROLE = 'leader';
export const LEADER_INK = 'text-role-leader';
const LEADER_WAIT_WIDTH = 'waited 59m 59s of 59m 59s'.length;

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

// The columns of a team on a phone: ROLE, TITLE and STATUS, then UPDATE and TIME on the second
// line. ROLE fits the longest role word of the contract.
const PHONE_ROW = 'grid w-full min-w-0 grid-cols-[var(--role)_minmax(0,1fr)_auto] items-baseline gap-x-3 py-1 text-left';

// The columns of a team on a wide screen: ROLE, TITLE, then the NOTE, or the time bar while the
// timeline shows, then TIME and STATUS. A hairline stands after ROLE, TITLE, NOTE and TIME.
// TIME fits `59m 59s` and STATUS fits `running`, so a ticking time never moves a column. TITLE takes at most a third
// of the row; a longer title wraps. A wide row carries the frame's own inset, so its hover fills
// the frame and its text lines up with the team line.
const WIDE_ROW = 'grid w-full min-w-0 grid-cols-[var(--role)_min(var(--title),32%)_minmax(0,1fr)_7ch_7ch] items-baseline gap-x-3 px-3';

// The hairline after a cell: in the middle of the gap, through the row inset, so the rows join it.
const CELL_RULE = 'relative self-stretch after:absolute after:-inset-y-1 after:-right-1.5 after:w-px after:bg-dialog-edge';

const FOCUS = 'rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent';

// The widths of the columns, in `ch` of the monospace font. ROLE is the same for every team. TITLE
// fits the team: its heading is the least width, and the bold title gets one more `ch`.
function columnWidths(rows: readonly Subagent[]): CSSProperties {
  const widest = (least: string, words: string[]) => Math.max(least.length, ...words.map((word) => word.length));
  return {
    '--role': `${ROLE_WIDTH}ch`,
    '--title': `${widest('title', rows.map((agent) => agentLabel(agent, rows))) + 1}ch`,
  } as CSSProperties;
}

// The columns of the chart of a team: the KEY of each lane, its time BAR, its TIME and its
// STATUS. The legend under the chart gives each KEY its ROLE, TITLE and the NOTE of its row.
const CHART_ROW = 'grid w-full min-w-0 grid-cols-[var(--key)_minmax(0,1fr)_7ch_7ch] items-baseline gap-x-3 px-3';
const LEGEND_ROW = 'grid w-full min-w-0 grid-cols-[var(--key)_var(--role)_min(var(--title),32%)_minmax(0,1fr)] items-baseline gap-x-3 px-3';

// The bar column of a chart row: the inset of the row, the KEY and one gap on the left; one gap,
// TIME, one gap, STATUS and the inset on the right. The grid lines stand there behind the bars.
const BAR_COLUMN: CSSProperties = { left: 'calc(1.5rem + var(--key))', right: 'calc(2.25rem + 14ch)' };

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

// One part of a lane: a solid bar for work, a dashed line for a wait.
function LanePart({ bar, ink }: { bar: AgentBar; ink: string }) {
  return (
    <span
      className={`absolute ${bar.isWaiting ? 'inset-y-1/2 border-t border-dashed border-dialog-hint' : `inset-y-0 ${ink}`}`}
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
  /** A wide screen shows the time bars of the team instead of the notes. */
  isTimeline?: boolean;
  /** The leader turn, drawn as the first lane of the chart. */
  leader?: LeaderLane;
}

// The table of one team. A row opens the sheet of its agent. A wide screen shows the note of
// each agent beside its title, or the time bars of the team on one axis.
function AgentTable(props: TableProps) {
  return useWide() ? <WideAgentTable {...props} /> : <PhoneAgentTable {...props} />;
}

function PhoneAgentTable({ rows, now, onSelect }: TableProps) {
  return (
    <ul className="divide-y divide-dialog-edge/40" style={columnWidths(rows)}>
      {rows.map((agent) => {
        const role = agentRole(agent);
        const status = agentStatus(agent);
        const update = agentUpdate(agent);
        const time = agentTime(agent, now);
        return (
          <li key={agent.session_id}>
            <button type="button" data-agent-row={agent.session_id} onClick={() => onSelect(agent.session_id)} className={`group ${FOCUS} ${PHONE_ROW}`}>
              <span className={`order-1 uppercase ${role.ink}`}>{role.text}</span>
              <HoverNote className="order-2 min-w-0" note={taskNote(agent.task)}>
                <span className="font-bold [overflow-wrap:anywhere] group-hover:underline">{agentLabel(agent, rows)}</span>
              </HoverNote>
              {update && (
                <HoverNote className="order-4 col-span-2 min-w-0" note={update}>
                  <span className="line-clamp-2 text-dialog-hint [overflow-wrap:anywhere]">{update}</span>
                </HoverNote>
              )}
              <span className={`order-3 whitespace-nowrap text-right uppercase ${status.ink}`}>{status.text}</span>
              <span className="order-5 col-start-3 whitespace-nowrap text-right text-dialog-hint tabular-nums">
                {time === undefined ? '' : formatAgentTime(time)}
              </span>
            </button>
          </li>
        );
      })}
    </ul>
  );
}

function WideAgentTable(props: TableProps) {
  const { rows, now, onSelect, isTimeline = false, leader } = props;
  if (isTimeline && hasTimeline(rows, leader)) return <TeamChart {...props} />;
  return (
    <div style={columnWidths(rows)}>
      <ul>
        {rows.map((agent) => {
          const role = agentRole(agent);
          const status = agentStatus(agent);
          const update = agentUpdate(agent);
          const time = agentTime(agent, now);
          return (
            <li key={agent.session_id}>
              <button
                type="button"
                data-agent-row={agent.session_id}
                onClick={() => onSelect(agent.session_id)}
                className={`group py-1 text-left hover:bg-white/5 ${FOCUS} ${WIDE_ROW}`}
              >
                <span className={`uppercase ${CELL_RULE} ${role.ink}`}>{role.text}</span>
                <HoverNote className={`min-w-0 ${CELL_RULE}`} note={taskNote(agent.task)}>
                  <span className="font-bold [overflow-wrap:anywhere] group-hover:underline">{agentLabel(agent, rows)}</span>
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

// The chart of a team on a wide screen: the time axis, the lane of the leader, then one lane for
// each agent in spawn order, on one scale. A lane shows only its key; the legend under the rule
// gives each key its role and title. An agent lane opens the sheet of its agent.
function TeamChart({ rows, now, onSelect, leader }: TableProps) {
  const span = chartSpan(rows, now, leader);
  if (!span) return null;
  const total = span.end - span.start;
  const share = shareOf(span);
  const bars = agentBars(rows, now, leader);
  const parts = leader ? leaderParts(leader, now) : [];
  const waited = parts.reduce((sum, part) => sum + (part.isWaiting ? part.end - part.start : 0), 0);
  const leaderTime = leader ? (leader.end ?? now) - leader.start : 0;
  const keys = rows.map((_, index) => agentKey(index));
  const titles = columnWidths(rows) as Record<string, string>;
  const widths = {
    ...titles,
    // The leader line `waited 59m 59s of 59m 59s` keeps one width while its clock ticks.
    '--title': leader ? `max(${titles['--title']}, ${LEADER_WAIT_WIDTH}ch)` : titles['--title'],
    '--key': `${Math.max(2, ...keys.map((key) => key.length))}ch`,
    '--role': `${Math.max(leader ? LEADER_ROLE.length : 0, ROLE_WIDTH)}ch`,
  } as CSSProperties;
  return (
    <div style={widths} className="pb-1" data-team-chart>
      <div aria-hidden className={`pt-1 text-dialog-hint ${CHART_ROW}`}>
        <span className="relative col-start-2 h-[calc(1lh+0.25rem)] border-b border-dialog-edge/50"><AxisLabels total={total} /></span>
      </div>
      <div className="relative">
        <GridLines total={total} />
        <ul aria-label="Timeline">
          {leader && (
            <li className={`py-1 ${CHART_ROW}`} data-leader-lane>
              <span className={`font-bold ${LEADER_INK}`}>{LEADER_KEY}<span className="sr-only"> {LEADER_ROLE}</span></span>
              <span aria-hidden className="relative h-2 self-center">
                {parts.map((part) => (
                  <LanePart key={part.start} bar={{ start: share(part.start), end: share(part.end), isWaiting: part.isWaiting }} ink="bg-white" />
                ))}
              </span>
              <span className="whitespace-nowrap text-right text-dialog-hint tabular-nums">{formatAgentTime(leaderTime)}</span>
              <StatusWord status={agentStatus({ status: leader.end == null ? 'running' : 'completed' } as Subagent)} />
            </li>
          )}
          {rows.map((agent, index) => {
            const role = agentRole(agent);
            const status = agentStatus(agent);
            const time = agentTime(agent, now);
            const bar = bars.get(agent.session_id);
            return (
              <li key={agent.session_id}>
                <button
                  type="button"
                  data-agent-row={agent.session_id}
                  onClick={() => onSelect(agent.session_id)}
                  className={`group py-1 text-left hover:bg-white/5 ${FOCUS} ${CHART_ROW}`}
                >
                  <span className={`group-hover:underline ${role.ink}`}>
                    {keys[index]}
                    <span className="sr-only"> {agentLabel(agent, rows)}</span>
                  </span>
                  <span aria-hidden className="relative h-2 self-center">{bar && <LanePart bar={bar} ink={BAR_INK[status.text]} />}</span>
                  <span className="whitespace-nowrap text-right text-dialog-hint tabular-nums">{time === undefined ? '' : formatAgentTime(time)}</span>
                  <StatusWord status={status} />
                </button>
              </li>
            );
          })}
        </ul>
      </div>
      <ul aria-label="Legend" className="mt-1 border-t border-dialog-edge/50 pt-1.5">
        {leader && (
          <li className={LEGEND_ROW} data-hover-note-frame>
            <span className={`font-bold ${LEADER_INK}`}>{LEADER_KEY}</span>
            <span className={`font-bold uppercase ${LEADER_INK}`}>{LEADER_ROLE}</span>
            <span className="whitespace-nowrap text-dialog-hint">{`waited ${formatAgentTime(waited)} of ${formatAgentTime(leaderTime)}`}</span>
            <HoverNote className="min-w-0" note={leader.note}>
              <span className="line-clamp-1 [overflow-wrap:anywhere] text-dialog-hint">{leader.note ?? ''}</span>
            </HoverNote>
          </li>
        )}
        {rows.map((agent, index) => {
          const role = agentRole(agent);
          const update = agentUpdate(agent);
          return (
            <li key={agent.session_id} className={LEGEND_ROW} data-hover-note-frame>
              <span className={role.ink}>{keys[index]}</span>
              <span className={`uppercase ${role.ink}`}>{role.text}</span>
              <HoverNote className="min-w-0" note={taskNote(agent.task)}>
                <span className="font-bold [overflow-wrap:anywhere]">{agentLabel(agent, rows)}</span>
              </HoverNote>
              <HoverNote className="min-w-0" note={update}>
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
  /** The leader turn of the team, for the first lane of its chart. */
  leader?: LeaderLane;
}

/**
 * The subagents of one turn as a table in a frame: the team line with the counts by role and
 * by status, then one row for each agent in spawn order. A row opens the session of its
 * agent. The dock shows the table without the frame. A subagent never asks a person: its
 * leader answers it, so a row has no question and no control for one.
 */
export function TurnAgents({ agents, onOpen, leader, placement = 'transcript', isTimeline }: TeamProps & {
  /** The dock above the composer while the team works, or the turn of the transcript after. */
  placement?: 'dock' | 'transcript';
  /** The dock owns the timeline switch of its teams; the transcript owns its own. */
  isTimeline?: boolean;
}) {
  const [isOwnTimeline, setOwnTimeline] = useState(false);
  const isWide = useWide();
  const now = useNow(agents.some((agent) => agent.status === 'running') || (leader != null && leader.end == null));
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const table = <AgentTable rows={rows} now={now} onSelect={onOpen} isTimeline={isTimeline ?? isOwnTimeline} leader={leader} />;
  const toggle = isWide && hasTimeline(rows, leader)
    ? <TimelineToggle isOn={isOwnTimeline} onToggle={() => setOwnTimeline((value) => !value)} />
    : undefined;
  return placement === 'transcript' ? (
    <section
      aria-label="Subagents"
      className="my-3 border border-dialog-edge font-mono text-meta text-white sm:text-ui"
      data-transcript-part
    >
      <div className="flex border-b border-dialog-edge bg-panel px-3 py-1 sm:py-1.5">
        <TeamLine agents={rows} action={toggle} />
      </div>
      <div className="px-3 sm:px-0">{table}</div>
    </section>
  ) : table;
}

/** The team rows that the transcript turns hold: each turn has the agents that it started. */
export function storedAgents(turns: readonly TranscriptTurn[]): Subagent[] {
  return turns.flatMap((turn) => turn.agents ?? []);
}

/** The session's subagents grouped by the turn that started them (`team_id`). */
export function agentsByTurn(agents: readonly Subagent[]): ReadonlyMap<string, Subagent[]> {
  const byTurn = new Map<string, Subagent[]>();
  for (const agent of agents) byTurn.set(agent.team_id, [...(byTurn.get(agent.team_id) ?? []), agent]);
  return byTurn;
}

/**
 * The teams of `sid` in the dock: the team of the running turn, and each team that
 * still has a queued or running agent after its turn ended. Spawn order.
 */
export function dockTeams(agents: readonly Subagent[], sid: string, runningTurnId?: string | null): Subagent[][] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) {
    if (agent.parent_id === sid) teams.set(agent.team_id, [...(teams.get(agent.team_id) ?? []), agent]);
  }
  return [...teams.entries()]
    .filter(([id, team]) => id === runningTurnId || team.some(isLive))
    .map(([, team]) => team);
}

/** No agents: one value, so a default argument keeps its identity. */
const NO_AGENTS: readonly Subagent[] = [];

const NO_SUBSCRIPTION = () => () => {};

/**
 * One reader for a session's subagents. It reads nothing from the gateway. The
 * `subscription.ready` frame of each stream connect gives the whole team of `sid`, and each
 * `session.team_updated` after it changes rows; the session hub keeps both. Before the first
 * ready frame, the last team that this device knew stands in. The rows of the transcript turns
 * (`stored`) are under them, so a team of an older turn still shows. A newer source replaces
 * the row of an older one.
 */
export function useAgentTeam(client: GatewayClient, sid: string, stored: readonly Subagent[] = NO_AGENTS) {
  const hub = useContext(SessionEventsContext);
  const subscribe = useCallback(
    (changed: () => void) => (hub ? hub.subscribeTeam(sid, changed) : NO_SUBSCRIPTION()),
    [hub, sid],
  );
  const streamed = useSyncExternalStore(subscribe, () => hub?.team(sid));
  const current = useMemo(() => {
    // The stream rows are newer than the transcript rows. The cached rows of an earlier visit are older.
    if (streamed) {
      if (!streamed.length || !stored.length) return streamed.length ? streamed : stored;
      return mergeAgents(stored, streamed);
    }
    const cached = client.cachedAgents?.(sid);
    return cached?.length ? mergeAgents(cached, stored) : stored;
  }, [client, sid, stored, streamed]);
  // A person stops subagents only through their leader: the stop of a leader turn also stops them.
  return { agents: current };
}

/** What `useAgentTeam` gives its views. */
export type AgentTeam = ReturnType<typeof useAgentTeam>;
