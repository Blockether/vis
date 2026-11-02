import {
  type CSSProperties,
  useCallback,
  useContext,
  useEffect,
  useEffectEvent,
  useId,
  useLayoutEffect,
  useRef,
  useState,
  useSyncExternalStore,
} from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import type { GatewayClient } from '../lib/gateway';
import type { AgentTimeline, AgentTimelineItem, Subagent } from '../lib/types';
import { councilMarkdown } from '../lib/council-markdown';
import { SessionEventsContext } from '../lib/session-events';
import { useClampMotion } from '../lib/clamp-motion';
import { Markdown } from './ChatContent';
import { CheckIcon, ChevronIcon, CloseIcon, MARK_NUDGE } from './icons';
import { Button, DialogFrame, Modal, TextButton } from './ui';

/** The roles of the agents contract, in its order. A role is a label: it sets no limit. */
export const ROLES: readonly { const: string; title: string }[] = agentsSchema.$defs.role.oneOf;

// The word of an agent without a role, in its row.
const NO_ROLE = 'agent';

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
 * The status word of an agent, with the theme ink that agrees with it: `queued`, `running`,
 * `done` or `failed`. The views write the word in capitals. A failed verdict, a cancel and a
 * spent budget are all `failed`; the update of the row tells which. A question to its leader
 * is no status: the leader answers it, not a person.
 */
export function agentStatus(agent: Subagent): { text: string; ink: string } {
  switch (agent.status) {
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

/** `45s`, `3m 04s`, `1h 02m`. */
export function formatAgentTime(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ${String(seconds % 60).padStart(2, '0')}s`;
  return `${Math.floor(minutes / 60)}h ${String(minutes % 60).padStart(2, '0')}m`;
}

/**
 * The update of a row. While the agent works: its newest progress note, the note that its
 * leader also reads, else its latest Activity line, else the first line of its task. After
 * its task: its result, else the reason of its end.
 */
export function agentUpdate(agent: Subagent): string | undefined {
  if (!isLive(agent)) return agent.result ?? END_REASON[agent.status];
  return agent.note ?? agent.activity ?? (agent.task.trim().split('\n', 1)[0].trim() || undefined);
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

function Counts({ counts, className = '' }: { counts: readonly TeamCount[]; className?: string }) {
  return (
    <span className={`flex flex-wrap gap-x-3 ${className}`}>
      {counts.map(({ text, count, ink }) => (
        <span key={text} className={`whitespace-nowrap uppercase tabular-nums ${ink}`}>{`${count} ${text}`}</span>
      ))}
    </span>
  );
}

/** A team on one line: its time, then its counts by status: `TEAM  3m 20s  1 RUNNING  2 DONE`. */
export function TeamLine({ agents }: { agents: readonly Subagent[] }) {
  const now = useNow(agents.some(isLive));
  const time = teamTime(agents, now);
  return (
    <span className="flex min-w-0 flex-1 flex-wrap items-baseline gap-x-4" data-team-line>
      <span className="font-bold uppercase text-white">team</span>
      {time !== undefined && <span className="whitespace-nowrap text-dialog-hint tabular-nums" data-team-time>{formatAgentTime(time)}</span>}
      <Counts counts={teamCounts(agents)} className="sm:ml-auto" />
    </span>
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

// A wide screen: the `sm` width of the theme. A row there unfolds in place, and on a phone it
// opens the sheet of its agent.
const WIDE = '(min-width: 640px)';

function useWide(): boolean {
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

/** Where the current task of an agent stands on the team time: its start and end, from 0 to 1. */
export interface AgentBar {
  start: number;
  end: number;
  isWaiting: boolean;
}

/**
 * The bar of each agent on the time of its team, by session id. A task goes from its start to
 * its end, or to now while it runs. A queued agent waits from its spawn to the end of the team.
 * An agent without the marks of its task has no bar.
 */
export function agentBars(rows: readonly Subagent[], now: number): Map<string, AgentBar> {
  const bars = new Map<string, AgentBar>();
  const starts = marks(rows.map((agent) => agent.created_at));
  const total = teamTime(rows, now);
  if (starts.length === 0 || !total) return bars;
  const from = Math.min(...starts);
  const share = (at: number) => Math.min(1, Math.max(0, (at - from) / total));
  for (const agent of rows) {
    const start = agent.status === 'queued' ? agent.created_at : startOf(agent);
    const end = agent.status === 'queued' ? from + total : agent.status === 'running' ? now : agent.finished_at;
    if (start == null || end == null) continue;
    bars.set(agent.session_id, { start: share(start), end: share(end), isWaiting: agent.status === 'queued' });
  }
  return bars;
}

// The steps of the time axis, in seconds. The axis takes the least step that gives at most five marks.
const AXIS_STEPS = [10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200];

/** The marks of a time axis of `ms`, in ms from 0: at most five, on a round step. */
export function axisMarks(ms: number): number[] {
  const seconds = ms / 1000;
  const step = AXIS_STEPS.find((each) => seconds / each < 5) ?? Math.ceil(seconds / 4 / 3600) * 3600;
  return Array.from({ length: Math.floor(seconds / step) + 1 }, (_, index) => index * step * 1000);
}

const axisLabel = (ms: number) => `${Math.floor(ms / 60_000)}:${pad(Math.floor(ms / 1000) % 60)}`;

// The bar ink of each status word.
const BAR_INK: Record<string, string> = {
  running: 'bg-accent-ink',
  done: 'bg-ok-ink',
  failed: 'bg-err-ink',
};

// The mark of a status word on a wide screen. The word stays for a screen reader.
function StatusMark({ status }: { status: string }) {
  if (status === 'done') return <CheckIcon className="size-3 text-ok-ink" />;
  if (status === 'failed') return <CloseIcon className="size-3 text-err-ink" />;
  if (status === 'queued') return <span className="inline-block size-2 rounded-full border border-dialog-hint" />;
  return <span className="inline-block size-2 rounded-full bg-accent-ink motion-safe:animate-pulse" />;
}

// The columns of a team on a phone: ROLE, TITLE and STATUS, then UPDATE and TIME on the second
// line. ROLE fits the longest word of the team.
const PHONE_ROW = 'grid w-full min-w-0 grid-cols-[var(--role)_minmax(0,1fr)_auto] items-baseline gap-x-3 py-1 text-left';

// The columns of a team on a wide screen: the chevron, ROLE, TITLE, the time bar, TIME and the
// status mark, and the UPDATE of an unfolded row below them. TIME fits `59m 59s`, so a ticking
// time never moves a column. TITLE takes at most a third of the row; a longer title wraps.
const WIDE_ROW = 'grid w-full min-w-0 grid-cols-[0.75rem_var(--role)_min(var(--title),32%)_minmax(0,1fr)_7ch_1rem] items-baseline gap-x-3 px-2';

const FOCUS = 'rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent';

// The widths of the columns that fit the team, in `ch` of the monospace font. A heading is the
// least width of its column; the bold title gets one more `ch`.
function columnWidths(rows: readonly Subagent[]): CSSProperties {
  const widest = (least: string, words: string[]) => Math.max(least.length, ...words.map((word) => word.length));
  return {
    '--role': `${widest('role', rows.map((agent) => agentRole(agent).text))}ch`,
    '--title': `${widest('title', rows.map((agent) => agentLabel(agent, rows))) + 1}ch`,
  } as CSSProperties;
}

// The time axis above the bars, as a ruler: a label over a tick at each mark.
function AxisMarks({ total }: { total: number }) {
  return axisMarks(total).map((at, index) => (
    <span key={at} className="absolute inset-y-0" style={{ left: `${(at / total) * 100}%` }}>
      <span className={`absolute top-0 ${index === 0 ? '' : '-translate-x-1/2'}`}>{axisLabel(at)}</span>
      <span className="absolute bottom-0 h-1 w-px bg-dialog-edge/50" />
    </span>
  ));
}

interface TableProps {
  rows: readonly Subagent[];
  now: number;
  error?: string | null;
  onSelect: (sid: string) => void;
}

// The table of one team. A phone row opens the sheet of its agent. A wide row unfolds its update,
// and its time bar opens the sheet.
function AgentTable(props: TableProps) {
  return useWide() ? <WideAgentTable {...props} /> : <PhoneAgentTable {...props} />;
}

function PhoneAgentTable({ rows, now, error, onSelect }: TableProps) {
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
              <span className="order-2 font-bold [overflow-wrap:anywhere] group-hover:underline" title={agent.task}>
                {agentLabel(agent, rows)}
              </span>
              {update && (
                <span className="order-4 col-span-2 line-clamp-2 text-dialog-hint [overflow-wrap:anywhere]" title={update}>{update}</span>
              )}
              <span className={`order-3 whitespace-nowrap text-right uppercase ${status.ink}`}>{status.text}</span>
              <span className="order-5 col-start-3 whitespace-nowrap text-right text-dialog-hint tabular-nums">
                {time === undefined ? '' : formatAgentTime(time)}
              </span>
            </button>
          </li>
        );
      })}
      {error && <li role="alert" className="py-1 text-err-ink">{error}</li>}
    </ul>
  );
}

function WideAgentTable({ rows, now, error, onSelect }: TableProps) {
  // The rows that a person folded or unfolded. Any other row is unfolded while its agent runs.
  const [folds, setFolds] = useState<ReadonlyMap<string, boolean>>(() => new Map());
  const total = teamTime(rows, now);
  const bars = agentBars(rows, now);
  return (
    <div style={columnWidths(rows)}>
      {rows.length > 1 && !!total && (
        <div aria-hidden className={`pt-1 text-dialog-hint ${WIDE_ROW}`}>
          <span className="relative col-start-4 h-[calc(1lh+0.5rem)] border-b border-dialog-edge/50"><AxisMarks total={total} /></span>
        </div>
      )}
      <ul>
        {rows.map((agent) => {
          const role = agentRole(agent);
          const status = agentStatus(agent);
          const update = agentUpdate(agent);
          const time = agentTime(agent, now);
          const bar = bars.get(agent.session_id);
          const label = agentLabel(agent, rows);
          const isOpen = folds.get(agent.session_id) ?? agent.status === 'running';
          const toggle = () => setFolds((before) => new Map(before).set(agent.session_id, !isOpen));
          return (
            <li key={agent.session_id} className={`py-1 hover:bg-white/5 ${WIDE_ROW}`}>
              <button
                type="button"
                data-agent-row={agent.session_id}
                aria-expanded={isOpen}
                onClick={toggle}
                className={`group col-span-3 grid grid-cols-subgrid items-baseline text-left ${FOCUS}`}
              >
                <ChevronIcon open={isOpen} className={`size-3 text-dialog-hint ${MARK_NUDGE}`} />
                <span className={`uppercase ${role.ink}`}>{role.text}</span>
                <span className="font-bold [overflow-wrap:anywhere] group-hover:underline" title={agent.task}>{label}</span>
              </button>
              <button
                type="button"
                aria-label={`Open ${label}`}
                title={`Open ${label}`}
                onClick={() => onSelect(agent.session_id)}
                className={`group/bar relative h-[1lh] cursor-pointer self-start ${FOCUS}`}
              >
                {bar && (
                  <span
                    className={`absolute top-1/2 -translate-y-1/2 transition-[height] motion-reduce:transition-none ${bar.isWaiting ? 'border-t border-dashed border-dialog-hint group-hover/bar:border-white' : `h-2 group-hover/bar:h-3.5 ${BAR_INK[status.text]}`}`}
                    style={{ left: `${bar.start * 100}%`, width: `max(2px, ${(bar.end - bar.start) * 100}%)` }}
                  />
                )}
              </button>
              <span className="whitespace-nowrap text-right text-dialog-hint tabular-nums">{time === undefined ? '' : formatAgentTime(time)}</span>
              <span className={`self-center text-center ${status.ink}`}>
                <span className="sr-only">{status.text}</span>
                <span aria-hidden className="inline-flex"><StatusMark status={status.text} /></span>
              </span>
              {isOpen && update && (
                <span className="col-[3/-1] line-clamp-3 text-dialog-hint [overflow-wrap:anywhere]" title={update}>{update}</span>
              )}
            </li>
          );
        })}
        {error && <li role="alert" className="py-1 text-err-ink">{error}</li>}
      </ul>
    </div>
  );
}

/** The props of a team block. `timeline` reads the steps of one child. */
export interface TeamProps {
  agents: readonly Subagent[];
  pending: string | null;
  error?: string | null;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  timeline: (child: string) => Promise<AgentTimeline>;
}

/**
 * The subagents of one turn as a table in a frame: the team line with the counts by role and
 * by status, then one row for each agent in spawn order. A row opens the timeline of its
 * agent. The dock shows the table without the frame. A subagent never asks a person: its
 * leader answers it, so a row has no question and no control for one.
 */
export function TurnAgents({ agents, pending, error, onOpen, onCancel, timeline, placement = 'transcript' }: TeamProps & {
  /** The dock above the composer while the team works, or the turn of the transcript after. */
  placement?: 'dock' | 'transcript';
}) {
  const [selected, setSelected] = useState<string | null>(null);
  const now = useNow(agents.some((agent) => agent.status === 'running'));
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const chosen = rows.find((agent) => agent.session_id === selected);
  const table = <AgentTable rows={rows} now={now} error={error} onSelect={setSelected} />;
  return (
    <>
      {placement === 'transcript' ? (
        <section
          aria-label="Subagents"
          className="my-3 border border-dialog-edge font-mono text-meta text-white sm:text-ui"
          data-transcript-part
        >
          <div className="flex border-b border-dialog-edge bg-panel px-3 py-1">
            <TeamLine agents={rows} />
          </div>
          <div className="px-3">{table}</div>
        </section>
      ) : table}
      {chosen && (
        <AgentTimelineSheet
          agent={chosen}
          label={agentLabel(chosen, rows)}
          now={now}
          pending={pending}
          load={timeline}
          onOpen={(child) => { setSelected(null); onOpen(child); }}
          onCancel={onCancel}
          onClose={() => setSelected(null)}
        />
      )}
    </>
  );
}

// The word of a step that starts a task or carries a message. A note is the agent's own prose,
// so it carries no word, and the end carries its status word.
const STEP_WORDS: Partial<Record<AgentTimelineItem['kind'], string>> = {
  started: 'Started',
  continued: 'Continued',
  message: 'Message',
};

/**
 * What the sheet shows above the timeline: the latest Activity line while the agent works, else
 * its result or the reason of its end. A note is a step of the timeline, so it never shows here.
 */
export function agentOutcome(agent: Subagent): string | undefined {
  const text = isLive(agent) ? agent.activity : agent.result ?? END_REASON[agent.status];
  return text?.trim() || undefined;
}

/**
 * One step as the timeline shows it: its word with its ink, then its text. The end shows its
 * status word. The sheet shows the outcome above the timeline, so the end does not repeat it.
 */
export function timelineStep(item: AgentTimelineItem, agent: Subagent): {
  word?: { text: string; ink: string; isStatus: boolean };
  text?: string;
} {
  if (item.kind !== 'finished') {
    const word = STEP_WORDS[item.kind];
    return { word: word ? { text: word, ink: 'text-dialog-hint', isStatus: false } : undefined, text: item.text };
  }
  const end = item.status ? agentStatus({ ...agent, status: item.status, outcome: item.outcome }) : undefined;
  const text = item.text?.trim() || (item.status ? END_REASON[item.status] : undefined);
  return {
    word: end && { ...end, isStatus: true },
    text: text === agentOutcome(agent) ? undefined : text,
  };
}

const pad = (value: number) => String(value).padStart(2, '0');

/** The local clock time of an epoch ms, `14:03:07`. */
export function clockTime(at: number): string {
  const date = new Date(at);
  return `${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(date.getSeconds())}`;
}

// The name of a part of the sheet. It is one step smaller than the text that it names.
const PART_NAME = 'mb-1 text-meta text-dialog-hint sm:text-ui';

// The task of an agent as Markdown, like a Council request: its first four lines, and all of it
// on request. The control under the task shows only when the four lines cut it.
function AgentTask({ task }: { task: string }) {
  const [isCut, setCut] = useState(false);
  const text = useRef<HTMLDivElement>(null);
  const { isOpen, clampClass, toggle } = useClampMotion(text);
  useLayoutEffect(() => {
    const element = text.current;
    // Only a clamped body tells whether four lines cut the task.
    if (element === null || !clampClass) return undefined;
    const measure = () => setCut(element.scrollHeight > element.clientHeight + 1);
    measure();
    if (typeof ResizeObserver === 'undefined') return undefined;
    const observer = new ResizeObserver(measure);
    observer.observe(element);
    return () => observer.disconnect();
  }, [task, clampClass]);
  return (
    <section aria-label="Task" className="max-w-[100ch]">
      <h3 className={PART_NAME}>Task</h3>
      <div ref={text} className={`[overflow-wrap:anywhere] ${clampClass}`}>
        <Markdown compact hardBreaks>{councilMarkdown(task)}</Markdown>
      </div>
      {(isCut || isOpen) && (
        <p className="mt-1 text-dialog-hint">
          <TextButton isTally aria-expanded={isOpen} onClick={toggle}>
            {isOpen ? 'Show less' : 'Show the whole task'}
          </TextButton>
        </p>
      )}
    </section>
  );
}

// How long an armed stop waits for its second press.
const STOP_ARMED_MS = 5000;

// The stop of a working agent, before the way into its session. The first press arms it and turns
// it red, and the second press stops the agent. It disarms on blur or after a wait.
function StopAgent({ isPending, onStop }: { isPending: boolean; onStop: () => void }) {
  const [isArmed, setArmed] = useState(false);
  const scope = useId();
  useEffect(() => {
    if (!isArmed) return undefined;
    const timer = window.setTimeout(() => setArmed(false), STOP_ARMED_MS);
    return () => window.clearTimeout(timer);
  }, [isArmed]);
  return (
    <>
      <span id={scope} className="sr-only">Stops this agent and its descendants, including queued work.</span>
      <Button
        variant={isArmed ? 'danger' : 'secondary'}
        aria-label={isArmed ? 'Confirm stop' : 'Stop agent'}
        disabled={isPending}
        aria-describedby={scope}
        title="Stops this agent and its descendants, including queued work."
        onBlur={() => setArmed(false)}
        onClick={() => {
          if (!isArmed) { setArmed(true); return; }
          setArmed(false);
          onStop();
        }}
      >
        Stop
      </Button>
    </>
  );
}

/**
 * The steps of one agent with their times, read from the timeline route, and the way into its
 * session. The sheet takes the whole session pane. The outcome comes first, then the task, then
 * the steps. Open and Stop stand at the right end of the status line on every width, so a long
 * timeline never hides them.
 */
export function AgentTimelineSheet({ agent, label, now, pending, load, onOpen, onCancel, onClose }: {
  agent: Subagent;
  label: string;
  now: number;
  pending: string | null;
  load: (child: string) => Promise<AgentTimeline>;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  onClose: () => void;
}) {
  const [steps, setSteps] = useState<{ items: AgentTimelineItem[] } | { error: string } | null>(null);
  const child = agent.session_id;
  // Read again when the agent moves on, not on a clock.
  const progress = `${agent.status}:${agent.iterations_used}:${agent.finished_at ?? ''}:${agent.resumed_at ?? ''}`;
  useEffect(() => {
    let current = true;
    load(child).then(
      (timeline) => { if (current) setSteps({ items: Array.isArray(timeline?.items) ? timeline.items : [] }); },
      (cause: unknown) => {
        if (current) setSteps({ error: cause instanceof Error && cause.message ? cause.message : 'Could not read the timeline. Try again.' });
      },
    );
    return () => { current = false; };
  }, [load, child, progress]);
  const role = agentRole(agent);
  const status = agentStatus(agent);
  const time = agentTime(agent, now);
  const live = isLive(agent);
  const outcome = agentOutcome(agent);
  const task = agent.task.trim();
  return (
    <Modal size="full" within="session" onDismiss={onClose}>
      <DialogFrame
        title={label}
        onClose={onClose}
      >
        <div className="w-full space-y-6 px-3 py-4 font-mono text-ui text-white sm:px-4 sm:text-body" data-agent-timeline>
          <div className="space-y-1">
            <div className="flex items-center gap-x-4">
              <div className="flex min-w-0 flex-1 flex-wrap items-baseline gap-x-4">
                <span className={`uppercase ${role.ink}`}>{role.text}</span>
                <span className={`uppercase ${status.ink}`}>{status.text}</span>
                {time !== undefined && <span className="text-dialog-hint tabular-nums">{formatAgentTime(time)}</span>}
              </div>
              <div className="flex shrink-0 gap-2">
                {live && <StopAgent isPending={pending === child} onStop={() => onCancel(child)} />}
                <Button aria-label="Open session" onClick={() => onOpen(child)}>Open</Button>
              </div>
            </div>
            {outcome && (
              <p className={`max-w-[100ch] whitespace-pre-wrap [overflow-wrap:anywhere] ${live ? '' : 'font-semibold'}`} data-agent-outcome>{outcome}</p>
            )}
          </div>
          {task && <AgentTask task={task} />}
          <section aria-label="Steps" className="max-w-[100ch]">
            <h3 className={PART_NAME}>Timeline</h3>
            {steps === null ? (
              <p className="text-dialog-hint">Reading the timeline…</p>
            ) : 'error' in steps ? (
              <p role="alert" className="text-err-ink">{steps.error}</p>
            ) : steps.items.length === 0 ? (
              <p className="text-dialog-hint">No steps yet.</p>
            ) : (
              <ol aria-label="Timeline" className="space-y-2">
                {steps.items.map((item, index) => {
                  const { word, text } = timelineStep(item, agent);
                  return (
                    <li key={index} className="grid grid-cols-[8ch_minmax(0,1fr)] gap-x-3">
                      <time dateTime={new Date(item.at).toISOString()} className="text-dialog-hint tabular-nums">{clockTime(item.at)}</time>
                      <p className="whitespace-pre-wrap [overflow-wrap:anywhere]">
                        {word && <span className={`mr-3 ${word.isStatus ? 'uppercase' : ''} ${word.ink}`}>{word.text}</span>}
                        {text}
                      </p>
                    </li>
                  );
                })}
              </ol>
            )}
          </section>
        </div>
      </DialogFrame>
    </Modal>
  );
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

/** How often the team of a gateway older than protocol 21 is read while it is live. */
const LEGACY_TEAM_POLL_MS = 3000;

/**
 * One reader for a session's subagents. It starts from the last team that this device
 * read for `sid`, so a switch back shows the dock at once. It has no timer: the rows of
 * each `session.team_updated` event go into the same cache. It reads the team on open,
 * after a stream reconnect, when the page shows again, after a control call, and on an
 * event without usable rows. It does not read when `teamTotal` is 0 and no rows are cached.
 *
 * `legacyRunning` is null for a gateway that pushes rows. For an older gateway it says whether
 * the leader's turn runs: such a gateway sends only counts, so the team is read every 3 s
 * while the turn runs or a child is live.
 */
export function useAgentTeam(
  client: GatewayClient,
  sid: string,
  teamTotal: number | null = null,
  legacyRunning: boolean | null = null,
) {
  const hub = useContext(SessionEventsContext);
  const [state, setState] = useState<{ sid: string; agents: Subagent[] }>(() => ({
    sid,
    agents: client.cachedAgents?.(sid) ?? [],
  }));
  const [pending, setPending] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  // Counts the applied events, so a read that an event passed reads again.
  const pushes = useRef(0);
  const current = state.sid === sid ? state.agents : (client.cachedAgents?.(sid) ?? []);
  const hasNoTeam = useEffectEvent(() => teamTotal === 0 && !client.cachedAgents?.(sid)?.length);
  const polls = legacyRunning !== null && (legacyRunning || current.some(isLive));
  useEffect(() => {
    const controller = new AbortController();
    let fetching = false;
    const refresh = async () => {
      if (fetching || controller.signal.aborted || document.hidden || hasNoTeam()) return;
      fetching = true;
      const seen = pushes.current;
      try {
        const rows = await client.agents(sid, controller.signal);
        if (!controller.signal.aborted && Array.isArray(rows)) setState({ sid, agents: rows });
      } catch {
        // The rows that the reader has stay; the next read tries again.
      } finally {
        fetching = false;
      }
      // The answer can be older than an event that came during the read.
      if (pushes.current !== seen) void refresh();
    };
    void refresh();
    const timer = polls ? window.setInterval(() => void refresh(), LEGACY_TEAM_POLL_MS) : undefined;
    document.addEventListener('visibilitychange', refresh);
    let wasConnected: boolean | null = null;
    const offConnection = hub?.subscribeConnection((connected) => {
      if (connected && wasConnected === false) void refresh();
      wasConnected = connected;
    });
    return () => {
      controller.abort();
      if (timer !== undefined) window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refresh);
      offConnection?.();
    };
  }, [client, hub, sid, revision, polls]);
  useEffect(() => {
    if (!hub) return;
    return hub.subscribeSession(
      sid,
      (event) => {
        if (event.type !== 'session.team_updated') return;
        const rows = client.applyTeamUpdate?.(sid, event) ?? null;
        if (!rows) {
          setRevision((value) => value + 1);
          return;
        }
        pushes.current += 1;
        setState({ sid, agents: rows });
      },
      { replay: false },
    );
  }, [client, hub, sid]);
  // One control call at a time for each child. It reads the team again after the call.
  // The result is the error text, or null when the call worked.
  const control = useCallback(async (child: string, call: () => Promise<unknown>, fallback: string) => {
    setPending(child);
    try {
      await call();
      return null;
    } catch (cause) {
      return cause instanceof Error && cause.message ? cause.message : fallback;
    } finally {
      setPending(null);
      setRevision((value) => value + 1);
    }
  }, []);
  const cancel = useCallback(async (child: string) => {
    setError(null);
    setError(await control(child, () => client.cancelAgent(sid, child), 'Could not stop agent. Try again.'));
  }, [client, sid, control]);
  const timeline = useCallback(
    (child: string) => client.agentTimeline(sid, child),
    [client, sid],
  );
  return { agents: current, pending, error, cancel, timeline };
}

/** What `useAgentTeam` gives its views. */
export type AgentTeam = ReturnType<typeof useAgentTeam>;
