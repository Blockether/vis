import { type CSSProperties, useCallback, useEffect, useId, useLayoutEffect, useRef, useState } from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import type { GatewayClient } from '../lib/gateway';
import type { AgentTimeline, AgentTimelineItem, Subagent } from '../lib/types';
import { BandButton, DialogFrame, Modal, TextButton } from './ui';

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

// The columns of the team table: ROLE, TITLE, UPDATE, STATUS and TIME, so the status stands
// right before the time. ROLE, TITLE and STATUS fit the longest word of the team, and TIME fits
// `59m 59s`, so a ticking time never moves a column. TITLE takes at most a third of the row; a
// longer title wraps. On a phone, UPDATE and TIME are the second line.
const COLUMNS = 'grid-cols-[var(--role)_minmax(0,1fr)_auto] sm:grid-cols-[var(--role)_min(var(--title),32%)_minmax(0,1fr)_var(--status)_7ch]';
const ROW = `w-full min-w-0 items-baseline gap-x-3 py-1 text-left ${COLUMNS}`;
const ROW_BUTTON = `group grid rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent ${ROW}`;

// The widths of the columns that fit the team, in `ch` of the monospace font. A heading is the
// least width of its column; the bold title gets one more `ch`.
function columnWidths(rows: readonly Subagent[]): CSSProperties {
  const widest = (least: string, words: string[]) => Math.max(least.length, ...words.map((word) => word.length));
  return {
    '--role': `${widest('role', rows.map((agent) => agentRole(agent).text))}ch`,
    '--title': `${widest('title', rows.map((agent) => agentLabel(agent, rows))) + 1}ch`,
    '--status': `${widest('status', rows.map((agent) => agentStatus(agent).text))}ch`,
  } as CSSProperties;
}

// The table of one team, with the column headings on a wide screen.
function AgentTable({ rows, now, error, onSelect }: {
  rows: readonly Subagent[];
  now: number;
  error?: string | null;
  onSelect: (sid: string) => void;
}) {
  return (
    <div style={columnWidths(rows)}>
      <div aria-hidden className={`hidden border-b border-dialog-edge text-dialog-hint uppercase sm:grid ${ROW}`}>
        <span>role</span>
        <span>title</span>
        <span>update</span>
        <span>status</span>
        <span className="text-right">time</span>
      </div>
      <ul className="divide-y divide-dialog-edge/40">
        {rows.map((agent) => {
          const role = agentRole(agent);
          const status = agentStatus(agent);
          const update = agentUpdate(agent);
          const time = agentTime(agent, now);
          return (
            <li key={agent.session_id}>
              <button type="button" data-agent-row={agent.session_id} onClick={() => onSelect(agent.session_id)} className={ROW_BUTTON}>
                <span className={`order-1 uppercase sm:order-none ${role.ink}`}>{role.text}</span>
                <span className="order-2 font-bold [overflow-wrap:anywhere] group-hover:underline sm:order-none" title={agent.task}>
                  {agentLabel(agent, rows)}
                </span>
                <span
                  className={`order-4 col-span-2 line-clamp-2 text-dialog-hint [overflow-wrap:anywhere] sm:order-none sm:col-span-1 ${update ? '' : 'max-sm:hidden'}`}
                  title={update}
                >
                  {update ?? ''}
                </span>
                <span className={`order-3 whitespace-nowrap text-right uppercase sm:order-none sm:text-left ${status.ink}`}>{status.text}</span>
                <span className="order-5 col-start-3 whitespace-nowrap text-right text-dialog-hint tabular-nums sm:order-none sm:col-start-auto">
                  {time === undefined ? '' : formatAgentTime(time)}
                </span>
              </button>
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

// The task of an agent: its first four lines, and all of it on request. The control under the
// task shows only when the four lines cut it.
function AgentTask({ task }: { task: string }) {
  const [isOpen, setOpen] = useState(false);
  const [isCut, setCut] = useState(false);
  const text = useRef<HTMLParagraphElement>(null);
  useLayoutEffect(() => {
    const element = text.current;
    if (element === null || isOpen) return undefined;
    const measure = () => setCut(element.scrollHeight > element.clientHeight + 1);
    measure();
    if (typeof ResizeObserver === 'undefined') return undefined;
    const observer = new ResizeObserver(measure);
    observer.observe(element);
    return () => observer.disconnect();
  }, [task, isOpen]);
  return (
    <section aria-label="Task">
      <h3 className={PART_NAME}>Task</h3>
      <p ref={text} className={`whitespace-pre-wrap [overflow-wrap:anywhere] ${isOpen ? '' : 'line-clamp-4'}`}>{task}</p>
      {(isCut || isOpen) && (
        <p className="mt-1 text-dialog-hint">
          <TextButton isTally aria-expanded={isOpen} onClick={() => setOpen((open) => !open)}>
            {isOpen ? 'Show less' : 'Show the whole task'}
          </TextButton>
        </p>
      )}
    </section>
  );
}

// How long an armed stop waits for its second press.
const STOP_ARMED_MS = 5000;

// The stop of a working agent: one quiet control after the time on the status line. The first
// press arms it, and the second press stops the agent. It disarms on blur or after a wait.
function StopAgent({ isPending, onStop }: { isPending: boolean; onStop: () => void }) {
  const [isArmed, setArmed] = useState(false);
  const scope = useId();
  useEffect(() => {
    if (!isArmed) return undefined;
    const timer = window.setTimeout(() => setArmed(false), STOP_ARMED_MS);
    return () => window.clearTimeout(timer);
  }, [isArmed]);
  return (
    <span className={isArmed ? 'text-err-ink' : 'text-dialog-hint'}>
      <span id={scope} className="sr-only">Stops this agent and its descendants, including queued work.</span>
      <TextButton
        isTally
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
        {isArmed ? 'Confirm stop' : 'Stop agent'}
      </TextButton>
    </span>
  );
}

/**
 * The steps of one agent with their times, read from the timeline route, and the way into its
 * session. The sheet takes the whole session pane. The outcome comes first, then the task, then
 * the steps. Its one verb, the way into the session, stands in the title band, so a long timeline
 * never hides it. A working agent also has a stop on its status line.
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
        actions={<BandButton onClick={() => onOpen(child)}>Open session</BandButton>}
      >
        <div className="w-full max-w-[100ch] space-y-6 px-3 py-4 font-mono text-ui text-white sm:px-4 sm:text-body" data-agent-timeline>
          <div className="space-y-1">
            <div className="flex flex-wrap items-baseline gap-x-4">
              <span className={`uppercase ${role.ink}`}>{role.text}</span>
              <span className={`uppercase ${status.ink}`}>{status.text}</span>
              {time !== undefined && <span className="text-dialog-hint tabular-nums">{formatAgentTime(time)}</span>}
              {live && <StopAgent isPending={pending === child} onStop={() => onCancel(child)} />}
            </div>
            {outcome && (
              <p className={`whitespace-pre-wrap [overflow-wrap:anywhere] ${live ? '' : 'font-semibold'}`} data-agent-outcome>{outcome}</p>
            )}
          </div>
          {task && <AgentTask task={task} />}
          <section aria-label="Steps">
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

/**
 * One reader for a session's subagents. It polls while a turn runs or an agent
 * works or waits, and reads again when `generation` changes.
 */
export function useAgentTeam(client: GatewayClient, sid: string, running: boolean, generation = 0) {
  const [state, setState] = useState<{ sid: string; agents: Subagent[] }>({ sid, agents: [] });
  const [pending, setPending] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  const current = state.sid === sid ? state.agents : [];
  const live = running || current.some(isLive);
  useEffect(() => {
    const controller = new AbortController();
    let fetching = false;
    const refresh = async () => {
      if (fetching || controller.signal.aborted || document.hidden) return;
      fetching = true;
      try {
        const rows = await client.agents(sid, controller.signal);
        if (!controller.signal.aborted && Array.isArray(rows)) setState({ sid, agents: rows });
      } catch {
        // The rows that the reader has stay; the next read tries again.
      } finally {
        fetching = false;
      }
    };
    void refresh();
    const timer = live ? window.setInterval(() => { void refresh(); }, 3000) : undefined;
    document.addEventListener('visibilitychange', refresh);
    return () => {
      controller.abort();
      if (timer !== undefined) window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refresh);
    };
  }, [client, sid, revision, generation, live]);
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
